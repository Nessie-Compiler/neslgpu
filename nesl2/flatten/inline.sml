(* inline.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * A simple expansive inliner for Flan.  The goal is to inline the functions that wrap primitive
 * operations and things like zip/unzip, which can then cancel out.
 *)

structure Inline : sig

    val transform : (FlanEnv.env * Flan.program) -> Flan.program

  end = struct

    structure F = Flan
    structure VMap = FlanVar.Map

  (* size threshold for inlining; this is chosen so that lifted binary operators are inlined *)
    val threshold = 20

  (* Debugging *)
    fun functToString (F.F{name, stamp, ...}) = Atom.toString name ^ Stamp.toString stamp
    val varToString = FlanVar.toString
    fun atomToString (F.Var x) = varToString x
      | atomToString _ = "<atom>"

  (* a per-function property that tracks its size *)
    val {setFn=setSize, getFn=(getSize : F.funct -> int), clrFn=clearSize, ...} = let
	  fun init (F.F{name, ...}) = raise Fail(concat["getSize(", Atom.toString name, ") undefined"])
	  in
	    PropList.newProp (F.propsOf, init)
	  end
(*
val getSize = fn f => let val sz = getSize f in print(concat["getSize(", FlanFunct.toString f, ") = ", Int.toString sz, "\n"]); sz end
*)

  (* a per-function property that tracks its definition *)
    val {setFn=setDef, getFn=(getDef : F.funct -> F.var list * F.exp), clrFn=clearDef, ...} = let
	  fun init (F.F{name, ...}) = raise Fail(concat["getDef(", Atom.toString name, ") undefined"])
	  in
	    PropList.newProp (F.propsOf, init)
	  end

    fun clear (F.TopFun(_, defs)) = List.app (fn (f, _, _) => (clearSize f; clearDef f)) defs
      | clear _ = ()

  (* analyse and record the size of a function definition *)
    fun doFn (f, params, e) = let
	  fun sizeOf exp = (case exp
		 of F.ExpForEach _ => raise Fail "unexpected ExpForEach"
		  | F.ExpLet(binds, e) =>
		      List.foldl (fn ((_, e), sz) => sizeOf e + sz) (sizeOf e) binds
		  | F.ExpTuple(_, atms, e) => List.length atms + sizeOf e
		  | F.ExpSeq(_, atms, e) => List.length atms + sizeOf e
		  | F.ExpPure(FlanPure.Base(Pure.PROJ _), [F.Var x]) =>
		      if List.exists (fn x' => FlanVar.same(x, x')) params
			then 0 (* bias toward parameter tuples *)
			else 1
		  | F.ExpPure _ => 1
		  | F.ExpCmd _ => 1
		  | F.ExpIf(_, e1, e2, _) => 1 + sizeOf e1 + sizeOf e2
		  | F.ExpApplyFun(g, atms) =>
		      if FlanFunct.same(f, g)
			then threshold+1  (* don't inline recursive functions *)
			else 1 + List.length atms
		  | F.ExpAtom _ => 0
		  | F.ExpLifted(e, _) => sizeOf e + 10
		(* end case *))
	  in
	    setSize (f, sizeOf e);
	    setDef (f, (params, e))
	  end

  (* the analysis pass determines the size of each function *)
    fun analyse tops = let
	  fun analTop (F.TopFun(_, defs)) = List.app doFn defs
	    | analTop (F.TopBind(_, e)) = ()
	    | analTop (F.TopExp(e, _, _)) = ()
	  in
	    List.app analTop tops
	  end

  (* copy a bound variable and extend the environment *)
    fun extend (env, x) = let
	  val x' = FlanVar.copy x
	  in
	    (x', VMap.insert(env, x, F.Var x'))
	  end

  (* rename a variable; if the variable is in the environment, then the use count of
   * its range should be correct.  If it is not, then it is a free variable and its
   * use count is increased.
   *)
    fun rename (env, x) = (case VMap.find(env, x)
	   of SOME atm => atm (* (Census.incAtom atm; atm) *)
	    | NONE => (Census.incUseCnt x; F.Var x)
	  (* end case *))

  (* inline-expand a function body *)
    fun expand (params, body, args) = let
	(* add a mapping from an function parameter to an argument and update the use
	 * use count of the argument.
	 *)
	  fun copyArg (x, atm, env) = (
		case atm
		 of F.Var x' => (
		    (* set useCnt(x') += useCnt(x)-1 *)
		      Census.decUseCnt x'; Census.combineUseCnt(x', x))
		  | _ => ()
		(* end case *);
		VMap.insert(env, x, atm))
	  fun copy (env, e) = (case e
		 of F.ExpForEach _ => raise Fail "unexpected ExpForEach"
		  | F.ExpLet(binds, e) => let
		      fun copyBind ((x, e), (bnds, env)) = let
			    val (x', env') = extend(env, x)
			    in
			      ((x', copy(env, e))::bnds, env')
			    end
		      val (binds', env') = List.foldl copyBind ([], env) binds
		      in
			F.mkLet(List.rev binds', copy(env', e))
		      end
		  | F.ExpTuple(lhs, rhs, e) => let
		      val (lhs', env') = extend(env, lhs)
		      in
			F.mkTuple(lhs', List.map (copyAtm env) rhs, copy(env', e))
		      end
		  | F.ExpSeq(lhs, rhs, e) => let
		      val (lhs', env') = extend(env, lhs)
		      in
			F.mkSeq(lhs', List.map (copyAtm env) rhs, copy(env', e))
		      end
		  | F.ExpPure(pure, args) => F.mkPure(pure, List.map (copyAtm env) args)
		  | F.ExpCmd(cmd, args) => F.mkCmd(cmd, List.map (copyAtm env) args)
		  | F.ExpIf(x, e1, e2, ty) =>
		      F.mkIf(copyAtm env x, copy(env, e1), copy(env, e2), ty)
		  | F.ExpApplyFun(f, args) => let
		    (* because of use before declare, we need to check for possible inlining here *)
		      val sz = getSize f
		      in
			if sz <= threshold
			  then let
			    val args' = List.map (copyAtm env) args
			    val (params, body) = getDef f
			    val env' = ListPair.foldlEq copyArg env (params, args')
			    in
(*
print(concat["nested inline expand ", functToString f, " (",
String.concatWith ", " (List.map atomToString args), "); size = ", Int.toString sz, "\n"]);
*)
			      copy (env', body)
			    end
			  else (
			    Census.incAppCnt f;
			    F.mkApplyFun(f, List.map (copyAtm env) args))
		      end
		  | F.ExpAtom atm => F.mkAtom(copyAtm env atm)
		  | F.ExpLifted(e, bnds) => raise Fail "FIXME: ExpLifted not supported"
		(* end case *))
	  and copyAtm env (F.Var x) = rename(env, x)
	    | copyAtm _ atm = atm
	(* construct the initial enviornment and also for each argument *)
	  val env = ListPair.foldlEq copyArg VMap.empty (params, args)
	  in
	    copy (env, body)
	  end

    fun transform (env, F.Program tops) = let
	  fun doTop (F.TopFun(f, defs)) = let
		fun doDef (fdef as (f', params, e)) =
		      if FlanFunct.appCnt f' > 0
			then (case doExp e
			   of NONE => fdef
			    | SOME e => let
				val fdef' = (f', params, e)
				in
				  doFn fdef';  (* update the size of the function *)
				  fdef'
				end
			  (* end case *))
			else fdef  (* ignore uncalled functions *)
		in
		  F.TopFun(f, List.map doDef defs)
		end
	    | doTop (top as F.TopBind(lhs, e)) = (case doExp e
		 of NONE => top
		  | SOME e => F.TopBind(lhs, e)
		(* end case *))
	    | doTop (top as F.TopExp(e, ty, p)) = (case doExp e
		 of NONE => top
		  | SOME e => F.TopExp(e, ty, p)
		(* end case *))
	  and doExp e = (case e
		 of F.ExpForEach _ => raise Fail "unexpected ForEach"
		  | F.ExpLet(bnds, e) => let
		      fun doBinds ([], false, _) = NONE
			| doBinds ([], true, bnds) = SOME(List.rev bnds)
			| doBinds ((x, e)::r, anyChange, bnds) = (case doExp e
			     of NONE => doBinds(r, anyChange, (x, e)::bnds)
			      | SOME e' => doBinds(r, true, (x, e')::bnds)
			    (* end case *))
		      in
			case (doBinds (bnds, false, []), doExp e)
			 of (NONE, NONE) => NONE
			  | (SOME bnds, NONE) => SOME(F.mkLet(bnds, e))
			  | (NONE, SOME e) => SOME(F.mkLet(bnds, e))
			  | (SOME bnds, SOME e) => SOME(F.mkLet(bnds, e))
			(* end case *)
		      end
		  | F.ExpTuple(x, atms, e) => (case doExp e
		       of NONE => NONE
			| SOME e' => SOME(F.mkTuple(x, atms, e'))
		      (* end case *))
		  | F.ExpSeq(lhs, atms, e) => (case doExp e
		       of NONE => NONE
			| SOME e' => SOME(F.mkSeq(lhs, atms, e'))
		      (* end case *))
		  | F.ExpPure _ => NONE
		  | F.ExpCmd _ => NONE
		  | F.ExpIf(x, e1, e2, ty) => (case (doExp e1, doExp e2)
		       of (NONE, NONE) => NONE
			| (SOME e1, NONE) => SOME(F.mkIf(x, e1, e2, ty))
			| (NONE, SOME e2) => SOME(F.mkIf(x, e1, e2, ty))
			| (SOME e1, SOME e2) => SOME(F.mkIf(x, e1, e2, ty))
		      (* end case *))
		  | F.ExpApplyFun(f, args) => let
		      val sz = getSize f
		      in
			if sz <= threshold
			  then let
			    val (params, body) = getDef f
			    in
(*
print(concat["inline expand ", functToString f, " (",
String.concatWith ", " (List.map atomToString args), "); size = ", Int.toString sz, "\n"]);
*)
			      Census.decAppCnt f;
			      SOME(expand (params, body, args))
			    end
			  else NONE
		      end
		  | F.ExpAtom _ => NONE
		  | F.ExpLifted(e, binds) => (case doExp e
		       of NONE => NONE
			| SOME e => SOME(F.mkLifted(e, binds))
		      (* end case *))
		(* end case *))
	  in
	    analyse tops;
	    F.Program(List.map doTop tops)
	      before List.app clear tops
	  end
(*DEBUG*) handle ex => raise ex

  end (* Inliner *)
