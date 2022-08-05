(* contract.sml
 *
 * COPYRIGHT (c) 2014 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *
 * Contraction phase for the Flan representation.
 * The contraction optimizations include:
 *  - inlining of functions that are called exactly once
 *  - elimination of dead functions and variables
 *  - constant folding for tuple construction/selection
 *    and vector construction/selection with __vector
 *
 * Question: how does this deal w/ dead functions that call themselves
 * recursively multiple times (like quicksort?)
 *)

(* FIXME:
 * -remove deleted instances from flanEnv (maybe doesn't matter?)
 *)

structure Contract : sig

  val contract : Flan.program * FlanEnv.env -> Flan.program

  end = struct

    structure F = Flan
    structure FF = FlanFunct
    structure P = FlanPure
    structure E = FlanEnv
    structure C = Census
    structure ST = Stats

    val debug = false
    fun pr l = if debug
	       then (print l; print "\n")
	       else ()

    (******* Flags *******)

    (* Per-function property indicating that function should be deleted,
     * because its use count is 1: either it's dead or it will be inlined. *)
    val {getFn=getToDelete, setFn=setToDelete} = FF.newFlag()
    fun markToDelete f = setToDelete(f, true)


    (* Don't handle a function's body
     * (during inlining or side effect analysis)
     * because we are inside that function body already.
     * (We do this instead of nulling out its binding because NESL is
     * first-order, so functions don't have bindings
     *)
	
    (* Per-function property indicating the function has been inlined. 
     * (Used to distinguish between inlined and recursive dead functions *)
    val {getFn=isInlined, setFn=setInlined} = FF.newFlag()
    fun markInlined f = setInlined(f, true)
		      


    (******* Counters *******)
    val cntUnusedVarElim          = ST.newCounter "contract:unused-var-elim"
    val cntRedundantVarElim       = ST.newCounter "contract:redundant-var-elim"
(*    val cntRedundantPureElim      = ST.newCounter "contract:redundant-pure-elim" *)
    val cntProjFold		  = ST.newCounter "contract:proj-fold"
    val cntTupleFold              = ST.newCounter "contract:tuple-fold"
    val cntDeadFnElim             = ST.newCounter "contract:dead-fn-elim"
    val cntDeadBranchElim         = ST.newCounter "contract:dead-branch-elim"
    val cntInline                 = ST.newCounter "contract:inline"
    val firstCounter              = cntUnusedVarElim
    val lastCounter               = cntInline
    val cntIters                  = ST.newCounter "contract:iterations"

    (******* Get variable info *******)
    fun useCntRef (F.V{useCnt, ...}) = useCnt
    fun useCntOf v = !(useCntRef v)

    fun unused v = (useCntOf v = 0)

    fun appCntRef (F.F{appCnt, ...}) = appCnt
    fun appCntOf f = !(appCntRef f)
    fun funUnused f = (appCntOf f = 0)


    (******* Effect analysis *******)

    fun getInst (env, f as F.F{name=n, ...}) = let
      val funct = FF.getFunct f
    in
      (case E.lookupInstance(env, funct, f)
	of SOME inst => inst
	 | NONE => raise Fail ("unbound function "^(Atom.toString n))
      (* end case *))
    end


    fun hasSideEffects env exp = let
(*       val _ = pr "hasSideEffects" *)
      val hasSE = hasSideEffects env
      val bindSE = bindSideEffects env
      val funSE = funSideEffects env
    in
      (case exp
	of F.ExpLet(binds, e) => List.exists bindSE binds orelse hasSE e
	 | F.ExpTuple(_, _, e) => hasSE e
	 | F.ExpSeq(_, _, e) => hasSE e
	 | F.ExpPure(p, atms) => false
	 | F.ExpCmd(c, atms) => true
	 | F.ExpIf(b, e1, e2, t) => hasSE e1 orelse hasSE e2
	 | F.ExpApplyFun(f, atms) => funSE f
	 | F.ExpAtom _ => false
	 | F.ExpLifted _ => false (* no side effects in forEach *)
	 | F.ExpForEach _ => raise Fail "found forEach in contract"
      (* end case *))
    end

    and bindSideEffects env (vs, e) = hasSideEffects env e
    and funSideEffects env (f as F.F{name=n, ty=ref(dom, ty), ...}) = let
(*      val _ = pr ("funSideEffects for: "^(Atom.toString n)^" at type: "^(FlanTypes.tyListToString dom)) *)
      val (_, _, e) = getInst (env, f)
    in
      if FF.getCanRecurse f
      then (FF.dontRecurse f;
	    let val se = hasSideEffects env e
	    in
	      (FF.doRecurse f; se)
	    end)
      else false
    end

    (******* Variable substitution *******)
    structure Subst = FlanVar.Map

    fun substitute (sub, F.Var v) =
	(case Subst.find(sub, v)
	  of SOME a => a
	   | NONE => F.Var v
	(* end case *))
      | substitute (sub, a) = a

    fun substitute' (sub, atms) = map (fn a => substitute(sub, a)) atms

    fun addSub (sub, v, a) = let
      val a' = substitute(sub, a)
      val _ = (case a' of F.Var v' => C.combineUseCnt(v', v)
		       | _ => ())
    in
      Subst.insert(sub, v, a')
    end

    fun addSub' (sub, [], []) = sub
      | addSub' (sub, vs, []) = raise Fail "addSub: var list/atm list length mismatch"
      | addSub' (sub, [], atms) = raise Fail "addSub: var list/atm list length mismatch"
      | addSub' (sub, v::vs, atm::atms) = let
	  val sub' = addSub(sub, v, atm)
	in
	  addSub'(sub', vs, atms)
	end



    (******* Some handy functions *******)

    fun atmKind (F.Var v) = F.kindOf v
      | atmKind _ = F.VK_None

    fun deleteExp (sub, exp) = let
      fun subFn e = substitute(sub, e)
      fun subFn' e = substitute'(sub, e)
    in
      C.delete(subFn, subFn', exp)
    end

    (******* Let Floating *******)

    fun addBind((v, e), exp) = let
      val e' = floatExp e
      val _ = F.setLetKind(v, e')
      val b = (v, e')
    in
      (case exp
	of F.ExpLet(bs, innerExp) => F.mkLet(b::bs, innerExp)
	 | _ => F.mkLet([b], exp)
      (* end case *))
    end

    and addBinds([], e) = e
      | addBinds(b::bs, e) = addBind(b, addBinds(bs, e))

    and floatLet([], exp) = floatExp exp
      | floatLet((vs, e)::bs, exp) = let
	  val inner = floatLet(bs, exp)
	in
	  (case e
	    of F.ExpLet(bs', e') => let
		 (* let vs = (let b1 = e1, b2 = e2...) in e' ... in exp) =>
		  * let b1 = e1 ...
		  * let vs = e'
		  *   in exp
		*)
		 val inner = floatExp(addBind((vs, e'), inner))
	       in
		 addBinds(bs', inner)
	       end
	     | F.ExpTuple(vs', atms, e') => let
		 (* let vs =
		  *   let vs' = atms
		  *   in e'
		  * in exp
		  * =>
		  * let vs' = atms
		  * let vs = e'
		  * in exp
		  *)
		 val inner = floatExp(addBind((vs, e'), inner))
	       in
		 F.mkTuple(vs', atms, inner)
	       end
	     | F.ExpSeq(vs', atms, e') => let
		 val inner = floatExp(addBind((vs, e'), inner))
	       in
		 F.mkSeq(vs', atms, inner)
	       end
	     | _ => addBind((vs, e), inner)
	  (* end case *))
	end

    and floatExp (F.ExpLet(binds, exp)) = floatLet(binds, exp)
      | floatExp (F.ExpTuple(v, atms, e)) = F.mkTuple(v, atms, floatExp e)
      | floatExp (F.ExpSeq(v, atms, e)) = F.mkSeq(v, atms, floatExp e)
      | floatExp (e as F.ExpPure _) = e
      | floatExp (e as F.ExpCmd _) = e
      | floatExp (F.ExpIf(a, e1, e2, t)) = F.mkIf(a, floatExp e1, floatExp e2, t)
      | floatExp (e as F.ExpApplyFun _) = e
      | floatExp (e as F.ExpAtom _) = e
      | floatExp (F.ExpLifted(e, binds)) = F.mkLifted(floatExp e, binds)

    fun floatTops tops = let
      fun floatTop ([], acc) = acc
	| floatTop (F.TopFun(funct, insts)::tops, acc) = let
	    val topFun = F.TopFun(funct, map (fn (f, args, e) => (f, args, floatExp e)) insts)
	  in
	    floatTop(tops, topFun::acc)
	  end
	| floatTop (F.TopBind(vs, exp)::tops, acc) = let
	    val exp' = floatExp exp
	    fun addTopBinds([], tops) = tops
	      | addTopBinds(b::bs, tops) = addTopBinds(bs, (F.TopBind b)::tops)
	  in
	    (case exp'
	      (* vs = (b1 = e1, b2 = e2...) in e  =>
	       * b1 = b1, b2 = e2 ... vs = e
	       * so we hoist binds to top level *)
	      of F.ExpLet(binds, e) => floatTop(tops, F.TopBind(vs, e)::(addTopBinds(binds, acc)))
	       | _ => floatTop(tops, F.TopBind(vs, exp')::acc)
	    (* end case *))
	  end
	| floatTop (F.TopExp(e, ty, p)::tops, acc) = floatTop(tops, F.TopExp(floatExp e, ty, p)::acc)
      val revTops = floatTop (tops, [])
    in
      List.rev revTops
    end


  (******* Contraction *******)

  (* check to see if a tuple expression v = (x1, x2, ..., xn) is reconstructing an available tuple;
   * if so, then return the tuple's name.  Otherwise return NONE.
   *)
    fun reconstructTuple (v, x::xs) = (case atmKind x
	   of F.VK_Let(F.ExpPure(P.Base(Pure.PROJ 1), [F.Var y])) => let
		fun chkArgs (i, []) = true
		  | chkArgs (i, x::xs) = (case atmKind x
		       of F.VK_Let(F.ExpPure(P.Base(Pure.PROJ j), [F.Var y'])) =>
			    (i = j) andalso FlanVar.same(y, y')
			| _ => false
		      (* end case *))
		in
		  if chkArgs (2, xs) andalso FlanTypes.sameTy(FlanVar.typeOf v, FlanVar.typeOf y)
		    then SOME y
		    else NONE
		end
	    | _ => NONE
	  (* end case *))

    fun doBindsDown (env, sub, [], bindAcc) = ((* pr "doBindsDown -- base case"; *)(sub, bindAcc))
      | doBindsDown (env, sub, (b::bs), bindAcc) = let
(*	  val _ = pr "doBindsDown" *)
	  val (v, e) = b
	in
	  if (not(hasSideEffects env e) andalso unused v)
	  then
	    (* Eliminate this binding, and decrement counts of all variables used in it *)
	    (ST.tick cntUnusedVarElim; (* pr "unusedVarElim in doBinds"; *)
	     deleteExp (sub, e);
	     doBindsDown (env, sub, bs, bindAcc))
	  else
	    (case e
	      (* If binding is of form x=y, add substitution and remove binding *)
	      of (F.ExpAtom a) =>
		 (ST.tick cntRedundantVarElim;  pr ("redundantVarElim: "^(FlanVar.toString v)^"maps to: "^(FlanUtil.atomToString a)^"\n");
		  C.decAtom (substitute(sub, a));
		  doBindsDown (env, addSub(sub, v, a), bs, bindAcc))
	       (* Otherwise, we just contract e, then handle the rest of the bindings *)
	       | _ => let
		   val e' = doExp (env, sub, e)
		   val _ = F.setLetKind(v, e')
		   val b' = (v, e')
		 in
		   doBindsDown (env, sub, bs, b'::bindAcc)
		 end
	    (* end case *))
	end

    (* Going up, we cull dead variables but don't try to add substitutions *)
    and doBindsUp(env, sub, [], bindAcc) = ((* pr "doBindsUp -- base case" ;*) bindAcc)
      | doBindsUp (env, sub, (v, e)::bs, bindAcc) =
	if (not(hasSideEffects env e) andalso unused v)
	then
	  (ST.tick cntUnusedVarElim; (* pr "unusedVarElim in doBinds"; *)
	   deleteExp (sub, e);
	   doBindsUp(env, sub, bs, bindAcc))
	else
	  ((* pr "doBindsUp -- no elim"; *)
	   doBindsUp(env, sub, bs, (v, e)::bindAcc))

    and doExp (env, sub, exp) = (case exp
	   of F.ExpLet(binds, e) => let
(* 	       val _ = pr "ExpLet"  *)
	       val (sub', binds') = doBindsDown (env, sub, binds, [])
	       val e' = doExp (env, sub', e)
(*
	       (* redo binding in reverse order since some use counts may have decremented *)
*)
	       val binds' = doBindsUp (env, sub', binds', [])
	     in
	       (case binds'
		 of [] => e'
		  | _ => (F.mkLet(binds', e'))
	       (* end case *))
	     end
	    | F.ExpTuple(v, atms, e) => let
(* 	       val _ = pr "ExpTuple" *)
		val atms' = substitute' (sub, atms)
		(* Need to make sure binding is up to date *) 
		val _ = F.mkTuple(v, atms', e)
		val _ = pr (String.concat["Tuple ", (FlanVar.toString v), " maps to: ", 
					  (FlanUtil.atomsToString atms')])
		in
		  if unused v
		    then (
		    (* decrement use counts for each atom, then delete v *)
		      ST.tick cntUnusedVarElim; pr "unusedVarElim for tuple";
		      List.app C.decAtom atms';
		      doExp (env, sub, e))
		  else (case reconstructTuple(v, atms) 
		       of SOME y => ( (* can substitute y for v *)
			  pr "tupleFold\n";
			    ST.tick cntTupleFold;
		      	    List.app C.decAtom atms';
			    doExp (env, addSub(sub, v, F.Var y), e))
			| NONE => let
			    val e' = doExp (env, sub, e)
			    in
			      (* Check again if v is unused; if so, delete *)
			      if unused v
				then (
				  ST.tick cntUnusedVarElim; pr ("unusedVarElim for Tuple after recursive call: "^(FlanVar.toString v)^"\n");
				  List.app C.decAtom atms';
				  e')
			      else F.mkTuple(v, atms', e')
			    end
		      (* end case *))
		end
	   | F.ExpSeq(v, atms, e) => let
(*	       val _ = pr "ExpSeq" *)
	       val atms' = substitute' (sub, atms)
	       val _ = F.mkSeq(v, atms', e)
	     in
	       if unused v
	       then
		 (ST.tick cntUnusedVarElim; pr "unused var elim in expseq";
		  List.app C.decAtom atms';
		  doExp (env, sub, e))
	       else
		 let
		   val e' = doExp (env, sub, e)
		 in
		   if unused v
		   then (ST.tick cntUnusedVarElim; pr "unused var elim after recursion in expseq";
			 List.app C.decAtom atms';
			 e')
		   else F.mkSeq(v, atms', e')
		 end
	     end
	   | F.ExpPure(P.Base(Pure.PROJ i), [atm]) => let
(*	       val _ = pr "ExpPure -- PROJ" *)
	       val atm' = substitute (sub, atm)
	     in
	       (case atmKind atm'
		 of F.VK_Tuple atms => let
		      val ai = substitute(sub, List.nth(atms, (i - 1)))
		    in
		      (ST.tick cntProjFold; pr "project constant folding";
		       pr (String.concat["Tuple: ", FlanUtil.atomToString atm',
					 "Projected atom: ", FlanUtil.atomToString ai,
					 "\n"]);
		       C.decAtom atm';
		       C.incAtom ai;
		       F.mkAtom ai)
		    end
		  | _ => F.mkPure(P.Base(Pure.PROJ i), [atm'])
	       (* end case *))
	     end
(*
	   (* MAKE_SEGDES is the inverse of LENGTHS *)
	   | F.ExpPure(P.Base Pure.MAKE_SEGDES, [atm]) => let
	       val atm' = substitute (sub, atm)
	     in
	       (case atmKind atm
		 of F.VK_Let(F.ExpPure(P.Base Pure.LENGTHS, [atm2])) =>  let
		      val atm2' = substitute(sub, atm2)
		    in
		      (ST.tick cntRedundantPureElim;
		       C.decAtom atm';
		       C.incAtom atm2';
		       F.mkAtom atm2')
		    end
		  | _ => F.mkPure(P.Base Pure.MAKE_SEGDES, [atm'])
	       (* end case *))
	     end
	   | F.ExpPure(P.Base Pure.LENGTHS, [atm]) => let
	       val atm' = substitute (sub, atm)
	     in
	       (case atmKind atm
		 of F.VK_Let(F.ExpPure(P.Base Pure.MAKE_SEGDES, [atm2])) =>  let
		      val atm2' = substitute(sub, atm2)
		    in
		      (ST.tick cntRedundantPureElim;
		       C.decAtom atm';
		       C.incAtom atm2';
		       F.mkAtom atm2')
		    end
		  | _ => F.mkPure(P.Base Pure.LENGTHS, [atm'])
	       (* end case *))
	     end
*)
	   | F.ExpPure(p, atms) => let
	       (*	       val _ = pr "ExpPure" *)
	       val atms' = substitute' (sub, atms)
	     in
	       F.mkPure(p, atms')
	     end
	   | F.ExpCmd(c, atms) => let
(*	       val _ = pr "ExpCmd" *)
	       val atms' = substitute' (sub, atms)
	     in
	       F.mkCmd(c, atms')
	     end
	   | F.ExpIf(b, e1, e2, t) => let
(*	       val _ = pr "ExpIf" *)
	       val b' = substitute(sub, b)
	     in
	       (case b'
		 of (F.Bool true) => (ST.tick cntDeadBranchElim; pr "dead branch elim";
				      doExp(env, sub, e1))
		  | (F.Bool false) => (ST.tick cntDeadBranchElim; pr "dead branch elim";
				       doExp(env, sub, e2))
		  | _ => F.mkIf(b', doExp(env, sub, e1), doExp(env, sub, e2), t)
	       (* end case *))
	     end
	   | F.ExpApplyFun(f, atms) => let
(*	       val _ = pr "ExpApplyFun" *)
	       val atms' = substitute'(sub, atms)
	       val (_, args, exp) = getInst (env, f)
	     in
	       if (appCntOf f = 1 andalso FF.getCanRecurse f)
	       then
		 (* inline it! *)
		 let
		   val F.F{name, ...} = f
		   val _ = pr ("inlining "^(Atom.toString name))
		   (* Combine use counts, then decrement
		    * (because atoms no longer used in function application *)
		   val sub' = addSub' (sub, args, atms')
		   val _ = (ST.tick cntInline; pr "inline")
		   val _ = List.app C.decAtom atms'
		   val _ = C.decAppCnt f
		   val _ = markInlined f
		   val e' = doExp(env, sub', exp)
(* 		   val _ = pr "returning from ExpApplyFun after inlining" *)
		 in
		   e'
		 end
	       else
		  F.mkApplyFun(f, atms')
	     end
	   | F.ExpAtom a => let
	       (* We can substitute an atom with its binding, provided that:
		* -It's bound to a tail expression (so the program doesn't get bigger)
		* -The atom is only used here (so we don't screw up inlining
		* or evaluate the same experssion multiple times)
		* - The expression has no side effects (since we're changing when it's evaluated)
		*)
(*	       val _ = pr "ExpAtom" *)
	       val a' = substitute(sub, a)
	       val canSub = (case a'
			      of F.Var v' => (useCntOf v' = 1)
			       | _ => false
			    (* end case *))
	     in
	       if canSub
	       then
		 (case atmKind a'
		   of (F.VK_Let (e as F.ExpPure(p, atms))) =>
		      (C.decAtom a';
		       List.app C.incAtom (substitute'(sub, atms));
		       F.mkPure(p, (substitute'(sub, atms))))
		    | (F.VK_Let (e as F.ExpApplyFun(f, atms))) =>
		      (* only make this substitution if function has no side effects. *)
			if funSideEffects env f
			  then F.mkAtom a'
			  else (
			    C.decAtom a';
			    C.incAppCnt f;
			    List.app C.incAtom (substitute'(sub, atms));
			    F.mkApplyFun(f, (substitute'(sub, atms))))
		    | (F.VK_Let (e as F.ExpAtom a2)) => (
			C.decAtom a';
			C.incAtom (substitute(sub, a2));
			F.mkAtom(substitute(sub, a2)))
		    | _ => F.mkAtom a'
		 (* end case *))
	       else F.mkAtom a'
	     end
	   | F.ExpLifted (e, vs) => F.mkLifted(doExp(env, sub, e), vs)
	(* end case *))


    fun doTops(env, sub, []) = []
      | doTops(env, sub, F.TopBind(v, e)::tops) = let
	  (* Run contraction *)
	  val _ = pr "doing TopBind\n"  
	  val (sub', bind') = doBindsDown (env, sub, [(v, e)], [])
	  (* Do rest of tree *)
	  val tops' = doTops(env, sub', tops)
(*	  val _ = pr "doing TopBind on upsweep\n" *)
	  val bind' = doBindsUp(env, sub', bind', [])
	in
	  (case bind'
	    of [] => tops' (* Binding was already eliminated *)
	     | [(v', e')] => F.TopBind(v', e')::tops'
	  (* end case *))
	end
      | doTops(env, sub, F.TopExp(e, t, p)::tops) = let
	  val _ = pr "doing TopExp\n" 
	  val e' = doExp(env, sub, e)
	  val top' = F.TopExp(e', t, p)
	  val tops' = doTops(env, sub, tops)
	(* No binding here, so don't need to rerun anything after recursive doTops call. *)
	in
	  top'::tops'
	end
      | doTops(env, sub, F.TopFun(funct, insts)::tops) = let
	  (* Going down, we handle instances in order.
	   * Going back up we handle them in reverse.
	   * Slightly messy because we don't enforce declare-before-use:
	   * Something could be inlined after we've processed it,
	   * so when we mark something to delete, we don't actually delete it
	   * until the next iteration.
	   *)
	  val name = Funct.nameOf funct
	  val _ = pr ("doing TopFun "^(Atom.toString name)^"\n")
	  fun doInstDown [] = []
	    | doInstDown ((f, args, e)::insts) =
	      if (funUnused f orelse getToDelete f)
	      then (if isInlined f
		    then ()
		    else (deleteExp (sub, e); pr "killing dead fxn";
			  ST.tick cntDeadFnElim);
		    doInstDown insts)
	      else (if (appCntOf f = 1)
		    then markToDelete f
		    else ();
		    let
		      val _ = FF.dontRecurse f
		      val e' = doExp(env, sub, e)
		      val _ = FF.doRecurse f
		      val _ = E.rebindInstance(env, funct, (f, args, e'))
		    in
		      (f, args, e')::(doInstDown insts)
		    end)
	  fun doInstUp [] = []
	    | doInstUp ((f, args, e)::insts) =
	      if (funUnused f) (* orelse getToDelete f) *)
	      then (if isInlined f
		    then ()
		    else (deleteExp (sub, e); ST.tick cntDeadFnElim; pr "killing dead fxn after recursion");
		    doInstUp insts)
	      else
		let
		  val _ = FF.dontRecurse f
		  val e' = doExp(env, sub, e)
		  val _ = FF.doRecurse f
		  val _ = E.rebindInstance(env, funct, (f, args, e'))
		in
		  ((f, args, e')::(doInstUp insts))
		end
	  val insts' = doInstDown insts
	  val tops' = doTops(env, sub, tops)
	  val _ = pr ("doing TopFun "^(Atom.toString name)^" on upsweep\n")
	  val insts' = List.rev (doInstUp (List.rev insts'))
	in
	  (case insts'
	    of [] => tops'
	     | _ => (F.TopFun(funct, insts')::tops')
	  (* end case *))
	end

    fun contract(prog, env) = let
      fun sumTicks () = ST.sum {from = firstCounter, to = lastCounter}
      (* Do census pass *)
      val _ = C.census(prog)   
      val F.Program tops = prog
      fun loop (prevTicks, tps) = let
	val _ = ST.tick cntIters
	val _ = pr "doing contract iteration"
      (* Initialize substitution map *)
	val subEmpty = Subst.empty
	val tps = floatTops tps
	val tps = doTops (env, subEmpty, tps)
	val ticks = sumTicks()
	val _ = pr (Int.toString(ticks)^ "ticks")
      in
	if ticks <> prevTicks
	then loop(ticks, tps)
	else tps
      end
      val tops' = loop(sumTicks(), tops)  
(*      val tops' = doTops(env, Subst.empty, tops)   *)
    in
      F.Program tops'
    end

end
