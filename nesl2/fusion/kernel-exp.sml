(* kernel-exp.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure KernelExp : sig

    val census : FuseAST.var list * FuseAST.kern_exp -> unit

    val contract : FuseAST.kern_exp -> FuseAST.kern_exp

  end = struct

    structure F = FuseAST
    structure V = FuseVar
    structure VMap = V.Map
    structure ST = Stats

    fun clrUseCnt (F.V{useCnt, ...}) = useCnt := 0

    fun incAtom (F.Var v) = V.incCnt v
      | incAtom _ = ()

    fun decAtom (F.Var v) = V.decCnt v
      | decAtom _ = ()

    fun delete (sub, kexp) = let
	  fun del kexp = (case kexp
		 of F.KExp_Let(vs, e1, e2) => (del e1; del e2)
		  | F.KExp_Pure(x, p, atms, e) => (List.app (decAtom o sub) atms; del e)
		  | F.KExp_Proj(v1, i, v2, e) => (decAtom (sub (F.Var v2)); del e)
		  | F.KExp_Tuple(v, atms, e) => (List.app (decAtom o sub) atms; del e)
		  | F.KExp_If(b, e1, e2) => (decAtom (sub b); del e1; del e2)
		  | F.KExp_Return vs => List.app decAtom vs
		(* end case *))
	      in
		del kexp
	      end

    fun census (params, body) = let
	  fun count e = (case e
		 of F.KExp_Let(vs, e1, e2) => (List.app clrUseCnt vs; count e1; count e2)
		  | F.KExp_Pure(x, s, atms, e) => (clrUseCnt x; List.app incAtom atms; count e)
		  | F.KExp_Proj(x, i, v2, e) => (clrUseCnt x; V.incCnt v2; count e)
		  | F.KExp_Tuple(x, atms, e) => (clrUseCnt x; List.app incAtom atms; count e)
		  | F.KExp_If(a, e1, e2) => (incAtom a; count e1; count e2)
		  | F.KExp_Return vs => List.app incAtom vs
		(* end case *))
	  in
	    List.app clrUseCnt params;
	    count body
	  end

  (******* Counters *******)
    val cntUnusedVarElim	= ST.newCounter "kcontract:unused-var-elim"
    val cntLetFloat		= ST.newCounter "kcontract:let-float"
    val cntLetRename		= ST.newCounter "kcontract:let-rename"
    val cntProjFold		= ST.newCounter "kcontract:proj-fold"
    val cntBranchElim		= ST.newCounter "kcontract:branch-elim"
    val firstCounter		= cntUnusedVarElim
    val lastCounter		= cntBranchElim
    val cntIters		= ST.newCounter "kcontract:iterations"

    fun rename env x = (case VMap.find (env, x)
	   of SOME x' => x'
	    | NONE => F.Var x
	  (* end case *))

    fun renameAtom env (F.Var x) = rename env x
      | renameAtom _ atm = atm

    fun extend (env, x, atm as F.Var(F.V{useCnt, ...})) = (
	(* add use count of x to the variable, since all uses of x will be replaced by y *)
	  useCnt := !useCnt + V.useCnt x;
	  VMap.insert (env, x, atm))
      | extend (env, x, atm) = VMap.insert (env, x, atm)

    fun doExp (env, e) = (case e
	   of F.KExp_Let(xs, F.KExp_Let(ys, e1, e2), e3) => (
		ST.tick cntLetFloat;
		doExp (env, F.mkKLet(ys, e1, F.mkKLet(xs, e2, e3))))
	    | F.KExp_Let(xs, F.KExp_Pure(y, s, atms, e1), e2) => (
		ST.tick cntLetFloat;
		doExp (env, F.mkKPure(y, s, atms, F.mkKLet(xs, e1, e2))))
	    | F.KExp_Let(xs, F.KExp_Proj(y, i, v, e1), e2) => (
		ST.tick cntLetFloat;
		doExp (env, F.mkKProj(y, i, v, F.mkKLet(xs, e1, e2))))
	    | F.KExp_Let(xs, F.KExp_Tuple(y, atms, e1), e2) => (
		ST.tick cntLetFloat;
		doExp (env, F.mkKTuple(y, atms, F.mkKLet(xs, e1, e2))))
	    | F.KExp_Let(xs, F.KExp_Return atms, e) => let
		val env = ListPair.foldlEq (fn (x, y, env) => extend(env, x, y)) env (xs, atms)
		in
		  ST.tick cntLetRename;
		  doExp (env, e)
		end
	    | F.KExp_Let(xs, e1, e2) => let
		fun unused () = if List.all (fn x => V.useCnt x = 0) xs
		      then (
			ST.tick cntUnusedVarElim;
			delete (renameAtom env, e1);
			true)
		      else false
		in
		  if unused()
		    then doExp(env, e2)
		    else let
		      val e2' = doExp(env, e2)
		      in
			if unused()
			  then e2'
			  else F.mkKLet(xs, doExp(env, e1), e2')
		      end
		end
	    | F.KExp_Pure(x, s, atms, e) => let
		val atms = List.map (renameAtom env) atms
		fun unused () = if V.useCnt x = 0
		      then (
			ST.tick cntUnusedVarElim;
			List.app decAtom atms;
			true)
		      else false
		in
		  if unused()
		    then doExp(env, e)
		    else let
		      val e' = doExp(env, e)
		      in
			if unused()
			  then e'
			  else F.mkKPure(x, s, atms, e')
		      end
		end
	    | F.KExp_Proj(x, i, v, e) => let
		val F.Var v = rename env v
		fun unused () = if V.useCnt x = 0
		      then (
			ST.tick cntUnusedVarElim;
			V.decCnt v;
			true)
		      else false
		in
		  if unused()
		    then doExp(env, e)
		    else (case V.binding v
		       of F.VB_KTuple atms => let
			    val y = renameAtom env (List.nth(atms, i-1))
			    in
			      ST.tick cntProjFold;
			      V.decCnt v;
			      doExp (extend(env, x, y), e)
			    end
			| _ => let
			    val e' = doExp(env, e)
			    in
			      if unused()
				then e'
				else F.mkKProj(x, i, v, e')
			    end
		      (* end case *))
		end
	    | F.KExp_Tuple(x, atms, e) => let
		val atms = List.map (renameAtom env) atms
		fun unused () = if V.useCnt x = 0
		      then (
			ST.tick cntUnusedVarElim;
			List.app decAtom atms;
			true)
		      else false
		in
		  if unused()
		    then doExp(env, e)
		    else let
		      val e' = doExp(env, e)
		      in
			if unused()
			  then e'
			  else F.mkKTuple(x, atms, e')
		      end
		end
	    | F.KExp_If(a, e1, e2) => (case renameAtom env a
		 of F.Bool true => (
		      ST.tick cntBranchElim;
		      delete (renameAtom env, e2);
		      doExp (env, e1))
		  | F.Bool false => (
		      ST.tick cntBranchElim;
		      delete (renameAtom env, e1);
		      doExp (env, e2))
		  | a' => F.mkKIf(a', doExp(env, e1), doExp(env, e2))
		(* end case *))
	    | F.KExp_Return atms => F.mkKReturn(List.map (renameAtom env) atms)
	  (* end case *))

    fun contract e = let
	  fun sumTicks () = ST.sum {from = firstCounter, to = lastCounter}
	  fun lp (prevTicks, e) = let
		val e = doExp (VMap.empty, e)
		val ticks = sumTicks()
		in
		  ST.tick cntIters;
		  if (prevTicks < ticks)
		    then lp (ticks, e)
		    else e
		end
	  in
	    lp (sumTicks (), e)
	  end

  end
