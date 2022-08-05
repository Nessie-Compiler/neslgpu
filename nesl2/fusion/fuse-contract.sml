(* fuse-contract.sml
 *
 * COPYRIGHT (c) 2014 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *
 * Contraction phase for the FuseAST representation.
 * The primary goals of contraction for this phase is to eliminate
 * unnecessary segment descriptors.
 * We eliminate dead kernels variables,
 * do constant folding for tuple construction/selection,
 * and use a shape -> variable map to eliminate redundant segment descriptors.
 * We do not inline functions or eliminate dead functions.
 * TODO:
 * - tuple reconstruction (if we need it?)
 * - dead function argument elimination
 *    general idea: one pass over top-levels to change arglist and fxn type
 *        (which means function types should be refs)
 *        and to build up a fxn -> dead arg position map
 *        which we use to update fxn call sites when we encounter them.
 * - maybe let floating, if we need it
 *)

structure FuseContract : sig

  val contract : FuseAST.program -> FuseAST.program

  end = struct

    structure F = FuseAST
    structure FF = FuseFunct
    structure FS = FuseShapes
    structure V = FuseVar
    structure C = FuseCensus
    structure ST = Stats
    structure IOp = InternalOp

    (******* Flags *******)
    val {getFn=getCanRecurse, setFn=setCanRecurse, ...} = FF.newProp (fn _ => true)

    (* per-function property holding its binding *)
    val {getFn=getBinding, setFn=setBinding, ...} = let
      fun init f : (F.funct * F.var list * F.exp) = raise Fail "function binding not set"
    in
      FF.newProp init
    end

(*    val {getFn=hasSE, setFn=setHasSE, peekFn=peekSE, ...} = FF.newFlag () *)

    (******* Counters *******)
    val cntLetFloat		  = ST.newCounter "fuse-contract:let-float"
    val cntUnusedVarElim          = ST.newCounter "fuse-contract:unused-var-elim"
    val cntRedundantVarElim       = ST.newCounter "fuse-contract:redundant-var-elim"
    val cntDeadKernelElim         = ST.newCounter "fuse-contract:dead-kernel-elim"
    val cntInternalFold		  = ST.newCounter "fuse-contract:internal-fold"
    val cntProjFold		  = ST.newCounter "fuse-contract:proj-fold"
    val cntTupleFold              = ST.newCounter "fuse-contract:tuple-fold"
(*
    val cntDeadFnElim             = ST.newCounter "contract:dead-fn-elim"
    val cntDeadBranchElim         = ST.newCounter "contract:dead-branch-elim"
*)
    val firstCounter              = cntLetFloat
    val lastCounter               = cntTupleFold
    val cntIters                  = ST.newCounter "fuse-contract:iterations"

    (******* Effect Analysis *******)

    (* Because functions are not declare-before-use,
     * we use a hash table from functs to function bodies
     * to run effect analysis on functions as needed.
     *)
    fun hasSideEffects exp = (case exp
	   of F.Exp_Let (vs, e1, e2) => hasSideEffects e1 orelse hasSideEffects e2
	    | F.Exp_RHS (v, F.RHS_Cmd _, e) => true
	    | F.Exp_RHS (v, rhs, e) => hasSideEffects e
	    | F.Exp_If (a, e1, e2) => hasSideEffects e1 orelse hasSideEffects e2
	    | F.Exp_Apply (f, atms) => funSideEffects f
	    | F.Exp_Atom a => false
	  (* end case *))

    and funSideEffects f =
	if getCanRecurse f
	then let
	  val _ = setCanRecurse(f, false)
	  val (_, _, body) = getBinding f
	  val hasFx = hasSideEffects body
	  (*  val _ = setHasSE(f, hasFx) *)
	  in
	    (setCanRecurse(f, true); hasFx)
	  end
	else false

  (******* Variable substitution *******)
    structure Subst = FuseVar.Map

    fun substitute(sub, F.Var v) =
	(case Subst.find(sub, v)
	  of SOME a => a
	   | NONE => F.Var v
	(* end case *))
      | substitute (sub, a) = a

    fun substitute' (sub, atms) = map (fn a => substitute(sub, a)) atms

    fun subVar (sub, v) =
	(case Subst.find(sub, v)
	  of SOME (F.Var v') => v'
	   | NONE => v
	(* end case *))

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

    (******* Shape substitution *******)
    (* When we encounter a segment descriptor,
     * we look up its shape in the shape map.
     * If it's already there, we can
     * add a variable substitution
     * from the current variable to the one with the same shape.
     *)
    fun shapeSub (shapeMap, sub, var) =
	(case V.typeOf var
	  of F.TySegdes =>
	     let
	       val shape = V.getShape var
	     in
	       (case FS.Map.find(shapeMap, shape)
		 of SOME var' => let
		      val sub' = addSub(sub, var, F.Var var')
		    in
		      ((* print ("substitute "^(FuseVar.toString var')^" for identical segdes "^(FuseVar.toString var)^"\n"); *)
		       C.clrUseCnt var;
		       (shapeMap, sub'))
		    end
		  | NONE => ((*print ("adding new segdes to shapeMap: "^(FuseVar.toString var)^"\n"); *)
			     (FS.Map.insert (shapeMap, shape, var), sub))
	       (* end case *))
	     end
	   | _ => (shapeMap, sub)
	(* end case *))

    fun shapeSub' (shapeMap, sub, []) = (shapeMap, sub)
      | shapeSub' (shapeMap, sub, v::vs) = let
	  val (shapeMap', sub') = shapeSub(shapeMap, sub, v)
	in
	  shapeSub'(shapeMap', sub', vs)
	end

    (******* Some handy functions *******)

    fun atmKind (F.Var v) = V.binding v
      | atmKind _ = F.VB_None

    fun deleteExp (sub, exp) = let
      fun subFn e = substitute(sub, e)
    in
      C.deleteExp(subFn, exp)
    end

    fun deleteRHS (sub, rhs) = let
      fun subFn e = substitute(sub, e)
    in
      C.deleteRHS(subFn, rhs)
    end

    fun unused v = (V.useCnt v = 0)
    fun unusedF f = (FF.appCnt f = 0)

    (******* Contract *******)

  (* the result of contracting a RHS *)
    datatype new_rhs = NO_CHANGE | DELETE of F.atom Subst.map

    fun doExp (shapeMap, sub, exp) = (case exp
	   of F.Exp_Let(lhs, F.Exp_Let(lhs', e1, e2), e3) => (
		ST.tick cntLetFloat;
		doExp (shapeMap, sub, F.mkLet(lhs', e1, F.mkLet(lhs, e2, e3))))
	    | F.Exp_Let(lhs, F.Exp_RHS(lhs', rhs, e1), e2) => (
		ST.tick cntLetFloat;
		doExp (shapeMap, sub, F.mkRHS(lhs', rhs, F.mkLet(lhs, e1, e2))))
	    | F.Exp_Let([var], F.Exp_Atom a, e) => (
	        ST.tick cntRedundantVarElim;
	        C.decAtom (substitute(sub, a));
	        doExp(shapeMap, addSub(sub, var, a), e))
	    | F.Exp_Let(vars, e1, e2) => if not(hasSideEffects e1) andalso List.all unused vars
		then (
		  ST.tick cntUnusedVarElim;
		  deleteExp (sub, e1); e2)
	        else let
		  val e1' = doExp(shapeMap, sub, e1)
		  val (shapeMap', sub') = shapeSub' (shapeMap, sub, vars)
		  val e2' = doExp(shapeMap', sub', e2)
		  in
		    if List.all unused vars
		      then (
			ST.tick cntUnusedVarElim;
		        deleteExp (sub, e1'); e2')
		      else F.mkLet(vars, e1', e2')
	          end
	    | F.Exp_RHS(vars, rhs, e) => let
		val (shapeMap', sub') = shapeSub' (shapeMap, sub, vars)
		val rhs' = subRHS(sub', rhs)
		in
		  case doRHS(sub', vars, rhs')
		   of NO_CHANGE => F.mkRHS(vars, rhs', doExp(shapeMap', sub', e))
		    | DELETE sub' => doExp(shapeMap', sub', e)
		  (* end case *)
		end
	    | F.Exp_If(a, e1, e2) =>
		F.mkIf(
		   substitute(sub, a),
		   doExp(shapeMap, sub, e1),
		   doExp(shapeMap, sub, e2))
	    | F.Exp_Apply(f, atms) => F.mkApply(f, substitute'(sub, atms))
	    | F.Exp_Atom a => F.mkAtom (substitute(sub, a))
	  (* end case *))

  (* contract a RHS binding.  We assume that the lhs and rhs variables have been processed
   * already.
   *)
    and doRHS (sub, lhs, rhs) = (case (rhs, List.all unused lhs)
	   of (F.RHS_Cmd _, _) => NO_CHANGE
	    | (_, true) => (
		ST.tick cntUnusedVarElim;
		deleteRHS (sub, rhs);
		DELETE sub)
	    | (F.RHS_Internal(rator, x), _) => let
		fun reduce y = (
		      ST.tick cntInternalFold;
		      C.decAtom x;
		      DELETE(addSub(sub, hd lhs, y)))
		in
		  case x
		   of F.Var x => (case V.binding x
			 of F.VB_RHS(_, _, F.RHS_Internal(rator', y)) => (case (rator, rator')
			       of (IOp.MAKE_SEGDES, IOp.LENGTHS) => reduce y
				| (IOp.LENGTHS, IOp.MAKE_SEGDES) => reduce y
				| (IOp.SCALAR_TO_SEQ _, IOp.SEQ_TO_SCALAR _) => reduce y
				| (IOp.SEQ_TO_SCALAR _, IOp.SCALAR_TO_SEQ _) => reduce y
				| _ => NO_CHANGE
			      (* end case *))
			  | _ => NO_CHANGE
			(* end case *))
		    | _ => NO_CHANGE
		  (* end case *)
		end
	    | (F.RHS_Proj(i, v), false) => (case (lhs, V.binding v)
		 of ([var], F.VB_RHS(1, 1, F.RHS_Tuple atms)) => (
		      ST.tick cntProjFold;
		      C.decUseCnt v;
		      DELETE(addSub(sub, var, List.nth(atms, i - 1))))
		  | _ => NO_CHANGE
		(* end case *))
	    | _ => NO_CHANGE
	 (* end case *))

  (* Update all atoms and variables used in RHS from substitution map *)
    and subRHS (sub, F.RHS_Scalar(s, atms)) = F.mkScalar(s, substitute'(sub, atms))
      | subRHS (sub, F.RHS_FlatGen(gen, wid, atms)) = F.mkFlatGen(gen, substitute(sub, wid), substitute'(sub, atms))
      | subRHS (sub, F.RHS_SegGen(gen, wid, vars)) = F.mkSegGen(gen, substitute(sub, wid), map (fn v => subVar(sub, v)) vars)
      | subRHS (sub, F.RHS_Map(kern, wid, vars)) = F.mkMap(kern, substitute(sub, wid), map (fn v => subVar(sub, v)) vars)
      | subRHS (sub, F.RHS_FlatReduce(r, wid, v)) = F.mkFlatReduce(r, substitute(sub, wid), subVar(sub, v))
      | subRHS (sub, F.RHS_SegReduce(r, wid, v1, v2)) = F.mkSegReduce(r, substitute(sub, wid), subVar(sub, v1), subVar(sub, v2))
      | subRHS (sub, F.RHS_FlatScan(s, wid, v)) = F.mkFlatScan(s, substitute(sub, wid), subVar(sub, v))
      | subRHS (sub, F.RHS_SegScan(s, wid, v1, v2)) = F.mkSegScan(s, substitute(sub, wid), subVar(sub, v1), subVar(sub, v2))
      | subRHS (sub, F.RHS_FlatVector(vec, atms)) = F.mkFlatVector(vec, substitute'(sub, atms))
      | subRHS (sub, F.RHS_SegVector(vec, atms)) = F.mkSegVector(vec, substitute'(sub, atms))
      | subRHS (sub, F.RHS_Internal(rator, atm)) = F.mkInternal(rator, substitute(sub, atm))
      | subRHS (sub, F.RHS_Cmd(c, atms)) = F.mkCmd(c, substitute'(sub, atms))
      | subRHS (sub, F.RHS_Seq(atms, t)) = F.mkSeq(substitute'(sub, atms), t)
      | subRHS (sub, F.RHS_Tuple atms) = F.mkTuple(substitute'(sub, atms))
      | subRHS (sub, F.RHS_Proj(i, v)) = F.mkProj(i, subVar(sub, v))

  (* We need to create our table of instances to use in effect analysis *)
    fun bindTopFun (F.Top_Funct(f, vs, e)) = setBinding(f, (f, vs, e))
      | bindTopFun _ = ()

    fun doTops (shapeMap, sub, []) = []
      | doTops (shapeMap, sub, (F.Top_Kern k)::tops) =
	  if (Kernel.useCnt k = 0)
	    then doTops(shapeMap, sub, tops)
	    else (F.Top_Kern k)::(doTops(shapeMap, sub, tops))
      | doTops (shapeMap, sub, F.Top_Funct(f, vs, e)::tops) = let
	  val (shapeMap', sub') = shapeSub' (shapeMap, sub, vs)
	  val e' = doExp (shapeMap', sub', e)
	  val _ = setBinding(f, (f, vs, e'))
	  in
	    F.mkTopFun(f, vs, e') :: doTops(shapeMap, sub, tops)
	  end
      | doTops (shapeMap, sub, F.Top_Let(vs, F.Exp_Let(vs', e1, e2))::tops) = (
	  ST.tick cntLetFloat;
	  doTops (shapeMap, sub, F.mkTopLet(vs', e1) :: F.mkTopLet(vs, e2) :: tops))
      | doTops (shapeMap, sub, F.Top_Let(vs, F.Exp_RHS(vs', rhs, e1))::tops) = (
	  ST.tick cntLetFloat;
	  doTops (shapeMap, sub, F.mkTopRHS(vs', rhs) :: F.mkTopLet(vs, e1) :: tops))
      | doTops (shapeMap, sub, F.Top_Let([x], F.Exp_Atom a) :: tops) = let
	  val a' = substitute(sub, a)
	  in
	    ST.tick cntRedundantVarElim;
	    C.decAtom a';
	    doTops (shapeMap, addSub(sub, x, a'), tops)
	  end
      | doTops (shapeMap, sub, F.Top_Let(vs, e)::tops) =
	  if List.all unused vs andalso not(hasSideEffects e)
	    then (
	      ST.tick cntUnusedVarElim;
	      deleteExp(sub, e);
	      doTops(shapeMap, sub, tops))
	    else let
	      val e' = doExp (shapeMap, sub, e)
	      val (shapeMap', sub') = shapeSub' (shapeMap, sub, vs)
	      val tops' = doTops(shapeMap', sub', tops)
	      in
		if not(hasSideEffects e') andalso List.all unused vs
		  then (
		    ST.tick cntUnusedVarElim;
		    deleteExp(sub, e');
		    tops')
		  else F.mkTopLet(vs, e')::tops'
	      end
      | doTops (shapeMap, sub, F.Top_RHS(vs, rhs)::tops) = let
	  val (shapeMap', sub') = shapeSub' (shapeMap, sub, vs)
	  val rhs' = subRHS(sub', rhs)
	  in
	    case doRHS(sub', vs, rhs')
	     of NO_CHANGE => F.mkTopRHS(vs, rhs') :: doTops (shapeMap', sub', tops)
	      | DELETE sub' =>  doTops (shapeMap', sub', tops)
	    (* end case *)
	  end
      | doTops (shapeMap, sub, F.Top_Exp(F.Exp_Let(xs, e1, e2), ty)::tops) = (
	  ST.tick cntLetFloat;
	  doTops (shapeMap, sub, F.mkTopLet(xs, e1) :: F.mkTopExp(e2, ty) :: tops))
      | doTops (shapeMap, sub, F.Top_Exp(F.Exp_RHS(xs, rhs, e1), ty)::tops) = (
	  ST.tick cntLetFloat;
	  doTops (shapeMap, sub, F.mkTopRHS(xs, rhs) :: F.mkTopExp(e1, ty) :: tops))
      | doTops (shapeMap, sub, F.Top_Exp(e, ty) :: tops) = let
	  val top' = F.mkTopExp(doExp (shapeMap, sub, e), ty)
	  in
	    top' :: doTops(shapeMap, sub, tops)
	  end

    fun contract (prog as F.Program tops) = let
	  fun sumTicks () = ST.sum {from = firstCounter, to = lastCounter}
	(* Create table of function bindings *)
	  val _ = List.app bindTopFun tops
	(* Do census pass *)
	  val _ = C.census(prog)
	  fun loop (prevTicks, tops) = let
		val _ = ST.tick cntIters
		val subEmpty = Subst.empty
		val shapeEmpty = FS.Map.empty
		val tops = doTops(shapeEmpty, subEmpty, tops)
		val ticks = sumTicks()
		in
		  if ticks <> prevTicks
		    then loop(ticks, tops)
		    else tops
		end
	  in
	    F.Program(loop(sumTicks(), tops))
	  end

end


(* First, create table of instances.
 * then run funSideEffects on every function def
 * then do other stuff
 *)
