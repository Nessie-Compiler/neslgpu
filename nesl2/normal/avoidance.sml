(* avoidance.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * The Vectorization Avoidance analysis from
 *
 *      Vectorization Avoidance
 *      by Gabriele Keller, Manuel Chakravarty, Roman Leshchinskiy,
 *      Ben Lippmeier, and Simon Peyton Jones
 *      Haskell Symposium, Copenhagen, 2012
 *)

structure Avoidance : sig

    val analyze : NormalAST.program -> unit

  end = struct

    structure NTy = NormalTy

  (* labels: P for parallel, S for scalar, and C for non-scalar types (i.e., tuples) *)
    datatype label = P | S | C

    infix |>

    fun t |> P = P
      | t |> _ = t

    fun labelFold labFn init items =
          List.foldl (fn (item, lab) => lab |> labFn item) init items

  (* a property for labeling expressions.  Note that for let bindings, we
   * label the expression based on the binding (not the body of the let).
   *)
    local
      val {setFn, getFn, ...} = PropList.newProp (fn h => h, fn _ => C)
    in
    fun setLabel (props, lab) = (setFn(props, lab); lab)
    fun labelOfExp (N.Exp(props, _)) = getFn props
    end

  (* label types *)
    fun labelTy (NTy.TyBase TypeBase.STREAM) = C
      | labelTy (NTy.TyBase _) = S
      | labelTy (NTy.TySeq _) = P
      | labelTy (NTy.TyBaseSeq _) = P
      | labelTy (NTy.TyTuple tys) = labelFold labelTy C tys
      | labelTy (NTy.TyFun _) = raise Fail "unexepected TyFun"

  (* label atoms *)
    fun labelAtom (Var x) = if isParVar x then P else labelTy(NVar.typeOf x)
      | labelAtom (String s) = P (* perhaps this should be C? *)
      | labelAtom _ = S

  (* label expressions *)
    fun labelExp (N.Exp(props, exp)) = (case exp
           of N.ExpLet((xs, e1), e2) => let
                val lab1 = labelExp e1
                in
                  if (lab1 = P) andalso ((* typeof xs is Scalar *))
                    then (
                      (* mark the xs as parallel *)
                      ignore (labelExp e2);
                      setLabel(props, P))
                    else setLabel(props, labelTy(??) |> lab1 |> labelExp e2)
                end
            | N.ExpTuple(x, ats, e) =>
	    | N.ExpLetVector(seg, v, x, e) =>
	    | N.ExpVector(x, seg, v, e) =>
            | N.ExpSeq(x, ats, e) =>
            | N.ExpSeqRange(x, at1, at2, at3, e) =>
            | N.ExpPure(x, opcode, ats, e) => (
		if Pure.isParallel opcode
		  then setLabel(props, P)
		  else setLabel(props, S);
		labelExp e)
            | N.ExpCmd(x, _, ats, e) => (* commands can never be parallel *)
            | N.ExpApply(f, ats) =>
                setLabel (props, labelFold labelAtom (labelOfFun f) ats)
            | N.ExpForEach(e, binds) => (
                ignore (labelExp e);
                setLabel (props, P))
            | N.ExpIf(at, e1, e2, ty) =>
                setLabel (props, labelTy ty |> labelAtom at |> labelExp e1 |> labelExp e2)
            | N.ExpAtom at => setLabel(props, labelAtom at)
          (* end case *))

  end
