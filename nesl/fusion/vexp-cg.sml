(* vexp-cg.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Call graph for VExp representation.
 *)

structure VExpCG = CallGraphFn (
  struct
    type func = VExp.func
    fun labelOf (VExp.FUNC(lab, _, _, _)) = lab
    fun appCalls doCall (VExp.FUNC(_, _, _, body)) = let
	  fun f (VExp.LET(_, _, stm)) = f stm
	    | f (VExp.LETPACK(_, _, _, _, stm)) = f stm
	    | f (VExp.CALL(_, lab, _, stm)) = (doCall lab; f stm)
	    | f (VExp.STMT(_, _, _, stm)) = f stm
	    | f (VExp.IF(_, _, thenStm, elseStm, stm)) = (f thenStm; f elseStm; f stm)
	    | f (VExp.RET _) = ()
	    | f VExp.EXIT = ()
	  in
	    f body
	  end
  end)
