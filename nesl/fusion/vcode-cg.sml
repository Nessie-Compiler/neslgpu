(* vcode-cg.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Call graph for VCode representation.
 *)

structure VCodeCG = CallGraphFn (
  struct
    type func = VCode.func
    fun labelOf (VCode.FUNC(lab, _)) = lab
    fun appCalls doCall (VCode.FUNC(_, code)) = let
	  fun f (VCode.CALL lab) = doCall lab
	    | f (VCode.IF(b1, b2)) = (List.app f b1; List.app f b2)
	    | f _ = ()
	  in
	    List.app f code
	  end
  end)
