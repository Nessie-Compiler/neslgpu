(* internal-op.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure InternalOp =
  struct

  (* Internal operations that cannot be lifted. *)
    datatype t
      = MAKE_SEGDES
      | LENGTHS
      | SCALAR_TO_SEQ of TypeBase.ty
      | SEQ_TO_SCALAR of TypeBase.ty
      | LENGTH of TypeBase.ty           (* LENGTH {INT, BOOL, FLOAT} *)
      | SUM_LENGTHS			(* sum of lengths in a segment descriptor *)

    fun toString opcode = (case opcode
	   of MAKE_SEGDES => "MAKE_SEGDES"
	    | LENGTHS => "LENGTHS"
	    | SCALAR_TO_SEQ ty => "SCALAR_TO_SEQ " ^ TypeBase.baseToString ty
	    | SEQ_TO_SCALAR ty => "SEQ_TO_SCALAR " ^ TypeBase.baseToString ty
	    | LENGTH ty => "LENGTH " ^ TypeBase.baseToString ty
	    | SUM_LENGTHS => "SUM_LENGTHS"
	  (* end case *))

  end
