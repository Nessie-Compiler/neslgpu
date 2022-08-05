(* gen-op.sml
 *
 * COPYRIGHT (c) 2014 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *)

structure GeneratorOp =
  struct

  (* Generator operations *)
    datatype t
      = INDEX
      | DIST of TypeBase.ty

    fun toString opcode = (case opcode
	   of INDEX => "INDEX"
	    | DIST ty => "DIST "^ TypeBase.baseToString ty
	  (* end case *))

  end


