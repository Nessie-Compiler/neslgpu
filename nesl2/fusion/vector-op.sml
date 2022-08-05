(* vector-op.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure VectorOp =
  struct

  (* vector operations *)
    datatype t
      = PERMUTE of TypeBase.ty		(* PERMUTE {INT, BOOL, FLOAT} *)
      | DPERMUTE of TypeBase.ty		(* DPERMUTE {INT, BOOL, FLOAT} *)
      | FPERMUTE of TypeBase.ty		(* FPERMUTE {INT, BOOL, FLOAT} *)
      | BPERMUTE of TypeBase.ty		(* BPERMUTE {INT, BOOL, FLOAT} *)
      | BFPERMUTE of TypeBase.ty	(* BFPERMUTE {INT, BOOL, FLOAT} *)
      | DFPERMUTE of TypeBase.ty	(* DFPERMUTE {INT, BOOL, FLOAT} *)
      | EXTRACT of TypeBase.ty		(* EXTRACT {INT, BOOL, FLOAT} *)
      | REPLACE of TypeBase.ty		(* REPLACE {INT, BOOL, FLOAT} *)
      | PACK of TypeBase.ty		(* PACK {INT, BOOL, FLOAT} *)
      | RANK_UP of TypeBase.ty		(* RANK_UP {INT, FLOAT} *)
      | RANK_DOWN of TypeBase.ty	(* RANK_DOWN {INT, FLOAT} *)

    fun toString opcode = (case opcode
           of PERMUTE ty => "PERMUTE " ^ TypeBase.baseToString ty
            | DPERMUTE ty => "DPERMUTE " ^ TypeBase.baseToString ty
            | FPERMUTE ty => "FPERMUTE " ^ TypeBase.baseToString ty
            | BPERMUTE ty => "BPERMUTE " ^ TypeBase.baseToString ty
            | BFPERMUTE ty => "BFPERMUTE " ^ TypeBase.baseToString ty
            | DFPERMUTE ty => "DFPERMUTE " ^ TypeBase.baseToString ty
            | EXTRACT ty => "EXTRACT " ^ TypeBase.baseToString ty
            | REPLACE ty => "REPLACE " ^ TypeBase.baseToString ty
	    | PACK ty => "PACK " ^ TypeBase.baseToString ty
            | RANK_UP ty => "RANK_UP " ^ TypeBase.baseToString ty
            | RANK_DOWN ty => "RANK_DOWN " ^ TypeBase.baseToString ty
	  (* end case *))

  end
