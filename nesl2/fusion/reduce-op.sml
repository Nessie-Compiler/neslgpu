(* reduce-op.sml
 *
 * COPYRIGHT (c) 2014 The Diderot Project (http://diderot-language.cs.uchicago.edu)
 * All rights reserved.
 *)

structure ReduceOp =
  struct

    datatype t
      = ADD_REDUCE of TypeBase.ty       (* +_REDUCE {INT, FLOAT} *)
      | MUL_REDUCE of TypeBase.ty       (* *_REDUCE {INT, FLOAT} *)
      | MAX_REDUCE of TypeBase.ty       (* MAX_REDUCE {INT, FLOAT} *)
      | MIN_REDUCE of TypeBase.ty       (* MIN_REDUCE {INT, FLOAT} *)
      | AND_REDUCE of TypeBase.ty       (* AND_REDUCE {INT, BOOL} *)
      | OR_REDUCE of TypeBase.ty        (* OR_REDUCE {INT, BOOL} *)
      | XOR_REDUCE of TypeBase.ty       (* XOR_REDUCE {INT, BOOL} *)

    fun toString opcode = (case opcode
           of ADD_REDUCE ty => "+_REDUCE " ^ TypeBase.baseToString ty
            | MUL_REDUCE ty => "*_REDUCE " ^ TypeBase.baseToString ty
            | MAX_REDUCE ty => "MAX_REDUCE " ^ TypeBase.baseToString ty
            | MIN_REDUCE ty => "MIN_REDUCE " ^ TypeBase.baseToString ty
            | AND_REDUCE ty => "AND_REDUCE " ^ TypeBase.baseToString ty
            | OR_REDUCE ty => "OR_REDUCE " ^ TypeBase.baseToString ty
            | XOR_REDUCE ty => "XOR_REDUCE " ^ TypeBase.baseToString ty
	  (* end case *))

    fun baseTy opcode = 
	(case opcode
	  of ADD_REDUCE ty => ty
	   | MUL_REDUCE ty => ty
	   | MAX_REDUCE ty => ty
	   | MIN_REDUCE ty => ty
	   | AND_REDUCE ty => ty
	   | OR_REDUCE ty => ty
	   | XOR_REDUCE ty => ty
	(* end case *))


  end
