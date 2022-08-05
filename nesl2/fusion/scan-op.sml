(* scan-op.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure ScanOp =
  struct

    datatype t
      = ADD_SCAN of TypeBase.ty		(* +_SCAN {INT, FLOAT} *)
      | MUL_SCAN of TypeBase.ty		(* *_SCAN {INT, FLOAT} *)
      | MAX_SCAN of TypeBase.ty         (* MAX_SCAN {INT, FLOAT} *)
      | MIN_SCAN of TypeBase.ty         (* MIN_SCAN {INT, FLOAT} *)
      | AND_SCAN of TypeBase.ty         (* AND_SCAN {INT, BOOL} *)
      | OR_SCAN of TypeBase.ty          (* OR_SCAN {INT, BOOL} *)
      | XOR_SCAN of TypeBase.ty         (* XOR_SCAN {INT, BOOL} *)

    fun toString opcode = (case opcode
           of ADD_SCAN ty => "ADD_SCAN " ^ TypeBase.baseToString ty
            | MUL_SCAN ty => "MUL_SCAN " ^ TypeBase.baseToString ty
            | MAX_SCAN ty => "MAX_SCAN " ^ TypeBase.baseToString ty
            | MIN_SCAN ty => "MIN_SCAN " ^ TypeBase.baseToString ty
            | AND_SCAN ty => "AND_SCAN " ^ TypeBase.baseToString ty
            | OR_SCAN ty => "OR_SCAN " ^ TypeBase.baseToString ty
            | XOR_SCAN ty => "XOR_SCAN " ^ TypeBase.baseToString ty
	  (* end case *))

    fun baseTy opcode = 
	(case opcode
	  of ADD_SCAN ty => ty
	   | MUL_SCAN ty => ty
	   | MAX_SCAN ty => ty
	   | MIN_SCAN ty => ty
	   | AND_SCAN ty => ty
	   | OR_SCAN ty => ty
	   | XOR_SCAN ty => ty
	(* end case *))

  end
