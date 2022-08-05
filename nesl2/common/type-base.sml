(* type-base.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Common definitions for types.
 *)

structure TypeBase =
  struct

  (* base types (plus STREAM) *)
    datatype ty = INT | BOOL | FLOAT | CHAR | STREAM | SEGDES

    datatype class
      = TY		(* uber class that includes non-base/data types *)
      | NUM		(* INT or FLOAT *)
      | ORD		(* INT, FLOAT, or CHAR *)
      | LOGIC		(* INT or BOOL *)
      | ANY		(* any base/data type, except STREAM *)

    fun baseToString INT = "int"
      | baseToString BOOL = "bool"
      | baseToString FLOAT = "float"
      | baseToString CHAR = "char"
      | baseToString STREAM = "stream"
      | baseToString SEGDES = "segdes"

    fun classToString TY = "TYPE"
      | classToString NUM = "NUM"
      | classToString ORD = "ORD"
      | classToString LOGIC = "LOGIC"
      | classToString ANY = "ANY"

    fun isScalar INT = true
      | isScalar BOOL = true
      | isScalar FLOAT = true
      | isScalar CHAR = true
      | isScalar _ = false

  end
