(* value.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Value =
  struct

    datatype value
      = B of bool
      | C of string
      | I of int
      | F of real
      | Seq of value vector
      | Tuple of value list

    fun toString (B b) = Bool.toString b
      | toString (C c) = String.concat["\\", Char.toCString c]
      | toString (I n) = Int.toString n
      | toString (F r) = Real.toString r
      | toString (Seq vs) = "<seq>"
      | toString (Tuple vs) = String.concat["<", String.concatWith ", " (List.map toString vs), ">"]

  end
