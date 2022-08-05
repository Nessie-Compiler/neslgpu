(* prim.sml
 *
 * COPYRIGHT (c) 2013 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *
 * NESL pure ops.
 *)

structure FlanPure =

  struct

  datatype pure = 
	   Base of Pure.pure
	 | Lifted of Pure.pure

  structure T = FlanTypes

    fun toString (Base p) = Pure.toString p
      | toString (Lifted p) = (Pure.toString p)^"^"

    fun hash pure = Atom.hash (Atom.atom (Pure.toString pure))


    fun getTy (Base (Pure.PROJ i)) = raise Fail "PROJ"
      | getTy (Lifted (Pure.PROJ i)) = raise Fail "PROJ^"
      | getTy (Base p) = FlanPrimTy.typeOfPure p
      | getTy (Lifted p) = let
(*
	  fun raiseTy (T.TyBase TypeBase.SEGDES) = T.TyBase TypeBase.SEGDES
	    | raiseTy (T.TyBase tb) = T.TyBaseSeq tb
	    | raiseTy (T.TyBaseSeq tb) = T.TyBaseSeq tb
	    | raiseTy (T.TyTuple ts) = T.TyTuple(map raiseTy ts)
*)
	  val (baseDom, baseRng) = FlanPrimTy.typeOfPure p
	  val rng = FlanTypes.tySeq baseRng
	  val dom = map FlanTypes.tySeq baseDom
	in
	  (dom, rng) 
(*	  (raiseTy baseDom, raiseTy baseRng) *)
	end
	  
    fun sameTy (TypeBase.INT, TypeBase.INT) = true
      | sameTy (TypeBase.BOOL, TypeBase.BOOL) = true
      | sameTy (TypeBase.FLOAT, TypeBase.FLOAT) = true
      | sameTy (TypeBase.CHAR, TypeBase.CHAR) = true
      | sameTy (TypeBase.STREAM, TypeBase.STREAM) = true
      | sameTy (TypeBase.SEGDES, TypeBase.SEGDES) = true
      | sameTy (_, _) = false

    fun same (Pure.ADD t1, Pure.ADD t2) = sameTy (t1, t2) 
      | same (Pure.SUB t1, Pure.SUB t2) = sameTy (t1, t2)
      | same (Pure.MUL t1, Pure.MUL t2) = sameTy (t1, t2)
      | same (Pure.DIV t1, Pure.DIV t2) = sameTy (t1, t2)
      | same (Pure.MOD, Pure.MOD) = true
      | same (Pure.LT t1, Pure.LT t2) = sameTy (t1, t2)
      | same (Pure.LTE t1, Pure.LTE t2) = sameTy (t1, t2)
      | same (Pure.GT t1, Pure.GT t2) = sameTy (t1, t2)
      | same (Pure.GTE t1, Pure.GTE t2) = sameTy (t1, t2)
      | same (Pure.EQ t1, Pure.EQ t2) = sameTy (t1, t2)
      | same (Pure.NEQ t1, Pure.NEQ t2) = sameTy (t1, t2)
      | same (Pure.LSHIFT, Pure.LSHIFT) = true
      | same (Pure.RSHIFT, Pure.RSHIFT) = true
      | same (Pure.NOT t1, Pure.NOT t2) = sameTy (t1, t2)
      | same (Pure.AND t1, Pure.AND t2) = sameTy (t1, t2)
      | same (Pure.OR t1, Pure.OR t2) = sameTy (t1, t2)
      | same (Pure.XOR t1, Pure.XOR t2) = sameTy (t1, t2)
      | same (Pure.SELECT t1, Pure.SELECT t2) = sameTy (t1, t2)
      | same (Pure.RAND, Pure.RAND) = true
      | same (Pure.FLOOR, Pure.FLOOR) = true
      | same (Pure.CEIL, Pure.CEIL) = true
      | same (Pure.TRUNC, Pure.TRUNC) = true
      | same (Pure.ROUND, Pure.ROUND) = true
      | same (Pure.I_TO_F, Pure.I_TO_F) = true
      | same (Pure.I_TO_B, Pure.I_TO_B) = true
      | same (Pure.B_TO_I, Pure.B_TO_I) = true
      | same (Pure.LOG, Pure.LOG) = true
      | same (Pure.SQRT, Pure.SQRT) = true
      | same (Pure.EXP, Pure.EXP) = true
      | same (Pure.SIN, Pure.SIN) = true
      | same (Pure.COS, Pure.COS) = true
      | same (Pure.TAN, Pure.TAN) = true
      | same (Pure.ASIN, Pure.ASIN) = true
      | same (Pure.ACOS, Pure.ACOS) = true
      | same (Pure.ATAN, Pure.ATAN) = true
      | same (Pure.SINH, Pure.SINH) = true
      | same (Pure.COSH, Pure.COSH) = true
      | same (Pure.TANH, Pure.TANH) = true
      | same (Pure.ADD_SCAN t1, Pure.ADD_SCAN t2) = sameTy (t1, t2)
      | same (Pure.MUL_SCAN t1, Pure.MUL_SCAN t2) = sameTy (t1, t2)
      | same (Pure.MAX_SCAN t1, Pure.MAX_SCAN t2) = sameTy (t1, t2)
      | same (Pure.MIN_SCAN t1, Pure.MIN_SCAN t2) = sameTy (t1, t2)
      | same (Pure.AND_SCAN t1, Pure.AND_SCAN t2) = sameTy (t1, t2)
      | same (Pure.OR_SCAN t1, Pure.OR_SCAN t2) = sameTy (t1, t2)
      | same (Pure.XOR_SCAN t1, Pure.XOR_SCAN t2) = sameTy (t1, t2)
      | same (Pure.ADD_REDUCE t1, Pure.ADD_REDUCE t2) = sameTy (t1, t2)
      | same (Pure.MUL_REDUCE t1, Pure.MUL_REDUCE t2) = sameTy (t1, t2)
      | same (Pure.MAX_REDUCE t1, Pure.MAX_REDUCE t2) = sameTy (t1, t2)
      | same (Pure.MIN_REDUCE t1, Pure.MIN_REDUCE t2) = sameTy (t1, t2)
      | same (Pure.AND_REDUCE t1, Pure.AND_REDUCE t2) = sameTy (t1, t2)
      | same (Pure.OR_REDUCE t1, Pure.OR_REDUCE t2) = sameTy (t1, t2)
      | same (Pure.XOR_REDUCE t1, Pure.XOR_REDUCE t2) = sameTy (t1, t2)
      | same (Pure.PERMUTE t1, Pure.PERMUTE t2) = sameTy (t1, t2)
      | same (Pure.DPERMUTE t1, Pure.DPERMUTE t2) = sameTy (t1, t2)
      | same (Pure.FPERMUTE t1, Pure.FPERMUTE t2) = sameTy (t1, t2)
      | same (Pure.BPERMUTE t1, Pure.BPERMUTE t2) = sameTy (t1, t2)
      | same (Pure.BFPERMUTE t1, Pure.BFPERMUTE t2) = sameTy (t1, t2)
      | same (Pure.DFPERMUTE t1, Pure.DFPERMUTE t2) = sameTy (t1, t2)
      | same (Pure.EXTRACT t1, Pure.EXTRACT t2) = sameTy (t1, t2)
      | same (Pure.REPLACE t1, Pure.REPLACE t2) = sameTy (t1, t2)
      | same (Pure.PACK t1, Pure.PACK t2) = sameTy (t1, t2)
      | same (Pure.RANK_UP t1, Pure.RANK_UP t2) = sameTy (t1, t2)
      | same (Pure.RANK_DOWN t1, Pure.RANK_DOWN t2) = sameTy (t1, t2)
      | same (Pure.DIST t1, Pure.DIST t2) = sameTy (t1, t2)
      | same (Pure.INDEX, Pure.INDEX) = true
      | same (Pure.LENGTH t1, Pure.LENGTH t2) = sameTy (t1, t2)
      | same (Pure.MAKE_SEGDES, Pure.MAKE_SEGDES) = true
      | same (Pure.LENGTHS, Pure.LENGTHS) = true

  end
