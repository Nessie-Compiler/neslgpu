(* check-prim.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure CheckPrim : sig

    datatype prim = PURE of Pure.pure | CMD of Cmd.cmd

  (* lookup primitive and return it and its type *)
    val check : string -> prim * NeslTypes.ty * NeslTypes.ty

  end = struct

    structure Ty = NeslTypes

    datatype prim = PURE of Pure.pure | CMD of Cmd.cmd

    fun matchTy ty = (case String.map Char.toUpper ty
	   of "INT" => TypeBase.INT
	    | "BOOL" => TypeBase.BOOL
	    | "FLOAT" => TypeBase.FLOAT
	    | "CHAR" => TypeBase.CHAR
	    | "STREAM" => TypeBase.STREAM
	    | "SEGDES" => TypeBase.SEGDES
	    | _ => raise Fail(concat["CheckPrim: unrecognized type \"", ty, "\""])
	  (* end case *))

(*    fun tyBaseSeq tb = Ty.TyBaseSeq(Ty.TyBase tb) *)
    fun tyBaseSeq tb = Ty.TySeq(Ty.TyBase tb)

  (* unsegmented sequence of ints *)
    val ty_iseq = tyBaseSeq TypeBase.INT

  (* unsegmented sequence of bools *)
    val ty_bseq = tyBaseSeq TypeBase.BOOL

  (* unsegmented sequence of chars *)
    val tyString = tyBaseSeq TypeBase.CHAR

    val tyBool = Ty.tyBool
    val tyInt = Ty.tyInt
    val tyChar = Ty.tyChar
    val tyFloat = Ty.tyFloat
    val tySegdes = Ty.tySegdes

    fun pureRelOp (pureCon, ty) = let
	    val tb = matchTy ty
	    val ty = Ty.TyBase tb
	    in
	      (PURE(pureCon tb), Ty.TyPair(ty, ty), tyBool)
	    end
    fun pureBinOp (pureCon, ty) = let
	    val tb = matchTy ty
	    val ty = Ty.TyBase tb
	    in
	      (PURE(pureCon tb), Ty.TyPair(ty, ty), ty)
	    end

    fun pureScanOp (pureCon, ty) = let
	    val tb = matchTy ty
	    val ty = tyBaseSeq tb
	    in
      (*(PURE(pureCon tb), Ty.TyPair(ty, tySegdes), ty) *)
              (PURE(pureCon tb), ty, ty)
	    end

(*    val pureReduceOp = pureScanOp *)
    fun pureReduceOp (pureCon, ty)=let
            val tb = matchTy ty
	    val scalarTy = Ty.TyBase tb
	    val seqTy = tyBaseSeq tb
            in
              (PURE(pureCon tb), seqTy, scalarTy)
            end

    val ty_ii = Ty.TyPair(tyInt, tyInt)
    val ty_sb = Ty.TyPair(tyString, tyBool)

    fun tyTuple [ty] = ty
      | tyTuple (ty::tys) = Ty.TyPair(ty, tyTuple tys)

    fun check rator = (case String.tokens Char.isSpace rator
	  (* Elementwise operations *)
	   of ["+", ty] => pureBinOp (Pure.ADD, ty)
	    | ["-", ty] => pureBinOp (Pure.SUB, ty)
	    | ["*", ty] => pureBinOp (Pure.MUL, ty)
	    | ["/", ty] => pureBinOp (Pure.DIV, ty)
	    | ["%"] => (PURE Pure.MOD, ty_ii, tyInt)
	    | ["<", ty] => pureRelOp (Pure.LT, ty)
	    | ["<=", ty] => pureRelOp (Pure.LTE, ty)
	    | [">", ty] => pureRelOp (Pure.GT, ty)
	    | [">=", ty] => pureRelOp (Pure.GTE, ty)
	    | ["=", ty] => pureRelOp (Pure.EQ, ty)
	    | ["!=", ty] => pureRelOp (Pure.NEQ, ty)
	    | ["LSHIFT"] => (PURE Pure.LSHIFT, ty_ii, tyInt)
	    | ["RSHIFT"] => (PURE Pure.RSHIFT, ty_ii, tyInt)
	    | ["NOT", ty] => let
		val tb = matchTy ty
		val ty = Ty.TyBase tb
		in
		  (PURE(Pure.NOT tb), ty, ty)
		end
	    | ["AND", ty] => pureBinOp (Pure.AND, ty)
	    | ["OR", ty] => pureBinOp (Pure.OR, ty)
	    | ["XOR", ty] => pureBinOp (Pure.XOR, ty)
	    | ["SELECT", ty] => let
		val tb = matchTy ty
		val ty = Ty.TyBase tb
		in
		  (PURE(Pure.SELECT tb), tyTuple[tyBool, ty, ty], ty)
		end
	    | ["RAND"] => (PURE Pure.RAND, tyInt, tyInt)
	    | ["FLOOR"] => (PURE Pure.FLOOR, tyFloat, tyInt)
	    | ["CEIL"] => (PURE Pure.CEIL, tyFloat, tyInt)
	    | ["TRUNC"] => (PURE Pure.TRUNC, tyFloat, tyInt)
	    | ["ROUND"] => (PURE Pure.ROUND, tyFloat, tyInt)
	    | ["I_TO_F"] => (PURE Pure.I_TO_F, tyInt, tyFloat)
	    | ["I_TO_B"] => (PURE Pure.I_TO_B, tyInt, tyBool)
	    | ["B_TO_I"] => (PURE Pure.B_TO_I, tyBool, tyInt)
	    | ["LOG"] => (PURE Pure.LOG, tyFloat, tyFloat)
	    | ["SQRT"] => (PURE Pure.SQRT, tyFloat, tyFloat)
	    | ["EXP"] => (PURE Pure.EXP, tyFloat, tyFloat)
	    | ["SIN"] => (PURE Pure.SIN, tyFloat, tyFloat)
	    | ["COS"] => (PURE Pure.COS, tyFloat, tyFloat)
	    | ["TAN"] => (PURE Pure.TAN, tyFloat, tyFloat)
	    | ["ASIN"] => (PURE Pure.ASIN, tyFloat, tyFloat)
	    | ["ACOS"] => (PURE Pure.ACOS, tyFloat, tyFloat)
	    | ["ATAN"] => (PURE Pure.ATAN, tyFloat, tyFloat)
	    | ["SINH"] => (PURE Pure.SINH, tyFloat, tyFloat)
	    | ["COSH"] => (PURE Pure.COSH, tyFloat, tyFloat)
	    | ["TANH"] => (PURE Pure.TANH, tyFloat, tyFloat)
	    | ["I_TO_C"] => (PURE Pure.I_TO_C, tyInt, tyChar)
	    | ["C_TO_I"] => (PURE Pure.C_TO_I, tyChar, tyInt)
	  (* Vector instructions *)
	    | ["+_SCAN", ty] => pureScanOp (Pure.ADD_SCAN, ty)
	    | ["*_SCAN", ty] => pureScanOp (Pure.MUL_SCAN, ty)
	    | ["MAX_SCAN", ty] => pureScanOp (Pure.MAX_SCAN, ty)
	    | ["MIN_SCAN", ty] => pureScanOp (Pure.MIN_SCAN, ty)
	    | ["AND_SCAN", ty] => pureScanOp (Pure.AND_SCAN, ty)
	    | ["OR_SCAN", ty] => pureScanOp (Pure.OR_SCAN, ty)
	    | ["XOR_SCAN", ty] => pureScanOp (Pure.XOR_SCAN, ty)
	    | ["+_REDUCE", ty] => pureReduceOp (Pure.ADD_REDUCE, ty)
	    | ["*_REDUCE", ty] => pureReduceOp (Pure.MUL_REDUCE, ty)
	    | ["MAX_REDUCE", ty] => pureReduceOp (Pure.MAX_REDUCE, ty)
	    | ["MIN_REDUCE", ty] => pureReduceOp (Pure.MIN_REDUCE, ty)
	    | ["AND_REDUCE", ty] => pureReduceOp (Pure.AND_REDUCE, ty)
	    | ["OR_REDUCE", ty] => pureReduceOp (Pure.OR_REDUCE, ty)
	    | ["XOR_REDUCE", ty] => pureReduceOp (Pure.XOR_REDUCE, ty)
	    | ["PERMUTE", ty] => let
		val tb = matchTy ty
		val seqTy = tyBaseSeq tb
		in
		  (PURE(Pure.PERMUTE tb), Ty.TyPair(seqTy, ty_iseq), seqTy)
		end
	    | ["DPERMUTE", ty] => let
		val tb = matchTy ty
		val seqTy = tyBaseSeq tb
		in
		  (PURE(Pure.DPERMUTE tb), tyTuple[seqTy, ty_iseq, seqTy], seqTy)
		end
	    | ["FPERMUTE", ty] => let
		val tb = matchTy ty
		val seqTy = tyBaseSeq tb
		in
		  (PURE(Pure.FPERMUTE tb), tyTuple[seqTy, ty_iseq, ty_bseq], seqTy)
		end
	    | ["BPERMUTE", ty] => let
		val tb = matchTy ty
		val seqTy = tyBaseSeq tb
		in
		  (PURE(Pure.BPERMUTE tb), Ty.TyPair(seqTy, ty_iseq), seqTy)
		end
	    | ["BFPERMUTE", ty] => let
		val tb = matchTy ty
		val seqTy = tyBaseSeq tb
		in
		  (PURE(Pure.BFPERMUTE tb), tyTuple[seqTy, ty_iseq, ty_bseq], seqTy)
		end
	    | ["DFPERMUTE", ty] => let
		val tb = matchTy ty
		val seqTy = tyBaseSeq tb
		in
		  (PURE(Pure.DFPERMUTE tb), tyTuple[seqTy, ty_iseq, ty_bseq, seqTy], seqTy)
		end
	    | ["EXTRACT", ty] => let
		val tb = matchTy ty
		val scalarTy = Ty.TyBase tb
		val seqTy = tyBaseSeq tb
		in
		  (PURE(Pure.EXTRACT tb), Ty.TyPair(seqTy, tyInt), scalarTy)
		end
	    | ["REPLACE", ty] => let
		val tb = matchTy ty
		val scalarTy = Ty.TyBase tb
		val seqTy = tyBaseSeq tb
		in
		  (PURE(Pure.REPLACE tb), tyTuple[seqTy, tyInt, scalarTy], seqTy)
(*		  (PURE(Pure.REPLACE tb), tyTuple[seqTy, ty_iseq, seqTy, tySegdes], seqTy) *)
		end
	    | ["PACK", ty] => let
		val tb = matchTy ty
		val seqTy = tyBaseSeq tb
		in
		  (PURE(Pure.PACK tb), Ty.TyPair(seqTy, tyBaseSeq TypeBase.BOOL), seqTy)
		end
	    | ["RANK_UP", ty] => let
		val tb = matchTy ty
		val seqTy = tyBaseSeq tb
		in
		  (PURE(Pure.RANK_UP tb), seqTy, ty_iseq)
		end
	    | ["RANK_DOWN", ty] => let
		val tb = matchTy ty
		val seqTy = tyBaseSeq tb
		in
		  (PURE(Pure.RANK_DOWN tb), seqTy, ty_iseq)
		end
	    | ["DIST", ty] => let
		val tb = matchTy ty
		val scalarTy = Ty.TyBase tb
		val seqTy = tyBaseSeq tb
		in
		  (PURE(Pure.DIST tb), Ty.TyPair(scalarTy, tyInt), seqTy)
		end
(*	    | ["INDEX"] => (PURE Pure.INDEX, tyTuple[ty_iseq, ty_iseq, tySegdes], ty_iseq) *)
	    | ["INDEX"] => (PURE Pure.INDEX, tyTuple[tyInt, tyInt, tyInt], ty_iseq)
	    | ["LENGTH", ty] => let
		val tb = matchTy ty
		in
		  (PURE(Pure.LENGTH tb), tyBaseSeq tb, tyInt)
		end
	    | ["SCALAR_TO_SEQ", ty] => let (* creates a base-sequence of length one from its scalar argument *)
		val tb = matchTy ty
		in
		  (PURE(Pure.SCALAR_TO_SEQ tb), Ty.TyBase tb, Ty.TyBaseSeq(Ty.TyBase tb))
		end
	    | ["SEQ_TO_SCALAR", ty] => let (* extracts the scalar value from a base-sequence of length one *)
		val tb = matchTy ty
		in
		  (PURE(Pure.SEQ_TO_SCALAR tb), Ty.TyBaseSeq(Ty.TyBase tb), Ty.TyBase tb)
		end
	  (* Segment descriptor instructions *)
	    | ["MAKE_SEGDES"] => (PURE Pure.MAKE_SEGDES, 
				  Ty.TyBaseSeq(Ty.TyBase TypeBase.INT), 
				  tySegdes)
	    | ["LENGTHS"] => (PURE Pure.LENGTHS, tySegdes, 
			      Ty.TyBaseSeq(Ty.TyBase TypeBase.INT))
	  (* Commands *)
	    | ["EXIT"] => (CMD Cmd.EXIT, tyInt, tyInt)
	    | ["READ", ty] => let
		val tb = matchTy ty
		in
		  (CMD(Cmd.READ tb), Ty.TyVoid, tyBaseSeq tb)
		end
	    | ["WRITE", ty] => let
		val tb = matchTy ty
		in
		  (CMD(Cmd.WRITE tb), tyBaseSeq tb, ty_sb)
		end
	    | ["FOPEN"] => 
		(CMD Cmd.FOPEN, Ty.TyPair(tyString, tyInt), tyTuple[tyInt, tyString, tyBool])
	    | ["FCLOSE"] => (CMD Cmd.FCLOSE, tyInt, ty_sb)
	    | ["FWRITE", ty] => let
		val tb = matchTy ty
		in (
		  CMD(Cmd.FWRITE tb), Ty.TyPair(tyBaseSeq tb, tyInt), ty_sb
		) end
	    | ["FREAD", ty] => let
		val tb = matchTy ty
		in (
		  CMD(Cmd.FREAD tb), tyInt, tyTuple[tyBaseSeq tb, tyString, tyBool]
		) end
	    | ["FREAD_CHAR"] => (
		CMD Cmd.FREAD_CHAR,
		tyTuple[tyString, tyInt, tyInt],
		tyTuple[tyString, tyInt, tyString, tyBool])
	    | ["START_TIMER"] => (CMD Cmd.START_TIMER, tyInt, tyInt)
	    | ["STOP_TIMER"] => (CMD Cmd.STOP_TIMER, tyInt, tyFloat)
	    | ["SRAND", "INT"] => (CMD Cmd.SRAND, tyInt, tyBool)
	    | _ => raise Fail(concat["CheckPrim: unrecognized primop \"", rator, "\""])
	  (* end case *))

  end
