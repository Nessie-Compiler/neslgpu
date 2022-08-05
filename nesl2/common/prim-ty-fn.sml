(* prim-ty-fn.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

functor PrimTyFn (Ty : sig

    type ty

    val tyBase : TypeBase.ty -> ty
    val tyBaseSeq : TypeBase.ty -> ty
    val tySeq : TypeBase.ty -> ty

  end) : sig

    type ty = Ty.ty

    val typeOfPure : Pure.pure -> (ty list * ty)

    val typeOfCmd : Cmd.cmd -> (ty list * ty list)

  end = struct

    type ty = Ty.ty

  (* unsegmented sequence of ints *)
    val ty_iseq = Ty.tySeq TypeBase.INT

  (* unsegmented sequence of bools *)
    val ty_bseq = Ty.tySeq TypeBase.BOOL

  (* unsegmented sequence of chars *)
    val tyString = Ty.tySeq TypeBase.CHAR

    val tyBool = Ty.tyBase TypeBase.BOOL
    val tyInt = Ty.tyBase TypeBase.INT
    val tyChar = Ty.tyBase TypeBase.CHAR
    val tyFloat = Ty.tyBase TypeBase.FLOAT
    val tySegdes = Ty.tyBase TypeBase.SEGDES

    fun relOp tb = let
	    val ty = Ty.tyBase tb
	    in
	      ([ty, ty], tyBool)
	    end
    fun binOp tb = let
	    val ty = Ty.tyBase tb
	    in
	      ([ty, ty], ty)
	    end
    fun scanOp tb = let
	    val ty = Ty.tySeq tb
	    in
              ([ty], ty)
	    end
    fun reduceOp tb = ([Ty.tySeq tb], Ty.tyBase tb)

    val ty_ii = [tyInt, tyInt]
    val ty_si = [tyInt, tyString]

    fun typeOfPure rator = (case rator
	  (* Elementwise operations *)
	   of Pure.ADD ty => binOp ty
	    | Pure.SUB ty => binOp ty
	    | Pure.MUL ty => binOp ty
	    | Pure.DIV ty => binOp ty
	    | Pure.MOD => (ty_ii, tyInt)
	    | Pure.LT ty => relOp ty
	    | Pure.LTE ty => relOp ty
	    | Pure.GT ty => relOp ty
	    | Pure.GTE ty => relOp ty
	    | Pure.EQ ty => relOp ty
	    | Pure.NEQ ty => relOp ty
	    | Pure.LSHIFT => (ty_ii, tyInt)
	    | Pure.RSHIFT => (ty_ii, tyInt)
	    | Pure.NOT tb => let
		val ty = Ty.tyBase tb
		in
		  ([ty], ty)
		end
	    | Pure.AND ty => binOp ty
	    | Pure.OR ty => binOp ty
	    | Pure.XOR ty => binOp ty
	    | Pure.SELECT tb => let
		val ty = Ty.tyBase tb
		in
		  ([tyBool, ty, ty], ty)
		end
	    | Pure.RAND => ([tyInt], tyInt)
	    | Pure.FLOOR => ([tyFloat], tyInt)
	    | Pure.CEIL => ([tyFloat], tyInt)
	    | Pure.TRUNC => ([tyFloat], tyInt)
	    | Pure.ROUND => ([tyFloat], tyInt)
	    | Pure.I_TO_F => ([tyInt], tyFloat)
	    | Pure.I_TO_B => ([tyInt], tyBool)
	    | Pure.B_TO_I => ([tyBool], tyInt)
	    | Pure.LOG => ([tyFloat], tyFloat)
	    | Pure.SQRT => ([tyFloat], tyFloat)
	    | Pure.EXP => ([tyFloat], tyFloat)
	    | Pure.SIN => ([tyFloat], tyFloat)
	    | Pure.COS => ([tyFloat], tyFloat)
	    | Pure.TAN => ([tyFloat], tyFloat)
	    | Pure.ASIN => ([tyFloat], tyFloat)
	    | Pure.ACOS => ([tyFloat], tyFloat)
	    | Pure.ATAN => ([tyFloat], tyFloat)
	    | Pure.SINH => ([tyFloat], tyFloat)
	    | Pure.COSH => ([tyFloat], tyFloat)
	    | Pure.TANH => ([tyFloat], tyFloat)
	    | Pure.I_TO_C => ([tyInt], tyChar)
	    | Pure.C_TO_I => ([tyChar], tyInt)
	  (* Vector instructions *)
	    | Pure.ADD_SCAN ty => scanOp ty
	    | Pure.MUL_SCAN ty => scanOp ty
	    | Pure.MAX_SCAN ty => scanOp ty
	    | Pure.MIN_SCAN ty => scanOp ty
	    | Pure.AND_SCAN ty => scanOp ty
	    | Pure.OR_SCAN ty => scanOp ty
	    | Pure.XOR_SCAN ty => scanOp ty
	    | Pure.ADD_REDUCE ty => reduceOp ty
	    | Pure.MUL_REDUCE ty => reduceOp ty
	    | Pure.MAX_REDUCE ty => reduceOp ty
	    | Pure.MIN_REDUCE ty => reduceOp ty
	    | Pure.AND_REDUCE ty => reduceOp ty
	    | Pure.OR_REDUCE ty => reduceOp ty
	    | Pure.XOR_REDUCE ty => reduceOp ty
	    | Pure.PERMUTE tb => let
		val seqTy = Ty.tySeq tb
		in
		  ([seqTy, ty_iseq], seqTy)
		end
	    | Pure.DPERMUTE tb => let
		val seqTy = Ty.tySeq tb
		in
                  ([seqTy, ty_iseq, seqTy], seqTy)
		end
	    | Pure.FPERMUTE tb => let
		val seqTy = Ty.tySeq tb
		in
		  ([seqTy, ty_iseq, ty_bseq], seqTy)
		end
	    | Pure.BPERMUTE tb => let
		val seqTy = Ty.tySeq tb
		in
		  ([seqTy, ty_iseq], seqTy)
		end
	    | Pure.BFPERMUTE tb => let
		val seqTy = Ty.tySeq tb
		in
		  ([seqTy, ty_iseq, ty_bseq], seqTy)
		end
	    | Pure.DFPERMUTE tb => let
		val seqTy = Ty.tySeq tb
		in
		  ([seqTy, ty_iseq, ty_bseq, seqTy], seqTy)
		end
	    | Pure.EXTRACT tb => let
		val seqTy = Ty.tySeq tb
		val scalarTy = Ty.tyBase tb
		in
		  ([seqTy, tyInt], scalarTy)
		end
	    | Pure.REPLACE tb => let
		val seqTy = Ty.tySeq tb
		val scalarTy = Ty.tyBase tb
		in
		  ([seqTy, tyInt, scalarTy], seqTy)
		end
	    | Pure.PACK tb => let
		val seqTy = Ty.tySeq tb
		in
		  ([seqTy, ty_bseq], seqTy)
		end
	    | Pure.RANK_UP tb => let
		val seqTy = Ty.tySeq tb
		in
		  ([seqTy], ty_iseq)
		end
	    | Pure.RANK_DOWN tb => let
		val seqTy = Ty.tySeq tb
		in
		  ([seqTy], ty_iseq)
		end
	    | Pure.DIST tb => let
		val seqTy = Ty.tySeq tb
		val scalarTy = Ty.tyBase tb
		in
		  ([scalarTy, tyInt], seqTy)
		end
	    | Pure.INDEX => ([tyInt, tyInt, tyInt], ty_iseq)
	    | Pure.LENGTH tb => ([Ty.tySeq tb], tyInt)
	    | Pure.SCALAR_TO_SEQ tb => (* creates a base-sequence of length one from its scalar argument *)
		([Ty.tyBase tb], Ty.tyBaseSeq tb)
	    | Pure.SEQ_TO_SCALAR tb => (* extracts the scalar value from a base-sequence of length one *)
		([Ty.tyBaseSeq tb], Ty.tyBase tb)
	  (* Segment descriptor instructions *)
	    | Pure.MAKE_SEGDES => ([Ty.tyBaseSeq TypeBase.INT], tySegdes)
	    | Pure.LENGTHS => ([tySegdes], Ty.tyBaseSeq (TypeBase.INT))
	    | _ => raise Fail(concat["typeOfPure(", Pure.toString rator, ")"])
	  (* end case *))


  (* Commands *)
    fun typeOfCmd cmd = (case cmd
	   of Cmd.EXIT => ([tyInt], [tyInt])
	    | Cmd.READ tb => ([], [Ty.tySeq tb])
	    | Cmd.WRITE tb => ([Ty.tySeq tb], [Ty.tySeq tb, tyBool]) 
	    | Cmd.FOPEN => ([tyString, tyInt], [tyInt, tyString, tyBool])
	    | Cmd.FCLOSE => ([tyInt], ty_si)
	    | Cmd.FWRITE tb => ([Ty.tySeq tb, tyInt], ty_si)
	    | Cmd.FREAD tb => ([tyInt], [Ty.tySeq tb, tyString, tyBool])
	    | Cmd.FREAD_CHAR => ([tyString, tyInt, tyInt], [tyString, tyInt, tyString, tyBool])
	    | Cmd.START_TIMER => ([tyInt], [tyInt])
	    | Cmd.STOP_TIMER => ([tyInt], [tyFloat])
	    | Cmd.SRAND => ([tyInt], [tyBool])
	  (* end case *))

  end

(* typing of primitive operators with tuple types *)
functor WrappedPrimTyFn (Ty : sig

    type ty

    val tyBase : TypeBase.ty -> ty
    val tyBaseSeq : TypeBase.ty -> ty
    val tySeq : TypeBase.ty -> ty
    val tyTuple : ty list -> ty

  end) : sig

    type ty = Ty.ty

    val typeOfPure : Pure.pure -> (ty * ty)

    val typeOfCmd : Cmd.cmd -> (ty * ty)

  end = struct

    structure PTF = PrimTyFn (Ty)

    type ty = Ty.ty

    fun typeOfPure p = let
	  val (domTy, rngTy) = PTF.typeOfPure p
	  in
	    (Ty.tyTuple domTy, rngTy)
	  end

    fun typeOfCmd cmd = let
	  val (domTy, rngTy) = PTF.typeOfCmd cmd
	  in
	    (Ty.tyTuple domTy, Ty.tyTuple rngTy)
	  end

  end
