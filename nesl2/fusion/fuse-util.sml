(* fuse-util.sml
 *
 * COPYRIGHT (c) 2014 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *)

structure FuseUtil =
  struct

  structure FA = FuseAST
  structure S = FuseShapes
  structure V = FuseVar

  fun tyToString (FA.TyScalar t) = TypeBase.baseToString t
    | tyToString (FA.TySegdes) = "segdes"
    | tyToString (FA.TySeq(t, SOME n)) = String.concat[
	  "[", TypeBase.baseToString t, " ## ", Int.toString n, "]"
	]
    | tyToString (FA.TySeq(t, NONE)) = String.concat["[", TypeBase.baseToString t, "]"]
    | tyToString (FA.TyTuple ts) = String.concat["(", tyListToString ts, ")"]

  and tyListToString [] = ""
    | tyListToString (t::[]) = tyToString t
    | tyListToString (t::ts) = String.concat[tyToString t, ",", tyListToString ts]

  (* For type-checking purposes, vectors need not be the same length to be the same type *)
  fun sameTy (FA.TyScalar t1, FA.TyScalar t2) = (t1 = t2)
    | sameTy (FA.TySeq (t1, _), FA.TySeq (t2, _)) = (t1 = t2)
    | sameTy (FA.TySegdes, FA.TySegdes) = true
    | sameTy (FA.TyTuple ts1, FA.TyTuple ts2) = sameTys(ts1, ts2)
    | sameTy _ = false

  and sameTys ([], []) = true
    | sameTys (ty1::tys1, ty2::tys2) = sameTy(ty1, ty2) andalso sameTys(tys1, tys2)
    | sameTys _ = false

  fun seqTy (tb, l) = FA.TyTuple [FA.TySegdes, FA.TySeq (tb, l)]

  fun getAtmTy (FA.Var v) = V.typeOf v
    | getAtmTy (FA.Bool _) = FA.TyScalar TypeBase.BOOL
    | getAtmTy (FA.Char _) = FA.TyScalar TypeBase.CHAR
    | getAtmTy (FA.Int _) = FA.TyScalar TypeBase.INT
    | getAtmTy (FA.Float _) = FA.TyScalar TypeBase.FLOAT

  fun getAtmTy' atms = map getAtmTy atms

  fun getAtmShape (FA.Var v) = V.getShape v
    | getAtmShape _ = S.SCALAR

  fun atomToString (FA.Var v) = V.toString v
    | atomToString (FA.Bool b) = Bool.toString b
    | atomToString (FA.Int i) = IntInf.toString i
    | atomToString (FA.Char c) = Char.toString c
    | atomToString (FA.Float f) = f
    | atomToString (FA.String s) = s

  fun rhsToString rhs = let
	val varsToS = List.map V.toString
	val atomsToS = List.map atomToString
	fun toS (prefix, rator, args) = concat[
		prefix, "(", rator, ", ", String.concatWith ", " args, ")"
	      ]
	in
	  case rhs
	   of FA.RHS_Scalar(rator, args) => toS("RHS_Scalar", ScalarOp.toString rator, atomsToS args)
	    | FA.RHS_FlatGen(rator, wid, args) => toS("RHS_FlatGen", GeneratorOp.toString rator, atomsToS (wid::args))
	    | FA.RHS_SegGen(rator, wid, args) => toS("RHS_SegGen", GeneratorOp.toString rator, atomToString wid :: varsToS args)
	    | FA.RHS_Map(kern, wid, args) => toS("RHS_Map",  Kernel.toString kern, atomToString wid :: varsToS args)
	    | FA.RHS_FlatReduce(rator, wid, arg) => toS("RHS_FlatReduce", ReduceOp.toString rator, [atomToString wid, V.toString arg])
	    | FA.RHS_SegReduce(rator, wid, seg, data) =>
		toS("RHS_SegReduce", ReduceOp.toString rator, [atomToString wid, V.toString seg, V.toString data])
	    | FA.RHS_FlatScan(rator, wid, arg) =>
		toS("RHS_FlatScan", ScanOp.toString rator, [atomToString wid, V.toString arg])
	    | FA.RHS_SegScan(rator, wid, seg, data) =>
		toS("RHS_SegScan", ScanOp.toString rator, [atomToString wid, V.toString seg, V.toString data])
	    | FA.RHS_FlatVector(rator, args) => toS("RHS_FlatVector", VectorOp.toString rator, atomsToS args)
	    | FA.RHS_SegVector(rator, args) => toS("RHS_SegVector", VectorOp.toString rator, atomsToS args)
	    | FA.RHS_Internal(rator, arg) => toS("RHS_Internal", InternalOp.toString rator, [atomToString arg])
	    | FA.RHS_Cmd(cmd, args) => toS("RHS_Cmd", Cmd.toString cmd, atomsToS args)
	    | FA.RHS_Seq _ => "RHS_Seq"
	    | FA.RHS_Tuple _ => "RHS_Tuple"
	    | FA.RHS_Proj _ => "RHS_Proj"
	  (* end case *)
	end

  fun vecLength (FA.V{ty, ...}) =
      (case ty
	of FA.TyTuple [_, FA.TySeq(_, len)] => len
	 | _ => raise Fail "vecLength: not a vector"
      (* end case *))

  fun segVecLength (FA.V{ty, ...}) =
      (case ty
	of FA.TyTuple [_, FA.TyTuple[_, FA.TySeq(_, len)]] => len
	 | _ => raise Fail "segVecLength: not a segmented vector"
      (* end case *))

  (* Smart constructor to set the shape of var
   * when RHS is Tuple or Proj.
   * Assumes RHS is bound to a single variable.
   *)
  fun setShapeFromRHS (var, rhs) =
      (case rhs
	of FA.RHS_Tuple atms =>
	   (V.setShape(var, S.TUPLE(map getAtmShape atms)))
	 | FA.RHS_Proj (i, v) => let
	     val shapes =
		 (case V.getShape v
		   of S.TUPLE s => s
		    | _ => raise Fail (String.concat["projecting from variable ", V.toString v, " but it's shape isn't a tuple."])
		 (* end case *))
	   in
	     (V.setShape(var, List.nth(shapes, i - 1)))
	   end
	 | _ => ()
      (* end case *))

  fun mkRHS' ([], e) = e
    | mkRHS' ((var, rhs)::rhss, e) = let
    val _ = setShapeFromRHS(var, rhs)
    val rest = mkRHS'(rhss, e)
  in
    FA.mkRHS([var], rhs, rest)
  end

  fun getFlatShape v = let
    val S.TUPLE [_, flatShape] = V.getShape v
  in
    flatShape
  end

  fun getInnerFlatShape v = let
    val S.TUPLE [_, S.TUPLE[_, shape]] = V.getShape v
  in
    shape
  end

  (* Getting types of scalar ops *)
  val tyInt = FA.TyScalar TypeBase.INT
  val tyBool = FA.TyScalar TypeBase.BOOL
  val tyChar = FA.TyScalar TypeBase.CHAR
  val tyFloat = FA.TyScalar TypeBase.FLOAT

  val ty_ii = [tyInt, tyInt]

  fun binOp tb = let
    val ty = FA.TyScalar tb
  in
    ([ty, ty], ty)
  end

  fun relOp tb = let
    val ty = FA.TyScalar tb
    in
    ([ty, ty], tyBool)
  end

  val unaryFloatOp = ([tyFloat], tyFloat)

  fun typeOfScalar opcode =
      (case opcode
          of ScalarOp.ADD ty => binOp ty
           | ScalarOp.SUB ty => binOp ty
           | ScalarOp.MUL ty => binOp ty
           | ScalarOp.DIV ty => binOp ty
           | ScalarOp.MOD => (ty_ii, tyInt)
           | ScalarOp.LT ty => relOp ty
           | ScalarOp.LTE ty => relOp ty
           | ScalarOp.GT ty => relOp ty
           | ScalarOp.GTE ty => relOp ty
           | ScalarOp.EQ ty => relOp ty
           | ScalarOp.NEQ ty => relOp ty
           | ScalarOp.LSHIFT => (ty_ii, tyInt)
           | ScalarOp.RSHIFT => (ty_ii, tyInt)
           | ScalarOp.NOT tb => let
	       val ty = FuseAST.TyScalar tb
	     in
	       ([ty], ty)
	     end
           | ScalarOp.AND ty => binOp ty
           | ScalarOp.OR ty => binOp ty
           | ScalarOp.XOR ty => binOp ty
           | ScalarOp.SELECT tb => let
	       val ty = FuseAST.TyScalar tb
	     in
	       ([tyBool, ty, ty], ty)
	     end
           | ScalarOp.RAND => ([tyInt], tyInt)
           | ScalarOp.FLOOR => ([tyFloat], tyInt)
           | ScalarOp.CEIL => ([tyFloat], tyInt)
           | ScalarOp.TRUNC => ([tyFloat], tyInt)
           | ScalarOp.ROUND => ([tyFloat], tyInt)
           | ScalarOp.I_TO_F => ([tyInt], tyFloat)
           | ScalarOp.I_TO_B => ([tyInt], tyBool)
           | ScalarOp.B_TO_I => ([tyBool], tyInt)
           | ScalarOp.LOG => unaryFloatOp
           | ScalarOp.SQRT => unaryFloatOp
           | ScalarOp.EXP => unaryFloatOp
           | ScalarOp.SIN => unaryFloatOp
           | ScalarOp.COS => unaryFloatOp
           | ScalarOp.TAN => unaryFloatOp
           | ScalarOp.ASIN => unaryFloatOp
           | ScalarOp.ACOS => unaryFloatOp
           | ScalarOp.ATAN => unaryFloatOp
           | ScalarOp.SINH => unaryFloatOp
           | ScalarOp.COSH => unaryFloatOp
           | ScalarOp.TANH => unaryFloatOp
	   | ScalarOp.I_TO_C => ([tyInt], tyChar)
	   | ScalarOp.C_TO_I => ([tyChar], tyInt)
	(* end case *))

  fun typeOfGen opcode =
      (case opcode
	of GeneratorOp.INDEX => ([FA.TyScalar TypeBase.INT, FA.TyScalar TypeBase.INT,
			FA.TyScalar TypeBase.INT],
		       FA.TySeq (TypeBase.INT, NONE))
	 | GeneratorOp.DIST ty => ([FA.TyScalar ty, FA.TyScalar TypeBase.INT],
			 FA.TySeq (ty, NONE))
      (* end case *))

  fun typeOfSegGen opcode =
      (case opcode
	of GeneratorOp.INDEX => ([FA.TySeq (TypeBase.INT, NONE), FA.TySeq (TypeBase.INT, NONE),
			FA.TySeq (TypeBase.INT, NONE)],
		       FA.TySeq (TypeBase.INT, NONE))
	 | GeneratorOp.DIST ty => ([FA.TySeq (ty, NONE), FA.TySeq (TypeBase.INT, NONE)],
			 FA.TySeq (ty, NONE))
      (* end case *))


  fun typeOfVector opcode =
      (case opcode
        of VectorOp.PERMUTE ty => ([FA.TySeq (ty, NONE),FA.TySeq (TypeBase.INT, NONE)], FA.TySeq (ty, NONE))
         | VectorOp.DPERMUTE ty => ([FA.TySeq (ty, NONE),FA.TySeq (TypeBase.INT, NONE), FA.TySeq (ty, NONE)],
				    FA.TySeq (ty, NONE))
         | VectorOp.FPERMUTE ty => raise Fail "FIXME: FPERMUTE"
         | VectorOp.BPERMUTE ty => ([FA.TySeq (ty, NONE),FA.TySeq (TypeBase.INT, NONE)],
				    FA.TySeq (ty, NONE))
         | VectorOp.BFPERMUTE ty => raise Fail "FIXME: BFPERMUTE"
         | VectorOp.DFPERMUTE ty => raise Fail "FIXME: DFPERMUTE"
         | VectorOp.EXTRACT ty => ([FA.TySeq (ty, NONE), FA.TyScalar TypeBase.INT],
				   FA.TyScalar ty)
         | VectorOp.REPLACE ty => ([FA.TySeq (ty, NONE), FA.TyScalar TypeBase.INT, FA.TyScalar ty],
				  FA.TySeq (ty, NONE))
	 | VectorOp.PACK ty => ([FA.TySeq (ty, NONE), FA.TySeq (TypeBase.BOOL, NONE)],
				FA.TyTuple [FA.TySegdes,FA.TySeq (ty, NONE)])
         | VectorOp.RANK_UP ty => raise Fail "FIXME: RANK_UP"
         | VectorOp.RANK_DOWN ty => raise Fail "FIXME: RANK_DOWN"
      (* end case *))


  fun typeOfSegVector opcode =
      (case opcode
        of VectorOp.PERMUTE ty => ([FA.TySegdes,
				   FA.TySeq (ty, NONE),
				   FA.TySeq (TypeBase.INT, NONE)],FA.TySeq (ty, NONE))
         | VectorOp.DPERMUTE ty => ([FA.TySegdes,FA.TySeq (ty, NONE),FA.TySeq (TypeBase.INT, NONE),
				     FA.TySegdes,FA.TySeq (ty, NONE)],
				   FA.TySeq (ty, NONE))
         | VectorOp.FPERMUTE ty => raise Fail "FIXME: FPERMUTE"
         | VectorOp.BPERMUTE ty => ([FA.TySegdes,FA.TySeq (ty, NONE),
				     FA.TySegdes,FA.TySeq (ty, NONE)],
				   FA.TySeq (ty, NONE))
         | VectorOp.BFPERMUTE ty => raise Fail "FIXME: BFPERMUTE"
         | VectorOp.DFPERMUTE ty => raise Fail "FIXME: DFPERMUTE"
         | VectorOp.EXTRACT ty => ([FA.TySegdes,FA.TySeq (ty, NONE),FA.TySeq (TypeBase.INT, NONE)],
				  FA.TySeq (ty, NONE))
         | VectorOp.REPLACE ty => ([FA.TySegdes,FA.TySeq (ty, NONE),FA.TySeq (TypeBase.INT, NONE),FA.TySeq (ty, NONE)],
				  FA.TySeq (ty, NONE))
	 | VectorOp.PACK ty => ([FA.TySegdes,FA.TySeq (ty, NONE),FA.TySeq (TypeBase.BOOL, NONE)],
				FA.TyTuple [FA.TySegdes,FA.TySeq (ty, NONE)])
         | VectorOp.RANK_UP ty => raise Fail "FIXME: RANK_UP"
         | VectorOp.RANK_DOWN ty => raise Fail "FIXME: RANK_DOWN"
      (* end case *))

  fun typeOfInternal opcode =
      (case opcode
	of InternalOp.MAKE_SEGDES => ([FA.TySeq (TypeBase.INT, NONE)], FA.TySegdes)
	 | InternalOp.LENGTHS => ([FA.TySegdes],FA.TySeq (TypeBase.INT, NONE))
	 | InternalOp.SCALAR_TO_SEQ ty => ([FA.TyScalar ty],FA.TySeq (ty, NONE))
	 | InternalOp.SEQ_TO_SCALAR ty => ([FA.TySeq (ty, NONE)], FA.TyScalar ty)
	 | InternalOp.LENGTH ty => ([FA.TySeq (ty, NONE)], FA.TyScalar TypeBase.INT)
	 | InternalOp.SUM_LENGTHS => ([FA.TySegdes], FA.TyScalar TypeBase.INT)
      (* end case *))

  fun typeOfCmd cmd =
      (case cmd
	of Cmd.START_TIMER => ([FA.TyScalar TypeBase.INT], [FA.TyScalar TypeBase.INT])
	 | Cmd.STOP_TIMER => ([FA.TyScalar TypeBase.INT], [FA.TyScalar TypeBase.FLOAT])
	 | _ => raise Fail "FIXME"
      (* end case *))


  end

