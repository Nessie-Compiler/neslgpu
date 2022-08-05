(* to-fuse.sml
 * COPYRIGHT (c) 2014 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *
 * Translation from Flan to FuseAST.
 *)

(* We still need:
 * -lifted Generators
 * -filling in cvtPureShape
 * -make cmd return multiple values?
 * -in general, more tuple unpacking?
 *)

structure ToFuse : sig

    val transform : Flan.program -> FuseAST.program

  end = struct

    structure F = Flan
    structure FT = FlanTypes
    structure FV = FlanVar
    structure FF = FlanFunct
    structure P = FlanPure
    structure FA = FuseAST
    structure E = FuseEnv
    structure U = FuseUtil
    structure S = ScalarOp
    structure G = GeneratorOp
    structure R = ReduceOp
    structure SN = ScanOp
    structure V = VectorOp
    structure I = InternalOp

    datatype pureOp
      = Scalar of S.t
      | Gen of G.t
      | Scan of SN.t
      | Reduce of R.t
      | Vector of V.t
      | Int of I.t

    fun cvtPureOp p = (case p
	  (* Elementwise operations *)
	  of Pure.ADD ty => (Scalar (S.ADD ty), SOME ty)
	   | Pure.SUB ty => (Scalar (S.SUB ty), SOME ty)
	   | Pure.MUL ty => (Scalar (S.MUL ty), SOME ty)
	   | Pure.DIV ty => (Scalar (S.DIV ty), SOME ty)
	   | Pure.MOD => (Scalar (S.MOD), NONE)
	   | Pure.LT ty => (Scalar (S.LT ty), SOME ty)
	   | Pure.LTE ty => (Scalar (S.LTE ty), SOME ty)
	   | Pure.GT ty => (Scalar (S.GT ty), SOME ty)
	   | Pure.GTE ty => (Scalar (S.GTE ty), SOME ty)
	   | Pure.EQ ty => (Scalar (S.EQ ty), SOME ty)
	   | Pure.NEQ ty => (Scalar (S.NEQ ty), SOME ty)
	   | Pure.LSHIFT => (Scalar (S.LSHIFT), NONE)
	   | Pure.RSHIFT => (Scalar (S.RSHIFT), NONE)
	   | Pure.NOT ty => (Scalar (S.NOT ty), SOME ty)
	   | Pure.AND ty => (Scalar (S.AND ty), SOME ty)
	   | Pure.OR ty => (Scalar (S.OR ty), SOME ty)
	   | Pure.XOR ty => (Scalar (S.XOR ty), SOME ty)
	   | Pure.SELECT ty => (Scalar (S.SELECT ty), SOME ty)
	   | Pure.RAND => (Scalar (S.RAND), NONE)
	   | Pure.FLOOR => (Scalar (S.FLOOR), NONE)
	   | Pure.CEIL => (Scalar (S.CEIL), NONE)
	   | Pure.TRUNC => (Scalar (S.TRUNC), NONE)
	   | Pure.ROUND => (Scalar (S.ROUND), NONE)
	   | Pure.I_TO_F => (Scalar (S.I_TO_F), NONE)
	   | Pure.I_TO_B => (Scalar (S.I_TO_B), NONE)
	   | Pure.B_TO_I => (Scalar (S.B_TO_I), NONE)
	   | Pure.LOG => (Scalar (S.LOG), NONE)
	   | Pure.SQRT => (Scalar (S.SQRT), NONE)
	   | Pure.EXP => (Scalar (S.EXP), NONE)
	   | Pure.SIN => (Scalar (S.SIN), NONE)
	   | Pure.COS => (Scalar (S.COS), NONE)
	   | Pure.TAN => (Scalar (S.TAN), NONE)
	   | Pure.ASIN => (Scalar (S.ASIN), NONE)
	   | Pure.ACOS => (Scalar (S.ACOS), NONE)
	   | Pure.ATAN => (Scalar (S.ATAN), NONE)
	   | Pure.SINH => (Scalar (S.SINH), NONE)
	   | Pure.COSH => (Scalar (S.COSH), NONE)
	   | Pure.TANH => (Scalar (S.TANH), NONE)
	   | Pure.I_TO_C => (Scalar (S.I_TO_C), NONE)
	   | Pure.C_TO_I => (Scalar (S.C_TO_I), NONE)
	   (* Generators *)
	   | Pure.DIST ty => (Gen (G.DIST ty), SOME ty)
	   | Pure.INDEX => (Gen G.INDEX, NONE)
	   (* Scans *)
	   | Pure.ADD_SCAN ty => (Scan (SN.ADD_SCAN ty), SOME ty)
	   | Pure.MUL_SCAN ty => (Scan (SN.MUL_SCAN ty), SOME ty)
	   | Pure.MAX_SCAN ty => (Scan (SN.MAX_SCAN ty), SOME ty)
	   | Pure.MIN_SCAN ty => (Scan (SN.MIN_SCAN ty), SOME ty)
	   | Pure.AND_SCAN ty => (Scan (SN.AND_SCAN ty), SOME ty)
	   | Pure.OR_SCAN ty => (Scan (SN.OR_SCAN ty), SOME ty)
	   | Pure.XOR_SCAN ty => (Scan (SN.XOR_SCAN ty), SOME ty)
	   (* Reductions *)
	   | Pure.ADD_REDUCE ty => (Reduce (R.ADD_REDUCE ty), SOME ty)
	   | Pure.MUL_REDUCE ty => (Reduce (R.MUL_REDUCE ty), SOME ty)
	   | Pure.MAX_REDUCE ty => (Reduce (R.MAX_REDUCE ty), SOME ty)
	   | Pure.MIN_REDUCE ty => (Reduce (R.MIN_REDUCE ty), SOME ty)
	   | Pure.AND_REDUCE ty => (Reduce (R.AND_REDUCE ty), SOME ty)
	   | Pure.OR_REDUCE ty => (Reduce (R.OR_REDUCE ty), SOME ty)
	   | Pure.XOR_REDUCE ty => (Reduce (R.XOR_REDUCE ty), SOME ty)
	   (* Vector ops *)
	   | Pure.PERMUTE ty => (Vector (V.PERMUTE ty), SOME ty)
	   | Pure.DPERMUTE ty => (Vector (V.DPERMUTE ty), SOME ty)
	   | Pure.FPERMUTE ty => (Vector (V.FPERMUTE ty), SOME ty)
	   | Pure.BPERMUTE ty => (Vector (V.BPERMUTE ty), SOME ty)
	   | Pure.BFPERMUTE ty => (Vector (V.BFPERMUTE ty), SOME ty)
	   | Pure.DFPERMUTE ty => (Vector (V.DFPERMUTE ty), SOME ty)
	   | Pure.EXTRACT ty => (Vector (V.EXTRACT ty), SOME ty)
	   | Pure.REPLACE ty => (Vector (V.REPLACE ty), SOME ty)
	   | Pure.PACK ty => (Vector (V.PACK ty), SOME ty)
	   | Pure.RANK_UP ty => (Vector (V.RANK_UP ty), SOME ty)
	   | Pure.RANK_DOWN ty => (Vector (V.RANK_DOWN ty), SOME ty)
	(* Internal ops *)
	   | Pure.MAKE_SEGDES => (Int I.MAKE_SEGDES, NONE)
	   | Pure.LENGTHS => (Int I.LENGTHS, NONE)
	   | Pure.SCALAR_TO_SEQ ty => (Int (I.SCALAR_TO_SEQ ty), SOME ty)
	   | Pure.SEQ_TO_SCALAR ty => (Int (I.SEQ_TO_SCALAR ty), SOME ty)
	   | Pure.LENGTH ty => (Int (I.LENGTH ty), SOME ty)
	   | _ => raise Fail(concat["cvtPureOp(", Pure.toString p, ")"])
	(* end case *))

    fun cvtTy t =
	(case t
	  of FT.TyBase (TypeBase.SEGDES) => FA.TySegdes
	   | FT.TyBase tb => FA.TyScalar tb
	   | FT.TyBaseSeq tb => FA.TySeq (tb, NONE)
	   | FT.TyTuple ts => FA.TyTuple (map cvtTy ts)
	(* end case *))

    fun cvtAtom (env, flanAtom) =
	(case flanAtom
	  of F.Var v => FA.Var (E.lookupVar(env, v))
	   | F.Bool b => FA.Bool b
	   | F.Char c => FA.Char c
	   | F.Int i => FA.Int i
	   | F.Float f => FA.Float f
	   | F.String s => FA.String s
	(* end case *))

    fun cvtAtom' (env, flanAtoms) = map (fn a => cvtAtom(env, a)) flanAtoms
(*
    (* Use arguments to pureOp to infer result shape. *)
    fun cvtPureShape(env, P.Base p, atms) = let
      val (p', _) = cvtPureOp p
    in
      (case p'
	of Scalar _ => FuseShapes.SCALAR
	 | Reduce _ => FuseShapes.SCALAR
	 | Scan _ => let
	     val [FA.Var v] = cvtAtom'(env, atms)
	     val shape = FuseVar.getShape v
	   in
	     FuseShapes.cloneShape shape
	   end
	 | Gen _ => raise Fail "FIXME: cvtPureShape (Gen)"
	 | Vector _ => raise Fail "FIXME: cvtPureShape (Vector)"
	 | _ => raise Fail "FIXME: cvtPureShape"
      (* end case *))
    end
      | cvtPureShape (env, P.Lifted p, atms) = let
	  val (p', _) = cvtPureOp p
	in
	  (case p'
	    of Scalar _ => let
		 val (FA.Var v) = cvtAtom(env, hd atms)
		 val shape = FuseVar.getShape v
	       in
		 FuseShapes.cloneVecShape shape
	       end
	     | _ => raise Fail "Fixme: cvtPureShape (lifted)"
	  (* end case *))
	end
*)
    fun cvtParam (env, flanP) =
	(case Shapes.kindOf flanP
	  of Shapes.Known a => FuseShapes.Known(cvtAtom(env, a))
	   | _ => FuseShapes.Symbolic(Shapes.getId flanP)
	(* end case *))

    fun cvtShape (env, flanShape, ty) =
	(case ty
	  of FA.TyScalar _ => FuseShapes.SCALAR
	   | FA.TySegdes =>
	     (case flanShape
	       of Shapes.VEC(p1, p2) =>
		  if Shapes.isOne p1
		  then FuseShapes.SINGLE_SEG((Shapes.getId p2, FuseShapes.dummyId),
					     cvtParam(env, p2))
		  else FuseShapes.SEG(Shapes.getId p2, FuseShapes.dummyId)
		| Shapes.UNIFORM_VEC(p1, p2) => FuseShapes.UNIFORM_SEG((Shapes.getId p1, Shapes.getId p2),
								       cvtParam(env, p1),
								       cvtParam(env, p2))
		| _ => raise Fail "invalid segdes shape"
	     (* end case *))
	   | FA.TySeq _ =>
	     (case flanShape
	       of Shapes.VEC(p1, p2) => FuseShapes.VEC(cvtParam(env, p2))
		| Shapes.UNIFORM_VEC(p1, p2) => FuseShapes.UNIFORM_VEC(cvtParam(env, p1),
								       cvtParam(env, p2))
		| _ => raise Fail "Invalid sequence shape"
	     (* end case *))
	   | FA.TyTuple ts => let
	       val Shapes.TUPLE shapes = flanShape
	     in
	       FuseShapes.TUPLE (ListPair.map (fn (s, t) => cvtShape(env, s, t)) (shapes, ts))
	     end
	(* end case *))

    fun cvtVarShape (env, flanV, fuseV) = let
      val flanShape = Shapes.getShape flanV
      val ty = cvtTy(FV.typeOf flanV)
    in
      FuseVar.setShape(fuseV, cvtShape(env, flanShape, ty))
    end

    fun cvtVar (env, flanV) = let
      val name = Atom.toString (FV.nameOf flanV)
      fun addLenToTy (FA.TySeq(tb, _), s) = FA.TySeq(tb, Shapes.getOptLength s)
	| addLenToTy (FA.TyTuple ts, Shapes.TUPLE shps) =
	  FA.TyTuple (ListPair.map addLenToTy (ts, shps))
	| addLenToTy (t, s) = t
      val ty = addLenToTy (cvtTy (FV.typeOf flanV), Shapes.getShape flanV)
      val fuseV = FuseVar.new(name, ty)
      val env' = E.bindLocalVar(env, flanV, fuseV)
    in
      (cvtVarShape (env', flanV, fuseV);
       (env', fuseV))
    end

    fun cvtVar' (env, []) = (env, [])
      | cvtVar' (env, v::vs) = let
	  val (env', v') = cvtVar (env, v)
	  val (env', vs') = cvtVar'(env', vs)
	in
	  (env', v'::vs')
	end

    fun cvtFunct (env, f) = let
          val (dom, rng) = FF.typeOf f
          val dom' = map cvtTy dom
          val rng' = cvtTy rng
          val name = FF.nameOf f
          val fuseF = FuseFunct.new(name, (dom', rng'))
          in
            E.bindFunct(env, f, fuseF);
            fuseF
          end

    fun mkKernel (env, scalarOp) = let
	  val arity = S.arity(scalarOp)
	  val (domTy, rngTy) = U.typeOfScalar(scalarOp)
	  val params = let
		fun mkParam i = FuseVar.new("arg"^Int.toString i, List.nth(domTy, i))
		in
		  List.tabulate(arity, mkParam)
		end
	  val result = FuseVar.new("result", rngTy)
	  val _ = List.app (fn a => FuseVar.setShape(a, FuseShapes.SCALAR)) params
	  val body = FA.mkKPure(result, scalarOp, List.map FA.Var params, FA.mkKReturn[FA.Var result])
	  val kern = Kernel.new (S.kernelName scalarOp, params, body, [rngTy])
	  val inst = FA.mkTopKern kern
	  in
	    E.bindKern(env, scalarOp, inst);
	    kern
	  end

  (* code to get the length of a vector data vector; we return (len, lenBind), where
   * len is an atom that specifies the length and lenBind is a (possibly empty)
   * binding list used to define len.
   *)
      fun mkGetLength v = (case FuseVar.typeOf v
	     of FA.TySeq(_, SOME n) => (FA.Int(IntInf.fromInt n), [])
	      | FA.TySeq(ty, NONE) => let
		  val len = FuseVar.new("width", FA.TyScalar TypeBase.INT)
		  in
		    (FA.Var len, [(len, FA.mkInternal(InternalOp.LENGTH ty, FA.Var v))])
		  end
	      | _ => raise Fail "mkGetLength: not data vector!"
	    (* end case *))

    (* We pass the shape of the expression so we can properly set the shapes
     * of the new variables introduced for RHS_Pure and RHS_Cmd.
     * This is necessary becuase ExpPure and ExpCmd are tail forms in Flan.
     *)
    fun cvtExp (env, exp, expShape) =
	(case exp
	  of F.ExpLet (binds, e) =>
	     cvtBinds (env, binds, e, expShape)
	   | F.ExpTuple (v, atms, e) => let
	       val atms' = cvtAtom' (env, atms)
	       val (env', v') = cvtVar (env, v)
	       val e' = cvtExp (env', e, expShape)
	     in
	       FA.mkRHS([v'], FA.mkTuple atms', e')
	     end
	   | F.ExpSeq (v, atms, e) => let
	       val atms' = cvtAtom' (env, atms)
	       (* flatSeq = [atm1, ..., atmn]
		* lengthSeq = SCALAR_TO_SEQ(n)
		* segdes = MAKE_SEGDES(lengthSeq)
		* v = (segdes, flatV)
		*)
	       val (env', v') = cvtVar (env, v)
	       val (tb, len) = (case FuseVar.typeOf v'
		       of FA.TyTuple[FA.TySegdes, FA.TySeq (tb, len)] => (tb, len)
			| _ => raise Fail(concat["expected pair type for ", FV.toString v])
		      (* end case *))
	       val flatV = FuseVar.new("flatSeq", FA.TySeq (tb, len))
	       val lengthSeq = FuseVar.new("lengthSeq", FA.TySeq (TypeBase.INT, SOME 1))
	       val segdes = FuseVar.new("segdes", FA.TySegdes)
	       val e' = cvtExp (env', e, expShape)
	       val FuseShapes.TUPLE[segShape, flatShape] = FuseVar.getShape v'
	       val _ = FuseVar.setShape(flatV, flatShape)
	       val _ = FuseVar.setShape(segdes, segShape)
	       val _ = FuseVar.setShape(lengthSeq, FuseShapes.VEC(FuseShapes.Known(FA.Int 1)))
	       val lenAtom = FA.Int(IntInf.fromInt(List.length(atms)))
	     in
	       U.mkRHS'([(flatV, FA.mkSeq (atms', tb)),
			 (lengthSeq, FA.mkInternal (I.SCALAR_TO_SEQ TypeBase.INT, lenAtom)),
			 (segdes, FA.mkInternal (I.MAKE_SEGDES, FA.Var lengthSeq)),
			 (v', FA.mkTuple [FA.Var segdes, FA.Var flatV])],
			e')
	     end
	   | F.ExpIf (a, e1, e2, _) => let
	       val a' = cvtAtom (env, a)
	       val e1' = cvtExp (env, e1, expShape)
	       val e2' = cvtExp (env, e2, expShape)
	     in
	       FA.mkIf(a', e1', e2')
	     end
	   | F.ExpPure (P.Base(Pure.PROJ i), [atm]) => let
	       val FA.Var v = cvtAtom (env, atm)
	       val FA.TyTuple tys = FuseVar.typeOf v
	       val resTy = List.nth(tys, (i - 1))
	       val shape = cvtShape(env, expShape, resTy)
	       val projResult = FuseVar.new("projResult", resTy)
	       val _ = FuseVar.setShape(projResult, shape)
	       val tailExp = FA.mkAtom(FA.Var projResult)
	     in
	       FA.mkRHS([projResult], FA.mkProj(i, v),
			FA.mkAtom(FA.Var projResult))
	     end
	   | F.ExpPure (p, atms) => let
	       val (_, resTy) = P.getTy p
	       val resTy' = cvtTy resTy
	       val shape = cvtShape(env, expShape, resTy')
	       val pureResult = FuseVar.new("pureResult", resTy')
	       val _ = FuseVar.setShape(pureResult, shape)
	       val tailExp = FA.mkAtom(FA.Var pureResult)
	     in
	       cvtPure(env, pureResult, exp, tailExp)
	     end
	   | F.ExpCmd (c, atms) => let
	       val (_, resTy) = FlanPrimTy.typeOfCmd c
	       val resTy' =
		   (case resTy
		     of [t] => cvtTy t
		      | ts => FA.TyTuple (map cvtTy ts)
		   (* end case *))
	       val shape = cvtShape(env, expShape, resTy')
	       val cmdResult = FuseVar.new("cmdResult", resTy')
	       val _ = FuseVar.setShape(cmdResult, shape)
	       val tailExp = FA.mkAtom(FA.Var cmdResult)
	     in
	       FA.mkRHS([cmdResult], cmdToRHS env exp, tailExp)
	     end
	   | F.ExpApplyFun (f, atms) => let
	       val f' = E.lookupFunct (env, f)
	       val atms' = cvtAtom' (env, atms)
	     in
	       FA.mkApply(f', atms')
	     end
	   | F.ExpAtom a => FA.mkAtom(cvtAtom(env, a))
	   | F.ExpLifted _ => raise Fail "ExpLifted"
	   | F.ExpForEach _ => raise Fail "Unexpected ExpForEach"
	(* end case *))

    and cvtBinds (env, [], exp, expShape) = cvtExp (env, exp, expShape)
      | cvtBinds (env, (var, e)::binds, exp, expShape) = let
	  val (env', var') = cvtVar (env, var)
	  val exp' = cvtBinds (env', binds, exp, expShape)
	in
	  (case e
	    of (F.ExpPure(P.Base(Pure.PROJ i), atms)) => let
		 val [F.Var v] = atms
		 val v' = E.lookupVar(env, v)
	       in
		 FA.mkRHS([var'],
			  FA.mkProj(i, v'),
			  exp')
	       end
	     | F.ExpPure _ => cvtPure(env, var', e, exp')
	     | F.ExpCmd _ => FA.mkRHS([var'], cmdToRHS env e, exp')
	     | _ => FA.mkLet([var'], cvtExp(env, e, Shapes.getShape var), exp')
	  (* end case *))
	end

    (* convert v = pureExp in tailExp
     * pureExp is a Flan expression, the others are already fuseAST
     *)
    and cvtPure (env, v, F.ExpPure(pureOp, atms), tailExp) = let
      val atms' = cvtAtom' (env, atms)
    in
      (case pureOp
	of P.Base p =>
	   (case cvtPureOp p
	     of (Scalar p', _) => FA.mkRHS([v], FA.mkScalar(p', atms'),
					   tailExp)
	      | (Gen p', _) => genToExp(v, p', atms', tailExp)
	      | (Scan p', t) => scanToExp(v, p', t, atms', tailExp)
	      | (Reduce p', t) =>
		reduceToExp (v, p', t, atms', tailExp)
	      | (Vector p', _) =>
		vectorToExp (v, p', atms', tailExp)
	      | (Int(I.LENGTH ty), _) => let

		  (* LENGTH : [:a:] -> int
		   * let (segdes, flatVec) = 
		   * let v = LENGTH flatVec
		   * in body
		   *)
		 val [FA.Var p] = atms'
		 val dataVec = FuseVar.new("data", FA.TySeq (ty, U.vecLength p))
		 in
		   U.mkRHS'([
		       (dataVec, FA.mkProj(2, p)),
		       (v, FA.mkInternal(I.LENGTH ty, FA.Var dataVec))
		     ],
		     tailExp)
		 end
	      | (Int p', _) => FA.mkRHS([v], FA.mkInternal(p', hd atms'), tailExp)
	      (* end case *))
	 | P.Lifted p =>
	   (case cvtPureOp p
	     of (Scalar p', _) => let
		  (* segdes = PROJ 1 v0
		   * flatV0 = PROJ 2 v0
		   * flatV1 = PROJ 2 v1
		   * ...
		   * flatResult = kern(flatArg0, ..., flatArgn)
		   * v = (segdes, flatResult)
		   *)
		  val vs = map (fn FA.Var v => v) atms'
		  val kern = scalarToKern (env, p')
		  val segdes = FuseVar.new("segdes", FA.TySegdes)
		  val flatVs = let
			fun varToFlatVar var = let
			      val name = FuseVar.name var
			      val FA.TyTuple [segTy, valTy] = FuseVar.typeOf var
			      in
				FuseVar.new("flat"^name, valTy)
			      end
			in
			  List.map varToFlatVar vs
			end
		  val (len, lenBind) = mkGetLength (hd flatVs) 
		  (* Figure out type of flatResult, and create it. *)
		  val FA.TyTuple [segTy, valTy] = FuseVar.typeOf v
		  val flatResult = FuseVar.new("flatResult", valTy)
		  (* Set flatResult's shape, based on result's shape *)
		  val FuseShapes.TUPLE[_, flatShape] = FuseVar.getShape v
		  val _ = FuseVar.setShape(flatResult, flatShape)
		  (* segdes = PROJ 1 arg0 *)
		  val segBind = (segdes, FA.mkProj(1, hd vs))
		  (* flatV0 = PROJ 2 v0 ... *)
		  val projBinds = ListPair.map
				    (fn (flatV, v) => (flatV, FA.mkProj(2, v)))
				    (flatVs, vs)
		(* flatResult = kern(flatArg0, ..., flatArgn) *)
		  val kernBind = (flatResult, FA.mkMap(kern, len, flatVs))
		(* v = (segdes, flatResult) *)
		  val resultBind = (v, FA.mkTuple [FA.Var segdes, FA.Var flatResult])
		  val binds = segBind::(projBinds@lenBind@[kernBind, resultBind])
		in
		  U.mkRHS'(binds, tailExp)
(*		  FA.mkRHS([v], FA.mkMap(kern, vs), tailExp) *)
		end
	      | (Gen p', _) => segGenToExp(v, p', atms', tailExp)
	      | (Scan p', ty) => segScanToExp(v, p', ty, atms', tailExp)
	      | (Reduce p', ty) => segReduceToExp(v, p', ty, atms', tailExp)
	      | (Vector p', _) => segVectorToExp(v, p', atms', tailExp)
	      | (Int _, _) => raise Fail "cannot lift internal ops"
	   (* end case *))
      (* end case *))
    end

    and scalarToKern (env, p) =
	(case E.lookupKern(env, p)
	  of SOME(FA.Top_Kern k) => k
	   | NONE => mkKernel (env, p)
	(* end case *))

    and genToExp(result, pure, atms, body) =
	(case pure
	  of G.INDEX => let
	       (* INDEX : (int, int, int) -> [:int:]
		* flatResult = INDEX(start, stride, len)
		* lengthSeq = SCALAR_TO_SEQ(len)
		* segdes = MAKE_SEGDES lengthSeq
		* result = (segdes, flatResult)
		* in body
		*)
	       val [_, _, lengthAtom] = atms
	       val flatResult = FuseVar.new("flatResult", FA.TySeq (TypeBase.INT, U.vecLength result))
	       val lengthSeq = FuseVar.new("lengthSeq", FA.TySeq (TypeBase.INT, SOME 1))
	       val segdes = FuseVar.new("segdes", FA.TySegdes)
	       val FuseShapes.TUPLE[segShape, flatShape] = FuseVar.getShape result
	       val _ = FuseVar.setShape(flatResult, flatShape)
	       val _ = FuseVar.setShape(lengthSeq, FuseShapes.mkSingletonVec lengthAtom)
	       val _ = FuseVar.setShape(segdes, segShape)
	     in
	       U.mkRHS'([(flatResult, FA.mkFlatGen(G.INDEX, lengthAtom, atms)),
			  (lengthSeq, FA.mkInternal(I.SCALAR_TO_SEQ TypeBase.INT, lengthAtom)),
			  (segdes, FA.mkInternal(I.MAKE_SEGDES, FA.Var lengthSeq)),
			  (result, FA.mkTuple[FA.Var segdes, FA.Var flatResult])],
			 body)
	     end
	   | G.DIST ty => let
	       (* DIST : (a, int) -> [:a:]
		* flatResult = DIST(v, len)
		* lengthSeq = SCALAR_TO_SEQ(len)
		* segdes = MAKE_SEGDES (length_seq)
		* result = (segdes, flatResult)
		* in body
		*)
	       val [v, len] = atms
	       val FuseShapes.TUPLE[segShape, flatShape] = FuseVar.getShape result
	       val flatResult = FuseVar.new("flatResult", FA.TySeq (ty, U.vecLength result))
	       val lengthSeq = FuseVar.new("lengthSeq", FA.TySeq (TypeBase.INT, SOME 1))
	       val segdes = FuseVar.new("segdes", FA.TySegdes)
	       val _ = FuseVar.setShape(flatResult, flatShape)
	       val _ = FuseVar.setShape(lengthSeq, FuseShapes.mkSingletonVec len)
	       val _ = FuseVar.setShape(segdes, segShape)
	     in
	       U.mkRHS'([(flatResult, FA.mkFlatGen(G.DIST ty, len, atms)),
			  (lengthSeq, FA.mkInternal(I.SCALAR_TO_SEQ TypeBase.INT, len)),
			  (segdes, FA.mkInternal(I.MAKE_SEGDES, FA.Var lengthSeq)),
			  (result, FA.mkTuple[FA.Var segdes, FA.Var flatResult])],
			 body)
	     end
	(* end case *))

    and segGenToExp(result, pure, atms, body) =
	(case pure
	  (* segmented INDEX: ([:int:], [:int:], [:int:]) -> [:int:]
	   * let (outerSeg, flatStarts) = starts
	   * let (_, flatStrides) = strides
	   * let (_, flatLengths) = lengths
	   * let flatResult = INDEX(flatStarts, flatStrides, flatLengths)
	   * let innerSeg = MAKE_SEGDES (flatLengths)
	   * let result = (outerSeg, (innerSeg, flatResult))
	   * in body
	   *)
	  of G.INDEX => let
	       val [FA.Var starts, FA.Var strides, FA.Var lengths] = atms
	       val tyInt = FA.TySeq (TypeBase.INT, U.vecLength starts)
	       val tyResultInt = FA.TySeq(TypeBase.INT, U.segVecLength result)
	       val outerSeg = FuseVar.new("outerSeg", FA.TySegdes)
	       val flatStarts = FuseVar.new("flatStarts", tyInt)
	       val flatStrides = FuseVar.new("flatStrides", tyInt)
	       val flatLengths = FuseVar.new("flatLengths", tyInt)
(*	       val (segCnt, segBinds) = mkGetLength flatLengths *)
	       val width = FuseVar.new("width", FA.TyScalar TypeBase.INT)
	       val flatResult = FuseVar.new("flatResult", tyResultInt)
	       val innerSeg = FuseVar.new("innerSeg", FA.TySegdes)
	       val innerResult = FuseVar.new("innerResult", FA.TyTuple [FA.TySegdes, tyResultInt])
	       (* Set shapes of new variables *)
	       val FuseShapes.TUPLE[seg1, FuseShapes.TUPLE[seg2, v]] = FuseVar.getShape result
	       val _ = FuseVar.setShape(flatResult, v)
	       val _ = FuseVar.setShape(innerSeg, seg2)
	     in
	       U.mkRHS'((outerSeg, FA.mkProj(1, starts))::
			(flatStarts, FA.mkProj(2, starts))::
			(flatStrides, FA.mkProj(2, strides))::
			(flatLengths, FA.mkProj(2, lengths))::
			[(innerSeg, FA.mkInternal(I.MAKE_SEGDES, FA.Var flatLengths)),
			(* sum up our lengths to find the width *)
			 (width, FA.mkInternal(I.SUM_LENGTHS, FA.Var innerSeg)),
			 (flatResult, FA.mkSegGen(G.INDEX, FA.Var width, [flatStarts,
									  flatStrides,
									  flatLengths])),
			 (innerResult, FA.mkTuple [FA.Var innerSeg, FA.Var flatResult]),
			 (result, FA.mkTuple [FA.Var outerSeg, FA.Var innerResult])],
			body)
	     end
	   | G.DIST ty => let
	       (* segmented DIST: ([:a:], [:int:]) -> [:a:]
		* let (outerSeg, length) = val
		* let (_, flatLength) = length
		* let flatResult = DIST(flatVal, flatLen)
		* let innerSeg = MAKE_SEGDES(flatLength)
		* let result = (outerSeg, (innerSeg, flatResult))
		* in body
		*)
	       val [FA.Var v, FA.Var len] = atms
	       val tySeq = FA.TySeq (ty, U.vecLength v)
	       val tyResultSeq = FA.TySeq(ty, U.segVecLength result)
	       val outerSeg = FuseVar.new("outerSeg", FA.TySegdes)
	       val flatVal = FuseVar.new("flatVal", tySeq)
	       val flatLength = FuseVar.new("flatLength", FA.TySeq(TypeBase.INT, U.vecLength len))
(*	       val (segCnt, segBinds) = mkGetLength flatLength *)
	       val width = FuseVar.new("width", FA.TyScalar TypeBase.INT)
	       val flatResult = FuseVar.new("flatVal", tyResultSeq)
	       val innerSeg = FuseVar.new("innerSeg", FA.TySegdes)
	       val innerResult = FuseVar.new("innerResult", FA.TyTuple [FA.TySegdes, tyResultSeq])
	       (* Set shapes *)
	       val FuseShapes.TUPLE[seg1, FuseShapes.TUPLE[seg2, data]] = FuseVar.getShape result
	       val _ = FuseVar.setShape(flatResult, data)
	       val _ = FuseVar.setShape(innerSeg, seg2)
	     in
	       U.mkRHS'((outerSeg, FA.mkProj(1, v))::
			(flatVal, FA.mkProj(2, v))::
			(flatLength, FA.mkProj(2, len))::
			[(innerSeg, FA.mkInternal(I.MAKE_SEGDES, FA.Var flatLength)),
			 (width, FA.mkInternal(I.SUM_LENGTHS, FA.Var innerSeg)),
			 (flatResult, FA.mkSegGen(G.DIST ty, FA.Var width, [flatVal, flatLength])),
			 
			 (innerResult, FA.mkTuple [FA.Var innerSeg, FA.Var flatResult]),
			 (result, FA.mkTuple [FA.Var outerSeg, FA.Var innerResult])],
			body)
	     end
	(* end case *))

    and scanToExp (result, pure, SOME baseTy, atms, body) = let
      (* scanOp : [:a:] -> [:a:]
       * let (segdes, flatVec) = atm
       * let flatResult = pure(flatVec)
       * let result = (segdes, flatResult)
       * in body
       *)
      val [FA.Var v] = atms
      val tySeq = FA.TySeq(baseTy, U.vecLength v)
      val segdes = FuseVar.new("segdes", FA.TySegdes)
      val flatVec = FuseVar.new("flatVec", tySeq)
      val (len, lenBind) = mkGetLength flatVec
      val flatResult = FuseVar.new("flatResult", tySeq)
      val _ = (FuseVar.setShape(flatResult, U.getFlatShape result))
    in
      U.mkRHS'(
	(segdes, FA.mkProj(1, v)) ::
	(flatVec, FA.mkProj(2, v)) ::
	lenBind @ [
	    (flatResult, FA.mkFlatScan(pure, len, flatVec)),
	    (result, FA.mkTuple[FA.Var segdes, FA.Var flatResult])
	  ],
	body)
    end

    and segScanToExp (result, pure, SOME baseTy, atms, body) = let
      (* segScanOp : (segdes, [:a:]) -> [:a:]
       * let (outerSeg, (innerSeg, flatVec)) = atm
       * let flatResult = pure(innerSeg, flatVec)
       * let innerResult = (innerseg, flatResult)
       * let result = (outerSeg, innerResult)
       * in body
       *)
      val [FA.Var v] = atms
      val len = U.segVecLength v
      val innerTy = U.seqTy (baseTy, len)
      val outerSeg = FuseVar.new("outerSeg", FA.TySegdes)
      val innerVec = FuseVar.new("innerVec", innerTy)
      val innerSeg = FuseVar.new("innerSeg", FA.TySegdes)
      val flatVec = FuseVar.new("flatVec", FA.TySeq (baseTy, len))
      val flatResult = FuseVar.new("flatResult", FA.TySeq (baseTy, len))
      val _ = FuseVar.setShape(flatResult, U.getInnerFlatShape result)
      val innerResult = FuseVar.new("innerResult", innerTy)
      val (len, lenBind) = mkGetLength flatVec
    in
      U.mkRHS'(
	(outerSeg, FA.mkProj(1, v)) ::
	(innerVec, FA.mkProj(2, v)) ::
	(innerSeg, FA.mkProj(1, innerVec)) ::
	(flatVec, FA.mkProj(2, innerVec)) ::
	(flatResult, FA.mkSegScan(pure, len, innerSeg, flatVec)) ::
	lenBind @ [
	    (innerResult, FA.mkTuple[FA.Var innerSeg, FA.Var flatResult]),
	    (result, FA.mkTuple[FA.Var outerSeg, FA.Var innerResult])
	  ],
	body)
    end

    and reduceToExp (result, pure, SOME baseTy, atms, body) = let
      (* reduceOp : [:a:] -> a
       * let (_, flatVec) = atm
       * let result = pure(flatVec)
       * in body
       *)
      val [FA.Var v] = atms
      val flatVec = FuseVar.new("flatVec", FA.TySeq (baseTy, U.vecLength v))
      val (len, lenBind) = mkGetLength flatVec
    in
      U.mkRHS'(
	(flatVec, FA.mkProj(2, v)) ::
	lenBind@[(result, FA.mkFlatReduce(pure, len, flatVec))],
	body)
    end

    and segReduceToExp (result, pure, SOME baseTy, atms, body) = let
      (* segReduceOp : (segdes, [:a:]) -> [:a:]
       * let (outerSeg, (innerSeg, flatVec)) = atm
       * let flatResult = pure(innerSeg, flatVec)
       * let result = (outerSeg, flatResult)
       * in body
       *)
      val [FA.Var v] = atms
      val len = U.segVecLength v
      val innerTy = U.seqTy (baseTy, len)
      val outerSeg = FuseVar.new("outerSeg", FA.TySegdes)
      val innerVec = FuseVar.new("innerVec", innerTy)
      val innerSeg = FuseVar.new("innerSeg", FA.TySegdes)
      val flatVec = FuseVar.new("flatVec", FA.TySeq (baseTy, len))
      val flatResult = FuseVar.new("flatResult", FA.TySeq (baseTy, len))
      val _ = FuseVar.setShape(flatResult, U.getFlatShape result)
      val (wid, widBind) = mkGetLength flatVec
    in
      U.mkRHS'(
	(outerSeg, FA.mkProj(1, v)) ::
	(innerVec, FA.mkProj(2, v)) ::
	(innerSeg, FA.mkProj(1, innerVec)) ::
	(flatVec, FA.mkProj(2, innerVec)) ::
	widBind @ [
	    (flatResult, FA.mkSegReduce(pure, wid, innerSeg, flatVec)),
	    (result, FA.mkTuple [FA.Var outerSeg, FA.Var flatResult])
	  ],
	body)
    end

    and vectorToExp (result, pure, atms, body) =
	(case pure
	  of V.PERMUTE ty => let
	       (* PERMUTE : ([:a:], [:int:]) -> [:a:]
		* let (segdes, flatVals) = vals
		* let flatIdx = #2 idxs
		* let flatResult = Permute(flatVals, flatIdx)
		* let result = (segdes, flatResult)
		* in body
		*)
	       val [FA.Var vals, FA.Var idxs] = atms
	       val len = U.vecLength vals
	       val segdes = FuseVar.new("segdes", FA.TySegdes)
	       val flatVals = FuseVar.new("flatVals", FA.TySeq (ty, len))
	       val flatIdx = FuseVar.new("flatIdx", FA.TySeq (TypeBase.INT, len))
	       val flatResult = FuseVar.new("flatResult", FA.TySeq (ty, len))
	       val _ = FuseVar.setShape(flatResult, U.getFlatShape result)
	     in
	       U.mkRHS'([(segdes, FA.mkProj(1, vals)),
			 (flatVals, FA.mkProj(2, vals)),
			 (flatIdx, FA.mkProj(2, idxs)),
			 (flatResult, FA.mkFlatVector(V.PERMUTE ty, [FA.Var flatVals, FA.Var flatIdx])),
			 (result, FA.mkTuple[FA.Var segdes, FA.Var flatResult])],
			body)
	     end
	   | V.DPERMUTE ty => let
	       (* DPERMUTE : ([:a:], [:int:], [:a:]) -> [:a:]
		* let (segdes, flatDef) = def
		* let flatVals = #2 vals
		* let flatIdx = #2 idxs
		* let flatResult = DPERMUTE(flatVals, flatIdx, flatDef)
		* let result = (segdes, flatResult)
		* in body
		*)
	       val [FA.Var vals, FA.Var idxs, FA.Var def] = atms
	       val valLen = U.vecLength vals
	       val resLen = U.vecLength result
	       val segdes = FuseVar.new("segdes", FA.TySegdes)
	       val flatVals = FuseVar.new("flatVals", FA.TySeq (ty, valLen))
	       val flatIdx = FuseVar.new("flatIdx", FA.TySeq (TypeBase.INT, valLen))
	       val flatDef = FuseVar.new("flatDef", FA.TySeq (ty, resLen))
	       val flatResult = FuseVar.new("flatResult", FA.TySeq (ty, resLen))
	       val _ = FuseVar.setShape(flatResult, U.getFlatShape result)
	     in
	       U.mkRHS'([(segdes, FA.mkProj(1, def)),
			 (flatVals, FA.mkProj(2, vals)),
			 (flatIdx, FA.mkProj(2, idxs)),
			 (flatDef, FA.mkProj(2, def)),
			 (flatResult, FA.mkFlatVector(V.DPERMUTE ty, [FA.Var flatVals, FA.Var flatIdx, FA.Var flatDef])),
			 (result, FA.mkTuple[FA.Var segdes, FA.Var flatResult])],
			body)
	     end
	   | V.FPERMUTE ty => raise Fail "FIXME: FPERMUTE"
	   | V.BPERMUTE ty => let
	       (* BPERMUTE : ([:a:], [:int:]) -> [:a:]
		* let (segdes, flatIdx) = idxs
		* let flatVals = #2 vals
		* let flatResult = BPERMUTE(flatVals, flatIdx)
		* let result = (segdes, flatResult)
		* in body
		*)
	       val [FA.Var vals, FA.Var idxs] = atms
	       val idxLen = U.vecLength idxs
	       val segdes = FuseVar.new("segdes", FA.TySegdes)
	       val flatVals = FuseVar.new("flatVals", FA.TySeq (ty, U.vecLength vals))
	       val flatIdx = FuseVar.new("flatIdx", FA.TySeq (TypeBase.INT, idxLen))
	       val flatResult = FuseVar.new("flatResult", FA.TySeq (ty, idxLen))
	       val _ = FuseVar.setShape(flatResult, U.getFlatShape result)
	     in
	       U.mkRHS'([(segdes, FA.mkProj(1, idxs)),
			 (flatVals, FA.mkProj(2, vals)),
			 (flatIdx, FA.mkProj(2, idxs)),
			 (flatResult, FA.mkFlatVector(V.BPERMUTE ty, [FA.Var flatVals, FA.Var flatIdx])),
			 (result, FA.mkTuple[FA.Var segdes, FA.Var flatResult])],
			body)
	     end
	   | V.BFPERMUTE ty => raise Fail "Fixme: BFPERMUTE"
	   | V.DFPERMUTE ty => raise Fail "Fixme: DFPERMUTE"
	   | V.EXTRACT ty => let
	       (* EXTRACT: ([:a:], int) -> a
		* let flatVec = #2 v
		* let result = EXTRACT(flatVec, idx)
		* in body
		*)
	       val [(FA.Var v), idx]  = atms
	       val flatVec = FuseVar.new("flatVec", FA.TySeq (ty, U.vecLength v))
	     in
	       U.mkRHS'([(flatVec, FA.mkProj(2, v)),
			 (result, FA.mkFlatVector(V.EXTRACT ty, [FA.Var flatVec, idx]))],
			body)
	     end
	   | V.REPLACE ty => let
	     (* REPLACE : ([:a:], i, a) -> [:a:]
	      * let (segdes, flatVec) = v
	      * let flatResult = REPLACE(flatVec, idx, elt)
	      * let result = (segdes, flatResult)
	      * in body
	      *)
	       val [FA.Var v, idx, elt] = atms
	       val len = U.vecLength v
	       val segdes = FuseVar.new("segdes", FA.TySegdes)
	       val flatVec = FuseVar.new("flatVec", FA.TySeq (ty, len))
	       val flatResult = FuseVar.new("flatResult", FA.TySeq (ty, len))
	       val _ = FuseVar.setShape(flatResult, U.getFlatShape result)
	     in
	       U.mkRHS'([(segdes, FA.mkProj(1, v)),
			 (flatVec, FA.mkProj(2, v)),
			 (flatResult, FA.mkFlatVector(V.REPLACE ty, [FA.Var flatVec, idx, elt])),
			 (result, FA.mkTuple[FA.Var segdes, FA.Var flatResult])],
			body)
	     end
	   | V.PACK ty => let
	       (* PACK : ([:a:], [:bool:]) -> (segdes, [:a:]) *)
	       val [FA.Var vals, FA.Var flags] = atms
	       val len = U.vecLength vals
	       val resLen = U.vecLength result
	       val flatVec = FuseVar.new("flatVec", FA.TySeq (ty, len))
	       val flatFlags = FuseVar.new("flatFlags", FA.TySeq (TypeBase.BOOL, len))
	     in
	       U.mkRHS'([(flatVec, FA.mkProj(2, vals)),
			 (flatFlags, FA.mkProj(2, flags)),
			 (result, FA.mkFlatVector(V.PACK ty, [FA.Var flatVec, FA.Var flatFlags]))],
			body)
	     end
	   | _ => raise Fail "FIXME : vector op"
	(* end case *))


    and segVectorToExp (result, pure, atms, body) =
	(case pure
	  of V.PERMUTE ty => let
	       (* Segmented PERMUTE : (segdes, [:a:], [:int:]) => [:a:]
		* let (outerSeg, (innerSeg, flatVals)) = vals
		* let (_, (_, flatIdx)) = idxs
		* let flatResult = SegVectorOp(PERMUTE, innerSeg, flatVals, flatIdx)
		* let innerResult = (innerSeg, flatResult)
		* let result = (outerSeg, innerResult)
		* in body
		*)
	       val [FA.Var vals, FA.Var idxs] = atms
	       val len = U.segVecLength vals
	       val valTy = U.seqTy (ty, len)
	       val idxTy = U.seqTy (TypeBase.INT, len)
	       val outerSeg = FuseVar.new("outerSeg", FA.TySegdes)
	       val innerSeg = FuseVar.new("innerSeg", FA.TySegdes)
	       val innerVals = FuseVar.new("innerVals", valTy)
	       val innerIdxs = FuseVar.new("innerIdxs", idxTy)
	       val flatVals = FuseVar.new("flatVals", FA.TySeq (ty, len))
	       val flatIdx = FuseVar.new("flatIdx", FA.TySeq (TypeBase.INT, len))
	       val flatResult = FuseVar.new("flatResult", FA.TySeq (ty, U.segVecLength result))
	       val _ = FuseVar.setShape(flatResult, U.getInnerFlatShape result)
	       val innerResult = FuseVar.new("innerResult", valTy)
	     in
	       U.mkRHS'([(outerSeg, FA.mkProj(1, vals)),
			 (innerVals, FA.mkProj(2, vals)),
			 (innerSeg, FA.mkProj(1, innerVals)),
			 (flatVals, FA.mkProj(2, innerVals)),
			 (innerIdxs, FA.mkProj(2, idxs)),
			 (flatIdx, FA.mkProj(2, innerIdxs)),
			 (flatResult, FA.mkSegVector(V.PERMUTE ty, [FA.Var innerSeg, FA.Var flatVals, FA.Var flatIdx])),
			 (innerResult, FA.mkTuple[FA.Var innerSeg, FA.Var flatResult]),
			 (result, FA.mkTuple[FA.Var outerSeg, FA.Var innerResult])],
			body)
	     end
	   | V.DPERMUTE ty => let
	       (* Segmented DPERMUTE : (segdes, [:a:], [:int:], segdes, [:a:]) -> [:a:]
		* let (outerSeg, (innerValSeg, flatVals)) = vals
		* let (_, (_, flatIdx)) = idxs
		* let (_, (innerDefSeg, flatDef)) = def
		* let flatResult = segmented DPERMUTE (innerValSeg, flatVals, flatIdx, innerDefSeg, flatDef)
		* let innerResult = (innerDefSeg, flatResult)
		* let result = (segdes, innerResult)
		*)
	       val [FA.Var vals, FA.Var idxs, FA.Var def] = atms
	       val valLen = U.segVecLength vals
	       val resLen = U.segVecLength result
	       val valTy = U.seqTy (ty, valLen)
	       val idxTy = U.seqTy (TypeBase.INT, valLen)
	       val outerSeg = FuseVar.new("outerSeg", FA.TySegdes)
	       val innerVals = FuseVar.new("innerVals", valTy)
	       val innerSeg = FuseVar.new("innerSeg", FA.TySegdes)
	       val flatVals = FuseVar.new("flatVals", FA.TySeq (ty, valLen))
	       val innerIdx = FuseVar.new("innerIdx", idxTy)
	       val flatIdx = FuseVar.new("flatIdx", FA.TySeq (TypeBase.INT, valLen))
	       val innerDef = FuseVar.new("innerDef", valTy)
	       val innerDefSeg = FuseVar.new("innerDefSeg", FA.TySegdes)
	       val flatDef = FuseVar.new("flatDef", FA.TySegdes)
	       val flatResult = FuseVar.new("flatResult", FA.TySeq (ty, resLen))
	       val _ = FuseVar.setShape(flatResult, U.getInnerFlatShape result)
	       val innerResult = FuseVar.new("innerResult", valTy)
	     in
	       U.mkRHS'([(outerSeg, FA.mkProj(1, vals)),
			  (innerVals, FA.mkProj(2, vals)),
			  (innerSeg, FA.mkProj(1, innerVals)),
			  (flatVals, FA.mkProj(2, innerVals)),
			  (innerIdx, FA.mkProj(2, idxs)),
			  (flatIdx, FA.mkProj(2, innerIdx)),
			  (innerDef, FA.mkProj(2, def)),
			  (innerDefSeg, FA.mkProj(1, innerDef)),
			  (flatDef, FA.mkProj(2, innerDef)),
			  (flatResult, FA.mkSegVector(V.DPERMUTE ty, [FA.Var innerSeg, FA.Var flatVals, FA.Var flatIdx, FA.Var innerDefSeg, FA.Var flatDef])),
			  (innerResult, FA.mkTuple[FA.Var innerDefSeg, FA.Var flatResult]),
			  (result, FA.mkTuple[FA.Var outerSeg, FA.Var innerResult])],
		       body)
	     end
	   | V.FPERMUTE ty => raise Fail "FIXME : FPERMUTE"
	   | V.BPERMUTE ty => let
	       (* Segmented BPERMUTE : (segdes, [:a:], segdes, [:int:]) -> [:a:]
		* let (outerSeg, (innerValSeg, flatVals)) = vals
		* let (_, (innerIdxSeg, flatIdxs)) = idxs
		* let flatResult = segmented BPERMUTE (innerValSeg, flatVals, innerIdxSeg, flatIdxs)
		* let innerResult = (innerIdxSeg, flatResult)
		* let result = (outerSeg, innerResult)
		*)
	       val [FA.Var vals, FA.Var idxs] = atms
	       val valLen = U.segVecLength vals
	       val valTy = U.seqTy (ty, valLen)
	       val idxLen = U.segVecLength idxs
	       val idxTy = U.seqTy (TypeBase.INT, idxLen)
	       val outerSeg = FuseVar.new("outerSeg", FA.TySegdes)
	       val innerVals = FuseVar.new("innerVals", valTy)
	       val innerSeg = FuseVar.new("innerSeg", FA.TySegdes)
	       val flatVals = FuseVar.new("flatVals", FA.TySeq (ty, valLen))
	       val innerIdxs = FuseVar.new("innerIdxs", idxTy)
	       val innerIdxSeg = FuseVar.new("innerIdxSeg", FA.TySegdes)
	       val flatIdx = FuseVar.new("flatIdx", FA.TySeq (TypeBase.INT, idxLen))
	       val flatResult = FuseVar.new("flatResult", FA.TySeq (ty, U.segVecLength result))
	       val _ = FuseVar.setShape(flatResult, U.getInnerFlatShape result)
	       val innerResult = FuseVar.new("innerResult", valTy)
	     in
	       U.mkRHS'([(outerSeg, FA.mkProj(1, vals)),
			 (innerVals, FA.mkProj(2, vals)),
			 (innerSeg, FA.mkProj(1, innerVals)),
			 (flatVals, FA.mkProj(2, innerVals)),
			 (innerIdxs, FA.mkProj(2, idxs)),
			 (innerIdxSeg, FA.mkProj(1, innerIdxs)),
			 (flatIdx, FA.mkProj(2, innerIdxs)),
			 (flatResult, FA.mkSegVector(V.BPERMUTE ty, [FA.Var innerSeg, FA.Var flatVals, FA.Var innerIdxSeg, FA.Var flatIdx])),
			 (innerResult, FA.mkTuple [FA.Var innerIdxSeg, FA.Var flatResult]),
			 (result, FA.mkTuple [FA.Var outerSeg, FA.Var innerResult])],
			body)
	     end
	   | V.EXTRACT ty => let
	       (* Segmented EXTRACT : (segdes, [:a:], [:int:]) -> [:a:]
		* let (outerSeg, (innerSeg, flatVals)) = vals
		* let flatIdxs = #2 idxs
		* let flatResult = segmented EXTRACT(innerSeg, flatVals, flatIdxs)
		* let result = (outerSeg, flatResult)
		*)
	       val [FA.Var vals, FA.Var idxs] = atms
	       val valTy = U.seqTy (ty, U.segVecLength vals)
	       val outerSeg = FuseVar.new("outerSeg", FA.TySegdes)
	       val innerVals = FuseVar.new("innerVals", valTy)
	       val innerSeg = FuseVar.new("innerSeg", FA.TySegdes)
	       val flatVals = FuseVar.new("flatVals", FA.TySeq (ty, U.segVecLength vals))
	       val flatIdxs = FuseVar.new("flatIdxs", FA.TySeq (TypeBase.INT, U.vecLength idxs))
	       val flatResult = FuseVar.new("flatResult", FA.TySeq (ty, U.segVecLength result))
	       val _ = FuseVar.setShape(flatResult, U.getFlatShape result)
	     in
	       U.mkRHS'([(outerSeg, FA.mkProj(1, vals)),
			  (innerVals, FA.mkProj(2, vals)),
			  (innerSeg, FA.mkProj(1, innerVals)),
			  (flatVals, FA.mkProj(2, innerVals)),
			  (flatIdxs, FA.mkProj(2, idxs)),
			  (flatResult, FA.mkSegVector(V.EXTRACT ty, [FA.Var innerSeg, FA.Var flatVals, FA.Var flatIdxs])),
			  (result, FA.mkTuple[FA.Var outerSeg, FA.Var flatResult])],
			 body)
	     end
	   | V.REPLACE ty => let
	       (* Segmented REPLACE : (segdes, [:a:], [:int:], [:a:]) -> [:a:]
		* let (outerSeg, (innerSeg, flatVals)) = vals
		* let flatIdxs = #2 idxs
		* let flatElts = #2 elts
		* let flatResult = segmented REPLACE(innerSeg, flatVals, flatIdxs, flatElts)
		* let innerResult = (innerSeg, result)
		* let result = (outerSeg, innerResult)
		*)
	       val [FA.Var vals, FA.Var idxs, FA.Var elts] = atms
	       val valTy = U.seqTy (ty, U.segVecLength vals)
	       val outerSeg = FuseVar.new("outerSeg", FA.TySegdes)
	       val innerVals = FuseVar.new("innerVals", valTy)
	       val innerSeg = FuseVar.new("innerSeg", FA.TySegdes)
	       val flatVals = FuseVar.new("flatVals", FA.TySeq (ty, U.segVecLength vals))
	       val flatIdxs = FuseVar.new("flatIdxs", FA.TySeq (TypeBase.INT, U.vecLength idxs))
	       val flatElts = FuseVar.new("flatElts", FA.TySeq (ty, U.vecLength elts))
	       val flatResult = FuseVar.new("flatResult", FA.TySeq (ty, U.vecLength result))
	       val _ = FuseVar.setShape(flatResult, U.getInnerFlatShape result)
	       val innerResult = FuseVar.new("innerResult", valTy)
	     in
	       U.mkRHS'([(outerSeg, FA.mkProj(1, vals)),
			 (innerVals, FA.mkProj(2, vals)),
			 (innerSeg, FA.mkProj(1, innerVals)),
			 (flatVals, FA.mkProj(2, innerVals)),
			 (flatIdxs, FA.mkProj(2, idxs)),
			 (flatElts, FA.mkProj(2, elts)),
			 (flatResult, FA.mkSegVector(V.REPLACE ty, [FA.Var innerSeg, FA.Var flatVals, FA.Var flatIdxs, FA.Var flatElts])),
			 (innerResult, FA.mkTuple[FA.Var innerSeg, FA.Var flatResult]),
			 (result, FA.mkTuple[FA.Var outerSeg, FA.Var innerResult])],
			body)
	     end
	   | V.PACK ty => let
	       (* Segmented PACK : (segdes, [:a:], [:bool:]) -> (segdes, [:a:])
		* let (outerSeg, (innerSeg, flatVals)) = vals
		* let (_, (_, flatFlags)) = flags
		* let innerResult = PACK(innerSeg, flatVals, flatFlags)
		* let result = (outerSeg, innerResult)
		* in body
		*)
	       val [FA.Var vals, FA.Var flags] = atms
	       val valTy = U.seqTy (ty, U.segVecLength vals)
	       val flagTy = U.seqTy (TypeBase.BOOL, U.segVecLength vals)
	       val outerSeg = FuseVar.new("outerSeg", FA.TySegdes)
	       val innerVals = FuseVar.new("innerVals", valTy)
	       val innerSeg = FuseVar.new("innerSeg", FA.TySegdes)
	       val flatVals = FuseVar.new("flatVals", FA.TySeq (ty, U.segVecLength vals))
	       val innerFlags = FuseVar.new("innerFlags", flagTy)
	       val flatFlags = FuseVar.new("flatFlags", FA.TySeq (TypeBase.BOOL, U.segVecLength flags))
	       val innerResult = FuseVar.new("innerResult", valTy)
	       (* Setting shapes *)
	       val FuseShapes.TUPLE[seg1, innerShape] = FuseVar.getShape result
	       val _ = FuseVar.setShape(innerResult, innerShape)
	     in
	       U.mkRHS'([(outerSeg, FA.mkProj(1, vals)),
			 (innerVals, FA.mkProj(2, vals)),
			 (innerSeg, FA.mkProj(1, innerVals)),
			 (flatVals, FA.mkProj(2, innerVals)),
			 (innerFlags, FA.mkProj(2, flags)),
			 (flatFlags, FA.mkProj(2, innerFlags)),
			 (innerResult, FA.mkSegVector(V.PACK ty, [FA.Var innerSeg, FA.Var flatVals, FA.Var flatFlags])),
			 (result, FA.mkTuple [FA.Var outerSeg, FA.Var innerResult])],
			body)
	     end
	   | _ => raise Fail "FIXME: segVectorToExp"
	(* end case *))


    and cmdToRHS env (F.ExpCmd(c, atms)) = let
      (* FIXME: some commands also need to be changed to not take segment descriptors.
       * but none of our benchmarks use those. *)
      val atms' = cvtAtom' (env, atms)
    in
      FA.mkCmd(c, atms')
    end

    fun transform (F.Program tops) = let
      val fuseEnv = E.new()
      (* Because function bindings are not declare-before-use,
       * we add top-level bindings to environment first,
       * then convert the rest of the program. *)
      fun addTop (F.TopFun (_, insts)) =
	  (List.map (fn (f, _, _) => cvtFunct(fuseEnv, f)) insts;
	   ())
	| addTop (F.TopBind (v, e)) = let
	    val (_, v') = cvtVar(fuseEnv, v)
	  in
	    E.bindGlobalVar(fuseEnv, v, v')
	  end
	| addTop (F.TopExp _) = ()
      fun doTops ((F.TopFun (_, insts)), fuseTops) = let
	fun doInst((f, args, e), tops) = let
	  val f' = E.lookupFunct(fuseEnv, f)
	  val (fuseEnv', args') = cvtVar'(fuseEnv, args)
	  val (_, resultShape) = Shapes.getFunShape f
	  val e' = cvtExp (fuseEnv', e, resultShape)
	in
	  ((FA.mkTopFun(f', args', e'))::tops)
	end
      in
	List.foldl doInst fuseTops insts
      end
	| doTops ((F.TopBind (v, e)), fuseTops) = let
	    val v' = E.lookupVar(fuseEnv, v)
	    val shape = Shapes.getShape v
	    val e' = cvtExp (fuseEnv, e, Shapes.getShape v)
	  in
	    ((FA.mkTopLet([v'], e'))::fuseTops)
	  end
	| doTops ((top as F.TopExp(e, ty, p)), fuseTops) = let
	    val shape = Shapes.getExpShape top
	    val e' = cvtExp (fuseEnv, e, shape)
	  in
	    (FA.mkTopExp(e', cvtTy ty))::fuseTops
	  end
      val _ = List.app addTop tops
      val fuseTops = List.foldl doTops [] tops
      val fuseTops = List.rev fuseTops
      val kernTops = E.listKerns fuseEnv
    in
      FA.Program(kernTops@fuseTops)
    end

end



