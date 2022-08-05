(* shapes.sml
 * 
 * COPYRIGHT (c) Nora Sandler (nlsandler@uchicago.edu)
 * All rights reserved.
 *
 *)

structure Shapes =
  struct

  structure F = Flan
  structure V = FlanVar
  structure T = FlanTypes

  val debug = false

  fun pr s = if debug
	     then print s
	     else ()

  datatype param_kind
    (* these atoms should only be bound to constant scalars  *)
    = Known of F.atom
    (*     | Known_List of F.atm list *)
    | Single
    | Unknown
      
  and param = P of {
      id : Stamp.stamp,
      kind : param_kind ref,
      equals : param list ref,          (* Other parameters it's been set equal to. *)
      unifiedTo : param option ref      (* Parameter it's been unified with, if any *)
  }
		   
  and shape = VEC of param * param (* Can apply to segment descriptors or flat vectors: 
				    * first param is length (so it's always a scalar),
				    * second param is its value (which may be single or known if the vec has length 1, 
				    * and unknown otherwise *)
	    | UNIFORM_VEC of param * param (* Can apply to segment descriptors or flat vectors:
					    * first param is length, second is value of each element,
					    * so both are scalars
					    * e.g. UNIFORM_VEC(3, 4) would be [4, 4, 4].
					    *)
(*
	    | S_SEG of param (* For segment descriptors of length one; param is value of single element. *)
	    | M_SEG of param * param * param (* For segment descriptors of length greater than one. 
					      * First param is number of segments.
					      * Second param is segment lengths (i.e. actual values in segdes)
					      * Third param is sum of segment lengths (i.e. length of underlying vector
					      *)
*)
	    | SCAL of param (* val *)
	    | TUPLE of shape list


  structure Map = RedBlackMapFn (
    struct
      type ord_key = param
      fun compare(P{id=a, ...}, P{id=b, ...}) = Stamp.compare(a, b)
    end)

(********** TO STRING **************)

  fun kindToString (Known(Flan.Int n)) = concat["K(", IntInf.toString n, ")"]
    | kindToString (Known(Flan.Var x)) = concat["K(", FlanVar.toString x, ")"]
    | kindToString (Known a) = concat["K(", FlanUtil.atomToString a, ")"]	(* shouldn't happen *)
    | kindToString Single = "S"
    | kindToString Unknown = "U"

  fun paramToString (P{id, kind, ...}) = "p"^(Stamp.toString id)^"{"^(kindToString (!kind))^"}"

  fun toString (VEC(p1, p2)) = "VEC("^(paramToString p1)^", "^(paramToString p2)^")"
    | toString (UNIFORM_VEC(p1, p2)) = "UVEC("^(paramToString p1)^", "^(paramToString p2)^")"
    | toString (SCAL p) = "SCAL("^(paramToString p)^")"
    | toString (TUPLE shps) = "TUP["^(toString' shps)^ "]"

  and toString'(shp::[]) = toString shp
    | toString' (shp::shps) = (toString shp)^", "^(toString' shps)


(******* CONVENIENCE FUNCTIONS FOR MANIPULATING PARAMETERS *********)

  fun getId (P{id, ...}) = id
  fun same (P{id=id1, ...}, P{id=id2, ...}) = Stamp.same(id1, id2)
  fun kindOf (P{kind=k, ...}) = !k
  fun setKind (P{kind, ...}, k) = kind := k
  fun equalsOf (P{equals, ...}) = !equals
  fun setEquals (P{equals, ...}, es) = equals := es
  fun addEqual (p, p') = let
    val ps = equalsOf p
  in
    setEquals(p, p'::ps)
  end
  fun clrEquals p = setEquals(p, [])
  fun unifyTo(p as P{unifiedTo, ...}, newP) = 
      if same(p, newP)
      then ()
      else unifiedTo := SOME newP
  fun getComponent(P{unifiedTo, ...}) = !unifiedTo
  fun isUnified(P{unifiedTo, ...}) = 
      (case !unifiedTo
	of SOME p => true
	 | NONE => false
      (* end case *))

(********* MAKING NEW PARAMS/SHAPES **********)

  fun newParam kind = 
      P{id = Stamp.new(), kind = ref kind, equals = ref nil, unifiedTo = ref NONE}

  fun fresh () = newParam Unknown
  fun scalar () = newParam Single
  (* Known(1) comes up a lot. *)
  fun one _ = newParam (Known (F.Int 1))
  fun isOne p = 
      (case (kindOf p)
	of (Known (F.Int 1)) => true
	 | _ => false
      (* end case *))


  (* Strip literal values from param *)
  fun strip p =
      (case kindOf p
	of Known _ => Single
	 | Single => Single
(* 	 | Known_List => Unknown *)
	 | Unknown => Unknown
      (* end case *))
  fun copy p = newParam(kindOf p)

  fun mkShape (T.TyBase (TypeBase.SEGDES)) = VEC(scalar(), fresh())
    | mkShape (T.TyBase _) = SCAL(scalar())
    | mkShape (T.TyBaseSeq _) = VEC(scalar(), fresh())
    | mkShape (T.TyTuple ts) = TUPLE(map mkShape ts)

  fun mkScalar () = SCAL(scalar())

  fun mkFlatShape () = let
    val len = scalar()
  in
    TUPLE[VEC(one (), len), VEC(len, fresh())]
  end

  fun mkFlatWithLen (len : param) =
      TUPLE[VEC(one (), len), VEC(len, fresh())]

  fun mkSegShape () = let
    val numberOfSegs = scalar()
  in
    TUPLE[VEC(one (), numberOfSegs), TUPLE[VEC(numberOfSegs, fresh()), VEC(scalar(), fresh())]]
  end

(* Given a segment vector shape, return the shape of a flat vector
 * of the same length *)
  fun flattenSegShape (TUPLE[outerseg, TUPLE[VEC(numsegs, _), data]]) =
      TUPLE[outerseg, VEC(numsegs, fresh())]

  fun mkShapeWithLen (ty, len) = let
    val lenParam = newParam(Known (F.Int (IntInf.fromInt len)))
  in
    (case mkShape ty
      of TUPLE[seg, VEC(_, _)] => TUPLE[VEC(one (), lenParam), VEC(lenParam, fresh())]
       | TUPLE[seg, TUPLE[VEC(_, _), rest]] => TUPLE[VEC(one (), lenParam), 
						     TUPLE[VEC(lenParam, fresh()),
							   rest]]
       | _ => raise Fail "not a well-formed vector"
    (* end case *))
  end

  (* Given a well-formed vector, return another vector with identical parameters, except for 
   * the value of the flat data vector itself *)
  fun cloneVector (VEC(len, v)) = VEC(len, newParam(strip v))
    | cloneVector (UNIFORM_VEC(len, v)) = UNIFORM_VEC(len, newParam(strip v))
    | cloneVector (TUPLE[seg, rest]) = TUPLE[seg, cloneVector rest]
    | cloneVector _ = raise Fail "cloneVector : not a well-formed vector"

(************* SHAPE PROPERTIES **************)
			       
  (* per-variable property to get/set shapes *)
  val {getFn=getShape, setFn=setShape, peekFn=peekShape, ...} = let
    fun init (v as F.V{ty, ...}) : shape = raise Fail ("shape of "^(FlanVar.toString v)^" is not initialized")
  in
    V.newProp(init)
  end

  (* per-function property to get/set shape of return value *)
  val {getFn=getFunShape, setFn=setFunShape, ...} = let
    fun init f = let val (dom, rng) = FlanFunct.typeOf f
		 in (map mkShape dom, mkShape rng)
		 end
  in
    FlanFunct.newProp(init)
  end

  (* per-function flag to indicate whether or not it's been analyzed. 
   * this is a work-around because we don't have declare before use *)
  val {getFn=isAnalyzed, setFn=setAnalyzed, ...} = FlanFunct.newFlag()

  (* Property on TopExps to get/set shape of result *)
  val {setFn=setExpShape, getFn=getExpShape, ...} = let
    fun expProp(F.TopExp(_, t, p)) = p
    fun init (F.TopExp(_, t, _)) = mkShape t
  in
    PropList.newProp(expProp, init)
  end

(********** ADDING CONSTRAINTS ************)
  fun setParamsEqual(p1, p2) = 
      if same(p1, p2)
      then ()
      else (addEqual(p1, p2); addEqual(p2, p1))

  fun setParamsEqual' (p, []) = ()
    | setParamsEqual' (p, p'::ps') = (setParamsEqual(p, p'); setParamsEqual'(p, ps'))

  fun setShapesEqual (VEC(len1, v1), VEC(len2, v2)) = 
      (setParamsEqual(len1, len2); setParamsEqual(v1, v2))
    | setShapesEqual (UNIFORM_VEC(len1, v1), UNIFORM_VEC(len2, v2)) =
      (setParamsEqual (len1, len2); setParamsEqual (v1, v2))
    | setShapesEqual (UNIFORM_VEC(len1, v1), VEC(len2, v2)) =
      (setParamsEqual(len1, len2);
       (* If it's a singleton vector, v1 and v2 are comparable *)
       if (isOne len1) orelse (isOne len2)
       then setParamsEqual(v1, v2)
       else ())
    | setShapesEqual(VEC(len1, v1), UNIFORM_VEC(len2, v2)) =
      (setParamsEqual(len1, len2);
       (* If it's a singleton vector, v1 and v2 are comparable *)
       if (isOne len1) orelse (isOne len2)
       then setParamsEqual(v1, v2)
       else ())
    | setShapesEqual(SCAL v1, SCAL v2) = setParamsEqual(v1, v2)
    | setShapesEqual (TUPLE shapes1, TUPLE shapes2) =
      ListPair.app setShapesEqual (shapes1, shapes2)

  (* Not sure whether we'll need this *)
  fun setVarsEqual(v1, v2) = setShapesEqual(getShape v1, getShape v2)

  (* Make a copy of the shape information for function f, 
   * maintaining relationships between parameters in 
   * arguments are results. 
   * Requires instance to be unified first.
   *)

  fun copyFunShape f = let
    val (domShape, rngShape) = getFunShape f
(* Return copy of param p, and the map. 
 * If there isn't already a copy of, create one. *)
    fun copyOfP(map, p) =
	(case Map.find(map, p)
	  of SOME p' => (map, p')
	   | NONE => let
	       val p' = copy p
	     in
	       (Map.insert(map, p, p'), p')
	     end
	(* end case *))
    fun copyOfS(map, VEC(p1, p2)) = let
      val (map, p1') = copyOfP(map, p1)
      val (map, p2') = copyOfP(map, p2)
    in
      (map, VEC(p1', p2'))
    end
      | copyOfS(map, UNIFORM_VEC(p1, p2)) = let
	  val (map, p1') = copyOfP(map, p1)
	  val (map, p2') = copyOfP(map, p2)
	in
	  (map, VEC(p1', p2'))
	end
      | copyOfS(map, SCAL p) = let
	  val (map, p') = copyOfP(map, p)
	in
	  (map, SCAL p')
	end
      | copyOfS(map, TUPLE shps) = let
	  val (map, shps') = copyOfS'(map, shps)
	in
	  (map, TUPLE shps')
	end
    and copyOfS'(map, []) = (map, [])
      | copyOfS'(map, shp::shps) = let
	  val(map, shp') = copyOfS(map, shp)
	  val (map, shps') = copyOfS'(map, shps)
	in
	  (map, shp'::shps')
	end
    val empty = Map.empty
    val (map, domShape') = copyOfS'(empty, domShape)
    val (_, rngShape') = copyOfS(map, rngShape)
  in
    (domShape', rngShape')
  end

  fun mergeKinds(Unknown, _) = Unknown
    | mergeKinds(_, Unknown) = Unknown
    | mergeKinds(Single, _) = Single
    | mergeKinds(_, Single) = Single
    | mergeKinds(Known a1, Known a2) = 
      if FlanUtil.sameAtom(a1, a2)
      then Known a1
      else Single

  fun mergeParams(p1, p2) =
      (* FIXME: use graph to figure out whether they're equal, instead of just seeing whether they're identical. *)
      if same(p1, p2) 
      then p1
      else newParam(mergeKinds(kindOf p1, kindOf p2))

  fun mergeShapes(VEC(len1, v1), VEC(len2, v2)) = VEC(mergeParams(len1, len2), mergeParams(v1, v2))
    | mergeShapes(VEC(len1, v1), UNIFORM_VEC(len2, v2)) = let
	val len = mergeParams(len1, len2)
      in
	if isOne len
	then VEC(len, mergeParams(v1, v2))
	else VEC(len, fresh())
      end
    | mergeShapes(UNIFORM_VEC(len1, v1), VEC(len2, v2)) = let
	val len = mergeParams(len1, len2)
      in
	if isOne len
	then VEC(len, mergeParams(v1, v2))
	else VEC(len, fresh())
      end
    | mergeShapes(UNIFORM_VEC(len1, v1), UNIFORM_VEC(len2, v2)) = let
	val len = mergeParams(len1, len2)
	val v = mergeParams(v1, v2)
      in
	UNIFORM_VEC(len, v)
      end
    | mergeShapes(SCAL v1, SCAL v2) = SCAL(mergeParams(v1, v2))
    | mergeShapes(TUPLE shapes1, TUPLE shapes2) = TUPLE (ListPair.map mergeShapes (shapes1, shapes2))
    | mergeShapes _ = raise Fail "cannot merge incompatible shapes"

  (* pureShape and associated functions return
   * a list of argument shapes and a return shape,
   * with params already unified.
   *)

  fun scanShape _ = let
    val arg = mkFlatShape()
    val result = cloneVector arg
  in
    ([arg], result)
  end

  fun reduceShape _ = let
    val arg = mkFlatShape()
    val result = mkScalar()
  in
    ([arg], result)
  end

  val unaryShape = scanShape

  fun binaryShape _ = let
    val arg1 = mkFlatShape()
    val arg2 = cloneVector arg1
    val result = cloneVector arg1
  in
    ([arg1, arg2], result)
  end

  fun liftedScanShape _ = let
    val arg = mkSegShape()
    val result = cloneVector arg
  in
    ([arg], result)
  end

  fun liftedReduceShape _ = let
    val arg = mkSegShape()
    (* Result is a flat vector whose length equals the number of segments in the argument *)
    val TUPLE[outerseg, TUPLE[VEC(innerlen, _), data]] = arg
    val result = flattenSegShape arg
  in
    ([arg], result)
  end

  (* Functions to return shapes for various pure ops;
   * mkVecFn should be mkFlatShape for base op, 
   * mkSegShape for lifted op
   *)

  fun dpermuteShape mkVecFn = let
    (* val vector and index vector are same shape. 
     * default and result are same shape. *)
    val valArg = mkVecFn()
    val idxArg = cloneVector valArg
    val defArg = mkVecFn()
    val result = cloneVector defArg
  in
    ([valArg, idxArg, defArg], result)
  end

  fun fpermuteShape mkVecFn = raise Fail "Fixme: fpermute"
(* let
    (* All arguments are of same shape, length of result unknown *)
    val valArg = mkVecFn()
    val idxArg = mkVecFn()
    val result = cloneVector idxArg
  in
    ([valArg, idxArg], result)
  end
*)
  fun bpermuteShape mkVecFn = let
    (* Result has same shape as index vector. *)
    val valArg = mkVecFn()
    val idxArg = mkVecFn()
    val result = cloneVector idxArg
  in
    ([valArg, idxArg], result)
  end
			      
  fun bfpermuteShape mkVecFn = let
    (* Index, flags and result have same length. *)
    val valArg = mkVecFn()
    val idxArg = mkVecFn()
    val flagArg = cloneVector idxArg
    val result = cloneVector idxArg
  in
    ([valArg, idxArg, flagArg], result)
  end

  fun dfpermuteShape mkVecFn = let
    (* Values, index and flags have same length. 
     * Default and result have same length. *)
    val valArg = mkFlatShape()
    val idxArg = cloneVector valArg
    val flagArg = cloneVector valArg
    val defArg = mkFlatShape()
    val result = cloneVector defArg
  in
    ([valArg, idxArg, flagArg, defArg], result)
  end

							  
  fun pureShape(FlanPure.Base p) =
      (case p
	of (Pure.ADD_SCAN _) => scanShape()
	 | (Pure.MUL_SCAN _) => scanShape()
	 | (Pure.MAX_SCAN _) => scanShape()
	 | (Pure.MIN_SCAN _) => scanShape()
	 | (Pure.AND_SCAN _) => scanShape()
	 | (Pure.OR_SCAN _) => scanShape()
	 | (Pure.XOR_SCAN _) => scanShape()
	 | (Pure.ADD_REDUCE _) => reduceShape()
	 | (Pure.MUL_REDUCE _) =>  reduceShape()
	 | (Pure.MAX_REDUCE _) => reduceShape()
	 | (Pure.MIN_REDUCE _) => reduceShape()
	 | (Pure.AND_REDUCE _) => reduceShape()
	 | (Pure.OR_REDUCE _) => reduceShape()
	 | (Pure.XOR_REDUCE _) => reduceShape()
	 | (Pure.PERMUTE _) => binaryShape()
	 | (Pure.DPERMUTE _ ) => dpermuteShape mkFlatShape
	 | (Pure.FPERMUTE _ ) => fpermuteShape mkFlatShape
	 | (Pure.BPERMUTE _) => bpermuteShape mkFlatShape
	 | (Pure.BFPERMUTE _) => bfpermuteShape mkFlatShape
	 | (Pure.DFPERMUTE _) => dfpermuteShape mkFlatShape
	 | (Pure.EXTRACT _) => ([mkFlatShape(), mkScalar()], mkScalar())
	 | (Pure.REPLACE _) => let
	     val vecArg = mkFlatShape()
	     val idxArg = mkScalar()
	     val vArg = mkScalar()
	     val result = cloneVector vecArg
	   in
	     ([vecArg, idxArg, vArg], result)
	   end
	 | (Pure.PACK _) => let
	     val vecArg = mkFlatShape()
	     val flagArg = cloneVector vecArg
	     val result = mkFlatShape()
	   in
	     ([vecArg, flagArg], result)
	   end
	 | (Pure.RANK_UP _) => unaryShape()
	 | (Pure.RANK_DOWN _) => unaryShape()
	 | (Pure.DIST _) => let
	     val len = scalar()
	     val v = scalar()
	     val lenArg = SCAL len
	     val vArg = SCAL v
	     val result = TUPLE[VEC(one (), len), UNIFORM_VEC(len, v)]
	   in
	     ([vArg, lenArg], result)
	   end
	 | Pure.INDEX => let
	     val len = scalar()
	     val lenArg = SCAL len
	     val result = mkFlatWithLen len
	   in
	     ([SCAL(scalar()), SCAL(scalar()), lenArg], result)
	   end
	 | (Pure.LENGTH _) => let
	     val len = scalar()
	     val arg = mkFlatWithLen len
	     val result = SCAL len
	   in
	     ([arg], result)
	   end
	 | (Pure.SCALAR_TO_SEQ _) => let
	     val v = scalar()
	     val arg = SCAL v
	     val result = VEC(one (), v)
	   in
	     ([arg], result)
	   end
	 | (Pure.SEQ_TO_SCALAR _) => let
	     val v = scalar()
	     val arg = VEC(one (), v)
	     val result = SCAL v
	   in
	     ([arg], result)
	   end
	 | Pure.MAKE_SEGDES => let
	     val arg = VEC(scalar(), fresh())
	   in
	     ([arg], arg)
	   end
	 | Pure.LENGTHS => let
	     val arg = VEC(scalar(), fresh())
	   in
	     ([arg], arg)
	   end
	 | (Pure.PROJ i) => raise Fail "PROJ"
	 (* Scalars; just use type info to get shape *)
	 | pur => let
	     val (domTy, rngTy) = FlanPrimTy.typeOfPure pur
	   in
	     (map mkShape domTy, mkShape rngTy)
	   end
      (* end case *))
    | pureShape(FlanPure.Lifted p) =
      (case p
	of (Pure.ADD _) => binaryShape()
	 | (Pure.SUB _) => binaryShape()
	 | (Pure.MUL _) => binaryShape()
	 | (Pure.DIV _) => binaryShape()
	 | Pure.MOD => binaryShape()
	 | (Pure.LT _) => binaryShape()
	 | (Pure.LTE _) => binaryShape()
	 | (Pure.GT _) => binaryShape()
	 | (Pure.GTE _) => binaryShape()
	 | (Pure.EQ _) => binaryShape()
	 | (Pure.NEQ _) => binaryShape()
	 | Pure.LSHIFT => binaryShape()
	 | Pure.RSHIFT => binaryShape()
	 | (Pure.NOT _) => unaryShape()
	 | (Pure.AND _) => binaryShape()
	 | (Pure.OR _) => binaryShape()
	 | (Pure.XOR _) => binaryShape()
	 | (Pure.SELECT _) => let
	     val flagArg = mkFlatShape()
	     val v1Arg = cloneVector flagArg
	     val v2Arg = cloneVector flagArg
	     val result = cloneVector flagArg
	   in
	     ([flagArg, v1Arg, v2Arg], result)
	   end
  (* QUESTION: normally if the input is uniform, the output is too.
   * This is not the case with RAND. So, are we losing information with all the
   * others, or getting inaccurate information with this one? 
   * I think losing info on the others--which is fine. 
   *)
	 | Pure.RAND => unaryShape()
	 | Pure.FLOOR => unaryShape()
	 | Pure.CEIL => unaryShape()
	 | Pure.TRUNC => unaryShape()
	 | Pure.ROUND => unaryShape()
	 | Pure.I_TO_F => unaryShape()
	 | Pure.I_TO_B => unaryShape()
	 | Pure.B_TO_I => unaryShape()
	 | Pure.LOG => unaryShape()
	 | Pure.SQRT => unaryShape()
	 | Pure.EXP => unaryShape()
	 | Pure.SIN => unaryShape()
	 | Pure.COS => unaryShape()
	 | Pure.TAN => unaryShape()
	 | Pure.ASIN => unaryShape()
	 | Pure.ACOS => unaryShape()
	 | Pure.ATAN => unaryShape()
	 | Pure.SINH => unaryShape()
	 | Pure.COSH => unaryShape()
	 | Pure.TANH => unaryShape()
	 | Pure.I_TO_C => unaryShape()
	 | Pure.C_TO_I => unaryShape()
	 | (Pure.ADD_SCAN _) => liftedScanShape()
	 | (Pure.MUL_SCAN _) => liftedScanShape()
	 | (Pure.MAX_SCAN _) => liftedScanShape()
	 | (Pure.MIN_SCAN _) => liftedScanShape()
	 | (Pure.AND_SCAN _) => liftedScanShape()
	 | (Pure.OR_SCAN _) => liftedScanShape()
	 | (Pure.XOR_SCAN _) => liftedScanShape()
	 | (Pure.ADD_REDUCE _) => liftedReduceShape()
	 | (Pure.MUL_REDUCE _) => liftedReduceShape()
	 | (Pure.MAX_REDUCE _) => liftedReduceShape()
	 | (Pure.MIN_REDUCE _) => liftedReduceShape()
	 | (Pure.AND_REDUCE _) => liftedReduceShape()
	 | (Pure.OR_REDUCE _) => liftedReduceShape()
	 | (Pure.XOR_REDUCE _) => liftedReduceShape()
	 | (Pure.PERMUTE _) => let
	     val valArg = mkSegShape()
	     val idxArg = cloneVector valArg
	     val result = cloneVector valArg
	   in
	     ([valArg, idxArg], result)
	   end
	 | (Pure.DPERMUTE _ ) => dpermuteShape mkSegShape
	 | (Pure.FPERMUTE _ ) => fpermuteShape mkSegShape
	 | (Pure.BPERMUTE _) => bpermuteShape mkSegShape
	 | (Pure.BFPERMUTE _) => bfpermuteShape mkSegShape
	 | (Pure.DFPERMUTE _) => dfpermuteShape mkSegShape
	 | (Pure.EXTRACT _) => let
	     val vecArg = mkSegShape()
	     val idxArg = flattenSegShape vecArg
	     val result = cloneVector idxArg
	   in
	     ([vecArg, idxArg], result)
	   end
	 | (Pure.REPLACE _) => let
	     val vecArg = mkSegShape()
	     val idxArg = flattenSegShape vecArg
	     val repArg = cloneVector idxArg
	     val result = cloneVector vecArg
	   in
	     ([vecArg, idxArg, repArg], result)
	   end
	 | (Pure.PACK _) => let
	     val vecArg = mkSegShape()
	     val flagArg = cloneVector vecArg
	     (* Result has same number of segments, but unknown segment lengths/total length *)
	     val TUPLE[outer, TUPLE[VEC(numsegs, _), data]] = vecArg
	     val result = TUPLE[outer, TUPLE[VEC(numsegs, fresh()), VEC(scalar(), fresh())]]
	   in
	     ([vecArg, flagArg], result)
	   end
	 | (Pure.RANK_UP _) => let
	     val arg = mkSegShape()
	     val result = cloneVector arg
	   in
	     ([arg], result)
	   end
	 | (Pure.RANK_DOWN _) => let
	     val arg = mkSegShape()
	     val result = cloneVector arg
	   in
	     ([arg], result)
	   end
	 | (Pure.DIST _) => let
	     val lenArg = mkFlatShape()
	     val vArg = cloneVector lenArg
	     (* length arg is segment descriptor of result *)
	     val TUPLE[lenSeg, lenVal] = lenArg
	     (* If vArg is uniform, data vector is also uniform, but we don't try to track that. *)
	     val result = TUPLE[lenSeg, TUPLE[lenVal, VEC(scalar(), fresh())]]
	   in
	     ([vArg, lenArg], result)
	   end
	 | Pure.INDEX => let
	     val lenArg = mkFlatShape()
	     val startArg = cloneVector lenArg
	     val strideArg = cloneVector lenArg
      (* length arg is segment descriptor of result *)
	     val TUPLE[lenSeg, lenVal] = lenArg
	     val result = TUPLE[lenSeg, TUPLE[lenVal, VEC(scalar(), fresh())]]
	   in
	     ([startArg, strideArg, lenArg], result)
	   end
	 | _ => raise Fail ("Cannot lift"^(Pure.toString p))
      (* end case *))

  fun cmdShape c =
      (case c
	of Cmd.EXIT => mkScalar()
	 | Cmd.READ _ => mkFlatShape()
	 | Cmd.WRITE _ => TUPLE[mkFlatShape(), mkScalar()]
	 | Cmd.FOPEN => TUPLE[mkScalar(), mkFlatShape(), mkScalar()]
	 | Cmd.FCLOSE => TUPLE[mkScalar(), mkFlatShape()]
	 | Cmd.START_TIMER => mkScalar()
	 | Cmd.STOP_TIMER => mkScalar()
	 | _ => raise Fail "Fixme : cmdShape"
      (* end case *))      

(************** SOLVING CONSTRAINTS **********)

  fun unifyKinds (Unknown, k2) = k2
    | unifyKinds (k1, Unknown) = k1
    | unifyKinds (k1 as Known _, Single) = k1
    | unifyKinds (Single, k2 as Known _) = k2
    | unifyKinds (Single, Single) = Single
    | unifyKinds (Known a1, Known a2) = 
      if FlanUtil.sameAtom(a1, a2)
      then Known a1
      else raise Fail "can't unify two different known shapes"
(*		 		
  (* Merge p2 into p1; can then discard p2 *)
  fun unifyParams(p1, p2) = let
    val P{unifiedTo, ...} = p2
  in
    (case !unifiedTo 
      of SOME newP => unifyParams(p1, newP)
       | NONE =>
	 let
	   val k = unifyKinds(kindOf p1, kindOf p2)
	   val es = equalsOf p2
	   (* Don't want to add params to worklist that we've already handled *)
(*	   val es' = List.filter (fn e => not(isUnified e)) es *)
	 in
	   (unifyTo(p2, p1);
	    setKind(p1, k);
	    setParamsEqual'(p1, es))
	 end
    (* end case *))
  end
      
  fun unifyParams'(p, []) = ()
    | unifyParams'(p, p'::ps) =
      (unifyParams(p, p');
       unifyParams'(p, ps))
*)

  fun unifyRecursive(pmap, p, original) = 
      (case Map.find(pmap, p)
	(* If it's been discovered 
	 * and is already in this component, do nothing. *)
	of SOME p' => 
	   if same(p', original)
	   then pmap
	   (* But if it's unified to something else, we shouldn't be seeing it here. *)
	   else raise Fail (String.concat["param ", paramToString p,
					  " already mapped to ", paramToString p'])
	 | NONE => let
	     val pmap' = Map.insert(pmap, p, original)
	     fun unifyEquals (pm, []) = pm
	       | unifyEquals (pm, eq::eqs) = 
		 unifyEquals(unifyRecursive(pm, eq, original),
			     eqs)
	   in
	     (* If p is already unified with another component, these two
	      * components should be merged. *)
	     (case getComponent p
	       of SOME p' =>
		  (unifyTo(p, original);
		   unifyRecursive(pmap', p', original))
		| NONE =>
		  (unifyTo(p, original);
		   setKind(original, 
			   unifyKinds(kindOf p, kindOf original));
		   unifyEquals(pmap', equalsOf p))
	     (* end case *))
	   end
      (* end case *))
(* we want to mark p as discovered in map and unifiedTo field
 * if p was unified with another component in an earlier pass, 
 * we should merge those components.
 * otherwise we just call unifyRecursive on all its equals. 
 *)
(*
(* Unify parameters via depth-first search *)
  fun unifyRecursive(pmap, p, original) = 
      let
	val pmap = 
	    (* p should not already be in the map. we insert it now. 
	     * This means the original component will map to itself. *)
	    (case Map.find(pmap, p)
	      of SOME p' => raise Fail "param already discovered"
	       | NONE => Map.insert(pmap, p, original)
	    (* end case *))
	fun unifyR (pmap, []) = pmap
	  | unifyR (pmap, e::es) = let
	      val pmap =  
		  (case Map.find(pmap, e)
		    of SOME p' => pmap
		     (* e has not been discovered. Unify it now. *)
		     | NONE => 
		       (case getComponent e
			 (* e was unified to another component in a previous pass. 
			  * We unify with that component instead, unless it's been discovered. *)
			 of SOME comp => 
			  | NONE => unifyRecursive(pmap, e, original)
		       (* end case *))
		  (* end case *))
(*
		  (case getComponent e
		    (* If e was unified with some other comp in an earlier unify pass, want to unify with that instead  *)
		    (* If e was unified with this component, we've already handled it and don't need to do anything else. *)
		    of SOME comp => 
		       if same(comp, original) 
		       then pmap
		       else unifyRecursive(pmap, comp, original)
		     | NONE => 
		       (case Map.find(pmap, e)
			 (* If e is already in the map, but not dealt with in previous case,
			  * it wasn't unified to this component as it should have been *)
			 of SOME p' => raise Fail "param discovered but not unified to this component"
(*
			    if same(p', original)
			    then pmap
			    else raise Fail "param already discovered, but not for this component"
*)
			  | NONE => unifyRecursive(pmap, e, original)
		       (* end case *))
		  (* end case *))
*)
	    in
	      unifyR (pmap, es)
	    end
	val k  = unifyKinds(kindOf p, kindOf original)
      in
	(setKind(original, k);
	 unifyTo(p, original);
	 unifyR(pmap, equalsOf p))
      end
*)
(* If the parameter is not already marked as part of a connected component,
 * find its component and unify it.
 * We use map in addition to isUnified property because we unify within instances,
 * then unify again at the end.
 * So if a parameter was already unified in unifyInst, we can unify it again at the end
 * without breaking anything, because it won't be found in the map.
 *)
  fun unifyComponent (pmap, p) = 
      (case Map.find (pmap, p)
	of SOME p' => (pr (String.concat["param ", paramToString p, " already mapped to ",
					    paramToString p', "\n"]);
		       pmap)
	 | NONE =>
	   (case getComponent p
	     of SOME p' => (pr (String.concat["param ", paramToString p, " mapped to ",
					      paramToString p', " from previous call."]);
			    unifyComponent(pmap, p'))
	      | NONE =>
		(pr (String.concat["unifying around param ",
				   paramToString p, "\n"]);
		 unifyRecursive(pmap, p, p))
	   (* end case *))
      (* end case *))

(*
      if isUnified p
      then ()
      else unifyRecursive(p, p)
  *) 
(*
  (* Unify entire connected component into one node at param p, using depth-first search *)
  (* NOT QUITE RIGHT! *)
  fun unifyComponent p =
      if isUnified p
      then ()
      else
	(unifyTo(p, p);
	 case equalsOf p
	  of [] => ()
	   | es =>
	     ((*clrEquals p;*)
(*	      map (fn e => unifyTo(e, p)) es; *)
	      unifyParams'(p, es);
	      setEquals(p, (List.filter (fn e => not(isUnified e)) (equalsOf p)))
	      unifyComponent(p))
	(* end case *))
*)
  fun unifyShape(pmap, VEC(p1, p2)) = let
    val pmap = unifyComponent(pmap, p1)
    val pmap = unifyComponent(pmap, p2)
  in
    pmap
  end
    | unifyShape(pmap, UNIFORM_VEC(p1, p2)) = let
	val pmap = unifyComponent(pmap, p1)
	val pmap = unifyComponent(pmap, p2)
      in
	pmap
      end
    | unifyShape(pmap, SCAL p) = unifyComponent(pmap, p)
    | unifyShape (pmap, TUPLE shapes) = List.foldl (fn (s, pmap) => unifyShape(pmap, s)) pmap shapes
(* List.app unifyShape shapes *)

  fun unifyVar (pmap, v) = unifyShape(pmap, getShape v) 

  (**** Updating after we've solved constraints *******)
  fun updateParam (p as P{unifiedTo, ...}) = 
      (case !unifiedTo
	of SOME p' => 
	   if same(p, p')
	   then p
	   else updateParam p'
	 | NONE => p
      (* end case *))

  fun updateShape (VEC(p1, p2)) = VEC(updateParam p1, updateParam p2)
    | updateShape (UNIFORM_VEC(p1, p2)) = UNIFORM_VEC(updateParam p1, updateParam p2)
    | updateShape (SCAL p) = SCAL(updateParam p)
    | updateShape (TUPLE shapes) = TUPLE (map updateShape shapes)

  fun updateVar v = let
      (* If this is a scalar, and its param isn't unified to anything else,
       * (i.e. this is the first time it shows up), we know its kind is Known(this var)
       *)
    val shape = getShape v
    val _ =
	(case shape
	  of SCAL p =>
	     if isUnified p
	     then ()
	     else setKind(p, Known(F.Var v))
	   | _ => ()
	(* end cae *))
  in
    setShape(v, updateShape(getShape v))
  end

  fun updateFun f = let
    val (domShape, rngShape) = getFunShape f
  in
    (map updateShape domShape; updateShape rngShape)
  end

  (*********** MISCELLANEOUS *************)

  fun getAtmShape(F.Var v) = getShape v
    | getAtmShape a = SCAL (newParam(Known a))

  fun getOptLength s = let
    val lenParam = 
	(case s
	  of VEC(p1, p2) => p1
	   | UNIFORM_VEC(p1, p2) => p1
	(* end case *))
  in
    (case (kindOf lenParam)
      of Known(F.Int i) => SOME (IntInf.toInt i)
       | _ => NONE
    (* end case *))
  end
  
  end


