(* gen-cuda.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure GenCUDA : sig

    val genTask : CuLambda.TaskGraph.task -> CLang.decl

  end = struct

    structure Cu = CuLambda
    structure CL = CLang
    structure GPU = Cu.GPU
    structure T = Cu.TaskGraph
    structure N = CudaNames

    val floatTy = Util.floatTy
    val intTy = Util.intTy
    val mathFn = Util.mathFn

    fun add (a, b) = CL.mkBinOp(a, CL.#+, b)
    fun mul (a, b) = CL.mkBinOp(a, CL.#*, b)
    fun sel (x, f) = CL.mkSelect(CL.mkVar x, f)

  (* the statements for computing the global block and element indices.  We assume
   * a 2D grid of 1D blocks.
   *
   *	int _blockId = blockIdx.x + blockIdx.y * gridDim.x;
   *    int _blockSz = blockDim.x;
   *	int _id = threadIdx.x
   *	int _gId = _blockId * _blockSz + _id;
   *)
    val computeBlockIdx = CL.mkDeclInit (CL.intTy, N.blockIdx,
	  add(N.cudaBlockIdx_x, mul(N.cudaBlockIdx_y, N.cudaGridDim_x)))
    val computeBlockSz = CL.mkDeclInit (CL.intTy, N.blockSz, N.cudaBlockDim_x)
    val computeLocalElemIdx = CL.mkDeclInit (CL.intTy, N.localElemIdx, N.cudaThreadIdx_x)
    val computeElemIdx = CL.mkDeclInit (CL.intTy, N.elemIdx,
	  add(mul(N.blockIdxV, N.blockSzV), N.localElemIdxV))

  (* extract an element *)
    fun element name = CL.mkSubscript(CL.mkVar name, N.elemIdxV)

    local
    (* a property to track the binding of a variable *)
      val {setFn=setBinding, getFn=(getBinding : GPU.var -> CL.exp), ...} =
	    CuVar.newProp (fn v => raise Fail("no binding for "^CuVar.toString v))
    (* return the CUDA type of a variable *)
      fun typeOf v = Util.gpuTypeToCUDA (CuVar.typeOf v)
    in
  (* generate CUDA code for a kernel; results is a list of l-value expressions for storing
   * the kernel results.
   *)
    fun genKernel (results : CL.exp list, body) = let
	  val genArg = Util.genAtom getBinding
	  fun genVar x = let
		val x' = CL.mkVar(CuVar.uniqueName x)
		in
		  setBinding(x, x');
		  x'
		end
	  fun genExp (dsts, GPU.ExpLet(xs, e, cont), stms) = let
	      (* for each bound variable, we need to declare it and add it to the destination list *)
		fun doLHSVars (x::xs, dsts, stms) = let
		      val x' = CuVar.uniqueName x
		      val dst = CL.mkVar x'
		      in
			setBinding(x, dst);
			doLHSVars (xs, dst::dsts, CL.mkDecl(typeOf x, x', NONE)::stms)
		      end
		  | doLHSVars ([], dsts, stms) = (List.rev dsts, stms)
		val (dsts', stms) = doLHSVars (xs, [], stms)
		in
		  genExp (dsts, cont, genExp (dsts', e, stms))
		end
	    | genExp (dsts, GPU.ExpPrim(x, rator, args, cont), stms) = let
	      (* create the expression for the primitive *)
		val rhs = Util.genScalarOp(rator, List.map genArg args)
		in
		  if CuVar.useCnt x > 1
		    then let
		      val x' = CuVar.uniqueName x
		      in
			setBinding (x, CL.mkVar x');
			genExp (dsts, cont, CL.mkDeclInit(typeOf x, x', rhs) :: stms)
		      end
		    else (setBinding(x, rhs); genExp(dsts, cont, stms))
		end
	    | genExp (dsts, GPU.ExpIf(cond, e1, e2), stms) = let
		val stms1 = genExp(dsts, e1, [])
		val stms2 = genExp(dsts, e2, [])
		in
		  CL.mkIfThenElse(genArg cond, CL.mkBlock stms1, CL.mkBlock stms2) :: stms
		end
	    | genExp (dsts, GPU.ExpTuple args, stms) = let
		fun gen (x, y, stms) = CL.mkAssign(x, genArg y) :: stms
		in
		  List.rev (ListPair.foldlEq gen stms (dsts, args))
(* DEBUG *)handle ex => raise ex
		end
	  in
	    genExp (results, body, [])
	  end

  (* finite maps on host variables *)
    structure VMap = RedBlackMapFn (
      struct
	type ord_key = Cu.var
	val compare = CuVar.compare
      end)

  (* checking a task graph for various properties (we might want to cache these at some point) *)
    fun hasReduce (T.Task{sinks, ...}) = let
	  fun isReduce (T.SinkFlatReduce _) = true
	    | isReduce (T.SinkSegReduce _) = true
	    | isReduce _ = false
	  in
	    List.exists isReduce sinks
	  end
    fun hasScan (T.Task{body, ...}) = let
	  fun isScan (T.NodeMap _) = false
	    | isScan _ = true
	  in
	    List.exists isScan body
	  end
    fun needsSyncThreads tg = hasReduce tg orelse hasScan tg

(* variable reuse is going to require liveness analysis
  (* to allow efficient reuse of memory, task paramaters can be inputs, outputs, or both *)
    datatype direction = IN | OUT | INOUT

  (* for each input and output variable of a task, determine the direction *)
    fun directionMap (tg : T.task) = let
	  val inMap = List.foldl (fn (x, map) => VMap.insert(map, x, IN)) VMap.empty (#inputs tg)
	  val outMap = List.foldl (fn (x, map) => VMap.insert(map, x, OUT)) VMap.empty (#outputs tg)
	  fun merge (IN, OUT) = INOUT
	    | merge _ = raise Fail "impossible"
	  in
	    VMap.unionWith merge (inMap, outMap)
	  end
*)

  (* generate a list of initialization statements and set the bindings of a task's sources. *)
    fun genSourceInit (T.Task{srcs, ...}) = let
	  val genSrcAtom = Util.genAtom (fn x => CL.mkVar(CuVar.uniqueName x))
	  fun genSrc (src, stms) = let
		val (x, isAtom, srcExp) = (case src
		       of T.SrcVar(x, x') => (x, false, element(CuVar.uniqueName x'))
			| T.SrcFlatDist(x, atm) => (x, true, genSrcAtom atm)
			| T.SrcSegDist(x, values, lengths) => raise Fail "FIXME"
			| T.SrcFlatIndex(x, start, stride) => let
			  (* start + stride * _elemIdx *)
			    val e = N.elemIdxV
			    val (isAtom, e) = (case stride
				   of Cu.Int 1 => (true, e)
				    | _ => (false, mul(genSrcAtom stride, e))
				  (* end case *))
			    val (isAtom, e) = (case start
				   of Cu.Int 0 => (isAtom, e)
				    | _ => (false, add(genSrcAtom start, e))
				  (* end case *))
			    in
			      (x, isAtom, e)
			    end
			| T.SrcSegIndex(x, segdes, startVec, strideVec) => raise Fail "FIXME"
		      (* end case *))
		in
		  if (CuVar.useCnt x > 1) andalso (not isAtom)
		    then (
		      setBinding(x, CL.mkVar(CuVar.uniqueName x));
		      CL.mkDeclInit(typeOf x, CuVar.uniqueName x, srcExp) :: stms)
		    else (setBinding(x, srcExp); stms)
		end
	  in
	    List.foldr genSrc [] srcs
	  end

    fun mkIfElement blk = CL.mkIfThen(CL.mkBinOp(N.elemIdxV, CL.#<, N.widthV), blk)

  (* generate CUDA code for a task graph; results is a list of l-value expressions for storing
   * the kernel results.
   *)
(* QUESTION: how do we specify reuse of variables? *)
    fun genTask (tg as T.Task{name, inputs, body, sinks, ...}) = let
val _ = PPCuLambda.outputTask (TextIO.stdOut, tg)
(*
	  val dirMap = directionMap tg
*)
	  val outputs = T.outputs tg
	  val srcInit = genSourceInit tg
	  val params = let
	        fun doParam input (x, params) = let
		      val x' = CuVar.uniqueName x
		      val ty = (case CuVar.typeOf x
			     of Cu.TyScalar ty => if input
				  then Util.gpuTypeToCUDA ty
				  else CL.T_RestrictPtr(Util.gpuTypeToCUDA ty)
			      | Cu.TySegdes => CL.T_RestrictPtr CL.intTy
			      | Cu.TySeq(ty, _) => CL.T_RestrictPtr(Util.gpuTypeToCUDA ty)
			      | Cu.TyTuple _ => raise Fail "unexpected tuple parameter"
			    (* end case *))
		      in
			CL.PARAM([], ty, x') :: params
		      end
		in
		  List.foldr (doParam false)
		    (List.foldr (doParam true) [] inputs)
		      outputs
		end
	  val params = CL.PARAM([], CL.intTy, N.width) :: params
	  val body = if (hasScan tg)
		then raise Fail "FIXME"
		else let
		(* simple "map" task; the body should be a single kernel and the sinks should
		 * all be variables.
		 *)
		  val [T.NodeMap(results, kern)] = body
		  val results = List.map (fn x => element(CuVar.uniqueName x)) outputs
		  in [
		    computeBlockIdx,
		    computeBlockSz,
		    computeLocalElemIdx,
		    computeElemIdx,
		    mkIfElement(CL.mkBlock(List.concat [
			srcInit,
			genKernel (results, kern)
		      ]))
		  ] end
	  val body = let
		fun f (T.SinkFlatReduce arg) = SOME arg
		  | f _ = NONE
		in
		  case List.mapPartial f sinks
		   of [] => body
		    | reds => body @ GenReduce.gen reds
		  (* end case *)
		end
	  in
	    CL.D_Func(["__global__"], CL.voidTy, name, params, CL.mkBlock body)
	  end

    end (* local *)

  end
