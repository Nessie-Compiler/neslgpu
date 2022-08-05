(* gen-reduce.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure GenReduce : sig

  (* a flat reduction *)
    type reduction = (CuLambda.var * ReduceOp.t * CuLambda.GPU.var)

    val gen : reduction list -> CLang.stm list

(*
    val gen : reduction list -> {
	    decls : CLang.stm list,	(* declarations of shared memory for reduction *)
	    init : (CuLambda.GPU.var * CLang.exp) list,
					(* lhs targets for initializing the associated argument's
					 * shared memory.
					 *)
	    oobInit : CLang.stm list,	(* initialization for out-of-bounds elements *)
	    reduce : CLang.stm list	(* the actual reduction code *)
	  }
*)

  end = struct

    structure R = ReduceOp
    structure Cu = CuLambda
    structure CL = CLang
    structure GPU = Cu.GPU
    structure U = Util
    structure N = CudaNames

    type reduction = (Cu.var * R.t * GPU.var)

    val syncthreads = CL.mkCall("__syncthreads", [])

  (* the identity for the given reduction operator *)
    fun identity rator = (case rator
	   of R.ADD_REDUCE TypeBase.INT => CL.mkInt 0
	    | R.ADD_REDUCE TypeBase.FLOAT => CL.mkFlt "0.0"
	    | R.MUL_REDUCE TypeBase.INT => CL.mkInt 1
	    | R.MUL_REDUCE TypeBase.FLOAT => CL.mkFlt "1.0"
	    | R.MAX_REDUCE TypeBase.INT => CL.mkVar "NESL_MIN_INT"
	    | R.MAX_REDUCE TypeBase.FLOAT => CL.mkVar "NESL_MIN_FLOAT"
	    | R.MIN_REDUCE TypeBase.INT => CL.mkVar "NESL_MAX_INT"
	    | R.MIN_REDUCE TypeBase.FLOAT => CL.mkVar "NESL_MAX_FLOAT"
	    | R.AND_REDUCE TypeBase.INT => CL.mkInt ~1
	    | R.AND_REDUCE TypeBase.BOOL => CL.mkVar "true"
	    | R.OR_REDUCE TypeBase.INT => CL.mkInt 0
	    | R.OR_REDUCE TypeBase.BOOL => CL.mkVar "false"
	    | R.XOR_REDUCE TypeBase.INT => CL.mkInt 0
	    | R.XOR_REDUCE TypeBase.BOOL => CL.mkVar "false"
	    | _ => raise Fail "impossible"
	  (* end case *))

  (* generate the statement that implements a single step of the reduction *)
    fun reduceStep (sdata, rator) = let
	  val dst = CL.mkSubscript(CL.mkVar sdata, N.elemIdxV)
	  val src = CL.mkSubscript(CL.mkVar sdata, CL.mkBinOp(N.elemIdxV, CL.#+, CL.mkVar "_offset"))
	  in
	    case rator
	     of R.ADD_REDUCE ty => CL.mkAssign' (dst, CL.+=, src)
	      | R.MUL_REDUCE ty => CL.mkAssign' (dst, CL.*=, src)
	      | R.MAX_REDUCE ty => CL.mkIfThen(CL.mkBinOp(src, CL.#>, dst), CL.mkAssign(dst, src))
	      | R.MIN_REDUCE ty => CL.mkIfThen(CL.mkBinOp(src, CL.#<, dst), CL.mkAssign(dst, src))
	      | R.AND_REDUCE TypeBase.INT => CL.mkAssign' (dst, CL.&=, src)
	      | R.AND_REDUCE TypeBase.BOOL => raise Fail "FIXME"
	      | R.OR_REDUCE TypeBase.INT => CL.mkAssign' (dst, CL.|=, src)
	      | R.OR_REDUCE TypeBase.BOOL => raise Fail "FIXME"
	      | R.XOR_REDUCE TypeBase.INT => CL.mkAssign' (dst, CL.^=, src)
	      | R.XOR_REDUCE TypeBase.BOOL => raise Fail "FIXME"
	      | _ => raise Fail "impossible"
	    (* end case *)
	  end

  (* generate code for the block-level reduction *)
    fun blockReduce body = let
	  val offsetV = CL.mkVar "_offset"
	  in
	    CL.S_For (
	      [(CL.intTy, "_offset", CL.mkBinOp(N.blockSzV, CL.#>>, CL.mkInt 1))],
	      CL.mkBinOp(offsetV, CL.#>, CL.mkInt 0),
	      [CL.mkAssignOp(offsetV, CL.>>=, CL.mkInt 1)],
	      CL.mkBlock [
		  CL.mkIfThen(CL.mkBinOp(N.localElemIdxV, CL.#<, CL.mkVar "_offset"), body),
		  syncthreads
		])
	  end

    fun gen reds = let
	  val srcVars = List.map (fn (_, _, x) => CuVar.uniqueName x) reds
	  val sharedVars = List.map (fn x' => x' ^ "_sdata") srcVars
	  val sharedDcls = ListPair.map
		(fn ((_, _, x), x') => CL.S_Decl(
		    ["extern", "__shared__"],
		    CL.T_Array(U.gpuTypeToCUDA(CuVar.typeOf x), NONE), x', NONE))
		  (reds, sharedVars)
	  val setupCode = ListPair.map
		(fn (x', x) => CL.mkAssign(CL.mkSubscript(CL.mkVar x', N.elemIdxV), CL.mkVar x))
		  (sharedVars, srcVars)
	  val reduces = ListPair.map (fn (x, (_, rator, _)) => reduceStep (x, rator)) (sharedVars, reds)
	  in
	    sharedDcls @
	    setupCode @
	    syncthreads ::
	    blockReduce (CL.mkBlock reduces) ::
	    []
	  end

  end
