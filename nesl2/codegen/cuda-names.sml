(* cuda-names.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure CudaNames =
  struct

    local
      structure CL = CLang
    in

  (* pre-defined CUDA variables used for indexing *)
    val cudaGridDim_x = CL.mkSelect(CL.mkVar "gridDim", "x")		(* X dimension of grid *)
    val cudaGridDim_y = CL.mkSelect(CL.mkVar "gridDim", "y")
    val cudaBlockDim_x = CL.mkSelect(CL.mkVar "blockDim", "x")		(* X dimension of block *)
    val cudaBlockDim_y = CL.mkSelect(CL.mkVar "blockDim", "y")
    val cudaBlockIdx_x = CL.mkSelect(CL.mkVar "blockIdx", "x")		(* width of block *)
    val cudaBlockIdx_y = CL.mkSelect(CL.mkVar "blockIdx", "y")
    val cudaThreadIdx_x = CL.mkSelect(CL.mkVar "threadIdx", "x")
    val cudaThreadIdx_y = CL.mkSelect(CL.mkVar "threadIdx", "y")	(* X part of block-local thread ID *)

  (* global block index variable name *)
    val blockIdx = "_blockId"
    val blockIdxV = CL.mkVar blockIdx

  (* block size variable name *)
    val blockSz = "_blockSz"
    val blockSzV = CL.mkVar blockSz

  (* local element index variable name *)
    val localElemIdx = "_id"
    val localElemIdxV = CL.mkVar localElemIdx

  (* global element index variable name *)
    val elemIdx = "_gId"
    val elemIdxV = CL.mkVar elemIdx

  (* computation width variable name *)
    val width = "_width"
    val widthV = CL.mkVar "_width"

    end (* local *)
  end
