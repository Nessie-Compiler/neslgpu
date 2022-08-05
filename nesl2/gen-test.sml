(* gen-test.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure GenTest =
  struct

    local
      structure C = CuLambda
      structure G = C.GPU
      structure T = C.TaskGraph
    in

  (* the initialization task for muladd *)
    fun muladdInitTask (nums1, nums2, nums3) = let
	  val arg1 = CuVar.new("arg1", G.TyInt)
	  val arg2 = CuVar.new("arg2", G.TyInt)
	  val arg3 = CuVar.new("arg3", G.TyInt)
	  val rand1 = CuVar.new("rand1", G.TyInt)
	  val rand2 = CuVar.new("rand2", G.TyInt)
	  val rand3 = CuVar.new("rand3", G.TyInt)
	  val res1 = CuVar.new("res1", G.TyInt)
	  val res2 = CuVar.new("res2", G.TyInt)
	  val res3 = CuVar.new("res3", G.TyInt)
	  val kern =  G.Kern{
		  freeVars = [],
		  params = [arg1, arg2, arg3],
		  body =
		    G.ExpPrim(rand1, C.RAND, [C.Var arg1],
		    G.ExpPrim(rand2, C.RAND, [C.Var arg2],
		    G.ExpPrim(rand3, C.RAND, [C.Var arg3],
		      G.ExpTuple [C.Var rand1, C.Var rand2, C.Var rand3])))
		}
	  val task = T.Task{
		  name = "muladdInit",
		  width = C.Int 100,
		  inputs = [],
		  srcs = [T.SrcFlatDist(arg1, C.Int 100), T.SrcFlatDist(arg2, C.Int 100), T.SrcFlatDist(arg3, C.Int 100)],
		  body = [T.NodeMap([res1, res2, res3], kern, [], [arg1, arg2, arg3])],
		  sinks = [T.SinkVar(nums1, res1), T.SinkVar(nums2, res2), T.SinkVar(nums3, res3)]
		}
	  in
	    task
	  end

    fun muladdInitTask' () = let
	  val intSeqTy = C.TySeq(G.TyInt, NONE)
	  val nums1 = CuVar.new("nums1", intSeqTy)
	  val nums2 = CuVar.new("nums2", intSeqTy)
	  val nums3 = CuVar.new("nums3", intSeqTy)
	  in
	    GenCUDA.genTask (muladdInitTask (nums1, nums2, nums3))
	  end

  (* the computational task from the muladd example *)
    fun muladdTask (nums1, nums2, nums3, result) = let
	  val arg1 = CuVar.new("arg1", G.TyInt)
	  val arg2 = CuVar.new("arg2", G.TyInt)
	  val arg3 = CuVar.new("arg3", G.TyInt)
	  val a = CuVar.new("a", G.TyInt)
	  val b = CuVar.new("b", G.TyInt)
	  val kern =  G.Kern{
		  freeVars = [],
		  params = [arg1, arg2, arg3],
		  body =
		    G.ExpPrim(a, C.MUL TypeBase.INT, [C.Var arg1, C.Var arg2],
		    G.ExpPrim(b, C.ADD TypeBase.INT, [C.Var a, C.Var arg3],
		      G.ExpTuple [C.Var b]))
		}
	  val task = T.Task{
		  name = "muladd",
		  width = C.Int 100,
		  inputs = [nums1, nums2, nums3],
		  srcs = [T.SrcVar(arg1, nums1), T.SrcVar(arg2, nums2), T.SrcVar(arg3, nums3)],
		  body = [T.NodeMap([b], kern, [], [arg1, arg2, arg3])],
		  sinks = [T.SinkVar(result, b)]
		}
	  in
	    task
	  end

    fun muladdTask' () = let
	  val intSeqTy = C.TySeq(G.TyInt, NONE)
	  val nums1 = CuVar.new("nums1", intSeqTy)
	  val nums2 = CuVar.new("nums2", intSeqTy)
	  val nums3 = CuVar.new("nums3", intSeqTy)
	  val result = CuVar.new("result", intSeqTy)
	  in
	    GenCUDA.genTask (muladdTask (nums1, nums2, nums3, result))
	  end

  (*  create the program representation for the muladd example *)
    fun muladd () = let
	  val intSeqTy = C.TySeq(G.TyInt, NONE)
	  val nums1 = CuVar.new("nums1", intSeqTy)
	  val nums2 = CuVar.new("nums2", intSeqTy)
	  val nums3 = CuVar.new("nums3", intSeqTy)
	  val result = CuVar.new("result", intSeqTy)
	  val t0 = CuVar.new("t0", C.TyScalar G.TyFloat)
	  val dt = CuVar.new("dt", C.TyScalar G.TyFloat)
	  in
	    C.mkTask{
		task = muladdInitTask(nums1, nums2, nums3),
		cont = C.mkCmd{
		    results = [t0], cmd = Cmd.START_TIMER, args = [C.Int 0],
		    cont = C.mkTask{
			task = muladdTask (nums1, nums2, nums3, result),
			cont = C.mkCmd{
			    results = [dt], cmd = Cmd.STOP_TIMER, args = [C.Var t0],
			    cont = C.mkReturn [(*C.Var dt*)]
			  }
		      }
		  }
	      }
	  end

    fun print dcl = let
	  val strm = PrintAsCUDA.new TextIO.stdOut
	  in
	    PrintAsCUDA.output (strm, dcl);
	    PrintAsCUDA.close strm
	  end

    end (* local *)

  end (* local *)
