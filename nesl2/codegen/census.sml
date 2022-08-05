(* census.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure CuCensus : sig

    val census : CuLambda.program -> unit

  end = struct

    structure Cu = CuLambda
    structure V = CuVar
    structure GPU = Cu.GPU
    structure TG = Cu.TaskGraph

    fun incAtom (Cu.Var x) = V.incCnt x
      | incAtom _ = ()

    fun doTaskGraph (TG.Task{width, inputs, srcs, body, sinks, ...}) = let
	(* note that for sources, we count the rhs variables as inputs *)
	  fun doSrc (TG.SrcVar(x, _)) = V.clrCnt x
	    | doSrc (TG.SrcFlatDist(x, _)) = V.clrCnt x
	    | doSrc (TG.SrcSegDist(x, _, _)) = V.clrCnt x
	    | doSrc (TG.SrcFlatIndex(x, _, _)) = V.clrCnt x
	    | doSrc (TG.SrcSegIndex(x, _, _, _)) = V.clrCnt x
	  fun doExp (GPU.ExpLet(lhs, e1, e2)) = (doExp e1; List.app V.clrCnt lhs; doExp e2)
	    | doExp (GPU.ExpPrim(x, _, args, e)) = (List.app incAtom args; V.clrCnt x; doExp e)
	    | doExp (GPU.ExpIf(cond, e1, e2)) = (incAtom cond; doExp e1; doExp e2)
	    | doExp (GPU.ExpTuple args) = List.app incAtom args
	  fun doNode (TG.NodeMap(lhs, e)) = (doExp e; List.app V.clrCnt lhs)
	    | doNode (TG.NodeFlatScan(x, _, y)) = (V.incCnt y; V.clrCnt x)
	    | doNode (TG.NodeSegScan(x, _, seg, data)) = (V.incCnt seg; V.incCnt data; V.clrCnt x)
	  fun doSink (TG.SinkVar(x, y)) = (V.clrCnt x; V.incCnt y)
	    | doSink (TG.SinkFlatReduce(x, _, y)) = (V.clrCnt x; V.incCnt y)
	    | doSink (TG.SinkSegReduce(x, _, seg, data)) = (V.clrCnt x; V.incCnt seg; V.incCnt data)
	  in
	    List.app V.incCnt inputs;
	    List.app doSrc srcs;
	    List.app doNode body;
	    List.app doSink sinks
	  end

    fun census (Cu.Program(exp, _)) = let
	  fun doExp (_, e) = (case e
		 of Cu.ExpFun{f, params, body, cont, ...} => (
		      List.app V.clrCnt params;
		      doExp body;
		      doExp cont)
		  | Cu.ExpLet{lhs, rhs, cont} => (
		      doExp rhs;
		      List.app V.clrCnt lhs;
		      doExp cont)
		  | Cu.ExpTask{task, cont} => (
		      doTaskGraph task;
		      doExp cont)
		  | Cu.ExpVectorOp{result, args, cont, ...} => (
		      List.app incAtom args;
		      V.clrCnt result;
		      doExp cont)
		  | Cu.ExpCPUOp{result, arg, cont, ...} => (
		      incAtom arg;
		      V.clrCnt result;
		      doExp cont)
		  | Cu.ExpSeq{result, args, cont} => (
		      List.app incAtom args;
		      V.clrCnt result;
		      doExp cont)
		  | Cu.ExpTuple{result, args, cont} => (
		      List.app incAtom args;
		      V.clrCnt result;
		      doExp cont)
		  | Cu.ExpPrim{result, args, cont, ...} => (
		      List.app incAtom args;
		      V.clrCnt result;
		      doExp cont)
		  | Cu.ExpCmd{results, args, cont, ...} => (
		      List.app incAtom args;
		      List.app V.clrCnt results;
		      doExp cont)
		  | Cu.ExpIf{cond, trueExp, falseExp} => (
		      V.incCnt cond;
		      doExp trueExp;
		      doExp falseExp)
		  | Cu.ExpApply{args, ...} => List.app incAtom args
		  | Cu.ExpReturn args => List.app incAtom args
		(* end case *))
	  in
	    doExp exp
	  end

  end
