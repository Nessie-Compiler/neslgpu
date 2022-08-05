(* liveness.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Liveness analysis for CPU-side references to sequences.
 *)

structure Liveness : sig

    structure VSet : ORD_SET where type Key.ord_key = CuLambda.var

    val analyze : CuLambda.exp -> unit

    val liveOut : CuLambda.exp -> VSet.set

  end = struct

    structure Cu = CuLambda
    structure V = CuVar
    structure VSet = RedBlackSetFn (
      struct
	type ord_key = Cu.var
	val compare = V.compare
      end)

  (* property attached to exp nodes to track *)
    local
      val {getFn, setFn, ...} = PropList.newProp (fn (e : Cu.exp) => #1 e, fn _ => VSet.empty)
    in
    val setLiveOut = setFn
    val liveOut = getFn
    end

    fun killOne (live, x) = VSet.delete(live, x) handle _ => live
    fun kill (live, xs) = VSet.difference(live, VSet.fromList xs)
    fun gen (live, xs) = List.foldl VSet.add' live xs
    fun gen' (live, atms) = List.foldl (fn (Cu.Var x, s) => VSet.add(s, x) | (_, s) => s) live atms

  (* analyse variable liveness for a function, annotating the expressions with their
   * liveOut sets.  This analysis focuses on sequences, so we ignore scalars.  Also,
   * since the IR does not have loops, we can do the analysis in one bottom-up pass
   * over the function's body.
   *)
    fun analyseFunct (f, params, body) = let
	  fun analyseExp (exp : Cu.exp, liveOut) = (case #2 exp
		 of Cu.ExpFun{f, params, body, cont} => let
		      val _ = analyseFunct (f, params, body);
		      val liveOut = analyseExp (cont, liveOut)
		      in
			setLiveOut (exp, liveOut);
			liveOut
		      end
		  | Cu.ExpLet{lhs, rhs, cont} => let
		      val liveOut = analyseExp (cont, liveOut)
		      val liveIn = kill(analyseExp (rhs, liveOut), lhs)
		      in
			setLiveOut (exp, liveOut);
			liveIn
		      end
		  | Cu.ExpTask{task, cont} => let
		      val liveOut = analyseExp (cont, liveOut)
		      val liveIn = gen (kill(liveOut, Cu.TaskGraph.outputs task), Cu.TaskGraph.inputs task)
		      in
			setLiveOut (exp, liveOut);
			liveIn
		      end
		  | Cu.ExpVectorOp{result, isFlat, rator, args, cont} => let
		      val liveOut = analyseExp (cont, liveOut)
		      val liveIn = gen' (killOne(liveOut, result), args)
		      in
			setLiveOut (exp, liveOut);
			liveIn
		      end
		  | Cu.ExpCPUOp{result, rator, arg, cont} => let
		      val liveOut = analyseExp (cont, liveOut)
		      val liveIn = gen' (killOne(liveOut, result), [arg])
		      in
			setLiveOut (exp, liveOut);
			liveIn
		      end
		  | Cu.ExpSeq{result, args, cont} => let
		      val liveOut = analyseExp (cont, liveOut)
		      val liveIn = gen' (killOne(liveOut, result), args)
		      in
			setLiveOut (exp, liveOut);
			liveIn
		      end
		  | Cu.ExpTuple{result, args, cont} => let
		      val liveOut = analyseExp (cont, liveOut)
		      val liveIn = gen' (killOne(liveOut, result), args)
		      in
			setLiveOut (exp, liveOut);
			liveIn
		      end
		  | Cu.ExpPrim{result, rator, args, cont} => let
		      val liveOut = analyseExp (cont, liveOut)
		      in
		      (* since we are only tracking sequences, not scalars, we can ignore this node *)
			setLiveOut (exp, liveOut);
			liveOut
		      end
		  | Cu.ExpCmd{results, cmd, args, cont} => let
		      val liveOut = analyseExp (cont, liveOut)
		      val liveIn = gen' (kill(liveOut, results), args)
		      in
			setLiveOut (exp, liveOut);
			liveIn
		      end
		  | Cu.ExpIf{cond, trueExp, falseExp} => let
		      val liveOut1 = analyseExp (trueExp, liveOut)
		      val liveOut2 = analyseExp (falseExp, liveOut)
		      val liveOut = VSet.union(liveOut1, liveOut2)
		      in
		      (* since we are only tracking sequences, not scalars, we ignore the cond *)
			setLiveOut (exp, liveOut);
			liveOut
		      end
		  | Cu.ExpApply{f, args} => (
		      setLiveOut (exp, liveOut);
		      gen' (liveOut, args))
		  | Cu.ExpReturn args => (
		      setLiveOut (exp, liveOut);
		      gen' (liveOut, args))
		(* end case *))
	  in
	    ignore (analyseExp (body, VSet.empty))
	  end

    fun analyze prog = analyseFunct("", [], prog)

  end
