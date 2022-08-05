(* inliner.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Inliner for VCode
 *)

structure Inliner : sig

    val inlineThreshold : int ref

    val inline : VCode.program -> VCode.program

  end = struct

    structure V = VCode
    structure CG = VCodeCG
    structure ATbl = AtomTable
    structure ASet = AtomSet

    val inlineThreshold = ref 2         (* functions bigger than this are not inlined *)

  (* apply a function to the labels of call sites in a code block *)
    fun applyToCalls f = let
          fun apply (V.CALL lab) = f lab
            | apply (V.IF(b1, b2)) = (apply' b1; apply' b2)
            | apply _ = ()
          and apply' code = List.app apply code
          in
            apply'
          end

(*DEBUG*)
  (* check the result of inlining by making sure that all called functions are defined *)
    fun check prog = let
          val anyErrors = ref false
          val funcs = List.foldl (fn (V.FUNC(lab, _), s) => ASet.add(s, lab)) ASet.empty prog
          fun checkFunc (V.FUNC(f, code)) = let
                fun checkCall lab = if ASet.member(funcs, lab)
                      then ()
                      else (
                        anyErrors := true;
                        print(concat[
                            "call to undefined ", Atom.toString lab, " in ", Atom.toString f, "\n"
                          ]))
                in
                  applyToCalls checkCall code
                end
          in
            List.app checkFunc prog;
            if !anyErrors then raise Fail "inlining failed" else prog
          end
(* DEBUG *)

  (* call-graph property for tracking function size *)
    local
      val {getFn, setFn, ...} = CG.newProp(fn _ => 0)
    in
    val sizeOf = getFn
    val setSize = setFn
    end

  (* call-graph property for tracking number of calls *)
    local
      val {getFn, ...} = CG.newProp(fn _ => ref 0)
    in
    fun useCnt nd = !(getFn nd)
    fun incCnt nd = let val cnt = getFn nd in cnt := !cnt+1 end
    fun decCnt nd = let val cnt = getFn nd val n = !cnt-1 in cnt := n; n end
    end

  (* call-graph property for marking recursive functions *)
    local
      val {getFn, setFn} = CG.newFlag()
    in
    val isRec = getFn
    fun markRec nd = setFn(nd, true)
    end

  (* call-graph property for expanded function bodies *)
    local
      val {getFn, ...} = CG.newProp(fn (CG.Nd(f, _, _)) => ref f)
    in
    fun funcOfNd nd = !(getFn nd)
    fun bodyOfNd nd = let val V.FUNC(_, body) = !(getFn nd) in body end
    fun setFuncOfNd (nd, func) = getFn nd := func
    end

val xx60 = Atom.atom "++_60"

  (* compute the size of a function *)
    fun sizeOfFunc ndOfLab (V.FUNC(lab, b))= let
          fun size (opcode, sz) = (case opcode
                 of V.COPY(i, j) => sz
                  | V.POP(i, j) => sz
                  | V.CPOP(i, j) => sz
                  | V.PAIR => sz
                  | V.UNPAIR => sz
                  | V.EXIT => sz
                  | V.IF(b1, b2) => sizeOfBlk(b2, sizeOfBlk(b1, sz))
                  | V.CALL f => (incCnt(ndOfLab f); sz)
                  | _ => 1 + sz
                (* end case *))
          and sizeOfBlk (b, sz) = List.foldl size sz b
	  in
	    setSize (ndOfLab lab, sizeOfBlk (b, 0))
	  end

  (* compute the sizes of functions *)
    fun computeSizes (ndOfLab, prog) = (
	(* the first function is the main routine, which is called from the runtime. *)
	  case prog of V.FUNC(lab, _)::_ => incCnt(ndOfLab lab);
	(* compute sizes *)
	  List.app (sizeOfFunc ndOfLab) prog)

  (* should a function call be inlined? *)
    fun shouldInline nd = not(isRec nd)
          andalso ((useCnt nd = 1) orelse (sizeOf nd <= !inlineThreshold))

    fun inline prog = let
        (* build and sort the program's call graph *)
          val {nodes, ndOfLab, topOrder} = CG.mkCallGraph prog
        (* compute function sizes *)
          val _ = computeSizes (ndOfLab, prog)
        (* mark recursive functions *)
          val _ = let
                fun f (CG.SIMPLE _) = ()
                  | f (CG.RECURSIVE nds) = List.app markRec nds
                in
                  List.app f topOrder
                end
(*
val _ = let
fun prGrp (CG.SIMPLE nd) = print(concat["  (", Int.toString(useCnt nd), ") ", CG.nodeToString nd, "\n"])
  | prGrp (CG.RECURSIVE nds) = (
      print "  RECURSIVE\n";
      List.app (fn nd => print(concat["    (", Int.toString(useCnt nd), ") ", CG.nodeToString nd, "\n"])) nds)
in
print "***** Call graph *****\n";
List.app prGrp topOrder;
print "*****\n"
end
*)
	(* for recomputing the size of a function after inlining *)
	  val sizeOfFunc = sizeOfFunc ndOfLab
        (* inline-expand calls within a function *)
          fun expandFunc (nd as CG.Nd(V.FUNC(lab, _), _, _)) = let
(*val _ = print(concat["expandFunc: ", CG.nodeToString nd, "\n"])*)
                fun expand (prefix, []) = List.rev prefix
                  | expand (prefix, V.CALL lab::r) = let
                      val nd = ndOfLab lab
                      in
                        if shouldInline nd
                          then let
                            val body = bodyOfNd nd
                            in
(*print(concat["  inline ", Atom.toString lab, "#", Int.toString(useCnt nd), "\n"]);*)
			      if (decCnt nd > 0)
				then applyToCalls (incCnt o ndOfLab) body
				else setFuncOfNd(nd, V.FUNC(lab, []));
                              expand(List.revAppend(body, prefix), r)
                            end
                          else expand(V.CALL lab :: prefix, r)
                      end
                  | expand (prefix, V.IF(b1, b2)::r) =
                      expand (V.IF(expandBlk b1, expandBlk b2)::prefix, r)
                  | expand (prefix, opcode::r) = expand(opcode::prefix, r)
                and expandBlk b = expand ([], b)
		val newFunc = V.FUNC(lab, expandBlk(bodyOfNd nd))
                in
		  sizeOfFunc newFunc;
                  setFuncOfNd(nd, newFunc)
                end
        (* expand a group of functions *)
          fun expandGrp (CG.SIMPLE nd) = expandFunc nd
            | expandGrp (CG.RECURSIVE nds) = List.app expandFunc nds
          val _ = List.app expandGrp (List.rev topOrder)
        (* filter functions that are not called; we decrement the call counts for the
         * expanded body of the function.
         *)
          fun removeUncalled nd = let
                val f as V.FUNC(_, code) = funcOfNd nd
                in
                  if (useCnt nd > 0)
                    then SOME f
                    else (applyToCalls (ignore o decCnt o ndOfLab) code; NONE)
                end
          in
            check (List.mapPartial removeUncalled nodes)
          end

  end
