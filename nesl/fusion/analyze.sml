(* analyze.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * An analysis of VCode that does some stack simulation.
 *)

structure Analyze =
  struct

    structure V = VCode
    structure CG = VCodeCG
    structure ATbl = AtomTable

  (***** Symbolic stack evaluation *****)

    datatype stk_value
      = ARG of int
      | EXP of V.opcode * stk_value list
      | UNKNOWN

    fun sameValue (ARG i1, ARG i2) = (i1 = i2)
(* FIXME
      | sameValue (EXP(op1, args1), EXP(op2, args2)) =
          V.sameOpcode(op1, op2) andalso ListPair.all sameValue (args1, args2)
*)
      | sameValue (UNKNOWN, UNKNOWN) = true
      | sameValue _ = false

    fun valueToString (ARG i) = "ARG" ^ Int.toString i
      | valueToString (EXP(opcode, [])) = concat["(", V.toString opcode, ")"]
      | valueToString (EXP(opcode, args)) = concat[
            "(", V.toString opcode, " @ ", String.concatWith " " (List.map valueToString args), ")"
          ]
      | valueToString UNKNOWN = "??"

    fun stackToString stk = String.concatWith " " (List.map valueToString stk)

  (* symbolic evaluation of a call, where stk is the incoming stack and (nParams, results)
   * represent the approximate effect of the function.
   *)
    fun evalCall (nParams, results, stk) = let
          fun actual i = List.nth(stk, nParams - i - 1)
        (* mapping from function parameter slots to arguments *)
          val mapping = Array.tabulate(nParams, actual)
        (* rewrite results *)
          fun rewrite (ARG i) = Array.sub(mapping, i-1)
            | rewrite (EXP(opcode, args)) = EXP(opcode, List.map rewrite args)
            | rewrite UNKNOWN = UNKNOWN
          in
            List.map rewrite results @ List.drop(stk, nParams)
          end

    fun eval signOfLab {nodes, ndOfLab, topOrder} = let
          val nFuncs = List.length nodes
        (* maps labels to function info *)
          val infoTbl = let
                val tbl = ATbl.mkTable (nFuncs, Fail "infoTbl")
                val insert = ATbl.insert tbl
                fun insInfo (CG.Nd(V.FUNC(lab, _), _, _)) = let
                      val (nParams, nResults) = signOfLab lab
                      in
                        insert (lab, {
                            nParams = nParams,
                            results = ref(List.tabulate(nResults, fn _ => UNKNOWN))
                          })
                      end
                in
                  List.app insInfo nodes;
                  tbl
                end
          val infoOfLab = ATbl.lookup infoTbl 
        (* functions to manipulate the stack *)
          fun expr1 (opcode, x::stk) = EXP(opcode, [x])::stk
          fun expr2 (opcode, y::x::stk) = EXP(opcode, [x, y])::stk
          fun expr3 (opcode, z::y::x::stk) = EXP(opcode, [x, y, z])::stk
          fun expr4 (opcode, x3::x2::x1::x0::stk) =
                EXP(opcode, [x0, x1, x2, x3])::stk
          fun expr5 (opcode, x4::x3::x2::x1::x0::stk) =
                EXP(opcode, [x0, x1, x2, x3, x4])::stk
          fun expr6 (opcode, x5::x4::x3::x2::x1::x0::stk) =
                EXP(opcode, [x0, x1, x2, x3, x4, x5])::stk
          fun splitStk (stk, n) = let
                fun lp (0, prefix, rest) = (prefix, rest)
                  | lp (i, prefix, x::rest) = lp (i-1, x::prefix, rest)
                in
                  lp (n, [], stk)
                end
          fun unknown (nArgs, nResults, stk) =
                List.tabulate(nResults, fn _ => UNKNOWN) @ List.drop(stk, nArgs)
        (* symbolically evaluate a function *)
          fun evalFunc (V.FUNC(lab, body)) = let
                fun evalOp (opcode, stk) = (
(* DEBUG *)
print(concat["  ", V.toString opcode, ":  ", stackToString stk, "\n"]);
                      case opcode
                       of V.ADD _ => expr2 (opcode, stk)
                        | V.SUB _ => expr2 (opcode, stk)
                        | V.MUL _ => expr2 (opcode, stk)
                        | V.DIV _ => expr2 (opcode, stk)
                        | V.MOD => expr2 (opcode, stk)
                        | V.LT _ => expr2 (opcode, stk)
                        | V.LTE _ => expr2 (opcode, stk)
                        | V.GT _ => expr2 (opcode, stk)
                        | V.GTE _ => expr2 (opcode, stk)
                        | V.EQ _ => expr2 (opcode, stk)
                        | V.NEQ _ => expr2 (opcode, stk)
                        | V.LSHIFT => expr2 (opcode, stk)
                        | V.RSHIFT => expr2 (opcode, stk)
                        | V.NOT _ => expr1 (opcode, stk)
                        | V.AND _ => expr2 (opcode, stk)
                        | V.OR _ => expr2 (opcode, stk)
                        | V.XOR _ => expr2 (opcode, stk)
                        | V.SELECT _ => expr3 (opcode, stk)
                        | V.RAND => expr1 (opcode, stk)
                        | V.FLOOR => expr1 (opcode, stk)
                        | V.CEIL => expr1 (opcode, stk)
                        | V.TRUNC => expr1 (opcode, stk)
                        | V.ROUND => expr1 (opcode, stk)
                        | V.I_TO_F => expr1 (opcode, stk)
                        | V.I_TO_B => expr1 (opcode, stk)
                        | V.B_TO_I => expr1 (opcode, stk)
                        | V.LOG => expr1 (opcode, stk)
                        | V.SQRT => expr1 (opcode, stk)
                        | V.EXP => expr1 (opcode, stk)
                        | V.SIN => expr1 (opcode, stk)
                        | V.COS => expr1 (opcode, stk)
                        | V.TAN => expr1 (opcode, stk)
                        | V.ASIN => expr1 (opcode, stk)
                        | V.ACOS => expr1 (opcode, stk)
                        | V.ATAN => expr1 (opcode, stk)
                        | V.SINH => expr1 (opcode, stk)
                        | V.COSH => expr1 (opcode, stk)
                        | V.TANH => expr1 (opcode, stk)
                      (* Vector instructions *)
                        | V.ADD_SCAN _ => expr2 (opcode, stk)
                        | V.MUL_SCAN _ => expr2 (opcode, stk)
                        | V.MAX_SCAN _ => expr2 (opcode, stk)
                        | V.MIN_SCAN _ => expr2 (opcode, stk)
                        | V.AND_SCAN _ => expr2 (opcode, stk)
                        | V.OR_SCAN _ => expr2 (opcode, stk)
                        | V.XOR_SCAN _ => expr2 (opcode, stk)
                        | V.ADD_REDUCE _ => expr2 (opcode, stk)
                        | V.MUL_REDUCE _ => expr2 (opcode, stk)
                        | V.MAX_REDUCE _ => expr2 (opcode, stk)
                        | V.MIN_REDUCE _ => expr2 (opcode, stk)
                        | V.AND_REDUCE _ => expr2 (opcode, stk)
                        | V.OR_REDUCE _ => expr2 (opcode, stk)
                        | V.XOR_REDUCE _ => expr2 (opcode, stk)
                        | V.PERMUTE _ => expr3 (opcode, stk)
                        | V.DPERMUTE _ => expr5 (opcode, stk)
                        | V.FPERMUTE _ => expr5 (opcode, stk)
                        | V.BPERMUTE _ => expr4 (opcode, stk)
                        | V.BFPERMUTE _ => expr5 (opcode, stk)
                        | V.DFPERMUTE _ => expr6 (opcode, stk)
                        | V.EXTRACT _ => expr3 (opcode, stk)
                        | V.REPLACE _ => expr4 (opcode, stk)
                        | V.PACK _ => raise Fail "TODO: PACK"
                        | V.RANK_UP _ => expr2 (opcode, stk)
                        | V.RANK_DOWN _ => expr2 (opcode, stk)
                        | V.DIST _ => expr2 (opcode, stk)
                        | V.INDEX => expr3 (opcode, stk)
                        | V.LENGTH _ => expr1 (opcode, stk)
                      (* Segment descriptor instructions *)
                        | V.MAKE_SEGDES => expr1 (opcode, stk)
                        | V.LENGTHS => expr1 (opcode, stk)
                      (* Control instructions *)
                        | V.COPY(i, j) => let
                            val copied = List.take(List.drop(stk, j), i)
                            in
(* FIXME: do we want to record the sharing? *)
                              copied @ stk
                            end
                        | V.POP(i, 0) => List.drop(stk, i)
                        | V.POP(i, j) => let
                            val (prefix, rest) = splitStk (stk, j)
                            in
                              List.revAppend (prefix, List.drop(rest, i))
                            end
                        | V.CPOP(i, j) => let
                            val (prefix, rest) = splitStk (stk, j)
                            val (moved, rest) = splitStk (rest, i)
                            in
                              List.revAppend (moved, List.revAppend(prefix, rest))
                            end
                        | V.PAIR => UNKNOWN :: List.drop(stk, 2)
                        | V.UNPAIR => UNKNOWN :: UNKNOWN :: List.tl stk
                        | V.CALL f => let
                            val {nParams, results} = infoOfLab f
                            in
                              evalCall (nParams, !results, stk)
                            end
                        | V.IF(b1, b2) => let
                            val stk1 = evalBlock(b1, stk)
                            val stk2 = evalBlock(b2, stk)
                            fun merge (v1, v2) = if sameValue(v1, v2) then v1 else UNKNOWN
                            in
                              ListPair.mapEq merge (stk1, stk2)
                            end
                        | V.CONST _ => EXP(opcode, []) :: stk
                      (* I/O instructions *)
                        | V.READ _ => UNKNOWN::UNKNOWN::UNKNOWN::stk
                        | V.WRITE _ => unknown (1, 2, stk)
                        | V.FOPEN => unknown (2, 3, stk)
                        | V.FCLOSE => unknown (1, 2, stk)
                        | V.FWRITE _ => unknown (2, 2, stk)
                        | V.FREAD _ => unknown (1, 3, stk)
                        | V.FREAD_CHAR => unknown (3, 4, stk)
                      (* undocumented instuctions *)
                        | V.EXIT => stk
(* values from vcode_table.c, but they don't seem right
                        | V.START_TIMER => UNKNOWN::stk
                        | V.STOP_TIMER => UNKNOWN::(List.tl stk)
*)
                        | V.START_TIMER => stk
                        | V.STOP_TIMER => UNKNOWN::stk
                        | V.SRAND => UNKNOWN::(List.tl stk)
                        | V.FUSED id => raise Fail "unexpected FUSED"
                      (* end case *))
                and evalBlock (code, stk) = List.foldl evalOp stk code
                val (nParams, nResults) = signOfLab lab
val _ = print(concat["eval ", Atom.toString lab, " ", Int.toString nParams, " paramaters\n"]);
                val params = List.tabulate (nParams, fn i => ARG(nParams - i))
                in
                  #results(infoOfLab lab) := evalBlock (body, params)
                end
          fun evalGrp (CG.SIMPLE(CG.Nd(f, _, _))) = evalFunc f
            | evalGrp (CG.RECURSIVE nds) = raise Fail "unimplemented"
          in
            List.app evalGrp (List.rev topOrder)
          end


    fun analyze prog = let
        (* compute program call graph *)
          val cg = CG.mkCallGraph prog
        (* compute signs of functions *)
          val signOfLab = Sign.analyze cg
        (* symbol evaluation of functions *)
          val _ = eval signOfLab cg
          in
            ()
          end

  end
