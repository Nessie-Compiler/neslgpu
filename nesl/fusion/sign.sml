(* sign.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Analyse the sign (# inputs and outputs) of VCode
 *)

structure Sign : sig

  (* compute the "sign" of an opcode, which is a pair of integers describing
   * the number of stack inputs and outputs (resp.).  This function should
   * not be called on CALL or IF opcodes.
   *)
    val signOfOp : VCode.opcode -> (int * int)

  (* analyze a program returning a function that maps function labels to the
   * sign of function.
   *)
    val analyze : VCodeCG.cg -> VCode.label -> (int * int)

  (* compute the sign of a block, assuming that we've already computed the signs of the
   * functions in the program.
   *)
    val signOfBlock : (VCode.label -> (int * int)) -> VCode.opcode list -> (int * int)

  end = struct

    structure V = VCode
    structure CG = VCodeCG
    structure ATbl = AtomTable

  (* compute the "sign" of an opcode, which is a pair of integers describing
   * the number of stack inputs and outputs (resp.).  This function should
   * not be called on CALL or IF opcodes.
   * See vcode_table.c for this info.
   *)
    fun signOfOp opcode = (case opcode
           of V.ADD _ => (2, 1)
            | V.SUB _ => (2, 1)
            | V.MUL _ => (2, 1)
            | V.DIV _ => (2, 1)
            | V.MOD => (2, 1)
            | V.LT _ => (2, 1)
            | V.LTE _ => (2, 1)
            | V.GT _ => (2, 1)
            | V.GTE _ => (2, 1)
            | V.EQ _ => (2, 1)
            | V.NEQ _ => (2, 1)
            | V.LSHIFT => (2, 1)
            | V.RSHIFT => (2, 1)
            | V.NOT _ => (1, 1)
            | V.AND _ => (2, 1)
            | V.OR _ => (2, 1)
            | V.XOR _ => (2, 1)
            | V.SELECT _ => (3, 1)
            | V.RAND => (1, 1)
            | V.FLOOR => (1, 1)
            | V.CEIL => (1, 1)
            | V.TRUNC => (1, 1)
            | V.ROUND => (1, 1)
            | V.I_TO_F => (1, 1)
            | V.I_TO_B => (1, 1)
            | V.B_TO_I => (1, 1)
            | V.LOG => (1, 1)
            | V.SQRT => (1, 1)
            | V.EXP => (1, 1)
            | V.SIN => (1, 1)
            | V.COS => (1, 1)
            | V.TAN => (1, 1)
            | V.ASIN => (1, 1)
            | V.ACOS => (1, 1)
            | V.ATAN => (1, 1)
            | V.SINH => (1, 1)
            | V.COSH => (1, 1)
            | V.TANH => (1, 1)
          (* Vector instructions *)
            | V.ADD_SCAN _ => (2, 1)
            | V.MUL_SCAN _ => (2, 1)
            | V.MAX_SCAN _ => (2, 1)
            | V.MIN_SCAN _ => (2, 1)
            | V.AND_SCAN _ => (2, 1)
            | V.OR_SCAN _ => (2, 1)
            | V.XOR_SCAN _ => (2, 1)
            | V.ADD_REDUCE _ => (2, 1)
            | V.MUL_REDUCE _ => (2, 1)
            | V.MAX_REDUCE _ => (2, 1)
            | V.MIN_REDUCE _ => (2, 1)
            | V.AND_REDUCE _ => (2, 1)
            | V.OR_REDUCE _ => (2, 1)
            | V.XOR_REDUCE _ => (2, 1)
            | V.PERMUTE _ => (3, 1)
            | V.DPERMUTE _ => (5, 1)
            | V.FPERMUTE _ => (5, 1)
            | V.BPERMUTE _ => (4, 1)
            | V.BFPERMUTE _ => (5, 1)
            | V.DFPERMUTE _ => (6, 1)
            | V.EXTRACT _ => (3, 1)
            | V.REPLACE _ => (4, 1)
            | V.PACK _ => (3, 2)
            | V.RANK_UP _ => (2, 1)
            | V.RANK_DOWN _ => (2, 1)
            | V.DIST _ => (2, 1)
            | V.INDEX => (3, 1)
            | V.LENGTH _ => (1, 1)
          (* Segment descriptor instructions *)
            | V.MAKE_SEGDES => (1, 1)
            | V.LENGTHS => (1, 1)
          (* Control instructions *)
            | V.COPY(i, j) => (i+j, i+i+j)
            | V.POP(i, j) => (i+j, j)
            | V.CPOP(i, j) => (i+j, i+j)
            | V.PAIR => (2, 1)
            | V.UNPAIR => (1, 2)
            | V.CALL _ => raise Fail "unexpected CALL"
            | V.IF _ => raise Fail "unexpected IF"
            | V.CONST _ => (0, 1)
          (* I/O instructions *)
            | V.READ _ => (0, 3)
            | V.WRITE _ => (1, 2)
            | V.FOPEN => (2, 3)
            | V.FCLOSE => (1, 2)
            | V.FWRITE _ => (2, 2)
            | V.FREAD _ => (1, 3)
            | V.FREAD_CHAR => (3, 4)
          (* undocumented instuctions *)
            | V.EXIT => (0, 0)
(* values from vcode_table.c, but they don't seem right
            | V.START_TIMER => (0, 1)
            | V.STOP_TIMER => (1, 1)
*)
            | V.START_TIMER => (0, 0)
            | V.STOP_TIMER => (0, 1)
            | V.SRAND => (1, 1)
            | V.FUSED id => raise Fail "unexpected FUSED"
          (* end case *))

  (* update the cumulate stack-depth info with the effect of an opcode.  We use
   * curDepth = ~1 to represent that an EXIT has been encountered, which means that
   * this code is unreachable.
   *)
    fun update ((inSz, curDepth), (nArgs, nResults)) =
          if (nArgs <= curDepth)
            then (inSz, (curDepth - nArgs) + nResults)
          else if (curDepth >= 0)
            then (inSz + (nArgs - curDepth), nResults)
            else (Int.max(inSz, nArgs), ~1)

  (* merge the cumulative stack-depth info from two branches; if one branch ends in an exit
   * then we use the depth from the other branch.
   *)
    fun merge (lab, (in1, d1 : int), (in2, d2)) =
          if (d1 < 0) orelse (d2 < 0)
            then (Int.max(in1, in2), Int.max(d1, d2))
          else if (d1-in1 <> d2-in2)
            then raise Fail(concat[
                "then(", Int.toString in1, ",", Int.toString d1, ")/else(",
                Int.toString in2, ",", Int.toString d2,
                ") depth mismatch in ", Atom.toString lab
              ])
            else (Int.max(in1, in2), Int.max(d1, d2))

fun s2s (inSz, d) = concat["(", Int.toString inSz, ",", Int.toString d, ")"]

    fun analyze {nodes, ndOfLab, topOrder} = let
          val nFuncs = List.length nodes
        (* maps labels to functions *)
          fun funcOfLab lab = let val CG.Nd(f, _, _) = ndOfLab lab in f end
(* FIXME: should attach sign to node property list *)
        (* maps labels to sign info *)
          val signTbl = let
                val tbl = ATbl.mkTable (nFuncs, Fail "signTbl")
                val insert = ATbl.insert tbl
                in
                  List.app (fn (CG.Nd(V.FUNC(lab, _), _, _)) => insert(lab, (0, ~1))) nodes;
                  tbl
                end
          val signOfLab = ATbl.lookup signTbl 
        (* compute the sign of a function; return true if the sign changed from its
         * previous value.
         *)
          fun computeSign (V.FUNC(lab, body)) = let
                fun signOfBlock (code, sgn) = let
                      fun signOf ([], sgn) = sgn
                        | signOf (_, sgn as (_, ~1)) = sgn (* unreachable *)
                        | signOf (V.EXIT::_, sgn as (nArgs, _)) = (nArgs, ~1)
                        | signOf (V.CALL f::r, sgn) = let
                            val sgn' = update(sgn, signOfLab f)

                            in
(*
print(concat["signOf (CALL ", Atom.toString f, ") ", s2s sgn, " --> ", s2s sgn', "\n"]);
*)
                              if (#2 sgn' < 0)
                                then sgn' (* recursive call to function with unknown sign *)
                                else signOf (r, sgn')
                            end
                        | signOf (V.IF(b1, b2)::r, sgn) = let
                            val sgn = update(sgn, (1, 0))
                            val s1 = signOfBlock(b1, (0, 0))
                            val s2 = signOfBlock(b2, (0, 0))
(*
val _ = print(concat["signOf (ENDIF): s1 = ", s2s s1, ", s2 = ", s2s s2, "\n"]);
*)
                            val sgn' = merge (lab, s1, s2)
                            in
                              if (#2 sgn' < 0)
                                then sgn'
                                else signOf (r, update(sgn, sgn'))
                            end
                        | signOf (opcode::r, sgn) = signOf (r, update (sgn, signOfOp opcode))
(*
let val sgn' = update (sgn, signOfOp opcode) in
print(concat["signOf (", VCode.toString opcode, ") ", s2s sgn, " --> ", s2s sgn', "\n"]);
signOf (r, sgn')
end
*)
                      in
                        signOf (code, sgn)
                      end
(*
val _ = print(concat["computeSign ", Atom.toString lab, ": ", s2s(signOfLab lab), "\n"]);
*)
                val sgn = signOfBlock (body, (0, 0))
                val changed = (signOfLab lab <> sgn)
                in
                  ATbl.insert signTbl (lab, sgn);
                  changed
                end
          fun signOfNd (CG.Nd(f, _, _)) = computeSign f
          fun signOfGrp (CG.SIMPLE nd) = ignore (signOfNd nd)
            | signOfGrp (CG.RECURSIVE nds) = let
                fun lp () = if (List.exists signOfNd nds)
                      then lp()
                      else () (* we reached the fixed point *)
                in
                  lp()
                end
          val _ = List.app signOfGrp (List.rev topOrder)
(* DEBUG *
val _ = List.app (fn (V.FUNC(lab, _)) => let
val (nArgs, nResults) = signOfLab lab
in
print(concat["FUNC ", Atom.toString lab, " ", Int.toString nArgs, " -> ", Int.toString nResults, "\n"])
end) funcs
* DEBUG *)
          in
            signOfLab
          end

    val a_signOfBlock = Atom.atom "signOfBlock"

  (* compute the sign of a block, assuming that we've already computed the signs of the
   * functions in the program.
   *)
    fun signOfBlock signOfLab code = let
          fun signOfBlk (code, sgn) = let
                fun signOf ([], sgn) = sgn
                  | signOf (_, sgn as (_, ~1)) = sgn (* unreachable *)
                  | signOf (V.EXIT::_, sgn as (nArgs, _)) = (nArgs, ~1)
                  | signOf (V.CALL f::r, sgn) = signOf (r, update(sgn, signOfLab f))
                  | signOf (V.IF(b1, b2)::r, sgn) = let
                      val sgn = update(sgn, (1, 0))
                      val s1 = signOfBlk(b1, (0, 0))
                      val s2 = signOfBlk(b2, (0, 0))
                      in
                        signOf (r, update(sgn, merge (a_signOfBlock, s1, s2)))
                      end
                  | signOf (opcode::r, sgn) = signOf (r, update (sgn, signOfOp opcode))
                in
                  signOf (code, sgn)
                end
          in
            signOfBlk (code, (0, 0))
          end

  end
