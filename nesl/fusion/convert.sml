(* convert.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Convert VCODE programs to the expression representation.
 *)

structure Convert =
  struct

    structure VC = VCode
    structure VE = VExp
    structure Ty = VCodeTyping

    type stack = VE.var list

(*DEBUG*)
fun stkToString stk = String.concat(List.map (fn x => " " ^ VE.varToString false x) stk) ^ " []"
fun stkTyToString stk = String.concat(List.map (fn (VE.V{ty, ...}) => " " ^ VC.tyToString ty) stk) ^ " []"
(*DEBUG*)

    fun inc (VE.V{useCnt, ...}) = useCnt := !useCnt + 1
    fun use x = (inc x; VE.VAR x)

    val newVar = VE.newVar

    fun splitStk (stk, n) = let
          fun lp (0, prefix, rest) = (prefix, rest)
            | lp (i, prefix, x::rest) = lp (i-1, x::prefix, rest)
	    | lp _ = raise Fail(concat["splitStk (", stkTyToString stk, ", ", Int.toString n, ")"])
          in
            lp (n, [], stk)
          end

  (* convert a function from VCODE to expression form *)
    fun cvtFunc typing (VC.FUNC(lab, body)) = let
	  val typeOfLab = Ty.typeOfLab typing
          val signOfIf = Ty.typeOfIf typing
          fun cvtOp (stk, opcode, k : stack -> VE.stm) = let
(*
val _ = print(concat["cvtOp ", VC.toString opcode, ": stk = ", stkTyToString stk, "\n"])
*)
                fun letOp (ty, opcode, args, stk) = let
                      val rhs = VE.PURE(opcode, List.map use args)
                      val x = newVar(ty, VE.VB_LET rhs)
                      in
                        VE.LET(x, rhs, k (x :: stk))
                      end
                fun letOp1 (ty, opcode, x1::stk) = letOp(ty, opcode, [x1], stk)
                fun letOp2 (ty, opcode, x2::x1::stk) = letOp(ty, opcode, [x1, x2], stk)
                fun letOp3 (ty, opcode, x3::x2::x1::stk) = letOp(ty, opcode, [x1, x2, x3], stk)
                fun letOp4 (ty, opcode, x4::x3::x2::x1::stk) = letOp(ty, opcode, [x1, x2, x3, x4], stk)
                fun letOp5 (ty, opcode, x5::x4::x3::x2::x1::stk) = letOp(ty, opcode, [x1, x2, x3, x4, x5], stk)
                fun letOp6 (ty, opcode, x6::x5::x4::x3::x2::x1::stk) = letOp(ty, opcode, [x1, x2, x3, x4, x5, x6], stk)
                fun letStmt (opcode, nArgs, resultTys, stk) = let
                      val lhs = List.map (fn ty => newVar(ty, VE.VB_UNKNOWN)) resultTys
                      val (args, stk) = splitStk (stk, nArgs)
handle ex => raise ex
                      in
                        VE.STMT(lhs, opcode, List.map use args, k(List.revAppend(lhs, stk)))
                      end
                in
                  case opcode
                   of VC.ADD ty => letOp2 (ty, VE.ADD ty, stk)
                    | VC.SUB ty => letOp2 (ty, VE.SUB ty, stk)
                    | VC.MUL ty => letOp2 (ty, VE.MUL ty, stk)
                    | VC.DIV ty => letOp2 (ty, VE.DIV ty, stk)
                    | VC.MOD => letOp2 (VE.INT, VE.MOD, stk)
                    | VC.LT ty => letOp2 (VE.BOOL, VE.LT ty, stk)
                    | VC.LTE ty => letOp2 (VE.BOOL, VE.LTE ty, stk)
                    | VC.GT ty => letOp2 (VE.BOOL, VE.GT ty, stk)
                    | VC.GTE ty => letOp2 (VE.BOOL, VE.GTE ty, stk)
                    | VC.EQ ty => letOp2 (VE.BOOL, VE.EQ ty, stk)
                    | VC.NEQ ty => letOp2 (VE.BOOL, VE.NEQ ty, stk)
                    | VC.LSHIFT => letOp2 (VE.INT, VE.LSHIFT, stk)
                    | VC.RSHIFT => letOp2 (VE.INT, VE.RSHIFT, stk)
                    | VC.NOT ty => letOp1 (ty, VE.NOT ty, stk)
                    | VC.AND ty => letOp2 (ty, VE.AND ty, stk)
                    | VC.OR ty => letOp2 (ty, VE.OR ty, stk)
                    | VC.XOR ty => letOp2 (ty, VE.XOR ty, stk)
                    | VC.SELECT ty => letOp3 (ty, VE.SELECT ty, stk)
                    | VC.RAND => letOp1 (VE.INT, VE.RAND, stk)
                    | VC.FLOOR => letOp1 (VE.INT, VE.FLOOR, stk)
                    | VC.CEIL => letOp1 (VE.INT, VE.CEIL, stk)
                    | VC.TRUNC => letOp1 (VE.INT, VE.TRUNC, stk)
                    | VC.ROUND => letOp1 (VE.INT, VE.ROUND, stk)
                    | VC.I_TO_F => letOp1 (VE.FLOAT, VE.I_TO_F, stk)
                    | VC.I_TO_B => letOp1 (VE.BOOL, VE.I_TO_B, stk)
                    | VC.B_TO_I => letOp1 (VE.INT, VE.B_TO_I, stk)
                    | VC.LOG => letOp1 (VE.FLOAT, VE.LOG, stk)
                    | VC.SQRT => letOp1 (VE.FLOAT, VE.SQRT, stk)
                    | VC.EXP => letOp1 (VE.FLOAT, VE.EXP, stk)
                    | VC.SIN => letOp1 (VE.FLOAT, VE.SIN, stk)
                    | VC.COS => letOp1 (VE.FLOAT, VE.COS, stk)
                    | VC.TAN => letOp1 (VE.FLOAT, VE.TAN, stk)
                    | VC.ASIN => letOp1 (VE.FLOAT, VE.ASIN, stk)
                    | VC.ACOS => letOp1 (VE.FLOAT, VE.ACOS, stk)
                    | VC.ATAN => letOp1 (VE.FLOAT, VE.ATAN, stk)
                    | VC.SINH => letOp1 (VE.FLOAT, VE.SINH, stk)
                    | VC.COSH => letOp1 (VE.FLOAT, VE.COSH, stk)
                    | VC.TANH => letOp1 (VE.FLOAT, VE.TANH, stk)
                  (* Vector instructions *)
                    | VC.ADD_SCAN ty => letOp2 (ty, VE.ADD_SCAN ty, stk)
                    | VC.MUL_SCAN ty => letOp2 (ty, VE.MUL_SCAN ty, stk)
                    | VC.MAX_SCAN ty => letOp2 (ty, VE.MAX_SCAN ty, stk)
                    | VC.MIN_SCAN ty => letOp2 (ty, VE.MIN_SCAN ty, stk)
                    | VC.AND_SCAN ty => letOp2 (ty, VE.AND_SCAN ty, stk)
                    | VC.OR_SCAN ty => letOp2 (ty, VE.OR_SCAN ty, stk)
                    | VC.XOR_SCAN ty => letOp2 (ty, VE.XOR_SCAN ty, stk)
                    | VC.ADD_REDUCE ty => letOp2 (ty, VE.ADD_REDUCE ty, stk)
                    | VC.MUL_REDUCE ty => letOp2 (ty, VE.MUL_REDUCE ty, stk)
                    | VC.MAX_REDUCE ty => letOp2 (ty, VE.MAX_REDUCE ty, stk)
                    | VC.MIN_REDUCE ty => letOp2 (ty, VE.MIN_REDUCE ty, stk)
                    | VC.AND_REDUCE ty => letOp2 (ty, VE.AND_REDUCE ty, stk)
                    | VC.OR_REDUCE ty => letOp2 (ty, VE.OR_REDUCE ty, stk)
                    | VC.XOR_REDUCE ty => letOp2 (ty, VE.XOR_REDUCE ty, stk)
                    | VC.PERMUTE ty => letOp3 (ty, VE.PERMUTE ty, stk)
                    | VC.DPERMUTE ty => letOp5 (ty, VE.DPERMUTE ty, stk)
                    | VC.FPERMUTE ty => letOp5 (ty, VE.FPERMUTE ty, stk)
                    | VC.BPERMUTE ty => letOp4 (ty, VE.BPERMUTE ty, stk)
                    | VC.BFPERMUTE ty => letOp5 (ty, VE.BFPERMUTE ty, stk)
                    | VC.DFPERMUTE ty => letOp6 (ty, VE.DFPERMUTE ty, stk)
                    | VC.EXTRACT ty => letOp3 (ty, VE.EXTRACT ty, stk)
                    | VC.REPLACE ty => letOp4 (ty, VE.REPLACE ty, stk)
                    | VC.PACK ty => let
                      (* pack is a special case that returns two results *)
                        val (args, stk) = splitStk (stk, 3)
handle ex => raise ex
                        val args = List.map use args
                        val x = newVar (ty, VE.VB_UNKNOWN)
                        val y = newVar (VE.SEGDES, VE.VB_UNKNOWN)
                        in
                          VE.LETPACK(x, y, ty, args, k (y :: x :: stk))
                        end
                    | VC.RANK_UP ty => letOp2 (ty, VE.RANK_UP ty, stk)
                    | VC.RANK_DOWN ty => letOp2 (ty, VE.RANK_DOWN ty, stk)
                    | VC.DIST ty => letOp2 (ty, VE.DIST ty, stk)
                    | VC.INDEX => letOp3 (VE.INT, VE.INDEX, stk)
                    | VC.LENGTH ty => letOp1 (VE.INT, VE.LENGTH ty, stk)
                  (* Segment descriptor instructions *)
                    | VC.MAKE_SEGDES => letOp1 (VE.SEGDES, VE.MAKE_SEGDES, stk)
                    | VC.LENGTHS => letOp1 (VE.INT, VE.LENGTHS, stk)
                  (* Control instructions *)
                    | VC.COPY(i, j) => let
                        val copied = List.take(List.drop(stk, j), i)
                        in
                          k (copied @ stk)
                        end
                    | VC.POP(i, 0) => k (List.drop(stk, i))
                    | VC.POP(i, j) => let
                        val (prefix, rest) = splitStk (stk, j)
handle ex => raise ex
                        in
                          k (List.revAppend (prefix, List.drop(rest, i)))
                        end
                    | VC.CPOP(i, j) => let
                        val (prefix, rest) = splitStk (stk, j)
handle ex => raise ex
                        val (moved, rest) = splitStk (rest, i)
handle ex => raise ex
                        in
                          k (List.revAppend (moved, List.revAppend(prefix, rest)))
                        end
                    | VC.PAIR => raise Fail "PAIR unimplemented"
                    | VC.UNPAIR => raise Fail "UNPAIR unimplemented"
                    | VC.CALL f => let
                        val (paramTys, SOME resultTys) = typeOfLab f
                        val lhs = List.map (fn ty => newVar(ty, VE.VB_UNKNOWN)) resultTys
                        val (args, stk) = splitStk (stk, List.length paramTys)
handle ex => raise ex
                        in
                          VE.CALL(lhs, f, List.map use args, k(List.revAppend(lhs, stk)))
                        end
                    | VC.IF(b1, b2) => raise Fail "unexpected IF"
                    | VC.CONST(ty, values) => letOp(ty, VE.CONST(ty, values), [], stk)
                  (* I/O instructions *)
                    | VC.READ ty => letStmt(VE.READ ty, 0, [ty, VE.CHAR, VE.BOOL], stk)
                    | VC.WRITE ty => letStmt(VE.WRITE ty, 1, [VE.CHAR, VE.BOOL], stk)
                    | VC.FOPEN => letStmt(VE.FOPEN, 2, [VE.INT, VE.CHAR, VE.BOOL], stk)
                    | VC.FCLOSE => letStmt(VE.FCLOSE, 1, [VE.CHAR, VE.BOOL], stk)
                    | VC.FWRITE ty => letStmt(VE.FWRITE ty, 1, [VE.CHAR, VE.BOOL], stk)
                    | VC.FREAD ty => letStmt(VE.FREAD ty, 1, [ty, VE.CHAR, VE.BOOL], stk)
                    | VC.FREAD_CHAR => letStmt(VE.FREAD_CHAR, 3, [VE.CHAR, VE.INT, VE.CHAR, VE.BOOL], stk)
                  (* undocumented instuctions *)
                    | VC.EXIT => raise Fail "unexpected EXIT"
(* values from vcode_table.c, but they don't seem right
                    | VC.START_TIMER => letStmt(VE.START_TIMER, 0, 1, stk)
                    | VC.STOP_TIMER => letStmt(VE.STOP_TIMER, 1, 1, stk)
*)
                    | VC.START_TIMER => letStmt(VE.START_TIMER, 0, [], stk)
                    | VC.STOP_TIMER => letStmt(VE.STOP_TIMER, 0, [VE.FLOAT], stk)
                    | VC.SRAND => letStmt(VE.SRAND, 1, [VE.BOOL], stk)
                    | VC.FUSED _ => raise Fail "unexpected FUSED"
                  (* end case *)
                end
          and cvtBlock (stk, path, code, nResults) = let
                fun mkRET stk = VE.RET(List.map use (List.rev (List.take (stk, nResults))))
                fun cvtOne (stk, _, []) = mkRET stk
                  | cvtOne (stk, _, VC.EXIT :: _) = VE.EXIT
                  | cvtOne (stk, path, VC.IF(b1, b2) :: rest) = let
(*
val _ = print(concat["IF: stk tys = ", stkTyToString stk, "\n  sign = ", Ty.signToString(signOfIf path), "\n"])
*)
                      val ([cond], stk) = splitStk (stk, 1)
handle ex => raise ex
		      in
			case signOfIf path
			 of (inTys, SOME resTys) => let
			      val nResults = List.length resTys
			      val (prefix, stk) = splitStk (stk, List.length inTys)
handle ex => raise ex
			      val prefix = List.rev prefix
			      val e1 = cvtBlock (prefix, Ty.thenPath path, b1, nResults)
			      val e2 = cvtBlock (prefix, Ty.elsePath path, b2, nResults)
			      val lhs = List.map (fn ty => newVar(ty, VE.VB_UNKNOWN)) resTys
			      in
				VE.IF(lhs, use cond, e1, e2,
				  cvtOne(List.revAppend(lhs, stk), Ty.endifPath path, rest))
			      end
			  | (inTys, NONE) => let (* neither branch returns *)
			      val (prefix, stk) = splitStk (stk, List.length inTys)
handle ex => raise ex
			      val prefix = List.rev prefix
			      val e1 = cvtBlock (prefix, Ty.thenPath path, b1, 0)
			      val e2 = cvtBlock (prefix, Ty.elsePath path, b2, 0)
			      in
				VE.IF([], use cond, e1, e2, VE.EXIT)
			      end
			(* end case *)
		      end
                  | cvtOne (stk, path, insn::rest) =
		      cvtOp (stk, insn, fn stk => cvtOne(stk, path, rest))
                in
                  cvtOne (stk, path, code)
                end
          val (paramTys, SOME resultTys) = typeOfLab lab
	(* the parameters in reverse order *)
          val (_, params) = List.foldl
		(fn (ty, (i, params)) => (i+1, newVar(ty, VE.VB_PARAM i)::params))
		  (0, []) paramTys
          in
            VE.FUNC(lab, List.rev params, resultTys,
	      cvtBlock (params, Ty.rootPath lab, body, List.length resultTys))
          end

    fun convert prog = let
        (* compute the signs of the program's functions *)
          val typeOfLab = Ty.analyze (VCodeCG.mkCallGraph prog)
        (* convert the program's functions *)
          val prog = List.map (cvtFunc typeOfLab) prog
          in
            prog
          end

  end
