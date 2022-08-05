(* vcode.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * This is code for converting the sequences of VCODE operations found in ":primitive"
 * defops to a nested expression form.  It removes the stack operations (e.g., COPY and
 * POP) by reordering arguments.
 *)

(* example:

(defop (extzero a)  ! ((int <- int))
  (:primitive (CONST INT 0) (COPY 1 1) (LENGTH INT) (MAKE_SEGDES)
              (EXTRACT INT)))

	["CONST INT 0", "COPY 1 1", "LENGTH INT", "MAKE_SEGDES", "EXTRACT INT"] (a)

    ====>

	__prim "EXTRACT INT" (a, 0, __prim "MAKE_SEGDES" (__prim "LENGTH" (a)))

  NOTE: args on stack from right to left (i.e., right-most arg is top)
*)

(*
val t = translate (
	  ["CONST INT 0", "COPY 1 1", "LENGTH INT", "MAKE_SEGDES", "EXTRACT INT"],
	  ["a"])
*)

structure VCode : sig

    datatype exp
      = Var of string
      | Const of string 
      | Tuple of (int * exp)		(* represents a multi-result function *)
      | PrimApply of string * exp list

    val translate : string list * string list -> exp

  end = struct

    datatype exp
      = Var of string
      | Const of string 
      | Tuple of (int * exp)		(* represents a multi-result function *)
      | PrimApply of string * exp list

    fun toString (Var x) = x
      | toString (Const x) = x
      | toString (Tuple(i, e)) = concat["#", Int.toString i, "(", toString e, ")"]
      | toString (PrimApply(s, args)) = "PrimApply"

    fun toInt n = valOf(Int.fromString n)

    fun splitStk (stk, n) = let
          fun lp (0, prefix, rest) = (prefix, rest)
            | lp (i, prefix, x::rest) = lp (i-1, x::prefix, rest)
	    | lp _ = raise Fail "splitStk"
          in
            lp (n, [], stk)
          end

    fun translate ([prim], args) = (* the trivial comon case *)
	  PrimApply(prim, List.map Var args)
      | translate (prims, args) = let
	  fun eval ([], [e]) = e
	    | eval ([], _) = raise Fail "non-singleton evaluation stack"
	    | eval (opcode::r, stk) = let
		fun letOp1 (opcode, x1::stk) =
		      eval(r, PrimApply(opcode, [x1]) :: stk)
		fun letOp2 (opcode, x2::x1::stk) =
		      eval(r, PrimApply(opcode, [x1, x2]) :: stk)
                fun letOp3 (opcode, x3::x2::x1::stk) =
		      eval(r, PrimApply(opcode, [x1, x2, x3]) :: stk)
                fun letOp4 (opcode, x4::x3::x2::x1::stk) =
		      eval(r, PrimApply(opcode, [x1, x2, x3, x4]) :: stk)
                fun letOp5 (opcode, x5::x4::x3::x2::x1::stk) =
		      eval(r, PrimApply(opcode, [x1, x2, x3, x4, x5]) :: stk)
                fun letOp6 (opcode, x6::x5::x4::x3::x2::x1::stk) =
		      eval(r, PrimApply(opcode, [x1, x2, x3, x4, x5, x6]) :: stk)
                fun letStmt (opcode, nArgs, nResults, stk) = let
                      val (args, stk) = splitStk (stk, nArgs)
		      val appExp = PrimApply(opcode, args)
                      in
			if nResults <> 1
			  then eval (r, Tuple(nResults, appExp)::stk)
			  else eval (r, appExp::stk)
                      end
		in
(*
print(concat["eval (\"", opcode, "\", [", String.concatWith ", " (List.map toString stk), "])\n"]);
*)
		  case String.tokens Char.isSpace opcode
		   of ["ADD", _] => letOp2 (opcode, stk)
		    | ["SUB", _] => letOp2 (opcode, stk)
		    | ["MUL", _] => letOp2 (opcode, stk)
		    | ["DIV", _] => letOp2 (opcode, stk)
		    | ["MOD"] => letOp2 (opcode, stk)
		    | ["LT", _] => letOp2 (opcode, stk)
		    | ["LTE", _] => letOp2 (opcode, stk)
		    | ["GT", _] => letOp2 (opcode, stk)
		    | ["GTE", _] => letOp2 (opcode, stk)
		    | ["EQ", _] => letOp2 (opcode, stk)
		    | ["NEQ", _] => letOp2 (opcode, stk)
		    | ["LSHIFT"] => letOp2 (opcode, stk)
		    | ["RSHIFT"] => letOp2 (opcode, stk)
		    | ["NOT", _] => letOp1 (opcode, stk)
		    | ["AND", _] => letOp2 (opcode, stk)
		    | ["OR", _] => letOp2 (opcode, stk)
		    | ["XOR", _] => letOp2 (opcode, stk)
		    | ["SELECT", _] => letOp3 (opcode, stk)
		    | ["RAND"] => letOp1 (opcode, stk)
		    | ["FLOOR"] => letOp1 (opcode, stk)
		    | ["CEIL"] => letOp1 (opcode, stk)
		    | ["TRUNC"] => letOp1 (opcode, stk)
		    | ["ROUND"] => letOp1 (opcode, stk)
		    | ["I_TO_F"] => letOp1 (opcode, stk)
		    | ["I_TO_B"] => letOp1 (opcode, stk)
		    | ["B_TO_I"] => letOp1 (opcode, stk)
		    | ["LOG"] => letOp1 (opcode, stk)
		    | ["SQRT"] => letOp1 (opcode, stk)
		    | ["EXP"] => letOp1 (opcode, stk)
		    | ["SIN"] => letOp1 (opcode, stk)
		    | ["COS"] => letOp1 (opcode, stk)
		    | ["TAN"] => letOp1 (opcode, stk)
		    | ["ASIN"] => letOp1 (opcode, stk)
		    | ["ACOS"] => letOp1 (opcode, stk)
		    | ["ATAN"] => letOp1 (opcode, stk)
		    | ["SINH"] => letOp1 (opcode, stk)
		    | ["COSH"] => letOp1 (opcode, stk)
		    | ["TANH"] => letOp1 (opcode, stk)
		  (* Vector instructions *)
		    | ["ADD_SCAN", _] => letOp2 (opcode, stk)
		    | ["MUL_SCAN", _] => letOp2 (opcode, stk)
		    | ["MAX_SCAN", _] => letOp2 (opcode, stk)
		    | ["MIN_SCAN", _] => letOp2 (opcode, stk)
		    | ["AND_SCAN", _] => letOp2 (opcode, stk)
		    | ["OR_SCAN", _] => letOp2 (opcode, stk)
		    | ["XOR_SCAN", _] => letOp2 (opcode, stk)
		    | ["ADD_REDUCE", _] => letOp2 (opcode, stk)
		    | ["MUL_REDUCE", _] => letOp2 (opcode, stk)
		    | ["MAX_REDUCE", _] => letOp2 (opcode, stk)
		    | ["MIN_REDUCE", _] => letOp2 (opcode, stk)
		    | ["AND_REDUCE", _] => letOp2 (opcode, stk)
		    | ["OR_REDUCE", _] => letOp2 (opcode, stk)
		    | ["XOR_REDUCE", _] => letOp2 (opcode, stk)
		    | ["PERMUTE", _] => letOp3 (opcode, stk)
		    | ["DPERMUTE", _] => letOp5 (opcode, stk)
		    | ["FPERMUTE", _] => letOp5 (opcode, stk)
		    | ["BPERMUTE", _] => letOp4 (opcode, stk)
		    | ["BFPERMUTE", _] => letOp5 (opcode, stk)
		    | ["DFPERMUTE", _] => letOp6 (opcode, stk)
		    | ["EXTRACT", _] => letOp3 (opcode, stk)
		    | ["REPLACE", _] => letOp4 (opcode, stk)
		    | ["PACK", _] => letOp2 (opcode, stk)  (* this is pre-flattening API *)
		    | ["RANK_UP", _] => letOp2 (opcode, stk)
		    | ["RANK_DOWN", _] => letOp2 (opcode, stk)
		    | ["DIST", _] => letOp2 (opcode, stk)
		    | ["INDEX"] => letOp3 (opcode, stk)
		    | ["LENGTH", _] => letOp1 (opcode, stk)
		  (* Segment descriptor instructions *)
		    | ["MAKE_SEGDES"] => letOp1 (opcode, stk)
		    | ["LENGTHS"] => letOp1 (opcode, stk)
		    | ["COPY", i, j] => let
(* FIXME: may need to handle PAIRS on the stack *)
                        val copied = List.take(List.drop(stk, toInt j), toInt i)
                        in
                          eval (r, copied @ stk)
                        end
		    | ["POP", i, j] => let
                        val (prefix, rest) = splitStk (stk, toInt j)
                        in
                          eval (r, List.revAppend (prefix, List.drop(rest, toInt i)))
                        end
                    | ["CPOP", i, j] => let
                        val (prefix, rest) = splitStk (stk, toInt j)
                        val (moved, rest) = splitStk (rest, toInt i)
                        in
                          eval (r, List.revAppend (moved, List.revAppend(prefix, rest)))
                        end
		    | ["CONST", "INT", n] => eval (r, Const n :: stk)
(* For the IO operations, many return multiple results, which end up being represented as pairs *)
(*****
                    | ["READ", _] => letStmt(VE.READ ty, 0, 3, stk)
                    | ["WRITE", _] => letStmt(VE.WRITE ty, 1, 2, stk)
                    | ["FOPEN"] => letStmt(VE.FOPEN, 2, 3, stk)
                    | ["FCLOSE"] => letStmt(VE.FCLOSE, 1, 2, stk)
                    | ["FWRITE", _] => letStmt(VE.FWRITE ty, 1, 2, stk)
                    | ["FREAD", _] => letStmt(VE.FREAD ty, 1, 3, stk)
                    | ["FREAD_CHAR"] => letStmt(VE.FREAD_CHAR, 3, 4, stk)
                  (* undocumented instuctions *)
                    | ["EXIT"] => raise Fail "FIXME: EXIT" (* FIXME *)
                    | ["START_TIMER"] => letStmt(VE.START_TIMER, 0, [], stk)
                    | ["STOP_TIMER"] => letStmt(VE.STOP_TIMER, 0, [VE.FLOAT], stk)
                    | ["SRAND"] => letStmt(VE.SRAND, 1, [VE.BOOL], stk)
*****)
		    | _ => raise Fail(concat["unsupported opcode \"", opcode, "\""])
		  (* end case *)
		end
	  in
	  (* NOTE: args on stack from right to left (i.e., right-most arg is top) *)
	    eval (prims, List.rev (List.map Var args))
	  end (* translate *)

  end
