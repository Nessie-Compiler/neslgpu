(* check-super.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure CheckSuper =
  struct

    structure S = SuperOp

    local
      structure Ord =
        struct
          type ord_key = S.var
          fun compare (S.V a, S.V b) = Word.compare(a, b)
        end
    in
    structure VS = RedBlackSetFn (Ord)
    end

  (* arity of pure operations *)
    fun arityOfPure opcode = (case opcode
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
            | V.MAKE_SEGDES => (1, 1)
            | V.LENGTHS => (1, 1)
            | V.CONST _ => (0, 1)
          (* end case *))

    fun arityOfIO opcode = (case opcode
           of V.READ _ => (0, 3)
            | V.WRITE _ => (1, 2)
            | V.FOPEN => (2, 3)
            | V.FCLOSE => (1, 2)
            | V.FWRITE _ => (2, 2)
            | V.FREAD _ => (1, 3)
            | V.FREAD_CHAR => (3, 4)
            | V.EXIT => (0, 0)
            | V.START_TIMER => (0, 0)
            | V.STOP_TIMER => (0, 1)
            | V.SRAND => (1, 1)
          (* end case *))

    fun checkBlock (env, stm, nResults) = let
          fun chkStm (env, S.STM(_, t)) = (case t
                 of S.LET(x, opcode, args, s) =>
                  | S.FUSED(x, sop, args, s) =>
                  | S.LETPACK(x, y, _, args, s) =>
                  | S.CALL(lhs, f, args, s) =>
                  | S.STMT(lhs, opcode, args, s) =>
                  | S.IF(lhs, cond, s1, s2, s3) =>
                  | S.RET xs => if varsDefined(env, xs) andalso List.length xs = nResults
                      then ()
                      else (* error *)
                  | S.EXIT => ()
                (* end case *))
          in
            chkStm (env, stm)
          end

  end
