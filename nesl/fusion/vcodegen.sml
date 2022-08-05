(* vcodegen.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure VCodeGen : sig

    val gen : SuperOp.program -> VCode.program

  end = struct

    val debug = false	(* set to true to turn on debug output *)
    fun prl l = if debug then print(concat l) else ()

    structure S = SuperOp
    structure VC = VCode
    structure VE = VExp

    local
      structure Ord =
        struct
          type ord_key = S.var
          fun compare (S.V a, S.V b) = Word.compare(a, b)
        end
    in
    structure VS = RedBlackSetFn (Ord)
    structure VM = RedBlackMapFn (Ord)
    end

    fun toList v = List.tabulate(Vector.length v, fn i => Vector.sub(v, i))

  (* property to record the variables killed at a program point; i.e., the variables
   * that are live in, but not live out.  For RET nodes, we record the liveIn variables,
   * so that we can clean up the stack.
   *)
    val {getFn=getKilled, setFn=setKilled, ...} = PropList.newProp (fn h => h, fn _ => VS.empty)

    fun vsToString s = String.concat[
            "{", String.concatWith "," (List.map S.varToString (VS.listItems s)), "}"
          ]

  (* compute liveness for a function, attaching to info to the statement's property list *)
    fun computeLiveness (S.FUNC(_, _, _, body)) = let
          fun xfer (liveOut, def, use) = VS.addList(VS.difference(liveOut, VS.fromList def), use)
          fun killed (hold, liveIn, liveOut) = (
                setKilled(hold, VS.difference(liveIn, liveOut));
                liveIn)
          fun doBlock (stm, liveOut) = let
                fun doStm (S.STM(hold, t)) = (case t
                       of S.LET(x, _, args, stm) => let
                            val liveOut = doStm stm
                            val liveIn = xfer (liveOut, [x], args)
                            in
                              killed (hold, liveIn, liveOut)
                            end
                        | S.FUSED(x, _, args, stm) => let
                            val liveOut = doStm stm
                            val liveIn = xfer (liveOut, [x], toList args)
                            in
                              killed (hold, liveIn, liveOut)
                            end
                        | S.LETPACK(x, y, _, args, stm) => let
                            val liveOut = doStm stm
                            val liveIn = xfer (liveOut, [x, y], args)
                            in
                              killed (hold, liveIn, liveOut)
                            end
                        | S.CALL(lhs, _, args, stm) => let
                            val liveOut = doStm stm
                            val liveIn = xfer (liveOut, lhs, args)
                            in
                              killed (hold, liveIn, liveOut)
                            end
                        | S.STMT(lhs, _, args, stm) => let
                            val liveOut = doStm stm
                            val liveIn = xfer (liveOut, lhs, args)
                            in
                              killed (hold, liveIn, liveOut)
                            end
                        | S.IF(lhs, cond, s1, s2, s3) => let
                          (* live variables at the join point *)
                            val live3 = xfer (doBlock (s3, liveOut), lhs, [])
                          (* live variables for arms of branches *)
                            val live1 = doBlock (s1, live3)
                            val live2 = doBlock (s2, live3)
                          (* live out at the test is union of the branches *)
                            val liveOut = VS.union(live1, live2)
                          (* live in at the test*)
                            val liveIn = xfer (liveOut, [], [cond])
                            in
                              killed (hold, liveIn, liveOut)
                            end
                        | S.RET xs => (
                          (* for RET nodes, we record the liveOut set so that we can cleanup
                           * the stack before the return.
                           *)
                            setKilled (hold, liveOut);
                            VS.addList (liveOut, xs))
                        | S.EXIT => killed (hold, VS.empty, VS.empty)
                      (* end case *))
                in
                  doStm stm
                end
          in
            doBlock (body, VS.empty)
          end

  (* the current state of the stack is represented by a list of variables. *)
    type stack_state = S.var list

(*DEBUG*)fun stkToString ss = String.concat(List.foldr (fn (x, l) => " " :: S.varToString x :: l) [" []"] ss)

(*DEBUG *)
structure List =
  struct
    open List
    val drop = fn (l, n) => drop(l, n) handle ex => (
print(String.concat["drop(", stkToString l, ", ", Int.toString n, ")\n"]); raise ex)
  end
(*DEBUG *)

    fun splitStk (stk, n) = let
          fun lp (0, prefix, rest) = (prefix, rest)
            | lp (i, prefix, x::rest) = lp (i-1, x::prefix, rest)
          in
            lp (n, [], stk)
          end

    fun pop (ss : stack_state, i, 0) = List.drop (ss, i)
      | pop (ss, i, j) = let
          val (prefix, rest) = splitStk (ss, j)
          val rest = List.drop (rest, i)
          in
            List.revAppend (prefix, rest)
          end
(*DEBUG*)handle ex => raise ex

    fun copy (ss : stack_state, i, j) = let
          val (_, rest) = splitStk (ss, j)
          val copied = List.take (rest, i)
          in
            copied @ ss
          end

    fun cpop (ss : stack_state, i, j) = let
          val (prefix, rest) = splitStk (ss, j)
          val (moved, rest) = splitStk (rest, i)
          in
            moved @ List.revAppend (prefix, rest)
          end
(*
val cpop = fn (ss, i, j) => let val ss' = cpop(ss, i, j) in
prl [stkToString ss, " == CPOP ", Int.toString i, " ", Int.toString j, " ==> ",
stkToString ss', "\n"]; ss' end
*)

    fun varOffset (ss : stack_state, x) = let
          fun look ([], _) = raise Fail("unable to find " ^ S.varToString x)
            | look (y::ys, i) = if S.sameVar(x, y)
                then i
                else look (ys, i+1)
          in
            look (ss, 0)
          end

  (* generate code to push a list of arguments onto the stack, where the last argument
   * will be the top of the stack.
   *)
    fun pushArgs (ss, args, dead) = let
        (* this version of varOffset skips the first n stack entries, which are assumed to
         * be arguments that have already been pushed on the stack.
         *)
          fun varOffset (ss : stack_state, n, x) = let
                fun look ([], _) = raise Fail("unable to find " ^ S.varToString x)
                  | look (y::ys, i) = if S.sameVar(x, y)
                      then i
                      else look (ys, i+1)
                in
                  look (List.drop(ss, n), n)
                end
        (* pair the variables with liveness information.  Note that if a dead variable appears
         * more than once in the args list, then it is live for its earlier occurences.
         *)
          val args = let
                fun f (x, (dead, xs)) = let
                      val isDead = VS.member(dead, x)
                      in
                        (if isDead then VS.delete(dead, x) else dead, (x, isDead)::xs)
                      end
                in
                  #2 (List.foldr f (dead, []) args)
                end
          fun emit ([], _, ss, code) = (ss, code)
            | emit ((x, isDead)::r, d, ss, code) = let
              (* group adjacent variables that have the same lifetime *)
                fun grp ([], n, prevOffset) = emitOp (isDead, n, prevOffset, [], ss, code)
                  | grp (args as ((y, isDead')::r), n, prevOffset) =
                      if (isDead = isDead') andalso (prevOffset-1 = varOffset(ss, d, y))
                        then grp (r, n+1, prevOffset-1)
                        else emitOp (isDead, n, prevOffset, args, ss, code)
                and emitOp (true, n, 0, args, ss, code) = (* dead args already on top of stack *)
                      emit (args, d + n, ss, code)
                  | emitOp (true, i, j, args, ss, code) =
                      emit (args, d + i, cpop(ss, i, j), VC.CPOP(i, j)::code)
                  | emitOp (false, i, j, args, ss, code) =
                      emit (args, d + i, copy(ss, i, j), VC.COPY(i, j)::code)
                in
                (* note that we get the offset for the variable in the original stack, so that
                 * we don't grab an argument by accident.
                 *)
                  grp (r, 1, varOffset(ss, d, x))
                end
          val (ss, code) = emit (args, 0, ss, [])
          in
            (ss, List.rev code)
          end
(*DEBUG*)handle ex => raise ex

  (* *)
    fun emitPop (ss, i, j) = (pop(ss, i, j), [VC.POP(i, j)])

  (* emit code and adjust the stack to pop dead result variables; the variables xs are
   * on the stack in reverse order.
   *)
    fun popDead (ss, [], _) = (ss, [])
      | popDead (ss, xs, killed) = let
        (* order the variables from top to bottom and pair with liveness info *)
          val xs = List.map (fn x => (x, VS.member(killed, x))) (List.rev xs)
        (* remove variables in groups *)
          fun grp ([], ss, code) = (ss, code)
            | grp ((_, false)::r, ss, code) = grp (r, ss, code)
            | grp ((x, true)::r, ss, code) = let
                val j = varOffset(ss, x)
                fun emitOp ((_, true)::r, n) = emitOp(r, n+1)
                  | emitOp ((_, false)::r, n) =
                      grp (r, pop(ss, n, j), VC.POP(n, j)::code)
                  | emitOp ([], n) = (ss, VC.POP(n, j)::code)
                in
                  emitOp (r, 1)
                end
          val (ss, code) = grp (xs, ss, [])
          in
            (ss, List.rev code)
          end
(*DEBUG*)handle ex => raise ex

  (* emit code to remove dead variables (i.e., those not in live) from the stack *)
    fun removeDead (ss, live) = let
        (* we walk the stack from top to bottom grouping adjacent dead variables, but the
         * generated code pops the variables from bottom to top, which means that the stack
         * offsets do not have to be adjusted.
         *)
          fun walkStk ([], _, liveStk, code) = (List.rev liveStk, code)
            | walkStk (x::r, offset, liveStk, code) = if VS.member(live, x)
                then walkStk (r, offset+1, x::liveStk, code)
                else let
                  fun grp ([], n) = (List.rev liveStk, VC.POP(n, offset)::code)
                    | grp (x::r, n) = if VS.member(live, x)
                        then walkStk (x::r, offset+n, liveStk, VC.POP(n, offset)::code)
                        else grp (r, n+1)
                  in
                    grp (r, 1)
                  end
          in
            walkStk (ss, 0, [], [])
          end
(*DEBUG*)handle ex => raise ex

  (* convert a pure opcode; we assume that the top of the stack has the arguments *)
    fun cvtPure (ss, lhs, opcode) = let
          fun opcode0 (stk, opcode) = (lhs :: stk, opcode)
          fun opcode1 (_::r, opcode) = (lhs :: r, opcode)
          fun opcode2 (_::_::r, opcode) = (lhs :: r, opcode)
          fun opcode3 (_::_::_::r, opcode) = (lhs :: r, opcode)
          fun opcode4 (_::_::_::_::r, opcode) = (lhs :: r, opcode)
          fun opcode5 (_::_::_::_::_::r, opcode) = (lhs :: r, opcode)
          fun opcode6 (_::_::_::_::_::_::r, opcode) = (lhs :: r, opcode)
          in
            case opcode
             of VE.ADD ty => opcode2 (ss, VC.ADD ty)
              | VE.SUB ty => opcode2 (ss, VC.SUB ty)
              | VE.MUL ty => opcode2 (ss, VC.MUL ty)
              | VE.DIV ty => opcode2 (ss, VC.DIV ty)
              | VE.MOD => opcode2 (ss, VC.MOD)
              | VE.LT ty => opcode2 (ss, VC.LT ty)
              | VE.LTE ty => opcode2 (ss, VC.LTE ty)
              | VE.GT ty => opcode2 (ss, VC.GT ty)
              | VE.GTE ty => opcode2 (ss, VC.GTE ty)
              | VE.EQ ty => opcode2 (ss, VC.EQ ty)
              | VE.NEQ ty => opcode2 (ss, VC.NEQ ty)
              | VE.LSHIFT => opcode2 (ss, VC.LSHIFT)
              | VE.RSHIFT => opcode2 (ss, VC.RSHIFT)
              | VE.NOT ty => opcode1 (ss, VC.NOT ty)
              | VE.AND ty => opcode2 (ss, VC.AND ty)
              | VE.OR ty => opcode2 (ss, VC.OR ty)
              | VE.XOR ty => opcode2 (ss, VC.XOR ty)
              | VE.SELECT ty => opcode3 (ss, VC.SELECT ty)
              | VE.RAND => opcode1 (ss, VC.RAND)
              | VE.FLOOR => opcode1 (ss, VC.FLOOR)
              | VE.CEIL => opcode1 (ss, VC.CEIL)
              | VE.TRUNC => opcode1 (ss, VC.TRUNC)
              | VE.ROUND => opcode1 (ss, VC.ROUND)
              | VE.I_TO_F => opcode1 (ss, VC.I_TO_F)
              | VE.I_TO_B => opcode1 (ss, VC.I_TO_B)
              | VE.B_TO_I => opcode1 (ss, VC.B_TO_I)
              | VE.LOG => opcode1 (ss, VC.LOG)
              | VE.SQRT => opcode1 (ss, VC.SQRT)
              | VE.EXP => opcode1 (ss, VC.EXP)
              | VE.SIN => opcode1 (ss, VC.SIN)
              | VE.COS => opcode1 (ss, VC.COS)
              | VE.TAN => opcode1 (ss, VC.TAN)
              | VE.ASIN => opcode1 (ss, VC.ASIN)
              | VE.ACOS => opcode1 (ss, VC.ACOS)
              | VE.ATAN => opcode1 (ss, VC.ATAN)
              | VE.SINH => opcode1 (ss, VC.SINH)
              | VE.COSH => opcode1 (ss, VC.COSH)
              | VE.TANH => opcode1 (ss, VC.TANH)
            (* Vector instructions *)
              | VE.ADD_SCAN ty => opcode2 (ss, VC.ADD_SCAN ty)
              | VE.MUL_SCAN ty => opcode2 (ss, VC.MUL_SCAN ty)
              | VE.MAX_SCAN ty => opcode2 (ss, VC.MAX_SCAN ty)
              | VE.MIN_SCAN ty => opcode2 (ss, VC.MIN_SCAN ty)
              | VE.AND_SCAN ty => opcode2 (ss, VC.AND_SCAN ty)
              | VE.OR_SCAN ty => opcode2 (ss, VC.OR_SCAN ty)
              | VE.XOR_SCAN ty => opcode2 (ss, VC.XOR_SCAN ty)
              | VE.ADD_REDUCE ty => opcode2 (ss, VC.ADD_REDUCE ty)
              | VE.MUL_REDUCE ty => opcode2 (ss, VC.MUL_REDUCE ty)
              | VE.MAX_REDUCE ty => opcode2 (ss, VC.MAX_REDUCE ty)
              | VE.MIN_REDUCE ty => opcode2 (ss, VC.MIN_REDUCE ty)
              | VE.AND_REDUCE ty => opcode2 (ss, VC.AND_REDUCE ty)
              | VE.OR_REDUCE ty => opcode2 (ss, VC.OR_REDUCE ty)
              | VE.XOR_REDUCE ty => opcode2 (ss, VC.XOR_REDUCE ty)
              | VE.PERMUTE ty => opcode3 (ss, VC.PERMUTE ty)
              | VE.DPERMUTE ty => opcode5 (ss, VC.DPERMUTE ty)
              | VE.FPERMUTE ty => opcode5 (ss, VC.FPERMUTE ty)
              | VE.BPERMUTE ty => opcode4 (ss, VC.BPERMUTE ty)
              | VE.BFPERMUTE ty => opcode5 (ss, VC.BFPERMUTE ty)
              | VE.DFPERMUTE ty => opcode6 (ss, VC.DFPERMUTE ty)
              | VE.EXTRACT ty => opcode3 (ss, VC.EXTRACT ty)
              | VE.REPLACE ty => opcode4 (ss, VC.REPLACE ty)
              | VE.RANK_UP ty => opcode2 (ss, VC.RANK_UP ty)
              | VE.RANK_DOWN ty => opcode2 (ss, VC.RANK_DOWN ty)
              | VE.DIST ty => opcode2 (ss, VC.DIST ty)
              | VE.INDEX => opcode3 (ss, VC.INDEX)
              | VE.LENGTH ty => opcode1 (ss, VC.LENGTH ty)
              | VE.MAKE_SEGDES => opcode1 (ss, VC.MAKE_SEGDES)
              | VE.LENGTHS => opcode1 (ss, VC.LENGTHS)
              | VE.CONST(ty, vs) => opcode0 (ss, VC.CONST(ty, vs))
            (* end case *)
          end

    fun cvtIO (ss, lhs, opcode) = let
          fun opcode0 (stk, opcode) = (List.revAppend(lhs, stk), opcode)
          fun opcode1 (_::r, opcode) = (List.revAppend(lhs, r), opcode)
          fun opcode2 (_::_::r, opcode) = (List.revAppend(lhs, r), opcode)
          fun opcode3 (_::_::_::r, opcode) = (List.revAppend(lhs, r), opcode)
          in
            case opcode
             of VE.READ ty => opcode0 (ss, VC.READ ty)
              | VE.WRITE ty => opcode1 (ss, VC.WRITE ty)
              | VE.FOPEN => opcode2 (ss, VC.FOPEN)
              | VE.FCLOSE => opcode1 (ss, VC.FCLOSE)
              | VE.FWRITE ty => opcode2 (ss, VC.FWRITE ty)
              | VE.FREAD ty => opcode1 (ss, VC.FREAD ty)
              | VE.FREAD_CHAR => opcode3 (ss, VC.FREAD_CHAR)
              | VE.START_TIMER => opcode0 (ss, VC.START_TIMER)
              | VE.STOP_TIMER => opcode0 (ss, VC.STOP_TIMER)
              | VE.SRAND => opcode1 (ss, VC.SRAND)
          end

    fun cvtFunc (func as S.FUNC(lab, params, resTy, body)) = let
        (* first compute the liveness info for the function *)
          val _ = computeLiveness func
        (* the initial stack state *)
          val ss = List.rev params
        (* *)
          fun cvtStm (ss, S.STM(hold, stm)) = (case stm
                 of S.LET(x, opcode, args, stm) => let
(**)
val _ = prl [
"let ", S.varToString x, " = (", VExp.pureToString opcode,
if List.null args then "" else " @", String.concat(List.map (fn x => " "^S.varToString x) args),
"), killed = ", vsToString(getKilled hold), ", stk = ", stkToString ss, "\n"]
(**)
                      val (ss, precode) = pushArgs(ss, args, getKilled hold)
                      val (ss, opcode) = cvtPure (ss, x, opcode)
                      val (ss, postcode) = if VS.member(getKilled hold, x)
                            then emitPop(ss, 1, 0)
                            else (ss, [])
                      val (ss, code) = cvtStm (ss, stm)
                      in
                        (ss, precode @ (opcode :: postcode) @ code)
                      end
                  | S.FUSED(x, S.SOp{id, paramTys, ...}, args, stm) => let
(**)
val _ = prl [
"let ", S.varToString x, " = OP", Int.toString id, "(",
String.concatWith "," (List.map S.varToString (toList args)),
"), killed = ", vsToString(getKilled hold), ", stk = ", stkToString ss, "\n"]
(**)
                      val arity = List.length paramTys
                      val (ss, precode) = pushArgs(ss, toList args, getKilled hold)
                      val (ss, opcode) = (x :: pop(ss, arity, 0), VC.FUSED id)
                      val (ss, postcode) = if VS.member(getKilled hold, x)
                            then emitPop(ss, 1, 0)
                            else (ss, [])
                      val (ss, code) = cvtStm (ss, stm)
                      in
                        (ss, precode @ (opcode :: postcode) @ code)
                      end
                  | S.LETPACK(x, y, ty, args, stm) => let
                      val (ss, precode) = pushArgs(ss, args, getKilled hold)
                      val (ss, opcode) = (y :: x :: pop(ss, 3, 0), VC.PACK ty)
                      val (ss, postcode) = (
                            case (VS.member(getKilled hold, y), VS.member(getKilled hold, x))
                             of (false, false) => (ss, [])
                              | (false, true) => emitPop(ss, 1, 1)
                              | (true, false) => emitPop(ss, 1, 0)
                              | (true, true) => emitPop(ss, 2, 0)
                            (* end case *))
                      val (ss, code) = cvtStm (ss, stm)
                      in
                        (ss, precode @ (opcode :: postcode) @ code)
                      end
                  | S.CALL(xs, f, args, stm) => let
(**)
val _ = prl [
"let ... = CALL ", Atom.toString f, "(",
String.concatWith "," (List.map S.varToString args),
"), killed = ", vsToString(getKilled hold), ", stk = ", stkToString ss, "\n"]
(**)
                      val (ss, precode) = pushArgs(ss, args, getKilled hold)
                    (* replace arguments by results in stack state *)
                      val ss = List.revAppend(xs, List.drop(ss, List.length args))
(*DEBUG*)handle ex => raise ex
                      val (ss, postcode) = popDead(ss, xs, getKilled hold)
                      val (ss, code) = cvtStm (ss, stm)
                      in
                        (ss, precode @ (VC.CALL f :: postcode) @ code)
                      end
                  | S.STMT(xs, io, args, stm) => let
                      val (ss, precode) = pushArgs(ss, args, getKilled hold)
                      val (ss, opcode) = cvtIO (ss, xs, io)
                      val (ss, postcode) = popDead (ss, xs, getKilled hold)
                      val (ss, code) = cvtStm (ss, stm)
                      in
                        (ss, precode @ (opcode :: postcode) @ code)
                      end
                  | S.IF(xs, cond, s1, s2, s3) => let
(**)
val _ = prl [
"let ... = if (", S.varToString cond, "), killed = ", vsToString(getKilled hold), ", stk = ", stkToString ss, "\n"]
(**)
                      val (ss, precode) = pushArgs(ss, [cond], getKilled hold)
                      val ss = List.tl ss (* IF consumes the top of the stack *)
                      val (ss1, code1) = cvtStm (ss, s1)
                      val (ss2, code2) = cvtStm (ss, s2)
                      val arity = List.length xs
                    (* replace the top of the stack with the lhs variables *)
                      val ss = (case (ss1, ss2)
                             of ([], _) => List.revAppend(xs, List.drop(ss2, arity))
                              | (_, []) => List.revAppend(xs, List.drop(ss1, arity))
                              | _ => let
                                  val ss1' = List.drop(ss1, arity)
                                  val ss2' = List.drop(ss2, arity)
                                  in
                                  (* sanity check of the remaining stacks *)
                                    if (ListPair.allEq S.sameVar (ss1', ss2'))
                                      then ()
                                      else (
                                        print(concat[
                                            "then/else stack mismatch\n",
                                            "  arity = ", Int.toString arity,
                                            "\n  then stk:", stkToString ss1,
                                            "\n  else stk:", stkToString ss2, "\n"
                                          ]);
                                        raise Fail "stack mismatch");
                                    List.revAppend (xs, ss1')
                                  end
                            (* end case *))
(*DEBUG*)handle ex => raise ex
                      val (ss, code) = cvtStm (ss, s3)
                      in
                        (ss, precode @ (VC.IF(code1, code2) :: code))
                      end
                  | S.RET xs => let
                    (* the killed property is liveOut for RET nodes *)
                      val liveOut = getKilled hold
(**)
val _ = prl [
"ret (", String.concatWith "," (List.map S.varToString xs), "), liveout = ", vsToString liveOut,
", stk = ", stkToString ss, "\n"]
(**)
                    (* compute the liveIn set *)
                      val liveIn = VS.addList(liveOut, xs)
                    (* remove dead variables from the stack *)
                      val (ss, precode) = removeDead (ss, liveIn)
(**)
val _ = prl ["  live stk = ", stkToString ss, "\n"]
(**)
                      val killed = VS.difference(liveIn, liveOut)
(**)
val _ = prl ["  killed = ", vsToString killed, "\n"]
(**)
                      val (ss, code) = pushArgs(ss, xs, killed)
(**)
val _ = prl ["  ret stk =  ", stkToString ss, "\n"]
(**)
                      in
                        (ss, precode @ code)
                      end
                  | S.EXIT => ([], [VC.EXIT])
                (* end case *))
          in
            VC.FUNC(lab, #2 (cvtStm (ss, body)))
          end

    fun gen (S.PROG{fns, ...}) = List.map cvtFunc fns

  end
