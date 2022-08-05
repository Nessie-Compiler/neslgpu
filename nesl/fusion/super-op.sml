(* super-op.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Convert a fused program to super-operator format.
 *)

structure SuperOp =
  struct

    structure VE = VExp
    structure VMap = VE.VMap
    structure V = Vector

    datatype ty = datatype VCode.ty

  (* body of a super operator *)
    datatype exp
      = ARG of word                     (* argument positions numbered from left to right *)
      | EXP of VE.pure * exp list       (* operator application *)

    local
      fun hashTy VE.BOOL = 0w1019
        | hashTy VE.INT = 0w1087
        | hashTy VE.FLOAT = 0w1153
        | hashTy VE.CHAR = 0w1229
        | hashTy VE.SEGDES = 0w1297

      fun hashPure opcode = (case opcode
             of VE.ADD ty => hashTy ty + 0w37
              | VE.SUB ty => hashTy ty + 0w79
              | VE.MUL ty => hashTy ty + 0w131
              | VE.DIV ty => hashTy ty + 0w181
              | VE.MOD => 0w239
              | VE.LT ty => hashTy ty + 0w293
              | VE.LTE ty => hashTy ty + 0w359
              | VE.GT ty => hashTy ty + 0w421
              | VE.GTE ty => hashTy ty + 0w479
              | VE.EQ ty => hashTy ty + 0w557
              | VE.NEQ ty => hashTy ty + 0w613
              | VE.LSHIFT => 0w673
              | VE.RSHIFT => 0w743
              | VE.NOT ty => hashTy ty + 0w821
              | VE.AND ty => hashTy ty + 0w881
              | VE.OR ty => hashTy ty + 0w953
              | VE.XOR ty => hashTy ty + 0w1021
              | VE.SELECT ty => hashTy ty + 0w1091
              | VE.RAND => 0w1163
              | VE.FLOOR => 0w1231
              | VE.CEIL => 0w1301
              | VE.TRUNC => 0w1399
              | VE.ROUND => 0w1459
              | VE.I_TO_F => 0w1531
              | VE.I_TO_B => 0w1601
              | VE.B_TO_I => 0w1667
              | VE.LOG => 0w1747
              | VE.SQRT => 0w1831
              | VE.EXP => 0w1907
              | VE.SIN => 0w1997
              | VE.COS => 0w2069
              | VE.TAN => 0w2137
              | VE.ASIN => 0w2237
              | VE.ACOS => 0w2297
              | VE.ATAN => 0w2377
              | VE.SINH => 0w2441
              | VE.COSH => 0w2543
              | VE.TANH => 0w2633
              | VE.ADD_SCAN ty => hashTy ty + 0w2693
              | VE.MUL_SCAN ty => hashTy ty + 0w2753
              | VE.MAX_SCAN ty => hashTy ty + 0w2837
              | VE.MIN_SCAN ty => hashTy ty + 0w2917
              | VE.AND_SCAN ty => hashTy ty + 0w3011
              | VE.OR_SCAN ty => hashTy ty + 0w3089
              | VE.XOR_SCAN ty => hashTy ty + 0w3191
              | VE.ADD_REDUCE ty => hashTy ty + 0w3271
              | VE.MUL_REDUCE ty => hashTy ty + 0w3347
              | VE.MAX_REDUCE ty => hashTy ty + 0w3449
              | VE.MIN_REDUCE ty => hashTy ty + 0w3527
              | VE.AND_REDUCE ty => hashTy ty + 0w3583
              | VE.OR_REDUCE ty => hashTy ty + 0w3671
              | VE.XOR_REDUCE ty => hashTy ty + 0w3739
              | VE.PERMUTE ty => hashTy ty + 0w3833
              | VE.DPERMUTE ty => hashTy ty + 0w3917
              | VE.FPERMUTE ty => hashTy ty + 0w4003
              | VE.BPERMUTE ty => hashTy ty + 0w4079
              | VE.BFPERMUTE ty => hashTy ty + 0w4157
              | VE.DFPERMUTE ty => hashTy ty + 0w4243
              | VE.EXTRACT ty => hashTy ty + 0w4337
              | VE.REPLACE ty => hashTy ty + 0w4423
              | VE.RANK_UP ty => hashTy ty + 0w4513
              | VE.RANK_DOWN ty => hashTy ty + 0w4597
              | VE.DIST ty => hashTy ty + 0w4673
              | VE.INDEX => 0w4783
              | VE.LENGTH ty => hashTy ty + 0w4871
              | VE.MAKE_SEGDES => 0w4951
              | VE.LENGTHS => 0w5011
              | VE.CONST(ty, vs) => List.foldl
                  (fn (v, h) => h + HashString.hashString v)
                    (hashTy ty + 0w5101) vs
            (* end case *))

      fun samePure (p1 : VE.pure, p2) = (p1 = p2)

      fun hashExp (ARG w) = w
        | hashExp (EXP(opcode, args)) =
            List.foldl (fn (e, h) => h + hashExp e) (hashPure opcode) args

      fun sameExp (ARG w1, ARG w2) = (w1 = w2)
        | sameExp (EXP(opcode1, args1), EXP(opcode2, args2)) =
            (opcode1 = opcode2) andalso ListPair.allEq sameExp (args1, args2)
        |  sameExp _ = false
    in
    structure OpTbl = HashTableFn (
      struct
        type hash_key = exp
        val hashVal = hashExp
        val sameKey = sameExp
      end)
    end (* local *)

    datatype super_op = SOp of {
        id : int,
        useCnt : int ref,
        paramTys : ty list,
	resTy : ty,
        def : exp
      }

    fun incUseCnt (SOp{useCnt, ...}) = useCnt := !useCnt + 1

    datatype stm = STM of (PropList.holder * term)

    and term
      = LET of var * VE.pure * var list * stm                   (* unfused opcode *)
      | FUSED of var * super_op * var vector * stm
      | LETPACK of (var * var * VE.ty * var list * stm)         (* PACK ty *)
      | CALL of var list * VE.label * var list * stm
      | STMT of var list * VE.io * var list * stm
      | IF of var list * var * stm * stm * stm
      | RET of var list
      | EXIT

    and var = V of word

    datatype func = FUNC of VE.label * var list * ty list * stm

    datatype program = PROG of {
        sops : super_op list,
        fns : func list
      }

    local
      val cnt = ref 0w0
    in
    fun newVar () = let
          val id = !cnt
          in
            cnt := id + 0w1;
            V id
          end
    fun reset () = (cnt := 0w0)
    end

    fun varToString (V id) = "x" ^ StringCvt.padLeft #"0" 3 (Word.fmt StringCvt.DEC id)

    fun sameVar (V id1, V id2) = (id1 = id2)

    local
    fun mkSTM t = STM(PropList.newHolder(), t)
    in
      val mkLET = mkSTM o LET
      val mkFUSED = mkSTM o FUSED
      val mkLETPACK = mkSTM o LETPACK
      val mkCALL = mkSTM o CALL
      val mkSTMT = mkSTM o STMT
      val mkIF = mkSTM o IF
      val mkRET = mkSTM o RET
      fun mkEXIT () = mkSTM EXIT
    end

    fun insert (env, x, x') = VMap.insert(env, x, x')

    fun convert prog = let
          val nOps = ref 0
          val opTbl = OpTbl.mkTable(1024, Fail "opTbl")
        (* map a VExp to a super op *)
          fun superOp (e as VE.PURE(opcode, _)) = let
		val resTy = VE.resultTypeOfPure opcode
	      (* Convert a VExp.exp to an exp, an argument vector, and a list of argument types *)
                val (e', args, tys) = let
		      fun expToOp (VE.VAR x, i, xs) = (ARG i, i+0w1, x::xs)
			| expToOp (VE.PURE(opcode, args), i, xs) = let
			    fun f (e, (args, i, xs)) = let
				  val (e', i', xs') = expToOp(e, i, xs)
				  in
				    (e'::args, i', xs')
				  end
			    val (args, i, xs) = List.foldl f ([], i, xs) args
			    in
			      (EXP(opcode, List.rev args), i, xs)
			    end
		      val (e, _, xs) = expToOp (e, 0w0, [])
		      val args = List.rev xs
		      val tys = List.map (fn (VE.V{ty, ...}) => ty) args
		      in
			(e, Vector.fromList(List.rev xs), tys)
		      end
                in
                  case OpTbl.find opTbl e'
                   of SOME sop => (incUseCnt sop; (sop, args))
                    | NONE => let
                        val sop = SOp{
				id = !nOps,
                                useCnt = ref 1,
				paramTys = tys,
				resTy = VE.resultTypeOfPure opcode,
				def = e'
			      }
                        in
                          OpTbl.insert opTbl (e', sop);
                          nOps := !nOps + 1;
                          (sop, args)
                        end
                  (* end case *)
                end
        (* *)
          fun renameVar env x = (case VMap.find (env, x)
                 of SOME x' => x'
                  | NONE => raise Fail(concat["rename(_, ", VE.varToString false x, ")"])
                (* end case *))
        (* *)
          fun newVars (env, xs) = let
                fun f ([], env, xs') = (env, List.rev xs')
                  | f (x::xs, env, xs') = let
                      val x' = newVar()
                      in
                        f(xs, insert(env, x, x'), x'::xs')
                      end
                in
                  f (xs, env, [])
                end
        (* *)
          fun cvtExp (env, VE.VAR x, k) = k (env, renameVar env x)
            | cvtExp (env, e as VE.PURE(opcode, args), k) = let
                val res = newVar()
                fun cvtVar env (VE.VAR x) = renameVar env x
                  | cvtVar env _ = raise Fail "cvtVar: not a variable"
                in
                  if List.exists (fn (VE.VAR _) => false | _ => true) args
                    then let
                      val (sop, args) = superOp e
                      in
                        mkFUSED(res, sop, V.map (renameVar env) args, k(env, res))
                      end
                    else mkLET(res, opcode, List.map (cvtVar env) args, k(env, res))
                end
        (* *)
          fun cvtExps (env, exps, k) = let
                fun lp (env, [], xs) = k(env, List.rev xs)
                  | lp (env, e::es, xs) = cvtExp (env, e, fn (env, x) => lp (env, es, x::xs))
                in
                  lp (env, exps, [])
                end
        (* *)
          fun cvtStm (env, stm) = (case stm
                 of VE.LET(x, e, stm) =>
                      cvtExp (env, e, fn (env, x') => cvtStm (insert(env, x, x'), stm))
                  | VE.LETPACK(x, y, ty, args, stm) => cvtExps (env, args, fn (env, xs) => let
                      val x' = newVar()
                      val y' = newVar()
                      val env = insert (insert (env, x, x'), y, y')
                      in
                        mkLETPACK(x', y', ty, xs, cvtStm(env, stm))
                      end)
                  | VE.CALL(lhs, f, args, stm) => cvtExps (env, args, fn (env, xs) => let
                      val (env, lhs') = newVars(env, lhs)
                      in
                        mkCALL(lhs', f, xs, cvtStm(env, stm))
                      end)
                  | VE.STMT(lhs, io, args, stm) => cvtExps (env, args, fn (env, xs) => let
                      val (env, lhs') = newVars(env, lhs)
                      in
                        mkSTMT(lhs', io, xs, cvtStm(env, stm))
                      end)
                  | VE.IF(lhs, cond, s1, s2, s3) => cvtExp (env, cond, fn (env, x) => let
                      val (env, lhs') = newVars(env, lhs)
                      in
                        mkIF(lhs', x, cvtStm(env, s1), cvtStm(env, s2), cvtStm(env, s3))
                      end)
                  | VE.RET args => cvtExps (env, args, fn (_, xs) => mkRET xs)
                  | VE.EXIT => mkEXIT()
                (* end case *))
          fun cvtFunc (VE.FUNC(f, params, resultTys, stm)) = let
                val (env, params') = newVars(VMap.empty, params)
                in
                  FUNC(f, params', resultTys, cvtStm (env, stm))
                end
          val fns = List.map cvtFunc prog
          val sops = ListMergeSort.sort
                (fn (SOp{id=a, ...}, SOp{id=b, ...}) => Int.>(a, b))
                  (OpTbl.listItems opTbl)
          in
            PROG{sops = sops, fns = fns}
          end

  end
