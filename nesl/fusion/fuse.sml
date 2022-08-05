(* fuse.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Fusion for VExp format.
 *)

structure Fuse : sig

  (* controls *)
    val fuseFlg : bool ref

    val transform : VExp.program -> VExp.program

  end = struct

    structure VE = VExp
    structure VSet = VExp.VSet
    structure ATbl = AtomTable

  (* controls *)
    val fuseFlg = ref true              (* fuse element-wise operations *)

  (* variable tweaking *)
    fun useCnt (VE.V{useCnt, ...}) = !useCnt
    fun decCnt (VE.V{useCnt, ...}) = useCnt := !useCnt - 1
    fun setBinding (VE.V{binding, ...}, b) = binding := VE.VB_LET b

    fun isElementWise pure = (case pure
           of VE.ADD _ => true
            | VE.SUB _ => true
            | VE.MUL _ => true
            | VE.DIV _ => true
            | VE.MOD => true
            | VE.LT _ => true
            | VE.LTE _ => true
            | VE.GT _ => true
            | VE.GTE _ => true
            | VE.EQ _ => true
            | VE.NEQ _ => true
            | VE.LSHIFT => true
            | VE.RSHIFT => true
            | VE.NOT _ => true
            | VE.AND _ => true
            | VE.OR _ => true
            | VE.XOR _ => true
            | VE.SELECT _ => true
            | VE.RAND => false
            | VE.FLOOR => true
            | VE.CEIL => true
            | VE.TRUNC => true
            | VE.ROUND => true
            | VE.I_TO_F => true
            | VE.I_TO_B => true
            | VE.B_TO_I => true
            | VE.LOG => true
            | VE.SQRT => true
            | VE.EXP => true
            | VE.SIN => true
            | VE.COS => true
            | VE.TAN => true
            | VE.ASIN => true
            | VE.ACOS => true
            | VE.ATAN => true
            | VE.SINH => true
            | VE.COSH => true
            | VE.TANH => true
            | _ => false
          (* end case *))

    datatype rhs = EXP of VE.pure * VE.exp list | OTHER

    fun contract (avail, opcode, args) = let
          fun rhsOf (x as VE.V{binding, ...}) =
                if VSet.member(avail, x)
                  then (case !binding
                     of VE.VB_LET(VE.PURE arg) => EXP arg
                      | _ => OTHER
                    (* end case *))
                  else OTHER
          fun isFlatDist (VE.DIST ty, [VE.PURE(VE.CONST _, _), _]) = true
            | isFlatDist _ = false
          in
            case opcode
             of VE.DIST ty => let
                  val [VE.VAR x, VE.VAR segdes] = args
                  in
                    case rhsOf x
                     of EXP(opcode as VE.CONST(ty, [value]), _) => let
(***** causes issues with the interpreter *****
                          val segdes = (case rhsOf segdes
                                 of EXP(VE.MAKE_SEGDES, [VE.VAR d]) => (case rhsOf d
                                       of EXP(con as VE.CONST(_, [_]), _) => (
                                            decCnt segdes;
                                            VE.PURE(VE.MAKE_SEGDES, [VE.PURE(con, [])]))
                                        | _ => VE.VAR segdes
                                      (* end case *))
                                  | _ => VE.VAR segdes
                                (* end case *))
*)
                          val segdes = VE.VAR segdes
                          in
                          (* flat DIST *)
                            decCnt x;
                            SOME(VE.PURE(VE.DIST ty, [VE.PURE(opcode, []), segdes]))
                          end
                      | _ => NONE
                    (* end case *)
                  end
(***** the following fusion causes issues for the interpreter *****
	      | VE.MAKE_SEGDES => let
                  val [VE.VAR x] = args
                  in
                    case rhsOf x
		     of EXP(opcode as VE.CONST(ty, [value]), _) => (
			  decCnt x;
			  SOME(VE.PURE(VE.MAKE_SEGDES, [VE.PURE(opcode, [])])))
		      | _ => NONE
		    (* end case *)
		  end
*)
              | _ => if isElementWise opcode
                  then let
                    fun substArgs ([], false, _) = NONE
                      | substArgs ([], true, args) =
                          SOME(VE.PURE(opcode, List.rev args))
                      | substArgs ((arg as VE.VAR x)::rest, expanded, args) = (
                          case rhsOf x
                           of EXP(opcode', args') => let
                                fun subst () = (
                                      decCnt x;
                                      substArgs (rest, true, VE.PURE(opcode', args')::args))
                                fun noSubst () =
                                      substArgs (rest, expanded, arg::args)
                                in
                                  if isElementWise opcode'
                                  orelse isFlatDist (opcode', args')
                                    then subst()
                                    else noSubst ()
                                end
                            | _ => substArgs (rest, expanded, arg::args)
                          (* end case *))
                      | substArgs (arg::rest, expanded, args) =
                          substArgs (rest, expanded, arg::args)
                    in
                      substArgs (args, false, [])
                    end
                  else NONE
            (* end case *)
          end

  (* fuse expressions *)
    fun fuse (VE.FUNC(lab, params, resultTys, body)) = let
          fun substStm (env, stm) = (case stm
                 of VE.LET(x, e as VE.PURE(opcode, args), stm) => let
                      val e = (case contract (env, opcode, args)
                             of SOME e' => (setBinding(x, e'); e')
                              | NONE => e
                            (* end case *))
                      val stm = substStm(VSet.add(env, x), stm)
                      in
                        if useCnt x = 0
                          then stm
                          else VE.LET(x, e, stm)
                      end
                  | VE.LET(x, VE.VAR y, stm) =>
                      VE.LET(x, VE.VAR y, substStm(env, stm))
                  | VE.LETPACK(x, y, ty, args, stm) =>
                      VE.LETPACK(x, y, ty, args, substStm(env, stm))
                  | VE.CALL(xs, f, args, stm) =>
                      VE.CALL(xs, f, args, substStm(env, stm))
                (* we don't want to fuse code across timer operations, since it changes
                 * performance measurements.
                 *)
                  | VE.STMT(_, VE.START_TIMER, _, stm) =>
                      VE.STMT([], VE.START_TIMER, [], substStm(VSet.empty, stm))
                  | VE.STMT(xs, VE.STOP_TIMER, _, stm) =>
                      VE.STMT(xs, VE.STOP_TIMER, [], substStm(VSet.empty, stm))
                  | VE.STMT(xs, io, args, stm) =>
                      VE.STMT(xs, io, args, substStm(env, stm))
                  | VE.IF(xs, cond, s1, s2, s3) =>
                      VE.IF(xs, cond, substStm(env, s1), substStm(env, s2), substStm(env, s3))
                  | VE.RET es => VE.RET es
                  | VE.EXIT => stm
                (* end case *))
          in
            VE.FUNC(lab, params, resultTys, substStm(VSet.empty, body))
          end

    fun transform prog = if (!fuseFlg)
	  then List.map fuse prog
	  else prog

  end
