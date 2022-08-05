(* gen-cpu.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Code generation for the host program.
 *)

structure GenCPU : sig

    val genProgram : string * CuLambda.program -> CLang.decl list

  end = struct

    structure Cu = CuLambda
    structure GPU = Cu.GPU
    structure TG = Cu.TaskGraph
    structure CL = CLang
    structure L = Liveness
    structure VecOp = VectorOp

  (* code fragments *)
    fun includeStuff substitutions = CL.verbatimDcl [
	    HeaderFrag.text,
	    CheckErrorFrag.text,
	    PickDevFrag.text,
	    CommandsFrag.text,
	    CPUTypesFrag.text
	  ] substitutions

    val floatTy = Util.floatTy
    val intTy = Util.intTy
    val mathFn = Util.mathFn

  (* a property to track the binding of a variable *)
    val {setFn=setBinding, getFn=(getBinding : Cu.var -> CL.exp), ...} =
	  CuVar.newProp (fn v => raise Fail("no binding for "^CuVar.toString v))

    fun mkVar x = CL.mkVar(CuVar.uniqueName x)

    fun mkInt n = CL.mkInt(IntInf.fromInt n)

  (* convert a CuLambda CPU type to a CUDA type *)
    fun toCUDATy ty = (case ty
	   of Cu.TyScalar ty => Util.gpuTypeToCUDA ty
	    | Cu.TySegdes => CL.T_Named "Segdes"
	    | Cu.TySeq(ty, _) => (case ty
		 of GPU.TyBool => CL.T_Named "BoolSeq_t"
		  | GPU.TyChar => CL.T_Named "CharSeq_t"
		  | GPU.TyInt => CL.T_Named "IntSeq_t"
		  | GPU.TyFloat => CL.T_Named "FloatSeq_t"
		(* end case *))
	    | Cu.TyTuple[ty1, ty2] => CL.T_Template("std::pair", [toCUDATy ty1, toCUDATy ty2])
	    | Cu.TyTuple tys => raise Fail "FIXME"
	  (* end case *))

  (* return the CUDA type of a variable *)
    fun typeOf x = toCUDATy (CuVar.typeOf x)

    fun genProgram (srcFile, Cu.Program(prog, progTy)) = let
	  val _ = L.analyze prog  (* compute liveness information for the program *)
	  val genArg = Util.genAtom getBinding
	  val kernels = ref []
	  val functions = ref []
	  fun genAlloc width (x, stms) = let
		val x' = CuVar.uniqueName x
		val stm = (case CuVar.typeOf x
		       of Cu.TyScalar ty => CL.mkDecl(Util.gpuTypeToCUDA ty, x', NONE)
			| ty => let
			    val ty = toCUDATy ty
			    in
			      CL.mkDeclInit(ty, x', CL.mkCons(ty, [width]))
			    end
		      (* end case *))
		in
		  stm :: stms
		end
	(* given an argument list of atoms, return a list of the variables that are not in live *)
	  fun deadArgs (live, args) = let
		fun chkFree (Cu.Var x) = if L.VSet.member(live, x)
		      then NONE
		      else SOME x
		  | chkFree _ = NONE
		in
		  List.mapPartial chkFree args
		end
	  fun genFree (x, stms) = CL.mkCallExp(CL.mkSelect(mkVar x, "free"), []) :: stms
	  fun bindResult (result, rhs, stms) = if CuVar.useCnt result > 1
		then let
		  val result' = CuVar.uniqueName result
		  in
		    setBinding (result, CL.mkVar result');
		    CL.mkDeclInit(typeOf result, result', rhs) :: stms
		  end
		else (setBinding(result, rhs); stms)
	  fun genExp (dsts, exp, stms) = (case (#2 exp)
		 of Cu.ExpFun{f, params, body, cont} => let
		      val f' = genFunct (CL.voidTy, f, params, body)
		      in
			functions := f' :: !functions;
			genExp (dsts, cont, stms)
		      end
		  | Cu.ExpLet{lhs, rhs, cont} => let
		    (* for each bound variable, we need to declare it and add it to the destination list *)
		      fun doLHSVars (x::xs, dsts, stms) = let
			    val x' = CuVar.uniqueName x
			    val dst = CL.mkVar x'
			    in
			      setBinding (x, dst);
			      doLHSVars (xs, dst::dsts, CL.mkDecl(typeOf x, x', NONE)::stms)
			    end
			| doLHSVars ([], dsts, stms) = (List.rev dsts, stms)
		      val (dsts', stms) = doLHSVars (lhs, [], stms)
		      val stms = List.revAppend (genExp (dsts', rhs, []), stms)
		      in
			genExp (dsts, cont, stms)
		      end
		  | Cu.ExpTask{task as TG.Task{name, width, inputs, ...}, cont} => let
		      val live = L.liveOut exp
		      val width = genArg width
		      val outputs = TG.outputs task
(* NOTE: those inputs that are not in the expression's liveOut set are available for reuse *)
		      val freeInputs = List.filter (fn x => not (L.VSet.member(live, x))) inputs
		      val freeOutputs = List.filter (fn x => not (L.VSet.member(live, x))) outputs
		      val stms = List.foldl (genAlloc width) stms outputs
		      fun mkOutArg x = (case CuVar.typeOf x
			     of Cu.TyScalar _ => CL.mkUnOp(CL.%&, mkVar x)
			      | Cu.TyTuple _ => raise Fail "tuple kernel argument"
			      | _ => CL.mkSelect(mkVar x, "_datap")
			    (* end case *))
		      fun mkInArg x = CL.mkSelect(getBinding x, "_datap")
		      val stms = CL.mkKernCall(
			      name,
			      [CL.mkApply("_gridDim", [width]), CL.mkApply("_blockDim", [width])],
			      width :: List.map mkOutArg outputs @ List.map mkInArg inputs
			    ) :: stms
		      val stms = List.foldl genFree stms freeInputs
		      val stms = List.foldl genFree stms freeOutputs
		      in
			List.app (fn x => setBinding(x, mkVar x)) outputs;
			kernels := GenCUDA.genTask task :: !kernels;
			genExp (dsts, cont, stms)
		      end
		  | Cu.ExpVectorOp{result, isFlat, rator, args, cont} => let
		      val freeArgs = deadArgs (L.liveOut exp, args)
		      val stms' = (case rator
			     of VecOp.PERMUTE ty => raise Fail "FIXME"
			      | VecOp.DPERMUTE ty => raise Fail "FIXME"
			      | VecOp.FPERMUTE ty => raise Fail "FIXME"
			      | VecOp.BPERMUTE ty => raise Fail "FIXME"
			      | VecOp.BFPERMUTE ty => raise Fail "FIXME"
			      | VecOp.DFPERMUTE ty => raise Fail "FIXME"
			      | VecOp.EXTRACT ty => if isFlat
				  then let
				    val [seq, idx] = List.map genArg args
				    val result' = CuVar.uniqueName result
				    val dst = CL.mkVar result'
				    val stm = CL.mkDeclInit(
					  typeOf result, result',
					  CL.mkApplyExp(CL.mkSelect(seq, "extract"), [idx]))
				    in
				      setBinding (result, dst);
				      [stm]
				    end
				  else raise Fail "FIXME"
			      | VecOp.REPLACE ty => raise Fail "FIXME"
			      | VecOp.PACK ty => raise Fail "FIXME"
			      | VecOp.RANK_UP ty => raise Fail "FIXME"
			      | VecOp.RANK_DOWN ty => raise Fail "FIXME"
			    (* end case *))
		      val stms = List.revAppend(stms', stms)
		      val stms = List.foldl genFree stms freeArgs
		      in
			genExp (dsts, cont, stms)
		      end
		  | Cu.ExpCPUOp{result, rator, arg, cont} => raise Fail "FIXME: CPUOp"
		  | Cu.ExpSeq{result, args, cont} => let
		      val seqTy as Cu.TySeq(ty, SOME n) = CuVar.typeOf result
		      val tmp = CuVar.new("initSeq", seqTy)
		      val initDcl = CL.mkDecl(
			    CL.T_Array(Util.gpuTypeToCUDA ty, SOME n), CuVar.uniqueName tmp,
			    SOME(CL.I_Exps(List.map (fn arg => CL.I_Exp(genArg arg)) args)))
		      val ty = typeOf result
		      val result' = CuVar.uniqueName result
		      val initStm = CL.mkDeclInit(ty, result', CL.mkCons(ty, [mkInt n, mkVar tmp]))
		      in
			setBinding (result, CL.mkVar result');
			genExp (dsts, cont, initStm :: initDcl :: stms)
		      end
		  | Cu.ExpTuple{result, args as [_, _], cont} => let
		      val rhs = CL.mkCons(typeOf result, List.map genArg args)
		      in
			genExp (dsts, cont, bindResult (result, rhs, stms))
		      end
		  | Cu.ExpTuple{result, args, cont} => raise Fail "FIXME: Tuple"
		  | Cu.ExpPrim{result, rator, args, cont} => let
		    (* create the expression for the primitive *)
		      val rhs = Util.genScalarOp(rator, List.map genArg args)
		      in
			genExp (dsts, cont, bindResult (result, rhs, stms))
		      end
		  | Cu.ExpCmd{results, cmd, args, cont} => let
		      val results' = List.map CuVar.uniqueName results
		      val cmdStms = (case (results', cmd, List.map genArg args)
			     of (_, Cmd.EXIT, args) => [CL.mkCall("EXIT", args)]
			      | (_, Cmd.READ ty, args) => [] (* FIXME *)
			      | (_, Cmd.WRITE ty, args) => [] (* FIXME *)
			      | (_, Cmd.FOPEN, args) => [] (* FIXME *)
			      | (_, Cmd.FCLOSE, args) => [] (* FIXME *)
			      | (_, Cmd.FWRITE ty, args) => [] (* FIXME *)
			      | (_, Cmd.FREAD ty, args) => [] (* FIXME *)
			      | (_, Cmd.FREAD_CHAR, args) => [] (* FIXME *)
			      | ([t], Cmd.START_TIMER, args) => (* FIXME *)
				  [CL.mkDeclInit(CL.doubleTy, t, CL.mkApply("START_TIMER", args))]
			      | ([t], Cmd.STOP_TIMER, args) =>
				  [CL.mkDeclInit(floatTy, t, CL.mkApply("STOP_TIMER", args))]
			      | (_, Cmd.SRAND, args) => [] (* FIXME *)
			    (* end case *))
		      in
			ListPair.app (fn (x, x') => setBinding(x, CL.mkVar x')) (results, results');
			genExp (dsts, cont, List.revAppend (cmdStms, stms))
		      end
		  | Cu.ExpIf{cond, trueExp, falseExp} => let
		      val stms1 = genExp(dsts, trueExp, [])
		      val stms2 = genExp(dsts, falseExp, [])
		      in
			List.rev (
			  CL.mkIfThenElse(getBinding cond, CL.mkBlock stms1, CL.mkBlock stms2) :: stms)
		      end
		  | Cu.ExpApply{f, args} => raise Fail "FIXME"
		  | Cu.ExpReturn args => let
		      fun gen (x, y, stms) = CL.mkAssign(x, genArg y) :: stms
		      in
			if List.null dsts
			  then (case args (* tail-position in function, so return result *)
			     of [] => List.rev (CL.mkReturn NONE :: stms)
			      | [x] => List.rev (CL.mkReturn(SOME(genArg x)) :: stms)
			      | _ => raise Fail "unexpected multiple return arguments"
			    (* end case *))
			  else List.rev (ListPair.foldlEq gen stms (dsts, args))
(* DEBUG *)handle ex => raise ex
		      end
		(* end case *))
	  and genFunct (retTy, f, params, body) = let
		fun doParam x = let
		      val x' = CuVar.uniqueName x
		      in
			setBinding (x, CL.mkVar x');
			CL.PARAM([], typeOf x, x')
		      end
		val inParams = List.map doParam params
		val results = [] (* FIXME *)
		val outParams = [] (* FIXME *)
		in
		  CL.D_Func([], retTy, f, inParams @ outParams, CL.mkBlock(genExp(results, body, [])))
		end
	  val (progIsVoid, progTy) = (case progTy
		 of NONE => (true, CL.voidTy)
		  | SOME ty => (false, toCUDATy ty)
		(* end case *))
	  val progFn = genFunct (progTy, "nesl_main", [], prog)
	  val mainBody = if progIsVoid
		then [CL.mkCall("nesl_main", [])]
		else [
		    CL.mkDeclInit(progTy, "result", CL.mkApply("nesl_main", [])),
		    CL.mkExpStm(
		      CL.mkBinOp(
			CL.mkBinOp(
			  CL.mkBinOp(CL.mkVar "std::cout", CL.#<<, CL.mkStr "result = "),
			  CL.#<<, CL.mkVar "result"),
			CL.#<<, CL.mkVar "std::endl"))
		  ]
	  val mainFn = CL.D_Func([], CL.intTy, "main",
		[CL.PARAM([], CL.intTy, "argc"), CL.PARAM(["const"], CL.T_Ptr CL.charPtr, "argv")],
		CL.mkBlock(
		  CL.verbatimStm [MainInitFrag.text] [] ::
		  mainBody @
		  [CL.mkReturn(SOME(CL.mkInt 0))]))
	  in
	    includeStuff [
		("SRCFILE", srcFile),
(* FIXME: these parameters should be target based! *)
		("MAX_BLOCK_WIDTH", "65535"),
		("THREADS_PER_BLOCK", "256")
	      ] :: List.revAppend (!kernels, List.revAppend (!functions, [progFn, mainFn]))
	  end

  end
