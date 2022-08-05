(* convert.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Convert : sig

    val transform : FuseAST.program -> CuLambda.program

  end = struct

    structure F = FuseAST
    structure VMap = FuseVar.Map
    structure FFun = FuseFunct
    structure FKern = Kernel
    structure GenOp = GeneratorOp
    structure TG = TaskGraph
    structure C = CuLambda
    structure G = C.GPU
    structure CV = CuVar
    structure CVSet = RedBlackSetFn(
      struct
	type ord_key = C.var
	val compare = CV.compare
      end)

    local
      fun cvtScalarTy TypeBase.INT = G.TyInt
        | cvtScalarTy TypeBase.BOOL = G.TyBool
        | cvtScalarTy TypeBase.FLOAT = G.TyFloat
        | cvtScalarTy TypeBase.CHAR = G.TyChar
        | cvtScalarTy tb = raise Fail (concat["cvtScalarTy(", TypeBase.baseToString tb, ")"])
    in

    fun toGPUTy (F.TyScalar tb) = cvtScalarTy tb
      | toGPUTy (F.TySeq(tb, len)) = cvtScalarTy tb
      | toGPUTy ty = raise Fail (concat["toGPUTy(", FuseUtil.tyToString ty, ")"])

    fun toCPUTy (F.TyScalar tb) = C.TyScalar(cvtScalarTy tb)
      | toCPUTy F.TySegdes = C.TySegdes
      | toCPUTy (F.TySeq(tb, len)) = C.TySeq(cvtScalarTy tb, len)
      | toCPUTy (F.TyTuple tys) = C.TyTuple(List.map toCPUTy tys)

    end (* local *)

  (* a property to map FuseAST functions to CuLambda functions *)
    val {getFn = (getFunct : F.funct -> C.funct), setFn = setFunct, ...} =
	  FFun.newProp (fn f => raise Fail(FFun.toString f))
(* NOTE: right now, we are assuming that all of our functions have unique names! *)
    fun newFunct f = let
	  val f' = FuseFunct.nameOf f
	  in
	    setFunct(f, f');
	    f'
	  end

  (* property to map a TaskGraph.task to its CuLambda translation *)
    val {getFn=(getTask : TG.task -> C.TaskGraph.task), setFn=setTask, ...} =
	  TG.newProp(fn _ => raise Fail "help")

  (* property to mark tasks that have been invoked *)
    val {getFn=hasBeenInvoked, setFn=markInvoked} = TG.newFlag()

    datatype env = Env of {
	avail : CVSet.set,		(* CPU vars that have been bound; used to determine
					 * when we can invoke a kernel.
					 *)
	cpuMap : C.var VMap.map,
	gpuMap : G.var VMap.map
      }

    val empty = Env{avail = CVSet.empty, cpuMap = VMap.empty, gpuMap = VMap.empty}

    local
      fun lookup (vMap, x) = (case VMap.find(vMap, x)
	     of NONE => raise Fail("unbound variable " ^ FuseVar.toString x)
	      | SOME x' => x'
	    (* end case *))
      fun extendEnv newVar (vMap, x) = let
	    val x' = newVar x
	    in
	      (x', VMap.insert(vMap, x, x'))
	    end
      fun extendEnv' newVar (vMap, xs) = let
	    fun extend ([], xs', vMap) = (List.rev xs', vMap)
	      | extend (x::xs, xs', vMap) = let
		  val x' = newVar x
		  in
		    extend (xs, x'::xs', VMap.insert(vMap, x, x'))
		  end
	    in
	      extend (xs, [], vMap)
	    end
      fun cvtAtom lookup env atm = (case atm
	     of F.Var x => C.Var(lookup(env, x))
	      | F.Bool b => C.Bool b
	      | F.Char c => C.Char c
	      | F.Int n => C.Int n
	      | F.Float f => C.Float f
	      | F.String s => raise Fail "FIXME"
	    (* end case *))
    in
  (* CPU variable and environment functions *)
    fun cpuNewVar x = CV.new(FuseVar.name x, toCPUTy(FuseVar.typeOf x))
    fun cpuExtend (Env{avail, cpuMap, gpuMap}, x) = let
	  val (x, cpuMap) = extendEnv cpuNewVar (cpuMap, x)
	  in
	    (x, Env{avail=avail, cpuMap=cpuMap, gpuMap=gpuMap})
	  end
    fun cpuExtend' (Env{avail, cpuMap, gpuMap}, xs) = let
	  val (xs, cpuMap) = extendEnv' cpuNewVar (cpuMap, xs)
	  in
	    (xs, Env{avail=avail, cpuMap=cpuMap, gpuMap=gpuMap})
	  end
    fun cpuBind (x, Env{avail, cpuMap, gpuMap}) =
	  Env{avail=CVSet.add(avail, x), cpuMap=cpuMap, gpuMap=gpuMap}
    fun removeAvail (Env{avail, ...}, xs) = List.filter (fn x => not(CVSet.member(avail, x))) xs
    fun cpuFind (Env{cpuMap, ...}) x = VMap.find(cpuMap, x)
    fun cpuLookup (Env{cpuMap, ...}, x) = lookup(cpuMap, x)
handle ex => raise ex
    val cpuCvtAtom = cvtAtom cpuLookup
  (* GPU variable and environment functions *)
    fun gpuNewVar x = CV.new(FuseVar.name x, toGPUTy(FuseVar.typeOf x))
handle ex => (print(concat["gpuNewVar(", FuseVar.toString x, ") failed\n"]); raise ex);
    fun gpuExtend (Env{avail, cpuMap, gpuMap}, x) = let
	  val (x, gpuMap) = extendEnv gpuNewVar (gpuMap, x)
	  in
	    (x, Env{avail=avail, cpuMap=cpuMap, gpuMap=gpuMap})
	  end
    fun gpuExtend' (Env{avail, cpuMap, gpuMap}, xs) = let
	  val (xs, gpuMap) = extendEnv' gpuNewVar (gpuMap, xs)
	  in
	    (xs, Env{avail=avail, cpuMap=cpuMap, gpuMap=gpuMap})
	  end
    fun gpuLookup (Env{gpuMap, ...}, x) = lookup(gpuMap, x)
handle ex => raise ex
    val gpuCvtAtom = cvtAtom gpuLookup
    end

    fun cvtTop (env, top, k) = (case top
	   of F.Top_Kern kern => k env
	    | F.Top_Funct(f, xs, body) => let
		val f' = newFunct f
		val (xs', env') = cpuExtend' (env, xs)
		val body' = cvtExp (env', body)
		in
		  C.mkFun{f = f', params = xs', body = body', cont = k env}
		end
	    | F.Top_Let(xs, e) => let
		val e' = cvtExp (env, e)
		val (xs', env') = cpuExtend' (env, xs)
		in
		  C.mkLet{lhs = xs', rhs = e', cont = k env'}
		end
	    | F.Top_RHS(xs, rhs) => cvtRHS (env, xs, rhs, k)
	    | F.Top_Exp(e, ty) => let
		val result = CV.new ("dummy", toCPUTy ty)
		in
		  C.mkLet{lhs = [result], rhs = cvtExp(env, e), cont = k env}
		end
	  (* end case *))

    and cvtExp (env, exp) = (case exp
	   of F.Exp_Let(xs, e1, e2) => let
		val e1' = cvtExp (env, e1)
		val (xs', env') = cpuExtend' (env, xs)
		in
		  C.mkLet{lhs = xs', rhs = e1', cont = cvtExp(env', e2)}
		end
	    | F.Exp_RHS(xs, rhs, e) =>
		cvtRHS (env, xs, rhs, fn env' => cvtExp (env', e))
	    | F.Exp_If(F.Var cond, e1, e2) => C.mkIf{
		  cond = cpuLookup (env, cond),
		  trueExp = cvtExp (env, e1),
		  falseExp = cvtExp (env, e2)
		}
	    | F.Exp_Apply(f, args) => C.mkApply{
		  f = getFunct f,
		  args = List.map (cpuCvtAtom env) args
		}
	    | F.Exp_Atom atm => C.mkReturn[cpuCvtAtom env atm]
	  (* end case *))

    and cvtRHS (env, lhs, rhs, k) = let
	  fun withLHS (f : C.var list * env -> C.exp) = let
		val (lhs', env') = cpuExtend' (env, lhs)
		in
		  f (lhs', List.foldl cpuBind env' lhs')
		end
	  fun maybeEmitTask (env, lhs as x::_) = (case TG.getTask x
		 of SOME task => if not(hasBeenInvoked task)
		      then let
			val task' = getTask task
			val inputs = removeAvail (env, C.TaskGraph.inputs task')
			in
			  if List.null inputs
			    then (
			      markInvoked (task, true);
			      C.mkTask{
				  task = getTask task,
				  cont = k (List.foldl cpuBind env (C.TaskGraph.outputs task'))
				})
			    else k env
			end
		      else k env
		  | NONE => raise Fail(FuseVar.toString x ^ " does not have an assigned task")
		(* end case *))
	  in
	    case rhs
	     of F.RHS_Scalar(rator, args) => withLHS (fn ([lhs'], env') => C.mkPrim{
		    result = lhs',
		    rator = rator,
		    args = List.map (cpuCvtAtom env) args,
		    cont = k env'
		  })
	      | F.RHS_FlatGen(rator, width, args) => maybeEmitTask (env, lhs)
	      | F.RHS_SegGen(rator, width, args) => maybeEmitTask (env, lhs)
	      | F.RHS_Map(kernel, width, args) => maybeEmitTask (env, lhs)
	      | F.RHS_FlatReduce(rator, width, x) => maybeEmitTask (env, lhs)
	      | F.RHS_SegReduce(rator, width, seg, data) => maybeEmitTask (env, lhs)
	      | F.RHS_FlatScan(rator, width, x) => maybeEmitTask (env, lhs)
	      | F.RHS_SegScan(rator, width, seg, data) => maybeEmitTask (env, lhs)
	      | F.RHS_FlatVector(rator, args) => withLHS (fn ([lhs'], env') => C.mkVectorOp{
		    result = lhs',
		    isFlat = true,
		    rator = rator,
		    args = List.map (cpuCvtAtom env) args,
		    cont = k env'
		  })
	      | F.RHS_SegVector(rator, args) => withLHS (fn ([lhs'], env') => C.mkVectorOp{
		    result = lhs',
		    isFlat = false,
		    rator = rator,
		    args = List.map (cpuCvtAtom env) args,
		    cont = k env'
		  })
	      | F.RHS_Internal(rator, arg) => withLHS (fn ([lhs'], env') => C.mkCPUOp{
		    result = lhs',
		    rator = rator,
		    arg = cpuCvtAtom env arg,
		    cont = k env'
		  })
	      | F.RHS_Cmd(cmd, args) => withLHS (fn (lhs', env') => C.mkCmd{
		    results = lhs',
		    cmd = cmd,
		    args = List.map (cpuCvtAtom env) args,
		    cont = k env'
		  })
	      | F.RHS_Seq(args, ty) => withLHS (fn ([lhs'], env') => C.mkSeq{
		    result = lhs',
		    args = List.map (cpuCvtAtom env) args,
		    cont = k env'
		  })
	      | F.RHS_Tuple args => withLHS (fn ([lhs'], env') => C.mkTuple{
		    result = lhs',
		    args = List.map (cpuCvtAtom env) args,
		    cont = k env'
		  })
	      | F.RHS_Proj(i, x) => raise Fail "FIXME: RHS_Proj"
	    (* end case *)
	  end

    datatype dst_var = CPUVar of C.var | GPUVar of G.var

  (* lookup function for variables used in a task graph.  If they have a GPU binding, then
   * we use that, otherwise we look for a CPU binding.
   *)
    fun findVar (Env{cpuMap, gpuMap, ...}, x) = (case VMap.find(gpuMap, x)
	   of SOME x' => GPUVar x'
	    | NONE => (case VMap.find(cpuMap, x)
		 of SOME x' => CPUVar x'
		  | NONE => raise Fail("unbound variable " ^ FuseVar.toString x)
		(* end case *))
	  (* end case *))

  (* convert a FuseAST kernel to a CuLambda kernel *)
    fun cvtKernel (env, kern, args) = let
	  val (params, body) = Kernel.defn kern
	  val binding = let
		fun f (x, atm, bEnv) = VMap.insert(bEnv, x, C.Var atm)
		val bEnv = ListPair.foldr f VMap.empty (params, args)
		in
		  fn (env, x) => (case VMap.find(bEnv, x)
		   of SOME atm => atm
		    | NONE => C.Var(gpuLookup(env, x)))
		end
	  fun cvtAtom env (F.Var x) = binding(env, x)
	    | cvtAtom env atm = (case atm
		 of F.Bool b => C.Bool b
		  | F.Char c => C.Char c
		  | F.Int n => C.Int n
		  | F.Float f => C.Float f
		  | _ => raise Fail "FIXME"
		(* end case *))
	  fun cvtExp (env, exp) = (case exp
		 of F.KExp_Let(xs, e1, e2) => let
		      val (xs', env') = gpuExtend' (env, xs)
		      in
			G.ExpLet(xs', cvtExp(env, e1), cvtExp(env', e2))
		      end
		  | F.KExp_Pure(x, rator, args, e) => let
		      val (x', env') = gpuExtend (env, x)
		      in
			G.ExpPrim(x', rator, List.map (cvtAtom env) args, cvtExp(env', e))
		      end
		  | F.KExp_Proj(x, i, y, e) => raise Fail "FIXME"
		  | F.KExp_Tuple(x, args, e) => let
		      val (x', env') = gpuExtend (env, x)
		      in
			G.ExpLet([x'], G.ExpTuple(List.map (cvtAtom env) args), cvtExp(env', e))
		      end
		  | F.KExp_If(cond, e1, e2) =>
		      G.ExpIf(cvtAtom env cond, cvtExp(env, e1), cvtExp(env, e2))
		  | F.KExp_Return args =>
		      G.ExpTuple(List.map (cvtAtom env) args)
		(* end case *))
	  in
	    cvtExp (env, body)
	  end

    fun cvtTask (task, env) = let
	  val _ = TG.printTask(TextIO.stdOut, task)
	  fun addInputVar (x, (inps, args)) = let
		val x' = cpuLookup (env, x)
		in
		  if List.exists (fn y => CV.same(x', y)) inps
		    then (inps, x'::args)
		    else (x'::inps, x'::args)
		end
	  fun addInputAtm (arg, (inps, args)) = (case cpuCvtAtom env arg
		 of arg' as C.Var x' => if List.exists (fn y => CV.same(x', y)) inps
		      then (inps, arg'::args)
		      else (x'::inps, arg'::args)
		  | arg' => (inps, arg'::args)
		(* end case *))
	(* handle a rhs argument variable, which may be the output of another node in
	 * the task graph or may be a source input variable.
	 *)
	  fun addPredVar nd (x, (env, inps, srcs, args)) = if TG.isPred(nd, x)
		then (env, inps, srcs, gpuLookup (env, x)::args)
		else let
		  val x' = cpuLookup (env, x)
		  in
		    if List.exists (fn y => CV.same(x', y)) inps
		      then let (* x is already an input for some other part of the task *)
			val x'' = gpuLookup (env, x)
			in
			  (env, inps, srcs, x''::args)
			end
		      else let
			val (x'', env') = gpuExtend (env, x)
			in
			  (env', x'::inps, C.TaskGraph.SrcVar(x'', x')::srcs, x''::args)
			end
		  end
	(* handle a lhs result variable, which may be the input of another node in
	 * the task graph or may be a output variable.
	 *)
	  fun addSuccVar nd (x, (env, xs, sinks)) = if TG.isSucc(nd, x)
		then let
		  val (x', env') = gpuExtend(env, x)
		  in
		    (env', x'::xs, sinks)
		  end
		else let
		  val (x', env') = gpuExtend(env, x)
		  val (x'', env'') = cpuExtend(env', x)
		  in
		    (env'', x'::xs, C.TaskGraph.SinkVar(x'', x')::sinks)
		  end
	  fun setWid (NONE, atm) = SOME atm
	    | setWid (wid, _) = wid
	  fun cvt (env, nd::nds, width, inputs, srcs, nodes, sinks) = (
		case TG.defn nd
		 of ([x], F.RHS_FlatGen(rator, wid, args)) => let
		      val (inputs', args') = List.foldl addInputAtm (inputs, []) args
		      val width' = setWid (width, wid)
		      val (env', [x'], sinks') = addSuccVar nd (x, (env, [], sinks))
		      val src' = (case (rator, args')
			     of (GenOp.DIST ty, [y, _]) => C.TaskGraph.SrcFlatDist(x', y)
			      | (GenOp.INDEX, [stride, start]) =>
				  C.TaskGraph.SrcFlatIndex(x', start, stride)
			      | _ => raise Fail "bogus RHS_FlatGen"
			    (* end case *))
		      in
			cvt (env', nds, width', inputs', src'::srcs, nodes, sinks')
		      end
		  | ([x], F.RHS_SegGen(rator, wid, args)) => let
		      val (inputs', args') = List.foldl addInputVar (inputs, []) args
		      val width' = setWid (width, wid)
		      val (env', [x'], sinks') = addSuccVar nd (x, (env, [], sinks))
		      val src' = (case (rator, args')
			     of (GenOp.DIST ty, [y, _]) => raise Fail "FIXME"
			      | (GenOp.INDEX, [stride, start, segdes]) =>
				  C.TaskGraph.SrcSegIndex(x', segdes, start, stride)
			      | _ => raise Fail "bogus RHS_FlatGen"
			    (* end case *))
		      in
			cvt (env', nds, width', inputs', src'::srcs, nodes, sinks')
		      end
		  | (xs, F.RHS_Map(kernel, wid, args)) => let
		    (* since a map has both multiple inputs and outputs, it can induce
		     * additional sources (SrcVar) and sinks (SinkVar) in the graph.
		     *)
		      val width' = setWid (width, wid)
		      val (env', inputs', srcs', args') = List.foldl (addPredVar nd) (env, inputs, srcs, []) args
		      val args' = List.rev args'
		      val (xs', env') = gpuExtend' (env', xs)
		      val (env', xs', sinks') = List.foldl (addSuccVar nd) (env, [], sinks) xs
		      val node' = C.TaskGraph.NodeMap(List.rev xs', cvtKernel(env', kernel, args'))
		      in
			cvt (env', nds, width', inputs', srcs', node'::nodes, sinks')
		      end
		  | ([x], F.RHS_FlatReduce(rator, wid, y)) => let
		      val width' = setWid (width, wid)
		      val (env', inputs', srcs', [y']) = addPredVar nd (y, (env, inputs, srcs, []))
		      val (x', env') = cpuExtend (env', x)
		      val sink' = C.TaskGraph.SinkFlatReduce(x', rator, y')
		      in
			cvt (env', nds, width', inputs', srcs', nodes, sink'::sinks)
		      end
(*
		  | ([x], F.RHS_SegReduce(rator, seg, data)) => let
		      in
		      end
		  | ([x], F.RHS_FlatScan(rator, y)) => let
		      val nd' = C.TaskGraph.NodeFlatScan(x', rator, y')
		      in
			cvt (env', nds, inputs, srcs, nd'::nodes, sinks)
		      end
		  | ([x], F.RHS_SegScan(rator, seg, data)) => let
		      in
		      end
*)
		  | (_, rhs) => raise Fail(FuseUtil.rhsToString rhs)
		(* end case *))
	    | cvt (env, [], SOME w, inputs, srcs, nodes, sinks) = let
		val inputs = List.rev inputs
		val task' = C.TaskGraph.Task{
			name = TG.taskName task,
			width = cpuCvtAtom env w,
			inputs = inputs,
			srcs = List.rev srcs,
			body = List.rev nodes,
			sinks = List.rev sinks
		      }
		in
		  setTask (task, task');
		  env
		end
	  in
	    cvt (env, TG.nodes task, NONE, [], [], [], [])
	  end

    fun transform (prog as F.Program body) = let
	  val tasks = TG.mkGraph prog
	  val env = List.foldl cvtTask empty tasks
	  val programTy = ref NONE
	  fun cvtProg (env, [F.Top_Exp(e, ty)]) =  let
	      (* the last part of a program should always be an expression or a variable
	       * binding; when it is an expression, we return the result of the expression.
	       *)
		val ty = toCPUTy ty
		val result = CV.new ("result", ty)
		in
		  programTy := SOME ty;
		  C.mkLet{lhs = [result], rhs = cvtExp(env, e), cont = C.mkReturn[C.Var result]}
		end
	    | cvtProg (env, []) = C.mkReturn[]
	    | cvtProg (env, top::tops) =
		cvtTop (env, top, fn env' => cvtProg (env', tops))
	  val prog' = C.Program(cvtProg (env, body), !programTy)
	  in
	    CuCensus.census prog';
	    prog'
	  end

  end
