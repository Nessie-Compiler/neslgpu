(* fusion.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Fusion : sig

    val transform : FuseAST.program -> FuseAST.program

  end = struct

    structure F = FuseAST
    structure V = FuseVar
    structure VMap = V.Map

    fun extendEnv (env, x) = let
	  val x' = V.copy x
	  in
	    (x', VMap.insert(env, x, x'))
	  end

    fun extendEnv' (env, params) = let
	  fun f (x, (xs, env)) = let
		val (x', env') = extendEnv (env, x)
		in
		  (x'::xs, env')
		end
	  in
	    List.foldr f ([], env) params
	  end

    fun rename env x = (case VMap.find(env, x)
	   of SOME x' => x'
	    | _ => raise Fail(concat["rename: unbound variable ", V.toString x])
	  (* end case *))

    fun renameAtom env (F.Var x) = F.Var(rename env x)
      | renameAtom _ atm = atm

  (* a mapping from variables to available definitions; we need to use this mechanism
   * (instead of just using the bindings), because we do not want to fuse across commands
   * (e.g., START_TIMER).
   *)
    type avail_maps = (F.kernel * F.atom * F.var list) VMap.map

  (* copy a kernel expression, renaming bound variables *)
    fun copyKernExp (env, exp) = (case exp
	   of F.KExp_Let(xs, e1, e2) => let
		val (xs', env') = extendEnv' (env, xs)
		in
		  F.mkKLet(xs', copyKernExp (env, e1), copyKernExp (env', e2))
		end
	    | F.KExp_Pure(x, rator, atms, e) => let
		val (x', env') = extendEnv (env, x)
		in
		  F.mkKPure(x', rator, List.map (renameAtom env) atms, copyKernExp(env', e))
		end
	    | F.KExp_Proj(x, i, y, e) => let
		val (x', env') = extendEnv (env, x)
		in
		  F.mkKProj(x', i, rename env y, copyKernExp(env', e))
		end
	    | F.KExp_Tuple(x, atms, e) => let
		val (x', env') = extendEnv (env, x)
		in
		  F.mkKTuple(x', List.map (renameAtom env) atms, copyKernExp(env', e))
		end
	    | F.KExp_If(atm, e1, e2) =>
		F.mkKIf(renameAtom env atm, copyKernExp (env, e1), copyKernExp (env, e2))
	    | F.KExp_Return atms => F.mkKReturn(List.map (renameAtom env) atms)
	  (* end case *))

  (* eliminate redundant arguments in a kernel definition/invocation.  For example,
   *
   *	kernel k (a, b) = a + b
   *	... k (xs, xs) ...
   *
   * will get specialized to
   *
   *    kernel k' (a) = a + a
   *    ... k' (xs) ...
   *)
    fun elimRedundantArgs (params, body, args) = let
	  fun chk ([], [], _, _, []) = NONE
	    | chk ([], [], args', params', redundantParams) = let
		val env = List.foldl VMap.insert' VMap.empty redundantParams
		val env = List.foldl (fn (x, env) => VMap.insert(env, x, x)) env params'
		in
		  SOME(List.rev params', copyKernExp(env, body), List.rev args')
		end
	    | chk (arg::args, param::params, args', params', redundantParams) = let
		fun findArg (x::xs, param::params) = if V.same(arg, x)
		      then SOME param
		      else findArg (xs, params)
		  | findArg _ = NONE
		in
		  case findArg (args', params')
		   of SOME param' =>
			chk(args, params, args', params', (param, param')::redundantParams)
		    | NONE => chk(args, params, arg::args', param::params', redundantParams)
		  (* end case *)
		end
	  in
	    chk (args, params, [], [], [])
	  end

  (* Fuse a kernel with its arguments.  We are assuming that kernels are single result at
   * this point.  For each argument to the kernel, we check to see if it is the result of
   * a kernel application.  If so, we fuse the kernels.  After fusion, we check for
   * redundant arguments
   *)
    fun fuse (availMaps, kern, wid, args) = let
	(* for each argument, we check to see if it is bound to the result of
	 * another kernel.
	 *)
	  fun doArgs (x::xs, y::ys, changed, wid, args, params, body) = (case VMap.find(availMaps, x)
		 of SOME(kern', wid', args') => let
		      val (params', body') = Kernel.defn kern'
		      val (newParams', env') = extendEnv' (VMap.empty, params')
		      val newWid = (case (wid, wid')  (* favor constant over variable widths *)
			     of (F.Var _, F.Int _) => wid'
			      | _ => wid
			    (* end case *))
		      in
			doArgs (xs, ys, true, newWid,
			  List.revAppend(args', args),
			  List.revAppend(newParams', params),
			  F.mkKLet([y], copyKernExp(env', body'), body))
		      end
		  | NONE => (* argument is not fusable *)
		      doArgs (xs, ys, changed, wid, x::args, y::params, body)
		(* end case *))
	    | doArgs ([], [], changed, wid, args, params, body) = let
		val params = List.rev params
		val args = List.rev args
		val (changed, args, params, body) = (
		      case elimRedundantArgs (params, body, args)
		       of NONE => (changed, args, params, body)
			| SOME(params, body, args) => (true, args, params, body)
		      (* end case *))
		in
		  if changed
		    then let
		      val (_, rngTy) = Kernel.typeOf kern
		      val params = List.rev params
		      val body = (
			    KernelExp.census (params, body);
			    KernelExp.contract body)
		      val newKern = Kernel.new("FUSED", params, body, rngTy)
		      in
			SOME(
			  F.mkTopKern newKern,
			  newKern,
			  wid,
			  List.rev args)
		      end
		    else NONE
		end
	(* get the kernel's binding *)
	  val (params, body) = Kernel.defn kern
	(* rename parameters and initialize the renaming environment *)
	  val (newParams, env) = extendEnv' (VMap.empty, params)
	  in
	    doArgs (args, newParams, false, wid, [], [], copyKernExp(env, body))
	  end

    fun transform (F.Program prog) = let
	  val newKernels = ref []
	  fun xformMap (availMaps, [lhs], k, wid, args) = let
		val mapExp = (case fuse(availMaps, k, wid, args)
		       of SOME(kernelBind, k', wid', args') => (
			    newKernels := kernelBind :: !newKernels;
			    Kernel.decCnt k;  FuseCensus.decAtom wid; List.app V.decCnt args;
			    Kernel.incCnt k'; FuseCensus.incAtom wid'; List.app V.incCnt args';
			    (k', wid', args'))
			| NONE => (k, wid, args)
		      (* end case *))
		in
		  (F.RHS_Map mapExp, VMap.insert(availMaps, lhs, mapExp))
		end
	  fun xformTop (availMaps, top) = (case top
		 of F.Top_Kern k => (
		      (top, availMaps))
		  | F.Top_Funct(f, xs, body) =>
		      (F.mkTopFun(f, xs, xformExp(VMap.empty, body)), availMaps)
		  | F.Top_Let(xs, e) =>
		      (F.mkTopLet(xs, xformExp(availMaps, e)), availMaps)
		  | F.Top_RHS(xs, F.RHS_Map(k, wid, args)) => let
		      val (rhs, availMaps) = xformMap (availMaps, xs, k, wid, args)
		      in
			(F.mkTopRHS(xs, rhs), availMaps)
		      end
		  | F.Top_RHS(xs, F.RHS_Cmd _) => (top, VMap.empty) (* clear available maps *)
		  | F.Top_RHS(xs, rhs) => (top, availMaps)
		  | F.Top_Exp(e, ty) => (F.mkTopExp(xformExp(availMaps, e), ty), availMaps)
		(* end case *))
	  and xformExp (availMaps : avail_maps, e) = (case e
		 of F.Exp_Let(xs, e1, e2) =>
		      F.mkLet(xs, xformExp(availMaps, e1), xformExp(availMaps, e2))
		  | F.Exp_RHS(xs, rhs as F.RHS_Map(k, wid, args), e) => let
		      val (rhs, availMaps) = xformMap (availMaps, xs, k, wid, args)
		      in
			F.mkRHS(xs, rhs, xformExp(availMaps, e))
		      end
		  | F.Exp_RHS(xs, rhs as F.RHS_Cmd _, e) => (* clear available maps *)
		      F.mkRHS(xs, rhs, xformExp(VMap.empty, e))
		  | F.Exp_RHS(xs, rhs, e) => F.mkRHS(xs, rhs, xformExp(availMaps, e))
		  | F.Exp_If(atm, e1, e2) =>
		      F.mkIf(atm, xformExp(availMaps, e1), xformExp(availMaps, e2))
		  | F.Exp_Apply _ => e
		  | F.Exp_Atom _ => e
		(* end case *))
	  val prog = let
		fun f (top, (tops, availMaps)) = let
		      val (top, availMaps) = xformTop (availMaps, top)
		      in
			(top::tops, availMaps)
		      end
		in
		  List.rev (#1 (List.foldl f ([], VMap.empty) prog))
		end
	  in
	    F.Program(List.revAppend (!newKernels, prog))
	  end

  end
