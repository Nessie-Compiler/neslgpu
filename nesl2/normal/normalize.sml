(* normalize.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

(* normalization translates something like
 *
 *	function unzip3 (v : [(a, (b, c))]) = let
 *	    (xs,yzs) = unzip(v);
 *	    (ys,zs) = unzip(yzs);
 *	  in
 *	    (xs,ys,zs) $
 *
 * to
 *
 *	function unzip3 (v) = let
 *	    p1 = unzip(v);
 *	    xs = #1 p1
 *	    p2 = unzip(yzs);
 *	    ys = #2.1 p2
 *	    zs = #2.2 p2
 *	    t = (ys, zs)
 *	    t' = (xs, t)
 *	  in
 *	    t' $
 *)

structure Normalize : sig

    val transform : MonoAST.program -> NormalEnv.env * NormalAST.program

  (* normalize a function *)
    val normalizeFunct : NormalEnv.env
	  -> (MonoAST.funct * MonoAST.pat * MonoAST.exp)
	    -> (NormalAST.funct * NormalAST.var list * NormalAST.exp)

  end = struct

    structure M = MonoAST
    structure N = NormalAST
    structure NTy = NormalTy
    structure NV = NormalVar
    structure E = NormalEnv

  (* NormalAST types are the same as MonoAST types (for now) *)
    fun cvtTy ty = ty

  (* return the type of an atom *)
    fun typeOf (N.Var x) = NV.typeOf x
      | typeOf (N.Int _) = NTy.tyInt
      | typeOf (N.Float _) = NTy.tyFloat
      | typeOf (N.Bool _) = NTy.tyBool
      | typeOf (N.String _) = NTy.tyString
      | typeOf (N.Char _) = NTy.tyChar

  (* generate a local variable *)
    local
      val cnt = ref 0w0
    in
    fun newVar (name, ty) = let
	  val id = !cnt
	  in
	    cnt := id + 0w1;
	    NV.new(Atom.atom(concat[name, "_", Word.toString id]), ty)
	  end
    fun newTmp ty = newVar ("_t", ty)
    end (* local *)

    type env = E.env

    val bindFunct = E.bindFunct
    val bindLocalVar = E.bindLocalVar
    val bindGlobalVar = E.bindGlobalVar
    val lookupFunct = E.lookupFunct
    val lookupVar = E.lookupVar

  (* convert a monomorphic MonoAST.funct to a NormalAST.funct *)
    fun cvtFunct (env, f) = let
	  val f' = N.F{
		  name = MonoFunct.nameOf f,
		  stamp = Stamp.new(),
		  props = PropList.newHolder(),
		  ty = let val (domTy, rngTy) = MonoFunct.typeOf f in (cvtTy domTy, cvtTy rngTy) end
		}
	  in
	    bindFunct (env, f, f');
	    f'
	  end

  (* convert a local MonoAST variable to a NormalAST variable *)
    fun cvtLocalVar (env, x) = let
	  val x' = NV.new(MonoVar.nameOf x, cvtTy(MonoVar.typeOf x))
	  val env' = bindLocalVar (env, x, x')
	  in
	    (x', env')
	  end

  (* convert a top-level MonoAST variable to a NormalAST variable *)
    fun cvtGlobalVar (env, x) = let
	  val x' = NV.new(MonoVar.nameOf x, cvtTy(MonoVar.typeOf x))
	  in
	    bindGlobalVar (env, x, x');
	    (x', env)
	  end

  (* extend an environment by mapping MonoAST pattern variables to new NormalAST variables *)
    fun cvtPatVars (cvtVar : env * M.var -> N.var * env) (env, pat) = let
	  fun cvt (M.PatPair(pat1, pat2), env) = cvt(pat2, cvt(pat1, env))
	    | cvt (M.PatVector(pat1, pat2, _), env) = cvt(pat2, cvt(pat1, env))
	    | cvt (M.PatVar x, env) = #2 (cvtVar(env, x))
	  in
	    cvt (pat, env)
	  end

    datatype pending_binding
      = SelectBind of (N.var * int * N.var)	(* x = #i(y) *)
      | VectorBind of (N.var * N.var * N.var)	(* __vector(x, y) = v *)

  (* we convert patterns into the triple (rootVar, binds), where rootVar is the
   * variable bound to the value matched by the pattern and binds is a list of
   * pending_bindings.
   *)
    fun cvtPat (env, M.PatVar x) = let
	  val x = lookupVar(env, x)
	  in
	    (x, [])
	  end
      | cvtPat (env, pat) = let
	  fun cvt (M.PatPair(pat1, pat2)) = let
		val (x1, binds1) = cvt pat1
		val (x2, binds2) = cvt pat2
		val x' = newTmp (NTy.tyPair(NV.typeOf x1, NV.typeOf x2))
		val binds = SelectBind(x1, 1, x') :: binds1 @ SelectBind(x2, 2, x') :: binds2
		in
		  (x', binds)
		end
	    | cvt (M.PatVector(pat1, pat2, ty)) = let
		val (x1, binds1) = cvt pat1
		val (x2, binds2) = cvt pat2
		val x' = newTmp (cvtTy ty)
		val binds = VectorBind(x1, x2, x') :: binds1 @ binds2
		in
		  (x', binds)
		end
	    | cvt (M.PatVar x) = let
		val x = lookupVar (env, x)
		in
		  (x, [])
		end
	  in
	    cvt pat
	  end

  (* wrap bindings around an expression *)
    fun wrapBinds ([], e) = e
      | wrapBinds (SelectBind(x, i, y)::r, e) =
	  N.mkPure(x, Pure.PROJ i, [N.Var y], wrapBinds(r, e))
      | wrapBinds (VectorBind(x, y, v)::r, e) = N.mkLetVector(x, y, N.Var v, wrapBinds(r, e))

    fun normalizeFunct (env : env) = let
	  fun normalizeFn (f, pat, body) = let
		val f' = cvtFunct (env, f)
		val env = cvtPatVars cvtLocalVar (env, pat)
		val (param, binds) = cvtPat (env, pat)
		val body = normalizeToExp (env, body, fn (e, _) => e)
		in
		  (f', [param], wrapBinds(binds, body))
		end
	  in
	    normalizeFn
	  end

    and normalizeForEach (env, body, binds, ty) = let
	(* map the bound variables to their NormalAST counterparts *)
	  val env = List.foldl (fn ((pat, _), env) => cvtPatVars cvtLocalVar (env, pat)) env binds
	(* each binding of the form "pat in exp" gets converted to "x in xs" along with
	 * a list of bindings for the pattern and a binding "xs = exp".  We first convert
	 * the patterns and just remember the (x in exp) list for later processing.
	 *)
	  val (patBinds, expBinds) = let
		fun cvtBind ((pat, exp), (patBinds, expBinds)) = let
		      val (x, binds) = cvtPat (env, pat)
		      in
			(binds @ patBinds, (x, exp)::expBinds)
		      end
		in
		  List.foldr cvtBind ([], []) binds
		end
	(* convert the function body and wrap it with the pattern bindings *)
	  val body = wrapBinds(patBinds, normalizeToExp (env, body, fn (e, _) => e))
	(* now we convert the sequence expressions *)
	  fun cvtExp ([], binds') = N.mkForEach(body, List.rev binds')
	    | cvtExp ((x, exp)::binds, binds') =
		normalizeToAtom (env, exp, fn (N.Var xs) => cvtExp(binds, (x, xs)::binds'))
	  in
	    (cvtExp (expBinds, []), cvtTy ty)
	  end

    and normalizeBind (env, (pat, exp), cxt : env -> N.exp) : N.exp =
	  normalizeToExp (env, exp, fn (rhs, ty) => let
	    val env = cvtPatVars cvtLocalVar (env, pat)
	    val (lhs, binds) = cvtPat (env, pat)
	    in
	      N.mkLet(lhs, rhs, wrapBinds(binds, cxt env))
	    end)

    and normalizeBinds (env, bind::binds, cxt : env -> N.exp) =
	  normalizeBind (env, bind, fn env => normalizeBinds (env, binds, cxt))
      | normalizeBinds (env, [], cxt) = cxt env

    and normalizeToExp (env, e : M.exp, cxt : (N.exp * N.ty) -> N.exp) : N.exp = (case e
	   of M.ExpPair(e1, e2) =>
		normalizeToAtom (env, e1, fn a1 =>
		  normalizeToAtom (env, e2, fn a2 => let
		    val ty = NTy.tyPair(typeOf a1, typeOf a2)
		    val y = newVar("pair", ty)
		    in
		      N.mkPair (y, a1, a2, cxt (N.mkVar y, ty))
		    end))
	    | M.ExpIf(e1, e2, e3) =>
		normalizeToAtom (env, e1, fn a1 => cxt (
		  N.mkIf(a1,
		    normalizeToExp (env, e2, fn (e2, _) => e2),
		    normalizeToExp (env, e3, fn (e3, _) => e3)),
		  cvtTy (MonoUtil.typeOf e2)))
	    | M.ExpLet(binds, e) =>
		normalizeBinds (env, binds, fn env => normalizeToExp (env, e, cxt))
	    | M.ExpApplyForEach(e, binds, ty) => cxt (normalizeForEach (env, e, binds, ty))
	    | M.ExpApply(f, e) => let
		val f' as N.F{ty, ...} = lookupFunct(env, f)
		in
		  normalizeToAtom (env, e, fn arg => cxt(N.mkApply(f', [arg]), cvtTy(#2 ty)))
		end
	    | M.ExpSeqRange(e1, e2, e3) =>
		normalizeToAtom (env, e1, fn a1 =>
		  normalizeToAtom (env, e2, fn a2 =>
		    normalizeToAtom (env, e3, fn a3 => let
		      val y = newVar("seq", NTy.tySeq NTy.tyInt)
		      in
			N.mkSeqRange(y, a1, a2, a3, cxt (N.mkVar y, NTy.tySeq NTy.tyInt))
		      end)))
	    | M.ExpSeq(exps, ty) =>
		normalizeToAtoms (env, exps, fn ats => let
		  val ty' = cvtTy ty
		  val y = newVar("seq", N.TySeq ty')
		  in
		    N.mkSeq(y, ats, cxt (N.mkVar y, ty'))
		  end)
	    | M.ExpVar x => let
		val x' as N.V{ty, ...} = lookupVar(env, x)
		in
		  cxt (N.mkVar x', ty)
		end
	    | M.ExpInt n => cxt (N.mkAtom(N.Int n), NTy.tyInt)
	    | M.ExpFloat f => cxt (N.mkAtom(N.Float f), NTy.tyFloat)
	    | M.ExpBool b => cxt (N.mkAtom(N.Bool b), NTy.tyBool)
	    | M.ExpString s => cxt (N.mkAtom(N.String s), NTy.tyString)
	    | M.ExpChar c => cxt (N.mkAtom(N.Char c), NTy.tyChar)
	    | M.ExpPureApply(primOp, e, ty) =>
		normalizeTupleToAtoms (env, e, Pure.arity primOp, fn ats => let
		  val ty = cvtTy ty
		  val y = newVar ("pure", ty)
		  in
		    N.mkPure(y, primOp, ats, cxt(N.mkVar y, ty))
		  end)
	    | M.ExpCmdApply(cmd, e, ty) => let
		val (domArity, rngArity) = Cmd.arity cmd
		val lhsTys = (case cvtTy ty
		       of NTy.TyTuple tys => tys
			| ty => [ty]
		      (* end case *))
		in
		  normalizeTupleToAtoms (env, e, domArity, fn ats => let
		    val ty = cvtTy ty
		  (* generate the left-hand-side variables *)
		    val ys = List.tabulate (rngArity,
			  fn i => newVar("cmd_"^Int.toString i, List.nth(lhsTys, i)))
		    fun mkBody ([], _) = raise Fail "no result"
		      | mkBody ([x], cxt') = cxt' x
		      | mkBody (x::xs, cxt') =
			  mkBody (xs, fn y => let
			    val ty' = NTy.tyPair(NV.typeOf x, NV.typeOf y)
			    val z = newVar("_t", ty')
			    in
			      N.mkTuple(z, [N.Var x, N.Var y], cxt' z)
			    end)
		    in
		      N.mkCmd(ys, cmd, ats,
			mkBody (ys, fn y => cxt(N.mkVar y, NV.typeOf y)))
		    end)
		end
	    | M.ExpVector(e1, e2, ty) =>
		normalizeToAtom (env, e1, fn atm1 =>
		  normalizeToAtom (env, e2, fn atm2 => let
		    val ty = cvtTy ty
		    val x = newVar("vec", ty)
		    in
		      N.mkVector(x, atm1, atm2, cxt(N.mkVar x, ty))
		    end))
	  (* end case *))

    and normalizeToAtom (env, e : M.exp, cxt : N.atom -> N.exp) : N.exp = (case e
	   of M.ExpPair(e1, e2) =>
		normalizeToAtom (env, e1, fn a1 =>
		  normalizeToAtom (env, e2, fn a2 => let
		    val y = newVar("pair", NTy.tyPair(typeOf a1, typeOf a2))
		    in
		      N.mkPair (y, a1, a2, cxt (N.Var y))
		    end))
	    | M.ExpIf(e1, e2, e3) =>
		normalizeToAtom (env, e1, fn a1 => let
		  val y = newVar("cond", cvtTy(MonoUtil.typeOf e2))
		  in
		    N.mkLet(y,
		      N.mkIf(a1,
			normalizeToExp (env, e2, fn (e2, _) => e2),
			normalizeToExp (env, e3, fn (e3, _) => e3)),
		      cxt(N.Var y))
		  end)
	    | M.ExpLet(binds, e) =>
		normalizeBinds (env, binds, fn env => normalizeToExp (env, e, fn (e, _) => e))
	    | M.ExpApplyForEach _ =>
		normalizeToExp (env, e, fn (exp, ty) => let
		  val result = newVar("v", cvtTy ty)
		  in
		    N.mkLet (result, exp, cxt(N.Var result))
		  end)
	    | M.ExpApply(f, e) => let
		val f' as N.F{ty, ...} = lookupFunct(env, f)
		val y = newVar("result", cvtTy(#2 ty))
		in
		  normalizeToAtom (env, e, fn arg =>
		    N.mkLet(y, N.mkApply(f', [arg]), cxt (N.Var y)))
		end
	    | M.ExpSeqRange(e1, e2, e3) =>
		normalizeToAtom (env, e1, fn a1 =>
		  normalizeToAtom (env, e2, fn a2 =>
		    normalizeToAtom (env, e3, fn a3 => let
		      val y = newVar("seq", NTy.tySeq NTy.tyInt)
		      in
			N.mkSeqRange(y, a1, a2, a3, cxt (N.Var y))
		      end)))
	    | M.ExpSeq(exps, ty) =>
		normalizeToAtoms (env, exps, fn ats => let
		  val y = newVar("seq", NTy.tySeq (cvtTy ty))
		  in
		    N.mkSeq(y, ats, cxt(N.Var y))
		  end)
	    | M.ExpVar x => cxt (N.Var(lookupVar(env, x)))
	    | M.ExpInt n => cxt (N.Int n)
	    | M.ExpFloat f => cxt (N.Float f)
	    | M.ExpBool b => cxt (N.Bool b)
	    | M.ExpString s => cxt (N.String s)
	    | M.ExpChar c => cxt (N.Char c)
	    | M.ExpPureApply(primOp, e, ty) =>
		normalizeTupleToAtoms (env, e, Pure.arity primOp, fn ats => let
		  val ty = cvtTy ty
		  val y = newVar ("pure", ty)
		  in
		    N.mkPure(y, primOp, ats, cxt(N.Var y))
		  end)
	    | M.ExpCmdApply(cmd, e, ty) => let
		val (domArity, rngArity) = Cmd.arity cmd
		val lhsTys = (case cvtTy ty
		       of NTy.TyTuple tys => tys
			| ty => [ty]
		      (* end case *))
		in
		  normalizeTupleToAtoms (env, e, domArity, fn ats => let
		    val ty = cvtTy ty
		  (* generate the left-hand-side variables *)
		    val ys = List.tabulate (rngArity,
			  fn i => newVar("cmd_"^Int.toString i, List.nth(lhsTys, i)))
		    fun mkBody ([], _) = raise Fail "no result"
		      | mkBody ([x], cxt') = cxt' x
		      | mkBody (x::xs, cxt') =
			  mkBody (xs, fn y => let
			    val ty' = NTy.tyPair(NV.typeOf x, NV.typeOf y)
			    val z = newVar("_t", ty')
			    in
			      N.mkTuple(z, [N.Var x, N.Var y], cxt' z)
			    end)
		    in
		      N.mkCmd(ys, cmd, ats,
			mkBody (ys, fn y => cxt(N.Var y)))
		    end)
		end
	    | M.ExpVector(e1, e2, ty) =>
		normalizeToAtom (env, e1, fn atm1 =>
		  normalizeToAtom (env, e2, fn atm2 => let
		    val ty = cvtTy ty
		    val x = newVar("vec", ty)
		    in
		      N.mkVector(x, atm1, atm2, cxt(N.Var x))
		    end))
	  (* end case *))

    and normalizeToAtoms (env, es : M.exp list, cxt : N.atom list -> N.exp) = let
	  fun norm ([], ats) = cxt(List.rev ats)
	    | norm (e::es, ats) = normalizeToAtom (env, e, fn at => norm(es, at::ats))
	  in
	    norm (es, [])
	  end

  (* given a tuple expression represented as right-nested pairs, normalize the tuple
   * components to a list of atoms.
   *)
    and normalizeTupleToAtoms (env, tplExp, arity, cxt : N.atom list -> N.exp) = let
	  fun norm (1, exp, ats) = normalizeToAtom (env, exp, fn at => cxt(List.rev(at::ats)))
	    | norm (n, M.ExpPair(e1, e2), ats) =
		normalizeToAtom (env, e1, fn at => norm(n-1, e2, at::ats))
	    | norm (n, e, ats) = raise Fail "FIXME"
	  in
	    norm (arity, tplExp, [])
	  end

    fun transform (M.Program prog) = let
	  val env = NormalEnv.new()
	  fun normalizeTop (M.TopFun(f, instances), tops) =
		N.TopFun(f, List.map (normalizeFunct env) instances) :: tops
	    | normalizeTop (M.TopBind(pat, exp), tops) = let
		val env = cvtPatVars cvtGlobalVar (env, pat)
		val (rootVar, binds) = cvtPat (env, pat)
		val rhs = normalizeToExp (env, exp, fn (e, _) => e)
		fun wrapBind (SelectBind(x, i, y), tops) = let
		      val x' = NV.new(NV.nameOf x, NV.typeOf x)
		      in
			N.TopBind(x, N.mkPure(x', Pure.PROJ i, [N.Var y], N.mkVar x')) :: tops
		      end
		  | wrapBind (VectorBind(x, y, v), tops) = raise Fail "FIXME: top-level __vector"
		in
		  List.foldl wrapBind (N.TopBind(rootVar, rhs) :: tops) binds
		end
	    | normalizeTop (M.TopExp(e, ty), tops) =
		N.TopExp(normalizeToExp (env, e, fn (e, _) => e), cvtTy ty) :: tops
	  val tops = List.rev(List.foldl normalizeTop [] prog)
	  in
	    (env, N.Program tops)
	  end

  end
