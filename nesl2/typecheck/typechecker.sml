(* typechecker.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Typechecker for Nesl.
 *)

structure Typechecker : sig

    exception Error

    val check : Error.err_stream * Env.env * ParseTree.program -> AST.program * Env.env

  end = struct

    structure PT = ParseTree
    structure T = AST
    structure TB = TypeBase
    structure Ty = NeslTypes
    structure E = Env
    structure U = Unify
    structure Set = AtomSet

  (* terms used when there is an error *)
    val bogusTyVar = TyVar.new(Atom.atom "bogus", TypeBase.ANY)
    val bogusExp = T.ExpError
    val bogusPat = T.PatError

  (* useful atoms *)
    val wildAtom = Atom.atom "_wild_"	(* for wildcard patterns *)
    val segdesAtom = Atom.atom "segdes"
    val vectorAtom = Atom.atom "vector"
    val functionAtom = Atom.atom "function"

    exception Error

    fun error (errStrm, span, msg) = Error.errorAt(errStrm, span, msg)

    datatype token
      = S of string | A of Atom.atom
      | V of Var.var | TY of Ty.ty | TYS of Ty.ty list | C of TB.class

    fun err (errStrm, span, toks) = let
	  fun tok2str (S s) = s
	    | tok2str (A a) = Atom.toString a
	    | tok2str (V x) = Atom.toString(Var.nameOf x)
	    | tok2str (TY ty) = Ty.toString ty
	    | tok2str (TYS []) = "()"
	    | tok2str (TYS[ty]) = Ty.toString ty
	    | tok2str (TYS tys) = String.concat[
		  "(", String.concatWith ", " (List.map Ty.toString tys), ")"
		]
	    | tok2str (C cls) = TB.classToString cls
	  in
	    error(errStrm, span, List.map tok2str toks)
	  end

  (* create a new type variable *)
    fun newTyVar name = TyVar.new (Atom.atom name, TB.TY)	(*should this be ANY? *)

  (* unwrap a mark to get the span and tree *) 
    fun unmark (errStrm, env, {span, tree}) = (errStrm, span, env, tree)

    fun remark ({span, tree}, tr) = {span=span, tree=tr}

  (* instantiate a function type *)
    fun instantiate (ty as Ty.Scheme([], _, _)) = ty
      | instantiate (Ty.Scheme(tvs, ty1, ty2)) = let
	  val tvs' = List.map TyVar.fresh tvs
	  val subst = ListPair.foldl
		(fn (tv1, tv2, s) => TyVar.Map.insert(s, tv1, tv2))
		  TyVar.Map.empty
		    (tvs, tvs')
	  fun appSubst (Ty.TyVar(Ty.TV{inst=ref(Ty.TY ty), ...})) = appSubst ty
	    | appSubst (Ty.TyVar tv) = (case TyVar.Map.find(subst, tv)
		 of NONE => raise Fail("unbound type variable " ^ TyVar.toString tv)
		  | SOME tv' => Ty.TyVar tv'
		(* end case *))
	    | appSubst Ty.TyError = Ty.TyError
	    | appSubst Ty.TyVoid = Ty.TyVoid
	    | appSubst (ty as Ty.TyBase _) = ty
	    | appSubst (ty as Ty.TyBaseSeq(Ty.TyBase _)) = ty
	    | appSubst (Ty.TyBaseSeq(ty as Ty.TyVar _)) = Ty.TyBaseSeq(appSubst ty)
	    | appSubst (Ty.TySeq ty) = Ty.TySeq(appSubst ty)
	    | appSubst (Ty.TyPair(ty1, ty2)) = Ty.TyPair(appSubst ty1, appSubst ty2)
	    | appSubst (Ty.TyData(dt, tys)) = Ty.TyData(dt, List.map appSubst tys)
	    | appSubst (Ty.TyFun(ty1, ty2)) = Ty.TyFun(appSubst ty1, appSubst ty2)
	  in
	    Ty.Scheme(tvs', appSubst ty1, appSubst ty2)
	  end

  (* close a function type *)
    fun closeTy (domTy, rngTy) = let
	  fun close (ty, tvs) = (case ty
		 of (Ty.TyVar(Ty.TV{inst=ref(Ty.TY ty), ...})) => close(ty, tvs)
		  | (Ty.TyVar tv) => TyVar.Set.add(tvs, tv)
		  | Ty.TyError => tvs
		  | Ty.TyVoid => tvs
		  | Ty.TyBase _ => tvs
		  | Ty.TyBaseSeq ty => close(ty, tvs)
		  | Ty.TySeq ty => close(ty, tvs)
		  | Ty.TyPair(ty1, ty2) => close(ty2, close(ty1, tvs))
		  | Ty.TyData(_, tys) => List.foldl close tvs tys
		  | Ty.TyFun(ty1, ty2) => close(ty2, close(ty1, tvs))
		(* end case *))
	  val tvs = TyVar.Set.listItems (close (rngTy, close (domTy, TyVar.Set.empty)))
	  in
	    Ty.Scheme(tvs, domTy, rngTy)
	  end

  (* patch the type arguments of a recursive function *)
    fun patchFunct (f, body) = (case Funct.typeOf f
	   of Ty.Scheme([], _, _) => ()
	    | Ty.Scheme(tvs, _, _) => let
		fun patch (T.ExpMark{tree, ...}) = patch tree
		  | patch (T.ExpPair(e1, e2)) = (patch e1; patch e2)
		  | patch (T.ExpIf(e1, e2, e3)) = (patch e1; patch e2; patch e3)
		  | patch (T.ExpLet(binds, e)) = (List.app patchBind binds; patch e)
		  | patch (T.ExpApplyForEach(e, binds, NONE, _)) = (
		      patch e; List.app patchBind binds)
		  | patch (T.ExpApplyForEach(e, binds, SOME e', _)) = (
		      patch e; List.app patchBind binds; patch e')
		  | patch (T.ExpApply(f', tyArgs, e)) = (
		      if Funct.same(f, f')
			then tyArgs := List.map Ty.TyVar tvs
			else ();
		      patch e)
		  | patch (T.ExpApplyVar(_, e)) = patch e
		  | patch (T.ExpCons(_, _, e)) = patch e
		  | patch (T.ExpSeqRange(e1, e2, e3)) = (patch e1; patch e2; patch e3)
		  | patch (T.ExpSeq(es, _)) = List.app patch es
		  | patch (T.ExpVar _) = ()
		  | patch (T.ExpInt _) = ()
		  | patch (T.ExpFloat _) = ()
		  | patch (T.ExpBool _) = ()
		  | patch (T.ExpString _) = ()
		  | patch (T.ExpChar _) = ()
		  | patch T.ExpError = ()
		  | patch (T.ExpBaseTypecase _) = raise Fail "unexpected ExpBaseTypecase"
		  | patch (T.ExpPolyTypecase _) = raise Fail "unexpected ExpPolyTypecase"
		  | patch (T.ExpPureApply(_, e, _)) = patch e
		  | patch (T.ExpCmdApply(_, e, _)) = patch e
		  | patch (T.ExpVector(e, _, _)) = patch e
		and patchBind (T.Bind{tree=(_, e), ...}) = patch e
		in
		  patch body
		end
	  (* end case *))

    fun check (errStrm, env, PT.Program files) = let
	  fun chkFile (env, [], tops') = (tops', env)
	    | chkFile (env, PT.File{name, contents} :: files, tops') = let
		val (env, tops') = List.foldl (chkTop errStrm) (env, tops') contents
		in
		  chkFile (env, files, tops')
		end
	  val (tops, env) = chkFile (env, files, [])
	  in
	    (T.Program(List.rev tops), env)
	  end

    and chkTop errStrm ({span, tree}, (env, tops')) = let
	  fun mark (env, top) = (env, {span=span, tree=top}::tops')
	  fun chk (env, PT.TopFun(f, pat, SOME ty, e)) = let
		val (_, ty1 as Ty.Scheme(tvs1, _, _)) = chkFunTy (errStrm, env, ty)
	      (* to typecheck the body, we instantiate the type with fresh type variables *)
		val tyInBody as Ty.Scheme(tvs2, domTy2, rngTy2) = instantiate ty1
		val f' = Funct.new(f, tyInBody)
		val env' = E.insertFun(env, f')
		val (pat', env'') = chkPat (errStrm, span, env', domTy2, pat)
		val (e', bodyTy) = chkExp (errStrm, span, env'', e)
		fun chkTV (tv, Ty.TV{inst, ...}) = (case !inst
		       of Ty.UNIV => (inst := Ty.TY(Ty.TyVar tv); true)
			| Ty.TYPARAM => (inst := Ty.TY(Ty.TyVar tv); true)
			| Ty.TY(Ty.TyVar tv2) => chkTV(tv, tv2)
			| Ty.TY Ty.TyError => true (* ignore errors *)
			| _ => false
		      (* end case *))
		in
		  if U.unifyTy(bodyTy, rngTy2)
		    then ()
		    else err (errStrm, span, [
			S "return type of function ", A f, S " does not agree with body",
			S "\n  expected: ", TY rngTy2,
			S "\n  found:    ", TY bodyTy
		      ]);
		(* check that none of the tvs have been specialized *)
		  if ListPair.all chkTV (tvs1, tvs2)
		    then ()
		    else err (errStrm, span, [
			S "unclosed type for function ", A f, S " : ", TY domTy2, S " -> ", TY rngTy2
		      ]);
		(* patch the type of the function *)
		  Funct.updateTy (f', ty1);
		  mark (env', T.TopFun(f', pat', e'))
		end
	    | chk (env, PT.TopFun(f, pat, NONE, e)) = let
		val domTy = Ty.TyVar(newTyVar "_dom")
		val rngTy = Ty.TyVar(newTyVar "_rng")
		val f' = Funct.new(f, Ty.Scheme([], domTy, rngTy))
		val env' = E.insertFun (env, f')
		val (pat', env'') = chkPat (errStrm, span, env', domTy, pat)
		val (e', bodyTy) = chkExp (errStrm, span, env'', e)
		in
		  if U.unifyTy(bodyTy, rngTy)
		    then ()
		    else err (errStrm, span, [
			S "range and body types of ", A f, S " do not match\n",
			S "\n range type: ", TY rngTy,
			S "\n body type:  ", TY bodyTy
		      ]);
		(* patch the type of the function *)
		  Funct.updateTy (f', closeTy(domTy, rngTy));
		  patchFunct (f', e');
		  mark (env', T.TopFun(f', pat', e'))
		end
	    | chk (env, PT.TopPrimFun(f, pat, ty, e)) =
		mark (chkPrimFun (errStrm, span, env, f, pat, ty, e))
	    | chk (env, PT.TopData(dt, tydef)) = let
		val (tvs, ty) = chkTyDef (errStrm, env, tydef)
		val dt' = Dataty.new(dt, tvs, ty)
		in
		  mark (E.insertDataty(env, dt'), T.TopData dt')
		end
	    | chk (env, PT.TopBind(pat, e)) = let
		val (e', ty) = chkExp (errStrm, span, env, e)
		val (pat', env) = chkPat (errStrm, span, env, ty, pat)
		in
		  mark (env, T.TopBind(pat', e'))
		end
	    | chk (env, PT.TopExp e) =
		mark (env, T.TopExp(chkExp (errStrm, span, env, e)))
	  in
	    chk (env, tree)
	  end

  (* check the body of a primitive function.  expectedTy is the ascribed type of the function *)
    and chkPrimFun (errStrm, span, env, f, pat, ty, e) = let
	  val (env', fnTy as Ty.Scheme(tvs, domTy, rngTy)) = chkFunTy (errStrm, env, ty)
	(* check the type-variable argument to a typecase, returning its index in the list of
	 * type parameters.
	 *)
	  fun chkTyVar (construct, id) = let
		fun find (_, []) = (
		      err (errStrm, span, [
			  S construct, S " expects type variable, but found ", A id
			]);
		      ~1)
		  | find (i, tv::tvs) = if Atom.same(id, TyVar.nameOf tv)
		      then i
		      else find (i+1, tvs)
		in
		  find (0, tvs)
		end
	  val f' = Funct.new(f, fnTy)
	  val env' = E.insertFun(env', f')
	  val e' = (case e
		 of PT.ExpBaseTypecase(id, rules) => let
		      val tvIdx = chkTyVar("__base_typecase", id)
		      fun chkRule {span, tree=(lhs, rhs)} = let
			  (* check the lhs type *)
			    val lhs = (case E.findTyBind(env, lhs)
				   of SOME(E.Base(Ty.TyBase bTy)) => T.TyPatBase bTy
				    | _ =>
					if Atom.same(functionAtom, id) then T.TyPatFun
					else (
					  err (errStrm, span, [
					      S "expected type pattern, found ", A id
					    ]);
					  T.TyPatBase TypeBase.INT (* placeholder *))
				  (* end case *))
			    val lhsTy = (case lhs
				   of T.TyPatBase bty => Ty.TyBase bty
				    | T.TyPatFun => raise Fail "FIXME: lhsTy for function"
				  (* end case *))
			  (* create a fresh instantiation of the function's type for checking this clause
			   * of the typecase.
			   *)
			    val scheme as Ty.Scheme(tvs', domTy', rngTy') = instantiate fnTy
			    val tyParam = List.nth(tvs', tvIdx)
			    val _ = if Unify.unifyVarWithTy(tyParam, lhsTy)
				  then ()
				  else err (errStrm, span, [
				      S "lhs type ", TY lhsTy, S " is not in ",
				      C(TyVar.classOf tyParam)
				    ])
			    val (pat', env') = chkPat (errStrm, span, env', domTy', pat)
			    val (rhs', rhsTy) = chkExp (errStrm, span, env', rhs)
			    in
			      if Unify.unifyTy(rngTy', rhsTy)
				then ()
				else ();
			      (lhs, T.TyCaseRHS(pat', rhs', scheme))
			    end
		      in
			if (tvIdx >= 0)
			  then T.ExpBaseTypecase(List.nth(tvs, tvIdx), List.map chkRule rules)
			  else bogusExp
		      end
		  | PT.ExpPolyTypecase(id, e1, e2, optE3) => let
		      val tvIdx = chkTyVar("__poly_typecase", id)
		      in
			if (tvIdx >= 0)
			  then let
			    val clause1 = let (* non-sequence case *)
				  val scheme as Ty.Scheme(tvs', domTy', rngTy') = instantiate fnTy
				  val tyParam = List.nth(tvs', tvIdx)
				  val (pat', env') = chkPat (errStrm, span, env', domTy', pat)
				  val (e1, ty) = chkExp (errStrm, span, env', e1)
				  in
				    if Unify.unifyTy(ty, rngTy')
				      then ()
				      else err (errStrm, span, [
					    S "return type of primitive function ", A id, S " does not agree with body",
					    S "\n  expected: ", TY rngTy',
					    S "\n  found:    ", TY ty
					]);
				    T.TyCaseRHS(pat', e1, scheme)
				  end
			    val clause2 = let (* sequence type case *)
				  val scheme as Ty.Scheme(tvs', domTy', rngTy') = instantiate fnTy
				  val tyParam = List.nth(tvs', tvIdx)
				(* the tyParam has the shape "[ty]" *)
				  val _ = if Unify.unifyVarWithTy(tyParam,
					    Ty.TySeq(Ty.TyVar(TyVar.new (Atom.atom "_a", TB.ANY))))
					then ()
					else raise Fail "impossible"
				  val (pat', env') = chkPat (errStrm, span, env', domTy', pat)
				  val (e2, ty) = chkExp (errStrm, span, env', e2)
				  in
				    if Unify.unifyTy(ty, rngTy')
				      then ()
				      else err (errStrm, span, [
					    S "return type of primitive function ", A id, S " does not agree with body",
					    S "\n  expected: ", TY rngTy',
					    S "\n  found:    ", TY ty
					]);
				    T.TyCaseRHS(pat', e2, scheme)
				  end
			    val optClause3 = (case optE3
				   of SOME e3 => let (* pair-type *)
					val scheme as Ty.Scheme(tvs', domTy', rngTy') = instantiate fnTy
					val tyParam = List.nth(tvs', tvIdx)
				      (* the tyParam has the shape "(ty1, ty2)" *)
					val _ = if Unify.unifyVarWithTy(tyParam,
						  Ty.TyPair(
						    Ty.TyVar(TyVar.new (Atom.atom "_a", TB.ANY)),
						    Ty.TyVar(TyVar.new (Atom.atom "_b", TB.ANY))))
					      then ()
					      else raise Fail "impossible"
					val (pat', env') = chkPat (errStrm, span, env', domTy', pat)
					val (e3, ty) = chkExp (errStrm, span, env', e3)
					in
					  if Unify.unifyTy(ty, rngTy')
					    then ()
					    else err (errStrm, span, [
						  S "return type of primitive function ", A id, S " does not agree with body",
						  S "\n  expected: ", TY rngTy',
						  S "\n  found:    ", TY ty
					      ]);
					  SOME(T.TyCaseRHS(pat', e3, scheme))
					end
				    | NONE => NONE
				  (* end case *))
			    in
			      T.ExpPolyTypecase(List.nth(tvs, tvIdx), clause1, clause2, optClause3)
			    end
			  else bogusExp
		      end
		  | _ => raise Fail "expected typecase in primitive function body"
		(* end case *))
	  in
	  (* return the top-level environment extended with the function binding, plus the binding *)
	    (E.insertFun(env, f'), T.TopPrimFun(f', e'))
	  end (* chkPrimFun *)

    and chkTyDef (errStrm, env, PT.TyDef{span, tree=(ty, tvBinds)}) = let
	  val (tvs', env') = chkTVBinds (errStrm, env, tvBinds)
	  val ty' = chkTy (errStrm, span, env', ty)
	  in
	    (tvs', ty')
	  end

    and chkFunTy (errStrm, env, PT.TyFun{span, tree=(domTy, rngTy, tvBinds)}) = let
	  val (tvs', env') = chkTVBinds (errStrm, env, tvBinds)
	  val domTy' = chkTy (errStrm, span, env', domTy)
	  val rngTy' = chkTy (errStrm, span, env', rngTy)
	  in
	    (env', Ty.Scheme(tvs', domTy', rngTy'))
	  end

    and chkTVBinds (errStrm, env, tvBinds) = let
	  fun chkBind ({span, tree=(tv, cls)}, (env, tvs, binds)) =
		if Set.member(binds, tv)
		  then (
		    err (errStrm, span, [
			S "multiple occurrences of type variable ", A tv
		      ]);
		    (env, tvs, binds))
		  else let
		    val tv' = TyVar.new(tv, cls)
		    in
		      (E.insertTyvar(env, tv'), tv'::tvs, Set.add(binds, tv))
		    end
	  val (env, tvs, _) = List.foldl chkBind (env, [], Set.empty) tvBinds
	  in
	    (List.rev tvs, env)
	  end

    and chkTy (errStrm, span, env, ty) : NeslTypes.ty = (case ty
	   of PT.TyMark m => chkTy (unmark (errStrm, env, m))
	    | PT.TyId(id, []) => (case E.findTyBind(env, id)
		of SOME(E.Base ty) => ty
		 | SOME(E.Data dt) => if Dataty.arity dt = 0
		      then Ty.TyData(dt, [])
		      else (
			err (errStrm, span, [
			    S "missing type arguments for ", A(Dataty.nameOf dt)
			  ]);
			Ty.TyError)
		  | SOME(E.TyVar tv) => Ty.TyVar tv
		  | NONE => (
		      err (errStrm, span, [
			  S "undefined type ", A id
			]);
		      Ty.TyError)
		(* end case *))
	    | PT.TyId(id, args) => (case E.findTyBind(env, id)
		 of SOME(E.Base _) => raise Fail "impossible"
		  | SOME(E.Data dt) => let
		      val args' = List.map (fn ty => chkTy(errStrm, span, env, ty)) args
		      in
			if Dataty.arity dt = List.length args'
			  then Ty.TyData(dt, args')
			  else (
			    err (errStrm, span, [
				S "wrong number of arguments for type ", A id
			      ]);
			    Ty.TyError)
		      end
		  | SOME(E.TyVar tv) => (
		      err (errStrm, span, [
			  S "unexpected arguments to type variable ", A id
			]);
		      Ty.TyError)
		  | NONE => (
		      err (errStrm, span, [
			  S "undefined type ", A id
			]);
		      Ty.TyError)
		(* end case *))
	    | PT.TyPair(ty1, ty2) =>
		Ty.TyPair(chkTy(errStrm, span, env, ty1), chkTy(errStrm, span, env, ty2))
	    | PT.TySeq ty => Ty.TySeq(chkTy (errStrm, span, env, ty))
	    | PT.TyBaseSeq id => (case E.findTyBind(env, id)
		  of SOME(E.Base(Ty.TyBase bTy)) => if TB.isScalar bTy
			then Ty.TyBaseSeq(Ty.TyBase bTy)
			else (
			  err (errStrm, span, [S "expected scalar type"]);
			  Ty.TyError)
		   | SOME(E.TyVar tv) => Ty.TyBaseSeq(Ty.TyVar tv)
		   | _ => (
		      err (errStrm, span, [S "expected base type"]);
		      Ty.TyError)
		(* end case *))
	  (* end case *))

    and chkPat (errStrm, span, env, expectedTy, pat) : (T.pat * E.env) = let
	(* check for linearity *)
	  fun chkPatBinds (span, xs, pat) = (case pat
		 of PT.PatMark{span, tree} => chkPatBinds (span, xs, tree)
		  | PT.PatPair(pat1, pat2) => chkPatBinds (span, chkPatBinds (span, xs, pat1), pat2)
		  | PT.PatCons(_, pat) => chkPatBinds (span, xs, pat)
		  | PT.PatVector pat => chkPatBinds (span, xs, pat)
		  | PT.PatVar x => if Set.member (xs, x)
		      then (
			err (errStrm, span, [
			  S "multiple occurences of variable ", A x, S " in pattern"
			]);
			xs)
		      else Set.add (xs, x)
		  | PT.PatWild => xs
		(* end case *))
	(* typecheck the pattern *)
	  fun chk (span, env, pat) = (case pat
		 of PT.PatMark{span, tree} => chk (span, env, tree)
		  | PT.PatPair(pat1, pat2) => let
		      val (pat1', ty1, env) = chk (span, env, pat1)
		      val (pat2', ty2, env) = chk (span, env, pat2)
		      in
			(T.PatPair(pat1', pat2'), Ty.TyPair(ty1, ty2), env)
		      end
		  | PT.PatCons(con, pat) => (case E.findCons(env, con)
		       of SOME con' => let
			    val Ty.Scheme(tvs, ty1, ty2) = instantiate(Dataty.typeOf con')
			    val (pat', ty, env') = chk (span, env, pat)
			    in
			      if U.unifyTy(ty1, ty)
				then ()
				else err (errStrm, span, [
				    S "type mismatch in application of ", A con
				  ]);
			      (T.PatCons(con', List.map TyVar.resolve tvs, pat'), ty2, env')
			    end
			| NONE => (
			    err (errStrm, span, [
				S "undefined constructor ", A con
			      ]);
			    (bogusPat, Ty.TyError, env))
		      (* end case *))
		  | PT.PatVector pat => let
		      val vTy = Ty.TyVar(newTyVar "_v")
		      val dataTy = Ty.TyVar(newTyVar "_data")
		      val (pat', ty', env') = chk (span, env, pat)
		      val expectedTy = Ty.TyPair(Ty.tySegdes, dataTy)
		      in
			if Unify.unifyTy(ty', expectedTy)
			  then ()
			  else err (errStrm, span, [
			      S "type mismatch in __vector pattern",
			      S "\n  expected: ", TY expectedTy,
			      S "\n  but found: ", TY ty'
			    ]);
			(T.PatVector(pat', dataTy, vTy), vTy, env')
		      end
		  | PT.PatVar x => let
		      val ty = Ty.TyVar(newTyVar "_t")
		      val x' = Var.new(x, ty)
		      in
			(T.PatVar x', ty, E.insertVar(env, x'))
		      end
		  | PT.PatWild => let
		      val ty = Ty.TyVar(newTyVar "_t")
		      val x' = Var.new(wildAtom, ty)
		      in
			(T.PatVar x', ty, E.insertVar(env, x'))
		      end
		(* end case *))
	  val _ = chkPatBinds (span, Set.empty, pat)
	  val (pat', ty, env') = chk (span, env, pat)
	  in
	    if U.unifyTy(ty, expectedTy)
	      then ()
	      else err (errStrm, span, [
		  S "type mismatch in pattern",
		  S "\n  expected:  ", TY expectedTy,
		  S "\n  but found: ", TY ty
		]);
	    (pat', env')
	  end

    and chkExp (errStrm, span, env, exp) : (T.exp * Ty.ty) = (case exp
	   of PT.ExpMark m => let
		val (e', ty) = chkExp (unmark (errStrm, env, m))
		in
		  (T.ExpMark(remark(m, e')), ty)
		end
	    | PT.ExpPair(e1, e2) => let
		val (e1', ty1) = chkExp (errStrm, span, env, e1)
		val (e2', ty2) = chkExp (errStrm, span, env, e2)
		in
		  (T.ExpPair(e1', e2'), Ty.TyPair(ty1, ty2))
		end
	    | PT.ExpIf(e1, e2, e3) => let
		val (e1', ty1) = chkExp (errStrm, span, env, e1)
		val (e2', ty2) = chkExp (errStrm, span, env, e2)
		val (e3', ty3) = chkExp (errStrm, span, env, e3)
		in
		  if U.matchBaseTy (TB.BOOL, ty1)
		    then ()
		    else err (errStrm, span, [
			S "expected bool for conditional, but found ", TY ty1
		      ]);
		  if U.unifyTy(ty2, ty3)
		    then ()
		    else err (errStrm, span, [
			S "arms of conditional must have the same type\n",
			S "  then branch: ", TY ty2,
			S "\n  else branch: ", TY ty3
		      ]);
		  (T.ExpIf(e1', e2', e3'), ty2)
		end
	    | PT.ExpLet(binds, e) => let
		val (env, binds') = chkBinds (errStrm, env, binds)
		val (e', ty) = chkExp (errStrm, span, env, e)
		in
		  (T.ExpLet(binds', e'), ty)
		end
	    | PT.ExpApplyForEach(body, binds, optPred) => let
		val (env, binds') = chkRBinds (errStrm, env, binds)
		val (body', elemTy) = chkExp (errStrm, span, env, body)
		val optPred' = (case optPred
		       of SOME e => let
			    val (e', ty) = chkExp (errStrm, span, env, e)
			    in
			      (* does the predicate have bool type? *)
			      if U.matchBaseTy (TB.BOOL, ty)
				then ()
				else err (errStrm, span, [
				    S "expected type bool for predicate, but found ",
				    TY ty
				  ]);
			      SOME e'
			    end
			| NONE => NONE
		      (* end case *))
		val ty' = Ty.TySeq elemTy
		in
		  (T.ExpApplyForEach(body', binds', optPred', ty'), ty')
		end
	    | PT.ExpBinary(e1, rator, e2) => (case E.findVarBind (env, rator)
		 of SOME(E.Fun rator) => let
		      val Ty.Scheme(tvs, Ty.TyPair(argTy1, argTy2), resTy) = instantiate(Funct.typeOf rator)
		      val (e1', ty1) = chkExp (errStrm, span, env, e1)
		      val (e2', ty2) = chkExp (errStrm, span, env, e2)
		      in
			if U.unifyTy(argTy1, ty1) andalso U.unifyTy(argTy2, ty2)
			  then ()
			  else err (errStrm, span, [
			      S "type mismatch for binary operator \"", A(Funct.nameOf rator),
			      S "\"\n  expected: ", TYS[argTy1, argTy2],
			      S "\n  found:   ", TYS[ty1, ty2]
			  ]);
			(T.ExpApply(rator, ref(List.map TyVar.resolve tvs), T.ExpPair(e1', e2')), resTy)
		      end
		  | NONE => raise Fail(concat["undefined binary operator \"", Atom.toString rator, "\""])
		(* end case *))
	    | PT.ExpUnary(rator, e) => (case E.findVarBind (env, rator)
		 of SOME(E.Fun rator) => let
		      val Ty.Scheme(tvs, argTy, resTy) = instantiate(Funct.typeOf rator)
		      val (e', ty) = chkExp (errStrm, span, env, e)
		      in
			if U.unifyTy(argTy, ty)
			  then ()
			  else err (errStrm, span, [
			      S "type mismatch for unary operator \"", A(Funct.nameOf rator),
			      S "\"\n  expected: ", TY argTy,
			      S "\n  found:   ", TY ty
			  ]);
			(T.ExpApply(rator, ref(List.map TyVar.resolve tvs), e'), resTy)
		      end
		  | NONE => raise Fail(concat["undefined unary operator \"", Atom.toString rator, "\""])
		(* end case *))
	    | PT.ExpSubscript(e1, e2) => (case E.findVarBind (env, Atom.atom "elt")
		 of SOME(E.Fun rator) => let
		      val Ty.Scheme(tvs, Ty.TyPair(argTy1, argTy2), resTy) = instantiate(Funct.typeOf rator)
		      val (e1', ty1) = chkExp (errStrm, span, env, e1)
		      val (e2', ty2) = chkExp (errStrm, span, env, e2)
		      in
			if U.unifyTy(argTy1, ty1) andalso U.unifyTy(argTy2, ty2)
			  then ()
			  else err (errStrm, span, [
			      S "type mismatch for subscript"
			    ]);
			(T.ExpApply(rator, ref(List.map TyVar.resolve tvs), T.ExpPair(e1', e2')), resTy)
		      end
		  | NONE => (
		      err (errStrm, span, [S "subscript operator \"elt\" not defined"]);
		      (bogusExp, Ty.TyError))
		(* end case *))
	    | PT.ExpTime e => let
		val (e', ty) = chkExp (errStrm, span, env, e)
		in
		  (T.ExpTime(e', ty), Ty.TyPair(ty, Ty.tyFloat))
		end
	    | PT.ExpApply(f, e) => let
		val (e', ty) = chkExp (errStrm, span, env, e)
		in
		  case E.findVarBind (env, f)
		   of SOME(E.Fun f') => let
			val Ty.Scheme(tvs, argTy, resTy) = instantiate(Funct.typeOf f')
			in
			  if U.unifyTy(argTy, ty)
			    then ()
			    else err (errStrm, span, [
				S "type mismatch for application of function ", A f,
				S "\n  expected: ", TY argTy,
				S "\n  found:   ", TY ty
			      ]);
			  (T.ExpApply(f', ref(List.map TyVar.resolve tvs), e'), resTy)
			end
		    | SOME(E.Var f') => let
			val resTy = Ty.TyVar(newTyVar "_rng")
			in
			(* try to unify with the variable's type *)
			  if U.unifyTy(Ty.TyFun(ty, resTy), Var.typeOf f')
			    then (T.ExpApplyVar(f', e'), resTy)
			    else (
			      err (errStrm, span, [
				  S "type mismatch for ", A f, S " in application ",
				  S "\n  expected: ", TY(Ty.TyFun(ty, resTy)),
				  S "\n  found:   ", TY(Var.typeOf f')
				]);
			      (bogusExp, Ty.TyError))
			end
		    | SOME(E.Cons dt) => let
			val Ty.Scheme(tvs, argTy, resTy) = instantiate(Dataty.typeOf dt)
			in
			  if U.unifyTy(argTy, ty)
			    then ()
			    else err (errStrm, span, [
				S "type mismatch for application of constructor", A f,
				S "\n  expected: ", TY argTy,
				S "\n  found:   ", TY ty
			      ]);
			  (T.ExpCons(dt, List.map TyVar.resolve tvs, e'), resTy)
			end
		    | NONE => (
			err (errStrm, span, [
			    S "unbound function ", A f
			  ]);
			(bogusExp, Ty.TyError))
		  (* end case *)
		end
	    | PT.ExpSeqEmpty ty => let
		val ty' = Ty.TySeq(chkTy(errStrm, span, env, ty))
		in
		  (T.ExpSeq([], ty'), ty')
		end
	    | PT.ExpSeqRange(e1, e2, optStep) => let
		val (e1', ty1) = chkExp (errStrm, span, env, e1)
		val (e2', ty2) = chkExp (errStrm, span, env, e2)
		fun chkForInt ty = if U.matchBaseTy(TB.INT, ty)
		      then ()
		      else err (errStrm, span, [
			  S "expected type int for range, but found ", TY ty
			])
		val optStep' = (case optStep
		       of SOME e => let
			    val (e', ty) = chkExp (errStrm, span, env, e)
			    in
			      chkForInt ty;
			      e'
			    end
			| NONE => T.ExpInt 1
		      (* end case *))
		in
		  chkForInt ty1;
		  chkForInt ty2;
		  (T.ExpSeqRange(e1', e2', optStep'), Ty.TySeq Ty.tyInt)
		end
	    | PT.ExpSeq[] => raise Fail "impossible empty sequence"
	    | PT.ExpSeq(e::es) => let
		val (e', ty) = chkExp (errStrm, span, env, e)
		fun chk (e, es') = let
		      val (e', ty') = chkExp (errStrm, span, env, e)
		      in
			if U.unifyTy(ty, ty')
			  then ()
			  else err (errStrm, span, [
			      S "types of sequence elements must match; expected ",
			      TY ty, S " but found ", TY ty'
			    ]);
			e'::es'
		      end
		val es' = List.foldr chk [] es
		in
		  (T.ExpSeq(e'::es', ty), Ty.TySeq ty)
		end
	    | PT.ExpParen e => chkExp (errStrm, span, env, e)
	    | PT.ExpVar x => (case E.findVarBind (env, x)
		 of SOME(E.Fun _) => (
		      err (errStrm, span, [
			  S "unexpected use of function ", A x, S " in non-application context"
			]);
		      (bogusExp, Ty.TyError))
		  | SOME(E.Var x') => (T.ExpVar x', Var.typeOf x')
		  | SOME(E.Cons dt) => (
		      err (errStrm, span, [
			  S "unexpected use of constructor ", A x, S " in non-application context"
			]);
		      (bogusExp, Ty.TyError))
		  | NONE => (
		      err (errStrm, span, [
			  S "unbound variable ", A x
			]);
		      (bogusExp, Ty.TyError))
		(* end case *))
	    | PT.ExpInt n => (T.ExpInt n, Ty.tyInt)
	    | PT.ExpFloat f => (T.ExpFloat f, Ty.tyFloat)
	    | PT.ExpBool b => (T.ExpBool b, Ty.tyBool)
	    | PT.ExpString s => (T.ExpString s, Ty.tyString)
	    | PT.ExpChar c => (T.ExpChar c, Ty.tyChar)
	  (* primitive expressions *)
	    | PT.ExpBaseTypecase _ => raise Fail "unexpected base-typecase" (* handled in chkPrimBody *)
	    | PT.ExpPolyTypecase _ => raise Fail "unexpected poly-typecase" (* handled in chkPrimBody *)
	    | PT.ExpPrimApply(primOp, e) => let
		val (e', ty) = chkExp (errStrm, span, env, e)
		val resTy = Ty.TyVar(newTyVar "_primTy")
		val (primOp', domTy, rngTy) = CheckPrim.check primOp
		in
		  if Unify.unifyTy(domTy, ty)
		    then ()
		    else err (errStrm, span, [
			S "type mismatch for application of \"", S primOp, S "\"",
			S "\n  expected: ", TY domTy,
			S "\n  found:   ", TY ty
		      ]);
		  case primOp'
		   of CheckPrim.PURE pureOp => (T.ExpPureApply(pureOp, e', rngTy), rngTy)
		    | CheckPrim.CMD cmd => (T.ExpCmdApply(cmd, e', rngTy), rngTy)
		  (* end case *)
		end
	    | PT.ExpVector e => let
		val (e', ty) = chkExp (errStrm, span, env, e)
		val vTy = Ty.TySeq(Ty.TyVar(newTyVar "_v"))
		val dataTy = Ty.TyVar(newTyVar "_data")
		val expectedTy = Ty.TyPair(Ty.tySegdes, dataTy)
		in
		  if Unify.unifyTy(ty, expectedTy)
		    then ()
		    else err (errStrm, span, [
			S "type mismatch in __vector expression",
			S "\n  expected: ", TY expectedTy,
			S "\n  but found: ", TY ty
		      ]);
		(* note that we disconnect the type of a __vector expression and its argument,
		 * since the argument type depends on flattening.
		 *)
		  (T.ExpVector(e', dataTy, vTy), vTy)
		end
	  (* end case *))

  (* check let bindings *)
    and chkBinds (errStrm, env, binds) = let
	  fun chk (PT.Bind{span, tree=(pat, exp)}, (env, binds)) = let
		val (exp', ty) = chkExp (errStrm, span, env, exp)
		val (pat', env') = chkPat (errStrm, span, env, ty, pat)
		in
		  (env', T.Bind{span=span, tree=(pat', exp')} :: binds)
		end
	  val (env', binds) = List.foldl chk (env, []) binds
	  in
	    (env', List.rev binds)
	  end

  (* check iteration bindings *)
    and chkRBinds (errStrm, env, binds) = let
	  fun chk (PT.Bind{span, tree=(pat, exp)}, (env, binds)) = let
		val (exp', ty) = chkExp (errStrm, span, env, exp)
		val tv = newTyVar  "_t"
		val _ = if U.unifyTy(Ty.TySeq(Ty.TyVar tv), ty)
		      then ()
		      else err (errStrm, span, [
			  S "binding type mismatch"
			])
		val (pat', env') = chkPat (errStrm, span, env, TyVar.resolve tv, pat)
		in
		  (env', T.Bind{span=span, tree=(pat', exp')} :: binds)
		end
	  val (env, binds) = List.foldl chk (env, []) binds
	  in
	    (env, List.rev binds)
	  end

  end
