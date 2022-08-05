(* monomorphize.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Monomorphize :> sig

    type mono_env

    val transform : NeslBasis.basis * AST.program -> mono_env * MonoAST.program

    type instance = (Funct.funct * (MonoAST.funct * MonoAST.pat * MonoAST.exp))

  (* given a AST function applied to the given list of monomorphic types, this function returns
   * the monomorphic version of the function, along with any additional top-level function bindings
   * that are required.
   *)
    val instantiate : mono_env -> Funct.funct * MonoTy.ty list -> MonoAST.funct * instance list

  (* for debugging: is a function defined in the environment? *)
    val inEnv : mono_env * Funct.funct -> bool

  end = struct

    structure Ty = NeslTypes
    structure A = AST
    structure T = MonoAST
    structure MF = MonoFunct
    structure MTy = MonoTy
    structure MV = MonoVar
    structure FEnv = FunEnvRep
    structure Util = MonoUtil

  (* set to tru to generate dubugging output from this phase *)
    val debug = true

    fun print l = if debug then TextIO.print(concat l) else ()

    fun deepInsert (Ty.TyVar(Ty.TV{inst=ref(Ty.TY ty), ...}), monoTy, m) =
	  deepInsert (ty, monoTy, m)
      | deepInsert (Ty.TyVar tv, monoTy, m) = TyVar.Map.insert(m, tv, monoTy)
      | deepInsert (Ty.TyBase _, MTy.TyBase _, m) = m
      | deepInsert (Ty.TySeq ty, MTy.TySeq monoTy, m) = deepInsert (ty, monoTy, m)
      | deepInsert (Ty.TyBaseSeq ty, MTy.TyBaseSeq tb, m) =
	  deepInsert (ty, MTy.TyBase tb, m)
      | deepInsert (Ty.TyBaseSeq ty, MTy.TySeq(MTy.TyBase tb), m) =
	  deepInsert (ty, MTy.TyBase tb, m)
      | deepInsert (Ty.TyPair(ty1, ty2), MTy.TyTuple[monoTy1, monoTy2], m) =
	  deepInsert (ty2, monoTy2, deepInsert (ty1, monoTy1, m))
      | deepInsert (ty, monoTy, _) =
	  raise Fail(concat["type application mismatch: ", Ty.toString ty, " <> ", MTy.toString monoTy])

  (* create a type variable to mono-type mapping; when used in a typecase, the type
   * variables in the domain may have been partially specialized, so we need to do a
   * deep mapping.
   *)
    fun mkTVMap (tvs, monoTys : MTy.ty list) = let
	  fun bind (Ty.TV{inst=ref(Ty.TY ty), ...}, monoTy, m) =
		deepInsert(ty, monoTy, m)
	    | bind (tv, monoTy, m) = TyVar.Map.insert(m, tv, monoTy)
	  in
	    ListPair.foldl bind TyVar.Map.empty (tvs, monoTys)
	  end
handle ex => raise ex

(*DEBUG*)
    fun tvmapToString tvMap = String.concat[
	    "{",
	    String.concatWith ", "
	      (List.map (fn (tv, ty) => concat[TyVar.toString tv, " :> ", MTy.toString ty])
		(TyVar.Map.listItemsi tvMap)),
	    "}"
	  ]
(*DEBUG*)

  (* The __vector pattern/constructor acts as a "firewall" between the surface language sequence
   * types and the segmented representation used by the primitive operators.  This function takes
   * a type-variable map, the specialized outer type of a __vector, and the unspecialized
   * inner type of the vector.  It returns the type-variable map augmented to specialize the
   * inner type.  Specifically, if we have
   *
   *	__vector (s, dv : innerTy) : outerTy
   *
   * with the outerTy = [ty], then innerTy = {{ ty }}, where
   *
   *    {{ b }}		==> [: b :]
   *	{{ t1, t2 }}	==> < {{ t1 }}, {{ t2 }} >
   *	{{ [ t ] }}	==> [ t ]
   *	{{ [: t :] }}	==> [: t :]
   *)
    fun mkVectorMap (tvMap, innerTy, outerTy) = let
	  fun flatten (MTy.TyBase tb) =  MTy.TyBaseSeq tb
	    | flatten (MTy.TyTuple[ty1, ty2]) = MTy.TyTuple[flatten ty1, flatten ty2]
	    | flatten ty = ty
	  fun match (Ty.TyVar tv, outerTy) = (case outerTy
		 of MTy.TySeq ty => TyVar.Map.insert(tvMap, tv, flatten ty)
		  | _ => raise Fail("unexpected outer type for __vector: " ^ MTy.toString outerTy)
		(* end case *))
	    | match (Ty.TyBaseSeq ty, MTy.TySeq eTy) = deepInsert (ty, eTy, tvMap)
	    | match (Ty.TyPair(ty1, ty2), MTy.TySeq(MTy.TyTuple[ty1', ty2'])) =
		deepInsert (ty2, flatten ty2', deepInsert(ty1, flatten ty1', tvMap))
	    | match (Ty.TySeq eTy1, MTy.TySeq(MTy.TySeq eTy2)) = deepInsert (eTy1, eTy2, tvMap)
	    | match (innerTy, outerTy) =
		raise Fail(concat["__vector type mismatch: ", Ty.toString innerTy, " <> ", MTy.toString outerTy])
	  in
(*
print["mkVectorMap: (", tvmapToString tvMap, ", ", Ty.toString innerTy, ", ", MTy.toString outerTy, ")\n"];
*)
	    case innerTy
	     of Ty.TyVar tv => match (TyVar.resolve tv, outerTy)
	      | ty => match (ty, outerTy)
	    (* end case *)
	  end
handle ex => raise ex

    type instance = (Funct.funct * FEnv.instance)

  (* a mono_env encapsulates the information that we need to generate monomorphic instances
   * of functions during the flattening phase.
   *)
    datatype mono_env = MEnv of {
	basis : NeslBasis.basis,
	fEnv : FEnv.env,
	vMap : T.var Var.Map.map
      }

  (* for debugging: is a function defined in the environment? *)
    fun inEnv (MEnv{fEnv, ...}, f) = isSome(FEnv.find(fEnv, f))

  (* given a type-variable map, convert an AST type to a MonoAST type *)
    fun specializeTy tvMap = let
	  fun cvt (Ty.TyVar(tv as Ty.TV{inst, ...})) = (case TyVar.Map.find(tvMap, tv)
		 of SOME monoTy => monoTy
		  | NONE => (case !inst
		       of Ty.TY ty => cvt ty
			| _ => raise Fail("undefined type variable " ^ TyVar.toString tv)
		      (* end case *))
		(* end case *))
	    | cvt (Ty.TyError) = raise Fail "unexpected TyError"
	    | cvt Ty.TyVoid = raise Fail "TyVoid unimplemented"
	    | cvt (Ty.TyBase tb) = MTy.TyBase tb
	    | cvt (Ty.TySeq ty) = MTy.TySeq(cvt ty)
	    | cvt (Ty.TyBaseSeq ty) = (case cvt ty
		 of MTy.TyBase tb => MTy.TyBaseSeq tb
		  | _ => raise Fail("expected base type, but found " ^ Ty.toString ty)
		(* end case *))
	    | cvt (Ty.TyPair(ty1, ty2)) = MTy.tyPair(cvt ty1, cvt ty2)
	    | cvt (Ty.TyData(dt, tys)) = specializeDT (dt, List.map cvt tys)
	    | cvt (Ty.TyFun(ty1, ty2)) = raise Fail "unimplemented" (* FIXME *)
	  in
	    fn ty => cvt ty handle ex => (
TextIO.print(concat["specializeTy ", tvmapToString tvMap, " ", Ty.toString ty, "\n"]); raise ex)
	  end

    and specializeDT (dt, monoTys) = let
	  val Ty.Scheme(tvs, repTy, _) = Dataty.typeOf dt
	  val tvMap = mkTVMap (tvs, monoTys)
	  in
	    specializeTy tvMap repTy
	  end

    fun codeSpecialize (basis : NeslBasis.basis, env, allInstances) = let
	  fun findFunct f = FEnv.find(env, f)
	  fun cvtVar (tvMap, vMap, x) = let
		val x' = MV.new(Var.nameOf x, specializeTy tvMap (Var.typeOf x))
		in
		  (x', Var.Map.insert(vMap, x, x'))
		end
	  fun cvtPat (tvMap, vMap, A.PatMark{span, tree}) = cvtPat(tvMap, vMap, tree)
	    | cvtPat (tvMap, vMap, A.PatPair(pat1, pat2)) = let
		val (pat1', vMap) = cvtPat (tvMap, vMap, pat1)
		val (pat2', vMap) = cvtPat (tvMap, vMap, pat2)
		in
		  (T.PatPair(pat1', pat2'), vMap)
		end
	    | cvtPat (tvMap, vMap, A.PatCons(dt, tys, pat)) = let
		val tys' = List.map (specializeTy tvMap) tys
		val dt' = specializeDT(dt, tys')
		val (pat', vMap) = cvtPat (tvMap, vMap, pat)
		in
		  (pat', vMap)
		end
	    | cvtPat (tvMap, vMap, A.PatVector(pat, ty1, ty2)) = let
	      (* we use the specialized outer type of the pattern to extend the type-variable map
	       * used to monomorphize the subpattern.
	       *)
		val ty2 = specializeTy tvMap ty2
		val tvMap = mkVectorMap(tvMap, ty1, ty2)
		in
		  case cvtPat (tvMap, vMap, pat)
		   of (T.PatPair(pat1', pat2'), vMap) =>
			(T.PatVector(pat1', pat2', ty2), vMap)
		    | _ => raise Fail "impossible"
		  (* end case *)
		end
	    | cvtPat (tvMap, vMap, A.PatVar x) = let
		val (x', vMap) = cvtVar (tvMap, vMap, x)
		in
		  (T.PatVar x', vMap)
		end
	    | cvtPat (_, _, A.PatError) = raise Fail "unexpected PatError"
	  fun cvtExp (tvMap, vMap, A.ExpMark{span, tree}) = cvtExp(tvMap, vMap, tree)
	    | cvtExp (tvMap, vMap, A.ExpPair(e1, e2)) =
		T.ExpPair(cvtExp (tvMap, vMap, e1), cvtExp (tvMap, vMap, e2))
	    | cvtExp (tvMap, vMap, A.ExpIf(e1, e2, e3)) =
		T.ExpIf(
		  cvtExp (tvMap, vMap, e1),
		  cvtExp (tvMap, vMap, e2),
		  cvtExp (tvMap, vMap, e3))
	    | cvtExp (tvMap, vMap, A.ExpLet(binds, exp)) = let
		val (vMap', binds') = cvtBinds (tvMap, vMap, binds)
		in
		  T.ExpLet(binds', cvtExp(tvMap, vMap', exp))
		end
	    | cvtExp (tvMap, vMap, A.ExpApplyForEach(exp, binds, SOME pred, ty)) = let
	      (* as per 4.1.1 in Keller's PhD dissertation, convert
	       *
	       *    { e : x in xs | b }
	       *
	       * to
	       *
	       *    let t = xs
	       *        fs = { b : x in t }
	       *        ys = old_pack(t, fs)
	       *    in
	       *       { e[y/x] : y in ys }
	       *)
		val (vMap', binds') = cvtBinds (tvMap, vMap, binds)
		val pred' = cvtExp(tvMap, vMap', pred)
		val exp' = cvtExp(tvMap, vMap', exp)
		val fs = MV.new (Atom.atom "flags", MTy.TySeq MTy.tyBool)
	      (* simplify a binding "p in e", by creating a temporary t let bound to e
	       * (if e is not a value) and add renaming info to the renameMap
	       *)
		fun simplifyBind ((pat, exp), (letBinds, packs, filterBinds, binds, renameMap)) = let
		      val bindSeqTy as MTy.TySeq bindElemTy = Util.typeOf exp
		      val (letBinds, rhs) = if Util.isValue exp
			    then (letBinds, exp)
			    else let
			      val t = MV.new(Atom.atom "t", bindSeqTy)
			      in
				((T.PatVar t, exp)::letBinds, T.ExpVar t)
			      end
		      val pack = specializeFunct (vMap, #old_pack basis, [bindElemTy])
		      val ys = MV.new (Atom.atom "ys", bindSeqTy)
		      val packs = (T.PatVar ys, T.ExpApply(pack, T.ExpPair(rhs, T.ExpVar fs))) :: packs
		      val filterBinds = (pat, rhs) :: filterBinds
		      val (pat', renameMap) = Util.copyPat (renameMap, pat)
		      val binds = (pat', T.ExpVar ys) :: binds
		      in
			(letBinds, packs, filterBinds, binds, renameMap)
		      end (* simplifyBind *)
		val (letBinds, packs, filterBinds, foreachBinds, renameMap) =
		      List.foldl simplifyBind ([], [], [], [], MV.Map.empty) binds'
		in
		  T.ExpLet(
		    List.revAppend(letBinds,
		      (T.PatVar fs, T.ExpApplyForEach(pred', filterBinds, MTy.TySeq MTy.tyBool)) :: List.rev packs),
		    T.ExpApplyForEach(Util.copyExp(renameMap, exp'), foreachBinds, specializeTy tvMap ty))
		end
	    | cvtExp (tvMap, vMap, A.ExpApplyForEach(exp, binds, NONE, ty)) = let
		val (vMap', binds') = cvtBinds (tvMap, vMap, binds)
		val exp' = cvtExp(tvMap, vMap', exp)
		in
		  T.ExpApplyForEach(exp', binds', specializeTy tvMap ty)
		end
	    | cvtExp (tvMap, vMap, A.ExpTime(e, ty)) = let
	      (* expand out "time" operation *)
		val startTimer = specializeFunct (vMap, #start_timer basis, [])
		val stopTimer = specializeFunct (vMap, #stop_timer basis, [])
		val t = MV.new(Atom.atom "t", MTy.tyInt)
		val result = MV.new(Atom.atom "result", specializeTy tvMap ty)
		in
		  T.ExpLet([
		      (T.PatVar t, T.ExpApply(startTimer, T.ExpInt 0)),
		      (T.PatVar result, cvtExp (tvMap, vMap, e))
		    ],
		    T.ExpPair(T.ExpVar result, T.ExpApply(stopTimer, T.ExpVar t)))
		end
	    | cvtExp (tvMap, vMap, A.ExpApply(f, tys, arg)) = let
		val tys' = List.map (specializeTy tvMap) (!tys)
		val f' = specializeFunct (vMap, f, tys')
		in
		  T.ExpApply(f', cvtExp (tvMap, vMap, arg))
		end
	    | cvtExp (tvMap, vMap, A.ExpApplyVar(f, arg)) =
(* should specialize the enclosing function to each function argument! *)
		raise Fail "unimplemented"
	    | cvtExp (tvMap, vMap, A.ExpCons(dt, tys, e)) = cvtExp(tvMap, vMap, e)
	    | cvtExp (tvMap, vMap, A.ExpSeqRange(e1, e2, e3)) =
		T.ExpSeqRange(
		  cvtExp (tvMap, vMap, e1),
		  cvtExp (tvMap, vMap, e2),
		  cvtExp (tvMap, vMap, e3))
	    | cvtExp (tvMap, vMap, A.ExpSeq(exps, ty)) =
		T.ExpSeq(
		  cvtExps (tvMap, vMap, exps),
		  specializeTy tvMap ty)
	    | cvtExp (_, vMap, A.ExpVar x) = (case Var.Map.find (vMap, x)
		 of SOME x' => T.ExpVar x'
		  | NONE => raise Fail("unbound variable " ^ Atom.toString(Var.nameOf x))
		(* end case *))
	    | cvtExp (_, _, A.ExpInt n) = T.ExpInt n
	    | cvtExp (_, _, A.ExpFloat f) = T.ExpFloat f
	    | cvtExp (_, _, A.ExpBool b) = T.ExpBool b
	    | cvtExp (_, _, A.ExpString s) = T.ExpString s
	    | cvtExp (_, _, A.ExpChar c) = T.ExpChar c
	    | cvtExp (_, _, A.ExpError) = raise Fail "unexpected ExpError"
	  (* primitive operations *)
	    | cvtExp (_, _, A.ExpBaseTypecase _) = raise Fail "unexpected ExpBaseTypecase"
	    | cvtExp (_, _, A.ExpPolyTypecase _) = raise Fail "unexpected ExpPolyTypecase"
	    | cvtExp (tvMap, vMap, A.ExpPureApply(primOp, e, ty)) =
		T.ExpPureApply(primOp, cvtExp (tvMap, vMap, e), specializeTy tvMap ty)
	    | cvtExp (tvMap, vMap, A.ExpCmdApply(cmd, e, ty)) =
		T.ExpCmdApply(cmd, cvtExp (tvMap, vMap, e), specializeTy tvMap ty)
	    | cvtExp (tvMap, vMap, A.ExpVector(e, ty1, ty2)) = let
(* FIXME: for __vector expressions, we need to use type info from the body to determine the type of the result *)
	      (* we use the specialized outer type of the expression to extend the type-variable map
	       * used to monomorphize the subexpression.
	       *)
		val ty2 = specializeTy tvMap ty2
		val tvMap = mkVectorMap(tvMap, ty1, ty2)
		in
		  case cvtExp (tvMap, vMap, e)
		   of T.ExpPair(exp1', exp2') => T.ExpVector(exp1', exp2', ty2)
		    | exp' => let
			val ty1 = specializeTy tvMap ty1
			val v = MV.new(Atom.atom "vp", MTy.tyPair(MTy.tySegdes, ty1))
			in
			  T.ExpLet([(T.PatVar v, exp')],
			    T.ExpVector(
			      T.ExpPureApply(Pure.PROJ 1, T.ExpVar v, MTy.tySegdes),
			      T.ExpPureApply(Pure.PROJ 2, T.ExpVar v, ty1),
			      ty2))
			end
		  (* end case *)
		end
	  and cvtExps (tvMap, vMap, exps) = let
		val exps' = List.foldl (fn (e, es') => cvtExp(tvMap, vMap, e)::es') [] exps
		in
		  List.rev exps'
		end
	  and cvtBinds (tvMap, vMap, binds) = let
		fun cvt (vMap, [], binds') = (vMap, List.rev binds')
		  | cvt (vMap, A.Bind{span, tree=(p, e)}::binds, binds') = let
		      val (p', vMap') = cvtPat (tvMap, vMap, p)
		      val e' = cvtExp (tvMap, vMap, e)
		      in
			cvt (vMap', binds, (p', e')::binds')
		      end
		in
		  cvt (vMap, binds, [])
		end
	(* specialize an application instance of a function or primop.  The parameters
	 * are:
	 *    vMap	-- the mapping from AST variables to MonoAST variables.
	 *    f		-- the polymorphic function being specialized
	 *    monoTys	-- the MonoAST version of the type arguments to f
	 *)
	  and specializeFunct (vMap, f, monoTys) = (case findFunct f
		 of NONE => raise Fail("unbound function " ^ Atom.toString(Funct.nameOf f))
		  | SOME(FEnv.FunDef(pat, exp), instances) => let
		      fun match (T.F{inst=(_, tys'), ...}, _, _) = MTy.sameTys(monoTys, tys')
		      in
			case List.find match (!instances)
			 of SOME(f', _, _) => f'
			  | NONE => let (* create a new monomorphic instance *)
			      val Ty.Scheme(tvs, domTy, rngTy) = Funct.typeOf f
(*
val _ = print["specialize ", Atom.toString(Funct.nameOf f), " @ <",
String.concatWith ", " (ListPair.map (fn (tv, ty) => concat[TyVar.toString tv, " :> ", MTy.toString ty]) (tvs, monoTys)),
">\n"]
*)
			      val tvMap = mkTVMap (tvs, monoTys)
			      val domTy' = specializeTy tvMap domTy
			      val rngTy' = specializeTy tvMap rngTy
			      val (pat', vMap') = cvtPat (tvMap, vMap, pat)
			      val f' = MF.new(Funct.nameOf f, (domTy', rngTy'), (f, monoTys))
			    (* add a dummy entry to instances so that we handle recursive
			     * functions correctly.
			     *)
			      val saveInstances = !instances
			      val _ = (instances := (f', pat', T.ExpInt 0) :: saveInstances)
			    (* monomorphize the body *)
			      val exp' = cvtExp (tvMap, vMap', exp)
(*
val _ = print["specialize ", Atom.toString(Funct.nameOf f), " as ", MonoFunct.toString f', " done\n"]
*)
			      val instance = (f', pat', exp')
			      in
			      (* record the new instance in both the list of instances for f and
			       * the global list of all instances.
			       *)
				instances := instance :: saveInstances;
				allInstances := (f, instance) :: !allInstances;
				f'
			      end
			(* end case *)
		      end
		  | SOME(FEnv.PrimDef pexp, instances) => let
		      fun match (T.F{inst=(_, tys'), ...}, _, _) = MTy.sameTys(monoTys, tys')
		      in
(*
print["specialize ", Atom.toString(Funct.nameOf f), " @ <",
String.concatWith ", " (List.map MTy.toString monoTys), ">\n"];
*)
			case List.find match (!instances)
			 of SOME(f', _, _) => f'
			  | NONE => let
			    (* monomorphize the RHS of a typecase rule *)
			      fun specializeRule (A.TyCaseRHS(pat, e, Ty.Scheme(tvs, domTy, rngTy))) = let
				    val tvMap = mkTVMap (tvs, monoTys)
				    val domTy' = specializeTy tvMap domTy
				    val rngTy' = specializeTy tvMap rngTy
				    val (pat', vMap') = cvtPat (tvMap, vMap, pat)
				    val f' = MF.new(Funct.nameOf f, (domTy', rngTy'), (f, monoTys))
				    in
				      (f', pat', cvtExp (tvMap, vMap', e))
				    end
			    (* find the type argument that corresponds to the given type parameter *)
			      fun findTyArg tyVar = let
				    val Ty.Scheme(tvs, _, _) = Funct.typeOf f
				    fun find (tv::tvs, ty::tys) =
					  if TyVar.same(tv, tyVar) then SOME ty else find(tvs, tys)
				      | find _ = NONE
				    in
				      find (tvs, monoTys)
				    end
			    (* monomorphize the body *)
			      val instance = (case pexp
				     of A.ExpBaseTypecase(tyVar, rules) => (
					  case findTyArg tyVar
					   of SOME tyArg => let
						fun match (tyPat, _) = (case (tyArg, tyPat)
						       of (MTy.TyBase tyb, A.TyPatBase tyb') => (tyb = tyb')
							| (MTy.TyFun _, A.TyPatFun) => true
							| _ => false
						      (* end case *))
						in
						  case List.find match rules
						   of SOME(_, rhs) => specializeRule rhs
						    | NONE => raise Fail(concat[
							  "base-typecase match failure in ",
							  Funct.toString f, "; type arg = ",
							  MTy.toString tyArg
							])
						  (* end case *)
						end
					    | _ => raise Fail "base-typecase unbound type variable"
					  (* end case *))
				      | A.ExpPolyTypecase(tyVar, e1, e2, optRHS3) => (
					(* cases are: scalar, sequence, pair(optional) *)
					  case (findTyArg tyVar, optRHS3)
					   of (NONE, _) => raise Fail "poly-typecase unbound type variable"
					    | (SOME(MTy.TyTuple _), SOME rhs) => specializeRule rhs
					    | (SOME(MTy.TySeq _), _) => specializeRule e2
					    | (SOME ty, _) => specializeRule e1
					  (* end case *))
				      | _ => raise Fail "expected typecase in prim function body"
				    (* end case *))
			      in
(*
print["specialize ", Atom.toString(Funct.nameOf f), " done\n"];
*)
			      (* record the new instance in both the list of instances for f and
			       * the global list of all instances.
			       *)
				instances := instance :: !instances;
				allInstances := (f, instance) :: !allInstances;
				#1 instance
			      end
			(* end case *)
		      end
		(* end case *))
	  and specializeBind (vMap, pat, exp) = let
		val (pat', vMap') = cvtPat (TyVar.Map.empty, vMap, pat)
		val exp' = cvtExp (TyVar.Map.empty, vMap, exp)
		in
		  (vMap', pat', exp')
		end
	  in {
	    specializeFunct = specializeFunct,
	    specializeBind = specializeBind,
	    specializeExp = fn (vMap, e) => cvtExp(TyVar.Map.empty, vMap, e)
	  } end

    fun revMapAppend cons = let
	  fun rma ([], l) = l
	    | rma (x::xs, l) = rma(xs, cons x :: l)
	  in
	    rma
	  end

    fun transform (basis, A.Program topDefs) = let
	  val env = FEnv.new ()
	  fun insertFunct (f, pat, exp) = FEnv.insert env (f, FEnv.FunDef(pat, exp))
	  fun insertPrim (f, pexp) = FEnv.insert env (f, FEnv.PrimDef pexp)
	  val {specializeFunct,  specializeBind, specializeExp} = codeSpecialize (basis, env, ref [])
	  fun doTop (vMap, []) = (vMap, [])
	    | doTop (vMap, {span, tree}::rest) = (case tree
		 of A.TopFun(f, pat, exp) => let
		      val r = insertFunct (f, pat, exp)
		      val (vMap, rest') = doTop (vMap, rest)
		      in
			(vMap, T.TopFun(f, List.rev(!r)) :: rest')
		      end
		  | A.TopPrimFun(f, primExp) => let
		      val r = insertPrim (f, primExp)
		      val (vMap, rest') = doTop (vMap, rest)
		      in
			(vMap, T.TopFun(f, List.rev(!r)) :: rest')
		      end
		  | A.TopData dt => doTop (vMap, rest)
		  | A.TopBind(p, e) => let
		      val (vMap, p', e') = specializeBind (vMap, p, e)
		      val (vMap', rest') = doTop (vMap, rest)
		      in
			(vMap', T.TopBind(p', e') :: rest')
		      end
		  | A.TopExp(e, ty) => let
		      val e' = specializeExp (vMap, e)
		      val ty' = specializeTy TyVar.Map.empty ty
		      val (vMap', rest') = doTop (vMap, rest)
		      in
			(vMap', T.TopExp(e', ty') :: rest')
		      end
		(* end case *))
	  val (vMap, prog) = doTop (Var.Map.empty, topDefs)
	  in
	    (MEnv{basis=basis, fEnv=env, vMap=vMap}, T.Program prog)
	  end

  (* given a AST function applied to the given list of monomorphic types, this function returns
   * the monomorphic version of the function, along with any additional top-level function bindings
   * that are required.
   *)
    fun instantiate (MEnv{basis, fEnv, vMap}) = let
	  val allInstances = ref[]
	  val {specializeFunct, ...} = codeSpecialize (basis, fEnv, allInstances)
	  fun instantiate' (f, monoTys) = (
		allInstances := [];
		(specializeFunct (vMap, f, monoTys), !allInstances))
	  in
	    instantiate'
	  end

  end
