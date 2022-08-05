(* util.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure MonoUtil : sig

    type rename_map = MonoVar.var MonoVar.Map.map

  (* is an expression a value (i.e., constant or variable)? *)
    val isValue : MonoAST.exp -> bool

  (* copy a pattern while extending the rename_map with new bindings *)
    val copyPat : rename_map * MonoAST.pat -> MonoAST.pat * rename_map

  (* copy a monomorphic expression with renaming.  Free variables are renamed using
   * the supplied environment, while bound variables are replaced with fresh copies.
   *)
    val copyExp : rename_map * MonoAST.exp -> MonoAST.exp

  (* return the type of an monomorphized expression *)
    val typeOf : MonoAST.exp -> MonoTy.ty

  end = struct

    structure M = MonoAST
    structure MTy = MonoTy
    structure MV = MonoVar
    structure VMap = MV.Map

    type rename_map = MV.var VMap.map

  (* is an expression a value (i.e., constant or variable)? *)
    fun isValue (M.ExpVar _) = true
      | isValue (M.ExpInt _) = true
      | isValue (M.ExpFloat _) = true
      | isValue (M.ExpBool _) = true
      | isValue (M.ExpString _) = true
      | isValue (M.ExpChar _) = true
      | isValue (M.ExpSeqRange(e1, e2, e3)) = (isValue e1) andalso (isValue e2) andalso (isValue e3)
      | isValue _ = false

    fun rename env x = (case VMap.find(env, x)
	   of SOME x' => x'
	    | NONE => x
	  (* end casde *))

    fun copyPat (env, pat) = (case pat
	   of M.PatPair(p1, p2) => let
		val (p1, env) = copyPat (env, p1)
		val (p2, env) = copyPat (env, p2)
		in
		  (M.PatPair(p1, p2), env)
		end
	    | M.PatVector(p1, p2, ty) => let
		val (p1, env) = copyPat (env, p1)
		val (p2, env) = copyPat (env, p2)
		in
		  (M.PatVector(p1, p2, ty), env)
		end
	    | M.PatVar x => let
		val x' = MV.copy x
		in
		  (M.PatVar x', VMap.insert(env, x, x'))
		end
	  (* end case *))

    fun copyExp (env, exp) = let
	  fun copyE (env, exp) = (case exp
		 of M.ExpPair(e1, e2) => M.ExpPair(copyE (env, e1), copyE (env, e2))
		  | M.ExpIf(e1, e2, e3) => M.ExpIf(copyE (env, e1), copyE (env, e2), copyE (env, e3))
		  | M.ExpLet(binds, e) => let
		      val (binds, env) = copyBinds (env, binds)
		      in
			M.ExpLet(binds, copyE(env, e))
		      end
		  | M.ExpApplyForEach(e, binds, ty) => let
		      val (binds, env) = copyBinds (env, binds)
		      in
			M.ExpApplyForEach(copyE(env, e), binds, ty)
		      end
		  | M.ExpApply(f, e) => M.ExpApply(f, copyE (env, e))
		  | M.ExpSeqRange(e1, e2, e3) =>
		      M.ExpSeqRange(copyE (env, e1), copyE (env, e2), copyE (env, e3))
		  | M.ExpSeq(es, ty) => M.ExpSeq(List.map (fn e => copyE(env, e)) es, ty)
		  | M.ExpVar x => M.ExpVar(rename env x)
		  | M.ExpInt _ => exp
		  | M.ExpFloat _ => exp
		  | M.ExpBool _ => exp
		  | M.ExpString _ => exp
		  | M.ExpChar _ => exp
		  | M.ExpPureApply(pure, e, ty) => M.ExpPureApply(pure, copyE (env, e), ty)
		  | M.ExpCmdApply(cmd, e, ty) => M.ExpCmdApply(cmd, copyE (env, e), ty)
		  | M.ExpVector(e1, e2, ty) => M.ExpVector(copyE (env, e1), copyE (env, e2), ty)
		(* end case *))
	  and copyBinds (env0, binds) = let
		fun copy ((p, e), (binds, env)) = let
		      val (p, env') = copyPat (env, p)
		      in
			((p, copyE(env, e))::binds, env')
		      end
		val (binds, env) = List.foldl copy ([], env0) binds
		in
		  (List.rev binds, env)
		end
	  in
	    copyE (env, exp)
	  end

    fun typeOf exp = (case exp
	   of M.ExpPair(e1, e2) => MTy.tyPair(typeOf e1, typeOf e2)
	    | M.ExpIf(_, e2, _) => typeOf e2
	    | M.ExpLet(_, e) => typeOf e
	    | M.ExpApplyForEach(_, _, ty) => ty
	    | M.ExpApply(f, _) => #2 (MonoFunct.typeOf f)
	    | M.ExpSeqRange _ => MTy.tySeq MTy.tyInt
	    | M.ExpSeq(_, ty) => ty
	    | M.ExpVar x => MV.typeOf x
	    | M.ExpInt _ => MTy.tyInt
	    | M.ExpFloat _ => MTy.tyFloat
	    | M.ExpBool _ => MTy.tyBool
	    | M.ExpString _ => MTy.tyString
	    | M.ExpChar _ => MTy.tyChar
	    | M.ExpPureApply(_, _, ty) => ty
	    | M.ExpCmdApply(_, _, ty) => ty
	    | M.ExpVector(_, _, ty) => ty
	  (* end case *))

  end
