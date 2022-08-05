(* check-mono.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Typechecker for monomorphic representation.
 *)

structure CheckMono : sig

  (* check a monomorphic program for type correctness; returns true if there were errors *)
    val check : Monomorphize.mono_env * MonoAST.program -> bool

  end = struct

    structure M = MonoAST
    structure MTy = MonoTy

    val anyErrors = ref false

    fun patToString pat = (case pat
	   of M.PatPair(p1, p2) => concat["(", patToString p1, ",", patToString p2, ")"]
	    | M.PatVector(p1, p2, _) => concat["__vector(", patToString p1, ",", patToString p2, ")"]
	    | M.PatVar x => MonoVar.toString x
	  (* end case *))

    datatype token
      = S of string | A of Atom.atom
      | V of M.var | P of M.pat | TY of MTy.ty

    fun error toks = let
	  fun tok2str (S s) = s
	    | tok2str (A a) = Atom.toString a
	    | tok2str (V x) = MonoVar.toString x
	    | tok2str (P pat) = patToString pat
	    | tok2str (TY ty) = MTy.toString ty
	  in
	    anyErrors := true;
	    Log.msg (String.concat("** " :: List.map tok2str toks))
	  end

    fun check (mEnv, M.Program prog) = let
	  fun chkTop top = (case top
	       of M.TopFun(f, instances) => let
		      fun chk (f, pat, exp) = let
			    val (domTy, rngTy) = MonoFunct.typeOf f
			    val paramTy = chkPat pat
			    val resultTy = chkExp exp
			    in
			      if not(MTy.same(domTy, paramTy))
				then error [
				    S "type mismatch in function ", A(MonoFunct.nameOf f), S ":",
				    S "\n  expected domain type: ", TY domTy,
				    S "\n  found parameter type: ", TY paramTy, S "\n"
				  ]
				else ();
			      if not(MTy.same(rngTy, resultTy))
				then error [
				    S "type mismatch in function ", A(MonoFunct.nameOf f), S ":",
				    S "\n  expected range type: ", TY rngTy,
				    S "\n  found result type:   ", TY resultTy, S "\n"
				  ]
				else ()
			    end
		    in
		      if not(Monomorphize.inEnv(mEnv, f))
			then error [
			    S "function ", A(Funct.nameOf f), S " is missing from environment\n"
			  ]
			else ();
		      List.app chk instances
		    end
		| M.TopBind(pat, exp) => chkLetBind(pat, exp)
		| M.TopExp(exp, ty) => let
		    val ty' = chkExp exp
		    in
		      if not(MTy.same(ty, ty'))
			then error [
			    S "type mismatch in top-level expression;",
			    S "\n  expected: ", TY ty,
			    S "\n  found: ", TY ty', S "\n"
			  ]
			else ()
		  end
	      (* end case *))

	  and chkPat pat = (case pat
	       of M.PatPair(p1, p2) => MTy.tyPair(chkPat p1, chkPat p2)
		| M.PatVector(_, _, ty) => ty
		| M.PatVar(M.V{ty, ...}) => ty
	      (* end case *))

	  and chkExp exp = (case exp
	       of M.ExpPair(e1, e2) => MTy.tyPair(chkExp e1, chkExp e2)
		| M.ExpIf(e1, e2, e3) => let
		    val ty1 = chkExp e1
		    val ty2 = chkExp e2
		    val ty3 = chkExp e3
		    in
		      if not(MTy.same(ty1, MTy.TyBase TypeBase.BOOL))
			then error[S "expected bool for 'if', but found ", TY ty1, S "\n"]
			else ();
		      if not(MTy.same(ty2, ty3))
			then error[
			    S "type mismatch in branches of 'if':",
			    S "\n  then branch: ", TY ty2,
			    S "\n  else branch: ", TY ty3, S "\n"
			  ]
			else ();
		      ty2
		    end
		| M.ExpLet(binds, exp) => (
		    List.app chkLetBind binds;
		    chkExp exp)
		| M.ExpApplyForEach(e, binds, ty) => let
		    val bodyTy = MTy.tySeq(chkExp e)
		    in
		      List.app chkForBind binds;
		      if not(MTy.same(ty, bodyTy))
			then error[
			    S "type mismatch in foreach body:",
			    S "\n  expected type: ", TY ty,
			    S "\n  found type:    ", TY bodyTy, S "\n"
			  ]
			else ();
		      bodyTy
		    end
		| M.ExpApply(f, arg) => let
		    val (domTy, rngTy) = MonoFunct.typeOf f
		    val argTy = chkExp arg
		    in
		      if not(MTy.same(domTy, argTy))
			then error[
			    S "type mismatch in application of ", A(MonoFunct.nameOf f), S ":",
			    S "\n  expected type: ", TY domTy,
			    S "\n  found type:    ", TY argTy, S "\n"
			  ]
			else ();
		      rngTy
		    end
		| M.ExpSeqRange(e1, e2, e3) => let
		    fun chkForInt (_, MTy.TyBase TypeBase.INT) = ()
		      | chkForInt (msg, ty) = error [
			    S "expected int type for ", S msg, S ", but found ", TY ty, S "\n"
			  ]
		    in
		      chkForInt ("range lower bound", chkExp e1);
		      chkForInt ("range upper bound", chkExp e1);
		      chkForInt ("range step", chkExp e1);
		      MTy.tySeq(MTy.TyBase TypeBase.INT)
		    end
		| M.ExpSeq(exps, ty) => let
		    fun chk e = let
			  val ty' = chkExp e
			  in
			    if not(MTy.same(chkExp e, ty))
			      then error[
				  S "type mismatch in sequence :",
				  S "\n  expected type: ", TY ty,
				  S "\n  found type:    ", TY ty', S "\n"
				]
			      else ()
			  end
		    in
		      List.app chk exps;
		      MTy.tySeq ty
		    end
		| M.ExpVar(M.V{ty, ...}) => ty
		| M.ExpInt _ => MTy.tyInt
		| M.ExpFloat _ => MTy.tyFloat
		| M.ExpBool _ => MTy.tyBool
		| M.ExpString _ => MTy.tyString
		| M.ExpChar _ => MTy.tyChar
		| M.ExpPureApply(primOp, e, ty) => let
		    val argTy = chkExp e
		    val (domTy, _) = MonoPrimTy.typeOfPure primOp
		    in
		      if not(MTy.same(domTy, argTy))
			then error[
			    S "type mismatch in application of ", S(Pure.toString primOp), S ":",
			    S "\n  expected type: ", TY domTy,
			    S "\n  found type:    ", TY argTy, S "\n"
			  ]
			else ();
		      ty
		    end
		| M.ExpCmdApply(cmd, e, ty) => let
		    val argTy = chkExp e
		    val (domTy, _) = MonoPrimTy.typeOfCmd cmd
		    in
		      if not(MTy.same(domTy, argTy))
			then error[
			    S "type mismatch in application of ", S(Cmd.toString cmd), S ":",
			    S "\n  expected type: ", TY domTy,
			    S "\n  found type:    ", TY argTy, S "\n"
			  ]
			else ();
		      ty
		    end
		| M.ExpVector(e1, e2, ty) => (ignore(chkExp e1); ignore(chkExp e2); ty)
	      (* end case *))

	  and chkLetBind (pat, exp) = let
		val lhsTy = chkPat pat
		val rhsTy = chkExp exp
		in
		  if not(MTy.same(lhsTy, rhsTy))
		    then error[
			S "type mismatch in let ", P pat, S " = ...",
			S "\n  lhs type: ", TY lhsTy,
			S "\n  rhs type: ", TY rhsTy, S "\n"
		      ]
		    else ()
		end

	  and chkForBind (pat, exp) = let
		val lhsTy = chkPat pat
		val rhsTy = chkExp exp
		in
		  if not(MTy.same(MTy.tySeq lhsTy, rhsTy))
		    then error[
			S "type mismatch in foreach ", P pat, S " in ...",
			S "\n  element type:  ", TY lhsTy,
			S "\n  sequence type: ", TY rhsTy, S "\n"
		      ]
		    else ()
		end
	  in
	    anyErrors := false;
	    List.app chkTop prog;
	    !anyErrors
	  end
  end
