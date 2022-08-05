(* check-flan.sml
 *
 * COPYRIGHT (c) 2014 Nora Sandler
 * All rights reserved.
 *
 * Typechecker for flattened representation.
 *)

structure CheckFlan : sig

  (* check a Flan program for type correctness; return true if there were errors *)
    val check : Flan.program -> bool

  end = struct

    structure F = Flan
    structure FTy = FlanTypes
    structure U = FlanUtil

    val anyErrors = ref false

    datatype token = S of string | A of Atom.atom | F of F.funct
		   | V of F.var | TY of FTy.ty | TYS of FTy.ty list

    fun tyTuple [t] = t
      | tyTuple ts = FTy.TyTuple ts

    fun error toks = let
      fun tok2str (S s) = s
	| tok2str (A a) = Atom.toString a
	| tok2str (F f) = let
	    val F.F{name, stamp, ...} = f
	  in
	    (Atom.toString name ^ Stamp.toString stamp)
	  end
	| tok2str (V x) = let
	    val F.V{name, stamp, ...} = x
	  in
	    (Atom.toString name ^ Stamp.toString stamp)
	  end
	| tok2str (TY ty) = FTy.toString ty
	| tok2str (TYS tys) = tok2str (TY (FTy.TyTuple tys))
    in
      anyErrors := true;
      Log.msg (String.concat("** " :: List.map tok2str toks))
    end

    fun check (F.Program prog) = (
	anyErrors := false;
	List.app chkTop prog;
	!anyErrors)

    and chkTop top = (case top
	 of F.TopFun(funct, insts) => let
	      fun chkFun (f, args, exp) = let
		val F.F{ty=ref(domTy, rngTy), ...} = f
		val paramTy = chkArgs args
		val resultTy = chkExp exp
	      in
		if not(ListPair.all FTy.sameTy (domTy, paramTy))
		then error [
		     S "type mismatch in function ", F f, S ":",
		     S "\n expected domain type: ", TYS domTy,
		     S "\n found parameter type: ", TYS paramTy, S "\n"
		     ]
		else ();
		if not(FTy.sameTy(rngTy, resultTy))
		then error [
		     S "type mismatch in function ", F f, S ":",
		     S "\n expected range type: ", TY rngTy,
		     S "\n found result type:   ", TY resultTy, S "\n"
		     ]
		else ()
	      end
	    in
	      List.app chkFun insts
	    end
	  | F.TopBind (vars, exp) => chkLetBind(vars, exp)
	  | F.TopExp(exp, ty, p) => let
	      val ty' = chkExp exp
	    in
	      if not(FTy.sameTy(ty, ty'))
	      then error [
		   S "type mismatch in top-level expression;",
		   S "\n expected: ", TY ty,
		   S "\n found:    ", TY ty', S "\n"
		   ]
	      else ()
	    end
        (* end case *))

    and chkArgs vs = map FlanVar.typeOf vs
(*
    and chkAtms (atm::[]) = U.atomTy atm
      | chkAtms atms = FTy.TyTuple(map U.atomTy atms)
*)

    and chkAtms atms = map U.atomTy atms

    and chkExp exp = (case exp
	 of F.ExpForEach(e, _) => (
	    error [S "unflattened forEach found\n"];
	    chkExp e)
	  | F.ExpLet (binds, e) => (
	    List.app chkLetBind binds;
	    chkExp e)
	  | F.ExpTuple (v, atms, e) => let
	      val varTy = FlanVar.typeOf v
	      val tupleTy = FTy.TyTuple (chkAtms atms)
	    in
	      if not(FTy.sameTy(varTy, tupleTy))
	      then error [S "type mismatch in tuple binding for var ", V v, S ":",
			  S "\n expected type: ", TY varTy,
			  S "\n found type:    ", TY tupleTy, S "\n"]
	      else ();
	      chkExp e
	    end
	  | F.ExpSeq (v, atms, e) => let
	      val vTy = FlanVar.typeOf v
	      val seqTy = (case vTy
			    of FTy.TyTuple[FTy.TyBase TypeBase.SEGDES, t] => FTy.tyFlat vTy
			     | t => (error[
				     S "expected variable of sequence type",
				     S "\n found type: ", TY t
				     ]; t)
	                  (* end case *))
	      (* Check that an atom has same type as first atom in sequence *)
	      fun tyMismatch a = let
		val aTy = U.atomTy a
	      in
		if not(FTy.sameTy(aTy, seqTy))
		then error[
		     S "type mismatch in ", V v, S " = [ ... ]:",
		     S "\n expected type: ", TY seqTy,
		     S "\n found type:    ", TY aTy, S "\n"
		     ]
		else ()
	      end
	    in
	      List.app tyMismatch atms;
	      chkExp e
	    end
	  | F.ExpPure (FlanPure.Base (Pure.PROJ i), arg) => let
	      val argTys = (case (chkAtms arg)
			     of [FTy.TyTuple ts] => ts
			      | ts => (error[
				      S "type mismatch in application of PROJ:",
				      S "\n expected tuple type, ",
				      S "\n found type: ", TYS ts, S "\n"
				      ];
				      ts)
			   (* end case *))
	    in
	      List.nth(argTys, (i - 1))
	    end
	  | F.ExpPure (FlanPure.Lifted (Pure.PROJ i), args) => let
	      val elemTy = (case (chkAtms args)
			     of [FTy.TyTuple [FTy.TyBase TypeBase.SEGDES, t]] => t
			      | ts => (error[
				     S "type mismatch in application of PROJ^:",
				     S "\n expected sequence type, ",
				     S "\n found types: ", TYS ts, S "\n"
				     ];
				      hd ts)
			   (* end case *))
	      val FTy.TyTuple nestedArgs = elemTy
	    in
	      FTy.TyTuple[FTy.tySegdes, List.nth(nestedArgs, (i - 1))]
	    end
	  | F.ExpPure (p, args) => let
	      val argTy = chkAtms args
	      val (domTy, rngTy) = FlanPure.getTy p
	    in
	      if not (FTy.sameTys(domTy, argTy))
	      then error[
		   S "type mismatch in application of prim ", S (FlanPure.toString p), S ":",
		   S "\n expected type: ", TYS domTy,
		   S "\n found type:    ", TYS argTy, S "\n"
		   ]
	      else ();
	      rngTy
	    end
	  | F.ExpCmd (c, args) => let
	      val argTy = chkAtms args
	      val (domTy, rngTy) = FlanPrimTy.typeOfCmd c
	    in
	      if not (FTy.sameTys(domTy, argTy))
	      then error[
		   S "type mismatch in application of cmd ", S (Cmd.toString c), S ":",
		   S "\n expected type: ", TYS domTy,
		   S "\n found type:    ", TYS argTy, S "\n"
		   ]
	      else ();
	      tyTuple rngTy
	    end
	  | F.ExpIf (b, e1, e2, t) => let
	      val predTy = U.atomTy b
	      val ty1 = chkExp e1
	      val ty2 = chkExp e2
	    in
	      if not(FTy.sameTy(predTy, FTy.TyBase TypeBase.BOOL))
	      then error [S "expected bool for 'if', but found ",
			  TY predTy, S "\n"]
	      else ();
	      if not(FTy.sameTy(ty1, ty2))
	      then error [S "type mismatch in branches of 'if':",
			  S "\n then branch: ", TY ty1,
			  S "\n else branch: ", TY ty2, S "\n"
			 ]

	      else ();
	      ty1
	    end
	  | F.ExpApplyFun (f, atms) => let
	      val F.F{ty=ref(domTy, rngTy), ...} = f
	      val atmTys = map U.atomTy atms
	    in
	      if not(ListPair.all FTy.sameTy (domTy, atmTys))
	      then error[
		   S "type mismatch in application of ", F f, S ":",
		   S "\n expected type: ", TYS domTy,
		   S "\n found type:    ", TYS atmTys, S "\n"
		   ]
	      else ();
	      rngTy
	    end
	  | F.ExpLifted (e, binds) => (
	    List.app chkForBind binds;
	    FTy.tySeq (chkExp e))
	  | F.ExpAtom a => U.atomTy a
        (* end case *))

    and chkForBind (v, vs) = let
      val varTy = FlanVar.typeOf v
      val seqTy = FlanVar.typeOf vs
    in
      if not (FTy.sameTy(FTy.tySeq varTy, seqTy))
      then error [
	   S "type mismatch in foreach bind ", V v, S " :",
	   S "\n element type:  ", TY varTy,
	   S "\n sequence type: ", TY seqTy, S "\n"
	   ]
      else ()
    end

    and chkLetBind (var, exp) = let
      val lhsTy = chkBinds var
      val rhsTy = chkExp exp
    in
      if not(FTy.sameTy(lhsTy, rhsTy))
      then error (List.concat[
		  [S "type mismatch in let "],
		  [V var],
		  [S " = ...",
		   S "\n lhs type: ", TY lhsTy,
		   S "\n rhs type: ", TY rhsTy, S "\n"
		 ]])
      else ()
    end

    and chkBinds v = FlanVar.typeOf v

end
