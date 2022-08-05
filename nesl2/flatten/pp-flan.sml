(* pp-flan.sml
 *
 * COPYRIGHT (c) 2013 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *
 * Pretty-printing for Flan IR
 *)

structure PPFlan : sig

    val output : TextIO.outstream * Flan.program * string -> unit

  end = struct

    structure F = Flan
    structure P = FlanPure
    structure PP = TextIOPP

    val indent0 = PP.Abs 0
    val indent2 = PP.Abs 2
    val indent4 = PP.Abs 4

    fun ppList ppFn (left, sep, right) (ppStrm, list) = let
	  fun sp () = PP.space ppStrm 1
	  val string = PP.string ppStrm
	  fun pp [] = string right
	    | pp [x] = (ppFn(ppStrm, x); string right)
	    | pp (x::xs) = (ppFn(ppStrm, x); string sep; sp(); pp xs)
	  in
	    string left; pp list
	  end

    fun ppList' ppFn (left, sep, right) (ppStrm, list) = let
	  fun sp () = PP.space ppStrm 1
	  val string = PP.string ppStrm
	  fun pp [] = (string left; string right)
	    | pp [x] = (ppFn x; string right)
	    | pp (x::xs) = (ppFn x; string sep; sp(); pp xs)
	  in
	    case list
	     of [x] => ppFn x
	      | _ => pp list
	    (* end case *)
	  end

    fun ppTy (ppStrm, ty) = PP.string ppStrm (FlanTypes.toString ty)

    fun ppVar (ppStrm, F.V{name, stamp, useCnt, ...}) =
	  PP.string ppStrm (Atom.toString name ^ Stamp.toString stamp)

    fun ppVarWithShape (ppStrm, v) = let
      val shapeStr = (case (Shapes.peekShape v)
		       of SOME shape => String.concat[" <Shape: ", Shapes.toString(shape), ">"]
			| NONE => ""
		     (* end case *))
    in
      PP.string ppStrm (FlanVar.toString v ^ shapeStr)
    end

    fun ppPureName (ppStrm, p) =
	PP.string ppStrm (String.concat["\"", (P.toString p), "\""])

    fun ppCmdName (ppStrm, c) =
	PP.string ppStrm (String.concat["\"", Cmd.toString c, "\""])

    fun ppFunctName (ppStrm, F.F{name, stamp, appCnt, ...}) =
	PP.string ppStrm (Atom.toString name ^ Stamp.toString stamp ^ "<" ^ Int.toString (!appCnt) ^ ">")

    fun ppVarBind (ppStrm, v as F.V{name, stamp, ty, useCnt, ...}) = let 
      val shapeStr = (case (Shapes.peekShape v)
		       of SOME shape => String.concat[" <Shape: ", Shapes.toString(shape), ">"]
			| NONE => ""
		     (* end case *))
    in
      PP.string ppStrm (String.concat[
			Atom.toString name, Stamp.toString stamp, " : ", FlanTypes.toString ty,
			"<",Int.toString (!useCnt), ">",
			shapeStr
		       ])
    end

    fun ppAtom (ppStrm, atom) = (case atom
	  of F.Var x => ppVar(ppStrm, x)
	   | F.Int i => PP.string ppStrm (Format.format "%d" [Format.LINT i])
	   | F.Float f => PP.string ppStrm f
	   | F.Bool b => PP.string ppStrm (Bool.toString b)
	   | F.String s => PP.string ppStrm (concat["\"", String.toCString s, "\""])
	   | F.Char c => PP.string ppStrm ("`" ^ String.str c)
	 (* end case *))

    fun ppAtoms (ppStrm, atoms) = ppList ppAtom ("(", ",", ")") (ppStrm, atoms)

    fun ppParam (ppStrm, ps) = (
	  PP.openHBox ppStrm;
            ppList ppVarWithShape ("(", "; ", ")") (ppStrm, ps);
	  PP.closeBox ppStrm)

    fun isBind exp = (case exp
	   of F.ExpForEach _ => false
	    | F.ExpLet _ => true
	    | F.ExpTuple _ => true
	    | F.ExpSeq _ => true
	    | F.ExpPure _ => false
	    | F.ExpCmd _ => false
	    | F.ExpApplyFun _ => false
	    | F.ExpIf _ => false
	    | F.ExpLifted _ => false
	    | F.ExpAtom _ => false
	 (* end case *))

    fun ppExp (ppStrm, e) = let
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  val string = PP.string ppStrm
	  fun ppBlk exp = let
		fun ppBody e = (
		    PP.openVBox ppStrm indent2;
		      PP.string ppStrm "in"; nl();
		      pp e;
		    PP.closeBox ppStrm);
		fun ppLet body ppFn = (
		      PP.openHBox ppStrm;
			string "let"; sp(); ppFn ();
		      PP.closeBox ppStrm;
		      nl(); ppBlk body)
		in
		  case exp
		   of F.ExpLet(bind::[], e) => ppLet e (fn () => (ppBind (ppStrm, bind)))
		    | F.ExpLet(bind::binds, e) =>
			ppLet (F.ExpLet(binds, e)) (fn () => (ppBind (ppStrm, bind)))
		    | F.ExpLet([], e) => raise Fail "this shouldn't happen"
		    | F.ExpTuple(v, atms, e) =>
			ppLet e (fn () => (
			  ppVarBind (ppStrm, v);
			  sp(); string "="; sp();
			  ppAtoms (ppStrm, atms)))
		    | F.ExpSeq (v, atms, e) =>
			ppLet e (fn () => (
			  ppVarBind (ppStrm, v);
			  sp(); string "="; sp();
			  ppList ppAtom ("[", ",", "]") (ppStrm, atms)))
		    | _ => ppBody exp
		  (* end case *)
		end
	  and pp (exp) = if isBind exp
		 then (
		   PP.openVBox ppStrm indent0;
		     ppBlk exp;
		   PP.closeBox ppStrm)
		 else (case exp
		    of F.ExpApplyFun(f, atms) => (
		       PP.openHBox ppStrm;
			 ppFunctName (ppStrm, f); sp();
			 ppAtoms (ppStrm, atms);
		       PP.closeBox ppStrm)
		     | F.ExpPure (p, atms) => (
			 PP.openHBox ppStrm;
			   ppPureName (ppStrm, p); sp();
			   ppAtoms (ppStrm, atms);
			 PP.closeBox ppStrm)
		     | F.ExpCmd (c, atms) => (
			 PP.openHBox ppStrm;
			   ppCmdName (ppStrm, c); sp();
			   ppAtoms (ppStrm, atms);
			 PP.closeBox ppStrm)
		     | F.ExpIf (a, e1, e2, t) => (
			 PP.openHOVBox ppStrm indent2;
			   PP.openHBox ppStrm;
			     string "if"; sp(); ppAtom (ppStrm, a);
			   PP.closeBox ppStrm;
			   sp();
			   PP.openHBox ppStrm;
			     string "then"; sp(); pp e1;
			   PP.closeBox ppStrm;
			   sp();
			   PP.openHBox ppStrm;
			     string "else"; sp(); pp e2;
			   PP.closeBox ppStrm;
			 PP.closeBox ppStrm)
		     | F.ExpLifted (e, binds) => pp (F.ExpForEach(e, binds))
		     | F.ExpAtom a => ppAtom (ppStrm, a)
		     | F.ExpForEach (e, binds) => let
			 fun ppBind (ppStrm, (x, xs)) = (
			       PP.openHBox ppStrm;
				 ppVarBind (ppStrm, x); sp(); string "in"; sp(); ppVar(ppStrm, xs);
			       PP.closeBox ppStrm)
			 in
			   PP.openHVBox ppStrm indent2;
			     string "{";
			     pp e;
			     sp(); string ":"; sp();
			     ppList ppBind("", ";", "") (ppStrm, binds);
			     string "}";
			   PP.closeBox ppStrm
			 end
		     | _ => raise Fail "impossible"
		  (* end case *))
	  in
	    pp e
	  end

    and ppBind (ppStrm, (v, e)) = let
	  fun sp () = PP.space ppStrm 1
	  val string = PP.string ppStrm
	  in
	    PP.openHBox ppStrm;
	      ppVarBind (ppStrm, v);
	      sp(); string "="; sp();
	      PP.openBox ppStrm indent2;
		ppExp (ppStrm, e);
	      PP.closeBox ppStrm;
	    PP.closeBox ppStrm
	  end
    fun ppFunTy (ppStrm, (domTy, rngTy)) =
	PP.string ppStrm (String.concat[
	(FlanTypes.tyListToString domTy), " -> ", (FlanTypes.toString rngTy)])

    fun ppFunct (ppStrm, f) = let
	val (id, args, exp) = f
	fun sp () = PP.space ppStrm 1
	fun nl () = PP.newline ppStrm
	val string = PP.string ppStrm
	val F.F{name=_, stamp=_, ty=t, appCnt=appCnt, ...} = id
        in
	  (PP.openVBox ppStrm indent2;
	    PP.openHBox ppStrm;
	      string "function"; sp(); ppFunctName(ppStrm, id); sp();
	      ppParam (ppStrm, args);
	      sp(); string ":"; sp (); sp();
	      ppFunTy (ppStrm, !t);
	      sp(); string "<appCnt: ";
	      string (Int.toString (!appCnt)); string "> ";
	      string "=";
	    PP.closeBox ppStrm;
	    nl();
	    ppExp (ppStrm, exp);
	  PP.closeBox ppStrm)
        end

    fun ppTop ppStrm top = let
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  val string = PP.string ppStrm
	  in
	    case top
	     of F.TopFun(_, []) => ()
	      | F.TopFun(_, instances) => let
		  fun pp f = (ppFunct (ppStrm, f); nl())
		in
		  List.app pp instances
		end
	      | F.TopBind b => (ppBind (ppStrm, b); nl())
	      | F.TopExp (e, t, p) => (ppExp (ppStrm, e); nl())
	     (* end case *)
	  end

    fun output (outS, F.Program prog, msg) = let
	  val ppStrm = PP.openOut {dst = outS, wid = 120}
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
              PP.string ppStrm (concat["%-- Flattened Program start ", msg, "--%"]); PP.newline ppStrm;
	      List.app (ppTop ppStrm) prog;
	      PP.string ppStrm (concat["%-- Flattened Program end ", msg, "--%"]); PP.newline ppStrm;
	    PP.closeBox ppStrm;
	    PP.closeStream ppStrm
          end

end
