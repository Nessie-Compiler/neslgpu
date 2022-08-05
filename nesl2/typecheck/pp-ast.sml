(* pp-ast.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure PPAST : sig

    val output : TextIO.outstream * AST.program -> unit

  end = struct

    structure A = AST
    structure PP = TextIOPP

    val indent0 = PP.Abs 0
    val indent = PP.Abs 2
    val indent4 = PP.Abs 4

    fun ppList ppFn (left, sep, right) (ppStrm, list) = let
	  fun sp () = PP.space ppStrm 1
	  val string = PP.string ppStrm
	  fun pp [] = string right
	    | pp [x] = (ppFn x; string right)
	    | pp (x::xs) = (ppFn x; string sep; sp(); pp xs)
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

    fun ppAtom (ppStrm, a) = PP.string ppStrm (Atom.toString a)
    fun ppVar (ppStrm, x) = ppAtom(ppStrm, Var.nameOf x)
    fun ppFunct (ppStrm, f) = ppAtom(ppStrm, Funct.nameOf f)

    fun ppTy (ppStrm, ty) = PP.string ppStrm (NeslTypes.toString ty)
    fun ppFunTy (ppStrm, ty) = PP.string ppStrm (NeslTypes.schemeToString ty)

    fun ppVarBind (ppStrm, x) = (
	  PP.openHBox ppStrm;
	    ppAtom(ppStrm, Var.nameOf x);
	    PP.space ppStrm 1;
	    PP.string ppStrm ":";
	    PP.space ppStrm 1;
	    ppTy (ppStrm, Var.typeOf x);
	  PP.closeBox ppStrm)

    fun ppIdAtType (ppStrm, id, []) = PP.string ppStrm id
      | ppIdAtType (ppStrm, id, tys) = (
	  PP.openHBox ppStrm;
	    PP.string ppStrm id;
	    PP.string ppStrm "_";
	    ppList (fn ty => ppTy(ppStrm, ty)) ("<", ",", ">") (ppStrm, tys);
	  PP.closeBox ppStrm)

    fun ppPat (ppStrm, p) = let
	  fun sp () = PP.space ppStrm 1
	  val string = PP.string ppStrm
	  fun pp (A.PatMark{tree, ...}) = pp tree
	    | pp (p as A.PatPair _) = let
		fun flatten (A.PatPair(p1, p2)) = p1 :: flatten p2
		  | flatten p = [p]
		in
		  ppList pp ("(", ",", ")") (ppStrm, flatten p)
		end
	    | pp (A.PatCons(dc, tys, p)) = (
		PP.openHVBox ppStrm indent;
		  ppIdAtType (ppStrm, Atom.toString(Dataty.nameOf dc), tys);
		  sp(); ppParam(ppStrm, p);
		PP.closeBox ppStrm)
	    | pp (A.PatVector(p, ty1, ty2)) = (
		PP.openHVBox ppStrm indent;
		  string "__vector";
		  sp(); string "("; ppPat(ppStrm, p);
		  string ","; sp(); ppTy(ppStrm, ty1);
		  string ","; sp(); ppTy(ppStrm, ty2); string ")";
		PP.closeBox ppStrm)
	    | pp (A.PatVar x) = ppVarBind (ppStrm, x)
	    | pp A.PatError = PP.string ppStrm "*PatError*"
	  in
	    PP.openHBox ppStrm;
	      pp p;
	    PP.closeBox ppStrm
	  end

    and ppParam (ppStrm, A.PatMark{tree, ...}) = ppParam (ppStrm, tree)
      | ppParam (ppStrm, p as A.PatPair _) = ppPat (ppStrm, p)
      | ppParam (ppStrm, p) = (
	  PP.openHBox ppStrm;
	    PP.string ppStrm "(";
	    ppPat (ppStrm, p);
	    PP.string ppStrm ")";
	  PP.closeBox ppStrm)

    fun ppExp (ppStrm, e) = let
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  val string = PP.string ppStrm
	  fun ppTyPat (A.TyPatBase ty) = string(TypeBase.baseToString ty)
	    | ppTyPat (A.TyPatFun) = string "function"
	  fun pp (A.ExpMark{tree, ...}) = pp tree
	    | pp (e as A.ExpPair _) = let
		fun flatten (A.ExpPair(e1, e2)) = e1 :: flatten e2
		  | flatten e = [e]
		in
		  ppList pp ("(", ",", ")") (ppStrm, flatten e)
		end
	    | pp (A.ExpIf(e1, e2, e3)) = (
		PP.openHOVBox ppStrm indent;
		  PP.openHBox ppStrm;
		    string "if"; sp(); pp e1;
		  PP.closeBox ppStrm;
		  sp();
		  PP.openHBox ppStrm;
		    string "then"; sp(); pp e2;
		  PP.closeBox ppStrm;
		  sp();
		  PP.openHBox ppStrm;
		    string "else"; sp(); pp e3;
		  PP.closeBox ppStrm;
		PP.closeBox ppStrm)
	    | pp (A.ExpLet(bind::binds, e)) = (
		PP.openVBox ppStrm indent0;
		  PP.openVBox ppStrm indent4;
		    PP.openHBox ppStrm;
		      string "let"; sp(); ppBind bind;
		    PP.closeBox ppStrm;
		    List.app (fn b => (nl(); ppBind b)) binds;
		  PP.closeBox ppStrm;
		  nl();
		  PP.openHBox ppStrm;
		    string "in"; sp(); pp e;
		  PP.closeBox ppStrm;
		PP.closeBox ppStrm)
	    | pp (A.ExpApplyForEach(e, binds, optE, _)) = (
		PP.openHVBox ppStrm indent;
		  string "{";
		  pp e;
		  sp(); string ":"; sp();
		  ppList ppRBind ("", "; ", "") (ppStrm, binds);
		  case optE
		   of NONE => ()
		    | SOME e => (sp(); string "|"; sp(); pp e)
		  (* end case *);
		  string "}";
		PP.closeBox ppStrm)
	    | pp (A.ExpTime(e, _)) = (
		PP.openHVBox ppStrm indent;
		  string  "time"; sp(); ppArg e;
		PP.closeBox ppStrm)
	    | pp (A.ExpApply(f, tys, e)) = (
		PP.openHVBox ppStrm indent;
		  ppIdAtType (ppStrm, Atom.toString(Funct.nameOf f), !tys);
		  sp(); ppArg e;
		PP.closeBox ppStrm)
	    | pp (A.ExpApplyVar(f, e)) = (
		PP.openHVBox ppStrm indent;
		  ppVar (ppStrm, f); sp(); ppArg e;
		PP.closeBox ppStrm)
	    | pp (A.ExpCons(dc, tys, e)) = (
		PP.openHVBox ppStrm indent;
		  ppIdAtType (ppStrm, Atom.toString(Dataty.nameOf dc), tys);
		  sp(); ppArg e;
		PP.closeBox ppStrm)
	    | pp (A.ExpSeqRange(e1, e2, e3)) = (
		PP.openHBox ppStrm;
		  string "["; pp e1;
		  sp(); string ":"; sp(); pp e2;
		  sp(); string ":"; sp(); pp e3;
		  string "]";
		PP.closeBox ppStrm)
	    | pp (A.ExpSeq(es, _)) = ppList pp ("[", ",", "]") (ppStrm, es)
	    | pp (A.ExpVar x) = ppVar(ppStrm, x)
	    | pp (A.ExpInt n) = string(Format.format "%d" [Format.LINT n])
	    | pp (A.ExpFloat f) = string f
	    | pp (A.ExpBool b) = string(Bool.toString b)
	    | pp (A.ExpString s) = string(concat["\"", String.toCString s, "\""])
	    | pp (A.ExpChar c) = string("`" ^ String.str c)
	    | pp A.ExpError = string "*ExpError*"
	    | pp (A.ExpBaseTypecase(tv, rules)) = (
		PP.openHVBox ppStrm indent;
		  PP.openHBox ppStrm;
		    string "__base_typecase"; sp(); string(TyVar.toString tv);
		  PP.closeBox ppStrm;
		  List.app (fn (pat, rhs) => (
		      PP.openHBox ppStrm;
			nl(); string "|"; sp(); ppTyPat pat; string ":"; sp(); ppTyCaseRHS rhs;
		      PP.closeBox ppStrm))
		    rules;
		PP.closeBox ppStrm)
	    | pp (A.ExpPolyTypecase(tv, clause1, clause2, optClause3)) = (
		PP.openHOVBox ppStrm indent;
		  PP.openHBox ppStrm;
		    string "__poly_typecase"; sp(); string(TyVar.toString tv);
		  PP.closeBox ppStrm;
		  sp();
		  PP.openHBox ppStrm;
		    string "|"; sp(); ppTyCaseRHS clause1;
		  PP.closeBox ppStrm;
		  sp();
		  PP.openHBox ppStrm;
		    string "|"; sp(); ppTyCaseRHS clause2;
		  PP.closeBox ppStrm;
		  case optClause3
		   of SOME clause3 => (
			sp();
			PP.openHBox ppStrm;
			  string "|"; sp(); ppTyCaseRHS clause3;
			PP.closeBox ppStrm)
		    | NONE => ()
		  (* end case *);
		PP.closeBox ppStrm)
	    | pp (A.ExpPureApply(rator, arg, _)) = (
		PP.openHBox ppStrm;
		  string "__prim"; sp(); string(Pure.toString rator); sp();
		  string "("; pp arg; string ")";
		PP.closeBox ppStrm)
	    | pp (A.ExpCmdApply(cmd, arg, _)) = (
		PP.openHBox ppStrm;
		  string "__prim"; sp(); string(Cmd.toString cmd); sp();
		  string "("; pp arg; string ")";
		PP.closeBox ppStrm)
	    | pp (A.ExpVector(arg, ty1, ty2)) = (
		PP.openHBox ppStrm;
		  string "__vector";
		  sp(); string "("; pp arg;
		  string ","; sp(); ppTy(ppStrm, ty1);
		  string ","; sp(); ppTy(ppStrm, ty2); string ")";
		PP.closeBox ppStrm)
	  and ppArg e = (string "("; pp e; string ")")
	  and ppTyCaseRHS (A.TyCaseRHS(pat, rhs, scheme)) = (
		  PP.openHBox ppStrm;
		  ppParam (ppStrm, pat);
		  sp (); string "=>"; sp();
		  pp rhs;
		PP.closeBox ppStrm)
	  and ppBind (A.Bind{span, tree=(p, e)}) = (
		PP.openHBox ppStrm;
		  ppPat (ppStrm, p); sp(); string "="; sp(); ppExp (ppStrm, e);
		PP.closeBox ppStrm)
	  and ppRBind (A.Bind{span, tree=(p, e)}) = (
		PP.openHBox ppStrm;
		  ppPat (ppStrm, p); sp(); string "in"; sp(); ppExp (ppStrm, e);
		PP.closeBox ppStrm)
	  in
	    pp e
	  end

    fun ppTop ppStrm {span, tree} = let
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  val string = PP.string ppStrm
	  in
	    case tree
	     of A.TopFun(f, p, e) => let
		  val NeslTypes.Scheme(tyParams, domTy, rngTy) = Funct.typeOf f
		  in
		    PP.openVBox ppStrm indent;
		      PP.openHBox ppStrm;
			string "function"; sp(); ppFunct(ppStrm, f); sp();
			ppParam (ppStrm, p);
			sp (); string ":"; sp ();
			ppFunTy (ppStrm, Funct.typeOf f);
			sp (); string "=";
		      PP.closeBox ppStrm;
		      nl();
		      ppExp (ppStrm, e);
		    PP.closeBox ppStrm;
		    nl()
		  end
	      | A.TopPrimFun(f, e) => let
		  val NeslTypes.Scheme(tyParams, domTy, rngTy) = Funct.typeOf f
		  in
		    PP.openVBox ppStrm indent;
		      PP.openHBox ppStrm;
			string "__prim"; sp(); string "function"; sp(); ppFunct(ppStrm, f);
			sp (); string ":"; sp ();
			ppFunTy (ppStrm, Funct.typeOf f);
			sp (); string "=";
		      PP.closeBox ppStrm;
		      nl();
		      ppExp (ppStrm, e);
		    PP.closeBox ppStrm;
		    nl()
		  end
	      | A.TopData dt => let
		  val NeslTypes.Scheme(tyParams, domTy, _) = Dataty.typeOf dt
		  in
		    PP.openHBox ppStrm;
		      string "datatype"; sp(); string (Atom.toString(Dataty.nameOf dt));
		      sp(); ppTy (ppStrm, domTy);
(* FIXME: type parameters *)
		    PP.closeBox ppStrm;
		    nl()
		  end
	      | A.TopBind(p, e) => (
		  PP.openHBox ppStrm;
		    ppPat (ppStrm, p); sp(); string "="; sp(); ppExp (ppStrm, e);
		  PP.closeBox ppStrm;
		  nl())
	      | A.TopExp(e, ty) => (ppExp(ppStrm, e); nl())
	    (* end case *)
	  end

    fun output (outS, A.Program tree) = let
	  val ppStrm = PP.openOut {dst = outS, wid = 120}
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
	      PP.string ppStrm "%-- Program start --%"; PP.newline ppStrm;
	      List.app (ppTop ppStrm) tree;
	      PP.string ppStrm "%-- Program end --%"; PP.newline ppStrm;
	    PP.closeBox ppStrm;
	    PP.closeStream ppStrm
	  end

  end
