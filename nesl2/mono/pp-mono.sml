(* pp-mono.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure PPMono : sig

    val output : TextIO.outstream * MonoAST.program -> unit

  end = struct

    structure A = MonoAST
    structure PP = TextIOPP
    structure MTy = MonoTy

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

    fun ppPolyTy (ppStrm, ty) = PP.string ppStrm (NeslTypes.toString ty)

    fun ppTy (ppStrm, ty) = PP.string ppStrm (MTy.toString ty)

    fun ppIdAtType (ppStrm, id, tys) = (
	  PP.openHBox ppStrm;
	    PP.string ppStrm id;
	    case tys
	     of [] => ()
	      | _ => (
		  PP.string ppStrm "_";
		  ppList (fn ty => ppTy(ppStrm, ty)) ("<", ",", ">") (ppStrm, tys))
	    (* end case *);
	  PP.closeBox ppStrm)

    fun ppVar (ppStrm, A.V{name, stamp, ...}) =
	  PP.string ppStrm (Atom.toString name ^ Stamp.toString stamp)
    fun ppFunct (ppStrm, f as A.F{name, inst=(_, tys), ...}) =
	  ppIdAtType (ppStrm, MonoFunct.toString f, tys)

    fun ppVarBind (ppStrm, A.V{name, stamp, ty, ...}) =
	  PP.string ppStrm (String.concat[
	      Atom.toString name, Stamp.toString stamp, " : ", MTy.toString ty
	    ])

    fun ppPat (ppStrm, p) = let
	  fun sp () = PP.space ppStrm 1
	  val string = PP.string ppStrm
	  fun pp (A.PatPair(p1 as A.PatPair _, p2)) = (
		PP.openHBox ppStrm;
		  string "("; pp p1; string "),";
		PP.closeBox ppStrm;
		PP.cut ppStrm; pp p2)
	    | pp (A.PatPair(p1, p2)) = (
		pp p1; string ","; PP.cut ppStrm; pp p2)
	    | pp (A.PatVar x) = ppVarBind (ppStrm, x)
	    | pp (A.PatVector(p1, p2, ty)) = (
		PP.openHBox ppStrm;
		  ppIdAtType(ppStrm, "__vector", [ty]);
		  string "("; pp p1; string ","; PP.cut ppStrm; pp p2; string ")";
		PP.closeBox ppStrm)
	  in
	    PP.openHBox ppStrm;
	      pp p;
	    PP.closeBox ppStrm
	  end

    and ppParam (ppStrm, p) = (
	  PP.openHBox ppStrm;
	    PP.string ppStrm "(";
	    ppPat (ppStrm, p);
	    PP.string ppStrm ")";
	  PP.closeBox ppStrm)

    fun ppExp (ppStrm, e) = let
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  val string = PP.string ppStrm
	  fun pp (A.ExpPair(e1 as A.ExpPair _, e2)) = (
		PP.openHBox ppStrm;
		  string "("; pp e1; string "),";
		PP.closeBox ppStrm;
		PP.cut ppStrm; pp e2)
	    | pp (A.ExpPair(e1, e2)) = (
		pp e1; string ","; PP.cut ppStrm; pp e2)
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
	    | pp (A.ExpApplyForEach(e, binds, _)) = (
		PP.openHVBox ppStrm indent;
		  string "{";
		  pp e;
		  sp(); string ":"; sp();
		  ppList ppRBind ("", "; ", "") (ppStrm, binds);
		  string "}";
		PP.closeBox ppStrm)
	    | pp (A.ExpApply(f, e)) = (
		PP.openHVBox ppStrm indent;
		  ppFunct (ppStrm, f); sp(); ppArg e;
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
	    | pp (A.ExpPureApply(rator, arg, _)) = (
		PP.openHBox ppStrm;
		  string "__prim"; sp(); string(concat["\"", Pure.toString rator, "\""]); sp(); ppArg arg;
		PP.closeBox ppStrm)
	    | pp (A.ExpCmdApply(cmd, arg, _)) = (
		PP.openHBox ppStrm;
		  string "__prim"; sp(); string(concat["\"", Cmd.toString cmd, "\""]); sp(); ppArg arg;
		PP.closeBox ppStrm)
	    | pp (A.ExpVector(e1, e2, ty)) = (
		PP.openHVBox ppStrm indent;
		  ppIdAtType(ppStrm, "__vector", [ty]);
		  string "("; pp e1; string ","; PP.cut ppStrm; pp e2; string ")";
		PP.closeBox ppStrm)
	    | pp _ = string "<exp>"
	  and ppArg e = (string "("; pp e; string ")")
	  and ppBind (p, e) = (
		PP.openHBox ppStrm;
		  ppPat (ppStrm, p); sp(); string "="; sp(); ppExp (ppStrm, e);
		PP.closeBox ppStrm)
	  and ppRBind (p, e) = (
		PP.openHBox ppStrm;
		  ppPat (ppStrm, p); sp(); string "in"; sp(); ppExp (ppStrm, e);
		PP.closeBox ppStrm)
	  in
	    pp e
	  end

    fun ppTop ppStrm top = let
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  val string = PP.string ppStrm
	  in
	    case top
	     of A.TopFun(f, []) => (
		  PP.openHBox ppStrm;
		    string "function"; sp(); string(Atom.toString(Funct.nameOf f)); string ";";
		  PP.closeBox ppStrm;
		  nl())
	      | A.TopFun(f, instances) => let
		  fun pp (f, p, e) = let
			val A.F{ty=(domTy, rngTy), inst=(polyF, tys), ...} = f
			in
			  PP.openHBox ppStrm;
			    PP.string ppStrm "%"; PP.space ppStrm 1;
			    ppIdAtType (ppStrm, Atom.toString(Funct.nameOf polyF), tys);
			    PP.space ppStrm 1; PP.string ppStrm "%";
			  PP.closeBox ppStrm;
			  nl();
			  PP.openVBox ppStrm indent;
			    PP.openHBox ppStrm;
			      string "function"; sp(); ppFunct(ppStrm, f); sp();
			      ppParam (ppStrm, p);
			      sp (); string ":"; sp ();
			      ppTy (ppStrm, domTy); sp(); string "->"; sp(); ppTy (ppStrm, rngTy);
			      sp (); string "=";
			    PP.closeBox ppStrm;
			    nl();
			    ppExp (ppStrm, e);
			  PP.closeBox ppStrm;
			  nl()
			end
		  in
		    List.app pp instances
		  end
	      | A.TopBind(p, e) => (
		  PP.openHBox ppStrm;
		    ppPat (ppStrm, p); sp(); string "="; sp(); ppExp (ppStrm, e);
		  PP.closeBox ppStrm;
		  nl())
	      | A.TopExp(e, ty) => (ppExp(ppStrm, e); nl())
	    (* end case *)
	  end

    fun output (outS, A.Program prog) = let
	  val ppStrm = PP.openOut {dst = outS, wid = 120}
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
	      PP.string ppStrm "%-- Monomorphic Program start --%"; PP.newline ppStrm;
	      List.app (ppTop ppStrm) prog;
	      PP.string ppStrm "%-- Monomorphic Program end --%"; PP.newline ppStrm;
	    PP.closeBox ppStrm;
	    PP.closeStream ppStrm
	  end

  end
