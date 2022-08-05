(* pp-fuse.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure PPFuse : sig

    val output : TextIO.outstream * FuseAST.program * string -> unit

    val outputKernel : TextIO.outstream * FuseAST.kernel * string -> unit

  end = struct

    structure A = FuseAST
    structure U = FuseUtil
    structure PP = TextIOPP

    val indent0 = PP.Abs 0
    val indent2 = PP.Abs 2
    val indent4 = PP.Abs 4

    fun ppList ppFn (left, sep, right) (ppStrm : PP.stream, list) = let
	  fun sp () = PP.space ppStrm 1
	  val string = PP.string ppStrm
	  fun pp [] = string right
	    | pp [x] = (ppFn(ppStrm, x); string right)
	    | pp (x::xs) = (ppFn(ppStrm, x); string sep; sp(); pp xs)
	  in
	    string left; pp list
	  end

    fun ppList' ppFn (left, sep, right) (ppStrm : PP.stream, list) = let
	  fun sp () = PP.space ppStrm 1
	  val string = PP.string ppStrm
	  fun pp [] = (string left; string right)
	    | pp [x] = (ppFn(ppStrm, x); string right)
	    | pp (x::xs) = (ppFn(ppStrm, x); string sep; sp(); pp xs)
	  in
	    case list
	     of [x] => ppFn(ppStrm, x)
	      | _ => pp list
	    (* end case *)
	  end

    fun ppTy (ppStrm, ty) = PP.string ppStrm (U.tyToString ty)

    fun ppVar (ppStrm, x) = PP.string ppStrm (FuseVar.toString x)
    fun ppFunct (ppStrm, f) = PP.string ppStrm (FuseFunct.toString f)

    fun ppKern (ppStrm, kern) = PP.string ppStrm (Kernel.toString kern)

    fun ppVarBind (ppStrm, v as A.V{name, stamp, ty, useCnt, ...}) = let
      val idStr =
	  (case ty
	    of A.TySegdes => FuseShapes.idToString(FuseShapes.getShapeId(FuseVar.getShape v))
	     | _ => ""
	  (* end case *))
    in
      PP.string ppStrm (String.concat[
	  name, Stamp.toString stamp, "#", Int.toString(!useCnt), idStr, " : ", U.tyToString ty
	])
    end

    fun ppVarBinds (ppStrm, vars) = ppList ppVarBind ("(", ",", ")") (ppStrm, vars)

    fun ppAtom (ppStrm, atom) = (case atom
           of A.Var x => ppVar(ppStrm, x)
            | A.Int n => PP.string ppStrm (Format.format "%d" [Format.LINT n])
            | A.Float f => PP.string ppStrm f
            | A.Bool b => PP.string ppStrm (Bool.toString b)
            | A.Char c => PP.string ppStrm ("`" ^ String.str c)
	    | A.String s => PP.string ppStrm s
          (* end case *))

    fun ppAtoms (ppStrm, atoms) = ppList ppAtom ("(", ",", ")") (ppStrm, atoms)

    fun isBinding exp = (case exp
          of A.Exp_Let _ => true
           | A.Exp_RHS _ => true
           | _ => false
        (* end case *))

    fun ppKernExp (ppStrm : PP.stream, e) = let
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  val string = PP.string ppStrm
	  fun ppLet body ppFn = (
		PP.openHBox ppStrm;
		  string "let"; sp(); ppFn ();
		PP.closeBox ppStrm;
		nl(); pp body)
	  and pp exp = (case exp
		 of A.KExp_Let(xs, rhs, e) => ppLet e (fn () => (
		      ppList' ppVarBind ("(", ",", ")") (ppStrm, xs);
		      sp(); string "="; sp();
		      pp rhs))
		  | A.KExp_Pure(x, rator, atms, e) => ppLet e (fn () => (
		      ppVarBind(ppStrm, x);
		      sp (); string "="; sp();
		      string(ScalarOp.toString rator);
		      ppAtoms (ppStrm, atms)))
		  | A.KExp_Proj(x, i, y, e) => ppLet e (fn () => (
		      ppVarBind(ppStrm, x);
		      sp (); string "="; sp();
		      string "#"; string(Int.toString i); sp();
		      ppVar (ppStrm, y)))
		  | A.KExp_Tuple(x, ys, e) => ppLet e (fn () => (
		      ppVarBind (ppStrm, x);
		      sp (); string "="; sp ();
		      ppList' ppAtom ("(", ",", ")") (ppStrm, ys)))
		  | A.KExp_If(a, e1, e2) => (
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
		  | A.KExp_Return xs => ppList' ppAtom ("(", ",", ")") (ppStrm, xs)
		(* end case *))
	  in
	    pp e
	  end

    fun ppRHS (ppStrm, rhs : A.rhs) = let
	  fun sp () = PP.space ppStrm 1
	  val string = PP.string ppStrm
	  fun ppApp (prefix, toS, ppArg) (rator, args) = (
		PP.openHBox ppStrm;
		  if prefix <> "" then (string prefix; sp()) else ();
		  string (toS rator);
		  sp();
		  ppList ppArg ("(", ",", ")") (ppStrm, args);
		PP.closeBox ppStrm)
	  fun ppAppWid (prefix, toS, ppArg) (rator, wid, args) = (
		PP.openHBox ppStrm;
		  if prefix <> "" then (string prefix; sp()) else ();
		  string (toS rator);
		  sp();
		  ppList ppArg ("(", ",", ")") (ppStrm, args);
		  sp();
		  string "##"; sp(); ppAtom (ppStrm, wid);
		PP.closeBox ppStrm)
	  in
	    case rhs
	     of A.RHS_Scalar(rator, xs) => ppApp ("", ScalarOp.toString, ppAtom) (rator, xs)
	      | A.RHS_FlatGen(rator, width, xs) =>
		  ppAppWid ("flat", GeneratorOp.toString, ppAtom) (rator, width, xs)
	      | A.RHS_SegGen(rator, width, xs) =>
		  ppAppWid ("", GeneratorOp.toString, ppVar) (rator, width, xs)
	      | A.RHS_Map(kern, width, xs) =>
		  ppAppWid ("map", Kernel.toString, ppVar) (kern, width, xs)
	      | A.RHS_FlatReduce(rator, width, x) =>
		  ppAppWid ("flat", ReduceOp.toString, ppVar) (rator, width, [x])
	      | A.RHS_SegReduce(rator, width, seg, data) =>
		  ppAppWid ("", ReduceOp.toString, ppVar) (rator, width, [seg, data])
	      | A.RHS_FlatScan(rator, width, x) =>
		  ppAppWid ("flat", ScanOp.toString, ppVar) (rator, width, [x])
	      | A.RHS_SegScan(rator, width, seg, data) =>
		  ppAppWid ("", ScanOp.toString, ppVar) (rator, width, [seg, data])
	      | A.RHS_FlatVector(rator, xs) => ppApp ("flat", VectorOp.toString, ppAtom) (rator, xs)
	      | A.RHS_SegVector(rator, xs) => ppApp ("", VectorOp.toString, ppAtom) (rator, xs)
	      | A.RHS_Internal(rator, x) => ppApp ("", InternalOp.toString, ppAtom) (rator, [x])
	      | A.RHS_Cmd(cmd, xs) => ppApp ("", Cmd.toString, ppAtom) (cmd, xs)
	      | A.RHS_Seq (xs, _) => ppList ppAtom ("[", ",", "]") (ppStrm, xs)
	      | A.RHS_Tuple xs => (*ppList ppAtom ("(", ",", ")") *) ppAtoms (ppStrm, xs)
	      | A.RHS_Proj(i, x) => (string "#"; string(Int.toString i); sp(); ppVar (ppStrm, x))
	    (* end case *)
	  end

    fun ppExp (ppStrm : PP.stream, e) = let
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  val string = PP.string ppStrm
          fun ppBlk exp = let
                fun ppLet body ppFn = (
                      PP.openHBox ppStrm;
                        string "let"; sp(); ppFn ();
                      PP.closeBox ppStrm;
                      nl(); ppBlk body)
                in
                  case exp
                   of A.Exp_Let(xs, e1, e2) =>
                        ppLet e2 (fn () => (
                          ppVarBinds (ppStrm, xs);
                          sp(); string "="; sp();
                          pp e1))
                    | A.Exp_RHS(xs, rhs, e) =>
                        ppLet e (fn () => (
                          ppVarBinds (ppStrm, xs);
                          sp(); string "="; sp();
                          ppRHS (ppStrm, rhs)))
                    | e => (
			PP.openVBox ppStrm indent2;
			  PP.string ppStrm "in"; nl();
			  pp e;
			PP.closeBox ppStrm)
                  (* end case *)
                end
          and pp (exp : A.exp) = if isBinding exp
                then (
                  PP.openVBox ppStrm indent0;
                    ppBlk exp;
                  PP.closeBox ppStrm)
                else (case exp
                   of A.Exp_If(a, e1, e2) => (
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
		    | A.Exp_Apply(f, atoms) => (
                        PP.openHBox ppStrm;
                          ppFunct (ppStrm, f); sp();
                          ppAtoms (ppStrm, atoms);
                        PP.closeBox ppStrm)
                    | A.Exp_Atom a => ppAtom (ppStrm, a)
                    | _ => raise Fail "impossible"
                     (* end case *))
    in
      pp e
    end

    fun ppKernel (ppStrm, kern) = let
	  val (params, body) = Kernel.defn kern
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  val string = PP.string ppStrm
	  in
	    PP.openVBox ppStrm indent2;
	      PP.openHBox ppStrm;
		string "kernel"; sp(); ppKern(ppStrm, kern); sp();
		ppList ppVarBind ("(", ",", ")") (ppStrm, params);
		sp (); string "=";
	      PP.closeBox ppStrm;
	      nl();
	      ppKernExp (ppStrm, body);
	    PP.closeBox ppStrm;
	    nl()
	  end

    fun ppTop ppStrm top = let
      fun sp () = PP.space ppStrm 1
      fun nl () = PP.newline ppStrm
      val string = PP.string ppStrm
    in
      case top
       of A.Top_Kern kern => ppKernel (ppStrm, kern)
	| A.Top_Funct(funct, params, body) => let
	    val A.F{name, stamp, ty=(domTy, rngTy), ...} = funct
	    in
	      PP.openVBox ppStrm indent2;
		PP.openHBox ppStrm;
		  string "function"; sp(); ppFunct(ppStrm, funct); sp();
		  ppList ppVar ("(", ",", ")") (ppStrm, params);
		  sp (); string ":"; sp ();
		  ppList ppTy ("(", ",", ")") (ppStrm, domTy); sp(); string "->"; sp(); ppTy (ppStrm, rngTy);
		  sp (); string "=";
		PP.closeBox ppStrm;
		nl();
		ppExp (ppStrm, body);
	      PP.closeBox ppStrm;
	      nl()
	    end
	| A.Top_Let(xs, e) => (
	    PP.openHBox ppStrm;
	      ppVarBinds (ppStrm, xs);
	      sp(); string "="; sp(); ppExp (ppStrm, e);
	    PP.closeBox ppStrm;
	    nl())
	| A.Top_RHS(xs, rhs) => (
	    PP.openHBox ppStrm;
	      ppVarBinds (ppStrm, xs);
	      sp(); string "="; sp(); ppRHS (ppStrm, rhs);
	    PP.closeBox ppStrm;
	    nl())
	| A.Top_Exp(e, _) => (ppExp(ppStrm, e); nl())
    (* end case *)
    end

    fun output (outS, A.Program prog, msg) = let
	  val ppStrm = PP.openOut {dst = outS, wid = 120}
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
	      PP.string ppStrm (concat["%-- FuseAST Program start ", msg, "--%"]); PP.newline ppStrm;
	      List.app (ppTop ppStrm) prog;
	      PP.string ppStrm (concat["%-- FuseAST Program end ", msg, "--%"]); PP.newline ppStrm;
	    PP.closeBox ppStrm;
	    PP.closeStream ppStrm
	  end

    fun outputKernel (outS, kernel, msg) = let
	  val ppStrm = PP.openOut {dst = outS, wid = 120}
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
	      PP.string ppStrm (concat["%-- Start ", msg, "--%"]); PP.newline ppStrm;
	      ppKernel (ppStrm, kernel);
	      PP.string ppStrm (concat["%-- End ", msg, "--%"]); PP.newline ppStrm;
	    PP.closeBox ppStrm;
	    PP.closeStream ppStrm
	  end

  end
