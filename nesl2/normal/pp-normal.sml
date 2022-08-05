(* pp-normal.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure PPNormal : sig

    val output : TextIO.outstream * NormalAST.program -> unit

  end = struct

    structure A = NormalAST
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

    fun ppTy (ppStrm, ty) = PP.string ppStrm (NormalTy.toString ty)

    fun ppIdAtType (ppStrm, id, tys) = (
	  PP.openHBox ppStrm;
	    PP.string ppStrm id;
	    case tys
	     of [] => ()
	      | _ => (
		  PP.string ppStrm "_";
		  ppList ppTy ("<", ",", ">") (ppStrm, tys))
	    (* end case *);
	  PP.closeBox ppStrm)

    fun ppVar (ppStrm, A.V{name, stamp, ...}) =
	  PP.string ppStrm (Atom.toString name ^ Stamp.toString stamp)
    fun ppFunct (ppStrm, A.F{name, stamp, ...}) =
	  PP.string ppStrm (Atom.toString name ^ Stamp.toString stamp)

    fun ppVarBind (ppStrm, A.V{name, stamp, ty, ...}) =
	  PP.string ppStrm (String.concat[
	      Atom.toString name, Stamp.toString stamp, " : ", NormalTy.toString ty
	    ])

    fun ppAtom (ppStrm, atom) = (case atom
           of A.Var x => ppVar(ppStrm, x)
            | A.Int n => PP.string ppStrm (Format.format "%d" [Format.LINT n])
            | A.Float f => PP.string ppStrm f
            | A.Bool b => PP.string ppStrm (Bool.toString b)
            | A.String s=> PP.string ppStrm (concat["\"", String.toCString s, "\""])
            | A.Char c => PP.string ppStrm ("`" ^ String.str c)
          (* end case *))

    fun ppAtoms (ppStrm, atoms) = ppList ppAtom ("(", ",", ")") (ppStrm, atoms)

    fun ppPath (ppStrm, path) =
	  PP.string ppStrm
	    (String.concat (List.foldr (fn (i, p) => "." :: Int.toString i :: p) [] path))

    fun isBinding (A.Exp(_, nd)) = (case nd
           of A.ExpLet _ => true
            | A.ExpTuple _ => true
            | A.ExpLetVector _ => true
            | A.ExpVector _ => true
            | A.ExpSeq _ => true
            | A.ExpSeqRange _ => true
            | A.ExpPure _ => true
            | A.ExpCmd _ => true
            | A.ExpApply _ => false
            | A.ExpForEach _ => false
            | A.ExpIf _ => false
            | A.ExpAtom _ => false
          (* end case *))

    fun ppExp (ppStrm : PP.stream, e) = let
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  val string = PP.string ppStrm
          fun ppBlk (exp as A.Exp(_, nd)) = let
                fun ppBody e = (
                      PP.openVBox ppStrm indent2;
                        PP.string ppStrm "in"; nl();
                        pp e;
                      PP.closeBox ppStrm)
                fun ppLet body ppFn = (
                      PP.openHBox ppStrm;
                        string "let"; sp(); ppFn ();
                      PP.closeBox ppStrm;
                      nl(); ppBlk body)
                in
                  case nd
                   of A.ExpLet((x, e1), e2) =>
                        ppLet e2 (fn () => (
                          ppVarBind (ppStrm, x);
                          sp(); string "="; sp();
                          pp e1))
                    | A.ExpTuple(x, atoms, e) =>
                        ppLet e (fn () => (
                          ppVarBind (ppStrm, x);
                          sp(); string "="; sp();
                          ppAtoms (ppStrm, atoms)))
		    | A.ExpLetVector (x, y, atm, e) =>
			ppLet e (fn () => (
			  string "__vector(";
			  ppVarBind (ppStrm, x); string ","; sp();
			  ppVarBind (ppStrm, y); string ")";
			  sp(); string "="; sp();
			  ppAtom (ppStrm, atm)))
		    | A.ExpVector (x, atm1, atm2, e) =>
			ppLet e (fn () => (
			  ppVarBind (ppStrm, x); sp(); string "="; sp();
			  string "__vector"; ppAtoms (ppStrm, [atm1, atm2])))
                    | A.ExpSeq(x, atoms, e) =>
                        ppLet e (fn () => (
                          ppVarBind (ppStrm, x);
                          sp(); string "="; sp();
                          ppList ppAtom ("[", ",", "]") (ppStrm, atoms)))
                    | A.ExpSeqRange(x, a1, a2, a3, e) =>
                        ppLet e (fn () => (
                          ppVarBind (ppStrm, x);
                          sp(); string "="; sp();
                          string "["; ppAtom(ppStrm, a1); string ":";
                          ppAtom(ppStrm, a2); string ":";
                          ppAtom(ppStrm, a3); string "]"))
                    | A.ExpPure(x, opcode, atoms, e) =>
                        ppLet e (fn () => (
                          ppVarBind (ppStrm, x);
                          sp(); string "="; sp();
                          string(concat["\"", Pure.toString opcode, "\""]); ppAtoms (ppStrm, atoms)))
                    | A.ExpCmd(xs, opcode, atoms, e) =>
                        ppLet e (fn () => (
                          ppList' ppVarBind ("(", ",", ")") (ppStrm, xs);
                          sp(); string "="; sp();
                          string(concat["\"", Cmd.toString opcode, "\""]); ppAtoms (ppStrm, atoms)))
                    | _ => ppBody exp
                  (* end case *)
                end
          and pp (exp : NormalAST.exp) = if isBinding exp
                then (
                  PP.openVBox ppStrm indent0;
                    ppBlk exp;
                  PP.closeBox ppStrm)
                else (case exp
                   of A.Exp(_, A.ExpApply(f, atoms)) => (
                        PP.openHBox ppStrm;
                          ppFunct (ppStrm, f); sp();
                          ppAtoms (ppStrm, atoms);
                        PP.closeBox ppStrm)
                    | A.Exp(_, A.ExpForEach(e, binds)) => let
                        fun ppBind (ppStrm, (x, xs)) = (
                              PP.openHBox ppStrm;
                                ppVarBind (ppStrm, x); sp(); string "in"; sp(); ppVar(ppStrm, xs);
                              PP.closeBox ppStrm)
                        in
                            PP.openHVBox ppStrm indent2;
                              string "{";
                              pp e;
                              sp(); string ":"; sp();
                              ppList ppBind ("", "; ", "") (ppStrm, binds);
                              string "}";
                            PP.closeBox ppStrm
                        end
                    | A.Exp(_, A.ExpIf(a, e1, e2)) => (
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
                    | A.Exp(_, A.ExpAtom a) => ppAtom (ppStrm, a)
                    | _ => raise Fail "impossible"
                  (* end case *))
          in
            pp e
          end

    fun ppTop ppStrm top = let
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  val string = PP.string ppStrm
	  in
	    case top
	     of A.TopFun(_, []) => ()
	      | A.TopFun(_, instances) => let
		  fun pp (f, xs, e) = let
			val A.F{ty=(domTy, rngTy), ...} = f
			in
			  PP.openVBox ppStrm indent2;
			    PP.openHBox ppStrm;
			      string "function"; sp(); ppFunct(ppStrm, f); sp();
			      ppList ppVar ("(", ",", ")") (ppStrm, xs);
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
	      | A.TopBind(x, e) => (
		  PP.openHBox ppStrm;
		    ppVarBind (ppStrm, x);
                    sp(); string "="; sp(); ppExp (ppStrm, e);
		  PP.closeBox ppStrm;
		  nl())
	      | A.TopExp(e, ty) => (ppExp(ppStrm, e); nl())
	    (* end case *)
	  end

(*
    fun ppPrimInstance ppStrm (A.F{name, ty, ...}) = (
	  PP.openHBox ppStrm;
	    PP.string ppStrm "instance";
	    PP.space ppStrm 1;
	    ppIdAtType (ppStrm, Atom.toString name, tys);
	    PP.space ppStrm 2;
	    PP.string ppStrm ":";
	    PP.space ppStrm 2;
	    PP.string ppStrm (NormalTy.funtyToString ty);
	  PP.closeBox ppStrm;
	  PP.newline ppStrm)
*)

    fun output (outS, A.Program prog) = let
	  val ppStrm = PP.openOut {dst = outS, wid = 120}
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
	      PP.string ppStrm "%-- Normalized Program start --%"; PP.newline ppStrm;
	      List.app (ppTop ppStrm) prog;
	      PP.string ppStrm "%-- Normalized Program end --%"; PP.newline ppStrm;
	    PP.closeBox ppStrm;
	    PP.closeStream ppStrm
	  end

  end
