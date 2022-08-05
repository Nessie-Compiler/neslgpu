(* gen.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Gen =
  struct

    local
      open LNesl
      structure PP = TextIOPP
      structure A = Atom

      val indent0 = PP.Abs 0
      val indent2 = PP.Abs 2
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
      fun isBinOp rator = List.exists (fn x => (x = rator)) [
	      "or", "xor", "nor",
	      "and", "nand",
	      "=", "==", "/=", "<=", ">=", "<", ">",
	      "+", "-", "++", "<-",
	      "*", "/", "->", "||",
	      "^"
	    ]
      fun isUnOp rator = List.exists (fn x => (x = rator)) [
	      "#", "@", "-"
	    ]

    (* ensure that a primitive-function name is a syntatically correct Nesl ID *)
      fun cleanId id = String.translate (fn #"-" => "_" | #"+" => "plus" | c => String.str c) id
      fun cleanOp id = if (isBinOp id) orelse (isUnOp id)
	      then String.concat ["`", id, "`"]
	      else cleanId id

    in

    (* generate a NESL function from the contents of a Defop *)
      fun genNeslFn (ppS, isPrim, name, params, ty, body) = let
            fun sp () = PP.space ppS 1
            val string = PP.string ppS
            fun ppPat (VarPat x) = string(cleanId x)
              | ppPat (ConPat("vector", pats)) = (
                  string "__vector"; ppList ppPat ("(", ",", ")") (ppS, pats))
              | ppPat (ConPat(con, pats)) = (
                  string con; ppList ppPat ("(", ",", ")") (ppS, pats))
	    fun ppParams () = ppList ppPat ("(", ",", ")") (ppS, params);
            fun ppExp exp = (case exp
                   of VarExp x => string(cleanId x)
                    | ConstExp c => string c
                    | OpExp("vector", args) => ppApply("__vector", args)
                    | OpExp(rator, [e]) => let
                        fun needsParen (VarExp _) = false
                          | needsParen (ConstExp _) = false
                          | needsParen _ = true
                        in
                          if isUnOp rator
                            then (
                              PP.openHBox ppS;
                                string rator;
                                if needsParen e then ppParenExp e else ppExp e;
                              PP.closeBox ppS)
                            else ppApply(rator, [e])
                        end
                    | OpExp(rator, [e1, e2]) => let
                        fun needsParen (OpExp(rator, _)) =
                              isBinOp rator orelse isUnOp rator
                          | needsParen _ = false
                        in
                          if isBinOp rator
                            then (
                              PP.openHBox ppS;
                                if needsParen e1 then ppParenExp e1 else ppExp e1;
                                sp(); string rator; sp();
                                if needsParen e2 then ppParenExp e2 else ppExp e2;
                              PP.closeBox ppS)
                            else ppApply(rator, [e1, e2])
                        end
                    | OpExp(rator, args) => ppApply(rator, args)
                    | IfExp(e1, e2, e3) => (
                        PP.openVBox ppS indent2;
                          PP.openHBox ppS;
                            string "if"; sp(); ppExp e1;
                          PP.closeBox ppS;
                          PP.newline ppS;
                          PP.openHBox ppS;
                            string "then"; sp(); ppExp e2;
                          PP.closeBox ppS;
                          PP.newline ppS;
                          PP.openHBox ppS;
                            string "else"; sp(); ppExp e3;
                          PP.closeBox ppS;
                        PP.closeBox ppS)
                    | WithExp([], body) => ppExp body
                    | WithExp(bnd::binds, body) => (
                        PP.openVBox ppS indent0;
                          PP.openVBox ppS indent4;
                            PP.openHBox ppS;
                              string "let"; sp(); ppBind bnd;
                            PP.closeBox ppS;
                            List.app (fn b => (PP.newline ppS; ppBind b)) binds;
                          PP.closeBox ppS;
                          PP.newline ppS;
                          PP.openVBox ppS indent2;
                            string "in"; PP.newline ppS;
                            ppExp body;
                          PP.closeBox ppS;
                        PP.closeBox ppS)
                    | OverExp([], body) => ppExp body
                    | OverExp(bnd::bnds, body) => (
                        PP.openHVBox ppS indent2;
                          string "{"; ppExp body; sp(); string ":"; sp();
                          ppForEachBind bnd;
                          List.app (fn b => (string ";"; sp(); ppForEachBind b)) bnds;
                          string "}";
                        PP.closeBox ppS)
                    | BaseTypecaseExp(tv, rules) => let
			fun ppRule (ty, exp) = (
			      PP.newline ppS;
			      PP.openHBox ppS;
				string "|"; sp(); string(tyToString ty); string ":"; sp(); ppExp exp;
			      PP.closeBox ppS)
			in
			  PP.openVBox ppS indent2;
			    PP.openHBox ppS;
			      string "__base_typecase"; sp(); string tv;
			    PP.closeBox ppS;
			    List.app ppRule rules;
			  PP.closeBox ppS
			end
                    | PolyTypecaseExp(tv, e1, e2, optE3) => let
			fun ppE e = (
			      PP.newline ppS;
			      PP.openHBox ppS;
				string "|"; sp(); ppExp e;
			      PP.closeBox ppS)
			in
			  PP.openVBox ppS indent2;
			    PP.openHBox ppS;
			      string "__poly_typecase"; sp(); string tv;
			    PP.closeBox ppS;
			    ppE e1;
			    ppE e2;
			    Option.app ppE optE3;
			  PP.closeBox ppS
			end			  
                    | PrimExp [] => ppParams()
                    | PrimExp primOps => let
			fun flatPat (VarPat x, xs) = (cleanId x) :: xs
			  | flatPat (ConPat(_, pats), xs) = List.foldr flatPat xs pats
			fun ppVCode (VCode.Var x) = string(cleanId x)
			  | ppVCode (VCode.Const c) = string c
			  | ppVCode (VCode.Tuple _) = raise Fail "Tuple"
			  | ppVCode (VCode.PrimApply(prim, args)) = (
				string "__prim"; sp(); string "\""; string prim; string "\""; sp();
				ppList ppVCode ("(", ",", ")") (ppS, args))
			in
			  PP.openHBox ppS;
			    ppVCode (VCode.translate(primOps, List.foldr flatPat [] params))
			      handle ex => string(concat[
				  "%** tranlation failure: ", String.concatWith "; " primOps, " **%"
				]);
			  PP.closeBox ppS
			end
                  (* end case *))
            and ppParenExp e = (string "("; ppExp e; string ")")
            and ppApply (f, args) = (
                  PP.openHBox ppS;
                    string (cleanOp f);
                    ppList ppExp ("(", ",", ")") (ppS, args);
                  PP.closeBox ppS)
            and ppBind (pat, exp) = (
                  PP.openHBox ppS;
                    ppPat pat; sp(); string "="; sp(); ppExp exp; string ";";
                  PP.closeBox ppS)
            and ppForEachBind (VarPat x, VarExp y) = if (x = y)
                  then string(cleanId x)
                  else (
                    PP.openHBox ppS;
                      string x; sp(); string "in"; sp(); string y;
                    PP.closeBox ppS)
              | ppForEachBind (pat, exp) = (
                  PP.openHBox ppS;
                    ppPat pat; sp(); string "in"; sp(); ppExp exp;
                  PP.closeBox ppS)
            in
              PP.openVBox ppS indent2;
                PP.openHBox ppS;
                  if isPrim then (string "__prim"; sp()) else ();
		  string "function"; sp(); string(cleanOp name); sp();
                  ppParams();
                  case ty
                   of SOME fty => (
                        sp(); string ":"; sp(); string(funtyToString fty))
                    | NONE => ()
                  (* end case *);
                  sp(); string "="; PP.newline ppS;
                  ppExp body; sp(); string "$";
                PP.closeBox ppS;
              PP.closeBox ppS;
              PP.newline ppS
            end

    fun genLibFuns src = let
	  val ppStrm = PP.openOut {dst = TextIO.stdOut, wid = 120}
	  fun isPrim (BaseTypecaseExp _) = true
	    | isPrim (PolyTypecaseExp _) = true
	    | isPrim _ = false
          fun pp (Defop{name, params, ty, def=DEF body, ...}) =
                genNeslFn (ppStrm, isPrim body, name, params, ty, body)
	    | pp (Defop{name, params, ty, def=PRIM s, ...}) =
		genNeslFn (ppStrm, false, name, params, ty, PrimExp[s])
            | pp _ = ()
	  val defs = doFile src
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
	      PP.string ppStrm (concat[
		  "%-- Nesl functions from \"",
		  OS.Path.file src,
		  "\" --%"
		]);
	      PP.newline ppStrm;
	      List.app pp defs;
	      PP.string ppStrm (concat["%-- end \"", OS.Path.file src, "\" --%"]);
	      PP.newline ppStrm;
	    PP.closeBox ppStrm;
	    PP.closeStream ppStrm
	  end

    fun parScalar () = genLibFuns "from-neslsrc/scalar-ops.lnesl"
    fun seqScalar () = genLibFuns "from-neslseqsrc/scalar-ops.lnesl"

    fun parNestOps () = genLibFuns "from-neslsrc/nest-ops.lnesl"

    fun parVector () = genLibFuns "from-neslsrc/vector-ops.lnesl"
    fun seqVector () = genLibFuns "from-neslseqsrc/vector-ops.lnesl"

    fun parIO () = genLibFuns "from-neslsrc/io.lnesl"
    fun seqIO () = genLibFuns "from-neslseqsrc/io-ops.lnesl"

    end (* local open LNesl *)
  end
