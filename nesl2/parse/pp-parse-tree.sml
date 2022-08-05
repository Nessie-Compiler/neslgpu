(* pp-parse-tree.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure PPParseTree : sig

    val output : TextIO.outstream * ParseTree.program -> unit

  end = struct

    structure PT = ParseTree
    structure PP = TextIOPP

    val indent0 = PP.Abs 0
    val indent = PP.Abs 3

    fun ppCon (ppS, name) ppBody = (
	  PP.openHBox ppS;
	    PP.string ppS name; PP.newline ppS;
	  PP.closeBox ppS;
	  PP.openVBox ppS indent;
	    ppBody ppS;
	  PP.closeBox ppS)
    fun ppCon' (ppS, msg) = ppCon (ppS, String.concat msg)
    fun ppLeaf (ppS, msg) = (
	  PP.openHBox ppS;
	    PP.string ppS (String.concat msg); PP.newline ppS;
	  PP.closeBox ppS)
    fun ppOpt (ppS, NONE) _ = ppLeaf (ppS, ["NONE"])
      | ppOpt (ppS, SOME item) ppFn = ppFn (ppS, item)

    fun programPP (ppS, PT.Program files) =
	  ppCon (ppS, "Program")
	    (fn ppS => (List.app (fn f => filePP(ppS, f)) files))

    and filePP (ppS, PT.File{name, contents}) =
	  ppCon' (ppS, ["File \"", name, "\""])
	    (fn ppS => (List.app (fn {tree, ...} => toplevelPP(ppS, tree)) contents))

    and toplevelPP (ppS, top) = (case top
	   of PT.TopFun(f, pat, optFnTy, e) => ppCon' (ppS, ["TopFun ", Atom.toString f])
		(fn ppS => (
		  patPP(ppS, pat);
		  ppOpt (ppS, optFnTy) funtyPP;
		  expPP (ppS, e)))
	    | PT.TopPrimFun(f, pat, funty, e) => ppCon' (ppS, ["PrimTopFun ", Atom.toString f])
		(fn ppS => (
		  patPP(ppS, pat);
		  funtyPP(ppS, funty);
		  expPP (ppS, e)))
	    | PT.TopData(dt, tydef) => ppCon' (ppS, ["TopData ", Atom.toString dt])
		(fn ppS => (tydefPP (ppS, tydef)))
	    | PT.TopBind(pat, exp) => ppCon (ppS, "TopBind")
		(fn ppS => (patPP(ppS, pat); expPP(ppS, exp)))
	    | PT.TopExp exp => ppCon (ppS, "TopExp") (fn ppS => expPP(ppS, exp))
	  (* end case *))

    and funtyPP (ppS, PT.TyFun{tree=(domTy, rngTy, tvs), ...}) = ppCon (ppS, "TyFun")
	  (fn ppS => ())

    and tydefPP (ppS, PT.TyDef{tree=(ty, tvs), ...}) = ppCon (ppS, "TyFun")
	  (fn ppS => ())

    and tyPP (ppS, ty) = (case ty
	   of PT.TyMark{tree, ...} => tyPP(ppS, tree)
	    | PT.TyId(id, tys) => ppCon' (ppS, ["TyId ", Atom.toString id])
		(fn ppS => (List.app (fn ty => tyPP(ppS, ty)) tys))
	    | PT.TyPair(ty1, ty2) => ppCon (ppS, "TyPair")
		(fn ppS => (tyPP(ppS, ty1); tyPP(ppS, ty1)))
	    | PT.TySeq ty => ppCon (ppS, "TySeq") (fn ppS => tyPP(ppS, ty))
	  (* end case *))

    and patPP (ppS, pat) = (case pat
	   of PT.PatMark{tree, ...} => patPP (ppS, tree)
	    | PT.PatPair(p1, p2) => ppCon (ppS, "PatPair")
		(fn ppS => (patPP(ppS, p1); patPP(ppS, p2)))
	    | PT.PatCons(dt, p) => ppCon' (ppS, ["PatCons ", Atom.toString dt])
		(fn ppS => patPP(ppS, p))
	    | PT.PatVector p => ppCon' (ppS, ["PatVector"]) (fn ppS => patPP(ppS, p))
	    | PT.PatVar x => ppLeaf (ppS, ["ExpVar ", Atom.toString x])
	    | PT.PatWild => ppLeaf (ppS, ["PatWild"])
	  (* end case *))

    and expPP (ppS, exp) = (case exp
	   of PT.ExpMark{tree, ...} => expPP (ppS, tree)
	    | PT.ExpPair(e1, e2) => ppCon (ppS, "ExpPair")
		(fn ppS => (expPP (ppS, e1); expPP (ppS, e2)))
	    | PT.ExpIf(e1, e2, e3) => ppCon (ppS, "ExpIf")
		(fn ppS => (expPP (ppS, e1); expPP (ppS, e2); expPP(ppS, e3)))
	    | PT.ExpLet(binds, e) => ppCon (ppS, "ExpLet")
		(fn ppS => (List.app (fn b => bindPP(ppS, b)) binds; expPP(ppS, e)))
	    | PT.ExpApplyForEach(e, binds, optExp) => ppCon (ppS, "ExpApplyForEach")
		(fn ppS => (
		  expPP(ppS, e);
		  List.app (fn b => bindPP(ppS, b)) binds;
		  ppOpt (ppS, optExp) expPP))
	    | PT.ExpBinary(e1, bop, e2) => ppCon' (ppS, ["ExpBinary ", binop2s bop])
		(fn ppS => (expPP (ppS, e1); expPP (ppS, e2)))
	    | PT.ExpUnary(uop, e) => ppCon' (ppS, ["ExpUnary ", unop2s uop])
		(fn ppS => expPP (ppS, e))
	    | PT.ExpSubscript(e1, e2) => ppCon (ppS, "ExpSubscript")
		(fn ppS => (expPP (ppS, e1); expPP (ppS, e2)))
	    | PT.ExpApply(f, e) => ppCon' (ppS, ["ExpApply ", Atom.toString f])
		(fn ppS => expPP (ppS, e))
	    | PT.ExpSeqEmpty ty => ppCon (ppS, "ExpSeqEmpty") (fn ppS => (tyPP(ppS, ty)))
	    | PT.ExpSeqRange(e1, e2, optExp) => ppCon (ppS, "ExpSeqRange")
		(fn ppS => (expPP (ppS, e1); expPP (ppS, e2); ppOpt (ppS, optExp) expPP))
	    | PT.ExpSeq exps => ppCon (ppS, "ExpSeq")
		(fn ppS => (List.app (fn e => expPP(ppS, e)) exps))
	    | PT.ExpParen e => expPP(ppS, e)
	    | PT.ExpVar x => ppLeaf (ppS, ["ExpVar ", Atom.toString x])
	    | PT.ExpInt n => ppLeaf (ppS, ["ExpInt ", IntInf.toString n])
	    | PT.ExpFloat f => ppLeaf (ppS, ["ExpFloat ", f])
	    | PT.ExpBool b => ppLeaf (ppS, ["ExpBool ", Bool.toString b])
	    | PT.ExpString s => ppLeaf (ppS, ["ExpString \"", String.toString s, "\""])
	    | PT.ExpChar c => ppLeaf (ppS, ["ExpChar \"", String.str c, "\""])
            | PT.ExpBaseTypecase(id, rules) => () (* FIXME *)
            | PT.ExpPolyTypecase(id, e1, e2) => () (* FIXME *)
            | PT.ExpPrimApply(prim, arg) => ppCon' (ppS, ["ExpPrimApply ", Atom.toString prim])
		(fn ppS => expPP (ppS, e))
	    | PT.ExpVector e => ppCon' (ppS, ["ExpVector"]) (fn ppS => patPP(ppS, e))
	  (* end case *))

    and bindPP (ppS, PT.Bind({tree=(pat, exp), ...})) = ppCon (ppS, "Bind")
	  (fn ppS => (patPP(ppS, pat); expPP(ppS, exp)))

    and binop2s PT.BinOpOr = "BinOpOr"
      | binop2s PT.BinOpNor = "BinOpNor"
      | binop2s PT.BinOpXor = "BinOpXor"
      | binop2s PT.BinOpAnd = "BinOpAnd"
      | binop2s PT.BinOpNand = "BinOpNand"
      | binop2s PT.BinOpEq = "BinOpEq"
      | binop2s PT.BinOpNeq = "BinOpNeq"
      | binop2s PT.BinOpLt = "BinOpLt"
      | binop2s PT.BinOpGt = "BinOpGt"
      | binop2s PT.BinOpLte = "BinOpLte"
      | binop2s PT.BinOpGte = "BinOpGte"
      | binop2s PT.BinOpAdd = "BinOpAdd"
      | binop2s PT.BinOpSub = "BinOpSub"
      | binop2s PT.BinOpConcat = "BinOpConcat"
      | binop2s PT.BinOpWrite = "BinOpWrite"
      | binop2s PT.BinOpMul = "BinOpMul"
      | binop2s PT.BinOpDiv = "BinOpDiv"
      | binop2s PT.BinOpGet = "BinOpGet"
      | binop2s PT.BinOpPad = "BinOpPad"
      | binop2s PT.BinOpPow = "BinOpPow"

    and unop2s PT.UnOpLen = "UnOpLen"
      | unop2s PT.UnOpNeg = "UnOpNeg"
      | unop2s PT.UnOpToString = "UnOpToString"

    fun output (outS, prog) = let
	  val ppStrm = PP.openOut {dst = outS, wid = 120}
	  in
	    PP.openVBox ppStrm (PP.Abs 0);
	      PP.string ppStrm "%-- Parse tree start --%"; PP.newline ppStrm;
	      programPP (ppStrm, prog);
	      PP.string ppStrm "%-- Parse tree Program end --%"; PP.newline ppStrm;
	    PP.closeBox ppStrm;
	    PP.closeStream ppStrm
	  end

  end
