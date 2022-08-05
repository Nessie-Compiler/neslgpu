(* print-as-cuda.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Print the CLang representation using CUDA syntax.
 *)

structure PrintAsCUDA : sig

    type strm

    val new : TextIO.outstream -> strm

    val close : strm -> unit

    val output : strm * CLang.decl -> unit

  end = struct

    structure CL = CLang
    structure PP = TextIOPP

    type strm = PP.stream

    val indent0 = (PP.Abs 0)
    val indent = (PP.Abs 4)     (* standard indentation amount *)

    fun new outs = PP.openOut {dst = outs, wid = 120}

    val close = PP.closeStream

    fun output (strm, decl) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun inHBox f = (PP.openHBox strm; f(); PP.closeBox strm)
          fun ppCom s = inHBox (fn () => (str "// "; str s))
          fun ppComLn s = (ppCom s; PP.newline strm)
          fun ppList {pp, sep, l} = let
                fun ppList' [] = ()
                  | ppList' [x] = pp x
                  | ppList' (x::xs) = (pp x; sep(); ppList' xs)
                in
                  ppList' l
                end
          fun ppTy (ty, optVar) = let
                fun getBaseTy (CL.T_Ptr ty) = getBaseTy ty
		  | getBaseTy (CL.T_RestrictPtr ty) = getBaseTy ty
                  | getBaseTy (CL.T_Array(ty, _)) = getBaseTy ty
                  | getBaseTy (CL.T_Num rty) = (case rty
                       of CL.Int8 => "signed char"
                        | CL.UInt8 => "unsigned char"
                        | CL.Int16 => "short"
                        | CL.UInt16 => "unsigned short"
                        | CL.Int32 => "int"
                        | CL.UInt32 => "unsigned int"
                        | CL.Int64 => "long"
                        | CL.UInt64 => "unsigned long"
                        | CL.Float => "float"
                        | CL.Double => "double"
                      (* end case *))
                  | getBaseTy (CL.T_Named ty) = ty
		  | getBaseTy (CL.T_Template(name, tys)) = concat[
			name, "<", String.concatWith "," (List.map getBaseTy tys), ">"
		      ]
                  | getBaseTy (CL.T_Qual(attr, ty)) =
                      concat[attr, " ", getBaseTy ty]
                fun pp (isFirst, CL.T_Ptr ty, optVar) = (
                      if isFirst then sp() else ();
                      case ty
                       of CL.T_Array _ => (
                            str "(*"; pp(false, ty, optVar); str ")")
                        | _ => (str "*"; pp(false, ty, optVar))
                      (* end case *))
		  | pp (isFirst, CL.T_RestrictPtr ty, optVar) = (
                      if isFirst then sp() else ();
                      case ty
                       of CL.T_Array _ => (
                            str "(*"; sp(); str "__restrict__"; sp(); pp(false, ty, optVar); str ")")
                        | _ => (str "*"; sp(); str "__restrict__"; sp(); pp(false, ty, optVar))
                      (* end case *))
                  | pp (isFirst, CL.T_Array(ty, optN), optVar) = (
                      pp (isFirst, ty, optVar);
                      case optN
                       of NONE => str "[]"
                        | SOME n => (str "["; str(Int.toString n); str "]")
                      (* end case *))
                  | pp (isFirst, CL.T_Qual(_, ty), optVar) =
                      pp (isFirst, ty, optVar)
                  | pp (isFirst, _, SOME x) = (
                      if isFirst then sp() else ();
                      str x)
                  | pp (_, _, NONE) = ()
                in
                  str (getBaseTy ty);
                  pp (true, ty, optVar)
                end
          fun ppAttrs [] = ()
            | ppAttrs attrs = (
                ppList {pp=str, sep=sp, l = attrs};
                sp())
          fun ppDecl dcl = (case dcl
                 of CL.D_Pragma l => (
		      inHBox (fn () => (
			str "#pragma";
			List.app (fn s => (sp(); str s)) l));
		      PP.newline strm)
		  | CL.D_Comment l => List.app ppComLn l
                  | CL.D_Verbatim l => List.app str l
                  | CL.D_Var(attrs, ty, x, optInit) => (
                      inHBox (fn () => (
                        ppAttrs attrs;
                        ppTy (ty, SOME x);
                        case optInit
                         of SOME init => (sp(); str "="; sp(); ppInit init)
                          | NONE => ()
                        (* end case *);
                        str ";"));
                      PP.newline strm)
                  | CL.D_Proto(attrs, ty, f, params) => (
                      inHBox (fn () => (
                        ppAttrs attrs;
                        ppTy(ty, SOME f);
                        sp(); str "(";
                        ppList {pp=ppParam, sep=fn () => (str ","; sp()), l=params};
                        str ");"));
                      PP.newline strm)
                  | CL.D_Func(attrs, ty, f, params, body) => (
                      PP.openVBox strm indent0;
                        inHBox (fn () => (
                          ppAttrs attrs;
                          ppTy(ty, SOME f);
                          sp(); str "(";
                          ppList {pp=ppParam, sep=fn () => (str ","; sp()), l=params};
                          str ")"));
                        PP.newline strm;
                        ppBlock (case body of CL.S_Block stms => stms | stm => [stm]);
                      PP.closeBox strm;
                      PP.newline strm)
                  | CL.D_StructDef(SOME name, fields, NONE) => (
                      PP.openVBox strm indent0;
                        inHBox (fn () => (str "struct"; sp(); str name; sp(); str "{"));
                        PP.openVBox strm indent;
                          List.app (fn (ty, x) => (
                              PP.newline strm;
                              inHBox (fn () => (ppTy(ty, SOME x); str ";"))))
                            fields;
                        PP.closeBox strm;
                        PP.newline strm;
                        str "};";
                      PP.closeBox strm;
                      PP.newline strm)
                  | CL.D_StructDef(optStruct, fields, SOME tyName) => (
                      PP.openVBox strm indent0;
                        str "typedef struct {"; 
                        PP.openVBox strm indent;
                          List.app (fn (ty, x) => (
                              PP.newline strm;
                              inHBox (fn () => (ppTy(ty, SOME x); str ";"))))
                            fields;
                        PP.closeBox strm;
                        PP.newline strm;
                        inHBox (fn () => (str "}"; sp(); str tyName; str ";"));
                        PP.closeBox strm;
                      PP.newline strm)
                (* end case *))
          and ppParam (CL.PARAM(attrs, ty, x)) = (
                ppAttrs attrs;
                ppTy(ty, SOME(CL.varToString x)))
          and ppInit init = (case init
                 of CL.I_Exp e => ppExp e
		  | CL.I_Exps fields => (
                      str "{";
                      PP.openHVBox strm indent;
                        List.app (fn init => (
                            PP.break strm;
			    inHBox (fn () => (ppInit init; str ","))))
                          fields;
                      PP.closeBox strm;
                      str "}")
                  | CL.I_Struct fields => (
                      str "{";
                      PP.openHVBox strm indent;
                        List.app (fn (lab, init) => (
                            PP.break strm;
                            inHBox (fn () => (
                              str("." ^ lab); sp(); str "="; sp(); ppInit init; str ","))))
                          fields;
                      PP.closeBox strm;
                      str "}")
                  | CL.I_Array elems => (
                      str "{";
                      PP.openHVBox strm indent;
                        List.app (fn (i, init) => (
                            PP.break strm;
                            inHBox (fn () => (
                              str(concat["[", Int.toString i, "]"]); sp(); str "="; sp();
                              ppInit init; str ","))))
                          elems;
                      PP.closeBox strm;
                      str "}")
                (* end case *))
          and ppBlock stms =  (
                str "{";
                PP.openVBox strm indent;
                  List.app (fn stm => (PP.newline strm; ppStm stm)) stms;
                PP.closeBox strm;
                PP.newline strm;
                str "}")
          and ppStm stm = (case stm
                 of CL.S_Block stms => ppBlock stms
                  | CL.S_Comment l => List.app ppCom l
                  | CL.S_Verbatim [] => ()
		  | CL.S_Verbatim (stm::stms) => (
		      str stm;
		      List.app (fn stm => (PP.newline strm; str stm)) stms)
                  | CL.S_Decl(attrs, ty, x, NONE) => inHBox (fn () => (
                      ppAttrs attrs;
                      ppTy(ty, SOME x); str ";"))
                  | CL.S_Decl(attrs, ty, x, SOME(CL.I_Exp(CL.E_Cons(_, args)))) =>
		      inHBox (fn () => (
			ppAttrs attrs;
			ppTy(ty, SOME x); ppArgs args; str ";"))
                 | CL.S_Decl(attrs, ty, x, SOME e) => inHBox (fn () => (
                      ppAttrs attrs;
                      ppTy(ty, SOME x); sp(); str "="; sp(); ppInit e; str ";"))
                  | CL.S_Exp e => inHBox (fn () => (ppExp e; str ";"))
                  | CL.S_If(e, blk, CL.S_Block[]) =>
                      inHBox (fn () => (str "if"; sp(); ppExp e; ppStms blk))
                  | CL.S_If(e, blk1, blk2) => (
                      PP.openVBox strm indent0;
                        inHBox (fn () => (str "if"; sp(); ppExp e; ppStms blk1));
                        PP.newline strm;
                        inHBox (fn () => (str "else"; ppStms blk2));
                      PP.closeBox strm)
                  | CL.S_While(e, blk) =>
                      inHBox (fn () => (str "while"; sp(); ppExp e; ppStms blk))
                  | CL.S_DoWhile(blk, e) =>
                      inHBox (fn () => (
                        str "do"; ppStms blk; sp(); str "while"; sp(); ppExp e))
                  | CL.S_For(inits, cond, incrs, blk) => let
                      fun ppInit (ty, x, e) = inHBox (fn () => (
                            ppTy(ty, SOME x);
                            sp(); str "="; sp();
                            ppExp e))
                      in
                        inHBox (fn () => (
                          str "for"; sp(); str "(";
                          ppList {pp = ppInit, sep = fn () => str ",", l = inits};
                          str ";"; sp();
                          ppExp cond; str ";"; sp();
                          ppList {pp = ppExp, sep = fn () => str ",", l = incrs};
                          str ")";
                          ppStms blk))
                      end
                  | CL.S_KernCall(f, config, args) => inHBox (fn () => (
		      str f;
		      str "<<<";
		      PP.openHOVBox strm indent;
			PP.cut strm;
			ppList {
			    pp = fn e => (PP.openHBox strm; ppExp e; PP.closeBox strm),
			    sep = fn () => (str ","; sp()),
			    l = config
			  };
			str ">>>";
		      PP.closeBox strm;
		      ppArgs args; str ";"))
                  | CL.S_Return(SOME e) => inHBox (fn () => (str "return"; sp(); ppExp e; str ";"))
                  | CL.S_Return _ => str "return;"
                  | CL.S_Break => str "break;"
                  | CL.S_Continue => str "continue;"
                (* end case *))
          and ppStms (CL.S_Block stms) = (sp(); ppBlock stms)
            | ppStms stm = (
                PP.openHOVBox strm indent;
                  sp ();
                  ppStm stm;
                PP.closeBox strm)
          and ppExp e = (case e
                 of CL.E_Grp e => (str "("; ppExp e; str ")")
                  | CL.E_AssignOp(lhs, rator, rhs) => (
                      ppExp lhs; sp(); str(CL.assignopToString rator); sp(); ppExp rhs)
                  | CL.E_Cond(e1, e2, e3) => (
                      ppExp e1; sp(); str "?"; sp(); ppExp e2; sp(); str ":"; sp(); ppExp e3)
                  | CL.E_BinOp(e1, rator, e2) => (ppExp e1; str(CL.binopToString rator); ppExp e2)
                  | CL.E_UnOp(rator, e) => (str(CL.unopToString rator); ppExp e)
                  | CL.E_PostOp(e, rator) => (ppExp e; str(CL.postopToString rator))
                  | CL.E_Apply(e, args) => (ppExp e; ppArgs args)
                  | CL.E_Cons(ty, args) => (ppTy(ty, NONE); ppArgs args)
                  | CL.E_New(ty, args) => (
		      str "new"; sp(); ppTy(ty, NONE);
		      case (ty, args)
		       of (CL.T_Named ty, []) => str ty
			| (CL.T_Template _, []) => ppTy(ty, NONE)
			| (CL.T_Named ty, args) => (str ty; ppArgs args)
			| (CL.T_Template _, args) => (ppTy(ty, NONE); ppArgs args)
			| (ty, []) => ppTy(ty, NONE)
			| _ => raise Fail "bogus new"
		      (* end case *))
                  | CL.E_Subscript(e1, e2) => (ppExp e1; str "["; ppExp e2; str "]")
                  | CL.E_Select(e, f) => (ppExp e; str "."; str f)
                  | CL.E_Indirect(e, f) => (ppExp e; str "->"; str f)
                  | CL.E_Cast(ty, e) => (
                      str "("; ppTy(ty, NONE); str ")"; ppExp e)
                  | CL.E_Var x => str(CL.varToString x)
                  | CL.E_Int(n, CL.T_Num(CL.Int64)) =>
                      str(IntInf.toString n ^ "l")
                  | CL.E_Int(n, _) => str(IntInf.toString n)
                  | CL.E_Flt f => str f
                  | CL.E_Bool b => str(Bool.toString b)
                  | CL.E_Str s => str(concat["\"", String.toCString s, "\""])
                  | CL.E_Char c => str(concat["'", Char.toCString c, "'"])
                  | CL.E_Sizeof ty => (str "sizeof("; ppTy(ty, NONE); str ")")
                (* end case *))
          and ppArgs args = (
                str "(";
                PP.openHOVBox strm indent;
                  PP.cut strm;
                  ppList {
                      pp = fn e => (PP.openHBox strm; ppExp e; PP.closeBox strm),
                      sep = fn () => (str ","; sp()),
                      l = args
                    };
                  str ")";
                PP.closeBox strm)
          in
            ppDecl decl
          end

  end
