(* print-super-op.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Print the SuperOp representation
 *)

structure PrintSuperOp : sig

    val outputProg : TextIO.outstream * SuperOp.program -> unit
    val outputFunc : TextIO.outstream * SuperOp.func -> unit
    val outputStm : TextIO.outstream * SuperOp.stm -> unit
    val outputSOp : TextIO.outstream * SuperOp.super_op -> unit

    val prProg : SuperOp.program -> unit
    val prFunc : SuperOp.func -> unit
    val prStm : SuperOp.stm -> unit

  end = struct

    structure VE = VExp
    structure S = SuperOp
    structure PP = TextIOPP
    structure V = Vector

    type ppStrm = PP.stream

    val indent0 = (PP.Abs 0)
    val indent2 = (PP.Abs 2)
    val indent4 = (PP.Abs 4)

    fun new outs = PP.openOut {dst = outs, wid = 120}

    val close = PP.closeStream

    fun v2s (S.V id) = "x" ^ StringCvt.padLeft #"0" 3 (Word.fmt StringCvt.DEC id)

    val tyToString = VExp.tyToString
    val pureToString = VExp.pureToString
    val ioToString = VExp.ioToString

    fun isBinding (S.STM(_, S.RET _)) = false
      | isBinding (S.STM(_, S.EXIT)) = false
      | isBinding _ = true

    fun ppSuperOp (ppStrm, S.SOp{id, useCnt,  paramTys, resTy, def}) = let
	  val str = PP.string ppStrm
	  fun sp () = PP.space ppStrm 1
          fun v2s i = ("$" ^ Int.toString i)
	  fun inHBox f = (PP.openHBox ppStrm; f(); PP.closeBox ppStrm)
          fun ppExp e = (case e
                 of S.EXP(opcode, []) =>
                      inHBox (fn () => (str "("; str (pureToString opcode); str")"))
                  | S.EXP(opcode, args) => (
                      PP.openHOVBox ppStrm indent2;
                        str "("; str (pureToString opcode); sp(); str "@";
                        List.app (fn e => (sp(); ppExp e)) args;
                        str ")";
                      PP.closeBox ppStrm)
                  | S.ARG x => str(v2s(Word.toIntX x))
                (* end case *))
	  val params = List.rev (
		#2 (List.foldl (fn (ty, (i, xs)) => (i+1, concat[v2s i, " : ", VCode.tyToString ty] :: xs))
		  (0, []) paramTys))
          in
            inHBox (fn () => (
              str "fused"; sp(); str ("OP"^Int.toString id);
	      str "#"; str(Int.toString(!useCnt)); sp(); str "(";
              str (String.concatWith ", " params); str ")";
              sp(); str "="; sp(); ppExp def))
          end

    fun ppStm (ppStrm, stm) = let
	  val str = PP.string ppStrm
	  val atom = PP.string ppStrm o Atom.toString
          val ppV = str o v2s
	  fun sp () = PP.space ppStrm 1
	  fun inHBox f = (PP.openHBox ppStrm; f(); PP.closeBox ppStrm)
	  fun ppList {pp, sep, l} = let
		fun ppList' [] = ()
		  | ppList' [x] = pp x
		  | ppList' (x::xs) = (pp x; sep(); ppList' xs)
		in
		  ppList' l
		end
          fun ppVBs [] = str "()"
            | ppVBs [x] = ppV x
            | ppVBs xs = (
                str "("; ppList{pp = ppV, sep = fn () => (str ", "; sp()), l = xs}; str ")") 
          fun ppArgs (l, sep) = ppList {pp = ppV, sep = fn () => str sep, l = l}
          fun ppStm (S.STM(_, t)) = (case t
                 of S.LET(x, opcode, args, stm) => (
                      inHBox (fn () => (
                        str "let"; sp(); ppV x; sp(); str "="; sp();
                        str "("; str(pureToString opcode);
                        if List.null args
                          then ()
                          else (sp(); str "@"; sp(); ppArgs(args, " "));
                        str ")"));
                      PP.newline ppStrm;
                      ppScope stm)
                  | S.FUSED(x, S.SOp{id, ...}, args, stm) => (
                      inHBox (fn () => (
                        str "let"; sp(); ppV x; sp(); str "="; sp();
                        str ("OP"^Int.toString id); sp(); str "(";
                        ppArgs (List.tabulate(V.length args, fn i => V.sub(args, i)), ",");
                        str ")"));
                      PP.newline ppStrm;
                      ppScope stm)
                  | S.LETPACK(a, b, ty, args, stm) => (
                      inHBox (fn () => (
                        str "let"; sp(); ppVBs [a, b]; sp(); str "="; sp();
                        str "(PACK "; str(tyToString ty); sp(); str "@"; sp();
                        ppArgs(args, " "); str ")"));
                      PP.newline ppStrm;
                      ppScope stm)
                  | S.CALL(xs, f, args, stm) => (
                      inHBox (fn () => (
                        str "let"; sp(); ppVBs xs; sp(); str "="; sp();
                        str "CALL"; sp(); atom f; str "("; ppArgs(args, ", "); str ")"));
                      PP.newline ppStrm;
                      ppScope stm)
                  | S.STMT(xs, opcode, args, stm) => (
                      inHBox (fn () => (
                        str "let"; sp(); ppVBs xs; sp(); str "="; sp(); str "(";
                        str(ioToString opcode);
                        if List.null args
                          then ()
                          else (sp(); str "@"; List.app (fn e => (sp(); ppV e)) args);
                        str ")"));
                      PP.newline ppStrm;
                      ppScope stm)
                  | S.IF(xs, cond, trueS, falseS, cont) => (
                      PP.openVBox ppStrm indent0;
                        inHBox (fn () => (
                          str "let"; sp(); ppVBs xs; sp(); str "="; sp();
                          str "if"; sp(); ppV cond; sp(); str "then"));
                        PP.openVBox ppStrm indent2;
                          PP.newline ppStrm;
                          inHBox (fn () => ppStm trueS);
                        PP.closeBox ppStrm; PP.newline ppStrm;
                        str "else";
                        PP.openVBox ppStrm indent2;
                          PP.newline ppStrm;
                          inHBox (fn () => ppStm falseS);
                        PP.closeBox ppStrm; PP.newline ppStrm;
                      PP.closeBox ppStrm;
                      ppScope cont)
                  | S.RET es => inHBox (fn () => (
                      str "RET"; sp(); str "("; ppArgs (es, ", "); str ")"))
                  | S.EXIT => str "EXIT"
                (* end case *))
          and ppScope s = if isBinding s
                then ppStm s
                else (
                  PP.openVBox ppStrm indent2;
                    str "in"; PP.newline ppStrm; ppStm s;
                  PP.closeBox ppStrm)
          in
            PP.openVBox ppStrm indent0;
              ppStm stm;
            PP.closeBox ppStrm
          end

    fun ppFunc (ppStrm, S.FUNC(label, params, resTy, body)) = let
	  val str = PP.string ppStrm
	  val atom = PP.string ppStrm o Atom.toString
          val ppVB = str o v2s
	  fun sp () = PP.space ppStrm 1
	  fun inHBox f = (PP.openHBox ppStrm; f(); PP.closeBox ppStrm)
	  fun ppList {pp, sep, l} = let
		fun ppList' [] = ()
		  | ppList' [x] = pp x
		  | ppList' (x::xs) = (pp x; sep(); ppList' xs)
		in
		  ppList' l
		end
          in
            PP.openVBox ppStrm indent4;
              inHBox (fn () => (
                str "function "; atom label; sp(); str "(";
                ppList {pp = ppVB, sep = fn () => str ", ", l = params};
                str ")"));
              PP.newline ppStrm;
              ppStm (ppStrm, body);
            PP.closeBox ppStrm
          end

    fun outputWithStrm ppFn (outS, item) = let
          val ppStrm = new outS
          in
            ppFn (ppStrm, item);
            close ppStrm
          end

    val outputStm = outputWithStrm ppStm
    val outputFunc = outputWithStrm ppFunc
    val outputProg = let
          fun pp (ppStrm, S.PROG{sops, fns}) = (
                PP.openVBox ppStrm indent0;
                  List.app (fn sop => (ppSuperOp (ppStrm, sop); PP.newline ppStrm)) sops;
                  List.app (fn f => (ppFunc (ppStrm, f); PP.newline ppStrm)) fns;
                PP.closeBox ppStrm)
          in
            outputWithStrm pp
          end
    val outputSOp = outputWithStrm ppSuperOp

    fun prStm stm = outputStm (TextIO.stdOut, stm)
    fun prFunc f = outputFunc (TextIO.stdOut, f)
    fun prProg prog = outputProg (TextIO.stdOut, prog)

  end
