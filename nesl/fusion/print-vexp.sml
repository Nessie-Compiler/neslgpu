(* print-vexp.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Print VExp representation
 *)

structure PrintVExp : sig

    val outputProg : TextIO.outstream * VExp.func list -> unit
    val outputFunc : TextIO.outstream * VExp.func -> unit
    val outputStm : TextIO.outstream * VExp.stm -> unit

    val prProg : VExp.func list -> unit
    val prFunc : VExp.func -> unit
    val prStm : VExp.stm -> unit

  end = struct

    structure VE = VExp
    structure SA = AnalyzeSizes
    structure PP = TextIOPP

    type ppStrm = PP.stream

    val indent0 = (PP.Abs 0)
    val indent2 = (PP.Abs 2)
    val indent4 = (PP.Abs 4)

    fun new outs = PP.openOut {dst = outs, wid = 120}

    val close = PP.closeStream

    fun vb2s x = VE.varToString true x ^ Sizes.toString(AnalyzeSizes.sizeOfVar x)
    val v2s = VE.varToString false
    val tyToString = VE.tyToString
    val pureToString = VE.pureToString
    val ioToString = VE.ioToString
    val expToString = VE.expToString

    fun isBinding (VE.RET _) = false
      | isBinding VE.EXIT = false
      | isBinding _ = true

    fun ppStm (ppStrm, stm) = let
	  val str = PP.string ppStrm
	  val atom = PP.string ppStrm o Atom.toString
          val ppVB = str o vb2s
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
            | ppVBs [x] = ppVB x
            | ppVBs xs = (
                str "("; ppList{pp = ppVB, sep = fn () => (str ","; sp()), l = xs}; str ")") 
          fun ppExp e = (case e
                 of VE.PURE(opcode, []) => inHBox (fn () => (str "("; str (pureToString opcode); str")"))
                  | VE.PURE(opcode, args) => (
                      PP.openHOVBox ppStrm indent2;
                        str "("; str (pureToString opcode); sp(); str "@";
                        List.app (fn e => (sp(); ppExp e)) args;
                        str ")";
                      PP.closeBox ppStrm)
                  | VE.VAR x => ppV x
                (* end case *))
          and ppExps (l, sep) = ppList {pp = ppExp, sep = fn () => str sep, l = l}
          fun ppStm stm = (case stm
                 of VE.LET(x, e, stm) => (
                      inHBox (fn () => (str "let"; sp(); ppVB x; sp(); str "="; sp(); ppExp e));
                      PP.newline ppStrm;
                      ppScope stm)
                  | VE.LETPACK(a, b, ty, args, stm) => (
                      inHBox (fn () => (
                        str "let"; sp(); ppVBs [a, b]; sp(); str "="; sp();
                        str "PACK "; str(tyToString ty); sp(); str "@"; sp();
                        ppExps (args, " "); str ")"));
                      PP.newline ppStrm;
                      ppScope stm)
                  | VE.CALL(xs, f, args, stm) => (
                      inHBox (fn () => (
                        str "let"; sp(); ppVBs xs; sp(); str "="; sp();
                        str "CALL"; sp(); atom f; str "("; ppExps(args, ", "); str ")"));
                      PP.newline ppStrm;
                      ppScope stm)
                  | VE.STMT(xs, opcode, args, stm) => (
                      inHBox (fn () => (
                        str "let"; sp(); ppVBs xs; sp(); str "="; sp(); str "(";
                        str(ioToString opcode);
                        if List.null args
                          then ()
                          else (sp(); str "@"; List.app (fn e => (sp(); ppExp e)) args);
                        str ")"));
                      PP.newline ppStrm;
                      ppScope stm)
                  | VE.IF(xs, cond, trueS, falseS, cont) => (
                      PP.openVBox ppStrm indent2;
                        inHBox (fn () => (
                          str "let"; sp(); ppVBs xs; sp(); str "="; sp();
                          str "if"; sp(); ppExp cond; sp(); str "then"));
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
                  | VE.RET es => inHBox (fn () => (
                      str "RET"; sp(); str "("; ppExps (es, ", "); str ")"))
                  | VE.EXIT => str "EXIT"
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

    fun ppFunc (ppStrm, VE.FUNC(label, params, resultTys, body)) = let
	  val str = PP.string ppStrm
	  val atom = PP.string ppStrm o Atom.toString
          val ppVB = str o vb2s
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
                str ")"; sp(); str ":"; sp();
		ppList {pp = fn ty => str(VCode.tyToString ty), sep = fn () => str " ", l = resultTys}));
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
          fun pp (ppStrm, prog) = List.app (fn f => (ppFunc (ppStrm, f); PP.newline ppStrm)) prog
          in
            outputWithStrm pp
          end

    fun prStm stm = outputStm (TextIO.stdOut, stm)
    fun prFunc f = outputFunc (TextIO.stdOut, f)
    fun prProg prog = outputProg (TextIO.stdOut, prog)

  end
