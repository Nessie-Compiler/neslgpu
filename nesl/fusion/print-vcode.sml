(* print-vcode.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Print VCode representation
 *)

structure PrintVCode : sig

    val outputProg : TextIO.outstream * VCode.func list -> unit
    val outputFunc : TextIO.outstream * VCode.func -> unit

    val prProg : VCode.func list -> unit
    val prFunc : VCode.func -> unit

  end = struct

    structure VC = VCode
    structure F = Format

    fun pr (outS, s) = TextIO.output(outS, s)
    fun prf (outS, fmt, items) = pr(outS, F.format fmt items)

    fun outputFunc (outS, VC.FUNC(lab, body)) = let
          fun outOp (VC.IF(b1, b2)) = (
                pr (outS, "IF\n");
                outBlk b1;
                pr (outS, "ELSE\n");
                outBlk b2;
                pr (outS, "ENDIF\n"))
            | outOp opcode = prf (outS, "%s\n", [F.STR(VC.toString opcode)])
          and outBlk opcodes = List.app outOp opcodes
          in
            prf (outS, "FUNC %s\n", [F.ATOM lab]);
            outBlk body;
            pr (outS, "RET\n")
          end
          
    fun outputProg (outS, prog) = let
          fun out [] = ()
            | out [f] = outputFunc (outS, f)
            | out (f::r) = (outputFunc (outS, f); pr(outS, "\n"); out r)
          in
            out prog
          end

    fun prProg prog = outputProg (TextIO.stdOut, prog)
    fun prFunc func = outputFunc (TextIO.stdOut, func)

  end
