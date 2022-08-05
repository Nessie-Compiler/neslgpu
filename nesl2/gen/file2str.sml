(* file2str.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Convert a file to a string embedded in an SML module.
 *)

structure File2Str : sig

    val main : string * string list -> OS.Process.status

    val doit : string * string * string -> unit

  end = struct

    structure F = Format

    val header = "\
          \(* %s\n\
          \ *\n\
          \ * COPYRIGHT (c) 2014 John Reppy and Nora Sandler\n\
          \ * All rights reserved.\n\
          \ *\n\
          \ * Generated from %s\n\
          \ *)\n\
          \\n\
          \structure %s =\n\
          \  struct\n\
          \\n\
          \"

    val trailer = "\
          \\n\
          \  end\n\
          \"

    fun load srcFile = let
          val inS = TextIO.openIn srcFile
          fun lp l = (case TextIO.inputLine inS
                 of NONE => List.rev l
                  | SOME ln => lp(ln::l)
                (* end case *))
          in
            (lp [] before TextIO.closeIn inS)
              handle ex => (TextIO.closeIn inS; raise ex)
          end

    fun doit (srcFile, module, outFile) = let
          val text = load srcFile
          val outS = TextIO.openOut outFile
          fun prf (fmt, items) = TextIO.output(outS, F.format fmt items)
          in
            prf (header, [F.STR(OS.Path.file outFile), F.STR srcFile, F.STR module]);
            prf ("    val text = \"\\\n", []);
            List.app (fn ln => prf("          \\%s\\\n", [F.STR(String.toString ln)])) text;
            prf ("          \\\"\n", []);
            prf (trailer, []);
            TextIO.closeOut outS
          end

    fun main (_, [srcFile, module, dstFile]) = (
          (doit(srcFile, module, dstFile); OS.Process.success) handle _ => OS.Process.failure)
      | main _ = (
          TextIO.output(TextIO.stdErr, "usage:\n");
          TextIO.output(TextIO.stdErr, "    file2str srcfile module outfile\n");
          OS.Process.failure)

  end
