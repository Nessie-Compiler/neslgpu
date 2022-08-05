(* parser.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Parser glue.
 *)

structure Parser : sig

    exception ERROR

  (* parse a file; return NONE if there are syntax errors *)
    val parseFile : string -> VCode.program

  end = struct

  (* glue together the lexer and parser *)
    structure VCodeParser = VCodeParseFn(VCodeLex)

  (* error function for lexers *)
    fun lexErr errStrm (pos, msg) = Error.errorAt(errStrm, (pos, pos), msg)

  (* map tokens to strings *)
    fun tokToString (VCodeTokens.LABEL x) = Atom.toString x
      | tokToString (VCodeTokens.NUM i) = IntInf.toString i
      | tokToString (VCodeTokens.REAL f) = f
      | tokToString (VCodeTokens.BOOLEAN b) = b
      | tokToString (VCodeTokens.STRING s) = s
      | tokToString (VCodeTokens.STREAM s) = s
      | tokToString tok = VCodeTokens.toString tok

  (* error function for parsers *)
    val parseErr = Error.parseError tokToString

  (* parse a file, returning a parse tree *)
    fun parser (errStrm, file) = let
	  fun get () = TextIO.input file
	  val lexer = VCodeLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
	  in
	    case VCodeParser.parse lexer (VCodeLex.streamify get)
	     of (SOME pt, _, []) => (TextIO.closeIn file; SOME pt)
	      | (_, _, errs) => (
		  TextIO.closeIn file;
		  List.app (parseErr errStrm) errs;
		  NONE)
	    (* end case *)
	  end

    fun err s = TextIO.output (TextIO.stdErr, s)
    fun err1 c =  TextIO.output1 (TextIO.stdErr, c)
    fun errnl s = (err s; err1 #"\n")

    exception ERROR

    fun quitWithError srcFile = raise Fail("Error in compiling " ^ srcFile)

  (* check for errors and report them if there are any *)
    fun checkForErrors errStrm = (
	  Error.report (TextIO.stdErr, errStrm);
	  if Error.anyErrors errStrm
	    then quitWithError (Error.sourceFile errStrm)
	    else ())

  (* compiler front end (parsing, typechecking, and simplification *)
    fun parseFile filename = let
	  val errStrm = Error.mkErrStream filename
	  val _ = if OS.FileSys.access(filename, [OS.FileSys.A_READ])
		then ()
		else (
		  err(concat["source file \"", filename, "\" does not exist or is not readable\n"]);
		  raise ERROR)
          val inS = TextIO.openIn filename
          val pt = parser (errStrm, inS)
          in
            TextIO.closeIn inS;
            checkForErrors errStrm;
            valOf pt
          end

  end
