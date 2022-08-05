(* parser.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Parser glue.
 *)

structure Parser : sig

  (* parse a file; return NONE if there are syntax errors *)
    val parseFile : (Error.err_stream * TextIO.instream) -> ParseTree.file option

  end = struct

  (* glue together the lexer and parser *)
    structure NeslParser = NeslParseFn(NeslLex)

  (* error function for lexers *)
    fun lexErr errStrm (pos, msg) = Error.errorAt(errStrm, (pos, pos), msg)

  (* map tokens to strings *)
    fun tokToString (NeslTokens.ID x) = Atom.toString x
      | tokToString (NeslTokens.INTCONST i) = IntInf.toString i
      | tokToString (NeslTokens.FLOATCONST f) = f
      | tokToString (NeslTokens.BOOLCONST false) = "F"
      | tokToString (NeslTokens.BOOLCONST true) = "T"
      | tokToString (NeslTokens.STRINGCONST s) = concat["\"", String.toCString s, "\""]
      | tokToString (NeslTokens.CHARCONST c) = "`" ^ String.str c
      | tokToString tok = NeslTokens.toString tok

  (* error function for parsers *)
    val parseErr = Error.parseError tokToString

  (* parse a file, returning a parse tree *)
    fun parseFile (errStrm, file) = let
	  fun get () = TextIO.input file
	  val lexer = NeslLex.lex (Error.sourceMap errStrm) (lexErr errStrm)
	  in
	    case NeslParser.parse lexer (NeslLex.streamify get)
	     of (SOME pt, _, []) => (
                  TextIO.closeIn file;
                  SOME(ParseTree.File{
                      name = Error.sourceFile errStrm,
                      contents = pt
                    }))
	      | (_, _, errs) => (
		  TextIO.closeIn file;
		  List.app (parseErr errStrm) errs;
		  NONE)
	    (* end case *)
	  end

  end
