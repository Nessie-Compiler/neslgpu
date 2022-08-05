(* keywords.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Keywords : sig

    val lookup : string -> NeslTokens.token

  end = struct

    structure T = NeslTokens

    val lookup = let
	  val tbl = AtomTable.mkTable (64, Fail "Keywords.lookup")
	  fun ins (id, tok) = AtomTable.insert tbl (Atom.atom id, tok)
	  fun look id = let
		val id = Atom.atom id
		in
		  case AtomTable.find tbl id
		   of SOME tok => tok
		    | NONE => T.ID id
		  (* end case *)
		end
	  in
	  (* initialize the table *)
	    List.app ins [
		("and",		T.KW_and),
		("any",		T.KW_any),
		("datatype",	T.KW_datatype),
		("else",	T.KW_else),
		("f",		T.BOOLCONST false),
		("F",		T.BOOLCONST false),
		("function",	T.KW_function),
		("if",		T.KW_if),
		("in",		T.KW_in),
		("let",		T.KW_let),
		("logical",	T.KW_logical),
		("nand",	T.KW_nand),
		("nor",		T.KW_nor),
		("number",	T.KW_number),
		("or",		T.KW_or),
		("ordinal",	T.KW_ordinal),
		("then",	T.KW_then),
		("time",	T.KW_time),
		("t",		T.BOOLCONST true),
		("T",		T.BOOLCONST true),
		("xor",		T.KW_xor)
	      ];
	    look
	  end

  end
