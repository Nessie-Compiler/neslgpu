(* nesl.lex
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

%name NeslLex;

%arg (lexErr);

%defs(

    structure T = NeslTokens

    type lex_result = T.token

    fun eof () = T.EOF
);

%let digit = [0-9];
%let num = {digit}+;
%let letter = [a-zA-Z];
%let idchar = {letter}|{digit}|"_";
%let id = {letter}{idchar}*;
%let ws = " "|[\t\n\v\f\r];

(* whitespace *)
{ws}+	=> (skip());

(* comments *)
"%"[^%]*"%"	=> (skip());

(* punctuation *)
":"			=> (T.COLON);
"::"			=> (T.DCOLON);
","			=> (T.COMMA);
";"			=> (T.SEMICOLON);
"$"			=> (T.DOLLAR);
"|"			=> (T.BAR);
"="			=> (T.EQ);
"=="			=> (T.DEQ);
"("			=> (T.LPAREN);
")"			=> (T.RPAREN);
"{"			=> (T.LBRACE);
"}"			=> (T.RBRACE);
"["			=> (T.LBRACKET);
"]"			=> (T.RBRACKET);
"[:"			=> (T.BASE_LBRACKET);
":]"			=> (T.BASE_RBRACKET);
"/="			=> (T.SLASHEQ);
"<"			=> (T.LT);
"<="			=> (T.LTE);
">"			=> (T.GT);
">="			=> (T.GTE);
"+"			=> (T.PLUS);
"-"			=> (T.MINUS);
"++"			=> (T.DPLUS);
"<-"			=> (T.LARROW);
"->"			=> (T.RARROW);
"*"			=> (T.STAR);
"/"			=> (T.SLASH);
"^"			=> (T.HAT);
"#"			=> (T.HASH);
"@"			=> (T.AT);
"__base_typecase"	=> (T.KW_base_typecase);
"__poly_typecase"	=> (T.KW_poly_typecase);
"__prim"		=> (T.KW_prim);
"__vector"		=> (T.KW_vector);
"_"			=> (T.WILD);

"`=`"			=> (T.OPID(Atom.atom "="));
"`==`"			=> (T.OPID(Atom.atom "=="));
"`/=`"			=> (T.OPID(Atom.atom "/="));
"`<`"			=> (T.OPID(Atom.atom "<"));
"`<=`"			=> (T.OPID(Atom.atom "<="));
"`>`"			=> (T.OPID(Atom.atom ">"));
"`>=`"			=> (T.OPID(Atom.atom ">="));
"`+`"			=> (T.OPID(Atom.atom "+"));
"`-`"			=> (T.OPID(Atom.atom "-"));
"`++`"			=> (T.OPID(Atom.atom "++"));
"`<-`"			=> (T.OPID(Atom.atom "<-"));
"`->`"			=> (T.OPID(Atom.atom "->"));
"`*`"			=> (T.OPID(Atom.atom "*"));
"`/`"			=> (T.OPID(Atom.atom "/"));
"`^`"			=> (T.OPID(Atom.atom "^"));
"`and`"			=> (T.OPID(Atom.atom "and"));
"`nand`"		=> (T.OPID(Atom.atom "nand"));
"`nor`"			=> (T.OPID(Atom.atom "nor"));
"`or`"			=> (T.OPID(Atom.atom "or"));
"`xor`"			=> (T.OPID(Atom.atom "xor"));
"`#`"			=> (T.OPID(Atom.atom "#"));
"`@`"			=> (T.OPID(Atom.atom "@"));

(* keywords and identifiers; note that NESL is case insensitive, so we map to lower case *)
{id}	=> (Keywords.lookup(CharVector.map Char.toLower yytext));

(* literals *)
{num}	=> (T.INTCONST(valOf(IntInf.fromString yytext)));
[0-9]*\.{num}([eE][-+]?{num})?
		=> (T.FLOATCONST yytext);
{num}\.	=> (T.FLOATCONST(yytext ^ "0"));
\"[^\"]*\"	=> (T.STRINGCONST(Substring.string(
		      Substring.triml 1 (Substring.trimr 1 yysubstr))));
"`"[\033-\126]  => (T.CHARCONST(String.sub(yytext, 1)));

(* anything else *)
.	=> (lexErr(yypos, ["bad character \"", String.toString yytext, "\""]);
	    continue());
