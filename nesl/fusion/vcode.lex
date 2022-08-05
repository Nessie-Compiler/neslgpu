(* vcode.lex
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

%name VCodeLex;

%arg (lexErr);

%defs(

    structure T = VCodeTokens

  (* some type lex_result is necessitated by ml-ulex *)
    type lex_result = T.token

  (* eof : unit -> lex_result *)
  (* ml-ulex requires this as well *)
    fun eof () = T.EOF
);

%let digit = [0-9];
%let integer = [-+]?{digit}+;
%let float = [-+]?{digit}+\.({digit}+)?([Ee][-+]?{digit}+)?;

"{"[^}]*"}"     => (skip());
[ \t\n]         => (skip());

"("             => (T.LP);
")"             => (T.RP);

{integer}       => (T.NUM(valOf(IntInf.fromString yytext)));
{float}         => (T.REAL yytext);
\"[^"]*\"       => (T.STRING yytext);

<INITIAL>"F"	        => (T.BOOLEAN "F");
<INITIAL>"ELSE"	        => (T.ELSE);
<INITIAL>"ENDIF"        => (T.ENDIF);
<INITIAL>"FCLOSE"	=> (T.FCLOSE);
<INITIAL>"FWRITE"	=> (T.FWRITE);
<INITIAL>"FPERMUTE"	=> (T.FPERMUTE);
<INITIAL>"FUNC"	        => (T.FUNC);
<INITIAL>"WRITE"        => (T.WRITE);
<INITIAL>"-"	        => (T.SUB);
<INITIAL>"FOPEN"        => (T.FOPEN);
<INITIAL>"="	        => (T.EQ);
<INITIAL>"!="	        => (T.NEQ);
<INITIAL>"COS"	        => (T.COS);
<INITIAL>"COPY"	        => (T.COPY);
<INITIAL>"/"	        => (T.DIV);
<INITIAL>"IF"	        => (T.IF);
<INITIAL>"SIN"	        => (T.SIN);
(*<INITIAL>"SPAWN"        => (T.SPAWN);*)
<INITIAL>"STDIN"        => (T.STREAM "STDIN");
<INITIAL>"I_TO_F"	=> (T.I_TO_F);
<INITIAL>"COSH"	        => (T.COSH);
<INITIAL>"MAX_REDUCE"	=> (T.MAX_REDUCE);
<INITIAL>"MIN_REDUCE"	=> (T.MIN_REDUCE);
(*<INITIAL>"MAIN"	        => (T.MAIN);*)
<INITIAL>"SEGDES"	=> (T.SEGDES);
<INITIAL>"<="	        => (T.LTE);
<INITIAL>"MAX_SCAN"	=> (T.MAX_SCAN);
<INITIAL>"MIN_SCAN"	=> (T.MIN_SCAN);
<INITIAL>"NULL_STREAM"	=> (T.STREAM "NULL_STREAM");
<INITIAL>"EXP"	        => (T.EXP);
<INITIAL>"SINH"	        => (T.SINH);
<INITIAL>"PERMUTE"	=> (T.PERMUTE);
<INITIAL>"CPOP"	        => (T.CPOP);
<INITIAL>"MAKE_SEGDES"	=> (T.MAKE_SEGDES);
<INITIAL>"BPERMUTE"	=> (T.BPERMUTE);
<INITIAL>"BFPERMUTE"	=> (T.BFPERMUTE);
<INITIAL>"+_REDUCE"	=> (T.ADD_REDUCE);
<INITIAL>"PACK"	        => (T.PACK);
<INITIAL>"<"	        => (T.LT);
<INITIAL>"CEIL"	        => (T.CEIL);
<INITIAL>"CALL"	        => (T.CALL);
<INITIAL>"+_SCAN"	=> (T.ADD_SCAN);
<INITIAL>">="	        => (T.GTE);
<INITIAL>"*_REDUCE"	=> (T.MUL_REDUCE);
<INITIAL>"%"	        => (T.MOD);
<INITIAL>"FLOOR"        => (T.FLOOR);
<INITIAL>"*_SCAN"	=> (T.MUL_SCAN);
<INITIAL>"REPLACE"	=> (T.REPLACE);
<INITIAL>"LOG"	        => (T.LOG);
<INITIAL>"CHAR"	        => (T.CHAR);
<INITIAL>"FREAD_CHAR"	=> (T.FREAD_CHAR);
<INITIAL>"I_TO_B"	=> (T.I_TO_B);
<INITIAL>"B_TO_I"	=> (T.B_TO_I);
<INITIAL>"LENGTHS"	=> (T.LENGTHS);
<INITIAL>"UNPAIR"	=> (T.UNPAIR);
<INITIAL>"RANK_DOWN"	=> (T.RANK_DOWN);
<INITIAL>"XOR_REDUCE"	=> (T.XOR_REDUCE);
<INITIAL>"LENGTH"	=> (T.LENGTH);
<INITIAL>"FREAD"        => (T.FREAD);
<INITIAL>"POP"	        => (T.POP);
<INITIAL>"DPERMUTE"	=> (T.DPERMUTE);
<INITIAL>"DFPERMUTE"	=> (T.DFPERMUTE);
<INITIAL>"STDERR"	=> (T.STREAM "STDERR");
<INITIAL>"XOR_SCAN"	=> (T.XOR_SCAN);
<INITIAL>"STOP_TIMER"	=> (T.STOP_TIMER);
<INITIAL>"START_TIMER"	=> (T.START_TIMER);
<INITIAL>"AND_REDUCE"	=> (T.AND_REDUCE);
<INITIAL>"ASIN"	        => (T.ASIN);
<INITIAL>"ATAN"	        => (T.ATAN);
<INITIAL>"OR_REDUCE"	=> (T.OR_REDUCE);
<INITIAL>"AND_SCAN"	=> (T.AND_SCAN);
<INITIAL>"+"	        => (T.ADD);
<INITIAL>"SRAND"        => (T.SRAND);
<INITIAL>"BOOL"	        => (T.BOOL);
<INITIAL>"INDEX"        => (T.INDEX);
<INITIAL>"ACOS"	        => (T.ACOS);
<INITIAL>"OR_SCAN"	=> (T.OR_SCAN);
<INITIAL>"EXIT"	        => (T.EXIT);
<INITIAL>"FLOAT"        => (T.FLOAT);
<INITIAL>">"	        => (T.GT);
<INITIAL>"EXTRACT"	=> (T.EXTRACT);
<INITIAL>"PAIR"	        => (T.PAIR);
<INITIAL>"TRUNC"        => (T.TRUNC);
<INITIAL>"CONST"        => (T.CONST);
<INITIAL>"RANK_UP"	=> (T.RANK_UP);
<INITIAL>"NOT"	        => (T.NOT);
<INITIAL>"TAN"	        => (T.TAN);
<INITIAL>"*"	        => (T.MUL);
<INITIAL>"SQRT"	        => (T.SQRT);
<INITIAL>"SELECT"	=> (T.SELECT);
<INITIAL>"STDOUT"	=> (T.STREAM "STDOUT");
<INITIAL>"INT"	        => (T.INT);
<INITIAL>"TANH"	        => (T.TANH);
<INITIAL>"XOR"	        => (T.XOR);
<INITIAL>"RAND"	        => (T.RAND);
<INITIAL>"READ"	        => (T.READ);
<INITIAL>"ROUND"        => (T.ROUND);
<INITIAL>"OR"	        => (T.OR);
<INITIAL>"LSHIFT"	=> (T.LSHIFT);
<INITIAL>"AND"	        => (T.AND);
<INITIAL>"RET"	        => (T.RET);
<INITIAL>"RSHIFT"	=> (T.RSHIFT);
<INITIAL>"DIST"	        => (T.DIST);
<INITIAL>"T"            => (T.BOOLEAN "T");

[-+*/%<>=!._A-Za-z0-9]+
                => (T.LABEL(Atom.atom yytext));

<INITIAL> .		=> (lexErr(yypos, ["bad character `", String.toString yytext]);
			    continue());
