/*
* Copyright (c) 1992, 1993, 1994, 1995 Carnegie Mellon University 
*/

#ifndef _PARSE_H
#define _PARSE_H 1

/* declarations needed for parsers */
#include <stdio.h>

/* need this to store CONSTANT values */
typedef union {
	int ival;
	double dval;
	int bval;
    } const_union;

/* defined by yacc */
extern FILE 	*yyin;
extern int 	yylex();
extern int	yylineno;
extern int 	yyparse();

/* Different versions of yacc declare yytext with different types */
#ifdef FLEX_SCANNER
  int yylineno;
  typedef char *YYTEXT_T;
#else
#if __PARAGON__
  typedef unsigned char YYTEXT_T [200];
#else
#if __hpux | _AIX | __osf__
  typedef unsigned char YYTEXT_T[];

#else
  typedef char YYTEXT_T[];
#endif
#endif
#endif

extern YYTEXT_T yytext;

#ifndef YY_TYPEDEF_YY_SIZE_T
#define YY_TYPEDEF_YY_SIZE_T
typedef size_t yy_size_t;
#endif

#ifdef __linux
extern int 	yyleng;
#else
extern yy_size_t 	yyleng;
#endif

/* defined in grammar.yy */
extern int      parse_error;            /* set to true if error during parse */
extern const_union constant;		/* CONSTANT values */
extern char     current_fn[];		/* last fn name read by lexer */
extern char     main_fn[];   		/* name of top level fn */
extern int      main_defined;           /* Has the top level fn been defined? */
extern int      first_function;         /* Is this the first fn in the file? */

/* defined in actions.c */
extern void prog_init PROTO_((void));
extern TYPE type PROTO_((int));
extern void parse_stmt PROTO_((VOPCODE, TYPE));
extern void parse_fused PROTO_((VOPCODE, int));
extern void parse_label PROTO_((char*));
extern void parse_call PROTO_((char*));
extern void parse_stack PROTO_((VOPCODE, int, int));
extern void parse_const PROTO_((VOPCODE, TYPE));
extern void parse_begin_vec PROTO_((TYPE));
extern void parse_val_vec PROTO_((TYPE));
extern void parse_end_vec PROTO_((void));
extern void parse_string PROTO_((YYTEXT_T, int));
extern void parse_if PROTO_((void));
extern void parse_else PROTO_((void));
extern void parse_endif PROTO_((void));
extern void parse_return PROTO_((void));

#endif /* _PARSE_H */
