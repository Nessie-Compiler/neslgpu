(* ast.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Nesl abstract syntax.
 *)

structure AST =
  struct

    type 'a mark = 'a Error.mark

    type tyvar = NeslTypes.tyvar
    type dataty = NeslTypes.dataty
    type ty = NeslTypes.ty
    type funct = Funct.funct
    type var = Var.var

    datatype program
      = Program of toplevel mark list

    and toplevel
      = TopFun of funct * pat * exp
      | TopPrimFun of funct * exp		(* exp is either ExpBaseTypecase or ExpPolyTypecase *)
      | TopData of dataty
      | TopBind of pat * exp
      | TopExp of exp * ty

    and pat
      = PatMark of pat mark
      | PatPair of pat * pat
      | PatCons of dataty * ty list * pat
      | PatVector of pat * ty *ty		(* types are type of inner sequence and outer type *)	
      | PatVar of var
      | PatError				(* placeholder for erroneous patterns *)

  (* some expression constructors (e.g., ExpApplyForEach) have a type argument; these specify
   * the _result_ type of the expression.
   *)
    and exp
      = ExpMark of exp mark
      | ExpPair of exp * exp
      | ExpIf of exp * exp * exp
      | ExpLet of bind list * exp
      | ExpApplyForEach of exp * bind list * exp option * ty
      | ExpTime of exp * ty			(* time the expression *)
      | ExpApply of funct * ty list ref * exp	(* application includes operators and instantiation of *)
						(* type arguments.  We use a ref here to allow patching *)
						(* of recursive function applications. *)
      | ExpApplyVar of var * exp		(* application of function parameter *)
      | ExpCons of dataty * ty list * exp
      | ExpSeqRange of exp * exp * exp
      | ExpSeq of exp list * ty			
      | ExpVar of var
      | ExpInt of IntInf.int
      | ExpFloat of string
      | ExpBool of bool
      | ExpString of string
      | ExpChar of char
      | ExpError				(* placeholder for erroneous expressions *)
    (* primitive expressions *)
      | ExpBaseTypecase of tyvar * (typat * tycase_rhs) list
      | ExpPolyTypecase of tyvar * tycase_rhs * tycase_rhs * tycase_rhs option
      | ExpPureApply of Pure.pure * exp * ty
      | ExpCmdApply of Cmd.cmd * exp * ty
      | ExpVector of exp * ty * ty		(* types are type of inner sequence and outer type *)

  (* rhs of clause in a typecase; this records the function parameters and a fresh
   * copy of the function's type for when this clause is chosen.
   *)
    and tycase_rhs = TyCaseRHS of (pat * exp * NeslTypes.scheme)

    and typat
      = TyPatBase of TypeBase.ty		(* match base type *)
      | TyPatFun				(* match function type *)

    and bind = Bind of (pat * exp) mark

  end
