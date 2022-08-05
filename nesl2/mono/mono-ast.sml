(* mono-ast.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Monomorphic Nesl abstract syntax.
 *
 * NOTE: we could get rid of the datatypes, since they can be replaced by tuples (they
 * are not recursive).
 *)

structure MonoAST =
  struct

    datatype ty = datatype MonoTy.ty

    datatype funct = F of {
	name : Atom.atom,
	stamp : Stamp.stamp,
	props : PropList.holder,
	ty : ty * ty,
	inst : Funct.funct * ty list
      }

    datatype var = V of {
	name : Atom.atom,
	stamp : Stamp.stamp,
	ty : ty
      }

    datatype program = Program of toplevel list		(* the specialized program *)

    and toplevel
      = TopFun of Funct.funct * (funct * pat * exp) list
      | TopBind of bind
      | TopExp of exp * ty

    and pat
      = PatPair of pat * pat
      | PatVector of pat * pat * ty
      | PatVar of var

    and exp
      = ExpPair of exp * exp
      | ExpIf of exp * exp * exp
      | ExpLet of bind list * exp
      | ExpApplyForEach of exp * bind list * ty
      | ExpApply of funct * exp		(* application includes operators and instantiation *)
					(* of type arguments *)
      | ExpSeqRange of exp * exp * exp
      | ExpSeq of exp list * ty
      | ExpVar of var
      | ExpInt of IntInf.int
      | ExpFloat of string
      | ExpBool of bool
      | ExpString of string
      | ExpChar of char
      | ExpPureApply of Pure.pure * exp * ty
      | ExpCmdApply of Cmd.cmd * exp * ty
      | ExpVector of exp * exp * ty	(* segdes, sequence, outer type *)

    withtype bind = (pat * exp)

  end
