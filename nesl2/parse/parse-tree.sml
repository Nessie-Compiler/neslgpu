(* parse-tree.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure ParseTree =
  struct

    type 'a mark = 'a Error.mark

    type id = Atom.atom
    type binop = Atom.atom
    type unop = Atom.atom

    datatype program = Program of file list

    and file
      = File of {
            name : string,
            contents : toplevel mark list
          }

    and toplevel
      = TopFun of id * pat * funty option * exp
      | TopPrimFun of id * pat * funty * exp
      | TopData of id * tydef
      | TopBind of pat * exp
      | TopExp of exp

    and funty = TyFun of (ty * ty * (id * TypeBase.class) mark list) mark

    and tydef = TyDef of (ty * (id * TypeBase.class) mark list) mark

    and ty
      = TyMark of ty mark
      | TyId of id * ty list	(* base type, type variable, or named typed with optional args *)
      | TyPair of ty * ty	(* may want to generalize to tuples *)
      | TySeq of ty
      | TyBaseSeq of id		(* unsegmented sequence of base type *)

    and pat
      = PatMark of pat mark
      | PatPair of pat * pat	(* may want to generalize to tuples *)
      | PatCons of id * pat
      | PatVector of pat	(* __vector(pattern) *)
      | PatVar of id
      | PatWild

    and exp
      = ExpMark of exp mark
      | ExpPair of exp * exp	(* may want to generalize to tuples *)
      | ExpIf of exp * exp * exp
      | ExpLet of bind list * exp
      | ExpApplyForEach of exp * bind list * exp option
      | ExpBinary of exp * binop * exp
      | ExpUnary of unop * exp
      | ExpSubscript of exp * exp
      | ExpTime of exp
      | ExpApply of id * exp
      | ExpSeqEmpty of ty
      | ExpSeqRange of exp * exp * exp option
      | ExpSeq of exp list
      | ExpParen of exp		(* '(' exp ')'; used to resolve ambiguity *)
      | ExpVar of id
      | ExpInt of IntInf.int
      | ExpFloat of string
      | ExpBool of bool
      | ExpString of string
      | ExpChar of char
  (* special forms only found in primitive basis functions *)
      | ExpBaseTypecase of id * (id * exp) mark list
      | ExpPolyTypecase of id * exp * exp * exp option
      | ExpPrimApply of string * exp	(* '__prim' STRING '(' exp ')' *)
      | ExpVector of exp		(* '__vector' '(' exp ')' *)

    and bind = Bind of (pat * exp) mark

(*
    and binop
      = BinOpOr | BinOpNor | BinOpXor
      | BinOpAnd | BinOpNand
      | BinOpEq | BinOpNeq | BinOpLt | BinOpGt | BinOpLte | BinOpGte
      | BinOpAdd | BinOpSub | BinOpConcat | BinOpWrite (* "<-" *)
      | BinOpMul | BinOpDiv | BinOpGet (* "->" *) | BinOpPad (* "||" *)
      | BinOpPow

    and unop
      = UnOpLen | UnOpNeg | UnOpToString (* "@" *)
*)

    val binOpOr = Atom.atom "or"
    val binOpNor = Atom.atom "nor"
    val binOpXor = Atom.atom "xor"
    val binOpAnd = Atom.atom "and"
    val binOpNand = Atom.atom "nand"
    val binOpEq = Atom.atom "="
    val binOpEqEq = Atom.atom "=="
    val binOpNeq = Atom.atom "/="
    val binOpLt = Atom.atom "<"
    val binOpGt = Atom.atom ">"
    val binOpLte = Atom.atom "<="
    val binOpGte = Atom.atom ">="
    val binOpAdd = Atom.atom "+"
    val binOpSub = Atom.atom "-"
    val binOpConcat = Atom.atom "++"
    val binOpWrite = Atom.atom "<-"
    val binOpMul = Atom.atom "*"
    val binOpDiv = Atom.atom "/"
    val binOpGet = Atom.atom "->"
    val binOpPad = Atom.atom "||"
    val binOpPow = Atom.atom "^"

    val unOpLen = Atom.atom "#"
    val unOpNeg = Atom.atom "negate"
    val unOpToString = Atom.atom "@"

  end
