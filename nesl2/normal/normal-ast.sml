(* normal-ast.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Normalized monomorphic abstract syntax for Nesl
 *)

structure NormalAST =
  struct

    datatype ty = datatype NormalTy.ty

    datatype funct = F of {
	name : Atom.atom,
	stamp : Stamp.stamp,
	props : PropList.holder,
	ty : ty * ty
      }

    datatype var = V of {
	name : Atom.atom,
	stamp : Stamp.stamp,
	props : PropList.holder,
	ty : ty
      }

    datatype program = Program of toplevel list

    and toplevel
      = TopFun of Funct.funct * (funct * var list * exp) list
      | TopBind of bind
      | TopExp of exp * ty

    and exp = Exp of (PropList.holder * exp_nd)

    and exp_nd
      = ExpLet of bind * exp                    (* let (x1, ..., xn) = e in e' *)
      | ExpTuple of var * atom list * exp       (* let x = (y1, ..., yn) in e *)
      | ExpLetVector of var * var * atom * exp	(* let __vector(x, y) = v in e *)
      | ExpVector of var * atom * atom * exp	(* let x = __vector(y1, y2) in e *)
      | ExpSeq of var * atom list * exp         (* let x = [y1, ..., yn] in e *)
      | ExpSeqRange of var * atom * atom * atom * exp
                                                (* let x = [y1 : y2 : y3] in e *)
      | ExpPure of var * Pure.pure * atom list * exp
                                                (* let x = p(y1, ..., yn) in e *)
      | ExpCmd of var list * Cmd.cmd * atom list * exp
                                                (* let (x1, ..., xn) = cmd(y1, ..., yn) in e *)
    (* tail forms *)
      | ExpApply of funct * atom list           (* f (x1, ..., xn) *)
      | ExpForEach of exp * (var * var) list    (* { e | y in ys, z in zs ... } *)
      | ExpIf of atom * exp * exp               (* if x then e1 else e2 *)
      | ExpAtom of atom

    and atom
      = Var of var
      | Int of IntInf.int
      | Float of string
      | Bool of bool
      | String of string
      | Char of char

    withtype bind = (var * exp)

    fun mkExp nd = Exp(PropList.newHolder(), nd)
    fun mkLet (x, e1, e2) = mkExp(ExpLet((x, e1), e2))
    fun mkTuple arg = mkExp(ExpTuple arg)
    fun mkLetVector arg = mkExp(ExpLetVector arg)
    fun mkVector arg = mkExp(ExpVector arg)
    fun mkPair (x, a, b, e) = mkTuple(x, [a, b], e)
    fun mkSeq arg = mkExp(ExpSeq arg)
    fun mkSeqRange arg = mkExp(ExpSeqRange arg)
    fun mkPure arg = mkExp(ExpPure arg)
    fun mkCmd arg = mkExp(ExpCmd arg)
    fun mkApply arg = mkExp(ExpApply arg)
    fun mkForEach arg = mkExp(ExpForEach arg)
    fun mkIf arg = mkExp(ExpIf arg)
    fun mkAtom arg = mkExp(ExpAtom arg)
    fun mkVar x = mkAtom(Var x)

    fun mkSelect (y, path, x, body) = let
	  fun sel ([i], rhs) = mkPure(y, Pure.PROJ i, [Var rhs], body)
	    | sel (i::path, rhs as V{ty=TyTuple tys, ...}) = let
		val x = V{
			name = Atom.atom("_t"^Int.toString i),
			stamp = Stamp.new(),
			props = PropList.newHolder(),
			ty = List.nth(tys, i)
		      }
		in
		  mkPure(x, Pure.PROJ i, [Var rhs], sel (path, x))
		end
	  in
	    sel (path, x)
	  end

  end
