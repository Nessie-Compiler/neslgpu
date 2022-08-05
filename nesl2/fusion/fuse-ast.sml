(* fuse-ast.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * This representation is a bridge between Flan and CuLambda.  Lifted operators are
 * now represented as mapped kernels and pure operations have been split into different
 * categories.
 *)

structure FuseAST =
  struct

  (* scalar operations *)
    datatype scalar_op = datatype ScalarOp.t

  (* reduce operations *)
    datatype reduce_op = datatype ReduceOp.t

  (* scan operations *)
    datatype scan_op = datatype ScanOp.t

  (* vector operations *)
    datatype vector_op = datatype VectorOp.t

  (* generator operations *)
    datatype gen_op = datatype GeneratorOp.t

  (* internal operations, which have no lifted form *)
    datatype internal_op = datatype InternalOp.t

    datatype ty
      = TyScalar of TypeBase.ty		     (* scalars *)
      | TySegdes			     (* segment descriptors *)
      | TySeq of TypeBase.ty * int option    (* sequence with optional length information *)
      | TyTuple of ty list		     (* tuples *)

    datatype funct = F of {
	name : string,
	stamp : Stamp.stamp,
	ty : ty list * ty,
	appCnt : int ref,
	props : PropList.holder
      }

    datatype top
      = Top_Kern of kernel
      | Top_Funct of funct * var list * exp
      | Top_Let of var list * exp
      | Top_RHS of var list * rhs
      | Top_Exp of exp * ty

    and exp
      = Exp_Let of var list * exp * exp
      | Exp_RHS of var list * rhs * exp
      | Exp_If of atom * exp * exp
      | Exp_Apply of funct * atom list
      | Exp_Atom of atom

    and rhs
      = RHS_Scalar of scalar_op * atom list
      | RHS_FlatGen of gen_op * atom * atom list	(* (rator, width, args) *)
      | RHS_SegGen of gen_op * atom * var list		(* (rator, width, args) *)
      | RHS_Map of kernel * atom * var list		(* (kernel, width, args) *)
      | RHS_FlatReduce of reduce_op * atom * var	(* (rator, width, data) *)
      | RHS_SegReduce of reduce_op * atom * var * var	(* (rator, width, segdes, data) *)
      | RHS_FlatScan of scan_op * atom * var		(* (rator, width, data) *)
      | RHS_SegScan of scan_op * atom * var * var	(* (rator, width, segdes, data) *)
      | RHS_FlatVector of vector_op * atom list
      | RHS_SegVector of vector_op * atom list
      | RHS_Internal of internal_op * atom
      | RHS_Cmd of Cmd.cmd * atom list
      | RHS_Seq of atom list * TypeBase.ty (* List may be empty, so we tag with type *)
      | RHS_Tuple of atom list
      | RHS_Proj of int * var

    and kernel = K of {
	name : string,
	stamp : Stamp.stamp,
	(* Question: should we let kernels take/return tuples, or only scalars? *)
	ty : ty list * ty list,		(* note: types are scalars (not sequences!) *)
	params : var list,
	body : kern_exp,
	appCnt : int ref,
	props : PropList.holder
      }

(* Question: how to handle kernels that return multiple vectors?
 * Should the kernel just return a tuple? (Probably.)
 * Should we let it explicitly return multiple values?
 *)
    and kern_exp
      = KExp_Let of var list * kern_exp * kern_exp
      | KExp_Pure of var * scalar_op * atom list * kern_exp
      | KExp_Proj of var * int * var * kern_exp (* v1 = #i v2 in exp *)
      | KExp_Tuple of var * atom list * kern_exp (* v = (v1, ..., vn) in exp *)
    (* One day we will have vectorization avoidance, and this will be useful. *)
      | KExp_If of atom * kern_exp * kern_exp
    (* To return multiple vectors. *)
      | KExp_Return of atom list

    and atom
      = Var of var
      | Bool of bool
      | Char of char
      | Int of IntInf.int
      | Float of string
      | String of string

    and var = V of {
	name : string,
	stamp : Stamp.stamp,
	ty : ty,
	binding : var_bind ref,
	useCnt : int ref,
	props : PropList.holder
      }

    and var_bind
      = VB_None
      | VB_Param
      | VB_Let of (int * int * exp)	(* Top_Let/Exp_Let; args are  (position, rhs arity, rhs) *)
      | VB_RHS of (int * int * rhs)	(* Top_RHS/Exp_RHS; args are  (position, rhs arity, rhs) *)
      | VB_KParam                       (* Kernel parameter *)
      | VB_KLet of (int * int * kern_exp)
      | VB_KPure of (scalar_op * atom list)
      | VB_KProj of (int * var)
      | VB_KTuple of atom list

    datatype program = Program of top list

  (* Smart constructors *)
    local
      fun bindingOf (V{binding, ...}) = !binding
      fun setBinding (V{binding, ...}, b) = binding := b
    in

  (* Top level *)
    fun mkTopKern (k as K{params, ...}) = (
	  List.app (fn v => setBinding(v, VB_KParam)) params;
	  Top_Kern k)
    fun mkTopFun (f, vars, e) = (
	  List.app (fn v => setBinding (v, VB_Param)) vars;
	  Top_Funct(f, vars, e))
    fun mkTopLet (vs, e) = let
	  val arity = List.length vs
	  fun setK i = setBinding(List.nth(vs, i), VB_Let(i + 1, arity, e))
	  in
	    List.tabulate(arity, setK);
	    Top_Let(vs, e)
	  end
    fun mkTopRHS (vs, rhs) = let
	  val arity = List.length vs
	  fun setK i = setBinding(List.nth(vs, i), VB_RHS(i + 1, arity, rhs))
	  in
	    List.tabulate(arity, setK);
	    Top_RHS(vs, rhs)
	  end
    fun mkTopExp arg = Top_Exp arg

  (* Exp *)
    fun mkLet(vs, e, tailExp) = let
	  val arity = List.length(vs)
	  fun setK i = setBinding(List.nth(vs, i), VB_Let(i + 1, arity, e))
	  in
	    List.tabulate(arity, setK);
	    Exp_Let(vs, e, tailExp)
	  end
    fun mkRHS(vs, rhs, tailExp) = let
	  val arity = List.length(vs)
	  fun setK i = setBinding(List.nth(vs, i), VB_RHS(i + 1, arity, rhs))
	  in
	    List.tabulate(arity, setK);
	    Exp_RHS(vs, rhs, tailExp)
	  end
    fun mkIf(a, e1, e2) = Exp_If(a, e1, e2)
    fun mkApply(f, atms) = Exp_Apply(f, atms)
    fun mkAtom(atm) = Exp_Atom atm

  (* Rhs *)
    fun mkScalar arg = RHS_Scalar arg
    fun mkFlatGen arg = RHS_FlatGen arg
    fun mkSegGen arg = RHS_SegGen arg
    fun mkMap arg = RHS_Map arg
    fun mkFlatReduce arg = RHS_FlatReduce arg
    fun mkSegReduce arg = RHS_SegReduce arg
    fun mkFlatScan arg = RHS_FlatScan arg
    fun mkSegScan arg = RHS_SegScan arg
    fun mkFlatVector arg = RHS_FlatVector arg
    fun mkSegVector arg = RHS_SegVector arg
    fun mkInternal arg = RHS_Internal arg
    fun mkCmd arg = RHS_Cmd arg
    fun mkTuple arg = RHS_Tuple arg
    fun mkSeq arg = RHS_Seq arg
    fun mkProj arg = RHS_Proj arg

  (* kern_exp *)
    fun mkKLet (vs, e, tailExp) = let
	  val arity = List.length(vs)
	  fun setK i = setBinding(List.nth(vs, i), VB_KLet(i + 1, arity, e))
	  in
	    List.tabulate(arity, setK);
	    KExp_Let(vs, e, tailExp)
	  end
    fun mkKPure (x, s, atms, e) = (
	  setBinding(x, VB_KPure(s, atms));
	  KExp_Pure(x, s, atms, e))
    fun mkKProj (v1, i, v2, tailExp) = (
	  setBinding(v1, VB_KProj(i, v2));
	  KExp_Proj(v1, i, v2, tailExp))
    fun mkKTuple (v, atms, tailExp) = (
	  setBinding(v, VB_KTuple atms);
	  KExp_Tuple(v, atms, tailExp))
    fun mkKIf (atm, e1, e2) = KExp_If(atm, e1, e2)
    fun mkKReturn atms = KExp_Return atms
    end (* local *)

  end
