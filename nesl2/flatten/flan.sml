(* flan.sml
 *
 * COPYRIGHT (c) 2013 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *
 * Flattened NESL syntax.
 *)


structure Flan =
  struct

    structure P = FlanPure
    structure T = FlanTypes

    type ty = T.ty

    datatype funct = F of {
	name : Atom.atom,
	stamp : Stamp.stamp,
	ty: (ty list * ty) ref,		(* we use a ref here so that arity-raise can change the type *)
	isLifted : bool ref,
	appCnt : int ref,
	props : PropList.holder
      }

  datatype program = Program of toplevel list

  and var_kind
    = VK_Uninitialized           (* Should never hit this during contract *)
    | VK_None
    | VK_Param
    | VK_Let of exp
    | VK_Tuple of atom list      (* Used for tuples *)
    | VK_Seq of atom list

  and var = V of {
      name : Atom.atom,
      stamp : Stamp.stamp,
      ty : ty,
      useCnt : int ref,
      kind : var_kind ref,
      props : PropList.holder
  }

  and toplevel
    (*  = TopFun of funct * var list * exp *)
    = TopFun of Funct.funct * (funct * var list * exp) list
    | TopBind of bind
    | TopExp of exp * ty * PropList.holder (* prop list to hold shape of expression result *)

  and exp
    = ExpForEach of exp * (var * var) list (* Eliminated in flattening pass. *)
    | ExpLet of bind list * exp
    | ExpTuple of var * atom list * exp
    (* All atoms must be scalars! *)
    | ExpSeq of var * atom list * exp
    (* Tail forms *)
    | ExpPure of P.pure * atom list
    | ExpCmd of Cmd.cmd * atom list
    | ExpIf of atom * exp * exp * ty
    | ExpApplyFun of funct * atom list
    | ExpAtom of atom
    | ExpLifted of exp * (var * var) list (* parallel map over
					   * lambda-lifted sequential expressions 
					   * and the vectors they're bound to 
					   * for vectorization avoidance *)

  and atom
    = Var of var
    | Int of IntInf.int
    | Float of string
    | Bool of bool
    | String of string
    | Char of char

  withtype bind = (var * exp)

  (* Get and set lifted function from base function's PropList *)
    fun propsOf (F{props, ...}) = props
(*
    val {getFn=getL, setFn=setL, peekFn=peekL, ...} = PropList.newProp(
	  propsOf,
	  fn _ => F{
	      name=Atom.atom "init",
	      stamp = Stamp.new(),
	      ty = ref([T.tyInt], T.tyInt),
	      isLifted=false,
	      appCnt=ref 0,
	      props = PropList.newHolder()
	    })

    val getLifted = peekL
    val setLifted = setL
*)
(*
    val {getFn=getFunct, setFn=setFunct, peekFn=peekFunct, ...} = PropList.newProp(
	  propsOf,
	  fn _ => Funct.new(Atom.atom "init", NeslTypes.Scheme([], NeslTypes.tyInt, NeslTypes.tyInt)))
*)

(*
  val {getFn=getToDelete, setFn=setToDelete, peekFn=peekToDelete, ...} =
      PropList.newProp(propsOf, (fn _ => false))

  fun markToDelete f = setToDelete(f, true)
*)
  (* This property tells us whether a function has been inlined, so we can
   * distinguish between inlined and recursive dead functions
   *)
(*
  val {getFn=getInlined, setFn=setInlined, peekFn=peekInlined, ...} =
      PropList.newProp(propsOf, (fn _ => false))

  val isInlined = getInlined
  fun markInlined f = setInlined(f, true)

  val {getFn=getOkToRecurse, setFn=setOkToRecurse, peekFn=peekOkToRecurse, ...} =
      PropList.newProp(propsOf, (fn _ => true))

  val canRecurse = getOkToRecurse
  fun dontRecurse f = setOkToRecurse(f, false)
  fun doRecurse f = setOkToRecurse(f, true)
*)

  (* Convenience functions *)

  (* Deal with variable bindings *)
    fun kindOf (V{kind, ...}) = !kind
    fun setKind (V{kind, ...}, b) = kind := b

  (* let v = exp *)
    fun setLetKind (v, exp) = setKind(v, VK_Let exp)

  (* smart constructors *)
  fun mkForEach(e, binds) = ExpForEach(e, binds)
  fun mkLet(binds, e) = (List.app setLetKind binds;
			 ExpLet(binds, e))
  fun mkTuple(v, atms, e) = (setKind(v, VK_Tuple atms);
			     ExpTuple(v, atms, e))
  fun mkSeq(v, atms, e) = (setKind(v, VK_Seq atms);
			   ExpSeq(v, atms, e))

  fun mkPure(p, atms) = ExpPure(p, atms)
  fun mkCmd(c, atms) = ExpCmd(c, atms)
  fun mkIf(b, e1, e2, ty) = ExpIf(b, e1, e2, ty)
  fun mkApplyFun(f, atms) = ExpApplyFun(f, atms)
  fun mkLifted(e, vs) = ExpLifted(e, vs)
  fun mkAtom a = ExpAtom a

  end

