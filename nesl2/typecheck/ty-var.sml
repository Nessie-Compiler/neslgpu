(* ty-var.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure TyVar :> sig

    type tyvar = NeslTypes.tyvar

  (* create a new type variable *)
    val new : Atom.atom * TypeBase.class -> tyvar

  (* create a fresh instance of a variable *)
    val fresh : tyvar -> tyvar

    val nameOf : tyvar -> Atom.atom

    val classOf : tyvar -> TypeBase.class

    val toString : tyvar -> string

  (* return true if two type variables are the same (i.e., have the same stamp) *)
    val same : tyvar * tyvar -> bool

  (* set the kind field of a type variable (used to mark TYPARAMs *)
    val setKind : tyvar * NeslTypes.kind -> unit

  (* resolve a type variable to its instantiation (or itself, if uninstantiated) *)
    val resolve : tyvar -> NeslTypes.ty

  (* finite sets of type variables *)
    structure Set : ORD_SET where type Key.ord_key = NeslTypes.tyvar

  (* finite maps on type variables *)
    structure Map : ORD_MAP where type Key.ord_key = NeslTypes.tyvar

  end = struct

    datatype tyvar = datatype NeslTypes.tyvar

  (* create a new type variable *)
    fun new (name, cls) = TV{name = name, stamp = Stamp.new(), cls = cls, inst = ref NeslTypes.UNIV}

    fun fresh (TV{name, cls, ...}) = new (name, cls)

    fun nameOf (TV{name, ...}) = name

    fun classOf (TV{cls, ...}) = cls

    val toString = NeslTypes.tyvarToString

  (* return true if two type variables are the same (i.e., have the same stamp) *)
    fun same (TV{stamp=a, ...}, TV{stamp=b, ...}) = Stamp.same(a, b)

  (* mark a type variable as a typecase parameter *)
    fun setKind (TV{inst, ...}, k) = inst := k

    fun resolve (tv as TV{inst, ...}) = (case !inst
	   of NeslTypes.UNIV => NeslTypes.TyVar tv
	    | NeslTypes.TYPARAM => NeslTypes.TyVar tv
	    | NeslTypes.TY(NeslTypes.TyVar tv) => resolve tv
	    | NeslTypes.TY ty => ty
	  (* end case *))

    structure Set = RedBlackSetFn (
      struct
	type ord_key = tyvar
	fun compare (TV{stamp = a, ...}, TV{stamp = b, ...}) = Stamp.compare(a, b)
      end)

    structure Map = RedBlackMapFn (
      struct
	type ord_key = tyvar
	fun compare (TV{stamp = a, ...}, TV{stamp = b, ...}) = Stamp.compare(a, b)
      end)

  end
