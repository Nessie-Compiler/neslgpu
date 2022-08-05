(* normal-var.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure NormalVar : sig

    type var = NormalAST.var

    val new : Atom.atom * NormalTy.ty -> var

    val nameOf : var -> Atom.atom
    val toString : var -> string

    val typeOf : var -> NormalTy.ty

    val same : var * var -> bool
    val hash : var -> word

    val newProp : (var -> 'a) -> {
	    clrFn : var -> unit,
	    getFn : var -> 'a,
	    peekFn : var -> 'a option,
	    setFn : var * 'a -> unit
	  }
    val newFlag : unit -> {getFn : var -> bool, setFn : var * bool -> unit}

    structure Map : ORD_MAP where type Key.ord_key = var
    structure Set : ORD_SET where type Key.ord_key = var
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = var

  end = struct

    datatype var = datatype NormalAST.var

    fun new (name, ty) = V{
	    name = name,
	    stamp = Stamp.new(),
	    props = PropList.newHolder(),
	    ty = ty
	  }

    fun nameOf (V{name, ...}) = name

    fun toString (V{name, stamp, ...}) = Atom.toString name ^ Stamp.toString stamp

    fun typeOf (V{ty, ...}) = ty

    fun compare (V{stamp=s1, ...}, V{stamp=s2, ...}) = Stamp.compare(s1, s2)
    fun same (V{stamp=s1, ...}, V{stamp=s2, ...}) = Stamp.same(s1, s2)
    fun hash (V{stamp, ...}) = Stamp.hash stamp

    fun newProp mkProp = PropList.newProp (fn (V{props, ...}) => props, fn x => mkProp x)
    fun newFlag () = PropList.newFlag (fn (V{props, ...}) => props)

    local
      structure Key = struct
	  type ord_key = var
	  val compare = compare
	end
    in
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)
    end

    structure Tbl = HashTableFn (
      struct
	type hash_key = var
	val hashVal = hash
	val sameKey = same
      end)

  end
