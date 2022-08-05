(* flan-var.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure FlanVar : sig

    type var = Flan.var

    val new : string * FlanTypes.ty -> var

    val copy : var -> var

    val nameOf : var -> Atom.atom
    val toString : var -> string

    val typeOf : var -> FlanTypes.ty

    val same : var * var -> bool
    val hash : var -> word

    val newProp : (var -> 'a) -> {
		  clrFn : var -> unit,
		  getFn : var -> 'a,
		  peekFn : var -> 'a option,
		  setFn : var * 'a -> unit
		  }

    structure Map : ORD_MAP where type Key.ord_key = var
    structure Set : ORD_SET where type Key.ord_key = var
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = var

  end = struct

    datatype var = datatype Flan.var

    fun new (name, ty) = V{
	    name = Atom.atom name,
	    stamp = Stamp.new(),
	    ty = ty,
	    useCnt = ref 0,
	    kind = ref Flan.VK_Uninitialized,
	    props = PropList.newHolder()
	  }

    fun copy (V{name, ty, ...}) = V{
	    name = name,
	    stamp = Stamp.new(),
	    ty = ty,
	    useCnt = ref 0,
	    kind = ref Flan.VK_Uninitialized,
	    props = PropList.newHolder()
	  }

    fun nameOf (V{name, ...}) = name

    fun toString (V{name, stamp, ...}) = Atom.toString name ^ Stamp.toString stamp

    fun typeOf (V{ty, ...}) = ty

    fun kindOf (V{kind, ...}) = !kind
    fun setKind (V{kind, ...}, b) = kind := b

    fun compare (V{stamp=s1, ...}, V{stamp=s2, ...}) = Stamp.compare(s1, s2)
    fun same (V{stamp=s1, ...}, V{stamp=s2, ...}) = Stamp.same(s1, s2)
    fun hash (V{stamp, ...}) = Stamp.hash stamp

    fun newProp mkProp = PropList.newProp (fn (V{props, ...}) => props, fn f => mkProp f)

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
