(* var.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Var : sig

    type var

    val new : Atom.atom * NeslTypes.ty -> var

    val nameOf : var -> Atom.atom
    val toString : var -> string

    val typeOf : var -> NeslTypes.ty

    val same : var * var -> bool
    val hash : var -> word

    structure Map : ORD_MAP where type Key.ord_key = var
    structure Set : ORD_SET where type Key.ord_key = var
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = var

  end = struct

    datatype var = V of {
	name : Atom.atom,
	stamp : Stamp.stamp,
	ty : NeslTypes.ty
      }

    fun new (name, ty) = V{
	    name = name,
	    stamp = Stamp.new(),
	    ty = ty
	  }

    fun nameOf (V{name, ...}) = name

    fun toString (V{name, stamp, ...}) = Atom.toString name

    fun typeOf (V{ty, ...}) = ty

    fun compare (V{stamp=s1, ...}, V{stamp=s2, ...}) = Stamp.compare(s1, s2)
    fun same (V{stamp=s1, ...}, V{stamp=s2, ...}) = Stamp.same(s1, s2)
    fun hash (V{stamp, ...}) = Stamp.hash stamp

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
