(* funct.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Nesl function identifiers.
 *)

structure Funct : sig

    type funct

    val new : Atom.atom * NeslTypes.scheme -> funct

    val nameOf : funct -> Atom.atom
    val toString : funct -> string

    val typeOf : funct -> NeslTypes.scheme

    val updateTy : funct * NeslTypes.scheme -> unit

    val same : funct * funct -> bool
    val hash : funct -> word

    structure Map : ORD_MAP where type Key.ord_key = funct
    structure Set : ORD_SET where type Key.ord_key = funct
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = funct

  end = struct

    datatype funct = F of {
	name : Atom.atom,
	stamp : Stamp.stamp,
	ty : NeslTypes.scheme ref
      }

    fun new (name, ty) = F{
	    stamp = Stamp.new(),
	    name = name,
	    ty = ref ty
	  }

    fun nameOf (F{name, ...}) = name

    fun toString (F{name, stamp, ...}) = Atom.toString name

    fun typeOf (F{ty, ...}) = !ty

    fun updateTy (F{ty, ...}, ty') = (ty := ty')

    fun compare (F{stamp=s1, ...}, F{stamp=s2, ...}) = Stamp.compare(s1, s2)
    fun same (F{stamp=s1, ...}, F{stamp=s2, ...}) = Stamp.same(s1, s2)
    fun hash (F{stamp, ...}) = Stamp.hash stamp

    local
      structure Key = struct
	  type ord_key = funct
	  val compare = compare
	end
    in
    structure Map = RedBlackMapFn (Key)
    structure Set = RedBlackSetFn (Key)
    end

    structure Tbl = HashTableFn (
      struct
	type hash_key = funct
	val hashVal = hash
	val sameKey = same
      end)

  end
