(* mono-funct.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure MonoFunct : sig

    type funct = MonoAST.funct

    val new : Atom.atom * (MonoAST.ty * MonoAST.ty) * (Funct.funct * MonoAST.ty list) -> funct

    val nameOf : funct -> Atom.atom
    val toString : funct -> string

    val typeOf : funct -> MonoAST.ty * MonoAST.ty

    val instOf : funct -> Funct.funct * MonoAST.ty list

    val same : funct * funct -> bool
    val hash : funct -> word

    val newProp : (funct -> 'a) -> {
	    clrFn : funct -> unit,
	    getFn : funct -> 'a,
	    peekFn : funct -> 'a option,
	    setFn : funct * 'a -> unit
	  }
    val newFlag : unit -> {getFn : funct -> bool, setFn : funct * bool -> unit}

    structure Map : ORD_MAP where type Key.ord_key = funct
    structure Set : ORD_SET where type Key.ord_key = funct
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = funct

  end = struct

    structure M = MonoAST

    type funct = M.funct

    fun new (f, (domTy, rngTy), inst) = M.F{
	    name = f,
	    stamp = Stamp.new(),
	    props = PropList.newHolder(),
	    ty = (domTy, rngTy),
	    inst = inst
	  }

    fun nameOf (M.F{name, ...}) = name

    fun toString (M.F{name, stamp, ...}) = Atom.toString name ^ Stamp.toString stamp

    fun typeOf (M.F{ty, ...}) = ty

    fun instOf (M.F{inst, ...}) = inst

    fun compare (M.F{stamp=s1, ...}, M.F{stamp=s2, ...}) = Stamp.compare(s1, s2)
    fun same (M.F{stamp=s1, ...}, M.F{stamp=s2, ...}) = Stamp.same(s1, s2)
    fun hash (M.F{stamp, ...}) = Stamp.hash stamp

    fun newProp mkProp = PropList.newProp (fn (M.F{props, ...}) => props, fn f => mkProp f)
    fun newFlag () = PropList.newFlag (fn (M.F{props, ...}) => props)

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
