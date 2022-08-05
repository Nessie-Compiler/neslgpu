(* fuse-funct.sml
 *
 * COPYRIGHT (c) 2014 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *)

structure FuseFunct : sig

    type funct = FuseAST.funct

    val new : Atom.atom * (FuseAST.ty list * FuseAST.ty) -> funct

    val nameOf : funct -> string
    val toString : funct -> string

    val typeOf : funct -> FuseAST.ty list * FuseAST.ty

    val appCnt : funct -> int

    val same : funct * funct -> bool
    val hash : funct -> word

    val newProp :  (funct -> 'a) -> {
	    clrFn : funct -> unit,
	    getFn : funct -> 'a,
	    peekFn : funct -> 'a option,
	    setFn : funct * 'a -> unit
	  }
    val newFlag : unit -> {getFn : funct -> bool, setFn : funct * bool -> unit}

    structure Map : ORD_MAP where type Key.ord_key = funct
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = funct

  end = struct

    structure F = FuseAST
		  
    type funct = F.funct
		 
    fun newProp mkProp = PropList.newProp (fn (F.F{props, ...}) => props, fn f => mkProp f)
    fun newFlag () = PropList.newFlag (fn (F.F{props, ...}) => props)

    fun new (name, ty) = F.F{
	    name = Atom.toString name,
	    stamp = Stamp.new(),
	    ty = ty,
	    appCnt = ref 0,
	    props = PropList.newHolder()
	  }

    fun nameOf (F.F{name, ...}) = name
    fun toString (F.F{name, stamp, ...}) = name ^ Stamp.toString stamp
    fun typeOf (F.F{ty, ...}) = ty
    fun appCnt (F.F{appCnt, ...}) = !appCnt				
    fun same (F.F{stamp=s1, ...}, F.F{stamp=s2, ...}) = Stamp.same(s1, s2)
    fun compare (F.F{stamp=s1, ...}, F.F{stamp=s2, ...}) = Stamp.compare(s1, s2)
    fun hash (F.F{stamp, ...}) = Stamp.hash(stamp)

    local
      structure Key = struct
	  type ord_key = funct
	  val compare = compare
	end
    in
    structure Map = RedBlackMapFn (Key)
    end

    structure Tbl = HashTableFn (
      struct
	type hash_key = funct
	val hashVal = hash
	val sameKey = same
      end)

  end
