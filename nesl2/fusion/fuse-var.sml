(* cu-var.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure FuseVar : sig

    type var = FuseAST.var

    val new : (string * FuseAST.ty) -> var
    val copy : var -> var

    val name : var -> string
    val toString : var -> string

    val same : var * var -> bool

    val typeOf : var -> FuseAST.ty

    val binding : var -> FuseAST.var_bind
    val setBinding : var * FuseAST.var_bind -> unit

    val useCnt : var -> int
    val incCnt : var -> unit
    val decCnt : var -> unit

    val newProp : (var -> 'a)
	-> {
	    clrFn: var -> unit,
	    getFn: var -> 'a,
	    peekFn: var -> 'a option,
	    setFn: var * 'a -> unit
	  }
    val newFlag : unit -> {getFn : var -> bool, setFn : var * bool -> unit}

    val setShape : var * FuseShapes.shape -> unit
    val getShape : var -> FuseShapes.shape

    structure Map : ORD_MAP where type Key.ord_key = var
    structure Set : ORD_SET where type Key.ord_key = var
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = var

  end = struct

    datatype var = datatype FuseAST.var

    fun new (name, ty) = V{
	    name = name,
	    stamp = Stamp.new(),
	    ty = ty,
	    binding = ref FuseAST.VB_None,
	    useCnt = ref 0,
	    props = PropList.newHolder()
	  }

    fun copy (V{name, ty, ...}) = new (name, ty)

    fun name (V{name, ...}) = name

    fun toString (V{name, stamp, ...}) = name ^ Stamp.toString stamp

    fun typeOf (V{ty, ...}) = ty

    fun binding (V{binding, ...}) = !binding
    fun setBinding (V{binding, ...}, b) = binding := b

    fun useCnt (V{useCnt, ...}) = !useCnt
    fun incCnt (V{useCnt, ...}) = useCnt := !useCnt + 1
    fun decCnt (V{useCnt, ...}) = useCnt := !useCnt - 1

    fun compare (V{stamp=s1, ...}, V{stamp=s2, ...}) = Stamp.compare(s1, s2)
    fun same (V{stamp=s1, ...}, V{stamp=s2, ...}) = Stamp.same(s1, s2)
    fun hash (V{stamp, ...}) = Stamp.hash stamp

    fun newProp init = PropList.newProp (fn (V{props, ...}) => props, init)

    fun newFlag () = PropList.newFlag (fn (V{props, ...}) => props)

  (* per-variable property to get/set shapes *)
    val {getFn=getShape, setFn=setShape, peekFn=peekShape, ...} = let
	  fun init (v : var) : FuseShapes.shape =
		raise Fail(concat["shape of ", toString v, " is not initialized"])
	  in
	    newProp init
	  end

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
