(* cu-var.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure CuVar : sig

    type 'ty var

    val new : (string * 'ty) -> 'ty var

    val name : 'ty var -> string
    val uniqueName : 'ty var -> string
    val toString : 'ty var -> string

    val typeOf : 'ty var -> 'ty

    val useCnt : 'a var -> int
    val incCnt : 'a var -> unit
    val clrCnt : 'a var -> unit

    val compare : 'ty var * 'ty var -> order
    val same : 'ty var * 'ty var -> bool
    val hash : 'ty var -> word

    val newProp : ('a var -> 'b)
	-> {
	    clrFn: 'a var -> unit,
	    getFn: 'a var -> 'b,
	    peekFn: 'a var -> 'b option,
	    setFn: 'a var * 'b -> unit
	  }
    val newFlag : unit -> {getFn : 'a var -> bool, setFn : 'a var * bool -> unit}

  end = struct

    datatype 'ty var = V of {
	name : string,
	stamp : Stamp.stamp,
	ty : 'ty,
	useCnt : int ref,
	props : PropList.holder
      }

    fun new (name, ty) = V{
	    name = name,
	    stamp = Stamp.new(),
	    ty = ty,
	    useCnt = ref 0,
	    props = PropList.newHolder()
	  }

    fun name (V{name, ...}) = name

    fun uniqueName (V{name, stamp, ...}) = Stamp.suffix(name, stamp)

    fun toString (V{name, stamp, ...}) = name ^ Stamp.toString stamp

    fun typeOf (V{ty, ...}) = ty

    fun useCnt (V{useCnt, ...}) = !useCnt
    fun incCnt (V{useCnt, ...}) = useCnt := !useCnt + 1
    fun clrCnt (V{useCnt, ...}) = useCnt := 0

    fun compare (V{stamp=s1, ...}, V{stamp=s2, ...}) = Stamp.compare(s1, s2)
    fun same (V{stamp=s1, ...}, V{stamp=s2, ...}) = Stamp.same(s1, s2)
    fun hash (V{stamp, ...}) = Stamp.hash stamp

    fun newProp init = PropList.newProp (fn (V{props, ...}) => props, init)

    fun newFlag () = PropList.newFlag (fn (V{props, ...}) => props)

  end
