(* kernel.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Utility code for FuseAST.kernel values.
 *)

structure Kernel : sig

    val new : string * FuseAST.var list * FuseAST.kern_exp * FuseAST.ty list -> FuseAST.kernel

    val same : FuseAST.kernel * FuseAST.kernel -> bool

    val toString : FuseAST.kernel -> string

    val typeOf : FuseAST.kernel -> FuseAST.ty list * FuseAST.ty list

    val defn : FuseAST.kernel -> FuseAST.var list * FuseAST.kern_exp

  (* application count *)
    val useCnt : FuseAST.kernel -> int
    val incCnt : FuseAST.kernel -> unit
    val decCnt : FuseAST.kernel -> unit

    val newProp : (FuseAST.kernel -> 'a)
	-> {
	    clrFn: FuseAST.kernel -> unit,
	    getFn: FuseAST.kernel -> 'a,
	    peekFn: FuseAST.kernel -> 'a option,
	    setFn: FuseAST.kernel * 'a -> unit
	  }
    val newFlag : unit -> {getFn : FuseAST.kernel -> bool, setFn : FuseAST.kernel * bool -> unit}

  end = struct

    fun new (name, params, body, rngTy) = FuseAST.K{
	    name = name,
	    stamp = Stamp.new(),
	    ty = (List.map FuseVar.typeOf params, rngTy),
	    params = params,
	    body = body,
	    appCnt = ref 0,
	    props = PropList.newHolder()
	  }

    fun same (FuseAST.K{stamp=s1, ...}, FuseAST.K{stamp=s2, ...}) = Stamp.same(s1, s2)

    fun toString (FuseAST.K{name, stamp, ...}) = name ^ Stamp.toString stamp

    fun typeOf (FuseAST.K{ty, ...}) = ty

    fun defn (FuseAST.K{params, body, ...}) = (params, body)

    fun useCnt (FuseAST.K{appCnt, ...}) = !appCnt
    fun incCnt (FuseAST.K{appCnt, ...}) = appCnt := !appCnt + 1
    fun decCnt (FuseAST.K{appCnt, ...}) = appCnt := !appCnt - 1

    fun newProp init = PropList.newProp (fn (FuseAST.K{props, ...}) => props, init)

    fun newFlag () = PropList.newFlag (fn (FuseAST.K{props, ...}) => props)

  end
