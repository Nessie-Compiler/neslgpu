(* flan-funct.sml
 *
 * COPYRIGHT (c) 2014 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *)

structure FlanFunct : sig

    type funct = Flan.funct

    val new : Atom.atom * (Flan.ty list * Flan.ty) * bool * Funct.funct -> funct

    val nameOf : funct -> Atom.atom
    val toString : funct -> string

    val typeOf : funct -> Flan.ty list * Flan.ty

    val appCnt : funct -> int

    val same : funct * funct -> bool
    val hash : funct -> word

    val newProp : (funct -> 'a) -> {
		  clrFn : funct -> unit,
		  getFn : funct -> 'a,
		  peekFn : funct -> 'a option,
		  setFn : funct * 'a -> unit
		  }
    val newFlag : unit -> {getFn : funct -> bool, setFn : funct * bool -> unit}

    val getLifted : funct -> funct option
    val setLifted : funct * funct -> unit

    val isLifted : funct -> unit

    val getCanRecurse : funct -> bool
    val dontRecurse : funct -> unit
    val doRecurse : funct -> unit

    val getFunct : funct -> Funct.funct
    val setFunct : funct * Funct.funct -> unit

    structure Map : ORD_MAP where type Key.ord_key = funct
    structure Set : ORD_SET where type Key.ord_key = funct
    structure Tbl : MONO_HASH_TABLE where type Key.hash_key = funct

  end = struct


    structure F = Flan
    structure T = FlanTypes

    type funct = F.funct

    fun nameOf (F.F{name, ...}) = name
    fun toString (F.F{name, stamp, ...}) = Atom.toString name ^ Stamp.toString stamp

    fun typeOf (F.F{ty, ...}) = !ty

    fun appCnt (F.F{appCnt, ...}) = !appCnt

    fun compare (F.F{stamp=s1, ...}, F.F{stamp=s2, ...}) = Stamp.compare(s1, s2)
    fun same (F.F{stamp=s1, ...}, F.F{stamp=s2, ...}) = Stamp.same(s1, s2)
    fun hash (F.F{stamp, ...}) = Stamp.hash stamp

    fun isLifted(F.F{isLifted, ...}) = isLifted := true

    fun newProp mkProp = PropList.newProp (fn (F.F{props, ...}) => props, fn f => mkProp f)
    fun newFlag () = PropList.newFlag (fn (F.F{props, ...}) => props)

    (* For function f, property tracks lifted function f^ *)
    val {getFn=getL, setFn=setL, peekFn=peekL, ...} = newProp (
	(fn _ => F.F{
	     name=Atom.atom "init",
	     stamp=Stamp.new(),
	     ty=ref([T.tyInt], T.tyInt),
	     isLifted=ref(false),
	     appCnt=ref 0,
	     props=PropList.newHolder()
	}))
    val getLifted = peekL
    val setLifted = setL

    (* For Flan function f, property tracks AST funct *)
    val {getFn=getFunct, setFn=setFunct, peekFn=peekFunct, ...} = newProp (
	(fn _ => Funct.new(Atom.atom "init", NeslTypes.Scheme([], NeslTypes.tyInt, NeslTypes.tyInt))))

    fun new (name, (domTys, rngTy), isLifted, funct) = let
      val f = F.F{
		name = name,
		stamp = Stamp.new(),
		ty = ref(domTys, rngTy),
		isLifted = ref(isLifted),
		appCnt = ref 0,
		props = PropList.newHolder()
		}
    in
      setFunct(f, funct);
      f
    end

    (* Per-function property tracks function args and body *)
    val {getFn=getInst, setFn=setInst, ...} = let
      fun init f : (F.funct * F.var list * F.exp) = raise Fail "getInst: function not bound"
    in
      newProp init
    end

    (* Per-function property indicating that we are currently inside the body of the function,
     * so if we encounter any calls to that function we should ignore them.
     * Avoids infinite recursion in inlining, effect analysis and shape analysis *)
    val {getFn=getCanRecurse, setFn=setCanRecurse, ...} = newProp (fn _ => true)
    fun dontRecurse f = setCanRecurse(f, false)
    fun doRecurse f = setCanRecurse(f, true)

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


