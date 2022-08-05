(* dataty.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Dataty :> sig

    type dataty = NeslTypes.dataty

  (* create a new datatype *)
    val new : Atom.atom * NeslTypes.tyvar list * NeslTypes.ty -> dataty

    val nameOf : dataty -> Atom.atom

    val arity : dataty -> int

    val typeOf : dataty -> NeslTypes.scheme

  (* return true if two datatypes are the same (i.e., have the same stamp) *)
    val same : dataty * dataty -> bool

  (* hash a datatype *)
    val hash : dataty -> word

  (* finite maps on datatypes *)
    structure Map : ORD_MAP where type Key.ord_key = NeslTypes.dataty

  end = struct

    structure Ty = NeslTypes

    datatype dataty = datatype Ty.dataty

  (* create a new type variable *)
    fun new (name, params, rep) = DT{
	    name = name, stamp = Stamp.new(), params = params, rep = rep
	  }

    fun nameOf (DT{name, ...}) = name

    fun arity (DT{params, ...}) = List.length params

    fun typeOf (dt as DT{params, rep, ...}) = 
	  Ty.Scheme(params, rep, Ty.TyData(dt, List.map Ty.TyVar params))

  (* return true if two type variables are the same (i.e., have the same stamp) *)
    fun same (DT{stamp=a, ...}, DT{stamp=b, ...}) = Stamp.same(a, b)

  (* hash a datatype *)
    fun hash (DT{stamp, ...}) = Stamp.hash stamp

    structure Map = RedBlackMapFn (
      struct
	type ord_key = dataty
	fun compare (DT{stamp = a, ...}, DT{stamp = b, ...}) = Stamp.compare(a, b)
      end)

  end
