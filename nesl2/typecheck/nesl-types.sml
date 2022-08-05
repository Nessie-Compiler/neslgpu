(* nesl-types.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure NeslTypes =
  struct

    local
      structure TB = TypeBase
    in

    datatype ty
      = TyVar of tyvar
      | TyError
      | TyVoid			(* type for primitive operations that do not take arguments and/or return results *)
      | TyBase of TB.ty
      | TySeq of ty
      | TyBaseSeq of ty		(* sequence of base-type values (used for primops); ty must be TyVar or TyBase *)
      | TyPair of ty * ty
      | TyData of dataty * ty list
      | TyFun of ty * ty	(* NOTE: should not occur in return type *)

    and dataty = DT of {
	name : Atom.atom,	(* type (and constructor) name *)
	stamp : Stamp.stamp,	(* unique ID *)
	params : tyvar list,	(* type parameters *)
	rep : ty		(* representation (i.e., constructor arg type) *)
      }

    and tyvar = TV of {
	name : Atom.atom,
	stamp : Stamp.stamp,
	cls : TypeBase.class,
	inst : kind ref
      }

    and kind
      = UNIV			(* universally quantified *)
      | TY of ty		(* instantiated to ty *)
      | TYPARAM			(* parameter to typecase *)

  (* polymorphic function type *)
    datatype scheme = Scheme of tyvar list * ty * ty

  (* standard types *)
    val tyInt = TyBase(TB.INT)
    val tyBool = TyBase(TB.BOOL)
    val tyFloat = TyBase(TB.FLOAT)
    val tyChar = TyBase(TB.CHAR)
    val tyString = TySeq(tyChar)
    val tyStream = TyBase(TB.STREAM)
    val tySegdes = TyBase(TB.SEGDES)

    val blackhole = DT{
	    name = Atom.atom "<blackhole>",
	    stamp = Stamp.new(),
	    params = [],
	    rep = TyError
	  }

  (* convert a type to a string *)
    fun toString ty = let
	  fun toS (ty, l) = (case ty
		 of TyVar(TV{inst as ref(TY ty), ...}) => (
		      inst := TY(TyData(blackhole, []));
		      toS(ty, l) before inst := TY ty)
		  | TyVar(TV{name, stamp, ...}) =>
		      "'" :: Atom.toString name :: Stamp.toString stamp :: l
		  | TyError => "<error>" :: l
		  | TyVoid => "<void>" :: l
		  | TyBase tb => TB.baseToString tb :: l
		  | TySeq ty => "[" :: toS(ty, "]" :: l)
		  | TyBaseSeq ty => "[:" :: toS(ty, ":]" :: l)
		  | TyPair(ty as TyPair _, ty') => "(" :: toS(ty, ")," :: toS(ty', l))
		  | TyPair(ty, ty') => toS(ty, "," :: toS(ty', l))
		  | TyData(DT{name, ...}, []) => Atom.toString name :: l
		  | TyData(DT{name, ...}, [ty]) =>
		      Atom.toString name :: "(" :: toS(ty, ")"::l)
		  | TyData(DT{name, ...}, ty::tys) =>
		      Atom.toString name :: "(" :: toS'(ty, tys, ")"::l)
		  | TyFun(domTy, rngTy) => toS(domTy, " -> " :: toS(rngTy, l))
		(* end case *))
	  and toS' (ty, tys, l) = toS(ty, List.foldr (fn (ty, l) => "," :: toS(ty, l)) l tys)
	  in
	    String.concat (toS (ty, []))
	  end
    end (* local *)

  (* convert a type variable to a string *)
    fun tyvarToString (TV{name, stamp, inst as ref(TY ty), ...}) =
	  concat["<", Atom.toString name, Stamp.toString stamp, " == ", toString ty, ">"]
      | tyvarToString (TV{name, stamp, cls, ...}) = let
	  val cls = (case cls
		 of TypeBase.TY => ""
(*		  | TypeBase.ANY => ""*)
		  | _ => " in " ^ TypeBase.classToString cls
		(* end case *))
	  in
	    String.concat["'", Atom.toString name, Stamp.toString stamp, cls]
	  end

  (* convert a polymorphic function type (type scheme) to a string *)
    fun schemeToString (Scheme(tvs, domTy, rngTy)) = let
	  val domTy = toString domTy
	  val rngTy = toString rngTy
	  val tvs = (case tvs
		 of [] => []
		  | tvs => [
			" :: (",
			String.concatWith ", " (List.map tyvarToString tvs),
			")"
		      ]
		(* end case *))
	  in
	    String.concat(domTy :: " -> " :: rngTy :: tvs)
	  end

  end
