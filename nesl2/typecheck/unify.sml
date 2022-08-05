(* unify.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Unify : sig

    val unifyTy : NeslTypes.ty * NeslTypes.ty -> bool

    val unifyVarWithTy : NeslTypes.tyvar * NeslTypes.ty -> bool

  (* match a type against a base type *) 
    val matchBaseTy : TypeBase.ty * NeslTypes.ty -> bool

  end = struct

    structure TB = TypeBase
    structure Ty = NeslTypes

    fun occurs (tv, ty) = let
	  fun occ (Ty.TyVar(Ty.TV{inst=ref(Ty.TY ty), ...})) = occ ty
	    | occ (Ty.TyVar tv') = TyVar.same(tv, tv')
	    | occ Ty.TyError = false
	    | occ Ty.TyVoid = false
	    | occ (Ty.TyBase _) = false
	    | occ (Ty.TySeq ty) = occ ty
	    | occ (Ty.TyBaseSeq ty) = occ ty
	    | occ (Ty.TyPair(ty1, ty2)) = occ ty1 orelse occ ty2
	    | occ (Ty.TyData(_, tys)) = List.exists occ tys
	    | occ (Ty.TyFun(ty1, ty2)) = occ ty1 orelse occ ty2
	  in
	    occ ty
	  end

  (* is a type an instance of a type class *)
    fun isA (ty, cls) = (case (ty, cls)
	   of (_, TB.TY) => true
	    | (Ty.TyVar _, TB.ANY) => true
	    | (Ty.TyVar(Ty.TV{cls=TB.TY, ...}), _) => true
	    | (Ty.TyVar(Ty.TV{cls=TB.ANY, ...}), _) => true
	    | (Ty.TyVar(Ty.TV{cls=TB.NUM, ...}), TB.ORD) => true
	    | (Ty.TyVar(Ty.TV{cls, ...}), c) => (cls = c)
	    | (Ty.TyError, _) => true
	    | (Ty.TyVoid, _) => false
	    | (Ty.TyBase TB.SEGDES, TB.ANY) => false
	    | (Ty.TyBase _, TB.ANY) => true
	    | (Ty.TyBase TB.INT, TB.NUM) => true
	    | (Ty.TyBase TB.INT, TB.ORD) => true
	    | (Ty.TyBase TB.INT, TB.LOGIC) => true
	    | (Ty.TyBase TB.BOOL, TB.LOGIC) => true
	    | (Ty.TyBase TB.FLOAT, TB.NUM) => true
	    | (Ty.TyBase TB.FLOAT, TB.ORD) => true
	    | (Ty.TyBase TB.CHAR, TB.ORD) => true
	    | (Ty.TySeq _, TB.ANY) => true
	    | (Ty.TyBaseSeq _, _) => false
	    | (Ty.TyPair _, TB.ANY) => true
	    | (Ty.TyData _, TB.ANY) => true
	    | _ => false
	  (* end case *))

    fun unifyTy (ty1, ty2) = (case (ty1, ty2)
	   of (Ty.TyError, _) => true
	    | (_, Ty.TyError) => true
	    | (Ty.TyVar tv1, Ty.TyVar tv2) => 
		TyVar.same(tv1, tv2) orelse unifyVarWithVar(tv1, tv2)
	    | (Ty.TyVar tv, _) => unifyVarWithTy(tv, ty2)
	    | (_, Ty.TyVar tv) => unifyVarWithTy(tv, ty1)
	    | (Ty.TyVoid, Ty.TyVoid) => true
	    | (Ty.TyBase tb1, Ty.TyBase tb2) => (tb1 = tb2)
	    | (Ty.TySeq ty1, Ty.TySeq ty2) => unifyTy(ty1, ty2)
	    | (Ty.TyBaseSeq ty1, Ty.TyBaseSeq ty2) => unifyTy(ty1, ty2)
	    | (Ty.TyPair(ty11, ty12), Ty.TyPair(ty21, ty22)) =>
		unifyTy(ty11, ty21) andalso unifyTy(ty12, ty22)
	    | (Ty.TyData(dt1, tys1), Ty.TyData(dt2, tys2)) =>
		Dataty.same(dt1, dt2) andalso ListPair.allEq unifyTy (tys1, tys2)
	    | (Ty.TyFun(ty11, ty12), Ty.TyFun(ty21, ty22)) =>
		unifyTy(ty11, ty21) andalso unifyTy(ty12, ty22)
	    | _ => false
	  (* end case *))

    and unifyVarWithTy (Ty.TV{inst=ref(Ty.TY ty1), ...}, ty2) = unifyTy(ty1, ty2)
      | unifyVarWithTy (Ty.TV{inst=ref Ty.TYPARAM, ...}, _) = raise Fail "unexpected TYPARAM"
      | unifyVarWithTy (tv1, Ty.TyVar tv2) =
		TyVar.same(tv1, tv2) orelse unifyVarWithVar(tv1, tv2)
      | unifyVarWithTy (tv as Ty.TV{cls, inst as ref Ty.UNIV, ...}, ty) =
	  if occurs(tv, ty) orelse not(isA(ty, cls))
	    then false
	    else (inst := Ty.TY ty; true)

    and unifyVarWithVar (tv1 as Ty.TV{inst=inst1, cls=c1, ...}, tv2 as Ty.TV{inst=inst2, cls=c2, ...}) = (
	  case (!inst1, !inst2)
	   of (Ty.TYPARAM, _) => raise Fail "unexpected TYPARAM"
	    | (_, Ty.TYPARAM) => raise Fail "unexpected TYPARAM"
	    | (Ty.TY ty1, _) => unifyVarWithTy (tv2, ty1)
	    | (_, Ty.TY ty2) => unifyVarWithTy (tv1, ty2)
	    | (Ty.UNIV, Ty.UNIV) => (
	      (* we can unify type variables when their classes are the same, or when
	       * one class is a subclass of the other.  In that case, we instantiate
	       * the more general variable to the more specific.
	       *)
		case (c1, c2)
		 of (TB.TY, _) => (inst1 := Ty.TY(Ty.TyVar tv2); true)
		  | (_, TB.TY) => (inst2 := Ty.TY(Ty.TyVar tv1); true)
		  | (TB.ANY, _) => (inst1 := Ty.TY(Ty.TyVar tv2); true)
		  | (_, TB.ANY) => (inst2 := Ty.TY(Ty.TyVar tv1); true)
		  | (TB.ORD, TB.NUM) => (inst1 := Ty.TY(Ty.TyVar tv2); true)
		  | (TB.NUM, TB.ORD) => (inst2 := Ty.TY(Ty.TyVar tv1); true)
		  | _ => if (c1 = c2)
		      then (inst1 := Ty.TY(Ty.TyVar tv2); true)
		      else false
		(* end case *))
	  (* end case *))

(*
val unifyTy = fn (ty1, ty2) => let
val s = concat["unifyTy (", Ty.toString ty1, ", ", Ty.toString ty2, ")"]
val result = unifyTy (ty1, ty2)
in
  if not result then print(s ^ " = false\n") else ();
  result
end
*)
(*
val unifyTy = fn (ty1, ty2) => let
val _ = print(concat["unifyTy (", Ty.toString ty1, ", ", Ty.toString ty2, ")\n"])
val result = unifyTy (ty1, ty2)
in
  print(concat["  = ", Bool.toString result, "\n"]);
  result
end
*)

  (* match a type against a base type *) 
    fun matchBaseTy (_, Ty.TyError) = true
      | matchBaseTy (bty, Ty.TyBase bty') = (bty = bty')
      | matchBaseTy (bty, Ty.TyVar tv) = unifyVarWithTy (tv, Ty.TyBase bty)
      | matchBaseTy _ = false

  end
