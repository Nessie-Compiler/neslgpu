(* mono-ty.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure MonoTy =
  struct

    datatype ty
      = TyBase of TypeBase.ty
      | TySeq of ty
      | TyBaseSeq of TypeBase.ty
      | TyTuple of ty list
      | TyFun of ty * ty	(* NOTE: should not occur in return type *)

    val tySegdes = TyBase TypeBase.SEGDES

    fun tyPair (ty1, ty2) = TyTuple[ty1, ty2]

(*
    fun tySeq (ty as TyTuple[TyBase TypeBase.SEGDES, _]) = tyPair(tySegdes, ty)
      | tySeq (TyFlatSeq _) = raise Fail "unexpected TyFlatSeq"
      | tySeq ty = tyPair(tySegdes, TyFlatSeq ty)
*)
    fun tySeq ty = TySeq ty

  (* equality test on MonoAST.ty lists *)
    fun sameTys ([], []) = true
      | sameTys (ty1::tys1, ty2::tys2) = same(ty1, ty2) andalso sameTys(tys1, tys2)
      | sameTys _ = false

  (* are two MonoAST types the same? *)
    and same (ty1, ty2) = (case (ty1, ty2)
	   of (TyBase tb1, TyBase tb2) => (tb1 = tb2)
	    | (TySeq ty1, TySeq ty2) => same (ty1, ty2)
	    | (TyBaseSeq tb1, TyBaseSeq tb2) => (tb1 = tb2)
	    | (TyTuple tys1, TyTuple tys2) => ListPair.allEq same (tys1, tys2)
	    | (TyFun(ty11, ty12), TyFun(ty21, ty22)) => same (ty11, ty21) andalso same (ty12, ty22)
	    | _ => false
	  (* end case *))

  (* some common types *)
    val tyInt = TyBase TypeBase.INT
    val tyFloat = TyBase TypeBase.FLOAT
    val tyBool = TyBase TypeBase.BOOL
    val tyChar = TyBase TypeBase.CHAR
    val tyString = tySeq tyChar

    fun toString ty = let
	  fun toS (ty, l) = (case ty
		 of TyBase tb => TypeBase.baseToString tb :: l
		  | TySeq ty => "[" :: toS(ty, "]" :: l)
		  | TyBaseSeq tb => "[:" :: TypeBase.baseToString tb :: ":]" :: l
		  | TyTuple (ty1::tys) => let
		      fun toS' (ty as TyTuple _, l) = "(" :: toS(ty, ")" :: l)
			| toS' (ty, l) = toS(ty, l)
		      in
			toS' (ty1, List.foldr (fn (ty, l) => ", " :: toS'(ty, l)) l tys)
		      end
		  | TyFun(ty1, ty2) => toS(ty1, " -> " :: toS(ty2, l))
		(* end case *))
	  in
	    String.concat (toS (ty, []))
	  end

    fun funtyToString (domTy, rngTy) = concat[toString domTy, " -> ", toString rngTy]

  end
