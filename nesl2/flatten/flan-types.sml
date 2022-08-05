(* flan-types.sml
 *
 * COPYRIGHT (c) 2013 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 * 
 *)

structure FlanTypes =
  struct

    datatype ty 
      = TyBase of TypeBase.ty
      | TyBaseSeq of TypeBase.ty
      | TyTuple of ty list

    val tySegdes = TyBase(TypeBase.SEGDES)

    fun tyPair (ty1, ty2) = TyTuple[ty1, ty2]


(* We convert a sequence type [t] to its internal representation 
 * with transformation {{ . }} : Ty -> FlatTy , as follows:
 *   {{ b }}         ==> [: b :]                 -- flat sequences of base type (should be paired with segdes)
 *   {{ (t1, t2) }}  ==> ( {{ t1 }}, {{ t2 }} )  -- flat sequences of tuples (should be paired with segdes)
 *   {{ [t] }}       ==> (segdes, {{t}})         -- surface-level sequences
 *)

    fun tyTransform (TyBase t) = TyBaseSeq t
      | tyTransform (TyTuple[TyBase TypeBase.SEGDES, t]) = 
	tyPair(tySegdes, tyTransform t)
      | tyTransform (TyTuple ts) = TyTuple (map tyTransform ts)
(*      | tyTransform (TyBaseSeq tb)= raise Fail "unexpected TyBaseSeq"  *)
      | tyTransform (TyBaseSeq tb)= TyBaseSeq tb 

    fun tySeq t = tyTransform (tyPair(tySegdes, t))

(*
    fun tySeq (ty as TyTuple[TyBase TypeBase.SEGDES, _]) = tyPair(tySegdes, ty)
(*      | tySeq (TyBase TypeBase.SEGDES) = TyBase TypeBase.SEGDES *)
      | tySeq (ty as TyBase t) = tyPair(tySegdes, TyBaseSeq t)
      | tySeq (TyTuple ts) = tyPair(tySegdes, TyTuple (map tySeq ts))
      | tySeq (TyBaseSeq t) = tySeq (TyBase t)
(*      | tySeq (TyBaseSeq _) = raise Fail "unexpected TyBaseSeq"  *)
(*      | tySeq (TyBaseSeq t) = tySeq (tySeq (TyBase t) ) *)
*)


    (* standard types *)
    val tyBool = TyBase(TypeBase.BOOL)
    val tyInt = TyBase(TypeBase.INT)
    val tyFloat = TyBase(TypeBase.FLOAT)
    val tyChar = TyBase(TypeBase.CHAR)
    val tyString = tySeq tyChar
    val tyStream = TyBase(TypeBase.STREAM)
		   
    fun sameTy (t1, t2) = (case (t1, t2)
	    of (TyBase tb1, TyBase tb2) => (tb1 = tb2)
	     | (TyBaseSeq tb1, TyBaseSeq tb2) => (tb1 = tb2)
	     | (TyTuple tys1, TyTuple tys2) => sameTys(tys1, tys2)
	     | _ => false
	  (* end case *))
    and sameTys ([], []) = true
      | sameTys (ty1::tys1, ty2::tys2) = sameTy(ty1, ty2) andalso sameTys(tys1, tys2)
      | sameTys _ = false 

  (* convert a list of types into a tuple
   * single types remain unchanged *)
    fun listToTuple (t::[]) = t
      | listToTuple ts = TyTuple ts

    (* reverse of listToTuple *)
    fun tupleToList (TyTuple ts) = 
	if ((hd ts) = tySegdes)
	then [TyTuple ts]
	else ts
      | tupleToList t = [t]		  

    fun toString (TyBase tb) = TypeBase.baseToString tb
(*      | toString (TyTuple[TyBase TypeBase.SEGDES, TyBaseSeq tb]) = 
	String.concat["[", TypeBase.baseToString tb, "]"] *)
      | toString (TyBaseSeq tb) = String.concat["[:", TypeBase.baseToString tb,":]"]
(*      | toString (TyTuple[TyBase TypeBase.SEGDES, ty]) = 
	String.concat["[", toString ty, "]"] *)
      | toString (TyTuple tys) = String.concat["(", tyListToString tys, ")"]

    and tyListToString ([]) = ""
      | tyListToString (t::[]) = toString t
      | tyListToString (t::ts) = String.concat[toString t, ",", tyListToString ts]

    fun tyFlat (ty as TyTuple[TyBase TypeBase.SEGDES, innerTy]) = (case innerTy
	   of TyBaseSeq tb => TyBase tb
	    | TyTuple [TyBase TypeBase.SEGDES, _] => innerTy
	    | TyTuple _ => let
		fun flat (TyBaseSeq tb) = TyBase tb
		  | flat (ty as TyTuple [TyBase TypeBase.SEGDES, t]) = ty
		  | flat (TyTuple ts) = TyTuple (map flat ts)
		  | flat (TyBase _) = raise Fail "unexpected TyBase"
	      in
		flat innerTy
	      end
	    | TyBase tb => raise Fail "unexpected TyBase as inner type"
	  (* end case *))
      | tyFlat ty = raise Fail(concat["tyFlat: ", toString ty, " is not a sequence"])
		 
  end

