(* mono-prim-ty.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure MonoPrimTy = WrappedPrimTyFn (
  struct

    type ty = MonoTy.ty

    val tyBase = MonoTy.TyBase
    val tyBaseSeq = MonoTy.TyBaseSeq
    val tySeq = (fn tb => MonoTy.TySeq (MonoTy.TyBase tb))
    fun tyTuple [ty] = ty	 
      | tyTuple (ty::tys) = MonoTy.TyTuple[ty, tyTuple tys]

  end)
