structure FlanPrimTy = PrimTyFn (
  struct

    type ty = FlanTypes.ty

    val tyBase = FlanTypes.TyBase
    val tyBaseSeq = FlanTypes.TyBaseSeq
    val tySeq = (fn tb => FlanTypes.tySeq (FlanTypes.TyBase tb))
    fun tyTuple [t] = t
      | tyTuple tys = FlanTypes.TyTuple tys

  end)
