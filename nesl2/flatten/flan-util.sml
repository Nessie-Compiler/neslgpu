(* flan-util.sml
 * 
 * COPYRIGHT (c) 2014 Nora Sandler
 * All rights reserved.
 *)

structure FlanUtil : sig

    val tupleUp : Flan.atom list * (Flan.atom list -> Flan.exp) -> Flan.exp 
    val atomTy : Flan.atom -> Flan.ty 
    val expTy : Flan.exp -> Flan.ty

    val mkDefaultAtom : TypeBase.ty -> Flan.atom
    val mkDefaultExp : Flan.ty -> Flan.exp

    val sameAtom : Flan.atom * Flan.atom -> bool
    val sameAtoms : Flan.atom list * Flan.atom list -> bool
    val atomToString : Flan.atom -> string
    val atomsToString : Flan.atom list -> string

  end = struct 

  structure F = Flan
  structure V = FlanVar
  structure T = FlanTypes
								       
  (* Getting type info *)
								       
  (* Get types of atoms *)
  fun constTy c = (case c
		    of F.Int _ => T.tyInt
		     | F.Float _ => T.tyFloat
		     | F.Bool _ => T.tyBool
		     | F.String _ => T.tyString
		     | F.Char _ => T.tyChar
	          (* end case *))
		  
  fun atomTy a = (case a
		   of (F.Var (F.V{ty=t, ...})) => t
		    | c => constTy c
		 (* end case *))
		 
  fun expTy (F.ExpForEach (e, binds)) = T.tySeq (expTy e)
    | expTy (F.ExpLet (binds, e)) = expTy e
    | expTy (F.ExpTuple (v, atoms, e)) = expTy e
    | expTy (F.ExpSeq (v, atoms, e)) = expTy e
    | expTy (F.ExpIf (pred, e1, e2, t)) = t
    | expTy (F.ExpPure (p, atms)) = #2 (FlanPure.getTy p)
    | expTy (F.ExpCmd (c, atms)) = T.TyTuple (#2 (FlanPrimTy.typeOfCmd c))
    | expTy (F.ExpApplyFun(F.F{ty, ...}, atms)) = #2(! ty)
    | expTy (F.ExpLifted(e, binds)) = T.tySeq (expTy e)
    | expTy (F.ExpAtom a) = atomTy a

  fun mkDefaultAtom TypeBase.INT = F.Int 0
    | mkDefaultAtom TypeBase.BOOL = F.Bool true
    | mkDefaultAtom TypeBase.FLOAT = F.Float "0.0"
    | mkDefaultAtom TypeBase.CHAR = F.Char #"a"

  fun mkDefaultExp ty =
      (case ty
	of (T.TyBase TypeBase.SEGDES) => let
	     val seq = V.new("dvSeqVar", T.TyBaseSeq (TypeBase.INT))
	     val RH = F.mkPure(FlanPure.Base(Pure.SCALAR_TO_SEQ TypeBase.INT), 
			       [F.Int 1])
	     val bind = (seq, RH)
	     val body = F.mkPure(FlanPure.Base Pure.MAKE_SEGDES,
				 [F.Var seq])
	   in
	     F.mkLet([bind], body)
	   end
	 | T.TyBase tb => F.mkAtom(mkDefaultAtom tb)
	 | T.TyBaseSeq tb => F.mkPure(FlanPure.Base(Pure.SCALAR_TO_SEQ tb),
				      [mkDefaultAtom tb])
	 | T.TyTuple ts => let 
	     val var = V.new("dvTupleVar", T.TyTuple ts)
	     val vars = map (fn t => V.new("dvTupleElem", t)) ts
	     val es = map mkDefaultExp ts
	     val binds = ListPair.zip(vars, es)
	     val atoms = map (fn v => F.Var v) vars
	     val body = F.mkTuple(var, atoms, F.mkAtom(F.Var var))
	   in
	     F.mkLet(binds, body)
	   end
      (* end case *))

  (* Generate code to convert tuple of atoms into nested pair before
   * passing them to function or prim. *)
  fun tupleUp (atoms : F.atom list, finalExp : F.atom list -> F.exp) : F.exp = let
    val atoms' = List.rev atoms
    fun tupleUp' atms =
	(case atms
	  of (atm::[]) => finalExp [atm]
	   | (atm1::atm2::atms) => let
	       val pairVar = V.new("pair",
				   T.tyPair(atomTy atm2,
					    atomTy atm1))
	       val body = tupleUp' ((F.Var pairVar)::atms)
	     in
	       F.mkTuple(pairVar, [atm2, atm1], body)
	     end
	(* end case *))
  in
    tupleUp' atoms'
  end

  fun sameAtom (F.Var v1, F.Var v2) = FlanVar.same(v1, v2)
    | sameAtom(F.Int i1, F.Int i2) = (i1 = i2)
    | sameAtom(F.Float f1, F.Float f2) = (f1 = f2)
    | sameAtom(F.Bool b1, F.Bool b2) = (b1 = b2)
    | sameAtom(F.String s1, F.String s2) = (s1 = s2)
    | sameAtom(F.Char c1, F.Char c2) = (c1 = c2)
    | sameAtom _ = false

  fun sameAtoms(atms1, atms2) = ListPair.all sameAtom (atms1, atms2)

  fun atomToString (F.Var v) = "Var["^(FlanVar.toString v)^"]"
    | atomToString (F.Int i) = "Int["^(IntInf.toString i)^"]"
    | atomToString (F.Float f) = "Float["^f^"]"
    | atomToString (F.Bool b) = "Bool["^(Bool.toString b)^"]"
    | atomToString (F.Char c) = "Char["^(Char.toString c)^"]"
    | atomToString (F.String s) = "String["^s^"]"

  fun atomsToString [] = ""
    | atomsToString [atm] = atomToString atm
    | atomsToString (atm::atms) = (atomToString atm)^", "^(atomsToString atms)

end

