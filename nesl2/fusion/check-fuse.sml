(* check-fuse.sml
 *
 * COPYRIGHT (c) 2014 Nora Sandler
 * All rights reserved.
 *
 * Typechecker for FuseAST representation.
 *)

structure CheckFuse : sig

    val check : FuseAST.program -> bool

  end = struct

    structure F = FuseAST
    structure U = FuseUtil

    val anyErrors = ref false

    datatype token = S of string | A of Atom.atom | F of F.funct
		   | K of F.kernel | V of F.var | VS of F.var list
		   | TY of F.ty | TYS of F.ty list

    fun tyTuple [t] = t
      | tyTuple ts = F.TyTuple ts

    fun error toks = let
      fun tok2str (S s) = s
	| tok2str (A a) = Atom.toString a
	| tok2str (F f) = FuseFunct.toString f
	| tok2str (K k) = Kernel.toString k
	| tok2str (V x) = FuseVar.toString x
	| tok2str (VS xs) = let
	    val strs = map FuseVar.toString xs
	    in
	      String.concat(["(", String.concatWith "," strs, ")"])
	    end
	| tok2str (TY ty) = U.tyToString ty
	| tok2str (TYS ts) = tok2str (TY (F.TyTuple ts))
    in
      anyErrors := true;
      Log.msg (String.concat("** " :: List.map tok2str toks))
    end

    fun check (F.Program prog) = (
	anyErrors := false;
	List.app chkTop prog;
	!anyErrors)

    and chkTop top = (case top
	   of F.Top_Kern k => let
		val (domTy, rngTy) = Kernel.typeOf k
		val (args, exp) = Kernel.defn k
		val paramTy = chkArgs args
		val resultTy = chkKernExp exp
		in
		  if not(ListPair.all U.sameTy (domTy, paramTy))
		  then error [
		       S "type mismatch in kernel ", K k, S ":",
		       S "\n expected domain type : ", TYS domTy,
		       S "\n found parameter type : ", TYS paramTy, S "\n"
		       ]
		  else ();
		  if not(ListPair.all U.sameTy(rngTy, resultTy))
		  then error [
		       S "type mismatch in kernel ", K k, S ":",
		       S "\n expected range type: ", TYS rngTy,
		       S "\n found result type: ", TYS resultTy, S "\n"
		       ]
		  else ()
		end
	    | F.Top_Funct(f, args, exp) => let
		val F.F{ty=(domTy, rngTy), ...} = f
		val paramTy = chkArgs args
		val resultTy = chkExp exp
		in
		  if not(ListPair.all U.sameTy (domTy, paramTy))
		  then error [
		       S "type mismatch in function ", F f, S ":",
		       S "\n expected domain type : ", TYS domTy,
		       S "\n found parameter type : ", TYS paramTy, S "\n"
		       ]
		  else ();
		  if not(U.sameTy(rngTy, resultTy))
		  then error [
		       S "type mismatch in function ", F f, S ":",
		       S "\n expected range type: ", TY rngTy,
		       S "\n found result type: ", TY resultTy, S "\n"
		       ]
		  else ()
		end
	    | F.Top_Let(vs, e) => let
		val varTy = tyTuple (chkArgs vs)
		val resultTy = chkExp e
		in
		  if not (U.sameTy(varTy, resultTy))
		  then error [
		       S "type mismatch in top-level binding ", VS vs, S ":",
		       S "\n expected var type : ", TY varTy,
		       S "\n found exp type : ", TY resultTy, S "\n"
		       ]
		  else ()
		end
	    | F.Top_RHS(vs, rhs) => let
		 val varTy = tyTuple(chkArgs vs)
		 val rhsTy = tyTuple(chkRHS rhs)
		 in
		   if not (U.sameTy(varTy, rhsTy))
		     then error [
			 S "type mismatch in rhs ", VS vs, S ":",
			 S "\n expected var type : ", TY varTy,
			 S "\n found rhs type : ", TY rhsTy, S "\n"
		       ]
		     else ()
		 end
	    | F.Top_Exp(e, _) => (chkExp e; ())
	  (* end case *))

    and chkArgs vs = map FuseVar.typeOf vs

    and chkAtms atms = map U.getAtmTy atms

    and chkExp exp =
	(case exp
	  of F.Exp_Let(vs, e1, e2) => let
	       val varTy = tyTuple(chkArgs vs)
	       val rhsTy = chkExp e1
	     in
	       if not (U.sameTy(varTy, rhsTy))
	       then error [
		    S "type mismatch in let ", VS vs, S ":",
		    S "\n expected var type : ", TY varTy,
		    S "\n found exp type : ", TY rhsTy, S "\n"
		    ]
	       else ();
	       chkExp e2
	     end
	   | F.Exp_RHS(vs, rhs, e) => let
	       val varTy = tyTuple(chkArgs vs)
	       val rhsTy = tyTuple(chkRHS rhs)
	     in
	       if not (U.sameTy(varTy, rhsTy))
	       then error [
		    S "type mismatch in rhs ", VS vs, S ":",
		    S "\n expected var type : ", TY varTy,
		    S "\n found rhs type : ", TY rhsTy, S "\n"
		    ]
	       else ();
	       chkExp e
	     end
	   | F.Exp_If(a, e1, e2) => let
	       val ty1 = chkExp e1
	       val ty2 = chkExp e2
	     in
	       (if not (U.sameTy(U.getAtmTy a, F.TyScalar TypeBase.BOOL))
		then error [
		     S "type error in 'if':",
		     S "\n expected boolean atom",
		     S "\n found atom ", S(U.atomToString a),
		     S "of type : ", TY (U.getAtmTy a), S "\n"
		     ]
		else ();
		if not (U.sameTy(ty1,  ty2))
		then error [
		     S "type mismatch in branches of 'if':",
		     S "\n then branch: ", TY ty1,
		     S "\n else branch: ", TY ty2, S "\n"
		     ]
		else (); ty1)
	     end
	   | F.Exp_Apply(f, atms) => let
	       val F.F{ty=(domTy, rngTy), ...} = f
	       val args = U.getAtmTy' atms
	     in
	       if not (ListPair.all U.sameTy (args, domTy))
	       then error [
		    S "type mismatch in application of function ", F f, S ":",
		    S "\n function expects type: ", TYS domTy,
		    S "\n found type: ", TYS args, S "\n"
		    ]
	       else ();
	       rngTy
	     end
	   | F.Exp_Atom a => U.getAtmTy a
	(* end case *))


    (* An RHS returns a list of values,
     * because a kernel returns a list of values.
     * I think this should make merging independent kernels easier,
     * but if not we can change it.
     *)
    and chkRHS rhs =
	(case rhs
	  of F.RHS_Scalar(scalOp, atms) => let
	       val args = U.getAtmTy' atms
	       val (dom, rng) = U.typeOfScalar scalOp
	     in
	       if not (ListPair.all U.sameTy(args, dom))
	       then error [
		    S "type mismatch in application of ", S (ScalarOp.toString scalOp), S ":",
		    S "\n operator expects type: ", TYS dom,
		    S "\n found type: ", TYS args, S "\n"
		    ]
	       else ();
	       [rng]
	     end
	   | F.RHS_Map(kern, wid, args) => let
	       val args = chkArgs args
	       val F.K{ty=(dom, rng), ...} = kern
	       fun scalToSeq ty =
		   (case ty
		     of F.TyScalar t => F.TySeq (t, NONE)
		      | F.TyTuple ts => F.TyTuple (map scalToSeq ts)
		      | _ => raise Fail "found sequence or segdes type in kernel"
		   (* end case *))
	       val dom' = map scalToSeq dom
	       val rng' = map scalToSeq rng
	     in
	       if not (ListPair.all U.sameTy(args, dom'))
	       then error [
		    S "type mismatch in application of ", K kern, S ":",
		    S "\n kernel expects type: ", TYS dom',
		    S "\n found type: ", TYS args, S "\n"
		    ]
	       else ();
	       rng'
	     end
	   | F.RHS_FlatGen(genOp, wid, atms) => let
	       val args = U.getAtmTy' atms
	       val (dom, rng) = U.typeOfGen genOp
	     in
	       if not (ListPair.all U.sameTy(args, dom))
	       then error [
		    S "type mismatch in application of ", S (GeneratorOp.toString genOp), S ":",
		    S "\n operator expects type: ", TYS dom,
		    S "\n found type: ", TYS args, S "\n"
		    ]
	       else ();
	       [rng]
	     end
	   | F.RHS_SegGen(genOp, wid, segArgs) => let
	       val args = chkArgs segArgs
	       val (dom, rng) = U.typeOfSegGen genOp
	     in
	       if not (ListPair.all U.sameTy(args, dom))
	       then error [
		    S "type mismatch in application of segmented ", S (GeneratorOp.toString genOp), S ":",
		    S "\n operator expects type: ", TYS dom,
		    S "\n found type: ", TYS args, S "\n"
		    ]
	       else ();
	       [rng]
	     end
	   | F.RHS_FlatReduce(reduceOp, wid, arg) => let
	       val arg = FuseVar.typeOf arg
	       val ty = ReduceOp.baseTy reduceOp
	       val domTy = F.TySeq (ty, NONE)
	     in
	       if not (U.sameTy(arg, domTy))
	       then error [
		    S "type mismatch in application of ", S (ReduceOp.toString reduceOp), S ":",
		    S "\n expected type: ", TY domTy,
		    S "\n found type: ", TY arg, S "\n"
		    ]
	       else ();
	       [F.TyScalar ty]
	     end
	   | F.RHS_SegReduce(reduceOp, wid, seg, data) => let
	       val segTy = FuseVar.typeOf seg
	       val dataTy = FuseVar.typeOf data
	       val seqTy = F.TySeq(ReduceOp.baseTy reduceOp, NONE)
	     in
	       if not (U.sameTy(segTy, F.TySegdes))
	       then error [
		    S "type mismatch in application of segmented ", S (ReduceOp.toString reduceOp), S ":",
		    S "\n expected segdes as first argument",
		    S "\n found: ", TY segTy, S "\n"
		    ]
	       else ();
	       if not (U.sameTy(dataTy, seqTy))
	       then error [
		    S "type mismatch in application of segmented ", S (ReduceOp.toString reduceOp), S ":",
		    S "\n expected second argument type: ", TY seqTy,
		    S "\n found type: ", TY dataTy, S "\n"
		    ]
	       else ();
	       [seqTy]
	     end
	   | F.RHS_FlatScan(scanOp, wid, arg) => let
	       val arg = FuseVar.typeOf arg
	       val seqTy = F.TySeq(ScanOp.baseTy scanOp, NONE)
	     in
	       if not (U.sameTy(arg, seqTy))
	       then error [
		    S "type mismatch in application of ", S (ScanOp.toString scanOp), S ":",
		    S "\n expected type: ", TY seqTy,
		    S "\n found type: ", TY arg, S "\n"
		    ]
	       else ();
	       [seqTy]
	     end
	   | F.RHS_SegScan(scanOp, wid, seg, data) => let
	       val segTy = FuseVar.typeOf seg
	       val dataTy = FuseVar.typeOf data
	       val seqTy = F.TySeq(ScanOp.baseTy scanOp, NONE)
	     in
	       if not (U.sameTy(segTy, F.TySegdes))
	       then error [
		    S "type mismatch in application of segmented ", S (ScanOp.toString scanOp), S ":",
		    S "\n expected segdes as first argument",
		    S "\n found: ", TY segTy, S "\n"
		    ]
	       else ();
	       if not (U.sameTy(dataTy, seqTy))
	       then error [
		    S "type mismatch in application of segmented ", S (ScanOp.toString scanOp), S ":",
		    S "\n expected second argument type: ", TY seqTy,
		    S "\n found type: ", TY dataTy, S "\n"
		    ]
	       else ();
	       [seqTy]
	     end
	   | F.RHS_FlatVector(v, atms) => let
	       val args = U.getAtmTy' atms
	       val (dom, rng) = U.typeOfVector v
	     in
	       if not (ListPair.all U.sameTy(args, dom))
	       then error [
		    S "type mismatch in application of ", S (VectorOp.toString v), S ":",
		    S "\n operator expects type: ", TYS dom,
		    S "\n found type: ", TYS args, S "\n"
		    ]
	       else ();
	       [rng]
	     end
	   | F.RHS_SegVector(v, atms) => let
	       val args = U.getAtmTy' atms
	       val (dom, rng) = U.typeOfSegVector v
	     in
	       if not (ListPair.all U.sameTy(args, dom))
	       then error [
		    S "type mismatch in application of segmented ", S (VectorOp.toString v), S ":",
		    S "\n operator expects type: ", TYS dom,
		    S "\n found type: ", TYS args, S "\n"
		    ]
	       else ();
	       [rng]
	     end
	   | F.RHS_Internal(i, atm) => let
	       val arg = U.getAtmTy atm
	       val (dom, rng) = U.typeOfInternal i
	     in
	       if not (ListPair.all U.sameTy([arg], dom))
	       then error [
		    S "type mismatch in application of ", S (InternalOp.toString i), S ":",
		    S "\n operator expects type: ", TYS dom,
		    S "\n found type: ", TY arg, S "\n"
		    ]
	       else ();
	       [rng]
	     end
	   | F.RHS_Cmd (c, atms) => let
	       val arg = U.getAtmTy' atms
	       val (dom, rng) = U.typeOfCmd c
	     in
	       if not (ListPair.all U.sameTy(arg, dom))
	       then error [
		    S "type mismatch in application of ", S (Cmd.toString c), S ":",
		    S "\n operator expects type: ", TYS dom,
		    S "\n found type: ", TYS arg, S "\n"
		    ]
	       else ();
	       rng
	     end
	   | F.RHS_Seq (atms, tb) => let
	       val args = U.getAtmTy' atms
	       val elemTy = F.TyScalar tb
	       fun mismatch arg =
		   if not(U.sameTy(arg, elemTy))
		   then error [
			S "expected sequence element of type ", TY elemTy,
			S "\n found type: ", TY arg, S "\n"
			]
		   else ();
	     in
	       List.app mismatch args;
	       [F.TySeq (tb, NONE)]
	     end
	   | F.RHS_Tuple atms => U.getAtmTy' atms
	   | F.RHS_Proj (i, v) =>
	     (case FuseVar.typeOf v
	       of (F.TyTuple ts) => [List.nth(ts, i - 1)]
		| t => (error [
			S "expected ", V v, S " to be of tuple type",
			S "but found type: ", TY t, S "\n"
			]; [t])
	     (* end case *))
	(* end case *))

    and chkKernExp exp =
	(case exp
	  of F.KExp_Let(vs, e1, e2) => let
	       val args = tyTuple(chkArgs vs)
	       val rhsTy = tyTuple(chkKernExp e1)
	     in
	       if not (U.sameTy(args, rhsTy))
	       then error [
		    S "type mismatch in kernel let ", VS vs, S ":",
		    S "\n expected var type : ", TY args,
		    S "\n found exp type : ", TY rhsTy, S "\n"
		    ]
	       else ();
	       chkKernExp e2
	     end
	   | F.KExp_Pure(x, s, atms, e) => let
	       val args = U.getAtmTy' atms
	       val (dom, rng) = U.typeOfScalar s
	     in
	       if not(ListPair.all U.sameTy (args, dom))
	       then error [
		    S "type mismatch in kernel application of ", S (ScalarOp.toString s), S ":",
		    S "\n expected arg type : ", TYS dom,
		    S "\n found type : ", TYS args, S "\n"
		    ]
	       else ();
	       if not (U.sameTy(FuseVar.typeOf x, rng))
	       then error [
		    S "type mismatch in kernel let ", V x, S ":",
		    S "\n expected var type: ", TY(FuseVar.typeOf x),
		    S "\n found exp type:    ", TY rng, S "\n"
		    ]
	       else ();
	       chkKernExp e
	     end
	   | F.KExp_Proj(v, i, tup, e) => let
	       val varTy = FuseVar.typeOf v
	       val tupTy = FuseVar.typeOf tup
	     in
	       (case tupTy
		 of F.TyTuple ts =>
		    if not (U.sameTy(List.nth(ts, i - 1), varTy))
		    then error [
			 S "type mismatch in tuple projection in kernel:",
			 S "\n tuple element has type: ", TY (List.nth(ts, i -1)),
			 S "\n but bound variable has type: ", TY varTy, S "\n"
			 ]
		    else ()
		  | _ => error [
			 S "type error in tuple projection:",
			 S "\n expected variable ", V tup, S " to have tuple type",
			 S "\n but found type ", TY tupTy, S "\n"
			 ]
	       (* end case *));
	       chkKernExp e
	     end
	   | F.KExp_Tuple(v, atms, e) => let
	       val varTy = FuseVar.typeOf v
	       val atmsTy = tyTuple(U.getAtmTy' atms)
	     in
	       if not(U.sameTy (varTy, atmsTy))
	       then error [
		    S "type mismatch in tuple construction:",
		    S "\n tuple arguments have type: ", TY atmsTy,
		    S "\n but bound variable has type: ", TY varTy, S "\n"
		    ]
	       else ();
	       chkKernExp e
	     end
	   | F.KExp_If _ => raise Fail "FIXME"
	   | F.KExp_Return atms => U.getAtmTy' atms
	(* end case *))

end



