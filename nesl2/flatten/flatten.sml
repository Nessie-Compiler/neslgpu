(* flatten.sml
 *
 * Copyright (c) 2013 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *)

structure Flatten : sig
  
    val flatten: Flan.program * FlanEnv.env * 
		 (Monomorphize.mono_env * NormalEnv.env) * NeslBasis.basis
		 -> (Flan.program * FlanEnv.env)

  end = struct

    structure N = NormalAST
    structure F = Flan
    structure V = FlanVar
    structure FF = FlanFunct
    structure FTy = FlanTypes
    structure U = FlanUtil
    structure E = FlanEnv
    structure P = FlanPure
    structure NTy = NormalTy


    (* convenience functions *)
    fun addBinding (letBinds, var, RH) = let
      val newBind = [(var, RH)]
    in
      (var, letBinds@newBind)
    end

    fun zipBinds(vs, rhs) = ListPair.zip (vs, rhs)

    val mkVar = V.new
    
    fun flatten (F.Program prog, flanEnv, (monoEnv, normEnv), basis) = let
      (* Basis functions we may need *)
      val ({pack=pack, dist=dist, length=length, length_lifted=length_lifted, zip=zip, put=put, zip_lifted=zip_lifted, unzip=unzip, unzip_lifted=unzip_lifted, flatten=flatten', flatten_lifted=flatten_lifted, partition=partition, partition_lifted=partition_lifted, flag_merge=flag_merge, ...}:NeslBasis.basis) = basis
      fun flattenTop (top, tops) = let
	fun flattenExp e : F.exp = let

	  fun flattenBind (var, e) = let
	    val flatExp = flattenExp e
	  in
	    (var, flatExp)
	  end

	  fun flattenApplyForEach (F.ExpForEach(exp, binds)) = let
	    (* Look up or instantiate basis function *)
	    fun getPrim (funct, argTys, dom) = let
	      val (f, insts) = ToFlan.instantiate ((monoEnv, normEnv, flanEnv), basis)
						  (funct, argTys, [dom])
	      fun flattenInst (funct, (flanFunct, flanArgs, flanExp)) = let
		val flatExp = flattenExp flanExp
	      in
		E.rebindInstance (flanEnv, funct, (flanFunct, flanArgs, flatExp))
	      end
	    in
	      List.app flattenInst insts; f
	    end

	    (* Make lifted version of a variable. *)
	    fun liftVar v = let
	      val n = Atom.toString(V.nameOf v)
	      val t = FTy.tySeq (V.typeOf v)
	    in
	      V.new(n, t)
	    end

	    fun liftFun flanFunct = let
	      val F.F{name=n, stamp=s, ty=ref(dom, rng), props=p, isLifted=ref(isL), ...} = flanFunct
	      val funct = FF.getFunct flanFunct 
	      val name_string = Atom.toString n
	    in
	      case FF.getLifted flanFunct
	       of SOME liftedF => liftedF (* This function has already been lifted. *)
		| NONE => 
		  if isL
		  then
		    let fun applyFun(atms) = 
			    F.mkApplyFun(flanFunct, atms)
			val flanFunct' = liftFunAgain((dom, rng), n, funct, applyFun)
			val _ = FF.setLifted(flanFunct, flanFunct')
		    in
		      flanFunct'
		    end

		  (* FIXME: janky way to set lifted function manually until we have source annotations to do it *)
		  else if ((name_string = "partition") orelse
			   (name_string = "flatten") orelse
			   (name_string = "length") orelse
			   (name_string = "zip") orelse
			   (name_string = "unzip"))
		  then
		    let
		      (* We assume there's only one argument for now *)
		      val domArg = hd dom
		      val (prim, tyvars) = (case name_string
				   of "partition" => let
					val FTy.TyTuple[valSeq, countSeq] = domArg
(*					val _ = (print (FTy.toString valSeq); print "\n") *)
				      in
					(partition_lifted, [FTy.tyFlat valSeq])
				      end
				    | "flatten" => (flatten_lifted, [FTy.tyFlat (FTy.tyFlat domArg)])
				    | "length" => (length_lifted, [FTy.tyFlat domArg])
				    | "zip" => let
					val FTy.TyTuple[aseq, bseq] = domArg
				      in
					(zip_lifted, [FTy.tyFlat aseq, FTy.tyFlat bseq])
				      end
				    | "unzip" => let
					val FTy.TyTuple[a, b] = FTy.tyFlat domArg
				      in
					(unzip_lifted, [a, b])
				      end
				    | _ => raise Fail "impossible"
				  (* end case *))
		      val flanFunct' = getPrim(prim, tyvars, FTy.tySeq domArg)
		      val _ = FF.isLifted(flanFunct')
(*
		      val _ = (print "lifting: "; print name_string ;
			       print (FTy.tyListToString dom); print "\n")
*)
		      val _ = FF.setLifted(flanFunct, flanFunct')
		    in
		      flanFunct'
		    end

		  else
		    let
		      val newName = Atom.atom((Atom.toString n)^"^")
(*
		      val _ = (print "lifting: "; print (Atom.toString n);
			       print (FTy.tyListToString dom); print "\n")
*)
		      val dom' = map FTy.tySeq dom
		      val rng' = FTy.tySeq rng
		      val flanFunct' = FF.new(newName, (dom', rng'), true, funct)
		      val (_, args, exp) = 
			  (case E.lookupInstance(flanEnv, funct, flanFunct)
			    of SOME inst => inst
			     | NONE => raise Fail "can't lift unbound function"
			  (* end case *))
		      val args' = map liftVar args
		      val exp' = F.mkForEach(exp, ListPair.zip(args, args'))
		      val _ = FF.setLifted(flanFunct, flanFunct')
		      val exp' = flattenExp exp'
		      val _ = E.bindLiftedInstance(flanEnv, funct, (flanFunct', args', exp'))
		    in
		      flanFunct'
		    end
	    end

	    and liftFunAgain((dom, rng), name : Atom.atom, 
			     funct : Funct.funct, 
			     applyFun : (F.atom list) ->  F.exp) : F.funct =
		(* Keller's transformation rule 4.8 *)
		let
(*		  val domArg = FTy.tySeq dom
		  val domArg' = FTy.tySeq domArg
		  val dom' = map FTy.tySeq dome *)
		  val dom' = map FTy.tySeq dom
		  (* We assume only singleton domains, because we are pre-arity-raising *)
		  (* FIXME: shouldn't assume that. BUT, we use domain of first argument for calling length *)
		  val dom1 = hd dom
		  val dom1' = hd dom'
		in
		  (case E.lookupLiftedInstanceByDom(flanEnv, funct, dom')
		    of SOME (f, _, _) => f
		     | NONE => let
			 val rng' = FTy.tySeq rng
			 val name' = Atom.atom((Atom.toString name)^"^")
			 val newFunct = FF.new(name', (dom', rng'), true, funct)
(*			 val _ = (print (Atom.toString name'); print (FTy.tyListToString dom); print "\n")     *)
(*			 val domArg = hd dom *)
(*			 val flat = getPrim(flatten', [FTy.tyFlat theDom], theDom') *)
			 val flats = map (fn ty => getPrim(flatten', [FTy.tyFlat ty], FTy.tySeq ty)) dom
			 val part = getPrim(partition, [FTy.tyFlat rng], 
					    FTy.tyPair(rng, FTy.tySeq FTy.tyInt))
			 val len = getPrim(length, [FTy.tyFlat dom1], dom1)
(*			 val arg = mkVar("arg", theDom') *)
			 val args = map (fn ty => mkVar("pureArg", ty)) dom'
			 (* flatArg = flatten(arg) *)
			 (* flatArg1 = flatten(arg1)
			  * flatArg2 = flatten(arg2)
			  * ...
			  *) 
			 val flatArgs = map (fn ty => mkVar("flatArg", ty)) dom
			 val flatRHs = ListPair.map (fn (arg, flatFun) => F.mkApplyFun(flatFun, [F.Var arg])) (args, flats)
			 val letBinds = ListPair.zip(flatArgs, flatRHs)
(*
			 val (flatArg, letBinds) = let
			   val var = mkVar("flatArg", theDom)
			   val RH = F.mkApplyFun(flat, [F.Var arg])
			 in
			   addBinding([], var, RH)
			 end
*)
			 (* counts = { length (v) : v in arg1 } *)
			 val (counts, letBinds) = let
			   val var = mkVar("counts", FTy.tySeq FTy.tyInt)
(*			   val argElt = mkVar("arg_elt", FTy.tyFlat dom) *)
			   val argElt = mkVar("arg_elt", dom1)
(*			   val _ = print("type of arg_elt in double-flatten:\n")
			   val _ = print(FTy.toString (FTy.tyFlat domArg)) 
			   val _ = print("\n") *)
			   val RH = F.mkForEach(F.mkApplyFun(len, [F.Var argElt]),
						 [(argElt, hd args)])
			 in
			   addBinding(letBinds, var, RH)
			 end
			 (* res = p^(flatArg) *)
			 val (res, letBinds) = let
			   val var = mkVar("res", rng)
			   val atms = map (fn v => F.Var v) flatArgs
			   val RH = applyFun(atms)
			 in
			   addBinding(letBinds, var, RH)
			 end
			 (* partition (res, seg) *)
			 val pair = mkVar("pair", FTy.TyTuple[rng, FTy.tySeq FTy.tyInt])
			 val body = F.mkTuple(pair, [F.Var res, F.Var counts],
					       F.mkApplyFun(part, [F.Var pair]))
			 val newExp = F.mkLet(letBinds, body)
			 (* Add to environment and flatten *)
			 val newExp = flattenExp newExp
			 val _ = E.bindLiftedInstance(flanEnv, funct,
						(newFunct, args, newExp))
		       in
			 newFunct
		       end
		  (* end case *))
		end

	    fun liftPureToFun pureOp = let
	      val (dom, rng) = P.getTy (P.Lifted pureOp)
	      val name = Atom.atom(Pure.toString pureOp)
	      val funct = NeslBasis.pureToFunct basis pureOp
	      fun applyFun atms = F.mkPure(P.Lifted pureOp, atms)
	    in
	      liftFunAgain((dom, rng), name, funct, applyFun)
	    end

	    (* Convenience functions *)
	    fun mkNonTailExp(letBindings, forEachBindings, exp) = let
	      val newForEachBindings = binds@forEachBindings
	      val body = F.mkForEach(exp, newForEachBindings)
	    in
	      F.mkLet(letBindings, body)
	    end

	    fun mkArg (atm, (atmVars, letBinds, count)) = let
	      val var = mkVar(String.concat["arg", Int.toString count],
			      FTy.tySeq (U.atomTy atm))
	      val RH = F.mkForEach(F.mkAtom(atm), binds)
	      val (atmVar, lb) = addBinding(letBinds, var, RH)
	    in
	      (atmVars@[atmVar], lb, count + 1)
	    end

	    fun mkArgs atms = let
	      val (atmVars, letBinds, _) = List.foldl mkArg ([], [], 0) atms
	    in
	      (atmVars, letBinds)
	    end

	    (* For handling rule 4.1: dist(a, #xs1) *)
	    fun distAtom a = let
(*	      val _ = print "distAtom\n" *)
	      val (x1, xs1) = hd binds
	      val bindTy = V.typeOf x1
(*	      val _ = (print "instantiating dist on type: "; print (FTy.toString (F.atomTy a)); print "\n")   *)
	      val dst = getPrim(dist, [U.atomTy a], FTy.TyTuple[U.atomTy a, FTy.tyInt])
	      val len = getPrim (length, [bindTy], FTy.tySeq bindTy)
	      val (bindLen, letBinds) = let
		val var = mkVar("bindLen", FTy.tyInt)
		val RH = F.mkApplyFun(len, [F.Var xs1])
	      in
		addBinding([], var, RH)
	      end
	      val pair = mkVar("distPair", FTy.TyTuple[U.atomTy a, FTy.tyInt])
	      val body = F.mkTuple(pair, [a, F.Var bindLen],
				    F.mkApplyFun(dst, [F.Var pair]))
(* 	      val _ = print "done instantiating dist\n"  *)
	    in
	      F.mkLet(letBinds, body)
	    end

	  in 
	    (case exp 
	      of F.ExpForEach(e, b) => let
(*		   val _ = print "flattening forEach\n"  *)
		   val forEachFlattened = flattenApplyForEach(exp)
		 in
		   (* Now that inner forEach is flattened, we can flatten out forEach *)
		   flattenApplyForEach(F.mkForEach(forEachFlattened, binds))
		 end
	       | F.ExpLet(letBinds, e2) => let
(*
		   fun flattenBind(v, e1) = let
		     val vs = liftVar v
		     val rh = F.mkForEach(e1, binds)
		   in
		     ([(v, vs)], (vs, rh))
		   end
		   val (forEachBinds, letBinds) = ListPair.unzip (map flattenBind letBinds)
		   val forEachBinds = List.concat forEachBinds
*)
(*		   val _ = print "flattening ExpLet\n"  *)
		   fun flattenBind([], flattenedLetBs, forEachBs) = (flattenedLetBs, forEachBs)
		     | flattenBind((v, e1)::bs, flattenedLetBs, forEachBs) = let
			 val vs = liftVar v
			 (* forEachBinds will be backwards; shouldn't matter *)
			 val rh = F.mkForEach(e1, forEachBs@binds)
			 val letB = (vs, rh)
			 val forEachB = (v, vs)
		       in
			 flattenBind(bs, letB::flattenedLetBs, forEachB::forEachBs)
		       end
		   val (letBinds, forEachBinds) = flattenBind(letBinds, [], [])
		   val letBinds = List.rev letBinds
		 in
		   mkNonTailExp(letBinds, forEachBinds, e2)
		 end
	       | F.ExpTuple(v, atoms, e) => let
(*		   val _ = print "ExpTuple\n"  *)
		   (* {let v = (a1, a2, ...) in e : x in xs} =>
		    * let atmVar1 = {a1 : x in xs}
		    * ...
		    * let vs = zip(atmVar1, ...)
		    * in {e : x in xs, v in vs}; 
		    *)
		   val atomTys = map U.atomTy atoms
		   (* Domain of zip: tuple of vectors of atom types *)
		   val zipDom = map (fn t => FTy.tySeq t) atomTys
		   (* zip expects a pair *)
		   (* FIXME could this cause tuple vs. pair issues? *)
		   val zp = getPrim(zip, atomTys, FTy.TyTuple zipDom)
		   val (atmVars, letBinds) = mkArgs atoms
		   val pair = mkVar("pair", FTy.TyTuple zipDom)
		   val (vs, letBinds) = let
		     val var = liftVar v
		     (* RH : let pair = (a1, a2, ...) in zip(pair) *)
		     val RH = F.mkTuple(pair, map (fn v => F.Var v) atmVars,
					F.mkApplyFun(zp, [F.Var pair]))
		   in
		     addBinding(letBinds, var, RH)
		   end
		   val forEachBinds = [(v, vs)]
		 in
		   mkNonTailExp(letBinds, forEachBinds, e)
		 end

	       | F.ExpSeq(var, [], e) => let
(*		   val _ = print "ExpSeq\n"  *)
		   val vars = liftVar var
		 in
		   F.mkSeq(vars, [], F.mkForEach(e, (var, vars)::binds))
		 end
	       | F.ExpSeq(var, atoms, e) => let
(*		   val _ = print "ExpSeq\n"  *)
		   val letBinds = []
		   (* Information about sequence *)
		   val seqLen = List.length(atoms)
		   val seqTy = U.atomTy (hd atoms)
		   (* Primitives and pure ops *)
		   val mult = P.Base(Pure.MUL TypeBase.INT)
		   val iseq = P.Base(Pure.INDEX)
		   val dst = getPrim(dist, [seqTy], FTy.TyTuple[seqTy, FTy.tyInt])
		   val len = let
		     val ty = V.typeOf(#1 (hd binds))
		   in
		     getPrim(length, [ty], FTy.tySeq ty )
		   end
		   val put = getPrim(put, [seqTy], 
				     FTy.tyPair (FTy.tySeq seqTy,
						 FTy.tyPair (FTy.tySeq FTy.tyInt, 
							     FTy.tySeq seqTy)))
		   val part = getPrim(partition, [seqTy],
				      FTy.TyTuple [FTy.tySeq seqTy, FTy.tySeq FTy.tyInt])
		   val distForSegdes = P.Base(Pure.DIST TypeBase.INT)      
		   (* default value *)
		   val (dv, letBinds) = let
		     val var = mkVar("dv", seqTy)
		     val RH = U.mkDefaultExp seqTy
		   in
		     addBinding(letBinds, var, RH)
		   end
		   (* k = length of sequence (known at compile time) *)
		   val (kVar, letBinds) = let
		     val var = mkVar("k", FTy.tyInt)
		     val RH = F.mkAtom(F.Int(IntInf.fromInt seqLen))
		   in
		     addBinding(letBinds, var, RH)
		   end
		   (* bindLen = length(xs1) *)
		   val (bindLenVar, letBinds) = let
		     val var = mkVar("bindLen", FTy.tyInt)
		     val RH = F.mkApplyFun(len, [F.Var (#2(hd binds))])
		   in
		     addBinding(letBinds, var, RH)
		   end
		   (* flatLen = k * bindLen *)
		   val (flatLenVar, letBinds) = let
		     val var = mkVar("flatLen", FTy.tyInt)
		     val RH = F.mkPure(mult, [F.Var kVar, F.Var bindLenVar])
		   in
		     addBinding(letBinds, var, RH)
		   end
		   (* es1 = {e1 : bind in binds}; ...esk = {e1 : bind in binds} *)
		   val (atmVars, letBinds') = mkArgs atoms
		   val letBinds = letBinds @ letBinds'
		   (* is1 = iseq(0, k, k * bindLen); ... isk = iseq(k-1, k, k * bindLen) *)
		   val indexVars = List.tabulate(seqLen,
						 (fn i => mkVar("is"^Int.toString i,
								FTy.tySeq FTy.tyInt)))
		   val indexRHs = List.tabulate(seqLen, 
						(fn i => let
						      val iAtm = F.Int (IntInf.fromInt i)
						      val kAtm = F.Var kVar
						      val lenAtm = F.Var flatLenVar
						    in
						      F.mkPure(iseq, [iAtm, kAtm, lenAtm])
						    end))
		   val letBinds = letBinds@(zipBinds(indexVars, indexRHs))
		   (* d = dist(dv, flatlen) *)
		   val (dVar, letBinds) = let
		     val var = mkVar("d", FTy.tySeq(seqTy))
		     val dPair = mkVar("dPair", FTy.TyTuple[V.typeOf dv, V.typeOf flatLenVar])
		     val RH = F.mkTuple(dPair, [F.Var dv, F.Var flatLenVar],
					F.mkApplyFun(dst, [F.Var dPair]))
		   in
		     addBinding(letBinds, var, RH)
		   end
		   (* ts_1 = put(es1, is1, d); 
		    * ...
		    * ts_k = put (esk, isk, ts_k-1 *)
		   val seqVars = List.tabulate(seqLen,
					       (fn i =>mkVar("ts"^Int.toString i,
							     FTy.tySeq seqTy)))
		   (* FIXME: not pairing properly here *)
		   (* I think this is fixed now? *)
		   val seqRHs = List.tabulate(seqLen,
					      (fn i => let
						    val v = F.Var (List.nth (atmVars, i))
						    val idx = F.Var (List.nth (indexVars, i))
						    val dv = if (i = 0)
							       then F.Var dVar
							       else F.Var (List.nth(seqVars, i - 1))
						    fun mkExp aList = F.mkApplyFun(put, aList)
						  in
						    U.tupleUp([v, idx, dv], mkExp)
						  end))
		   val letBinds = letBinds@(zipBinds(seqVars, seqRHs))
		   (* s = dist(k, bindLen) *)
		   val (sVar, letBinds) = let
		     val var = mkVar("s", FTy.tySeq(FTy.tyInt))
		     val RH = F.mkPure(distForSegdes, [F.Var kVar, F.Var bindLenVar])
		   in
		     addBinding(letBinds, var, RH)
		   end
		   val (vars, letBinds) = let
		     val v = liftVar var
		     val tskAtm = F.Var(List.nth(seqVars, seqLen - 1))
		     val sAtm = F.Var sVar
		     val pair = mkVar("partPair", FTy.TyTuple[U.atomTy tskAtm, U.atomTy sAtm])
		     val RH = F.mkTuple(pair, [tskAtm, sAtm],
					F.mkApplyFun(part, [F.Var pair]))
		   in
		     addBinding(letBinds, v, RH)
		   end
		 in
		   mkNonTailExp(letBinds, [(var, vars)], e)
		 end
	       | F.ExpPure(P.Base (Pure.PROJ i), arg) => let
(*		   val _ = print "PROJ\n"  *)
		   val (args, letBinds) = mkArgs (arg)
		   val argLifted = hd args
		   val dom = FlanVar.typeOf argLifted
		   val FTy.TyTuple [a, b] = FTy.tyFlat dom
		   val unzp = getPrim(unzip, [a, b], dom)
		   val (pair, letBinds) = let
		     val var = mkVar("unzipped", FTy.tyPair(FTy.tySeq a,
							    FTy.tySeq b))
		     val RH = F.mkApplyFun(unzp, [F.Var argLifted])
		   in
		     addBinding(letBinds, var, RH)
		   end
		   val body = F.mkPure(P.Base (Pure.PROJ i), [F.Var pair])
		 in
		   F.mkLet(letBinds, body)
		 end
	       | F.ExpPure(P.Base p, atms) => let
(*		   val _ = print "ExpPure (base)\n"  *)
		   (* atms1 = {atm1 : xn in xs} ... *)
		   val (args, letBinds) = mkArgs atms
		   val argAtms = map (fn v => F.Var v) args
		   (* p^(atoms) *)
		   val body = F.mkPure(P.Lifted p, argAtms)
		 in
		   F.mkLet(letBinds, body)
		 end
	       | F.ExpPure(P.Lifted p, atms) => let
(*		   val _ = print "ExpPure (lifted)\n"  *)
		   (* {p^(arg1, arg2, ...) : xn in xsn } =>
		    * let args1 = {arg1 : xn in xsn}
		    * let args2 ...
		    * let arg = zip(args1, args2, ...)
		    * in p^^(arg)
		    *)
		   (* atms1 = {atm1 : xn in xsn} ... *)
		   val (args, letBinds) = mkArgs atms
		   (* p^^(atoms) *)
		   val f = liftPureToFun p
		   val argAtms = map (fn v => F.Var v) args
		   val body = F.mkApplyFun(f, argAtms)
		 in
		   F.mkLet(letBinds, body)
		 end
	       | F.ExpCmd _ => raise Fail "side-effecting op in forEach"
	       | F.ExpApplyFun (f, atoms) => let
(*		   val _ = print (String.concat["ExpApplyFun: ", FF.toString f, "\n" ]) *)
		   (* atms1 = {atm1 : xn in xsn} ... *)
		   val (args, letBinds) = mkArgs atoms 
		   val argAtms = map (fn v => F.Var v) args
		   (* f^(atms1, ...) *)
		   val body = F.mkApplyFun(liftFun f, argAtms)
		 in
		   F.mkLet(letBinds, body)
		 end
	       | F.ExpIf(b, e1, e2, expType) => let
(*		   val _ = print "ExpIf\n"  *)
		   val (xn, xsn) = ListPair.unzip(binds)
		   (* Primitives *)
		   (* Question: could not also be instantiated on ints? or will be always be a boolean? *)
		   val notLifted = P.Lifted (Pure.NOT TypeBase.BOOL)
		   val packs = map (fn F.V{ty, ...} => getPrim(pack, [ty], 
							       FTy.tySeq (
							       FTy.tyPair(ty, 
									  FTy.tyBool))))
				   xn
		   val zips = map (fn F.V{ty, ...} => getPrim(zip, [ty, FTy.tyBool],
							      FTy.tyPair (FTy.tySeq ty,
									   FTy.tyBool))) xn
		   val combine = getPrim(flag_merge, [expType], 
					 FTy.tyPair (FTy.tySeq FTy.tyBool,
						     FTy.tyPair (FTy.tySeq expType, 
								 FTy.tySeq expType)))
		   (* fs = { b : xn in xsn} *)
		   val letBinds = []
		   val (fs, letBinds) = let
		     val var = mkVar("fs", FTy.tySeq FTy.tyBool)
		     val RH = F.mkForEach(F.mkAtom b, binds)
		   in
		     addBinding(letBinds, var, RH)
		   end
		   (* nfs = not^ fs *)
		   val (nfs, letBinds) = let
		     val var = mkVar("nfs", FTy.tySeq FTy.tyBool)
		     val RH = F.mkPure(notLifted, [F.Var fs])
		   in
		     addBinding(letBinds, var, RH)
		   end
		   (* gs1 = pack(xs1, fs) ... *)
		   (* hs1 = pack(xs1, fs) ... *)
		   fun mkBranch (varName, flag) (xsi (* as F.V{name, ty, ...}*), (vs, letBinds, count)) = let
		     val name = V.nameOf xsi
		     val ty = V.typeOf xsi
		     (* xzip1 = zip(xs1, fs) *)
		     val flatTy = FTy.tyFlat ty
		     val zipVar = mkVar(String.concat[varName, "zip", Int.toString count], 
					FTy.tySeq(FTy.tyPair(flatTy, FTy.tyBool)))
		     val zp = List.nth(zips, count)
		     (* let pair = (xs1, fs) in zip(pair) *)
		     val pair = mkVar("pair", FTy.TyTuple [ty, FTy.tySeq FTy.tyBool])
		     val zipRH = F.mkTuple(pair, [F.Var xsi, F.Var flag],
					   F.mkApplyFun(zp, [F.Var pair]))
		     val (_, lb) = addBinding(letBinds, zipVar, zipRH)
		     val var = mkVar(String.concat[varName, Int.toString count], ty)
		     val pk = List.nth(packs, count)
		     val RH = F.mkApplyFun(pk, [F.Var zipVar])
		     val (v, lb) = addBinding(lb, var, RH)
		   in
		     (vs@[v], lb, count + 1)
		   end
		   val mkGs = mkBranch ("g", fs)
		   val (gs, letBinds, _) = List.foldl mkGs ([], letBinds, 0) xsn
		   val mkHs = mkBranch ("h", nfs)
		   val (hs, letBinds, _) = List.foldl mkHs ([], letBinds, 0) xsn
		   (* g = if len g1 > 0 then {e1 : xn in gn} else [] *)
		   val len = getPrim(length, [V.typeOf (hd xn)], V.typeOf (hd xsn))
		   val (gLen, letBinds) = let
		     val var = mkVar("gLen", FTy.tyInt)
		     val RH = F.mkApplyFun(len, [F.Var (hd gs)])
		   in
		     addBinding(letBinds, var, RH)
		   end
		   val (gEmpty, letBinds) = let
		     val var = mkVar("gEmpty", FTy.tyBool)
		     val RH = F.mkPure(P.Base (Pure.EQ TypeBase.INT), [F.Var gLen, F.Int 0])
		   in
		     addBinding(letBinds, var, RH)
		   end
		   val (g, letBinds) = let
		     val var = mkVar("g", FTy.tySeq expType)
		     val thenVar = mkVar("empty", FTy.tySeq expType)
		     val thenRH = F.mkSeq(thenVar, [], F.mkAtom (F.Var thenVar))
		     val elseRH = F.mkForEach(e1, ListPair.zip(xn, gs))
		     val RH = F.mkIf(F.Var gEmpty, thenRH, elseRH, FTy.tySeq expType)
		   in
		     addBinding(letBinds, var, RH)
		   end
		   (* h = if len h1 > 0 then {e2 : xn in hn} else [] *)
		   val (hLen, letBinds) = let
		     val var = mkVar("hLen", FTy.tyInt)
		     val RH = F.mkApplyFun(len, [F.Var (hd hs)])
		   in
		     addBinding(letBinds, var, RH)
		   end
		   val (hEmpty, letBinds) = let
		     val var = mkVar("hEmpty", FTy.tyBool)
		     val RH = F.mkPure(P.Base (Pure.EQ TypeBase.INT), [F.Var hLen, F.Int 0])
		   in
		     addBinding(letBinds, var, RH)
		   end
		   val (h, letBinds) = let
		     val var = mkVar("h", FTy.tySeq expType)
		     val thenVar = mkVar("empty", FTy.tySeq expType)
		     val thenRH = F.mkSeq(thenVar, [], F.mkAtom (F.Var thenVar))
		     val elseRH = F.mkForEach(e2, ListPair.zip(xn, hs))
		     val RH = F.mkIf(F.Var hEmpty, thenRH, elseRH, FTy.tySeq expType)
		   in
		     addBinding(letBinds, var, RH)
		   end
		   (* combine(g,h,fs) *)
		   val body = U.tupleUp([F.Var fs, F.Var g, F.Var h],
					(fn aList => F.mkApplyFun(combine, aList)))
		 in
		   F.mkLet(letBinds, body)
		 end
	       | F.ExpAtom (a as (F.Var v)) => let
(*		   val _ = print "ExpAtom\n"  *)
		   fun isID (x, xs) = V.same(x, v)
(*		   val _ = print (String.concat["list of binds: ", Int.toString(List.length binds), "\n"]) *)
		 in
		   (case List.find isID binds
		     (* Var is in binds, Keller's rule 4.2 applies *)
		     of SOME (x, xs) => F.mkAtom(F.Var xs) 
		      (* Var is not in binds, rule 4.1 applies *)
		      | NONE => distAtom (F.Var v)
		   (* end case *))
		 end
	       | F.ExpAtom a => distAtom a
	       | F.ExpLifted _ => raise Fail "unexpected ExpLifted"
(*	       | F.ExpLifted (vs, e, atms) => let
		 (* Keller's rule 4.8 *)
		   (* FIXME: this is pseudocode *)
		   val flats = map getPrim atms
		   val part = getPrim partition
		   val flatAtms = map ExpApplyFun (flats, atms)
		   val segdes = PROJ 1 (hd atms)
		   val res' = F.mkLifted (vs, e, flatAtms)
		 in
		   partition(res', segdes)
		 end
*)
	    (* end case *))
	  end
	in
	  (case e
	    of F.ExpForEach _ => let
		 val flatExp = flattenApplyForEach e
	       in
		 flattenExp flatExp
	       end
	     | F.ExpLet(binds, tail) => let
		 val binds' = List.map flattenBind binds
		 val tail' = flattenExp tail
	       in
		 F.mkLet(binds', tail')
	       end
	     | F.ExpTuple(v, atoms, tail) => let
		 val tail' = flattenExp tail
	       in
		 F.mkTuple(v, atoms, tail')
	       end
	     | F.ExpSeq(v, atoms, tail) => let
		 val tail' = flattenExp tail
	       in
		 F.mkSeq(v, atoms, tail')
	       end
	     | F.ExpIf(b, e1, e2, t) => let
		 val e1' = flattenExp e1
		 val e2' = flattenExp e2
	       in
		 F.mkIf(b, e1', e2', t)
	       end
	     | _ => e
	  (* end case *))
	  end
      in
	(case top
	  of F.TopFun(funct, instances) => let
	       fun doFun (f, vs, e) = let
(*
		 val _ = print (String.concat["flattening ", FF.toString f, 
					      " at type " , FTy.tyListToString (#1 (FF.typeOf f)),
					      "\n"])
*)
		 val flatExp = flattenExp e
	       (* 		 val allFuns = List.foldl flattenTop tops newFuns *)
	       in
		 E.rebindInstance(flanEnv, funct, (f, vs, flatExp))
	       end
	     in
	       (List.app doFun instances;
		(* Add instances back in later *)
		F.TopFun(funct, [])::tops)
	     end
	   | F.TopBind(v, e) => let
(*	       val _ = print "TopBind\n" *)
	       val flatExp = flattenExp e
	     in
	       (F.setLetKind(v, flatExp);
		F.TopBind(v, flatExp)::tops)
	     end
	   | F.TopExp(e, ty, prop) => let
(*	       val _ = print "TopExp" *)
	       val flatExp = flattenExp e
	     in
	       F.TopExp(flatExp, ty, prop)::tops
	     end
	(* end case *))
	end
	val tops = List.foldl flattenTop [] prog
	fun addInstances (tl, tops) =
	    (case tl
	      of F.TopFun (funct, _) => let
		   val instances = (E.lookupInstances(flanEnv, funct))@(E.lookupLiftedInstances(flanEnv, funct))
		   val tl' = F.TopFun(funct, instances)
		 in
		   tl'::tops
		 end
	       | _ => tl::tops
	    (* end case *))
	val tops = List.rev (List.foldl addInstances [] tops)
    in
      (F.Program tops, flanEnv)
    end

  end


	


