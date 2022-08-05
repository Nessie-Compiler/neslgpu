(* transform.sml
 *
 * Copyright (c) 2013 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *)

structure ToFlan : sig

    type env = (Monomorphize.mono_env * NormalEnv.env * FlanEnv.env)

    type instance = (Funct.funct * (Flan.funct * Flan.var list * Flan.exp))

    val instantiate : env * NeslBasis.basis -> 
		      Funct.funct * FlanTypes.ty list * FlanTypes.ty list
		      -> Flan.funct * instance list

    val transform : (Monomorphize.mono_env * NormalEnv.env * NeslBasis.basis) 
		    -> NormalAST.program -> (Flan.program * FlanEnv.env) 

  end = struct

    structure N = NormalAST
    structure M = Monomorphize
    structure MTy = MonoTy
    structure F = Flan
    structure V = FlanVar
    structure FF = FlanFunct
    structure FTy = FlanTypes
    structure U = FlanUtil
    structure E = FlanEnv
    structure P = FlanPure
    structure NTy = NormalTy

    type env = (M.mono_env * NormalEnv.env * E.env)

    type instance = (Funct.funct * (F.funct * F.var list * F.exp))

(* Conversion from NormalAST to Flan *)

    fun cvtTy t = 
	(case t
          of NTy.TyBase tb => FTy.TyBase tb
	   | NTy.TyBaseSeq t => FTy.TyBaseSeq t
	   | NTy.TyTuple ts => FTy.TyTuple (map cvtTy ts)
	   | NTy.TySeq ty => FTy.tySeq (cvtTy ty)
	   | NTy.TyFun _ => raise Fail "FIXME: no funty in Flan"
        (* end case *))
     
		  
    fun cvtVar (v as N.V{name=n, ty=t, ...}, env) = let
          val ty' = cvtTy t
	  val v' = V.new(Atom.toString(n), ty')
	  val env' = E.bindLocalVar(env, v, v')
          in
            (v', env')
          end

    fun cvtVars ([], env) = ([], env)
      | cvtVars(v::vs, env) = let
	  val (v', env') = cvtVar (v, env)
	  val (vs', env') = cvtVars(vs, env')
	  in 
	    (v'::vs', env')
	  end

    fun cvtAtom (a, env) = (case a
          of N.Var v => F.Var(E.lookupVar(env, v))
	   | N.Int i => F.Int i
	   | N.Float f => F.Float f
	   | N.Bool b => F.Bool b
	   | N.String s => F.String s
	   | N.Char c => F.Char c
	 (* end case *))

    fun cvtAtoms (ats, env) = map (fn a => cvtAtom (a, env)) ats

    fun cvtFunct (normalFun as N.F{name=n, props=p, ty=(dom, rng), ...}, funct, env) = let
      val (dom', rng') = (cvtTy dom, cvtTy rng)
      val f' = FF.new(n, ([dom'], rng'), false, funct)
    in
      (E.bindFunct(env, normalFun, f');
       f')
    end
			   
    fun cvtExp (env, basis) (N.Exp(pl, nd)) = let
      val (monoEnv, normEnv, flanEnv) = env
      fun cvtE fEnv = cvtExp ((monoEnv, normEnv, fEnv), basis)
    in
      (case nd
        of N.ExpLet((var, rh), e) => let
	     val (var', flanEnv') =  cvtVar (var, flanEnv)
	     val rh' = cvtE flanEnv' rh
	     val e' = cvtE flanEnv' e
	   in
	     F.mkLet([(var', rh')], e')
	   end
	 | N.ExpTuple(var, atoms, e) => let
	     val (var', flanEnv') = cvtVar(var, flanEnv)
	     val atoms' = cvtAtoms(atoms, flanEnv')
	     val e' = cvtE flanEnv' e
	   in
	     F.mkTuple(var', atoms', e')
	   end
	 | N.ExpLetVector (x, y, a, e) => let
	     (* x = PROJ 1 a, y = PROJ 2 a in e *)
	     val a' = cvtAtom(a, flanEnv)
	     val (x', flanEnv') = cvtVar(x, flanEnv)
	     val (y', flanEnv') = cvtVar(y, flanEnv')
	     val e' = cvtE flanEnv' e
	     val bind1 = (x', F.mkPure(P.Base(Pure.PROJ 1), [a']))
	     val bind2 = (y', F.mkPure(P.Base(Pure.PROJ 2), [a']))
	   in
	     F.mkLet([bind1, bind2], e')
	   end
	 | N.ExpVector (v, y1, y2, e) => let
	     val y1' = cvtAtom(y1, flanEnv)
	     val y2' = cvtAtom(y2, flanEnv)
	     val (v', flanEnv') = cvtVar(v, flanEnv)
	     val e' = cvtE flanEnv' e
	   in
	     F.mkTuple(v', [y1', y2'], e')
	   end
	 | N.ExpSeq(var, atoms, e) => let
	     fun mkSeqRecursive(v, atms, e) = 
		 (case (U.atomTy (hd atms))
		   of FTy.TyBase tb => F.mkSeq(v, atms, e)
		    | FTy.TyTuple[FTy.TyBase TypeBase.SEGDES, t] => let
		    (* v = [a1, ..., an] in e =>
		     * seq1 = a1 ++ a2
		     * seq2 = seq1 ++ a3
		     * ...
		     * seqn-1 = seqn-2 ++ an
		     * len_1 = length a2
		     * ...
		     * len_n = length an
		     * lenSeq = [len1, ..., lenn]
		     * v = partition(seqn-1, lenSeq)
		     * in e
		     *)
			val seqLen = List.length(atms)
			val atmTy = FTy.TyTuple[FTy.tySegdes, t]
			fun getPrim(f, domTy) = instantiate (env, basis) 
							    (f, [t], [domTy])
			val {concat=concatFn, length=lengthFn, partition=partFn, ...} : NeslBasis.basis = basis
			val (conct, _) = getPrim (concatFn, FTy.tyPair(atmTy, atmTy))
			val (len, _) = getPrim(lengthFn, atmTy)
			val (part, _) = getPrim(partFn, FTy.tyPair(atmTy, FTy.tySeq FTy.tyInt))
			val atm1::atm2::rest = atms
			(* create all bindings of form
			 * seqi = seqi-1 ++ ai+1
			 *)
			val seqs = List.tabulate(seqLen - 1,
					      fn i => FlanVar.new("seq"^(Int.toString i),
								  atmTy))
				   
			fun concatAtoms i = 
			    if i = 0
			    then (hd seqs, F.mkApplyFun(conct, [atm1, atm2]))
			    else (List.nth(seqs, i),
				  F.mkApplyFun(conct, [F.Var (List.nth(seqs, i - 1)),
						       List.nth(atms, i + 1)]))
			val seqBinds = List.tabulate(seqLen - 1, concatAtoms)
			(* Last element of seqBinds is what we want to partition *)
			val (flattenedSeq, _) = hd (List.rev seqBinds)
			(* create all bindings of form
			 * leni = length (ai)
			 *)
			val lengths = List.tabulate(seqLen, fn i => FlanVar.new("len"^(Int.toString i),
										FTy.tyInt))
			val lengthAtms = map (fn v => F.Var v) lengths
			fun mkLength i = (List.nth(lengths, i), F.mkApplyFun(len, [List.nth(atms, i)]))
			val lenBinds = List.tabulate(seqLen, mkLength)
			(* sequence of lengths *)
			val lenSeq = FlanVar.new("lenSeq", FTy.tySeq FTy.tyInt)
			val binds = seqBinds@lenBinds
			val partBind = (v, F.mkApplyFun(part, [F.Var flattenedSeq,
							       F.Var lenSeq]))
		      in
			F.mkLet(binds,
				F.mkSeq(lenSeq, lengthAtms,
					F.mkLet([partBind], e)))
		      end
		    | FTy.TyTuple _ => raise Fail "cvtExp: ExpSeqRange (tuple)"
		    (* This assumes that all tuples are pairs!
		     * << >> here gives the transformation on sequences
		     * carried out in mkSeqRecursive
		     * v = [a1, ..., an] in e =>
		     * let a1_1 = PROJ 1 a1
		     * ...
		     * let an_1 = PROJ 1 an
		     * let a1_2 = PROJ 2 a1
		     * ...
		     * let an_2 = PROJ 2 an
		     * let v_1 = <<[a1_1, ..., an_1]>>
		     * let v_2 = <<[a1_2, ..., an_2]>>
		     * let v = zip(v_1, v_2)
		     * in e
		     *)
		    | FTy.TyBaseSeq _ => raise Fail "cvtExp: can't construct sequence of base sequences"
		 (* end case *))
	     val (var', flanEnv') = cvtVar(var, flanEnv)
	     val atoms' = cvtAtoms(atoms, flanEnv')
	     val e' = cvtE flanEnv' e
  	   in
	     mkSeqRecursive (var', atoms', e')
	   end
	 | N.ExpSeqRange(var, sAtm, eAtm, dAtm, e) => let
	     (* Note: even though seqRange constructs take the form [s:e:d],
	      * where s is the start, e is the end, and d is the stride,
	      * iseq takes the arguments in order (s, d, e)
	      *)
	     val sAtm' = cvtAtom(sAtm, flanEnv)
	     val eAtm' = cvtAtom(eAtm, flanEnv)
	     val dAtm' = cvtAtom(dAtm, flanEnv)
	     fun getPrim(f, tyVars, domTy) = instantiate (env, basis) (f, tyVars, [domTy])
	     val {iseq=iseqFunct, ...} : NeslBasis.basis = basis
	     val (seqRange, _) = getPrim(iseqFunct, [], FTy.tyPair (FTy.tyInt, 
								    FTy.tyPair (FTy.tyInt,
										FTy.tyInt)))

	     (* Pair up atms *)
	     val pair1 = FlanVar.new("pair1", FTy.tyPair(FTy.tyInt, FTy.tyInt))
	     val pair2 = FlanVar.new("pair2", FTy.tyPair(FTy.tyInt,
						   FTy.tyPair(FTy.tyInt,
							      FTy.tyInt)))


	     val RH = U.tupleUp([sAtm', dAtm', eAtm'], 
				(fn alist => F.mkApplyFun(seqRange, alist)))
 
(* 	     val RH = F.mkApplyFun(seqRange, [sAtm', dAtm', eAtm']) *)
	     val (var', flanEnv') = cvtVar(var, flanEnv)
	     val e' = cvtE flanEnv' e
	   in
	     F.mkLet([(var', RH)], e')
	   end
	 | N.ExpPure(var, pure, atms, e) => let
	     val atms' = cvtAtoms(atms, flanEnv)
(*
	     val RH = U.tupleUp(atms', 
				(fn alist => F.mkPure(P.Base pure, alist))) 
*)
	     val RH = F.mkPure(P.Base pure, atms')
	     val (var', flanEnv') = cvtVar(var, flanEnv)
	     val e' = cvtE flanEnv' e
	   in
	     F.mkLet([(var', RH)], e')
	   end
	 | N.ExpCmd(vars, cmd, atms, e) => let
	     val atms' = cvtAtoms(atms, flanEnv)
	     val RH = F.mkCmd(cmd, atms')
	     val (vars', flanEnv') = cvtVars(vars, flanEnv)
	     val e' = cvtE flanEnv' e
	   in
	     (case vars'
	       of (var'::[]) => F.mkLet([(var', RH)], e')
		| _ => let
		    val resVar = V.new("cmdRes", FTy.TyTuple(map V.typeOf vars'))
		    val letBinds = [(resVar, RH)]
		    fun mkProj (lb, [], _) = lb
		      | mkProj (lb, v::vs, i) = let
			  val RH = F.mkPure(P.Base(Pure.PROJ i), [F.Var resVar])
			  val lb' = (v, RH)::lb
			in
			  mkProj(lb', vs, i + 1)
			end
		    val letBinds = List.rev(mkProj(letBinds, vars', 1))
		  in
		    F.mkLet(letBinds, e')
		  end
	     (* end case *))
	   end
	 | N.ExpApply (f, atoms) => let
	     val atoms' = cvtAtoms(atoms, flanEnv)
	     val f' = (case E.lookupFunct(flanEnv, f)
			of SOME flanF => flanF
			 | NONE => let
			     val N.F{name, stamp, ...} = f
			   in
			     raise Fail("undefined function "^(Atom.toString name)^(Stamp.toString stamp))
			   end
		      (* end case *))
	   in
	     F.mkApplyFun(f', atoms')
	   end
	 | N.ExpForEach(e, binds) => let
	     fun cvtBind((var1, var2), env) = let
	       val (var1', env') = cvtVar (var1, env)
	       val var2' = E.lookupVar(env', var2)
	     in
	       ((var1', var2'), env')
	     end
	     fun cvtBinds ([], env) = ([], env)
	       | cvtBinds(b::bs, env) = let
		   val (b', env') = cvtBind (b, env)
		   val (bs', env') = cvtBinds(bs, env')
		 in
		   (b'::bs', env')
		 end
	     val (binds', flanEnv') = cvtBinds(binds, flanEnv)
	     val e' = cvtE flanEnv' e
	   in
	     F.mkForEach(e', binds')
	   end
	 | N.ExpIf (atom, e1, e2) => let
	     val atom' = cvtAtom(atom, flanEnv)
	     val e1' = cvtE flanEnv e1
	     val e2' = cvtE flanEnv e2
	     val t = U.expTy e1'
	   in
	     F.mkIf(atom', e1', e2', t)
	   end
	 | N.ExpAtom a => let
	     val a' = cvtAtom (a, flanEnv)
	   in 
	     F.mkAtom a'
	   end
      (* end case *))
    end
				   
    and transform (monoEnv, normEnv, basis) (N.Program tops) = let
      val flanEnv = E.new()
      fun doTopLevel (tl, tops) = 
	  (case tl
	    of N.TopFun(funct, instances) => let
		 fun doFun (f, vs, e) = let
		   (* If f is the lifted version of something else,
		    * it's already in our hash table.
		    *)
		   val f' = (case E.lookupFunct(flanEnv, f)
			      of SOME liftedF => liftedF
			       | NONE => let
				   val newF = cvtFunct (f, funct, flanEnv)
				 in 
				   newF
				 end
			    (* end case *))
		   val (vs', flanEnv') = cvtVars (vs, flanEnv)
		   val e' = cvtExp ((monoEnv, normEnv, flanEnv'), basis) e
		 in
		   (E.bindInstance(flanEnv, funct, (f', vs', e')))
		 end
	       in
		 (List.app doFun instances;
		  (* We'll add instances back in later *)
		  F.TopFun(funct, [])::tops)
	       end
	     | N.TopBind(v, e) => let
		 val (v', _) = cvtVar (v, flanEnv)
		 (* Add vs' as global rather than local variables *)
		 val _ = E.bindGlobalVar (flanEnv, v, v')
		 val e' = cvtExp ((monoEnv, normEnv, flanEnv), basis) e
	       in
		 F.setLetKind(v', e'); 
		 F.TopBind(v', e') :: tops
	       end
	     | N.TopExp (e, t) => F.TopExp(cvtExp ((monoEnv, normEnv, flanEnv), basis) e, cvtTy t, PropList.newHolder()) :: tops
	  (* end case *))
      val tops = List.foldl doTopLevel [] tops
      fun addInstances (tl, tops) = 
	  (case tl
	    of F.TopFun (funct, _) => let
		 val instances = E.lookupInstances(flanEnv, funct)
		 val tl' = F.TopFun (funct, instances)
	       in
		 tl'::tops
	       end
	     | _ => tl::tops
	  (* end case *))
      val tops = List.foldl addInstances [] tops
    in 
      (F.Program(List.rev tops), flanEnv)
    end
						       
						       
    (* Back-convert types to monoTy to use instantiation interface *)
    and toMono (FTy.TyBase t) = MTy.TyBase t
      | toMono (t as FTy.TyTuple[FTy.TyBase TypeBase.SEGDES, seqTy]) =
	let
	  val innerTy = FTy.tyFlat t
	in
	  MTy.TySeq (toMono innerTy)
	end
      | toMono (FTy.TyTuple ts) = MTy.TyTuple (map toMono ts)
      | toMono (FTy.TyBaseSeq t) = MTy.TyBaseSeq t

    and instantiate ((monoEnv, normEnv, flanEnv), basis) (f, tys, dom) =
	(case E.lookupInstanceByDom (flanEnv, f, dom)
	  of SOME (flanPrim, _, _) => (flanPrim, [])
	   | NONE => let
	       val fTyString = (case tys
				 of [] => ""
				  | (ty::[]) => FTy.toString ty
				  | _ => FTy.toString (FTy.TyTuple tys)
			       (* end case *))
(*	       val _ = print ("Flan-type: "^fTyString^"\n") *)
(*	       val _ = print ("Instantiating primitive "^(Atom.toString (Funct.nameOf f))^"on type "^(FTy.tyListToString dom)^"\n")    *)
	       val env = (monoEnv, normEnv, flanEnv)
	       val monoTys = map toMono tys
	       val tyString = (case monoTys
				of [] => ""
				 | (ty::[]) => MonoTy.toString ty
				 | _ => MonoTy.toString(MonoTy.TyTuple monoTys)
			      (* end case *))
(*	       val _ = print (String.concat ["Mono-type: ", tyString, "\n"]) *)
	       val (normPrim, normInstances) = FunEnv.instantiate (monoEnv, normEnv) 
								  (f, monoTys)
	       val flanInstances = map (transformFunct (env, basis))
				       (List.rev normInstances)
	       val flanPrim = (case E.lookupFunct (flanEnv, normPrim)
				of SOME f => f
				 | NONE => raise Fail "normal funct missing from environment"
			      (* end case *))
	     in
	       (flanPrim, flanInstances)
	     end
	(* end case *))
      
    and transformFunct ((monoEnv, normEnv, flanEnv), basis) (funct, (normFun, normArgs, normExp)) = let
      val N.F{name=n, ty=(dom, _), stamp=s, ...} = normFun
      val flanDom = cvtTy dom
(*      val _ = print ("Transforming primitive "^(Atom.toString n)^(Stamp.toString s)^"on type "^(FTy.toString flanDom)^"\n") *)
    in
      (case E.lookupInstanceByDom (flanEnv, funct, [flanDom])
	of SOME inst => (funct, inst)
	 | NONE => let
	     val flanFun = cvtFunct (normFun, funct, flanEnv)
	     val (flanArgs, flanEnv') = cvtVars (normArgs, flanEnv)
	     val flanExp = cvtExp ((monoEnv, normEnv, flanEnv'), basis) normExp
	     val inst = (flanFun, flanArgs, flanExp)
	   in
	     (E.bindInstance (flanEnv, funct, inst);
	      (funct, (flanFun, flanArgs, flanExp)))
	   end
      (* end case *))      
    end

end
