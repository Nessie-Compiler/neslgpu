(* flan-env.sml
 * 
 * Copyright (c) 2014 Nora Sandler 
 * All rights reserved.
 *)

structure FlanEnv : sig

    type env
    type instance = (Flan.funct * Flan.var list * Flan.exp)

  (* create a new environment *)
    val new : unit -> env

  (* insert a global-variable binding into the environment *)
    val bindGlobalVar : env * NormalAST.var * Flan.var -> unit

  (* insert a local-variable binding into the environment *)
    val bindLocalVar : env * NormalAST.var * Flan.var -> env

  (* lookup a variable in the environment *)
    val lookupVar : env * NormalAST.var -> Flan.var

  (* insert a function binding into the environment *)
    val bindFunct : env * NormalAST.funct * Flan.funct -> unit

  (* lookup a function in the environment *)
    val lookupFunct : env * NormalAST.funct -> Flan.funct option

    (* Associate a new Flan function with Funct.funct *)
    val bindInstance : env * Funct.funct * instance -> unit

    val bindLiftedInstance : env * Funct.funct * instance -> unit

    (* This saves us from knowing whether it counts as lifted or no. *)
    val rebindInstance : env * Funct.funct * instance -> unit

    (* Get all Flan functions associated with Funct.funct *)
    val lookupInstances : env * Funct.funct -> instance list

    val lookupLiftedInstances : env * Funct.funct -> instance list

    (* Don't need to know whether it's lifted or not. *)
    val lookupInstance : env * Funct.funct * Flan.funct -> instance option

    val lookupInstanceByDom : env * Funct.funct * FlanTypes.ty list -> instance option

    val lookupLiftedInstanceByDom : env * Funct.funct * FlanTypes.ty list -> instance option

(*    val deleteInstance : env * instance -> unit *)

(*
  (* insert a mapping from (name, domain type) to Flan function *)
  (* This means new primitives will be in two hash tables AND a list. Eesh. *)
    val bindFunctByType : env * (Atom.atom * FlanTypes.ty) * Flan.funct -> unit

  (* lookup a function by its name and domain type *)
    val lookupFunctByType : env * (Atom.atom * FlanTypes.ty) -> Flan.funct option
*)
  end = struct						  

    
    structure N = NormalAST
    structure NV = NormalVar
    structure F = Flan
    structure P = Pure
    structure FTy = FlanTypes

    (* FTbl is a hash table mapping NormalAST functions to Flan functions *)

    fun same (N.F{stamp=s1, ...}, N.F{stamp=s2, ...}) = Stamp.same(s1, s2)
    fun hash (N.F{stamp, ...}) = Stamp.hash stamp

    structure FTbl = HashTableFn(
      struct
        type hash_key = N.funct
        val hashVal = hash
        val sameKey = same
      end)
(*
    (* VTbl is a hash table from global NormalAST variables to Flan variables *)
    fun sameVar(N.V{stamp=s1, ...}, N.V{stamp=s2, ...}) = Stamp.same(s1, s2)
    fun hashVar(N.V{stamp, ...}) = Stamp.hash stamp

    structure VTbl = HashTableFn (
      struct
        type hash_key = N.var
        val hashVal = hashVar
	val sameKey = sameVar
      end)

    (* VMap maps local NormalAST variables to local Flan variables *)
    structure VMap = RedBlackMapFn(
      struct
        type ord_key = NormalAST.var
        fun compare (N.V{stamp=s1, ...}, N.V{stamp=s2, ...}) = Stamp.compare(s1, s2)
      end)

*)
(*
    fun sameFun ((atm1, ty1), (atm2, ty2)) = 
	Atom.same(atm1, atm2) andalso FTy.sameTy (ty1, ty2)

    fun hashFun (atm, ty) = let
      val nameAtTy = (Atom.toString atm)^(FTy.toString ty)
    in
      Atom.hash(Atom.atom(nameAtTy))
    end

    structure TyTbl = HashTableFn(
      struct
       type hash_key = Atom.atom * FTy.ty
       val hashVal = hashFun
       val sameKey = sameFun
      end)
*)

    type instance = (F.funct * F.var list * F.exp)

    datatype env = E of {
	fTbl : F.funct FTbl.hash_table,
        vTbl : F.var NV.Tbl.hash_table, (*VTbl.hash_table, *)
        vMap : F.var NV.Map.map, (* VMap.map, *)
	iTbl : instance list Funct.Tbl.hash_table,
	(* List of lifted instances of that funct.
	 * Some functs (e.g. length_lifted, zip_lifted) are only lifted version of other functs.
	 * E.g. if we instantiate length_lifted, that will go in the iTbl associated with the
	 * length_lifted Funct.funct. If we then lift that instance, it goes in the lTbl associated
	 * with the length_lifted funct. Thus, the lTbl for length, zip, and other primitive
	 * vector ops should always be empty. *)
	lTbl : instance list Funct.Tbl.hash_table
    }
     
    fun new () = E{
	    fTbl = FTbl.mkTable (1024, Fail "fTbl"),
            vTbl = NV.Tbl.mkTable (32, Fail "vTbl"),
            vMap = NV.Map.empty,
(*	    tTbl = TyTbl.mkTable (1024, Fail "tTbl"), *)
	    iTbl = Funct.Tbl.mkTable(1024, Fail "iTbl"),
	    lTbl = Funct.Tbl.mkTable(1024, Fail "lTbl")
	}

    fun bindGlobalVar(E{vTbl, ...}, x, x') = NV.Tbl.insert vTbl (x, x')

    fun bindLocalVar(E{fTbl, vTbl, iTbl, lTbl, vMap}, x, x') =
	E{fTbl=fTbl, iTbl=iTbl, lTbl=lTbl, vTbl=vTbl, vMap=NV.Map.insert(vMap, x, x')}

    fun lookupVar (E{vTbl, vMap, ...}, x) = (case NV.Map.find(vMap, x)
	   of NONE => (case NV.Tbl.find vTbl x
	         of NONE => raise Fail("unable to find " ^ NormalVar.toString x)
		  | SOME x' => x'
		 (* end case *))
	    | SOME x' => x'
	   (* end case *))

    fun bindFunct (E{fTbl, ...}, f, f') = FTbl.insert fTbl (f, f')
    
    fun lookupFunct (E{fTbl, ...}, f) = FTbl.find fTbl f
(*
    fun bindFunctByType (E{tTbl, ...}, key, f) = 
	TyTbl.insert tTbl (key, f)

    fun lookupFunctByType (E{tTbl, ...}, key) = TyTbl.find tTbl key
*)

    fun bindInstance (E{iTbl, ...}, f, i) = let
      val (flanFun, args, _) = i
      val (dom, rng) = FlanFunct.typeOf flanFun
      fun setParamKind arg = F.setKind(arg, F.VK_Param)
      val _ = List.app setParamKind args
      fun findDup inst = let
	val (instFun, _, _) = inst
	val (d, r) = FlanFunct.typeOf instFun
      in
	FlanFunct.same(flanFun, instFun) orelse FTy.sameTys(dom, d)
      end
    in
      case Funct.Tbl.find iTbl f
       of SOME instances => 	   
	  (* FIXME: more useful error message *)
	  (case List.find findDup instances
	    of SOME (flanF, _, _) => raise Fail (String.concat["Duplicate function: ", 
							      FlanFunct.toString flanF,
							      FTy.tyListToString(#1 (FlanFunct.typeOf flanF))])
	     | NONE => Funct.Tbl.insert iTbl (f, i::instances)
	  (* end case *))
	| NONE => Funct.Tbl.insert iTbl (f, [i])
    end
      
    fun lookupInstances (E{iTbl, ...}, f) = 
	(case Funct.Tbl.find iTbl f
	  of SOME insts => insts
	   | NONE => []
	(* end case *))

    fun lookupInstanceByDom (E{iTbl, ...}, f, domTys) = 
	(case Funct.Tbl.find iTbl f
	  of SOME insts => let
	       fun sameFun i = let
		 val (F.F{ty=ref(dom, rng), ...}, _, _) = i
		 in
		   ListPair.all FTy.sameTy (dom, domTys)
		 end
	     in
	       List.find sameFun insts
	     end
	   | NONE => NONE
	(* end case *))


    fun bindLiftedInstance (E{lTbl, ...}, f, i) = let
      val (flanFun, args, _) = i
      val (dom, rng) = FlanFunct.typeOf flanFun
      fun setParamKind arg = F.setKind(arg, F.VK_Param)
      val _ = List.app setParamKind args
    in
      case Funct.Tbl.find lTbl f
       of SOME instances => let
	    fun findDup inst = let
	      val (instFun, _, _) = inst
	      val (d, r) = FlanFunct.typeOf instFun
	    in
	      FlanFunct.same(flanFun, instFun) orelse FTy.sameTys(dom, d)
	    end
	  in
	    (* FIXME: better error message *)
	    (if List.exists findDup instances
	     then raise Fail "Duplicate instance found. Try rebindInstance instead?"
	     else ();
	     Funct.Tbl.insert lTbl (f, i::instances))
	  end
	| NONE => Funct.Tbl.insert lTbl (f, [i])
    (* end case *)
    end
      
    fun lookupLiftedInstances (E{lTbl, ...}, f) = 
	(case Funct.Tbl.find lTbl f
	  of SOME insts => insts
	   | NONE => []
	(* end case *))
	
    fun lookupLiftedInstanceByDom (E{lTbl, ...}, f, domTys) = 
	(case Funct.Tbl.find lTbl f
	  of SOME insts => let
	       fun sameFun i = let
		 val (F.F{ty=ref(dom, rng), ...}, _, _) = i
		 in
		   ListPair.all FTy.sameTy (dom, domTys)
		 end
	     in
	       List.find sameFun insts
	     end
	   | NONE => NONE
	(* end case *))

    fun rightFun rightF (f, _, _) = FlanFunct.same(rightF, f)

    fun lookupI (env, funct, f) =
	(case lookupInstances(env, funct)
	  of [] => NONE
	   | insts => List.find (rightFun f) insts
	(* end case *))


    fun lookupLI (env, funct, f) =
	(case lookupLiftedInstances(env, funct)
	  of [] => NONE
	   | insts => List.find (rightFun f) insts
	(* end case *))

    fun lookupInstance (env as E{iTbl, lTbl, ...}, f, flanFun) =
	(case lookupI(env, f, flanFun)
	  of SOME inst => SOME inst
	   | NONE => (case lookupLI(env, f, flanFun)
		       of SOME inst => SOME inst
			| NONE => NONE
		     (* end case *))
	(* end case *))
			

    fun rebindLI (env as E{lTbl, ...}, f, i as (flanF, args, e)) = let
      val rightF = rightFun flanF
    in
      (case lookupLiftedInstances(env, f) 
	of [] => raise Fail "function to rebind not found"
	 | insts =>
	   if List.exists rightF insts
	   then Funct.Tbl.insert lTbl(f, i::(List.filter (fn i => not(rightF i)) insts))
	   else raise Fail "function to rebind not found"
      (* end case *))
    end

    fun rebindI (env as E{iTbl, ...}, f, i as (flanF, args, e)) = let
      val rightF = rightFun flanF
    in
      (case lookupInstances(env, f)
	of [] => false
	 | insts =>
	   if List.exists rightF insts
	   then (Funct.Tbl.insert iTbl(f, i::(List.filter(fn i => not(rightF i)) insts));
		 true)
	   else false
      (* end case *))
    end
					 
    fun rebindInstance (env, f, i) =
	if rebindI (env, f, i)
	then ()
	else rebindLI (env, f, i)

   end
