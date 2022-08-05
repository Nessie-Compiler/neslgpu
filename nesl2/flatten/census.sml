(* census.sml
 *
 * COPYRIGHT (c) 2014 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 * 
 * Initialize use counts of Flan variables.
 *)

structure Census : sig

  (* Question: should this also update bindings, or just use counts? *)
    val census : Flan.program -> unit			       

    val delete : (Flan.atom -> Flan.atom) *
		 (Flan.atom list -> Flan.atom list) *
		 Flan.exp 
		 -> unit 

    val incUseCnt : Flan.var -> unit
    val combineUseCnt : Flan.var * Flan.var -> unit
    val decUseCnt : Flan.var -> unit

    val incAtom : Flan.atom -> unit
    val decAtom : Flan.atom -> unit

    val incAppCnt : Flan.funct -> unit
    val decAppCnt : Flan.funct -> unit

  end = struct

    structure F = Flan
    structure Env = FlanEnv

    fun addToUseCnt (F.V{useCnt, ...}, i) = (useCnt := (!useCnt + i))
    fun incUseCnt v = addToUseCnt(v, 1)
    fun combineUseCnt (v1, v2) = let
      val F.V{useCnt, ...} = v2
    in
      addToUseCnt(v1, !useCnt)
    end
    fun decUseCnt v = addToUseCnt(v, ~1)

    fun incAppCnt (F.F{appCnt, ...}) = (appCnt := (!appCnt + 1))
    fun decAppCnt (F.F{appCnt, ...}) = (appCnt := (!appCnt - 1))

    fun clearVar (F.V{useCnt, ...}) = useCnt := 0
    fun clearFun (F.F{appCnt, ...}) = appCnt := 0

    (* If an atom is a variable, increase its use count. 
     * Otherwise ignore. *)
    fun incAtom (F.Var v) = incUseCnt v
      | incAtom _ = ()

    fun decAtom (F.Var v) = decUseCnt v
      | decAtom _ = ()


    fun clearBind (v, e) = (clearVar v; clearExp e)

    and clearExp (F.ExpLet(binds, e)) = (List.app clearBind binds; clearExp e)
      | clearExp (F.ExpTuple(v, atms, e)) = (clearVar v; clearExp e)
      | clearExp (F.ExpSeq(v, atms, e)) = (clearVar v; clearExp e)
      | clearExp (F.ExpIf(b, e1, e2, t)) = (clearExp e1; clearExp e2)
      | clearExp (F.ExpPure _) = ()
      | clearExp (F.ExpCmd _) = ()
      | clearExp (F.ExpApplyFun _) = () 
      | clearExp (F.ExpLifted(e, binds)) = (List.app (fn (v, vs) => clearVar v) binds;
					    clearExp e)
      | clearExp (F.ExpAtom _) = ()

    fun clearTop (F.TopFun(_, insts)) = let
      fun clearInst (f, args, e) = (clearFun f; List.app clearVar args; clearExp e)				 
    in
      List.app clearInst insts
    end
      | clearTop (F.TopBind(v, e)) = (clearVar v; clearExp e)
      | clearTop (F.TopExp(e, t, p)) = clearExp e
	
    (* Decrement counts of all variables in exp
     * for non-free variables this will give us garbage but
     * we don't care because we're deleting them...I think? *)
    fun delete (sub, sub', exp) = 
	(case exp
	  of F.ExpLet(binds, e) => 
	     (List.app (fn (vs, e') => delete (sub, sub', e')) binds;
	      delete (sub, sub', e))
	   | F.ExpTuple(_, atms, e) => 
	     (List.app (fn a => decAtom (sub a)) atms;
	      delete (sub, sub', e))
	   | F.ExpSeq(_, atms, e) =>
	     (List.app (fn a => decAtom (sub a)) atms;
	      delete (sub, sub', e))
	   | F.ExpIf (b, e1, e2, t) =>
	     (decAtom b;
	      delete(sub, sub', e1);
	      delete(sub, sub', e2))
	   | F.ExpPure(p, atms) => List.app decAtom (sub' atms)
	   | F.ExpCmd(c, atms) => List.app decAtom (sub' atms)
	   | F.ExpApplyFun(f, atms) =>
	     (decAppCnt f; 
	      List.app decAtom (sub' atms))
	   | F.ExpLifted(e, vs) =>
	     (List.app (fn (var, vars) => decUseCnt vars) vs;
	      delete(sub, sub', e))
	   | F.ExpAtom atm => decAtom (sub atm)
	   | F.ExpForEach _ => raise Fail "found forEach after flattening"
	(* end case *))
	

    fun doE (F.ExpLet(binds, e)) = (List.app doBind binds; doE e)
      | doE (F.ExpTuple(v, atms, e)) = (List.app incAtom atms; doE e)
      | doE (F.ExpSeq(v, atms, e)) = (List.app incAtom atms; doE e)
      | doE (F.ExpPure(p, atms)) = (List.app incAtom atms)
      | doE (F.ExpCmd(c, atms)) = (List.app incAtom atms)
      | doE (F.ExpIf(b, e1, e2, t)) = (incAtom b; doE e1; doE e2)
      | doE (F.ExpApplyFun(f, atms)) = (incAppCnt f; List.app incAtom atms)
      | doE (F.ExpAtom atm) = incAtom atm
      | doE (F.ExpLifted (e, binds)) = (List.app (fn (v, vs) => incUseCnt vs) binds; doE e)
      | doE (F.ExpForEach _) = raise Fail "found forEach in census"

    and doBind (vars, e) = doE e

    fun doTop (F.TopFun(_, insts)) = List.app (fn(f, a, e) => doE e) insts
      | doTop (F.TopBind (vs, e)) = doE e
      | doTop (F.TopExp (e, ty, p)) = doE e


    fun census (F.Program tops) = (List.app clearTop tops; List.app doTop tops)

end
