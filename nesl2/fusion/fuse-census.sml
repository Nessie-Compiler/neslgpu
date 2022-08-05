(* fuse-census.sml
 *
 * COPYRIGHT (c) 2014 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *
 * FIXME: lots of redundancy with FuseVar and FuseFunct
 *)

structure FuseCensus : sig

    val census : FuseAST.program -> unit

    val deleteExp : (FuseAST.atom -> FuseAST.atom) *
		    FuseAST.exp
		    -> unit

    val deleteRHS : (FuseAST.atom -> FuseAST.atom) *
		    FuseAST.rhs
		    -> unit

    val incUseCnt : FuseAST.var -> unit
    val combineUseCnt : FuseAST.var * FuseAST.var -> unit
    val decUseCnt : FuseAST.var -> unit
    val clrUseCnt : FuseAST.var -> unit

    val incAtom : FuseAST.atom -> unit
    val decAtom : FuseAST.atom -> unit

    val incAppCnt : FuseAST.funct -> unit
    val decAppCnt : FuseAST.funct -> unit

    val incKernCnt : FuseAST.kernel -> unit
    val decKernCnt : FuseAST.kernel -> unit

end = struct

    structure F = FuseAST
    structure V = FuseVar

    fun incUseCnt v = V.incCnt v
    fun combineUseCnt (v1, v2) = let
      val F.V{useCnt=useCnt1, ...} = v1
      val F.V{useCnt=useCnt2, ...} = v2
    in
      useCnt1 := (!useCnt1 + !useCnt2)
    end
    fun decUseCnt (F.V{useCnt, ...}) = useCnt := (!useCnt - 1)
    fun clrUseCnt (F.V{useCnt, ...}) = useCnt := 0

    fun incAtom (F.Var v) = incUseCnt v
      | incAtom _ = ()

    fun decAtom (F.Var v) = decUseCnt v
      | decAtom _ = ()

    fun incAppCnt (F.F{appCnt, ...}) = appCnt := (!appCnt + 1)
    fun decAppCnt (F.F{appCnt, ...}) = appCnt := (!appCnt - 1)
    fun clrAppCnt (F.F{appCnt, ...}) = appCnt := 0

    fun incKernCnt (F.K{appCnt, ...}) = appCnt := (!appCnt + 1)
    fun decKernCnt (F.K{appCnt, ...}) = appCnt := (!appCnt - 1)
    fun clrKernCnt (F.K{appCnt, ...}) = appCnt := 0

    fun deleteExp (sub, exp) = let
      val sub'  = map sub
    in
      (case exp
	of F.Exp_Let(vs, e1, e2) => (deleteExp(sub, e1); deleteExp(sub, e2))
	 | F.Exp_RHS(vs, rhs, e) => (deleteRHS(sub, rhs); deleteExp(sub, e))
	 | F.Exp_If(a, e1, e2) => (decAtom (sub a);
				   deleteExp(sub, e1);
				   deleteExp(sub, e2))
	 | F.Exp_Apply(f, atms) => (decAppCnt f;
				    List.app decAtom (sub' atms))
	 | F.Exp_Atom a => decAtom (sub a)
      (* end case *))
    end

    and deleteRHS (sub, rhs) = let
	  val sub' = map sub
	  in
	    case rhs
	     of F.RHS_Scalar(s, atms) => List.app decAtom (sub' atms)
	      | F.RHS_FlatGen(g, wid, atms) => (
		  decAtom (sub wid);
		  List.app decAtom (sub' atms))
	      | F.RHS_SegGen(g, wid, vars) => (
		  decAtom (sub wid);
		  List.app decAtom (map (fn v => sub (F.Var v)) vars))
	      | F.RHS_Map(k, wid, vars) => (
		  decKernCnt k; decAtom (sub wid);
		  List.app decAtom (map (fn v => sub (F.Var v)) vars))
	      | F.RHS_FlatReduce(r, wid, v) => (
		  decAtom (sub wid); decAtom (sub (F.Var v)))
	      | F.RHS_SegReduce(r, wid, v1, v2) => (
		  decAtom (sub wid); decAtom (sub (F.Var v1)); decAtom (sub (F.Var v2)))
	      | F.RHS_FlatScan(s, wid, v) => (decAtom (sub wid); decAtom (sub (F.Var v)))
	      | F.RHS_SegScan(s, wid, v1, v2) => (
		  decAtom (sub wid); decAtom (sub (F.Var v1)); decAtom (sub (F.Var v2)))
	      | F.RHS_FlatVector(vc, atms) => List.app decAtom (sub' atms)
	      | F.RHS_SegVector(vc, atms) => List.app decAtom (sub' atms)
	      | F.RHS_Internal(i, atm) => decAtom (sub atm)
	      | F.RHS_Cmd(c, atms) => List.app decAtom (sub' atms)
	      | F.RHS_Seq(atms, t) => List.app decAtom (sub' atms)
	      | F.RHS_Tuple atms => List.app decAtom (sub' atms)
	      | F.RHS_Proj (i, v) => decAtom (sub (F.Var v))
	    (* end case *)
	  end

    and deleteKernExp (sub, kexp) = let
      val sub' = map sub
    in
      (case kexp
	of F.KExp_Let(vs, e1, e2) => (deleteKernExp(sub, e1);
				      deleteKernExp(sub, e2))
	 | F.KExp_Pure(x, p, atms, e) => (List.app decAtom (sub' atms);
					  deleteKernExp(sub, e))
	 | F.KExp_Proj(v1, i, v2, e) => (decAtom (sub (F.Var v2));
					 deleteKernExp(sub, e))
	 | F.KExp_Tuple(v, atms, e) => (List.app decAtom (sub' atms);
					deleteKernExp(sub, e))
	 | F.KExp_If(b, e1, e2) => (decAtom (sub b);
				    deleteKernExp(sub, e1);
				    deleteKernExp(sub, e2))
	 | F.KExp_Return vs => List.app decAtom vs
      (* end case *))
    end

    fun clrExp e =
	(case e
	  of F.Exp_Let(vs, e1, e2) => (List.app clrUseCnt vs;
				       clrExp e1; clrExp e2)
	   | F.Exp_RHS(vs, rhs, e) => (List.app clrUseCnt vs;
				       clrExp e)
	   | F.Exp_If(a, e1, e2) => (clrExp e1; clrExp e2)
	   | _ => ()
	(* end case *))

    fun clrTop (F.Top_Kern k) = clrKernCnt k  (* the kernel body is handled elsewhere *)
      | clrTop (F.Top_Funct(f, vars, e)) = (clrAppCnt f;
				   List.app clrUseCnt vars;
				   clrExp e)
      | clrTop (F.Top_Let(vs, e)) = (List.app clrUseCnt vs; clrExp e)
      | clrTop (F.Top_RHS(vs, rhs)) = List.app clrUseCnt vs
      | clrTop (F.Top_Exp(e, _)) = clrExp e

    fun doRHS rhs = (case rhs
	   of F.RHS_Scalar(s, atms) => List.app incAtom atms
	    | F.RHS_FlatGen(g, wid, atms) => (incAtom wid; List.app incAtom atms)
	    | F.RHS_SegGen(g, wid, vars) => (incAtom wid; List.app incUseCnt vars)
	    | F.RHS_Map(k, wid, vars) => (incKernCnt k; incAtom wid; List.app incUseCnt vars)
	    | F.RHS_FlatReduce(r, wid, v) => (incAtom wid; incUseCnt v)
	    | F.RHS_SegReduce(r, wid, v1, v2) => (incAtom wid; incUseCnt v1; incUseCnt v2)
	    | F.RHS_FlatScan(s, wid, v) => (incAtom wid; incUseCnt v)
	    | F.RHS_SegScan(s, wid, v1, v2) => (incAtom wid; incUseCnt v1; incUseCnt v2)
	    | F.RHS_FlatVector(vc, atms) => List.app incAtom atms
	    | F.RHS_SegVector(vc, atms) => List.app incAtom atms
	    | F.RHS_Internal(i, atm) => incAtom atm
	    | F.RHS_Cmd(c, atms) => List.app incAtom atms
	    | F.RHS_Seq (atms, t) => List.app incAtom atms
	    | F.RHS_Tuple atms => List.app incAtom atms
	    | F.RHS_Proj(i, v) => incUseCnt v
	  (* end case *))

    fun doExp e = (case e
	   of F.Exp_Let(vs, e1, e2) => (doExp e1; doExp e2)
	    | F.Exp_RHS (vs, rhs, exp) => (doRHS rhs; doExp exp)
	    | F.Exp_If (a, e1, e2) => (incAtom a; doExp e1; doExp e2)
	    | F.Exp_Apply (f, atms) => (incAppCnt f; List.app incAtom atms)
	    | F.Exp_Atom atm => incAtom atm
	  (* end case *))

    fun doTop (F.Top_Kern k) = KernelExp.census (Kernel.defn k)
      | doTop (F.Top_Funct(_, _, e)) = doExp e
      | doTop (F.Top_Let(_, e)) = doExp e
      | doTop (F.Top_RHS(_, rhs)) = doRHS rhs
      | doTop (F.Top_Exp(e, _)) = doExp e

    fun census (F.Program prog) = (List.app clrTop prog; List.app doTop prog)

end
