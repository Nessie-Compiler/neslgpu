(* normal-env.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure NormalEnv : sig

    type env

  (* create a new environment *)
    val new : unit -> env

  (* insert a global-variable binding into the environment *)
    val bindGlobalVar : env * MonoAST.var * NormalAST.var -> unit

  (* insert a local-variable binding into the environment *)
    val bindLocalVar : env * MonoAST.var * NormalAST.var -> env

  (* lookup a variable in the environment *)
    val lookupVar : env * MonoAST.var -> NormalAST.var

  (* insert a function binding into the environment *)
    val bindFunct : env * MonoAST.funct * NormalAST.funct -> unit

  (* lookup a function in the environment *)
    val lookupFunct : env * MonoAST.funct -> NormalAST.funct

  end = struct

    structure M = MonoAST
    structure MV = MonoVar
    structure MF = MonoFunct
    structure N = NormalAST

  (* an environment has hash tables for tracking top-level functions and variables, and
   * a functional map for local variables.
   *)
    datatype env = E of {
	vEnv : N.var MV.Map.map,
	vTbl : N.var MV.Tbl.hash_table,
	fTbl : N.funct MF.Tbl.hash_table
      }

    fun new () = E{
	    vEnv = MV.Map.empty,
	    vTbl = MV.Tbl.mkTable (32, Fail "vTbl"),
	    fTbl = MF.Tbl.mkTable (1024, Fail "fTbl")
	  }

    fun bindGlobalVar (E{vTbl, ...}, x, x') = MV.Tbl.insert vTbl (x, x')

    fun bindLocalVar (E{vEnv, vTbl, fTbl}, x, x') =
	  E{vEnv = MV.Map.insert(vEnv, x, x'), vTbl=vTbl, fTbl=fTbl}

    fun lookupVar (E{vEnv, vTbl, ...}, x) = (case MV.Map.find(vEnv, x)
	   of NONE => (case MV.Tbl.find vTbl x
		 of NONE => raise Fail("unable to find variable " ^ MV.toString x)
		  | SOME x' => x'
		(* end case *))
	    | SOME x' => x'
	  (* end case *))

    fun bindFunct (E{fTbl, ...}, f, f') = MF.Tbl.insert fTbl (f, f')

    fun lookupFunct (E{fTbl, ...}, f) = (case MF.Tbl.find fTbl f
	   of NONE => raise Fail("unable to find function " ^ MF.toString f)
	    | SOME f' => f'
	  (* end case *))

  end
