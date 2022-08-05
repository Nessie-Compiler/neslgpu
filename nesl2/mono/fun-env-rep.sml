(* fun-env-rep.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure FunEnvRep : sig

    datatype fun_def
      = FunDef of AST.pat * AST.exp
      | PrimDef of AST.exp

    type instance = (MonoAST.funct * MonoAST.pat * MonoAST.exp)

    type env

  (* create a new environment *)
    val new : unit -> env

    val insert : env -> Funct.funct * fun_def -> instance list ref

    val find : env * Funct.funct -> (fun_def * instance list ref) option

  end = struct

    local
    structure A = AST
    structure T = MonoAST
    in

  (* hash tables with function keys *)
    structure FTbl = HashTableFn (
      struct
	type hash_key = Funct.funct
	val hashVal = Funct.hash
	val sameKey = Funct.same
      end)

    datatype fun_def
      = FunDef of AST.pat * AST.exp
      | PrimDef of AST.exp

    type instance = (T.funct * T.pat * T.exp)

    type env = (fun_def * instance list ref) FTbl.hash_table

    fun new () : env = FTbl.mkTable (1024, Fail "FunEnvTbl")

    fun insert (env : env) (f, fdef) = (case FTbl.find env f
	   of SOME _ => raise Fail "duplicate insert"
	    | NONE => let
		val instRef = ref[]
		in
		  FTbl.insert env (f, (fdef, instRef));
		  instRef
		end
	  (* end case *))

    fun find (env : env, f) = FTbl.find env f

    end (* local *)
  end
