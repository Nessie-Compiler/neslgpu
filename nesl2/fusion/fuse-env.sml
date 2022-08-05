structure FuseEnv : sig
  (* We should probably keep track of kernels here too,
   * starting w/ a map from pure ops to kernels. *)

    type env

    (* create a new environment *)
    val new  : unit -> env

    (* insert a global-variable binding into the environment *)
    val bindGlobalVar : env * Flan.var * FuseAST.var -> unit

    (* insert a local-variable binding into the environment *)
    val bindLocalVar : env * Flan.var * FuseAST.var -> env

    (* lookup a variable in the environment *)
    val lookupVar : env * Flan.var -> FuseAST.var

    (* insert a function binding into the environment *)
    val bindFunct : env * Flan.funct * FuseAST.funct -> unit

    (* lookup a function in the environment *)
    val lookupFunct : env * Flan.funct -> FuseAST.funct

    (* bind a kernel (Top_Kern) in the environment *)
    val bindKern : env * ScalarOp.t * FuseAST.top -> unit
		   
    (* lookup a kernel in the environment *)
    val lookupKern : env * ScalarOp.t -> FuseAST.top option

    val listKerns : env -> FuseAST.top list

  end = struct

     structure FV = FlanVar
     structure FF = FlanFunct
     structure Fuse = FuseAST
     structure S = ScalarOp

     datatype env = E of {
	      vEnv : Fuse.var FV.Map.map,
	      vTbl : Fuse.var FV.Tbl.hash_table,
	      fTbl : Fuse.funct FF.Tbl.hash_table,
	      kTbl : Fuse.top ScalarOp.Tbl.hash_table
     }

     fun new () = E{
		  vEnv = FV.Map.empty,
		  vTbl = FV.Tbl.mkTable (32, Fail "vTbl"),
		  fTbl = FF.Tbl.mkTable (1024, Fail "fTbl"),
		  kTbl = S.Tbl.mkTable (1024, Fail "kTbl")
		  }

     fun bindGlobalVar (E{vTbl, ...}, x, x') = FV.Tbl.insert vTbl (x, x')

     fun bindLocalVar (E{vEnv, vTbl, fTbl, kTbl}, x, x') = 
	 E{vEnv=FV.Map.insert(vEnv, x, x'), vTbl=vTbl, fTbl=fTbl, kTbl=kTbl}

     fun lookupVar (E{vEnv, vTbl, ...}, x) = 
	 (case FV.Map.find(vEnv, x)
	   of NONE => (case FV.Tbl.find vTbl x
			of NONE => raise Fail("unable to find variable " ^ FV.toString x)
			 | SOME x' => x'
		      (* end case *))
	    | SOME x' => x'
	 (* end case *))

     fun bindFunct (E{fTbl, ...}, f, f') = FF.Tbl.insert fTbl (f, f')

     fun lookupFunct (E{fTbl, ...}, f) = 
	 (case FF.Tbl.find fTbl f
	   of NONE => raise Fail("unable to find function " ^ FF.toString f)
	    | SOME f' => f'
	 (* end case *))

     fun bindKern (E{kTbl, ...}, scalOp, k) = S.Tbl.insert kTbl (scalOp, k)

     fun lookupKern (E{kTbl, ...}, scalOp) = S.Tbl.find kTbl scalOp

     fun listKerns (E{kTbl, ...}) = S.Tbl.listItems kTbl

end
