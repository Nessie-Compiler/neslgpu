(* env.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Env : sig

    datatype ty_binding
      = Base of NeslTypes.ty	(* builtin base type (e.g., int, char, etc.) *)
      | Data of Dataty.dataty	(* user-defined data type *)
      | TyVar of TyVar.tyvar	(* type variable *)

    datatype binding
      = Fun of Funct.funct	(* function *)
      | Var of Var.var		(* variable *)
      | Cons of Dataty.dataty	(* data constructor *)

    type env

    val empty : env

    val insertBasety : env * Atom.atom * NeslTypes.ty -> env
    val insertDataty : env * Dataty.dataty -> env
    val insertTyvar : env * TyVar.tyvar -> env
    val insertFun : env * Funct.funct -> env
    val insertVar : env * Var.var -> env

    val findTyBind : env * Atom.atom -> ty_binding option

    val findVarBind : env * Atom.atom -> binding option

    val findCons : env * Atom.atom -> Dataty.dataty option

  (*DEBUG *)
    val dumpVarEnv : env * string -> unit

  end = struct

    structure AMap = AtomMap

    datatype ty_binding
      = Base of NeslTypes.ty	(* builtin type (e.g., int, char, etc.) *)
      | Data of Dataty.dataty	(* user-defined data type *)
      | TyVar of TyVar.tyvar	(* type variable *)

    datatype binding
      = Fun of Funct.funct	(* function *)
      | Var of Var.var		(* variable *)
      | Cons of Dataty.dataty	(* data constructor *)

    datatype env = Env of {
	types : NeslTypes.dataty list,			(* list of all datatypes *)
	tyEnv : ty_binding AMap.map,			(* type environment *)
	vEnv  : binding AMap.map			(* binding environment *)
      }

    val empty = Env{types = [], tyEnv = AMap.empty, vEnv = AMap.empty}

    fun insertBasety (Env{types, tyEnv, vEnv}, id, ty) = Env{
	    types = types,
	    tyEnv = AMap.insert(tyEnv, id, Base ty),
	    vEnv = vEnv
	  }

    fun insertDataty (Env{types, tyEnv, vEnv}, dt) = Env{
	    types = dt::types,
	    tyEnv = AMap.insert(tyEnv, Dataty.nameOf dt, Data dt),
	    vEnv = AMap.insert(vEnv, Dataty.nameOf dt, Cons dt)
	  }

    fun insertTyvar (Env{types, tyEnv, vEnv}, tv) = Env{
	    types = types,
	    tyEnv = AMap.insert(tyEnv, TyVar.nameOf tv, TyVar tv),
	    vEnv = vEnv
	  }

    fun findTyBind (Env{tyEnv, ...}, id) = AMap.find(tyEnv, id)

    fun insertFun (Env{types, tyEnv, vEnv}, f) = Env{
	    types = types,
	    tyEnv = tyEnv,
	    vEnv = AMap.insert(vEnv, Funct.nameOf f, Fun f)
	  }

    fun insertVar (Env{types, tyEnv, vEnv}, x) = Env{
	    types = types,
	    tyEnv = tyEnv,
	    vEnv = AMap.insert(vEnv, Var.nameOf x, Var x)
	  }

    fun findVarBind (Env{vEnv, ...}, x) = AMap.find(vEnv, x)

    fun findCons (Env{vEnv, ...}, dt) = (case AMap.find(vEnv, dt)
	   of SOME(Cons cons) => SOME cons
	    | _ => NONE
	  (* end case *))

  (*+DEBUG *)
    fun dumpVarEnv (Env{vEnv, ...}, msg) = let
	  fun prBinding (x, Fun f') = print (concat[
		  "  ", Atom.toString x, " : ", NeslTypes.schemeToString(Funct.typeOf f'), "\n"
		])
	    | prBinding (x, Var x') = print (concat[
		  "  ", Atom.toString x, " : ", NeslTypes.toString(Var.typeOf x'), "\n"
		])
	    | prBinding (x, Cons _) = ()
	  in
	    print(concat["***** ", msg, " *****\n"]);
	    AMap.appi prBinding vEnv;
	    print "***** end *****\n"
	  end
  (*-DEBUG*)

  end
