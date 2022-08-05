(* fun-env.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * This module provides an interface for instantiating NormalAST functions from
 * AST functions on demand.
 *)

structure FunEnv : sig

    type env = (Monomorphize.mono_env * NormalEnv.env)

(*    type instance = (Funct.funct * (NormalAST.funct * NormalAST.pat * NormalAST.exp)) *)
    type instance = (Funct.funct * (NormalAST.funct * NormalAST.var list * NormalAST.exp))

    val instantiate : env -> Funct.funct * NormalTy.ty list -> NormalAST.funct * instance list

  end = struct

(*    type env = (Monomorphize.fun_env * NormalEnv.env) *)
    type env = (Monomorphize.mono_env * NormalEnv.env)

    type instance = (Funct.funct * (NormalAST.funct * NormalAST.var list * NormalAST.exp))

    fun instantiate (monoEnv, normalEnv) (f, tys) = let
	(* first, we instantiate the monomorphic version of the function *)
	  val (f', instances') = Monomorphize.instantiate monoEnv (f, tys)
	(* normalize the instances *)
	  val instances'' = let
		fun cvt (f, inst) = let
		      val inst' = Normalize.normalizeFunct normalEnv inst
		      in
		      (* remember the binding from the monomorphic function f to its normalized version *)
			NormalEnv.bindFunct(normalEnv, #1 inst, #1 inst');
			(f, inst')
		      end
		in
		(* we have to process the instances in reverse order to guarantee that we bind
		 * functions in the normalEnv before the are used in other instances.
		 *)
		  List.rev (List.map cvt (List.rev instances'))
		end
	(* map the monomorphic function to the normalized function *)
	  val f'' = NormalEnv.lookupFunct (normalEnv, f')
	  in
(*
print(concat["instantiate ", Funct.toString f, " @ [",
String.concatWith "; " (map MonoTy.toString tys), "] --> ",
MonoFunct.toString f', " --> ", let val NormalAST.F{name, stamp, ...} = f''
in Atom.toString name ^ Stamp.toString stamp end, "\n"]);
*)
	    (f'', instances'')
	  end

  end
