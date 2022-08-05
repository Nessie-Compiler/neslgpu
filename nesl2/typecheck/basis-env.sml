(* basis-env.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * TODO:
 *    NESL defines three datatypes in neslsrc/types.lnesl and neslsrc/io.lnesl
 *
 *	datatype pair (a, b) :: (a in any; b in any);
 *	datatype vector (segdes, a) :: (a in any);
 *	datatype stream (int);
 *)

structure BasisEnv : sig

  (* a minimal basis environment intialized with the builtin types *)
    val env0 : Env.env

  end = struct

  (* initialize basis environment *)
    val env0 = let
	  fun insBaseTy ((name, ty), env) = Env.insertBasety(env, Atom.atom name, ty)
	(* insert base types *)
	  val env = List.foldl insBaseTy Env.empty [
		  ("int",	NeslTypes.tyInt),
		  ("bool",	NeslTypes.tyBool),
		  ("float",	NeslTypes.tyFloat),
		  ("char",	NeslTypes.tyChar),
		  ("string",	NeslTypes.tyString),
		  ("stream",	NeslTypes.tyStream),
		  ("segdes",	NeslTypes.tySegdes)
		]
	  in
	    env
	  end

  end
