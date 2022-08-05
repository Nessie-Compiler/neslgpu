(* arity-raise.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * A simple arity raiser for Flan.
 *)

structure ArityRaise : sig

    val transform : FlanEnv.env * Flan.program -> Flan.program

  end = struct

    structure F = Flan
    structure FTy = FlanTypes

    fun mapi f = let
	  fun mapf (_, []) = []
	    | mapf (i, x::xs) = f(i, x) :: mapf (i+1, xs)
	  in
	    fn l => mapf (0, l)
	  end

  (* this type is used to first characterize the call-site behavior of a function *)
    datatype flat_arg = FLAT of flat_arg list

    fun flatToString (FLAT[]) = "_"
      | flatToString (FLAT flts) = String.concat[
	    "{", String.concatWith " " (List.map flatToString flts), "}"
	  ]

    fun functToString (F.F{name, stamp, ...}) = Atom.toString name ^ Stamp.toString stamp
    val varToString = FlanVar.toString
    fun atomToString (F.Var x) = varToString x
      | atomToString _ = "<atom>"

    fun unify (a as FLAT[], _) = a
      | unify (_, b as FLAT[]) = b
      | unify (FLAT args1, FLAT args2) = FLAT(ListPair.mapEq unify (args1, args2))
(*DEBUG*) handle ex => raise ex

  (* map a Flan type to its flat-argument characterization *)
    fun tyToFlatArg (FTy.TyTuple tys) = FLAT(List.map tyToFlatArg tys)
      | tyToFlatArg _ = FLAT[]

  (* map a Flan argument to its flat-argument characterization *)
    fun atomToFlatArg (F.Var(F.V{kind, ...})) = (case !kind
	   of F.VK_Tuple atms => FLAT(List.map atomToFlatArg atms)
	    | _ => FLAT[]
	  (* end case *))
      | atomToFlatArg _ = FLAT[]

  (* a per-function property that tracks its callsites *)
    val {getFn=getSites, clrFn=clearSites, ...} = let
	  fun init (F.F{ty=ref(dom, _), ...}) = ref (List.map tyToFlatArg dom)
	  in
	    PropList.newProp (F.propsOf, init)
	  end

  (* the analysis pass determines the call sites of each function *)
    fun analyse tops = let
	  fun analTop (F.TopFun(_, defs)) = let
		fun doFn (f, params, e) = analExp e  (* the function's sites property is set on first access *)
		in
		  List.app doFn defs
		end
	    | analTop (F.TopBind(_, e)) = analExp e
	    | analTop (F.TopExp(e, _, _)) = analExp e
	  and analExp (F.ExpForEach _) = raise Fail "unexpected ForEach"
	    | analExp (F.ExpLet(binds, e)) = (
		List.app (fn (_, e) => analExp e) binds;
		analExp e)
	    | analExp (F.ExpTuple(_, _, e)) = analExp e
	    | analExp (F.ExpSeq(_, _, e)) = analExp e
	    | analExp (F.ExpPure _) = ()
	    | analExp (F.ExpCmd _) = ()
	    | analExp (F.ExpIf(_, e1, e2, _)) = (analExp e1; analExp e2)
	    | analExp (F.ExpApplyFun(f, args)) = let
		val sites = getSites f
		fun doArg (flt, x) = unify(flt, atomToFlatArg x)
		in
(
		  sites := ListPair.mapEq doArg (!sites, args)
) handle ex => (
print(concat["Apply(", functToString f, ", [",
String.concatWith ", " (List.map atomToString args),
"]); sites = ",
String.concatWith ", " (List.map flatToString (!sites)), "\n"]); raise ex)
		end
	    | analExp (F.ExpAtom _) = ()
	    | analExp (F.ExpLifted(e, _)) = analExp e
	  in
	    List.app analTop tops
	  end
(*DEBUG*) handle ex => raise ex

    fun clear (F.TopFun(_, defs)) = List.app (fn (f, _, _) => clearSites f) defs
      | clear _ = ()

    fun flattenTupleBinds ([], e) = e
      | flattenTupleBinds ((x, atms)::r, e) = flattenTupleBinds (r, F.mkTuple(x, atms, e))

    fun flattenParams (params, flts) = let
	  fun flatten ([], [], _, []) = NONE
	    | flatten ([], [], params, binds) = SOME(List.rev params, List.rev binds)
	    | flatten (x::xs, FLAT[] :: flts, params, binds) =
		flatten (xs, flts, x::params, binds)
	    | flatten (x::xs, flt :: flts, params, binds) = let
		fun flatten' (x, FLAT[], params, binds) = (x::params, binds)
		  | flatten' (x as F.V{name, ty, ...}, FLAT flts, params, binds) = let
		      val FTy.TyTuple tys = ty
		      val prefix = Atom.toString name
		      val vars = mapi
			    (fn (i, ty) => FlanVar.new(concat[prefix, "_", Int.toString(i+1)], ty))
			      tys
		      val binds = (x, List.map F.Var vars) :: binds
		      in
			List.app Census.incUseCnt vars; (* count the uses in the tuple construction *)
			ListPair.foldlEq
			  (fn (x, flt, (params, binds)) => flatten' (x, flt, params, binds))
			    (params, binds)
			      (vars, flts)
		      end
		val (params, binds) = flatten' (x, flt, params, binds)
		in
		  flatten (xs, flts, params, binds)
		end
	  in
	    flatten (params, flts, [], [])
	  end
(*DEBUG*) handle ex => raise ex

    fun flattenArgs (args, flts) = let
	  fun flatten ([], [], _, []) = NONE
	    | flatten ([], [], args, binds) = SOME(List.rev args, List.rev binds)
	    | flatten (x::xs, FLAT[] :: flts, args, binds) = (
		Census.incAtom x; (* count use of x as argument *)
		flatten (xs, flts, x::args, binds))
	    | flatten (F.Var x::xs, flt :: flts, args, binds) = let
		fun flatten' (x, FLAT[], args, binds) = (
		      Census.incUseCnt x; (* count use of x as argument *)
		      (F.Var x::args, binds))
		  | flatten' (x as F.V{name, ty, ...}, FLAT flts, args, binds) = let
		      val FTy.TyTuple tys = ty
		      val prefix = Atom.toString name
		      val vars = mapi
			    (fn (i, ty) => FlanVar.new(concat[prefix, "_", Int.toString(i+1)], ty))
			      tys
		      fun mkBind (i, y) = (
			    Census.incUseCnt x;
			    (y, F.mkPure(FlanPure.Base(Pure.PROJ(i+1)), [F.Var x])))
		      val newBinds = mapi mkBind vars
		      val binds = List.revAppend (newBinds, binds)
		      in
			ListPair.foldlEq
			  (fn (x, flt, (args, binds)) => flatten' (x, flt, args, binds))
			    (args, binds)
			      (vars, flts)
		      end
		val (args, binds) = flatten' (x, flt, args, binds)
		in
		  flatten (xs, flts, args, binds)
		end
	  in
	  (* first decrement argument use counts so that we don't count uses twice *)
	    List.app Census.decAtom args;
	  (* compute the flat argument list *)
	    flatten (args, flts, [], [])
	  end
(*DEBUG*) handle ex => raise ex

    fun transform (env, F.Program tops) = let
	  fun doTop (F.TopFun(f, defs)) = let
		fun doDef (f' as F.F{ty, ...}, params, e) = let
		      val flts = !(getSites f')
		      in
			case flattenParams (params, flts)
			 of SOME(params, binds) => let
			      val inst = (f', params, flattenTupleBinds(binds, doExp e))
			      in
				FlanEnv.rebindInstance (env, f, inst);
				ty := (List.map FlanVar.typeOf params, #2(!ty));
				inst
			      end
			  | NONE => (f', params, doExp e)
			(* end case *)
		      end
		in
		  F.TopFun(f, List.map doDef defs)
		end
	    | doTop (F.TopBind(lhs, e)) = F.TopBind(lhs, doExp e)
	    | doTop (F.TopExp(e, ty, props)) = F.TopExp(doExp e, ty, props)
	  and doExp (F.ExpForEach _) = raise Fail "unexpected ForEach"
	    | doExp (F.ExpLet(binds, e)) =
		F.mkLet (List.map (fn (lhs, e) => (lhs, doExp e)) binds, doExp e)
	    | doExp (F.ExpTuple(x, atms, e)) = F.mkTuple(x, atms, doExp e)
	    | doExp (F.ExpSeq(lhs, atms, e)) = F.mkSeq(lhs, atms, doExp e)
	    | doExp (e as F.ExpPure _) = e
	    | doExp (e as F.ExpCmd _) = e
	    | doExp (F.ExpIf(x, e1, e2, ty)) = F.mkIf(x, doExp e1, doExp e2, ty)
	    | doExp (e as F.ExpApplyFun(f, args)) = (
		case flattenArgs (args, !(getSites f))
		 of SOME(args, binds) => F.mkLet(binds, F.mkApplyFun(f, args))
		  | NONE => e
		(* end case *))
	    | doExp (e as F.ExpAtom _) = e
	    | doExp (F.ExpLifted(e, binds)) = F.mkLifted(doExp e, binds)
	  in
	    analyse tops;
	    F.Program(List.map doTop tops)
	      before List.app clear tops
	  end
(*DEBUG*) handle ex => raise ex

  end
