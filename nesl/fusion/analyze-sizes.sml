(* analyze-sizes.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure AnalyzeSizes : sig

    val sizeOfVar : VExp.var -> Sizes.size

    val analyze : VExp.program -> VExp.program

  end = struct

    structure V = VExp
    structure CG = VExpCG
    structure S = Sizes
    structure VMap = V.VMap
    structure ATbl = AtomTable

  (* property to hold size information about a variable *)
    val {getFn=sizeOfVar, setFn=setSize, ...} = V.newProp (fn _ => Sizes.fresh())

    fun expSize (V.PURE(rator, args)) = S.pureSize(rator, List.map expSize args)
      | expSize (V.VAR x) = sizeOfVar x

  (* instantiate the size information for a function by making fresh copies of the
   * sizes.
   *)
    fun instance (paramSzs, resSzs) = let
	  fun copy (env, sz) = (case S.Map.find(env, sz)
		 of SOME sz' => (env, sz')
		  | NONE => let
		      val (env, sz') = (case S.kindOf(S.prune sz)
			     of S.UNKNOWN => (env, S.fresh())
			      | S.EQUAL _ => raise Fail "unexpected EQUAL"
			      | S.KNOWN n => (env, S.newSize(S.KNOWN n))
			      | S.SCALAR sz => let
				  val (env, sz') = copy(env, sz)
				  in
				    (env, S.newSize(S.SCALAR sz'))
				  end
			      | S.KNOWN_SCALAR n => (env, S.newSize(S.KNOWN_SCALAR n))
			    (* end case *))
		      in
			(S.Map.insert(env, sz, sz'), sz')
		      end
		(* end case *))
	  fun copyList ([], env, l) = (env, List.rev l)
	    | copyList (sz::szs, env, l) = let
		val (env, sz') = copy(env, sz)
		in
		  copyList (szs, env, sz'::l)
		end
	  val (env, paramSzs) = copyList (paramSzs, S.Map.empty, [])
	  val (_, resSzs) = copyList (resSzs, env, [])
	  in
	    (paramSzs, resSzs)
	  end

    fun stmSize tbl = let
	  fun size stm = (case stm
		 of V.LET(x, exp, stm) => (
		      setSize (x, expSize exp);
		      size stm)
		  | V.LETPACK(x1, x2, _, [a, b, c], stm) => (
		      ignore (S.unify(expSize a, expSize b));
		      ignore (expSize c);
		      setSize (x1, S.fresh());
		      setSize (x2, S.fresh());
		      size stm)
		  | V.CALL(xs, f, args, stm) => let
		      val argSzs = List.map expSize args
		      val (paramSzs, resSzs) = (case ATbl.find tbl f
			     of SOME funcSign => instance funcSign
			      | NONE => (
				    List.map (fn _ => S.fresh()) argSzs,
				    List.map (fn _ => S.fresh()) xs
				  )
			    (* end case *))
		      in
			ListPair.appEq (ignore o S.unify) (argSzs, paramSzs);
			ListPair.appEq setSize (xs, resSzs);
			size stm
		      end
		  | V.STMT(xs, io, args, stm) => let
		      val szs = S.ioSize(io, List.map expSize args)
		      in
			ListPair.appEq setSize (xs, szs);
			size stm
		      end
		  | V.IF(xs, e, thenStm, elseStm, joinStm) => let
		      val _ = S.unify(S.scalar(), expSize e)
		      val thenSz = size thenStm
		      val elseSz = size elseStm
		      val ifSz = ListPair.mapEq S.merge (thenSz, elseSz)
		      in
			ListPair.appEq setSize (xs, ifSz);
			size joinStm
		      end
		  | V.RET exps => List.map expSize exps
		  | V.EXIT => []
		(* end case *))
	  in
	    size
	  end

    and funcSize tbl (V.FUNC(f, params, _, body)) = let
	  val paramSzs = List.map
		(fn x => let val sz = S.fresh() in setSize(x, sz); sz end)
		  params
	  val sz = (paramSzs, stmSize tbl body)
	  in
(* DEBUG
print(concat["FUNC ", Atom.toString f, " : (", String.concatWith "," (List.map S.toString (#1 sz)),
") -> (", String.concatWith "," (List.map S.toString (#2 sz)), ")\n"]);
*)
	    ATbl.insert tbl (f, sz)
	  end

(* FIXME: use CG properties to hold function size info *)
    fun analyze prog = let
	  val funcSzTbl = ATbl.mkTable (List.length prog, Fail "funcSzTbl")
        (* build and sort the program's call graph *)
          val {nodes, ndOfLab, topOrder} = CG.mkCallGraph prog
          fun sizeOfNd (CG.Nd(f, _, _)) = funcSize funcSzTbl f
          fun sizeOfGrp (CG.SIMPLE nd) = sizeOfNd nd
            | sizeOfGrp (CG.RECURSIVE nds) = List.app sizeOfNd nds
	  in
	    List.app sizeOfGrp (List.rev topOrder);
	    prog
	  end

  end


