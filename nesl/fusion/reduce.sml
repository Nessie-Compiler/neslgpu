(* reduce.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Dead-variable elimination for VExp format.
 *)

structure Reduce : sig

  (* controls *)
    val deadVarElimFlg : bool ref

    val transform : VExp.program -> VExp.program

  end = struct

    structure VE = VExp
    structure VSet = VExp.VSet
    structure ATbl = AtomTable

  (* controls *)
    val deadVarElimFlg = ref true       (* eliminate unused variables *)

  (* variable tweaking *)
    fun useCnt (VE.V{useCnt, ...}) = !useCnt
    fun decCnt (VE.V{useCnt, ...}) = useCnt := !useCnt - 1
    fun setBinding (VE.V{binding, ...}, b) = binding := VE.VB_LET b

  (* apply a function to the variables in an expression *)
    fun applyToExp f e = let
          fun appf (VE.PURE(_, args)) = List.app appf args
            | appf (VE.VAR x) = f x
          in
            appf e
          end

    fun applyToExps f = List.app (applyToExp f)

  (* detect unused function parameters *)
    fun findUnusedParams prog = let
	  val elimTbl : int list ATbl.hash_table =
		ATbl.mkTable (List.length prog, Fail "elim table")
	  val insert = ATbl.insert elimTbl
	  fun doFunc (VE.FUNC(lab, params, _, _)) = let
		fun f (x, (i, elims)) = if (useCnt x = 0)
			then (i+1, i::elims)
			else (i+1, elims)
		val (_, elims) = List.foldl f (0, []) params
		in
		  insert (lab, List.rev elims)
		end
	  in
	    List.app doFunc prog;
	    ATbl.lookup elimTbl
	  end

    fun removeElements (xs, idxs) = let
	  fun remove (x::xs, i, idx::idxs) =
		if (i = idx)
		  then remove (xs, i+1, idxs)
		  else x :: remove (xs, i+1, idx::idxs)
	    | remove (xs, _, []) = xs
	    | remove ([], _, _) = []
	  in
	    remove (xs, 0, idxs)
	  end

  (* remove unused variable bindings. *)
    fun removeDead unusedParams (VE.FUNC(lab, params, resTy, body)) = let
          fun remove (VE.LET(x, e, stm)) = if useCnt x = 0
                then (applyToExp decCnt e; remove stm)
                else let
                  val stm = remove stm
                  in
                    if (useCnt x = 0)
                      then (applyToExp decCnt e; stm)
                      else VE.LET(x, e, stm)
                  end
            | remove (VE.LETPACK(x, y, ty, args, stm)) = if (useCnt x = 0) andalso (useCnt y = 0)
                then (applyToExps decCnt args; remove stm)
                else let
                  val stm = remove stm
                  in
                    if (useCnt x = 0) andalso (useCnt y = 0)
                      then (applyToExps decCnt args; stm)
                      else VE.LETPACK(x, y, ty, args, stm)
                  end
            | remove (VE.CALL(xs, f, args, stm)) = let
		val args = (case unusedParams f
		       of [] => args
			| elims => removeElements (args, elims)
		      (* end case *))
		in
		  VE.CALL(xs, f, args, remove stm)
		end
            | remove (VE.STMT(xs, opcode, args, stm)) = VE.STMT(xs, opcode, args, remove stm)
            | remove (VE.IF(xs, cond, s1, s2, s3)) =
                VE.IF(xs, cond, remove s1, remove s2, remove s3)
            | remove stm = stm
          in
            VE.FUNC(lab, List.filter (fn x => useCnt x <> 0) params, resTy, remove body)
          end

    fun transform prog =if !deadVarElimFlg
	  then List.map (removeDead (findUnusedParams prog)) prog
	  else prog

  end
