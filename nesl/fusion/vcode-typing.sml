(* vcode-typing.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Analyse the signature (i.e., typed of inputs and outputs) of VCode
 *)

structure VCodeTyping : sig

  (* an abstract object that contains type information about a program *)
    type typing

    datatype ty = INT | BOOL | FLOAT | CHAR | SEGDES | ANY

  (* the signature of an opcode, block, or function.  The types are in the reverse order of
   * the stack (i.e., the first type is deepest in the stack), which is the convention used
   * in the VExp representation.  A second component of NONE represents divergence or EXIT.
   *)
    type sign = (ty list * ty list option)

    val signToString : sign -> string

  (* a path that specifies the location of an if in a function. *)
    type path_to_if

    val rootPath : VCode.label -> path_to_if
    val thenPath : path_to_if -> path_to_if
    val elsePath : path_to_if -> path_to_if
    val endifPath : path_to_if -> path_to_if

  (* analyze a program returning a function that maps function labels to the
   * signature of the function.
   *)
    val analyze : VCodeCG.cg -> typing

  (* lookup a function type in a typing *)
    val typeOfLab : typing -> VCode.label -> sign

  (* lookup a type of an if *)
    val typeOfIf : typing -> path_to_if -> sign

  end = struct

    val debug = false	(* set to true to turn on debug output *)
    fun prl l = if debug then print(concat l) else ()

    structure V = VCode
    structure CG = VCodeCG
    structure ATbl = AtomTable

  (* utility function that combines map wth rev *)
    fun revmap f xs = let
	  fun mapf ([], l) = l
	    | mapf (x::xs, l) = mapf (xs, f x :: l)
	  in
	    mapf (xs, [])
	  end

    datatype ty = datatype V.ty

    type sign = (ty list * ty list option)	(* (in, out); NONE means divergence *)

  (* a path that specifies the location of an if in a function.  The path starts from the if
   * and proceeds back to the beginning of the function.
   *)
    datatype mark = THEN | ELSE | ENDIF
    type path_to_if = VCode.label * mark list

    fun rootPath lab = (lab, [])
    fun thenPath (lab, p) = (lab, THEN::p)
    fun elsePath (lab, p) = (lab, ELSE::p)
    fun endifPath (lab, p) = (lab, ENDIF::p)

  (* hash tables keyed by paths *)
    structure PathTbl = HashTableFn (
      struct
	type hash_key = path_to_if
	fun hashVal (lab, path) = let
	      fun f (THEN, w) = Word.<<(w, 0w2)
		| f (ELSE, w) = Word.<<(w, 0w2) + 0w1
		| f (ENDIF, w) = Word.<<(w, 0w2) + 0w2
	      in
		List.foldl f 0w0 path + Atom.hash lab
	      end
	fun sameKey ((lab1, p1) : path_to_if, (lab2, p2)) = Atom.same(lab1, lab2) andalso (p1 = p2)
      end)

  (* meta variables used to type stack slots *)
    datatype slot_ty
      = TY of ty
      | UNK of ty option ref

  (* a more compact string representation of types *)
    fun tyToString INT = "I"
      | tyToString BOOL = "B"
      | tyToString FLOAT = "F"
      | tyToString CHAR = "C"
      | tyToString SEGDES = "S"
      | tyToString ANY = "_"

    fun slotToString (TY ty) = tyToString ty
      | slotToString (UNK r) = (case !r
	   of SOME ty => tyToString ty
	    | NONE => "_"
	  (* end case *))

    fun stkToString stk = let
	  fun f [] = [" []"]
	    | f (slot::remaining) = " " :: slotToString slot :: f remaining
	  in
	    String.concat (f stk)
	  end

    fun signToString (inStk, resStk) = let
	  fun stk2s xs = String.concatWith " " (List.map tyToString xs)
	  val res = (case resStk
		 of NONE => "_|_"
		  | SOME xs => stk2s xs
		(* end case *))
	  in
	    concat[stk2s inStk, " --> ", res]
	  end

  (* we track the state of the stack as a pair, with the first element representing
   * our knowledge about the state of the stack at function entry and the second
   * representing the current stack depth.
   *)
    type stack_state = (slot_ty list * slot_ty list option)

    fun stateToString (inStk, SOME curStk) =
          String.concat[stkToString inStk, " ##", stkToString curStk]
      | stateToString (inStk, NONE) = stkToString inStk ^ " ## _|_"

  (* convert a stack slot to a type *)
    fun slotToTy (TY ty) = ty
      | slotToTy (UNK(ref(SOME ty))) = ty
      | slotToTy (UNK(ref NONE)) = ANY

  (* convert a stack state to a sign; this involves reversing the items'
   * order and converting slots into types.
   *)
    fun stateToSign (inStk, curStk) = let
	  val slotsToTys = revmap slotToTy
	  in
	    (slotsToTys inStk, Option.map slotsToTys curStk)
	  end

  (* convert a sign to a stack state by reversing the items' order and
   * tagging them with TY.
   *)
    fun signToState (inTys, outTys) = let
          val tag = revmap TY
          in
            (tag inTys, Option.map tag outTys)
          end

  (* compute the stack effect of an opcode, which is a pair of type lists describing
   * the stack inputs and outputs (resp.).  Note that these are in shallowest to
   * deepest order, which is the opposite of the VCODE manual.  This function
   * should not be called on CALL, IF, or stack manipulation opcodes.
   * See vcode_table.c for this info.
   *)
    fun signOfOp opcode = (case opcode
           of V.ADD ty => ([TY ty, TY ty], SOME[TY ty])
            | V.SUB ty => ([TY ty, TY ty], SOME[TY ty])
            | V.MUL ty => ([TY ty, TY ty], SOME[TY ty])
            | V.DIV ty => ([TY ty, TY ty], SOME[TY ty])
            | V.MOD => ([TY INT, TY INT], SOME[TY INT])
            | V.LT ty => ([TY ty, TY ty], SOME[TY BOOL])
            | V.LTE ty => ([TY ty, TY ty], SOME[TY BOOL])
            | V.GT ty => ([TY ty, TY ty], SOME[TY BOOL])
            | V.GTE ty => ([TY ty, TY ty], SOME[TY BOOL])
            | V.EQ ty => ([TY ty, TY ty], SOME[TY BOOL])
            | V.NEQ ty => ([TY ty, TY ty], SOME[TY BOOL])
            | V.LSHIFT => ([TY INT, TY INT], SOME[TY INT])
            | V.RSHIFT => ([TY INT, TY INT], SOME[TY INT])
            | V.NOT ty => ([TY ty], SOME[TY ty])
            | V.AND ty => ([TY ty, TY ty], SOME[TY ty])
            | V.OR ty => ([TY ty, TY ty], SOME[TY ty])
            | V.XOR ty => ([TY ty, TY ty], SOME[TY ty])
            | V.SELECT ty => ([TY ty, TY ty, TY BOOL], SOME[TY ty])
            | V.RAND => ([TY INT], SOME[TY INT])
            | V.FLOOR => ([TY FLOAT], SOME[TY INT])
            | V.CEIL => ([TY FLOAT], SOME[TY INT])
            | V.TRUNC => ([TY FLOAT], SOME[TY INT])
            | V.ROUND => ([TY FLOAT], SOME[TY INT])
            | V.I_TO_F => ([TY INT], SOME[TY FLOAT])
            | V.I_TO_B => ([TY INT], SOME[TY BOOL])
            | V.B_TO_I => ([TY BOOL], SOME[TY INT])
            | V.LOG => ([TY FLOAT], SOME[TY FLOAT])
            | V.SQRT => ([TY FLOAT], SOME[TY FLOAT])
            | V.EXP => ([TY FLOAT], SOME[TY FLOAT])
            | V.SIN => ([TY FLOAT], SOME[TY FLOAT])
            | V.COS => ([TY FLOAT], SOME[TY FLOAT])
            | V.TAN => ([TY FLOAT], SOME[TY FLOAT])
            | V.ASIN => ([TY FLOAT], SOME[TY FLOAT])
            | V.ACOS => ([TY FLOAT], SOME[TY FLOAT])
            | V.ATAN => ([TY FLOAT], SOME[TY FLOAT])
            | V.SINH => ([TY FLOAT], SOME[TY FLOAT])
            | V.COSH => ([TY FLOAT], SOME[TY FLOAT])
            | V.TANH => ([TY FLOAT], SOME[TY FLOAT])
          (* Vector instructions *)
            | V.ADD_SCAN ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.MUL_SCAN ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.MAX_SCAN ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.MIN_SCAN ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.AND_SCAN ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.OR_SCAN ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.XOR_SCAN ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.ADD_REDUCE ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.MUL_REDUCE ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.MAX_REDUCE ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.MIN_REDUCE ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.AND_REDUCE ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.OR_REDUCE ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.XOR_REDUCE ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.PERMUTE ty => ([TY SEGDES, TY INT, TY ty], SOME[TY ty])
            | V.DPERMUTE ty => ([TY SEGDES, TY SEGDES, TY ty, TY INT, TY ty], SOME[TY ty])
            | V.FPERMUTE ty => ([TY SEGDES, TY SEGDES, TY BOOL, TY INT, TY ty], SOME[TY ty])
            | V.BPERMUTE ty => ([TY SEGDES, TY SEGDES, TY INT, TY ty], SOME[TY ty])
            | V.BFPERMUTE ty => ([TY SEGDES, TY SEGDES, TY BOOL, TY INT, TY ty], SOME[TY ty])
            | V.DFPERMUTE ty =>
                ([TY SEGDES, TY SEGDES, TY ty, TY BOOL, TY INT, TY ty], SOME[TY ty])
            | V.EXTRACT ty => ([TY SEGDES, TY INT, TY ty], SOME[TY ty])
            | V.REPLACE ty => ([TY SEGDES, TY ty, TY INT, TY ty], SOME[TY ty])
            | V.PACK ty => ([TY SEGDES, TY BOOL, TY ty], SOME[TY SEGDES, TY ty])
            | V.RANK_UP ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.RANK_DOWN ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.DIST ty => ([TY SEGDES, TY ty], SOME[TY ty])
            | V.INDEX => ([TY SEGDES, TY INT, TY INT], SOME[TY INT])
            | V.LENGTH ty => ([TY ty], SOME[TY INT])
          (* Segment descriptor instructions *)
            | V.MAKE_SEGDES => ([TY INT], SOME[TY SEGDES])
            | V.LENGTHS => ([TY SEGDES], SOME[TY INT])
          (* Control instructions *)
            | V.CONST(ty, _) => ([], SOME[TY ty])
          (* I/O instructions *)
            | V.READ ty => ([], SOME[TY BOOL, TY CHAR, TY ty])
            | V.WRITE ty => ([TY ty], SOME[TY BOOL, TY CHAR])
            | V.FOPEN => ([TY INT, TY CHAR], SOME[TY BOOL, TY CHAR, TY INT])
            | V.FCLOSE => ([TY INT], SOME[TY BOOL, TY CHAR])
            | V.FWRITE ty => ([TY INT, TY ty], SOME[TY BOOL, TY CHAR])
            | V.FREAD ty => ([TY INT], SOME[TY BOOL, TY CHAR, TY ty])
            | V.FREAD_CHAR =>
                ([TY INT, TY INT, TY CHAR], SOME[TY BOOL, TY CHAR, TY INT, TY CHAR])
          (* undocumented instuctions *)
(* values from vcode_table.c, but they don't seem right
            | V.START_TIMER => (0, 1)
            | V.STOP_TIMER => (1, 1)
*)
            | V.START_TIMER => ([], SOME[])
            | V.STOP_TIMER => ([], SOME[TY FLOAT])
            | V.SRAND => ([TY INT], SOME[TY BOOL])
            | V.FUSED id => raise Fail "unexpected FUSED"
	  (* most control instructions are handled above *)
	    | _ => raise Fail("unexpected " ^ V.toString opcode)
          (* end case *))

    fun splitStk (stk, n) = let
          fun lp (0, prefix, rest) = (prefix, rest)
            | lp (i, prefix, x::rest) = lp (i-1, x::prefix, rest)
          in
            lp (n, [], stk)
          end

  (* extend a stack to at least the spcified depth by adding unknown slots *)
    fun extendStk (inStk, curStk, d) = let
	  fun f (0, _) = (inStk, curStk) (* no extension required *)
	    | f (d, []) = let
		val ext = List.tabulate (d, fn _ => UNK(ref NONE))
		in
		  (inStk @ ext, curStk @ ext)
		end
	    | f (d, _::r) = f (d-1, r)
	  in
	    f (d, curStk)
	  end

    fun unifySlot (ty, TY ty') = (ty = ty')
      | unifySlot (ty, UNK r) = (case !r
	   of NONE => (r := SOME ty; true)
	    | SOME ty' => (ty = ty')
	  (* end case *))

    fun unifySlots (TY ty1, TY ty2) = (ty1 = ty2)
      | unifySlots (TY ty, slot) = unifySlot (ty, slot)
      | unifySlots (slot, TY ty) = unifySlot (ty, slot)
      | unifySlots (UNK r1, UNK r2) = (case (!r1, !r2)
	   of (NONE, NONE) => true
	    | (SOME ty, NONE) => (r2 := SOME ty; true)
	    | (NONE, SOME ty) => (r1 := SOME ty; true)
	    | (SOME ty1, SOME ty2) => (ty1 = ty2)
	  (* end case *))

  (* update the stack state with the effect of an opcode. *)
    fun update ((inStk, SOME curStk), (args, SOME results)) = let
	  fun matchArgs ([], stk) = (inStk, SOME(results @ stk))
	    | matchArgs (remainingArgs, []) = (* need to grow the inStk *)
		(inStk @ remainingArgs, SOME results)
	    | matchArgs (arg::remainingArgs, slot::slots) =
		if unifySlots (arg, slot)
		  then matchArgs (remainingArgs, slots)
		  else (
		    print (concat[
			"** type mismatch in matchArgs:\n",
			"** inStk = ", stkToString inStk, "\n",
			"** stk   = ", stkToString curStk, "\n",
			"** args  = ", stkToString args, "\n"
		      ]);
		    raise Fail "type mismatch")
	  in
	    matchArgs (args, curStk)
	  end
      | update ((inStk, SOME curStk), (args, NONE)) = let
	  fun matchArgs ([], stk) = (inStk, NONE)
	    | matchArgs (remainingArgs, []) = (* need to grow the inStk *)
		(inStk @ remainingArgs, NONE)
	    | matchArgs (arg::remainingArgs, slot::slots) =
		if unifySlots (arg, slot)
		  then matchArgs (remainingArgs, slots)
		  else (
		    print (concat[
			"** type mismatch in matchArgs:\n",
			"** inStk = ", stkToString inStk, "\n",
			"** stk   = ", stkToString curStk, "\n",
			"** args  = ", stkToString args, "\n"
		      ]);
		    raise Fail "type mismatch")
	  in
	    matchArgs (args, curStk)
	  end
      | update ((inStk, NONE), _) = (inStk, NONE)

    fun mergeInStks (f, inStk1, inStk2) = let
	  fun merge ([], [], inStk) = List.rev inStk
	    | merge (remaining1, [], inStk) = List.revAppend(inStk, remaining1)
	    | merge ([], remaining2, inStk) = List.revAppend(inStk, remaining2)
	    | merge (slot1::remaining1, slot2::remaining2, inStk) =
		if unifySlots(slot1, slot2)
		  then merge (remaining1, remaining2, slot1::inStk)
		  else (
		    print (concat[
			"** merge in-stack mismatch in ", Atom.toString f, "\n",
			"** inStk1 = ", stkToString inStk1, "\n",
			"** inStk2 = ", stkToString inStk2, "\n"
		      ]);
		    raise Fail "type mismatch")
	  in
	    merge (inStk1, inStk2, [])
	  end

  (* merge two stack states from the two branches of an if *)
    fun merge (f, (inStk1, SOME curStk1), (inStk2, SOME curStk2)) = let
	  fun mergeStks ([], [], stk) = (mergeInStks(f, inStk1, inStk2), SOME(List.rev stk))
	    | mergeStks ([], remaining2, stk) =
		(mergeInStks(f, inStk1, inStk2), SOME(List.revAppend(stk, remaining2)))
	    | mergeStks (remaining1, [], stk) =
		(mergeInStks(f, inStk1, inStk2), SOME(List.revAppend(stk, remaining1)))
	    | mergeStks (slot1::remaining1, slot2::remaining2, stk) =
		if unifySlots(slot1, slot2)
		  then mergeStks (remaining1, remaining2, slot1::stk)
		  else (
		    print (concat[
			"** merge type mismatch in ", Atom.toString f, "\n",
			"** inStk1 = ", stkToString inStk1, "\n",
			"** stk1   = ", stkToString curStk1, "\n",
			"** inStk2 = ", stkToString inStk2, "\n",
			"** stk2   = ", stkToString curStk2, "\n"
		      ]);
		    raise Fail "type mismatch")
	  in
	    mergeStks (curStk1, curStk2, [])
	  end
      | merge (f, (inStk1, SOME curStk1), (inStk2, NONE)) =
	  (mergeInStks(f, inStk1, inStk2), SOME curStk1)
      | merge (f, (inStk1, NONE), (inStk2, SOME curStk2)) =
	  (mergeInStks(f, inStk1, inStk2), SOME curStk2)
      | merge (f, (inStk1, _), (inStk2, _)) = (mergeInStks(f, inStk1, inStk2), NONE)

    fun doBlock (typeOfLab : Atom.atom -> stack_state, pathTbl) lab (code, ss) = let
	  fun signOf ([], _, ss) = ss
	    | signOf (_, _, ss as (_, NONE)) = ss (* unreachable *)
	    | signOf (V.COPY(i, j)::r, path, (inStk, SOME curStk)) = let
(* DEBUG *)
val _ = prl ["signOf (COPY ", Int.toString i, " ", Int.toString j, "): ", stkToString inStk, " ##", stkToString curStk, "\n"];
(* DEBUG *)
		val (inStk, curStk) = extendStk (inStk, curStk, i+j)
		val (_, rest) = splitStk (curStk, j)
		val copied = List.take(rest, i)
		in
		  signOf (r, path, (inStk, SOME(copied @ curStk)))
		end
	    | signOf (V.POP(i, 0)::r, path, (inStk, SOME curStk)) = let
(* DEBUG *)
val _ = prl ["signOf (POP ", Int.toString i, " 0): ", stkToString inStk, " ##", stkToString curStk, "\n"];
(* DEBUG *)
		val (inStk, curStk) = extendStk (inStk, curStk, i)
		in
		  signOf (r, path, (inStk, SOME(List.drop(curStk, i))))
		end
	    | signOf (V.POP(i, j)::r, path, (inStk, SOME curStk)) = let
(* DEBUG *)
val _ = prl ["signOf (POP ", Int.toString i, " ", Int.toString j, "): ", stkToString inStk, " ##", stkToString curStk, "\n"];
(* DEBUG *)
		val (inStk, curStk) = extendStk (inStk, curStk, i+j)
		val (prefix, rest) = splitStk (curStk, j)
		in
		  signOf (r, path, (inStk, SOME(List.revAppend(prefix, List.drop(rest, i)))))
		end
	    | signOf (V.CPOP(i, j)::r, path, (inStk, SOME curStk)) = let
(* DEBUG *)
val _ = prl ["signOf (CPOP ", Int.toString i, " ", Int.toString j, "): ", stkToString inStk, " ##", stkToString curStk, "\n"];
(* DEBUG *)
		val (inStk, curStk) = extendStk (inStk, curStk, i+j)
		val (prefix, rest) = splitStk (curStk, j)
		val (copied, rest) = splitStk (rest, i)
		in
		  signOf (r, path, (inStk, SOME(List.revAppend(copied, List.revAppend(prefix, rest)))))
		end
	    | signOf (V.EXIT::_, _, (inStk, _)) = (inStk, NONE)
	    | signOf (V.CALL f::r, path, ss) = let
(* DEBUG *)
val _ = prl ["signOf (CALL ", Atom.toString f, "): ", stateToString ss, "\n"];
(* DEBUG *)
		val ss' = update(ss, typeOfLab f)
		in
		  signOf (r, path, ss')
		end
(******
	    | signOf (V.IF(b1, b2)::r, path, ss) = let
(* DEBUG *)
val _ = prl ["signOf (IF): ", stateToString ss, "\n"];
(* DEBUG *)
		val ss = update(ss, ([TY V.BOOL], SOME[]))
		val ss1 = signOf(b1, THEN::path, ss)
val _ = prl ["signOf (ELSE)\n"]
		val ss2 = signOf(b2, ELSE::path, ss)
(* DEBUG *)
val _ = prl ["signOf (ENDIF):\n  ss1 = ", stateToString ss1, "\n  ss2 = ", stateToString ss2, "\n"];
(* DEBUG *)
		val ss' = merge (lab, ss1, ss2)
(* DEBUG *)
val _ = prl ["  ss' = ", stateToString ss', "\n"];
(* DEBUG *)
		val sign = (case (ss, ss')
		       of ((_, SOME inStk1), (_, inStk2)) => stateToSign(inStk1, inStk2)
			| _ => raise Fail "impossible"
		      (* end case *))
		in
		(* record IF's type in path table *)
		  PathTbl.insert pathTbl ((lab, path), sign);
		  signOf (r, ENDIF::path, ss')
		end
*****)
	    | signOf (V.IF(b1, b2)::r, path, ss) = let
(* DEBUG *)
val _ = prl ["signOf (IF): ", stateToString ss, "\n"];
(* DEBUG *)
		val ss = update(ss, ([TY V.BOOL], SOME[]))
		val ss1 = signOf(b1, THEN::path, ([], SOME[]))
val _ = prl ["signOf (ELSE)\n"]
		val ss2 = signOf(b2, ELSE::path, ([], SOME[]))
(* DEBUG *)
val _ = prl ["signOf (ENDIF):\n  ss1 = ", stateToString ss1, "\n  ss2 = ", stateToString ss2, "\n"];
(* DEBUG *)
		val ss' = merge (lab, ss1, ss2)
(* DEBUG *)
val _ = prl ["  ss' = ", stateToString ss', "\n"];
(* DEBUG *)
		val sign = stateToSign ss'
		in
		(* record IF's type in path table *)
		  PathTbl.insert pathTbl ((lab, path), sign);
		  signOf (r, ENDIF::path, update (ss, ss'))
		end
	    | signOf (opcode::r, path, ss) = (
(* DEBUG *)
prl ["signOf (", VCode.toString opcode, "): ", stateToString ss, "\n"];
(* DEBUG *)
		signOf (r, path, update (ss, signOfOp opcode)))
	  in
	    signOf (code, [], ss)
	  end

    datatype typing = TYPING of {
	typeOfLab : VCode.label -> sign,
	typeOfPath : path_to_if -> sign
      }

    fun analyze {nodes, ndOfLab, topOrder} = let
          val nFuncs = List.length nodes
        (* maps labels to functions *)
          fun funcOfLab lab = let val CG.Nd(f, _, _) = ndOfLab lab in f end
(* FIXME: should attach type to node property list *)
        (* maps labels to stack states *)
          val tyTbl = let
                val tbl : stack_state ATbl.hash_table = ATbl.mkTable (nFuncs, Fail "tyTbl")
                val insert = ATbl.insert tbl
                in
                  List.app (fn (CG.Nd(V.FUNC(lab, _), _, _)) => insert(lab, ([], NONE))) nodes;
                  tbl
                end
          val typeOfLab = ATbl.lookup tyTbl
        (* maps paths to type info *)
          val pathTbl : sign PathTbl.hash_table = PathTbl.mkTable (2 * nFuncs, Fail "pathTbl")
	(* specialize the dBlock function *)
	  val doBlock = doBlock (typeOfLab, pathTbl)
        (* compute the signature of a function; return true if the sign changed from its
         * previous value.
         *)
          fun computeSig (V.FUNC(lab, body)) = let
		val initSS = typeOfLab lab
(* DEBUG *)
val _ = prl ["computeSign ", Atom.toString lab, " :", stateToString initSS, "\n"];
(* DEBUG *)
                val ss = let val paramTys = #1 initSS
		      in
			doBlock lab (body, (paramTys, SOME paramTys))
		      end
(* DEBUG *)
val _ = prl ["** typeOf ", Atom.toString lab, " =", stateToString ss, "\n"];
(* DEBUG *)
                val changed = let
		      fun changedSlot (TY ty1, TY ty2) = (ty1 <> ty2)
			| changedSlot (UNK r1, TY ty2) = (case !r1
			     of SOME ty1 => (ty1 <> ty2)
			      | _ => true
			    (* end case *))
			| changedSlot (UNK r1, UNK r2) = (case (!r1, !r2)
			     of (SOME ty1, SOME ty2) => (ty1 <> ty2)
			      | (NONE, SOME _) => true
			      | (NONE, NONE) => false
			      | _ => raise Fail "impossible"
			    (* end case *))
			| changedSlot _ = raise Fail "impossible"
		      fun changedStk (stk1, stk2) = ListPair.exists changedSlot (stk1, stk2)
		      in
			case (initSS, ss)
			 of ((inStk1, SOME outStk1), (inStk2, SOME outStk2)) =>
			      changedStk(inStk1, inStk2) orelse changedStk(outStk1, outStk2)
			  | ((inStk1, NONE), (inStk2, NONE)) => changedStk(inStk1, inStk2)
			  | ((inStk1, NONE), (inStk2, _)) => true
			  | _ => raise Fail "impossible"
			(* end case *)
		      end
                in
                  if changed then ATbl.insert tyTbl (lab, ss) else ();
                  changed
                end
          fun signOfNd (CG.Nd(f, _, _)) = computeSig f
          fun signOfGrp (CG.SIMPLE nd) = ignore (signOfNd nd)
            | signOfGrp (CG.RECURSIVE nds) = let
                fun lp () = if (List.exists signOfNd nds)
                      then lp()
                      else () (* we reached the fixed point *)
                in
                  lp()
                end
          val _ = List.app signOfGrp (List.rev topOrder)
          in
	    TYPING{
	      (* construct the signature map from the stack states *)
		typeOfLab = ATbl.lookup (ATbl.map stateToSign tyTbl),
		typeOfPath = PathTbl.lookup pathTbl
	      }
          end

  (* lookup a function type in a typing *)
    fun typeOfLab (TYPING{typeOfLab, ...}) = typeOfLab

  (* lookup a type of an if *)
    fun typeOfIf (TYPING{typeOfPath, ...}) = typeOfPath

  end
