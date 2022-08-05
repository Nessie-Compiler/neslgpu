(* sizes.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure Sizes =
  struct

    structure V = VExp

    type width = IntInf.int

    datatype kind
      = UNKNOWN
      | EQUAL of size		(* length equal to another size *)
      | KNOWN of width		(* known fixed length vector *)
      | SCALAR of size		(* scalar (length 1) vector with known value *)
      | KNOWN_SCALAR of width	(* scalar (length 1) vector with known value *)

    and size = SZ of {
	  id : word,
	  kind : kind ref
	}

    local
      val count = ref 0w0
    in
    fun newSize kind = let val id = !count
	  in
	    count := id + 0w1;
	    SZ{id = id, kind = ref kind}
	  end
    fun fresh () = newSize UNKNOWN
    fun scalar () = newSize(KNOWN 1)
    end

    structure Map = RedBlackMapFn(
      struct
	type ord_key = size
	fun compare(SZ{id=a, ...}, SZ{id=b, ...}) = Word.compare(a, b)
      end)

    fun same (SZ{id=id1, ...}, SZ{id=id2, ...}) = (id1 = id2)

    fun kindOf (SZ{kind, ...}) = !kind

    fun prune (sz as SZ{kind, ...}) : size = (case !kind
	   of EQUAL sz' => (case kindOf sz'
		 of k' as KNOWN _ => (kind := k'; sz')
		  | EQUAL sz' => let
		      val sz' = prune sz'
		      in
			kind := EQUAL sz'; sz'
		      end
		  | k' as KNOWN_SCALAR _ => (kind := k'; sz')
		  | _ => sz'
		(* end case *))
	    | SCALAR v => (case kindOf(prune v)
		 of KNOWN n => (kind := KNOWN_SCALAR n; sz)
		  | _ => sz
		(* end case *))
	    | _ => sz
	  (* end case *))

    fun toString sz = let
	  val SZ{id, kind} = prune sz
	  in
	    case !kind
	     of UNKNOWN => concat["<'a", Word.toString id, ">"]
	      | EQUAL sz => toString sz
	      | KNOWN n => concat["<", IntInf.toString n, ">"]
	      | SCALAR v => concat["<1>(=", toString v, ")"]
	      | KNOWN_SCALAR n => concat["<1>(=", IntInf.toString n, ")"]
	    (* end case *)
	  end

  (* remove any value information from a size *)
    fun strip sz = (case kindOf sz
	   of SCALAR _ => scalar()
	    | KNOWN_SCALAR _ => scalar()
	    | _ => sz
	  (* end case *))

  (* does sz1 occur in sz2 *)
    fun occurs (sz1, sz2) = (case kindOf sz2
	   of EQUAL sz => same(sz1, sz) orelse occurs(sz1, sz)
	    | SCALAR sz => same(sz1, sz) orelse occurs(sz1, sz)
	    | _ => false
	  (* end case *))

  (* unify two sizes that are known to be equal.  This operation does not
   * unify known scalar values, since the sizes may belong to different
   * vectors with different contents, but it will preserve such information.
   *)
    fun unify (sz1, sz2) = let
	    val sz1 as SZ{kind=k1, ...} = prune sz1
	    val sz2 as SZ{kind=k2, ...} = prune sz2
	    fun fail () = raise Fail(concat["unify(", toString sz1, ",", toString sz2, ")"])
	    in
	      case (!k1, !k2)
	       of (EQUAL _, _) => raise Fail "unexpected EQUAL"
		| (_, EQUAL _) => raise Fail "unexpected EQUAL"
		| (UNKNOWN, UNKNOWN) => if same(sz1, sz2) then sz1 else (k1 := EQUAL sz2; sz2)
		| (UNKNOWN, KNOWN n2) => (k1 := KNOWN n2; sz1)
		| (UNKNOWN, SCALAR _) => (k1 := KNOWN 1; sz1)
		| (UNKNOWN, KNOWN_SCALAR _) => (k1 := KNOWN 1; sz1)
		| (KNOWN n1, UNKNOWN) => (k2 := KNOWN n1; sz1)
		| (KNOWN n1, KNOWN n2) => if (n1 = n2) then sz1 else fail()
		| (KNOWN 1, SCALAR _) => sz1
		| (KNOWN 1, KNOWN_SCALAR _) => sz1
		| (KNOWN _, _) => fail()
		| (SCALAR _, UNKNOWN) => (k2 := KNOWN 1; sz2)
		| (SCALAR _, KNOWN 1) => sz2
		| (SCALAR _, KNOWN _) => fail()
		| (SCALAR _, SCALAR _) => scalar()
		| (SCALAR _, KNOWN_SCALAR _) => scalar()
		| (KNOWN_SCALAR _, UNKNOWN) => (k2 := KNOWN 1; sz2)
		| (KNOWN_SCALAR _, KNOWN 1) => sz2
		| (KNOWN_SCALAR _, KNOWN _) => fail()
		| (KNOWN_SCALAR _, SCALAR _) => scalar()
		| (KNOWN_SCALAR _, KNOWN_SCALAR _) => scalar()
	      (* end case *)
	    end
(* DEBUG
val unify = fn (sz1, sz2) => let
val _ = print(concat["unify(", toString sz1, ", ", toString sz2, ") = "]);
val sz = unify(sz1, sz2)
in
print(toString sz ^ "\n");
sz
end
*)

    fun unifyList [] = fresh()
      | unifyList (sz::szs) = let
	  fun u ([], sz') = sz'
	    | u (sz::szs, sz') = u(szs, unify(sz, sz'))
	  in
	    u (szs, sz)
	  end

    fun merge (sz1, sz2) = let
	val sz1 = prune sz1
	val sz2 = prune sz2
	in
	  if same(sz1, sz2)
	    then sz1
	    else (case (kindOf sz1, kindOf sz2)
	       of (EQUAL sz1, _) => raise Fail "unexpected EQUAL"
		| (_, EQUAL sz2) => raise Fail "unexpected EQUAL"
		| (KNOWN n1, KNOWN n2) => if (n1 = n2)
		    then newSize(KNOWN n1)
		    else fresh()
		| (SCALAR sz1, SCALAR sz2) => if same(sz1, sz2)
		    then newSize(SCALAR sz1)
		    else scalar()
		| (KNOWN_SCALAR n1, KNOWN_SCALAR n2) => if (n1 = n2)
		    then newSize(KNOWN_SCALAR n1)
		    else scalar()
		| (KNOWN 1, SCALAR _) => scalar()
		| (KNOWN 1, KNOWN_SCALAR _) => scalar()
		| (SCALAR _, KNOWN 1) => scalar()
		| (KNOWN_SCALAR _, KNOWN 1) => scalar()
		| (SCALAR _, KNOWN_SCALAR _) => scalar()
		| (KNOWN_SCALAR _, SCALAR _) => scalar()
		| _ => fresh()
	      (* end case *))
	  end
(* DEBUG
val merge = fn (sz1, sz2) => let
val _ = print(concat["merge(", toString sz1, ", ", toString sz2, ") = "]);
val sz = merge(sz1, sz2)
in
print(toString sz ^ "\n");
sz
end
*)

  (* type signatures for vcode operators *)
    fun pureSize (rator, args) = (case (rator, args)
	  (* Elementwise operations *)
	   of (V.ADD _, [a, b]) => unify(a, b)
	    | (V.SUB _, [a, b]) => unify(a, b)
	    | (V.MUL _, [a, b]) => unify(a, b)
	    | (V.DIV _, [a, b]) => unify(a, b)
	    | (V.MOD, [a, b]) => unify(a, b)
	    | (V.LT _, [a, b]) => unify(a, b)
	    | (V.LTE _, [a, b]) => unify(a, b)
	    | (V.GT _, [a, b]) => unify(a, b)
	    | (V.GTE _, [a, b]) => unify(a, b)
	    | (V.EQ _, [a, b]) => unify(a, b)
	    | (V.NEQ _, [a, b]) => unify(a, b)
	    | (V.LSHIFT, [a, b]) => unify(a, b)
	    | (V.RSHIFT, [a, b]) => unify(a, b)
	    | (V.NOT _, [a]) => strip a
	    | (V.AND _, [a, b]) => unify(a, b)
	    | (V.OR _, [a, b]) => unify(a, b)
	    | (V.XOR _, [a, b]) => unify(a, b)
	    | (V.SELECT _, [a, b, c]) => unifyList[a, b, c]
	    | (V.RAND, [a]) => strip a
	    | (V.FLOOR, [a]) => strip a
	    | (V.CEIL, [a]) => strip a
	    | (V.TRUNC, [a]) => strip a
	    | (V.ROUND, [a]) => strip a
	    | (V.I_TO_F, [a]) => strip a
	    | (V.I_TO_B, [a]) => strip a
	    | (V.B_TO_I, [a]) => strip a
	    | (V.LOG, [a]) => strip a
	    | (V.SQRT, [a]) => strip a
	    | (V.EXP, [a]) => strip a
	    | (V.SIN, [a]) => strip a
	    | (V.COS, [a]) => strip a
	    | (V.TAN, [a]) => strip a
	    | (V.ASIN, [a]) => strip a
	    | (V.ACOS, [a]) => strip a
	    | (V.ATAN, [a]) => strip a
	    | (V.SINH, [a]) => strip a
	    | (V.COSH, [a]) => strip a
	    | (V.TANH, [a]) => strip a
	  (* Vector instructions *)
	    | (V.ADD_SCAN _, [a, b]) => strip a
	    | (V.MUL_SCAN _, [a, b]) => strip a
	    | (V.MAX_SCAN _, [a, b]) => strip a
	    | (V.MIN_SCAN _, [a, b]) => strip a
	    | (V.AND_SCAN _, [a, b]) => strip a
	    | (V.OR_SCAN _, [a, b]) => strip a
	    | (V.XOR_SCAN _, [a, b]) => strip a
	    | (V.ADD_REDUCE _, [a, b]) => strip b
	    | (V.MUL_REDUCE _, [a, b]) => strip b
	    | (V.MAX_REDUCE _, [a, b]) => strip b
	    | (V.MIN_REDUCE _, [a, b]) => strip b
	    | (V.AND_REDUCE _, [a, b]) => strip b
	    | (V.OR_REDUCE _, [a, b]) => strip b
	    | (V.XOR_REDUCE _, [a, b]) => strip b
	    | (V.PERMUTE _, [a, b, c]) => unify(a, b)
	    | (V.DPERMUTE _, [a, b, c, d, e]) => (
		ignore(unify(d, e)); unify(b, c))
	    | (V.FPERMUTE _, [a, b, c, d, e]) => (
		ignore(unifyList[a, b, c]); ignore(unify(d, e)); fresh())
	    | (V.BPERMUTE _, [a, b, c, d]) => (
		ignore(unify(c, d)); unify(a, b))
	    | (V.BFPERMUTE _, [a, b, c, d, e]) => (
		ignore(unify(d, e)); unify(b, c))
	    | (V.DFPERMUTE _, [a, b, c, d, e, f]) => (
		ignore(unify(a, unify(b, c))); ignore(unify(e, f)); d)
	    | (V.EXTRACT _, [a, b, c]) => unify(b, c)
	    | (V.REPLACE _, [a, b, c, d]) => (ignore(unifyList[b, c, d]); a)
	    | (V.RANK_UP _, [a, b]) => strip a
	    | (V.RANK_DOWN _, [a, b]) => strip a
	    | (V.DIST _, [a, b]) => (
		ignore (unify(a, b));
		case kindOf b
		 of SCALAR sz => newSize(EQUAL sz)
		  | KNOWN_SCALAR n => newSize(KNOWN n)
		  | _ => fresh()
		(* end case *))
	    | (V.INDEX, [a, b, c]) => (
		ignore(unify(a, unify(b, c))); fresh())
	    | (V.LENGTH _, [a]) => (case kindOf(prune a)
		 of UNKNOWN => newSize(SCALAR a)
		  | EQUAL _ => raise Fail "unexpected EQUAL"
		  | KNOWN n => newSize(KNOWN_SCALAR n)
		  | _ => newSize(KNOWN_SCALAR 1)
		(* end case *))
	  (* Segment descriptor instructions *)
	    | (V.MAKE_SEGDES, [a]) => a
	    | (V.LENGTHS, [a]) => a
	    | (V.CONST(V.INT, [n]), []) => newSize(KNOWN_SCALAR(valOf(IntInf.fromString n)))
	    | (V.CONST(V.SEGDES, [n]), []) => newSize(KNOWN_SCALAR(valOf(IntInf.fromString n)))
	    | (V.CONST(_, vs), []) => newSize(KNOWN(IntInf.fromInt(List.length vs)))
	    | _ => raise Fail("bogus operation: " ^ V.pureToString rator)
	  (* end case *))
(* DEBUG
val pureSize = fn (arg as (rator, args)) => let
  val res = (pureSize arg) handle ex => (
  print(concat["pureSize (", V.pureToString rator, ", [",
  String.concatWith "," (List.map toString args), "])\n"]); raise ex)
  in res end
*)

    fun ioSize (rator, args) = (case (rator, args)
	   of (V.READ _, []) => [scalar(), fresh(), fresh()]
	    | (V.WRITE _, [_]) => [scalar(), fresh()]
	    | (V.FOPEN, [a, b]) => (
		ignore(unify(b, scalar())); [scalar(), fresh(), scalar()])
	    | (V.FCLOSE, [a]) => (ignore(unify(a, scalar())); [scalar(), fresh()])
	    | (V.FWRITE _, [a, b]) => (
		ignore(unify(b, scalar())); [scalar(), fresh()])
	    | (V.FREAD _, [a]) => (
		ignore(unify(a, scalar())); [scalar(), fresh(), fresh()])
	    | (V.FREAD_CHAR, [a, b, c]) => (
		ignore(unify(a, scalar())); [scalar(), fresh(), scalar(), fresh()])
	    | (V.START_TIMER, []) => []
	    | (V.STOP_TIMER, []) => [scalar()]
	    | (V.SRAND, [a]) => (ignore(unify(scalar(), a)); [scalar()])
	    | _ => raise Fail("bogus IO operation: " ^ V.ioToString rator)
	  (* end case *))

  end

(* test code

function MAIN1_1 ()
    let t00#2 = (CONST INT 10000000)			; t00 : INT<1>(=10000000)
    let t02#2 = (MAKE_SEGDES @ (CONST INT 10000000))	; t02 : SEGDES<1>(=10000000)
    let t03#1 = (DIST INT @ (CONST INT 100) t02)	; t03 : INT<10000000>
    let t04#1 = (RAND @ t03)				; t04 : INT<10000000>
    let t06#1 = (MAKE_SEGDES @ t00)			; t06 : SEGDES<1>(=10000000)
    let t07#1 = (DIST INT @ (CONST INT 100) t06)	; t07 : INT<10000000>
    let t08#1 = (RAND @ t07)				; t08 : INT<10000000>
    let t10#1 = (MAKE_SEGDES @ t00)			; t10 : SEGDES<10000000>
    let t11#1 = (DIST INT @ (CONST INT 100) t10)	; t11 : INT<10000000>
    let t12#1 = (RAND @ t11)				; t12 : INT<10000000>
    let () = (START_TIMER)
    let t15#1 = (+ INT @ ( * INT @ t04 t08) t12)		; t15 : INT<10000000>
    let u017#1 = (STOP_TIMER)
    let t19#1 = (EXTRACT INT @ t15 (CONST INT 0) t02)	; t19 : INT<1>
    in
      RET (t19, u017)

*)

(*
local
  open VExp Sizes
in
  val t00 = pureSize(CONST(INT, ["10000000"]), [])
  val t01 = pureSize(CONST(INT, ["10000000"]), [])
  val t02 = pureSize(MAKE_SEGDES, [t01])
  val t03 = pureSize(DIST INT, [pureSize(CONST(INT, ["100"]), []), t02])
  val t04 = pureSize(RAND, [t03])
  val t06 = pureSize(MAKE_SEGDES, [t00])
  val t07 = pureSize(DIST INT, [pureSize(CONST(INT, ["100"]), []), t06])
  val t08 = pureSize(RAND, [t07])
  val t10 = pureSize(MAKE_SEGDES, [t00])
  val t11 = pureSize(DIST INT, [pureSize(CONST(INT, ["100"]), []), t10])
  val t12 = pureSize(RAND, [t11])
end
*)
