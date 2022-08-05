(* lnesl.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Code to extract information about NESL's builtin functions
 * from the NESL ".lnesl" Lisp files.
 *)

structure LNesl =
  struct

    local
      structure A = Atom
      structure SE = SExp

    (* some standard atoms used in the lnesl files *)
      val a_defop = A.atom "defop"
      val a_defrec = A.atom "defrec"
      val a_set = A.atom "set"
      val a_bang = A.atom "!"
      val a_stub = A.atom ":stub"
      val a_primitive = A.atom ":primitive"

    (* atoms used in types *)
      val a_arrow = A.atom "<-"
      val a_any = A.atom "any"
      val a_pair = A.atom "pair"
      val a_stream = A.atom "stream"
      val a_logical = A.atom "logical"
      val a_ordinal = A.atom "ordinal"
      val a_number = A.atom "number"
      val a_alpha = A.atom "alpha"
      val a_beta = A.atom "beta"
      val a_v = A.atom "v."
    in

  (********** TYPES **********)

    datatype ty
      = INT | BOOL | CHAR | FLOAT | STREAM | SEGDES
      | FUNCTION (* function type for typecase *)
      | VAR of string
      | SEQ of ty
      | PAIR of (ty * ty)

    datatype class = ANY | LOGICAL | ORDINAL | NUMBER

    type fun_ty = {
	dom : ty list,
	rng : ty,
	bnds : (string * class) list
      }

    fun classToString ANY = "any"
      | classToString LOGICAL = "logical"
      | classToString ORDINAL = "ordinal"
      | classToString NUMBER = "number"

    fun tyToString ty = let
          fun toString (ty, l) = (case ty
                 of INT => "int" :: l
                  | BOOL => "bool" :: l
                  | CHAR => "char" :: l
                  | FLOAT => "float" :: l
                  | STREAM => "stream" :: l
                  | SEGDES => "segdes" :: l
                  | FUNCTION => "function" :: l
                  | VAR s => s :: l
                  | SEQ ty => "[" :: toString (ty, "]"::l)
                  | PAIR(ty1 as PAIR _, ty2) =>
                      "(" :: toString (ty1, "), " :: toString(ty2, l))
                  | PAIR(ty1, ty2) => toString (ty1, ", " :: toString(ty2, l))
                (* end case *))
          in
            String.concat(toString(ty, []))
          end

    fun funtyToString {dom, rng, bnds} = let
          fun tyToString (ty, l) = (case ty
                 of INT => "int" :: l
                  | BOOL => "bool" :: l
                  | CHAR => "char" :: l
                  | FLOAT => "float" :: l
                  | STREAM => "stream" :: l
                  | SEGDES => "segdes" :: l
                  | FUNCTION => "function" :: l
                  | VAR s => s :: l
                  | SEQ ty => "[" :: tyToString (ty, "]"::l)
                  | PAIR(ty1 as PAIR _, ty2) =>
                      "(" :: tyToString (ty1, "), " :: tyToString(ty2, l))
                  | PAIR(ty1, ty2) => tyToString (ty1, ", " :: tyToString(ty2, l))
                (* end case *))
          and tysToString ([], l) = l
            | tysToString ([ty as PAIR _], l) = "(" :: tyToString(ty, ")" :: l)
            | tysToString ([ty], l) = tyToString(ty, l)
            | tysToString ((ty as PAIR _) :: r, l) =
                "(" :: tyToString(ty, "), " :: tysToString(r, l))
            | tysToString (ty :: r, l) =
                tyToString(ty, ", " :: tysToString(r, l))
          fun bndToString (tv, cls, l) = tv :: " in " :: classToString cls :: l
          fun bndsToString ([], l) = l
            | bndsToString ([(tv, cls)], l) = bndToString (tv, cls, l)
            | bndsToString ((tv, cls)::r, l) =
                bndToString (tv, cls, "; " :: bndsToString(r, l))
	  val rng = " -> " :: tyToString(rng,
		      if null bnds
			then []
			else " :: (" :: bndsToString(bnds, [")"]))
          in
	    case dom
	     of [ty] => String.concat(tyToString(ty, rng))
	      | _ => String.concat("(" :: tysToString(dom, ")" :: rng))
	    (* end case *)
          end

    fun cvtTy (sexps : SE.value list) = let
	  fun error msg = (
		SExpPrinter.print(TextIO.stdErr, SE.LIST sexps);
		raise Fail("cannot parse type: " ^ msg))
	  fun cvtTySymb sym = let
		fun cvt ("v" :: r) = SEQ(cvt r)
		  | cvt ["int"] = INT
		  | cvt ["bool"] = BOOL
		  | cvt ["char"] = CHAR
		  | cvt ["float"] = FLOAT
		  | cvt ["stream"] = STREAM
		  | cvt ["segdes"] = SEGDES
                  | cvt ["function"] = FUNCTION
		  | cvt ["alpha"] = VAR "alpha"
		  | cvt ["beta"] = VAR "beta"
		  | cvt s = error (concat["bogus symbol \"", String.concatWith "." s, "\""])
		in
		  cvt (String.tokens (fn #"." => true | _ => false) (A.toString sym))
		end
	  fun cvt (SE.SYMBOL sym :: r) =
		if A.same(a_v, sym)
		  then let
		    val (ty, r) = cvtTy r
		    in
		      (SEQ ty, r)
		    end
		else if A.same(a_pair, sym)
		  then let
		    val (ty1, r) = cvtTy r
		    val (ty2, []) = cvtTy r
		    in
		      (PAIR(ty1, ty2), [])
		    end
		else if A.same(a_stream, sym)
		  then (STREAM, [])
		  else (cvtTySymb sym, r)
	    | cvt (SE.LIST(SE.SYMBOL first :: rest)::r) =
		if A.same(a_pair, first)
		  then let
		    val (ty1, rest) = cvtTy rest
		    val (ty2, _) = cvtTy rest
		    in
		      (PAIR(ty1, ty2), r)
		    end
		else if A.same(a_stream, first)
		  then (STREAM, [])
		  else error "not pair"
	    | cvt _ = error "syntax"
          in
            cvt sexps
          end

    fun cvtBnd v = let
	  fun error msg = (
		SExpPrinter.print(TextIO.stdErr, v);
		raise Fail("cannot parse type-variable bindings: " ^ msg))
	  in
	    case v
	     of (SE.LIST[SE.SYMBOL var, SE.SYMBOL cls]) => let
		  val var = A.toString var
		  in
		    if A.same(a_any, cls) then (var, ANY)
		    else if A.same(a_logical, cls) then (var, LOGICAL)
		    else if A.same(a_ordinal, cls) then (var, ORDINAL)
		    else if A.same(a_number, cls) then (var, NUMBER)
		    else error "expected class"
		  end
	      | v => error "expected type-variable binding"
	    (* end case *)
	  end

    fun cvtFunTy (SE.LIST((SE.LIST ty) :: restOfDef)) = let
	  fun error msg = (
		SExpPrinter.print(TextIO.stdErr, SE.LIST ty);
		raise Fail("cannot parse type: " ^ msg))
	  fun cvtTys ([], tys) = List.rev tys
	    | cvtTys (rest, tys) = let
		val (ty, rest) = cvtTy rest
		in
		  cvtTys (rest, ty::tys)
		end
	(* first, we get the return type *)
	  val (rngTy, rest) = cvtTy ty
	(* then consume the "<-" *)
	  val (rest, restOfDef) = (case rest
		 of SE.SYMBOL sym :: r =>
		      if A.same(a_arrow, sym) then (r, restOfDef) else error "missing <-"
		  | [] => (case restOfDef
		       of SE.SYMBOL sym :: r =>
			    if A.same(a_arrow, sym) then (r, []) else error "missing <-"
			| _ => error "missing <-"
		      (* end case *))
		  | _ => error "missing <-"
		(* end case *))
	(* get the domain *)
	  val dom = cvtTys (rest, [])
	(* then get the variable bounds *)
	  val bnds = List.map cvtBnd restOfDef
	  in
	    {dom = dom, rng = rngTy, bnds = bnds}
	  end
      | cvtFunTy ty = (SExpPrinter.print(TextIO.stdErr, ty); raise Fail "cannot parse function type")

  (********** Expressions **********)

    datatype exp
      = VarExp of string
      | ConstExp of string
      | OpExp of string * exp list
      | IfExp of exp * exp * exp
      | WithExp of (pat * exp) list * exp
      | OverExp of (pat * exp) list * exp
      | BaseTypecaseExp of (string * (ty * exp) list)
      | PolyTypecaseExp of (string * exp * exp * exp option)
      | PrimExp of string list

    and pat
      = VarPat of string
      | ConPat of string * pat list

    fun patToString pat = let
          fun toS (VarPat x, l) = x::l
            | toS (ConPat(con, pats), l) = "(" :: con :: patsToS(pats, ")" :: l)
          and patsToS ([], l) = l
	    | patsToS (pat::pats, l) = " " :: toS(pat, patsToS (pats, l))
	  in
	    String.concat(toS (pat, []))
	  end

    fun expToString exp = let
	  fun toS (VarExp x, l) = x::l
	    | toS (ConstExp c, l) = c::l
	    | toS (OpExp(rator, args), l) =
		"(" :: rator :: argsToS(args, ")" :: l)
	    | toS (IfExp(e1, e2, e3), l) =
		"(if" :: argsToS([e1, e2, e3], ")" :: l)
	    | toS (WithExp(binds, e), l) = "(with ...)" :: l (* FIXME *)
	    | toS (OverExp(binds, e), l) = "(over ...)" :: l (* FIXME *)
            | toS (BaseTypecaseExp(var, cases), l) =
                "(base-typecase " :: var :: " ...)" :: l (* FIXME *)
            | toS (PolyTypecaseExp(var, e1, e2, _), l) =
                "(poly-typecase " :: var :: " ...)" :: l (* FIXME *)
            | toS (PrimExp primops, l) = "(primitive: ...)" :: l (* FIXME *)
	  and argsToS ([], l) = l
	    | argsToS (arg::args, l) = " " :: toS(arg, argsToS (args, l))
	  in
	    String.concat(toS (exp, []))
	  end

  (* convert an S-expression to an exp *)
    fun cvtExp sexp = let
	  fun error msg = (
		SExpPrinter.print(TextIO.stdErr, sexp);
		raise Fail("cannot parse expression: " ^ msg))
	  fun cvtOp (SE.SYMBOL sym :: r) = (case A.toString sym
		 of "if" => (case List.map cvtArg r
		       of [e1, e2, e3] => IfExp(e1, e2, e3)
			| _ => error "malformed if"
		      (* end case *))
		  | "with" => (case r
		       of [SE.LIST binds, body] =>
			    WithExp(List.map cvtBind binds, cvtArg body)
			| _ => error "malformed with"
		      (* end case *))
		  | "over" => (case r
		       of [SE.LIST binds, body] =>
			    OverExp(List.map cvtBind binds, cvtArg body)
			| _ => error "malformed over"
		      (* end case *))
                  | "base-typecase" => (case r
                       of (SE.SYMBOL tv)::cases => let
                            fun cvtCase (SE.LIST[ty, exp]) =
                                  (#1(cvtTy [ty]), cvtArg exp)
                              | cvtCase _ = error "expected type-case rule"
                            in
                              BaseTypecaseExp(A.toString tv, List.map cvtCase cases)
                            end
                        | _ => error "malformed base-typecase"
		      (* end case *))
                  | "poly-typecase" => (case r
                       of [SE.SYMBOL tv, exp1, exp2] =>
                            PolyTypecaseExp(A.toString tv, cvtArg exp1, cvtArg exp2, NONE)
			| [SE.SYMBOL tv, exp1, exp2, exp3] =>
                            PolyTypecaseExp(A.toString tv, cvtArg exp1, cvtArg exp2, SOME(cvtArg exp3))
                        | _ => error "malformed poly-typecase"
		      (* end case *))
		  | ":primitive" => let
			fun cvtPrim (SE.LIST[SE.SYMBOL prim]) = A.toString prim
			  | cvtPrim (SE.LIST[SE.SYMBOL prim1, SE.SYMBOL prim2]) =
			      String.concat[A.toString prim1, " ", A.toString prim2]
			  | cvtPrim (SE.LIST[SE.SYMBOL prim1, SE.INT n]) =
			      String.concat[A.toString prim1, " ", IntInf.toString n]
			  | cvtPrim (SE.LIST[SE.SYMBOL prim1, SE.SYMBOL prim2, SE.INT m]) =
			      String.concat[A.toString prim1, " ", A.toString prim2, " ", IntInf.toString m]
			  | cvtPrim (SE.LIST[SE.SYMBOL prim1, SE.INT n, SE.INT m]) =
			      String.concat[A.toString prim1, " ", IntInf.toString n, " ", IntInf.toString m]
			  | cvtPrim _ = error "malformed primitive operator"
			in
			  PrimExp(List.map cvtPrim r)
			end
		  | rator => OpExp(rator, List.map cvtArg r)
		(* end case *))
	    | cvtOp _ = error "expected operator"
	  and cvtBind (SE.LIST[pat, value]) =
                (cvtPat pat, cvtArg value)
            | cvtBind _ = error "expected binding"
	  and cvtArg (SE.LIST exp) = cvtOp exp
	    | cvtArg (SE.SYMBOL sym) = VarExp(A.toString sym)
	    | cvtArg (SE.BOOL b) = ConstExp(Bool.toString b)
	    | cvtArg (SE.INT n) = ConstExp(Format.format "%d" [Format.LINT n])
	    | cvtArg (SE.FLOAT r) = ConstExp(Format.format "%f" [Format.REAL r])
	    | cvtArg (SE.STRING s) = ConstExp s
	  in
	    cvtArg sexp
	  end

    and cvtPat sexp = let
	  fun error msg = (
		SExpPrinter.print(TextIO.stdErr, sexp);
		raise Fail("cannot parse pattern: " ^ msg))
          fun cvt (SE.SYMBOL sym :: r) = ConPat(A.toString sym, List.map cvtAPat r)
            | cvt _ = error "expected variable"
          and cvtAPat (SE.LIST pat) = cvt pat
            | cvtAPat (SE.SYMBOL sym) =  VarPat(A.toString sym)
            | cvtAPat _ = error "expected atomic pattern"
          in
            cvtAPat sexp
          end

  (********** Definitions **********)

    datatype defn
      = Defop of {
	  name : string,
	  params : pat list,
	  ty : fun_ty option,
	  def : def
	}
      | Set of {name : string, value : exp}
      | Defrec of {
	  name : string,			(* datatype name *)
	  dom : ty,				(* domain type *)
	  bnds : (string * class) list		(* type-variable bounds *)
	}

    and def = STUB | PRIM of string | DEF of exp

    fun isSymbol sym (SE.SYMBOL sym' :: r) = A.same(sym, sym')
      | isSymbol _ _ = false

    fun parseDefOp ((SE.LIST params) :: rest) = let
	  val (name, params) = (case params
                 of (SE.SYMBOL name :: params) => (A.toString name, List.map cvtPat params)
                  | _ => raise Fail "expected operator and parameters"
                (* end case *))
	  val (ty, rest) = (case rest
		 of (SE.SYMBOL sym :: (ty as SE.LIST _) :: r) =>
		      if A.same(a_bang, sym)
			then (SOME(cvtFunTy ty), r)
			else (NONE, rest)
		  | _ => (NONE, rest)
		(* end case *))
handle ex => (print(concat["error parsing type of \"", name, "\"\n"]); raise ex)
	  val def = (case rest
		 of SE.SYMBOL sym :: _ =>
		      if A.same(a_stub, sym)
			then STUB
			else (
			  SExpPrinter.print(TextIO.stdErr, SE.LIST rest);
			  raise Fail "expected :stub")
		  | (exp as SE.LIST(SE.SYMBOL sym :: r)) :: _ =>
		      if A.same(a_primitive, sym)
                        then (case r
                           of [SE.SYMBOL f] => PRIM(A.toString f)
			    | [SE.LIST[SE.SYMBOL sym]] => PRIM(A.toString sym)
			    | [SE.LIST[SE.SYMBOL sym1, SE.SYMBOL sym2]] =>
				PRIM(concat[A.toString sym1, " ", A.toString sym2])
                            | _ => PRIM "<primitive>" (* FIXME *)
                          (* end case *))
                        else DEF(cvtExp exp)
		  | (item::r) => (
		      SExpPrinter.print(TextIO.stdErr, SE.LIST rest);
		      raise Fail "expected definition")
		  | [] => raise Fail "missing definition"
		(* end case *))
	  in
	    Defop{
		name = name,
		params = params,
		ty = ty,
		def = def
	      }
	  end
      | parseDefOp sexp = (
	  SExpPrinter.print(TextIO.stdErr, SE.LIST sexp);
	  raise Fail "expected defop body")

    fun parseDefRec (SE.LIST(SE.SYMBOL name :: tys) :: restOfDef) = let
	(* then get the variable bounds *)
	  fun cvtTys [] = raise Fail "expected type for defrec"
	    | cvtTys [ty] = #1 (cvtTy [ty])
	    | cvtTys tys = let
		val (ty, rest) = cvtTy tys
		in
		  PAIR(ty, cvtTys rest)
		end
	  val dom = cvtTys tys
	  val bnds = List.map cvtBnd restOfDef
	  in
	    Defrec{
		name = Atom.toString name,
		dom = dom,
		bnds = bnds
	      }
	  end
      | parseDefRec sexp = (
	  SExpPrinter.print(TextIO.stdErr, SE.LIST sexp);
	  raise Fail "expected defrec body")

    fun parseDef (sexp as (SE.LIST((SE.SYMBOL first) :: rest))) =
	  if A.same(a_defop, first)
	    then parseDefOp rest
	  else if A.same(a_defrec, first)
	    then parseDefRec rest
	  else if A.same(a_set, first)
	    then (case rest
	       of SE.SYMBOL name :: v :: _ => Set{name = A.toString name, value = cvtExp v}
		| _ => (
		    SExpPrinter.print(TextIO.stdErr, sexp);
		    raise Fail "expected set")
	      (* end case *))
	    else (
	      SExpPrinter.print(TextIO.stdErr, sexp);
	      raise Fail "not definition")
      | parseDef sexp = (
	  SExpPrinter.print(TextIO.stdErr, sexp);
	  raise Fail "expected list")

    fun doFile file = List.map parseDef (SExpParser.parseFile file)

  (* print out the name and type of a primitive operator *)
    fun prPrim (Defop{name, params, ty, def}) = let
          val out = (case def
                 of STUB => [" = stub\n"]
                  | PRIM s => [" = ", s, "\n"]
                  | DEF body => [" = \n  ", expToString body, "\n"]
                (* end case *))
          val out = (case ty
                 of SOME fty => ": " :: funtyToString fty :: out
                  | NONE => out
                (* end case *))
          val params = String.concatWith "," (List.map patToString params)
          in
            print(concat("function " :: name :: " (" :: params :: ") " :: out))
          end
      | prPrim (Defrec{name, dom, bnds}) = raise Fail "FIXME"
      | prPrim (Set{name, value}) = print(concat["set ", name, " = ", expToString value, "\n"])

    fun parScalar () = List.app prPrim (doFile "from-neslsrc/scalar-ops.lnesl")
    fun seqScalar () = List.app prPrim (doFile "from-neslseqsrc/scalar-ops.lnesl")

    fun parVector () = List.app prPrim (doFile "from-neslsrc/vector-ops.lnesl")
    fun seqVector () = List.app prPrim (doFile "from-neslseqsrc/vector-ops.lnesl")

    fun parIO () = List.app prPrim (doFile "from-neslsrc/io.lnesl")
    fun seqIO () = List.app prPrim (doFile "from-neslseqsrc/io-ops.lnesl")

    end (* local *)
  end
