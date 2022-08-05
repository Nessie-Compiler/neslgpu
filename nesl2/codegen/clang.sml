(* clang.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * An tree representation of programs in a C-like language (e.g., C, CUDA,
 * or OpenCL).
 *)

structure CLang =
  struct

    type var = string
    type attr = string          (* e.g., "static", "kernel", etc ... *)

    datatype num_ty
      = Int8 | UInt8
      | Int16 | UInt16
      | Int32 | UInt32
      | Int64 | UInt64
      | Float | Double

    datatype ty
      = T_Num of num_ty
      | T_Ptr of ty
      | T_RestrictPtr of ty	(* pointer type with "restrict" annotation *)
      | T_Array of ty * int option
      | T_Named of string
      | T_Template of string * ty list
      | T_Qual of attr * ty     (* qualified type *)

    datatype typed_var = V of ty * var

    val voidTy = T_Named "void"
    val voidPtr = T_Ptr voidTy
    val charTy = T_Named "char"
    val boolTy = T_Named "bool"
    val charPtr = T_Ptr charTy
    val charArrayPtr = T_Ptr charPtr 
    val intTy = T_Named "int"
    val int8Ty = T_Num Int8
    val uint8Ty = T_Num UInt8
    val int32Ty = T_Num Int32
    val uint32Ty = T_Num UInt32
    val int64Ty = T_Num Int64
    val floatTy = T_Num Float
    val doubleTy = T_Num Double

    datatype decl
      = D_Pragma of string list
      | D_Comment of string list
    (* verbatim text (e.g., preprocessor directives) *)
      | D_Verbatim of string list
    (* global variable declaration *)
      | D_Var of attr list * ty * var * initializer option
    (* function prototype *)
      | D_Proto of attr list * ty * string * param list
    (* function definition *)
      | D_Func of attr list * ty * string * param list * stm
    (* struct type declaration; if the third argument is SOME name, then a
     * typedef is generated.
     *)
      | D_StructDef of string option * (ty * string) list * string option

    and initializer
      = I_Exp of exp
      | I_Exps of initializer list
      | I_Struct of (string * initializer) list	(* C99 labeled struct initializer *)
      | I_Array of (int * initializer) list	(* C99 labeled array initializer *)

    and param = PARAM of attr list * ty * var

    and stm
      = S_Block of stm list             (* "{" stms "}" *)
      | S_Comment of string list
      | S_Verbatim of string list
      | S_Decl of attr list * ty * var * initializer option
                                        (* ty var [ '=' exp ]';' *)
      | S_Exp of exp                    (* exp ';' *)
      | S_If of exp * stm * stm         (* 'if' exp stm 'else' stm *)
      | S_While of exp * stm            (* 'while' exp stm *)
      | S_DoWhile of stm * exp          (* 'do' stm 'while' exp *)
      | S_For of (ty * var * exp) list * exp * exp list * stm
                                        (* 'for' '(' inits ';' exp ';' incrs ')' stm *)
      | S_KernCall of string * exp list * exp list
					(* f "<<<" ... ">>>" "(" ... ")" *)
      | S_Return of exp option          (* 'return' [ exp ] ';' *)
      | S_Break                         (* 'break' ';' *)
      | S_Continue                      (* 'continue' ';' *)

    and exp
      = E_Grp of exp                    (* "(" e ")" *)
      | E_AssignOp of exp * assignop * exp (* lvalue op= e *)
      | E_Cond of exp * exp * exp       (* e "?" e ":" e *)
      | E_BinOp of exp * binop * exp    (* e op e *)
      | E_UnOp of unop * exp            (* op e *)
      | E_PostOp of exp * postfix       (* e op *)
      | E_Apply of exp * exp list	(* f "(" ... ")" *)
      | E_Cons of ty * exp list		(* ty "(" ... ")" *)
      | E_New of ty * exp list		(* "new" ty "(" ... ")" *)
      | E_Subscript of exp * exp        (* e "[" e "]" *)
      | E_Select of exp * string        (* e "." f *)
      | E_Indirect of exp * string      (* e "->" f *)
      | E_Cast of ty * exp              (* "(" ty ")" e *)
      | E_Var of var
      | E_Int of IntInf.int * ty
      | E_Flt of string
      | E_Bool of bool
      | E_Str of string
      | E_Char of char
      | E_Sizeof of ty                  (* "sizeof(" ty ")" *)

  (* assignment operators *)
    and assignop
      = $= | += | *= | /= | %= | <<= | >>= | &= | ^= | |=

  (* binary operators in increasing order of precedence *)
    and binop
      = #||
      | #&&
      | #|
      | #^
      | #&
      | #== | #!=
      | #< | #<= | #>= | #>
      | #<< | #>>
      | #+ | #-
      | #* | #/ | #%

    and unop = %- | %! | %& | %* | %~ | %++ | %--

    and postfix = ^++ | ^--

  (* smart constructors that add E_Grp wrappers based on operator precedence *)
    local
      val commaP        = 0
      val assignP       = 1
      val condP         = 2
      val lorP          = 3
      val landP         = 4
      val borP          = 5
      val bxorP         = 6
      val bandP         = 7
      val eqP           = 8
      val relP          = 9
      val shiftP        = 10
      val addP          = 11
      val mulP          = 12
      val castP         = 13
      val unaryP        = 14
      val preP          = 15
      val compundP      = 16    (* compound literal *)
      val postP         = 17
      val callP         = 18
      val subP          = 19
      val atomP         = 20
      fun precOfBinop rator = (case rator
             of #|| => lorP
              | #&& => landP
              | #| => borP
              | #^ => bxorP
              | #& => bandP
              | #== => eqP | #!= => eqP
              | #< => relP | #<= => relP | #>= => relP | #> => relP
              | #<< => shiftP | #>> => shiftP
              | #+ => addP | #- => addP
              | #* => mulP | #/ => mulP | #% => mulP
            (* end case *))
      fun prec (E_Grp _) = atomP
        | prec (E_AssignOp _) = assignP
        | prec (E_Cond _) = condP
        | prec (E_BinOp(_, rator, _)) = precOfBinop rator
        | prec (E_UnOp _) = preP
        | prec (E_PostOp _) = postP
        | prec (E_Apply _) = callP
        | prec (E_Cons _) = callP (* check this *)
        | prec (E_New _) = callP (* check this *)
        | prec (E_Subscript _) = postP
        | prec (E_Select _) = postP
        | prec (E_Indirect _) = postP
        | prec (E_Cast _) = castP
        | prec (E_Var _) = atomP
        | prec (E_Int _) = atomP
        | prec (E_Flt _) = atomP
        | prec (E_Bool _) = atomP
        | prec (E_Str _) = atomP
        | prec (E_Char _) = atomP
        | prec (E_Sizeof _) = callP
    in
    fun mkGrp e = if (prec e < atomP) then E_Grp e else e
    fun mkAssignOp (e1, rator, e2) = let
          val e1' = if prec e1 < unaryP then E_Grp e1 else e1
          val e2' = if prec e2 < assignP then E_Grp e2 else e2
          in
            E_AssignOp(e1', rator, e2')
          end
  (* note that we over-parenthesize here, but it makes nested conditionals easeier to read *)
    fun mkCond (e1, e2, e3) = E_Cond(
          if prec e1 <= condP then E_Grp e1 else e1,
          if prec e2 <= condP then E_Grp e2 else e2,
          if prec e3 < condP then E_Grp e3 else e3)
  (* Note that all C binary operators are left associative. *)
    fun mkBinOp (e1, #-, e2 as E_UnOp(%-, _)) = let
          val e1' = if prec e1 < addP then E_Grp e1 else e1
          val e2' = E_Grp e2
          in
            E_BinOp(e1', #-, e2')
          end
      | mkBinOp (e1, rator, e2) = let
          val p = precOfBinop rator
          val e1' = if prec e1 < p then E_Grp e1 else e1
          val e2' = if prec e2 <= p then E_Grp e2 else e2
          in
            E_BinOp(e1', rator, e2')
          end
    fun mkUnOp (%-, e as E_UnOp(%-, _)) = E_UnOp(%-, E_Grp e)
      | mkUnOp (%-, e as E_UnOp(%--, _)) = E_UnOp(%-, E_Grp e)
      | mkUnOp (%--, e as E_UnOp(%-, _)) = E_UnOp(%--, E_Grp e)
      | mkUnOp (%--, e as E_UnOp(%--, _)) = E_UnOp(%--, E_Grp e)
      | mkUnOp (%&, E_UnOp(%*, e)) = e
      | mkUnOp (%*, E_UnOp(%&, e)) = e
      | mkUnOp (rator, e) = if prec e < unaryP
          then E_UnOp(rator, E_Grp e)
          else E_UnOp(rator, e)
    fun mkPostOp (e, rator) = if prec e < postP
          then E_PostOp(E_Grp e, rator)
          else E_PostOp(e, rator)
    fun mkApply (f, args) = E_Apply(E_Var f, args)
(* TODO: check precedence *)
    fun mkApplyExp (e, args) = E_Apply(e, args)
    val mkCons = E_Cons
    val mkNew = E_New
    fun mkSubscript(e1, e2) = if prec e1 < postP
          then E_Subscript(E_Grp e1, e2)
          else E_Subscript(e1, e2)
    fun mkSelect (e, f) = if prec e < postP
          then E_Select(E_Grp e, f)
          else E_Select(e, f)
    fun mkIndirect (e, f) = if prec e < postP
          then E_Indirect(E_Grp e, f)
          else E_Indirect(e, f)
    fun mkCast (ty, e) = if prec e < castP
          then E_Cast(ty, E_Grp e)
          else E_Cast(ty, e)
    val mkVar = E_Var
    fun mkIntTy (n, ty) = if n < 0 then E_UnOp(%-, E_Int(~n, ty)) else E_Int(n, ty)
    fun mkInt n = mkIntTy(n, intTy)
    val mkFlt = E_Flt
    val mkBool = E_Bool
    val mkStr = E_Str
    val mkChar = E_Char
    val mkSizeof = E_Sizeof
    end (* local *)

    val skip = S_Block[]

    local
      fun paren (e as E_Grp _) = e
        | paren e = E_Grp e
    in
    val mkComment = S_Comment
    fun mkBlock [stm] = stm
      | mkBlock stms = S_Block stms
    fun unBlock (S_Block stms) = stms
      | unBlock stm = [stm]
    fun prependStm (stm, blk) = mkBlock(stm :: unBlock blk)
    fun appendStm (blk, stm) = mkBlock(unBlock blk @ [stm])
    fun concatBlocks blocks = mkBlock(List.concat(List.map unBlock blocks))
    fun mkDecl (ty, x, init) = S_Decl([], ty, x, init)
    fun mkDeclInit (ty, x, init) = S_Decl([], ty, x, SOME(I_Exp init))
    val mkAttrDecl = S_Decl
    val mkExpStm = S_Exp
    fun mkAssign (e1, e2) = S_Exp(mkAssignOp(e1, $=, e2))
    fun mkAssign' (e1, rator, e2) = S_Exp(mkAssignOp(e1, rator, e2))
    fun mkIfThenElse (e, b1, b2) = S_If(paren e, b1, b2)
    fun mkIfThen (e, b) = mkIfThenElse (e, b, skip)
    val mkFor = S_For
    fun mkWhile (e, b) = S_While(paren e, b)
    fun mkDoWhile (b, e) = S_DoWhile(b, paren e)
    fun mkCall (f, args) = S_Exp(mkApply(f, args))
    fun mkCallExp (f, args) = S_Exp(mkApplyExp(f, args))
    val mkKernCall = S_KernCall
    val mkReturn = S_Return
    val mkBreak = S_Break
    val mkContinue = S_Continue
    end (* local *)

  (* utility functions *)

    fun varToString x = x

    fun assignopToString rator = (case rator
           of $= => "="
            | += => "+="
            | *= => "*="
            | /= => "/="
            | %= => "%="
            | <<= => "<<="
            | >>= => ">>="
            | &= => "&="
            | ^= => "^="
            | |= => "|="
          (* end case *))

    fun binopToString rator = (case rator
           of #|| => "||"
            | #&& => "&&"
            | #== => "=="
            | #| => "|"
            | #^ => "^"
            | #& => "&"
            | #!= => "!="
            | #< => "<"
            | #<= => "<="
            | #>= => ">="
            | #> => ">"
            | #<< => "<<"
            | #>> => ">>"
            | #+ => "+"
            | #- => "-"
            | #* => "*"
            | #/ => "/"
            | #% => "%"
          (* end case *))

    fun unopToString rator = (case rator
           of %- => "-"
            | %! => "!"
            | %& => "&"
            | %* => "*"
            | %~ => "~"
            | %++ => "++"
            | %-- => "--"
          (* end case *))

    fun postopToString rator = (case rator
           of ^++ => "++"
            | ^-- => "--"
          (* end case *))

  (* generate verbatim text from a template string by substituting for placeholders
   * Placeholders have the syntax @<id>@ and are replaced with the string associated
   * with <id> in the list of substitutions.  If <id> is empty, then no substitution
   * is applied, instead the "@@" is replaced by "@".
   *)
    local
      structure SS = Substring
      fun verbatim sl subs = let
            fun scan (start, ss, n, frags) = (case SS.getc ss
                   of SOME(#"@", rest) => let
                        val frags = SS.slice(start, 0, SOME n) :: frags
                        val (expansion, rest) = scanPlaceholder rest
                        in
                          scan (rest, rest, 0, expansion::frags)
                        end
                    | SOME(_, rest) => scan (start, rest, n+1, frags)
                    | NONE => SS.concat(List.rev(start::frags))
                  (* end case *))
            and scanPlaceholder start = let
                  fun scan (ss, n) = (case SS.getc ss
                         of NONE => raise Fail "incomplete placeholder"
                          | SOME(#"@", rest) => (SS.string(SS.slice(start, 0, SOME n)), rest)
                          | SOME(_, rest) => scan (rest, n+1)
                        (* end case *))
                  val (placeholder, rest) = scan (start, 0)
                  in
                    if (placeholder = "")
                      then (SS.full "@", rest)
                      else (case List.find (fn (s, _) => (s = placeholder)) subs
                         of SOME(_, expansion) => (SS.full expansion, rest)
                          | NONE => raise Fail(concat["unknown placeholder @", placeholder, "@"])
                        (* end case *))
                  end
            fun expand s = let
                  val ss = SS.full s
                  in
                    scan (ss, ss, 0, [])
                  end
            in
              List.map expand sl
            end
    in
    fun verbatimDcl sl subs = D_Verbatim(verbatim sl subs)
    fun verbatimStm sl subs = S_Verbatim(verbatim sl subs)
    end (* local *)

  (* for debugging (not syntactically correct!) *)
    fun expToString e = let
          fun e2s (e, l) = (case e
               of E_Grp e => "(" :: e2s(e, ")"::l)
                | E_AssignOp(e1, rator, e2) => e2s(e1, assignopToString rator :: e2s(e2, l))
                | E_Cond(e1, e2, e3) => "(" :: e2s(e1, "?" :: e2s(e2, ":" :: e2s (e3, ")" :: l)))
                | E_BinOp(e1, rator, e2) => e2s(e1, binopToString rator :: e2s(e2, l))
                | E_UnOp(rator, e) => unopToString rator :: e2s(e, l)
                | E_PostOp(e, rator) => e2s(e, postopToString rator :: l)
                | E_Apply(e, es) => let
                    fun args2s ([], l) = l
                      | args2s ([e], l) = e2s(e, l)
                      | args2s (e::es, l) = e2s(e, ","::args2s(es, l))
                    in
                      e2s(e, "(" :: args2s(es, ")"::l))
                    end
		| E_Cons(ty, e) => raise Fail "FIXME"
		| E_New(ty, e) => raise Fail "FIXME"
                | E_Subscript(e1, e2) => e2s(e1, "[" :: e2s(e2, "]"::l))
                | E_Select(e, f) => e2s(e, "." :: f :: l)
                | E_Indirect(e, f) => e2s(e, "->" :: f :: l)
                | E_Cast(ty, e) => "(ty)" :: e2s(e, l)  (* FIXME: need tyToString *)
                | E_Var x => x::l
                | E_Int(n, _) => IntInf.toString n :: l
                | E_Flt f => f :: l
                | E_Bool b => Bool.toString b :: l
                | E_Str s => "\"" :: String.toCString s :: "\"" :: l
                | E_Char c => "'" :: Char.toCString c :: "'" :: l
                | E_Sizeof ty => "sizeof(ty)" :: l
              (* end case *))
          in
            String.concat(e2s(e, []))
          end

  end
