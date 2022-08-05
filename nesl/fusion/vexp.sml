(* vexp.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * An expression representation of VCODE programs.
 *)

structure VExp =
  struct

    datatype ty = datatype VCode.ty
    type value = VCode.value
    type label = VCode.label

  (* pure VCODE instructions *)
    datatype pure
    (* Elementwise operations *)
      = ADD of ty               (* + {INT, FLOAT} *)
      | SUB of ty               (* - {INT, FLOAT} *)
      | MUL of ty               (* * {INT, FLOAT} *)
      | DIV of ty               (* / {INT, FLOAT} *)
      | MOD                     (* % *)
      | LT of ty                (* < {INT, FLOAT} *)
      | LTE of ty               (* <= {INT, FLOAT} *)
      | GT of ty                (* > {INT, FLOAT} *)
      | GTE of ty               (* >= {INT, FLOAT} *)
      | EQ of ty                (* = {INT, FLOAT} *)
      | NEQ of ty               (* != {INT, FLOAT} *)
      | LSHIFT		        (* LSHIFT *)
      | RSHIFT		        (* RSHIFT *)
      | NOT of ty               (* NOT {BOOL, INT} *)
      | AND of ty               (* AND {BOOL, INT} *)
      | OR of ty                (* OR {BOOL, INT} *)
      | XOR of ty               (* XOR {BOOL, INT} *)
      | SELECT of ty            (* SELECT {INT, BOOL, FLOAT} *)
      | RAND                    (* RAND *)
      | FLOOR                   (* FLOOR *)
      | CEIL                    (* CIEL *)
      | TRUNC                   (* TRUNC *)
      | ROUND                   (* ROUND *)
      | I_TO_F                  (* I_TO_F *)
      | I_TO_B                  (* I_TO_B *)
      | B_TO_I                  (* B_TO_I *)
      | LOG		        (* LOG *)
      | SQRT		        (* SQRT *)
      | EXP		        (* EXP *)
      | SIN		        (* SIN *)
      | COS		        (* COS *)
      | TAN		        (* TAN *)
      | ASIN		        (* ASIN *)
      | ACOS		        (* ACOS *)
      | ATAN		        (* ATAN *)
      | SINH		        (* SINH *)
      | COSH		        (* COSH *)
      | TANH		        (* TANH *)
    (* Vector instructions *)
      | ADD_SCAN of ty          (* +_SCAN {INT, FLOAT} *)
      | MUL_SCAN of ty          (* *_SCAN {INT, FLOAT} *)
      | MAX_SCAN of ty          (* MAX_SCAN {INT, FLOAT} *)
      | MIN_SCAN of ty          (* MIN_SCAN {INT, FLOAT} *)
      | AND_SCAN of ty          (* AND_SCAN {INT, BOOL} *)
      | OR_SCAN of ty           (* OR_SCAN {INT, BOOL} *)
      | XOR_SCAN of ty          (* XOR_SCAN {INT, BOOL} *)
      | ADD_REDUCE of ty        (* +_REDUCE {INT, FLOAT} *)
      | MUL_REDUCE of ty        (* *_REDUCE {INT, FLOAT} *)
      | MAX_REDUCE of ty        (* MAX_REDUCE {INT, FLOAT} *)
      | MIN_REDUCE of ty        (* MIN_REDUCE {INT, FLOAT} *)
      | AND_REDUCE of ty        (* AND_REDUCE {INT, BOOL} *)
      | OR_REDUCE of ty         (* OR_REDUCE {INT, BOOL} *)
      | XOR_REDUCE of ty        (* XOR_REDUCE {INT, BOOL} *)
      | PERMUTE of ty		(* PERMUTE {INT, BOOL, FLOAT} *)
      | DPERMUTE of ty		(* DPERMUTE {INT, BOOL, FLOAT} *)
      | FPERMUTE of ty		(* FPERMUTE {INT, BOOL, FLOAT} *)
      | BPERMUTE of ty		(* BPERMUTE {INT, BOOL, FLOAT} *)
      | BFPERMUTE of ty		(* BFPERMUTE {INT, BOOL, FLOAT} *)
      | DFPERMUTE of ty		(* DFPERMUTE {INT, BOOL, FLOAT} *)
      | EXTRACT of ty		(* EXTRACT {INT, BOOL, FLOAT} *)
      | REPLACE of ty		(* REPLACE {INT, BOOL, FLOAT} *)
(* PACK returns two results, so we handle it as a special case
      | PACK of ty		(* PACK {INT, BOOL, FLOAT} *)
*)
      | RANK_UP of ty		(* RANK_UP {INT, FLOAT} *)
      | RANK_DOWN of ty		(* RANK_DOWN {INT, FLOAT} *)
      | DIST of ty		(* DIST {INT, BOOL, FLOAT} *)
      | INDEX		        (* INDEX *)
      | LENGTH of ty		(* LENGTH {INT, BOOL, FLOAT} *)
    (* Segment descriptor instructions *)
      | MAKE_SEGDES		(* MAKE_SEGDES *)
      | LENGTHS		        (* LENGTHS *)
      | CONST of ty * value list (* CONST {INT, BOOL, FLOAT, CHAR, SEGDES} value *)
    (* Flat vector instructions *)
      | ADD_SCAN_FLAT of ty	(* +_SCAN {INT, FLOAT} *)
      | MUL_SCAN_FLAT of ty	(* *_SCAN {INT, FLOAT} *)
      | MAX_SCAN_FLAT of ty	(* MAX_SCAN {INT, FLOAT} *)
      | MIN_SCAN_FLAT of ty	(* MIN_SCAN {INT, FLOAT} *)
      | AND_SCAN_FLAT of ty	(* AND_SCAN {INT, BOOL} *)
      | OR_SCAN_FLAT of ty	(* OR_SCAN {INT, BOOL} *)
      | XOR_SCAN_FLAT of ty	(* XOR_SCAN {INT, BOOL} *)
      | ADD_REDUCE_FLAT of ty	(* +_REDUCE {INT, FLOAT} *)
      | MUL_REDUCE_FLAT of ty	(* *_REDUCE {INT, FLOAT} *)
      | MAX_REDUCE_FLAT of ty	(* MAX_REDUCE {INT, FLOAT} *)
      | MIN_REDUCE_FLAT of ty	(* MIN_REDUCE {INT, FLOAT} *)
      | AND_REDUCE_FLAT of ty	(* AND_REDUCE {INT, BOOL} *)
      | OR_REDUCE_FLAT of ty	(* OR_REDUCE {INT, BOOL} *)
      | XOR_REDUCE_FLAT of ty	(* XOR_REDUCE {INT, BOOL} *)

  (* effectful VCODE operations *)
    datatype io
      = READ of ty		(* READ {INT, BOOL, FLOAT} *)
      | WRITE of ty		(* WRITE {INT, BOOL, FLOAT} *)
      | FOPEN		        (* FOPEN *)
      | FCLOSE		        (* FCLOSE *)
      | FWRITE of ty		(* FWRITE {INT, BOOL, FLOAT} *)
      | FREAD of ty		(* FREAD {INT, BOOL, FLOAT} *)
      | FREAD_CHAR		(* FREAD_CHAR *)
      | START_TIMER             (* START_TIMER *)
      | STOP_TIMER              (* STOP_TIMER *)
      | SRAND                   (* SRAND INT *)

    datatype var = V of {
        id : word,
	ty : ty,
        binding : var_bind ref,
        useCnt : int ref,
	props : PropList.holder
      }

(*
    and var_bind = VB_PARAM of int | VB_EXP of (exp * int) | VB_UNKNOWN

    and exp
      = LET of var list * exp * exp
      | CALL of var list * label * exp list * exp
      | STMT of var list * io * exp list * exp
      | IF of exp * exp * exp
      | PURE of pure * exp list
      | RET of exp list
      | EXIT
      | VAR of var
*)

    and var_bind = VB_PARAM of int | VB_LET of exp | VB_UNKNOWN

    and stm
      = LET of var * exp * stm
      | LETPACK of (var * var * ty * exp list * stm)         (* PACK ty *)
      | CALL of var list * label * exp list * stm
      | STMT of var list * io * exp list * stm
      | IF of var list * exp * stm * stm * stm
      | RET of exp list
      | EXIT

    and exp
      = PURE of pure * exp list
      | VAR of var

    datatype func = FUNC of label * var list * ty list * stm

    type program = func list

    local
      val cnt = ref 0w0
    in
    fun newVar (ty, binding) = let
          val id = !cnt
          in
            cnt := id + 0w1;
            V{
		id = id, ty = ty,
		binding = ref binding,
		useCnt = ref 0,
		props = PropList.newHolder()
	      }
          end
    fun reset () = (cnt := 0w0)
    end

    fun newProp f = PropList.newProp (fn (V{props, ...}) => props, f)
    fun newFlag () = PropList.newFlag (fn (V{props, ...}) => props)

    local
      val w2s = Word.fmt StringCvt.DEC
    in
    fun varToString long (V{id, ty, binding, useCnt, ...}) = let
          val prefix = (case !binding
                 of VB_PARAM i => "p" ^ (Int.toString i)
                  | VB_LET _ => "t" ^ StringCvt.padLeft #"0" 3 (w2s id)
                  | VB_UNKNOWN => "u" ^ StringCvt.padLeft #"0" 3 (w2s id)
                (* end case *))
          in
            if long
	      then concat[prefix, "#", Int.toString(!useCnt), ":", VCode.tyToString ty]
	      else prefix
          end
    end

    val tyToString = VCode.tyToString

    fun pureToString opcode = (case opcode
           of ADD ty => "+ " ^ tyToString ty
            | SUB ty => "- " ^ tyToString ty
            | MUL ty => "* " ^ tyToString ty
            | DIV ty => "/ " ^ tyToString ty
            | MOD => "%"
            | LT ty => "< " ^ tyToString ty
            | LTE ty => "<= " ^ tyToString ty
            | GT ty => "> " ^ tyToString ty
            | GTE ty => ">= " ^ tyToString ty
            | EQ ty => "= " ^ tyToString ty
            | NEQ ty => "!= " ^ tyToString ty
            | LSHIFT => "LSHIFT"
            | RSHIFT => "RSHIFT"
            | NOT ty => "NOT " ^ tyToString ty
            | AND ty => "AND " ^ tyToString ty
            | OR ty => "OR " ^ tyToString ty
            | XOR ty => "XOR " ^ tyToString ty
            | SELECT ty => "SELECT " ^ tyToString ty
            | RAND => "RAND"
            | FLOOR => "FLOOR"
            | CEIL => "CEIL"
            | TRUNC => "TRUNC"
            | ROUND => "ROUND"
            | I_TO_F => "I_TO_F"
            | I_TO_B => "I_TO_B"
            | B_TO_I => "B_TO_I"
            | LOG => "LOG"
            | SQRT => "SQRT"
            | EXP => "EXP"
            | SIN => "SIN"
            | COS => "COS"
            | TAN => "TAN"
            | ASIN => "ASIN"
            | ACOS => "ACOS"
            | ATAN => "ATAN"
            | SINH => "SINH"
            | COSH => "COSH"
            | TANH => "TANH"
          (* Vector instructions *)
            | ADD_SCAN ty => "ADD_SCAN " ^ tyToString ty
            | MUL_SCAN ty => "MUL_SCAN " ^ tyToString ty
            | MAX_SCAN ty => "MAX_SCAN " ^ tyToString ty
            | MIN_SCAN ty => "MIN_SCAN " ^ tyToString ty
            | AND_SCAN ty => "AND_SCAN " ^ tyToString ty
            | OR_SCAN ty => "OR_SCAN " ^ tyToString ty
            | XOR_SCAN ty => "XOR_SCAN " ^ tyToString ty
            | ADD_REDUCE ty => "+_REDUCE " ^ tyToString ty
            | MUL_REDUCE ty => "*_REDUCE " ^ tyToString ty
            | MAX_REDUCE ty => "MAX_REDUCE " ^ tyToString ty
            | MIN_REDUCE ty => "MIN_REDUCE " ^ tyToString ty
            | AND_REDUCE ty => "AND_REDUCE " ^ tyToString ty
            | OR_REDUCE ty => "OR_REDUCE " ^ tyToString ty
            | XOR_REDUCE ty => "XOR_REDUCE " ^ tyToString ty
            | PERMUTE ty => "PERMUTE " ^ tyToString ty
            | DPERMUTE ty => "DPERMUTE " ^ tyToString ty
            | FPERMUTE ty => "FPERMUTE " ^ tyToString ty
            | BPERMUTE ty => "BPERMUTE " ^ tyToString ty
            | BFPERMUTE ty => "BFPERMUTE " ^ tyToString ty
            | DFPERMUTE ty => "DFPERMUTE " ^ tyToString ty
            | EXTRACT ty => "EXTRACT " ^ tyToString ty
            | REPLACE ty => "REPLACE " ^ tyToString ty
            | RANK_UP ty => "RANK_UP " ^ tyToString ty
            | RANK_DOWN ty => "RANK_DOWN " ^ tyToString ty
            | DIST ty => "DIST " ^ tyToString ty
            | INDEX => "INDEX"
            | LENGTH ty => "LENGTH " ^ tyToString ty
          (* Segment descriptor instructions *)
            | MAKE_SEGDES => "MAKE_SEGDES"
            | LENGTHS => "LENGTHS"
            | CONST(ty, []) => concat["CONST ", tyToString ty, " ()"]
            | CONST(CHAR, [v]) => concat[
                  "CONST CHAR ",
                  String.translate (fn #"\n" => "\\n" | #"\t" => "\\t" | c => str c) v
                ]
            | CONST(ty, [v]) => concat["CONST ", tyToString ty, " ", v]
            | CONST(ty, vs) => concat[
                  "CONST ", tyToString ty, " (", String.concatWith " " vs, ")"
                ]
          (* end case *))

    fun ioToString opcode = (case opcode
           of READ ty => "READ " ^ tyToString ty
            | WRITE ty => "WRITE " ^ tyToString ty
            | FOPEN => "FOPEN"
            | FCLOSE => "FCLOSE"
            | FWRITE ty => "FWRITE " ^ tyToString ty
            | FREAD ty => "FREAD " ^ tyToString ty
            | FREAD_CHAR => "FREAD_CHAR"
            | START_TIMER => "START_TIMER"
            | STOP_TIMER => "STOP_TIMER"
            | SRAND => "SRAND INT"
          (* end case *))

    local
      val v2s = varToString false
    in
    fun expToString e = let
          fun f (VAR x, l) = v2s x :: l
            | f (PURE(opcode, []), l) = "(" :: pureToString opcode :: ")" :: l
            | f (PURE(opcode, args), l) =
                 "(" :: pureToString opcode :: " @"
                    :: (List.foldr (fn (e, l) => " " :: f(e, l)) (")" :: l) args)
          in
            String.concat (f (e, []))
          end
    end

    local
      structure Ord =
        struct
          type ord_key = var
          fun compare (V{id=a, ...}, V{id=b, ...}) = Word.compare(a, b)
        end
    in
    structure VMap = RedBlackMapFn (Ord)
    structure VSet = RedBlackSetFn (Ord)
    end (* local *)

    fun resultTypeOfPure pure = (case pure
	   of ADD ty => ty
	    | SUB ty => ty
	    | MUL ty => ty
	    | DIV ty => ty
	    | MOD => INT
	    | LT ty => BOOL
	    | LTE ty => BOOL
	    | GT ty => BOOL
	    | GTE ty => BOOL
	    | EQ ty => BOOL
	    | NEQ ty => BOOL
	    | LSHIFT => INT
	    | RSHIFT => INT
	    | NOT ty => ty
	    | AND ty => ty
	    | OR ty => ty
	    | XOR ty => ty
	    | SELECT ty => ty
	    | RAND => INT
	    | FLOOR => INT
	    | CEIL => INT
	    | TRUNC => INT
	    | ROUND => INT
	    | I_TO_F => FLOAT
	    | I_TO_B => BOOL
	    | B_TO_I => INT
	    | LOG => FLOAT
	    | SQRT => FLOAT
	    | EXP => FLOAT
	    | SIN => FLOAT
	    | COS => FLOAT
	    | TAN => FLOAT
	    | ASIN => FLOAT
	    | ACOS => FLOAT
	    | ATAN => FLOAT
	    | SINH => FLOAT
	    | COSH => FLOAT
	    | TANH => FLOAT
	  (* Vector instructions *)
	    | ADD_SCAN ty => ty
	    | MUL_SCAN ty => ty
	    | MAX_SCAN ty => ty
	    | MIN_SCAN ty => ty
	    | AND_SCAN ty => ty
	    | OR_SCAN ty => ty
	    | XOR_SCAN ty => ty
	    | ADD_REDUCE ty => ty
	    | MUL_REDUCE ty => ty
	    | MAX_REDUCE ty => ty
	    | MIN_REDUCE ty => ty
	    | AND_REDUCE ty => ty
	    | OR_REDUCE ty => ty
	    | XOR_REDUCE ty => ty
	    | PERMUTE ty => ty
	    | DPERMUTE ty => ty
	    | FPERMUTE ty => ty
	    | BPERMUTE ty => ty
	    | BFPERMUTE ty => ty
	    | DFPERMUTE ty => ty
	    | EXTRACT ty => ty
	    | REPLACE ty => ty
	    | RANK_UP ty => ty
	    | RANK_DOWN ty => ty
	    | DIST ty => ty
	    | INDEX => INT
	    | LENGTH ty => INT
	    | MAKE_SEGDES => SEGDES
	    | LENGTHS => INT
	    | CONST(ty, _) => ty
	  (* end case *))

  end
