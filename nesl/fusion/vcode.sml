(* vcode.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * VCode representation.
 *)

structure VCode =
  struct

  (* VCODE types.  Note that ANY is used for when we cannot determine the type
   * from the code; it is not part of the VCODE specification.
   *)
    datatype ty = INT | BOOL | FLOAT | CHAR | SEGDES | ANY

    type value = string         (* literal representation *)

    type label = Atom.atom

    datatype opcode
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
      | PACK of ty		(* PACK {INT, BOOL, FLOAT} *)
      | RANK_UP of ty		(* RANK_UP {INT, FLOAT} *)
      | RANK_DOWN of ty		(* RANK_DOWN {INT, FLOAT} *)
      | DIST of ty		(* DIST {INT, BOOL, FLOAT} *)
      | INDEX		        (* INDEX *)
      | LENGTH of ty		(* LENGTH {INT, BOOL, FLOAT} *)
    (* Segment descriptor instructions *)
      | MAKE_SEGDES		(* MAKE_SEGDES *)
      | LENGTHS		        (* LENGTHS *)
    (* Control instructions *)
      | COPY of int * int       (* COPY I J *)
      | POP of int * int        (* POP I J *)
      | CPOP of int * int       (* CPOP I J *)
      | PAIR                    (* PAIR *)
      | UNPAIR                  (* UNPAIR *)
      | CALL of label           (* CALL label *)
      | IF of block * block     (* IF ... ELSE ... ENDIF *)
      | CONST of ty * value list (* CONST {INT, BOOL, FLOAT, CHAR, SEGDES} value *)
    (* I/O instructions *)
      | READ of ty		(* READ {INT, BOOL, FLOAT} *)
      | WRITE of ty		(* WRITE {INT, BOOL, FLOAT} *)
      | FOPEN		        (* FOPEN *)
      | FCLOSE		        (* FCLOSE *)
      | FWRITE of ty		(* FWRITE {INT, BOOL, FLOAT} *)
      | FREAD of ty		(* FREAD {INT, BOOL, FLOAT} *)
      | FREAD_CHAR		(* FREAD_CHAR *)
    (* undocumented instuctions *)
      | EXIT		        (* EXIT *)
      | START_TIMER             (* START_TIMER *)
      | STOP_TIMER              (* STOP_TIMER *)
      | SRAND                   (* SRAND INT *)
    (* fused super operator *)
      | FUSED of int            (* FUSED id *)

    withtype block = opcode list

  (* FUNC label ... RET *)
    datatype func
      = FUNC of label * block  (* FUNC label *)

    type program = func list

    fun tyToString INT = "INT"
      | tyToString BOOL = "BOOL"
      | tyToString FLOAT = "FLOAT"
      | tyToString CHAR = "CHAR"
      | tyToString SEGDES = "SEGDES"
      | tyToString ANY = "ANY"

    fun toString opcode = (case opcode
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
            | ADD_SCAN ty => "+_SCAN " ^ tyToString ty
            | MUL_SCAN ty => "*_SCAN " ^ tyToString ty
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
            | PACK ty => "PACK " ^ tyToString ty
            | RANK_UP ty => "RANK_UP " ^ tyToString ty
            | RANK_DOWN ty => "RANK_DOWN " ^ tyToString ty
            | DIST ty => "DIST " ^ tyToString ty
            | INDEX => "INDEX"
            | LENGTH ty => "LENGTH " ^ tyToString ty
          (* Segment descriptor instructions *)
            | MAKE_SEGDES => "MAKE_SEGDES"
            | LENGTHS => "LENGTHS"
          (* Control instructions *)
            | COPY(i, j) => concat["COPY ", Int.toString i, " ", Int.toString j]
            | POP(i, j) => concat["POP ", Int.toString i, " ", Int.toString j]
            | CPOP(i, j) => concat["CPOP ", Int.toString i, " ", Int.toString j]
            | PAIR => "PAIR"
            | UNPAIR => "UNPAIR"
            | CALL lab => "CALL " ^ Atom.toString lab
            | IF _ => "IF ... THEN ... ELSE"
            | CONST(ty, []) => concat["CONST ", tyToString ty, " ()"]
            | CONST(ty, [v]) => concat["CONST ", tyToString ty, " ", v]
            | CONST(ty, vs) => concat[
                  "CONST ", tyToString ty, " (", String.concatWith " " vs, ")"
                ]
          (* I/O instructions *)
            | READ ty => "READ " ^ tyToString ty
            | WRITE ty => "WRITE " ^ tyToString ty
            | FOPEN => "FOPEN"
            | FCLOSE => "FCLOSE"
            | FWRITE ty => "FWRITE " ^ tyToString ty
            | FREAD ty => "FREAD " ^ tyToString ty
            | FREAD_CHAR => "FREAD_CHAR"
          (* undocumented instuctions *)
            | EXIT => "EXIT"
            | START_TIMER => "START_TIMER"
            | STOP_TIMER => "STOP_TIMER"
            | SRAND => "SRAND INT"
          (* fused super operator *)
            | FUSED id => "FUSED " ^ Int.toString id
          (* end case *))

  end
