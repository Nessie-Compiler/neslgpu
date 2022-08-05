(* pure.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Pure =
  struct

  (* operator types. *)
    datatype ty = datatype TypeBase.ty

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
      | CEIL                    (* CEIL *)
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
      | I_TO_C			(* I_TO_C *)
      | C_TO_I			(* C_TO_I *)
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
    (* additional instructions *)
      | PROJ of int		(* PROJ i -- project i'th component of tuple (first = 1, ...) *)
      | SCALAR_TO_SEQ of ty	(* SCALAR_TO_SEQ {INT, BOOL, FLOAT, CHAR} -- create singleton base sequence *)
      | SEQ_TO_SCALAR of ty	(* SEQ_TO_SCALAR {INT, BOOL, FLOAT, CHAR} -- extract value from singleton base sequence *)

  (* return true if the operation is parallel *)
    fun isParallel opcode = (case opcode
           of ADD _ => false
            | SUB _ => false
            | MUL _ => false
            | DIV _ => false
            | MOD => false
            | LT _ => false
            | LTE _ => false
            | GT _ => false
            | GTE _ => false
            | EQ _ => false
            | NEQ _ => false
            | LSHIFT => false
            | RSHIFT => false
            | NOT _ => false
            | AND _ => false
            | OR _ => false
            | XOR _ => false
            | SELECT _ => false
            | RAND => false
            | FLOOR => false
            | CEIL => false
            | TRUNC => false
            | ROUND => false
            | I_TO_F => false
            | I_TO_B => false
            | B_TO_I => false
            | LOG => false
            | SQRT => false
            | EXP => false
            | SIN => false
            | COS => false
            | TAN => false
            | ASIN => false
            | ACOS => false
            | ATAN => false
            | SINH => false
            | COSH => false
            | TANH => false
	    | I_TO_C => false
	    | C_TO_I => false
          (* Vector instructions *)
            | ADD_SCAN _ => true
            | MUL_SCAN _ => true
            | MAX_SCAN _ => true
            | MIN_SCAN _ => true
            | AND_SCAN _ => true
            | OR_SCAN _ => true
            | XOR_SCAN _ => true
            | ADD_REDUCE _ => true
            | MUL_REDUCE _ => true
            | MAX_REDUCE _ => true
            | MIN_REDUCE _ => true
            | AND_REDUCE _ => true
            | OR_REDUCE _ => true
            | XOR_REDUCE _ => true
            | PERMUTE _ => true
            | DPERMUTE _ => true
            | FPERMUTE _ => true
            | BPERMUTE _ => true
            | BFPERMUTE _ => true
            | DFPERMUTE _ => true
            | EXTRACT _ => true
            | REPLACE _ => true
	    | PACK _ => true
            | RANK_UP _ => true
            | RANK_DOWN _ => true
            | DIST _ => true
            | INDEX => true
            | LENGTH _ => false (* ?? *)
          (* Segment descriptor instructions *)
            | MAKE_SEGDES => false (* ?? *)
            | LENGTHS => false (* ?? *)
	  (* additional instructions *)
	    | PROJ _ => false
	    | SCALAR_TO_SEQ _ => false
	    | SEQ_TO_SCALAR _ => false
	  (* end case *))

  (* returns the number of arguments expected by the operator *)
    fun arity opcode = (case opcode
           of ADD _ => 2
            | SUB _ => 2
            | MUL _ => 2
            | DIV _ => 2
            | MOD => 2
            | LT _ => 2
            | LTE _ => 2
            | GT _ => 2
            | GTE _ => 2
            | EQ _ => 2
            | NEQ _ => 2
            | LSHIFT => 2
            | RSHIFT => 2
            | NOT _ => 1
            | AND _ => 2
            | OR _ => 2
            | XOR _ => 2
            | SELECT _ => 3
            | RAND => 1
            | FLOOR => 1
            | CEIL => 1
            | TRUNC => 1
            | ROUND => 1
            | I_TO_F => 1
            | I_TO_B => 1
            | B_TO_I => 1
            | LOG => 1
            | SQRT => 1
            | EXP => 1
            | SIN => 1
            | COS => 1
            | TAN => 1
            | ASIN => 1
            | ACOS => 1
            | ATAN => 1
            | SINH => 1
            | COSH => 1
            | TANH => 1
	    | I_TO_C => 1
	    | C_TO_I => 1
          (* Vector instructions *)
            | ADD_SCAN _ => 1
            | MUL_SCAN _ => 1
            | MAX_SCAN _ => 1
            | MIN_SCAN _ => 1
            | AND_SCAN _ => 1
            | OR_SCAN _ => 1
            | XOR_SCAN _ => 1
            | ADD_REDUCE _ => 1
            | MUL_REDUCE _ => 1
            | MAX_REDUCE _ => 1
            | MIN_REDUCE _ => 1
            | AND_REDUCE _ => 1
            | OR_REDUCE _ => 1
            | XOR_REDUCE _ => 1
            | PERMUTE _ => 2
            | DPERMUTE _ => 3
            | FPERMUTE _ => 3
            | BPERMUTE _ => 2
            | BFPERMUTE _ => 3
            | DFPERMUTE _ => 4
            | EXTRACT _ => 2
            | REPLACE _ => 3
	    | PACK _ => 2
            | RANK_UP _ => 1
            | RANK_DOWN _ => 1
            | DIST _ => 2
            | INDEX => 3
            | LENGTH _ => 1
          (* Segment descriptor instructions *)
            | MAKE_SEGDES => 1
            | LENGTHS => 1
	  (* additional instructions *)
	    | PROJ _ => 1
	    | SCALAR_TO_SEQ _ => 1
	    | SEQ_TO_SCALAR _ => 1
          (* end case *))

    fun toString opcode = (case opcode
           of ADD ty => "+ " ^ TypeBase.baseToString ty
            | SUB ty => "- " ^ TypeBase.baseToString ty
            | MUL ty => "* " ^ TypeBase.baseToString ty
            | DIV ty => "/ " ^ TypeBase.baseToString ty
            | MOD => "MOD"
            | LT ty => "< " ^ TypeBase.baseToString ty
            | LTE ty => "<= " ^ TypeBase.baseToString ty
            | GT ty => "> " ^ TypeBase.baseToString ty
            | GTE ty => ">= " ^ TypeBase.baseToString ty
            | EQ ty => "= " ^ TypeBase.baseToString ty
            | NEQ ty => "!= " ^ TypeBase.baseToString ty
            | LSHIFT => "LSHIFT"
            | RSHIFT => "RSHIFT"
            | NOT ty => "NOT " ^ TypeBase.baseToString ty
            | AND ty => "AND " ^ TypeBase.baseToString ty
            | OR ty => "OR " ^ TypeBase.baseToString ty
            | XOR ty => "XOR " ^ TypeBase.baseToString ty
            | SELECT ty => "SELECT " ^ TypeBase.baseToString ty
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
	    | I_TO_C => "I_TO_C"
	    | C_TO_I => "C_TO_I"
         (* Vector instructions *)
            | ADD_SCAN ty => "ADD_SCAN " ^ TypeBase.baseToString ty
            | MUL_SCAN ty => "MUL_SCAN " ^ TypeBase.baseToString ty
            | MAX_SCAN ty => "MAX_SCAN " ^ TypeBase.baseToString ty
            | MIN_SCAN ty => "MIN_SCAN " ^ TypeBase.baseToString ty
            | AND_SCAN ty => "AND_SCAN " ^ TypeBase.baseToString ty
            | OR_SCAN ty => "OR_SCAN " ^ TypeBase.baseToString ty
            | XOR_SCAN ty => "XOR_SCAN " ^ TypeBase.baseToString ty
            | ADD_REDUCE ty => "+_REDUCE " ^ TypeBase.baseToString ty
            | MUL_REDUCE ty => "*_REDUCE " ^ TypeBase.baseToString ty
            | MAX_REDUCE ty => "MAX_REDUCE " ^ TypeBase.baseToString ty
            | MIN_REDUCE ty => "MIN_REDUCE " ^ TypeBase.baseToString ty
            | AND_REDUCE ty => "AND_REDUCE " ^ TypeBase.baseToString ty
            | OR_REDUCE ty => "OR_REDUCE " ^ TypeBase.baseToString ty
            | XOR_REDUCE ty => "XOR_REDUCE " ^ TypeBase.baseToString ty
            | PERMUTE ty => "PERMUTE " ^ TypeBase.baseToString ty
            | DPERMUTE ty => "DPERMUTE " ^ TypeBase.baseToString ty
            | FPERMUTE ty => "FPERMUTE " ^ TypeBase.baseToString ty
            | BPERMUTE ty => "BPERMUTE " ^ TypeBase.baseToString ty
            | BFPERMUTE ty => "BFPERMUTE " ^ TypeBase.baseToString ty
            | DFPERMUTE ty => "DFPERMUTE " ^ TypeBase.baseToString ty
            | EXTRACT ty => "EXTRACT " ^ TypeBase.baseToString ty
            | REPLACE ty => "REPLACE " ^ TypeBase.baseToString ty
	    | PACK ty => "PACK " ^ TypeBase.baseToString ty
            | RANK_UP ty => "RANK_UP " ^ TypeBase.baseToString ty
            | RANK_DOWN ty => "RANK_DOWN " ^ TypeBase.baseToString ty
            | DIST ty => "DIST " ^ TypeBase.baseToString ty
            | INDEX => "INDEX"
            | LENGTH ty => "LENGTH " ^ TypeBase.baseToString ty
          (* Segment descriptor instructions *)
            | MAKE_SEGDES => "MAKE_SEGDES"
            | LENGTHS => "LENGTHS"
	  (* additional instructions *)
	    | PROJ i => "PROJ " ^ Int.toString i
	    | SCALAR_TO_SEQ ty => "SCALAR_TO_SEQ " ^ TypeBase.baseToString ty
	    | SEQ_TO_SCALAR ty => "SEQ_TO_SCALAR " ^ TypeBase.baseToString ty
          (* end case *))

  end
