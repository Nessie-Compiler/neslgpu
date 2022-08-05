(* scalar-op.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure ScalarOp =
  struct

    datatype t
      = ADD of TypeBase.ty              (* + {INT, FLOAT} *)
      | SUB of TypeBase.ty              (* - {INT, FLOAT} *)
      | MUL of TypeBase.ty              (* * {INT, FLOAT} *)
      | DIV of TypeBase.ty              (* / {INT, FLOAT} *)
      | MOD				(* % *)
      | LT of TypeBase.ty               (* < {INT, FLOAT} *)
      | LTE of TypeBase.ty              (* <= {INT, FLOAT} *)
      | GT of TypeBase.ty               (* > {INT, FLOAT} *)
      | GTE of TypeBase.ty              (* >= {INT, FLOAT} *)
      | EQ of TypeBase.ty               (* = {INT, FLOAT} *)
      | NEQ of TypeBase.ty              (* != {INT, FLOAT} *)
      | LSHIFT		        	(* LSHIFT *)
      | RSHIFT		        	(* RSHIFT *)
      | NOT of TypeBase.ty              (* NOT {BOOL, INT} *)
      | AND of TypeBase.ty              (* AND {BOOL, INT} *)
      | OR of TypeBase.ty               (* OR {BOOL, INT} *)
      | XOR of TypeBase.ty		(* XOR {BOOL, INT} *)
      | SELECT of TypeBase.ty		(* SELECT {INT, BOOL, FLOAT} *)
      | RAND                    	(* RAND *)
      | FLOOR                   	(* FLOOR *)
      | CEIL                    	(* CEIL *)
      | TRUNC                   	(* TRUNC *)
      | ROUND                   	(* ROUND *)
      | I_TO_F                  	(* I_TO_F *)
      | I_TO_B                  	(* I_TO_B *)
      | B_TO_I                  	(* B_TO_I *)
      | LOG		        	(* LOG *)
      | SQRT		        	(* SQRT *)
      | EXP		        	(* EXP *)
      | SIN		        	(* SIN *)
      | COS		        	(* COS *)
      | TAN		        	(* TAN *)
      | ASIN		        	(* ASIN *)
      | ACOS		        	(* ACOS *)
      | ATAN				(* ATAN *)
      | SINH		        	(* SINH *)
      | COSH		        	(* COSH *)
      | TANH		        	(* TANH *)
      | I_TO_C				(* I_TO_C *)
      | C_TO_I				(* C_TO_I *)

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
	  (* end case *))

    fun kernelName rator = (case rator
           of ADD _ => "ADD"
            | SUB _ => "SUB"
            | MUL _ => "MUL"
            | DIV _ => "DIV"
            | MOD => "MOD"
            | LT _ => "LT"
            | LTE _ => "LTE"
            | GT _ => "GT"
            | GTE _ => "GTE"
            | EQ _ => "EQ"
            | NEQ _ => "NEQ"
            | LSHIFT => "LSHIFT"
            | RSHIFT => "RSHIFT"
            | NOT _ => "NOT"
            | AND _ => "AND"
            | OR _ => "OR"
            | XOR _ => "XOR"
            | SELECT _ => "SELECT"
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
	  (* end case *))

    fun hashOp opcode = Atom.hash(Atom.atom(toString opcode))
    fun sameOp (op1, op2) = Atom.same(Atom.atom(toString op1), Atom.atom(toString op2))

    structure Tbl = HashTableFn (
      struct
        type hash_key = t
	val hashVal = hashOp
	val sameKey = sameOp
      end)

    fun arity opcode = 
	(case opcode
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
	(* end case *))
  end
