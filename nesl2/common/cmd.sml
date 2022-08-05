(* cmd.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Cmd =
  struct

  (* operator types. *)
    datatype ty = datatype TypeBase.ty

  (* effectful VCODE operations *)
    datatype cmd
      = EXIT
      | READ of ty		(* READ {INT, BOOL, FLOAT} *)
      | WRITE of ty		(* WRITE {INT, BOOL, FLOAT} *)
      | FOPEN		        (* FOPEN *)
      | FCLOSE		        (* FCLOSE *)
      | FWRITE of ty		(* FWRITE {INT, BOOL, FLOAT} *)
      | FREAD of ty		(* FREAD {INT, BOOL, FLOAT} *)
      | FREAD_CHAR		(* FREAD_CHAR *)
      | START_TIMER             (* START_TIMER *)
      | STOP_TIMER              (* STOP_TIMER *)
      | SRAND                   (* SRAND INT *)

    fun toString opcode = (case opcode
           of EXIT => "EXIT"
	    | READ ty => "READ " ^ TypeBase.baseToString ty
            | WRITE ty => "WRITE " ^ TypeBase.baseToString ty
            | FOPEN => "FOPEN"
            | FCLOSE => "FCLOSE"
            | FWRITE ty => "FWRITE " ^ TypeBase.baseToString ty
            | FREAD ty => "FREAD " ^ TypeBase.baseToString ty
            | FREAD_CHAR => "FREAD_CHAR"
            | START_TIMER => "START_TIMER"
            | STOP_TIMER => "STOP_TIMER"
            | SRAND => "SRAND INT"
          (* end case *))

    fun arity opcode = (case opcode
           of EXIT => (1, 1)
	    | READ ty => (0, 1)
            | WRITE ty => (1, 2)
            | FOPEN => (2, 3)
            | FCLOSE => (1, 2)
            | FWRITE ty => (2, 2)
            | FREAD ty => (1, 3)
            | FREAD_CHAR => (3, 4)
            | START_TIMER => (1, 1)
            | STOP_TIMER => (1, 1)
            | SRAND => (1, 1)
	  (* end case *))

  end
