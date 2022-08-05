(* log.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Log : sig

    val enabled : unit -> bool

    val init : string -> unit

    val logFile : unit -> TextIO.outstream

    val msg : string -> unit

    val reportTiming : PhaseTimer.timer -> unit

    val finish : unit -> unit

  end = struct

    val enabledFlg = ref false
    val logStrm : TextIO.outstream option ref = ref NONE

    fun enabled () = !enabledFlg

    fun init file = (case !logStrm
	   of NONE => let
		val outS = TextIO.openOut file
		in
		  enabledFlg := true;
		(* turn off buffering *)
		  TextIO.StreamIO.setBufferMode (TextIO.getOutstream outS, IO.NO_BUF);
		  logStrm := SOME outS
		end
	    | SOME strm => raise Fail "multiple initialization of log file"
	  (* end case *))

    fun finish () = (case !logStrm
	   of SOME strm => (
		enabledFlg := false;
		TextIO.closeOut strm;
		logStrm := NONE)
	    | NONE => ()
	  (* end case *))

    fun logFile () = (case !logStrm
	   of NONE => (init "/dev/null"; enabledFlg := false; logFile())
	    | SOME outS => outS
	  (* end case *))

    fun msg s = if !enabledFlg then TextIO.output(logFile(), s) else ();

    fun reportTiming timer = PhaseTimer.report (logFile(), timer)

  end

