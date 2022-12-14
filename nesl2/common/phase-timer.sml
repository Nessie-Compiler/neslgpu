(* phase-timer.sml
 *
 * COPYRIGHT (c) 2011 The Diderot Project (http://diderot-language.cs.uchicago.edu)
 * All rights reserved.
 *)

structure PhaseTimer : sig

    type timer

    val newTimer : string -> timer

    val newPhase : timer * string -> timer

    val start : timer -> unit
    val stop : timer -> unit
    val withTimer : timer -> ('a -> 'b) -> 'a -> 'b

    val report : TextIO.outstream * timer -> unit

  end = struct

    datatype timer = T of {
	parent : timer option,
	label : string,
	start : Time.time option ref,	(* SOME t when on, otherwise NONE *)
	tot : Time.time ref,
	childTot : Time.time ref,
	children : timer list ref
      }

    fun newTimer l = T{
	  parent = NONE,
	  label = l,
	  start = ref NONE,
	  tot = ref Time.zeroTime,
	  childTot = ref Time.zeroTime,
	  children = ref []
	}

    fun newPhase (timer as T{children, ...}, l) = let
	  val newT = T{
		  parent = SOME timer,
		  label = l,
		  start = ref NONE,
		  tot = ref Time.zeroTime,
		  childTot = ref Time.zeroTime,
		  children = ref []
		}
	  in
	    children := newT :: !children;
	    newT
	  end

    fun start (T{label, start, ...}) = (case !start
	   of NONE => start := SOME(Time.now())
	    | SOME _ => ()
	  (* end case *))

    fun stop (T{label, parent, start, tot, ...}) = (case !start
	   of SOME t0 => let
		val t = Time.-(Time.now(), t0)
		in
		  start := NONE;
		  tot := Time.+(!tot, t);
		  case parent
		   of SOME(T{childTot, ...}) => childTot := Time.+(!childTot, t)
		    | _ => ()
		  (* end case *)
		end
	    | NONE => ()
	  (* end case *))

    fun withTimer timer f x = let
	  val () = start timer
	  val y = (f x) handle ex => (stop timer; raise ex)
	  in
	    stop timer;
	    y
	  end

    fun report (outS, timer) = let
	  fun pr s = TextIO.output(outS, s)
	(* create a string by repeating a character n times *)
	  fun repeat (c, n) = CharVector.tabulate(n, fn _ => c)
	(* figure out the length of the longest label in the tree and the depth of the tree *)
	  val (maxLabelLen, depth) = let
		fun walk (T{label, children, ...}, maxLen, depth) = let
		      fun doChild (timer, (maxLen, depth)) = let
			    val (l, d) = walk (timer, maxLen, depth)
			    in
			      (Int.max(maxLen, l), Int.max(depth, d))
			    end
		      in
			List.foldl doChild (Int.max(size label, maxLen), depth+1) (!children)
		      end
		in
		  walk (timer, 0, 0)
		end
	  val labelWid = maxLabelLen + 2*depth + 4
	(* display a report line *)
	  fun display (indent, T{label, tot, childTot, children, ...}) = let
		fun prTime t = pr(StringCvt.padLeft #" " 7 (Time.fmt 3 t))
		in
		  pr(repeat (#" ", indent));
		  pr(StringCvt.padRight #"." (labelWid+4-indent) (label^" "));
		  pr " "; prTime (Time.-(!tot, !childTot));
		  pr "   "; prTime (!tot); pr "\n";
		  List.app (fn t => display(indent+2, t)) (List.rev (!children))
		end
	  fun center (s, wid) = let
		val padding = wid - String.size s
		val lPad = padding div 2
		val rPad = padding - lPad
		in
		  if padding < 0 then s
		    else concat[repeat(#" ", lPad), s, repeat(#" ", rPad)]
		end
	  in
	    pr (center ("Phase", labelWid + 2));
	    pr "  "; pr(center ("Exclusive", 9));
	    pr "  "; pr(center ("Total", 9));
	    pr "\n";
	    display (2, timer)
	  end

  end
