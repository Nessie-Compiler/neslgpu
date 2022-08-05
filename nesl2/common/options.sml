(* options.sml
 *
 * COPYRIGHT (c) 2011 The Diderot Project (http://diderot-language.cs.uchicago.edu)
 * All rights reserved.
 *
 * Support for processing runtime options.
 *)

structure Options : sig

    exception Usage of string

    val parseCmdLine : string list -> {
	    help : bool,			(* "-help" specified? *)
	    log : bool,				(* logging enabled? *)
	    double : bool,
	    debug : bool,
            output : string option,		(* "-o" specified file *)
	    baseName : string,			(* basename of last source file *)
	    files : string list
	  }

    val usage : string -> string

  end = struct

    structure G = GetOpt

    exception Usage of string

  (* some option flags *)
    val helpFlg = ref false
    val debugFlg = ref false
    val doubleFlg = ref false
    val outputOpt : string option ref = ref NONE
    val logFlg = ref false
    val statsFlg = Stats.reportStats

    fun setFlag (flg, value) = G.NoArg(fn () => (flg := value))

    fun mkFlagOption (tag, flag, desc) = let
	  val default = !flag
	  val tag = if default
		then "disable-"^tag
		else "enable-"^tag
	  in {
	    short = "", long = [tag],
	    desc = setFlag (flag, not default),
	    help = desc
	  } end

  (* create the target option descriptor. *)
    val optionList = [
	    { short = "h", long = ["help"],
	      desc = setFlag (helpFlg, true),
	      help = "print command-line options"
	    },
            { short = "o", long = ["output"],
              desc = G.ReqArg(fn s => outputOpt := SOME s, "file"),
              help = "specify the executable file name"
            },
	    { short = "g", long = ["debug"],
	      desc = setFlag (debugFlg, true),
	      help = "enable debugging information in executable"
	    },
            { short = "", long = ["double"],
              desc = setFlag (doubleFlg, true),
              help = "use double-precision floats for reals"
            },
	    { short = "", long = ["log"],
	      desc = setFlag (logFlg, true),
	      help = "generate compiler debugging log"
	    },
	    { short = "", long = ["stats"],
	      desc = setFlag (statsFlg, true),
	      help = "report optimization statistics"
	    }
	  ]

    fun parseCmdLine args = let
	  val (opts, files) = G.getOpt {
		  argOrder = G.RequireOrder,
		  options = optionList,
		  errFn = fn s => raise Usage s
		} args
	  val baseName = OS.Path.base(List.last files)
		handle Empty => raise Usage "missing source files"
	  in {
	    help = !helpFlg,
	    log = !logFlg,
	    double = !doubleFlg,
	    debug = !debugFlg,
            output = !outputOpt,
	    baseName = baseName,
	    files = files
	  } end

    fun usage cmd = G.usageInfo {
	    header = concat["usage: ", cmd, " [options] a.nesl ...\n  Options:"],
	    options = optionList
	  }

  end
