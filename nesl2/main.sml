(* main.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Main : sig

    val main : (string * string list) -> OS.Process.status

    val doit : string -> OS.Process.status

  (* use to compile multiple files.  For example:
   *
   *    doit' ["../nesl/neslseqsrc/other-ops.nesl", "tests/radix.nesl"]
   *)
    val doit' : string list -> OS.Process.status

    val dumpBasis : unit -> unit

  end = struct

  (* exception tracing magic *)
(*
    val _ = (
        SMLofNJ.Internals.TDP.mode := true;
        Coverage.install ();
        BackTrace.install ())
*)

    fun err s = TextIO.output (TextIO.stdErr, s)
    fun err1 c =  TextIO.output1 (TextIO.stdErr, c)
    fun errnl s = (err s; err1 #"\n")

    exception ERROR

    fun quitWithError srcFile = raise Fail("Error in compiling " ^ srcFile)

  (* check for errors and report them if there are any *)
    fun checkForErrors errStrm = (
	  Error.report (TextIO.stdErr, errStrm);
	  if Error.anyErrors errStrm
	    then quitWithError (Error.sourceFile errStrm)
	    else ())

    fun usage cmd = TextIO.output(TextIO.stdErr, Options.usage cmd)

  (* the NESL Basis Library *)
    val libFiles = [
	    "lib/constants.nesl",
	    "lib/prim-vector-ops.nesl",
	    "lib/scalar-ops.nesl",
	    "lib/basic-vector-ops.nesl",
	    "lib/nest-ops.nesl",
	    "lib/vector-ops.nesl",
	    "lib/io-ops.nesl"
	  ]

  (* parse and typecheck the given files *)
    fun parseAndCheck (errStrm, env, files) = let
	(***** PARSING *****)
	  val parseTree = PhaseTimer.withTimer Timers.timeParser (fn () => let
		fun parseFile filename = let
		      val _ = if OS.FileSys.access(filename, [OS.FileSys.A_READ])
			    then ()
			    else (
			      err (concat[
				  "source file \"", filename,
				  "\" does not exist or is not readable\n"
				]);
			      raise ERROR)
		      val inS = TextIO.openIn filename
val _ = print(concat["parsing ", filename, "\n"]);
		      val pt = Parser.parseFile (errStrm, inS)
		      in
			TextIO.closeIn inS;
			checkForErrors errStrm;
			valOf pt
		      end
		val parseTrees = List.map parseFile files
		in
		  ParseTree.Program parseTrees
		end) ()
(*DEBUG
	  val _ = PPParseTree.output (Log.logFile(), parseTree)
*)
	(***** TYPECHECKING *****)
	  val _ = PhaseTimer.start Timers.timeTypechecker
	  val (ast, env) = Typechecker.check (errStrm, env, parseTree)
	  val _ = PhaseTimer.stop Timers.timeTypechecker
	  val _ = checkForErrors errStrm
	  in
print "front-end done\n";
	    (ast, env)
	  end

  (* flags to control printing of the various intermediate representations *)
    val prASTFlg = ref false
    val prMonoFlg = ref false
    val prNormalFlg = ref false
    val prFlanFlg = ref false
    val prFuseFlg = ref true

    fun condPr (prFn, flg) prog = if !flg
	  then prFn (Log.logFile(), prog)
	  else ()
    fun condPr' (prFn, flg) (prog, msg) = if !flg
	  then prFn (Log.logFile(), prog, msg)
	  else ()

    val prAST = condPr (PPAST.output, prASTFlg)
    val prMono = condPr (PPMono.output, prMonoFlg)
    val prNormal = condPr (PPNormal.output, prNormalFlg)
    val prFlan = condPr' (PPFlan.output, prFlanFlg)
    val prFuse = condPr' (PPFuse.output, prFuseFlg)

  (* compiler front end (parsing, typechecking, and simplification *)
    fun frontEnd files = let
	  val errStrm = Error.mkErrStream (List.hd files)
	(***** Load Basis Library *****)
	  val (AST.Program basisAST, basisEnv) = parseAndCheck (errStrm, BasisEnv.env0, libFiles)
	  val basis = NeslBasis.mkBasis basisEnv
	(***** Parse and typecheck *****)
	  val (AST.Program ast, _) = parseAndCheck (errStrm, basisEnv, files)
	  val prog = AST.Program(basisAST @ ast)
	  val _ = prAST prog (* DEBUG *)
	  in
	    (basis, prog)
	  end

    fun middleEnd (basis, ast) = let
	(***** MONOMORPHIZATION *****)
	  val _ = PhaseTimer.start Timers.timeMono
	  val (monoEnv, mono) = Monomorphize.transform (basis, ast)
	  val _ = PhaseTimer.stop Timers.timeMono
	  val _ = prMono mono (* DEBUG *)
	  val _ = if CheckMono.check (monoEnv, mono) then raise ERROR else ()
	(***** NORMALIZATION *****)
	  val _ = PhaseTimer.start Timers.timeNormal
	  val (normEnv, normal) = Normalize.transform mono
	  val _ = PhaseTimer.stop Timers.timeNormal
	  val _ = prNormal normal (* DEBUG *)
	(***** FLATTENING *****)
	  val env = (monoEnv, normEnv) (* the environment for instantiating library functions *)
	  val _ = PhaseTimer.start Timers.timeFlatten
(*	  val _ = print "transforming\n"   *)
	  val (flan, flanEnv) = ToFlan.transform (monoEnv, normEnv, basis) normal
(*	  val _ = print "flattening\n"   *)
	  val (flan, flanEnv) = Flatten.flatten (flan, flanEnv, env, basis)
	  val _ = PhaseTimer.stop Timers.timeFlatten
(*	  val _ = print "done flattening\n"    *)
	  val _ = PhaseTimer.start Timers.timeOpt
	  val _ = Census.census flan
	  val _ = prFlan (flan, "after flattening ")
	  val _ = if CheckFlan.check flan then raise ERROR else ()
	  val flan = Contract.contract (flan, flanEnv)
	  val _ = prFlan (flan, "after contract(1) ")
	  val _ = if CheckFlan.check flan then raise ERROR else ()
	  val flan = Inline.transform (flanEnv, flan)
	  val _ = prFlan (flan, "after inlining(1) ")
	  val _ = if CheckFlan.check flan then raise ERROR else ()
	  val flan = Contract.contract (flan, flanEnv)
	  val _ = prFlan (flan, "after contract(2) ")
	  val _ = if CheckFlan.check flan then raise ERROR else ()
	  val flan = ArityRaise.transform (flanEnv, flan)
	  val _ = prFlan (flan, "after arity-raise ")
	  val _ = if CheckFlan.check flan then raise ERROR else ()
	  val flan = Contract.contract (flan, flanEnv)
	  val _ = prFlan (flan, "after contract(3) ")
	  val _ = if CheckFlan.check flan then raise ERROR else ()
	  val flan = Inline.transform (flanEnv, flan)
	  val _ = prFlan (flan, "after inlining(2) ")
	  val _ = if CheckFlan.check flan then raise ERROR else ()
	  val flan = Contract.contract (flan, flanEnv)
	  val _ = prFlan (flan, "after contract(4) ")
	  val _ = if CheckFlan.check flan then raise ERROR else ()
	  val _ = PhaseTimer.stop Timers.timeOpt
 	  val _ = AnalyzeShapes.analyze(flan, flanEnv)
 	  val _ = prFlan (flan, "after shape analysis ")
	(***** FUSION *****)
	  val _ = PhaseTimer.start Timers.timeFuse
	  val fuse = ToFuse.transform flan
	  val _ = prFuse (fuse, "after conversion to FuseAST")
	  val _ = if CheckFuse.check fuse then raise ERROR else ()
	  val fuse = FuseContract.contract fuse
	  val _ = prFuse (fuse, "after contract(1)")
	  val _ = if CheckFuse.check fuse then raise ERROR else ()
	  val fuse = Fusion.transform fuse
	  val _ = prFuse (fuse, "after fusion")
	  val _ = if CheckFuse.check fuse then raise ERROR else ()
	  val fuse = FuseContract.contract fuse
	  val _ = prFuse (fuse, "after contract(2)")
	  val _ = if CheckFuse.check fuse then raise ERROR else ()
	  val _ = PhaseTimer.stop Timers.timeFuse
	  in
	    fuse
	  end

    fun codegen (baseName, fuseAST) = let
	(***** Code generation *****)
	  val cuLam = Convert.transform fuseAST
	  val cuda = GenCPU.genProgram (baseName ^ ".nesl", cuLam)
	  val outFile = OS.Path.joinBaseExt{base=baseName, ext=SOME "cu"}
	  val outS = TextIO.openOut outFile
	  val strm = PrintAsCUDA.new outS
	  in
	    List.app (fn dcl => PrintAsCUDA.output (strm, dcl)) cuda;
	    PrintAsCUDA.close strm;
	    TextIO.closeOut outS
	  end

    fun compile (files, baseName, optOutput) = let
	  val ast = PhaseTimer.withTimer Timers.timeFront frontEnd files
	  val fuse = PhaseTimer.withTimer Timers.timeMiddle middleEnd ast
	  in
	    PhaseTimer.withTimer Timers.timeBack codegen (baseName, fuse)
	  end

    fun doFiles {log, debug, double, output, baseName, files} = (
	    if log then Log.init(baseName ^ ".log") else ();
	    PhaseTimer.withTimer Timers.timeCompiler
	      compile (files, baseName, output);
	    Stats.report ();
	    Log.reportTiming Timers.timeCompiler;
	    Log.finish ();
	    OS.Process.success
	  ) handle exn => (
	    err (concat [
		"uncaught exception ", General.exnName exn,
		" [", General.exnMessage exn, "]\n"
	      ]);
	    List.app (fn s => err (concat ["  raised at ", s, "\n"]))
	      (SMLofNJ.exnHistory exn);
	    Log.finish ();
	    OS.Process.failure)

    fun main (name: string, args: string list) = let
	  val {help, log, debug, double, output, baseName, files} =
		(Options.parseCmdLine args)
		  handle Options.Usage msg => (
		    err(concat[msg, "\n"]);
		    usage name;
		    OS.Process.exit OS.Process.failure)
	  in
	    if help
	      then (
		usage name;
		OS.Process.success)
	      else doFiles {
		  log=log, debug=debug, double=double, output=output,
		  baseName=baseName,  files=files
		}
	  end
	    handle ERROR => OS.Process.failure

  (* compile a single file *)
    fun doit file = doFiles {
            log=true, debug=false, double=false, output=NONE,
            baseName=OS.Path.base file, files = [file]
          }

    fun doit' files = doFiles {
            log = true, debug = false, double = false, output = NONE,
            baseName = OS.Path.base(List.last files),
	    files = files
          }

    fun dumpBasis () = Env.dumpVarEnv (BasisEnv.env0, "Basis environment")

  end
