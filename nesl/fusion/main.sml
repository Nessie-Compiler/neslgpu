(* main.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 * Options:
 *      -inline n       -- specify inline threshold (default 2)
 *      -no-inline      -- turn off inlining
 *      -print-inline   -- print after inlining
 *      -print-convert  -- print after conversion
 *      -print-reduce   -- print after reduction
 *      -print-super    -- print after super-op conversion
 *)

structure Main =
  struct

  (* optimization controls *)
    val inlineThreshold = Inliner.inlineThreshold
    val enableInline = ref true
    val enableDeadVarElim = Reduce.deadVarElimFlg
    val enableFusion = Fuse.fuseFlg

  (* printing controls *)
    val printInline = ref false
    val printConvert = ref false
    val printReduce = ref false
    val printSizes = ref true
    val printFused = ref false
    val printSuper = ref false
    val printFinal = ref false
	  
    fun compile f = let
          val _ = (VExp.reset(); SuperOp.reset())
          val vcode = Parser.parseFile f
          val vcode = Inliner.inline vcode
          val _ = if !printInline
                then (
                  print "********** after inlining **********\n";
                  PrintVCode.prProg vcode)
                else ()
          val vtree = Convert.convert vcode
          val _ = if !printConvert
                then (
                  print "********** tree representation **********\n";
                  PrintVExp.prProg vtree)
                else ()
          val vtree = Reduce.transform vtree
          val _ = if !printReduce
                then (
                  print "********** after reduction **********\n";
                  PrintVExp.prProg vtree)
                else ()
	  val vtree = AnalyzeSizes.analyze vtree
          val _ = if !printSizes
                then (
		  print "********** after size analysis **********\n";
		  PrintVExp.prProg vtree)
                else ()
          val vtree = Fuse.transform vtree
          val _ = if !printFused
                then (
                  print "********** after fusion **********\n";
                  PrintVExp.prProg vtree)
                else ()
          val stree = SuperOp.convert vtree
          val _ = if !printSuper
                then (
                  print "********** super-op representation **********\n";
                  PrintSuperOp.prProg stree)
                else ()
          val vcode = VCodeGen.gen stree
          val _ = if !printFinal
                then (
                  print "********** fused vcode representation **********\n";
                  PrintVCode.prProg vcode)
                else ()
          in
            (stree, vcode)
          end

    fun withFile f (file, data) = let
	  val outS = TextIO.openOut file
	  in
	    (f (outS, data); TextIO.closeOut outS) handle ex => (TextIO.closeOut outS; raise ex)
	  end

    fun doFile f = let
          val (SuperOp.PROG{sops,...}, vcode) = compile f
	  val base = (case OS.Path.splitBaseExt f
                 of {base, ext=SOME "vcode"} => base
                  | _ => f
                (* end case *))
	  val outFile = OS.Path.joinBaseExt{base=base, ext=SOME "fcode"}
	  val cudaFile = OS.Path.joinBaseExt{base=base, ext=SOME "cu"}
          in
	    withFile PrintVCode.outputProg (outFile, vcode);
            if (! enableFusion)
	      then withFile SuperOpGen.outputSuperOps (cudaFile, sops)
              else ()
          end

    fun generateUnfused (f, out) = let
          val origFuseFlg = !enableFusion
          val _ = (enableFusion := false)
          val (_, vcode) = compile f
          in
            enableFusion := origFuseFlg;
	    withFile PrintVCode.outputProg (out, vcode)
          end

    fun doUnfusedFile f = let
          val origFuseFlg = !enableFusion
          val _ = (enableFusion := false)
          val (_, vcode) = compile f
          val _ = enableFusion := origFuseFlg
          val outFile = (case OS.Path.splitBaseExt f
                 of {base, ext=SOME "vcode"} => OS.Path.joinBaseExt{base=base, ext=SOME "ucode"}
                  | _ => OS.Path.joinBaseExt{base=f, ext=SOME "ucode"}
                (* end case *))
          in
            withFile PrintVCode.outputProg (outFile, vcode)
          end

    fun doBenchmarks () = let
	  val DOTP_SIZES = ["10000000", "5000000", "1000000", "500000", "100000", "50000", "5000", "1000"]
	  val SORT_SIZES = ["1000000", "500000", "100000", "50000", "5000", "1000"]
	  val QUICKHULL_SIZES = ["5000000", "1000000", "500000", "100000", "50000", "5000", "1000"]
	  val BH_SIZES = ["75000", "50000", "20000", "10000", "5000", "1000", "500", "100"]
	  fun doBench prefix x = let
		val f = concat[prefix, x, ".vcode"]
		val _ = print(concat["Handling: ", f, "\n"])
		in
		  doUnfusedFile f;
		  doFile f
		end
	  val doDotp = doBench "../bench/dotp."
	  val doSort = doBench "../bench/qs."
	  val doQuickhull = doBench "../bench/quickhull."
	  val doBH = doBench "../bench/bh."
	  in
	    List.app doDotp DOTP_SIZES;
	    List.app doSort SORT_SIZES;
	    List.app doQuickhull QUICKHULL_SIZES;
	    List.app doBH BH_SIZES
	  end

  end
