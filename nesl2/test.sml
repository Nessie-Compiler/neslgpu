(* test.sml
 *
 * COPYRIGHT (c) 2013 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Run the Nesl compiler over the various test programs.
 *)

structure Test =
  struct

    local
      fun test path () = (
	    print(concat["** compiling ", path, "\n"]);
	    Main.doit path)
    in
    val batcher = test "tests/batcher.nesl"
    val bh = test "tests/bh.nesl"
    val blackscholes = test "tests/black-scholes.nesl"
    val delaunay = test "tests/delaunay.nesl"
    val dotp = test "tests/dotp.nesl"
    val fact = test "tests/fact.nesl"
    val logsum = test "tests/logsum.nesl"
    val mandelbrot = test "tests/mandelbrot.nesl"
    val muladd = test "tests/muladd.nesl"
    val nbody = test "tests/nbody.nesl"
    val qs = test "tests/qs.nesl"
    val quickhull = test "tests/quickhull.nesl"
    val radix = test "tests/radix.nesl"
    val sumlogsum = test "tests/sumlogsum.nesl"
    end

    fun all () = (
	  batcher();
	  bh();
	  blackscholes();
	  delaunay();
	  dotp();
	  fact();
	  logsum();
	  mandelbrot();
	  muladd();
	  nbody();
	  qs();
	  quickhull();
	  radix();
	  sumlogsum();
	  print "** done\n");

  end (* Test *)
