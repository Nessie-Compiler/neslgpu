(* test file for vcode conversion/reduction *)

CM.make "sources.cm";

val e = Convert.convert MulAdd.optProg;
val _ = (print "***** After conversion *****\n"; PrintVExp.prProg e);
val e' = Reduce.reduce e;
val _ = (print "***** After optmization *****\n"; PrintVExp.prProg e');
