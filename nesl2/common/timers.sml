(* timers.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Timers =
  struct

    val timeCompiler = PhaseTimer.newTimer "compiler"

    val timeFront = PhaseTimer.newPhase (timeCompiler, "front end")
    val timeParser = PhaseTimer.newPhase (timeFront, "parser")
    val timeTypechecker = PhaseTimer.newPhase (timeFront, "typechecker")

    val timeMiddle =  PhaseTimer.newPhase (timeCompiler, "middle end")
    val timeMono = PhaseTimer.newPhase (timeMiddle, "monomorphize")
    val timeNormal = PhaseTimer.newPhase (timeMiddle, "normalize")
    val timeFlatten = PhaseTimer.newPhase (timeMiddle, "flatten")
    val timeOpt = PhaseTimer.newPhase (timeMiddle, "optimization")
    val timeFuse = PhaseTimer.newPhase (timeMiddle, "fusion")

    val timeBack =  PhaseTimer.newPhase (timeCompiler, "back end")
    val timeCodegen = PhaseTimer.newPhase (timeBack, "code generation")

  end
