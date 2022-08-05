(* pp-cu-lambda.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure PPCuLambda : sig

    val outputTask : TextIO.outstream * CuLambda.TaskGraph.task -> unit

  end = struct

    structure C = CuLambda
    structure CV = CuVar
    structure TG = C.TaskGraph
    structure GPU = C.GPU
    structure PP = TextIOPP

    val indent0 = PP.Abs 0
    val indent2 = PP.Abs 2
    val indent4 = PP.Abs 4

  (* pretty print a list where a single item has no left/right markers *)
    fun ppList ppFn (left, sep, right) (ppStrm : PP.stream, list) = let
	  fun sp () = PP.space ppStrm 1
	  val string = PP.string ppStrm
	  fun pp [] = string right
	    | pp [x] = (ppFn(ppStrm, x); string right)
	    | pp (x::xs) = (ppFn(ppStrm, x); string sep; sp(); pp xs)
	  in
	    case list
	     of [x] => ppFn(ppStrm, x)
	      | _ => (string left; pp list)
	    (* end case *)
	  end

  (* pretty print a list that always has left/right markers *)
    fun ppList' ppFn (left, sep, right) (ppStrm : PP.stream, list) = let
	  fun sp () = PP.space ppStrm 1
	  val string = PP.string ppStrm
	  fun pp [] = string right
	    | pp [x] = (ppFn(ppStrm, x); string right)
	    | pp (x::xs) = (ppFn(ppStrm, x); string sep; sp(); pp xs)
	  in
	    string left; pp list
	  end

    local
      fun gpuTyToS GPU.TyBool = "bool"
	| gpuTyToS GPU.TyChar = "char"
	| gpuTyToS GPU.TyInt = "int"
	| gpuTyToS GPU.TyFloat = "float"
      fun cpuTyToS (C.TyScalar ty) = gpuTyToS ty
	| cpuTyToS C.TySegdes = "segdes"
	| cpuTyToS (C.TySeq(ty, NONE)) = concat["[", gpuTyToS ty, "]"]
	| cpuTyToS (C.TySeq(ty, SOME n)) = concat["[", gpuTyToS ty, " ## ", Int.toString n, "]"]
	| cpuTyToS (C.TyTuple tys) = concat["(", String.concatWith ", " (List.map cpuTyToS tys), ")"]
    in
    fun ppGPUTy (ppStrm, ty) = PP.string ppStrm (gpuTyToS ty)
    fun ppCPUTy (ppStrm, ty) = PP.string ppStrm (cpuTyToS ty)

    fun ppVar (ppStrm, x) = PP.string ppStrm (CV.toString x)

    fun ppGPUVarBind (ppStrm, x) = PP.string ppStrm (concat[
	    CV.toString x, "#", Int.toString(CV.useCnt x), " : ", gpuTyToS(CV.typeOf x)
	  ])
    fun ppCPUVarBind (ppStrm, x) = PP.string ppStrm (concat[
	    CV.toString x, "#", Int.toString(CV.useCnt x), " : ", cpuTyToS(CV.typeOf x)
	  ])
    end (* local *)

    fun ppAtom (ppStrm, atom) = (case atom
           of C.Var x => ppVar(ppStrm, x)
            | C.Int n => PP.string ppStrm (Format.format "%d" [Format.LINT n])
            | C.Float f => PP.string ppStrm f
            | C.Bool b => PP.string ppStrm (Bool.toString b)
            | C.Char c => PP.string ppStrm ("`" ^ String.str c)
          (* end case *))

    fun ppAtoms (ppStrm, atms) = ppList' ppAtom ("(", ",", ")") (ppStrm, atms)

    fun ppTask (ppStrm, TG.Task{name, width, inputs, srcs, body, sinks}) = let
	  fun sp () = PP.space ppStrm 1
	  fun nl () = PP.newline ppStrm
	  val str = PP.string ppStrm
	  fun ppSrc src = (
		nl();
		PP.openHBox ppStrm;
		  case src
		   of TG.SrcVar(x, x') => (
			ppGPUVarBind(ppStrm, x); str " = "; ppVar(ppStrm, x'); str "[idx]")
		    | TG.SrcFlatDist(x, atm) => (
			ppGPUVarBind(ppStrm, x); str " = "; ppAtom(ppStrm, atm))
		    | TG.SrcSegDist(x, values, lens) => (
			ppGPUVarBind(ppStrm, x); str " = (DIST ";
			ppVar(ppStrm, values); sp(); ppVar(ppStrm, lens); str ")[idx]")
		    | TG.SrcFlatIndex(x, start, stride) => (
			ppGPUVarBind(ppStrm, x); str " = (INDEX ";
			ppAtom(ppStrm, start); sp(); ppAtom(ppStrm, stride); str ")[idx]")
		    | TG.SrcSegIndex(x, segdes, starts, strides) => () (* FIXME *)
		  (* end case *);
		PP.closeBox ppStrm)
	  fun ppLet body ppFn = (
		PP.openHBox ppStrm;
		  str "let"; sp(); ppFn ();
		PP.closeBox ppStrm;
		nl(); ppExp body)
	  and ppExp exp = (case exp
		 of GPU.ExpLet(xs, rhs, e) => ppLet e (fn () => (
		      ppList ppGPUVarBind ("(", ",", ")") (ppStrm, xs);
		      sp(); str "="; sp();
		      ppExp rhs))
		  | GPU.ExpPrim(x, rator, atms, e) => ppLet e (fn () => (
		      ppGPUVarBind (ppStrm, x);
		      sp (); str "="; sp();
		      str (ScalarOp.toString rator); sp();
		      ppAtoms (ppStrm, atms)))
		  | GPU.ExpIf(a, e1, e2) => (
		      PP.openHOVBox ppStrm indent2;
			PP.openHBox ppStrm;
			  str "if"; sp(); ppAtom (ppStrm, a);
			PP.closeBox ppStrm;
			sp();
			PP.openHBox ppStrm;
			  str "then"; sp(); ppExp e1;
			PP.closeBox ppStrm;
			sp();
			PP.openHBox ppStrm;
			  str "else"; sp(); ppExp e2;
			PP.closeBox ppStrm;
		      PP.closeBox ppStrm)
		  | GPU.ExpTuple xs => ppList' ppAtom ("(", ",", ")") (ppStrm, xs)
		(* end case *))
	  fun ppNode node = (
		nl();
		case node
		 of TG.NodeMap(xs, body) => (
		      PP.openVBox ppStrm indent2;
			PP.openHBox ppStrm;
			  ppList ppGPUVarBind ("(", ",", ")") (ppStrm, xs);
			  sp(); str "="; sp(); str "kernel"; sp(); str "{";
			PP.closeBox ppStrm;
			PP.openVBox ppStrm indent2;
			  nl(); ppExp body;
			PP.closeBox ppStrm;
			nl(); str "}";
		      PP.closeBox ppStrm)
		  | TG.NodeFlatScan(x, rator, y) => (
		      str "__sync"; nl();
		      PP.openHBox ppStrm;
			ppGPUVarBind(ppStrm, x); str " = ("; str(ScanOp.toString rator);
			sp(); ppVar(ppStrm, y); str ")";
		      PP.closeBox ppStrm)
		  | TG.NodeSegScan(x, rator, seg, data) => (
		      str "__sync"; nl();
		      PP.openHBox ppStrm;
			ppGPUVarBind(ppStrm, x); str " = ("; str(ScanOp.toString rator);
			sp(); ppVar(ppStrm, seg); sp(); ppVar(ppStrm, data); str ")";
		      PP.closeBox ppStrm)
		(* end case *))
	  fun ppSink sink = (
		nl();
		PP.openHBox ppStrm;
		  case sink
		   of TG.SinkVar(x, x') => (
			ppVar(ppStrm, x); str "[idx] = "; ppVar(ppStrm, x'))
		    | TG.SinkFlatReduce(x, rator, y) => (
			ppVar(ppStrm, x); str " = ("; str(ReduceOp.toString rator);
			sp(); ppVar(ppStrm, y); str ")")
		    | TG.SinkSegReduce(x, rator, seg, data) => (
			ppVar(ppStrm, x); str " = ("; str(ReduceOp.toString rator);
			sp(); ppVar(ppStrm, seg); sp(); ppVar(ppStrm, data); str ")")
		  (* end case *);
		PP.closeBox ppStrm)
	  in
	    PP.openVBox ppStrm indent0;
	      PP.openHBox ppStrm;
		str "task"; sp(); str name;
		ppList' ppCPUVarBind ("(", ", ", ")") (ppStrm, inputs);
		str " ## "; ppAtom(ppStrm, width); sp(); str "{";
	      PP.closeBox ppStrm;
	      PP.openVBox ppStrm indent2;
		List.app ppSrc srcs;
		List.app ppNode body;
		List.app ppSink sinks;
	      PP.closeBox ppStrm;
	      nl();
	      str "}"; nl();
	    PP.closeBox ppStrm
	  end

    fun outputTask (outS, task) = let
	  val ppStrm = PP.openOut {dst = outS, wid = 120}
	  in
	    PP.openVBox ppStrm indent0;
	      PP.string ppStrm (concat["%-- Start ", TG.name task, "--%"]); PP.newline ppStrm;
	      ppTask (ppStrm, task);
	      PP.string ppStrm (concat["%-- End ", TG.name task, "--%"]); PP.newline ppStrm;
	    PP.closeBox ppStrm;
	    PP.closeStream ppStrm
	  end

  end
