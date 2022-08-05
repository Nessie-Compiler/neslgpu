(* super-op-gen.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

structure SuperOpGen : sig

    val outputSuperOps : TextIO.outstream * SuperOp.super_op list -> unit
    val prSuperOps : SuperOp.super_op list -> unit

    val doTestFile : string -> unit

  end = struct

    structure PP = TextIOPP
    structure S = SuperOp
    structure VC = VCode
    structure VE = VExp

    val indent0 = (PP.Abs 0)
    val indent2 = (PP.Abs 2)
    val indent4 = (PP.Abs 4)

    fun new outs = PP.openOut {dst = outs, wid = 120}

    val close = PP.closeStream
                        
    local
      structure Ord =
        struct
          type ord_key = Word.word
          val compare = Word.compare
        end
    in
    structure WMap = RedBlackMapFn (Ord)
    structure WSet = RedBlackSetFn (Ord)
    end (* local *)

    fun computeSignature (S.SOp{paramTys, resTy, def, ...}) = (resTy, paramTys)

    fun outputSuperOpsStm (outS, supers) = let
	  val str = PP.string outS
	  fun outputHeader () = (
		str "#include \"config.h\"";
		PP.newline outS;
		str "#include \"vcode.h\"";
		PP.newline outS;
		str "#include <cvl.h>";
		PP.newline outS;
		str "#include \"y.tab.h\"";
		PP.newline outS;
		str "#include <cutil_inline.h>";
		PP.newline outS;
		str "#include \"defins.cuh\"";
		PP.newline outS;
		PP.newline outS;
		str "MAXALIGN *ComputeMemory = NULL;";
		PP.newline outS;
		PP.newline outS;
		str "extern \"C\" void init (MAXALIGN *mem) {";
		PP.newline outS;
		str "  ComputeMemory = mem;";
		PP.newline outS;
		str "}";
		PP.newline outS;
		PP.newline outS)
	  fun outputSuperOpKernel (S.SOp{id, paramTys, resTy=VCode.SEGDES, def, ...}) = let
              val [v] = (case def
                         of S.EXP (VE.MAKE_SEGDES, [SuperOp.EXP(VE.CONST(_, vs), _)]) => vs
                          | _ => raise Fail (concat["Found a fused kernel with a return type of SEGDES that is not a simple MAKE_SEGDES(CONST(...),...)"]))
		val name = concat ["fused", Int.toString id, "Kernel"]
          in
	      str (concat["__global__ void ", name, "(MAXALIGN *data, int dst, int len, int scratch) {"]);
	      PP.openVBox outS indent2;
	      PP.newline outS;

	      str "int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;";
	      PP.newline outS;
	      str "if (address < (len-1)) {";
	      PP.openVBox outS indent2;
	      PP.newline outS;
              
	      str (concat["int *pDst = (int*)(&data[dst]);"]);
	      PP.newline outS;
              str (concat["pDst[address] = ", v, ";"]);
	      PP.closeBox outS;
	      PP.newline outS;
	      str "} else if (address == (len-1)) {";

	      PP.openVBox outS indent2;
	      PP.newline outS;
	      str (concat["int *pDst = (int*)(&data[dst]);"]);
	      PP.newline outS;
              str (concat["pDst[address] = len-1;"]);

	      PP.closeBox outS;
	      PP.newline outS;
	      str "}";

	      PP.closeBox outS;
	      PP.newline outS;
	      str "}";
	      PP.newline outS;
	      PP.newline outS
          end
	    |  outputSuperOpKernel (S.SOp{id, paramTys, resTy, def, ...}) = let
		val arity = List.length paramTys
		val name = concat ["fused", Int.toString id, "Kernel"]
		fun printParams i = if (i = arity)
		      then ()
		      else (
			str (concat["int s", Int.toString i, ", "]);
			printParams (i+1))
		fun tyToString VC.INT = "int"
		  | tyToString VC.BOOL = "int"
		  | tyToString VC.FLOAT = "float"
		  | tyToString VC.CHAR = raise Fail "Should not be seeing character arrays."
		  | tyToString VC.SEGDES = "int"
		  | tyToString VC.ANY = "int"	(* use int for unknown types *)
		fun printInputs () = let
		      fun prIn (_, []) = ()
			| prIn (i, ty::tys) = let
			    val typ = tyToString ty
			    val istr = Int.toString i
			    in
			      str (concat[
				  typ, " *pSrc", istr, " = (", typ, "*)(&data[s", istr, "]);"
				]);
			      PP.newline outS;
			      prIn (i+1, tys)
			    end
		      in
			prIn (0, paramTys)
		      end
		fun printDef def = let
		      fun printExp exp = (case exp
			     of S.ARG i => str (concat["pSrc", Int.toString (Word.toInt i), "[address]"])
			      | S.EXP (pure, exps) => (
				str "(";
				printPure (pure, exps);
				str ")")
			    (* end case *))
		      and printUnary (s, [u]) = (
			    str s;
			    str "(";
			    printExp u;
			    str ")")
			| printUnary (s, _) = raise Fail(concat[
                              "Unary operator ", s,
                              " called with with wrong number of arguments."
                            ])
		      and printBinary (s, [l,r]) = (
			    str s;
			    str "(";
			    printExp l;
			    str ", ";
			    printExp r;
			    str ")")
			| printBinary (s, _) = raise Fail(concat[
                              "Binary operator ", s,
                              " called with with wrong number of arguments."
                            ])
		      and printTernary (s, [a,b,c]) = (
			    str s;
			    str "(";
			    printExp a;
			    str ", ";
			    printExp b;
			    str ", ";
			    printExp c;
			    str ")")
			| printTernary (s, _) = raise Fail(concat[
                              "Ternary operator ", s,
                              " called with with wrong number of arguments."
                            ])
		    and printConst ([v]) = str v
		      | printConst _  = raise Fail "Found a multi-value constant."
		    and printPure (pure, exps) = (case pure
			   of VE.ADD ty => printBinary ("plus", exps)
			    | VE.SUB ty => printBinary ("minus", exps)
			    | VE.MUL ty => printBinary ("times", exps)
			    | VE.DIV ty => printBinary ("divide", exps)
			    | VE.MOD => printBinary ("mod", exps)
			    | VE.LT ty => printBinary ("lt", exps)
			    | VE.LTE ty => printBinary ("leq", exps)
			    | VE.GT ty => printBinary ("gt", exps)
			    | VE.GTE ty => printBinary ("geq", exps)
			    | VE.EQ ty => printBinary ("eq", exps)
			    | VE.NEQ ty => printBinary ("neq", exps)
			    | VE.LSHIFT => printBinary ("lshift", exps)
			    | VE.RSHIFT => printBinary ("rshift", exps)
			    | VE.NOT ty => (
                              case ty
                               of VCode.INT => printUnary ("bnot", exps)
                                | _ => printUnary ("nnot", exps))
			    | VE.AND ty => (
                              case ty
                               of VCode.INT => printBinary ("band", exps)
                                | _ => printBinary ("aand", exps))
			    | VE.OR ty => (
                              case ty
                               of VCode.INT => printBinary ("bor", exps)
                                | _ => printBinary ("oor", exps))
			    | VE.XOR ty => (
                              case ty
                               of VCode.INT => printBinary ("xxor", exps)
                                | _ => printBinary ("lxor", exps))
			    | VE.SELECT ty => printTernary ("selection", exps)
			    | VE.RAND => printUnary ("cvlrand", exps)
			    | VE.FLOOR => printUnary ("cvl_floor", exps)
			    | VE.CEIL => printUnary ("cvl_ceil", exps)
			    | VE.TRUNC => printUnary ("d_to_z", exps)
			    | VE.ROUND => printUnary ("cvl_round", exps)
			    | VE.I_TO_F => printUnary ("z_to_d", exps)
			    | VE.I_TO_B => printUnary ("z_to_b", exps)
			    | VE.B_TO_I => printUnary ("b_to_z", exps)
			    | VE.LOG => printUnary ("log", exps)
			    | VE.SQRT => printUnary ("sqrt", exps)
			    | VE.EXP => printUnary ("exp", exps)
			    | VE.SIN => printUnary ("sin", exps)
			    | VE.COS => printUnary ("cos", exps)
			    | VE.TAN => printUnary ("tan", exps)
			    | VE.ASIN => printUnary ("asin", exps)
			    | VE.ACOS => printUnary ("acos", exps)
			    | VE.ATAN => printUnary ("atan", exps)
			    | VE.SINH => printUnary ("sinh", exps)
			    | VE.COSH => printUnary ("cosh", exps)
			    | VE.TANH => printUnary ("tanh", exps)
			   (* Vector instructions *)
			    | VE.ADD_SCAN ty => ()
			    | VE.MUL_SCAN ty => ()
			    | VE.MAX_SCAN ty => ()
			    | VE.MIN_SCAN ty => ()
			    | VE.AND_SCAN ty => ()
			    | VE.OR_SCAN ty => ()
			    | VE.XOR_SCAN ty => ()
			    | VE.ADD_REDUCE ty => ()
			    | VE.MUL_REDUCE ty => ()
			    | VE.MAX_REDUCE ty => ()
			    | VE.MIN_REDUCE ty => ()
			    | VE.AND_REDUCE ty => ()
			    | VE.OR_REDUCE ty => ()
			    | VE.XOR_REDUCE ty => ()
			    | VE.PERMUTE ty => ()
			    | VE.DPERMUTE ty => ()
			    | VE.FPERMUTE ty => ()
			    | VE.BPERMUTE ty => ()
			    | VE.BFPERMUTE ty => ()
			    | VE.DFPERMUTE ty => ()
			    | VE.EXTRACT ty => ()
			    | VE.REPLACE ty => ()
			    | VE.RANK_UP ty => ()
			    | VE.RANK_DOWN ty => ()
			    | VE.DIST ty => (
				case exps
				 of [SuperOp.EXP(VE.CONST(VCode.INT, vs), _), _] => printConst vs
                                  | [SuperOp.EXP(VE.CONST(VCode.FLOAT, vs), _), _] => printConst vs
                                  | [SuperOp.EXP(VE.CONST(VCode.CHAR, vs), _), _] => printConst vs
                                  | [SuperOp.EXP(VE.CONST(VCode.BOOL, vs), _), _] => (
                                    case vs
                                     of ["T"] => str "TRUE"
                                      | ["F"] => str "FALSE"
                                      | _ => raise Fail (concat["Found a DIST CONST BOOL that was not T or F"]))
                                  | _ => raise Fail (concat["Found a DIST that did not have a CONST"])
				(* END CASE *))
			    | VE.INDEX => ()
			    | VE.LENGTH ty => ()
			    | VE.MAKE_SEGDES => (
				case exps
				 of [SuperOp.EXP(VE.CONST(_, vs), _)] => printConst vs
				  | _ => raise Fail (concat["Found a MAKE_SEGDES that did not have a CONST"])
				(* END CASE *))
			    | VE.LENGTHS => ()
			    | VE.CONST(ty, vs) => printConst vs
			  (* end case *))
		    in
		      str "pDst[address] = ";
		      printExp def;
		      str ";"
		    end (* printDef *)
		val resultTyp = tyToString resTy
		in
		  str (concat["__global__ void ", name, "(MAXALIGN *data, int dst, "]);
		  printParams 0;
		  str (concat["int len, int scratch) {"]);
		  PP.openVBox outS indent2;
		  PP.newline outS;

		  str "int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;";
		  PP.newline outS;
		  str "if (address < len) {";
		  PP.openVBox outS indent2;
		  PP.newline outS;
                  
		  str (concat[resultTyp, " *pDst = (", resultTyp, "*)(&data[dst]);"]);
		  PP.newline outS;
		  printInputs ();
		  PP.newline outS;


		  printDef def;

		  PP.closeBox outS;
		  PP.newline outS;
		  str "}";

		  PP.closeBox outS;
		  PP.newline outS;
		  str "}";
		  PP.newline outS;
		  PP.newline outS
		end                                   
	  fun outputSuperOpHost (S.SOp{id, paramTys, resTy, ...}) = let
		val arity = List.length paramTys
		val name = concat ["fused", Int.toString id]
		fun printParams i = if (i = arity)
		      then ()
		      else (str (concat["vec_p s", Int.toString i, ", "]); printParams (i+1))
		fun printArgs i = if (i = arity)
		      then ()
		      else (str (concat["s", Int.toString i, ", "]); printArgs (i+1))
		in
		  str (concat["void ", name, "(vec_p d, "]);
		  printParams 0;
		  str (concat["int len, vec_p scratch) {"]);
		  PP.openVBox outS indent2;
		  PP.newline outS;
		  str "if (len==0) {return;}";
		  PP.newline outS;
		  str "SYNC();";
		  PP.newline outS;
                  case resTy
                   of VCode.SEGDES => str "DEF_BLOCKS_PER_GRID(len+1);"
                    | _ => str "DEF_BLOCKS_PER_GRID(len);";
		  PP.newline outS;
		  str (concat[name, "Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, "]);
		  printArgs 0;
		  str "len, scratch);";
		  PP.newline outS;
		  str (concat["cutilCheckMsg(\"",name, " execution failed\\n\");"]);
		  PP.closeBox outS;
		  PP.newline outS;
		  str "}";
		  PP.newline outS;
		  PP.newline outS
		end
	  fun outputSuperOpScratch (S.SOp{id, ...}) = let
		val name = concat ["fused", Int.toString id]
		in
		(* FIXME: only works for elementwise and const *)
		  str (concat["make_no_scratch(",
			      name,
			      ")"]);
		  PP.newline outS
		end
	  fun outputSuperOpInplace (S.SOp{id, paramTys, resTy, ...}) = let
		val name = concat ["fused", Int.toString id]
                fun doParamTys ([], i) = "INPLACE_NONE"
                  | doParamTys (VCode.SEGDES::xs, i) = doParamTys (xs, i+1)
                  | doParamTys (_, i) = concat["INPLACE_", Int.toString i]
		in
		  str (concat["make_inplace(", name, ", ",
                              case resTy
                               of VCode.SEGDES => "INPLACE_NONE"
                                | _ => doParamTys (paramTys, 1),
                              ")"]);
		  PP.newline outS
		end
	  fun outputVops supers = let
		fun outputVopStart() = (
		      str "vopdes_t vops[] = {";
		      PP.openVBox outS indent2;
		      PP.newline outS)
		fun outputVopEnd() = (
		      PP.closeBox outS;
		      str "};";
		      PP.newline outS;
		      PP.newline outS)
		fun tyToString VC.INT = "Int"
		  | tyToString VC.BOOL = "Bool"
		  | tyToString VC.FLOAT = "Float"
		  | tyToString VC.CHAR = "Char"
		  | tyToString VC.SEGDES = "Segdes"
		  | tyToString VC.ANY = "Bool"	(* use Bool for unknown types *)
		fun outputVop (S.SOp{id, paramTys, resTy, ...}) = let
		    val name = concat ["fused", Int.toString id]
                    val arity = List.length paramTys
                    val maxArgs = 40
                    val opName = case resTy
                                  of VCode.SEGDES => "MAKE_SEGDES"
                                   | _ => "FUSED"

		in
                    if arity > maxArgs
                    then print (concat[
                                name, " has higher arity (",
                                Int.toString(arity),
                                ") than is supported by vinterp."])
                    else (str (concat["{", opName, ", \"",
				      name,
				      "\", ", Int.toString arity, ", 1,"]);
			  PP.newline outS;
                          str (concat["{", String.concatWith "," (List.map tyToString paramTys @
                                                                  (List.tabulate ((maxArgs - arity), (fn _ => "Illegal")))), "},"]);
			  PP.newline outS;
                          str (concat["{", String.concatWith "," ((List.tabulate (arity, (fn _ => "NONE"))) @
                                                                 (List.tabulate ((maxArgs - arity), (fn _ => "Illegal")))), "},"]);
			  PP.newline outS;
			  str (concat["{", tyToString resTy,",},"]);
			  PP.newline outS;
			  str (concat["{",
                                      case paramTys
                                       of VCode.SEGDES::xs => "COMPAT1"
                                        | _ => "AGREE1",
                                      ",},"]);
			  PP.newline outS;
			  str "{1,},";
			  PP.newline outS;
                          case resTy
                           of VCode.SEGDES => str (concat["SegOp},"])
                            | _ => str (concat["Elwise", Int.toString arity, "},"]);
			  PP.newline outS)
                end
	  in
	      outputVopStart();
	      List.app outputVop supers;
	      outputVopEnd()
	  end
	        fun outputCVLs supers = let
		fun outputCVLStart() = (
		      str "cvl_triple_t cvl_funs[] = {";
		      PP.openVBox outS indent2;
		      PP.newline outS)
		      fun outputCVLEnd() = (
		      PP.closeBox outS;
		      str "};";
		      PP.newline outS)
		fun outputCVL (S.SOp{id, ...}) = let
		      val name = concat ["fused", Int.toString id]
		      in
			str (concat ["{ { (void (*)())",
				     name,
				     ", (int (*)())",
				     name,
				     "_scratch, (unsigned (*)())",
				     name,
				     "_inplace },},"]);
			PP.newline outS
		      end
		in
		  outputCVLStart();
		  List.app outputCVL supers;
		  outputCVLEnd()
		end
	  in
	    outputHeader ();
	    List.app outputSuperOpKernel supers;
	    List.app outputSuperOpHost supers;
	    List.app outputSuperOpScratch supers;
	    List.app outputSuperOpInplace supers;
	    outputVops supers;
	    outputCVLs supers
	  end

    fun outputSuperOps (outS, item) = let
          val ppStrm = new outS
          in
            outputSuperOpsStm (ppStrm, item);

            PP.string ppStrm "/*";
	    PP.newline ppStrm;

            List.app (fn sop => (PrintSuperOp.outputSOp (outS, sop); (PP.newline ppStrm))) item;
            
            PP.string ppStrm "*/";
	    PP.newline ppStrm;
                  
            close ppStrm
          end

    fun prSuperOps supers = outputSuperOps (TextIO.stdOut, supers)

    fun doTestFile f = let
          val vcode = Parser.parseFile f
          val vcode = Inliner.inline vcode
          val vtree = Convert.convert vcode
          val vtree = Reduce.transform vtree
          val vtree = Fuse.transform vtree
          val S.PROG{sops,...} = SuperOp.convert vtree
	  in
	    prSuperOps sops
	  end

  end
