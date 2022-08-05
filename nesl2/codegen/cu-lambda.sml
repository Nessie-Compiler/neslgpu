(* cu-lambda.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * This is a representation for data-parallel programs that is designed to both be
 * easily translatable into CUDA and flexible enough to allow fusion of
 *)

structure CuLambda =
  struct

  (* scalar operations *)
    datatype scalar_op = datatype ScalarOp.t

  (* reduce operations *)
    datatype reduce_op = datatype ReduceOp.t

  (* scan operations *)
    datatype scan_op = datatype ScanOp.t

  (* vector operations *)
    datatype vector_op = datatype VectorOp.t

    datatype internal_op = datatype InternalOp.t

    datatype 'var atom_rep
      = Var of 'var
      | Bool of bool
      | Char of char
      | Int of IntInf.int
      | Float of string

  (* GPU kernel types and expressions *)
    structure GPU =
      struct

	datatype ty = TyBool | TyChar | TyInt | TyFloat

	type var = ty CuVar.var
	type atom = var atom_rep

	datatype exp
	  = ExpLet of var list * exp * exp
	  | ExpPrim of var * scalar_op * atom list * exp
	  | ExpIf of atom * exp * exp
	  | ExpTuple of atom list

      end

  (* host types *)
    datatype ty
      = TyScalar of GPU.ty		(* scalars *)
      | TySegdes			(* segment descriptors *)
      | TySeq of GPU.ty * int option	(* sequence with optional length information *)
      | TyTuple of ty list		(* tuples *)

    type var = ty CuVar.var
    type atom = var atom_rep

  (* A GPU task graph consists of a computation over a uniformly-sized collection of
   * sequences.  The source node(s) of the graph may involve generators and the
   * sink node(s) of the graph may involve reductions.  The interior nodes are either
   * maps (i.e., elementwise operations) or scan operations.  We use host variables to
   * label the inputs and outputs (which may overlap) and GPU variables to label the
   * interior communication.
   *)
    structure TaskGraph =
      struct
      (* source nodes *)
	datatype source
	  = SrcVar of GPU.var * var	(* sequence *)
(* NOTE: the segmented sources probably need to be moved to internal ops! *)
	  | SrcFlatDist of GPU.var * atom	(* DIST, where result is flat *)
	  | SrcSegDist of GPU.var * var * var	(* DIST, where the result is segmented; args are values and lengths *)
	  | SrcFlatIndex of GPU.var * atom * atom
					(* INDEX, where the result is flat; args are scalars start and stride *)
	  | SrcSegIndex of GPU.var * var * var * var
					(* INDEX, where the result is segmented; args are segdes, startvec, and stridevec *)
      (* internal nodes *)
	datatype node
	  = NodeMap of GPU.var list * GPU.exp
					(* NodeMap(results, exp) *)
	  | NodeFlatScan of GPU.var * scan_op * GPU.var
					(* NodeScan(result, rator, arg): prefix scan operation rator
					 * over flat sequence arg producing sequence result.
					 *)
	  | NodeSegScan of GPU.var * scan_op * GPU.var * GPU.var
					(* NodeScan(result, rator, segdes, data): prefix scan operation rator
					 * over segmented sequence, producing sequence result.
					 *)

      (* sink nodes *)
	datatype sink
	  = SinkVar of var * GPU.var
	  | SinkFlatReduce of var * reduce_op * GPU.var
					(* reduction, where the argument is a flat sequence *)
	  | SinkSegReduce of var * reduce_op * GPU.var * GPU.var
					(* segmented reduction, where the args are segdes, data *)
	datatype task = Task of {
	    name : string,
	    width : atom,		(* the width of the computation *)
	    inputs : var list,
	    srcs : source list,
	    body : node list,
	    sinks : sink list		(* result nodes of graph; these correspond (in order) to
					 * the outputs. *)
	  }
	fun name (Task{name, ...}) = name
	fun inputs (Task{inputs, ...}) = inputs
	fun outputs (Task{sinks, ...}) = let
	      fun f (SinkVar(x, _)) = x
		| f (SinkFlatReduce(x, _, _)) = x
		| f (SinkSegReduce(x, _, _, _)) = x
	      in
		List.map f sinks
	      end
      end

    type funct = string (* FIXME *)

  (* host expressions *)
    datatype exp_node
      = ExpFun of {		(* CPU-level function definition *)
	  f : funct,
	  params : var list,
	  body : exp,
	  cont : exp
	}
      | ExpLet of {		(* Let binding *)
	  lhs : var list,
	  rhs : exp,
	  cont : exp
	}
      | ExpTask of {		(* GPU task invocation *)
	  task : TaskGraph.task,
	  cont : exp
	}
      | ExpVectorOp of {	(* GPU synchronous vector operation *)
	  result : var,
	  isFlat : bool,	(* true when the arguments are flat vectors *)
	  rator : vector_op,
	  args : atom list,
	  cont : exp
	}
      | ExpCPUOp of {		(* CPU-side operations on vectors *)
	  result : var,
	  rator : internal_op,
	  arg : atom,
	  cont : exp
	}
      | ExpSeq of {
	  result : var,
	  args : atom list,
	  cont : exp
	}
      | ExpTuple of {
	  result : var,
	  args : atom list,
	  cont : exp
	}
      | ExpPrim of {		(* CPU-side scalar operation *)
	  result : var,
	  rator : scalar_op,
	  args : atom list,
	  cont : exp
	}
      | ExpCmd of {		(* I/O command, etc. *)
	  results : var list,
	  cmd : Cmd.cmd,
	  args : atom list,
	  cont : exp
	}
      | ExpIf of {		(* conditional *)
	  cond : var,
	  trueExp : exp,
	  falseExp : exp
	}
      | ExpApply of {		(* application *)
	  f : funct,
	  args : atom list
	}
      | ExpReturn of atom list	(* result of expression *)
    withtype exp = (PropList.holder * exp_node)	(* pair with properties to hold analysis info *)

    datatype program = Program of exp * ty option

    local
      fun mkExp mk args = (PropList.newHolder(), mk args)
    in
    val mkFun = mkExp ExpFun
    val mkLet = mkExp ExpLet
    val mkTask = mkExp ExpTask
    val mkVectorOp = mkExp ExpVectorOp
    val mkCPUOp = mkExp ExpCPUOp
    val mkSeq = mkExp ExpSeq
    val mkTuple = mkExp ExpTuple
    val mkPrim = mkExp ExpPrim
    val mkCmd = mkExp ExpCmd
    val mkIf = mkExp ExpIf
    val mkApply = mkExp ExpApply
    val mkReturn = mkExp ExpReturn
    end (* local *)

  end
