(* task-graph.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * This module implements an analysis of the fused program to figure out the task
 * graphs (i.e., CUDA kernels).
 *)

structure TaskGraph : sig

    type task
    type node

    val getNode : FuseAST.var -> node option
    val getTask : FuseAST.var -> task option

    val isSource : node -> bool
    val isSink : node -> bool
    val preds : node -> node list
    val succs : node -> node list
    val defn : node -> (FuseAST.var list * FuseAST.rhs)
    val nodeToString : node -> string

  (* given a node and a variable that occurs in the rhs (resp. lhs) of the node, return true if the
   * variable is defined by a FuseAST binding that corresponds to a predecessor (resp. successor)
   * of the node.
   *)
    val isPred : node * FuseAST.var -> bool
    val isSucc : node * FuseAST.var -> bool

    val taskName : task -> string
    val nodes : task -> node list	(* nodes will be in topological order *)

  (* per-task properties *)
    val newProp : (task -> 'a)
	-> {
	    clrFn: task -> unit,
	    getFn: task -> 'a,
	    peekFn: task -> 'a option,
	    setFn: task * 'a -> unit
	  }
    val newFlag : unit -> {getFn : task -> bool, setFn : task * bool -> unit}

    val mkGraph : FuseAST.program -> task list

    val printTask : TextIO.outstream * task -> unit

  end = struct

    structure F = FuseAST
    structure V = FuseVar
    structure VSet = V.Set

    datatype node = ND of {
	task : node_set URef.uref,	(* the connected subgraph that this node belongs to *)
	id : Stamp.stamp,		(* unique ID *)
	preds : (F.var * node) list,	(* predecessors of the node paired with the variable that
					 * induces the dependency. *)
	lhs : F.var list,		(* lhs variables of the node definition *)
	rhs : F.rhs,			(* rhs of the node definition *)
	succs : node list ref		(* successor nodes in the task graph *)
      }

    withtype node_set = node list

  (* a task is a connected set of nodes *)
    datatype task = TASK of {
	id : int,
	nodes : node_set,
	props : PropList.holder
      }

    fun newProp init = PropList.newProp (fn (TASK{props, ...}) => props, init)

    fun newFlag () = PropList.newFlag (fn (TASK{props, ...}) => props)

  (* properties to track the nodes and tasks associated with a variable *)
    val {getFn=(nodeOf : F.var -> node), peekFn=getNode, setFn=setNode, ...} =
	  V.newProp (fn _ => raise Fail "node prop")
    val {peekFn=(getTask : F.var -> task option), setFn=setTask, ...} =
	  V.newProp (fn _ => raise Fail "task prop")

    fun compare (ND{id=a, ...}, ND{id=b, ...}) = Stamp.compare(a, b)

  (* lists of nodes ordered by their stamps *)
    type node_set = node list
    fun merge (nds1 : node_set, nds2) = let
	  fun merge' ([], [], l) = List.rev l
	    | merge' ([], nds2, l) = List.revAppend(l, nds2)
	    | merge' (nds1, [], l) = List.revAppend(l, nds1)
	    | merge' (nds1 as (nd1::r1), nds2 as (nd2::r2), l) = (case compare(nd1, nd2)
		 of LESS => merge' (r1, nds2, nd1::l)
		  | EQUAL => merge' (r1, r2, nd1::l)
		  | GREATER => merge' (nds1, r2, nd2::l)
		(* end case *))
	  in
	    merge' (nds1, nds2, [])
	  end
    fun add (nd, nds) = merge([nd], nds)

  (* create a new graph node and link it into the graph *)
    fun new (lhs, rhs, rhsVars) = let
	  val preds = List.map (fn x => (x, nodeOf x)) rhsVars
	  val task = URef.uRef []
	  val nd = let
		val nd = ND{
			task = task,
			id = Stamp.new(),
			preds = preds,
			lhs = lhs,
			rhs = rhs,
			succs = ref[]
		      }
		in
		  URef.update(task, [nd]);
		  nd
		end
	(* for each predecessor, merge the task graphs and add the successor edge *)
	  val _ = let
		fun doPred (_, ND{task=task', succs, ...}) = (
		      URef.unify merge (task, task');
		      succs := nd :: !succs)
		in
		  List.app doPred preds
		end
	  in
	    nd
	  end

    fun sameNd (ND{id=a, ...}, ND{id=b, ...}) = Stamp.same(a, b)

    fun nodeToString (ND{lhs, ...}) = String.concat [
	    "{", String.concatWith ";" (List.map V.toString lhs), "}"
	  ]

    structure NdSet = RedBlackSetFn (
      struct
	type ord_key = node
	val compare = compare
      end)
    structure NdTbl = HashTableFn (
      struct
	type hash_key = node
	fun hashVal (ND{id, ...}) = Stamp.hash id
	val sameKey = sameNd
      end)

    fun preds (ND{preds, ...}) = List.map #2 preds
    fun succs (ND{succs, ...}) = !succs

  (* A node is a source if the preds list is empty *)
    fun isSource (ND{preds = [], ...}) = true
      | isSource _ = false

  (* a node is a sink if all of the uses of the lhs variables are sources *)
    fun isSink (ND{succs, ...}) = List.null(!succs)

    fun defn (ND{lhs, rhs, ...}) = (lhs, rhs)

  (* given a node and a variable that occurs in the rhs of the node, return true if the
   * variable is defined by a FuseAST binding that corresponds to a predecessor of the node.
   *)
    fun isPred (ND{preds, ...}, x) = (case getNode x
	   of NONE => false
	    | SOME nd => List.exists (fn (_, nd') => sameNd(nd, nd')) preds
	  (* end case *))

  (* given a node and a variable that occurs in the lhs of the node, return true if the
   * variable is defined by a FuseAST binding that corresponds to a successor of the node.
   *)
    fun isSucc (ND{succs, ...}, x) = let
	  fun chkSucc (ND{preds, ...}) = List.exists (fn (y, _) => V.same(x, y)) preds
	  in
	    List.exists chkSucc (!succs)
	  end

  (* topologically sort the nodes in a task such that nodes come before their successors *)
    fun sort nodes = let
	  fun dfs (nd::nds, visited, sortList) =
		if NdSet.member(visited, nd)
		  then dfs (nds, visited, sortList)
		  else let
		    val (visited, sortList) = dfs (succs nd, NdSet.add(visited, nd), sortList)
		    in
		      dfs(nds, visited, nd::sortList)
		    end
	    | dfs ([], visited, sortList) = (visited, sortList)
	  in
	    #2 (dfs (nodes, NdSet.empty, []))
	  end

  (* given a list of nodes, return a list of tasks. *)
    fun getTasks nodes = let
	  val nTasks = ref 0
	  val tbl = NdTbl.mkTable (List.length nodes, Fail "task table")
	  val tasks = ref []
	  fun doNode (nd as ND{task, lhs, ...}) = let
		val nodes as (first::_) = URef.!! task
		in
		  case NdTbl.find tbl first
		   of SOME task' => List.app (fn x => setTask(x, task')) lhs
		    | NONE => let
			val id = !nTasks
			val task' = TASK{id = id, nodes = sort nodes, props = PropList.newHolder()}
			in
			  nTasks := id+1;
			  List.app (fn x => setTask(x, task')) lhs;
			  tasks := task' :: !tasks;
			  NdTbl.insert tbl (first, task')
			end
		  (* end case *)
		end
	  in
	    List.app doNode nodes;
	    !tasks
	  end

    fun nodes (TASK{nodes, ...}) = nodes

    fun taskName (TASK{id, ...}) = "task" ^ Int.toString id

    fun printTask (outS, task) = let
	  fun pr s = TextIO.output(outS, s)
	  fun prNode (nd as ND{succs, ...}) = (
		pr "  ";  pr(nodeToString nd); pr " -->";
		List.app (fn nd => (pr " "; pr(nodeToString nd))) (!succs);
		pr "\n")
	  in
	    pr(concat["***** ", taskName task, " *****\n"]);
	    List.app prNode (nodes task);
	    pr "*****\n"
	  end

    fun filter (env, xs) = List.filter (fn x => VSet.member(env, x)) xs

    fun extend (env, xs) = VSet.addList (env, xs)

    fun mkGraph (F.Program prog) = let
	  val nodes = ref []
	  fun addNode args = let
		val nd = new args
		in
		  nodes := nd :: !nodes;
		  nd
		end
	  fun analTop (top, env) = (case top
		 of F.Top_Kern k => env
		  | F.Top_Funct(f, xs, body) => (analExp (VSet.empty, body); env)
		  | F.Top_Let(xs, e) => (analExp (env, e); env)
		  | F.Top_RHS(xs, rhs) => analRHS (env, xs, rhs)
		  | F.Top_Exp(e, _) => (analExp (env, e); env)
		(* end case *))
	  and analExp (env, exp) = (case exp
		 of F.Exp_Let(xs, e1, e2) => (
		      analExp (env, e1);
		      analExp (env, e2))
		  | F.Exp_RHS(xs, rhs, e) => analExp (analRHS (env, xs, rhs), e)
		  | F.Exp_If(_, e1, e2) => (
		    (* note that we do not merge tasks across conditionals *)
		      analExp (VSet.empty, e1);
		      analExp (VSet.empty, e2))
		  | F.Exp_Apply _ => ()
		  | F.Exp_Atom _ => ()
		(* end case *))
	  and analRHS (env, lhs, rhs) = (case rhs
		 of F.RHS_Scalar _ => env
		  | F.RHS_FlatGen _ => let
		      val nd = addNode(lhs, rhs, [])
		      in
			List.app (fn x => setNode(x, nd)) lhs;
			extend (env, lhs)
		      end
		  | F.RHS_SegGen _ => let
		      val nd = addNode(lhs, rhs, [])
		      in
			List.app (fn x => setNode(x, nd)) lhs;
			extend (env, lhs)
		      end
		  | F.RHS_Map(_, _, args) => let
		      val nd = addNode(lhs, rhs, filter (env, args))
		      in
			List.app (fn x => setNode(x, nd)) lhs;
			extend (env, lhs)
		      end
		  | F.RHS_FlatReduce(_, _, arg) => let
		      val nd = addNode(lhs, rhs, filter (env, [arg]))
		      in
			List.app (fn x => setNode(x, nd)) lhs;
			env
		      end
		  | F.RHS_SegReduce(_, _, seg, data) => let
		      val nd = addNode(lhs, rhs, filter (env, [seg, data]))
		      in
			List.app (fn x => setNode(x, nd)) lhs;
			env
		      end
		  | F.RHS_FlatScan(_, _, x) => let
		      val nd = addNode(lhs, rhs, filter (env, [x]))
		      in
			List.app (fn x => setNode(x, nd)) lhs;
			extend (env, lhs)
		      end
		  | F.RHS_SegScan(rator, _, seg, data) => let
		      val nd = addNode(lhs, rhs, filter (env, [seg, data]))
		      in
			List.app (fn x => setNode(x, nd)) lhs;
			extend (env, lhs)
		      end
		  | F.RHS_FlatVector(rator, args) => env
		  | F.RHS_SegVector(rator, args) => env
		  | F.RHS_Internal(rator, args) => env
		  | F.RHS_Cmd _ => VSet.empty (* don't fuse across commands *)
		  | F.RHS_Seq _ => env
		  | F.RHS_Tuple _ => env
		  | F.RHS_Proj _ => env
		(* end case *))
	  in
	    ignore (List.foldl analTop VSet.empty prog);
	    getTasks (!nodes)
	  end

  end
