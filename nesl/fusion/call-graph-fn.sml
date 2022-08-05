(* call-graph-fn.sml
 *
 * COPYRIGHT (c) 2012 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *)

signature CALL_GRAPH_FUNC =
  sig
    type func

    val labelOf : func -> VCode.label

  (* apply a function to the call sites of a function *)
    val appCalls : (VCode.label -> unit) -> func -> unit

  end

functor CallGraphFn (F : CALL_GRAPH_FUNC) : sig

    type func = F.func

    datatype cg_node = Nd of func * cg_node list ref * PropList.holder

    datatype component
      = SIMPLE of cg_node
      | RECURSIVE of cg_node list

    type cg = {
            nodes : cg_node list,       (* the CG nodes in program order *)
            ndOfLab : VCode.label -> cg_node,
            topOrder : component list
          }

    val mkCallGraph : func list -> cg

    val nodeToString : cg_node -> string

    val newProp : (cg_node -> 'a) -> {
            clrFn : cg_node -> unit,
            getFn : cg_node -> 'a,
            peekFn : cg_node -> 'a option,
            setFn : cg_node * 'a -> unit
          }
    val newFlag : unit -> {getFn : cg_node -> bool, setFn : cg_node * bool -> unit}

  end = struct

    structure ATbl = AtomTable

    type func = F.func

  (* call-graph node, which contains extra info about the function *)
    datatype cg_node = Nd of func * cg_node list ref * PropList.holder

    fun mkNode f = Nd(f, ref[], PropList.newHolder())
    fun edgesOfNd (Nd(_, ref l, _)) = l
    fun labelOfNd (Nd(f, _, _)) = F.labelOf f
    fun holderOfNd (Nd(_, _, h)) = h

    structure SCC = GraphSCCFn (
      struct
        type ord_key = cg_node
        fun compare (Nd(f1, _, _), Nd(f2, _, _)) = Atom.compare(F.labelOf f1, F.labelOf f2)
      end)

    datatype component = datatype SCC.component

    type cg = {
            nodes : cg_node list,       (* the CG nodes in program order *)
            ndOfLab : VCode.label -> cg_node,
            topOrder : component list
          }

    fun nodeToString (Nd(f, edges, _)) = concat[
            Atom.toString(F.labelOf f), " --> ",
            String.concatWith " " (List.map (Atom.toString o labelOfNd) (!edges))
          ]

  (* construct a call graph for the program and the topological sort of its strongly-connected
   * components.
   *)
    fun mkCallGraph funcs = let
        (* maps labels to nodes *)
          val nodeTbl = let
                val tbl = ATbl.mkTable (List.length funcs, Fail "nodeTbl")
                val insert = ATbl.insert tbl
                in
                  List.app (fn f => insert(F.labelOf f, mkNode f)) funcs;
                  tbl
                end
(*
          val ndOfLab = ATbl.lookup nodeTbl
*)
fun ndOfLab lab = (case ATbl.find nodeTbl lab
       of SOME nd => nd
	| NONE => raise Fail(concat["ndOfLab(", Atom.toString lab, ") not found"])
      (* end case *))
        (* list of nodes *)
          val nodes = List.map (fn f => ndOfLab(F.labelOf f)) funcs
        (* build call graph and compute initial purity info *)
          fun addEdges (Nd(f, edges, h)) = let
		fun doCall lab = if List.exists (fn nd => Atom.same(lab, labelOfNd nd)) (!edges)
		      then ()
		      else edges := ndOfLab lab :: !edges
		in
		  F.appCalls doCall f
                end
        (* construct edge list *)
          val _ = List.app addEdges nodes
        (* compute topological order of call-graph; we assume that the main function is
         * the first function.
         *)
          val topOrder = SCC.topOrder' {roots = nodes, follow = edgesOfNd}
          in
            {nodes = nodes, ndOfLab = ndOfLab, topOrder = topOrder}
          end

    fun newProp init = PropList.newProp (holderOfNd, init)
    fun newFlag () = PropList.newFlag holderOfNd

  end
