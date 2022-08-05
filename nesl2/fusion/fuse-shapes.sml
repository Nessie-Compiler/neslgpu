(* fuse-shapes.sml
 *
 * COPYRIGHT (c) 2014 Nora Sandler (nlsandler@cs.uchicago.edu)
 * All rights reserved.
 *
 * Shape information for FuseAST variables.
 *)

structure FuseShapes =
  struct

  structure F = FuseAST

  (* We want every segment descriptor shape to have a unique ID.
   * Because uniform segment descriptors have TWO parameters, and you can't
   * uniquely identify them by just one, we use a pair of stampss as the ID.
   * for shapes SINGLE_SEG and SEG, the second stamp will just be zero.
   * This is a pretty ugly hack; is there a better way to do it?
   *)
  type id = Stamp.stamp * Stamp.stamp

  fun compareIDs (id1, id2) = let
    val (id1_1, id1_2) = id1
    val (id2_1, id2_2) = id2
  in
    (case Stamp.compare(id1_1, id2_1)
      of EQUAL => Stamp.compare(id1_2, id2_2)
       | ord => ord
    (* end case *))
  end

  fun idToString (id_1, id_2) = String.concat["{", Stamp.toString id_1, ",", Stamp.toString id_2, "}"]

  datatype param = Known of F.atom
		 | Symbolic of Stamp.stamp (* An id used to relate known equal values *)
			       
  datatype shape = SINGLE_SEG of id * param (* value *)
		 | SEG of id
		 | VEC of param (* vector length *)
		 | UNIFORM_SEG of id * param * param (* id, length, value *)
		 | UNIFORM_VEC of param * param (* length, value *)
		 | TUPLE of shape list
		 | SCALAR

  val dummyId = Stamp.new()
  fun mkSingletonVec(atm) = VEC(Known atm)
(*
  (* Copy a shape, preserving length but not value information. *)
  fun cloneShape (s as VEC _) = s
    | cloneShape (UNIFORM_VEC(p1, p2)) = VEC p1
    | cloneShape (s as SEG _) = s
    | cloneShape (s as UNIFORM_SEG) = s
    | cloneShape (s as SINGLE_SEG) = s
    | cloneShape (TUPLE shapes) = TUPLE (map cloneShape shapes)
*)
  fun getShapeId (SINGLE_SEG(id, _)) = id
    | getShapeId (SEG id) = id
    | getShapeId (UNIFORM_SEG(id, _, _)) = id
    | getShapeId _ = raise Fail "Not a segment descriptor!"
					   
  fun compareShapes(s1, s2) = compareIDs(getShapeId s1, getShapeId s2)

  structure Map = RedBlackMapFn (
    struct
      type ord_key = shape
      val compare = compareShapes
    end)

end
