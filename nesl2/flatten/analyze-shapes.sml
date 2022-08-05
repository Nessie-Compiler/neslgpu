(* analyze-shapes.sml
 * 
 * COPYRIGHT (c) Nora Sandler (nlsandler@uchicago.edu)
 * All rights reserved.
 *)

structure AnalyzeShapes : sig

  val analyze : Flan.program * FlanEnv.env -> unit

end = struct

  structure F = Flan
  structure T = FlanTypes
  structure S = Shapes

  fun expShape (env : FlanEnv.env) e = 
      let val eShape = expShape env
	  val bShapes = bindShapes env
      in
	(case e 
	  of F.ExpLet(binds, e') => bShapes (binds, e')
(*
let
	       val sub = bShapes (sub, binds)
	     in
	       eShape (sub, e')
	     end
*)
	   | F.ExpTuple(v, atms, e') => let
	       val _ = S.setShape(v, S.TUPLE(map S.getAtmShape atms));
	     in
	       eShape e'
	     end
	   | F.ExpSeq(v, atms, e') => let

	       val shape = S.mkShapeWithLen(FlanVar.typeOf v, List.length atms)
	       val _ = S.setShape(v, shape)
(*
	       val vecLen = S.newParam(S.Known(F.Int(IntInf.fromInt(List.length atms))))
	       val S.TUPLE(seg::shps) = S.getShape v
	       val seg' = S.VEC(S.one, vecLen)
	       val (sub, seg') = S.unifySegdes(sub, seg, seg')
	       val _ = S.setShape(v, S.TUPLE(seg'::shps))
	       val (sub, _) = S.constrainVec(sub, v)
	       val (sub, shape) = eShape(sub, e')
*)
	     in
	       eShape e'
	     end
	   | F.ExpPure(FlanPure.Base(Pure.PROJ i), [atm]) => let
	       val (S.TUPLE shapes) = S.getAtmShape atm
	     in
	       List.nth(shapes, (i - 1))
	     end
	   (* Special-case LENGTHS and MAKE_SEGDES and lifted DIST and INDEX, since segdes is identical to one of the arguments. *)
	   | F.ExpPure(FlanPure.Base(Pure.MAKE_SEGDES), [arg]) => S.getAtmShape arg
	   | F.ExpPure(FlanPure.Base(Pure.LENGTHS), [arg]) => S.getAtmShape arg
	   | F.ExpPure(FlanPure.Lifted(Pure.DIST ty), [vals, lens]) => let
	       val valShape = S.getAtmShape vals
	       val lensShape = S.getAtmShape lens
	       (* valShape and lensShape are flat and of the same length *)
	       val flatShape = S.mkFlatShape()
	       val flatShape2 = S.cloneVector flatShape
	       val _ = (S.setShapesEqual(valShape, flatShape); 
			S.setShapesEqual(lensShape, flatShape2))
	       val S.TUPLE[outerSeg, innerSeg] = lensShape
	     in
	       S.TUPLE[outerSeg, S.TUPLE[innerSeg, S.VEC(S.scalar(), S.fresh())]]
	     end
	   | F.ExpPure(FlanPure.Lifted(Pure.INDEX), [starts, strides, lens]) => let
	       val startShape = S.getAtmShape starts
	       val strideShape = S.getAtmShape strides
	       val lensShape = S.getAtmShape lens
	   (* All arguments are flat and of the same length *)
	       val flatShape = S.mkFlatShape()
	       val flatShape2 = S.cloneVector flatShape
	       val flatShape3 = S.cloneVector flatShape
	       val _ = (S.setShapesEqual(startShape,flatShape);
			S.setShapesEqual(strideShape, flatShape2);
			S.setShapesEqual(lensShape, flatShape3))
	       val S.TUPLE[outerSeg, innerSeg] = lensShape
	     in
	       S.TUPLE[outerSeg, S.TUPLE[innerSeg, S.VEC(S.scalar(), S.fresh())]]
	     end
	   | F.ExpPure(p, atms) => let
	       val atmsShape = map S.getAtmShape atms
	       val (domShape, rngShape) = S.pureShape p
	     in
	       (ListPair.map S.setShapesEqual (domShape, atmsShape);
		rngShape)
	     end
	   | F.ExpCmd(c, atms) => S.cmdShape c
	   | F.ExpIf (_, e1, e2, _) => let
	       val shape1 = eShape e1
	       val shape2 = eShape e2
	     in
	       S.mergeShapes(shape1, shape2)
	     end
	   | F.ExpApplyFun (f, atms) => let
	       val _ = if not(S.isAnalyzed f)
		       then analyzeInst env 
					(Option.valOf 
					   (FlanEnv.lookupInstance(env, 
								   FlanFunct.getFunct f,
								   f)))
		       else ()
	       val atmsShape = map S.getAtmShape atms
	       val (domShape, rngShape) = S.copyFunShape f
	     in
	       (ListPair.map S.setShapesEqual (domShape, atmsShape);
		rngShape)
	     end
	   | F.ExpAtom a => S.getAtmShape a
	   | F.ExpLifted _ => raise Fail "FIXME"
	(* end case *))
      end

  and bindShapes env ([], exp) = expShape env exp
    | bindShapes env ((v, e)::bs, exp) = let
	val shape = expShape env e
	val _ = S.setShape(v, shape)
      in
	bindShapes env (bs, exp)
      end

  and analyzeInst env (f, args, e) = 
      if S.isAnalyzed f
      then ()
      else
	let
	  (* We mark it analyzed before actually analyzing function bodies
	   * so we don't follow recursive calls *)
	  val _ = (List.app (fn arg => 
				(S.setShape(arg, 
					    S.mkShape(FlanVar.typeOf arg))))
			    args)
	  val _ = S.setAnalyzed(f, true);
	  val shape = expShape env e
	in
	  (* We need to unify shape parameters in this instance in order to get useful function shape *)
	  (unifyInst (S.Map.empty, (f, args, e));
	   S.setFunShape(f, (map (fn a => S.updateShape (S.getShape a)) args, 
			     S.updateShape shape)))
	end

  and unifyInst (pMap, (f, args, e)) = let
(*    val _ = print (String.concat["unifyInst: ", FlanFunct.toString f, "\n"]) *)
    val pMap = List.foldl (fn (v, m) => S.unifyVar(m, v)) pMap args
  in
    unifyExp (pMap, e)
  end

  and unifyExp (pMap, exp) = 
      (case exp
	of F.ExpLet(binds, e') => let
	     fun unifyBind ((v, e), m) = let
	       val m' = S.unifyVar(m, v)
	     in
	       unifyExp(m', e)
	     end
	     val pMap' = List.foldl unifyBind pMap binds
	   in
	     unifyExp(pMap', e')
	   end
	 | F.ExpTuple(v, _, e') => let
	     val pMap' = S.unifyVar(pMap, v)
	   in
	     unifyExp(pMap', e')
	   end
(*(S.unifyVar v; unifyExp e')*)
	 | F.ExpSeq(v, _, e') =>  let
	     val pMap' = S.unifyVar(pMap, v)
	   in
	     unifyExp(pMap', e')
	   end
	 | F.ExpIf(_, e1, e2, _) => let
	     val pMap' = unifyExp(pMap, e1)
	     val pMap' = unifyExp(pMap', e2)
	   in
	     pMap'
	   end
	 | F.ExpLifted _ => raise Fail "Fixme: ExpLifted"
	 | _ => pMap
      (* end case *))

  fun updateExp e =
      (case e 
	of F.ExpLet(binds, e') => (map updateBind binds;
				   updateExp e')
	 | F.ExpTuple(v, atms, e') => (S.updateVar v;
				       updateExp e')
	 | F.ExpSeq(v, atms, e') => (S.updateVar v;
				     updateExp  e')
	 | F.ExpIf (_, e1, e2, _) => (updateExp e1;
				      updateExp e2)
	 | _ => ()
      (* end case *))

  and updateBind (v, e) = (S.updateVar v; updateExp e)

(*
  and bindShapes env (sub, []) = sub
    | bindShapes env (sub, (v, e)::binds) = let
	val (sub, shape) = expShape env (sub, e)
      in
	(S.setShape(v, shape);
	 (bindShapes env (sub, binds)))
      end
*)

  fun analyze (F.Program tops, env) = let
    fun analyzeTop (F.TopFun (f, insts)) = 
	List.app (fn i => analyzeInst env i) insts
      | analyzeTop (F.TopBind (v, e)) =
	S.setShape(v, expShape env e)
      | analyzeTop (top as F.TopExp(e,t,p)) = let
	  val shp = expShape env e
	in
	  S.setExpShape(top, shp)
	end
(*	(expShape env e; ()) *)
    fun unifyTops (pMap, []) = ()
      | unifyTops (pMap, F.TopFun(f, insts)::tops) = let
	  fun unifyInsts (pm, []) = pm
	    | unifyInsts (pm, i::is) = let
		val pm' = unifyInst(pm, i)
	      in
		unifyInsts(pm', is)
	      end
	  val pMap' = unifyInsts (pMap, insts)
	in
	  unifyTops(pMap', tops)
	end
(*	List.app unifyInst insts *)
      | unifyTops (pMap, F.TopBind(v, e)::tops) = let
	  val pMap' = S.unifyVar(pMap, v)
	  val pMap' = unifyExp(pMap', e)
	in
	  unifyTops(pMap', tops)
	end
(*	(S.unifyVar v; unifyExp e) *)
      | unifyTops (pMap, (top as F.TopExp(e, _, _))::tops) = let
	  val pMap' = S.unifyShape(pMap, S.getExpShape top)
	  val pMap' = unifyExp(pMap', e)
	in
	  unifyTops(pMap', tops)
	end
    fun updateTop (F.TopFun (f, insts)) = let
      fun updateInst(f, args, e) = (S.updateFun f;
				    List.app (fn v => S.updateVar v) args;
				    updateExp e)
    in
      List.app updateInst insts
    end
      | updateTop (F.TopBind(v, e)) = 
	(S.updateVar v; updateExp e)
      | updateTop (top as F.TopExp(e, t, p)) = (S.updateShape(S.getExpShape top);
						updateExp e)
  in
    ((*print "analyzing tops\n"; *)
     List.app analyzeTop tops;
(*     print "unifying tops\n"; *)
     unifyTops (S.Map.empty, tops);
(*     print "updating tops\n"; *)
     List.app updateTop tops)
  end


end
