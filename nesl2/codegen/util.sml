(* util.sml
 *
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Some code geration utility code.
 *)

structure Util =
  struct
    local
      structure Cu = CuLambda
      structure GPU = Cu.GPU
      structure CL = CLang
    in

  (* assuming 32-bit ints and floats *)
    val floatTy = CL.floatTy
    val intTy = CL.intTy
    fun mathFn f = f ^ "f"

  (* convert a GPU type to a CUDA type *)
    fun gpuTypeToCUDA ty = (case ty
	   of GPU.TyBool => CL.boolTy
	    | GPU.TyChar => CL.charTy
	    | GPU.TyInt => intTy
	    | GPU.TyFloat => floatTy
	  (* end case *))

  (* map an atom to a CLang expression; the genVar function is used for variables *)
    fun genAtom genVar atm = (case atm
	   of (Cu.Var x) => genVar x
	    | (Cu.Bool b) => CL.mkBool b
	    | (Cu.Char c) => CL.mkChar c
	    | (Cu.Int n) => CL.mkInt n
	    | (Cu.Float f) => CL.mkFlt f
	  (* end case *))

  (* normalize a boolean value; double negation ensures that
   * the value is normalized to 0 or 1.
   *)
    fun normalizeBool a = CL.mkUnOp(CL.%!, CL.mkUnOp(CL.%!, a))

  (* translate a CuLambda scalar operator to a CLang expression *)
    fun genScalarOp (rator, args) = (case (rator, args)
	   of (Cu.ADD _, [a, b]) => CL.mkBinOp(a, CL.#+, b)
	    | (Cu.SUB _, [a, b]) => CL.mkBinOp(a, CL.#-, b)
	    | (Cu.MUL _, [a, b]) => CL.mkBinOp(a, CL.#*, b)
	    | (Cu.DIV _, [a, b]) => CL.mkBinOp(a, CL.#/, b)
	    | (Cu.MOD, [a, b]) => CL.mkBinOp(a, CL.#%, b)
	    | (Cu.LT _, [a, b]) => CL.mkBinOp(a, CL.#<, b)
	    | (Cu.LTE _, [a, b]) => CL.mkBinOp(a, CL.#<=, b)
	    | (Cu.GT _, [a, b]) => CL.mkBinOp(a, CL.#>, b)
	    | (Cu.GTE _, [a, b]) => CL.mkBinOp(a, CL.#>=, b)
	    | (Cu.EQ _, [a, b]) => CL.mkBinOp(a, CL.#==, b)
	    | (Cu.NEQ _, [a, b]) => CL.mkBinOp(a, CL.#!=, b)
	    | (Cu.LSHIFT, [a, b]) => CL.mkBinOp(a, CL.#<<, b)
	    | (Cu.RSHIFT, [a, b]) => CL.mkBinOp(a, CL.#>>, b)
	    | (Cu.NOT TypeBase.BOOL, [a]) => CL.mkUnOp(CL.%!, a)
	    | (Cu.NOT TypeBase.INT, [a]) => CL.mkUnOp(CL.%~, a)
	    | (Cu.AND TypeBase.BOOL, [a, b]) => CL.mkBinOp(a, CL.#&&, b)
	    | (Cu.AND TypeBase.INT, [a, b]) => CL.mkBinOp(a, CL.#&, b)
	    | (Cu.OR TypeBase.BOOL, [a, b]) => CL.mkBinOp(a, CL.#||, b)
	    | (Cu.OR TypeBase.INT, [a, b]) => CL.mkBinOp(a, CL.#|, b)
	    | (Cu.XOR TypeBase.BOOL, [a, b]) =>
	      (* we implement boolean xor by (!!a ^ !!b) *)
		CL.mkBinOp(normalizeBool a, CL.#^, normalizeBool b)
	    | (Cu.XOR TypeBase.INT, [a, b]) => CL.mkBinOp(a, CL.#^, b)
	    | (Cu.SELECT _, [a, b, c]) => CL.mkCond (a, b, c)
	    | (Cu.RAND, [i]) => (* ((blockIdx.x * 83355 + 122407) % i) *)
		CL.mkBinOp(
		  CL.mkBinOp(
		    CL.mkBinOp(CL.mkSelect(CL.mkVar "blockIdx", "x"), CL.#*, CL.mkInt 83355),
		    CL.#+,
		    CL.mkInt 122407),
		  CL.#%,
		  i)
	    | (Cu.FLOOR, ys) => CL.mkCast(intTy, CL.mkApply(mathFn "floor", ys))
	    | (Cu.CEIL, ys) => CL.mkCast(intTy, CL.mkApply(mathFn "ceil", ys))
	    | (Cu.TRUNC, ys) => CL.mkCast(intTy, CL.mkApply(mathFn "trunc", ys))
	    | (Cu.ROUND, ys) => CL.mkCast(intTy, CL.mkApply(mathFn "round", ys))
	    | (Cu.I_TO_F, [a]) => CL.mkCast(floatTy, a)
	    | (Cu.I_TO_B, [a]) => CL.mkCast(CL.boolTy, CL.mkUnOp(CL.%!, CL.mkUnOp(CL.%!, a)))
	    | (Cu.B_TO_I, [a]) => CL.mkCast(intTy, a)
	    | (Cu.LOG, ys) => CL.mkApply(mathFn "log", ys)
	    | (Cu.SQRT, ys) => CL.mkApply(mathFn "sqrt", ys)
	    | (Cu.EXP, ys) => CL.mkApply(mathFn "exp", ys)
	    | (Cu.SIN, ys) => CL.mkApply(mathFn "sin", ys)
	    | (Cu.COS, ys) => CL.mkApply(mathFn "cos", ys)
	    | (Cu.TAN, ys) => CL.mkApply(mathFn "tan", ys)
	    | (Cu.ASIN, ys) => CL.mkApply(mathFn "asin", ys)
	    | (Cu.ACOS, ys) => CL.mkApply(mathFn "acos", ys)
	    | (Cu.ATAN, ys) => CL.mkApply(mathFn "atan", ys)
	    | (Cu.SINH, ys) => CL.mkApply(mathFn "sinh", ys)
	    | (Cu.COSH, ys) => CL.mkApply(mathFn "cosh", ys)
	    | (Cu.TANH, ys) => CL.mkApply(mathFn "tamh", ys)
	    | (Cu.I_TO_C, [y]) => CL.mkCast(CL.charTy, y)
	    | (Cu.C_TO_I, [y]) => CL.mkCast(intTy, y)
	  (* end case *))

    end (* lcoal *)
  end
