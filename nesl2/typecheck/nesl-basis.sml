(* nesl-basis.sml
 *
 * COPYRIGHT (c) 2012 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 *)

structure NeslBasis =
  struct

    type basis = {
	(* Equivalents of pure ops *)
          add : Funct.funct,            (* ADD : (a, a) -> a :: a in number *)
	  sub : Funct.funct,            (* SUB : (a, a) -> a :: a in number *)
	  mul : Funct.funct,            (* MUL : (a, a) -> a :: a in number *)
	  div_internal : Funct.funct,   (* DIV : (a, a) -> a :: a in number *)
          rem_internal : Funct.funct,   (* MOD : (int, int) -> int *)
          lt : Funct.funct,             (* LT  : (a, a) -> bool :: a in ordinal *)
          lte : Funct.funct,            (* LTE : (a, a) -> bool :: a in ordinal *)
          gt : Funct.funct,             (* GT  : (a, a) -> bool :: a in ordinal *)
          gte : Funct.funct,            (* GTE : (a, a) -> bool :: a in ordinal *)
          eq : Funct.funct,             (* EQ  : (a, a) -> bool :: a in ordinal *)
          neq : Funct.funct,            (* NEQ : (a, a) -> bool :: a in ordinal *)
	  lshift : Funct.funct,         (* LSHIFT : (int, int) -> int *)
	  rshift : Funct.funct,         (* RSHIFT : (int, int) -> int *)
          not : Funct.funct,            (* NOT : (a, a) -> a :: a in logical *)
          and' : Funct.funct,            (* AND : (a, a) -> a :: a in logical *)
          or : Funct.funct,             (* OR  : (a, a) -> a :: a in logical *)
          xor : Funct.funct,            (* XOR : (a, a) -> a :: a in logical *)
          select_scalar : Funct.funct,  (* SELECT : (bool, a, a) -> a :: a in any *)
          prim_rand : Funct.funct,      (* RAND : int -> int *)
          floor : Funct.funct,          (* FLOOR : float -> int *)
          ceil : Funct.funct,           (* CEIL : float -> int *)
          trunc : Funct.funct,          (* TRUNC : float -> int *)
          round : Funct.funct,          (* ROUND : float -> int *)           
          float : Funct.funct,          (* I_TO_F : int -> float *)
          btoi : Funct.funct,           (* B_TO_I : bool -> int *)
          prim_ln : Funct.funct,        (* LOG : float -> float *)
          prim_sqrt : Funct.funct,      (* SQRT : float -> float *)
          exp : Funct.funct,            (* EXP : float -> float *)
          sin : Funct.funct,            (* SIN : float -> float *)
          cos : Funct.funct,            (* COS : float -> float *)
          tan : Funct.funct,            (* TAN : float -> float *)
          asin : Funct.funct,           (* ASIN : float -> float *)
          acos : Funct.funct,           (* ACOS : float -> float *)
          atan : Funct.funct,           (* ATAN : float -> float *)
          sinh : Funct.funct,           (* SINH : float -> float *)
          cosh : Funct.funct,           (* COSH : float -> float *)
          tanh : Funct.funct,           (* TANH : float -> float *)
          code_char : Funct.funct,      (* I_TO_C : int -> char *)
          char_code : Funct.funct,      (* C_TO_I : char -> int *)
          plus_scan : Funct.funct,      (* ADD_SCAN : [a] -> [a] :: a in number *)
          mult_scan : Funct.funct,      (* MUL_SCAN : [a] -> [a] :: a in number *)
          max_scan  : Funct.funct,      (* MAX_SCAN : [a] -> [a] :: a in ordinal *)
          min_scan : Funct.funct,       (* MIN_SCAN : [a] -> [a] :: a in ordinal *)
          and_scan : Funct.funct,       (* AND_SCAN : [a] -> [a] :: a in logical *)
          or_scan : Funct.funct,        (* OR_SCAN : [a] -> [a] :: a in logical *)
          xor_scan : Funct.funct,       (* XOR_SCAN : [a] -> [a] :: a in logical *)
          sum : Funct.funct,            (* ADD_REDUCE : [a] -> a :: a in number *)
          product : Funct.funct,        (* MUL_REDUCE : [a] -> a :: a in number *)
          max_val : Funct.funct,        (* MAX_REDUCE : [a] -> a :: a in ordinal *)
          min_val : Funct.funct,        (* MIN_REDUCE : [a] -> a :: a in ordinal *)
          all : Funct.funct,            (* AND_REDUCE : [a] -> a :: a in logical *)
          any : Funct.funct,            (* OR_REDUCE  : [a] -> a ::  a in logical *)
          parity : Funct.funct,         (* XOR_REDUCE : [a] -> a :: a in logical *)
          prim_permute_scalar : Funct.funct, (* PERMUTE : ([:a:], [:int:], segdes) -> [:a:] :: a in any *)
	  prim_put_scalar : Funct.funct,     (* DPERMUTE : ([:a:], [:int:], [:a:], segdes, segdes -> [:a:] :: a in any *)
	  prim_fpermute : Funct.funct,       (* FPERMUTE : ([a], [int], [bool], segdes, segdes) -> [a] :: a in any *)
          prim_get_scalar : Funct.funct,     (* BPERMUTE : ([:a:], [:int:], segdes, segdes) -> [:a:] :: a in any *)
          prim_extract : Funct.funct,        (* EXTRACT : ([:a:], [:int:], segdes -> [:a:] :: a in any *)
          prim_rep_scalar : Funct.funct,     (* REPLACE : ([:a:], [:int:], [:a:], segdes) -> a :: a in any *)
	  old_pack_scalar : Funct.funct,     (* PACK : ([a], [bool]) -> [a] :: a in any *)
          prim_dist_scalar: Funct.funct,     (* DIST : (a, segdes) -> [:a:] :: a in any *)
          prim_iseq : Funct.funct,           (* INDEX : (int, int, segdes) -> [:int:] *)
          prim_length : Funct.funct,         (* LENGTH : [:a:] -> int :: a in any *)
          make_segdes : Funct.funct,         (* MAKE_SEGDES : int -> segdes *)
	  prim_seg_lengths : Funct.funct,    (* LENGTHS : segdes -> int *)
          prim_scalar_to_seq : Funct.funct,  (* SCALAR_TO_SEQ : a -> [:a:] :: a in any *)
	(* library functions *)
          dist : Funct.funct,		(* (a, int) -> [a] :: a in any *)
          length : Funct.funct,         (* [a] -> int :: a in any *)
	  length_lifted : Funct.funct, 
          zip : Funct.funct,            (* ([a], [b]) -> [(a, b)] :: a in any, b in any *)
	  zip_lifted: Funct.funct,       
	  unzip : Funct.funct,
	  unzip_lifted : Funct.funct, 
	  partition : Funct.funct,	(* ([a], [int]) -> [[a]] :: a in any *)
	  partition_lifted: Funct.funct,(* [([a], [int]) -> [[a]] :: a in any *) 
	  pack : Funct.funct,		(* [(a, bool)] -> [a] :: a in any *)
	  old_pack : Funct.funct,	(* ([a], [bool]) -> [a] :: a in any *)
	  flatten : Funct.funct,        (* [[a]] -> [a] :: a in any *)
	  flatten_lifted: Funct.funct,  (* [[[a]]] -> [[a]] *) 
          put : Funct.funct,            (* ([a], [int], [a]) -> [a] :: a in any *)
          flag_merge : Funct.funct,	(* ([bool], [a], [a]) -> [a] :: a in any *)
          iseq : Funct.funct,           (* (int, int, int) -> [int] *)
	  concat : Funct.funct,         (* ([a], [a]) -> [a] :: a in any *)
	  start_timer : Funct.funct,	(* int -> int *)
	  stop_timer : Funct.funct	(* int -> float *)
	}

    fun mkBasis env = let
	  fun lookupFunct id = (case Env.findVarBind (env, Atom.atom id)
		 of SOME(Env.Fun f) => f
		  | _ => raise Fail("lookupFunct "^id)
		(* end case *))
	  in {
      (* pure op equivalents *)
            add = lookupFunct "+",
            sub = lookupFunct "-",
            mul = lookupFunct "*",
            div_internal = lookupFunct "div_internal",
            rem_internal = lookupFunct "rem_internal",
            lt = lookupFunct "<",
            lte = lookupFunct "<=",
            gt = lookupFunct ">",
            gte = lookupFunct ">=",
            eq = lookupFunct "=",
            neq = lookupFunct "/=",
            lshift = lookupFunct "lshift",
            rshift = lookupFunct "rshift",
            not = lookupFunct "not",
            and' = lookupFunct "and",
            or = lookupFunct "or",
            xor = lookupFunct "xor",
            select_scalar = lookupFunct "select_scalar",
            prim_rand = lookupFunct "prim_rand",
            floor = lookupFunct "floor",
            ceil = lookupFunct "ceil",
	    trunc = lookupFunct "trunc",
            round = lookupFunct "round",
            float = lookupFunct "float",
            btoi = lookupFunct "btoi",
            prim_ln = lookupFunct "prim_ln",
            prim_sqrt = lookupFunct "prim_sqrt",
            exp = lookupFunct "exp",
            sin = lookupFunct "sin",
            cos = lookupFunct "cos",
	    tan = lookupFunct "tan",
            asin = lookupFunct "asin",
            acos = lookupFunct "acos",
            atan = lookupFunct "atan",
            sinh = lookupFunct "sinh",
            cosh = lookupFunct "cosh",
            tanh = lookupFunct "tanh",
            code_char = lookupFunct "code_char",
            char_code = lookupFunct "char_code",
            plus_scan = lookupFunct "plus_scan",
            mult_scan = lookupFunct "mult_scan",
            max_scan = lookupFunct "max_scan",
            min_scan = lookupFunct "min_scan",
            and_scan = lookupFunct "and_scan",            
            or_scan = lookupFunct "or_scan",
            xor_scan = lookupFunct "xor_scan",
            sum = lookupFunct "sum",
            product = lookupFunct "product", 
            max_val = lookupFunct "max_val",
            min_val = lookupFunct "min_val",
            all = lookupFunct "all", 
            any = lookupFunct "any",
            parity = lookupFunct "parity",
            prim_permute_scalar = lookupFunct "prim_permute_scalar",
            prim_put_scalar = lookupFunct "prim_put_scalar",
	    prim_fpermute = lookupFunct "prim_fpermute",
            prim_get_scalar = lookupFunct "prim_get_scalar",
            prim_extract = lookupFunct "prim_extract",
            prim_rep_scalar = lookupFunct "prim_rep_scalar",
	    old_pack_scalar = lookupFunct "old_pack_scalar",
            prim_dist_scalar = lookupFunct "prim_dist_scalar",
            prim_iseq = lookupFunct "prim_iseq",
            prim_length = lookupFunct "prim_length",
            make_segdes = lookupFunct "make_segdes",
	    prim_seg_lengths = lookupFunct "prim_seg_lengths",
            prim_scalar_to_seq = lookupFunct "prim_scalar_to_seq",
	    dist = lookupFunct "dist",
	    length = lookupFunct "length",
	    length_lifted = lookupFunct "length_lifted",
	    zip = lookupFunct "zip",
	    zip_lifted = lookupFunct "zip_lifted",
	    unzip = lookupFunct "unzip",
	    unzip_lifted = lookupFunct "unzip_lifted",
	    partition = lookupFunct "partition",
	    partition_lifted = lookupFunct "partition_lifted",
	    pack = lookupFunct "pack",
	    flatten = lookupFunct "flatten",
	    flatten_lifted = lookupFunct "flatten_lifted",
	    put = lookupFunct "put",
	    flag_merge = lookupFunct "flag_merge",
	    iseq = lookupFunct "iseq",
	    concat = lookupFunct "++",
	    old_pack = lookupFunct "old_pack",
	    start_timer = lookupFunct "start_timer",
	    stop_timer = lookupFunct "stop_timer"
	  } end

    fun pureToFunct bsis pureOp = let
      val {add, sub, mul, div_internal, rem_internal, lt, lte, gt, gte, eq, neq, lshift,
	   rshift, not, and', or, xor, select_scalar, prim_rand, floor, ceil, trunc,
	   round, float, btoi, prim_ln, prim_sqrt, exp, sin, cos, tan, asin, acos, atan,
	   sinh, cosh, tanh, code_char, char_code, plus_scan, mult_scan, max_scan, 
	   min_scan, and_scan, or_scan, xor_scan, sum, product, max_val, min_val, all,
	   any, parity, prim_permute_scalar, prim_put_scalar, prim_fpermute,
	   prim_get_scalar, prim_extract, prim_rep_scalar, old_pack_scalar, 
	   prim_dist_scalar, prim_iseq, prim_length, make_segdes, prim_seg_lengths,
	   prim_scalar_to_seq, ...} : basis = bsis
    in
	(case pureOp
	  of Pure.ADD _ => add
	   | Pure.SUB _ => sub
	   | Pure.MUL _ => mul
	   | Pure.DIV _ => div_internal
	   | Pure.MOD => rem_internal
	   | Pure.LT _ => lt
	   | Pure.LTE _ => lte
	   | Pure.GT _ => gt
	   | Pure.GTE _ => gte
	   | Pure.EQ _ => eq
	   | Pure.NEQ _ => neq
	   | Pure.LSHIFT => lshift
	   | Pure.RSHIFT => rshift
	   | Pure.NOT _ => not
	   | Pure.AND _ => and'
	   | Pure.OR _ => or
	   | Pure.XOR _ => xor
	   | Pure.SELECT _ => select_scalar
	   | Pure.RAND => prim_rand
	   | Pure.FLOOR => floor
	   | Pure.CEIL => ceil
	   | Pure.TRUNC => trunc
	   | Pure.ROUND => round
	   | Pure.I_TO_F => float
	   | Pure.B_TO_I => btoi
	   | Pure.LOG => prim_ln
	   | Pure.SQRT => prim_sqrt
	   | Pure.EXP => exp
	   | Pure.SIN => sin
	   | Pure.COS => cos
	   | Pure.TAN => tan
	   | Pure.ASIN => asin
	   | Pure.ACOS => acos
	   | Pure.ATAN => atan
	   | Pure.SINH => sinh
	   | Pure.COSH => cosh
	   | Pure.TANH => tanh
	   | Pure.I_TO_C => code_char
	   | Pure.C_TO_I => char_code
	   | Pure.ADD_SCAN _ => plus_scan
	   | Pure.MUL_SCAN _ => mult_scan
	   | Pure.MAX_SCAN _ => max_scan
	   | Pure.MIN_SCAN _ => min_scan
	   | Pure.AND_SCAN _ => and_scan
	   | Pure.OR_SCAN _ => or_scan
	   | Pure.XOR_SCAN _ => xor_scan
	   | Pure.ADD_REDUCE _ => sum
	   | Pure.MUL_REDUCE _ => product
	   | Pure.MAX_REDUCE _ => max_val
	   | Pure.MIN_REDUCE _ => min_val
	   | Pure.AND_REDUCE _ => all
	   | Pure.OR_REDUCE _ => any
	   | Pure.XOR_REDUCE _ => parity
	   | Pure.PERMUTE _ => prim_permute_scalar
	   | Pure.DPERMUTE _ => prim_put_scalar
	   | Pure.FPERMUTE _ => prim_fpermute
	   | Pure.BPERMUTE _ => prim_get_scalar
	   | Pure.EXTRACT _ => prim_extract
	   | Pure.REPLACE _ => prim_rep_scalar
	   | Pure.PACK _ => old_pack_scalar
	   | Pure.DIST _ => prim_dist_scalar
	   | Pure.INDEX => prim_iseq
	   | Pure.LENGTH _ => prim_length
	   | Pure.MAKE_SEGDES => make_segdes
	   | Pure.LENGTHS => prim_seg_lengths
	   | Pure.SCALAR_TO_SEQ _ => prim_scalar_to_seq
	   | _ => raise Fail ("unknown pureOp"^(Pure.toString pureOp))
	(* end case *))
    end

  end
