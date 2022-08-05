/* Arithmetic functions, min and max, only defined on z, f, d */
twofun(max_wuz, max, int, int)
twofun(max_wud, max, float, float)
twofun(min_wuz, min, int, int)
twofun(min_wud, min, float, float)
twofun(add_wuz, plus, int, int)
twofun(add_wud, plus, float, float)
twofun(sub_wuz, minus, int, int)
twofun(sub_wud, minus, float, float)
twofun(mul_wuz, times, int, int)
twofun(mul_wud, times, float, float)
twofun(div_wuz, divide, int, int)
twofun(div_wud, divide, float, float)

twofun(grt_wuz, gt, int, cvl_bool)
twofun(grt_wud, gt, float, cvl_bool)
twofun(les_wuz, lt, int, cvl_bool)
twofun(les_wud, lt, float, cvl_bool)
twofun(geq_wuz, geq, int, cvl_bool)
twofun(geq_wud, geq, float, cvl_bool)
twofun(leq_wuz, leq, int, cvl_bool)
twofun(leq_wud, leq, float, cvl_bool)


/* shifts, mod and random only on integers */
twofun(lsh_wuz, lshift, int, int)
twofun(rsh_wuz, rshift, int, int)
twofun(mod_wuz, mod, int, int)

/* comparison functions: valid on all input types and returns a cvl_bool */
twofun(eql_wub, eq, cvl_bool, cvl_bool)
twofun(eql_wuz, eq, int, cvl_bool)
twofun(eql_wud, eq, float, cvl_bool)
twofun(neq_wub, neq, cvl_bool, cvl_bool)
twofun(neq_wuz, neq, int, cvl_bool)
twofun(neq_wud, neq, float, cvl_bool)

/* selection functions */
selfun(sel_wuz, select, int) 
selfun(sel_wub, select, cvl_bool) 
selfun(sel_wud, select, float) 

/* logical functions */
onefun(not_wub, not, cvl_bool, cvl_bool)
twofun(xor_wub, xor, cvl_bool, cvl_bool)

twofun(ior_wub, or, cvl_bool, cvl_bool)    /* be careful!: && and || short circuit */
twofun(and_wub, and, cvl_bool, cvl_bool)

/* bitwise functions */
twofun(ior_wuz, bor, int, int)
twofun(and_wuz, band, int, int)
onefun(not_wuz, bnot, int, int)
twofun(xor_wuz, xor, int, int)

/* conversion routines */
onefun(int_wud, d_to_z, float, int)
onefun(int_wub, b_to_z, cvl_bool, int)
onefun(dbl_wuz, z_to_d, int, float)
onefun(boo_wuz, z_to_b, int, cvl_bool)

/* float routines */
onefun(flr_wud, cvl_floor, float, int)
onefun(cei_wud, cvl_ceil, float, int)
onefun(trn_wud, d_to_z, float, int)
onefun(rou_wud, cvl_round, float, int)
onefun(exp_wud, exp, float, float)
onefun(log_wud, log, float, float)
onefun(sqt_wud, sqrt, float, float)
onefun(sin_wud, sin, float, float)
onefun(cos_wud, cos, float, float)
onefun(tan_wud, tan, float, float)
onefun(asn_wud, asin, float, float)
onefun(acs_wud, acos, float, float)
onefun(atn_wud, atan, float, float)
onefun(snh_wud, sinh, float, float)
onefun(csh_wud, cosh, float, float)
onefun(tnh_wud, tanh, float, float)

onefun(cpy_wuz, ident, int, int)
onefun(cpy_wub, ident, cvl_bool, cvl_bool)
onefun(cpy_wud, ident, float, float)
