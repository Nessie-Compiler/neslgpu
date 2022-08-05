typedef union maxaligned { int i; float f;} MAXALIGN;
typedef int cvl_bool;

/* TODO: implement random numbers */

#define cvlrand(i)  ((get_global_id(0)*83355+122407)%i)
#define mmax(i,j)	((i) > (j) ? (i) : (j))
#define mmin(i,j)	((i) < (j) ? (i) : (j))
#define maxi(i,j)	((*i) > (*j) ? (j++, *i++) : (i++, *j++))
#define mini(i,j)	((*i) < (*j) ? (j++, *i++) : (i++, *j++))

#define lshift(i,j)	((i) << (j))
#define rshift(i,j)	((i) >> (j))
#define plus(i,j)	((i) + (j))
#define minus(i,j)	((i) - (j))
#define times(i,j)	((i) * (j))
#define divide(i,j)	((i) / (j))
#define mod(i,j)	((i) % (j))
#define gt(i,j)		((i) > (j))
#define lt(i,j)		((i) < (j))
#define eq(i,j)		((i) == (j))
#define neq(i,j)	((i) != (j))
#define leq(i,j)	((i) <= (j))
#define geq(i,j)	((i) >= (j))

#define oor(i,j)		((i) || (j))  		 /* logical or */
#define bor(i,j)	((i) | (j))              /* bitwise or */
#define aand(i,j)	((i) && (j))
#define band(i,j)	((i) & (j))
#define nnot(i)		(! (i))
#define bnot(i)		(~ (i))
#define xxor(i,j)	((i) ^ (j))		/* bitwise xor */
#define lxor(i,j)	((!!(i)) ^ (!!(j)))	/* logical xor */

#define neg(i)		(-(i))
#define ident(i)	(i)
#define notnot(i)	(!!(i))

#define selection(i,j,k)   ((i) ? (j) : (k))
#define d_to_z(x) ((int) (x))
#define b_to_z(x) ((int) (x))
#define z_to_d(x) ((float) (x))
#define z_to_b(x) ((cvl_bool) notnot(x))

#define cvl_round(x) ((int) ((x) + 0.5))

#define cvl_floor(x) ((int) floor(x))
#define cvl_ceil(x) ((int) ceil(x))

#define	MAX_INT	((int)(((unsigned) ~0)>>1))
#define	MIN_INT	(~MAX_INT)

/* arbritrarily choosen, MACHINE DEPENDENT.  Works on Suns, Vaxs and
   RTs. HUGE (from math.h) won't work since some compilers complain it is
   too big, and because it returns Infinity or NaN when printed. */
#define MIN_FLOAT   ((float)-1.0e36)
#define MAX_FLOAT   ((float)1.0e36)

#define MAX_DOUBLE (double)MAX_FLOAT
#define MIN_DOUBLE (double)MIN_FLOAT

#define onefun(_fname, _op, _typ1, _typ2)    \
__kernel void _fname##Kernel( \
    __global MAXALIGN*data, \
    int dst, \
    int src, \
    int len, \
    int scratch) \
{\
    int address = get_global_id(0);           \
    __global _typ2 *pDst = (__global _typ2*)(&data[dst]); \
    __global _typ1 *pSrc = (__global _typ1*)(&data[src]); \
    \
    if (address < len) { \
   pDst[address] = _op(pSrc[address]); \
    } \
}

#define twofun(_fname, _op, _typ1, _typ2)    \
__kernel void _fname##Kernel( \
    __global MAXALIGN*data, \
    int dst, \
    int src1, \
    int src2, \
    int len, \
    int scratch) \
{\
    int address = get_global_id(0);           \
    __global _typ2 *pDst = (__global _typ2*)(&data[dst]); \
    __global _typ1 *pSrc1 = (__global _typ1*)(&data[src1]); \
    __global _typ1 *pSrc2 = (__global _typ1*)(&data[src2]); \
    \
    if (address < len) { \
    pDst[address] = _op(pSrc1[address], pSrc2[address]); \
    } \
}

#define selfun(_fname, _op, _typ)    \
__kernel void _fname##Kernel( \
    __global MAXALIGN *data, \
    int dst, \
    int src1, \
    int src2, \
    int src3, \
    int len, \
    int scratch) \
{\
    int address = get_global_id(0);           \
    __global _typ *pDst = (__global _typ*)(&data[dst]); \
    __global cvl_bool *pSrc1 = (__global cvl_bool*)(&data[src1]); \
    __global _typ *pSrc2 = (__global _typ*)(&data[src2]); \
    __global _typ *pSrc3 = (__global _typ*)(&data[src3]); \
    if (address < len) { \
    pDst[address] = _op(pSrc1[address], pSrc2[address], pSrc3[address]); \
    } \
}

/* Keep in sync with elementwise.h! */
/* Arithmetic functions, min and max, only defined on z, f, d */
twofun(max_wuz, mmax, int, int)
twofun(max_wud, mmax, float, float)
twofun(min_wuz, mmin, int, int)
twofun(min_wud, mmin, float, float)
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
onefun(rnd_wuz, cvlrand, int, int)

/* comparison functions: valid on all input types and returns a cvl_bool */
twofun(eql_wub, eq, cvl_bool, cvl_bool)
twofun(eql_wuz, eq, int, cvl_bool)
twofun(eql_wud, eq, float, cvl_bool)
twofun(neq_wub, neq, cvl_bool, cvl_bool)
twofun(neq_wuz, neq, int, cvl_bool)
twofun(neq_wud, neq, float, cvl_bool)

/* selection functions */
selfun(sel_wuz, selection, int) 
selfun(sel_wub, selection, cvl_bool) 
selfun(sel_wud, selection, float) 

/* logical functions */
onefun(not_wub, nnot, cvl_bool, cvl_bool)
twofun(xor_wub, xxor, cvl_bool, cvl_bool)

twofun(ior_wub, oor, cvl_bool, cvl_bool)    /* be careful!: && and || short circuit */
twofun(and_wub, aand, cvl_bool, cvl_bool)

/* bitwise functions */
twofun(ior_wuz, bor, int, int)
twofun(and_wuz, band, int, int)
onefun(not_wuz, bnot, int, int)
twofun(xor_wuz, xxor, int, int)

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


