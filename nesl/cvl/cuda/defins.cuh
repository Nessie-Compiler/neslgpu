typedef union maxaligned { int i; float f;} MAXALIGN;
typedef int cvl_bool;
typedef unsigned int uint;

/* TODO: implement random numbers */

#define cvlrand(i)  ((blockIdx.x*83355+122407)%i)
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

#define MAX_DOUBLE (float)MAX_FLOAT
#define MIN_DOUBLE (float)MIN_FLOAT

#include "execution.h"
