/*
* Copyright (c) 1992, 1993, 1994, 1995 Carnegie Mellon University
*/

typedef union maxaligned { int i; float f;} MAXALIGN;
#define CVL_SIZE_UNIT sizeof(MAXALIGN)

/* ----- Miscellaneous ----------------------------------------*/

#define TRUE 1
#define FALSE 0

#define QUOTED(x) #x

/* Portable way to glue two strings together in a macro */
#ifdef __STDC__
# 	define GLUE(a,b) a##b
#else
#	define GLUE(a,b) a/**/b
#endif /* __STDC__ */

/* -------------- Scratch Functions ---------------------------*/
/* Every CVL function has an associated scratch function that tells
 * how much scratch space may be needed for it.  This memory must be
 * allocated by the calling function.
 */
#define make_scratch(_name, _body)				\
	int GLUE(_name,_scratch)(int len) {_body}

#define make2_scratch(_name, _body)                               \
	int GLUE(_name,_scratch)(int s_len, int d_len)			\
	{_body}

#define make_seg_scratch(_name, _body)				\
	int GLUE(_name,_scratch)(int vec_len, int seg_len) {_body}

#define make_seg2_scratch(_name, _body)           \
	int GLUE(_name,_scratch)				\
	        (int s_vec_len, int s_seg_len, int d_vec_len, int d_seg_len)	\
	{_body}

#define NO_SCRATCH return 0;
#define NULL_SCRATCH ((vec_p) NULL)
#define make_no_scratch(_name) make_scratch(_name,NO_SCRATCH)

#define make_no2_scratch(_name) make2_scratch(_name,NO_SCRATCH)

#define make_no_seg_scratch(_name) make_seg_scratch(_name,NO_SCRATCH)

#define make_no_seg2_scratch(_name) make_seg2_scratch(_name,NO_SCRATCH)

/* --------------Inplace Functions ----------------------------*/
/* Every CVL function has an associated inplace function indicating 
 * whether or not that function can be performed inplace.  This can
 * be used by the calling function to optimize memory usage.
 */

#define make_inplace(_name, _flag)				\
	unsigned int GLUE(_name,_inplace)() {return _flag;}


#define select(i,j,k)   ((i) ? (j) : (k))
#define selecti(i,j,k)   (*(i++) ? (k++, *(j++)) : (j++, *(k++)))

#define d_to_z(x) ((int) (x))
#define b_to_z(x) ((int) (x))
#define z_to_d(x) ((float) (x))
#define z_to_b(x) ((cvl_bool) notnot(x))

#define cvl_round(x) ((int) ((x) + 0.5))

#ifndef cray
#define cvl_floor(x) ((int) floor(x))
#define cvl_ceil(x) ((int) ceil(x))
#else
#define cvl_floor(x) ((int)(x) == (x) ? (int)(x) : ((int)(x) - ((x) < 0)))
#define cvl_ceil(x)  ((int)(x) == (x) ? (int)(x) : ((int)(x) + ((x) > 0)))
#endif

/* ----------------- Limits -------------------------------------*/
/* ANSI standard requires a limits.h file with this sort of information,
 * but it doesn't seem to exist in our environment.
 */

/* For {MAX,MIN}LONG, replace the 0 with 0L. */
/* Is there a standard .h file with this? */
#define	MAX_INT	((int)(((unsigned) ~0)>>1))
#define	MIN_INT	(~MAX_INT)

/* arbritrarily choosen, MACHINE DEPENDENT.  Works on Suns, Vaxs and
   RTs. HUGE (from math.h) won't work since some compilers complain it is
   too big, and because it returns Infinity or NaN when printed. */
#define MIN_FLOAT   ((float)-1.0e36)
#define MAX_FLOAT   ((float)1.0e36)

#define MAX_DOUBLE (double)MAX_FLOAT
#define MIN_DOUBLE (double)MIN_FLOAT

extern MAXALIGN*                   ComputeMemory;

#ifdef NDEBUG
#define DBG_PRINT(fmt,...)
#define SYNC()
#define CHECK(a)
#else
#define DBG_PRINT(fmt,...) printf(fmt, __VA_ARGS__); fflush(stdout);
#define SYNC() cutilSafeCall( cutilDeviceSynchronize() );
#define CHECK(a) cutilCheckMsg(a)
#endif

#include "execution.h"
