#include "defins.cuh"

#define TRUE 1
#define FALSE 0

/* -----------------Unsegmented Scans----------------------------------*/

/* simple scan template:
   d = destination vector
   s = source vector
   len = length of d, s
   _init = initial value (identity element)
   d and s should be vectors of the same size and type
*/
#define simpscan(_name, _func, _type, _init)			\
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int d, \
    int s, \
    int len, \
    int scratch)                         \
{\
    int i = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x; \
\
    if (i >= (len-1)) {return;}			\
    _type *pDst = (_type*)(&data[d+1]); \
    _type *pSrc = (_type*)(&data[s]); \
    if (i == 0) {                               \
        pDst[-1] = _init;                       \
    }                                           \
    pDst[i] = pSrc[i]; \
    __syncthreads();              \
    for (int offset = 1; offset < len; offset *= 2)   \
    {\
        _type t = _init; \
\
        if (i >= offset) t = pDst[i-offset]; \
        __syncthreads(); \
\
        if (i >= offset) pDst[i] = _func(t, pDst[i]);   \
        __syncthreads(); \
    } \
\
    return; \
}

simpscan(add_suz, plus, int, 0)		/* add scans */
simpscan(add_sud, plus, float, (float) 0.0)

simpscan(mul_suz, times, int, 1)	/* multiply scans */
simpscan(mul_sud, times, float, (float) 1.0)

simpscan(min_suz, mmin, int, MAX_INT)	/* min scans */
simpscan(min_sud, mmin, float, MAX_FLOAT)

simpscan(max_suz, mmax, int, MIN_INT)	/* max scans */
simpscan(max_sud, mmax, float, MIN_FLOAT)

simpscan(and_sub, aand, cvl_bool, 1)	/* logical and scan */
simpscan(and_suz, band, int, ~0)	/* bitwise and scan */

simpscan(ior_sub, oor, cvl_bool, 0)	/* logical or scan */
simpscan(ior_suz, bor, int, 0)		/* bitwise or scan */

simpscan(xor_sub, lxor, cvl_bool, 0)	/* logical or scan */
simpscan(xor_suz, xxor, int, 0)		/* bitwise xor scan */

/* ----------------- Segmented Scans --------------------------*/


/* --------------------Reduce Functions--------------------------------*/
/* reduce template */
#define reduce(_name, _funct, _typ, _identity)         \
__global__ void _name##Kernel(MAXALIGN *data, int src, int dst, int len) \
{ \
    int global_index = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;                    \
    if (global_index == 0) {                                \
        _typ *g_idata = (_typ*)(&data[src]);                \
        _typ *g_odata = (_typ*)(&data[dst]);                \
        _typ result = _identity;                            \
        unsigned int k = 0;                                 \
        for (k = 0; k < len; k++) {                         \
            result = _funct(result, g_idata[k]);            \
        }                                                   \
        g_odata[0] = result;                                 \
     }                                                       \
}


reduce(add_ruz, plus, int, 0)			/* add reduces */
reduce(add_rud, plus, float, (float) 0.0)

reduce(mul_ruz, times, int, 1)			/* multiply reduce */
reduce(mul_rud, times, float, (float) 1.0)

reduce(min_ruz, min, int, MAX_INT)		/* min reduces */
reduce(min_rud, min, float, MAX_FLOAT)

reduce(max_ruz, max, int, MIN_INT)		/* max reduces */
reduce(max_rud, max, float, MIN_FLOAT)

reduce(and_rub, aand, cvl_bool, TRUE)		/* logical and reduce */
reduce(and_ruz, band, int, (~0))		/* bitwise and scan */

reduce(ior_rub, oor, cvl_bool, FALSE)		/* logical or reduce */
reduce(ior_ruz, bor, int, 0)			/* bitwise or reduce */

reduce(xor_rub, lxor, cvl_bool, 0)	/* logical or reduce */
reduce(xor_ruz, xxor, int, 0)		/* bitwise xor reduce */

/* ------------------Segmented Reduces ---------------------------------*/
/* segmented reduce template:
 *	d = destination vector
 *	s = source vector
 *	sd = segment descriptor of source, with components n and m
 */
/* see implementation note above */
#define segreduce(_name, _funct, _typ, _identity, _unseg)	\
__global__ void _name##KernelA( \
    MAXALIGN *data, \
    int d, \
    int s, \
    int sd, \
    int n, \
    int m,\
    int scratch)                        \
{\
    int segment = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;    \
    if (segment >= m) { return; }			    \
    _typ *g_idata = (_typ*)(&data[scratch]);    \
    _typ *g_odata = (_typ*)(&data[d]);    \
    int *pSD = (int*)(&data[sd+n]);         \
    int *pEnds = (int*)(&data[scratch+n]);         \
    if (pSD[segment]==0) { \
    g_odata[segment] = _identity; \
    } else {					\
	g_odata[segment] = g_idata[pEnds[segment]-1];	\
    } \
} \
__global__ void _name##KernelOneSeg( \
    MAXALIGN *data, \
    int d, \
    int s, \
    int sd, \
    int n, \
    int m,\
    int scratch)                        \
{\
    int segment = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;    \
    if (segment > 0) { return; }			    \
    _typ *g_idata = (_typ*)(&data[scratch]);    \
    _typ *g_odata = (_typ*)(&data[d]);    \
    g_odata[0] = g_idata[n-1];                  \
} \
__global__ void _name##KernelIdent( \
    MAXALIGN *data, \
    int d)                        \
{\
    int segment = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;    \
    if (segment > 0) { return; }			    \
    _typ *g_odata = (_typ*)(&data[d]);    \
    g_odata[0] = _identity; \
}

segreduce(add_rez, plus, int, 0, add_ruz)		/* add reduces */
segreduce(add_red, plus, float, (float) 0.0, add_rud)

segreduce(mul_rez, times, int, 1, mul_ruz)		/* multiply scans */
segreduce(mul_red, times, float, (float) 1.0, mul_rud)

segreduce(min_rez, min, int, MAX_INT, min_ruz)		/* min reduces */
segreduce(min_red, min, float, MAX_FLOAT, min_rud)

segreduce(max_rez, max, int, MIN_INT, max_ruz)		/* max reduces */
segreduce(max_red, max, float, MIN_FLOAT, max_rud)

segreduce(and_reb, aand, cvl_bool, TRUE, and_rub)	/* logical and reduce */
segreduce(and_rez, band, int, ~0, and_ruz)		/* bitwise and reduce */

segreduce(ior_reb, oor, cvl_bool, FALSE, ior_rub)	/* logical or reduce */
segreduce(ior_rez, bor, int, 0, ior_ruz)		/* bitwise or reduce */

segreduce(xor_reb, lxor, cvl_bool, 0, xor_rub)		/* logical xor scan */
segreduce(xor_rez, xxor, int, 0, xor_ruz)		/* bitwise xor scan */

/* -------------------Extract-------------------------------------*/

/* segmented extract:
 *	d = destination vector (unsegmented),
 *	s = source vector (segmented, same type as d)
 *	i = index vector (unsegmented), length as d
 *  sd, n, m = segment descriptor for v
 */
#define make_seg_ext(_name, _type)			\
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int d, \
    int s, \
    int i, \
    int sd, \
    int n, \
    int m, \
    int scratch)                         \
{\
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;\
    if (address >= m) { return; } \
    _type *pSrc = (_type*)(&data[s]); \
    _type *pDst = (_type*)(&data[d]); \
    uint *pIndex = (uint*)(&data[i]);\
    uint *pScratch = (uint*)(&data[scratch]);\
    pDst[address] = pSrc[pScratch[address]+pIndex[address]];	\
}

make_seg_ext(ext_vez, int)
make_seg_ext(ext_veb, cvl_bool)
make_seg_ext(ext_ved, float)

/* ------------------Replace-------------------------------------*/

/* replace:
 *	V = destination vector  (segmented)
 *  i = index in destination vector
 *  val = value to write
 */
#define make_replace(_name, _typ, _funct)             \
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int V, \
    int i, \
    const _typ val, \
    int len, \
    int scratch)                         \
{						\
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;	\
    if (address == 0) {				\
    _typ *pDst = (_typ*)(&data[V]);		\
	pDst[i] = _funct(val);			\
    }						\
}

make_replace(rep_vuz, int, ident)
make_replace(rep_vub, cvl_bool, notnot)
make_replace(rep_vud, float, ident)

/* segmented replace:
 *	d = destination vector  (segmented)
 *	s = index vector	(unsegmented, one entry per segment of d)
 *	v = value vector    (ditto)
 *	sd, n, m = segment descriptor for d.
 */
#define make_seg_replace(_name, _type)		\
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int d, \
    int s, \
    int v, \
    int sd, \
    int n, \
    int m, \
    int scratch)                         \
{\
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;                         \
    if (address >= m) { return; }			    \
    _type *pDest = (_type*)(&data[d]);      \
    int *pIndex = (int*)(&data[s]);     \
    _type *pValues = (_type*)(&data[v]);    \
    int *pScratch = (int*)(&data[scratch]);           \
    pDest[pScratch[address] + pIndex[address]] = pValues[address];       \
}

make_seg_replace(rep_vez, int)
make_seg_replace(rep_veb, cvl_bool)
make_seg_replace(rep_ved, float)

/* ----------------Distribute-----------------------------------*/

/* distribute v to length len, return in d */
#define make_distribute(_name, _type)		\
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int d, \
    _type v, \
    int len, \
    int scratch)                         \
{\
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;    \
    if (address < len) { \
        _type *pDst = (_type*)(&data[d]);       \
        pDst[address] = v; \
    } \
    return; \
}

make_distribute(dis_vuz, int)
make_distribute(dis_vub, cvl_bool)
make_distribute(dis_vud, float)

/* segmented distribute:
 *  d = destination vector (segmented)
 *  v = value vector (unsegmented), same type as d
 *  sd, n, m = segment descriptor for d
 */
#define make_seg_distribute(_name, _type, _unseg)	\
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int d, \
    int v, \
    int sd, \
    int n, \
    int m, \
    int scratch)                         \
{\
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;		\
    if (address >= n) {							\
	return;								\
    }									\
    _type *pDst = (_type*)(&data[d]);					\
    _type *pV = (_type*)(&data[v]);					\
    if (m > 1) {                                    \
        int *pSD = (int*)(&data[sd]);					\
        pDst[address] = pV[pSD[address]];               \
    } else {                                            \
        pDst[address] = pV[0];                              \
    }                                                   \
    return;                                             \
}

make_seg_distribute(dis_vez, int, dis_vuz)
make_seg_distribute(dis_veb, cvl_bool, dis_vub)
make_seg_distribute(dis_ved, float, dis_vud)


/* --------------Permute---------------------------------------*/

/* simple permute: 
 *	d = destination vector
 *	s = source vector, same type as d
 *	i = index vector
 *	len = length of vectors
 */

#define make_smpper(_name, _type)			\
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int d, \
    int s, \
    int i, \
    int len, \
    int scratch)                         \
{\
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;    \
    if (address < len) { \
        _type *pDst = (_type*)(&data[d]);   \
        _type *pSrc = (_type*)(&data[s]);   \
        uint *pIndex = (uint*)(&data[i]);      \
        pDst[pIndex[address]] = pSrc[address]; \
    }\
}

make_smpper(smp_puz, int)
make_smpper(smp_pub, cvl_bool)
make_smpper(smp_pud, float)

/* segmented simple permute:
 *  d = destination vector (segmented)
 *  s = source vector (segmented), same type as d
 *  i = index vector (segmented)
 *  sd, n, m = segment descriptor
 */
#define make_seg_smpper(_name, _type, _unseg)		\
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int d, \
    int s, \
    int i, \
    int sd, \
    int n, \
    int m, \
    int scratch)                         \
{\
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;    \
    if (address >= n) { return; }				      \
    _type *pDst = (_type*)(&data[d]);       \
    _type *pSrc = (_type*)(&data[s]);       \
    uint *pIndex = (uint*)(&data[i]);       \
    int *pSD = (int*)(&data[sd]);       \
    int *pScratch = (int*)(&data[scratch]);       \
    pDst[pScratch[pSD[address]]+pIndex[address]] = pSrc[address];     \
}

make_seg_smpper(smp_pez, int, smp_puz)
make_seg_smpper(smp_peb, cvl_bool, smp_pub)
make_seg_smpper(smp_ped, float, smp_pud)

/*----------------------Back Permute-------------------*/
/* back permute: 
 *	d = destination vector
 *	s = source vector, same type as d
 *	i = index vector
 *	s_len = length of s
 *	d_len = length of d and i
 */

#define make_bckper(_name, _type)			\
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int d, \
    int s, \
    int i, \
    int s_len, \
    int d_len, \
    int scratch)                         \
{\
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;           \
    \
    if (address < d_len) { \
        _type *pDst = (_type*)(&data[d]);       \
        _type *pSrc = (_type*)(&data[s]);       \
        int *pIndex = (int*)(&data[i]);         \
        pDst[address] = pSrc[pIndex[address]];  \
    } \
}

make_bckper(bck_puz, int)
make_bckper(bck_pub, cvl_bool)
make_bckper(bck_pud, float)

/* segmented bck permute:
 *  d = destination vector (segmented)
 *  s = source vector (segmented), same type as d
 *  i = index vector (compatible with d)
 *  sd_s, n_s, m_s = source segment descriptor
 *  sd_d, n_d, n_d = dest segment descriptor
 */
#define make_seg_bckper(_name, _type, _unseg)	\
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int d, \
    int s, \
    int i, \
    int sd_s, \
    int n_s, \
    int m_s, \
    int sd_d, \
    int n_d, \
    int m_d, \
    int scratch)                         \
{\
        int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;  \
    \
	if (address >= n_d) {					    \
	    return;						    \
	}							    \
    _type *pDst = (_type*)(&data[d]);        \
    _type *pSrc = (_type*)(&data[s]);          \
    int *pIndex = (int*)(&data[i]);            \
    int *pSD_d = (int *)(&data[sd_d]);           \
    int *pOffsets = (int *)(&data[scratch]);                        \
	pDst[address] = pSrc[pOffsets[pSD_d[address]]+pIndex[address]];	\
}

make_seg_bckper(bck_pez, int, bck_puz)
make_seg_bckper(bck_peb, cvl_bool, bck_pub)
make_seg_bckper(bck_ped, float, bck_pud)
