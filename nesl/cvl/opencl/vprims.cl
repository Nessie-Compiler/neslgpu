//typedef union maxaligned { int i; float f;} MAXALIGN;
//typedef int cvl_bool;
#define mmax(i,j)	((i) > (j) ? (i) : (j))
#define mmin(i,j)	((i) < (j) ? (i) : (j))

int fflush(void *stream);
/* -----------------Unsegmented Scans----------------------------------*/

/* simple scan template:
   d = destination vector
   s = source vector
   len = length of d, s
   _init = initial value (identity element)
   d and s should be vectors of the same size and type
*/
#define simpscan(_name, _func, _type, _init)			\
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int len, \
    int scratch)                         \
{\
    __global _type *pDst = (__global _type*)(&data[d+1]); \
    __global _type *pSrc = (__global _type*)(&data[s]); \
    int i = get_global_id(0); \
    int n = get_global_size(0); \
\
    if (i == 0) {                               \
        pDst[-1] = _init;                       \
    }                                           \
    pDst[i] = pSrc[i]; \
    barrier(CLK_GLOBAL_MEM_FENCE);              \
    for (int offset = 1; offset < n; offset *= 2)   \
    {\
        _type t = _init; \
\
        if (i >= offset) t = pDst[i-offset]; \
        barrier(CLK_GLOBAL_MEM_FENCE); \
\
        if (i >= offset) pDst[i] = _func(t, pDst[i]);   \
        barrier(CLK_GLOBAL_MEM_FENCE); \
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

/* segmented simple scan template:
   d = destination vector
   s = source vector
   sd = segment descriptor of source
   n = number of elements in whole vector
   m = number of segments

   d and s should be vectors of the same size and type

   TODO: only handles one segment right now!
   Remember to keep shifts with init in the zero position.
*/
#define simpsegscan_orig(_name, _funct, _type, _init, _unseg)		\
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int sd, \
    int n,\
    int m,\
    int scratch)                        \
{\
    __global uint *pSD = (__global uint*)(&data[sd]); \
    __global _type *pSrc = (__global _type*)(&data[s]); \
    __global _type *pDst = (__global _type*)(&data[d+1]); \
    __global _type *pScratch = (__global _type*)(&data[scratch]);\
    int i = get_global_id(0);\
\
    if (i == 0) {                               \
        pDst[-1] = _init;                       \
    }                                           \
    /* scratch is the same length as the input. Compute flagged starts */\
    if (i < n) { \
        pScratch[i] = 0;                        \
        pDst[i] = pSrc[i];                      \
    } \
    barrier(CLK_GLOBAL_MEM_FENCE);\
    \
    if (i < n) {\
        int start = 0;\
        for (int j = 0; j < i; j++) {\
            start += pSD[j]; \
        } \
        pScratch[start] = 1; \
    } \
    barrier(CLK_GLOBAL_MEM_FENCE); \
 \
    for (int offset = 1; offset < n; offset *= 2) \
    { \
        _type t = _init; \
 \
        if ((i >= offset) && (pDst[i-offset] != 1)) \
            t = pDst[i-offset]; \
        barrier(CLK_GLOBAL_MEM_FENCE); \
 \
        if (i >= offset) pDst[i] = _funct(t, pDst[i]);   \
        barrier(CLK_GLOBAL_MEM_FENCE); \
    } \
}

/* TODO: a kernel with some parallelism
 * Segmented hack: this kernel is invoked with m global items,
 * one for each segment.
 */
#define simpsegscan(_name, _funct, _type, _init, _unseg)		\
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int sd, \
    int n,\
    int m,\
    int scratch)                        \
{\
    __global uint *pSD = (__global uint*)(&data[sd]);               \
    int segment = get_global_id(0);                                 \
    int startPos = 0;                                               \
    for (int i = 0; i < segment; i++) {                             \
        startPos += pSD[i];                                         \
    }                                                               \
    int segLen = pSD[segment];                                      \
    __global _type *pDst = (__global _type*)(&data[d+startPos]);    \
    __global _type *pSrc = (__global _type*)(&data[s+startPos]);    \
    pDst[0] = _init;                                                \
    _type val = _init;                                              \
    for (int k = 1; k < segLen; k++) {                              \
        val = _funct(val, pSrc[k-1]);                               \
        pDst[k] = val;                                              \
    }                                                               \
}

simpsegscan(add_sez, plus, int, 0, add_suz)		/* add scans */
simpsegscan(add_sed, plus, float, (float) 0.0, add_sud)

simpsegscan(mul_sez, times, int, 1, mul_suz)		/* multiply scans */
simpsegscan(mul_sed, times, float, (float) 1.0, mul_sud)

simpsegscan(min_sez, mmin, int, MAX_INT, min_suz)	/* min scans */
simpsegscan(min_sed, mmin, float, MAX_FLOAT, min_sud)

simpsegscan(max_sez, mmax, int, MIN_INT, max_suz)	/* max scans */
simpsegscan(max_sed, mmax, float, MIN_FLOAT, max_sud)

simpsegscan(and_seb, aand, cvl_bool, 1, and_sub)		/* logical and scan */
simpsegscan(and_sez, band, int, ~0, and_suz)		/* bitwise and scan */

simpsegscan(ior_seb, oor, cvl_bool, 0, ior_sub)		/* logical or scan */
simpsegscan(ior_sez, bor, int, 0, ior_suz)		/* bitwise or scan */

simpsegscan(xor_seb, lxor, cvl_bool, 0, xor_sub)	/* logical or scan */
simpsegscan(xor_sez, xxor, int, 0, xor_suz)		/* bitwise xor scan */

#define blockSize 128

/* --------------------Reduce Functions--------------------------------*/
/* reduce template */
#define reduce_faster(_name, _funct, _typ, _identity)                         \
__kernel void _name##Kernel(__global MAXALIGN *data, int src, int dst, unsigned int len, __local _typ* scratch) \
{ \
    __global _typ *g_idata = (__global _typ*)(&data[src]);              \
    __global _typ *g_odata = (__global _typ*)(&data[dst]);              \
    int length = len;                                                   \
    int global_index = get_global_id(0);                                \
    int local_index = get_local_id(0);                                  \
    /* Load data into local memory */                                   \
    if (global_index < length) {                                        \
        scratch[local_index] = g_idata[global_index];                   \
    } else {                                                            \
        scratch[local_index] = _identity;                               \
    }                                                                   \
    barrier(CLK_LOCAL_MEM_FENCE);                                       \
    for(int offset = get_local_size(0) / 2;                             \
        offset > 0;                                                     \
        offset >>= 1) {                                                 \
    if (local_index < offset) {                                         \
        _typ other = _identity;                                         \
        if (local_index+offset < length) {                              \
            other = scratch[local_index + offset];                      \
        }                                                               \
        _typ mine = scratch[local_index];                               \
        scratch[local_index] = _funct(mine,other);                      \
    }                                                                   \
    barrier(CLK_LOCAL_MEM_FENCE);                                       \
    }                                                                   \
    if (local_index == 0) {                                             \
        g_odata[get_group_id(0)] = scratch[0];                          \
    }                                                                   \
    barrier(CLK_GLOBAL_MEM_FENCE);                                      \
    if ((global_index == 0) && (local_index == 0)) {                    \
        for (unsigned int i = 1; i < get_num_groups(0); i++) {          \
            scratch[0] = _funct(scratch[0], g_odata[i]);                \
        }                                                               \
        g_odata[0] = scratch[0];                                        \
    }                                                                   \
}

#define reduce(_name, _funct, _typ, _identity)                         \
__kernel void _name##Kernel(__global MAXALIGN *data, int src, int dst, unsigned int len, __local _typ* scratch) \
{ \
    __global _typ *g_idata = (__global _typ*)(&data[src]);  \
    __global _typ *g_odata = (__global _typ*)(&data[dst]);  \
    int global_index = get_global_id(0);                    \
    if (global_index == 0) {                                \
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

reduce(min_ruz, mmin, int, MAX_INT)		/* min reduces */
reduce(min_rud, mmin, float, MAX_FLOAT)

reduce(max_ruz, mmax, int, MIN_INT)		/* max reduces */
reduce(max_rud, mmax, float, MIN_FLOAT)

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
#define segreduce_orig(_name, _funct, _type, _identity, _unseg)	\
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int sd, \
    int n, \
    int m,\
    int scratch)                        \
{\
    __global uint *pSD = (__global uint*)(&data[sd]); \
    __global _type *pSrc = (__global _type*)(&data[s]); \
    __global _type *pDst = (__global _type*)(&data[d]); \
    __global uint *pFlags = (__global uint*)(&data[scratch]);\
    __global _type *pScratch = (__global _type*)(&data[scratch+n]);\
    int i = get_global_id(0);\
\
    /* scratch is the same length as the input. Compute flagged starts */\
    if (i < n) { \
        pFlags[i] = 0;                        \
        pScratch[i] = pSrc[i];                      \
    } \
    barrier(CLK_GLOBAL_MEM_FENCE);\
    \
    if (i < m) {\
        int start = 0;\
        for (int j = 0; j < i; j++) {\
            start += pSD[j]; \
        } \
        pFlags[start] = i+1; \
    } \
    barrier(CLK_GLOBAL_MEM_FENCE); \
 \
    for (int offset = 1; offset < n; offset *= 2) \
    { \
        _type t = _identity; \
 \
        if ((i >= offset) && (pFlags[i-offset] == 0)) { \
            t = pScratch[i-offset]; \
        }                              \
        barrier(CLK_GLOBAL_MEM_FENCE); \
 \
        if (i >= offset) pScratch[i] = _funct(t, pScratch[i]);   \
        barrier(CLK_GLOBAL_MEM_FENCE); \
    } \
    if (pFlags[i] != 0) {\
        pDst[pFlags[i]-1] = pScratch[i];\
    } \
}

/* TODO: a kernel with some parallelism
 * Segmented hack: this kernel is invoked with m global items,
 * one for each segment.
 */
#define segreduce(_name, _funct, _typ, _identity, _unseg)	\
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int sd, \
    int n, \
    int m,\
    int scratch)                        \
{\
    __global _typ *g_idata = (__global _typ*)(&data[s]);    \
    __global _typ *g_odata = (__global _typ*)(&data[d]);    \
    __global int *pSD = (__global int*)(&data[sd]);         \
    int segment = get_global_id(0);                         \
    int startPos = 0;                                       \
    for (int i = 0; i < segment; i++) {                     \
        startPos += pSD[i];                                 \
    }                                                       \
    int endPos = startPos + pSD[segment];                   \
    _typ result = _identity;                                \
    int k = 0;                                              \
    for (k = startPos; k < endPos; k++) {                   \
        result = _funct(result, g_idata[k]);                \
    }                                                       \
    g_odata[segment] = result;                              \
}

segreduce(add_rez, plus, int, 0, add_ruz)		/* add reduces */
segreduce(add_red, plus, float, (float) 0.0, add_rud)

segreduce(mul_rez, times, int, 1, mul_ruz)		/* multiply scans */
segreduce(mul_red, times, float, (float) 1.0, mul_rud)

segreduce(min_rez, mmin, int, MAX_INT, min_ruz)		/* min reduces */
segreduce(min_red, mmin, float, MAX_FLOAT, min_rud)

segreduce(max_rez, mmax, int, MIN_INT, max_ruz)		/* max reduces */
segreduce(max_red, mmax, float, MIN_FLOAT, max_rud)

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
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int i, \
    int sd, \
    int n, \
    int m, \
    int scratch)                         \
{\
    __global uint *pSD = (__global uint*)(&data[sd]); \
    __global _type *pSrc = (__global _type*)(&data[s]); \
    __global _type *pDst = (__global _type*)(&data[d]); \
    __global uint *pIndex = (__global uint*)(&data[i]);\
    int address = get_global_id(0);\
    \
    /* Index contains 'm' items, one from each vector */ \
    if (address < m) {\
        int start = 0;\
        for (int j = 0; j < address; j++) {\
            start += pSD[j]; \
        } \
        pDst[address] = pSrc[start+pIndex[address]];\
    } \
    return; \
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
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int V, \
    int i, \
    const _typ val, \
    int len, \
    int scratch)                         \
{						\
    int address = get_global_id(0);	\
    __global _typ *pDst = (__global _typ*)(&data[V]);		\
    if (address == 0) {				\
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
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int v, \
    int sd, \
    int n, \
    int m, \
    int scratch)                         \
{\
    __global _type *pDest = (__global _type*)(&data[d]);      \
    __global int *pIndex = (__global int*)(&data[s]);     \
    __global int *pSD = (__global int*)(&data[sd]);           \
    __global _type *pValues = (__global _type*)(&data[v]);    \
    int address = get_global_id(0);                         \
    int offset = 0;                                         \
    for (int i = 0; i < address; i++) {                     \
        offset += pSD[i];                                   \
    }                                                       \
    offset += pIndex[address];                              \
    pDest[offset] = pValues[address];       \
}

make_seg_replace(rep_vez, int)
make_seg_replace(rep_veb, cvl_bool)
make_seg_replace(rep_ved, float)

/* ----------------Distribute-----------------------------------*/

/* distribute v to length len, return in d */
#define make_distribute(_name, _type)          \
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    const _type v, \
    int len, \
    int scratch)                         \
{\
    int address = get_global_id(0);    \
    __global _type *pDst = (__global _type*)(&data[d]);       \
    if (address < len) { \
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
    __kernel void _name##Kernel(                    \
                                __global MAXALIGN *data,    \
                                int d,                      \
                                int v,                      \
                                int sd,                     \
                                int n,                      \
                                int m,                      \
                                int scratch)                \
    {                                                            \
        int address = get_global_id(0);                          \
        __global _type *pDst = (__global _type*)(&data[d]);      \
        __global uint *pSD = (__global uint*)(&data[sd]);        \
        __global _type *pV = (__global _type*)(&data[v]);        \
        if (address >= n) {                                      \
            return;                                              \
        }                                                        \
        int segment = 0;                                         \
        int offset = address;                                    \
        int next = (int)pSD[segment];                            \
        while (offset >= next  && segment<m) {                   \
            offset -= next;                                      \
            segment++;                                           \
            next = pSD[segment];                                 \
        }                                                        \
        pDst[address] = pV[segment];                             \
        return;                                                  \
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
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int i, \
    int len, \
    int scratch)                         \
{\
    int address = get_global_id(0);    \
    __global _type *pDst = (__global _type*)(&data[d]);       \
    __global _type *pSrc = (__global _type*)(&data[s]);       \
    __global uint *pIndex = (__global uint*)(&data[i]);       \
    if (address < len) { \
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
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int i, \
    int sd, \
    int n, \
    int m, \
    int scratch)                         \
{\
    int address = get_global_id(0);    \
    __global _type *pDst = (__global _type*)(&data[d]);       \
    __global _type *pSrc = (__global _type*)(&data[s]);       \
    __global uint *pIndex = (__global uint*)(&data[i]);       \
    __global int *pSD = (__global int*)(&data[sd]);       \
    if (address < n) {\
        int offset = 0;\
        int index = 0; \
        int next = pSD[index]; \
        while ((address >= offset+next) && index<m) {    \
            index++; \
            offset += next; \
            next = pSD[index]; \
        } \
        pDst[offset+pIndex[address]] = pSrc[address]; \
    } \
    return; \
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
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int i, \
    int s_len, \
    int d_len, \
    int scratch)                         \
{\
    int address = get_global_id(0);           \
    __global _type *pDst = (__global _type*)(&data[d]); \
    __global _type *pSrc = (__global _type*)(&data[s]); \
    __global int *pIndex = (__global int*)(&data[i]); \
    \
    if (address < d_len) { \
   pDst[address] = pSrc[pIndex[address]]; \
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
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
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
        int address = get_global_id(0);  \
        __global _type *pDst = (__global _type*)(&data[d]);    \
        __global _type *pSrc = (__global _type*)(&data[s]);    \
        __global int *pIndex = (__global int*)(&data[i]);        \
        __global int *pSD_s = (__global int *)(&data[sd_s]); \
        __global int *pSD_d = (__global int *)(&data[sd_d]); \
    \
        int offset = 0;                                             \
        int index = 0;                                              \
        if (address < n_s) {                                        \
            /* For each index, compute its corresponding offset */  \
            int next = (int)pSD_d[index];                           \
            while ((address >= offset+next)  && index<m_d) {        \
                offset += pSD_s[index];                             \
                index++;                                            \
                next = pSD_d[index];                                \
            }                                                       \
        }                                                           \
        if (address < n_d) {                                        \
            pDst[address] = pSrc[offset+pIndex[address]];           \
        }                                                           \
}

make_seg_bckper(bck_pez, int, bck_puz)
make_seg_bckper(bck_peb, cvl_bool, bck_pub)
make_seg_bckper(bck_ped, float, bck_pud)
