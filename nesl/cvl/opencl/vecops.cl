//typedef union maxaligned { int i; float f;} MAXALIGN;
//typedef int cvl_bool;

#define TRUE 1
#define FALSE 0
int fflush(void *stream);

#define make_fpm(_name, _type)    \
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int dst, \
    int src, \
    int index, \
    int flags, \
    int len_src, \
    int len_dst, \
    int scratch) \
{\
    int address = get_global_id(0);           \
    __global _type *pDst = (__global _type*)(&data[dst]); \
    __global _type *pSrc = (__global _type*)(&data[src]); \
    __global int *pIndex = (__global int*)(&data[index]); \
    __global cvl_bool *pFlags = (__global cvl_bool *)(&data[flags]); \
    \
    if (address < len_dst && pFlags[address] == TRUE) {      \
        pDst[pIndex[address]] = pSrc[address];     \
    } \
}

#define make_seg_fpm(_name, _type, _unseg)				\
__kernel void _name##Kernel(\
    __global MAXALIGN *data, \
    int dst, \
    int s, \
    int i, \
    int f, \
    int sd_s, \
    int n_s, \
    int m_s, \
    int sd_d, \
    int n_d, \
    int m_d, \
    int scratch)                                        \
    {								\
        int address = get_global_id(0);  \
        __global _type *pDst = (__global _type*)(&data[dst]);    \
        __global _type *pSrc = (__global _type*)(&data[s]);    \
        __global int *pIndex = (__global int*)(&data[i]);        \
        __global cvl_bool *pFlags = (__global cvl_bool *)(&data[f]); \
        __global int *pSD_s = (__global int *)(&data[sd_s]); \
        __global int *pSD_d = (__global int *)(&data[sd_d]); \
        __global int *pScratch = (__global int*)(&data[scratch]);  \
        int length = get_global_size(0); \
    \
        if (m_s == 1) {_unseg##Kernel(data,dst,s,i,f,n_s,n_d,scratch); return;} \
        \
        /* First, fill in the scratch space with the unsegemented array offset values */ \
    pScratch[i] = 0;\
    barrier(CLK_GLOBAL_MEM_FENCE);\
    \
    if (address < length) {\
        int offset = 0;\
        int index = 0; \
        int next = (uint)pSD_s[index]; \
        while ((address > offset+next) && index<m_s) {   \
            index++; \
            offset += next; \
            next = pSD_d[index]; \
        } \
        pScratch[address] = offset; \
    } \
    barrier(CLK_GLOBAL_MEM_FENCE); \
 \
    if ( address < length) {                          \
        if (pFlags[address]) { \
            pDst[pIndex[address]+pScratch[address]] = pSrc[address]; \
        } else { \
            pDst[address] = pSrc[address]; \
        } \
    } \
    }

#define make_bfp(_name, _type)					\
__kernel void _name##Kernel(\
    __global MAXALIGN *data, \
    int dst, \
    int s, \
    int i, \
    int f, \
    int len_src, \
    int len_dest, \
    int scratch)                                        \
    {								\
        int address = get_global_id(0);  \
        __global _type *pDst = (__global _type*)(&data[dst]);    \
        __global _type *pSrc = (__global _type*)(&data[s]);    \
        /* TODO int *pIndex = (int*)(&data[i]);         */  \
        __global cvl_bool *pFlags = (__global cvl_bool *)(&data[f]); \
        \
        if (address < len_dest) { \
            if (pFlags[address]) { pDst[address] = pSrc[address]; } \
            else { pDst[address] = (_type) 0; }                     \
        } \
    }

#define make_seg_bfp(_name, _type, _unseg)			\
__kernel void _name##Kernel(\
    __global MAXALIGN *data, \
    int dst, \
    int s, \
    int i, \
    int f, \
    int sd_s, \
    int n_s, \
    int m_s, \
    int sd_d, \
    int n_d, \
    int m_d, \
    int scratch)                                        \
    {								\
/*         int address = get_global_id(0);  */ \
/*        _type *pDst = (_type*)(&data[dst]);    */ \
/*         _type *pSrc = (_type*)(&data[s]);    */ \
/*         int *pIndex = (int*)(&data[i]);        */ \
/*         cvl_bool *pFlags = (cvl_bool *)(&data[f]); */ \
/*         int *pSD_s = (int *)(&data[sd_s]); */ \
/*         int *pSD_d = (int *)(&data[sd_d]); */ \
/*         _type *pScratch = (_type*)(&data[scratch]);  */ \
    \
        /* TODO */ \
    }

#define make_seg_dpe1(_type_let,_type,_unseg)			\
__kernel void dpe_pe1##_type_let##Kernel (\
    __global MAXALIGN *data, \
    int dst, \
    int s, \
    int i, \
    int v, \
    int sd_s, \
    int n_s, \
    int m_s, \
    int sd_d, \
    int n_d, \
    int m_d, \
    int scratch)                                        \
    {								\
        int address = get_global_id(0);  \
        __global _type *pDst = (__global _type*)(&data[dst]);    \
        __global _type *pDefaults = (__global _type *)(&data[v]); \
    \
        if (address < n_d) {                                        \
            pDst[address] = pDefaults[address];                     \
        }                                                           \
    }

#define make_seg_dpe2(_type_let,_type,_unseg)			\
__kernel void dpe_pe2##_type_let##Kernel (\
    __global MAXALIGN *data, \
    int dst, \
    int s, \
    int i, \
    int v, \
    int sd_s, \
    int n_s, \
    int m_s, \
    int sd_d, \
    int n_d, \
    int m_d, \
    int scratch)                                        \
    {								\
        int address = get_global_id(0);  \
        __global _type *pDst = (__global _type*)(&data[dst]);    \
        __global _type *pSrc = (__global _type*)(&data[s]);    \
        __global int *pIndex = (__global int*)(&data[i]);        \
        __global int *pSD_d = (__global int *)(&data[sd_d]); \
        __global int *pSD_s = (__global int *)(&data[sd_s]); \
    \
        int offset = 0;                                             \
        int index = 0;                                              \
        if (address < n_s) {                                        \
            /* For each index, compute its corresponding offset */  \
            /* These are the difference between dst and src segs */ \
            int next = (int)pSD_s[index];                           \
            int remaining = address;                                \
            while ((remaining >= next)  && index<m_d) {             \
                offset += pSD_d[index];                             \
                index++;                                            \
                remaining -= next;                                  \
                next = pSD_s[index];                                \
            }                                                       \
        }                                                           \
        if (address < n_s) {                                        \
            pDst[pIndex[address]+offset] = pSrc[address];           \
        }                                                           \
    }

/* Keep in sync with vecops.h! */

make_fpm(fpm_puz, int)
make_fpm(fpm_pub, cvl_bool)
make_fpm(fpm_pud, float)

make_seg_fpm(fpm_pez, int, fpm_puz)
make_seg_fpm(fpm_peb, cvl_bool, fpm_pub)
make_seg_fpm(fpm_ped, float, fpm_pud)

make_bfp(bfp_puz, int)
make_bfp(bfp_pub, cvl_bool)
make_bfp(bfp_pud, float)

make_seg_bfp(bfp_pez, int, bfp_puz)
make_seg_bfp(bfp_peb, cvl_bool, bfp_pub)
make_seg_bfp(bfp_ped, float, bfp_pud)

make_seg_dpe1(z,int,dpe_puz)
make_seg_dpe1(b,cvl_bool,dpe_pub)
make_seg_dpe1(d,float,dpe_pud)

make_seg_dpe2(z,int,dpe_puz)
make_seg_dpe2(b,cvl_bool,dpe_pub)
make_seg_dpe2(d,float,dpe_pud)
