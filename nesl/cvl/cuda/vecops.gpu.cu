#include "defins.cuh"

#define TRUE 1
#define FALSE 0


#define make_fpm(_name, _type)    \
__device__ void _name##DevKernel( \
    MAXALIGN *data, \
    int dst, \
    int src, \
    int index, \
    int flags, \
    int len_src, \
    int len_dst, \
    int scratch) \
{\
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;           \
    \
    cvl_bool *pFlags = (cvl_bool *)(&data[flags]);           \
    if (address < len_dst && pFlags[address] == TRUE) {      \
        _type *pDst = (_type*)(&data[dst]);                  \
        _type *pSrc = (_type*)(&data[src]);                  \
        int *pIndex = (int*)(&data[index]);                  \
        pDst[pIndex[address]] = pSrc[address];     \
    } \
} \
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int dst, \
    int src, \
    int index, \
    int flags, \
    int len_src, \
    int len_dst, \
    int scratch) \
{\
 _name##DevKernel(data, dst, src, index, flags, len_src, len_dst, scratch); \
}

#define make_seg_fpm(_name, _type, _unseg)				\
__global__ void _name##Kernel(\
    MAXALIGN *data, \
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
        int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;  \
    \
        if (m_s == 1) {_unseg##DevKernel(data,dst,s,i,f,n_s,n_d,scratch); return;} \
        \
	if (address > n_d) { return; } \
        cvl_bool *pFlags = (cvl_bool *)(&data[f]); \
        if (pFlags[address]) { \
            _type *pDst = (_type*)(&data[dst]); \
            _type *pSrc = (_type*)(&data[s]);   \
            int *pIndex = (int*)(&data[i]);      \
            int *pScratch = (int*)(&data[scratch]);                     \
            pDst[address-pScratch[address]+pIndex[address]] = pSrc[address]; \
        } \
     }

#define make_bfp(_name, _type)					\
__global__ void _name##Kernel(\
    MAXALIGN *data, \
    int dst, \
    int s, \
    int i, \
    int f, \
    int len_src, \
    int len_dest, \
    int scratch)                                        \
    {								\
        int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;  \
        \
        if (address < len_dest) { \
            _type *pDst = (_type*)(&data[dst]); \
            _type *pSrc = (_type*)(&data[s]);               \
            /* TODO int *pIndex = (int*)(&data[i]);         */  \
            cvl_bool *pFlags = (cvl_bool *)(&data[f]);              \
            if (pFlags[address]) { pDst[address] = pSrc[address]; } \
            else { pDst[address] = (_type) 0; }                     \
        } \
    }

#define make_seg_bfp(_name, _type, _unseg)			\
__global__ void _name##Kernel(\
    MAXALIGN *data, \
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
        /* TODO */ \
    }

#define make_seg_dpe1(_type_let,_type,_unseg)			\
__global__ void dpe_pe1##_type_let##Kernel (\
    MAXALIGN *data, \
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
        int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;  \
    \
        if (address < n_d) {                                        \
            _type *pDst = (_type*)(&data[dst]);                     \
            _type *pDefaults = (_type *)(&data[v]);                 \
            pDst[address] = pDefaults[address];                     \
        }                                                           \
    }

#define make_seg_dpe2(_type_let,_type,_unseg)			\
__global__ void dpe_pe2##_type_let##Kernel (\
    MAXALIGN *data, \
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
        int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;  \
    \
        if (address < n_s) {                                        \
            _type *pDst = (_type*)(&data[dst]);                     \
            _type *pSrc = (_type*)(&data[s]);                       \
            int *pIndex = (int*)(&data[i]);                         \
            int *pSD_s = (int *)(&data[sd_s]);                      \
            int *pScratch = (int *)(&data[scratch]);                    \
            pDst[pIndex[address]+pScratch[pSD_s[address]]] = pSrc[address]; \
        }                                                           \
    }

/*
 * FIXME HACK FIXME
 * This horrible, terrible hack is to enforce the last-writer wins case, which only
 * matters for the remove_duplicates basis library code, which does not provide
 * a valid permutation but instead provides all-zeros and expects the last element
 * to be the winner.
 */
#define make_seg_dpe3(_type_let,_type,_unseg)			\
__global__ void dpe_pe3##_type_let##Kernel (\
    MAXALIGN *data, \
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
        int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;  \
        if (address >= n_s) { return; }         \
        _type *pDst = (_type*)(&data[dst]);    \
        _type *pSrc = (_type*)(&data[s]);    \
        int *pIndex = (int*)(&data[i]);        \
        int *pSD_s = (int *)(&data[sd_s]); \
        int *pScratch = (int *)(&data[scratch]); \
    \
        int src_seg = pSD_s[address];                                   \
        int seg_first = pScratch[src_seg];                              \
        int seg_last = seg_first+pSD_s[n_s+src_seg]-1;                    \
        if (address == seg_last) {                                    \
            pDst[pIndex[address]+seg_first] = pSrc[address];           \
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

make_seg_dpe3(z,int,dpe_puz)
make_seg_dpe3(b,cvl_bool,dpe_pub)
make_seg_dpe3(d,float,dpe_pud)
