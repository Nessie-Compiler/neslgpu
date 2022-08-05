#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <memory.h>

#include <cvl.h>
#include <cutil_inline.h>

#include <thrust/device_vector.h>
#include <thrust/scan.h>
#include <thrust/functional.h>

#include "defins.h"

#define make_fpm(_name, _type)					\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int, int); \
	void _name(vec_p d, vec_p s, vec_p i, vec_p f, int len_src, int len_dest, vec_p scratch)	\
    {                                                       \
	if (len_dest==0) return;						\
	SYNC();								\
    DEF_BLOCKS_PER_GRID(len_dest); \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, i, f, len_src, len_dest, scratch); \
	CHECK(#_name " execution failed\n"); \
      } \
make_no2_scratch(_name)						\
make_inplace(_name, INPLACE_NONE)

extern __global__ void all_onesKernel(MAXALIGN *data, int d, int n);

#define make_seg_fpm(_name, _type, _init, _unseg)			\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int, int, int, int, int, int); \
	void _name(vec_p d, vec_p s, vec_p i, vec_p f, vec_p sd_s, int n_s, int m_s, vec_p sd_d, int n_d, int m_d, vec_p scratch)	\
    {                                                       \
	if (n_d==0) return;						\
    if (m_s==1 && m_d==1) { \
        _unseg(d, s, i, f, n_s, n_d, scratch);  \
        return;             \
    } \
	SYNC();								\
    DEF_BLOCKS_PER_GRID(n_d); \
    all_onesKernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, scratch, n_d); \
    thrust::device_ptr<_type> d_input = thrust::device_pointer_cast ((_type*)(ComputeMemory+scratch));  \
    thrust::device_ptr<int> d_flags = thrust::device_pointer_cast ((int*)(ComputeMemory+sd_d));  \
    thrust::device_ptr<_type> d_exclusive = thrust::device_pointer_cast ((_type*)(ComputeMemory+scratch+n_d));  \
    thrust::equal_to<_type> binary_pred;				\
    thrust::exclusive_scan_by_key(d_flags, d_flags+n_d, d_input, d_exclusive, _init, binary_pred);  \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, i, f, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch+n_d); \
	CHECK(#_name " execution failed\n"); \
      } \
make_seg2_scratch(_name, return (d_vec_len*2);)	\
make_inplace(_name, INPLACE_NONE)

#define make_bfp(_name, _type)					\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int, int); \
	void _name(vec_p d, vec_p s, vec_p i, vec_p f, int len_src, int len_dest, vec_p scratch)	\
    {                                                       \
	if (len_dest==0) return;					\
	SYNC();								\
    DEF_BLOCKS_PER_GRID(len_dest); \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, i, f, len_src, len_dest, scratch); \
	CHECK(#_name " execution failed\n"); \
      } \
make_no2_scratch(_name)						\
make_inplace(_name, INPLACE_NONE)

#define make_seg_bfp(_name, _type, _unseg)			\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int, int, int, int, int, int); \
    void _name(vec_p d, vec_p s, vec_p i, vec_p f, vec_p sd_s, int n_s, int m_s, vec_p sd_d, int n_d, int m_d, vec_p scratch)  \
    {                                                       \
    if (n_d==0) return; \
    if (m_s==1 && m_d==1) { \
        _unseg(d, s, i, f, n_s, n_d, scratch);  \
        return;             \
    } \
    SYNC();								\
    fprintf(stderr, "NOTIMPL: running %s\n", #_name); \
    DEF_BLOCKS_PER_GRID(n_d); \
    _name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, i, f, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch); \
	CHECK(#_name " execution failed\n"); \
      } \
make_no_seg2_scratch(_name)					\
make_inplace(_name, INPLACE_NONE)

void dpe_puz(vec_p d, vec_p s, vec_p i, vec_p v, int len_src, int len_dest, vec_p scratch)
    {
    if (d != v) cpy_wuz(d, v, len_dest, scratch);
    smp_puz(d, s, i, len_src, scratch);
    }
make_inplace(dpe_puz, INPLACE_3)
make_no2_scratch(dpe_puz)

void dpe_pub(vec_p d, vec_p s, vec_p i, vec_p v, int len_src, int len_dest, vec_p scratch)
    {
    if (d != v) cpy_wub(d, v, len_dest, scratch);
    smp_pub(d, s, i, len_src, scratch);
    }
make_inplace(dpe_pub, INPLACE_3)
make_no2_scratch(dpe_pub)

void dpe_pud(vec_p d, vec_p s, vec_p i, vec_p v, int len_src, int len_dest, vec_p scratch)
    {
    if (d != v) cpy_wud(d, v, len_dest, scratch);
    smp_pud(d, s, i, len_src, scratch);
    }
make_inplace(dpe_pud, INPLACE_3)
make_no2_scratch(dpe_pud)


#define make_seg_dpe(_type_let,_type,_unseg)			\
    extern __global__ void dpe_pe1##_type_let##Kernel(MAXALIGN*, int, int, int, int, int, int, int, int, int, int, int); \
    extern __global__ void dpe_pe2##_type_let##Kernel(MAXALIGN*, int, int, int, int, int, int, int, int, int, int, int); \
    extern __global__ void dpe_pe3##_type_let##Kernel(MAXALIGN*, int, int, int, int, int, int, int, int, int, int, int); \
void dpe_pe##_type_let (vec_p d, vec_p s, vec_p i, vec_p v, vec_p sd_s, int n_s, int m_s, vec_p sd_d, int n_d, int m_d, vec_p scratch)\
    {                                                       \
    if (n_d == 0) return ;              \
    if (m_s==1 && m_d==1) { \
        _unseg(d, s, i, v, n_s, n_d, scratch);  \
        return;             \
    } \
	SYNC();								\
    {DEF_BLOCKS_PER_GRID(n_d);                                          \
        dpe_pe1##_type_let##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, i, v, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch);} \
	CHECK("dpe_pe1" #_type_let " execution failed\n"); \
	thrust::device_ptr<int> d_input = thrust::device_pointer_cast ((int*)(ComputeMemory+sd_d+n_d)); \
	thrust::device_ptr<int> d_scratch = thrust::device_pointer_cast ((int*)(ComputeMemory+scratch)); \
	thrust::exclusive_scan (d_input, d_input+m_d, d_scratch); \
	SYNC();								\
    if (n_s == 0) return ;                                              \
    {DEF_BLOCKS_PER_GRID(n_s);                                          \
        dpe_pe2##_type_let##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, i, v, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch);} \
	CHECK("dpe_pe2" #_type_let " execution failed\n"); \
    SYNC(); \
    {DEF_BLOCKS_PER_GRID(n_s);                                          \
        dpe_pe3##_type_let##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, i, v, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch);} \
	CHECK("dpe_pe3" #_type_let " execution failed\n"); \
      } \


make_inplace(dpe_pez, INPLACE_NONE)
int dpe_pez_scratch(int n_in, int m_in, int n_out, int m_out) {return (m_out);}

make_inplace(dpe_peb, INPLACE_NONE)
int dpe_peb_scratch(int n_in, int m_in, int n_out, int m_out) {return (m_out);}

make_inplace(dpe_ped, INPLACE_NONE)
int dpe_ped_scratch(int n_in, int m_in, int n_out, int m_out) {return (m_out);}

#define make_dfp(_type_let)					\
void dfp_pu##_type_let (vec_p d, vec_p s, vec_p i, vec_p f, vec_p v, int len_src, int len_dest, vec_p scratch)\
{								\
    if (d != v) cpy_wu##_type_let (d, v, len_dest, scratch);	\
    fpm_pu##_type_let (d, s, i, f, len_src, len_dest, scratch);	\
}

make_inplace(dfp_puz, INPLACE_4)
make_no2_scratch(dfp_puz)

make_inplace(dfp_pub, INPLACE_4)
make_no2_scratch(dfp_pub)

make_inplace(dfp_pud, INPLACE_4)
make_no2_scratch(dfp_pud)

#define make_seg_dfp(_type_let)					\
void dfp_pe##_type_let (vec_p d, vec_p s, vec_p i, vec_p f, vec_p v, vec_p sd_s, int n_s, int m_s, vec_p sd_d, int n_d, int m_d, vec_p scratch)\
{								\
    if (d != v) cpy_wu##_type_let (d, v, n_d, scratch);	\
    if (m_s == 1) {						\
        fpm_pu##_type_let (d, s, i, f, n_s, n_d, scratch);  \
    } else {							\
        fpm_pe##_type_let (d, s, i, f, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch);\
    }								\
}

make_inplace(dfp_pez, INPLACE_4)
make_no_seg2_scratch(dfp_pez)

make_inplace(dfp_peb, INPLACE_4)
make_no_seg2_scratch(dfp_peb)

make_inplace(dfp_ped, INPLACE_4)
make_no_seg2_scratch(dfp_ped)

#include "vecops.h"
