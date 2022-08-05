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

/* This file contains some library functions */

/*----------------------------index (iota)-----------------*/
/* Index(len) creates an integer vector of the numbers */

extern __global__ void ind_luzKernel(MAXALIGN*, int, int, int, int, int);
void ind_luz(vec_p d, int init, int stride, int count, vec_p scratch)
{
    if (count==0) return;
    SYNC();
    DEF_BLOCKS_PER_GRID(count);
    ind_luzKernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, init, stride, count, scratch);
    CHECK("ind_luz execution failed\n"); 
} 
make_no_scratch(ind_luz)
make_inplace(ind_luz,INPLACE_NONE)

/* Segmented index creates a segmented vector of index results, given a 
 * vector of lengths.
 */
__global__ void all_onesKernel(MAXALIGN *data, int d, int n) {
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
    if (address >= n) { return; }

    uint *pDst = (uint*)(&data[d]);
    pDst[address] = 1;
}


__global__ void ind_lezKernelA(MAXALIGN *data,
                            int d,
                            int init,
                            int stride,
                            int count,
                            int scratch)
{
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;           
    if (address >= count) { return; }
    
    uint *pDst = (uint*)(&data[d]);
    __shared__ int initV;
    __shared__ int strideV;

    if (threadIdx.x == 0) {
        uint *pInit = (uint*)(&data[init]); 
        uint *pStride = (uint*)(&data[stride]);
        initV = pInit[0];
        strideV = pStride[0];
    }
    __syncthreads();

    pDst[address] = initV+address*strideV;
    return; 
}

extern __global__ void ind_lezKernel(MAXALIGN*, int, int, int, int, int, int, int);
void ind_lez(vec_p d, vec_p init, vec_p stride, vec_p dest_segd, int vec_len, int seg_count, vec_p scratch)    
{
    if (vec_len==0) return;
    if (seg_count==1) {
        SYNC();
        DEF_BLOCKS_PER_GRID(vec_len);
        ind_lezKernelA<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, init, stride, vec_len, scratch);
        CHECK("ind_lezA execution failed\n");
        return;
    }
    SYNC();								
    DBG_PRINT("DEBUG: running ind_lez on len:%d\n", vec_len);
    
    /* all-ones array for scan to get index in the segment */
    {
    DEF_BLOCKS_PER_GRID(vec_len);
    all_onesKernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, scratch, vec_len);
    thrust::device_ptr<int> d_input = thrust::device_pointer_cast ((int*)(ComputeMemory+scratch)); 
    thrust::device_ptr<int> d_flags = thrust::device_pointer_cast ((int*)(ComputeMemory+dest_segd)); 
    thrust::device_ptr<int> d_exclusive = thrust::device_pointer_cast ((int*)(ComputeMemory+scratch+vec_len)); 
    thrust::equal_to<int> binary_pred;				
    thrust::exclusive_scan_by_key(d_flags, d_flags+vec_len, d_input, d_exclusive, 0, binary_pred);
    }

    {
    DEF_BLOCKS_PER_GRID(vec_len);
    ind_lezKernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, init, stride, dest_segd, vec_len, seg_count, scratch+vec_len);
    }
    CHECK("ind_lez execution failed\n");
}
make_seg_scratch(ind_lez, return (vec_len*2);)
make_inplace(ind_lez,INPLACE_NONE)

/* -------------------------- pack --------------------------*/

/* The pack functions take a source vector and a flag vector and return
 * in a destintation vector all those elements corresponding to True.
 *
 * Pack is divided into 2 phases to allow memeory to be allocated for
 * the result of the pack:
 * pk1 returns the number of true flags in the flag vector
 * pk2 does the pack.
 *
 * pk1:
 *	f = cvl_bool flag vector
 *	len = length of vectors
 */

/* TODO: both of these are terrible. But unseg is almost never called. */

extern __global__ void pk1_luvKernel(MAXALIGN*, int, int, int);
int pk1_luv(vec_p f, int len, vec_p scratch)
{
    if (len==0) return 0;
    SYNC();
    DEF_BLOCKS_PER_GRID(len);
    pk1_luvKernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, f, len, scratch);
    CHECK("pk1_luv execution failed\n");

    int result = 0;
    cutilSafeCall( cudaMemcpy(&result, ComputeMemory+scratch, sizeof(int), cudaMemcpyDeviceToHost));
    return result;
}
make_scratch(pk1_luv, return (len);)
make_inplace(pk1_luv,INPLACE_1)

#define make_pk2(_name, _type)					\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int); \
	void _name (vec_p d, vec_p s, vec_p f, int src_len, int dest_len, vec_p scratch)	\
{								\
    SYNC();								\
    _name##Kernel<<<1,1>>>(ComputeMemory, d, s, f, src_len, dest_len, scratch); \
    CHECK(#_name " execution failed\n");			\
}\
	make_no_scratch(_name)			\
	make_inplace(_name,INPLACE_NONE)

make_pk2(pk2_luz, int)
make_pk2(pk2_lub, cvl_bool)
make_pk2(pk2_lud, float)

/* segmented pack: Packs a segmented vector into dest and creates
 * segment descriptor for it in seg_dest
 * Pack is split up into two parts.  The first takes the 
 * packing flag vector and its segd and returns the lengths vector
 * describing the result of the final pack.
 */

/* pack1: returns the lengths vector of the result of the pack
 *	ds = destination segment descriptor
 * 	f = boolean flags (same length as s)
 * 	sd = source segment descriptor
 *	n = length of f
 *	m = number of segments of f
 */

void pk1_lev(vec_p ds, vec_p f, vec_p sd, int n, int m, vec_p scratch)
{
    add_rez(ds, f, sd, n, m, scratch);
}
make_seg_scratch(pk1_lev, return vec_len*2;)
make_inplace(pk1_lev, INPLACE_NONE)

/* pack2: returns the pack 
 */
#define make_pack2(_name, _type)				\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int, int, int, int, int); \
	void _name (vec_p d, vec_p s, vec_p f, vec_p sd_s, int n_s , int m_s, vec_p sd_d, int n_d, int m_d, vec_p scratch)\
	{								\
	thrust::device_ptr<int> d_flags = thrust::device_pointer_cast ((int*)(ComputeMemory+f)); \
	thrust::device_ptr<int> d_indices = thrust::device_pointer_cast ((int*)(ComputeMemory+scratch)); \
	thrust::inclusive_scan(d_flags, d_flags+n_s, d_indices);	\
	SYNC();								\
    DEF_BLOCKS_PER_GRID(n_s); \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, f, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch); \
	CHECK(#_name " execution failed\n"); \
    } \
    make_seg2_scratch(_name, return s_vec_len;) \
    make_inplace(_name, INPLACE_NONE)

make_pack2(pk2_lez, int)
make_pack2(pk2_leb, cvl_bool)
make_pack2(pk2_led, float)

