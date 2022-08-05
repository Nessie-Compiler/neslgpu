#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <memory.h>

#include <cvl.h>
#include <cutil_inline.h>

#include <thrust/device_vector.h>
#include <thrust/sequence.h>
#include <thrust/sort.h>

#include "defins.h"

/* This file contains the rank functions:
 * o upward and downward,
 * o integer and double,
 * o segmented and unsegmented.
 * The result of these functions is a permutation (suitable for use with
 * the smp_p** functions) that gives the sorted order of the elements
 * of the source vector.  The rank functions are stable, and use a radix
 * rank to get linear performance.
 */

__global__ void adjust_values(MAXALIGN *data, int d, int src, int len) {
    int i = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
    if (i >= len) { return; }
    int *pDst = (int*)(&data[d]); 
    int *pSrc = (int*)(&data[src]); 
    pDst[pSrc[i]] = i;
}

#define make_rk(_name, _letter, _type, _first, _second)			\
	void _name(vec_p d, vec_p s, int len, vec_p scratch)	\
    {                                                       \
	bool ascending = (_second == 1); /*ascending*/			\
	SYNC();								\
	cpy_wu##_letter(scratch, s, len, (vec_p)0);			\
	thrust::device_ptr<_type> d_keys = thrust::device_pointer_cast ((_type*)(ComputeMemory+scratch));  \
	thrust::device_ptr<int> d_values = thrust::device_pointer_cast ((int*)(ComputeMemory+scratch+len)); \
	thrust::device_ptr<int> d_output = thrust::device_pointer_cast ((int*)(ComputeMemory+d));  \
	if (ascending) { \
	    thrust::sequence (d_values, d_values+len, 0); \
	} else { \
	    thrust::sequence (d_values, d_values+len, (len-1), -1);	\
	}\
	thrust::stable_sort_by_key(d_keys, d_keys+len, d_values); \
    DEF_BLOCKS_PER_GRID(len); \
	adjust_values<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, scratch+len, len); \
	CHECK(#_name " execution failed\n");			\
      } \
int _name##scratch(int len) {return (2*len); } \
make_inplace(_name, INPLACE_NONE)

#define make_seg(_name, _letter, _type, _first, _second)			\
    void _name(vec_p d, vec_p s, vec_p segd, int vec_len, int seg_count, vec_p scratch) \
    {                                                       \
	if (seg_count==0) {return;}					\
	bool ascending = (_second == 1); /*ascending*/			\
	SYNC();								\
	cpy_wu##_letter(scratch, s, vec_len, (vec_p)0);		\
	int offset = 0;							\
	thrust::host_vector<int> h_segd(seg_count);		\
	thrust::device_ptr<int> d_segd = thrust::device_pointer_cast ((int*)(ComputeMemory+segd)); \
	thrust::copy(d_segd+vec_len, d_segd+vec_len+seg_count, h_segd.begin());	\
	thrust::device_ptr<_type> d_keys = thrust::device_pointer_cast (((_type*)(ComputeMemory+scratch))); \
	thrust::device_ptr<int> d_values = thrust::device_pointer_cast (((int*)(ComputeMemory+scratch+vec_len))); \
	thrust::device_ptr<int> d_output = thrust::device_pointer_cast (((int*)(ComputeMemory+d))); \
        for (int i = 0; i < seg_count; i++) {				\
	    int len = h_segd[i];					\
	    if (ascending) {						\
		thrust::sequence (d_values+offset, d_values+offset+len, 0);		\
	    } else {							\
		thrust::sequence (d_values+offset, d_values+offset+len, (len-1), -1);	\
	    }								\
	    thrust::stable_sort_by_key(d_keys+offset, d_keys+offset+len, d_values+offset); \
        DEF_BLOCKS_PER_GRID(len); \
	    adjust_values<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d+offset, scratch+vec_len+offset, len); \
	    CHECK(#_name " execution failed\n");		\
	    offset += len;						\
	}								\
    }									\
    int _name##_scratch(int vec_len, int seg_count) {return (2*vec_len); } \
make_inplace(_name, INPLACE_NONE)

/* ----------------------- double rank -----------------------*/

/* This function does all the double ranks: up, down, segmented,
 * unsegmented.  Algorithm is: 
 * 1. xor the elements of the data to handle signed numbers correctly:
 *   if upward rank, then flip the sign bit
 *   if down rank, then flip all the bits except the sign bit
 * 2. Use field_rank to do the rank
 * 3. Restore the flipped bits from 1
 * 4. If the sort is segemented, then use the segment number to do
 *    an additional rank.
 * 5. Get the final rank out of tmp
 */

#include "rank.h"
