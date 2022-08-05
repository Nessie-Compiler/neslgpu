#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <memory.h>

#include <cvl.h>
#include <cutil_inline.h>

#include <thrust/device_vector.h>
#include <thrust/generate.h>
#include <thrust/random.h>

#include "defins.h"

thrust::host_vector<int> random_vector(const size_t N)
{
    thrust::host_vector<int> temp(N);
    for(size_t i = 0; i < N; i++) {
        temp[i] = random();
    }
    return temp;
}

template<typename T>
struct cpp_mod : public thrust::binary_function<T,T,T> {
    __host__ __device__ T operator()(const T &lhs, const T &rhs) const {return lhs%rhs;}
};
cpp_mod<int> cpp_mod_int;

void rnd_wuz(vec_p dest, vec_p src, int len, vec_p scratch)
{
    if (len==0) return;
    thrust::device_ptr<int> d_input = thrust::device_pointer_cast ((int*)(ComputeMemory+src)); 
    thrust::device_ptr<int> d_output = thrust::device_pointer_cast ((int*)(ComputeMemory+dest));
    thrust::device_vector<int> randoms = random_vector(len);
    thrust::transform(randoms.begin(), randoms.end(), d_input, d_output, cpp_mod_int);
    CHECK("rnd_wuz execution failed\n");    
}
make_no_scratch(rnd_wuz);
make_inplace(rnd_wuz, INPLACE_1);

#define onefun(_name, UNUSED1, _srctype, _desttype)                      \
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int); \
    void _name (vec_p d, vec_p s, int len, vec_p scratch)                         \
    {                                                       \
      DBG_PRINT("DEBUG: running %s: dst: %d, src: %d, len: %d\n", #_name, d, s, len); \
	if (len==0) {return;}						\
	SYNC();								\
    DEF_BLOCKS_PER_GRID(len); \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, len, scratch); \
      CHECK(#_name "execution failed\n"); \
    } \
    make_no_scratch(_name)                                  \
    make_inplace(_name,INPLACE_1)

#define twofun(_name, UNUSED1, _srctype, _desttype)                      \
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int); \
    void _name (vec_p d, vec_p s1, vec_p s2, int len, vec_p scratch) \
    {                                                       \
      DBG_PRINT("DEBUG: running %s: dst: %lld, src1: %lld, src2: %lld, len: %d\n", #_name, (long long)d, (long long)s1, (long long)s2, len); \
	if (len==0) {return;}						\
	SYNC();								\
    DEF_BLOCKS_PER_GRID(len); \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s1, s2, len, scratch); \
	CHECK(#_name "execution failed\n");			\
    } \
    make_no_scratch(_name)                                  \
    make_inplace(_name,INPLACE_1|INPLACE_2)

#define selfun(_name, UNUSED1, _type)                      \
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int); \
    void _name (vec_p d, vec_p s1, vec_p s2, vec_p s3, int len, vec_p scratch)   \
    {                                                       \
      DBG_PRINT("DEBUG: running %s: d: %lld, s1: %lld, s2: %lld, s3: %lld, len: %d\n", #_name, (long long)d, (long long)s1, (long long)s2, (long long)s3, len); \
	if (len==0) {return;}						\
	SYNC();								\
    DEF_BLOCKS_PER_GRID(len); \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s1, s2, s3, len, scratch); \
	CHECK(#_name "execution failed\n");			\
    } \
    make_no_scratch(_name)                                  \
    make_inplace(_name,INPLACE_1|INPLACE_2|INPLACE_3)
    
#include "elementwise.h"

void cpy_wus(vec_p d, vec_p s, int n, int m, vec_p scratch)
    {
	cpy_wuz(d, s, m, scratch);
    }
int cpy_wus_scratch(int n, int m) { return cpy_wuz_scratch(m); }
make_inplace(cpy_wus,INPLACE_1)
