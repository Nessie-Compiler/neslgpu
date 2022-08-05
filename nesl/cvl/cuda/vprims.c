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

template<typename T>
struct cpp_plus_ : public thrust::binary_function<T,T,T> {
    __host__ __device__ T operator()(const T &lhs, const T &rhs) const {return lhs+rhs;}
};
cpp_plus_<int> cpp_plus_int;
cpp_plus_<float> cpp_plus_float;

template<typename T>
struct cpp_times : public thrust::binary_function<T,T,T> {
    __host__ __device__ T operator()(const T &lhs, const T &rhs) const {return lhs*rhs;}
};
cpp_times<int> cpp_times_int;
cpp_times<float> cpp_times_float;

template<typename T>
struct cpp_mmin : public thrust::binary_function<T,T,T> {
    __host__ __device__ T operator()(const T &i, const T &j) const {return ((i) < (j) ? (i) : (j));}
};
cpp_mmin<int> cpp_mmin_int;
cpp_mmin<float> cpp_mmin_float;

template<typename T>
struct cpp_mmax : public thrust::binary_function<T,T,T> {
    __host__ __device__ T operator()(const T &i, const T &j) const {return ((i) > (j) ? (i) : (j));}
};
cpp_mmax<int> cpp_mmax_int;
cpp_mmax<float> cpp_mmax_float;

template<typename T>
struct cpp_aand : public thrust::binary_function<T,T,T> {
    __host__ __device__ T operator()(const T &lhs, const T &rhs) const {return lhs&&rhs;}
};
cpp_aand<int> cpp_aand_int;

template<typename T>
struct cpp_band : public thrust::binary_function<T,T,T> {
    __host__ __device__ T operator()(const T &lhs, const T &rhs) const {return lhs&rhs;}
};
cpp_band<int> cpp_band_int;

template<typename T>
struct cpp_oor : public thrust::binary_function<T,T,T> {
    __host__ __device__ T operator()(const T &lhs, const T &rhs) const {return lhs||rhs;}
};
cpp_oor<int> cpp_oor_int;

template<typename T>
struct cpp_bor : public thrust::binary_function<T,T,T> {
    __host__ __device__ T operator()(const T &lhs, const T &rhs) const {return lhs|rhs;}
};
cpp_bor<int> cpp_bor_int;

template<typename T>
struct cpp_lxor : public thrust::binary_function<T,T,T> {
    __host__ __device__ T operator()(const T &i, const T &j) const {return ((!!(i)) ^ (!!(j)));}
};
cpp_lxor<int> cpp_lxor_int;

template<typename T>
struct cpp_xxor : public thrust::binary_function<T,T,T> {
    __host__ __device__ T operator()(const T &lhs, const T &rhs) const {return lhs^rhs;}
};
cpp_xxor<int> cpp_xxor_int;

/* -----------------Unsegmented Scans----------------------------------*/

/* simple scan template:
   d = destination vector
   s = source vector
   len = length of d, s
   _init = initial value (identity element)
   d and s should be vectors of the same size and type

	    SYNC();							\
        DEF_BLOCKS_PER_GRID(len-1); \
    _name##Kernel<<<blocksPerGrid,THREADS_PER_BLOCK>>>(ComputeMemory, d, s, len, scratch); \

*/
#define simpscan(_name, _func, _type, _init)			\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int); \
    void _name(vec_p d, vec_p s, int len, vec_p scratch)				\
    	{							\
	    if (len == 0) return;					\
	    thrust::device_ptr<_type> d_input = thrust::device_pointer_cast ((_type*)(ComputeMemory+s)); \
	    thrust::device_ptr<_type> d_output = thrust::device_pointer_cast ((_type*)(ComputeMemory+d)); \
	    DBG_PRINT("DEBUG: running %s on len:%d\n", #_name, len);	\
	    thrust::exclusive_scan (d_input, d_input+len, d_output, _init, _func); \
	    CHECK(#_name " execution failed\n");		\
      } \
    make_no_scratch(_name)					\
    make_inplace(_name,INPLACE_1)

/* ----------------- Segmented Scans --------------------------*/

/* segmented simple scan template:
   d = destination vector
   s = source vector
   sd = segment descriptor of source
   n = number of elements in whole vector
   m = number of segments

   d and s should be vectors of the same size and type
*/
#define simpsegscan(_name, _funct, _type, _init, _unseg)		\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int); \
    void _name (vec_p d, vec_p s, vec_p sd, int n, int m, vec_p scratch)				\
    { 									\
	if (m == 0) return;						\
    if (m == 1) {\
    _unseg(d, s, n, scratch); \
    return; \
    }\
	thrust::device_ptr<_type> d_input = thrust::device_pointer_cast ((_type*)(ComputeMemory+s)); \
	thrust::device_ptr<int> d_flags = thrust::device_pointer_cast ((int*)(ComputeMemory+sd)); \
	thrust::device_ptr<_type> d_exclusive = thrust::device_pointer_cast ((_type*)(ComputeMemory+d)); \
	thrust::equal_to<_type> binary_pred;				\
	thrust::exclusive_scan_by_key(d_flags, d_flags+n, d_input, d_exclusive, _init, binary_pred, _funct); \
    }									\
    make_no_seg_scratch(_name)    \
    make_inplace(_name,INPLACE_NONE)

/* --------------------Reduce Functions--------------------------------*/
/* reduce template */

#define reduce(_name, _funct, _cudatype, _type, _identity)	    \
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int); \
    _type _name(vec_p s, int len, vec_p scratch)			\
    {							\
	if (len == 0) return (_type)0;					\
	thrust::device_ptr<_cudatype> d_input = thrust::device_pointer_cast ((_cudatype*)(ComputeMemory+s)); \
	DBG_PRINT("DEBUG: running %s size 1\n", #_name);		\
	_cudatype cudares = thrust::reduce (d_input, d_input+len, _identity, _funct); \
	CHECK(#_name " execution failed\n");			\
	_type result = (_type)cudares;						\
      return result;                                                    \
    }                                                                   \
    make_no_scratch(_name)   \
    make_inplace(_name,INPLACE_NONE)


/* ------------------Segmented Reduces ---------------------------------*/
/* segmented reduce template:
 *	d = destination vector
 *	s = source vector
 *	sd = segment descriptor of source, with components n and m
 */
/* see implementation note above */
#define segreduce(_name, _funct, _type, _identity, _unseg)	\
    extern __global__ void _name##KernelA(MAXALIGN*, int, int, int, int, int, int); \
    extern __global__ void _name##KernelOneSeg(MAXALIGN*, int, int, int, int, int, int); \
    extern __global__ void _name##KernelIdent(MAXALIGN*, int); \
    void _name (vec_p d, vec_p s, vec_p sd, int n, int m, vec_p scratch)		\
    {							\
        if (m == 0) return;           \
        if (n == 0) { \
            DBG_PRINT("DEBUG: handline zero-element case\n", #_name, m); \
            SYNC();                                                     \
            DEF_BLOCKS_PER_GRID(m);                                     \
            _name##KernelIdent<<<blocksPerGrid,THREADS_PER_BLOCK>>>(ComputeMemory, d); \
            return;   \
        } \
        if (m == 1) { \
	    thrust::device_ptr<_type> d_input = thrust::device_pointer_cast ((_type*)(ComputeMemory+s)); \
	    thrust::device_ptr<_type> d_inclusive = thrust::device_pointer_cast ((_type*)(ComputeMemory+scratch)); \
	    thrust::inclusive_scan(d_input, d_input+n, d_inclusive, _funct); \
	    DBG_PRINT("DEBUG: running %s on one segment\n", #_name, m); \
	    SYNC();							\
        DEF_BLOCKS_PER_GRID(m); \
	    _name##KernelOneSeg<<<blocksPerGrid,THREADS_PER_BLOCK>>>(ComputeMemory, d, scratch, sd, n, m, scratch); \
	    CHECK(#_name " execution failed\n");		\
        } else {                                                        \
	    thrust::device_ptr<_type> d_input = thrust::device_pointer_cast ((_type*)(ComputeMemory+s)); \
	    thrust::device_ptr<int> d_flags = thrust::device_pointer_cast ((int*)(ComputeMemory+sd)); \
	    thrust::device_ptr<_type> d_inclusive = thrust::device_pointer_cast ((_type*)(ComputeMemory+scratch)); \
	    thrust::equal_to<int> binary_pred; \
	    thrust::inclusive_scan_by_key(d_flags, d_flags+n, d_input, d_inclusive, binary_pred, _funct); \
	    thrust::device_ptr<int> d_ends = thrust::device_pointer_cast ((int*)(ComputeMemory+scratch+n)); \
	    thrust::inclusive_scan(d_flags+n, d_flags+n+m, d_ends); \
	    /* Reduction results are the last element of each segment. */ \
	    DBG_PRINT("DEBUG: running %s on %d segments\n", #_name, m); \
	    SYNC();							\
        DEF_BLOCKS_PER_GRID(m); \
	    _name##KernelA<<<blocksPerGrid,THREADS_PER_BLOCK>>>(ComputeMemory, d, scratch, sd, n, m, scratch); \
	    CHECK(#_name " execution failed\n");		\
        }\
      } \
    make_seg_scratch(_name, return (vec_len+seg_len);)    \
    make_inplace(_name,INPLACE_NONE)

/* -------------------Extract-------------------------------------*/

/* extract ith element from V */
/* extract template */
#define make_extract(_name, _type, _cltype)                                     \
    _type _name (vec_p v, int i, int len, vec_p scratch)			\
	{ \
    _type result; \
    _cltype clval; \
    DBG_PRINT("DEBUG: running %s, srcVec: %d, i:%d, len:%d\n", #_name, v, i, len); \
      cutilSafeCall( cudaMemcpy(&clval, ComputeMemory+v+i, sizeof(_cltype), cudaMemcpyDeviceToHost)); \
    result = (_type)clval;      \
    return result; \
	} \
    make_no_scratch(_name)				\
    make_inplace(_name,INPLACE_NONE)

/* segmented extract:
 *	d = destination vector (unsegmented),
 *	s = source vector (segmented, same type as d)
 *	i = index vector (unsegmented), length as d
 *  sd, n, m = segment descriptor for v
 */
#define make_seg_ext(_name, _type)			\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int, int); \
    void _name (vec_p d, vec_p s, vec_p i, vec_p sd, int n, int m, vec_p scratch)		\
    {	                                  		\
	DBG_PRINT("DEBUG: running %s on %d elements and %d segments\n", #_name, n, m); \
	if (n == 0) return;						\
	SYNC();								\
	thrust::device_ptr<int> d_input = thrust::device_pointer_cast ((int*)(ComputeMemory+sd+n)); \
	thrust::device_ptr<int> d_output = thrust::device_pointer_cast ((int*)(ComputeMemory+scratch)); \
	thrust::exclusive_scan (d_input, d_input+m, d_output); \
    DEF_BLOCKS_PER_GRID(m); \
	_name##Kernel<<<blocksPerGrid,THREADS_PER_BLOCK>>>(ComputeMemory, d, s, i, sd+n, n, m, scratch); \
	CHECK(#_name " execution failed\n");			\
      } \
    make_seg_scratch(_name, return seg_len;) \
    make_inplace(_name,INPLACE_2)

/* ------------------Replace-------------------------------------*/

/* replace ith element of V with val */

#define make_replace(_name, _type, _coerced, _funct)    \
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, _coerced, int, int); \
    void _name(vec_p v, int i, _type val, int len, vec_p scratch)	      		\
    {						\
	DBG_PRINT("DEBUG: running %s on %d elements\n", #_name, len); \
	if (len == 0) return;						\
      _coerced valCoerced = val; \
	SYNC();								\
	_name##Kernel<<<1,1>>>(ComputeMemory, v, i, valCoerced, len, scratch); \
	CHECK(#_name " execution failed\n");			\
      } \
    make_no_scratch(_name)			\
    make_inplace(_name,INPLACE_NONE)

/* segmented replace:
 *	d = destination vector  (segmented)
 *	s = index vector	(unsegmented, one entry per segment of d)
 *	v = value vector    (ditto)
 *	sd, n, m = segment descriptor for d.
 */
#define make_seg_replace(_name, _type)		\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int, int); \
    void _name(vec_p d, vec_p s, vec_p v, vec_p sd, int n, int m, vec_p scratch)	\
    {						\
	if (n == 0) return;						\
	SYNC();								\
	thrust::device_ptr<int> d_input = thrust::device_pointer_cast ((int*)(ComputeMemory+sd+n)); \
	thrust::device_ptr<int> d_output = thrust::device_pointer_cast ((int*)(ComputeMemory+scratch)); \
	thrust::exclusive_scan (d_input, d_input+m, d_output); \
    DEF_BLOCKS_PER_GRID(m); \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, v, sd+n, n, m, scratch); \
	CHECK(#_name " execution failed\n");			\
      } \
    make_seg_scratch(_name, return seg_len;) \
    make_inplace(_name,INPLACE_NONE)

/* ----------------Distribute-----------------------------------*/

/* distribute v to length len, return in d */
#define make_distribute(_name, _type, _cltype)     \
    extern __global__ void _name##Kernel(MAXALIGN*, int, _cltype, int, int); \
    void _name(vec_p d, _type v, int len, vec_p scratch)		\
    { 						\
	if (len == 0) return;						\
	_cltype clv = v;						\
    DEF_BLOCKS_PER_GRID(len); \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, clv, len, scratch); \
	CHECK(#_name " execution failed\n");			\
      } \
    make_no_scratch(_name)			\
    make_inplace(_name,INPLACE_NONE)

/* segmented distribute:
 *  d = destination vector (segmented)
 *  v = value vector (unsegmented), same type as d
 *  sd, n, m = segment descriptor for d
 */
#define make_seg_distribute(_name, _type, _unseg)	\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int); \
    void _name(vec_p d, vec_p v, vec_p sd, int n, int m, vec_p scratch) 	\
    {						\
	if (n == 0) return;						\
	DBG_PRINT("DEBUG: running %s, n=%d\n", #_name, n);		\
    DEF_BLOCKS_PER_GRID(n); \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, v, sd, n, m, scratch); \
	CHECK(#_name " execution failed\n");			\
      } \
    make_no_seg_scratch(_name)    \
    make_inplace(_name,INPLACE_NONE)

/* --------------Permute---------------------------------------*/

/* simple permute: 
 *	d = destination vector
 *	s = source vector, same type as d
 *	i = index vector
 *	len = length of vectors
 */

#define make_smpper(_name, _type)			\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int); \
    void _name(vec_p d, vec_p s, vec_p i, int len, vec_p scratch)			\
    {							\
	if (len == 0) return;						\
	DBG_PRINT("DEBUG: running %s, n=%d\n", #_name, len);		\
    DEF_BLOCKS_PER_GRID(len); \
	_name##Kernel<<<blocksPerGrid,THREADS_PER_BLOCK>>>(ComputeMemory, d, s, i, len, scratch); \
	CHECK(#_name " execution failed\n");			\
      } \
    make_no_scratch(_name)				\
    make_inplace(_name,INPLACE_NONE)

/* segmented simple permute:
 *  d = destination vector (segmented)
 *  s = source vector (segmented), same type as d
 *  i = index vector (segmented)
 *  sd, n, m = segment descriptor
 */
#define make_seg_smpper(_name, _type, _unseg)		\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int, int); \
    void _name(vec_p d, vec_p s, vec_p i, vec_p sd, int n, int m, vec_p scratch)		\
    {							\
	if (n == 0) return;						\
    if (m == 1) {                                \
        _unseg(d, s, i, n, scratch);             \
        return;                                  \
    }\
	thrust::device_ptr<int> d_input = thrust::device_pointer_cast ((int*)(ComputeMemory+sd+n)); \
	thrust::device_ptr<int> d_exclusive = thrust::device_pointer_cast ((int*)(ComputeMemory+scratch)); \
	thrust::exclusive_scan(d_input, d_input+m, d_exclusive); \
	DBG_PRINT("DEBUG: running %s, n=%d\n", #_name, n);		\
    DEF_BLOCKS_PER_GRID(n); \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, i, sd, n, m, scratch); \
	CHECK(#_name " execution failed\n");			\
      } \
    make_seg_scratch(_name, return (seg_len);)	\
    make_inplace(_name,INPLACE_NONE)

/*----------------------Back Permute-------------------*/
/* back permute: 
 *	d = destination vector
 *	s = source vector, same type as d
 *	i = index vector
 *	s_len = length of s
 *	d_len = length of d and i
 */

#define make_bckper(_name, _type)			\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int); \
	void _name(vec_p d, vec_p s, vec_p i, int s_len, int d_len, vec_p scratch)	\
	{						\
	if (d_len == 0) return;						\
    DEF_BLOCKS_PER_GRID(d_len); \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, i, s_len, d_len, scratch); \
	CHECK(#_name " execution failed\n");			\
      } \
	make_no2_scratch(_name)				\
	make_inplace(_name,INPLACE_2)

/* segmented bck permute:
 *  d = destination vector (segmented)
 *  s = source vector (segmented), same type as d
 *  i = index vector (compatible with d)
 *  sd_s, n_s, m_s = source segment descriptor
 *  sd_d, n_d, n_d = dest segment descriptor
 */
#define make_seg_bckper(_name, _type, _unseg)	\
    extern __global__ void _name##Kernel(MAXALIGN*, int, int, int, int, int, int, int, int, int, int); \
    void _name(vec_p d, vec_p s, vec_p i, vec_p sd_s, int n_s, int m_s, vec_p sd_d, int n_d, int m_d, vec_p scratch)	\
    {						\
	if (n_d == 0) return;						\
    if ((m_s == 1) && (m_d == 1)) {                 \
        _unseg(d, s, i, n_s, n_d, scratch);         \
        return;                                     \
    }\
	thrust::device_ptr<int> d_input = thrust::device_pointer_cast ((int*)(ComputeMemory+sd_s+n_s)); \
	thrust::device_ptr<int> d_output = thrust::device_pointer_cast ((int*)(ComputeMemory+scratch)); \
	thrust::exclusive_scan (d_input, d_input+m_s, d_output); \
    DEF_BLOCKS_PER_GRID(n_d); \
	_name##Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s, i, sd_s, n_s, m_s, sd_d, n_d, m_d, scratch); \
	CHECK(#_name " execution failed\n");			\
      } \
    make_seg2_scratch(_name, return (s_seg_len*2);)	\
    make_inplace(_name,INPLACE_NONE)


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

#include "vprims.h"
