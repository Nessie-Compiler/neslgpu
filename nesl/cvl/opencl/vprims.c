#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <memory.h>

#ifdef __linux
#include <CL/cl.h>
#else
#include <OpenCL/opencl.h>
#endif

#include "defins.h"
#include <cvl.h>

/* -----------------Unsegmented Scans----------------------------------*/

/* simple scan template:
   d = destination vector
   s = source vector
   len = length of d, s
   _init = initial value (identity element)
   d and s should be vectors of the same size and type
*/
#define simpscan(_name, _func, _type, _init)			\
    static cl_kernel _name##Kernel = NULL; \
    void _name(vec_p d, vec_p s, cl_int len, vec_p scratch)				\
    	{							\
      unsigned int a = 0; \
\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &len); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (len == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(len-1);	\
      DBG_PRINT("DEBUG: running %s\n", #_name);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to execute kernel!\n", #_name);    \
        return; \
      } \
      err = CLFINISH(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
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
    static cl_kernel _name##Kernel = NULL; \
    void _name (vec_p d, vec_p s, vec_p sd, cl_int n, cl_int m, vec_p scratch)				\
    { 									\
      unsigned int a = 0; \
\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &sd); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &n); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &m); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
        printf("Error: %s: Failed to set kernel arguments!\n", #_name); \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (n == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(m);	\
      DBG_PRINT("DEBUG: running %s\n", #_name);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
        printf("Error: %s: Failed to execute kernel!\n", #_name); \
        return; \
      } \
      err = CLFINISH(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing1 %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
      } \
    make_seg_scratch(_name, return vec_len;)    \
    make_inplace(_name,INPLACE_NONE)


unsigned int nextPow2( unsigned int x ) {
    --x;
    x |= x >> 1;
    x |= x >> 2;
    x |= x >> 4;
    x |= x >> 8;
    x |= x >> 16;
    return ++x;
}

/* --------------------Reduce Functions--------------------------------*/
/* reduce template */
#define maxThreads 128
	
#define reduce(_name, _funct, _type, _identity)         \
    static cl_kernel _name##Kernel = NULL; \
    _type _name(vec_p s, cl_int len, vec_p scratch)			\
    {							\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      if (len == 0) { len = 1; } \
      int err = CL_SUCCESS; \
      vec_p d = scratch; \
      err |= clSetKernelArg(_name##Kernel,  0, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  1, sizeof(cl_int), &s); \
      err |= clSetKernelArg(_name##Kernel,  2, sizeof(cl_int), &d); \
      err |= clSetKernelArg(_name##Kernel,  3, sizeof(cl_int), &len); \
      err |= clSetKernelArg(_name##Kernel,  4, sizeof(_type)*1, NULL); \
      \
      \
      if (err != CL_SUCCESS) \
      { \
        printf("Error: %s: Failed to set kernel arguments!\n", #_name); \
        return (_type)0; \
      }\
\
      err = CL_SUCCESS; \
      size_t workSize = ROUND_WARP_SIZE_UP(len);	\
      DBG_PRINT("DEBUG: running %s\n", #_name);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
        printf("Error: %s: Failed to execute kernel!\n", #_name); \
        return (_type)0; \
      } \
      err = CLFINISH(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return (_type)0; \
      } \
      _type result;                                                   \
      err = clEnqueueReadBuffer(ComputeCommands, ComputeMemory, TRUE, ((size_t)d)*sizeof(MAXALIGN), sizeof(_type), (void *)&result, 0, NULL, NULL); \
      if (err != CL_SUCCESS)                                            \
      {                                                                 \
          printf("Error: %d: Failed to queue copy!\n", err);            \
          return (_type)0;                                              \
      }                                                                 \
      err = clFinish(ComputeCommands);                                  \
      if (err != CL_SUCCESS)                                            \
      {                                                                 \
          printf("Error: %d finishing copy\n", err);                    \
          return (_type)0;                                              \
      }                                                                 \
      return result;                                                    \
    }                                                                   \
\
    make_scratch(_name,return (len);)   \
    make_inplace(_name,INPLACE_NONE)


/* ------------------Segmented Reduces ---------------------------------*/
/* segmented reduce template:
 *	d = destination vector
 *	s = source vector
 *	sd = segment descriptor of source, with components n and m
 */
/* see implementation note above */
#define segreduce(_name, _funct, _type, _identity, _unseg)	\
    static cl_kernel _name##Kernel = NULL; \
    void _name (vec_p d, vec_p s, vec_p sd, cl_int n, cl_int m, vec_p scratch)		\
    {							\
      unsigned int a = 0; \
\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &sd); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &n); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &m); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
        printf("Error: %s: Failed to set kernel arguments!\n", #_name); \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (m == 0) return ;  \
      size_t workSize = ROUND_WARP_SIZE_UP(m);	\
      DBG_PRINT("DEBUG: running %s\n", #_name);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
        printf("Error: %s: Failed to execute kernel!\n", #_name); \
        return; \
      } \
      err = clFinish(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
      } \
    make_seg_scratch(_name, return (vec_len*2);)    \
    make_inplace(_name,INPLACE_NONE)

/* -------------------Extract-------------------------------------*/

/* extract ith element from V */
/* extract template */
#define make_extract(_name, _type, _cltype)                                     \
    _type _name (vec_p v, cl_int i, cl_int len, vec_p scratch)			\
	{ \
    _type result; \
    _cltype clval; \
    cl_int err; \
    DBG_PRINT("DEBUG: running %s, srcVec: %d, i:%d, len:%d\n", #_name, v, i, len); \
    err = clEnqueueReadBuffer(ComputeCommands, ComputeMemory, TRUE, ((size_t)v)*sizeof(MAXALIGN) + i*sizeof(_cltype), sizeof(_cltype), (void *)&clval, 0, NULL, NULL); \
    assert (err == CL_SUCCESS);  \
    err = clFinish (ComputeCommands); \
    assert (err == CL_SUCCESS); \
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
    static cl_kernel _name##Kernel = NULL; \
    void _name (vec_p d, vec_p s, vec_p i, vec_p sd, cl_int n, cl_int m, vec_p scratch)		\
    {	                                  		\
      unsigned int a = 0; \
\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &i); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &sd); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &n); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &m); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
        printf("Error: %s: Failed to set kernel arguments!\n", #_name); \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (n == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(n);	\
      DBG_PRINT("DEBUG: running %s on %d elements and %d segments\n", #_name, n, m); \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
        printf("Error: %s: Failed to execute kernel!\n", #_name); \
        return; \
      } \
      err = clFinish(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
      } \
    make_no_seg_scratch(_name)				\
    make_inplace(_name,INPLACE_2)

/* ------------------Replace-------------------------------------*/

/* replace ith element of V with val */

#define make_replace(_name, _type, _coerced, _funct)    \
    static cl_kernel _name##Kernel = NULL; \
    void _name(vec_p v, cl_int i, _type val, cl_int len, vec_p scratch)	      		\
    {						\
      unsigned int a = 0; \
\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      _coerced valCoerced = val; \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &v); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &i); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(_coerced), &valCoerced); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &len); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (len == 0) return ;    \
      size_t workSize = ROUND_WARP_SIZE_UP(1);	\
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to execute kernel!\n", #_name);    \
        return; \
      } \
      err = clFinish(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
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
    static cl_kernel _name##Kernel = NULL; \
    void _name(vec_p d, vec_p s, vec_p v, vec_p sd, cl_int n, cl_int m, vec_p scratch)	\
    {						\
      unsigned int a = 0; \
\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &v); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &sd); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &n); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &m); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (n == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(m);	\
      DBG_PRINT("DEBUG: running %s\n", #_name);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to execute kernel!\n", #_name);    \
        return; \
      } \
      err = clFinish(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
      } \
    make_no_seg_scratch(_name)			\
    make_inplace(_name,INPLACE_NONE)

/* ----------------Distribute-----------------------------------*/

/* distribute v to length len, return in d */
#define make_distribute(_name, _type, _cltype)     \
    static cl_kernel _name##Kernel = NULL; \
    void _name(vec_p d, _type v, cl_int len, vec_p scratch)		\
    { 						\
      unsigned int a = 0; \
\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      _cltype clv = v; \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(_cltype), &clv); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &len); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (len == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(len);	\
      DBG_PRINT("DEBUG: running %s\n", #_name);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to execute kernel!\n", #_name);    \
        return; \
      } \
      err = clFinish(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
      } \
    make_no_scratch(_name)			\
    make_inplace(_name,INPLACE_NONE)

/* segmented distribute:
 *  d = destination vector (segmented)
 *  v = value vector (unsegmented), same type as d
 *  sd, n, m = segment descriptor for d
 */
#define make_seg_distribute(_name, _type, _unseg)	\
    static cl_kernel _name##Kernel = NULL; \
    void _name(vec_p d, vec_p v, vec_p sd, cl_int n, cl_int m, vec_p scratch) 	\
    {						\
      unsigned int a = 0; \
\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &v); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &sd); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &n); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &m); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (n==0) return ;            \
      size_t workSize = ROUND_WARP_SIZE_UP(n);	\
      DBG_PRINT("DEBUG: running %s\n", #_name);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to execute kernel!\n", #_name);    \
        return; \
      } \
      err = clFinish(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
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
    static cl_kernel _name##Kernel = NULL; \
    void _name(vec_p d, vec_p s, vec_p i, cl_int len, vec_p scratch)			\
    {							\
      unsigned int a = 0; \
\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &i); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &len); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (len == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(len);	\
      DBG_PRINT("DEBUG: running %s\n", #_name);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to execute kernel!\n", #_name);    \
        return; \
      } \
      err = clFinish(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
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
    static cl_kernel _name##Kernel = NULL; \
    void _name(vec_p d, vec_p s, vec_p i, vec_p sd, cl_int n, cl_int m, vec_p scratch)		\
    {							\
      unsigned int a = 0; \
\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &i); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &sd); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &n); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &m); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (n == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(n);	\
      DBG_PRINT("DEBUG: running %s, m=%d\n", #_name, m);                     \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to execute kernel!\n", #_name);    \
        return; \
      } \
      err = clFinish(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
      } \
    make_no_seg_scratch(_name)				\
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
    static cl_kernel _name##Kernel = NULL; \
	void _name(vec_p d, vec_p s, vec_p i, cl_int s_len, cl_int d_len, vec_p scratch)	\
	{						\
      unsigned int a = 0; \
\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &i); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s_len); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &d_len); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (d_len == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(d_len);	\
      DBG_PRINT("DEBUG: running %s\n", #_name);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to execute kernel!\n", #_name);    \
        return; \
      } \
      err = clFinish(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
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
    static cl_kernel _name##Kernel = NULL; \
    void _name(vec_p d, vec_p s, vec_p i, vec_p sd_s, cl_int n_s, cl_int m_s, vec_p sd_d, cl_int n_d, cl_int m_d, vec_p scratch)	\
    {						\
      unsigned int a = 0; \
\
      if (_name##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(_name)) != 0) i++; \
          _name##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &i); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &sd_s); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &n_s); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &m_s); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &sd_d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &n_d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &m_d); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (n_d == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(n_d);	\
      DBG_PRINT("DEBUG: running %s\n", #_name);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to execute kernel!\n", #_name);    \
        return; \
      } \
      err = clFinish(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name);   \
          return; \
      } \
      \
      return; \
      } \
    make_no_seg2_scratch(_name)			\
    make_inplace(_name,INPLACE_2)


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
