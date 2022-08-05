#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <memory.h>

#ifdef __linux
#include <CL/cl.h>
#else
#include <OpenCL/opencl.h>
#endif

#include <cvl.h>
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


#define make_rk(_name, _first, _second)            \
    static cl_kernel _name##Kernel = NULL; \
	void _name(vec_p d, vec_p s, cl_int len, vec_p scratch)	\
    {                                                       \
      unsigned int a = 0; \
      unsigned int first = _first; \
      unsigned int second = _second; \
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
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &first); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &second); \
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
      printf("DEBUG: NOTIMPL running %s\n", #_name);                                   \
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
int _name##scratch(int len) {return siz_foz(len); } \
make_inplace(_name, INPLACE_NONE)

#define make_seg(_name, _first, _second)            \
    static cl_kernel _name##Kernel = NULL; \
    void _name(vec_p d, vec_p s, vec_p segd, cl_int vec_len, cl_int seg_count, vec_p scratch) \
    {                                                       \
      unsigned int a = 0; \
      unsigned int first = _first; \
      unsigned int second = _second; \
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
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &segd); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &vec_len); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &seg_count); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &first); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &second); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (vec_len == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(vec_len);	\
      printf("DEBUG: NOTIMPL running %s\n", #_name);                                   \
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
    int _name##_scratch(int vec_len, int seg_count) {return 2*siz_foz(vec_len); }    \
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
