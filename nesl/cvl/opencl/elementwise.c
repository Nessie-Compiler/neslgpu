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

#define onefun(_name, UNUSED1, _srctype, _desttype)                      \
    static cl_kernel _name##Kernel = NULL; \
    void _name (vec_p d, vec_p s, cl_int len, vec_p scratch)                         \
    {                                                       \
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
      size_t workSize = ROUND_WARP_SIZE_UP(len);	\
      DBG_PRINT("DEBUG: running %s: dst: %d, src: %d, len: %d\n", #_name, d, s, len); \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to execute kernel!\n", #_name);    \
        return; \
      } \
      err = CLFINISH(ComputeCommands);		\
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
      } \
    make_no_scratch(_name)                                  \
    make_inplace(_name,INPLACE_1)

#define twofun(_name, UNUSED1, _srctype, _desttype)                      \
    static cl_kernel _name##Kernel = NULL; \
    void _name (vec_p d, vec_p s1, vec_p s2, cl_int len, vec_p scratch) \
    {                                                       \
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
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s1); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s2); \
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
      DBG_PRINT("DEBUG: running %s: dst: %lld, src1: %lld, src2: %lld, len: %d\n", #_name, (long long)d, (long long)s1, (long long)s2, len); \
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
    make_no_scratch(_name)                                  \
    make_inplace(_name,INPLACE_1|INPLACE_2)

#define selfun(_name, UNUSED1, _type)                      \
    static cl_kernel _name##Kernel = NULL; \
    void _name (vec_p d, vec_p s1, vec_p s2, vec_p s3, cl_int len, vec_p scratch)   \
    {                                                       \
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
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s1); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s2); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &s3); \
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
      DBG_PRINT("DEBUG: running %s: d: %lld, s1: %lld, s2: %lld, s3: %lld, len: %d\n", #_name, (long long)d, (long long)s1, (long long)s2, (long long)s3, len); \
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
    make_no_scratch(_name)                                  \
    make_inplace(_name,INPLACE_1|INPLACE_2|INPLACE_3)
    
#include "elementwise.h"

void cpy_wus(vec_p d, vec_p s, int n, int m, vec_p scratch)
    {
	cpy_wuz(d, s, m, scratch);
    }
int cpy_wus_scratch(int n, int m) { return cpy_wuz_scratch(m); }
make_inplace(cpy_wus,INPLACE_1)
