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

#define make_fpm(_name, _type)					\
    static cl_kernel _name##Kernel = NULL; \
	void _name(vec_p d, vec_p s, vec_p i, vec_p f, cl_int len_src, cl_int len_dest, vec_p scratch)	\
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
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &i); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &f); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &len_src); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &len_dest); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (len_dest == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(len_dest);	\
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
make_no2_scratch(_name)						\
make_inplace(_name, INPLACE_NONE)

#define make_seg_fpm(_name, _type, _unseg)				\
    static cl_kernel _name##Kernel = NULL; \
	void _name(vec_p d, vec_p s, vec_p i, vec_p f, vec_p sd_s, cl_int n_s, cl_int m_s, vec_p sd_d, cl_int n_d, cl_int m_d, vec_p scratch)	\
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
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &i); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &f); \
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
      err = CLFINISH(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      \
      return; \
      } \
make_seg2_scratch(_name, return s_vec_len;) \
make_inplace(_name, INPLACE_NONE)

#define make_bfp(_name, _type)					\
    static cl_kernel _name##Kernel = NULL; \
	void _name(vec_p d, vec_p s, vec_p i, vec_p f, cl_int len_src, cl_int len_dest, vec_p scratch)	\
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
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &i); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &f); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &len_src); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &len_dest); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (len_dest == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(len_dest);	\
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
make_no2_scratch(_name)						\
make_inplace(_name, INPLACE_NONE)

#define make_seg_bfp(_name, _type, _unseg)			\
    static cl_kernel _name##Kernel = NULL; \
    void _name(vec_p d, vec_p s, vec_p i, vec_p f, vec_p sd_s, cl_int n_s, cl_int m_s, vec_p sd_d, cl_int n_d, cl_int m_d, vec_p scratch)  \
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
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &i); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &f); \
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
      DBG_PRINT("DEBUG: NOTIMPL running %s\n", #_name);                                   \
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
    static cl_kernel dpe_pe1##_type_let##Kernel = NULL; \
    static cl_kernel dpe_pe2##_type_let##Kernel = NULL; \
void dpe_pe##_type_let (vec_p d, vec_p s, vec_p i, vec_p v, vec_p sd_s, cl_int n_s, cl_int m_s, vec_p sd_d, cl_int n_d, cl_int m_d, vec_p scratch)\
    {                                                       \
      unsigned int a = 0; \
\
      if (dpe_pe1##_type_let##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(dpe_pe1##_type_let)) != 0) i++; \
          dpe_pe1##_type_let##Kernel = ComputeKernels[i]; \
      } \
      if (dpe_pe2##_type_let##Kernel == NULL) { \
          int i = 0; \
          while (strcmp(KernelNames[i], QUOTED(dpe_pe2##_type_let)) != 0) i++; \
          dpe_pe2##_type_let##Kernel = ComputeKernels[i]; \
      } \
      int err = CL_SUCCESS; \
      err |= clSetKernelArg(dpe_pe1##_type_let##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(dpe_pe1##_type_let##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(dpe_pe1##_type_let##Kernel,  a++, sizeof(cl_int), &s); \
      err |= clSetKernelArg(dpe_pe1##_type_let##Kernel,  a++, sizeof(cl_int), &i); \
      err |= clSetKernelArg(dpe_pe1##_type_let##Kernel,  a++, sizeof(cl_int), &v); \
      err |= clSetKernelArg(dpe_pe1##_type_let##Kernel,  a++, sizeof(cl_int), &sd_s); \
      err |= clSetKernelArg(dpe_pe1##_type_let##Kernel,  a++, sizeof(cl_int), &n_s); \
      err |= clSetKernelArg(dpe_pe1##_type_let##Kernel,  a++, sizeof(cl_int), &m_s); \
      err |= clSetKernelArg(dpe_pe1##_type_let##Kernel,  a++, sizeof(cl_int), &sd_d); \
      err |= clSetKernelArg(dpe_pe1##_type_let##Kernel,  a++, sizeof(cl_int), &n_d); \
      err |= clSetKernelArg(dpe_pe1##_type_let##Kernel,  a++, sizeof(cl_int), &m_d); \
      err |= clSetKernelArg(dpe_pe1##_type_let##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: dpe_pe1%s: Failed to set kernel arguments!\n", #_type_let); \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (n_d == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(n_d);	\
      DBG_PRINT("DEBUG: running dpe_pe1%s\n", #_type_let);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, dpe_pe1##_type_let##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: dpe_pe1%s: Failed to execute kernel!\n", #_type_let);  \
        return; \
      } \
      err = CLFINISH(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing dpe_pe1%s\n", err, #_type_let); \
          return; \
      } \
      \
      err = CL_SUCCESS; \
      a = 0; \
      err |= clSetKernelArg(dpe_pe2##_type_let##Kernel,  a++, sizeof(cl_mem), &ComputeMemory); \
      err |= clSetKernelArg(dpe_pe2##_type_let##Kernel,  a++, sizeof(cl_int), &d); \
      err |= clSetKernelArg(dpe_pe2##_type_let##Kernel,  a++, sizeof(cl_int), &s); \
      err |= clSetKernelArg(dpe_pe2##_type_let##Kernel,  a++, sizeof(cl_int), &i); \
      err |= clSetKernelArg(dpe_pe2##_type_let##Kernel,  a++, sizeof(cl_int), &v); \
      err |= clSetKernelArg(dpe_pe2##_type_let##Kernel,  a++, sizeof(cl_int), &sd_s); \
      err |= clSetKernelArg(dpe_pe2##_type_let##Kernel,  a++, sizeof(cl_int), &n_s); \
      err |= clSetKernelArg(dpe_pe2##_type_let##Kernel,  a++, sizeof(cl_int), &m_s); \
      err |= clSetKernelArg(dpe_pe2##_type_let##Kernel,  a++, sizeof(cl_int), &sd_d); \
      err |= clSetKernelArg(dpe_pe2##_type_let##Kernel,  a++, sizeof(cl_int), &n_d); \
      err |= clSetKernelArg(dpe_pe2##_type_let##Kernel,  a++, sizeof(cl_int), &m_d); \
      err |= clSetKernelArg(dpe_pe2##_type_let##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: dpe_pe2%s: Failed to set kernel arguments!\n", #_type_let); \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (n_s == 0) return ; \
      workSize = ROUND_WARP_SIZE_UP(n_s);	\
      DBG_PRINT("DEBUG: running dpe_pe2%s\n", #_type_let);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, dpe_pe2##_type_let##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: dpe_pe%s: Failed to execute kernel!\n", #_type_let);  \
        return; \
      } \
      err = CLFINISH(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing dpe_pe2%s\n", err, #_type_let); \
          return; \
      } \
      \
      return; \
      } \


make_inplace(dpe_pez, INPLACE_3)
int dpe_pez_scratch(int n_in, int m_in, int n_out, int m_out) {return n_out;}

make_inplace(dpe_peb, INPLACE_3)
int dpe_peb_scratch(int n_in, int m_in, int n_out, int m_out) {return n_out;}

make_inplace(dpe_ped, INPLACE_3)
int dpe_ped_scratch(int n_in, int m_in, int n_out, int m_out) {return n_out;}

#define make_dfp(_type_let)					\
void dfp_pu##_type_let (vec_p d, vec_p s, vec_p i, vec_p f, vec_p v, cl_int len_src, cl_int len_dest, vec_p scratch)\
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
void dfp_pe##_type_let (vec_p d, vec_p s, vec_p i, vec_p f, vec_p v, vec_p sd_s, cl_int n_s, cl_int m_s, vec_p sd_d, cl_int n_d, cl_int m_d, vec_p scratch)\
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
