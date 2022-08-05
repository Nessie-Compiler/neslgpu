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

/* This file contains some library functions */

/*----------------------------index (iota)-----------------*/
/* Index(len) creates an integer vector of the numbers */

static cl_kernel ind_luzKernel = NULL;                                  
void ind_luz(vec_p d, cl_int init, cl_int stride, cl_int count, vec_p scratch)
{                                 
      unsigned int a = 0; 

      if (ind_luzKernel == NULL) { 
          int i = 0; 
          while (strcmp(KernelNames[i], "ind_luz") != 0) i++; 
          ind_luzKernel = ComputeKernels[i]; 
      } 
      int err = CL_SUCCESS; 
      err |= clSetKernelArg(ind_luzKernel,  a++, sizeof(cl_mem), &ComputeMemory); 
      err |= clSetKernelArg(ind_luzKernel,  a++, sizeof(cl_int), &d); 
      err |= clSetKernelArg(ind_luzKernel,  a++, sizeof(cl_int), &init); 
      err |= clSetKernelArg(ind_luzKernel,  a++, sizeof(cl_int), &stride); 
      err |= clSetKernelArg(ind_luzKernel,  a++, sizeof(cl_int), &count); 
      err |= clSetKernelArg(ind_luzKernel,  a++, sizeof(cl_int), &scratch); 
      
      if (err != CL_SUCCESS) 
      { 
        printf("Error: ind_luz: Failed to set kernel arguments!\n"); 
        return; 
      }

      err = CL_SUCCESS; 
      if (count == 0) return ;
      size_t workSize = ROUND_WARP_SIZE_UP(count);
      DBG_PRINT("DEBUG: running ind_luz: %lld, stride: %d, count %d\n", (long long)d, stride, count);
      err |= clEnqueueNDRangeKernel(ComputeCommands, ind_luzKernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); 
      if (err != CL_SUCCESS) 
      { 
        printf("Error: ind_luz: Failed to execute kernel!\n"); 
        return; 
      }

      err = CLFINISH(ComputeCommands);
      if (err != CL_SUCCESS) 
      { 
          printf("Error: %d finishing ind_luz\n", err); 
          return; 
      }
      
      return; 
} 
make_no_scratch(ind_luz)
make_inplace(ind_luz,INPLACE_NONE)

/* Segmented index creates a segmented vector of index results, given a 
 * vector of lengths.
 */
static cl_kernel ind_lezKernel = NULL;
void ind_lez(vec_p d, vec_p init, vec_p stride, vec_p dest_segd, cl_int vec_len, cl_int seg_count, vec_p scratch)    
{                                 
      unsigned int a = 0; 

      if (ind_lezKernel == NULL) { 
          int i = 0; 
          while (strcmp(KernelNames[i], "ind_lez") != 0) i++; 
          ind_lezKernel = ComputeKernels[i]; 
      } 
      int err = CL_SUCCESS; 
      err |= clSetKernelArg(ind_lezKernel,  a++, sizeof(cl_mem), &ComputeMemory); 
      err |= clSetKernelArg(ind_lezKernel,  a++, sizeof(cl_int), &d); 
      err |= clSetKernelArg(ind_lezKernel,  a++, sizeof(cl_int), &init); 
      err |= clSetKernelArg(ind_lezKernel,  a++, sizeof(cl_int), &stride); 
      err |= clSetKernelArg(ind_lezKernel,  a++, sizeof(cl_int), &dest_segd); 
      err |= clSetKernelArg(ind_lezKernel,  a++, sizeof(cl_int), &vec_len); 
      err |= clSetKernelArg(ind_lezKernel,  a++, sizeof(cl_int), &seg_count); 
      err |= clSetKernelArg(ind_lezKernel,  a++, sizeof(cl_int), &scratch); 
      
      if (err != CL_SUCCESS) 
      { 
        printf("Error: ind_lez: Failed to set kernel arguments!\n"); 
        return; 
      }

      err = CL_SUCCESS; 
      if (vec_len == 0) return ; 
      size_t workSize = ROUND_WARP_SIZE_UP(vec_len);
      DBG_PRINT("DEBUG: running ind_lez %d\n", (int)workSize); 
      err |= clEnqueueNDRangeKernel(ComputeCommands, ind_lezKernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); 
      if (err != CL_SUCCESS) 
      { 
        printf("Error: ind_lez: Failed to execute kernel!\n"); 
        return; 
      } 
      
      err = CLFINISH(ComputeCommands);
      if (err != CL_SUCCESS) 
      { 
          printf("Error: %d finishing ind_lez\n", err); 
          return; 
      }
      return; 
} 
make_no_seg_scratch(ind_lez)
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

static cl_kernel pk1_luvKernel = NULL;          \
int pk1_luv(vec_p f, cl_int len, vec_p scratch)
{                                 
      unsigned int a = 0;
      unsigned int result = 0;

      if (pk1_luvKernel == NULL) { 
          int i = 0; 
          while (strcmp(KernelNames[i], "pk1_luv") != 0) i++; 
          pk1_luvKernel = ComputeKernels[i]; 
      } 
      int err = CL_SUCCESS; 
      err |= clSetKernelArg(pk1_luvKernel,  a++, sizeof(cl_mem), &ComputeMemory); 
      err |= clSetKernelArg(pk1_luvKernel,  a++, sizeof(cl_int), &f); 
      err |= clSetKernelArg(pk1_luvKernel,  a++, sizeof(cl_int), &len); 
      err |= clSetKernelArg(pk1_luvKernel,  a++, sizeof(cl_int), &scratch); 
      err |= clSetKernelArg(pk1_luvKernel,  a++, sizeof(cl_int)*len, NULL);
      
      if (err != CL_SUCCESS) 
      { 
        printf("Error: pk1_luv: Failed to set kernel arguments!\n"); 
        return 0;

      }

      err = CL_SUCCESS; 
      if (len == 0) return 0; 
      size_t workSize = ROUND_WARP_SIZE_UP(len);
      err |= clEnqueueNDRangeKernel(ComputeCommands, pk1_luvKernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); 
      if (err != CL_SUCCESS) 
      { 
        printf("Error: pk1_luv: Failed to execute kernel!\n"); 
        return 0;
      } 
      
      err = CLFINISH(ComputeCommands);
      if (err != CL_SUCCESS) 
      { 
          printf("Error: %d finishing pk1_luv\n", err); 
          return 0; 
      }

      err = clEnqueueReadBuffer(ComputeCommands, ComputeMemory, TRUE, ((size_t)scratch)*sizeof(MAXALIGN),
                                sizeof(unsigned int), (void *)&result, 0, NULL, NULL);
      assert (err == CL_SUCCESS);
      err = CLFINISH (ComputeCommands);
      assert (err == CL_SUCCESS); 

      DBG_PRINT("DEBUG: returning %d from pk1_luv\n", result);
      return result;
}
make_scratch(pk1_luv, return (len);)
make_inplace(pk1_luv,INPLACE_1)

#define make_pk2(_name, _type)					\
    static cl_kernel _name##Kernel = NULL; \
	void _name (vec_p d, vec_p s, vec_p f, cl_int src_len, cl_int dest_len, vec_p scratch)	\
{								\
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
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &f); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &src_len); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &dest_len); \
      err |= clSetKernelArg(_name##Kernel,  a++, sizeof(cl_int), &scratch); \
      \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to set kernel arguments!\n", #_name);  \
        return; \
      }\
\
      err = CL_SUCCESS; \
      if (dest_len == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(1);	\
      DBG_PRINT("DEBUG: running %s\n", #_name);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to execute kernel!\n", #_name);    \
        return; \
      } \
      \
      err = CLFINISH(ComputeCommands); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %d finishing %sn", err, #_name);  \
          return; \
      }\
      return; \
      } \
	make_no_scratch(_name)					\
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

static cl_kernel pk1_levKernel = NULL;                                  
void pk1_lev(vec_p ds, vec_p f, vec_p sd, cl_int n, cl_int m, vec_p scratch)
{		
      unsigned int a = 0; 

      if (pk1_levKernel == NULL) { 
          int i = 0; 
          while (strcmp(KernelNames[i], "pk1_lev") != 0) i++; 
          pk1_levKernel = ComputeKernels[i]; 
      } 
      int err = CL_SUCCESS; 
      err |= clSetKernelArg(pk1_levKernel,  a++, sizeof(cl_mem), &ComputeMemory); 
      err |= clSetKernelArg(pk1_levKernel,  a++, sizeof(cl_int), &ds); 
      err |= clSetKernelArg(pk1_levKernel,  a++, sizeof(cl_int), &f); 
      err |= clSetKernelArg(pk1_levKernel,  a++, sizeof(cl_int), &sd); 
      err |= clSetKernelArg(pk1_levKernel,  a++, sizeof(cl_int), &n); 
      err |= clSetKernelArg(pk1_levKernel,  a++, sizeof(cl_int), &m); 
      err |= clSetKernelArg(pk1_levKernel,  a++, sizeof(cl_int), &scratch); 
      
      if (err != CL_SUCCESS) 
      { 
        printf("Error: pk1_lev: Failed to set kernel arguments!\n"); 
        return; 
      }

      err = CL_SUCCESS; 
      if (n == 0) return ; \
      size_t workSize = ROUND_WARP_SIZE_UP(m);
      DBG_PRINT("DEBUG: running pk1_lev %d segments\n", (int)workSize);                                   
      err |= clEnqueueNDRangeKernel(ComputeCommands, pk1_levKernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); 
      if (err != CL_SUCCESS) 
      { 
        printf("Error: pk1_lev: Failed to execute kernel!\n"); 
        return; 
      } 
      
      err = CLFINISH(ComputeCommands);
      if (err != CL_SUCCESS) 
      { 
          printf("Error: %d finishing pk1_lev\n", err); 
          return; 
      }
      return; 
} 
make_seg_scratch(pk1_lev, return vec_len;)
make_inplace(pk1_lev, INPLACE_1)

/* pack2: returns the pack 
 */
#define make_pack2(_name, _type)				\
    static cl_kernel _name##Kernel = NULL; \
	void _name (vec_p d, vec_p s, vec_p f, vec_p sd_s, cl_int n_s , cl_int m_s, vec_p sd_d, cl_int n_d, cl_int m_d, vec_p scratch)\
    {								\
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
      size_t workSize = ROUND_WARP_SIZE_UP(m_d);	\
      DBG_PRINT("DEBUG: running %s\n", #_name);                                   \
      err |= clEnqueueNDRangeKernel(ComputeCommands, _name##Kernel, 1, NULL, &workSize, NULL, 0, NULL, NULL); \
      if (err != CL_SUCCESS) \
      { \
          printf("Error: %s: Failed to execute kernel!\n", #_name);    \
        return; \
      } \
      \
      err = CLFINISH(ComputeCommands); \
      if (err != CL_SUCCESS)  \
      {  \
          printf("Error: %d finishing %s\n", err, #_name); \
          return; \
      } \
      return; \
      } \
    make_seg2_scratch(_name, return s_vec_len;) \
    make_inplace(_name, INPLACE_NONE)

make_pack2(pk2_lez, int)
make_pack2(pk2_leb, cvl_bool)
make_pack2(pk2_led, double)

