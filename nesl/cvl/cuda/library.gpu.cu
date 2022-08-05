//typedef union maxaligned { int i; float f;} MAXALIGN;
//typedef int cvl_bool;

/* This file contains some library functions */

/*----------------------------index (iota)-----------------*/
/* Index(len) creates an integer vector of the numbers */
#include "defins.cuh"

__global__ void ind_luzKernel(MAXALIGN *data,
                            int d,
                            int init,
                            int stride,
                            int count,
                            int scratch)
{
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;           
    
    if (address < count) { 
        uint *pDst = (uint*)(&data[d]); 
        pDst[address] = init+(address*stride);    
    } 
    return; 
}

/* Segmented index creates a segmented vector of index results, given a 
 * vector of lengths.
 */
__global__ void ind_lezKernel(MAXALIGN *data,
                            int d,
                            int init,
                            int stride,
                            int dest_segd,
                            int vec_len,
                            int m,
                            int scratch)
{
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
    if (address >= vec_len) { return; }

    uint *pDst = (uint*)(&data[d]); 
    uint *pInit = (uint*)(&data[init]); 
    uint *pStride = (uint*)(&data[stride]); 
    uint *pSD = (uint*)(&data[dest_segd]);
    uint *pOffset = (uint*)(&data[scratch]);

    uint segment = pSD[address];
    pDst[address] = pInit[segment]+pOffset[address]*pStride[segment];
}

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

__global__ void pk1_luvKernel (MAXALIGN *data,
			       int f,
			       int len,
			       int s)
{
    int global_index = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
    if (global_index >= len) { return; }
    if (global_index == 0) {                                
        int *g_idata = (int*)(&data[f]);              
        int *g_odata = (int*)(&data[s]);              
        int sum = 0;                                        
        int k = 0;                                 
        for (k = 0; k < len; k++) {                         
            sum += g_idata[k];                              
        }                                                   
        g_odata[0] = sum;                                   
     }
}


#define make_pk2(_name, _type)					\
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int d, \
    int s, \
    int f, \
    int src_len, \
    int dest_len, \
    int scratch) \
{\
 uint *pDst = (uint*)(&data[d]);      \
 uint *pSrc = (uint*)(&data[s]);      \
 uint *pFlags = (uint*)(&data[f]);    \
 int dIndex = 0;                                        \
 for (int i = 0; i < src_len; i++) {                    \
     if (pFlags[i] != 0) {                              \
         pDst[dIndex] = pSrc[i];                        \
         dIndex++;                                      \
     }                                                  \
 }                                                      \
 return ;                                               \
}

make_pk2(pk2_luz, int)
make_pk2(pk2_lub, cvl_bool)
make_pk2(pk2_lud, double)

/* segmented pack: Packs a segmented vector into dest and creates
 * segment descriptor for it in seg_dest
 * Pack is split up into two parts.  The first takes the 
 * packing flag vector and its segd and returns the lengths vector
 * describing the result of the final pack.
 */

/* pack2: returns the pack 
 */
#define make_pack2(_name, _type)				\
__global__ void _name##Kernel( \
    MAXALIGN *data, \
    int d, \
    int s, \
    int f, \
    int sd_s,\
    int n_s,\
    int m_s,\
    int sd_d,\
    int n_d,\
    int m_d,\
    int scratch) \
{\
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;    \
    if (address >= n_s) { return; }			    \
    _type *pDst = (_type*)(&data[d]);			    \
    _type *pSrc = (_type*)(&data[s]);			    \
    uint *pFlags = (uint*)(&data[f]);			    \
    uint *pScratch = (uint*)(&data[scratch]);		    \
    if (pFlags[address] != 0) {				    \
	pDst[pScratch[address]-1] = pSrc[address];	    \
    }							    \
    return ;                                                \
}

make_pack2(pk2_lez, int)
make_pack2(pk2_leb, cvl_bool)
make_pack2(pk2_led, float)

