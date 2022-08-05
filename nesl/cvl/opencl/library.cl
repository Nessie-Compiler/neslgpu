//typedef union maxaligned { int i; float f;} MAXALIGN;
//typedef int cvl_bool;
/* This file contains some library functions */

/*----------------------------index (iota)-----------------*/
/* Index(len) creates an integer vector of the numbers */

__kernel void ind_luzKernel(__global MAXALIGN *data,
                            int d,
                            int init,
                            int stride,
                            int count,
                            int scratch)
{
    int address = get_global_id(0);           
    __global uint *pDst = (__global uint*)(&data[d]); 
    
    if (address < count) {
        pDst[address] = init+(address*stride);    
    } 
    return; 
}

/* Segmented index creates a segmented vector of index results, given a 
 * vector of lengths.
 */
__kernel void ind_lezKernel(__global MAXALIGN *data,
                            int d,
                            int init,
                            int stride,
                            int dest_segd,
                            int vec_len,
                            int seg_count,
                            int scratch)
{
    int address = get_global_id(0);           
    __global uint *pDst = (__global uint*)(&data[d]); 
    __global uint *pInit = (__global uint*)(&data[init]); 
    __global uint *pStride = (__global uint*)(&data[stride]); 
    __global uint *pSeg = (__global uint*)(&data[dest_segd]); 

    int i = 0;
    int relIndex = address;
    int segSize = pSeg[i];
    while ((relIndex >= segSize) && (i < seg_count)) {
        relIndex -= segSize;
        i++;
        segSize = pSeg[i];
    }

    if (relIndex < segSize) {
        pDst[address] = pInit[i]+(relIndex*pStride[i]);
    }
    return; 
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
__kernel void pk1_luvKernel_unusedfast (__global MAXALIGN *data, int f, int len, int s, __local int* scratch)
{
    __global int *g_idata = (__global int*)(&data[f]);              
    __global int *g_odata = (__global int*)(&data[s]);              
    int length = len;                                                   
    int global_index = get_global_id(0);                                
    int local_index = get_local_id(0);                                  
    /* Load data into local memory */                                   
    if (global_index < length) {                                        
        scratch[local_index] = g_idata[global_index];                   
    } else {                                                            
        scratch[local_index] = 0;                               
    }                                                                   
    barrier(CLK_LOCAL_MEM_FENCE);                                       
    for(int offset = get_local_size(0) / 2;                             
        offset > 0;                                                     
        offset >>= 1) {                                                 
    if (local_index < offset) {                                         
        int other = 0;                                         
        if (local_index+offset < length) {                              
            other = scratch[local_index + offset];                      
        }                                                               
        int mine = scratch[local_index];                               
        scratch[local_index] = mine + other;                      
    }                                                                   
    barrier(CLK_LOCAL_MEM_FENCE);                                       
    }                                                                   
    if (local_index == 0) {                                             
        g_odata[get_group_id(0)] = scratch[0];                          
    }                                                                   
    barrier(CLK_GLOBAL_MEM_FENCE);                                      
    if ((global_index == 0) && (local_index == 0)) {                    
        for (unsigned int i = 1; i < get_num_groups(0); i++) {          
            scratch[0] = scratch[0] + g_odata[i];
        }                                                               
        g_odata[0] = scratch[0];                                        
    }                                                                   
}

__kernel void pk1_luvKernel (__global MAXALIGN *data, int f, int len, int s, __local int* scratch)
{
    __global int *g_idata = (__global int*)(&data[f]);              
    __global int *g_odata = (__global int*)(&data[s]);              
    int global_index = get_global_id(0);                    
    if (global_index == 0) {                                
        int sum = 0;                                        
        int k = 0;                                 
        for (k = 0; k < len; k++) {                         
            sum += g_idata[k];                              
        }                                                   
        g_odata[0] = sum;                                   
     }
}


#define make_pk2(_name, _type)					\
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
    int d, \
    int s, \
    int f, \
    int src_len, \
    int dest_len, \
    int scratch) \
{\
 __global uint *pDst = (__global uint*)(&data[d]);      \
 __global uint *pSrc = (__global uint*)(&data[s]);      \
 __global uint *pFlags = (__global uint*)(&data[f]);    \
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

/* pack1: returns the lengths vector of the result of the pack
 *	ds = destination segment descriptor
 * 	f = boolean flags (same length as s)
 * 	sd = source segment descriptor
 *	n = length of f
 *	m = number of segments of f
 */

/* TODO: a kernel with some parallelism
 * Segmented hack: this kernel is invoked with m global items,
 * one for each segment.
 */
__kernel void pk1_levKernel (__global MAXALIGN *data,
                      int ds,
                      int f,
                      int sd,
                      int flen,
                      int sdlen,
                      int scratch)
{
    __global int *g_idata = (__global int*)(&data[f]);    
    __global int *g_odata = (__global int*)(&data[ds]);    
    __global int *pSD = (__global int*)(&data[sd]);       
    int segment = get_global_id(0);                         
    int startPos = 0;                                       
    for (int i = 0; i < segment; i++) {                     
        startPos += pSD[i];                                 
    }                                                       
    int endPos = startPos + pSD[segment];                   
    int sum = 0;                                            
    int k = 0;                                              
    for (k = startPos; k < endPos; k++) {                   
        sum += g_idata[k];                                  
    }                                                       
    g_odata[segment] = sum;                                 
}

/* pack2: returns the pack
 *
 * This poor version runs one thread per segment.
 * First, determine the source and destination starting
 * points as well as the source segment ending point.
 * Then, scan through source+flags positions, copying into
 * the destination location positions.
 */
#define make_pack2(_name, _type)				\
__kernel void _name##Kernel( \
    __global MAXALIGN *data, \
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
    __global uint *pDst = (__global uint*)(&data[d]);       \
    __global uint *pSrc = (__global uint*)(&data[s]);       \
    __global uint *pFlags = (__global uint*)(&data[f]);     \
    __global uint *pSD = (__global uint*)(&data[sd_s]);     \
    __global uint *pSD_d = (__global uint*)(&data[sd_d]);   \
    int segment = get_global_id(0);                         \
    int startPos = 0;                                       \
    for (int i = 0; i < segment; i++) {                     \
        startPos += pSD[i];                                 \
    }                                                       \
    int endPos = startPos + pSD[segment];                   \
                                                            \
    int destPos = 0;                                        \
    for (int i = 0; i < segment; i++) {                     \
        destPos += pSD_d[i];                                \
    }                                                       \
                                                            \
    for (int i = startPos; i < endPos; i++) {               \
        if (pFlags[i] != 0) {                               \
            pDst[destPos] = pSrc[i];                        \
            destPos++;                                      \
        }                                                   \
    }                                                       \
    return ;                                                \
}

make_pack2(pk2_lez, int)
make_pack2(pk2_leb, cvl_bool)
make_pack2(pk2_led, double)

