#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <memory.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <dlfcn.h>

#include <cvl.h>
#include <cutil_inline.h>

#include <thrust/device_vector.h>
#include <thrust/scan.h>
#include <thrust/functional.h>

#include "defins.h"


MAXALIGN * ComputeMemory = NULL;
char *fusedLib = NULL;

#ifdef __linux
void notify(const char *errinfo, const void *private_info, size_t cb, void *user_data) {
    printf ("NOTIFICATION: %s\n", errinfo);    
}
#endif

extern "C" void CVL_init (char *lib) {
    rnd_foz (0);
    fusedLib = lib;

//    cudaSetDevice( cutGetMaxGflopsDeviceId() );
}

/* -----------------------Timing functions-----------------------------*/
/* returns number of seconds and microseconds in argument
 * (number of clock ticks on cray)
 */
void tgt_fos(cvl_timer_t *cvl_timer_p)
{
#if __linux
    clock_gettime (CLOCK_REALTIME, cvl_timer_p);
#else
    struct rusage rusage;
    
    getrusage(0, &rusage);          /* user time of process */
    cvl_timer_p->sec = rusage.ru_utime.tv_sec;
    cvl_timer_p->usec = rusage.ru_utime.tv_usec;
#endif
}

/* double precision difference in seconds between two cvl_timer_t values:
 *   t1 occurs after t2
 */
double tdf_fos(cvl_timer_t *t1, cvl_timer_t *t2)
{
#if __linux
    return (double)(t1->tv_sec - t2->tv_sec) + (1.0e-9)*(double)(t1->tv_nsec - t2->tv_nsec);
#else
    return (double)(t1->sec - t2->sec) + (1.0e-6)*(double)(t1->usec - t2->usec);
#endif
}

/* --------------------Size Functions---------------------------------*/

/* siz_ (length): returns number of units of memory of vector.  Units are
   expressed in terms of the quantity CVL_SIZE_UNIT
*/


#define make_siz_func(_name, _type) 				\
   int _name (int length)						\
   {								\
	return (int) ( (length * sizeof(_type) + (CVL_SIZE_UNIT - 1)) / CVL_SIZE_UNIT);	\
   }		

make_siz_func(siz_fob, cvl_bool)		/* size of boolean vector */
make_siz_func(siz_foz, int)			/* size of integer vector */
make_siz_func(siz_fod, float)			/* size of double vector */

/* --------------------Segment Descriptors---------------------------*/

/* n = number of elements in segmented vector */
/* m = number of segments in vector */
/* The segment descriptor is an integer vector of m segment lengths */

int siz_fos(int n, int m)   /* size of segment descriptor */
{
    return (siz_foz(n+m));
}

/* create segment descriptor sd */
/* l is a vector of segment lengths */
__global__ void mke_fovKernel(MAXALIGN *data, int d, int sd, int m) {
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
    if (address >= m) { return; }
    int *g_odata = (int*)(&data[d]);				 
    int *pSD = (int*)(&data[sd]);				 
    g_odata[address] = pSD[address];
}

__global__ void setSegStartsKernel(MAXALIGN *data, int d, int lengths, int seg_starts, int m) {
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
    if (address >= m) { return; }
    int *pLengths = (int*)(&data[lengths]);				 
    if (pLengths[address] != 0) {
    int *g_odata = (int*)(&data[d]);				 
    int *pSegStart = (int*)(&data[seg_starts]);				 
	g_odata[pSegStart[address]] = address;
    }
}

void print(thrust::device_ptr<int> p, char *name, int n) {
#ifndef NDEBUG
    std::cout << name << "[";
    for (int i = 0; i < n; i++) {
	std::cout << p[i] <<", ";
    }
    std::cout << "]\n";
#endif
}

template<typename T>
struct cpp_mmax_b : public thrust::binary_function<T,T,T> {
    __host__ __device__ T operator()(const T &i, const T &j) const {return ((i) > (j) ? (i) : (j));}
};
cpp_mmax_b<int> cpp_mmax_int_b;

void mke_fov(vec_p sd, vec_p l, int n, int m, vec_p scratch) {
    DBG_PRINT("DEBUG: mke_fov, making segment descriptor on %d elements and %d segments, for %d blocks\n.", n, m, NUM_BLOCKS(n+m));
    if (m==0) { return; }

    vec_p segment_starts = scratch;
    vec_p inclusive_input = scratch+m;

    // First, fill the array with zeros
    cutilSafeCall(cudaMemset(ComputeMemory+inclusive_input, 0, n*sizeof(int)));

    // Compute the segment starts
    thrust::device_ptr<int> d_lengths = thrust::device_pointer_cast ((int*)(ComputeMemory+l));
    thrust::device_ptr<int> d_starts = thrust::device_pointer_cast ((int*)(ComputeMemory+segment_starts));
    thrust::exclusive_scan(d_lengths, d_lengths+m, d_starts);
    print(d_lengths, "lengths", m);
    print(d_starts, "starts", m);

    // Then, set a "n" at each segment start
    DEF_BLOCKS_PER_GRID(m);
    setSegStartsKernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, inclusive_input, l, segment_starts, m);
    CHECK("setSegStarts execution failed\n");       

    // Compute an inclusive MAX scan to put the segment numbers in each element
    thrust::device_ptr<int> d_inclusive_input = thrust::device_pointer_cast ((int*)(ComputeMemory+inclusive_input));
    thrust::device_ptr<int> d_sd = thrust::device_pointer_cast ((int*)(ComputeMemory+sd));
    thrust::inclusive_scan(d_inclusive_input, d_inclusive_input+n, d_sd, cpp_mmax_int_b);
    print(d_inclusive_input, "inclusive", n);
    print(d_sd, "goal", n);

    fflush(stdout);
    // Finally, set the segment lengths at the end of the segment descriptors
    mke_fovKernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, sd+n, l, m);
    CHECK("mke_fov execution failed\n");       
}

make_inplace(mke_fov, INPLACE_NONE)

/* Must take two args */
int mke_fov_scratch(int n , int m) {
    return n+m;
}

/* inverse of mke_fov */
__global__ void len_fosKernel(MAXALIGN *data, int d, int sd, int n, int m) {                        
    int index = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;		 
    if (index >= m) { return; }
    int *g_odata = (int*)(&data[d]);				 
    int *pSD = (int*)(&data[sd]);				 
    g_odata[index] = pSD[n+index];
}

void len_fos(vec_p l, vec_p sd, int n, int m, vec_p scratch) {
    DEF_BLOCKS_PER_GRID(m);
    len_fosKernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, l, sd, n, m);
}

int len_fos_scratch(int n, int m) {
	return 0;
}

make_inplace(len_fos, INPLACE_NONE)

/* -------------------Memory functions------------------------------*/
/* Our interpretation of vec_p is an integer offset into the allocated memory. */

/* allocate vector memory */
/* returns NULL if unsuccessful */
vec_p alo_foz(int size) {
    assert (ComputeMemory == NULL);

    //    printf ("Allocating buffer of size %d\n", (int)((size+1) * CVL_SIZE_UNIT));
    cutilSafeCall( cudaMalloc((void **)&ComputeMemory, (size+1)*CVL_SIZE_UNIT) );
    if (!ComputeMemory) {
        fprintf (stderr, "ComputeMemory could not be allocated on the device.\n");
        return (vec_p)0;
    }

    if (fusedLib[0] != '\0') {
        void *handle = dlopen(fusedLib, RTLD_NOW);
        if (handle == NULL) {
            fprintf(stderr, "Unable to dynamically load library %s, error %s.\n", fusedLib, dlerror());
            return (vec_p)0;
        }
    
        void (*initFn)(MAXALIGN*) = (void (*)(MAXALIGN*))dlsym(handle, "init");
        if (initFn == NULL) {
            fprintf(stderr, "Function init missing from dynamically load library %s.\n", fusedLib);
            return (vec_p)0;
        }
        initFn(ComputeMemory);

        dlclose(handle);
    }

    return (vec_p)(CVL_SIZE_UNIT);
}

/* free vector memory */
/* Since this is the only pair to the "init" function, we also release all of the
 * OpenCL-related administrative objects at this point as well.
 */
void fre_fov(vec_p pointer) {
    cutilSafeCall( cudaFree(ComputeMemory));
}

/* mov_fov is a memory move instruction.  it must handle overlapping regions */
void mov_fov(vec_p d, vec_p s, int size, vec_p scratch) {
    cutilSafeCall( cudaMemcpy(ComputeMemory+d, ComputeMemory+s, size*sizeof(MAXALIGN), cudaMemcpyDeviceToDevice) );
}

int mov_fov_scratch(int size) {
    return 0;
}

/* ----------------Memory Arithmetic Functions---------------------*/

/* Need to make sure these pointers are maximally aligned.
 * Therefore vec_p should be a pointer to something large enough to hold
 * any CVL data type. 
 */

/* add_fov returns a vec_p V, such that a vector at v, of size a, does
 * not overlap into V.
 */
vec_p add_fov(vec_p v, int a) {
    size_t vec = (size_t)v;
    return (vec_p)(vec+a);
}

/* subtract two pointers and return size of interval in CVL_SIZE_UNITs */
int sub_fov(vec_p v1, vec_p v2) {
    size_t first = (size_t)v1;
    size_t second = (size_t)v2;

    if (first > second)
        return first-second;
    else
        return second-first;
}

/* compare two vec_p's.  We need this because vectors maybe more complicated
 * in some implementations.
 */
int eql_fov(vec_p v1, vec_p v2) {
	return ((size_t) v1 == (size_t)v2);
}

/* compare two vec_p's for inequality.  Return is same as strcmp. */
int cmp_fov(vec_p v1, vec_p v2) {
    return ((size_t)v1 - (size_t)v2);
}

/* ----------------------CVL - C conversion functions -------------*/

#define make_v2c(_name, _type, _cltype)                                 \
    void _name(_type *d, vec_p s, int len, vec_p scratch)				\
    {                                                                   \
        DBG_PRINT("DEBUG: reading %s buffer src: %d, len: %d\n", #_cltype, s, len); \
        if (len == 0) { return; }                                       \
        _cltype  *raw = (_cltype*)malloc(len*sizeof(_cltype));          \
	cutilSafeCall(cudaMemcpy((void*)raw, ComputeMemory+s, len*sizeof(_cltype), cudaMemcpyDeviceToHost)); \
        for (int i = 0; i < len; i++) {                                 \
            d[i] = (_cltype)raw[i];                                     \
        }                                                               \
        free(raw);                                                      \
    }				 \
make_no_scratch(_name)					\
make_inplace(_name,INPLACE_NONE)

make_v2c(v2c_fuz, int, int)
make_v2c(v2c_fub, int, int)
make_v2c(v2c_fud, double, float)

#define make_c2v(_name, _type, _cltype)                             \
    void _name(vec_p d, _type *s, int len, vec_p scratch) { \
    DBG_PRINT("DEBUG: writing buffer dst: %d, len: %d, type: %s\n", d, len, #_type); \
    if (len == 0) { return; } \
    _cltype *raw = (_cltype*)malloc(len*sizeof(_cltype)); \
    for (int i = 0; i < len; i++) { \
        raw[i] = (_cltype)s[i]; \
    } \
    cutilSafeCall(cudaMemcpy(ComputeMemory+d, (void*)raw, len*sizeof(_cltype), cudaMemcpyHostToDevice)); \
    free(raw); \
	}							\
    make_no_scratch(_name)					\
    make_inplace(_name,INPLACE_NONE)

make_c2v(c2v_fuz, int, int)
make_c2v(c2v_fub, int, int)
make_c2v(c2v_fud, double, float)


void rnd_foz (int seed) {
    srandom (seed);
}


