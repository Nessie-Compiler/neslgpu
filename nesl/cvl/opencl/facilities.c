#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <memory.h>
#include <sys/stat.h>
#include <sys/resource.h>

#ifdef __linux
#include <CL/cl.h>
#else
#include <OpenCL/opencl.h>
#endif

#include <cvl.h>

#include "defins.h"


/* OpenCL-related globals */
cl_device_id            ComputeDeviceId;
cl_command_queue        ComputeCommands;
cl_context              ComputeContext;
cl_program              ComputeProgram;
cl_kernel*              ComputeKernels;
cl_mem                  ComputeMemory = 0;
size_t                  GlobalSize = 0;


char const *filenames[] = {"bin/elementwise.cl", "bin/library.cl", "bin/rank.cl", "bin/vecops.cl", "bin/vprims.cl"};
static char **LoadProgramSourceFromFiles() {
    struct stat statbuf;
    FILE        *fh;
    char        *source;
    char **results;
    const int NUMFILES = sizeof(filenames)/sizeof(char*);

    results = (char**)malloc(NUMFILES * sizeof(char*));
    
    for (int i = 0; i < sizeof(filenames)/sizeof(char*); i++) {
        fh = fopen(filenames[i], "r");
        if (fh == 0) {
            printf ("Could not find file %s\n", filenames[i]);
            return 0;
        }

        stat(filenames[i], &statbuf);
        source = (char *) malloc(statbuf.st_size+1);
        fread(source, statbuf.st_size, 1, fh);
        source[statbuf.st_size] = '\0';
        results[i] = source;
    }

    return results;
}

#ifdef __linux
void notify(const char *errinfo, const void *private_info, size_t cb, void *user_data) {
    printf ("NOTIFICATION: %s\n", errinfo);    
}
#endif

void CVL_init () {
    rnd_foz (0);

    int err;
    cl_platform_id clSelectedPlatformID = NULL; 
    cl_int ciErrNum = clGetPlatformIDs (1, &clSelectedPlatformID, NULL);
    if (ciErrNum != CL_SUCCESS)
    {
	printf("Error: Failed to locate a compute platform.\n");
	return;
    }


    // Connect to a GPU compute device, falling back to CPU if no GPU exists
    //
    err = clGetDeviceIDs(clSelectedPlatformID, CL_DEVICE_TYPE_DEFAULT, 1, &ComputeDeviceId, NULL);
    if (err != CL_SUCCESS)
    {
	printf("Error: Failed to locate a compute device!\n");
	return;
    }

    size_t returned_size = 0;
    size_t max_workgroup_size = 0;
    err = clGetDeviceInfo(ComputeDeviceId, CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(size_t), &max_workgroup_size, &returned_size);
    assert (err == CL_SUCCESS);

    cl_char vendor_name[1024] = {0};
    cl_char device_name[1024] = {0};
    err = clGetDeviceInfo(ComputeDeviceId, CL_DEVICE_VENDOR, sizeof(vendor_name), vendor_name, &returned_size);
    err|= clGetDeviceInfo(ComputeDeviceId, CL_DEVICE_NAME, sizeof(device_name), device_name, &returned_size);
    assert (err == CL_SUCCESS);
    
    //printf("Connecting to OpenCL device %s %s...\n", vendor_name, device_name);

    // Create a compute ComputeContext 
    //

#ifdef __linux
    ComputeContext = clCreateContext(0, 1, &ComputeDeviceId, notify, NULL, &err);
#else
    ComputeContext = clCreateContext(0, 1, &ComputeDeviceId, clLogMessagesToStdoutAPPLE, NULL, &err);
#endif
    if (!ComputeContext)
    {
        printf("Error: Failed to create a compute ComputeContext!\n");
        return;
    }

    // Create a command queue
    //
    ComputeCommands = clCreateCommandQueue(ComputeContext, ComputeDeviceId, 0, &err);
    if (!ComputeCommands || err != CL_SUCCESS)
    {
        printf("Error %d: Failed to create a command ComputeCommands!\n", err);
        return;
    }

    // Load all of the files
    char **source = LoadProgramSourceFromFiles();
    if(!source)
    {
        printf("Error: Failed to load compute program from files!\n");
        return;
    }
        
    // Create the compute program from the source buffer
    //
    ComputeProgram = clCreateProgramWithSource(ComputeContext, sizeof(filenames)/sizeof(char*), (const char **) source, NULL, &err);
    if (!ComputeProgram || err != CL_SUCCESS)
    {
        printf("Error: Failed to create compute program!\n");
        return;
    }
    
    // Build the program executable
    //
    err = clBuildProgram(ComputeProgram, 0, NULL, NULL, NULL, NULL);
    if (err != CL_SUCCESS)
    {
        size_t length;
        char build_log[20480];
        printf("Error: Failed to build program executable!\n");
        clGetProgramBuildInfo(ComputeProgram, ComputeDeviceId, CL_PROGRAM_BUILD_LOG, sizeof(build_log), build_log, &length);
        printf("%s\n", build_log);
        exit(1);
        return;
    }

    ComputeKernels = (cl_kernel*) malloc(KernelCount * sizeof(cl_kernel));
    char kernelName[256];
    for(int i = 0; i < KernelCount; i++)
    {    
        // Create each compute kernel from within the program
        //
        strcpy(kernelName, KernelNames[i]);
        strcat(kernelName, "Kernel");
        ComputeKernels[i] = clCreateKernel(ComputeProgram, kernelName, &err);
        if (!ComputeKernels[i] || err != CL_SUCCESS)
        {
            printf("Error: Failed to create compute kernel %s:%d!\n", KernelNames[i], i);
        }
		
		err = clGetKernelWorkGroupInfo(ComputeKernels[i], ComputeDeviceId, CL_KERNEL_WORK_GROUP_SIZE, sizeof(size_t), &GlobalSize, NULL); 
		if(err)
		{
			printf("Error: Failed to get kernel work group size\n");
		}
    }

    //printf ("Loaded all kernels.\n");
}

/* -----------------------Timing functions-----------------------------*/
/* returns number of seconds and microseconds in argument
 * (number of clock ticks on cray)
 */
void tgt_fos(cvl_timer_t *cvl_timer_p)	{
 	struct rusage rusage;
    
 	getrusage(0, &rusage);          /* user time of process */
	cvl_timer_p->sec = rusage.ru_utime.tv_sec;
	cvl_timer_p->usec = rusage.ru_utime.tv_usec;
}

/* double precision difference in seconds between two cvl_timer_t values:
 *   t1 occurs after t2
 */
double tdf_fos(cvl_timer_t *t1, cvl_timer_t *t2) {
  return (double)(t1->sec - t2->sec) +
	 (1.0e-6)*(double)(t1->usec - t2->usec);
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

make_siz_func(siz_fob, cvl_bool)		/* size of cvl_boolean vector */
make_siz_func(siz_foz, int)			/* size of integer vector */
make_siz_func(siz_fod, float)			/* size of double vector */

/* --------------------Segment Descriptors---------------------------*/

/* n = number of elements in segmented vector */
/* m = number of segments in vector */
/* The segment descriptor is an integer vector of m segment lengths */

int siz_fos(int n, int m)   /* size of segment descriptor */
{
  return (int) ((m * sizeof(int) + CVL_SIZE_UNIT-1) / CVL_SIZE_UNIT);
}

/* create segment descriptor sd */
/* l is a vector of segment lengths */
void mke_fov(vec_p sd, vec_p l, int n, int m, vec_p scratch) {
  if (sd != l) cpy_wuz(sd, l, m, scratch);
}

make_inplace(mke_fov,INPLACE_1)

/* Must take two args */
int mke_fov_scratch(int n , int m) {
	return 0;
}

/* inverse of mke_fov */
void len_fos(vec_p l, vec_p sd, int n, int m, vec_p scratch) {
    if (sd != l)
        cpy_wuz(l, sd, m, scratch);
}

int len_fos_scratch(int n, int m) {
	return 0;
}

make_inplace(len_fos,INPLACE_1)

/* -------------------Memory functions------------------------------*/
/* Our interpretation of vec_p is an integer offset into the allocated memory. */

/* allocate vector memory */
/* returns NULL if unsuccessful */
vec_p alo_foz(int size) {
    CVL_init();
    assert (ComputeMemory == NULL);

    //    printf ("Allocating buffer of size %d\n", (int)((size+1) * CVL_SIZE_UNIT));
    ComputeMemory = clCreateBuffer(ComputeContext, CL_MEM_READ_WRITE, (unsigned)(size+1) * CVL_SIZE_UNIT, NULL, NULL);
    if (!ComputeMemory) {
        printf ("Returned nothing -- error!\n");
        return (vec_p)0;
    }

    return (vec_p)(CVL_SIZE_UNIT);
}

/* free vector memory */
/* Since this is the only pair to the "init" function, we also release all of the
 * OpenCL-related administrative objects at this point as well.
 */
void fre_fov(vec_p pointer) {
    clReleaseMemObject (ComputeMemory);

    for(int i = 0; i < KernelCount; i++)
        clReleaseKernel(ComputeKernels[i]);

    clReleaseProgram(ComputeProgram);
    clReleaseCommandQueue(ComputeCommands);
    clReleaseContext(ComputeContext);
}

/* mov_fov is a memory move instruction.  it must handle overlapping regions */
void mov_fov(vec_p d, vec_p s, int size, vec_p scratch) {
    cl_int err;
    size_t dst = (size_t)d;
    size_t src = (size_t)s;

    if (dst==src)
        return;
    
    DBG_PRINT("DEBUG: copying buffer src: %lld, dst: %lld, len: %d\n", (long long)s, (long long)d, size);
    err = clEnqueueCopyBuffer (ComputeCommands, ComputeMemory, ComputeMemory, src*sizeof(MAXALIGN), dst*sizeof(MAXALIGN), size*sizeof(MAXALIGN), 0, NULL, NULL);
    assert (err == CL_SUCCESS);

    err = CLFINISH (ComputeCommands);
    assert (err == CL_SUCCESS);
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
cvl_bool eql_fov(vec_p v1, vec_p v2) {
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
        cl_int err;                                                     \
        _cltype  *raw = (_cltype*)malloc(len*sizeof(_cltype));          \
        DBG_PRINT("DEBUG: reading %s buffer src: %d, len: %d\n", #_cltype, s, len); \
        if (len == 0) { return; }                                       \
        err = clEnqueueReadBuffer(ComputeCommands, ComputeMemory, TRUE, ((size_t)s)*sizeof(MAXALIGN), len*sizeof(_cltype), (void *)raw, 0, NULL, NULL); \
        assert (err == CL_SUCCESS);                                     \
        err = clFinish (ComputeCommands);                               \
        assert (err == CL_SUCCESS);                                     \
                                                                        \
        for (int i = 0; i < len; i++) {                                 \
            d[i] = (_cltype)raw[i];                                     \
        }                                                               \
        free(raw);                                                      \
    }				 \
make_no_scratch(_name)					\
make_inplace(_name,INPLACE_NONE)

make_v2c(v2c_fuz, int, cl_int)
make_v2c(v2c_fub, cvl_bool, cl_int)
make_v2c(v2c_fud, double, cl_float)

#define make_c2v(_name, _type, _cltype)                             \
    void _name(vec_p d, _type *s, int len, vec_p scratch) { \
    cl_int err; \
    DBG_PRINT("DEBUG: writing buffer dst: %d, len: %d, type: %s\n", d, len, #_type); \
    if (len == 0) { return; } \
    _cltype *raw = (_cltype*)malloc(len*sizeof(_cltype)); \
    for (int i = 0; i < len; i++) { \
        raw[i] = (_cltype)s[i]; \
    } \
    err = clEnqueueWriteBuffer(ComputeCommands, ComputeMemory, TRUE, ((size_t)d)*sizeof(MAXALIGN), len*sizeof(_cltype), (void *)raw, 0, NULL, NULL); \
    assert (err == CL_SUCCESS);  \
    err = clFinish (ComputeCommands); \
    assert (err == CL_SUCCESS); \
    free(raw); \
	}							\
    make_no_scratch(_name)					\
    make_inplace(_name,INPLACE_NONE)

make_c2v(c2v_fuz, int, cl_int)
make_c2v(c2v_fub, cvl_bool, cl_int)
make_c2v(c2v_fud, double, cl_float)


void rnd_foz (int seed) {
    srandom (seed);
}


