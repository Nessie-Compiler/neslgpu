#include <stdio.h>
#include <cuda_runtime.h>

int main (int argc, const char **argv)
{
    int devCount;
    cudaGetDeviceCount (&devCount);
    for (int dev = 0;  dev < devCount;  dev++) {
	struct cudaDeviceProp prop;
	cudaGetDeviceProperties (&prop, dev);
	printf ("[%d] %s (%d.%d):\n", dev, prop.name, prop.major, prop.minor);
	printf ("  totalGlobalMem = %dk\n", (int)(prop.totalGlobalMem >> 10));
	printf ("  sharedMemPerBlock = %dk\n", (int)(prop.sharedMemPerBlock >> 10));
	printf ("  multiProcessorCount = %d\n", prop.multiProcessorCount);
	printf ("  warpSize = %d\n", prop.warpSize);
	printf ("  maxThreadsPerMultiProcessor = %d\n", prop.maxThreadsPerMultiProcessor);
	printf ("  maxThreadsPerBlock = %d\n", prop.maxThreadsPerBlock);
	printf ("  maxThreadsDim = %d x %d x %d\n",
	    prop.maxThreadsDim[0], prop.maxThreadsDim[1], prop.maxThreadsDim[2]);
	printf ("  maxGridSize = %d x %d x %d\n",
	    prop.maxGridSize[0], prop.maxGridSize[1], prop.maxGridSize[2]);
    }

    return 0;

}
