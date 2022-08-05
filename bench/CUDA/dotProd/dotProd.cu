/*
 * Copyright 1993-2010 NVIDIA Corporation.  All rights reserved.
 *
 * Please refer to the NVIDIA end user license agreement (EULA) associated
 * with this source code for terms and conditions that govern your use of
 * this software. Any use, reproduction, disclosure, or distribution of
 * this software and related documentation outside the terms of the EULA
 * is strictly prohibited.
 *
 */

// Utilities and system includes
#include <cublas_v2.h>
#include <shrUtils.h>
#include <shrQATest.h>
#include "cutil_inline.h"

// includes, kernels
//#include <matrixMul_kernel.cu>

static char *sSDKsample = "dotProd";

////////////////////////////////////////////////////////////////////////////////
// declaration, forward
void runTest(int argc, char** argv);
void randomInit(float*, int);
void printDiff(float*, float*, int, int, int, float);

void inline checkError(cublasStatus_t status, const char* msg)
{
    if(status != CUBLAS_STATUS_SUCCESS){
        printf(msg, status);
        exit(-1);
    }
}

////////////////////////////////////////////////////////////////////////////////
// Program main
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char** argv)
{
    shrQAStart(argc, argv);
	printf("[ %s ]\n", sSDKsample);

    //shrSetLogFileName ("matrixMul.txt");
    shrLog("%s Starting (CUDA and CUBLAS tests)...\n\n", argv[0]);

    runTest(argc, argv);
}

////////////////////////////////////////////////////////////////////////////////
//! Run a simple test for CUDA
////////////////////////////////////////////////////////////////////////////////
void runTest(int argc, char** argv)
{
    if(shrCheckCmdLineFlag(argc, (const char**)argv, "device"))
    {
        cutilDeviceInit(argc, argv);
    }
    else
    {
        cudaSetDevice(cutGetMaxGflopsDeviceId());
    }

    int devID;
    cudaDeviceProp props;

    // get number of SMs on this GPU
    cutilSafeCall(cudaGetDevice(&devID));
    cutilSafeCall(cudaGetDeviceProperties(&props, devID));

    printf("Device %d: \"%s\" with Compute %d.%d capability\n", devID, props.name, props.major, props.minor);

	// set seed for rand()
    srand(2006);

    // Optional Command-line multiplier for matrix sizes
    int uLen = 10000000;
    shrGetCmdLineArgumenti(argc, (const char**)argv, "size", &uLen); 

    shrLog("\nUsing array size %u\n", uLen);

    float* h_A = (float*)malloc(uLen*sizeof(float));
    float* h_B = (float*)malloc(uLen*sizeof(float));

    if (h_A==NULL || h_B==NULL) {
	printf("couldn't alloc");
	return;
    }
    
    // initialize host memory
    randomInit(h_A, uLen);
    randomInit(h_B, uLen);
    
    // allocate device memory
    float* d_A, *d_B;
    cutilSafeCall(cudaMalloc((void**) &d_A, uLen*sizeof(float)));
    cutilSafeCall(cudaMalloc((void**) &d_B, uLen*sizeof(float)));

    // copy host memory to device
    cutilSafeCall(cudaMemcpy(d_A, h_A, uLen*sizeof(float), cudaMemcpyHostToDevice) );
    cutilSafeCall(cudaMemcpy(d_B, h_B, uLen*sizeof(float), cudaMemcpyHostToDevice) );
    
   
    // create and start timer
    shrLog("Runing Kernels...\n\n");
    unsigned int timer_cublas    = 0;

    cublasHandle_t handle;
    checkError(cublasCreate(&handle), "cublasCreate() error %d!\n");
    
    // Start Timing
    cutilCheckError(cutCreateTimer(&timer_cublas));
    cutilCheckError(cutStartTimer(timer_cublas));

    float result = 0.0;
    checkError(cublasSdot (handle, uLen, d_A, 1, d_B, 1, &result), "Error with cublasSdot %d\n");

    // check if kernel execution generated and error
    cutilCheckMsg("CUBLAS Kernel execution failed");
    cutilDeviceSynchronize();
    // stop and destroy timer
    cutilCheckError(cutStopTimer(timer_cublas));


    double dSeconds = cutGetTimerValue(timer_cublas)/((double)1000.0);

    //Log througput, etc
    shrLogEx(LOGBOTH | MASTER, 0, "> CUBLAS         Time = %.5f s, Size = %.0f Ops\n\n", dSeconds, uLen);

    cutilCheckError(cutDeleteTimer(timer_cublas));

    // clean up memory
    free(h_A);
    free(h_B);
    cutilSafeCall(cudaFree(d_A));
    cutilSafeCall(cudaFree(d_B));

    cutilDeviceReset();
}

// Allocates a matrix with random float entries.
void randomInit(float* data, int size)
{
    for (int i = 0; i < size; i++) {
        data[i] = rand() / (float)RAND_MAX;
    }
}
