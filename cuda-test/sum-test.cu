/*! \file sum-test.cu
 *
 * \author John Reppy
 *
 * Benchmark some different ways to compute the sum of 10^7 floats.
 */

/*
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 */

#include <stdio.h>
#include <stdlib.h>

static void HandleError (cudaError_t err, const char *file, int line)
{
    fprintf(stderr, "%s in %s at line %d\n", cudaGetErrorString(err), file, line);
    exit (EXIT_FAILURE);
}
#define CHECK_ERROR(STMT)				\
    do {						\
	cudaError_t _err_ = STMT;			\
	if (_err_ != cudaSuccess)			\
	    HandleError(_err_, __FILE__, __LINE__);	\
    } while (0)

//#define N		10000000	// 10^7
#define N		100

__global__ void sum1 (float *a, float *b, float *res)
{
    int idx = blockIdx.x;
    if (idx < N) {
	res[idx] = a[idx] + b[idx];
    }

}

__global__ void sum2 (float *a, float *b, float *c, float *res1, float *res2)
{
    int idx = blockIdx.x;
    if (idx < N) {
	float tmp = a[idx];
	res1[idx] = tmp + b[idx];
	res2[idx] = tmp + c[idx];
    }

}


int main (int argc, const char **argv)
{
  // use device 1, which is the M2090
    CHECK_ERROR( cudaSetDevice (1) );

  // allocate the memory on the GPU
    float *a, *b, *c, *res1, *res2;
    a = (float *) malloc (N * sizeof(float));
    b = (float *) malloc (N * sizeof(float));
    c = (float *) malloc (N * sizeof(float));
    res1 = (float *) malloc (N * sizeof(float));
    res2 = (float *) malloc (N * sizeof(float));

  // initialize the arrays on the CPU
    for (int i = 0; i < N; i++) {
        a[i] = (float)-i;
        b[i] = (float)(i * i);
        c[i] = (float)i / 10.0f;
    }

  // allocate the memory on the GPU
    float *dev_a, *dev_b, *dev_c, *dev_res1, *dev_res2;
    CHECK_ERROR( cudaMalloc( (void**)&dev_a, N * sizeof(float) ) );
    CHECK_ERROR( cudaMalloc( (void**)&dev_b, N * sizeof(float) ) );
    CHECK_ERROR( cudaMalloc( (void**)&dev_c, N * sizeof(float) ) );
    CHECK_ERROR( cudaMalloc( (void**)&dev_res1, N * sizeof(float) ) );
    CHECK_ERROR( cudaMalloc( (void**)&dev_res2, N * sizeof(float) ) );

  // copy the arrays to the GPU
    CHECK_ERROR( cudaMemcpy( dev_a, a, N * sizeof(float), cudaMemcpyHostToDevice ) );
    CHECK_ERROR( cudaMemcpy( dev_b, b, N * sizeof(float), cudaMemcpyHostToDevice ) );
    CHECK_ERROR( cudaMemcpy( dev_c, c, N * sizeof(float), cudaMemcpyHostToDevice ) );

    sum1<<<N,1>>>( dev_a, dev_b, dev_res1 );
    sum1<<<N,1>>>( dev_a, dev_c, dev_res2 );

    sum2<<<N,1>>>( dev_a, dev_b, dev_c, dev_res1, dev_res2 );

    // copy the array 'c' back from the GPU to the CPU
    CHECK_ERROR( cudaMemcpy( c, dev_c, N * sizeof(int),
                              cudaMemcpyDeviceToHost ) );

    // display the results
    for (int i=0; i<10; i++) {
        printf( "%f + %f = %f\n", a[i], b[i], c[i] );
    }

  // free GPU memory
    CHECK_ERROR( cudaFree( dev_a ) );
    CHECK_ERROR( cudaFree( dev_b ) );
    CHECK_ERROR( cudaFree( dev_c ) );
    CHECK_ERROR( cudaFree( dev_res1 ) );
    CHECK_ERROR( cudaFree( dev_res2 ) );

  // free CPU memory
    free (a);
    free (b);
    free (c);
    free (res1);
    free (res2);

    return 0;
}
