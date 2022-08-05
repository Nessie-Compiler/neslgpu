/*! \file cutil_inline.h
 *
 * \author John Reppy
 *
 * The NVIDIA GPU Programming SDK used to have a file of this name (prior to CUDA 5.0).
 * We define our own version here to make things work on newer versions.
 */

/*
 * COPYRIGHT (c) 2014 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 */

#ifndef _CUTIL_INLINE_H_
#define _CUTIL_INLINE_H_

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef unsigned int uint;

#ifndef NDEBUG

static void HandleError (cudaError_t err, const char *file, int line)
{
    std::cerr << "** Error: " << cudaGetErrorString(err) << " in "
	<< file << " at line " << line << std::endl;
    exit (EXIT_FAILURE);
}

#define cutilSafeCall(STMT)				\
    do {						\
	cudaError_t _err_ = STMT;			\
	if (_err_ != cudaSuccess)			\
	    HandleError(_err_, __FILE__, __LINE__);	\
    } while (0)

// This will output the proper error string when calling cudaGetLastError
#define cutilCheckMsg(msg)      __getLastCudaError (msg, __FILE__, __LINE__)

inline void __getLastCudaError(const char *errorMessage, const char *file, const int line)
{
    cudaError_t err = cudaGetLastError();

    if (cudaSuccess != err)
    {
        fprintf(stderr, "%s(%i) : getLastCudaError() CUDA error : %s : (%d) %s.\n",
                file, line, errorMessage, (int)err, cudaGetErrorString(err));
        cudaDeviceReset();
        exit(EXIT_FAILURE);
    }
}

#else // NDEBUG

#define cutilSafeCall(STMT)	STMT
#define cutilCheckMsg(msg)

#endif

#endif // !_CUTIL_INLINE_H_
