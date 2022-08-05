# Makefile for CUDA tests
#

NVCC =		/usr/local/cuda/bin/nvcc -m64 -arch=sm_20

all:		cuda-info sum-test pack-test

cuda-info:	cuda-info.c
	$(NVCC) --compiler-options -std=c99 -o cuda-info cuda-info.c

sum-test:	sum-test.cu
	$(NVCC) -o sum-test sum-test.cu

pack-test:	pack-test.cu
	$(NVCC) -o pack-test pack-test.cu
