# Makefile for CUDA CVL

# default in case TOPCC isn't defined
TOPCC = /usr/local/cuda/bin/nvcc

#CUDA_SDK_INCLUDE=/home/larsberg/NVIDIA_GPU_Computing_SDK/C/common/inc/
CC = /usr/local/cuda/bin/nvcc
#CFLAGS = -arch=sm_20 --compiler-options -fPIC --compiler-options -Wall -DCUDA -DNDEBUG -I$(CUDA_SDK_INCLUDE) 
CFLAGS = -arch=sm_20 --compiler-options -fPIC --compiler-options -Wall -DCUDA -DNDEBUG

# root of NESL tree
ROOT = ../../nesl

INCLUDEDIR = $(ROOT)/include
INCLUDEDIR2 = $(ROOT)/vcode

LIBS = dotp.10000000.so dotp.5000000.so dotp.1000000.so dotp.500000.so dotp.100000.so dotp.50000.so dotp.5000.so dotp.1000.so \
       qs.1000000.so qs.500000.so qs.100000.so qs.50000.so qs.5000.so qs.1000.so \
       quickhull.5000000.so quickhull.1000000.so quickhull.500000.so quickhull.100000.so quickhull.50000.so quickhull.5000.so quickhull.1000.so \
       bh.75000.so bh.50000.so bh.20000.so bh.10000.so bh.5000.so bh.1000.so bh.500.so bh.100.so \
       black-scholes.10000000.so

.PHONY:		all
all: $(LIBS)

%.so : %.cu
	$(CC) -shared --linker-options -soname,$*.so -o $*.so $(CFLAGS) -I$(INCLUDEDIR) -I$(INCLUDEDIR2) $*.cu

