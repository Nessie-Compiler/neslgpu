# Makefile for CUDA CVL

# default in case TOPCC isn't defined
TOPCC = nvcc

CUDA_SDK_INCLUDE=/home/larsberg/NVIDIA_GPU_Computing_SDK/C/common/inc/

CC = nvcc
CFLAGS = -arch=sm_20 --compiler-options -fPIC --compiler-options -Wall -DCUDA -DNDEBUG -I$(CUDA_SDK_INCLUDE) 

# we should be in ROOT/fusion/example
ROOT = ../..

INCLUDEDIR = $(ROOT)/include
INCLUDEDIR2 = $(ROOT)/vcode
INCLUDE = cvl.h
LIB = libfused.so

CUOBJS = fusion.o

$(LIB): $(CUOBJS)
	-rm -f $(LIB)
	$(CC) -shared --linker-options -soname,$(LIB) -o $(LIB) $(CUOBJS)

fusion.o: fusion.cu
	$(CC) -c $(CFLAGS) -I$(INCLUDEDIR) -I$(INCLUDEDIR2) -o $@ $<

clean:
	-rm -f $(OBJS) $(CUOBJS) $(LIB)
