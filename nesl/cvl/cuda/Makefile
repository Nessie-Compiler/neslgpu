# Makefile for CUDA CVL

# default in case TOPCC isn't defined
TOPCC = /usr/local/cuda/bin/nvcc

#CUDA_SDK_INCLUDE=/home/larsberg/NVIDIA_GPU_Computing_SDK/C/common/inc/

CC = /usr/local/cuda/bin/nvcc
#CFLAGS = -arch=sm_20 --compiler-options -Wall -DCUDA -DNDEBUG -I$(CUDA_SDK_INCLUDE)
CFLAGS = -arch=sm_30 --compiler-options -Wall -DCUDA -DNDEBUG
CPPFLAGS = -arch=sm_30 --compiler-options -Wall

# we should be in ROOT/cvl/cuda
ROOT = ../..

INCLUDEDIR = $(ROOT)/include
INCLUDE = cvl.h
LIB = libcvl.a

OBJS = elementwise.o facilities.o library.o rank.o vecops.o vprims.o
CUOBJS = elementwise.gpu.o library.gpu.o vecops.gpu.o vprims.gpu.o

$(LIB): $(OBJS) $(CUOBJS)
#	libtool -static -o $(LIB) $(OBJS) $(CUOBJS)
	-rm -f $(LIB)
	ar r $(LIB) $(OBJS) $(CUOBJS)
	ranlib $(LIB)

%.gpu.o: %.gpu.cu $(INCLUDEDIR)/$(INCLUDE) defins.h
	$(CC) -c $(CPPFLAGS) -I$(INCLUDEDIR) -o $@ $<

%.o: %.c $(INCLUDEDIR)/$(INCLUDE) defins.h
	$(CC) -c -x cu $(CFLAGS) -I$(INCLUDEDIR) -o $@ $<


clean:
	-rm -f $(OBJS) $(CUOBJS) $(LIB)
