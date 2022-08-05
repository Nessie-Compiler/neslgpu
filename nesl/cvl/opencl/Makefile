# Makefile for OpenCL CVL

# default in case TOPCC isn't defined
TOPCC = gcc

CC = $(TOPCC)
BASEFLAGS = -g -Wall -std=c99 -DOPENCL -DNDEBUG

OS := $(shell uname -s)
ifeq ($(OS),Darwin)
CFLAGS = $(BASEFLAGS)
else
CFLAGS = $(BASEFLAGS) -I/usr/local/cuda/include
endif

# we should be in ROOT/cvl/opencl
ROOT = ../..

INCLUDEDIR = $(ROOT)/include
INCLUDE = cvl.h
LIB = libcvl.a

OBJS = elementwise.o facilities.o kernels.o library.o rank.o vecops.o vprims.o 

$(LIB): $(OBJS)
#	libtool -static -o $(LIB) $(OBJS)
	-rm -f $(LIB)
	ar r $(LIB) $(OBJS)
	ranlib $(LIB)

%.o: %.c $(INCLUDEDIR)/$(INCLUDE) defins.h
	$(CC) -c $(CFLAGS) -I$(INCLUDEDIR) -o $@ $<

clean:
	-rm -f $(OBJS) $(LIB)
