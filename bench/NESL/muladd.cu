#include "config.h"
#include "vcode.h"
#include <cvl.h>
#include "y.tab.h"
#include <cutil_inline.h>
#include "defins.cuh"

MAXALIGN *ComputeMemory = NULL;

extern "C" void init (MAXALIGN *mem) {
  ComputeMemory = mem;
}

__global__ void fused0Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (100);
  }
}

__global__ void fused1Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    
    pDst[address] = (plus((times(pSrc0[address], pSrc1[address])), pSrc2[address]));
  }
}

void fused0(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused0Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused0 execution failed\n");
}

void fused1(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused1Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused1 execution failed\n");
}

make_no_scratch(fused0)
make_no_scratch(fused1)
make_inplace(fused0, INPLACE_NONE)
make_inplace(fused1, INPLACE_1)
vopdes_t vops[] = {
  {FUSED, "fused0", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused1", 3, 1,
  {Int,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise3},
  };

cvl_triple_t cvl_funs[] = {
  { { (void (*)())fused0, (int (*)())fused0_scratch, (unsigned (*)())fused0_inplace },},
  { { (void (*)())fused1, (int (*)())fused1_scratch, (unsigned (*)())fused1_inplace },},
  };
/*
fused OP0#3 ($0 : SEGDES) = (DIST INT @ (CONST INT 100) $0)
fused OP1#1 ($0 : INT, $1 : INT, $2 : INT) = (+ INT @ (* INT @ $0 $1) $2)
*/
