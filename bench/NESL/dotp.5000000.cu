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

void fused0(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused0Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused0 execution failed\n");
}

make_no_scratch(fused0)
make_inplace(fused0, INPLACE_NONE)
vopdes_t vops[] = {
  {FUSED, "fused0", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  };

cvl_triple_t cvl_funs[] = {
  { { (void (*)())fused0, (int (*)())fused0_scratch, (unsigned (*)())fused0_inplace },},
  };
/*
fused OP0#2 ($0 : SEGDES) = (DIST INT @ (CONST INT 100) $0)
*/
