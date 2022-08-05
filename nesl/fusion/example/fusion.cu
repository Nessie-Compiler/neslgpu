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

__global__ void muladdKernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
    int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
  
    if (address < len) {
        pDst[address] = (plus((times(pSrc0[address], pSrc1[address])), pSrc2[address]));
    }
}

void muladd(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
    if (len==0) {return;}
    SYNC();
    DEF_BLOCKS_PER_GRID(len);
    muladdKernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
    cutilCheckMsg("muladd execution failed\n");
}

make_no_scratch(muladd)
make_inplace(muladd, INPLACE_NONE)
vopdes_t vops[] = {
    {FUSED, "muladd", 3, 1,
     {Int, Int,Int, Illegal, Illegal, Illegal, Illegal, Illegal, Illegal, Illegal, Illegal, Illegal, Illegal, Illegal, Illegal, Illegal, Illegal, Illegal, Illegal},
     {NONE, NONE, NONE, 0, 0, 0},
     {Int,},
     {AGREE1,},
     {1,},
     Elwise3},
};

cvl_triple_t cvl_funs[] = {
    { { (void (*)())muladd, (int (*)())muladd_scratch, (unsigned (*)())muladd_inplace },},
};
