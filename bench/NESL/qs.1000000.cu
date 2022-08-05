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

__global__ void fused1Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (b_to_z((lt(pSrc0[address], (2)))));
  }
}

__global__ void fused2Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    
    pDst[address] = (eq((minus(pSrc0[address], pSrc1[address])), pSrc2[address]));
  }
}

__global__ void fused3Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (divide(pSrc0[address], (2)));
  }
}

__global__ void fused4Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (1);
  }
}

__global__ void fused5Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (0);
  }
}

__global__ void fused6Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (nnot((lt(pSrc0[address], (2)))));
  }
}

__global__ void fused7Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (-1);
  }
}

__global__ void fused8Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (lt(pSrc0[address], (0)));
  }
}

__global__ void fused9Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    
    pDst[address] = (minus((plus(pSrc0[address], pSrc1[address])), pSrc2[address]));
  }
}

void fused0(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused0Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused0 execution failed\n");
}

void fused1(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused1Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused1 execution failed\n");
}

void fused2(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused2Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused2 execution failed\n");
}

void fused3(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused3Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused3 execution failed\n");
}

void fused4(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused4Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused4 execution failed\n");
}

void fused5(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused5Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused5 execution failed\n");
}

void fused6(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused6Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused6 execution failed\n");
}

void fused7(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused7Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused7 execution failed\n");
}

void fused8(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused8Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused8 execution failed\n");
}

void fused9(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused9Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused9 execution failed\n");
}

make_no_scratch(fused0)
make_no_scratch(fused1)
make_no_scratch(fused2)
make_no_scratch(fused3)
make_no_scratch(fused4)
make_no_scratch(fused5)
make_no_scratch(fused6)
make_no_scratch(fused7)
make_no_scratch(fused8)
make_no_scratch(fused9)
make_inplace(fused0, INPLACE_NONE)
make_inplace(fused1, INPLACE_1)
make_inplace(fused2, INPLACE_1)
make_inplace(fused3, INPLACE_1)
make_inplace(fused4, INPLACE_NONE)
make_inplace(fused5, INPLACE_NONE)
make_inplace(fused6, INPLACE_1)
make_inplace(fused7, INPLACE_NONE)
make_inplace(fused8, INPLACE_1)
make_inplace(fused9, INPLACE_1)
vopdes_t vops[] = {
  {FUSED, "fused0", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused1", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused2", 3, 1,
  {Int,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused3", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused4", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused5", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused6", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused7", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused8", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused9", 3, 1,
  {Int,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise3},
  };

cvl_triple_t cvl_funs[] = {
  { { (void (*)())fused0, (int (*)())fused0_scratch, (unsigned (*)())fused0_inplace },},
  { { (void (*)())fused1, (int (*)())fused1_scratch, (unsigned (*)())fused1_inplace },},
  { { (void (*)())fused2, (int (*)())fused2_scratch, (unsigned (*)())fused2_inplace },},
  { { (void (*)())fused3, (int (*)())fused3_scratch, (unsigned (*)())fused3_inplace },},
  { { (void (*)())fused4, (int (*)())fused4_scratch, (unsigned (*)())fused4_inplace },},
  { { (void (*)())fused5, (int (*)())fused5_scratch, (unsigned (*)())fused5_inplace },},
  { { (void (*)())fused6, (int (*)())fused6_scratch, (unsigned (*)())fused6_inplace },},
  { { (void (*)())fused7, (int (*)())fused7_scratch, (unsigned (*)())fused7_inplace },},
  { { (void (*)())fused8, (int (*)())fused8_scratch, (unsigned (*)())fused8_inplace },},
  { { (void (*)())fused9, (int (*)())fused9_scratch, (unsigned (*)())fused9_inplace },},
  };
/*
fused OP0#1 ($0 : SEGDES) = (DIST INT @ (CONST INT 100) $0)
fused OP1#1 ($0 : INT, $1 : SEGDES) = (B_TO_I @ (< INT @ $0 (DIST INT @ (CONST INT 2) $1)))
fused OP2#1 ($0 : INT, $1 : INT, $2 : INT) = (= INT @ (- INT @ $0 $1) $2)
fused OP3#2 ($0 : INT, $1 : SEGDES) = (/ INT @ $0 (DIST INT @ (CONST INT 2) $1))
fused OP4#13 ($0 : SEGDES) = (DIST INT @ (CONST INT 1) $0)
fused OP5#7 ($0 : SEGDES) = (DIST INT @ (CONST INT 0) $0)
fused OP6#1 ($0 : INT, $1 : SEGDES) = (NOT BOOL @ (< INT @ $0 (DIST INT @ (CONST INT 2) $1)))
fused OP7#1 ($0 : SEGDES) = (DIST INT @ (CONST INT -1) $0)
fused OP8#1 ($0 : INT, $1 : SEGDES) = (< INT @ $0 (DIST INT @ (CONST INT 0) $1))
fused OP9#1 ($0 : INT, $1 : INT, $2 : INT) = (- INT @ (+ INT @ $0 $1) $2)
*/
