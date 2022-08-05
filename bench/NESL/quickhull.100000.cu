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

__global__ void fused0Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    
    pDst[address] = (cvl_round((divide((times((z_to_d(pSrc0[address])), pSrc1[address])), pSrc2[address]))));
  }
}

__global__ void fused1Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (2.0);
  }
}

__global__ void fused2Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (minus(pSrc0[address], (1.0)));
  }
}

__global__ void fused3Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int s6, int s7, int s8, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    int *pSrc3 = (int*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    int *pSrc5 = (int*)(&data[s5]);
    float *pSrc6 = (float*)(&data[s6]);
    int *pSrc7 = (int*)(&data[s7]);
    int *pSrc8 = (int*)(&data[s8]);
    
    pDst[address] = (lt((plus((times((minus(pSrc0[address], (1.0))), (minus(pSrc2[address], (1.0))))), (times((minus(pSrc4[address], (1.0))), (minus(pSrc6[address], (1.0))))))), (1.0)));
  }
}

__global__ void fused4Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    int *pSrc3 = (int*)(&data[s3]);
    
    pDst[address] = (selection((eq(pSrc0[address], pSrc1[address])), pSrc2[address], pSrc3[address]));
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

__global__ void fused6Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (1);
  }
}

__global__ void fused7Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (lt(pSrc0[address], (2)));
  }
}

__global__ void fused8Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (b_to_z((lt(pSrc0[address], (2)))));
  }
}

__global__ void fused9Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    
    pDst[address] = (eq((minus(pSrc0[address], pSrc1[address])), pSrc2[address]));
  }
}

__global__ void fused10Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (nnot((lt(pSrc0[address], (2)))));
  }
}

__global__ void fused11Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (0.0);
  }
}

__global__ void fused12Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (gt(pSrc0[address], (0.0)));
  }
}

__global__ void fused13Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    
    pDst[address] = (times(pSrc0[address], (divide((z_to_d(pSrc1[address])), (z_to_d(pSrc2[address]))))));
  }
}

__global__ void fused14Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int s6, int s7, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    float *pSrc5 = (float*)(&data[s5]);
    float *pSrc6 = (float*)(&data[s6]);
    float *pSrc7 = (float*)(&data[s7]);
    
    pDst[address] = (minus((times((minus(pSrc0[address], pSrc1[address])), (minus(pSrc2[address], pSrc3[address])))), (times((minus(pSrc4[address], pSrc5[address])), (minus(pSrc6[address], pSrc7[address]))))));
  }
}

void fused0(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused0Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused0 execution failed\n");
}

void fused1(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused1Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused1 execution failed\n");
}

void fused2(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused2Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused2 execution failed\n");
}

void fused3(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, vec_p s6, vec_p s7, vec_p s8, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused3Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, s6, s7, s8, len, scratch);
  cutilCheckMsg("fused3 execution failed\n");
}

void fused4(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused4Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, len, scratch);
  cutilCheckMsg("fused4 execution failed\n");
}

void fused5(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused5Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused5 execution failed\n");
}

void fused6(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused6Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused6 execution failed\n");
}

void fused7(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused7Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
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

void fused10(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused10Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused10 execution failed\n");
}

void fused11(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused11Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused11 execution failed\n");
}

void fused12(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused12Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused12 execution failed\n");
}

void fused13(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused13Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused13 execution failed\n");
}

void fused14(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, vec_p s6, vec_p s7, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused14Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, s6, s7, len, scratch);
  cutilCheckMsg("fused14 execution failed\n");
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
make_no_scratch(fused10)
make_no_scratch(fused11)
make_no_scratch(fused12)
make_no_scratch(fused13)
make_no_scratch(fused14)
make_inplace(fused0, INPLACE_1)
make_inplace(fused1, INPLACE_NONE)
make_inplace(fused2, INPLACE_1)
make_inplace(fused3, INPLACE_1)
make_inplace(fused4, INPLACE_1)
make_inplace(fused5, INPLACE_NONE)
make_inplace(fused6, INPLACE_NONE)
make_inplace(fused7, INPLACE_1)
make_inplace(fused8, INPLACE_1)
make_inplace(fused9, INPLACE_1)
make_inplace(fused10, INPLACE_1)
make_inplace(fused11, INPLACE_NONE)
make_inplace(fused12, INPLACE_1)
make_inplace(fused13, INPLACE_1)
make_inplace(fused14, INPLACE_1)
vopdes_t vops[] = {
  {FUSED, "fused0", 3, 1,
  {Int,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused1", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused2", 2, 1,
  {Float,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused3", 9, 1,
  {Float,Segdes,Float,Segdes,Float,Segdes,Float,Segdes,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise9},
  {FUSED, "fused4", 4, 1,
  {Float,Float,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise4},
  {FUSED, "fused5", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused6", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused7", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused8", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused9", 3, 1,
  {Int,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused10", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused11", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused12", 2, 1,
  {Float,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused13", 3, 1,
  {Float,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused14", 8, 1,
  {Float,Float,Float,Float,Float,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise8},
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
  { { (void (*)())fused10, (int (*)())fused10_scratch, (unsigned (*)())fused10_inplace },},
  { { (void (*)())fused11, (int (*)())fused11_scratch, (unsigned (*)())fused11_inplace },},
  { { (void (*)())fused12, (int (*)())fused12_scratch, (unsigned (*)())fused12_inplace },},
  { { (void (*)())fused13, (int (*)())fused13_scratch, (unsigned (*)())fused13_inplace },},
  { { (void (*)())fused14, (int (*)())fused14_scratch, (unsigned (*)())fused14_inplace },},
  };
/*
fused OP0#1 ($0 : INT, $1 : FLOAT, $2 : FLOAT) = (ROUND @ (/ FLOAT @ (* FLOAT @ (I_TO_F @ $0) $1) $2))
fused OP1#1 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 2.0) $0)
fused OP2#2 ($0 : FLOAT, $1 : SEGDES) = (- FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 1.0) $1))
fused OP3#1 ($0 : FLOAT, $1 : SEGDES, $2 : FLOAT, $3 : SEGDES, $4 : FLOAT, $5 : SEGDES, $6 : FLOAT, $7 : SEGDES, $8 : SEGDES) = (< FLOAT
  @
  (+ FLOAT @
    (* FLOAT @ (- FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 1.0) $1)) (- FLOAT @ $2 (DIST FLOAT @ (CONST FLOAT 1.0) $3)))
    (* FLOAT @ (- FLOAT @ $4 (DIST FLOAT @ (CONST FLOAT 1.0) $5)) (- FLOAT @ $6 (DIST FLOAT @ (CONST FLOAT 1.0) $7))))
  (DIST FLOAT @ (CONST FLOAT 1.0) $8))
fused OP4#3 ($0 : FLOAT, $1 : FLOAT, $2 : INT, $3 : INT) = (SELECT INT @ (= FLOAT @ $0 $1) $2 $3)
fused OP5#4 ($0 : SEGDES) = (DIST INT @ (CONST INT 0) $0)
fused OP6#13 ($0 : SEGDES) = (DIST INT @ (CONST INT 1) $0)
fused OP7#1 ($0 : INT, $1 : SEGDES) = (< INT @ $0 (DIST INT @ (CONST INT 2) $1))
fused OP8#1 ($0 : INT, $1 : SEGDES) = (B_TO_I @ (< INT @ $0 (DIST INT @ (CONST INT 2) $1)))
fused OP9#1 ($0 : INT, $1 : INT, $2 : INT) = (= INT @ (- INT @ $0 $1) $2)
fused OP10#1 ($0 : INT, $1 : SEGDES) = (NOT BOOL @ (< INT @ $0 (DIST INT @ (CONST INT 2) $1)))
fused OP11#2 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 0.0) $0)
fused OP12#1 ($0 : FLOAT, $1 : SEGDES) = (> FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 0.0) $1))
fused OP13#1 ($0 : FLOAT, $1 : INT, $2 : INT) = (* FLOAT @ $0 (/ FLOAT @ (I_TO_F @ $1) (I_TO_F @ $2)))
fused OP14#1 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT, $4 : FLOAT, $5 : FLOAT, $6 : FLOAT, $7 : FLOAT) = (- FLOAT
  @ (* FLOAT @ (- FLOAT @ $0 $1) (- FLOAT @ $2 $3)) (* FLOAT @ (- FLOAT @ $4 $5) (- FLOAT @ $6 $7)))
*/
