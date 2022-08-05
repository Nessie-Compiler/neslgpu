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
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (100.0);
  }
}

__global__ void fused1Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (1.0);
  }
}

__global__ void fused2Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (0.05);
  }
}

__global__ void fused3Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (0.4);
  }
}

__global__ void fused4Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (2.0);
  }
}

__global__ void fused5Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    
    pDst[address] = (plus(pSrc0[address], (times((plus(pSrc1[address], pSrc2[address])), pSrc3[address]))));
  }
}

__global__ void fused6Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    
    pDst[address] = (minus(pSrc0[address], (times(pSrc1[address], pSrc2[address]))));
  }
}

__global__ void fused7Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    float *pSrc5 = (float*)(&data[s5]);
    
    pDst[address] = (minus((times(pSrc0[address], pSrc1[address])), (times((times(pSrc2[address], (exp((times(pSrc3[address], pSrc4[address])))))), pSrc5[address]))));
  }
}

__global__ void fused8Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    int *pSrc3 = (int*)(&data[s3]);
    
    pDst[address] = (oor((lt(pSrc0[address], pSrc1[address])), (geq(pSrc2[address], pSrc3[address]))));
  }
}

__global__ void fused9Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    
    pDst[address] = (times((exp((z_to_d(pSrc0[address])))), pSrc1[address]));
  }
}

__global__ void fused10Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (b_to_z((eq(pSrc0[address], (0.0)))));
  }
}

__global__ void fused11Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    
    pDst[address] = (eq((minus(pSrc0[address], pSrc1[address])), pSrc2[address]));
  }
}

__global__ void fused12Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (FALSE);
  }
}

__global__ void fused13Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    
    pDst[address] = (minus((0.0), pSrc1[address]));
  }
}

__global__ void fused14Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    
    pDst[address] = (times(pSrc0[address], (atan(pSrc1[address]))));
  }
}

__global__ void fused15Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    
    pDst[address] = (plus((1.0), (times((0.2316419), (selection(pSrc2[address], pSrc3[address], pSrc4[address]))))));
  }
}

__global__ void fused16Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    
    pDst[address] = (times((2.0), pSrc1[address]));
  }
}

__global__ void fused17Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    
    pDst[address] = (times(pSrc0[address], (selection(pSrc1[address], pSrc2[address], pSrc3[address]))));
  }
}

__global__ void fused18Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (3);
  }
}

__global__ void fused19Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (4);
  }
}

__global__ void fused20Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (5);
  }
}

__global__ void fused21Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int s6, int s7, int s8, int s9, int s10, int s11, int s12, int s13, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    int *pSrc3 = (int*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    float *pSrc5 = (float*)(&data[s5]);
    float *pSrc6 = (float*)(&data[s6]);
    float *pSrc7 = (float*)(&data[s7]);
    int *pSrc8 = (int*)(&data[s8]);
    float *pSrc9 = (float*)(&data[s9]);
    float *pSrc10 = (float*)(&data[s10]);
    float *pSrc11 = (float*)(&data[s11]);
    int *pSrc12 = (int*)(&data[s12]);
    float *pSrc13 = (float*)(&data[s13]);
    
    pDst[address] = (minus((1.0), (times((times(pSrc1[address], (exp(pSrc2[address])))), (plus((plus((plus((plus((times((0.31938153), pSrc4[address])), (times((times(pSrc5[address], pSrc6[address])), pSrc7[address])))), (times((1.781477937), pSrc9[address])))), (times(pSrc10[address], pSrc11[address])))), (times((1.330274429), pSrc13[address]))))))));
  }
}

__global__ void fused22Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (lt(pSrc0[address], (0.0)));
  }
}

__global__ void fused23Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (b_to_z((lt(pSrc0[address], (0.0)))));
  }
}

__global__ void fused24Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int s6, int s7, int s8, int s9, int s10, int s11, int s12, int s13, int s14, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    int *pSrc4 = (int*)(&data[s4]);
    float *pSrc5 = (float*)(&data[s5]);
    float *pSrc6 = (float*)(&data[s6]);
    float *pSrc7 = (float*)(&data[s7]);
    float *pSrc8 = (float*)(&data[s8]);
    int *pSrc9 = (int*)(&data[s9]);
    float *pSrc10 = (float*)(&data[s10]);
    float *pSrc11 = (float*)(&data[s11]);
    float *pSrc12 = (float*)(&data[s12]);
    int *pSrc13 = (int*)(&data[s13]);
    float *pSrc14 = (float*)(&data[s14]);
    
    pDst[address] = (minus((1.0), (minus((1.0), (times((times(pSrc2[address], (exp(pSrc3[address])))), (plus((plus((plus((plus((times((0.31938153), pSrc5[address])), (times((times(pSrc6[address], pSrc7[address])), pSrc8[address])))), (times((1.781477937), pSrc10[address])))), (times(pSrc11[address], pSrc12[address])))), (times((1.330274429), pSrc14[address]))))))))));
  }
}

__global__ void fused25Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    int *pSrc3 = (int*)(&data[s3]);
    
    pDst[address] = (oor((lt(pSrc0[address], (0))), (geq(pSrc2[address], pSrc3[address]))));
  }
}

__global__ void fused26Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    
    pDst[address] = (minus((1.0), pSrc1[address]));
  }
}

__global__ void fused27Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (gt(pSrc0[address], (0.0)));
  }
}

__global__ void fused28Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (b_to_z((nnot(pSrc0[address]))));
  }
}

void fused0(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused0Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused0 execution failed\n");
}

void fused1(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused1Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused1 execution failed\n");
}

void fused2(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused2Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused2 execution failed\n");
}

void fused3(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused3Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused3 execution failed\n");
}

void fused4(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused4Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused4 execution failed\n");
}

void fused5(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused5Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, len, scratch);
  cutilCheckMsg("fused5 execution failed\n");
}

void fused6(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused6Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused6 execution failed\n");
}

void fused7(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused7Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, len, scratch);
  cutilCheckMsg("fused7 execution failed\n");
}

void fused8(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused8Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, len, scratch);
  cutilCheckMsg("fused8 execution failed\n");
}

void fused9(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused9Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused9 execution failed\n");
}

void fused10(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused10Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused10 execution failed\n");
}

void fused11(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused11Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused11 execution failed\n");
}

void fused12(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused12Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused12 execution failed\n");
}

void fused13(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused13Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused13 execution failed\n");
}

void fused14(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused14Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused14 execution failed\n");
}

void fused15(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused15Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, len, scratch);
  cutilCheckMsg("fused15 execution failed\n");
}

void fused16(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused16Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused16 execution failed\n");
}

void fused17(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused17Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, len, scratch);
  cutilCheckMsg("fused17 execution failed\n");
}

void fused18(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused18Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused18 execution failed\n");
}

void fused19(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused19Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused19 execution failed\n");
}

void fused20(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused20Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused20 execution failed\n");
}

void fused21(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, vec_p s6, vec_p s7, vec_p s8, vec_p s9, vec_p s10, vec_p s11, vec_p s12, vec_p s13, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused21Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, len, scratch);
  cutilCheckMsg("fused21 execution failed\n");
}

void fused22(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused22Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused22 execution failed\n");
}

void fused23(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused23Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused23 execution failed\n");
}

void fused24(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, vec_p s6, vec_p s7, vec_p s8, vec_p s9, vec_p s10, vec_p s11, vec_p s12, vec_p s13, vec_p s14, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused24Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, len, scratch);
  cutilCheckMsg("fused24 execution failed\n");
}

void fused25(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused25Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, len, scratch);
  cutilCheckMsg("fused25 execution failed\n");
}

void fused26(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused26Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused26 execution failed\n");
}

void fused27(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused27Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused27 execution failed\n");
}

void fused28(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused28Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused28 execution failed\n");
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
make_no_scratch(fused15)
make_no_scratch(fused16)
make_no_scratch(fused17)
make_no_scratch(fused18)
make_no_scratch(fused19)
make_no_scratch(fused20)
make_no_scratch(fused21)
make_no_scratch(fused22)
make_no_scratch(fused23)
make_no_scratch(fused24)
make_no_scratch(fused25)
make_no_scratch(fused26)
make_no_scratch(fused27)
make_no_scratch(fused28)
make_inplace(fused0, INPLACE_NONE)
make_inplace(fused1, INPLACE_NONE)
make_inplace(fused2, INPLACE_NONE)
make_inplace(fused3, INPLACE_NONE)
make_inplace(fused4, INPLACE_NONE)
make_inplace(fused5, INPLACE_1)
make_inplace(fused6, INPLACE_1)
make_inplace(fused7, INPLACE_1)
make_inplace(fused8, INPLACE_1)
make_inplace(fused9, INPLACE_1)
make_inplace(fused10, INPLACE_1)
make_inplace(fused11, INPLACE_1)
make_inplace(fused12, INPLACE_NONE)
make_inplace(fused13, INPLACE_2)
make_inplace(fused14, INPLACE_1)
make_inplace(fused15, INPLACE_3)
make_inplace(fused16, INPLACE_2)
make_inplace(fused17, INPLACE_1)
make_inplace(fused18, INPLACE_NONE)
make_inplace(fused19, INPLACE_NONE)
make_inplace(fused20, INPLACE_NONE)
make_inplace(fused21, INPLACE_2)
make_inplace(fused22, INPLACE_1)
make_inplace(fused23, INPLACE_1)
make_inplace(fused24, INPLACE_3)
make_inplace(fused25, INPLACE_1)
make_inplace(fused26, INPLACE_2)
make_inplace(fused27, INPLACE_1)
make_inplace(fused28, INPLACE_1)
vopdes_t vops[] = {
  {FUSED, "fused0", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused1", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused2", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused3", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused4", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused5", 4, 1,
  {Float,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise4},
  {FUSED, "fused6", 3, 1,
  {Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused7", 6, 1,
  {Float,Float,Float,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise6},
  {FUSED, "fused8", 4, 1,
  {Int,Int,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise4},
  {FUSED, "fused9", 2, 1,
  {Int,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused10", 2, 1,
  {Float,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused11", 3, 1,
  {Int,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused12", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused13", 2, 1,
  {Segdes,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise2},
  {FUSED, "fused14", 2, 1,
  {Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused15", 5, 1,
  {Segdes,Segdes,Bool,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise5},
  {FUSED, "fused16", 2, 1,
  {Segdes,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise2},
  {FUSED, "fused17", 4, 1,
  {Float,Bool,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise4},
  {FUSED, "fused18", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused19", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused20", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused21", 14, 1,
  {Segdes,Float,Float,Segdes,Float,Float,Float,Float,Segdes,Float,Float,Float,Segdes,Float,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise14},
  {FUSED, "fused22", 2, 1,
  {Float,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused23", 2, 1,
  {Float,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused24", 15, 1,
  {Segdes,Segdes,Float,Float,Segdes,Float,Float,Float,Float,Segdes,Float,Float,Float,Segdes,Float,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise15},
  {FUSED, "fused25", 4, 1,
  {Int,Segdes,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise4},
  {FUSED, "fused26", 2, 1,
  {Segdes,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise2},
  {FUSED, "fused27", 2, 1,
  {Float,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused28", 1, 1,
  {Bool,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise1},
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
  { { (void (*)())fused15, (int (*)())fused15_scratch, (unsigned (*)())fused15_inplace },},
  { { (void (*)())fused16, (int (*)())fused16_scratch, (unsigned (*)())fused16_inplace },},
  { { (void (*)())fused17, (int (*)())fused17_scratch, (unsigned (*)())fused17_inplace },},
  { { (void (*)())fused18, (int (*)())fused18_scratch, (unsigned (*)())fused18_inplace },},
  { { (void (*)())fused19, (int (*)())fused19_scratch, (unsigned (*)())fused19_inplace },},
  { { (void (*)())fused20, (int (*)())fused20_scratch, (unsigned (*)())fused20_inplace },},
  { { (void (*)())fused21, (int (*)())fused21_scratch, (unsigned (*)())fused21_inplace },},
  { { (void (*)())fused22, (int (*)())fused22_scratch, (unsigned (*)())fused22_inplace },},
  { { (void (*)())fused23, (int (*)())fused23_scratch, (unsigned (*)())fused23_inplace },},
  { { (void (*)())fused24, (int (*)())fused24_scratch, (unsigned (*)())fused24_inplace },},
  { { (void (*)())fused25, (int (*)())fused25_scratch, (unsigned (*)())fused25_inplace },},
  { { (void (*)())fused26, (int (*)())fused26_scratch, (unsigned (*)())fused26_inplace },},
  { { (void (*)())fused27, (int (*)())fused27_scratch, (unsigned (*)())fused27_inplace },},
  { { (void (*)())fused28, (int (*)())fused28_scratch, (unsigned (*)())fused28_inplace },},
  };
/*
fused OP0#2 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 100.0) $0)
fused OP1#3 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 1.0) $0)
fused OP2#1 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 0.05) $0)
fused OP3#1 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 0.4) $0)
fused OP4#2 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 2.0) $0)
fused OP5#1 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT) = (+ FLOAT @ $0 (* FLOAT @ (+ FLOAT @ $1 $2) $3))
fused OP6#1 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT) = (- FLOAT @ $0 (* FLOAT @ $1 $2))
fused OP7#1 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT, $4 : FLOAT, $5 : FLOAT) = (- FLOAT @ (* FLOAT @ $0 $1)
  (* FLOAT @ (* FLOAT @ $2 (EXP @ (* FLOAT @ $3 $4))) $5))
fused OP8#1 ($0 : INT, $1 : INT, $2 : INT, $3 : INT) = (OR BOOL @ (< INT @ $0 $1) (>= INT @ $2 $3))
fused OP9#1 ($0 : INT, $1 : FLOAT) = (* FLOAT @ (EXP @ (I_TO_F @ $0)) $1)
fused OP10#1 ($0 : FLOAT, $1 : SEGDES) = (B_TO_I @ (= FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 0.0) $1)))
fused OP11#5 ($0 : INT, $1 : INT, $2 : INT) = (= INT @ (- INT @ $0 $1) $2)
fused OP12#4 ($0 : SEGDES) = (DIST BOOL @ (CONST BOOL F) $0)
fused OP13#1 ($0 : SEGDES, $1 : FLOAT) = (- FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.0) $0) $1)
fused OP14#1 ($0 : FLOAT, $1 : FLOAT) = (* FLOAT @ $0 (ATAN @ $1))
fused OP15#1 ($0 : SEGDES, $1 : SEGDES, $2 : BOOL, $3 : FLOAT, $4 : FLOAT) = (+ FLOAT @
  (DIST FLOAT @ (CONST FLOAT 1.0) $0) (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.2316419) $1) (SELECT FLOAT @ $2 $3 $4)))
fused OP16#1 ($0 : SEGDES, $1 : FLOAT) = (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 2.0) $0) $1)
fused OP17#1 ($0 : FLOAT, $1 : BOOL, $2 : FLOAT, $3 : FLOAT) = (* FLOAT @ $0 (SELECT FLOAT @ $1 $2 $3))
fused OP18#1 ($0 : SEGDES) = (DIST INT @ (CONST INT 3) $0)
fused OP19#1 ($0 : SEGDES) = (DIST INT @ (CONST INT 4) $0)
fused OP20#1 ($0 : SEGDES) = (DIST INT @ (CONST INT 5) $0)
fused OP21#1 ($0 : SEGDES, $1 : FLOAT, $2 : FLOAT, $3 : SEGDES, $4 : FLOAT, $5 : FLOAT, $6 : FLOAT, $7 : FLOAT, $8 : SEGDES, $9 : FLOAT, $10 : FLOAT, $11 : FLOAT, $12 : SEGDES, $13 : FLOAT) = (- FLOAT
  @ (DIST FLOAT @ (CONST FLOAT 1.0) $0)
  (* FLOAT @ (* FLOAT @ $1 (EXP @ $2))
    (+ FLOAT @
      (+ FLOAT @
        (+ FLOAT @
          (+ FLOAT @ (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.31938153) $3) $4) (* FLOAT @ (* FLOAT @ $5 $6) $7))
          (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.781477937) $8) $9)) (* FLOAT @ $10 $11))
      (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.330274429) $12) $13))))
fused OP22#2 ($0 : FLOAT, $1 : SEGDES) = (< FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 0.0) $1))
fused OP23#1 ($0 : FLOAT, $1 : SEGDES) = (B_TO_I @ (< FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 0.0) $1)))
fused OP24#1 ($0 : SEGDES, $1 : SEGDES, $2 : FLOAT, $3 : FLOAT, $4 : SEGDES, $5 : FLOAT, $6 : FLOAT, $7 : FLOAT, $8 : FLOAT, $9 : SEGDES, $10 : FLOAT, $11 : FLOAT, $12 : FLOAT, $13 : SEGDES, $14 : FLOAT) = (- FLOAT
  @ (DIST FLOAT @ (CONST FLOAT 1.0) $0)
  (- FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $1)
    (* FLOAT @ (* FLOAT @ $2 (EXP @ $3))
      (+ FLOAT @
        (+ FLOAT @
          (+ FLOAT @
            (+ FLOAT @ (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.31938153) $4) $5) (* FLOAT @ (* FLOAT @ $6 $7) $8))
            (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.781477937) $9) $10)) (* FLOAT @ $11 $12))
        (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.330274429) $13) $14)))))
fused OP25#1 ($0 : INT, $1 : SEGDES, $2 : INT, $3 : INT) = (OR BOOL @ (< INT @ $0 (DIST INT @ (CONST INT 0) $1))
  (>= INT @ $2 $3))
fused OP26#1 ($0 : SEGDES, $1 : FLOAT) = (- FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $0) $1)
fused OP27#1 ($0 : FLOAT, $1 : SEGDES) = (> FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 0.0) $1))
fused OP28#1 ($0 : BOOL) = (B_TO_I @ (NOT BOOL @ $0))
*/
