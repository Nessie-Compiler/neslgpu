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
    
    pDst[address] = (10.0);
  }
}

__global__ void fused1Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (minus(pSrc0[address], (5.0)));
  }
}

__global__ void fused2Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (1.0);
  }
}

__global__ void fused3Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (1);
  }
}

__global__ void fused4Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (0);
  }
}

__global__ void fused5Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (2);
  }
}

__global__ void fused6Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (3);
  }
}

__global__ void fused7Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (4);
  }
}

__global__ void fused8Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (5);
  }
}

__global__ void fused9Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (6);
  }
}

__global__ void fused10Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    
    pDst[address] = (divide((minus(pSrc0[address], pSrc1[address])), pSrc2[address]));
  }
}

__global__ void fused11Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    
    pDst[address] = (geq(pSrc0[address], (plus(pSrc1[address], pSrc2[address]))));
  }
}

__global__ void fused12Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    
    pDst[address] = (plus(pSrc0[address], (divide((minus(pSrc1[address], pSrc2[address])), pSrc3[address]))));
  }
}

__global__ void fused13Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (gt(pSrc0[address], (0)));
  }
}

__global__ void fused14Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (plus(pSrc0[address], (1)));
  }
}

__global__ void fused15Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (b_to_z((eq(pSrc0[address], (1)))));
  }
}

__global__ void fused16Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (TRUE);
  }
}

__global__ void fused17Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    
    pDst[address] = (eq((minus(pSrc0[address], pSrc1[address])), pSrc2[address]));
  }
}

__global__ void fused18Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    
    pDst[address] = (lt(pSrc0[address], (times((0.5), (sqrt(pSrc2[address]))))));
  }
}

__global__ void fused19Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (nnot((eq(pSrc0[address], (1)))));
  }
}

__global__ void fused20Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    int *pSrc3 = (int*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    int *pSrc5 = (int*)(&data[s5]);
    
    pDst[address] = (b_to_z((aand((aand((eq(pSrc0[address], (0.0))), (eq(pSrc2[address], (0.0))))), (eq(pSrc4[address], (0.0)))))));
  }
}

__global__ void fused21Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    
    pDst[address] = (times(pSrc0[address], (divide(pSrc1[address], (times(pSrc2[address], (sqrt(pSrc3[address]))))))));
  }
}

__global__ void fused22Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    
    pDst[address] = (divide((1.0), (times((times(pSrc1[address], pSrc2[address])), (sqrt(pSrc3[address]))))));
  }
}

__global__ void fused23Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int s6, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    float *pSrc5 = (float*)(&data[s5]);
    float *pSrc6 = (float*)(&data[s6]);
    
    pDst[address] = (divide((times((times(pSrc0[address], pSrc1[address])), (divide((1.0), (times((times(pSrc3[address], pSrc4[address])), (sqrt(pSrc5[address])))))))), pSrc6[address]));
  }
}

__global__ void fused24Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    
    pDst[address] = (divide((1.0), (times((times(pSrc1[address], pSrc2[address])), pSrc3[address]))));
  }
}

__global__ void fused25Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int s6, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    float *pSrc5 = (float*)(&data[s5]);
    float *pSrc6 = (float*)(&data[s6]);
    
    pDst[address] = (divide((times((times(pSrc0[address], pSrc1[address])), (divide((1.0), (times((times(pSrc3[address], pSrc4[address])), pSrc5[address])))))), pSrc6[address]));
  }
}

__global__ void fused26Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    int *pSrc3 = (int*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    int *pSrc5 = (int*)(&data[s5]);
    
    pDst[address] = (nnot((aand((aand((eq(pSrc0[address], (0.0))), (eq(pSrc2[address], (0.0))))), (eq(pSrc4[address], (0.0)))))));
  }
}

__global__ void fused27Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int s6, int s7, int s8, int s9, int len, int scratch) {
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
    float *pSrc8 = (float*)(&data[s8]);
    float *pSrc9 = (float*)(&data[s9]);
    
    pDst[address] = (selection((gt(pSrc0[address], (selection((gt(pSrc1[address], pSrc2[address])), pSrc3[address], pSrc4[address])))), pSrc5[address], (selection((gt(pSrc6[address], pSrc7[address])), pSrc8[address], pSrc9[address]))));
  }
}

__global__ void fused28Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    int *pSrc3 = (int*)(&data[s3]);
    int *pSrc4 = (int*)(&data[s4]);
    int *pSrc5 = (int*)(&data[s5]);
    
    pDst[address] = (mod((plus(pSrc0[address], (times(pSrc1[address], (minus((1), (divide(pSrc3[address], pSrc4[address])))))))), pSrc5[address]));
  }
}

__global__ void fused29Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (3.0);
  }
}

__global__ void fused30Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (neq(pSrc0[address], (1)));
  }
}

__global__ void fused31Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (b_to_z((neq(pSrc0[address], (1)))));
  }
}

__global__ void fused32Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (b_to_z((nnot(pSrc0[address]))));
  }
}

__global__ void fused33Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    int *pSrc3 = (int*)(&data[s3]);
    
    pDst[address] = (selection(pSrc0[address], (plus(pSrc1[address], pSrc2[address])), pSrc3[address]));
  }
}

__global__ void fused34Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (eq(pSrc0[address], (1)));
  }
}

__global__ void fused35Kernel(MAXALIGN *data, int dst, int s0, int s1, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    
    pDst[address] = (divide(pSrc0[address], (2.0)));
  }
}

__global__ void fused36Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (0.0);
  }
}

__global__ void fused37Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    
    pDst[address] = (gt((plus(pSrc0[address], pSrc1[address])), pSrc2[address]));
  }
}

__global__ void fused38Kernel(MAXALIGN *data, int dst, int s0, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    int *pDst = (int*)(&data[dst]);
    int *pSrc0 = (int*)(&data[s0]);
    
    pDst[address] = (7);
  }
}

__global__ void fused39Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    int *pSrc1 = (int*)(&data[s1]);
    int *pSrc2 = (int*)(&data[s2]);
    
    pDst[address] = (times(pSrc0[address], (divide((z_to_d(pSrc1[address])), (z_to_d(pSrc2[address]))))));
  }
}

__global__ void fused40Kernel(MAXALIGN *data, int dst, int s0, int s1, int s2, int s3, int s4, int s5, int len, int scratch) {
  int address = blockDim.y * blockIdx.y + blockDim.x * blockIdx.x + threadIdx.x;
  if (address < len) {
    float *pDst = (float*)(&data[dst]);
    float *pSrc0 = (float*)(&data[s0]);
    float *pSrc1 = (float*)(&data[s1]);
    float *pSrc2 = (float*)(&data[s2]);
    float *pSrc3 = (float*)(&data[s3]);
    float *pSrc4 = (float*)(&data[s4]);
    float *pSrc5 = (float*)(&data[s5]);
    
    pDst[address] = (plus((plus((times(pSrc0[address], pSrc1[address])), (times(pSrc2[address], pSrc3[address])))), (times(pSrc4[address], pSrc5[address]))));
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

void fused7(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused7Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused7 execution failed\n");
}

void fused8(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused8Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused8 execution failed\n");
}

void fused9(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused9Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused9 execution failed\n");
}

void fused10(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused10Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused10 execution failed\n");
}

void fused11(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused11Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused11 execution failed\n");
}

void fused12(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused12Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, len, scratch);
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

void fused15(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused15Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused15 execution failed\n");
}

void fused16(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused16Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused16 execution failed\n");
}

void fused17(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused17Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused17 execution failed\n");
}

void fused18(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused18Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused18 execution failed\n");
}

void fused19(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused19Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused19 execution failed\n");
}

void fused20(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused20Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, len, scratch);
  cutilCheckMsg("fused20 execution failed\n");
}

void fused21(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused21Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, len, scratch);
  cutilCheckMsg("fused21 execution failed\n");
}

void fused22(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused22Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, len, scratch);
  cutilCheckMsg("fused22 execution failed\n");
}

void fused23(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, vec_p s6, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused23Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, s6, len, scratch);
  cutilCheckMsg("fused23 execution failed\n");
}

void fused24(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused24Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, len, scratch);
  cutilCheckMsg("fused24 execution failed\n");
}

void fused25(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, vec_p s6, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused25Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, s6, len, scratch);
  cutilCheckMsg("fused25 execution failed\n");
}

void fused26(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused26Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, len, scratch);
  cutilCheckMsg("fused26 execution failed\n");
}

void fused27(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, vec_p s6, vec_p s7, vec_p s8, vec_p s9, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused27Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, len, scratch);
  cutilCheckMsg("fused27 execution failed\n");
}

void fused28(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused28Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, len, scratch);
  cutilCheckMsg("fused28 execution failed\n");
}

void fused29(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused29Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused29 execution failed\n");
}

void fused30(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused30Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused30 execution failed\n");
}

void fused31(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused31Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused31 execution failed\n");
}

void fused32(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused32Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused32 execution failed\n");
}

void fused33(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused33Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, len, scratch);
  cutilCheckMsg("fused33 execution failed\n");
}

void fused34(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused34Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused34 execution failed\n");
}

void fused35(vec_p d, vec_p s0, vec_p s1, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused35Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, len, scratch);
  cutilCheckMsg("fused35 execution failed\n");
}

void fused36(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused36Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused36 execution failed\n");
}

void fused37(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused37Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused37 execution failed\n");
}

void fused38(vec_p d, vec_p s0, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused38Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, len, scratch);
  cutilCheckMsg("fused38 execution failed\n");
}

void fused39(vec_p d, vec_p s0, vec_p s1, vec_p s2, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused39Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, len, scratch);
  cutilCheckMsg("fused39 execution failed\n");
}

void fused40(vec_p d, vec_p s0, vec_p s1, vec_p s2, vec_p s3, vec_p s4, vec_p s5, int len, vec_p scratch) {
  if (len==0) {return;}
  SYNC();
  DEF_BLOCKS_PER_GRID(len);
  fused40Kernel<<<blocksPerGrid, THREADS_PER_BLOCK>>>(ComputeMemory, d, s0, s1, s2, s3, s4, s5, len, scratch);
  cutilCheckMsg("fused40 execution failed\n");
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
make_no_scratch(fused29)
make_no_scratch(fused30)
make_no_scratch(fused31)
make_no_scratch(fused32)
make_no_scratch(fused33)
make_no_scratch(fused34)
make_no_scratch(fused35)
make_no_scratch(fused36)
make_no_scratch(fused37)
make_no_scratch(fused38)
make_no_scratch(fused39)
make_no_scratch(fused40)
make_inplace(fused0, INPLACE_NONE)
make_inplace(fused1, INPLACE_1)
make_inplace(fused2, INPLACE_NONE)
make_inplace(fused3, INPLACE_NONE)
make_inplace(fused4, INPLACE_NONE)
make_inplace(fused5, INPLACE_NONE)
make_inplace(fused6, INPLACE_NONE)
make_inplace(fused7, INPLACE_NONE)
make_inplace(fused8, INPLACE_NONE)
make_inplace(fused9, INPLACE_NONE)
make_inplace(fused10, INPLACE_1)
make_inplace(fused11, INPLACE_1)
make_inplace(fused12, INPLACE_1)
make_inplace(fused13, INPLACE_1)
make_inplace(fused14, INPLACE_1)
make_inplace(fused15, INPLACE_1)
make_inplace(fused16, INPLACE_NONE)
make_inplace(fused17, INPLACE_1)
make_inplace(fused18, INPLACE_1)
make_inplace(fused19, INPLACE_1)
make_inplace(fused20, INPLACE_1)
make_inplace(fused21, INPLACE_1)
make_inplace(fused22, INPLACE_2)
make_inplace(fused23, INPLACE_1)
make_inplace(fused24, INPLACE_2)
make_inplace(fused25, INPLACE_1)
make_inplace(fused26, INPLACE_1)
make_inplace(fused27, INPLACE_1)
make_inplace(fused28, INPLACE_1)
make_inplace(fused29, INPLACE_NONE)
make_inplace(fused30, INPLACE_1)
make_inplace(fused31, INPLACE_1)
make_inplace(fused32, INPLACE_1)
make_inplace(fused33, INPLACE_1)
make_inplace(fused34, INPLACE_1)
make_inplace(fused35, INPLACE_1)
make_inplace(fused36, INPLACE_NONE)
make_inplace(fused37, INPLACE_1)
make_inplace(fused38, INPLACE_NONE)
make_inplace(fused39, INPLACE_1)
make_inplace(fused40, INPLACE_1)
vopdes_t vops[] = {
  {FUSED, "fused0", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused1", 2, 1,
  {Float,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused2", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused3", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
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
  {FUSED, "fused6", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused7", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused8", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused9", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused10", 3, 1,
  {Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused11", 3, 1,
  {Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused12", 4, 1,
  {Float,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise4},
  {FUSED, "fused13", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused14", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused15", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused16", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused17", 3, 1,
  {Int,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused18", 3, 1,
  {Float,Segdes,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused19", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused20", 6, 1,
  {Float,Segdes,Float,Segdes,Float,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise6},
  {FUSED, "fused21", 4, 1,
  {Float,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise4},
  {FUSED, "fused22", 4, 1,
  {Segdes,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise4},
  {FUSED, "fused23", 7, 1,
  {Float,Float,Segdes,Float,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise7},
  {FUSED, "fused24", 4, 1,
  {Segdes,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise4},
  {FUSED, "fused25", 7, 1,
  {Float,Float,Segdes,Float,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise7},
  {FUSED, "fused26", 6, 1,
  {Float,Segdes,Float,Segdes,Float,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise6},
  {FUSED, "fused27", 10, 1,
  {Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise10},
  {FUSED, "fused28", 6, 1,
  {Int,Int,Segdes,Int,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise6},
  {FUSED, "fused29", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused30", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused31", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused32", 1, 1,
  {Bool,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise1},
  {FUSED, "fused33", 4, 1,
  {Bool,Int,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {AGREE1,},
  {1,},
  Elwise4},
  {FUSED, "fused34", 2, 1,
  {Int,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused35", 2, 1,
  {Float,Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise2},
  {FUSED, "fused36", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused37", 3, 1,
  {Int,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Bool,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused38", 1, 1,
  {Segdes,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Int,},
  {COMPAT1,},
  {1,},
  Elwise1},
  {FUSED, "fused39", 3, 1,
  {Float,Int,Int,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise3},
  {FUSED, "fused40", 6, 1,
  {Float,Float,Float,Float,Float,Float,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {NONE,NONE,NONE,NONE,NONE,NONE,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal,Illegal},
  {Float,},
  {AGREE1,},
  {1,},
  Elwise6},
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
  { { (void (*)())fused29, (int (*)())fused29_scratch, (unsigned (*)())fused29_inplace },},
  { { (void (*)())fused30, (int (*)())fused30_scratch, (unsigned (*)())fused30_inplace },},
  { { (void (*)())fused31, (int (*)())fused31_scratch, (unsigned (*)())fused31_inplace },},
  { { (void (*)())fused32, (int (*)())fused32_scratch, (unsigned (*)())fused32_inplace },},
  { { (void (*)())fused33, (int (*)())fused33_scratch, (unsigned (*)())fused33_inplace },},
  { { (void (*)())fused34, (int (*)())fused34_scratch, (unsigned (*)())fused34_inplace },},
  { { (void (*)())fused35, (int (*)())fused35_scratch, (unsigned (*)())fused35_inplace },},
  { { (void (*)())fused36, (int (*)())fused36_scratch, (unsigned (*)())fused36_inplace },},
  { { (void (*)())fused37, (int (*)())fused37_scratch, (unsigned (*)())fused37_inplace },},
  { { (void (*)())fused38, (int (*)())fused38_scratch, (unsigned (*)())fused38_inplace },},
  { { (void (*)())fused39, (int (*)())fused39_scratch, (unsigned (*)())fused39_inplace },},
  { { (void (*)())fused40, (int (*)())fused40_scratch, (unsigned (*)())fused40_inplace },},
  };
/*
fused OP0#2 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 10.0) $0)
fused OP1#6 ($0 : FLOAT, $1 : SEGDES) = (- FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 5.0) $1))
fused OP2#1 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 1.0) $0)
fused OP3#19 ($0 : SEGDES) = (DIST INT @ (CONST INT 1) $0)
fused OP4#13 ($0 : SEGDES) = (DIST INT @ (CONST INT 0) $0)
fused OP5#3 ($0 : SEGDES) = (DIST INT @ (CONST INT 2) $0)
fused OP6#2 ($0 : SEGDES) = (DIST INT @ (CONST INT 3) $0)
fused OP7#2 ($0 : SEGDES) = (DIST INT @ (CONST INT 4) $0)
fused OP8#2 ($0 : SEGDES) = (DIST INT @ (CONST INT 5) $0)
fused OP9#2 ($0 : SEGDES) = (DIST INT @ (CONST INT 6) $0)
fused OP10#3 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT) = (/ FLOAT @ (- FLOAT @ $0 $1) $2)
fused OP11#3 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT) = (>= FLOAT @ $0 (+ FLOAT @ $1 $2))
fused OP12#3 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT) = (+ FLOAT @ $0 (/ FLOAT @ (- FLOAT @ $1 $2) $3))
fused OP13#3 ($0 : INT, $1 : SEGDES) = (> INT @ $0 (DIST INT @ (CONST INT 0) $1))
fused OP14#5 ($0 : INT, $1 : SEGDES) = (+ INT @ $0 (DIST INT @ (CONST INT 1) $1))
fused OP15#4 ($0 : INT, $1 : SEGDES) = (B_TO_I @ (= INT @ $0 (DIST INT @ (CONST INT 1) $1)))
fused OP16#2 ($0 : SEGDES) = (DIST BOOL @ (CONST BOOL T) $0)
fused OP17#6 ($0 : INT, $1 : INT, $2 : INT) = (= INT @ (- INT @ $0 $1) $2)
fused OP18#2 ($0 : FLOAT, $1 : SEGDES, $2 : FLOAT) = (< FLOAT @ $0
  (* FLOAT @ (DIST FLOAT @ (CONST FLOAT 0.5) $1) (SQRT @ $2)))
fused OP19#4 ($0 : INT, $1 : SEGDES) = (NOT BOOL @ (= INT @ $0 (DIST INT @ (CONST INT 1) $1)))
fused OP20#1 ($0 : FLOAT, $1 : SEGDES, $2 : FLOAT, $3 : SEGDES, $4 : FLOAT, $5 : SEGDES) = (B_TO_I @
  (AND BOOL @
    (AND BOOL @ (= FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 0.0) $1)) (= FLOAT @ $2 (DIST FLOAT @ (CONST FLOAT 0.0) $3)))
    (= FLOAT @ $4 (DIST FLOAT @ (CONST FLOAT 0.0) $5))))
fused OP21#6 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT) = (* FLOAT @ $0
  (/ FLOAT @ $1 (* FLOAT @ $2 (SQRT @ $3))))
fused OP22#2 ($0 : SEGDES, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT) = (/ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $0)
  (* FLOAT @ (* FLOAT @ $1 $2) (SQRT @ $3)))
fused OP23#2 ($0 : FLOAT, $1 : FLOAT, $2 : SEGDES, $3 : FLOAT, $4 : FLOAT, $5 : FLOAT, $6 : FLOAT) = (/ FLOAT @
  (* FLOAT @ (* FLOAT @ $0 $1)
    (/ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $2) (* FLOAT @ (* FLOAT @ $3 $4) (SQRT @ $5)))) $6)
fused OP24#2 ($0 : SEGDES, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT) = (/ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $0)
  (* FLOAT @ (* FLOAT @ $1 $2) $3))
fused OP25#2 ($0 : FLOAT, $1 : FLOAT, $2 : SEGDES, $3 : FLOAT, $4 : FLOAT, $5 : FLOAT, $6 : FLOAT) = (/ FLOAT @
  (* FLOAT @ (* FLOAT @ $0 $1) (/ FLOAT @ (DIST FLOAT @ (CONST FLOAT 1.0) $2) (* FLOAT @ (* FLOAT @ $3 $4) $5))) $6)
fused OP26#1 ($0 : FLOAT, $1 : SEGDES, $2 : FLOAT, $3 : SEGDES, $4 : FLOAT, $5 : SEGDES) = (NOT BOOL @
  (AND BOOL @
    (AND BOOL @ (= FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 0.0) $1)) (= FLOAT @ $2 (DIST FLOAT @ (CONST FLOAT 0.0) $3)))
    (= FLOAT @ $4 (DIST FLOAT @ (CONST FLOAT 0.0) $5))))
fused OP27#1 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT, $4 : FLOAT, $5 : FLOAT, $6 : FLOAT, $7 : FLOAT, $8 : FLOAT, $9 : FLOAT) = (SELECT FLOAT
  @ (> FLOAT @ $0 (SELECT FLOAT @ (> FLOAT @ $1 $2) $3 $4)) $5 (SELECT FLOAT @ (> FLOAT @ $6 $7) $8 $9))
fused OP28#1 ($0 : INT, $1 : INT, $2 : SEGDES, $3 : INT, $4 : INT, $5 : INT) = (% @
  (+ INT @ $0 (* INT @ $1 (- INT @ (DIST INT @ (CONST INT 1) $2) (/ INT @ $3 $4)))) $5)
fused OP29#1 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 3.0) $0)
fused OP30#1 ($0 : INT, $1 : SEGDES) = (!= INT @ $0 (DIST INT @ (CONST INT 1) $1))
fused OP31#1 ($0 : INT, $1 : SEGDES) = (B_TO_I @ (!= INT @ $0 (DIST INT @ (CONST INT 1) $1)))
fused OP32#2 ($0 : BOOL) = (B_TO_I @ (NOT BOOL @ $0))
fused OP33#2 ($0 : BOOL, $1 : INT, $2 : INT, $3 : INT) = (SELECT INT @ $0 (+ INT @ $1 $2) $3)
fused OP34#1 ($0 : INT, $1 : SEGDES) = (= INT @ $0 (DIST INT @ (CONST INT 1) $1))
fused OP35#6 ($0 : FLOAT, $1 : SEGDES) = (/ FLOAT @ $0 (DIST FLOAT @ (CONST FLOAT 2.0) $1))
fused OP36#2 ($0 : SEGDES) = (DIST FLOAT @ (CONST FLOAT 0.0) $0)
fused OP37#1 ($0 : INT, $1 : INT, $2 : INT) = (> INT @ (+ INT @ $0 $1) $2)
fused OP38#1 ($0 : SEGDES) = (DIST INT @ (CONST INT 7) $0)
fused OP39#1 ($0 : FLOAT, $1 : INT, $2 : INT) = (* FLOAT @ $0 (/ FLOAT @ (I_TO_F @ $1) (I_TO_F @ $2)))
fused OP40#1 ($0 : FLOAT, $1 : FLOAT, $2 : FLOAT, $3 : FLOAT, $4 : FLOAT, $5 : FLOAT) = (+ FLOAT @
  (+ FLOAT @ (* FLOAT @ $0 $1) (* FLOAT @ $2 $3)) (* FLOAT @ $4 $5))
*/
