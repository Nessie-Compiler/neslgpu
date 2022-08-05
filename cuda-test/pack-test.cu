// test of single-block PACK implementation
//

#include <utility>
#include <iostream>
#include <cstring>
#include <cuda_runtime.h>
#include <sys/resource.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#define MAX_BLOCK_WIDTH		65535
#define THREADS_PER_BLOCK	256

// return the block dimension for iterating over a sequence of the given width; we
// use a 2D grid of 1D blocks.
inline dim3 _gridDim (int wid)
{
    int numBlocks = ((wid + (THREADS_PER_BLOCK-1)) / THREADS_PER_BLOCK);
    if (numBlocks <= MAX_BLOCK_WIDTH) {
	return dim3(numBlocks, 1);
    }
    else {
      /* FIXME: want n,m such that wid < n*m, n,m <= MAX_BLOCK_WIDTH, and n*m is smallest such product */
	std::cerr << "too many blocks" << std::endl;
	exit (1);
    }

}

inline dim3 _blockDim (int wid)
{
    return dim3(THREADS_PER_BLOCK, 1);
}

#ifndef NDEBUG

static void HandleError (cudaError_t err, const char *file, int line)
{
    std::cerr << "** Error: " << cudaGetErrorString(err) << " in "
	<< file << " at line " << line << std::endl;
    exit (EXIT_FAILURE);
}

#define CHECK_ERROR(STMT)				\
    do {						\
	cudaError_t _err_ = STMT;			\
	if (_err_ != cudaSuccess)			\
	    HandleError(_err_, __FILE__, __LINE__);	\
    } while (0)

#else

#define CHECK_ERROR(STMT)	STMT

#endif
// CPU-side wrapper around scalar results
template <typename T>
struct Scalar {
    T		*_datap;	// GPU-side data
    T		_cache;		// CPU-side cache of GPU-side values
    bool	_isValid;	// true if the cache is valid

    Scalar ()
    {
	CHECK_ERROR( cudaMalloc (&this->_datap, sizeof(T)) );
	this->_isValid = false;
    }

    ~Scalar ()
    {
	cudaFree (&this->_datap);
	this->_datap = 0;
    }

    T value ()
    {
	if (! this->_isValid) {
	    CHECK_ERROR( cudaMemcpy (&(this->_cache), this->_datap, sizeof(T), cudaMemcpyDeviceToHost) );
	}
	return this->_cache;
    }

};

typedef Scalar<int32_t>		IntResult_t;
typedef Scalar<float>		FloatResult_t;
typedef Scalar<bool>		BoolResult_t;
typedef Scalar<char>		CharResult_t;

// CPU-side representation of sequences
template <typename T>
struct Sequence {
    int32_t	_len;		// sequence length
    T		*_datap;	// GPU-side data
    T		*_cache;	// CPU-side cache of GPU-side values

    Sequence (int len)
	: _len(len), _cache(0)
    {
	CHECK_ERROR( cudaMalloc (&this->_datap, len * sizeof(T)) );
    }
    Sequence (int len, const T *initData)
	: _len(len), _cache(0)
    {
	CHECK_ERROR( cudaMalloc (&this->_datap, len * sizeof(T)) );
	CHECK_ERROR( cudaMemcpy (this->_datap, initData, len * sizeof(T), cudaMemcpyHostToDevice) );
    }
    ~Sequence () { this->free(); }

    void free ()
    {
	if (this->_datap != 0) {
	    cudaFree (&this->_datap);
	    this->_datap = 0;
	    if (this->_cache != 0) {
		this->_cache = 0;
		delete this->_cache;
	    }
	}
    }

    T extract (int i)
    {
	T result;
	CHECK_ERROR( cudaMemcpy (&result, (void *)((T *)this->_datap+i), sizeof(T), cudaMemcpyDeviceToHost) );
	return result;
    }

    void fetch ()
    {
	if (this->_cache == 0) {
	    size_t nbytes = this->_len * sizeof(T);
	    this->_cache = (T *) std::malloc(nbytes);
	    CHECK_ERROR( cudaMemcpy (this->_cache, this->_datap, nbytes, cudaMemcpyDeviceToHost) );
	}
    }

};

typedef Sequence<int32_t>	IntSeq_t;
typedef Sequence<float>		FloatSeq_t;
typedef Sequence<bool>		BoolSeq_t;
typedef Sequence<char>		CharSeq_t;

// Warp-width scan
inline __device__ int FlatPack_ScanWarp (volatile int *partials)
{
  // warp-level scan, assuming that each 32-element block of partials is separated
  // by a 16-element array of zeros.
    int t = partials[0];
    t += partials[- 1]; partials[0] = t;
    t += partials[- 2]; partials[0] = t;
    t += partials[- 4]; partials[0] = t;
    t += partials[- 8]; partials[0] = t;
    t += partials[-16]; partials[0] = t;

    return t;
}

// Step 1 single block kernel; handle upto 1024 wide inputs
//
__global__ void FlatPack_Step1_Small (const int _width, bool * __restrict__ flags, int * __restrict__ dsts, int * __restrict__ num)
{
    const int _id = threadIdx.x;
    const int _warp = (_id >> 5);
    const int _lane = (_id & 31);

  // shared array of partials with space for 50% zero padding
    __shared__ int partials[1024 + 512];

    const int _idx = 48 * _warp + 16 + _lane;
    partials[_idx - 16] = 0;
    if (_id < _width) {
	partials[_idx] = (int)flags[_id];  // NOTE: assuming normalized bool representation!
    }
    else {
	partials[_idx] = 0;
    }

  // scan each warp in the block
    int s = FlatPack_ScanWarp (partials + _idx);
    __syncthreads();

  // save result of warp-level scan in wsums
    __shared__ int wsums[32+16];
    if (_lane == 31) {
	wsums[_warp] = s;
    }
    __syncthreads();

  // scan the per-warp sums
    if (_warp == 0) {
	s = FlatPack_ScanWarp (&(wsums[_lane+16]));
    }
    __syncthreads();

  // fan the scanned per-warp sums back into the per-warp partials and save the results
    int offset = wsums[_warp+15];  // == (_warp+16) - 1
    dsts[_id] = partials[_idx] + offset;
    __syncthreads();

    if (_id == (_width-1)) {
	*num = partials[_idx] + offset;
    }
}

// Step 3 kernel copies source data to its destination
template <typename T>
__global__ void FlatPack_Step3 (const int _width, T * __restrict__ values, bool * __restrict__ flags, int * __restrict__ dsts, T * __restrict__ result)
{
    const int _blockId = blockIdx.x+blockIdx.y*gridDim.x;
    const int _id = threadIdx.x;
    const int _gId = _blockId*THREADS_PER_BLOCK+_id;

    if ((_gId < _width) && flags[_gId]) {
	result[dsts[_gId] - 1] = values[_gId];
    }

}


// CPU-side code
template <typename T>
Sequence<T> FlatPack (Sequence<T> const &values, BoolSeq_t const &flags)
{
    assert (values._len == flags._len);
    int len = values._len;

    int *dsts;
    CHECK_ERROR( cudaMalloc(&dsts, len*sizeof(int)) );
    IntResult_t numTrue;

    if (len <= 1024) {
      // single block scan
	FlatPack_Step1_Small<<<dim3(1), dim3((len+31)&~31)>>> (len, flags._datap, dsts, numTrue._datap);
    }
    else {
	std::cerr << "array too big\n";
	exit (1);
    }

std::cout << numTrue.value() << " true flags\n";

  // allocate space for the result
    Sequence<T> result(numTrue.value());

    FlatPack_Step3<<<_gridDim(len), _blockDim(len)>>>(len, values._datap, flags._datap, dsts, result._datap);
    cudaFree (dsts);

    return result;
}

int main ()
{
    int		data[64];
    bool	flgData[64];

    for (int i = 0;  i < 64;  i++) {
	data[i] = i;
	flgData[i] = (i & 1) ? true : false;
    }
    Sequence<int>	values(64, data);
    Sequence<bool>	flags(64, flgData);

    Sequence<int> result = FlatPack (values, flags);

    result.fetch();
    std::cout << "result[" << result._len << "] = {";
    if (result._len > 0) {
	std::cout << result._cache[0];
	for (int i = 1;  i < result._len;  i++) {
	    std::cout << ", " << result._cache[i];
	}
    }
    std::cout << "}" << std::endl;

    return 0;
}
