This source tree contains a port of NESL to run on NVIDIA GPUs using CUDA.

= Building NESL

Instructions for building NESL are in the nesl subdirectory.

= Benchmarks
There are three sets of benchmarks available in this repository.

- Copperhead

After installing Copperhead, these can be run directly by simply running "python
filename.py."

- NESL

Each of these benchmarks is based on a .nesl program. Rather than run them from
the interpreter, though, we have provided the .vcode, .ucode, and .fcode files,
representing the raw VCODE, basic optimized VCODE, and fused VCODE versions of
the files. They can each be provided directly as arguments to the various vcode
interpreters, which are built into the nesl/bin directory.

The raw .vcode and .ucode files will run directly. To build the .fcode files,
the corresponding .cu files for the custom kernel functions must also be
built. The provided Makefile in the bench/NESL directory will build them into
shared library (.so) files, so long as the NVIDIA CUDA SDK is installed. Once
built, you can then run the interpreter on the .fcode file, providing the
corresponding .so file as an argument to the -l flag.

- CUDA

These benchmarks require the NVIDIA CUDA SDK. They can each be built using the
Makefiles provided and run using the generated binaries.
