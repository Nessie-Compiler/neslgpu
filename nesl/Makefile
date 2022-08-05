# Top level Makefile for the NESL distribution.  Recursively calls Makefiles
# in the cvl, vcode and utils subdirectories

# Choose which compiler and flags you want for each architecture.
serialCC = gcc -g -O2

# linux gcc breaks on cvl/serial/elwise.c
linuxCvlCC = gcc

OS := $(shell uname -s)
ifeq ($(OS),Darwin)
OCLLIBFLAGS=-framework OpenCL
serialCC = clang -g
else
OCLLIBFLAGS=-lOpenCL
endif

crayCC = cc -O -hinline3,scalar3,vector3,task0

cm2CC = gcc -O2 -DCM2

cm5CC = gcc -O2 -DCM5

nvCC = nvcc -g -O3 -DCUDA

nvCCProf = nvcc -g -O3 -DCUDA -DVCODE_PROFILE

masparCC = mpl_cc

# For MPI, you also need to supply the location of your MPICH installation
cm5_mpiCC = gcc -O2 -DMPI -DMPI_cm5 -DNOSPAWN
cm5_mpidir = /u/ac/jch/mpi

paragon_mpiCC = icc -O3 -nx -DMPI -DMPI_paragon -DNOSPAWN
paragon_mpidir = /usr/paragon/local/mpi

sp1_mpiCC = mpcc -O2 -DMPI -DMPI_rs6000
sp1_mpidir = /usr/local/mpi

serial:
	$(MAKE) ARCH='serial' CC="$(serialCC)" cvlmake
	$(MAKE) ARCH='serial' CC="$(serialCC)" vcodemake
	$(MAKE) ARCH='serial' CC="$(serialCC)" utilsmake
	$(MAKE) lispsetup

cuda:
	$(MAKE) ARCH='cuda' CC="$(nvCC)" cvlmake
	$(MAKE) ARCH='cuda' CC="$(nvCC)" vcodemake
	$(MAKE) ARCH='cuda' CC="$(nvCC)" utilsmake
	$(MAKE) lispsetup

cudaprofile:
	$(MAKE) ARCH='cuda' PROFILE=1 CC="$(nvCC)" cvlmake
	$(MAKE) ARCH='cuda' PROFILE=1 CC="$(nvCCProf)" vcodemake
	$(MAKE) ARCH='cuda' PROFILE=1 CC="$(nvCC)" utilsmake
	$(MAKE) lispsetup

opencl:
	$(MAKE) ARCH='opencl' CC="$(serialCC)" cvlmake
	cp cvl/opencl/*.cl bin
	$(MAKE) ARCH='opencl' CC="$(serialCC)" vcodeopenclmake
	$(MAKE) ARCH='opencl' CC="$(serialCC)" utilsmake
	$(MAKE) lispsetup

darwin:
	$(MAKE) ARCH='serial' CC="$(serialCC)" cvlmake
	$(MAKE) MAKE="$(MAKE) -f Makefile.darwin" ARCH='serial' CC="$(serialCC)" vcodemake
	$(MAKE) ARCH='serial' CC="$(serialCC) -I/opt/X11/include" XLIBPATH='/opt/X11/lib/' utilsmake
	$(MAKE) lispsetup

linux:
	$(MAKE) ARCH='serial' CC="$(linuxCvlCC)" cvlmake
	$(MAKE) MAKE="$(MAKE) -f Makefile.linux" ARCH='serial' CC="$(serialCC)" vcodemake
	$(MAKE) ARCH='serial' CC="$(serialCC)" XLIBPATH='/usr/X11/lib/' utilsmake
	$(MAKE) lispsetup

cvlmake:
	(cd cvl/$(ARCH); \
	 $(MAKE) TOPCC='$(CC)'; \
	 mv libcvl.a ../../lib/libcvl-$(ARCH).a \
	)

vcodeopenclmake:
	(cd vcode; \
	 $(MAKE) TOPCC='$(CC)' EXTRALIB='$(OCLLIBFLAGS)' CVLLIB='-lcvl-$(ARCH)'; \
	 mv vinterp ../bin/vinterp.$(ARCH); \
	)

vcodemake:
	(cd vcode; \
	 $(MAKE) TOPCC='$(CC)' EXTRALIB='' CVLLIB='-lcvl-$(ARCH)'; \
	 mv vinterp ../bin/vinterp.$(ARCH) \
	)

utilsmake:
	(cd utils; \
	 $(MAKE) TOPCC='$(CC)' EXTRALIB=''; \
	 mv xneslplot ../bin/xneslplot.$(ARCH) \
	)

lispsetup:
	@echo ''
	@echo '**************************************************************'
	@echo '  Start up Common Lisp and type: (load "neslsrc/build.lisp")'
	@echo '**************************************************************'
	@echo ''

# This is an example of how you could make a second serial version for a
# different serial architecture.  The result will be called "vinterp.sun4"
sun4CC = acc -O2
sun4:
	$(MAKE) ARCH='sun4' CC="$(sun4CC)" cvlmake
	$(MAKE) ARCH='sun4' CC="$(sun4CC)" vcodemake
	$(MAKE) ARCH='sun4' CC="$(sun4CC)" utilsmake
	$(MAKE) lispsetup

# Now for the specialized supercomputer versions of CVL
cray:
	$(MAKE) ARCH='cray' CC='$(crayCC)' cvlmake
	$(MAKE) ARCH='cray' CC='$(crayCC)' vcodemake
	$(MAKE) ARCH='cray' CC='$(crayCC)' utilsmake

cm2:
	$(MAKE) ARCH='cm2' CC='$(cm2CC)' cvlmake
	(cd vcode; \
	 $(MAKE) TOPCC='$(cm2CC)' EXTRALIB='-lparis' CVLLIB='-lcvl-cm2'; \
	 mv vinterp ../bin/vinterp.cm2 \
	)
	$(MAKE) ARCH='cm2' CC='$(cm2CC)' utilsmake

cm5:
	$(MAKE) ARCH='cm5' CC='$(cm5CC)' cvlmake
	(cd vcode; \
	 $(MAKE) TOPCC='$(cm5CC)' cm5; \
	 mv vinterp ../bin/vinterp.cm5 \
	)
	$(MAKE) ARCH='cm5' CC='$(cm5CC)' utilsmake

maspar:
	$(MAKE) ARCH='maspar' CC='$(masparCC)' cvlmake
	cp cvl/maspar/unc_wrap.h include
	cp cvl/maspar/unc_globals.h include
	(cd vcode; \
	 $(MAKE) TOPCC='$(masparCC) -DMASPAR' CVLLIB='-lcvl-maspar'; \
	 mv vinterp ../bin/vinterp.maspar \
	)
	$(MAKE) ARCH='maspar' CC='$(masparCC)' utilsmake

cm5_mpi:
	(cd cvl/mpi; \
	 $(MAKE) TOPCC='$(cm5_mpiCC)' MPI_DIR='$(cm5_mpidir)' cm5mpi; \
	 mv libcvl.a ../../lib/libcvl-cm5-mpi.a \
	)
	(cd vcode; \
	 $(MAKE) TOPCC='$(cm5_mpiCC)' \
		 EXTRALIB='-lmpi' \
		 EXTRADIR='-L$(cm5_mpidir)/lib/cm5/ch_cmmd' \
		 cm5mpi; \
	 mv vinterp ../bin/vinterp.cm5.mpi \
	)

paragon_mpi:
	(cd cvl/mpi; \
	 $(MAKE) TOPCC='$(paragon_mpiCC)' MPI_DIR='$(paragon_mpidir)' \
		AR=ar860 ; \
	 mv libcvl.a ../../lib/libcvl-paragon-mpi.a \
	)
	(cd vcode; \
	 $(MAKE) TOPCC='$(paragon_mpiCC)' \
		 CVLLIB='-lcvl-paragon-mpi' \
		 EXTRALIB='-lmpi -lnx' \
		 EXTRADIR='-L$(paragon_mpidir)/lib/paragon/ch_nx'; \
	 mv vinterp ../bin/vinterp.mpi.paragon \
	)

sp1_mpi:
	(cd cvl/mpi; \
	 $(MAKE) TOPCC='$(sp1_mpiCC)' MPI_DIR='$(sp1_mpidir)'; \
	 mv libcvl.a ../../lib/libcvl-sp1-mpi.a \
	)
	(cd vcode; \
	 $(MAKE) TOPCC='$(sp1_mpiCC)' \
		 CVLLIB='-lcvl-sp1-mpi' \
		 EXTRALIB='-lmpi' \
		 EXTRADIR='-L$(sp1_mpidir)/lib/rs6000/ch_eui'; \
	 mv vinterp ../bin/vinterp.mpi.sp1 \
	)

clean:
	@echo 'Use one of:'
	@echo 'make openclclean, make cudaclean; make serialclean, make cm2clean, make cm5clean, make crayclean, make mpiclean'

vcodeutilsclean:
	(cd vcode; make clean)
	(cd utils; make clean)

cudaclean:
	(cd cvl/cuda; make clean)
	$(MAKE) vcodeutilsclean
	-rm lib/libcvl-cuda.a
	-rm bin/vinterp.cuda

cudaprofileclean:
	(cd cvl/cuda; make clean)
	$(MAKE) vcodeutilsclean
	-rm lib/libcvl-cudaprofile.a
	-rm bin/vinterp.cudaprofile

openclclean:
	(cd cvl/opencl; make clean)
	$(MAKE) vcodeutilsclean
	-rm lib/libcvl-opencl.a
	-rm bin/vinterp.opencl
	-rm bin/*.cl

serialclean:
	(cd cvl/serial; make clean)
	$(MAKE) vcodeutilsclean
	-rm lib/libcvl-serial.a
	-rm bin/vinterp.serial

cm2clean:
	(cd cvl/cm2; make clean)
	$(MAKE) vcodeutilsclean
	-rm lib/libcvl-cm2.a
	-rm bin/vinterp.cm2

cm5clean:
	(cd cvl/cm5; make clean)
	$(MAKE) vcodeutilsclean
	-rm lib/libcvl-cm5.a
	-rm bin/vinterp.cm5

crayclean:
	(cd cvl/cray; make clean)
	$(MAKE) vcodeutilsclean
	-rm lib/libcvl-cray.a
	-rm bin/vinterp.cray

mpiclean:
	(cd cvl/mpi; make clean)
	$(MAKE) vcodeutilsclean
	@echo 'Remove lib/libcvl-mpi-arch.a and bin/vinterp.mpi.arch by hand'
	@echo '(or modify this Makefile to do it for you)'
# -rm lib/libcvl-cm5-mpi.a
# -rm bin/vinterp.cm5.mpi
