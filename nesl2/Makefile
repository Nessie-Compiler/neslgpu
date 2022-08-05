# Makefile for nessie compiler
#

#
# directories for the local-install target
#
ROOTDIR =	$(shell pwd)
SRCDIR =	$(ROOTDIR)/src
LIBDIR =	$(ROOTDIR)/lib
BINDIR =	$(ROOTDIR)/bin
HEAPDIR =	$(BINDIR)/.heap

#INSTALL =	/opt/local/bin/ginstall -c
INSTALL =	/usr/bin/install -c
SHELL =		/bin/sh


INSTALL_SMLNJ_WRAPPER = $(BINDIR)/install-sml-wrapper.sh
ML_BUILD =		ml-build
ML_BUILD_FLAGS =	
ML_MAKEDEPEND =		ml-makedepend
ML_MAKEDEPEND_FLAGS =

PROGRAM =		nessie
HEAP_IMAGE =		$(PROGRAM).x86-darwin
ROOT_CM =		sources.cm

CODE_FRAG_FILES =       codegen/check-error.sml \
			codegen/commands.sml \
			codegen/cpu-types.sml \
			codegen/header.sml \
			codegen/main-init.sml \
			codegen/pick-dev.sml

.PHONY:		local-install
local-install:	$(HEAP_IMAGE)
	$(INSTALL_SMLNJ_WRAPPER) $(PROGRAM) $(BINDIR)

.PHONY:		build
build:		$(HEAP_IMAGE)

$(HEAP_IMAGE):	.depend
	$(ML_BUILD) $(ML_BUILD_FLAGS) $(ROOT_CM) Main.main $(PROGRAM)
	touch $(HEAP_IMAGE)

.depend:	$(ROOT_CM) $(CODE_FRAG_FILES)
	touch .depend
	$(ML_MAKEDEPEND) $(ML_MAKEDEPEND_FLAGS) -n -f .depend $(ROOT_CM) $(HEAP_IMAGE)

# synthesize files for the code fragments
#
codegen/commands.sml: codegen/fragments/commands_sml.in
	$(ROOTDIR)/gen/file2str.sh $< CommandsFrag $@

codegen/check-error.sml: codegen/fragments/check-error_sml.in
	$(ROOTDIR)/gen/file2str.sh $< CheckErrorFrag $@

codegen/cpu-types.sml: codegen/fragments/cpu-types_sml.in
	$(ROOTDIR)/gen/file2str.sh $< CPUTypesFrag $@

codegen/header.sml: codegen/fragments/header_sml.in
	$(ROOTDIR)/gen/file2str.sh $< HeaderFrag $@

codegen/main-init.sml: codegen/fragments/main-init_sml.in
	$(ROOTDIR)/gen/file2str.sh $< MainInitFrag $@

codegen/pick-dev.sml: codegen/fragments/pick-dev_sml.in
	$(ROOTDIR)/gen/file2str.sh $< PickDevFrag $@

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),devclean)
ifneq ($(MAKECMDGOALS),distclean)
sinclude .depend
endif
endif
endif

.PHONY:		clean, distclean

clean:
	rm -rf $(HEAP_IMAGE) $(CODE_FRAG_FILES)
	find . \( -name .cm -exec rm -rf {} \; -prune -print \)

distclean:
	rm -rf bin/$(PROGRAM) $(HEAPDIR)
