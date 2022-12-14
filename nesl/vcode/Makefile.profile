# Makefile for VCODE interpreter
#
# It assumes that the following variables are already set (passed in from
# the top-level Makefile):
# TOPCC		C compiler and flags
# CVLLIB 	Name of the CVL library to link with (null or with "-l" prefix)
# EXTRALIB	Any other libraries to link with (null or with "-l" prefix)
# EXTRADIR	Path to look for EXTRALIB (null or with "-L" prefix)
#
# To compile for the CM-5: there's a special cm5 target.  It uses TOPCC 
#   but ignores CVLLIB.
#
# To compile for most machines using MPI: call with EXTRALIB=-lmpi and 
#   EXTRADIR=-Lmpipath, where mpipath is the path to your MPICH library.  
#   TOPCC should include -DMPI -DMPI_arch, where arch is your architecture,
#   and -DNOSPAWN if the nodes of the machine cannot spawn subprocesses.
#
# To compile for the CM-5 using MPI: there's a special cm5mpi target.  It 
#   uses TOPCC but ignores CVLLIB.
#
# For both the CM-5 and MPI implementations, add lfibrng6a.o to NODE_OBJS
#   or CVLOBJS if you are using the portable pseudorandom number generator.
#
# To compile for the CM-2: call with EXTRALIB=-lparis

# Default in case TOPCC isn't defined
TOPCC = gcc -O2

CC = $(TOPCC)
DEBUG_FLAGS = -g
CFLAGS = -DVCODE_PROFILE $(DEBUG_FLAGS) -I$(INCDIR) 

VERSION = 2.1

ROOT = ..
INCDIR = $(ROOT)/include
LIBDIR = $(ROOT)/lib

SRC1 = actions.c link_list.c main.c symbol_table.c program.c vcode_table.c \
       rtstack.c stack.c vstack.c check_args.c cvl_table.c constant.c io.c \
       vcode_hash.c fusion.c vcode-profile.c

SRC2 = lex.yy.c y.tab.c
SRC3 = grammar.yy tokens.ll

SRCS = $(SRC1) $(SRC2) $(SRC3)

OBJS = $(SRC1:.c=.o) $(SRC2:.c=.o)

LIBS = -ll -lm -ldl $(CVLLIB) $(EXTRALIB) -lm

LFLAGS = -L$(LIBDIR) $(EXTRADIR)

TESTFILE = nesltest.vcode

vinterp:	$(OBJS)
	$(CC) $(LFLAGS) -o vinterp $(OBJS) $(LIBS)

test:	vinterp $(TESTFILE)
	./vinterp -c -m 500000 $(TESTFILE)

clean: 
	-rm -f $(OBJS) vinterp $(SRC2) 

.c.o:
	$(CC) $(CFLAGS) -c $*.c

lex.yy.c:	tokens.ll
	lex tokens.ll

lex.yy.o:	y.tab.h vcode.h parse.h vcode_hash.h lex.yy.c
	$(CC) -c $(CFLAGS) lex.yy.c

y.tab.c y.tab.h:	grammar.yy
	yacc -d grammar.yy

y.tab.o:	vcode.h parse.h y.tab.c
	$(CC) -c $(CFLAGS) y.tab.c

# Ugly CM-5 stuff starts here

CM5DIR = ../cvl/cm5

NODE_OBJS = $(CM5DIR)/n_elwise.o $(CM5DIR)/n_facilt.o $(CM5DIR)/n_internal.o \
		$(CM5DIR)/n_library.o $(CM5DIR)/n_permute.o \
		$(CM5DIR)/n_scan.o $(CM5DIR)/n_vecscalar.o

HOST_OBJS = $(CM5DIR)/h_elwise.o $(CM5DIR)/h_facilt.o $(CM5DIR)/h_internal.o \
		$(CM5DIR)/h_library.o $(CM5DIR)/h_permute.o \
		$(CM5DIR)/h_scan.o $(CM5DIR)/h_vecscalar.o

cm5:	$(OBJS)
	cmmd-ld -comp $(CC) -o vinterp \
	-node $(NODE_OBJS) -host $(HOST_OBJS) $(OBJS) $(LIBS)

# Ugly CM-5 MPI stuff starts here

MPIDIR = ../cvl/mpi

CVLOBJS = $(MPIDIR)/elwise.o $(MPIDIR)/facilt.o $(MPIDIR)/library.o \
	$(MPIDIR)/messages.o $(MPIDIR)/permute.o $(MPIDIR)/rank.o \
	$(MPIDIR)/reduce.o $(MPIDIR)/scan.o $(MPIDIR)/vecscalar.o

cm5mpi:	$(OBJS) 
	cmmd-ld -comp $(CC) -o vinterp $(OBJS) $(CVLOBJS) $(LFLAGS) $(LIBS)

# DO NOT DELETE THIS LINE -- make depend depends on it.

actions.o: actions.c
actions.o: config.h
actions.o: vcode.h
actions.o: y.tab.h
actions.o: symbol_table.h
actions.o: vcode.h
actions.o: link_list.h
actions.o: program.h
actions.o: vcode.h
actions.o: constant.h
actions.o: vcode.h
actions.o: vstack.h
actions.o: vcode.h
actions.o: parse.h
actions.o: parse.h
link_list.o: link_list.c
link_list.o: config.h
link_list.o: vcode.h
link_list.o: vstack.h
link_list.o: vcode.h
link_list.o: program.h
link_list.o: vcode.h
link_list.o: symbol_table.h
link_list.o: vcode.h
main.o: main.c
main.o: config.h
main.o: vcode.h
main.o: y.tab.h
main.o: symbol_table.h
main.o: vcode.h
main.o: link_list.h
main.o: parse.h
main.o: vstack.h
main.o: vcode.h
main.o: program.h
main.o: vcode.h
main.o: stack.h
main.o: vstack.h
main.o: program.h
main.o: check_args.h
main.o: rtstack.h
main.o: constant.h
main.o: vcode.h
main.o: vstack.h
main.o: parse.h
main.o: io.h
symbol_table.o: symbol_table.c
symbol_table.o: config.h
symbol_table.o: vcode.h
symbol_table.o: symbol_table.h
symbol_table.o: vcode.h
program.o: program.c
program.o: config.h
program.o: vcode.h
program.o: y.tab.h
program.o: symbol_table.h
program.o: vcode.h
program.o: vstack.h
program.o: vcode.h
program.o: program.h
program.o: vcode.h
program.o: constant.h
program.o: vcode.h
program.o: vstack.h
program.o: parse.h
vcode_table.o: vcode_table.c
vcode_table.o: config.h
vcode_table.o: vcode.h
vcode_table.o: y.tab.h
rtstack.o: rtstack.c
rtstack.o: config.h
rtstack.o: vcode.h
rtstack.o: rtstack.h
stack.o: stack.c
stack.o: config.h
stack.o: vcode.h
stack.o: vstack.h
stack.o: vcode.h
stack.o: program.h
stack.o: vcode.h
stack.o: constant.h
stack.o: vcode.h
stack.o: vstack.h
stack.o: parse.h
stack.o: stack.h
stack.o: vstack.h
stack.o: program.h
stack.o: io.h
vstack.o: vstack.c
vstack.o: config.h
vstack.o: vcode.h
vstack.o: vstack.h
vstack.o: vcode.h
check_args.o: check_args.c
check_args.o: config.h
check_args.o: vcode.h
check_args.o: y.tab.h
check_args.o: vstack.h
check_args.o: vcode.h
check_args.o: stack.h
check_args.o: vstack.h
check_args.o: program.h
check_args.o: vcode.h
check_args.o: program.h
check_args.o: check_args.h
cvl_table.o: cvl_table.c
constant.o: constant.c
constant.o: config.h
constant.o: vcode.h
constant.o: parse.h
constant.o: program.h
constant.o: vcode.h
constant.o: constant.h
constant.o: vcode.h
constant.o: vstack.h
constant.o: vcode.h
constant.o: parse.h
io.o: io.c
io.o: config.h
io.o: vcode.h
io.o: vstack.h
io.o: vcode.h
io.o: program.h
io.o: vcode.h
io.o: constant.h
io.o: vcode.h
io.o: vstack.h
io.o: parse.h
io.o: stack.h
io.o: vstack.h
io.o: program.h
fusion.o: fusion.c
