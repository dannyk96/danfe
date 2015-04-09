
# some notes on GNU make
#  DJK Feb 2002:
# $^ is all prerequisites, $< is just the first.  $@ is the target


VERSION=7.0
#--  implicit rules --
# (so far I have tended to avoid implicit rules
#.SUFFIXES: .exe .obj .ob4 .for .f90 .f

.f90.o:
	$(FC) -c $(F90FLAGS) $<
.f.o:
	$(FC) -c $(FFLAGS) $<


# directory to install binaries to 
#BINDIR=/usr/local/software/bin
BINDIR=~/bin/danfe

CWD     := $(shell pwd)
# next is 64 on 64bit architectures
IS3264BIT := $(shell getconf LONG_BIT)


# which system are we on.
# since linux runs on several hardware types - we add 'i686' etc.
# I think we also should add the compiler too because of compiler options, libraries, etc.
#OS      := $(shell uname)
#ARCH    := $(shell uname -m)
# be careful - not all unixes have both -s and -m options
ARCH    := $(shell uname -sm | sed 's/ /_/g')
#PLATFORM = $(ARCH)

#ifeq ($(OS), Linux)
#  PLATFORM = $(OS)_$(ARCH)
#else
#  PLATFORM = $(OS)
#endif

INT_GEN=int_gen.o

TOPDIR=$(CWD)
INCDIR=$(TOPDIR)/include
BASE=$(TOPDIR)


#-- choose whether we want the OpenMP parallelsed matrix routines --
MATRIX=matrix
#MATRIX=matrix_openmp

#-- machine specific values --
#include make.$(PLATFORM)
# note option to add a further qualifier - eg ifc v. g95
include make.$(ARCH)

# the files that make up the Finite element library
DANLIB= keyword.o k_mesh.o k_meshio.o k_bc.o \
	elements.o general.o small.o shape.o gauss.o
# add machine/compliler specific interface
DANLIB+=$(INT_GEN)
# add matrix library - sometime I want the OpenMP version.
DANLIB+=$(MATRIX).o

# my core graphics library
DANLIBG= draw.o draw_ps.o draw_dxf.o
# add drivers for third-party Pgplot
DANLIBG+=draw_pgplot.o
# draw_wmf.o needs char-based file I/O (fseek, etc.)
# draw_sal.o is for the odl Salford Software graphics library (DOS/Windows)
#DANLIBG += draw_wmf.o draw_sal.o
# possibly add drivers for MS Windows too.

# default way of compiling my Fortran
#%.o: %.f
#	$(FC) $(FFLAGS) -c $< -o $@
#%.o: %.F
#	$(FC) $(FFLAGS) $(CPPFLAGS)  -c $< -o $@
#%.o: %.f90
#	$(FC) $(FFLAGS) -c $< -o $@
#%.o: %.F90
#	$(FC) $(FFLAGS) $(CPPFLAGS)  -c $< -o $@



# The DANFE appplcations
APPS=danfe danmesh danplot danmung danfront qp qp_ps pl2ps_2win
APPS+=danfrontg
default:  $(APPS)
all: $(APPS) docs examples

danfe: danfe.o $(DANLIB) $(MPILIB)
	$(LN) -o $@ $^ $(BLASLIB)
danmesh: danmesh.o $(DANLIB) $(MPILIB)
	$(LN) -o $@ $^ $(BLASLIB)
danslip: danslip.o $(DANLIB)
	$(LN) -o $@ $^ 

danfront: danfront.o afront.o  $(INT_GEN)
	$(LN) -o $@ $^  
# varient that supports screen based mesh plots
danfrontg: danfront.o afrontg.o  $(INT_GEN)
	$(LN) -o $@ $^  $(PLOTLIB) 

danplot: danplot.o menu.o bits.o $(DANLIBG) $(DANLIB) $(MPILIB)  real4.o
	$(LN) -o $@ $^  $(BLASLIB) $(PLOTLIB)
danmung: danmung.o $(DANLIB) $(MPILIB)
	$(LN) $^ -o $@ $(BLASLIB)
qp: qp.o $(INT_GEN)
	$(LN) -o $@ $^ $(PLOTLIB)
qp_ps: qp_ps.o
	$(LN) -o $@ $^
# note OpenGL largely uses real*4 bindings - so dont compile with -r8
qp_ogl: qp_ogl.o
	$(LN) -o $@ $^  -L /usr/lib/glut -lf90GL -lfglut -lglut -lMesatk
pl2ps_2win: pl2ps_2win.o
	$(LN) -o $@ $^

#--- sources for the applications ----
danfe.o: src/danfe.f
	$(FC) -c -I include $<
danmesh.o: src/danmesh.f
	$(FC) -c $<
danslip.o: src/danslip.f
	$(FC) -c $<
danplot.o: src/danplot.f
	$(FC) -c $<
danfront.o: src/danfront.f90 afront.o
	$(FC) -c $(F90FLAGS) $<
qp.o: src/qp.f
	$(FC) -c $<
qp_ps.o: src/qp_ps.f
	$(FC_CPP) -c $<
pl2ps_2win.o: src/pl2ps_2win.f
	$(FC) -c $<
danmung.o: src/danmung.f
	$(FC) -c  $<

#----- Sources: Danfront's extras ----
# (note f90 module dependancies here)
afront.o: libsrc/afront.F90
	$(FC_CPP) -c $(F90FLAGS) $<
afrontg.o: libsrc/afront.F90
	$(FC_CPP) -c $(F90FLAGS) -DPGPLOT -o $@ $<

#---- all teh library code in libsrc
int_gen.o: libsrc/int_gen.F90
	$(FC) -c $(CPPFLAGS) $<
keyword.o: libsrc/keyword.f
	$(FC) -c $<
k_mesh.o: libsrc/k_mesh.f
	$(FC) -c $<
k_meshio.o: libsrc/k_meshio.f
	$(FC) -c $<
k_bc.o: libsrc/k_bc.f
	$(FC) -c $<
elements.o: libsrc/elements.f
	$(FC) -c $<
general.o: libsrc/general.f
	$(FC) -c  $<
small.o: libsrc/small.f
	$(FC) -c  $<
shape.o: libsrc/shape.f
	$(FC) -c  $<
gauss.o: libsrc/gauss.f
	$(FC) -c  $<
matrix.o: libsrc/matrix.F
	$(FC) $(CORE_FLAGS) -c $< 
mpi_nul.o: libsrc/mpi_nul.f
	$(FC) -c  $<

bits.o: src/bits.f
	$(FC) -c  $<
menu.o: src/menu.f
	$(FC) -c  $<
draw.o: libsrc/draw.f
	$(FC) -c  $<
draw_pgplot.o: libsrc/draw_pgplot.f
	$(FC) -c  $<
draw_ps.o: libsrc/draw_ps.f
	$(FC) -c  $<
draw_dxf.o: libsrc/draw_dxf.f
	$(FC) -c  $<
# next wont compile with gforttran as subrouitnes end in the @ symbol
draw_salford.o: libsrc/draw_salford.f
	$(FC) -c  $<
real4.o: libsrc/real4.f
	$(FC) -c  $<







# dynamic library versions
# (currentyl we dont bother - just link the object files as needed)
danplot_dll: danplot.o menu.o bits.o $(DANLIBG) $(DANLIB) $(MPILIB) real4.o
	$(LN) $< menu.o bits.o real4.o -o $@ -L. -ldanlib $(BLASLIB) $(MPILIB) $(PLOTLIB)

# specific options for some files
#int_gen.o: int_gen.F
#	$(FC) $(CPPFLAGS) -c $< -o $@

#
# Danlib as a staic and/or dynamic library
#
# We no longer build danlib as a .a library - instead we simply link the required .o files

libdanfe.a: $(DANLIB)  $(DANLIBG)
	ar q  $@ $^
libdanfe.so: $(DANLIB) $(DANLIBG)
	$(LN) -shared $^ -o $@

install:
	-mkdir -p $(BINDIR)
	-mv $(APPS) $(BINDIR) 

#-----------------------------------------------------------
#---------------------- doumentation ----------------------
#-----------------------------------------------------------
docs:
	@cd doc/keywords;  $(MAKE)

#-----------------------------------------------------------
#---------------------- examples----------------------------
#-----------------------------------------------------------

examples:
	cd examples; $(MAKE)

.PHONY: clean veryclean examples

clean:
	-rm *.o *.mod
veryclean: clean
	-rm -f $(APPS) *.a *.so



