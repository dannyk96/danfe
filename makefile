#
#   Makefile for teh suite of DANFE programs
#
# Applications:
#    danfe       The main FE Engine
#    danmesh     A Rule maths FEA preprocessor
#    danfront    An unstrructured triangle mesh generator
#    danplot     Interactive GUI based post-processor
#
#  Other applications:
#    qp          'QuickPlot' Draws the mesh on screen (needs graphics drivers)
#    qp_ps       as qp but using built-in postscript drivers
#    qp_opengl   as qp but to screen using OpenGL drivers (not recently tested)
#    qp_2win     as qp_ps but dsraws 2 pictures on a page: element #at the top, node# at the bottom
#    sierpinski  A demo program using F90 data structures to create recurive swiss cheese
#
#  To build
#    'make'    after first possibly modifying the make.<include> file for your architecture

#.f90.o:
#	$(FC) -c $(F90FLAGS) $<
#.f.o:
#	$(FC) -c $(FFLAGS) $<


#
# directory to install binaries to 
#
#BINDIR=/usr/local/software/bin
BINDIR=~/bin/danfe
#
# default compile options
#

#
# CORE_FLAGS is for the Matrix.f libarary: where almost all the cputime is spent
#  HEnce good to optimise this for performance
#
CORE_FLAGS= -O2
BLASLIB=

#
# Standard flags for building
#
FFLAGS=
#     I want to allow 132 columns even in fixed format source (not 72)
FFLAGS=-ffixed-line-length-132
#
# for runtime debugging
#
FFLAGS+=-g
#
# for  warnings of source code bugs
#
# 
FFLAGS+=-fno-backslash -Wall 
# Next I need as many subroutines have a common interface even if not all args are used.
FFLAGS+=-Wno-unused-dummy-argument
# next can be removed as hopefully all are fixed
#FFLAGS+= -Wno-unused-variable

#
# Default is to use gfortran
#
FC=gfortran $(FFLAGS)
FC_CPP=gfortran $(FFLAGS) -cpp
LN=gfortran
#
#  MPI libraries: default os to use my dummy driver
#
MPILIB=mpi_nul.o
#
#  Graphics libarary
#   cf  sudo apt-get install pgplot5
#
PLOTLIB= -L${HOME}/pgplot  -lpgplot -lX11



CWD     := $(shell pwd)
#
# One way to detect if we are on a 64bit architecture (unused)
# next is 64 on 64bit architectures
#IS3264BIT := $(shell getconf LONG_BIT)


#
#-- choose whether we want the OpenMP parallelsed matrix routines --
# TODO use ifdef to better effect hence a single version of matrix.F
#
MATRIX=matrix
#MATRIX=matrix_openmp

#
#-- machine specific values --
#  Instructions:
#  a) simplex:  This makefile will assume PLATFORM=default and pick up make.default
#      This is a symlink to include/make.Linux_x86_64 which should 'just work' using gfortran
#  b) custom: copy one of the make.<arch> files from include/ to the basedir to make.default
#      Edit as desired then build with just 'make'
#  c) fully custom: as but call the file say make.debug  
#      edit to add say -g then build with say 'make ARCH=debug' 
#
ARCH=default
include make.$(ARCH)


#
# the files that make up the Finite element library
#
DANLIB= keyword.o k_mesh.o k_meshio.o k_bc.o elements.o general.o small.o shape.o gauss.o

# add machine/compliler specific interface
INT_GEN=int_gen.o
DANLIB+=$(INT_GEN)

# add matrix library - sometime I want the OpenMP version.
DANLIB+=$(MATRIX).o

#
# my core graphics library
#
DANLIBG= draw.o draw_ps.o draw_dxf.o
#
# add drivers for third-party PGPLOT
#
DANLIBG+=draw_pgplot.o

# Caveats: draw_wmf.o needs char-based file I/O (fseek, etc.)
# draw_sal.o is for the old Salford Software graphics library (DOS/Windows)
#DANLIBG += draw_wmf.o draw_sal.o
# TODO possibly add drivers for MS Windows too.


#
# The DANFE appplcations
#
APPS=danfe danmesh danplot danmung danfront qp_pgplot qp_ps pl2ps_2win

#
# Optional varients: in this case with runtime graphics
#
APPS+=danfrontg
#
APPS+=sierpinski

default:  $(APPS)
all: $(APPS) docs examples

#
#------  nothing below this line should need to be edited ------
#

#
# The DANFE Applications 
#
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

danplot: danplot.o menu.o bits.o $(DANLIBG) $(DANLIB) $(MPILIB) real4.o
	$(LN) -o $@ $^  $(BLASLIB) $(PLOTLIB)
danmung: danmung.o $(DANLIB) $(MPILIB)
	$(LN) $^ -o $@ $(BLASLIB)

#
# screen based plotters
#
qp_pgplot: qp_pgplot.o $(INT_GEN)
	$(LN) -o $@ $^ $(PLOTLIB)
# note OpenGL largely uses real*4 bindings - so dont compile with -r8
qp_opengl: qp_opengl.o $(INT_GEN)
	$(LN) -o $@ $^  -L /usr/lib/glut -lf90GL -lfglut -lglut -lMesatk
#
# Postscript plotters
#
qp_ps: qp_ps.o
	$(LN) -o $@ $^
pl2ps_2win: pl2ps_2win.o
	$(LN) -o $@ $^

#
# Source code  for the applications in src/
#
danfe.o: src/danfe.f
	$(FC) -c -I libsrc $<
danmesh.o: src/danmesh.f
	$(FC) -c $<
danslip.o: src/danslip.f
	$(FC) -c $<
danplot.o: src/danplot.f
	$(FC) -c $<
danfront.o: src/danfront.f90 afront.o
	$(FC) -c $<
qp_pgplot.o: src/qp_pgplot.f
	$(FC) -c $<
qp_ps.o: src/qp_ps.f
	$(FC_CPP) -c $<
pl2ps_2win.o: src/pl2ps_2win.f
	$(FC) -c $<
danmung.o: src/danmung.f
	$(FC) -c  $<
sierpinski: src/sierpinski.f90
	$(FC)  $< -o $@

#
# Sources: Danfront's extras 
# (note f90 module dependancies here)
#
afront.o: libsrc/afront.f90
	$(FC_CPP) -c  $<
afrontg.o: libsrc/afront.f90
	$(FC_CPP) -c -DPGPLOT -o $@ $<

#
# Source code for the library routines in libsrc/
#
int_gen.o: libsrc/int_gen.F90
	$(FC_CPP) -c $(CPPFLAGS) $<
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
#
# next wont compile with gforttran as subrouitnes end in the @ symbol
#
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



