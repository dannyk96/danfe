
Copyright 2001, Dr. Dan Kidger
DANFE is written and developed by Dr. Dan Kidger (d.kidger@man.ac.uk)

---------------------- Introduction --------------------------

DANFE is the title given to a suite of software for Finite Element Analysis
The Major components are:
     DANMESH - a large 2d/3d mesh generator
     DANFRONT - a 2d unstructured triangle mesh generator
     DANFE   - the main analysis engine
     DANPLOT - a 3d interactive post-processor

DANFE is written in standard Fortran 77/90 and should compile on any Unix system with a suitable compiled. It has been extensively tested on the following systems:
      IRIX 6.5   	on SGI Origin 2000 systems
      Linux	 	on a variety of Intel Pentium systems (mostly RedHat)
      TurboLinux	on Intel IA-64 (64-bit)
      Unicos/mk		on Cray T3E-1200
      UXP/V		on Fujitsu VPP 300
      AIX		on IBM SP - Winterhawk PowerPC 
      OSF1		on Compaq Alphaserver


It has also been run under HP-UX, but not tested recently.

Microsoft Windows:
   I regularly run Danfe/Danplot under Windows 98,NT and 2000. Some changes are needed to the makefile - for example to swap / for \ and to swap .o for .obj.
I use Salford FTN95. This doesn't support standard c pre-procesing directives like #ifdef, hence you need to somehow prerposs under Unix and ship the converted file across - probably only matrix.F is affected in this way.

----------------------- Installation Instructions -------------------
To build DANFE,you need:
     - A Fortran compiler
     - a copy of make - gnu-make is recommended.

the Software also makes use of some external libraries:
     - BLAS     optimised matrix manipulation routines (optional)
     - MPI      message passing library for parallel machines (optional)
     - PGPLOT    required for DANPLOT only
     - salflibc.dll required for the Windows version on DANPLOT only.

The source codes lives in the directories 'src' and 'lib_src'. The compiled object files and executables are placed in 'bin'. To enable support for mulitple platforms, the binary are actually put into 'bin/$ARCH' where $ARCH is a platfrom sepecific indetifier (eg $ARCH=IRIX)

Normally you would 'cd' to this directory and run 'make' there. You need to copy (or better use 'ln -s') the main makefile and also teh platform specific make.$ARCH.
Then simply type 'make ARCH=IRIX (or whatever).

By default only 'danfe' is built'.
To build danplot, type 'make danplot', etc.
'make all' will attempt to build ALL of the software packages, including DANFE, DANPLOT and the others.

A note on porting.
------------------
The file 'makefile' should be generic and not need modifying.
The files make.$ARCH may well need changing for your system if you do not keep your libraries and compilers in the usual places. In a particular $PLOTLIB will probably need local modification.


tetsing the installation
------------------------


Typing any of './danfe.exe','./danmesh.exe', './danplot.exe' should run the executables. You should get a title banner and then some warning about a missign data file. There are many test datafiles in the directory 'examples' . Simply give one of these on the command line to teh appropriate package'


------------------------------ Contact Information -------------------------


If you have any Comments, suggestions, or bug reports, please send them to
    d.kidger@man.ac.uk

The homepage for DANFE is:
    http://www.danfe.co.uk
(note: this site is not yet live - 01/02/01)

Website with eariler versions of teh software and documentation may be found at:
    http://www.man.ac.uk/~mbgasdk/dansoft
and
    http://www.eng.man.ac.uk/geotech/software

DANFE is open source software. Whereas I will try and help and fix problems for you, I have of course other things to do with my time. 

I run a company called Finite Element Solutions, which does commercial finite element analysis for industry. DANFE is the principal tool used for this and as such has been used in over 20 major engineering projects over the last 10 years. Hence the software is definitely 'industry strength' but still open source - free for non-commercial use.


Dan Kidger
1st February 2001




