::
::   This buuld script will compile all the programs and subroutines 
::   using the FTN95 Compiler from Silversoft. 
::   This compiler was formerly known as the Salford Fortran90 compiler.
::   For many years the Salfrod Compiler was the primary environment for the
::   Danfe software. Of particular note is teh ClearWin+ library for graphics that
::   Danplot was once completely reliant on
::
::   Dan Kidger  07 April 2015
::   daniel.kidger@alumni.manchester.ac.uk   danile.kidger@gmail.com
::

rem #### Set compiler flags

set FCOMP=
:: Compiler option for performance
               ! add P>=Pentium Pro instructions
rem set FOPT=%FCOMP% /P6
::
:: compiler options for validating the source code
::

::
:: compiler options for performing runtime checking
::
rem set FCOMP=/DEBUG
::
::               ! runtime variab;le cecking
::
rem set FCOMP=%FCOMP% /UNDEF

::
:: compiler options for real*8 building
::    ! so kind=8 not kind=2
::
rem set FCOMP=%FCOMP% /ALT_KINDS

::
:: compiler options for Fotran syntax and style
::        | use yellow and red to highlight compiler errors (default?)
::
rem set FCOMP=%FCOMP% /COLOUR

rem set FCOMP=%GFCOMP% /PERSIST

rem set FCOMP=%FCOMP% /DEFINE PGPLOT  | if we ever support pgplot graphics library here

::
:: Allow .+2003 Fortran syntax
::
set FCOMP=%FCOMP% /F2K
rem set FCOMP=%FCOMP% /TIMING          | produces per routines wallclock :-)


::
::  Define the compiler used
::
rem set FC=pause;ftn95 %FCOMP%
set FC=ftn95 %FCOMP%
set FC_CPP=ftn95 /CFPP %FCOMP%

set LN=link77


set DANLIB=keyword.obj k_mesh.obj k_meshio.obj k_bc.obj elements.obj general.obj small.obj shape.obj gauss.obj matrix.obj 
rem set DANLIB+=int_gen.obj
set MPILIB=mpi_nul.obj
set DANLIBG=draw.obj draw_ps.obj draw_dxf.obj draw_pgplot.obj

:: #### Build the library routines
del *.obj
%FC% libsrc\keyword.f    /BINARY keyword.obj
%FC% libsrc\k_mesh.f     /BINARY k_mesh.obj
%FC% libsrc\k_meshio.f   /BINARY k_meshio.obj
%FC% libsrc\k_bc.f       /BINARY k_bc.obj
%FC% libsrc\elements.f   /BINARY elements.obj
%FC% libsrc\general.f    /BINARY general.obj
%FC% libsrc\small.f      /BINARY small.obj
%FC% libsrc\shape.f      /BINARY shape.obj
%FC% libsrc\gauss.f      /BINARY gauss.obj
%FC_CPP% libsrc\matrix.F /BINARY matrix.obj

::
:: Alternative : we concatonate all source files into one big source file
::
rem %FC_CPP% libsrc\danlib.f /BINARY danlib.obj
::
:: Null interface to MPI
::
%FC% libsrc\mpi_nul.f    /BINARY mpi_nul.obj
::
:: just those routines that are platform dependant eg command line/graphics
::
%FC_CPP% /DEFINE __FTN95 1 libsrc\int_gen.F90 /BINARY int_gen.obj

::
:: graphics routines 
::
%FC% libsrc\draw.f        /BINARY draw.obj
%FC% libsrc\draw_ps.f     /BINARY draw_ps.obj
%FC% libsrc\draw_dxf.f    /BINARY draw_dxf.obj
%FC% libsrc\draw_pgplot.f /BINARY draw_pgplot.obj
%FC% src\bits.f           /BINARY bits.obj
%FC% src\menu.f           /BINARY menu.obj

%FC_CPP% libsrc\afront.F90 /BINARY afront.obj

::
:: #### build the applications
::
%FC% src\sierpinski.f90 /LINK siepinski.exe 
%FC_CPP% src\qp_ps.f  /LINK qp_ps.exe
%FC% src\pl2ps_2win.f  /LINK pl2ps_2win.exe
%FC% src\qp.f         /LINK qp.exe       /LIBRARY int_gen.obj  
::
:: The 4 core applications of DANFE
::
%FC% src\danmesh.f    /LINK danmesh.exe  /LIBRARY %DANLIB% int_gen.obj
%FC% src\danmung.f    /LINK danmung.exe  /LIBRARY %DANLIB% %MPILIB% int_gen.obj
::%FC% src\danfront.f90 /LINK danfront.exe /LIBRARY afront.obj int_gen.obj
%FC% src\danfront.f90 /LINK danfront.exe /LIBRARY int_gen.obj
%FC% src\danplot.f    /LINK danplot.exe  /LIBRARY bits.obj menu.obj %DANLIB% %DANLIBG% int_gen.obj %MPILIB%

:: rem  %FC% src\danfront.f90 /MOD_PATH libsrc
%FC% src\danfe.f  /IN libsrc  /LINK danfe.exe    /LIBRARY %DANLIB% %MPILIB%  int_gen.obj 
:$changed 08/04/15
