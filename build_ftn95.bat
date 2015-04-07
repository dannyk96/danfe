rem
rem   This buuld script will compile all the programs and subroutines 
rem   using the FTN95 Compiler from Silversoft. 
rem   This compiler was formerly known as the Salford Fortran90 compiler.
rem   For many years the Salfrod Compiler was the primary environment for the
rem   Danfe software. Of particular note is teh ClearWin+ library for graphics that
rem   Danplot was once completely reliant on
rem
rem   Dan Kidger  07 April 2015
rem   daniel.kidger@alumni.manchester.ac.uk   danile.kidger@gmail.com
rem

rem #### Set compiler flags
set FC=ftn95 %FFLAGS%
set FC_CPP=ftn95 /CFPP
set LN=link77

rem #### Build the library routines
%FC% libsrc\keyword.f
%FC% libsrc\k_mesh.f
%FC% libsrc\k_meshio.f
%FC% libsrc\k_bc.f
%FC% libsrc\elements.f
%FC% libsrc\general.f
%FC% libsrc\small.f
%FC% libsrc\shape.f
%FC% libsrc\gauss.f
%FC_CPP% libsrc\matrix.F
%FC% libsrc\mpi_nul.f
%FC_CPP% libsrc\int_gen.F90

%FC_CPP% libsrc\afront.F90

rem %FC%

rem #### build the applications

%FC_CPP% src\qp_ps.f
%FC% src\qp.f libsrc\int_gen.obj
rem %FC% src\danfe.f

%FC% src\danfront.f90
rem %LN%

