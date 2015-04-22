!
!   This is the minimal version of int_gen.F
!
!  These routines allowed F77 extensions like getting the time,date, command line
! arguements etc.
!

!  now (2015) I would hope that all the danfe suite of code can use f90 intriniscs
! directly

! caveats: need to find a home for:
!   get_wallclock: wrapper arond F90's SYSTEM_CLOCK that caches the tick rate
!
!   get data file name that try the command line and if not tries file FILE etc.
!   or indeed prompts?
!
!   get_random : in F90 this is a subroutine not a fucntion

!-----------------------------------------------------------------------
      subroutine danfe_getarg(N,arg)
!     integer    :: i
      character  :: arg*(*)
      arg = ""
      call getarg (N,arg)     ! need to compile with -Vaxlib
! Fortran2003 adds 
!   CALL GET_COMMAND_ARGUMENT(number, value, length, status) 
!     - This is a subroutine returning the string value of the Nth argument (argument 0 being the command-name). 
!    The length (optional) returns the length of the value returned, and status (also optional) returns 0 normally, or non-zero if there is an error such as a non-existing argument).

      end
!-----------------------------------------------------------------------
      function danfe_argc() result(nargs)
      nargs=iargc()
! Fortran2003 adds COMMAND_ARGUMENT_COUNT()
!    - This is an integer function returning the number of argument strings on the command-line (excluding the initial command-name if there is one).
      end

!-----------------------------------------------------------------------
      subroutine my_getenv(envvar, string)
!
!     This routine is written as a already obsolete
!     A wrapper around he Intrinsic get_environment_variable() whcih appears in F2003
!     Dan Kidger 29/03/15
!
      character envvar*(*), string*(*)

#ifdef __FTN95
      character :: getenv@*(*)
      string = getenv@(envvar)
!     call dosparam@(envvar,string)
#else
      CALL get_environment_variable(envvar, string)
#endif
      end

!-----------------------------------------------------------------------
    subroutine my_getstderr(stderr)
!
!     returns the unit number for write() to stderr
!
!     use, intrinsic :: iso_fortran_env, only : error_unit=>stderr
      implicit none
      integer :: stderr
#ifdef __FTN95
      stderr=2   ! Salford FTN95 has 1,2,5,6 all going to the console
#else
      stderr=0   !- generic
#endif
    end


!-----------------------------------------------------------------------
      SUBROUTINE GET_DATA_FILE_NAME (FILE)
      CHARACTER*(*)   FILE      !/* The returned file name */
      INTEGER IPASS
      DATA IPASS/0/
      print*,'picking up a data file name ...'
      ipass=ipass+1
      FILE=" "                  !- make sure we are picking up a new name
      call getarg(ipass,FILE)   !(check)
      end

!-----------------------------------------------------------------------

  FUNCTION GET_RANDOM () result(x)
!
!     Return a real random number (0.->1.)
  real x
  call RANDOM_NUMBER(x)
! get_random=x
  end
!-----------------------------------------------------------------------
      SUBROUTINE GET_SECS (TIME)
!
!     Return the system 'seconds' counter - hence can time processes.
!     This is CPU, not wallclock
!
      save
      logical firstpass
      REAL   TIME, persec
      data firstpass/.true./

      IF (FIRSTPASS) THEN
        CALL SYSTEM_CLOCK (ICOUNT,ICOUNT_RATE,ICOUNT_MAX)
             print*,' the system clock has',icount_rate,' ticks/s'
        PERSEC=1./REAL(ICOUNT_RATE)
        FIRSTPASS=.FALSE.
      ENDIF
        CALL SYSTEM_CLOCK (ICOUNT)
        TIME=real(ICOUNT)*PERSEC
   end
!-----------------------------------------------------------------------
      FUNCTION FREE_MEMORY (IOP) RESULT (X)
      REAL X
      integer,intent(in) :: iop
      X=999.
      end
!-----------------------------------------------------------------------
      SUBROUTINE INTO_VGA (ICOL,IRESX,IRESY)
!      -- a more generic subroutine name ?
      IRESX=640
      IRESY=480
      CALL OPEN_GRAPH_W (ICOL,'Danplot',10, 10,IRESX,IRESY,ihandle)
      END

!-----------------------------------------------------------------------
      SUBROUTINE OPEN_GRAPH_W (ICOL,TITLE,IXO,IYO,IRESX,IRESY, ihandle)
      INTEGER IXO,IYO,IRESX,IRESY,ICOL
      CHARACTER TITLE*(*)
      END
!-----------------------------------------------------------------------
      SUBROUTINE CLOSE_GRAPH_W (ihandle)
      end
!-----------------------------------------------------------------------
      SUBROUTINE INTO_TEXT ()
!      -- a more generic subroutine name ?
      ihandle = 1
      CALL CLOSE_GRAPH_W (ihandle)
      END

!-----------------------------------------------------------------------
      SUBROUTINE DRAW_A_LINE (XF,YF,XT,YT,ICOL)
      ihandle = 1
      CALL DR_LINE (ihandle,XF,YF,0., XT,YT,0.,ICOL)
      RETURN
      END
!-----------------------------------------------------------------------
      SUBROUTINE DR_LINE (ihandle,XF,YF,ZF, XT,YT,ZT,ICOL)
      REAL XF,YF,XT,YT
      INTEGER ICOL
      END
!-----------------------------------------------------------------------
      CHARACTER*20 FUNCTION DATE_STAMP()
!
!     Returns the current date and time as a character string
!      format is:'hh:mm:ss  dd/mm/yy  '  (8+2+8+2)
!      (not realy system specific but included here for neatness)
      CHARACTER  TIME_C*8, DATE_C*8
      DATE_STAMP ( 1:9)  = TIME_C()
      DATE_STAMP (10:20) = DATE_C()
      RETURN
      END
!-----------------------------------------------------------------------
      CHARACTER*8 FUNCTION TIME_C()
      character mytime*12
      CALL DATE_AND_TIME(TIME=MYTIME)
      TIME_C=MYTIME(1:2)//':'//MYTIME(3:4)//':'//MYTIME(5:6)
      END
!-----------------------------------------------------------------------
      CHARACTER*8 FUNCTION DATE_C()
      character mydate*10
      CALL DATE_AND_TIME(DATE=MYDATE)
      DATE_C=MYDATE(7:8)//'/'//MYDATE(5:6)//'/'//MYDATE(3:4)
      end

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------







!TODO

