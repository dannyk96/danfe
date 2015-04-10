c
c              D A N L I B  Hardware Dependant Interfaces
c
c   These are the interfaces to those external routines that are hardware
c   platform dependant - for example the time, date, cpu_time.
c   Where possible the Fortran 90/95 code is used

c
c  Revisions
c  12-11-96 added ARGC,GETARG to return the count and N'th command line arg.
c  15- 7-97 re-build INT_NUL as the master copy
c   7- 9-99 generic interface: using cpp - use -DOS=UNICOS style to compile :-)
c           (under windows/dos, yoiu may need the explicit int_dbos.f if cpp isc            not available
c
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c  The routines are concern with:
c    1: Command line parsing
c    2: Returning system-info such as time,date,free memory +random numbers.
c    3: Basic Graphics (see DRAW_SAL.f in /DANLIBG/ too)

!  I need to add:
!     reading Environment variables
!     OS call ( eg 'ps2pdf')
c
c  ROUTINES:
c    ARGC       : returns the number of command-line arguments
c    GETARG     : returns the n'th argument from the command-line.
c    GET_DATA_FILE_NAME : returns the 'data file name'

c    GET_RANDOM : returns a pseudo-prandom number: 0.0000->0.9999
c    GET_SECS   : returns the 'seconds' counter (for timing processes.).
c    TIME_C     : returns the time of day as hh:mm:ss
c    DATE_C     : returns the date as DD:MM:YY
c      DATE_STAMP : returns time and date together.
c    FREE_MEMORY : returns the amount of free_meomory.
c      PRINT_FREE_MEMORY : prints the amount of remaining memory in Mb.
c   (GET_SORT   : ranks real numbers - I now have my own. )
c
c    INTO_VGA   : header to OPEN_GRAPH_W
c    OPEN_GRAPH_W: creates a graphics windows and returns its x,y size.
c    INTO_TEXT  : closes the graphics window.
c    DRAW_A_LINE: draw a 2d line from xy to XY, in a given colour
c    DR_LINE    : draw a 3d line (hence use on SGI etc.)
c
c  The original file manifested itself as:
c    INT_NUL.F   : For a generic Fortran enviroment : reads CL from 'FILE'
c                : This is the MASTER copy - others are derived from it.
c    INT_DBOS.F  : For Salford's DBOS enviroment on PC's
c    INT_SGI.F   : For Silicon Graphics (eg. O2) - also generic Unix ?

c--- cpp macro definitions ---
c.. these are any arbitary unique integers for the compiler flag: -DOS=value
c-- operating system platforms---
#define DBOS   10
#define UNIX   11
#define IRIX   12
#define HPUX   13
#define UNICOS 14
#define FUJI   15
#define LINUX  16
#define TRU64  17 
#define UNKNOWN 999

c-- Fortran version --
c assume default is extended f77, but allow f90 where usefull
#define F77    77
#define F90    90

c-- graphical sub-system --
c compare also with my explicit Postscript drivers
#define OPENGL    40
#define CLEARWIN  41

C-----------------------------------------------------------------------
      FUNCTION GET_RANDOM ()
C
C     Return a real random number (0.->1.)
C       eg. when spliting quads to tris or for voronoi mesh generation.
c     - use F90's RANDOM_NUMBER if available
C
      REAL X, RANF, get_random

#if OS==DBOS
c------ 1: DBOS
       REAL*4 RANDOM
       EXTERNAL RANDOM
#endif

!#ifdef F90
c----- Fortran 90 interface
       CALL RANDOM_NUMBER (X)
#if OS==DBOS
c----- DBOS interface
      X = RANDOM ()
#elif OS==UNICOS
      X=RANF()
#elif OS==FUJI
      X=RAND(0)   !- 0=next number, 1= reset to first number.


#elif OS==LINUX
c---- LINUX interface 
c     X=drand48()    !- never tetsed this?
      X=0.5
#else
c----- null interface
!     X = 0.50000          !- dummy is half way so no net difference(!)
#endif
      GET_RANDOM = X
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_SECS (TIME)
C
C     Return the system 'seconds' counter - hence can time processes.
C     This is CPU, not wallclock
C
      save
      logical firstpass
      integer*8 irtc_rate
      real*4 :: time4, etimes(4)
      real*8 :: time8
!     be careful - next might be better as generic integer ?
      integer*4 icount,icount_rate, icount_max
      data firstpass/.true./rate/1./
      REAL   TIME, persec, ticks(2)

#if OS==DBOS
c------ 1: DBOS
      REAL*8 TIME2
#endif

c----- null interface
      time=0

#if OS==FUJI
c      time=etime(dm)
c      time=dm(1)
c      call clockv(timev,timec,0,2)
       call clock(timec,0,2)   !- I think that this is best
       time=timec ! total
c      time=timev ! vector time only

#elif FORTRAN==F90

#define WALLCLOCK
!#define ETIME

#ifdef WALLCLOCK
! *CAVEAT* Intel's IFC 5.0 has tick rate of only one per second!
      IF (FIRSTPASS) THEN
        CALL SYSTEM_CLOCK (ICOUNT,ICOUNT_RATE,ICOUNT_MAX)
             print*,' the system clock has',icount_rate,' ticks/s'
        PERSEC=1./REAL(ICOUNT_RATE)
        FIRSTPASS=.FALSE.
      ENDIF
        CALL SYSTEM_CLOCK (ICOUNT)
        TIME=real(ICOUNT)*PERSEC
#elif defined(ETIME)
      call etime(etimes) 
      time = etimes(1)
#else
! *CAVEAT*  Intel's IFC 5.0 takes 15-22 seconds to call this function 
! for the first time!
!      call cpu_time(time8)
!      time=time8
#endif

#elif OS==DBOS
      CALL DCLOCK@(TIME2)
      TIME = TIME2
c      ticks(1)=0
c      ticks(2)=0
c      time=etime(ticks)
c     time=second()    !- beware this is real*4 :-(
#elif OS==UNICOS
c----- Unicos cf F90
        if (firstpass) then
          persec=1./real(irtc_rate())
          firstpass=.false.
        endif
        call irtc(icount)    !- as real*8
        time=icount*persec
c       call second(time) !- or try this

#elif OS==LINUX
c----- Linux
c      call SECOND(time)    !(check)      
       time=0
#elif OS==UNIX
c----- generic Unix
!       call SECONDS(time)
#else

#endif

      RETURN
      END

C-----------------------------------------------------------------------
      CHARACTER*8 FUNCTION TIME_C()
C
C     Returns the current time as a character string
C
      character mytime*12
#if OS==DBOS
      character time@*8
#endif

#if FORTRAN==F90
c-- generic Fortran 90 interface
      CALL DATE_AND_TIME(TIME=MYTIME)
      TIME_C=MYTIME(1:2)//':'//MYTIME(3:4)//':'//MYTIME(5:6)
#elif OS==DBOS
      TIME_C=time@()
#elif OS==LINUX
c  the TIME subroutine exists under g77, but I do not know the arguments
c      CALL TIME (MYTIME)
c      TIME_C=MYTIME
      TIME_C='h:mm:ss'
#elif OS==IRIX
      CALL TIME (MYTIME)
      TIME_C_MYTIME
#elif OS==UNICOS
      CALL TIME (MYTIME)
      TIME_C=MYTIME
c#elif OS=FUJI
c     CALL TIME(MYTIME)
#elif OS==UNIX
c----- Generic Unix ?
      TIME_C=DATE()
#else
c----- null interface
      TIME_C='h:mm:ss'
!      CALL DATE_AND_TIME(TIME=MYTIME)
!     TIME_C=MYTIME(1:2)//':'//MYTIME(3:4)//':'//MYTIME(5:6)
#endif
      RETURN
      END

C-----------------------------------------------------------------------
      CHARACTER*8 FUNCTION DATE_C()
C
C     Returns the today's date as a character string
C
c     intrinsic DATE
      character irix_date*28, mydate*10
#if OS==DBOS
c------ 1: DBOS
      character date@*8
#endif

#if FORTRAN==F90
c-- generic Fortran 90 interface
      CALL DATE_AND_TIME(DATE=MYDATE)
      DATE_C=MYDATE(7:8)//'/'//MYDATE(5:6)//'/'//MYDATE(3:4)
#elif OS==DBOS
      DATE_C=date@()

#elif OS == LINUX
c------ 3: Linux
      !call DATE(mydate)
      !date_c=mydate
      DATE_C='dd/mm/yy'
#elif OS == IRIX
c------ 4: Irix
      call DATE(irix_date)  !- need to cut fields out.
      date_c=irix_date(1:2)//irix_date(4:6)//irix_date(8:9)
#elif OS == UNICOS
c------ 5: Unicos
      call date(mydate)
      date_c=mydate
#elif OS ==FUJI
      call date(irix_date)
      date_c=irix_date(1:2)//irix_date(4:5)//irix_date(7:8)

#elif OS == UNIX
c----- Generic Unix
      TIME_C=DATE()
#else
c----- null interface
      DATE_C='dd/mm/yy'
#endif
      RETURN
      END

C-----------------------------------------------------------------------
      CHARACTER*20 FUNCTION DATE_STAMP()
C
C     Returns the current date and time as a character string
C      format is:'hh:mm:ss  dd/mm/yy  '  (8+2+8+2)
C      (not realy system specific but included here for neatness)
      CHARACTER  TIME_C*8, DATE_C*8
      DATE_STAMP ( 1:9)  = TIME_C()
      DATE_STAMP (10:20) = DATE_C()
      RETURN
      END

C-----------------------------------------------------------------------
      FUNCTION FREE_MEMORY (IOP)
C
C     Returns the amount of memory left in the system
C      IOP controls: a) if in Mb or bytes, b) real memory or virtual.
C        DJK 15-7-97
C
      REAL FREE_MEMORY
#if OS==DBOS
c----- DBOS
      INTEGER*4 FREE_SPACE_AVAILABLE@, NP1,NP2,NP3,NP4,NP5,NP6,NP7
#endif

#if OS==DBOS
c------ 1: DBOS
C NP1-7=total 640k, total ext, free_640, free_ext,
C       total disk, free disk,   No_of_page_swaps (=0 if no diskswaps)
c      FREE_MEMORY = FREE_SPACE_AVAILABLE@() /1024./1024.
      CALL GET_MEMORY_INFO@ (NP1,NP2,NP3,NP4,NP5,NP6,NP7)
      FREE_MEMORY = (NP3+NP4)*4096./1024./1024.
#else
c----- null interface
      FREE_MEMORY = 999.            !- assume that we have lots. (1Gb)
#endif

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE PRINT_FREE_MEMORY ()
C      *obsolete*
C     Prints the amount of free memory left in the system
C
      REAL FREE_MEMORY
      WRITE(*,'(A,f9.2,A)') '<> Remaining Memory =',FREE_MEMORY(1),' Mb'
      RETURN
      END

!-----------------------------------------------------------------------
      function danfe_argc()
!     returns the number of command line arguments
!
!#ifdef __IFC
      nargs=iargc()     ! need to compile with -Vaxlib
!#else
!      nargs=0    
!#endif

      danfe_argc = nargs
      end

!-----------------------------------------------------------------------
      subroutine danfe_getarg(N,arg)
!     returns the N'th command line argument.
!
      character arg*(*)
      arg = ""
!#ifdef __IFC
      call getarg (N,arg)     ! need to compile with -Vaxlib
!#endif
      end

!-----------------------------------------------------------------------
      subroutine my_getenv(envvar, string)
!
!     This routine is written as a already obsolete
!     A wrapper around he Intrinsic get_environment_variable() whcih appears in F2003
!     Dan Kidger 29/03/15
!
      character envvar*(*), string*(*)

      CALL get_environment_variable(envvar, string)
      return
      end

c-----------------------------------------------------------------------
! 13/10/02 renamed next to avoid conflicts.
      FUNCTION ARGC2()
c
c     Counts the number of *arguments* on the command-line (after the prog name)
c     cf in UNIX the program name is arg #1
c       DJK 12-11-96  (from B+G?)
c

      CHARACTER*255 STRNG, CMNAM

#if OS==DBOS
c------ 1: DBOS
      CALL CMNAMR()
      DO I=1,9999
        STRNG = CMNAM()
        IF (STRNG(1:1) .EQ. ' ') THEN
          ARGC = I+1              !- I=1 shurely ?
          CALL CMNAMR()
          RETURN
        ENDIF
      ENDDO
#elif OS==IRIX
      ARGC=IARGC()
#else
c----- null interface
c cf. using file 'FILE' as a dummy command line ?
      ARGC = 1              !- ie. no arguments- except the program name itself
c     ARGC=iargc()
#endif

      argc2=argc
      END

c-----------------------------------------------------------------------
      SUBROUTINE GETARG_2 (N, ARG, L)
c
c     Returns the N'th arguement from the command-line
c     maybe this would be better as a function ?
c     or even better scrapped completely and simply pseudo GETARG where necessary
c       DJK 12-11-96  (from B+G?)
c
      CHARACTER*(*) ARG
      CHARACTER*80 CMNAM, DUMMY
#if OS==DBOS
c------ 1: DBOS
      CALL CMNAMR()                         !- reset the command line
      DO I=1,N
        DUMMY =CMNAM()
      ENDDO
      CALL CMNAMR()                         !- and reset again (why?)

#elif OS==UNICOS
      call pxfgetarg(N,ARG,ilen,ierror)
#elif OS==FUJI
      call getarg(N,ARG)
#elif OS==IRIX
      call getarg(N,ARG)
#elif OS==HPUX
      ARG=getarg(N,ARG)
#elif OS==LINUX
      call getarg(N,ARG)       !- beware GETARG doesn't appear to be here
#elif OS==TRU64
      call getarg(N,ARG)       !- beware GETARG doesn't appear to be here

#elif OS==UNIX
c     ARG=getarg(N,arg)       !- is getarg generic?
      call getarg(N,arg)       !- is getarg generic?
#else
c----- null interface
       ARG = ""
#endif
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_DATA_FILE_NAME (FILE)
C
C     This gets the file name from the command-line
C     If none was given then SELECT_FILE is used to get one
C     .. should also test that the given file name is valid
c
c    15-7-97 maybe I should do formaly as:
C         if ARGC()=0, try a file called FILE
C         elseif FILE doesn't exist: try prompting using SELECT_FILE
c         else use the DOS-prompt
C         else : error  ??
C
      CHARACTER*(*)   FILE      !/* The returned file name */
      INTEGER IPASS
#if OS==DBOS
c----- DBOS
      CHARACTER CMNAM@*80       ! The command line function
#endif
      DATA IPASS/0/

      print*,'picking up a data file name ...'
      ipass=ipass+1
      FILE=" "                  !- make sure we are picking up a new name

#if OS==DBOS
c------ 1: DBOS
      FILE = CMNAM@()                  !-- the command-line
      IF (FILE.EQ.' ') CALL SELECT_FILE@('*.d*',FILE,*999)
      CALL TO_LOWER (FILE)     !- in lower case please (are you sure?)

#elif OS==UNICOS
c--- Unicos ---
      call pxfgetarg(ipass,FILE,ilen,ierror)
c     call getarg(ipass,FILE)   ! on UNICOS GETARG is a function I beleive

#elif OS==IRIX
      call getarg(ipass,FILE)
#elif OS==FUJI
      call getarg(ipass,FILE)
#elif OS==HPUX
      call getarg(ipass,FILE)   !(check this)
#elif OS==LINUX
      call getarg(ipass,FILE)   !(check)
#elif OS==TRU64
      call getarg(ipass,FILE)   

#else
c----- generic interface : use a file called 'FILE'
c we call this several times until all files have been handled.
c -- how should we support parallel systems - one file per PE ?
c   what about valid wildcards (cf escape sequences), is there a
c    precendence?
c  Ok try if file contains '_PE_' then substitute the PE number :-)
c  perhaps other '_FOO_' could also be accomodated?

c     FILE=' '
c      call getarg (ipass,FILE)
c      if (FILE.ne.' ') return

c--- method of extracting a filename from a file called 'FILE' ---
      OPEN (90,FILE='FILE',STATUS='OLD',iostat=ios)
      if (ios.ne.0) return   !- missing file called 'FILE'
      do i=1,ipass          !- allow for muliple calls
        READ (90,'(A)',iostat=ios) FILE
        if (ios.ne.0) file=' '      !- end of file
      enddo
      CLOSE (90)
#endif

      RETURN
! 999 call myerror (3,'FILE SELECTION aborted')
      END


C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE INTO_VGA (ICOL,IRESX,IRESY)
c      -- a more generic subroutine name ?
      IRESX=640
      IRESY=480
      CALL OPEN_GRAPH_W (ICOL,'Danplot',10, 10,IRESX,IRESY,ihandle)
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE OPEN_GRAPH_W (ICOL,TITLE,IXO,IYO,IRESX,IRESY, ihandle)
c
c     Creates a Window for drawing graphics,
c        and sets the background colour
c     where:
c     IXO, IYO is the window position (n/a for full-screen DOS)
c     IRESX,IRESY is the window size (=fixed 640/480 for generic PC)
c     ICOL is the backgound colour
c     IHANDLE is the window code# so that we can identify it later.
c       Dan Kidger 15-7-97
c
c    * Contrast a) a full-apllication with menus; b)a simple graph windown ;
c      c) full-screen (DOS) , d) DANPLOT_for_DOS
c
      INTEGER IXO,IYO,IRESX,IRESY,ICOL
      CHARACTER TITLE*(*)
c------ 2: Clearwin (DBOS+MS-Windows)
c      INCLUDE <WINDOWS.INS>             !- loadsa definitions
c      COMMON/G/ WHDC,WINDOW,XRES,YRES  !- in common so-> 'draw' routine

#if OS==DBOS
c----- DBOS
      print*,'press <Enter> to enter/leave graphics mode'
      read*
      CALL VGA@()
      CALL SET_PALETTE@(ICOL,ICOL)
      CALL SET_VIDEO_DAC@(ICOL, 0/4, 50/4, 10/4)
      CALL CLEAR_SCREEN_AREA@ (INT(0),INT(0),
     &                         INT(IRESX),INT(IRESY),ICOL)

#elif OS==CLEARWIN
c------ 2: Clearwin (DBOS+MS-Windows)
c.. put the next line at the top of the source file.
c.. or do we just need this at the top of the main program?
c     WINAPP  20000,20000,'GRAPH.RC'    !- resource file?
c  GetSystemMetrics() is used to find screen sizes, etc.
c      XRES_S = GetSystemMetrics (SM_CXSCREEN)  !- the total screen size.
c      YRES_S = GetSystemMetrics (SM_CYSCREEN)
c      XRES = jxw         !- note that the window frame are added to these
c      YRES = jyw         !- to make the overall size larger.
c < mung ixo,iyo to get the true window top-left corner. >
c < also calculate the size of the border, menu, scroll-bars, etc.>
       IXFRAME=0
       IYFRAME=0
       WINDOW  = CREATE_WINDOW ('Quickplot',ixo,iyo,
      & IRESX+ixframe,IRESY+iyframe)
c  < now enable the menus and their function call-backs >

c------ X11 / SGI / Starbase

c----- OpenGL :-)
#elif GRAPHICS==OPENGL
c call some C that initialises the graphics


#else
c------ NULL
      PRINT*,'** Graphics not available'
#endif

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE INTO_TEXT ()
c      -- a more generic subroutine name ?

      ihandle = 1
      CALL CLOSE_GRAPH_W (ihandle)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE CLOSE_GRAPH_W (ihandle)
C
C     This closes the given graphics window
C        .. maybe choose the text-mode (cf 50-line mode)
C

#if OS==DBOS
c------ 1: DBOS
       read*                      != wait for a <CR>
       CALL TEXT_MODE@()
#elif OS==CLEARWIN
c------ 2: Clearwin (DBOS+MS-Windows)
c     a kill or a 'make_picture_current' ?
      CALL UPDATE_WINDOW (IHANDLE)       !- Windows version

#else
c-- nothing to do
#endif
      END

C-----------------------------------------------------------------------
      SUBROUTINE DRAW_A_LINE (XF,YF,XT,YT,ICOL)
c      -- a more generic subroutine name ?
      ihandle = 1
      CALL DR_LINE (ihandle,XF,YF,0., XT,YT,0.,ICOL)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE DR_LINE (ihandle,XF,YF,ZF, XT,YT,ZT,ICOL)
C
C     This simply draws a line betwwen the 2 given screen coords
C     - realy I want to use model coords & so I can let the hardware do
C      the transformation.
C
      REAL XF,YF,XT,YT
      INTEGER ICOL

#if OS==CLEARWIN
c------ 2: Clearwin (DBOS+MS-Windows)
       INCLUDE <WINDOWS.INS>
       COMMON/G/ WHDC,WINDOW,XRES,YRES   !- in common so-> 'draw' routine
#endif

#if OS==DBOS
c------ 1: DBOS
C    (eg.from QUICKPLOT) - reverse axes here?
      CALL DRAW_LINE@ (NINT(XF),NINT(YF),
     &                 NINT(XT),NINT(YT), ICOL)

#elif OS==CLEARWIN
c------ 2: Clearwin (DBOS+MS-Windows)
c.. first create a 'pen' of the given RGB colour.
        IF (THE_BLACK_PEN.eq.0)
     &      THE_BLACK_PEN=CreatePen(PS_SOLID,1,z'00007F')    !- try this ?
      IPEN =  MIN(MAX(0,ICOL),7)              !- clip to 8 pens
      IPEN = THE_BLACK_PEN
c.. is WHDC the same as IHANDLE ?
      CALL SelectObject (WHDC,pens(IPEN) )
      CALL MOVETO (WHDC,INTS(XF),INTS(YF))           !- assume y is up?
      CALL LINETO (WHDC,INTS(XT),INTS(YT))


c------ 3: X11 / SGI / Starbase
c.. will probably pass the full 3d coords
c.. also note the probable need for 'make_picture_current.

c------ 4: PGPLOT ------------------

#else
c-- nothing to do
#endif
      END


C-----------------------------------------------------------------------

      subroutine flush_io_buffers()
c
c  This simply makes sure all output files are up to date on disk
c   so that we can grep them from the interactive shell to sheck progress
c     DJK 21-7-99
c
c  This is not very good genericaly eg. for daughter output files
c  or if used from outside DANFE :-(
c
c      call flush (40,istat)    ! .tab
c      call flush (80,istat)    ! .out file
c      call flush (101,istat)   !- stdout
      END
C-----------------------------------------------------------------------
c-- from the CLIPS library 12-2-00 
!    -DNOFLUSH if your system doesn't support it.
#ifdef NOFLUSH
      SUBROUTINE flush(unit)
! null subroutine call
      INTEGER :: unit
      END !SUBROUTINE flush
#endif

! a code fragment for wallclock timing
! uses F90 by default, or a machine dependant timer if available

!#elif CRAY
!! returns milliseconds
!      CALL timef(clips_time)
!      clips_time=clips_time*0.001_8
!#elif MPITIME
!        include ``mpif.h''
!      clips_time=mpi_wtime()
!#else
!!F90 standard timer
!      INTEGER :: count, count_rate, count_max
!      CALL system_clock(count, count_rate, count_max)
!      IF(count_rate>0)THEN
!       clips_time=REAL(count)/REAL(count_rate)
!      ELSE
!        clips_time=0.0_8
!      END IF
!#endif

