!      WINAPP
      PROGRAM QUICKPLOT
!
!     this will do a 'quick' 2D plot of a mesh
!     DJK 31-10-95
!     OpenGL version : Dan Kidger
!
      implicit none
#include <GL/fgl.h>
#include <GL/fglut.h>
!     integer :: GLUT_DOUBLE,GLUT_RGB,GLUT_DEPTH
!     integer :: GL_COLOR_BUFFER_BIT=16384,GL_MODELVIEW=5888
!     integer :: GL_LINES=1,GL_LINE_LOOP=2,GL_POLYGON=9

#ifdef SALFORD
!     integer winio@
#endif
      integer ctrl, cbnull
      external cbnull
      real :: resx,resy, one=1., zero=0.
      real*8 ::  done=1., dzero=0.
      integer :: ihandle

      INTEGER IGC,INUMS, iargc
      common /com/nums,nens,gc, nn,nel
      integer,parameter :: IGC=20000,INUMS = 20
      REAL GC(2,IGC)
      INTEGER NUMS(INUMS,IGC),NENS(IGC)

      INTEGER NN,NEL,I,J,IEL,NEN,ID,NODOF
      REAL XMIN,XMAX,YMIN,YMAX,AR, FACT,R,DATAX, ar2
      external draw_mesh
      
      CHARACTER FILE*80, wintitle*120
#ifdef SALFORD
      character CMNAM@*80
#endif
!---------------------- open the data file------------------------------
#ifdef SALFORD
!     FILE = CMNAM@()
#endif
      if (iargc().gt.0) then
        call getarg(1,file)
      else
        file="femur2.pl"
      endif
      OPEN (7,FILE=FILE, STATUS='OLD',ERR=999)
!------------------------ read the data --------------------------------
      READ(7,*) NODOF
      READ(7,*) NN
      DO I=1,NN
        READ(7,*) ID,(GC(J,I),J=1,2)
      ENDDO
      READ(7,*) NEL,(ID,NENS(I),(NUMS(J,I),J=1,NENS(I)),ID,I=1,NEL)
      print*,NN,' nodes and',NEL,' elements'
!--------------------- maxima and minima -------------------------------
      XMIN =  1.E37
      XMAX = -1.E37
      YMIN =  1.E37
      YMAX = -1.E37
      DO I = 1,NN
        XMAX = MAX (XMAX,GC(1,I))
        XMIN = MIN (XMIN,GC(1,I))
        YMAX = MAX (YMAX,GC(2,I))
        YMIN = MIN (YMIN,GC(2,I))
      ENDDO
      AR2 = (YMAX-YMIN)/(XMAX-XMIN)
!-------------------- go into graphics mode ----------------------------
!      resx=320; resy=240
       RESX = 300;  RESY = 300 * ar2
      AR = RESX / RESY
      write (wintitle,'(a,a,a)')  
     & 'QuickPlot - unregistered - [',trim(file),']'

#ifdef SALFORD
!-- for DBOS graphics
!     CALL VGA@()
!     CALL CLEAR_SCREEN_AREA@ (0,0,INT(RESX),INT(RESY),1)
#endif


!-- for OpenGL on Unix+X11
         call glutinit()
         call glutInitDisplayMode (GLUT_SINGLE+GLUT_RGB)
!        call glutInitDisplayMode (0)
         call glutInitWindowSize(int(resx),int(resy))
!        call glutInitWindowPosition(5, 105)
         ihandle=glutCreateWindow (wintitle)
         print*,'ihandle=',ihandle
!-- for OpenGL under Win98+Salford's Clearwin+
#if defined(SALFORD)
!       i=winio@('%es&')  !- 1: simplex no decoration --
!       i=winio@('%ww[no_frame,no_caption,casts_shadow]%og[static]&'
!    & ,int(resx),int(resy))

!       ctrl=0
!       i=winio@('%lw',ctrl)       !- dont auto-close this window

!       i=winio@('%es%ca@&',trim(wintitle))
!       i=winio@('%ww[no_border]%og[static]&',int(resx),int(resy))
!-- create a simple Win98 menu --
!        i=winio@('%mn[&File[&New]]&',cbNULL)
!        i=winio@('%mn[[&Open...]]&', cbNULL)
!        i=winio@('%mn[[&Save...,|]]&',cbNULL)
!        i=winio@('%mn[[&Print]]&', cbNULL)
!        i=winio@('%mn[[|,E&xit]]&','EXIT')
!        i=winio@('%mn[&Help]&', cbNULL)

!       ctrl=0
!       i=winio@('%lw',ctrl)       !- dont auto-close this window
!-- create some on screen buttons?
#endif

      print*,'<> window created'
!     CALL fglClearColor(0.0, 0.0, 127.0, 0.0)
      CALL fglClearColor(zero,zero,one*.25,zero,zero)
!     CALL fglClear(GL_COLOR_BUFFER_BIT)
!      CALL glMatrixMode(GL_PROJECTION)
      CALL fglLoadIdentity()
!      CALL glOrtho(0.d0, resx, 0.d0, resy, -1d0, 1d0)  !- OK?
!     call gluOrtho2D (0.d0, 10.d0,0.d0,10.d0)
      CALL fglMatrixMode(GL_MODELVIEW)
! or scale to the actual mode dimensions ?
      call fgluOrtho2D (dzero, done*resx,dzero,done*resy)   !- scale to the available window

!     call fglmatrixmode(GL_PROJECTION)
!     call fglloadidentity()
!     call fglortho(dzero, done, dzero, done, -done, done)

      print*,'<> projection created'

!---------------------- normalize the data -----------------------------
      FACT = 0.15         !- shrink factor
      FACT = 0.02         !- shrink factor
      R = (1.-2.*FACT)    !- resulting size

      DATAX = MAX ((XMAX - XMIN),(YMAX - YMIN)*AR)   !- scale factor

      DO I = 1,NN
        GC(1,I) =        RESX * (FACT+R*(GC(1,I)-XMIN) /DATAX)
!       GC(2,I) = RESY - RESX * (FACT+R*(GC(2,I)-YMIN) /DATAX)  !- if y-origin is at the top
        GC(2,I) =        RESX * (FACT+R*(GC(2,I)-YMIN) /DATAX)
      ENDDO
  999 CONTINUE
      call glutdisplayfunc(draw_mesh)
      call glutmainloop()

      END program quickplot

!----------------------- plot the mesh ---------------------------------
      subroutine draw_mesh()
      implicit none
#include <GL/fgl.h>
      common /com/nums,nens,gc, nn,nel
      integer,parameter :: IGC=20000,INUMS = 20
      REAL GC(2,IGC)
      INTEGER NUMS(INUMS,IGC),NENS(IGC)
      INTEGER NN,NEL,I,J,IEL,NEN,ID,NODOF
      integer :: ic=0 
      ic=ic+1
!     print*,ic,': lets ploti, nel,nn=',nel,nn
      CALL fglClear(GL_COLOR_BUFFER_BIT)
!     CALL fglColor3f(0.5, 0.2, 0.2)
!     CALL fglBegin(GL_POLYGON)
!       CALL fglVertex2f(0.1, 0.1);   CALL fglVertex2f(0.1, 0.9)
!       CALL fglVertex2f( 0.9, 0.9);    CALL fglVertex2f( 0.9, 0.1)
!     CALL fglEnd()

      DO IEL = 1,NEL
        NEN = NENS(IEL)
        CALL fglColor3f(.0, .0, 0.4)
        CALL fglBegin(GL_POLYGON)
        DO J=1,NEN
          CALL fglVertex2f(GC(1,NUMS(J,IEL)),GC(2,NUMS(J,IEL)) )
        ENDDO
        CALL fglEnd()

        CALL fglColor3f(1.0, 1.0, 0.7)
        call fglBegin(GL_LINE_LOOP)
        DO J=1,NEN
          CALL fglVertex2f(GC(1,NUMS(J,IEL)),GC(2,NUMS(J,IEL)) )
        ENDDO
        CALL fglEnd()

      ENDDO
!     CALL fglFlush()

!------------------- plot the nodes ------------------------------------
      CALL fglColor3f(1.0, 0.3, .2)
      call fglBegin(GL_LINES)
      DO I=1,NN
          CALL fglVertex2f(GC(1,I)-1.,GC(2,I)-1.)
          CALL fglVertex2f(GC(1,I)+1.,GC(2,I)+1.)
      ENDDO
      CALL fglEnd()
      CALL fglFlush()

      end subroutine draw_mesh
    
!.. now we wait for 'esc' to be pressed'
#ifdef SALFORD
!      CALL TEXT_MODE@()
#endif

!-------------------
!     integer function cbnull()
!     cbnull=1
!     end

!-------------------
