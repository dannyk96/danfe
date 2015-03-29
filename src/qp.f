      PROGRAM QUICKPLOT
C
C     this will do a 'quick' 2D plot of a mesh
C      Fortran 90 version (was Fortran 77) DJK 31-10-95
C
C
      PARAMETER (IGC=8000,INUMS = 20)
      REAL GC(2,IGC)
      INTEGER NUMS(INUMS,IGC),NENS(IGC), imats(igc)
      real x(100),y(100)
      CHARACTER FILE*100
      integer pal_default(3,16)
      integer pgopen

      DATA PAL_DEFAULT/
     +     0, 90, 10,   138, 75, 39,    83,128, 57,    90,  0,190,
     +   123, 45, 76,
     +   194, 49, 56,   252, 78,207,   253,202, 90,     0,255,  0, 
     +    40, 40,255,   169,133, 23,   255,  0,  0,   255,127,  0,
     +    255,255, 0,   255,255,255,   127,127,127/

C---------------------- open the data file------------------------------
      FILE='unknown'
      if (iargc() == 0) stop "Missing datafile name"
      call getarg(1,FILE)
c     IF (FILE.EQ.' ') CALL SELECT_FILE@('*.PL',FILE,*999)
      print*,'data file ="',FILE(1:len_trim(FILE)),'"'
      OPEN (UNIT=7, FILE=FILE, STATUS='OLD',ERR=999)
C------------------------ read the data --------------------------------
      READ(7,*) NODOF
      READ(7,*) NN
      WRITE(*,'(a,i7,a,i2,a)') '<> Reading', NN,' Nodes (',NODOF,'d)'
      DO I=1,NN
        READ(7,*) ID,(GC(J,I),J=1,2)
      ENDDO
      READ(7,*) NEL
      WRITE(*,'(a,i7,a,i2,a)') '<> Reading', NEL,' Elements '
      READ(7,*) (ID,NENS(I),(NUMS(J,I),J=1,NENS(I)),imats(i),I=1,NEL)
c-------------------- define the nominal screen size  ------------------
      RESX = 640
      RESY = 480
      AR = RESX / RESY
C--------------------- maxima and minima -------------------------------
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
c for Fortran 90
c     xmin=minval (gc(1,1:nn)); xmax=maxval (gc(1,1:nn))
c     ymin=minval (gc(2,1:nn)); ymax=maxval (gc(2,1:nn))
C---------------------- normalize the data -----------------------------
c no need for this with pgplot
      F = 0.10         !- shrink factor
      R = (1.-2.*F)    !- resulting size
      f1=1+f
      f2=1-f1

      DATAX = MAX ((XMAX - XMIN),(YMAX - YMIN)*AR)   !- scale factor
      xc= (xmax+xmin)/2.
      yc= (ymax+ymin)/2.
      rad = 0.5*max (xmax-xmin,ymax-ymin)
      
c     DO I = 1,NN
c       gc(1,i) = gc(1,i) + (xmax-xmin)*.20 ! ARBITRARY 10% SHIFT
c       GC(1,I =        RESX * (FACT+R*(GC(1,I)-XMIN) /DATAX)
c       GC(2,I) = RESY - RESX * (FACT+R*(GC(2,I)-YMIN) /DATAX) 
c       GC(2,I) =        RESX * (FACT+R*(GC(2,I)-YMIN) /DATAX) 
c     ENDDO 

C-----------------------  into graphics ---------------------------------
      
c     print*, 'pgopen retuns', pgopen('/XWIN')
      IF (PGOPEN('/XWIN') .LE. 0) THEN 
         print*,' Pgplot X-driver missing, trying /GIF'
         IF (PGOPEN('/GIF') .LE. 0) THEN 
            print*,' Pgplot GIF-driver missing, trying /PS'
         IF (PGOPEN('/PS') .LE. 0) STOP
     &      'Pgplot Postscript-driver missing, exiting'
         endif
      endif 

      CALL PGSAVE    !- push attributes onto stack (eg pen pos.)
      do i=1,16
        call pgscr (i, pal_default(1,i)/255.,
     &     pal_default(2,i)/255.,pal_default(3,i)/255.)
      enddo

      CALL PGSCI(16)          
c     CALL PGENV(0.,resx,0.,resy,1,1) !- set viewoport calls PGBOX) (-2)
      CALL PGENV(xmin*f1+xc*f2,xmax*f1+xc*f2,
     &  ymin*f1+yc*f2,ymax*f1+yc*f2,1,0) !- set viewoport calls PGBOX) (-2)
c     CALL PGENV(xc-rad*f1, xc+rad*f1
c    &  ,yc-rad*f1,yc+rad*f1,1,0) !- set viewoport calls PGBOX) (-2)

      
      CALL PGLAB(' ', ' ', 'Quickplot - pgplot version')
c     call pgscr (4,0.,0.,.2)
c     call pgscr (5,.4,.4,.4)

C----------------------- plot the mesh ---------------------------------
      DO IEL = 1,NEL
        NEN = NENS(IEL)
        imat= imats(iel)
        DO J=1,NEN
c         K = MOD(J,NEN)+1
          x(j)=GC(1,NUMS(J,IEL))
          y(j)=GC(2,NUMS(J,IEL))
        ENDDO
        CALL PGSFS(1)    !- 1=solid fill
        CALL PGSCI(imat)          
        CALL PGPOLY(nen,X,Y)   !- edge polygon
        CALL PGSFS(2)    !- 2=outline
        CALL PGSCI(15)          
        CALL PGPOLY(nen,X,Y)   !- edge polygon
c       call sleep(1)
      ENDDO
C------------------- plot the nodes ------------------------------------
      CALL PGSCI(15)          
      DO I=1,NN
        x(1) = gc(1,i)
        y(1) = gc(2,i) 
        call pgpt1 (gc(1,i),gc(2,i),1)
c       CALL FILL_ELLIPSE@ (INT(GC(1,I)),INT(GC(2,I)),1,1,12)
      ENDDO
C-----------------------------------------------------------------------
      CALL PGUNSA  !  pop back environment state
c     CALL PGEBUF  !- 'make picture current'

c     READ*
      call pgclos()    !- wait to CR then exit
  999 CONTINUE
      END
