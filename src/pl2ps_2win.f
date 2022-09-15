      PROGRAM QUICKPLOT
C
C     This will do a 'quick' 2D plot of a mesh to a Postscript file.
C       DJK 31-10-95
c       revised Sept 2002 to be 'Linux styled'
C
C
      PARAMETER (IGC=80000,INUMS = 20)
      REAL, allocatable :: GC(:,:)
      REAL ::  x(17),y(17)
      INTEGER,allocatable :: NUMS(:,:), NENS(:)
      CHARACTER ::FILE*80='foo',TEXT*255      !,CMNAM@*80
      
      call getarg(1,file)
C------------------------ parse the command line --------------------------------
!
!   options:
!      pl2ps  <filename>  [-o outfile>] [-C]  [--flags] [-S shrink]
!
!   flags could be a bitmask of things to draw, line node number, elem numbers, etc.
!    -S shrink   contract elements slightly like cracked mud.
!    -C colorize    for finer control , change the source.
!
!

      OPEN (UNIT=7, FILE=FILE, STATUS='OLD',IOSTAT=IOSTAT)
      IF (IOSTAT.NE.0) THEN
        PRINT*,' *** GIven .PL file does not exist!      <EXITING>'
        STOP
      ENDIF

C------------------------ read the data --------------------------------
      READ(7,*) NODOF
      READ(7,*) NN
      PRINT*,'<> Reading', NN,' nodes...'
      allocate (gc(nodof,nn))
      DO I=1,NN
        READ(7,*) ID,(GC(J,I),J=1,2)
      ENDDO
      READ(7,*) NEL
      PRINT*,'<> Reading', NEL,' elements...'
      allocate (NUMS(20,nel), nens(nel))
      do i=1,nel
        READ(7,*) ID,NENS(I),(NUMS(J,I),J=1,NENS(I)),ID
      enddo

c-------------------- go into graphics mode ----------------------------
      RESX = 8.*72.     !- A4 paper size ?
      RESY = 11.*72.
      resy = resy  /2.    !- do 2 images
      AR = RESX / RESY
C--------------------- maxima and minima -------------------------------
      XMIN =  1.E37            !- or simply MAXVAl and MINVAL in F90
      XMAX = -1.E37
      YMIN =  1.E37
      YMAX = -1.E37
      DO I = 1,NN
        XMAX = MAX (XMAX,GC(1,I))
        XMIN = MIN (XMIN,GC(1,I))
        YMAX = MAX (YMAX,GC(2,I))
        YMIN = MIN (YMIN,GC(2,I))
      ENDDO

C-------------------- write the Postscript headers ---------------------
c  consider writing to stdout,using ' -o' or munging a unique filename
      OPEN (20,FILE='plotfile.ps')
      PRINT*,'<> Plotting the titles'
      WRITE(20,'(A)')  '%!PS-Adobe-2.0 EPSF-1.2'
      WRITE(20,'(4A)') '%%Document title: ',FILE
      WRITE(20,'(A)')
     &    ' 0 setgray  .31 setlinewidth'             !- linewidth (was .3)
      CALL WRITE_PS_MACROS (20)

C---------------------- normalize the data -----------------------------
      FACT = 0.15         !- shrink factor
      R = (1.-2.*FACT)    !- resulting size

      DATAX = MAX ((XMAX - XMIN),(YMAX - YMIN)*AR)   !- scale factor
      DO I = 1,NN
        GC(1,I) =        RESX * (FACT+R*(GC(1,I)-XMIN) /DATAX)
        GC(2,I) =        RESX * (FACT+R*(GC(2,I)-YMIN) /DATAX) 
      ENDDO 

C----------------------- 0: TItles -------------------------
      WRITE (20,'(A)') '0 setgray'
      WRITE (20,'(f9.2,a)') 14., ' /Helvetica sf '
      write (text,'(a,a,a)')
     &    'QP_PS plotter, FILE=',FILE(1:LEN_TRIM(FILE))
      WRITE (20,'(3A,2G13.4,A)')
     &    ' (',TEXT(1:LEN_TRIM(TEXT)),') ', resx/2., 72./6.*4  , 'dtc'

      WRITE (20,'(f9.2,a)') 8., ' /Helvetica sf '
      WRITE (TEXT,'(A,i5,A,I5)')  
     &     'NN=',NN,' Nel=',NEL
      WRITE (20,'(3A,2G13.4,A)')  
     &    ' (',TEXT(1:LEN_TRIM(TEXT)),') ',resx/2., 72./6.*3   , 'dtc'
      WRITE (20,'(f9.2,a)') 6., ' /Helvetica sf '
      WRITE (TEXT,'(A)')  'by Dr. Kidger 21-11-95'
      WRITE (20,'(3A,2G13.4,A)')  
     &    ' (',TEXT(1:LEN_TRIM(TEXT)),') ',resx, 72./6.*3   , 'dtr'
C----------------------- plot the mesh 1: elements --------------------------
      PRINT*,'<> Plotting the element numbers'
      WRITE (20,'(f9.2,a)') 5., ' /Helvetica sf '
      DO IEL = 1,NEL
        NEN = NENS(IEL)
        xm = 0.
        ym = 0.

        DO J=1,NEN                !- assemble a polygon.
          x(j)=  GC(1,NUMS(J,IEL))
          y(j)=  GC(2,NUMS(J,IEL))
          xm = xm + x(j)
          ym = ym + y(j)
        ENDDO
        xm = xm/nen
        ym = ym/nen

        WRITE (20,'(99G13.4)') (X(j),y(j),j=1,nen)
        WRITE (20,'(i5,A)')   nen-1, ' .97 setgray fp'     !- fill

        WRITE (20,'(99G13.4)') (X(j),y(j),j=1,nen)
        WRITE (20,'(i5,A)')   nen-1, ' .0 setgray dp'      !- edge

        write (text,'(i5)')   iel
        WRITE (20,'(3A,2G13.4,A)')  
     &    ' (',TEXT(1:LEN_TRIM(TEXT)),') ',Xm,Ym   , 'dtc'
      ENDDO

C----------------------- plot the mesh 2: nodes --------------------------

      PRINT*,'<> Plotting the node numbers'
      WRITE (20,'(f9.2,a)') 4., ' /Helvetica sf '
      WRITE (20,'(2g14.4,A)')  0., resy, 'translate'
c     WRITE (20,'(A)') '.90 setgray'
      DO IEL = 1,NEL
        NEN = NENS(IEL)
        DO J=1,NEN                !- assemble a polygon.
          K = MOD(J,NEN)+1
           WRITE (20,'(99G13.4)') GC(1,NUMS(J,IEL)), GC(2,NUMS(J,IEL))
        ENDDO
        WRITE (20,'(i5,A)')   nen-1, ' .90 setgray dp'
      ENDDO

      WRITE (20,'(f9.2,a)') 5., ' /Helvetica sf '
      WRITE (20,'(A)') '0 setgray'
      DO I=1, NN
        XM = GC(1,I)
        YM = GC(2,I)
        write (text,'(i5)')   i
        WRITE (20,'(3A,2G13.4,A)')
     &    ' (',TEXT(1:LEN_TRIM(TEXT)),') ',Xm,Ym   , 'dtc'
       ENDDO

C------------------- plot the nodes ------------------------------------
c      DO I=1,NN
c        CALL FILL_ELLIPSE@ (INT(GC(1,I)),INT(GC(2,I)),1,1,12)
c      ENDDO
C-----------------------------------------------------------------------

      WRITE (20,'(a)') 'showpage'
      CLOSE (20)
      PRINT*,'<> "plotfile.ps" successfully created'
      PRINT*,' '
      PRINT*,'   Type   "gs plotfile.ps"           to preview onscreen'
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE WRITE_PS_MACROS (IO)
C
C     This simply writes out the Postscript macros as used by DR_PS ()
C      eg. from DANPLOT and DANGINO (+DANSLIP)
C
      WRITE(IO,'(A)') '%%BeginProlog'
     +,'/sc { setrgbcolor } def'
     +,'/sf { findfont exch scalefont setfont} def'
     +,'/Helv { /Helvetica sf} def'
     +,'/dl { newpath 3 1 roll moveto {lineto} repeat stroke } def'
     +,'/dp { newpath 3 1 roll moveto {lineto} repeat closepath'//
     +    ' stroke } def' 
     +,'/fp { newpath 3 1 roll moveto {lineto} repeat closepath'//
     +    ' fill } def' 
     +,'/dc { newpath 0 360 arc stroke } def'             !- but diameter ??
c    +,'/fc { newpath 0 360 arc fill } def'
     +,'/fc { newpath pop 4 2 roll 0 360 arc fill } def'

c    +,'/slen {stringwidth pop 0 exch sub 0 } def'        !- -ve text-length
     +,'/dt  { moveto show } def'                    !- text-LH
     +,'/dtc { moveto dup stringwidth pop 2 div'//   !- text-centre
     +    ' 0 exch sub 0 rmoveto show } def'
     +,'/dtr { moveto dup stringwidth pop'//         !- text-right
     +    ' 0 exch sub 0 rmoveto show } def'
     &   ,'%%EndProlog'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE DR_PS (TEXT,X,Y,N,ICOL, IOP)
C
C     This writes PostScript Output to unit 55 for lines,fill_areas etc.
C     IOP = 1 un-closed line
C           2    closed line = polygon
C           5   filled polygon  
c       (  10   filled circle  )
C          20   text string   (auto font & scale)
c          21/22 = centred/LHjust text
c..... written 29-11-93 
c      13-8-94 only do setrgbcolor if it has changed

      REAL X(*),Y(*)
      INTEGER N, ICOL,IOP, ICOL_OLD
      COMMON /PALETTE/PAL
      INTEGER PAL(3,0:255)
      CHARACTER FORMAT*50, ACTION*5, TEXT*(*) ! *80
      DATA ICOL_OLD/-1/     !- remember the 'last' colour
      DATA TXT_SIZE/-1./    !- remember the 'last' text size
 
      IF (ICOL.NE.ICOL_OLD) WRITE (55,'(3f5.2,a)')
     &           (PAL(J,ICOL)/255.,J=1,3) , ' sc'
      ICOL_OLD = ICOL


      IF (IOP.EQ. 1) THEN
         ACTION = ' dp'        !/* draw-polygon */
      ELSEIF (IOP.EQ. 2) THEN
         ACTION = ' fp'        !/* fill-polygon */
      ELSEIF (IOP.EQ. 3) THEN
         ACTION = ' dl'        !/* draw-line */
      ELSEIF (IOP.EQ.10) THEN
         ACTION = ' fc'        !/* fill-circle */
      ELSEIF (IOP.EQ.20) THEN
         ACTION = ' dt'        !/* draw-text */
      ELSEIF (IOP.EQ.21) THEN
         ACTION = ' dtc'       !/* draw-text-centered */
      ELSEIF (IOP.EQ.22) THEN
         ACTION = ' dtr'       !/* draw-text-RH-just */
      ELSE
         STOP 'unknown opcode in DR_PS'
      ENDIF
c--------------------- handle text strings -----------------------------
c.. 7-4-95 factor-down by 72. (into points) !
c.. nice to split so we define the font only when it changes (cf pencol)
      IF (IOP.ge.20.and.iop.le.29) THEN
        IF (ABS(X(2)-TXT_SIZE) .GT. 1.E-4) THEN
          TXT_SIZE = X(2)   !/72.
          WRITE (55,'(f9.2,a)') X(2), ' /Helvetica sf '
        ENDIF
        WRITE (55,'(3A,2f9.2,A)')       !-x2=font size !
c    +    ' (',TEXT(1:LENG(TEXT)),') ',X(1),Y(1), X(2)/72., ACTION
     &    ' (',TEXT(1:LEN_TRIM(TEXT)),') ',X(1),Y(1)   , ACTION

c----------------- polygons (use relative coords :-) -------------------
c.. note that MAX_DAT is the max number of tokens on a line
      ELSEIF (ACTION.eq.' dp'.or.ACTION.eq.' fp'
     &    .or.ACTION.eq.' dl') THEN
        N_LINE = 8                   !- max # per line
        N_REP = (2*N-1)/N_LINE       !- number of repeated lines
        N_REM = 2*N-N_REP*N_LINE     !- number on the last line

        IF (N_REP.NE.0) THEN
          WRITE (FORMAT,'(A,I2,A,I2,A,I2,A)')
     &     '(', N_REP,'(',N_LINE,'F9.2/),',   N_REM ,'F9.2, I4,A)'
        ELSE
          WRITE (FORMAT,'(A,I2,A)')
     &    '(',                                N_REM ,'F9.2, I4,A)'
        ENDIF
        WRITE (55,FORMAT) (X(I)-X(I+1),Y(I)-Y(I+1),I=1,N-1), X(n),Y(n)
     &         , N-1, ACTION             ! note relative coords

c----------------------- all other primitives --------------------------
c.. note that MAX_DAT is the max number of tokens on a line
      ELSE
        N_LINE = 8                   !- max # per line
        N_REP = (2*N-1)/N_LINE       !- number of repeated lines
        N_REM = 2*N-N_REP*N_LINE     !- number on the last line

        IF (N_REP.NE.0) THEN
          WRITE (FORMAT,'(A,I2,A,I2,A,I2,A)')
     &     '(', N_REP,'(',N_LINE,'F9.2/),',   N_REM ,'F9.2, I4,A)'
        ELSE
          WRITE (FORMAT,'(A,I2,A)')
     &     '(',                                N_REM ,'F9.2, I4,A)'
        ENDIF
        WRITE (55,FORMAT) (X(I),Y(I),I=1,N), N-1, ACTION
      ENDIF
      RETURN
      END
