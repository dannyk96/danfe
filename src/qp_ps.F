      PROGRAM QUICKPLOT
C
C     This will do a 'quick' 2D plot of a mesh to a Postscript file.
C     Simply type "QP_PS" to run
C     Then type "COPY PLOTFILE.PS PRN" to print.
C       DJK 31-10-95
C
C
      PARAMETER (IGC=8000,INUMS = 20)
      REAL GC(2,IGC)
      INTEGER NUMS(INUMS,IGC),NENS(IGC)
      CHARACTER FILE*80       !,CMNAM@*80
C---------------------- open the data file------------------------------
! TODO 
!   - remove colorisation
!   - parse datafile name from command line (only prompt if missing)
 
c#define COLOR
#ifdef COLOR
      WRITE (*,'(A//T12,A/,T12,a//a)')
     &      CHAR(27)//'[2J',  char(27)//'[;H'
#endif
#ifdef COLOR
      WRITE (*,'(T12,A/,T12,a//a)')
     & , char(27)//'[1;33m'//'QP_PS Postscript Plotter' 
     & , char(27)//'[1;35m'//'========================' 
     & , char(27)//'[1;37m'//'What is the name of your data file?'
     & , char(27)//'[0m'
#else
      WRITE (*,'(/T12,A/,T12,a//a)')
     & , 'QP_PS Postscript Plotter' 
     & , '========================' 
     & , 'What is the name of your data file?'
#endif
      READ(*,'(A)') FILE
      OPEN (UNIT=7, FILE=FILE, STATUS='OLD',IOSTAT=IOSTAT)
      IF (IOSTAT.NE.0) THEN
        PRINT*,' *** GIven .pl file does not exist!      <EXITING>'
        STOP
      ENDIF

C------------------------ read the data --------------------------------
      READ(7,*) NODOF
      READ(7,*) NN
      DO I=1,NN
        READ(7,*) ID,(GC(J,I),J=1,2)
      ENDDO
      READ(7,*) NEL,(ID,NENS(I),(NUMS(J,I),J=1,NENS(I)),ID,I=1,NEL)

c-------------------- go into graphics mode ----------------------------
      RESX = 8.*72.     !- A4 paper size ?
      RESY = 11.*72.
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
      OPEN (20,FILE='plotfile.ps')
      WRITE(20,'(A)')  '%!PS-Adobe-2.0 EPSF-1.2'
      WRITE(20,'(4A)') '%%Document title: ',FILE
C      WRITE(20,'(A,4I6)') 
C     +  '%%BoundingBox:', res(4),res(5), res(1)+res(4),res(2)+res(5)   
      WRITE(20,'(A)') 
C    &    ' gsave',                                  !- old transformation
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

C----------------------- plot the mesh ---------------------------------
      DO IEL = 1,NEL
        NEN = NENS(IEL)
        DO J=1,NEN                !- assemble a polygon.
          K = MOD(J,NEN)+1
           WRITE (20,'(99G13.4)') GC(1,NUMS(J,IEL)), GC(2,NUMS(J,IEL))
        ENDDO
        WRITE (20,'(i5,A)')   nen-1, ' .97 setgray fp'

        DO J=1,NEN                !- assemble a polygon.
          K = MOD(J,NEN)+1
           WRITE (20,'(99G13.4)') GC(1,NUMS(J,IEL)), GC(2,NUMS(J,IEL))
        ENDDO
        WRITE (20,'(i5,A)')   nen-1, ' .0 setgray dp'
      ENDDO

C------------------- plot the nodes ------------------------------------
c      DO I=1,NN
c        CALL FILL_ELLIPSE@ (INT(GC(1,I)),INT(GC(2,I)),1,1,12)
c      ENDDO
C-----------------------------------------------------------------------
      WRITE (20,'(a)') 'showpage'
      CLOSE (20)
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


c 3-8-00 other unused routines deleted
     
