c     options (fullcheck,undef)
c     WINAPP  10000,10000,'GRAPH.RC'    !- resource file?
      PROGRAM DANS_MESH_GENERATOR

      IMPLICIT NONE
      INTEGER MELS
      PARAMETER ( MELS =120 000 )   !-- Max # of elements (change as needed)
c     PARAMETER ( MELS =   100 )   !-- Max # of elements (change as needed)
c------------------------------------------------------------------------c
c           D A N   K I D G E R 'S   M E S H    G E N E R A T I O N      c
c------------------------------------------------------------------------c
      CHARACTER  VERSION*15
      PARAMETER (VERSION =             'Version 1.0'                   )
c------------------------------------------------------------------------c
c   ** DESCRIPTION **                                                    c
c                                                                        c
c   This programs is designed to create 2d/3d finite element meshes      c
c   from a 'Rule-based' data file using *Keywords                        c
c------------------------------------------------------------------------c
c   ** REVISION HISTORY **
c
c 26- 8-94  added *STRIP_FACETS to this basic 'shell'
c 16- 2-94  MESH8 : moved *2d *3d *PLOT and *LAYERED slope to DANLIB
c  4-10-93  added *LAYERED_SLOPE for 'Aquater' etc
c 21- 7-93  added IMPORT_SRF format .. also ** RAYSHADE ** output !
c  7- 7-93  MESH6  : added 3nt->4nq  and node sorting (?)
c 24- 5-93  MESH5  : pull-apart to keyword-handling subroutines
c  3- 4-93  MESH4B : 'circle_the_square'
c  3- 4-93  added 2D-to-3D  to *DANBLOCKS  
c  1- 4-93   >>>> added *DANBLOCKS ! <<<<
c  8-01-93 Added co-incident node deletion
c 24-01-93 built-up .. first working copy ?     
c 19-01-93 MESH1 .. first version (abstracted from GEN4.FOR)
c-----------------------------------------------------------------------
      INTEGER MNN,INF,IGC,INUMS,IFCETS,IRINGS, MDIM
      PARAMETER ( MNN = MELS     ! = 2 for 2d (=3 for 2d/3d)
c    &          ,MDIM = 3        !-- Max 2d/3d 
     &          ,MDIM = 4        !-- Max 2d/3d /4d
     &           ,IGC = MDIM     !
     &           ,INF = MDIM     ! =
     &         ,INUMS = 28       ! = max nodes per element
     &         ,IFCETS = MELS*6  ! Max # of facets
     &         ,IRINGS  = 2000 )  ! Max # of boundary edge nodes

c----------- problem sized arrays --------------------------------------
      REAL  GC  (IGC,MELS)       !- co-ordiantes of all the nodes
      INTEGER NUMS (INUMS,MNN)   !- the elements' nodes etc.
     &          ,P (IFCETS)      !- a 'workspace' of pointers 30-8-96
     &     ,FACETS (3,IFCETS)    !- table of ALL the 3D faces
     &     ,RINGS (irings)       !- polygons of the mesh boundary
     &          ,NF (INF,MNN)    !- the node freedom array (optional)

c----------------- 'small' arrays --------------------------------------
c     INTEGER  IOPS (30)      !  program control options
 
c--------------- character strings -------------------------------------
      CHARACTER KEYWORD*70   ! data input keyword 'token'
     &            ,FILE*80   ! input data file name
c     &              ,COL*7   !-- ANSI colours
c     &             ,COL1*7   !-- ANSI colours
c     &             ,COL2*7   !-- ANSI colours
c     &      ,TITLES(10)*80   ! The main titles
c     &            ,LINE*255  ! 'generic' line of text

      INTEGER U0             ! .INP : the original data file
     &       ,U1             !  --- : the current data file being read
c    &       ,U6             ! .DP2 : The Output 'keyword' mesh file
c    &       ,U7             ! .PL  : The Danplot output file
c      EXTERNAL COL
      LOGICAL FOUND, AUTOPLOT
c     CHARACTER CMNAM@*79
      REAL FREE_MEMORY
      EXTERNAL FREE_MEMORY
      INTEGER NN,NEL,NFCETS,NDIM, NODOF
     & , IKEYWORD,IPR, IOS
      REAL TIME1,TIME2
c------------------ DATA stataments ------------------------------------
      DATA    U0/40/      !,U7/10/
      DATA NEL,NN/2*0/            !-- initially NO elements etc.
      DATA NFCETS/0/              ! 1-10-97 no facets either.
      DATA NDIM/0/                !-- default is 0-dimensions !
      DATA AUTOPLOT /.FALSE./     !- don't plot every time
c      DATA (IOPS(I),I=1,10)/     !-------- default options ---------
c     + 0,   !1!    = 1  to inhibit any output for this load-step
      ipr = 99   !- 2 (or 3) =moderate feedback, 99=verbose

C =====================================================================
c     call into_windows (400,300, 20,20,' Danmesh-win')
      WRITE (*,'(A)') '-------- Dan''s Mesh Generator -------'
     +               ,'Version: '//VERSION
      FILE = ' '

c-- loop input file(s) -?
c    DO INFILE = 1, 999
c     file='c:\users\dan\danmesh\minislop.d'
c.. but how do we tell the next that this is not the first pass?
c.. ie. when do we init the command-line
      CALL GET_DATA_FILE_NAME (FILE)
      OPEN (U0,FILE=FILE,STATUS='OLD',iostat=ios) 
      IF (IOS.NE.0) STOP 'Datafile does not exist!'


C----------------- Parse the data file (s) -----------------------------
c.... get the next keyword: may skip out  at EOF or return keyword
      U1 = U0                         !-- start from the base file = U0
      DO IKEYWORD = 1, 9999
        CALL GET_KEYWORD (U1,U0,KEYWORD)
        !print*,'ikeyword=',ikeyword, ' keyword=',keyword(1:40)
        CALL PRINT_KEYWORD (IKEYWORD, KEYWORD,.true.)

c------------------------ find the '*keyword' --------------------------
      CALL GET_SECS (TIME1)
      FOUND = .TRUE.

      IF (KEYWORD.EQ.'*EOF') THEN
        close (u1)             !- always close this input file.
        if (u1.eq.u0) then     !- if back at the root
c         -- need a general CALL GET_NEXT_FILE (eg I might allow wildcards)
c         FILE = CMNAM@()                 !- the next token
          call GET_DATA_FILE_NAME(FILE)
          IF (FILE.EQ.' ') GOTO 888       !- *all done*
          OPEN (U0,FILE=FILE,STATUS='OLD',iostat=ios) 
          IF (IOS.NE.0) STOP 'Datafile does not exist!'
        ENDIF
      ELSEIF (KEYWORD.EQ.'*AUTOPLOT') THEN
        AUTOPLOT = .TRUE.
      ELSEIF (KEYWORD.EQ.'*CONTROL') THEN
c    - no need for control option in DANMESH ?


C-----------------------------------------------------------------------      
      ELSE
        FOUND = .FALSE.
      ENDIF

C--------------------- 'standard' keywords -----------------------------

      IF (.NOT.FOUND) CALL KEY_MESH_READ
     &        (FOUND,ipr,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      IF (.NOT.FOUND) CALL KEY_MESH_IO
     &        (FOUND,ipr,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      IF (.NOT.FOUND) CALL KEY_FACETS
     &        (FOUND,ipr,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
     &         FACETS,IFCETS,NFCETS, RINGS,IRINGS)
      NODOF=NDIM    !- enforce here ?
      IF (.NOT.FOUND) CALL KEY_NF
     &      (FOUND,ipr, KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &       ,NF,INF, NODOF)


c... only here if *None* of the handlers found the keyword
      IF (.NOT.FOUND) THEN
        CALL MYERROR(1,'unknown keyword:'//KEYWORD)
      ENDIF  

c---------------------------------------------------------------
C.. As a subroutine ?
c colour='red', but if=-1 then do no ansi codes?
c    CALL WR_STATUS_LINE (ANSI, NN,NEL, TIME2-TIME1) 
      IF (IPR.GE.3) THEN             ! (now same as in DANFE)   
         CALL GET_SECS (TIME2)
         WRITE(*,'(A, I7,a,I7,a, A, a,f7.3,a, A,  a,f8.2,A )')
     &    char(27)//'[41m'//
     &    '-------'
     &   , NN,' n'  ,NEL,' e' 
     &   ,'----------'
     &   ,' Mem=',FREE_MEMORY(1),'Mb'
     &   ,'----------' 
     &   ,' dt=',time2-time1
     &   ,' ----'
     &    //char(27)//'[40m'
      ENDIF

c      WRITE(*,'(A,2(A,I7),A)') '--------------------------------------'
c     & ,' Now:',NN,' Nodes and',NEL,' Elements'
        IF (AUTOPLOT) CALL PLOT_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ENDDO    !-- loop keywords/command-line data files.

C------------------------- end of program ----------------------------
  888 STOP '> > >       Program completed OK  :-)       < < <' 
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      FUNCTION COL(icode)
C
C     function to output ANSI colour control strings
C     hmm fuctions should be allowed to do I/O
C
      CHARACTER COL*7
      WRITE (COL,'(A,I2.2,A)')    CHAR(27)//'[1;',ICODE,'m'
      RETURN
      END
C-----------------------------------------------------------------------
c.... these files are now in 'DANLIB'
c     include 'k_mesh.f1'
c     include 'k_mmung.f1'

C-----------------------------------------------------------------------

