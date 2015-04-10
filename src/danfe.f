
c-----------------------------------------------------------------------
c
c           A Geotechnics Orientated Finite Element Package
c
c
c     Author :
c        Dr. D. Kidger,
c        Email: dan.kidger@quadrics.com d.kidger@man.ac.uk, dan.kidger@breathe.net
c        Web  : http://www.man.ac.uk/~mbgasdk (old)
c        Phone:  0117 956 8558
c
c-----------------------------------------------------------------------

      PROGRAM DANS_GENERAL_FE_PROGRAM

c.. 3-5-97 This is just the outer 'wrapper' to main program, hence can
c   run in a parallel context (using MPI)

c  18-9-99 new concepts: pass argv,argc to DANFE_MAIN
c           argv contains data file(s), and options: simply loop and
c      handle in order, then return. (cf file=FILE)
c      - slaves get no file ?
c      - slaves wait here until a master requests them - then they enter
C        DANFE_MAIN (maybe with '-s' to flag slave status?)
c      - 2ø DANFE_MAINs have PE/=0 but read their own files

      include 'mpif.h'

      parameter (maxprocs=512)
      common /MPP/iproc,nproc,status,proc_ma
      INTEGER IPROC,NPROC, status(20)

      integer proc_map(0:maxprocs)       ! not yet impliemnted
      character command_line*255         ! not yet impliemnted (n/a?)

c-- 4-11-99 conecpt of a processor-map --
c
c   have an array 'proc_map'
c     each box stores who this PE's parent is:
c      -1 is no parent (the supermaster?)
c       0 =parent is the root (ie. the usual case)
c
c since I know my id - I hence know who my paraent is.
c  I can abstarct this to a put_/get_ pair hence can be called from anywhere
c
c tasks:
c 1; gimme 5 processors
c      picks 5 vacent PEs (see later for stealing technology)
c      sets their tags to 'me' as their parent
c      tells them to become mine.
c 2: who is my parent? - simple
c 3: how many children do I have:wq

c------ 0: Initialise -----
      do i=1, maxprocs
        proc_map(i) = -1    !- all have no parent
      enddo

c---------- 1: all processes find their id number -------------
      CALL START_MPP (IPROC,NPROC)
c     if (nproc=-1) stop 'MPI init failed'

      IF (NPROC.gt.1) THEN
        print*,'Running in parallel on ',NPROC,' processors'
      ELSE
        print*,'Running is serial on one processor'
      ENDIF

c---------- 2: first PE takes the command-line  --------------
c   PE 0 always becomes a master.
c   It reads the real command-line data file
c   *SPAWN may quote a string which is the command-line to give
c   else *SPAWN simply asks for some of the master's slaves
c
      IF (IPROC.eq.0) THEN      !-- I am the 'MASTER'
c       PRINT*,'Starting DANFE Master...'
        iparent = -1   !- I have no parent (cf. slaves)
        proc_map(iproc)=iparent

c       pick up the command-line here
        command_line=' '
c       call get_command_line(command_line)

        CALL DANFE_MAIN (iproc,nproc,command_line,iparent)

c-------------------- 3: Limbo --------------------------
c  Limbo is where remaining processes wait until they requested.
c

      ELSE                      !-- I am a 'SLAVE'

        print*,'slave', iproc,' is entering limbo'
c - wait for a message :
c  123 call is_a_message waiting (imessage,MPI_ANY)
c      if (no_message) then
c         call idle_process (for say (500 milliseconds)
c         goto 123
c      endif

c commented out 21-8-00
       message=0    !- dummy value
c      call mpi_recv (iparent,1,MPI_INTEGER,MPI_ANY_SOURCE,MPI_ANY_TAG
c    & ,MPI_COMM_WORLD,status, ifail)
c       message=status(MPI_TAG)

c      if (ifail.ne.0)
c    & print*,'iparent=',iparent,' message=',message,'ifail=',ifail

c---- become a master with a data file defined in the main data file
      if (message.eq.1) then
        command_line=' '
        call danfe_main(iproc,nproc,command_line,iparent)

c---- become a slave (of danfe)
       elseif (message.eq.2) then
c        call danfe_main(iproc,nproc,' ',iparent)
c - now simply exit ?

c---- run another program (like danfront say)
c      elseif (message.eq.81) then
c       call danfront_main (command_line)
c---- run another slave (like danfront say)
c      elseif (message.eq.82) then
c       call danfront_main (' ')
c
c
c---- 'Silly' things: useful for debugging
       elseif (message.eq.7001) then
         print*,'The rain in Spain, falls mainly on the plain'
c.. next simply eats some CPU time.
       elseif (message.eq.7002) then
c        call perfect_number (icount)

c---- exit the whole package
       elseif (message.eq.-1) then


       else    !- unknown message
c
       endif

c- we get here when a task has finished.
c- now simply return to limbo state until we are called up again.

c------ end of a running


c       PRINT*,'Starting Slave #',IPROC,' ....'
c       CALL DANFE_SLAVE ()

      ENDIF
      print*, 'end of DANFE (spawned)'
      CALL END_MPP ()  !(IPROC)
      END !PROGRAM DANS_GENERAL_FE_PROGRAM

c-----------------------------------------------------------------------
      SUBROUTINE DANFE_MAIN (iproc,nproc,command_line,iparent)

      CHARACTER  VERSION*20
      PARAMETER (VERSION='Version 1.3.00')

c   Notes on Allocatable arrays...
c    large arrays come in three classes:
c      1: mesh sized like GC, NUMS   (may change)
c      1b: derived from NN, NEL, eg. G_DISPS, STRESS
c      2: 'workspaces' like KV, KDIAG
c      3: 'workspaces within modules eg. CUTHILL_MCGEE
c   Type 1 should be allocated a 'nominal' size, and increased when overflows
c     NUMS may increase in width too - eg for 4nqs->32nbs
c   maybe allow a '*REGION' statement to get us started.
c   Type 2 - we can allocate once we have calculated their size
c   Type 3 - are 'automatic' arrays - some F77 compilers will do this.
c
      character command_line*255
      INTEGER MNN,MEL,IKV, IKDIAG

c stiffness matrix
c     COMMON/BIG_ARRAYS/KV
c elements
c    &   ,STRESSES,EVPT, PORE
c nodal
c    &   ,GC,DISPS_INC,DISPS_TOT, FORCES,FORCES_TOT
c freedoms (=nodof*#nodes)
c    &   ,LOADS,DISPS,MB,P
c integer
c    &   ,NUMS, KDIAG ,FACETS,RINGS, GROUPS, PSTR

      PARAMETER (
c--- smaller 'test' set ----
c    &       MNN  =5000        !-- Max. # of nodes
c    &      ,MEL  =2000        !-- Max. # of elements
c    &      ,IKV  =  500 000   !-- Max. size of the stiffness matrix
c--- larger 'production' set.
c    &       MNN  =60 000     !-- Max. # of nodes
c    &      ,MEL  =25 000     !-- Max. # of elements
     &       MNN  =2 000 000     !-- Max. # of nodes
     &      ,MEL  = 80 000     !-- Max. # of elements
     &      ,IKV  = 9 000 000    ! for hop3c.d
c    &      ,IKV  =18 000 000   !--  PCG for Lamaload
c    &      ,IKV  =82 000 000   !-- SPARIN for Lamaload
c    &      ,IKV  =218 000 000   !-- BANRED for Lamaload (after sorting nodes)
c    &      ,IKV  =248 000 000   !-- BANRED for Lamaload (after sorting nodes)
c    &      ,IKV  =165 000 000    !-- SPARIN for BNFL B38
c  B38 BNFL mesh= 161892273.
c ( these become n/a for allocatable arrays)
c---
     &           )
c     COMMON/BIG_ARRAYS/KV
      PARAMETER (
     &       IKDIAG =MEL*61  !-  KV pointers (max = NN*NODOF for SPARIN)
     &      ,MNOD = 32       !-- Max. # of nodes per element
     &      ,MDIM = 3     )  !-- Max 2d/3d (4d?)
C----------------------------------------------------------------------
C                                                                     !
C     REVISION HISTORY                                                !
C     ----------------                                                !
C  May 1991  This is a re-hash of my code for FOS of 3d slopes        !
C                                                                     !
C  17-10-91  PARMETER's shuffled, FOS looping added, MUNG extended    !
C  26- 1-93  GEN5 .. complete rework with keywords etc.               !
C  16- 2-93  Gen5a.. back to work :-)                                 !
C  11- 3-93  Major work: BUILD_KV to its own subroutine               !
C  25- 3-93  GEN5d : first 'working' copy                             !
C  26- 3-93  GEN5f : plasticity to a subroutine : PCG support         !
C   1- 4-93  GEN5g : Von Mise if IOP*4 .eq. 2 (M-C is =1)             !
C   7- 4-93  GEN6  : Built-up/ Excavation support :-)                 !
C   8- 4-93  FORCES_TOT = total applied loads (for equilib ?)         !
C  10- 9-93  v7a patched in the mesh-generation library routines      !
C            ,moved libraries out,  added timing options              !
C  14- 9-93  v7b : LOADS now (0:n) .. to speed CG solver              !
C  14- 9-93  memory info.. & more array size checks                   !
C  21- 9-93  added K_MMUNG so P() too.                                !
C  18- 2-94  now DANFE1.FOR : added *RADIAL_DISPS (eg for bore-holes  !
C   3- 3-94  moved LOADS(0)=0. from DATA statement to explicit !      !
C   9- 6-94  added *FORM_MM and *SOLVE_DYNAMIC for Eigenvalue problems!
C  20- 6-94  added *KORM_KP for groundwater flow-nets
C   4- 4-95  v9.1 re-worked, main code within *ANALYSE, IDBG unified
C            LEGO CG solver,
C  18- 4-95  v9.2 now read *DISPLACEMENTS (eg. for restarts (stresses?)
C              *TITLES amd *LOAD_STEP_TITLE added, .KEY now called .OUT
C  15- 8-95  SRI added (for FORM_KM) as IOPS(13) for the K bit
C            PCG storage re-worked, upper-tri only option added
C            Fixed so PCG works for disp-loading.
C            LEGO generalised .. (+=IOP_1EL) .. need auto-finders tho!
C
C  19-12-95  STRESSES database added (from DANPLOT)
C            *OUTPUT to generalise *DEBUG options
C  17- 7-96  work on BIOT; KM_CODE for a generalised FORM_KM_GLOBAL
c  19- 2-97  Added #TOTAL_WARNING print to end of job.
C  10- 3-97  fixed glitch where EXPT was NULLed even if 'no plasticity'
C  17- 5-97  first Work on MPI version.
c  30- 5-97  work on BIOT: *TIME_STEPS (cf *LOAD_STEPS)
c  13- 8-97  additional comments for BIOT
c   6- 4-98  now a MPI_NUL.F  pseudo library (+ CALC_NODOF for BIOT)
C  17- 6-98  added proto-f90 REAL,ALLOCATBLE lines
c  16- 3-99  added PORE(:,NEL) for undrained excavations
c   8- 7-99  added *SCALE_DISPLACEMENTS and *ADD_DISPLACEMENTS
c  10- 3-01  added BANDRD Eigensolver (for Lamaload)

C----------------------------------------------------------------------
C     Max. element size parameters        Max. problem size params
C     ----------------------------        -------------------------
C        MN   = nodes per elem.
C        MDOF = Dof. per mode             MEL = elements in mesh
C        MDF  = freedoms per elem         IKV = global stiffness matrix
C
C        NS   = strains per GP           (ILDS  = loaded nodes per mesh)
C        IGRULE = GP's per elem           IQINC = load steps
C                                         IPRPTAB = material types
C-----------------------------------------------------------------------

       PARAMETER (
     &        MQINC  = 50              !-- Max. steps per loading
     &      ,IPRPTAB = 30              !-- Max. # of props per material (was 20)
     &        ,MMATS = 99              !-- max # of materials
     &          ,MDF = MDIM            !-- max Dimensionality
     &       ,IGRULE = 16              !-- max # of GP's per element
c    &       ,IGRULE = 8               !-- max # of GP's per element
     &       ,ILOADS = MNN*MDF   !-- max # of freedoms
     &                               )
C--------------------- 'dependant' array sizes -------------------------
c note MS maybe be +=1 if we have pore pressures.
      PARAMETER (
     &                NS = MDF*2       !-- stress-terms per GP (but =10 in 4D)
     &             ,MSEL = IGRULE*NS   !-- stress-terms per elem
     &          ,MSTRESS = MEL*(2+MSEL) !-- stress-term-database
     &  )
c------------------- dummy sizes for subroutine calls ------------------
      PARAMETER (
     &               IGC = MDIM        !-- width of GC (3 ?)
     &              ,INF = MDF         !-- node-freedom array (usu =IGC)
     &            ,INUMS = MNOD + 7    !-- width of NUMS (+3code+imat+3user)
     &          ,IGROUPS = MEL         !-* flags for each 'element group #'
     &           ,IEVPT2 = MSEL        !-- size of EVPT
     &          ,IEVSTRS2= MSEL        !-- size of EVSTRS (combine ?)
     &            ,IPORE = IGRULE
     &           ,IFCETS = MEL*6       !-- Max # of facets
     &          ,IRINGS  = 200000 )      !-- Max # of boundary edge nodes
C-----------------------------------------------------------------------

C-------------------- problem size depandant arrays --------------------
C#ifdef F90 then
      REAL, ALLOCATABLE :: KV(:)      !(try just the biggest array ?)
C     REAL, ALLOCATABLE :: &
C     REAL       KV (:)        &  !- The Global Stiffness Matrix
C    &          ,MB (:)        &  !- the lumped mass matrix
C    &          ,GC (:,:)      &  !- the coordinates of all the nodes
C    &    ,STRESSES (:)        &  !- * Gauss-point stresses *
C    &        ,EVPT (:)        &  !- all the strains (viscoplastic)
c     &       ,LOADS (:ILOADS) &  !- the applied loads  (at freedoms)
c     &       ,DISPS (:ILOADS) &  !- disps of this load step -- by freedom
c     &   ,DISPS_OLD (:ILOADS) &  !- disps from the last iteration (CHECON3)
c     &   ,DISPS_INC (:,:)     &  !- disps of this inc -- by node
c     &   ,DISPS_TOT (:,:)     &  !- the cumulative displacements
c     &    ,FORCES_N (:,:)     &  !- applied nodal loads
c     &  ,FORCES_TOT (:,:)     &  !- total applied nodal loads
c     &    ,BDYLDS_N (:,:)     &  !- the iterated excess forces
c     &     ,REACT_N (:,:)     &  !- resulting reaction forces (cf BDYLDS_N)
c     &      ,FACETS (:,:)     &  !- table of ALL the 3D faces
c     &        RINGS (:)          !- polygons of the mesh boundary
c      INTEGER                &
c     &         NUMS (:,:)    &   !- the nodes attached to every element
c     &          ,NF (:,:)    &   !- the node freedom array
c     &        ,PSTR (:)      &   !- pointer to each element's stress record
c     &       ,KDIAG (:)      &   !- pointers to each row of KV
c     &           ,P (:)      &   !- temporary node pointers (K_MMUNG)
c     &      ,GROUPS (:)          !- flag for every *different* KM group
c#else
      REAL
c    &           KV (IKV)         !- The Global Stiffness Matrix
     &           MB (ILOADS)      !- the lumped mass matrix
     &          ,GC (IGC,MNN)     !- the coordinates of all the nodes
     &    ,STRESSES (MSTRESS)     !- * Gauss-point stresses *
     &        ,EVPT (MSTRESS)     !- all the strains (viscoplastic)
     &        ,PORE (IPORE,MEL)   !- pore pressures for all gauss points

      REAL
     &        LOADS (0:ILOADS)    !-- the applied loads  (at freedoms)
     &       ,DISPS (0:ILOADS)    !-- disps of this load step -- by freedom
     &   ,DISPS_OLD (0:ILOADS)    !-- disps from the last iteration (CHECON3)
     &   ,DISPS_INC (MDF,MNN)     !-- disps of this inc -- by node
     &   ,DISPS_TOT (MDF,MNN)     !-- the cumulative displacements
     &    ,FORCES_N (MDF,MNN)     !-- applied nodal loads
     &  ,FORCES_TOT (MDF,MNN)     !-- total applied nodal loads
     &    ,BDYLDS_N (MDF,MNN)     !-- the iterated excess forces
c    &     ,REACT_N (MDF,MNN)     !-- resulting reaction forces (cf BDYLDS_N)

c-- data structures for the outer surfaces
c  perhaps embed these in a module ?
c   USE FACETS
      INTEGER   FACETS (3,IFCETS)    !- table of ALL the 3D faces
      INTEGER RINGS (irings)      !- polygons of the mesh boundary

      INTEGER
     &         NUMS (INUMS,MEL)   !-- the nodes attached to every element
     &          ,NF (INF,MNN)     !-- the node freedom array
     &        ,PSTR (MEL)         !-- pointer to each element's stress record
     &  ,NODE_ORDER (MNN)         !-- bandwidth optimiser, deafult is just 1:NN
     &       ,KDIAG (IKDIAG)      !-- pointers to each row of KV
     &           ,P (IFCETS)         !-- temporary node pointers (K_MMUNG)
     &      ,GROUPS (0:IGROUPS)   !-- flag for every *different* KM group
c#endif

c obsolete arrays
c    REAL   EVECTS (ievects,ievects)  !-- the calculated eigenmodes (size ??)
C----------------------- smaller FE arrays --------------------------
c.. here we have the storage of the number of load-steps etc.
      INTEGER   Q_NUMBER (MQINC)  !- number of loadsteps of this amount
      REAL PRPTAB (IPRPTAB,MMATS) !- the material properties
     & ,Q_PERCENT (MQINC)         !- the amount of each load-step
     & ,Q_DTIME (MQINC)           !- the length of each time step
      real scale(MDF)             !- generic x,y,z - for scaling displacements
      integer num(MNOD)           !- for the ruler line

C---------------------------------------------------------------------
      INTEGER  U0                 !- 'root' data file
     &        ,U1                 !- 'current' data file
     &        ,U2                 !-  (.RES Misc output file - unused)
     &        ,U3                 !-  Output file(s) : main .out  (keyword based)
!    &        ,u3d,u3s,u3e        !-  Output files: disps, stress,strains
     &        ,IOPS(30)           !- job-control options
     &        ,IDBG(10)           !- codes for 'debug' outputs
     &   ,Iters_cgs(50)           !- PCG iteration counts
     &   ,DRPTVAC(9)              !- list of freedom types (cf BIOT)
C---------------------------------------------------------------------
      CHARACTER   FILE*128        !-- the data file name
     &       ,file_root*128       !-- the root part of the files.
c    &        ,file_out*128       !-- an output file
     &         ,KEYWORD*60        !-- the keyword token
     &      ,KEYWORD_LAST*60      !-- the previous keyword
     &      ,TITLES(10)*79        !-- The main titles
     &        ,TITLE_LS*79        !-- A load step title
     &         ,KM_CODE*4         !-- 'KM', 'KP','KPI' or 'KE'
     &         ,ELCODE*8          !-- '4nq' etc.
!---------------------------------------------------------------------
! variables for parsing the command line
      CHARACTER :: filenames*1024=' ',    !- stack of input filenames
     &                  FILENAME*60 =' '  !- input datfile name
!    &                  basename*60       !- base for output filenames
      integer :: iverb=2           !  verbosity  so can increment or decrement from here.
     &          ,igraph=0          !- 0=no graphics, 1= ..
      character :: arg*80, arg1*1, arg2*1
      integer :: nargs   
!     integer :: danfe_argc           ! depricated
      logical :: debug = .false.,     !- cf. iverb (maybe cpp this?)
     &           colorise=.true.       !- toggle if use ANSI colours


      LOGICAL CONVERGED           !-- convergence flag
     &        ,FOUND              !-- valid keyword flag

      REAL TIME(20)               !-- Program run-timings
      CHARACTER DATE_STAMP*20     !-- current date function
      EXTERNAL  DATE_STAMP

      DATA U0,u2,U3/ 40, 78,80/   !- file numbers

!-----------------------------------------------------------------------
! notes on verbosity:
!  - we have general verbositiy, 'debug' output and 'performance' output
!    default should be:
!       - keyword name and one or two lines of output for each
!       - no debug 
!       verb+=1  addes the 'purple' line and more output for each module
!      performance appears in thinks like the PCG solver 
!         default is probably just total run time?
!         +=1 for PCG totals
!         +=1 for per loop PCG etc.
!       note here the colour flag -C v. -b
!       default I guess should be colour 
!
c   0=silent
c   1=keywords themselves only, possibly comma seperated ?
c   2=keyword effect (1-5 lines max)              (default)
c   3=ruler-line between keywords. (+ timings)

c   5=progress (bar-line)   or PCG status , etc.
c     cf reporting every 'one second wallclock'
c     or disabling completely if 'batch-mode'

c   9=verbose
c
      DATA (IDBG(I),I=1,10) /

     &   2,  ! (1) KEYWORDS: 1=echo, 2=show effect (usu =1-5 lines, eg BCs)
c                            3= --show NEL,NN every step etc. too.--
     &   1,  ! (2) TIMINGS : 1=job summary, 2=for every keyword
c                            3=for every iteration (CG solver, etc.)
     &   0,  ! (3) AUTOPLOT: 1=the mesh (deformed) after each step (cf GUI)
c                            2= contours too etc.. :-)
     &   0,  ! (4)
     &   0,  ! (5)
     &   0,  ! (6) COUNTERS : 2 = Elements in FORM_KM, PLASTICITY, etc. :-(

     &   1,  ! (7) ? LOADSTEP : 1 = show sum-loads, mean-disp
C                              * The sum-loads, avg-disp table every loadstep
c                              also end of *ANALYSE tables ..sigma-max, etc.?
c                              (=1 for just a counter?)
     &   0,  ! (8) PLATIC_ITERATION:  1=Show max error, 2=..?
     &   0,  ! (9) CG_ITERATIONS   :  1=total, 2= max error every iteration
     &   0/  !(10)

C-----------------------------------------------------------------------
      DATA (IOPS(I),I=1,10) /
     &   0,  ! (1)      how often we open a new results file (22-2-98)
     &   1,  ! (2)       0=elastic only, 1=plasticity (M_C) ..<-1=no stresses?>
     &   0,  ! (3)       0 = SPARIN, 1=CG, 2= Full NxN etc.   (auto ??)
     &1000,  ! (4)       Max Iterations for convergence
     &   4,  ! (5)       Convergence tolerance = 1/10**val (was only 3!)
     &   2,  ! (6)  (obs) NGP code  NODOF**val  (0= off) - KM or PLASTIC ?
     &   1,  ! (7)  (obs) *  1= Mohr-Coulomb  ; 2= Von Mise , etc. (mat prop ?)
     &   1,  ! (8)  (internal) *  1= loads are 'forces'; 2: loads are 'displacements'
     &   0,  ! (9)  (obs) if = 1 then hack to only generate the first elem KM
c                        (better as a hack of the LEGO method .. done!)
     &  -1/  !(10)       order of GP rule to use (-1 will force an 'auto' choice)

c.... also default # load-steps per load-case (*LOAD_STEPS)
c....      if a built-up model ?
c....      default Gauss-point rule (4 in 2d, 14? in 3D) as FI/RI/user-defined

      DATA (IOPS(I),I=11,20) /
     &   0,  ! (11)    *  !- =1 if LEGO 'KM structure' is activated
     &   1,  ! (12)       !- Preconditioner: 0=none, 1=diagonal..
     &   0,  ! (13)       !  SRI flags ? .. if /=0 use 10 for G, 13 for K
     &   1,  ! (14)    *  !/ Coordinate set: 1=cart., 2=axisym/polar,(3=spher)
     &   0,  ! (15)    *  !/ flag for plane-stress/ ?
     &   0,  ! (16)       !- 0 = lumped mass (Hinton), 1=simplex, (2=full)
     &   1,  ! (17)       !- global on/off switch for stress output
C                            (disps too), need option to same/diff .OUT files
     &   1,  ! (18)       !- global on/off switch for strain output
     &   6,  ! (19)       !/ max no of eigenvectors, 0=all, -ve = rev order
     &   1/  ! (20)       !/ =1 to automatically factor KV after FORM_KM (cf eigen)

      DATA NN/0/, NEL/0/, N/0/             !- initially no elements
      DATA NDIM,NODOF/2,2/                 !- default=2d analysis
      DATA IBIOT/0/                        !- =1 if BIOT
      DATA FOS_C/1./, FOS_PHI/1./          !- use the full c/phi
      DATA  Q_NUMBER (1)/1/,IQ_TOT/1/      !- just one sub-step of..
     &    , Q_PERCENT(1)/100./             !- 100% of the load
     &    , Q_DTIME(1)  /1./               !- applied in 1 second.
      DATA BIG_SPRING / 1.E30/             !- arbitary 'Big spring'
      DATA TIME/20*0./                     !- null the timings

c----------------------- 0: Initialisation ----------------------------
      LOADS(0) = 0.             !------- null the bit-buckets ---------
      DISPS(0) = 0.
      DISPS_OLD(0) = 0.

      NMATS = 0             !-- 11-3-98 this was missing! --

c      N_TITLES = 2
c      TITLES(1) ='         -- DANFE analysis module --'
c      TITLES(2) ='         --- No Title specified ----'
       N_TITLES = 0
       TITLES(1) =' '
c      TITLES(2) ='         --- No Title specified ----'

      DO J=1,MMATS             !- null all underdefined material properties
        DO I=1,IPRPTAB         !- (eg gravity, soil strengths, ?)
          PRPTAB (I,J) = 0.       ! so use 0 to flag mat=elastic only
        ENDDO
      ENDDO

C----------------------- 1: write titles -------------------------------
c... Write the boxed header to both screen and .OUT file ?
c but only if I am the MASTER process.
      CALL GET_SECS(TIME(1))               !- start time
c     CALL START_CLOCK ('cbig','go')
c     CALL READ_CLOCK ('cbig','read')
c     CALL KILL_CLOCK ('cbig')
c     CALL SET_PAGES_RESERVE@(5)           !- try this out ?

! hmm we havent parsed the command line for a no-color option yet
!  cf. using an environement variable (better I guess)
! Note here also the use of environment variables to set the size of KV
!      CALL WRITE_DANFE_BANNER (version,colorise)
      CALL WRITE_DANFE_BANNER (version,.false.)
      PRINT*, 'Job started: ',DATE_STAMP(),
     & MNN,' Nodes,',MEL,' Elements, IKV=',IKV

c------------ 2: get the input file name and open output files ---------
c ie. parse 'command-line'; put datafiles onto a stack, handle -flags
c maybe expand wildcards.
c.. hmm careful with *WRITE_DANPLOT
c.. as it may already have been opened here :-(
c.. at *EOF loop back for more data files ?
c.. also command-line switches such as -v for verbose (-q = quiet, etc.)
C     *autoplot ?
c.. genericaly:
c   1: get #args
c   2: loop args, handle any that are options, (skip datafilenames)
c       eg -?, -nographics, etc.
c   2a: if #files=0, then try file=FILE,
C   2b: if no luck output 'no datafile found' error and exit.
c   3: loop args (those which are filenames):
c   4:     get file name - first gives the root file name
c   5:     read from each until *EOF
c   6: report how many files we used

       nargs= iargc()     !- for IRIX etc.

!--------------- parse the command line ------------------
!     !nargs= danfe_argc()
      print*,'#args=',nargs
      iskip=0
      if (nargs==0) then
        call usage()
        stop
      endif
      do i=1,nargs
        if (iskip>=1) then
           iskip=iskip-1
           cycle
        endif
        call danfe_getarg(i,arg)
        arg1 = arg(1:1)
        arg2 = arg(2:2)
        if (arg1 /= '-') then       !- not a prefixed option
!  hmm perhaps instead check that no options *follow* the first filename found
!          if (i /=nargs) then
!            call error ("filename should be the last option")
!            call usage (); stop
!          else
             if (filenames/=' ') arg=','//arg(1:len_trim(arg))
             filenames=filenames(1:len_trim(filenames))
     &       //arg(1:len_trim(arg))
             nfilenames = nfilenames+1
!            print*,nfilenames,filenames
!          endif
        else                       !----- parse options begining with '-' or '--' -----
          if (arg2=='-') then           !--- '--' =Gnu style options----
            if (arg=='--usage') then
              call usage() ; stop
            elseif (arg=='--help') then
               call help(); stop
            else
            call error('unknown argument:'//arg); stop
            endif
          else                         !-- single '-'
!           now loop if we have say '-vvv'
            do j=2,len_trim(arg)
              arg2=arg(j:j)
              if (arg2 ==' ') then
                call error ('option missing')   !(or = stdin ?)
              elseif (arg2=='h') then
                call help(); stop
              elseif (arg2=='v') then    !---- verbosity level ----
                 iverb = iverb+1
              elseif (arg2=='s') then      !silent
                 iverb = 0
              elseif (arg2=='g') then    !---- graphics option
                 igraph = igraph+1          !or read option?

              elseif (arg2=='b') then    !---- toggle colourisation
                 colorise = .not.colorise
              elseif (arg2=='t') then      !- output file type (default is no file!)
                 if (i==nargs) then
                   call usage(); stop
                 endif
                 call danfe_getarg(i+1,arg); iskip=1
!                outfile_type=arg(1:len(outfile_type))
!                select (arg)                 !-  check it is valid type
!                case ('pl'.'dp',..)          !-


              else
                call error('unknown option:"'//arg2//'"'); stop
              endif
            enddo
          endif
        endif
        if (debug) write(*,'(i3," :",a)') i,arg
      enddo
      if (filenames ==" ") then
        call usage (); stop
      endif
      ipr=iverb          ! synonym

!-----------------------------------------------------------------------------
!    Loop input files
!-----------------------------------------------------------------------------

!do we reset everything here or carry on with current mesh,disps etc.?
      files: do ifiles = 1, huge(ifiles)
!       print*,'filenames=', filenames, len_trim(filenames)
        if (filenames==' ') exit files     !- all done
        ipos = index(filenames,',')
!          note be careful if ipos is very large (>len(filename))
        if (ipos>=2) then
          filename  = filenames(:ipos-1)
          filenames = filenames(ipos+1:)
        else
          filename = filenames(1:len(filename))
          filenames=' '
        endif
       if (iverb >=1) 
     &print*,"Processing '",filename(1:len_trim(filename)),"'"
      file=filename
      if (file.eq.' ') then
        print*,'Usage :'
        print*,' DANFE [options] data_file(s)'
        goto 888
c       stop         !- really GOTO 888 (cf MPI)
      endif

      WRITE(*,'(a,a)') '<> Data file name=',FILE

      OPEN (U0,FILE=FILE,STATUS='OLD',iostat=ios)
      if (ios.ne.0) then
        call myerror(2,'Data file not found')
      endif


! get the 'base name' to append outputfile extensions to.
! I guess we only do this if this is the *first* datafile ?
!     call to_lower(FILE)        !- lower case is easier to handle(?)
      FILE_ROOT=FILE(1:INDEX(FILE,'.d')-1)  ! fixme as .d extension is not obligatory

c next is better as getting the 'basename'
c best for outputfiles to go in the cwd
c  but use teh source directory location for #include files
!     OPEN (U2,FILE=FILE_ROOT(1:istr_end(file_root))//'.tab')  !-- Log (opt?)
      OPEN (U3,FILE=FILE_ROOT(1:istr_end(file_root))//'.out')  !-- Danplot
!     u3d=u3       !- output stream for DISPS (current)
!     u3s=u3       !   "       "        Stresses
!     u3e=u3       !   "       "        Strains

c.. hmm write this to all output files as we create them?
        WRITE(U3,'(A,78(''-'')/, A/A/A/, A,78(''-'')/ )')
     &   '#'
     &  ,'#               DANFE Finite Element Analysis Program '
     &                         //VERSION
     &  ,'#'
     &  ,'# Job started at: '//DATE_STAMP()
     &  ,'#'
c.. other info here too? - eg 'part 4' ?
c----------- acquire (spawn) the child processes ------------
c
c     nchild=nproc-0
      ichild1=1
      ichildn=nproc-1
c      print*,'setting up',ichildn,' children'
      DO I= ichild1,ichildn
        print*,'sending to PE#',I
c send message 'start danfe slave'
        icode= 2  !-'start a slave'
        call mpi_send (iproc,1,MPI_INTEGER,I,icode,MPI_COMM_WORLD,ifail)
        if (ifail.ne.0)
     &  print*,'spawn sent to PE#',I, ' ifail=',ifail
        print*,'spawn sent to PE#',I, ' ifail=',ifail
      ENDDO

C---------------- loop back point for multiple load-steps --------------
      U1 = U0                   !-- start from the base file = U0
      N_CASES  = 0              !-- No. of load cases so far

C----------------- Begining of a load-step -----------------------------
      allocate (kv(ikv))
c 1221 CONTINUE
c      TITLE_LS = ' --- No title was given --- '
       TITLE_LS = ' '
c      DO I=1,MPROPS
c        DO J=30,49            !-- Null the element loadings
c          PROP(J,I) = 0.      ! 30->49 = incremental
c        ENDDO                 ! 50->69 = cummulative (ie. total pressure etc.)
c      ENDDO
c     CALL NULL2D (FORCES_N,  INF,NODOF,NN)   !- applied nodal forces

c.. or loop to a given maximum - a demo may limit this to say 20
      KEYWORD=' '
      DO IKEYWORD = 1, 9999
c        CALL YIELD@(.FALSE.)             !- for multi-threading
         CALL GET_SECS (TIME(11))
c        CALL MYCLOCK ('keyword','set')

C----------------- Parse the data file (s) -----------------------------
c Note we could also be picking up keywords from a windows interface.
c hmm the next could also return keyword arguments too?
c eg. if line terminates with a comma then pick up the next line too
c or strip on the next line ?
      KEYWORD_LAST=KEYWORD
      CALL GET_KEYWORD (U1,U0,KEYWORD)
      IF (iverb.GE.1) CALL PRINT_KEYWORD (IKEYWORD,KEYWORD,colorise)

c  maybe also can write to a 'journal' file for replay ?
c  - need to echo the numbers read-in too.
C-----------------------------------------------------------------------
      FOUND = .TRUE.           !- assume OK so ELSEIF to .false.
      IF (KEYWORD.EQ.'*EOF') THEN        !-- end of file .. so exit
          GOTO 888             ! ok lets EXIT the parse-file loop
      ENDIF

C------- Action to do when we get a new keyword --------
c.. 1: open new output file(s)
      IF (KEYWORD_LAST.EQ.'*ANALYSE'.OR.
     &    KEYWORD_LAST.EQ.'*NULL_ARRAYS') THEN
        if (iops(1).ne.0) then
          call next_outfiles (iops(1), n_cases, u3,file_root)
        ENDIF
      ENDIF

c-------------------------------------------------------
c.. OK lets make *OUTPUT, hence AUTOPLOT=1, VERBOSE, QUIT, etc.
      IF (KEYWORD.EQ.'*AUTOPLOT') THEN   !- direct IDBG is better?
        IDBG(3) = 1                             ! codes for DISPS, etc?
      ELSEIF (KEYWORD.EQ.'*QUIET') THEN      !- zap all runtime output
        DO I=1,10
          IDBG(I) = 0
        ENDDO
      ELSEIF (KEYWORD.EQ.'*VERBOSE') THEN      !- maximum output
c        take an optional argument for the level?
        level=99
        DO I=1,10
          IDBG(I) = level
        ENDDO
        IDBG(3) = 0    !- but don't do plotting !

c------------------------- control options -----------------------------
c.. hmm better to rename *DEBUG as *OUTPUT, then use token=values
      ELSEIF (KEYWORD.EQ.'*CONTROLS') THEN   !- program control options
        CALL R_CONTROL_TOKEN (U1, IOPS,20, idbg(1))
      ELSEIF (KEYWORD.EQ.'*CONTROL') THEN   !- program control options
        CALL R_OPTIONS(U1,IOPS)
c     ELSEIF (KEYWORD.EQ.'*REGION') THEN    !- memory sizes :-(
c       READ(U1,*) MNN, MEL, IKV            ! (only KV really matters)
c       allocate GC(IGC,MNN)
c       allocate NUMS(INUMS,MEL)
c       allocate KV(IKV)                    ! * * *

      ELSEIF (KEYWORD.EQ.'*DEBUG') THEN     !- various print-outs
        CALL R_OPTIONS(U1,IDBG)
c      ELSEIF (KEYWORD.EQ.'*OUTPUT') THEN    !- various print-outs
c        CALL R_CONTROL_TOKEN (U1,IDBG,10, idbg(1))

c--------------------------- titles, etc. ------------------------------
c.. as a subroutine?
      ELSEIF (KEYWORD.EQ.'*JOB_TITLES') THEN
        DO I=1,10
          READ (U1,*,iostat=ios) TITLES(I)
          IF(TITLES(I)(1:1) .ne." ") GOTO 9123
          IF (ios.ne.0) GOTO 9123
        ENDDO
 9123   IF(I.NE.11) BACKSPACE (U1)   !-- point back to the next *KEYWORD
        N_TITLES = I-1
        if (colorise) then
          WRITE(*,'(A)') (char(27)//'[1;33m'//
     &    TITLES(I)(1:istr_end(titles(i)))
     &    //char(27)//'[;37m',I=1,N_TITLES)
        else
          WRITE(*,'(A)') 
     &    (TITLES(I)(1:istr_end(titles(i))),I=1,N_TITLES)
        endif
          WRITE(U3,'(A)') '*JOB_TITLES'
          WRITE(U3,'(A)') (''''//TITLES(I)(1:istr_end(titles(i)))
     &    //'''',I=1,N_TITLES)

      ELSEIF (KEYWORD.EQ.'*LOAD_STEP_TITLE') THEN
        READ (U1,'(A)') TITLE_LS
c       IF (TITLE_LS.EQ.' ') TITLE_LS = ' --- No title was given --- '
        WRITE(*,'(A)')  char(27)//'[1;33m'//TITLE_LS//char(27)//'[;37m'

c----------------------- Element Group settings ------------------------
c.. 28-1-98 push to K_MESH.F ?
c   15-4-98 pushed to K_mesh.f - but I still need to set a IOPS flag here
c        to use the LEGO concept
      ELSEIF (KEYWORD.EQ.'*SET_LEGO_ALLDIFF') THEN
        IOPS(11) = 1          ! for testing only: result should be == to
        DO IEL= 1,NEL         ! that without using IGROUP
          CALL PUT_EL_IGRP (NUMS,INUMS,IEL,IEL)
        ENDDO
      ELSEIF (KEYWORD.EQ.'*SET_LEGO_ALLSAME') THEN
        IOPS(11) = 1          ! as used my MEMSIZ_20, etc.
        DO IEL= 1,NEL
          CALL PUT_EL_IGRP (NUMS,INUMS,IEL,1)
        ENDDO
      ELSEIF (KEYWORD.EQ.'*SET_LEGO_IMAT') THEN   ! 2-2-97
        IOPS(11) = 1          !
        DO IEL= 1,NEL
          CALL GET_EL_IMAT (NUMS,INUMS,IEL,IMAT)
          CALL PUT_EL_IGRP (NUMS,INUMS,IEL,IMAT)
        ENDDO

c--------------------------- user-fudges -------------------------------
      ELSEIF (KEYWORD.EQ.'*FUDGE_TUNNEL_ZPLANES') THEN   !- gosh!
c         .. better as   '*SET_LEGO_MYTUNNEL'
c
c     For the Tunnel mesh (D.Ho), this sets the (usu 20) elements
c     along the line of the tunnel to the same group #
c     .. this will reduce the PCG KV size by a factor of 20 !
c
        IOPS(11) = 1          !- flag IGROUP 'on'
        DO Igrp=1,324         !- loop over the x-y plane
           DO IEL= igrp,nel, 324   !316 !308  !- loop down Z
             call put_el_igrp (nums,inums,iel,igrp)
           ENDDO
        ENDDO

c      ELSEIF (KEYWORD.EQ.'*FUDGE_TUNNEL_MATS') THEN   !- wow!
c
c     For the Tunnel mesh (D.Ho), this sets the 16 elements in the tunnel
c     cross-section to a different matrial # as we advance along the
c     tunnel line
c     ( I think the first 16 elem in the mesh are tunnel ones.)
C    < cf use of *BORE_MATERIALS >
c        imat = 2                       !- first tunnel plane is Mat # 2
c        DO ibase = 1, nel, 324         !- (324 elements per z-plane)
c          DO ITUN=0,15
c            call put_el_imat (nums,inums,ibase+itun,imat)
c          ENDDO
c          imat = imat + 1
c        ENDDO

c------------------ Set The # of Degrees of Freedom --------------------
c.. These are like 'k_mesh.f' but we set NODOF too
c.. maybe flag NODOF = 0, then auto set to NDIM at *NULL_ARRAYS if not
c.. yet set explicitly ?

      ELSEIF (KEYWORD.EQ.'*ONE_DIMENSIONAL') THEN
        NDIM = 1
        NODOF= 1

      ELSEIF (KEYWORD.EQ.'*TWO_DIMENSIONAL') THEN    !- =default -
        NDIM = 2
        NODOF= 2
      ELSEIF (KEYWORD.EQ.'*2D') THEN
        NDIM = 2
        NODOF= 2

      ELSEIF (KEYWORD.EQ.'*THREE_DIMENSIONAL') THEN
        NDIM = 3
        NODOF= 3
      ELSEIF (KEYWORD.EQ.'*3D') THEN
        NDIM = 3
        NODOF= 3

      ELSEIF (KEYWORD.EQ.'*ONE_DOF') THEN   !- eg. for fluid-flow
c.. no! pick up fluid-flow from a material type (and Kx etc.)
        NODOF= 1
      ELSEIF (KEYWORD.EQ.'*BIOT') THEN      !- add pore-pressure
c.. no! pick up BIOT from which elements are set up for it.
        IBIOT = 1                           !- (cf a control option?)
        NODOF= NDIM+IBIOT             !(this is always true)

c---------------- Alternatives to *CONTROL for options ---------------
      ELSEIF (KEYWORD.EQ.'*AXISYMMETRIC') THEN    !- cf *CONTROL
        IOPS(14) = 2
      ELSEIF (KEYWORD.EQ.'*PLANE_STRESS') THEN    !- cf *CONTROL
        IOPS(15) = 1          !- or just force ndim=3 ?

c---------------------- post-processing modules ------------------------
      ELSEIF (KEYWORD.EQ.'*CALC_VOLUMES') THEN
        nbins=20
        CALL CALC_VOLUMES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,
     &                           DISPS_tot,MDF,ndim,nbins)

C---------------------- Material Properties (E,v,c,phi...) -------------

      ELSEIF (KEYWORD.EQ.'*MATERIAL_PROPERTIES') THEN        !(obsolete)
        CALL R_PRPTAB (U1,IDBG(1), PRPTAB,IPRPTAB, NTYPES,NMATS)
      ELSEIF (KEYWORD.EQ.'*MAT_PROPS') THEN
c.. good to write them out again for
        CALL R_PROPS_TOKEN  (U1, PRPTAB,IPRPTAB,MMATS, NMATS, idbg(1))
        CALL WR_PROPS_TOKEN (U3, PRPTAB,IPRPTAB,MMATS, NMATS, idbg(1))

C----------- patch to alter the Material parameters
c.. better as a MAT_PROP then can do EBE (eg via *MP_BY_TOKEN)
      ELSEIF (KEYWORD.EQ.'*FOS') THEN
        READ (U1,*) FOS_C,FOS_PHI

c-----------------------------------------------------------------------
c         ALLOCATE and CLEAR all the Arrays that we will use
c-----------------------------------------------------------------------
c.. this should really be called '*BEGIN_ANALYSE' or '*BEGIN_ANALYSE'?

      ELSEIF (KEYWORD.EQ.'*NULL_ARRAYS') THEN
c.. hmm I need to do this much earlier (*THREE_DIMENSIONAL ?)
c--> I could do this section *automaticaly* after the keywords
c--> IF any of NN ,NEL (NDIM ?) changes
c.. cf a FLAG, if not yet done (at all) do at first CONSOL or PLASTICITY
C (ok so use the length of the stress vector (if=0. @CONSOL/FORM_KM invoke)
C    .. good to do BIOT->NODOF here too.

c.. 30-5-97 re. BIOT.
c    I think that it is at this stage that we need to parse the mesh
c    database, (together with options, such as if an eigenvalue analysis?)
c    This determines which DOF we have (disp,pressure,temp) and hence
c    sums them up and allocates appropriate columns of DISPS_TOT, etc.
c    - this info is also needed when convergence checking etc.
c    - the output of this is surely also NF with the 1's and 0's
c      appropriately marked.

c... perhaps never set NODOF before here ?
c  22-2-98 Determining NODOF.
c       CALL CALC_NODOF (NUMS,INUMS,NEL, NF,INF,NN,NODOF,IBIOT)
C    This routine loops all the elements and determines which freedoms
c    are present.
c    Perhaps in order is best:
C      1: Scan for structural (dx,dy,dz)
c      2: Scan for bending (Rxy, etc)
c      3: Scan for PP (for both BIOT and simple equipotentials)
c      4: Scan for Temperature etc.
c      5: Scan for Velocities (for fluid flow)
c      6: Scan for Accelerations (for Earthquake Dynamics)
c    Each time, we return a 'map' of NF for each present freedom
c    (can give an ICOLUMN for it to start in.)
c    Afterwards count the entries in that column: if =0 then can disregard
c    - hence get IBIOT directly as a flag.
c     So Build the table headers for each column and hence Malloc NF
c     That means that NF always caries the columns headers with it
c     (almost like NF as a set of pointers to the tables)

c     Consider NF2G..
c      This returns G - but perhaps we only want a subset (eg structurals)
c      so we need to tell it which G we want

c     Consider FB_RS etc.
c      Does this only set structural fixities?
c      In general I want to be able to choose which freedoms I am setting,
c      so I can set BIOT fixities independantly
c      In the data need codes for each freedom eg DxDyRz for a beam
c      D=disp., R=rotation, V=velocoity, A=accel.,
c      One DOFs ? eg Temp, PP ?
c
c     Element by element v. Global cases.
c      eg I can mix Beams and Bricks, Biot and non-Biot,
c       but not mix accelerations and not accel !
c


        IF (NODOF.LE.0)             !- if not yet set explicitly
     &  NODOF = NDIM                !-- default is the same as NDIM
        IF (NDIM.EQ.3 .AND. NODOF.EQ.2)
     &  NODOF = NDIM                !- fix *DANBLOCKS into 3d too.

        CALL CALC_NODOF (NUMS,INUMS,NEL, NF,INF,NN,NDIM, NODOF_,DRPTVAC)
! 11-3-01 added next. I think that doing it here is best, altnernative is to
!      do at teh top of teh program, or at least if and when NN changes.
        NODE_ORDER(1) = 0           !- flag as having No optimsied BW.
        IF (iverb >=4)
     &  write (*,'(A,12i3)') 'DRPTVAC=',(drptvac(i),i=1,7)
        IF (iverb >=4)
     &  print*,'nodof_=',nodof_, 'nodof=', nodof
        nodof=nodof_                !- ok lets run with it
        CALL CALC_IH (NODOF,IH)

c.. ON_FREEDOMS needs to know *which* elements have PP freedoms (ie mat_props).
c (cf a call to *ON_FREEDOMS)
        CALL ON_FREEDOMS (NUMS,INUMS,NEL, NF,INF,NN,NODOF,IBIOT) !- all to '1'

c========= a/ Init. the Stress (ie. GP)  tables ==========
c... hack for fixed width stress-tables.
c.. for variable table .. loop and just sum-up each elements requirements.
        CALL GET_NGP_MAX (NUMS,INUMS,NEL, NEL_USED,NGP_MAX, IOPS(10))
        NGP = NGP_MAX

c---- calculate the total storage requirements
c .. be careful if EVSTRS/EVPT are fixed-width 2D arrays
c ** 10-7-96 more work is needed on the NUMBER of terms stored at each
c      Gauss-point:
c        o always have IH stress terms
c        o will have IH pl. strain terms if viscoplastic
c        o will have 1 pore-pressure if BIOT (optional)
c        o will have 3x3 stress direction vectors if large-strain (cf Mok)

        IEVSTRS = NGP*IH           !- ** 11-3-97 Max width of stress array
        IEVPT   = NGP*IH           !- ** 11-3-97 Max width of strain array
        STORE_GP =    NEL *  NGP* (IH)      ! stress, pl. strain (pp?)
        IF (iverb>=2) THEN
          WRITE (*,'(A,I3,A,i8,a,f10.3,a)')
     &  'Storage at Gauss points =', IH+IH,' *', NEL*NGP_MAX,' GP''s  ='
     &   ,STORE_GP*8. /1024./1024.,' Mb'
        ENDIF

c     be careful about NODOF.. if BIOT then = NDIM+1
        STORE_NN = 6* NN  * (NODOF)            ! loads, disps (by freedom)
c                                              ! inc. disps, tot disps,
c                                              ! app forces, tot force, NF(int)
c.. note PCGs own 'local' storage too (4 vectors ?: Xnew,P,U,D)
        IF (iverb>=2) THEN
          WRITE (*,'(A,I3,A,i8,a,f10.3,a)')
     &     'Storage at Nodal points =', 6*NODOF,' *', NN,' Nodes ='
     &    ,8*STORE_NN /1024./1024.,' Mb'
        ENDIF

        IF (IH*NGP.gt.IEVSTRS)
     &  CALL MYERROR (3,'not enough space for elem stress in EVSTRS ')
        IF (STORE_GP.gt.MSTRESS) THEN
          PRINT*,'Store_gp=',STORE_GP,' mstress=',MSTRESS
        CALL MYERROR (3,'not enough space for elem stress in STRESSES')
        ENDIF

c.. I need a flag to not bother storing any streses (ie if just elastic)
c   - Hence in PLASTICITY section too
       IF (IOPS(2).GE.0) THEN    !- ie. IEVSTRS, IPORE are pseudo
         CALL NULL2D (STRESSES,IEVSTRS, IH*NGP_MAX,NEL) !- total stresses
         CALL NULL2D (PORE,IPORE,NGP_MAX,NEL)
       ENDIF

c========= b/ Init. the disp (ie. nodal)  tables ==========
        CALL NULL2D (DISPS_TOT, MDF ,NODOF,NN)   !- total disps
        CALL NULL2D (FORCES_N,MDF ,NODOF,NN)     !- applied forces
        CALL NULL2D (FORCES_TOT,MDF ,NODOF,NN)   !- total applied forces
c defer EVPT until Plasticity loop
C defer DISPS until Plasticity loop

c.. ON_FREEDOMS needs to know *which* elements have PP freedoms (ie mat_props).
c       CALL ON_FREEDOMS (NUMS,INUMS,NEL, NF,INF,NN,NODOF,IBIOT) !- all to '1'

C-----------------------------------------------------------------------
c... kill the total disps, stresses etc. (eg when doing a different FOS
c... need to split so can zap disps. but leave stresses ?
c... but would this kill equilibrium ('cos no strain) ??
c-- so ?why not EVPT too? ..cos nulled every load-step ?
c ** careful of NGP here !
c - Why cant I just use *NULL_ARRAYS again? is it just NF that we keep?
c   if so make NF part of the nodes' data structure.
      ELSEIF (KEYWORD.EQ.'*RESET_STATE') THEN
c       .. any need to do the GET_NGP_MAX here ?
        CALL GET_NGP_MAX (NUMS,INUMS,NEL, NEL_USED,NGP_MAX, IOPS(10))
        CALL NULL2D (DISPS_TOT, MDF ,NODOF,NN)           !- total disps
        CALL NULL2D (STRESSES,IEVSTRS, IH*NGP_MAX,NEL)   !- total stresses
        CALL NULL2D (PORE,IPORE,NGP_MAX,NEL)
        FOS_C=1. ; FOS_PHI=1.                            !- reset these too

c.. cf forces_tot and forces too.

      ELSEIF (KEYWORD.EQ.'*NULL_NF') THEN     !(junk)
c.. only do once or you have to re-apply your BC's
        CALL ON_FREEDOMS (NUMS,INUMS,NEL, NF,INF,NN,NODOF,IBIOT) !- all to '1'
      ELSEIF (KEYWORD.eq.'*ORDER_NODES_POINT') THEN ! 11-3-01
        CALL R_ORDER_NODES_POINT (U1,GC,IGC,NDIM,NN,NODE_ORDER)

c-----------------------------------------------------------------------
c     Read in 'old' results for a restart
c-----------------------------------------------------------------------
      ELSEIF (KEYWORD.EQ.'*LOAD_STEP_NUMBER') THEN
      ELSEIF (KEYWORD.EQ.'*DISPLACEMENTS') THEN
        CALL R_NODES (U1,DISPS_TOT,MDF,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (KEYWORD.EQ.'*SCALE_DISPLACEMENTS') THEN
c   This scales the current global displacemnts by the given sx,sy,(sz)
c   most usefull as a pre-step to *ADD_DISPLACEMENTS (hence vrml), but
c   also can of course be used to NULL Gdisps of course.
c      DJK 8-7-99
c
        READ (U1,*,iostat=ios) (scale(j),j=1,ndim)
c < cf an automatic way, as in DANPLOT >
c       CALL GET_MESH_RANGE (GC,IGC ,NN,NDIM, GC_MIN,GC_MAX,DIAG)
c       CALL GET_DEFAULT_DISP_SCALE (DISPS_TOT,MDF,NN,NODOF,1,DIAG,FACT)
c < cf animating via a scale factor in VRML >
c < cf 'contouring' via setting the nodal color as a f(disp) >

        do i=1,nn
          do j=1,ndim
            disps_tot(j,i) = disps_tot(j,i) * scale(j)
          enddo
        enddo

      ELSEIF (KEYWORD.EQ.'*ADD_DISPLACEMENTS') THEN
c  This add the displacements to the geometry.
c    used for large-strain analyses and for writing out deformed VRML models.
c      DJK 8-7-99
c
        do i=1,nn
          do j=1,ndim
            gc(j,i) = gc(j,i) + disps_tot(j,i)
            disps_tot(j,i) = 0.
          enddo
        enddo


C-------------------------- Stresses & Strains -------------------------
c.. 11-11-95 : initial support : I will probaly change the format several
c.. times until I get the 'final' style.

      ELSEIF (KEYWORD.EQ.'*STRAINS') THEN
c       .. currently ignored .. cf direct strains from the nodal disps.

c     ELSEIF (KEYWORD.EQ.'*PORE_PRESSURES') THEN     !- this too?
      ELSEIF (KEYWORD.EQ.'*STRESSES') THEN
c.. where do we zap this table if we don't have any stresses (eg an OFF file)
c  (Ans: only when we to a *NULL_ARRAYS)
c.. what is the equivalent *ZERO_STRESS_STATE ? (by material type ?)
c... need to differentiate between sx,sy,.., and s1,s2,s3, ..
        do iel=1,nel           !- first kill the data structure ?
          pstr(iel) = 0
        enddo
        lstresses = 0
        CALL R_STRESSES (U1,STRESSES, mstress,lstresses, pstr,nel)

c-----------------------------------------------------------------------
c                     STIFFNESS MATRIX MODULES
c-----------------------------------------------------------------------
c.. If have have a flag for whether KV has been ALLOCATED then I can
c.. auto-form KV @ the *ANALYSE stage, KM too.

C----------------- Form the global (diagonal) mass matrix --------------
c... this relies on that *FORM_KM has been done first ..hence NF(), etc.
c--> I guess that at this stage, we could 'invert' the MM and multiply
c.. in to KV .. or leave until later ??
c.. Note that if we have the consistent mass matrix (CHOLIN style), then
c.. will need to hold it until later for Eval extraction

      ELSEIF (KEYWORD.EQ.'*FORM_MM') THEN
      CALL FORM_MM_GLOBAL (GC,IGC,NN,NDIM,NUMS,INUMS,NEL,PRPTAB,IPRPTAB
     &         ,NF,INF,NODOF,N,MB,IMB,IR_MB,IOPS, IDBG(1) )
      tot_mass=0.
      do i=1,n
        tot_mass= tot_mass+MB(i)
      enddo 
      write(*,'(a,g25.6)') '<> Total mass=',tot_mass

C-------------- Form and reduce the global stiffness matrix ------------
c.. and big-springs too .. or defer B-S's unitl SPARIN stage
c.. it makes sense to flag when FORM_KM was last done, hence..
c     auto-do on *ANALYSE - cf BIOT which will always want to re-form KM
c
      ELSEIF (KEYWORD.EQ.'*FORM_KM'          ! 'stiffness'
     &    .or.KEYWORD.eq.'*FORM_KP'          ! 'fluid flow'
     &    .or.KEYWORD.eq.'*FORM_KPI') THEN   ! 'inverse fluid'

C       CALL FORM_KM ()
        CALL GET_SECS (TIME(3))
c     ... ( code to reseq NF and check sizes moved here 7-3-95 )

c---- build the 'current' NF array -----
c.. ie. reseq all *present* nodes that have non-zero BC's
c ** 11-3-01 We have a problem here as I want to support bandwidth optimisation
c   so for example we call *SORT_NF_POINT beforehand, or even Cuthill-McGee
c   - a more general approach is to pass CORRECT_NF an argument which contains an option
c     typically the *order* of nodes to use: default = <1:nn>, but other routines can
c     return other orders.
c       call get_node_order (GC,IGC,NN,NDIM,CENTRE, node_order)
c    default (at null_arrays is for 1:nn), or simply set node_order(1)=0 = use 1:nn
c    otherwise we have a usable list.

        CALL CORRECT_NF (NUMS,INUMS,NEL, NN,NODOF,NF,INF, N)
        CALL SORT_NF_ORDERED  (NF,INF,NN,NODOF,NODE_ORDER, N)

c---- form KDIAG for SPARIN and print KV requirements -----
c.. so can malloc
        ipr = idbg(1)
        CALL GET_KV_STATS (NUMS,INUMS,NEL, NN,NODOF,NF,INF, N,iloads
     &   ,IKV, IR, KDIAG,IKDIAG, IRK
     &   ,GROUPS,IGROUPS   ,IOPS(3),IOPS(11),iverb)
c.. so we can malloc now :-)

        IF (IR.eq.0)       CALL MYERROR (3,' Empty stiffness matrix')
        IF (IR.GT.IKV)     CALL MYERROR (3, 'KV Array too small')
        IF (IRK.GT.IKDIAG) THEN
          print*,'irk=',irk,'ikdiag=',ikdiag
          CALL MYERROR (3, 'KDIAG Array too small')
        ENDIF
c.. I can put these three in a 'wrapper' subroutine

c.. compare the next with a style where we set the 'style' of analysis
c.. ie if BIOT, fluid flow etc.
C.. really there is only one *FORM_KM but within that we may be forming
c  several diferent element matrices?
c  eg some BIOT elements, some structural, some beams, some only KP
         IF (KEYWORD.EQ.'*FORM_KM') THEN
           KM_CODE='KM  '
         ELSEIF (KEYWORD.EQ.'*FORM_KP') THEN
           KM_CODE='KP  '
         ELSEIF (KEYWORD.EQ.'*FORM_KPI') THEN
           KM_CODE='KPI '
         ELSEIF (KEYWORD.EQ.'*FORM_KE') THEN
           KM_CODE='BIOT'                         !- 30-5-97 for BIOT perhaps
         ELSE
           KM_CODE='    '
         ENDIF

c.. I also may consider passing the STRESSES array and some info about
c.. the current load step (DTIM, etc.)
c   BIOT will need DTIM and THETA too. (also note the use of NODOF)

         DTIM = 1.  !- hack .. really needs reading in as data.
         CALL FORM_KM_ANY_GLOBAL (GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &     ,PRPTAB,IPRPTAB, NF,INF,NODOF,N,ILOADS
     &     ,KV,IKV, IR, KDIAG,IKDIAG
     &     ,GROUPS,IGROUPS ,IOPS, idbg(1), KM_CODE,DTIM )

        CALL GET_SECS (TIME(4))
        IF (IDBG(2).GE.2) WRITE (*,'(a,f9.2,a)')
     &    '<> KV formation  =',time(4) -time(3),' secs'

c------- Displacement loading ------
        IF (IOPS(8).EQ.2)      !- if Disp loading
     &    CALL APPLY_BIG_SPRINGS (KV,IKV,IR,KDIAG,IKDIAG,N
     &      ,NF,INF,NN,NODOF, FORCES_N,MDF,BIG_SPRING,IOPS(3))

C-----------------------------------------------------------------------
c          now FACTORISE the STIFFNESS MATRIX (or build preconditioner)
c  ... maybe flag as 'not done' and leave until later ?
C-----------------------------------------------------------------------
        CALL GET_SECS (TIME(5))

c-------- PCG's Preconditioner --------
c.. cf treating this as the same as a FACTOR_KV for Sparin.
c.. also we can (should) store IPRECON within KDIAG too.
       IF (IOPS(3).eq.1.or.IOPS(3).eq.6) THEN         !- if CG (sq/tri)
          IF (IOPS(12).eq.1) THEN
            CALL FORM_PRECON_DIAG (KV,IKV,KDIAG,IKDIAG, N)
          ENDIF
        ENDIF
c.. also note here the ability to print out some KV statistics
c.. eg. condition number, max/min pivots, array allocation for PCG, etc.
c 11-3-01 how come it still works if I am doing Eigen analysis , say with NxN storage?
        IF (IOPS(20).eq.1) THEN  !- so can swicth off for eigenanalysis
          if (iverb>=2)
     &    print*,'<> Factorising the Stiffness Matrix...'
          CALL FACTOR_KV (KV,IR,KDIAG,N,iops(3), idbg(1))
        ENDIF

        CALL GET_SECS (TIME(6))
        IF (IDBG(2).GE.2) WRITE(*,'(A,F9.2,A)')
     &    '<> Matrix factor =', TIME(6) -TIME(5),' secs'

c=======================================================================
C            APPLIED LOADINGS and INITIAL STRESSES
c=======================================================================

C-----------------------------------------------------------------------
c... see K_BC.f  handler too

C--------------- The % of the load to apply in each increment ----------
      ELSEIF (KEYWORD.EQ.'*LOAD_STEPS') THEN
         CALL R_LOAD_STEPS (U1,Q_NUMBER,Q_PERCENT,MQINC,IQ_TOT)
         DO I=1,IQ_TOT
           Q_DTIME(I)=1.     !- pseudo to equal time steps of 1. second
         ENDDO
      ELSEIF (KEYWORD.EQ.'*TIME_STEPS') THEN       !- 30-5-97
         CALL R_TIME_STEPS (U1,Q_NUMBER,Q_PERCENT,Q_DTIME,MQINC,IQ_TOT)

c----------------------- initial stresses (CONS) -----------------------
c *APPLY_CONSOL and *APPLY_GRAVITY 'sort-of' produce the same effect
C for a regular block .. the first directly forms stresses due to
C overburden (Sy = Yc * GAMA + constant)
c The second produces nodal forces everywhere that *ANALYSE turns
C into stresses (AND DISPS!)
c 17-3-99 Should we be able to set xs pore pressure here too?

      ELSEIF (KEYWORD.EQ.'*APPLY_CONSOL') THEN
        CALL APPLY_CONSOL (GC,IGC,NN,NDIM,NUMS,INUMS,NEL,
     &          PRPTAB,IPRPTAB,STRESSES,IEVSTRS,NODOF, 'xy',IDBG(1) )

      ELSEIF (KEYWORD.EQ.'*APPLY_CONSOL_XZ') THEN !- for horiz slice models
c 25-8-98 note here that If I build a layer I would like to be able to
c     control its initial stress state, eg. prestress for anchor-rods,
c     and stress for tunnel linings etc.

        CALL APPLY_CONSOL (GC,IGC,NN,NDIM,NUMS,INUMS,NEL,
     &          PRPTAB,IPRPTAB,STRESSES,IEVSTRS,NODOF, 'xz',IDBG(1) )

c------------------------- Gravity Loading -----------------------------
      ELSEIF (KEYWORD.EQ.'*APPLY_GRAVITY') THEN
        CALL APPLY_GRAVITY (GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &  ,PRPTAB,IPRPTAB,FORCES_N,MDF,NODOF, 230964,1., IDBG(1),IOPS(14))

c=======================================================================
c               EXCAVATION   and  CONSTRUCTION
c=======================================================================
c.. need to use a subroutine to do the 'change_material' bit.
c.. should LOOP here too
c 17-3-99 if we build then:
C          1: must reset the BC (at least of the new bit)
C          2: should we ave control over the initial stress/pp ?
      ELSEIF (KEYWORD.EQ.'*BUILD_LAYER')  THEN
        READ (U1,*) jmat
        CALL CHANGE_MATS (NUMS,INUMS,NEL, -JMAT, JMAT,nchange)     !- 'on'
        WRITE (U3,'(a/2I4)') '*CHANGE_MATERIALS', -JMAT,JMAT
        IF (NCHANGE.EQ.0) then
          call myerror (1,'No elements found to add.')
          goto 888
        endif
        CALL APPLY_GRAVITY (GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &   ,PRPTAB,IPRPTAB,FORCES_N,MDF,NODOF, JMAT,1., IDBG(1),iops(14))
c... with construction .. start afresh so need to reapply BCs  :-(
        CALL ON_FREEDOMS (NUMS,INUMS,NEL, NF,INF,NN,NODOF,IBIOT)

c-----------------------------------------------------------------------
c... with excavation the existing BC's are all stil valid
c.. note we have 2 sets of forces:  BT*sigma *and* self-weight (-ve)
c   reaky self-weight is the existing stress state as the start of the analysis?

c.. * Instead of Jmat *any* flaging of elements is valid (via P())
c   hence *EXCAVATE_ELEMENT, and give a list (as ifrom:ito, or individual
c  so generic *EXCAVATE and *BUILD
c  and give codes of 'mat=4', 'iel=340', 'iels=254:260'
c  loop and parse, for each, mark elements in P, thence generic.

!  in most case we excavate a sequence of materials
! so:
!   a) read a set of materials - store on a stack and then keppign poping until the list is empty
!   b) extend syntax so we remember the last JMAT and say do JMAT+1 
!    - we can play with say excavting a random element from a target IMAT, etc.

      ELSEIF (KEYWORD.EQ.'*EXCAVATE_LAYER') THEN
        READ (U1,*) jmat
! so pick a criteria - form a list of elements and somehow flag them
!  then get forces, and negate gravity (err and any point loads?)
!   then set negate their IMAT 
!  (*CHANGE_MATERIALS  is tricky - cf. set_imat of an element list)
        CALL EXCAV_FORCES (GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &    ,STRESSES,IEVSTRS, PORE,IPORE
     &    ,FORCES_N,MDF,NODOF, JMAT, IDBG(1),IOPS(14))
        CALL APPLY_GRAVITY (GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &   ,PRPTAB,IPRPTAB,FORCES_N,MDF,NODOF, JMAT,-1., IDBG(1),IOPS(14))

        CALL CHANGE_MATS (NUMS,INUMS,NEL, JMAT, -JMAT,nchange)     !- 'off'
        WRITE (U3,'(a/2I4)') '*CHANGE_MATERIALS', JMAT,-JMAT   !- so plot chnges
c.. cf shape_optimisation writing
        IF (NCHANGE.EQ.0) then
          call myerror (1,'No elements found to excavate')
          goto 888
        endif

c=======================================================================
c                     SHAPE   OPTIMISATION
c=======================================================================
c by pore pressure too ?
      ELSEIF (KEYWORD.EQ.'*MATS_BY_STRESS') THEN
        CALL R_MATS_BY_STRESS (U1,GC,IGC,NN,NDIM,NUMS,INUMS,NEL, P,
     &               PRPTAB,IPRPTAB,STRESSES,IEVSTRS,NODOF, IDBG(1) )

C-----------------------------------------------------------------------
C-------------------- The Main Body of the Program ---------------------
c---------------------- Problem solution phase -------------------------
c-----------------------------------------------------------------------
c... in FTN90 I guess that I will use PROCEDURES for this and other
c... large blocks that otherwise would have too many parameters
C-----------------------------------------------------------------------
      ELSEIF (KEYWORD(1:8).EQ.'*ANALYSE') THEN
        CALL GET_SECS(TIME(9))        !(time for analyse)

C----------------------- check array sizes -----------------------------
c.... maybe do this as we go along  ?  (and do a summary at the end ?)

      iters_tot= 0
      iters_pcg_tot= 0
      N_CASES = N_CASES + 1     !-- run the rest of the program

c----------------------- Data summary Output ---------------------------
c     WRITE(*,'(70(''-''))')
c     IF (N_CASES.EQ.1) THEN      !-- only for the first pass ?
c       WRITE(*,'(A,I6,'' /'',I6,'' ('',F5.1,'' % )'')')
c     +  'Number of nodes     = ',NN   ,MELS    ,100.*NN/MELS,
c     +  'Number of elements  = ',NEL  ,MELS-1  ,100.*NEL/(MELS-1),
c     +  'Number of freedoms  = ',N    ,ILOADS  ,100.*N/ILOADS,
c     +  'Size of KV          = ',IR   ,IKV     ,100.*IR/IKV,
c     +  'Number of materials = ',NPROP,MPROPS  ,100.*NPROP/MPROPS
c       WRITE(*,'(70(''-''))')
c.. maybe rewrite the job title/ datafilename here ?

c---------------- Write Job Titles to the .OUT file --------------------
c.. no not here but do as soon as we read them.
c        IF (TITLES(1).ne.' ') THEN
c          WRITE(U3,'(A)') '*JOB_TITLES'
c          WRITE(U3,'(A)') (''''//TITLES(I)(1:istr_end(titles(i)))
c     &    //'''',I=1,N_TITLES)
c        ENDIF
c       ENDIF !- just for the first loadstep?

c-----------------------------------------------------------------------
c----------------- Standard Elasto-plastic Solution --------------------
c-----------------------------------------------------------------------
      IF (KEYWORD.EQ.'*ANALYSE') THEN
c     IF (KEYWORD.EQ.'*ANALYSE_EXPLICIT') THEN
c.... hmm these checks will happen as we go along surely.
C     WRITE(*,'(3(A,I8),
c      IF (IDBG(1).GE.2)
c     & PRINT*,  'nel=',nel, ' nn=',nn ,' n=',n,' ir=',ir
      IF   (      NN .GT. MNN
     & .OR. KDIAG(N) .GT. IKV
     & .OR.      NEL .GT. MEL
     & .OR.        N .GT. ILOADS )
     &  CALL MYERROR (2,'******* ARRAYS TOO SMALL ********')

C======================================================================
C======================= LOAD INCREMENT LOOP ==========================
C======================================================================
c.. remember again here the EXPLICIT method where:
C    we have DISPS_TOT as usual, (+ FORCES_TOT just for reference)
C      velocities and accels need their own arrays
C      DISPS is probaly not used.

      DO I=1,N                  !- just for the PCG solver ?
        DISPS(I) = 0.
      ENDDO

      IY1  = 1                  ! indeces into Q_NUMBER : step nuber
      IY2  = 0                  !                       : so far of this step
      PTOT = 0.                 !-- total of the load applied so far
      TIMETOT = 0.                 !-- the time (or init to a time of day ?)

      N_LOAD_STEPS = 0          !-- work out the total number of load-steps
      DO I=1,IQ_TOT
        N_LOAD_STEPS = N_LOAD_STEPS + Q_NUMBER(I)
      ENDDO
c.. work out the total time too?
C      TOTFORCE=0.
C      DO i=1,NN
C        DO J=1,NODOF
C          IF () FORCES_N(J,I
C        ENDDO
C      ENDDO

      loadsteps: DO IY = 1,N_LOAD_STEPS

c...... first work out how much of the 'total' load to apply
      IY2 = IY2 + 1
      IF (IY2.GT.Q_NUMBER(IY1)) THEN   !- go to the next stage
        IY1 = IY1 + 1
        IY2 = 1
      ENDIF

      DTIME= Q_DTIME (IY1)                  !- time-step length
      TIMETOT = TIMETOT + DTIME                   !- time now (at end of step)
      STEP = Q_PERCENT (IY1) /100.          !- (read in as a percentage)
      PTOT = PTOT + STEP                    !- total so far (unused)

      IF (IDBG(1).ge.3)                     !- each load step
     &   write(*,'(A,i4,a,i4,a,g12.3,a,g12.3,a)')
     &   'Load Step',iy,' /',N_LOAD_STEPS
     &   ,' Step=',Q_PERCENT (IY1)
     &   ,'% DTIME=',DTIME
     &   ,CHAR(27)//'[A'       !- DTIME too?

c------- for time-dependant problems... --------
c (when we originly formed KP we do MM-KP*DTIM ?
c loop elements, form KP, *DISPS_TOT (just PP column), add into LOADS
c      DO IEL=1,NEL
c        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
c     &      ,IMAT,IGRP,IUSER2,IUSER3)
c        IF (IMAT.LE.0) GOTO 91     !- CYCLE on 'missing' materials (cf CYCLE)
c        permx    = PRPTAB (15,IMAT)        !- cf over-writing E,v (,c)
c        permy    = PRPTAB (16,IMAT)
c        permz    = PRPTAB (17,IMAT)
c        CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
c        IDOF = NOD*1
c        CALL NULL2D (KM,IKM,IDOF,IDOF)            !- cf an SRI approach
c        CALL FORM_KP (KM,IKM, NDIME,NOD,ITYPE, COORD,ICOORD
c     &                  ,KAY,IKAY,NGP, IAXES)
cc    now mung KP to MM - (1-theta)*KP*DTIM
c
c          IFREE = NF(J,I)
c            LOADS(IFREE) =
c     &      LOADS(IFREE) + FORCES_N(J,I) *STEP*FAC   !-- applied loads
c        ENDDO
c     ENDDO

C================== set up iteration loop ===============================
c.. explicit methods will have no iteration.
      DO I=1,N
        DISPS_OLD(I) = 0.        ! for checking convergence
      ENDDO                      ! -or do within CHECON?
c.. maybe best to do this *within* the plasticity subroutine
c.. eg. if initial stress no need to do at all.
      IF (IOPS(2).GE.1)
     &CALL NULL2D (EVPT  ,IEVPT, IH*NGP,NEL)    !- plastic strains
c.. careful about whether we null the bodyforces every load-step
c.. or just once and 'work from that'
      IF (IOPS(2).GE.0)
     &CALL NULL2D (BDYLDS_N,INF, NODOF,NN)      !- just for now ?

      ITER_MAX = IOPS(4)       !- eg 0.001 if val = 3
      F_MSS_MAX = 0.               ! just for table output - first time round
      ngp_failed=0
      TOL = 10.**(-IOPS(5))

C======================= iteration loop ===============================
c        do i=1,n
c          disps(i) = 0.       ! hack for PCG init guess ?
c        enddo
      DO ITER = 1, ITER_MAX

      iters_tot= iters_tot + 1      !- total for this *ANALYSE

C-------------------- form the vector of applied loads -----------------
      DO I=1,N
        LOADS(I) = 0.
      ENDDO

      IF (IOPS(8).EQ.1) THEN
        FAC = 1.
      ELSEIF (IOPS(8).EQ.2) THEN
        FAC = BIG_SPRING
      ELSE
        CALL MYERROR (2,' Unknown method : Force/Displacemnt loading')
      ENDIF

      DO J=1,NODOF
        DO I=1,NN
          IFREE = NF(J,I)
            LOADS(IFREE) =
     &      LOADS(IFREE) + FORCES_N(J,I) *STEP*FAC   !-- applied loads
        ENDDO
      ENDDO
      TOTFORCE=0.
      DO IFREE=1,N
        TOTFORCE = TOTFORCE + LOADS(IFREE)
      ENDDO
      ipr=idbg(1)
      if (iverb.ge.5)
     &WRITE(*,'(A,E20.6)') 'Total applied load=',TOTFORCE

C------- if BIOT then add the KP*PP loads.
c.. cf. APPLY_GRAVITY
      IF (IBIOT.GE.1) then
c       CALL ADD_BIOT_LOADS (GC,IGC,NN,NDIM,NUMS,INUMS,NEL, DTIME
c    &   ,PRPTAB,IPRPTAB,FORCES_N,MDF,NODOF, JMAT,-1., IDBG(1),IOPS(14))
c-----
c     SUBROUTINE ADD_BIOT_LOADS
c       (GC,IGC,NN,NDIM,NUMS,INUMS,NEL, DTIME
c    &   ,PRPTAB,IPRPTAB,FORCES_N,MDF,NODOF, JMAT,-1., IDBG(1),IOPS(14))
c     This loops the elements (with BIOT activated)
c      for each it abstracts the (4) pore pressures from the (third)
C      column of DISPS_INC, (using G from NF)
c      forms the KP matrix, multiplies by the (4) PPs and adds into FORCES_N
c     (note here that DTIME is also used when forming KM.
c     END !(add_biot_loads)
c-----

      ENDIF

C------- if re-iterating then add the XS body-forces
c.. large strains too. ?
      IF (ITER.GT.1) THEN
        DO J=1,NODOF
          DO I=1,NN
            IFREE = NF(J,I)
            LOADS(IFREE) = LOADS(IFREE) + BDYLDS_N(J,I)  !-- XS bodyforces
          ENDDO
        ENDDO
      ENDIF
      LOADS(0) = 0.          ! reset (cos of BDYLDS?)
      DISPS(0) = 0.          ! disps too ? (PCG initial guess)


C======================= SOLVE THE EQUATIONS ===========================
c      DO III=1,5
      LOADS(0) = 0.          ! reset (cos of BDYLDS?)
      DISPS(0) = 0.          ! disps too ? (PCG initial guess)
      CALL GET_SECS(TIME(7))

      iters_cg = 0
      CALL BACKSUB_KV (KV,IKV,IR,KDIAG,N,IOPS(3)
     &              ,DISPS,LOADS,iters_cg, IDBG(1) )
c     iters_cgs(min(iters_tot,50)) = iters_cg    !- for all
      iters_cgs(min(iter,50)) = iters_cg         !- just until convergence
      iters_pcg_tot=iters_pcg_tot+iters_cg       !- total altogether

      CALL GET_SECS(TIME(8))
c      IF (IDBG(2).GE.2) WRITE(*,'(A,F9.2,A)')
c     &    '<> Matrix solve =', TIME(8) -TIME(7),' secs'
c      ENDDO
C=======================================================================

      IF (IOPS(2).LE.0) THEN        !- for 'elastic' only
         CONVERGED = .TRUE.      !- so we just update the stresses only
c   .. maybe use a different 'update stresses at convergence' routine ?
c   .. if IOPS(2)=-1 then we disable any stress arrays?
c   .. BT*sigma too

      ELSE                          !- for 'elasto-plasticity'
c.. compare with CHECONing on given columns of DISPS_INC
c.. This is necessary for BIOT
c YES I need a new checon that is told which columns of DISPS_INC to use.
c  or maybe call mulipley for each column - satisfy all for convergence.
c ie. loop x,y,z , then if BIOT do BIOT too.
c 12-5-98  more generically: loop 1:n and max_into foo(1:5)
c  so a list of whoch columns are still not convergent
c (also simple to count each column too)
c  -> perhaps defer until we have build DISPS_INC - a few lines later

c--- I can skip checking if this is the first pass.
        CALL CHECON3 (DISPS,DISPS_OLD,N+1,BIG,BIG_REL)     ! (0:n)
        CONVERGED = (BIG_REL.LT.TOL)    !-- convergence

        IF (IDBG(1).ge.5)
     &     WRITE(*,'(I5,A,G14.5,A,F10.5,A,F6.3,A,i7,A)')
     &      ITER,' : Big=',BIG,' Rel=',Big_Rel
     &   ,' F=',F_MSS_MAX, ' iters_cg=',iters_cg
     &   ,  char(27)//'[A'
      ENDIF
      DO I=1,N
        DISPS_OLD(I) = DISPS(I)     !- ready for the next pass
      ENDDO

c.. note we dont need G here (or indeed later?)
      DISPS(0) = 0.    !- just make sure !
      DO J=1,NODOF
        DO I=1,NN
c         DISPS_INC_OLD(J,I) = DISPS_INC(J,I) !- remember the last.?
          DISPS_INC(J,I) = DISPS(NF(J,I))
        ENDDO
      ENDDO
c.. for BIOT we would like to interpolate back to the mid-side nodes here
c ie. for each element, get NUM() and NUMF(), WTHATN parent and loop and
c     sample in NUMF  (can skip if node is in nodf )
c

C----------------------------------------------------------------------

c... skip for now .. but REACTIONS are undefined :-(

c... maybe move the element loop to the outside .. hence call a different
c... routine for different algorithms (eg CAM-CLAY or MONOT)

c.. This routine does *2* jobs:
c    1/ calcs the bodyforces BDYLDS_N where yielding occurs
c    2/ calcs the nodal reaction forces BDYLDS_N if CONVERGED

c---- hack for no STRESS arrays ----
c.. just means that the TOTAL_FORCE column of the results table is undefined
      IF (IOPS(2).lt.0) THEN
        CALL NULL2D (BDYLDS_N,INF, NODOF,NN)
      ELSE
c---- standard case ----
        IOP_VM = IOPS(7)         !- for M-C / Von Mise
c       F_MSS_MAX= 0.            !- just in case F_MSS gets undefined :-(
       CALL PLASTICITY (GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &  ,PRPTAB,IPRPTAB,IOPS(10),NODOF           !(10=IOP_NGP)
     &  ,DISPS_inc,mdf
     &  ,STRESSES,IEVSTRS, PORE,IPORE, EVPT,IEVPT, BDYLDS_N,INF
     &  ,CONVERGED,IOP_VM,FOS_C,FOS_PHI
     &  ,ngp_failed, F_MSS_MAX, IDBG(1),iops(14) )
c... what about an initial stress algorithm ?
      ENDIF
C---------- end of GP and elem loops .. can do some monitoring --------

        IF (CONVERGED) THEN          !- == 'EXIT'
          GOTO 1111
        ELSE
          IF (ITER.GE.ITER_MAX) then 
            CALL MYERROR(1, 'CONVERGENCE FAILED')
            exit loadsteps  !- skip out of iteration loop (and load step loop)
          endif
        ENDIF
      ENDDO    !-- loop the next iteration

C----------------------------------------------------------------------
C------------------- End of a load step so update arrays ---------------
C----------------------------------------------------------------------

 1111 CONTINUE   !- jump out of iteration loop

      DO J=1,NODOF    !-- total displacements (just for print-out)
        DO I=1,NN
          DISPS_TOT(J,I) = DISPS_TOT(J,I) + DISPS_INC(J,I)
        ENDDO
      ENDDO

c.. is the next valid for disp-loading?
      DO J=1,NODOF    !-- total applied loads ----  (unused)
        DO I=1,NN
          FORCES_TOT(J,I) = FORCES_TOT(J,I) + FORCES_N(J,I) * STEP
        ENDDO
      ENDDO

c-----------------------------------------------------------------------
c               Output the results to screen & file(s)
c-----------------------------------------------------------------------

c----------------------- headers for this load case --------------------
c.. this starts a sets of MODULEs that carry the results
c--> Need to differentiate, 'lifts' and 'sub-increments'
c.. so *LOAD_CASE_TITLE is for a whole *ANALYSE
C..  *LOAD_CASE_NUMBER is simple sequential, but Lift #, sub-inc # too ?
c (maybe write the (predicted) # load cases, so we can allocate storage

      WRITE (U3,'(A/,2I10)') '*LOAD_CASE_NUMBER', N_CASES,IY
      WRITE (U3,'(2(A,G12.3),A,G12.3)')
     &    '! Dtime=',dtime,' tot.Time=',TimeTot,' %applied=', step
      IF (TITLE_LS.ne.' ')
     &WRITE (U3,'(A/,A  )') '*LOAD_CASE_TITLE ', TITLE_LS

C--------------- Output displacements in DANPLOT style -------------
c... need a switch to disable output (cf SAGE_PIPE), eg. on the gravity step
c.. also maybe support other write_formats too?

c nice to show thhe max/min of each column too.
c perhaps I could have the RMS too?

c 12-5-98 what about options to specify which columns we want (so that we
c   can include the nodal coords here too -> eg for BAHA and EXCEL)
c   DANPLOT can cope with this I guess (Still needs NUMS - unless it
C    invents its own (Voronoi)?

c 12-5-98 what about isolating a subset of the data eg _by_box

      WRITE (U3,'(A,/,A,I3,A,G14.5)')
     &  '*DISPLACEMENTS',
     &  '!  Load-step No.',IY,' Total Load=',9999.
      DO I=1,NN
        WRITE (U3,'(I7,9E16.6)')  I,(DISPS_TOT(J,I),J=1,NODOF)
      ENDDO
c      DO I=1,NN
c        WRITE (U3,'(I7,13E12.4)')
C           I,(GC(J,I),J=1,NDIM),(DISPS_TOT(J,I),J=1,NODOF)
c      ENDDO

c----------------------- Output the stresses ---------------------------
C.. nice to have options to print:
c    1/ SIGM,DSBAR,THETA,  2/ SIG1,SIG2,SIG3
c    3/ S,T  4/ Tf, Mob shear strength,  5/ GP x,y,z coords
c.. needs a global on/off switch  (also control whether to a 2nd file too.)
c.. can use this routine for strains too, but must flag so that we /=2
c   the shear strains to give the 'tensoral' strain (for INVAR, etc)

      IF (IOPS(17).GT.0)            !- stress -!
     &   CALL WRITE_STRESSES_OLD (U3, GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &    ,PRPTAB,IPRPTAB, STRESSES,IEVSTRS, PORE,IPORE  ,NODOF
     &    ,IDBG(1),.false.,IOPS(10) )

      IF (IOPS(18).GT.0)            !- (plastic) strain -!
     &   CALL WRITE_STRESSES_OLD (U3, GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &    ,PRPTAB,IPRPTAB, EVPT,IEVPT, PORE,IPORE  ,NODOF
     &    ,IDBG(1),.true.,IOPS(10) )     !(PP not relevant here?!)

c--- provide a log of when this section was reached
c    and CPU time, both for this load step and cummulative
c    if time is 'running out' then perhaps we should clean up and go.
c
c      WRITE (U3,'()')
c     &'# time=', ' dtime='

C-------------------- Output the screen Summary table ------------------
c   nice flag to write to a file too
c   (eg, pass IO, if IO.ne.0) write to this unit.
c     call calc_fmax_dmean_slope ()
      IF (IDBG(1).GE.2) THEN                   !- was IDBG(6) -191295
       CALL WRITE_SUMMARY_TABLE (6,IY,ITER, ngp_failed,F_MSS_MAX
     &     ,NF,INF, BDYLDS_N,MDF,NN,NODOF, N,iters_pcg_tot
     &     ,FORCES_N, MDF ,DISPS_INC,MDF)

c--- show details of the CG solver iteration count
C  4-3-97 cf. doing this as a column of the summary table
       IF (IDBG(1).GE.4.and.(IOPS(3).eq.1.or.IOPS(3).eq.6))
     &    WRITE(*,'(A,99I5)')' Iters_cg:',
     &    (iters_cgs(i),i=1,min(iter,50))

      IF (iy.eq.n_load_steps) then     !-- close table
       CALL WRITE_SUMMARY_TABLE (6,-1,ITER, ngp_failed,F_MSS_MAX
     &   ,NF,INF,BDYLDS_N,MDF,NN,NODOF, N,iters_pcg_tot
     &  ,FORCES_N, MDF,DISPS_INC,MDF)
      endif

c       now write the total plast_iters, cg_iters and %age for this loadcase
      ENDIF


C-----------------------------------------------------------------------
C --- end of the load steeping loop.. print final state afterwards? ----
      ENDDO loadsteps   !--- loop the load-steps

C-----------------------------------------------------------------------


c--------- simple 'elastic' analysis for steady-state fluids -----------
c.. this also should be OK for Elastic solids I think
      ELSEIF (KEYWORD.EQ.'*ANALYSE_POTENTIALS'
     &    .or.KEYWORD.eq.'*ANALYSE_FLOWLINES'   ) THEN

c----------- build the vector of applied loads ------
        DO I=1,N
          LOADS(I) = 0.
        ENDDO
        FAC = 1.
        IF (IOPS(8).EQ.2) FAC = BIG_SPRING
        DO J=1,NODOF
          DO I=1,NN
            IFREE = NF(J,I)
              LOADS(IFREE) =
     &        LOADS(IFREE) + FORCES_N(J,I) *FAC   !-- applied loads
          ENDDO
        ENDDO

c------------ solve the equations --------
      CALL GET_SECS(TIME(7))
        CALL BACKSUB_KV (KV,IKV,IR,KDIAG,N,IOPS(3)
     &              ,DISPS,LOADS,iters_cg, idbg(1) )  !_ nevershow this?
      CALL GET_SECS(TIME(8))
      IF (IDBG(2).GE.2) WRITE(*,'(A,F9.2,A)')
     &    '<> Bacsub time =', TIME(8) -TIME(7),' secs'

c---------- accumulate the displacements.. by node -------------------
      IF (KEYWORD.EQ.'*ANALYSE_POTENTIALS') icol = 1   !- in column #1
      IF (KEYWORD.EQ.'*ANALYSE_FLOWLINES' ) icol = 2   !- in column #2
      DO I=1,NN
        DO J=1,NODOF
          IFREE = NF(J,I)
          DISPS_TOT(  icol,I) = DISPS_TOT(icol,I) + DISPS(IFREE)
          DISPS_TOT(3-icol,I) = 0.          !- and ZAP the other (?)
        ENDDO
      ENDDO

C--------------- output pressure/stream-func in DANPLOT style ----------
c.. cf an option to combine the 2 tables, with a rescale
c.. so we have a single table that we can draw a full flow-net from
c
        WRITE (U3,'(A,/,A,I3,A,G12.5)')
     &    '*DISPLACEMENTS',
     &    '#  Load-step No.',IY,' Total Load=', 9999.
        DO I=1,NN
          WRITE (U3,'(I5,3E16.6)')  I,(DISPS_TOT(J,I),J=1,2)
        ENDDO


c-----------------------------------------------------------------------
c                     EIGENVALUE  SOLVERS
c-----------------------------------------------------------------------
c
c======================= Eigenvalue solutions ==========================
      ELSEIF (KEYWORD.EQ.'*ANALYSE_EIGEN'             !- just KM
     &    .or.KEYWORD.EQ.'*ANALYSE_DYNAMIC') THEN   !- KM and MM
! note we can support
      IF (IOPS(3).eq.2.or.iops(3).eq.3) THEN
      ELSE
        call myerror(1,
     &   '** You must use either full NxN matrix <*OPTIONS (3)=2 >')
        call myerror(1,'** or Upper Banded Matrix <*OPTIONS (3)=3 >')
        STOP
      ENDIF

      IF (KEYWORD.EQ.'*ANALYSE_DYNAMIC') THEN   !- include MM
      if (idbg(1).ge.4) print*,'Inverting Mass Matrix..'
C      ....... invert the Lumped Mass Matrix .......
        DO I=1,N
          MB(I) = 1./ SQRT (MB(I))
        ENDDO
C       ........ form KM * MM^-1 .........
      if (idbg(1).ge.4) print*,'Forming KV *MB *MB ...'
        IF (IOPS(3).eq.2) then  ! added 9-3-01
          DO J=1,N           !- really call matrix*vector
            DO I=1,N
              K = (J-1)*N + I
              KV(K) = KV (K) * MB(I)*MB(J)
            ENDDO
          ENDDO
        ELSE IF (IOPS(3).eq.3) then  ! BANDRD added 9-3-01
          IBW = IR/N -1
          do i=1,n
            if (i.le.n-ibw) jj=ibw+1   ! above diagonnal
            if (i.gt.n-ibw) jj=N-I+1   ! in little triangle at base (?)
            do j=1,jj
              k = (j-1)*n + i          ! as if it was kv(i,j)
              kv(k)= kv(k) *mb(i)*mb(i+j-1)
            enddo
          enddo
        ENDIF
      ENDIF
c     .. note use of DISPS as a workspace & 1d->2D nature of KV

c---------- calculate the eigenvalues ------------
      if (idbg(1).ge.4) print*,'Caculating Eiegnvalues...'

      if (iops(3).eq.2) then
c      CALL TRED2 (n,n, KV,LOADS, DISPS, EVECTS,ievects)     ! from EISPACK
c      CALL TQL2  (n,n,    LOADS, DISPS, EVECTS,ievects, IFAIL)
c.. 19-6-97 try overwriting KV (cos it is a spare 'large' matrix.0
        CALL TRED2 (n,n, KV,LOADS, DISPS, KV,n , idbg(1)  )     ! from EISPACK
        CALL TQL2  (n,n,    LOADS, DISPS, KV,n, IFAIL)

      else if (iops(3).eq.3) then
c-- we need to save the modified stifness matrix and so we can recall
c   it later, once for each eigenmode.

      if (idbg(1).ge.4) print*,'Saving KV matrix to disk'
        OPEN (97,STATUS='scratch',FORM='unformatted')
        write (97) (kv(i),i=1,ir)
        IBW = IR/N -1
c       call bandrd (n,iw,kv,ikv, diag, udiag, loads)
        if (idbg(1).ge.4) print*,'calling BANDRD...'
        call get_secs(time(5))
ccc        call bandrd (n,ibw,kv,n, loads, disps, idbg(1))
        call get_secs(time(6))
        if (idbg(1).ge.4) print*,'BANDRD took', time(6)-time(5),' s'

        ifail=1
c       .. hmm old book has 1.e-293, newbook has 1.e-30
c       call bisect (n, 1.e-30, loads, disps,ifail)
        call get_secs(time(7))
        if (idbg(1).ge.4) print*,'calling BISECT...'
ccc        call bisect (n, 1.e-293, loads, disps,ifail, idbg(1))
        call get_secs(time(8))
        if (idbg(1).ge.4) print*,'BISECT took', time(8)-time(7),' s'
        if (ifail.ne.0) call myerror (2,'BISECT is signaling failure')
      endif
       print*,'loads(0,1,2)=', loads(0),loads(1),loads(2)
       print*,'disps(0,1,2)=', disps(0),disps(1),disps(2)

      WRITE (U3,'(A)') '*EIGENVALUES'
!     CALL WRVAL (U3,LOADS,MIN(N,100))
      write(u3,'(4G18.6)') (loads(i),i=0,min(n,100))

      WRITE(*,'(A,F6.1,A)')  'Gives Eigenvalues of:-'
      CALL WRVAL (6,LOADS,MIN(N,20))        !(assume stdout=6)
      print*, 'etc..(', N,' ) Eigenvalues'


c---------- calculate and print the eigenmodes ------------
c for NxN we get all the eigenvectors together (as rows in KV)
c for Banred, we loop and solve for one eigenvector at a time.

c-------------- loop eigenmodes and process --------
      nmodes = min(abs(iops(19)), n)
      if (nmodes.eq.0) nmodes = n         !- 0=lets us print all?
      DO K=1,NMODES
        imode=k
        IF (IOPS(19).LT.0) IMODE=NMODES+1-IMODE   !- *REVERSE* the order (why?)

C-------- solve for the eigenmodes ---------
        IF (KEYWORD.EQ.'*ANALYSE_DYNAMIC') THEN   !- multiply by MM^-1
          if (iops(3).eq.2) then
c           the eigenmodes need scaling by the mass matrix (why>)
c               EVECTS(I,J) = EVECTS(I,J) * MB(I)
c               KV ((j-1)*n+i) = KV ((j-1)*n+i) *MB(i)   !- why is this here ?
            do i=1,n
              DISPS(I) = KV((imode-1)*N + I) * MB(I)
            enddo
          else if (iops(3).eq.3) then
c..         here as we will re-read KV from file
            rewind (97)
            read (97) (kv(i),i=1,ir)
            if (k.eq.nmodes) close (97)     !- remove the scratch file on last pass
!           if (imode.eq.5) then
!             imode = 1  ! hack
!           elseif (imode.eq.1) then
!             imode = 5
!           endif
!           imode = 6  ! hack
!           imode = 11-imode    !- hack
            rlambda = loads(imode-1)      !- eigenvalue (first is in loads(0))
            do i=1,n
              kv(i) = kv(i) - rlambda     !- subtract this eigenvalue from
            enddo                         !- every diagonal term

!           call nulvec (disps,n)         !- RHS to 1,0,0,0,..
!           disps(0)=0.
            do i=0,n
              disps(i)=0.
            enddo

            ifree=2
            print*,'<> debug: imode=',imode,'rlambda=',rlambda,
     &  ' kv(ifree)=',kv(ifree), ' (cf. BIG_SPRING)'

            BIG_SPRING=kv(ifree)*1.e10
            BIG_SPRING=kv(ifree)*1.e20
            kv(ifree) = kv(ifree) + BIG_SPRING
            disps(ifree) = kv(ifree)
            IBW= IR/N -1 
            call BANRED (kv,n,ibw,idbg(1))
            CALL BACSUB (KV,DISPS,N,IBW,idbg(1))    !- solve to get eigenmode.
            do i=1,n
              disps(i)=disps(i) * mb(i)
            enddo
          endif
        ENDIF
c       deallocate (MB)   !- dont need the (diagonal) mass matrix any more.

c
c------ store this eigenmode -----
c and while we are at it find its magnitude SC = max movaemnt at a node.
          disps(0)=0.      ! jsut make sure we have cleared the bit bucket
          sc=0.
          inode=0
          DO I=1,NN
            sc1=0.
            DO J=1,NODOF
              IFREE = NF(J,I)
              IF (IFREE.NE.0) THEN
                DISPS_TOT(J,I) = disps(ifree)
              ELSE
                DISPS_TOT(J,I) = 0.
              ENDIF
              sc1 = sc1 + disps_tot(J,I)**2
            ENDDO
            if (sc1.gt.sc) then
              inode =i 
              sc= sc1
            endif
          ENDDO

c--- scale eigenmode so max =1. at any node. -----
          SC= sqrt (SC)
          if (ipr.ge.4) print*, 'scaling from a max disp=',sc,
     &  '(at node #',inode,')'
          sc = 1./sc
          DO I=1,NN
            DO J=1,NODOF
              DISPS_TOT(J,I) = DISPS_TOT(J,I) * SC
            ENDDO
          ENDDO

          IF (IPR.ge.4) THEN
c------  check scale of this eigenmode -----
          sc=0.
          inode=0
          DO I=1,NN
            sc1=0.
            DO J=1,NODOF
                sc1 = sc1 + disps_tot(J,I)**2
            ENDDO
            if (sc1.gt.sc) then
              inode =i 
              sc= sc1
            endif
          ENDDO
          SC= sqrt (SC)
          if (ipr.ge.4) print*, 'now max disp=',sc,
     &  '(at node #',inode,')'
          endif


c------ print this eigenmode -----
c       ... nice to allow a format as a BIG table of values ?
c           cf *EIGENMODES
          WRITE (U3,'(A)') '*DISPLACEMENTS'
          WRITE (U3,'(A,I3,A,E12.5)')
     &     '# Eigenmode ',imode,' Eigenvalue=',LOADS(imode-1)     ! LOADS(0:n-1)
          DO I=1,NN
            WRITE (U3,'(I5,3E16.6)')  I,(DISPS_TOT(J,I),J=1,NODOF)
          ENDDO


c-----------------------------------------
        ENDDO   !-- loop eigenmodes
c       deallocate (KV)   !- dont need the stiffness matrix either.


c----- transient dynamic analysis ------
c.. this is by 'modal superpostion'
c  Method:
c   calc eigenvalues/vectors
c   choose how many modes to use for recombination. (eg 6)
c   get node to apply cyclical force to, & its freq (OMEGA)
c   get damping ratio (DR=0.05)
c   loop thru time steps. (eg. 20)
c     ( note here *always* having a concept of time, even in Book6_0.d )
c     loop modes (6)
c       calc {Udiag} = [KV] * {Evec}        < isn't KV now the Evals ? >
c         < note Udiag is not time dependant so we can precalculate >
c       F = Udiag(forced node)
c       X1= eval(this node) - w^2
c       X2=X1^2 + 4 w^2*Dr^2*Eval
c       X4= F * 2 w *dr/X2 *ûeval
c       Xmod(mode) =  X3 coswt + x4 sinwt      <sinwt =dforce/dt cf vel.>

c     loop freedoms
c       disp(ifree) += xmod(mode) * KV * Mass(ifree)
c     end loop
c    ir. disp(free) += xmod(mode) * Udiag from above.

c     end loop modes
c     write out table of:  time, F=coswt, 'tip' disp
c   end loop time steps
c.. note that this method is *not* really time stepping, as the previous
c  solution is not used.

c------------------------ DANFE : EXPLICIT ---------------------
c in the style of book11_4, I could have an explicit dynamics code
c   This would be written in a 'parallel-friendly' style.
c   method:
c     Read mesh, read mat_props, forn NF
c     null arrays (stress,strain, disp, vel, accel.)
c     build global MM
c     loop time (eg 300 steps)
c        disp (X1) += vel*dtim + accel * dtim^2/2.
c        loop elements
c           extract an element's disps
c           loop gauss-points:
c           DER->JAC->BEE
c           dEPS = BEE*ELD ; hence dSIG=DEE*dEPS
c           check yield hence BLOAD  (update stresses every time.)
c         form global BDYLDS
c         BDYLDS= BDYLDS + applied loads (can be a f(time) I guess )
c         BDYLDS /= MM
c         really BDYLDS = new accel.
c         vel += dtim * (old accel+new accel)/2.
c         accel = bdylds
c         write time, disp,vel,accel at the selected nodes.
c       end loop timesteps
c       this should run very fast, as no SPARIN. for real speed we
c       should store the BEEs
c



       ENDIF   !--- all posible *ANALYSE types (Eigen / flow, etc.)
       CALL GET_SECS  (TIME(10))

c----- notes on timing routines ------
c.. for counter need a start_watch, stop_watch pair, thus run_time and
c   sum_time columns, eg.
c    or. CALL STOPWATCH (2, 'START')
c      CALL INIT_WATCH (2) .. can auto-init on first call.
c      CALL START_WATCH (2) ..  3 columns: start_time, flag, sum_time
c          CALL LONG_SUBROUTINE
c      CALL STOP_WATCH (2) .. can use START_WATCH as a 'START/STOP'
c      CALL GET_WATCH (2,time, tottime)
c      PRINT_WATCH (2, '<> Solution Phase =' )  !- for consistency?
c       IF (IDBG(2).GE.2) WRITE(*,'(A,F9.2,A)')
c     &    '<> Solution Phase =', TIME(10) -TIME(9),' secs'

c----- reset 'applied loads' ready for the next load step -------
        CALL NULL2D (FORCES_N,MDF ,NODOF,NN)

c-----------------------------------------------------------------------

      ELSE
        FOUND = .FALSE.
      ENDIF

C--------------------- 'standard' keywords -----------------------------
c...  split READ / NF / TRACTIONS ?
      IF (.NOT.FOUND) CALL KEY_MISC (FOUND,iverb, KEYWORD,U1)

      IF (.NOT.FOUND) CALL KEY_NF
     &      (FOUND,iverb, KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &       ,NF,INF, NODOF)

c      IF (.NOT.FOUND) CALL KEY_BOUNDARIES
c     &      (FOUND,iverb, KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL
c     &     ,NF,INF, FORCES_N, MDF, NODOF, IOPS(8) )

      IF (.NOT.FOUND) CALL KEY_LOADS
     &      (FOUND,iverb,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &      ,FORCES_N,MDF, NODOF, IOPS(8) )

      IF (.NOT.FOUND) CALL KEY_MESH_READ
     &   (FOUND,iverb,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      IF (.NOT.FOUND) CALL KEY_MESH_IO
     &   (FOUND,iverb,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

c--- 8-3-98 added here so *BORE_MATERIALS will work in DANFE.
      IF (.NOT.FOUND) CALL KEY_FACETS
     &        (FOUND,iverb,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
     &         FACETS,IFCETS,NFCETS, RINGS,IRINGS)

c-----------------------------------------------------------------------
      IF (.NOT.FOUND) THEN
        CALL MYERROR(1,'unknown keyword:'//KEYWORD)
        PRINT*,' ** Press <Enter> to continue'      !- crash if a batch file?
        READ*
      ENDIF

c--------------- things to do after processing each keyword ------------
c.. eg report if the mesh changes size & plot it, memory used?
c.. clear screen, write to a logfile, check total memory usage
c.. validate the mesh (orphans etc.)
c--- OK extend this lines to show:
c.. nice to fill all blacks with a symbol eg. '---'
C       NN, NEL, RUNTIME, FREE_MEM(Mb), max_disp?

c-------- mesh size checking -----
      IF (NN .GT. MNN)  call myerror(2,'Too many nodes')
      IF (NEL.GT. MEL)  call myerror(2,'Too many elements')
      IF (N .GT. ILOADS)call myerror(2,'Too many freedoms')
c     IF (KDIAG(N).GT.IKV)call myerror(2,'Too large a stiffnes matrix')


c-------- AUTO mesh plotting --------
c... can also plot the deformed mesh - contouring etc.
c just need to define the scale factor
C maybe also allow plotting to file (eg .ps)
c - and what if I want this for every *LOADSTEP ?
      IF (IDBG(3).eq.1) CALL PLOT_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c     IF (IDBG(3).eq.2) CALL PLOT_MESH_DISP
C       (GC,IGC,NDIM,NN,NUMS,INUMS,NEL, DISPS_TOT,MDF)

c--------- Ruler line ---------
      IPR=IDBG(1)
c   verbosity (1-5):
c    0 = off, 1= mini (just *KEYWORDS)
c    2=normal    (maybe 2 lines output per Keyword)
c    3= extra (eg. other KV sizes, PCG iters
c    4= verbose show iterations as we iterate
c    5= everything!


! memory could be the BSS of the current process ?
      elcode='/-/-/-/-'
      IF (iverb.GE.3) THEN                  !- was 3
         if (nel.gt.1) then
           CALL GET_ELEMENT (NUMS,INUMS,1, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IGRP,IUSER2,IUSER3)
           CALL NUM_TO_CODE (NDIME,NOD,ITYPE, ELCODE)
         endif
         CALL GET_SECS (TIME(12))
         WRITE(*,'(A,i1,a, a, I7,a,I7,a, A, a,f7.3,a, A,  a,f8.2,A )')
     &    char(27)//'[0;45m'//
     &    '---',ndim,'D--'
     &   , elcode,    NN,' n'  ,NEL,' e'
     &   ,'------'
     &   ,' Mem=',FREE_MEMORY(1),'Mb'
     &   ,'------'
     &   ,' dt=',time(12)-time(11)
     &   ,' ------'
     &    //char(27)//'[40m'//char(27)//'[0;37m'
      ENDIF

C-----------------------------------------------------------------------
      ENDDO        !-- get the next keyword
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

c--------------------- End of a data file ------------------------------
  888 CONTINUE
      close (u1)
      enddo files                 ! loop input files
c-------------------- End of all data files ----------------------------
c 777 CONTINUE
      close(u3)     !- close output files ?

      call myerror(0,'#TOTAL_WARNINGS')       !- any errors?
      IF (IDBG(1).GE.2) THEN
        CALL GET_SECS (TIME(2))
        CALL WRITE_RUNTIMES (TIME,idbg(2))
c       IF (IDBG(1).GE.1) CALL PRINT_FREE_MEMORY ()
      ENDIF
      WRITE(*,'(A)') '<> DANFE analysis completed'
      END                         ! end of the main subroutine DANFE.
C-----------------------------------------------------------------------
C°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
C°°°°°°°°°° End of the Main 'Event Handling' Program °°°°°°°°°°°°°°°°°°
C°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE WRITE_RUNTIMES (TIME,IPR)
C
C     Writes a summary table of timings : total run-time
C
      IMPLICIT NONE
      INTEGER IPR
      REAL TIME(10), T1,T2
      CHARACTER UNIT*1
c     WRITE(*,'(79(''-''))')

      T1 = time(2)-time(1)
      t2 = t1
      unit = 's'
      IF (t2.GT.60) then
        T2 = T2 / 60.
        unit = 'm'
        IF (T2.GT.60) THEN
          T2 = T2 / 60.
          unit = 'h'
        ENDIF
      ENDIF
      IF (IPR.GE.1)
     & WRITE(*,'(A, f5.2, a,a,f13.2,a)')
     & '<> Total Run Time =', t2,unit,' =', t1,'s'

      IF (IPR.GE.2) then    !- extra timing info
        WRITE(*,'(T5,3(A,F10.2,A))')
     &      'FORM_KV =', time(4) - time(3),' s '
     &     ,'Factor  =', time(6) - time(5),' s '
     &     ,'Bacsub  =', time(8) - time(7),' s '
     &     ,'all ANALYSE=', time(10)- time(9),' s '
        WRITE(*,'(79(''Ä''))')
      ENDIF
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE WRITE_SUMMARY_TABLE (IO,IY, ITER, ngp_failed,F_MSS_MAX
     &    ,NF,INF, BDYLDS_N, IBDYLDS_N, NN,NODOF, N,iters_pcg_tot
     &    ,FORCES_n,IFORCES_N    ,DISPS_INC,IDISPS_INC)
!
!     Write the loadstep summary-table
!      if IY= 1 then the table headers are written as well
!      if IY=-1 then just table ends are written

!     1/6/04  consider >80 columns since I am no longer 80x25 constrained.
!       consider also seperating out the calc from the printing e.g. calc_Ftot_Dmean()
c
C--- print the total force and the mean displacemnt of the loaded nodes
c... note Gravity_loads will therefore include ALL nodes
c.. OK so call write_table (as of years_of_old) : just need
c...    IY, FORCES_N (flags only), DISPS_TOT, BDYLDS_N
c.. maybe can sum the REACTIONS too (where NF=0) to compare
c
c.. It is better to make the 3rd column the % of the 'elastic' stiffness
c   5-12-95  Can't I have an option to write to 2 or more files ?

C     IMPLICIT NONE
      SAVE
      INTEGER IY,NN, IFORCES_N,IBDYLDS_N,IDISPS_INC, INF
      REAL   BDYLDS_N (ibdylds_n,NN)     !- inc.nodal reactions
     &      ,FORCES_N (iforces_n,NN)     !- applied nodal forces (just for 'markers')
     &     ,DISPS_INC(idisps_inc,NN)     !- inc. nodal displascements
      INTEGER   NF(INF,NN)               !- node freedoms
      CHARACTER TIME_C*8                 !- current time function
      EXTERNAL TIME_C
      CHARACTER VBAR*1
c------------- internal variables ---------------
      REAL S_D, S_F, S_F_tot,s_d_tot,  K_elastic,K_tangent
      INTEGER iters_tot
      DATA Ntable/0/           !- # of table (ie *ANALYSIS's) done


!-------------------------- on the last pass ---------------------------
!  note some data is only relevant if using PCG
      IF (IY.EQ.-1) THEN
        PCG_MEAN =iters_pcg_tot/real(max(1,iters_tot))
        WRITE (IO,'(A)')
     &   '+-------+------+-------------+-------------'//
     &   '+--------+--------+-----+--------+'
        WRITE (IO,'(a, i6,a, i6,a, f8.1,a ,f5.2,a)')
     &  'Total iterations='
     &  ,iters_tot,' plastic and'
     &  ,iters_pcg_tot,' PCG, mean='
     &  ,pcg_mean, '  ie.'
     &  ,pcg_mean/max(1.,real(n))*100.,'%N'
        RETURN
      ENDIF

c---------------- Sum the load and disps at 'loaded nodes' -------------
c    call calc_meanF_totD (S_D,S_F, FORCES_N,DISP_INC,BDYLDS_N,NN,NODOF)
      S_D = 0.      !- Disp
      S_F = 0.      !- Force
      IC = 0
      DO I=1,NN
        S_D1 = 0.
        S_F1 = 0.
        DO J=1,NODOF
          IF (NF(J,I).NE.0 .AND.                   !- Skip fixities
     &        ABS(FORCES_N(J,I)).GT.1.E-20) THEN   !- if an applied force
            IC = IC + 1
            S_D1 = S_D1 + DISPS_INC (J,I) **2
            S_F1 = S_F1 + BDYLDS_N  (J,I) **2
          ENDIF
        ENDDO
        S_D = S_D + SQRT(S_D1)
        S_F = S_F + SQRT(S_F1)
      ENDDO
      K_tangent = 0.
      IF (abs(s_D).gt.1.e-20)  K_tangent = S_F / S_D

c------------------- Headers on the first pass -------------------------
      IF (IY.EQ.1) THEN           !- Only do headers once
        if (io.lt.10)             !- only for stdout ?
     &  Ntable = Ntable + 1
        iters_tot = 0            !- sum total of all iterations
        S_F_tot = 0.             !- Total force
        S_D_tot = 0.             !- Total disp
        K_elastic = K_tangent    !- 'initial stiffness'
        IF (abs(K_elastic).lt.1.e-20) K_elastic=1.e-20

        WRITE(IO,'(A,i4,t53,i7,a)')
     &      'Table #', Ntable,
     &     IC,' loaded freedoms'
        WRITE (IO,'(A)')
     &   '+-------+------+-------------+-------------'//
     &   '+--------+--------+-----+--------+'
        WRITE (IO,'(A)')
     &   '| Step# | #Its | Tot.Force   |  Mean disp. '//
     &   '| %Elast |%Mob-SS |  ngp|  Time  |'
        WRITE (IO,'(A)')
     &   '+-------+------+-------------+-------------'//
     &   '+--------+--------+-----+--------+'


      ENDIF

c-------------------- write each line in the table ---------------------
c      IF (IC.EQ.0) THEN
c        WRITE (IO,'(A)') '** No loaded nodes were found !'
c.. also print warning if all disps are zero. (cf CHECON)
c        RETURN
c      ENDIF
      S_F_TOT = S_F_TOT + S_F
      IF (IC.GT.0)
     &S_D_TOT = S_D_TOT + S_D / real(IC)
      slope=100.
      if (k_elastic.ne.0.)
     &slope = K_tangent / K_elastic *100.
      iters_tot = iters_tot + iter

c     VBAR='³'
      VBAR='|'

      WRITE (IO,'(A,I6 ,A,I5  '//
     &         ',A,G13.4, G13.4, A,F7.2,A,f7.2, a,i5, A,A8,A)')
     &    VBAR, IY
     &  , '  ', Iter
     &  , ' '//VBAR,  S_F_TOT, S_D_TOT
     &  , ' '//VBAR, slope
     &  , '  ', F_MSS_MAX *100.
     &  , '  ', ngp_failed
     &  , VBAR, TIME_C (),VBAR

      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE WRITE_DANFE_BANNER (version,colorise)
C
C     Just writes out the mast head of the Danfe Analysis Package
C     Colorize is a logical flag - whether to use fancy colours or not.
C     Dan Kidger 16-7-97
C
      CHARACTER VERSION*(20)
      LOGICAL colorise
      LOGICAL DOS           !- can use DOS character set?
      character  esc,black*7,red*7,green*7,yellow*7,blue*7,magenta*7,
     &     cyan*7,white*7

      esc = char(27)
      black  = esc//'[1;30m'
      red    = esc//'[1;31m'
      green  = esc//'[1;32m'
      yellow = esc//'[1;33m'
      blue   = esc//'[1;34m'
      magenta= esc//'[1;35m'
      cyan   = esc//'[1;36m'
      white  = esc//'[1;37m'

      IF (colorise) then
      WRITE (*,'(A)')
     & WHITE//'  +------------------------------------------'//
     &       '--------------------------------+'
      WRITE (*,'(A)')
     & WHITE//'  |              '//yellow//'D A N F E'//
     &  green//'   2D/3D Finite Element Analysis Program     '//
     & WHITE//'      '//WHITE//'|'

      WRITE (*,'(A)')
     & WHITE//'  |                             '//white//VERSION//
     &           '                         '//
     & WHITE//'|'

      WRITE (*,'(A)')
     & WHITE//'  |                   '//cyan//'Dr. Dan Kidger,'//
     &        ' School of Engineering            '//
     & '      '//WHITE//'|'

      WRITE (*,'(A)')
     & WHITE//'  |                  '//cyan//'University'//
     &  ' of Manchester, Manchester,UK             '//
     & '    '//WHITE//'|'

      WRITE (*,'(A)')
     & WHITE//'  |             '//cyan//'d.kidger@man.ac.uk'//
     & '  http://www.man.ac.uk/~mbgasdk            ' //
     & WHITE//'|'

      WRITE (*,'(A)')
     & WHITE//'  +--------------------------------------'//
     &                 '------------------------------------+'

      ELSE   !=== uncolored version ===
      WRITE (*,
     & '(    '' +'',   76(''-'')      ,''+'' /,       '//
     & '   5('' |'',   T15,A      ,T79,''|''/),       '//
     & '     '' +'',   76(''-'')      ,''+'' /       )'  )
     &   '      D A N F E  Finite Element Analysis Program'
     &  ,'                   '//VERSION
     &  ,' . . .             '
     &  ,'  Author : Dr. Dan Kidger, (ex. University of Manchester)'
     &  ,'daniel.kidger@quadrics.com '

      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE NEXT_OUTFILES (iop, n_cases, u3,file_root)
c
c  This open a new output file each time it is called.
c  it also records these new files to the end of the root file (u3=80)
c       Dan Kidger 27-7-98

c OLD notes:
c   so that *LOAD_STEP_TITLE, *EXCAVATE_LAYER, etc. appear at the head
c   of each section.
c
c.. do after *NULL_ARRAYS, or after *ANALYSE (but only if more data folows
c.. keep the root file open, and write "#includes" as we go along
c.. or force a *BEGIN_ANALYSE, which forces it.
c (on first pass *BEGIN_ANALYSE should trigger *null_arrays)
c
      integer u3
      character file_root*(*)
      character file_out*80
      data ipass/0/

      if (mod(n_cases-1,iop) .ne.0) then     !new file frequency
        return
      endif

      ipass=ipass+1
      if (u3.eq.80) then
        u3=u3+1            !- keep the original file open
      else
        close (U3)
      endif

      WRITE (FILE_OUT,'(A,A,I2.2)')
     & file_root(1:istr_end(file_root)),'.o',ipass
      open (u3, file=file_out)          !- also stress and strain?

      write(80, '(A,A,A,A)') '#include ',
     &  '"',file_out(1:istr_end(file_out)),'"'

c--- optionaly open seperate stress and strain files ---
c      WRITE (FILE_OUT,'(A,A,I2.2)')
c     &   file_root(1:istr_end(file_root)),'.s',N_CASES
c      open (u3s, file=file_out)          !- for stresses
c      WRITE (FILE_OUT,'(A,A,I2.2)')
c     &   file_root(1:istr_end(file_root)),'.e',N_CASES
c      open (u3e, file=file_out)          !- for strains

      RETURN
      END

!-----------------------------------------------------------------------
!             -  Message passing Interface -
!-----------------------------------------------------------------------
      SUBROUTINE START_MPP (IPROC,NPROC)
!
!     This Initialises a multi-threaded job.
!     This call the MPi init function and returns my process id
!     Dan Kidger 3-5-97
!

!.. if we have a real MPI binding present .. :-)
      include 'mpif.h'

      integer*4 ifail,iproc4,nproc4

      nproc=1  !default is to assume just a single processor version
      iproc=0

      call MPI_init (ifail)      ! Init MPI
      if (ifail.ne.0) then
        call myerror(3,'MPI init failed')
        nproc = -1   ! flag an error
        return
      endif
      call MPI_Comm_rank (MPI_COMM_WORLD, iproc4,ifail)
      call MPI_Comm_size (MPI_COMM_WORLD, nproc4,ifail)
!     if (iproc.eq.0) print*,'Running on ',nproc,' processors'

      iproc = iproc4
      nproc = nproc4
 
      END !SUBROUTINE START_MPP

c-----------------------------------------------------------------------
      SUBROUTINE END_MPP () !(IPROC)
c
c     Ends a Parallel job.
c
c     This is done at the end of an analysis
c     It may also be done perhaps when a processor 'gives up', for
c     example after an error condition, so perhaps MYERROR should
c     also call it ?
c     Dan Kidger 3-5-97
      integer*4 ifail

      call MPI_finalize (ifail)

      END !SUBROUTINE END_MPI

c-----------------------------------------------------------------------
      SUBROUTINE SPAWN_DANFE
c
c     Runs the DANFE main entrypoint via a SALFORD vitrual stack of 20Mb.
c     Dan Kidger 3-5-97
!     This routine is now considered obsolete
c
c     INTEGER STACKSIZE
c     PARAMETER (STACKSIZE=999 000 000)
c     INTEGER*1 STACK
c     COMMON /SPAWN_STACK/STACK
!     EXTERNAL DANFE_MAIN

c     CALL DANFE_MAIN ()
c     CALL SPAWN@ (DANFE_MAIN, STACK,STACKSIZE, HANDLE)

c--- a 'background beeper' (maybe try as a third thread?)
c     do i=1,10
c       call sleep@ (1.D0)
c       call sound@ (1024,1)
c       but if DANFE_MAIN has finished we jump out of this loop.
c       call yield@()
c     enddo
      END !SUBROUTINE SPAWN_DANFE

C=======================================================================

c-----------------------------------------------------------------------
      SUBROUTINE DANFE_SLAVE ()
c
c     This is the slave processor for DANFE
c       Dan Kidger 3-5-97
c
c   - It responds to incoming requests
c     Then handles them 'eg 'FORM_A_KM', ' ADV_FRONT, etc.
c   - Note that some requests wil return their output to the MASTER
C     eg. calc_a_bandwidth.
c   - Others have store data in the internal data structure for later.
c     eg. Build_Some_KMs

c hmm maybe extend these TAGS to ALL of DANFE/DANPLOT etc.
c  ie. every operation has a unique code. - whether we are multi-tasking
c    or not.


c     Call get_message_tag
c     IF (tag.eq.-1) THEN
c        goto 1    !- just cycle (or yield?)
c     ELSEIF (tag.eq. DANFE_FORM_KM) then

c     ELSEIF (tag.eq. DANFE_BC_BY_BOX) then

c     ELSEIF (tag.eq. DANFE_ADV_FRONT) then

c     ENDIF


      END !SUBROUTINE DANFE_SLAVE
c-----------------------------------------------------------------------

!----------------------------------------------------------------------------------------
      subroutine usage()
!
      write (*,'(a)')
     &"Usage:  danfe [-vh] [-g graphics] [input file(s)]"

      end
!----------------------------------------------------------------------------------------
      subroutine help()
!
      write (*,'(a)')
     & "Usage:  danfe [OPTION..]" , 
     & "-v  verbose (may be repeated)" ,
     & "(more options need to be added here)"

      end

!----------------------------------------------------------------------------------------
      subroutine error(string)
!
      character string*(*)
      write (*,'(a)') "error:", string
      end

