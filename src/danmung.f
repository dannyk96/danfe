      PROGRAM DANS_MESH_CONVERTER

      IMPLICIT NONE
      INTEGER MELS
      PARAMETER ( MELS =400 000 )   !-- Max # of elements (change as needed)

!------------------------------------------------------------------------c
!   ** DESCRIPTION **                                                    c
!                                                                        c
!      This program will convert Finite element mesh files from          c
!      one file format to another.                                       c
!      additionaly many popular raytracing/solid modelling packages are  c
!      supported.                                                        c
!------------------------------------------------------------------------c
!
!  .. this program is a cut-down version of full FE package: DANMESH/DANFE
!
      INTEGER MNN,INF,IGC,INUMS,IFCETS,IRINGS
      PARAMETER ( MNN = MELS     ! = 2 for 2d (=3 for 2d/3d)
     &           ,INF = 3        ! = 2 for 2d (=3 for 2d/3d)
     &           ,IGC = INF      !
     &         ,INUMS = 28       ! = max nodes per element
     &         ,IFCETS = MELS*6  ! Max # of facets
     &         ,IRINGS  = 5000   ! Max # of boundary edge nodes
     &            )

!----------- problem sized arrays --------------------------------------
      REAL  GC  (IGC,MELS)       !- co-ordiantes of all the nodes
      INTEGER NUMS (INUMS,MNN)   !- the elements' nodes etc.
!    &          ,P (MNN)         !- a 'workspace' of pointers
     &          ,P (IFCETS)      !- a 'workspace' of pointers 30-8-96
!    &     ,FACETS (3,IFCETS)    !- table of ALL the 3D faces (rev)
!    &     ,RINGS (irings)       !- polygons of the mesh boundary

 
!--------------- character strings -------------------------------------
      CHARACTER LINE*255     ! the whole command line
!    &         ,KEYWORD*70   ! data input keyword 'token'
     &         ,FILE_IN*80   ! input data file name
     &        ,FILE_OUT*80   ! output data file name
     &  ,EXT_IN*3, EXT_OUT*3 ! input /poutput file extensions

      INTEGER U1             ! input mesh file
     &       ,U2             ! output mesh file

      INTEGER NN,NEL,NDIM,IPR, IOS, nargs
      INTEGER IBEG,IEND,IC
      INTEGER IARGC           ! a function in int_gen.F to return command line args

!------------------ DATA stataments ------------------------------------
      DATA    U1/40/, U2/50/
      DATA NEL,NN/2*0/            !-- initially NO elements etc.
      DATA NDIM/0/                !-- default is 0-dimensions !
      ipr = 2  !- 2 = moderate feedback

!======================================================================
      WRITE (*,'(A)') '-------- DANMUNG Mesh File translator -------'
     &               ,'    by Dr. Dan.Kidger (d.kidger@quadrics,com)'

       nargs= iargc()     !- for IRIX etc.
!     28-06-01 hmm can I use STDIN and STDOUT instead (and so use a flag for output format)?

!  try
!  0. danmung foo.pl
!  1. danmung -t dp  foo.pl 
!  2. danmung foo.pl  bar.pl
!  3. danmung -t dp  foo.pl -o bar.d1
! ie. 
!  0 create some default format - danfe presumably
!  1 standard case - generate output filename based on type
!  2 explicit output filename   (yes I know it is the same type as the input)
!  
! passing options
!  many file formats, both input and output could have control options
!  like
!    ps  : node number on plot, shaded by imat, etc.
!    nff : what do I do with the material colours?
!
!  -  perhaps I could query the list of supported file format with a one-line
!         description?
!  - sometime file extension isn't the same as the filetype eg .wrl=VRML

       if (nargs/=2) then
         call danmung_usage ()
         stop
       endif
       call getarg(1,file_in)
       call getarg(2,file_out)

!---- recognise the file type by its extension (or other heurestic) ----

      CALL GET_MESHFILE_TYPE (FILE_IN,  EXT_IN )
      CALL GET_MESHFILE_TYPE (FILE_OUT, EXT_OUT)
!      print*,'-',ext_in,'-',ext_out,'-'
      open (u1,file=file_in,status='old',action='read',iostat=ios)
      if (ios.ne.0) then
       print*,'file_in=',file_in
       call myerror (2,'cannot open INPUT file')
      endif

      if (ext_out.eq.'pcx') then    !- we will open a binary file later.
      else
        open (u2,file=file_out,action='write',iostat=ios)   !- open for output
      endif
      if (ios.ne.0) then
        print*,'file_out=',file_out
        call myerror (2,'cannot open OUTPUT file')
      endif

!---- now read the file and re-write in the new format ------
!   contrast passing the filename, or a Fortran unit number.
      print*,'.... reading ',file_in
      ipr=2
      CALL R_MESHFILE   (U1,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P, EXT_IN)
      print*,'.... writing ',file_out
      CALL WR_MESHFILE  (U2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P, EXT_OUT)

!------------------------- end of program ----------------------------
      write (*,'(a)')
     &  '------------ translation completed -----------'
      END

!-----------------------------------------------------------------------
       subroutine danmung_usage ()
       write(*,'(a)') 
     & ' ',
     & ' Usage:   danmung file_in file_out [options]',
     & ' ',
     & '  where: /file_in/ has a suffix of:',
     & '    .PL .DP .GEO .SRF .3D .CAR .TEC .DXF .RAW .PHA .UCD',
     & '  and: /file_out/ has a suffix of:',
     & '    .PL .DAN .GEO .SRF  .DXF .RAY .SDA    .PCX .GIF .PS .WMF'

!   should support X11 too (via pgplot).
       end
!-----------------------------------------------------------------------
