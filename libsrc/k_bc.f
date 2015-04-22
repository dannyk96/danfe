C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C    BOUNDARY CONDITIONS READING MODULES
c
c     This section contains modules for reading (sorting?) and writing
c     the boundary conditions
c      cf. k_mesh.f  for 'mesh' type data
c
c      1/ the NF data
c      2/ the Load-step table (just INT& REAL pairs!)
c      3/ the DISPS and FORCES tractions
c      4/ Misc. eg. read mat props (just MxN REAL numbers?)
c
c
c    To keep more in-line with an interactive mouse-based mesh generator
C    I would prefer to split all modules into 2 :
C       The first would *select* a set of nodes
C       The second (common to all) would apply the 'condition' to all
C       nodes (or elements) that are in th ecurrent 'selected' list.
c
c    15-1-98  split into KEY_NF and KEY_LOADS
c
C-----------------------------------------------------------------------
c      SUBROUTINE KEY_BOUNDARIES
c     &          (FOUND,IPR,KEYWORD,IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL
c     &           FORCES_N,MDF, NODOF,ITYPE)
      SUBROUTINE KEY_NF
     &          (FOUND,IPR,KEYWORD,IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &           ,NF,INF, NODOF )
C
C     A Keyword handler for handling the boundary conditions (NF)
C        DJK 25-8-93
C    split from key_loadings 15-1-97
C
      REAL      GC (IGC,*)            ! Nodal coords
      INTEGER NUMS (INUMS,*)          ! Node steering
     &         ,NF (INF,*)            ! the node freedom data
c    &         ,P (*)                 ! dont need workspace here.s

c     integer IOPS(20)       !- a dummy copy of the real IOPS
      LOGICAL FOUND
      CHARACTER KEYWORD*(*)

      FOUND = .TRUE.      !-- assume its here : ELSEIF it to false

c-----------------------------------------------------------------------
C                       Boundary Conditions
c-----------------------------------------------------------------------
c.. nice to have by equation of a line (plane in 3D. eg. Kettleman Hills)
c..  (maybe use RINGS to enclose a plane-polygon)

      IF (KEYWORD.EQ.'*ON_FREEDOMS') THEN              !- set all to '1'
c       cf. the use of *NULL_ARRAYS which sub-calls ON_FREEDOMS
        IBIOT=0    !- hack to 'off' for now?
        CALL ON_FREEDOMS (NUMS,INUMS,NEL, NF,INF,NN,NODOF,IBIOT)

      ELSEIF (KEYWORD.EQ.'*NF') THEN                    !- explicit
        CALL R_BC_NODE (IO,GC,IGC,NN,NDIM,NF,INF,NODOF,IPR)

      ELSEIF (KEYWORD.EQ.'*FB') THEN                !- automatic! :-)
        CALL FIXED_BASE (IPR,GC,IGC,NN,NDIM,NF,INF,NODOF)

      ELSEIF (KEYWORD.EQ.'*RS') THEN                !- automatic! :-)
        CALL ROLLER_SIDES (IPR,GC,IGC,NN,NDIM,NF,INF,NODOF)

      ELSEIF (KEYWORD.EQ.'*RS_CYLINDER') THEN
        CALL ROLLER_SIDES_CYL (IPR,GC,IGC,NN,NDIM,NF,INF,NODOF)

      ELSEIF (KEYWORD.EQ.'*FB_RS') THEN             !- * the standard *
        CALL FIXED_BASE (IPR,GC,IGC,NN,NDIM,NF,INF,NODOF)
        CALL ROLLER_SIDES (IPR,GC,IGC,NN,NDIM,NF,INF,NODOF)

c      ELSEIF (KEYWORD.EQ.'*RB_RS') THEN
c        CALL ROLLER_BASE (IPR,GC,IGC,NN,NDIM,NF,INF,NODOF)
c        CALL ROLLER_SIDES (IPR,GC,IGC,NN,NDIM,NF,INF,NODOF)

      ELSEIF (KEYWORD.EQ.'*BC_BY_NODE') THEN        !- by sets of node number
        CALL R_BC_NODE (IO,GC,IGC,NN,NDIM,NF,INF,NODOF,IPR)

      ELSEIF (KEYWORD.EQ.'*BC') THEN                !- by token
c       CALL R_BC_GENERAL (IO,GC,IGC,NN,NDIM,NF,INF,NODOF,P)

c      ELSEIF (KEYWORD.EQ.'*BC_BY_DIR') THEN        !- by x/y/z = value
c        CALL R_BC_DIR (IO,GC,IGC,NN,NDIM,NF,INF,NODOF)   !- not yet done

      ELSEIF (KEYWORD.EQ.'*BC_BY_COORD') THEN       !- by coord (+wildcard)
        CALL R_BC_COORD (IO,IPR,GC,IGC,NN,NDIM,NF,INF,NODOF)

      ELSEIF (KEYWORD.EQ.'*BC_BY_BOX') THEN         !- by box
        CALL R_BC_BOX (IO,GC,IGC,NN,NDIM,NF,INF,NODOF)

c----------------------------------
      ELSEIF (KEYWORD.EQ.'*WRITE_NF') THEN          !- to .OUT file.
        IO2 = 80     !- hack unit for now.
        ibrief = 1
        CALL WR_BC (IO2,GC,IGC,NN,NDIM,NF,INF,NODOF, ibrief)

      ELSEIF (KEYWORD.EQ.'*WRITE_BOOK59') THEN
        IO2 = 81     !- hack unit for now
        OPEN (IO2,FILE='p59.dat')
        CALL WR_MESH_SG3 (IO2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL, 'P59')
        CALL WR_NF_SG3 (IO2,NF,INF, NN,NODOF)
        CLOSE(IO2)
      ELSEIF (KEYWORD.EQ.'*WRITE_BOOK68') THEN
        IO2 = 81     !- hack unit for now
        OPEN (IO2,FILE='p68.dat')
        CALL WR_MESH_SG3 (IO2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL, 'P68')
        CALL WR_NF_SG3 (IO2,NF,INF, NN,NODOF)
        CLOSE(IO2)

C------------------------ end of possibilities -------------------------
      ELSE
        FOUND = .FALSE.
      ENDIF

      RETURN
      END


C-----------------------------------------------------------------------
C   >>>>   Boundary Condition routines
C-----------------------------------------------------------------------
      SUBROUTINE R_NF (IO,GC,IGC,NN,NDIM,NF,INF,NODOF)
C
C     This is the simplest possible way of defining Boundary Conditions
C     based simply on their node number .. I can use this routine to
c     read a 'whole' NF table (eg after manual editing)
c      note that any nodes not specficied are 'defaulted' (usu. = 1)
C          D. Kidger  8-3-97
C     data as : <<inode, BC>>
C
      REAL GC(IGC,NN)
      INTEGER NF (INF,NN), NF_DATA(5)
      CHARACTER FMT*25

      DO KK = 1,99999
    1   READ (IO,*,IOSTAT=IOS)  inode,(nf_data(j),j=1,nodof)
        CALL IN_TEST (IO,IOS,*1,*999)
        DO J=1,NODOF
          NF (J,INODE) = NF(J,INODE) * NF_DATA(J)  !- product-in!
        ENDDO
        WRITE (FMT,'(A,I2,A)')  '(I3,A,I5,A,',NODOF,'I1,A)'
      ENDDO
 999  write (*,'(A,i6,a)')   '<>',KK,' nodes with BCs read in'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_BC_GENERAL (IO,GC,IGC,NN,NDIM,NF,INF,NODOF,P)
c
c    * Generic Boundary condition setting *
c        The sets the given nodes to the given fixity (eg 0 1 1)
c          Dan Kidger 20-2-98 (Tricia's Birthday)
c
      REAL GC(IGC,NN)
      INTEGER NF (INF,NN), P(*)
c     INTEGER NF_DATA(5)
c     CHARACTER LINE*255

      DO KK = 1,99999
c   1   READ (IO,*,IOSTAT=IOS)  (nf_data(j),j=1,nodof), line
c       CALL IN_TEST (IO,IOS,*1,*999)

c------- 1: read the first line -----
c  eg. 0 1 1   mat=2  nodes=(/3:12,17,19,21 /)

c------- 2: read the BC -----
c here we need to be careful of the column order
c  some nodes have rotational freedoms (+temperature?)
c     LINE(:IEND) = ' '    !- clear the 0 1 1 away.

c------- 3: read the BC and select those nodes -----
c     CALL SELECT_NODES (GC,IGC,NN,NDIM, LINE, P)
c... what if the line runs to several lines ?
c   need a continuation marker (&)
c  perhaps only explicit node lists will need this marker

c------- 4: simple loop to apply -----
c        DO I=1,N_DATA
c          INODE = P(I)
c          DO J=1,NODOF
c            NF (J,INODE) = NF(J,INODE) * NF_DATA(J)  !- product-in!
c          ENDDO
c        ENDDO


      ENDDO   ! kk
      END

C-----------------------------------------------------------------------
      SUBROUTINE SELECT_NODES (GC,IGC,NN,NDIM, LINE, P)
      REAL GC(IGC,NN)
c
c     This selects a set of nodes, based on parsing a string
c            Dan Kidger 20-2-98
c
c
c
c
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SELECT_ELEMENTS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,
     & LINE, P)
c
c     This selects a set of elements, based on parsing a string
c            Dan Kidger 20-2-98
c
c     ? perhaps it should read the file itself, so can handle muliple lines
c       (But that precludes general use from Danplot say.)
c
c     ? maybe I can use multiple selections, eg IMAT=5  IMAT=9
c     This can be used by:
c       - DANBLOCKS so can work on an IGRP say
c       - Drawing for zooming?
c       - Setting IMATS in the first place
c       - excavation and construction (so can do individual elements)
c              (Carolus would like this)
c
c
c     Possible selections:
c
c      ALL             !- (cf 230964)
c      IMAT=5
c      IGRP=7:9
c      IEL=201:230, 240:249
c      BOX=xXyYzZ
c      ELTYPE=3 20 1
c      ELTYPE=20nb
c
c    Numeric format as:
c       1: a single number
c       2: from:to
c       3: from:to:istep
c       4: a set of 1: and 2: seperated by commas
c       5: a box needs 4 (6) numbers, maybe as x>2.5&x<3.5 ?
c

      REAL GC(IGC,NN)
      INTEGER NUMS (INUMS,NEL)  !-- the topology of the elements

      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE R_BC_NODE (IO,GC,IGC,NN,NDIM,NF,INF,NODOF,IPR)
C
C     This is the simplest possible way of defining Boundary Conditions
C     based simply on their node number .. I can use this routine to
c     read a 'whole' NF table (eg after manual editing)
C
C     data as : #nodes, BC,  list_of_nodes
c     11-3-01 change NODE_LIST to be automatic, was only 99 :-(
C
      REAL GC(IGC,NN)
      INTEGER NF (INF,NN), NF_DATA(5), NODE_LIST (NN)
      CHARACTER FMT*25

      DO KK = 1,99999
    1   READ (IO,*,IOSTAT=IOS)  n_data,(nf_data(j),j=1,nodof),
     &                                (node_list(j),j=1,n_data)
        CALL IN_TEST (IO,IOS,*1,*999)

        DO I=1,N_DATA
          INODE = NODE_LIST(I)
          DO J=1,NODOF
            NF (J,INODE) = NF(J,INODE) * NF_DATA(J)  !- product-in!
          ENDDO
        ENDDO
        if (n_data.ne.1) then   !- do not echo too verbosely
          if (IPR>=2) then
        WRITE (FMT,'(A,I2,A)')  '(I3,A,I5,A,',NODOF,'I1,A)'
        WRITE(*,FMT) I,': ',N_DATA,' nodes found of BC type '''
     +         ,(NF_DATA(K),K=1,NODOF),''''
           endif
        endif
      ENDDO
 999  CONTINUE
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_BC (IO,GC,IGC,NN,NDIM,NF,INF,NODOF,ibrief)
C
C     Simply writes out the whole NF table
C     data as : #nodes, BC,  list_of_nodes
c      ibrief = 0 : all nodes are written
c             = 1 : only non-'non-zero' nodes are writtn
c     ?       = 2 : consequtive 'equal nodes' are lumped (cf my Ph.D.)
C        DJK 8-12-96
C
      REAL GC(IGC,NN)
      INTEGER NF (INF,NN)
      logical write

      if (ibrief.eq.2) then
        WRITE (IO,'(A)')  '*BC_BY_NODE'   !- is this what I want ?
      else
        WRITE (IO,'(A)')  '*NF'   !- is this what I want ?
      endif
c.. should I bundle similar nodes together ? .eg to a maximum of 10 ?
c.. ie loop I=1:NN, + get NF: if in_list compare, if diff print.
C.. if      'new_sec' start-list ??
      ic = 0
      DO I = 1,NN
         write = .true.
        jmin = 1
        do j=1,nodof
          jmin = min(jmin,abs(nf(j,i)))
        enddo
        if (ibrief .gt.0 .and. jmin.ne.0) write= .false.
        if (write) then
          WRITE (IO,'(9I8)') I,(NF(J,I),J=1,NODOF)
          ic = ic + 1
        endif
      ENDDO
      write (*,'(A,i6,a)')   '<>',IC,' nodes with BCs written'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_NF_SG3 (IO,NF,INF, NN,NODOF)
C
C     This writes out the fixities in 'Smith and Griffiths -3' formats
C     in particular P59 and P68
C      Dan Kidger 18-1-97
C
      INTEGER NF (INF,*)
      CHARACTER format*15
      INTEGER BUFFER(20)

c----- p6: Boundary fixities (NF)
c.. first we need to print the total number of nodes with fixities (NR)
      nr = 0
      DO I = 1,NN
        jmin = 1
        do j=1,nodof
          jmin = min(jmin,abs(nf(j,i)))
        enddo
        if (jmin.eq.0) nr = nr + 1
      ENDDO
      write (IO,*) nr

      nr = 0
      ic = 0
      DO I = 1,NN
        jmin = 1
        do j=1,nodof
          jmin = min(jmin,abs(nf(j,i)))
        enddo
        if (jmin.eq.0) then   !-  we have some zeros here
          nr = nr + 1
          ic = ic + 1
          BUFFER(IC) = i
          IF (IC.EQ.5 .OR. I.EQ.NN) THEN
            write (format,'(A,i1,a)')  '(99(i6,',NODOF,'i2))'
            WRITE (IO,FORMAT) (BUFFER(jj)
     &       ,(min(max(0,NF(j,buffer(jj))),1)         !- so ok after *SORTNF
     &       ,j=1,nodof),jj=1,IC)
            IC = 0
          ENDIF

        endif   !- if this node is not all 'free'
      ENDDO
      write (*,'(A,i6,a)')   '<>',nr,' nodes with BCs written'


c.. perhaps I should miss out these last two 'cos I can always append
c  a file of applied loads using a shell script ?

c----- p7: Applied loads (LOADED_NODES)
c... data as #node, then <<node#, fx,fy,fz >>
c (maybe I should allow my *NODAL_LOADS to input, hence export some loadings ?
c     WRITE (IO,'(A)')  '0',' '       !- dummy

c----- p8: Applied Dispalcements (LOADS)
c... data as #node, then <<node#, fx,fy,fz >>
c     WRITE (IO,'(A)')  '0',' '

      WRITE (*,'(a)')
     & '>> Please add the "applied loadings" manualy'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_BC_COORD (IO,ipr,GC,IGC,NN,NDIM,NF,INF,NODOF)
C
C     This reads in the nodal BOUNDARY CONDITIONS
C     based on their coordinates
C
      REAL GC(IGC,NN), POINT(5)
      INTEGER NF (INF,NN), NF_DATA(5)

      DO I = 1,99999
    1   READ (IO,*,IOSTAT=IOS)
     &  (POINT(J),J=1,NDIM), (NF_DATA(J),J=1,NODOF)
        CALL IN_TEST (IO,IOS,*1,*999)
        CALL BC_BY_COORD (ipr,GC,IGC,NN,NDIM,NF,INF,NODOF,point,nf_data
     &  ,npts)

        if (npts.ne.1) then   !- do not echo too verbosely
c       WRITE (FMT,'(A,I2,A)')  '(A,I6,A,',NODOF,'I1,A)'
        if (IPR>=2) then
        WRITE(*,'(A,I6,A,5I1)')
     &    '<>',npts,' nodes found of BC type '
     &       ,(NF_DATA(K),K=1,NODOF)
          endif
        endif
      ENDDO
 999  CONTINUE
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE FIXED_BASE
     &              (IPR,GC,IGC,NN,NDIM,NF,INF,NODOF)
C
C     This applies those BC's that 95% of all Geotechnics Meshes have:
C       Namely a fully fixed base and rollers on all vertical sides
C       .. this works for 2D, 3D, 4D?..
C
      REAL GC(IGC,NN), POINT(5), GC_MIN(5),GC_MAX(5)
      INTEGER NF (INF,NN), NF_DATA(5)

      CALL GET_MESH_RANGE (GC,IGC, NN,NDIM,  GC_MIN,GC_MAX,DIAG)
      IF (IPR.GE.3) THEN
        WRITE(*,'(A,5G14.4)') ' xyz min=', (GC_MIN(j),J=1,ndim)
        WRITE(*,'(A,5G14.4)') ' xyz MAX=', (GC_MAX(j),J=1,ndim)
      ENDIF
      IF (NDIM.LE.1) RETURN !- no vertical

c------------------------------ fixed base -----------------------------
      do j=1,ndim
        nf_data(j) = 0      !- all fixed
      enddo
      do j=1,ndim
        point(j) = 230964.  !- wildcard all
      enddo
      point(2) = gc_min(2)
      ipr2=0
      CALL BC_BY_COORD (IPR2,GC,IGC,NN,NDIM,NF,INF,NODOF,point,nf_data
     &  ,npts )
      IF (IPR.ge.2) WRITE (*,'(A,i7,a)') '<>',npts,' base nodes fixed'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE ROLLER_SIDES (IPR,GC,IGC,NN,NDIM,NF,INF,NODOF)
C
C     This applies those BC's that 95% of all Geotechnics Meshes have:
C       Namely a fully fixed base and rollers on all vertical sides
C       .. this works for 2D, 3D, 4D?..
C
      REAL GC(IGC,NN), POINT(5), GC_MIN(5),GC_MAX(5)
      INTEGER NF (INF,NN), NF_DATA(5)
      character dir*8
      data dir/'xyzw5678'/       !- direction labals

      CALL GET_MESH_RANGE (GC,IGC, NN,NDIM,  GC_MIN,GC_MAX,DIAG)
      IF (IPR.GE.3) THEN
        WRITE(*,'(A,5G14.4)') ' xyz min=', (GC_MIN(j),J=1,ndim)
        WRITE(*,'(A,5G14.4)') ' xyz MAX=', (GC_MAX(j),J=1,ndim)
      ENDIF
c     IF (NDIM.LE.1) RETURN !- no vertical

c----------------------------- roller sides ----------------------------
c.. careful if ndim /= nodof ..
c    eg do we wish to do anything with biot freedoms here ?
c.. no but we need a similar routine to clamp BIOT freedoms
      do i=1,ndim   !-- loop all sides
        if (i.ne.2) then   !- mustn't do 'y' again
          do j=1,ndim
            nf_data(j) = 1      !- all free
          enddo
          nf_data(i) = 0        !- just fix the 'current' direction

          do j=1,ndim
            point(j) = 230964.  !- wildcard all
          enddo
          point(i) = gc_min(i)   !--- 'left hand side of mesh'
          ipr2=0
          CALL BC_BY_COORD (IPR2,GC,IGC,NN,NDIM,NF,INF,NODOF
     &       ,point,nf_data, npts1)

          point(i) = gc_max(i)   !--- 'right hand side of mesh'
          ipr2=0
          CALL BC_BY_COORD (IPR2,GC,IGC,NN,NDIM,NF,INF,NODOF
     &       ,point,nf_data,npts2)
          IF (IPR.ge.2) WRITE (*,'(A,2i7,3a)')
     &      '<>',npts1,npts2,' (',dir(i:i),') side nodes fixed'
c-----------------------------------------------------------------------
        endif
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE ROLLER_SIDES_CYL (IPR,GC,IGC,NN,NDIM,NF,INF,NODOF)
C
C     This is a varient to *RS that sets the BC to all nodes that have
c     an XZ radius equal to the maximum
C        Dan Kidger  22-2-98
C     Inspired by Lee Elliot's Cylindrial sheet pile wall mesh
c     - also need for my ancient 'Spud can with 2 piles' mesh
c   * In 2d this is the same as roller LHS ?
C
      REAL GC(IGC,NN)
      INTEGER NF (INF,NN), NF_DATA(5)
c     character dir*8
c     data dir/'xyzw5678'/       !- direction labals

      IF (NDIM.LT.3) RETURN                !- no vertical axis
c------------- find Rmax --------------
      RMIN= 9.99e12
      RMAX=-9.99e12
      DO I=1,NN
        R = SQRT (GC(1,I)**2+GC(3,I)**2)
        RMIN=MIN(RMIN,R)
        RMAX=MAX(RMAX,R)
      ENDDO

      IF (IPR.GE.3) THEN
        WRITE(*,'(A,5G14.4)') ' Rmin,Rmax=', Rmin,Rmax
      ENDIF

c----------------------------- roller sides ----------------------------
c.. careful if ndim /= nodof ..
c    eg do we wish to do anything with biot freedoms here ?
c  .. note we use a tolerance of 0.1 millionth of the model's radius

      do j=1,ndim
        nf_data(j) = 1      !- all free
      enddo
      nf_data(1) = 0      !- fix x
      nf_data(3) = 0      !- fix z

      npts1=0
      npts2=0
      do i=1,nn
        R = SQRT (GC(1,I)**2+GC(3,I)**2)

        if ( abs(r-rmin)/rmax.lt.1.e-5   .and.       !- inner
     &   abs(rmin).gt. .0001) then                   !- but skip central axis
          npts1= npts1+1
          DO J=1,NODOF
            NF (J,I) = NF(J,I) * NF_DATA(J)  !- product-in!
          ENDDO

        elseif (abs(r-rmax)/rmax.lt.1.e-5 ) then    !-outer
          npts2= npts2+1
          DO J=1,NODOF
            NF (J,I) = NF(J,I) * NF_DATA(J)  !- product-in!
          ENDDO

        endif
      enddo

      IF (IPR.ge.2) WRITE (*,'(A,2i7,3a)')
     &      '<>',npts1,npts2,' inner/outer nodes fixed'
c-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE BC_BY_COORD (ipr,GC,IGC,NN,NDIM,NF,INF,NODOF
     &               ,point,nf_data,npts)
c
c     Applies the given Boundary Condition (NF_DATA)
c
      REAL GC(IGC,*), POINT(ndim)
      INTEGER NF (INF,*), NF_DATA(ndim)
c     CHARACTER FMT*25

C----- now consult the table to find the nodes
      IC = 0
      IFROM = 1
      DO IC = 1, NN
        CALL FIND_NODE (GC,IGC,NN,NDIM,POINT,IFROM,NN,INODE)
        IF (INODE.EQ.0) GOTO 99     !- no more found
        DO J=1,NODOF
          NF (J,INODE) = NF(J,INODE) * NF_DATA(J)  !- product-in!
        ENDDO
        IFROM = INODE + 1
        IF (INODE.GE.NN) GOTO 99     !- no more to find
      ENDDO
   99 IC = IC -1        ! revert the counter to the last 'successful'
c      IF (IPR.GE.2) THEN
c        WRITE (FMT,'(A,I2,A)')  '(A,I6,A,',NODOF,'I1,A)'
c        WRITE(*,FMT) '<>',IC,' nodes found of BC type '''
c     &       ,(NF_DATA(K),K=1,NODOF),''''
c      ENDIF
      npts=ic
      IF (IC.EQ.0) THEN
        print*,'point at',(point(i),i=1,nodof)
        CALL MYERROR (1,'no node found')
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_BC_BOX (IO,GC,IGC,NN,NDIM,NF,INF,NODOF)
C
C     this reads in a set of boundary conditions based on
C     'X-from, X-to, Y-from, Y-to, Z-from, Z-to' co-ords
C      where GC contains all the nodal co-ordinates
C
C     21-9-93 from L1A (READNF) to here as R_BC_BOX
C
      REAL GC(IGC,*), XMIN(5),XMAX(5)
      INTEGER NF (INF,*), NF_DATA(5)
      CHARACTER FMT*25

      TOL=  1.E-20

      DO I = 1,99999
    1   READ (IO,*,IOSTAT=IOS)     (NF_DATA(J),J=1,NODOF)
     &                       ,(XMIN(J),XMAX(J),J=1,NDIM)
        CALL IN_TEST (IO,IOS,*1,*999)

        IC = 0
        DO J=1,NN
          DO K=1,NDIM
            IF (GC(K,J).LT.XMIN(K)-TOL) GOTO 2    !- skip to next node
            IF (GC(K,J).GT.XMAX(K)+TOL) GOTO 2    ! (cf CYCLE)
          ENDDO
          DO K=1,NODOF                      !- found so set NF
            NF(K,J) = NF(K,J) * NF_DATA(K)
          ENDDO
          IC = IC + 1
    2     CONTINUE                    !-  skip-to-point
        ENDDO
        WRITE (FMT,'(A,I2,A)')  '(I3,A,I5,A,',NODOF,'I1,A)'
        WRITE(*,FMT) I,': ',IC,' nodes found of BC type '''
     &         ,(NF_DATA(K),K=1,NODOF),''''
        IF (IC.EQ.0) CALL MYERROR (1,'no nodes found')
      ENDDO

 999  CONTINUE       !- next keyword found so exit the routine
      RETURN
      END

C-----------------------------------------------------------------------

c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c     routines for handling the node freedom array NF
c-----------------------------------------------------------------------

C-------------------- calculate NODOF --------------------
c > perhaps each element (or material) carries a flag that determines
c   what type of freedoms it carries.  default =XY(z) disps,
c   so if *ONE_DOF, we have either PPs or Temperatures.
c    TYPES=
c       CONTINUUM   nodof=NDIM     standard structures (default)
c       HEADS       nodof=1        flow nets - equipotentials
c       FLOWS       nodof=1        flow nets - streamlines
c       TEMPERATURE nodof=1        thermal (cf. equipotentials)
c       BIOT        nodof=NDIM+1   consolidation.
c       NAVIER      nodof=NDIM+1   velocities and pressure.
c
C-----------------------------------------------------------------------
      SUBROUTINE CALC_NODOF (NUMS,INUMS,NEL, NF,INF,NN,NDIM
     & ,NODOF,DRPTVAC)
C
C     This routine calculates the no. of columns of freedoms for the mesh
C        Dan Kidger 22-4-98
C
C     Notes:
C       we return the number of freedoms in each catagory in DRPTVAC
C
C
      INTEGER INUMS,NEL,INF,NN,NODOF
      INTEGER NUMS (INUMS,NEL)    !- element steering
     &         ,NF (INF,NN)       !- node freedom array
     &        ,NUM (99)           !- an elements' node numbers'
c     character code*8
      integer drptvac (*)         !(9) x,y,z,pp,rx,ry,rz

c------------ 0: assume there are no freedoms of anything --------------
      do i=1,9
        drptvac(i) = 0
      enddo

c------------ 1: loop elements and .AND. in each ---------------
      DO IEL=1,NEL
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        IF (IMAT.LE.0) GOTO 22    !-- -ve materials are 'absent'

        icode=itype               !- pick up the element 'type'
c       icode=prptab(23,imat)     !
c       if (icode.eq.0) icode=1   !default prptab=0 . mung to 1=disps

        icode2=icode
        do ibit=1,7
          if (mod(icode2,2).eq.1) DRPTVAC(IBIT) = 1
          icode2=icode2/2
        enddo
  22    CONTINUE
      ENDDO

c----------- 2: sum up the types that we have got -----------
      nrot=0
      if (ndim.eq.2) nrot=1      !- potential bending moments
      if (ndim.eq.3) nrot=3
      if (ndim.eq.4) nrot=6
      nodof =0
      if (drptvac(1).gt.0) nodof=nodof+ndim         ! Displacements
      if (drptvac(2).gt.0) nodof=nodof+nrot         ! Rotations
      if (drptvac(3).gt.0) nodof=nodof+1            ! Pressure
      if (drptvac(4).gt.0) nodof=nodof+1            ! Temperature
      if (drptvac(5).gt.0) nodof=nodof+ndim         ! Velocity
      if (drptvac(6).gt.0) nodof=nodof+ndim         ! Acceleration
      if (drptvac(7).gt.0) nodof=nodof+1            ! Concentration
c     write (*,'(A,12i3)') 'DRPTVAC=',(drptvac(i),i=1,7)
      RETURN
      END

c-----------------------------------------------------------------------
c     SUBROUTINE  q_freedoms (icode,codes)
c
c     This returns the list of freedoms carried by each element type
c       d=disps xy(z)
c       p=pressure 1
c       b=bending x (yz)
c       t=temperature (t)
c    Contrast this with explicit bit-pattern setting:
c       1 - d - disps
c       2 - d - bending
c       3 - d - pp
c       4 - d - temperature
c       8 - d - velocoities
c       8 - d - accelerations
c    so of course we can make *ADD_BIOT loop through and set this bit
c    in PRPTAB()
c
c      integer icode
c      character codes*(9)               !-standard
c      if (icode.eq.0.or.icode.eq.1) then
c        codes='d'
c      elseif (icode.eq.5) then          !- biot
c        codes='dp'
c      elseif (icode.eq.0) then
c      endif
c
c      return
c      end

c-----------------------------------------------------------------------
      SUBROUTINE ON_FREEDOMS (NUMS,INUMS,NEL, NF,INF,NN,NODOF,IBIOT)
C
C     This 'switches' on the freedoms at all 'present' nodes (IMAT>0)
C     27-2-96 BIOT added (cf use of ITYPE>=2 to flag certain elements)
c
c     Notes:
c       1/ Any non-active nodes (eg excavation) are set to zero
c       2/ any non-active columns are set to zero (eg. Biot PP, BMs)
c       3/ originally called 'CORRECT_NF'
C       4/ forget IBIOT: use itype=5 for BIOT
c
      INTEGER INUMS,NEL,INF,NN,NODOF,IBIOT
      INTEGER NUMS (INUMS,NEL)    !-- eg. as max # elements
     &         ,NF (INF,NN)       !-- DITTO
     &        ,NUM (99)           !-- an elements' node numbers'
     &        ,NUMf(99)           !-- a biot daughters' NUM
      INTEGER I,J,IEL

c  (  Transient analyses may require a MASS matrix as well as KM)
c
c   Address here that we need a *BEGIN_ANALYSE, so that we can:
C    1/ calc NODOF at this point, and hence null the appropriate columns
c    2/ initialise other arrays such as STRESSES and STRAINS.
c     CALL CALC_NODOF (NUMS,INUMS,NEL, NF,INF,NN,NODOF,DRPTVAC)
c--------- 1: malloc NF ----
c    IF (ALLOCATED(NF)) DEALLOCATE (NF)
c    ALLOCATE (NF(NODOF,NN))

C--------- 2: switch OFF all freedoms -----------
c     NF=0
      DO I=1,NN
        DO J=1,NODOF       !- really the 'maximum #DOF' across the whole mesh
          NF(J,I) = 0
        ENDDO
      ENDDO

c---------- 3: switch ON only the 'present' elements' freedoms ---------
      DO IEL=1,NEL
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        IF (IMAT.GT.0) THEN    !-- (-ve) materials are 'absent'
c IMAT then gives us the type (eg. BIOT v. Temperature)
c  so find the columns and null just these. (maybe not even contiguous)
c note that Biot elements may be 8+4 so we need to flag those of the 8
c that need Biot freedoms: effectively we are generating a 'mask' (like G)
c that maps from a NOD by NODOF grid to G ?
c  > so loop these IDOF entries - pick up the x,y of each and set
c      NF(x,y) = 1 here
c   note here that we need flexibility in G() so we can pull out only
C   the temperatures, or only the disps say.

c 12-5-98 Consider then a more powerful G() that; for any element stores
c  which freedoms this element carries and identifies them
c  < compare this giving each number in LOADS() a key for the data type
c  so can CHECON can treat each seperately >
c   This relates to G storing the node# and the freedom column for each.
c   (3 rows?)
c   thence set IBIOT_COL to '3'(in 2d), so can test 'is this freedom a
c     pore pressure?'


c   call get freedom_list() ?

c     IF (ITYPE.eq.1.or.ITYPE.eq.5) then    !- realy if  bit(0) is set
          DO I=1,NOD
            DO J=1,NODOF        !(realy just NODOF of struct freedoms)
              icol = j          ! column number
              NF(icol,NUM(I)) = 1
            ENDDO
          ENDDO

C------------- handle BIOT freedoms -----------
C .. need to:
C     1/ get the NODF of the daughter
C     2/ identify which nodes in the parent are aligned (eg the corners)
C         -> really uses WTHATN technology
C     3/  hence loop these (global) nodes & set their freddoms 'on'

c---- 12-4-98 What about a generic template pattern? ----
c  as a NUMxNODOF table of 0s and 1s
c  here we .AND. in this into NF


c cf. the process of building G()
          IF (ITYPE.eq.5) then    !- realy if  bit(2) is set
c         IF (IBIOT.GE.1) THEN
          CALL GET_BIOT_DAUGHTER (NOD,NDIME,ITYPE, NODF, NUMF)
c           ICOL = NODOF+1     !- NF column number
            DO I=1,NODF
C             NF(ICOL,NUM(NUMF(I))) = 1
            ENDDO
          ENDIF
c---------------------
        ENDIF
      ENDDO
      END

C-------------------------------------------------------------------
c.. the next 2 routines could move to MATRIX.F since they do not use any
C   structures particular to my method of elements.
C-----------------------------------------------------------------------
      SUBROUTINE CORRECT_NF (NUMS,INUMS,NEL, NN,NODOF,NF,INF, N)
C
C     This does a logical AND between the BC-declared freedoms
C       and those freedoms in the current mesh
C     The remaining freedoms are then resequenced
c     .. the same as ON_FREEDOMS () but preserves existed 'zeros'
C         DJK 17-6-94
c     11-3-01 removed the call the SORTNF - do from teh calling program instaed
C
      INTEGER NUMS (INUMS,NEL)  !-- the topology of the elements
     &         ,NF (INF,NN)     !-- node freedom array
!    &        ,node_order(nn)   !- order to the freems
     &        ,NUM (32)

c------------ Mark all the freedoms as 'possible-freedoms --------------
c  only 'active' non-zero freedoms are considered.
      DO I=1,NN
        DO J=1,NODOF
          IF (NF(J,I) .NE.0) NF(J,I) = 99
        ENDDO
      ENDDO

c------------ now AND-in all the ACTUAL freedoms that are present ------
      DO IEL=1,NEL
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        IF (IMAT.GT.0) THEN                !- skip 'missing' materials
          DO I=1,NOD
            INODE = NUM(I)
c ** need to handle BIOT here too.
            DO J=1,NODOF         !- mark these
              IF (NF(J,INODE).NE.0) NF(J,INODE) = 2
            ENDDO
          ENDDO
        ENDIF
      ENDDO   !- elements

c.. so now NF contains 0 (-fixities), 99 (=unused freedoms), 2 (=current)
c------------------ Hence ZAP all the non-present freedoms -------------
      DO I=1,NN
        DO J=1,NODOF
          IF (NF(J,I).EQ.99) NF(J,I) = 0
        ENDDO
      ENDDO

c------------------ finally resequence these freedoms ------------------
c  11-3-01 removed - better to call from the calling routine (eg danfe)
c     so that we can handle optimised bandwidths, eg. cuthill_mcgee
c     CALL SORTNF (NF,INF,NN,NODOF,N)  !-- check for a valid 'N' after !
      RETURN
      END

C-------------------------------------------------------------------
      SUBROUTINE SORT_NF_ORDERED  (NF,INF,NN,NODOF,NODE_ORDER, N)
C
C     This resequences all the non-zero terms in NF to an ascending
C     sequence and returns the total number of freedoms in N
c     11-3-01 renamed from SORTNF() cos it now has an additonal argument.      
C
      INTEGER NF(INF,*), NODE_ORDER(*)
      LOGICAL USE_NODE_ORDER
      USE_NODE_ORDER = NODE_ORDER(1).ne.0  !- or some other test.
!       NODE_ORDER to be valid *has* to list every node, and each only once.
      N = 0
      DO I=1,NN
        INODE=I
        IF (USE_NODE_ORDER) INODE=NODE_ORDER(I)
        IF (inode.lt.0.or. inode.gt.nn) 
     &    call myerror (3,'NODE_ORDER is invalid (SORT_NF_ORDERED)')
        DO J=1,NODOF
          IF (NF(J,INODE).NE.0) THEN
            N = N + 1
            NF(J,INODE) = N
          ENDIF
        ENDDO
      ENDDO
      END

!-----------------------------------------------------------------------
      SUBROUTINE R_ORDER_NODES_POINT (IO,GC,IGC,NDIM,NN,NODE_ORDER)
!
!     This will sort the freedoms wrt distance from a point
!
      REAL GC(IGC,*), CENTRE(5)
      INTEGER NODE_ORDER(*)

    1 READ (IO,*,IOSTAT=IOS)  (CENTRE(J),J=1,NDIM)
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL ORDER_NODES_POINT (GC,IGC,NDIM,NN,CENTRE,NODE_ORDER)
      RETURN
 999  CALL MYERROR(2,'Missing coordinate in *ORDER_NODES_POINT')
      END

!-----------------------------------------------------------------------
      SUBROUTINE ORDER_NODES_POINT (GC,IGC,NDIM,NN,CENTRE,NODE_ORDER)

!     This creates a new ordering for teh nodes based on distance from a given point
!     This helps us produce a more optimal bandwidth
!     A Method like Cuthill_McGee is better, but this is simpler to impliment.
!     added 11-3-01 (DJK) - for Lamaload mesh.
!
      REAL GC(IGC,*), CENTRE(5)
      INTEGER NODE_ORDER(*)

      REAL DEPTHS(NN)     !- 2 'automatic temporary arrays.
      INTEGER P(NN)

      DO I=1,NN
        D = 0.
        DO J=1,NDIM
          D = D + (GC(J,I) - CENTRE(J))**2
        ENDDO
        DEPTHS(I) = D
      ENDDO

      CALL INDEX_REALS  (DEPTHS,NN, P)

!... we need to invert P() from indexing (as in depth_sort) to a ranking
!........ isn't this next bit the same as 'JUGGLE_P' ?? ....
!... cf as a subroutine like in "NR in F77"
      OPEN (97,STATUS='SCRATCH',FORM='UNFORMATTED')
      DO I=1,NN
        WRITE(97) P(I)
      ENDDO
      REWIND(97)
      DO I=1,NN
      READ(97) J
c       P(J) = I
      ENDDO
      CLOSE(97)

      do i=1,nn
        node_order(i) = p(i)
!       node_order(p(i)) = i     !- this may be simpler?
      enddo
      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE FORM_G (NF,INF,NOD,NUM,NODOF,G,IDOF)
c     SUBROUTINE NUM2G (NF,INF,NOD,NUM,NODOF,G)
C
C     This copies the NF data into the steering G (via NUM)
C     this orders the local freedoms as:
C        x,y of node 1, x,y of node 2, .. etc.
C        (then PP of node 1, PP of node 3 if BIOT)
c     Notes:
C     1/ This of course *has* to mimic the order in BEE (hence in KM)
c     2/ 30-5-97 Notes on BIOT Pore Pressures and Temperature effects:
c        - maybe both ON_FREEDOMS and FORM_G use the same prototype routine..
c        that returns the 'mask' (cf G)
c        note the presidence  (order?) of vars: disp, rot, pp, temp ?
C     3/ for a BIOT option, probably best to have a EXTEND_G_BIOT (q.v.)
c     4/ 29-10-97 or return any of the Gs - ALL, DISPS, PP, etc. ?
c        if we ask for PP of a non-BIOT element we simply get a nul set.
c     5/ We can use these Gs then in ON_FREEDOMS, as we loop and set
c        each's bit of NF
c     6/ coding: can an element have Biot+Temp say.
C        if so code as 'DPT' = disps+pressure+temp  ?
C        so we loop only these.     ( 'H','F' or 'HF'=heads and flowrates?)
C
      INTEGER NF(INF,*),NUM(*),G(*)
      IDOF=0
      DO I=1,NOD
        DO J=1,NODOF       !- 'structural' NODOF or total ?
          IDOF = IDOF+1
          G(IDOF) = NF(J,NUM(I))
        ENDDO
      ENDDO

      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE EXTEND_G_BIOT (NF,INF,NOD,NUM, G,IDOF)
C
C     This extends the G() vectors by adding the Pore pressure freedoms
C     after the (given) structural freedoms.
C     The given IDOF is also incremented
C     .. this routine may also be used in a Navier-Stokes to add a
C      pressure-term after the x,y velocities *usualy a pressure at all nodes?
C          DJK  25-7-96
C
      INTEGER NF(INF,*),NUM(*),NUM2(32),G(*)

c .. need NUMF too. -> therefore better to post call a FORM_BIOT_NUM
c    then concatonate
c   so do I use the ordinary NUM then convert internally or do I rely
c   on the driving routine ?
      CALL GET_BIOT_DAUGHTER (NOD,NDIME,ITYPE, NODF, NUM2)

c.. note that NUM2 is a list of the global node number for the 4 corners.
c   we need to be given NUM, hence use a set of rules to abstract the
c   daughter eg nodes (1..3..5..7) for an 8nq->4nq.
      DO I=1,NODF
        IDOF = IDOF+1
c       G(IDOF) = NF(NODOF+I,NUM(NUM2(I)) )
      ENDDO

      RETURN
      END

C-----------------------------------------------------------------------
C     >>>  Definitions of Loadings routines
C-----------------------------------------------------------------------
c      SUBROUTINE KEY_BOUNDARIES
c     &          (FOUND,IPR,KEYWORD,IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL
c     &           ,NF,INF, FORCES_N,MDF, NODOF, ITYPE )
      SUBROUTINE KEY_LOADS
     &          (FOUND,IPR,KEYWORD,IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &           ,FORCES_N,MDF, NODOF, ITYPE )
C
C     A Keyword handler for handling the boundary conditions
C        DJK 25-8-93
C
c     .. if DISP loading we really need to return a 'flag' that this
c        has happened OR use seperate vectors ?
C     .. MDF = the max # of DOF (not always the same as NDIM or NODOF)
C
      REAL      GC (IGC,*)            ! Nodal coords
     &   ,FORCES_N (MDF,*)            ! Applied forces
      INTEGER NUMS (INUMS,*)          ! Node steering
c    &         ,P (*)                 ! P is just workspace (1:NN)
c    &         ,NF (INF,*)            ! the node freedom data

c     integer IOPS(20)       !- a dummy copy of the real IOPS
      LOGICAL FOUND
      CHARACTER KEYWORD*(*)

      FOUND = .TRUE.      !-- assume its here : ELSEIF it to false

c-----------------------------------------------------------------------
C                             Loadings
c-----------------------------------------------------------------------

      IF (KEYWORD.EQ.'*LOADS') THEN         !- as inode, dx,dy,dz
        CALL R_LOADS (IO,GC,IGC,NN,NDIM,FORCES_N,MDF,NODOF)
        ITYPE = 1
      ELSEIF (KEYWORD.EQ.'*WRITE_LOADS') THEN
        io2 = 80    ! hack
        ibrief = 1  ! 0=full, 1= only non-zeros.
        CALL WR_LOADS (IO2,GC,IGC,NN,NDIM,FORCES_N,MDF,NODOF,ibrief)


      ELSEIF (KEYWORD.EQ.'*NODAL_LOADS_BY_NODE') THEN
        ITYPE = 1
        CALL R_LOADS_NODE (IO,GC,IGC,NN,NDIM,FORCES_N,MDF,NODOF)

      ELSEIF (KEYWORD.EQ.'*NODAL_LOADS_BY_BOX') THEN
        ITYPE = 1
        CALL R_LOADS_BOX (IO,IPR,GC,IGC,NN,NDIM,FORCES_N,MDF,NODOF)

      ELSEIF (KEYWORD.EQ.'*NODAL_LOADS') THEN  ! Point Loads (by coord)
        ITYPE = 1
        CALL R_LOADS_COORD (IO,GC,IGC,NN,NDIM,FORCES_N,MDF,NODOF)

C------------------ Gravity/Pressure loading ---------------------------
c.. these are proportional to the surface area/volume of an element.

c... maybe apply to only *some* of the elements
c... ?just zap Gy= (RHO) by hand AFTER gravity_loading (or cf EMC_PIPE style)
c     ELSEIF (KEYWORD.EQ.'*APPLY_GRAVITY') THEN
c        CALL APPLY_GRAVITY (GC,IGC,NN,NDIM,NUMS,INUMS,NEL,
c     &                    PRPTAB,IPRPTAB,FORCES_N,MDF,NODOF)
      ELSEIF (KEYWORD.EQ.'*PRESSURE_LOADS') THEN
        CALL R_LOADS_PRES (IO,GC,IGC,NN,NDIM,NUMS,INUMS,NEL,
     &                    FORCES_N,MDF,NODOF)

c----------- applied disps -----------
      ELSEIF (KEYWORD.EQ.'*NODAL_DISPS_BY_NODE') THEN
        ITYPE = 2
        CALL R_LOADS_NODE (IO,GC,IGC,NN,NDIM,FORCES_N,MDF,NODOF)

      ELSEIF (KEYWORD.EQ.'*NODAL_DISPS_BY_BOX') THEN
        ITYPE = 2
        CALL R_LOADS_BOX (IO,IPR,GC,IGC,NN,NDIM,FORCES_N,MDF,NODOF)

      ELSEIF (KEYWORD.EQ.'*NODAL_DISPS') THEN  ! Point Disps (by coord)
        ITYPE = 2
        CALL R_LOADS_COORD (IO,GC,IGC,NN,NDIM,FORCES_N,MDF,NODOF)

      ELSEIF (KEYWORD.EQ.'*RADIAL_DISPLACEMENTS') THEN  !Point Disps (by radius)
        ITYPE = 2
        CALL R_RADIAL_LOADS (IO,GC,IGC,NN,NDIM,FORCES_N,MDF,NODOF)

C------------------------ end of possibilities -------------------------
      ELSE
        FOUND = .FALSE.
      ENDIF

      RETURN
      END

C-----------------------------------------------------------------------
c   .. I will need some LOADS by MATERIAL (or BOX) and Facet #
c      for surface loading (eg. the footing from S+G)
C-----------------------------------------------------------------------
      SUBROUTINE R_LOADS (IO,GC,IGC,NN,NDIM,FORCES,IFORCES,NODOF)
C
C     This is the simplest possible way of defining Nodal loads
C     based simply on their node number
C
C     data as : <<inode, x/y/z load>>
C
      REAL GC(IGC,*), FORCES(IFORCES,*), DATA(5)

      DO kk = 1,99999
    1   READ (IO,*,IOSTAT=IOS) inode,(DATA(J),J=1,NODOF)
        CALL IN_TEST (IO,IOS,*1,*999)

        DO JJ=1,NODOF
          FORCES (JJ,INODE) = FORCES(JJ,INODE) + DATA(JJ)
        ENDDO
      ENDDO

 999  write (*,'(A,i6,a)')   '<>',KK,' nodes with LOADs read in'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_LOADS (IO,GC,IGC,NN,NDIM,FORCES,IFORCES,NODOF
     &  , ibrief)
C
C     Simply writes out the whole applied LOADS table (eg for plotting)
c      ibrief = 0 : all nodes are written
c             = 1 : only non-'non-zero' nodes are writtn
c     ?       = 2 : consequtive 'equal nodes' are lumped (cf my Ph.D.)
C        DJK 8-12-96
C
      REAL GC(IGC,*), FORCES(IFORCES,*)
      LOGICAL WRITE

      if (ibrief.eq.2) then
        WRITE (IO,'(A)')  '*NODAL_LOADS_BY_NODE'
      else
        WRITE (IO,'(A)')  '*LOADS'
      endif

      ic = 0
      tol = 1.e-20
      DO i=1,nn
        write = .true.
        fmax = 0.
        do j=1,nodof
          fmax = max(fmax,abs(forces(j,i)))
        enddo
        if (ibrief .gt.0 .and. fmax.lt.tol) write= .false.
        if (write) then
          WRITE (IO,'(I7,9G13.4)') I,(forces(J,I),J=1,NODOF)
          ic = ic + 1
        endif
      ENDDO

      write (*,'(A,i6,a)') '<>',ic,' nodes with LOADs written out'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_LOADS_NODE (IO,GC,IGC,NN,NDIM,FORCES,IFORCES,NODOF)
C
C     This is the simplest possible way of defining Nodal loads
C     based simply on their node number
C
C     data as : #nodes, x/y/z load,  list_of_nodes
C
      REAL GC(IGC,*), FORCES(IFORCES,*), DATA(5)
      INTEGER NODE_LIST(99)
      CHARACTER FMT*25

      DO II = 1,99999
    1   READ (IO,*,IOSTAT=IOS) n_data ,(DATA(J),J=1,NODOF)
     &                               ,(node_list(j),j=1,n_data)
        CALL IN_TEST (IO,IOS,*1,*999)

        IC = 0
        DO I=1,N_DATA
          INODE = NODE_LIST(I)
          DO JJ=1,NODOF
            FORCES (JJ,INODE) = FORCES(JJ,INODE) + DATA(JJ)
          ENDDO
        ENDDO
        WRITE (FMT,'(A,I2,A)')  '(I3,A,I5,A,',NODOF,'I1,A)'
        WRITE(*,FMT) iI,': ',N_data,' loading nodes found '
      ENDDO

 999  CONTINUE       !- next keyword found so exit the routine
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_LOADS_COORD (IO,GC,IGC,NN,NDIM,FORCES,IFORCES,NODOF)
c     was  SUBROUTINE R_N_LOADS
C
C     This reads in the nodal loads based on their coordinates
C     data as :  <<cx,cy,cz,   fx,fy,fz>>
C
      REAL GC(IGC,*), FORCES(IFORCES,*), POINT(5) ,data(5)

      ifail =0        !- number of failed loadings.
      DO I = 1,99999
    1   READ (IO,*,IOSTAT=IOS)  (POINT(J),J=1,NDIM),(data(j),j=1,nodof)
        CALL IN_TEST (IO,IOS,*1,*999)

C----------------now consult the table to find the nodes ---------------
c.. hmm but if INODE=NN, then IFROM=NN+1 !
c.. henc we get a reverse search (NN+1->NN) :-) - s we find INODE again!
        IFROM = 1
        DO IC = 1, 99999
          CALL FIND_NODE (GC,IGC,NN,NDIM,POINT,IFROM,NN,INODE)
          IF (INODE.LT.1) GOTO 99
          DO J=1,NODOF
            FORCES (J,INODE) = FORCES(J,INODE) + DATA(J)
          ENDDO
          IFROM = INODE + 1
          if (ifrom.gt.nn) goto 99
        ENDDO
  99    nfound = ic-1
        if (nfound.ne.1) PRINT*, I,': Loads found=',IC-1  !- save paper?
        if (nfound.le.0) ifail=ifail+1
        ENDDO
 999  CONTINUE
      if (ifail.ne.0) call myerror(1,'No node(s) found for some loads')
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE R_LOADS_BOX (IO,IPR,GC,IGC,NN,NDIM,FORCES,IFORCES
     & ,NODOF)
C
C     this reads in a set of nodal loads/disps based on a box
C     data as :  << xXyYzZ,  fx,fy,fz>>
C
C      20-6-94 modified from R_BC_BOX
C
      REAL GC(IGC,*), FORCES(IFORCES,*), DATA(5), XMIN(5), XMAX(5)
      CHARACTER FMT*25

      TOL=  1.E-20

      DO I = 1,99999
    1   READ (IO,*,IOSTAT=IOS) (XMIN(J),XMAX(J),J=1,NDIM)
     &                  ,(DATA(j),j=1,NODOF)
        CALL IN_TEST (IO,IOS,*1,*999)

        IC = 0
        DO J=1,NN
          DO K=1,NDIM
            IF (GC(K,J).LT.XMIN(K)-TOL) GOTO 2    !- skip to next node
            IF (GC(K,J).GT.XMAX(K)+TOL) GOTO 2    ! (cf CYCLE)
          ENDDO
          if (ipr>=4) print*,j, (gc(kk,j),kk=1,ndim)
          DO JJ=1,NODOF
            FORCES (JJ,J) = FORCES(JJ,J) + DATA(JJ)
          ENDDO
          IC = IC + 1
    2     CONTINUE                    !-  skip-to-point
        ENDDO
        if (ipr>=2) then
          WRITE (FMT,'(A,I2,A)')  '(I3,A,I5,A,',NODOF,'I1,A)'
          WRITE(*,FMT) I,': ',IC,' loading nodes found  '
        endif
        IF (IC.EQ.0) CALL MYERROR (1,'no nodes found')
      ENDDO

 999  CONTINUE       !- next keyword found so exit the routine
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_RADIAL_LOADS (IO,GC,IGC,NN,NDIM,FORCES,IFORCES,NODOF)
C
C     This reads in the nodal loads based on their radial coordinate
C     (usu. this is used to apply radial displacements to a borehole)
C       DATA as  (XC,YC) of the arc-centre, RADII (lo,hi) and the VALUE
C       DJK 18-2-94
C      .. written from a request by Derek Egan.
C
      INTEGER IO,IGC,NN,NDIM,IFORCES,NODOF
      REAL GC(IGC,*), FORCES(IFORCES,*)

      REAL XC,YC,RAD_LO,RAD_HI,VALUE,RAD_LO2,RAD_HI2,RAD2,DCX,DCY
      INTEGER II,I,IC

      DO II = 1,99999
    1   READ (IO,*,IOSTAT=IOS)  XC,YC,RAD_LO,RAD_HI, VALUE
        CALL IN_TEST (IO,IOS,*1,*999)

C----------------now consult the table to find the nodes ---------------
        RAD_LO2 = RAD_LO**2
        RAD_HI2 = RAD_HI**2
        IC = 0
        DO I = 1, NN
          RAD2 = (GC(1,I) - XC) **2 + (GC(2,I) - XC) **2
          IF (RAD2.GT.RAD_LO2.AND.RAD2.LT.RAD_HI2) THEN  !- a hit ?
            IC = IC + 1
            DCX= (GC(1,I) - XC) / SQRT(RAD2)   !- direction cosines
            DCY= (GC(2,I) - YC) / SQRT(RAD2)
            FORCES (1,I) = FORCES(1,I) + VALUE * DCX
            FORCES (2,I) = FORCES(2,I) + VALUE * DCY
          ENDIF
        ENDDO
c-----------------------------------------------------------------------
c 99    PRINT*, II,': Loads found=',IC
        ENDDO
 999  CONTINUE
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_LOADS_PRES (IO,GC,IGC,NN,NDIM,NUMS,INUMS,NEL,
     &                    FORCES_N,IFORCES_N,NODOF)
C
C     Reads and applies a set of pressure loads.
C     specified in terms of the element IMAT, the face # (1-6), and the value.
C        DJK 28-2-97

c     Notes:
C       1: For 2d elements we probably want to apply line loads but if we
c          have shell elements then we stil want real 'pressure'
c       2: There are several different possible pressures:
C          (a) 'standard' contstant 'normal' pressure: val
C          (b)  triangular 'hydrostatic' pressure: h0, pg
c          (c) x-y-z loads (eg wind loads.)
C          (d) others?
c       3: 22-11-98 Hydrostatic pressure would be very usefull
c          so we will need h0 (because often water surface will move)
c
      INTEGER IO,IGC,NN,NDIM,IFORCES_N,NODOF
      INTEGER nums(inums,*)
      REAL GC(IGC,*), FORCES_N(IFORCES_N,*)

c     REAL XC,YC,RAD_LO,RAD_HI,VALUE,RAD_LO2,RAD_HI2,RAD2,DCX,DCY
      INTEGER II,I,IC

C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD = 20        !-- max # nodes per element
     &          ,M_NDIM= 4 )       !-- max # freedoms per node
      PARAMETER (ICOORD = M_NOD,IPRESLO = M_NOD)
      REAL
     &      COORD (ICOORD,M_NDIM)  !- nodal coords
     &    ,PRESLO (IPRESLO,M_NDIM) !- nodal forces
     &       ,PRES (M_NDIM+1)      !- x,y,z components of pressure?
      INTEGER NUM (M_NOD)          !- node numbers of an element

      DO II = 1,99999
    1   READ (IO,*,IOSTAT=IOS)  JMAT,IFACE, (pres(i),i=1,ndim+1)
        CALL IN_TEST (IO,IOS,*1,*999)

C--------------- loop through the elements ---------------------------
      IC = 0
      FAC = 1.    !- fraction of the total pressure to add.
      DO IEL=1,NEL
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
        IF (IMAT.GT.0) THEN                !- skip 'missing' materials
        IF (IMAT.eq.JMAT .or.JMAT.eq.230964) THEN   !- wildcard ?
c       IF (IPR_EL.GE.3) CALL PR_EL_COUNT ('Elem',IEL,NEL,IPR_EL)
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords

        ic = ic + 1
        iaxes=1   !- 1=2d/3d, 2=axisym   !- hmm what about axisym ??
        CALL APPLY_PRES_1EL (COORD,ICOORD,NUM, NOD,NDIM ,NDIME,itype
     &               ,IFACE,PRES,PRESLO,IPRESLO, iaxes)
c hmm noce to return the total pressure so we can quote it in the print-out

        DO J=1,NODOF        !-- add into the global array
          DO I=1,NOD          !- cf NODOF (=NDIM+1 if BIOT)
            FORCES_N (J,NUM(I)) = FORCES_N (J,NUM(I))
     &                   + PRESLO(I,J)*FAC
          ENDDO
        ENDDO
        ENDIF     !-- only a particular material
        ENDIF   !-- only if element is 'present'
      ENDDO   !-- elements

c-----------------------------------------------------------------------
      write (*,'(A,i4,a,i6,a)') '<> jmat=',jmat,
     &  ' : Pressure applied to ',IC,' elements'

        ENDDO
 999  CONTINUE
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE APPLY_PRES_1EL (COORD,ICOORD,NUM,NOD,NDIM ,NDIME,itype
     &               ,IFACE,PRES, PRESLO,IPRESLO, iaxes)
C
C     This forms the pressure loading for a single eleemnt
C     from the 'Database' of elements in NUMS and GC
C     (Material props in PRPTAB)
c     .. why do we *not* use IAXES ?
C
      REAL      COORD (ICOORD, NDIM)  !-- nodal coords
     &        ,PRESLO (IPRESLO,NDIM)  !-- the resulting nodal forces
     &          ,PRES (4)             !- pressure/x,y,z forces

      INTEGER     NUM (NOD)           !-- node numbers

C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD = 20           !-- max # nodes per element
     &          ,M_NDIM= 4            !-- max # freedoms per node
     &          ,ISMP  = 27 )         !-- max GP's per element
      PARAMETER (IDER=M_NDIM,IJAC=M_NDIM)

      REAL    FUN (M_NOD)             !- shape funs
     &       ,DER (IDER,M_NOD)        !- derivs of shape funs
     &       ,JAC (IJAC,IJAC)         !- the 'Jacobian'
     &       ,SMP (ISMP,M_NDIM)       !- Integration sampling points
     &       ,WTS (ISMP)              !- Integration point 'weights'
     &       ,VEC (3)                 !- normal vector
c    &    ,COORD2()

      INTEGER  FS (27)                !- nodes on a facet
     &      ,NUM2 (32)                !- dummy node numbers

      CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)    !- cf user-defined
      CALL GET_ANY_GAUSS   (SMP,ISMP,WTS,NDIME,NGP,NOD,AREA)
      CALL NULL2D (PRESLO,IPRESLO, NOD, NDIM)

      DO I=1,NOD
        NUM2(I) = I
      ENDDO
      CALL GET_FACE (NUM2,NOD,NDIME,ITYPE,IFACE,FS,NN_F,NN_FT, 2)

C-------------------- loop the facet's nodes --------------------
c  METHOD 1 : find the explicit normal vector to this facet
c        DO I=1,NN_FT
c          K  = FS (I)                  !- node #
c          J  = FS ( MOD(I,NN_F) + 1)   !- node #+1
c          C1 = C1 + (GC(3,K)+GC(3,J)) * (GC(2,J)-GC(2,K)) /2.
c          C2 = C2 + (GC(1,K)+GC(1,J)) * (GC(3,J)-GC(3,K)) /2.
c          C3 = C3 + (GC(2,K)+GC(2,J)) * (GC(1,J)-GC(1,K)) /2.
c        ENDDO
c        C4 = ABS (C1*C1 + C2*C2 + C3*C3 )
c        IF (C4.GT.1.E-12) THEN      !- skip Zero-area-elems :-)
c        ENDIF


c------------------------ loop Gauss Points -----------------------
c  METHOD 2 : form the Jacobian at each Gauss Point.
c.. what if 2d what is JAC like?
c.. 22-11-98 We need to sample 'xyz' at each gp - hence from the mean 'y'
c    we can set pres=pg*(y-ho)
c   cf. the use of MAT_PROPS as in SAGE-PIPE
c
      DO IGP=1,NGP
        CALL GSF (2,NN_F,1, DER,IDER,FUN,SMP,ISMP,IGP)
c       do i=1,nod
c       yp = yp + fun(i)*coord(i,2)
c       enddo
        DO I=1,NN_F
          DER(3,I) = 0.      !- hack third dimension ?
        ENDDO

c.. alternative 'explicit' method
        DO I=1,2        !- NDIME of this 'FACE'.
          DO J=1,NDIM
            X=0.0
            DO K=1,NN_F
              X=X+DER(I,K)*COORD(FS(K),J)
            ENDDO
            JAC(I,J)=X
          ENDDO
        ENDDO
        VEC(1) = JAC(1,2)*JAC(2,3) - JAC(2,2)*JAC(1,3)
        VEC(2) = JAC(1,3)*JAC(2,1) - JAC(2,3)*JAC(1,1)
        VEC(3) = JAC(1,1)*JAC(2,2) - JAC(2,1)*JAC(1,2)

c       CALL MAT_MULT (DER,IDER,COORD,ICOORD,JAC,IJAC,NDIM,NN_F,NDIM)
        JAC(3,1) = 0.      !- hack third dimension ?
        JAC(3,2) = 0.      !- hack third dimension ?
        JAC(3,3) = 1.      !- hack third dimension ?
c       CALL INVERT_JAC (JAC,IJAC,DET,NDIM)
c        JACI11 =  JAC(2,2)*JAC(3,3) - JAC(3,2)*JAC(2,3)
c        JACI21 = -JAC(2,1)*JAC(3,3) + JAC(3,1)*JAC(2,3)
c        JACI31 =  JAC(2,1)*JAC(3,2) - JAC(3,1)*JAC(2,2)
c        DET = JACI11 + JACI21 + JACI31
        DET=0.25  !1.      ! hack to 1/4 here ?

        DET = DET*2.       ! totaly arbitary hack!

c        write (*,'(i4,a,3g12.3,a,g12.3)')
c     &    IGP,' normal=',VEC, ' det=',det

        WEIGHT= DET * WTS(IGP) * AREA

c.. use PRES(4) for the normal pressure.
c.. or use PRES(1:3) for the three components
        DO J=1,NDIM           !- explicit VVMULT
          DO I=1,NN_F   ! NOD
            II = FS(I)
            PRESLO(II,J) = PRESLO(II,J) + WEIGHT*FUN(I)*PRES(4)*VEC(J)
            PRESLO(II,J) = PRESLO(II,J) + WEIGHT*FUN(I)*PRES(J)
          ENDDO
        ENDDO
      ENDDO      !- loop GP's
c      do j=1,ndim
c        write(*,'(i3,99g12.3)') j,(preslo(i,j),i=1,nod)
c      enddo
      RETURN
      END

c-----------------------------------------------------------------------


c-----------------------------------------------------------------------
c                Misc. data reading modules
C-----------------------------------------------------------------------
      SUBROUTINE R_PRPTAB (IO,IPR,PRPTAB,IPRPTAB,NTYPES,NPROPS)
C       (*Obsolete*)
C     This reads in a table of data (eg. material properties)
C     NTYPES = returned # of material props on each line (obs?)
C     NPROPS = returned number of materials that were defined
C              (cf highest material # !)
C
      REAL PRPTAB(IPRPTAB,*)

      NPROPS = 0
    1 READ(IO,*,IOSTAT=IOS) NTYPES     !- the # of data points per line
      CALL IN_TEST (IO,IOS,*1,*999)

      DO I =1,99999
    2   READ(IO,*,IOSTAT=IOS) IMAT,(PRPTAB(J,IMAT),J=1,NTYPES)  !-  a line
        CALL IN_TEST (IO,IOS,*2,*999)
      ENDDO
  999 NPROPS = I-1
      IF (IPR.ge.2) PRINT*,NPROPS,' lines of material properties read'
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_PROPS_TOKEN (IO, PROP,MPROPW,MPROPS,NPROP,IPR)
C
C     This writes out the material property values
c          Dan Kidger 23-2-97
C
      REAL PROP(MPROPW,MPROPS)
      CHARACTER TOKEN*40                     !- a token and its value
      real value
      character names(50)*7                  !- non-zero properties names
      real     values(50)                    !- and values
      integer istr_end

      WRITE(IO,'(A)') '*MAT_PROPS'
     & ,'# These are the defined material properties'
     & ,'# note that only non-zero entries need to be printed'

      DO IMAT=1,MPROPS
c      DO IMAT=1,NPROP
        CALL TOKENCODE_MATS ('HITS', ihitcode,1)   !- get its IVAL
        ihits = nint(prop(ihitcode,imat))
        IF (ihits.ge.0) THEN    !- only materials that have been defined.

c... now loop and build up the line of output
C.. first count the number of non-zero entries.
          IC = 1
          token='MAT'
          write(names(ic),'(a7)') token(1:istr_end(token))
          values(ic) = imat
          DO J=1,MPROPW
            VALUE = PROP(J,IMAT)
            IF (j.eq.ihitcode) goto 22              !- cycle hitcode itself
            IF (abs(value).lt. 1.e-20) goto 22      !- cycle if zero
              CALL TOKENCODE_MATS (TOKEN, J,2)      !- get its token
              IF (TOKEN.eq.' ') goto 22             !- is it non-null?
                IC = IC + 1
c               names(IC)  = token(istr_beg(token):)   !- strip leading spaces
                write(names(ic),'(a7)') token(1:istr_end(token))
                values(ic) = prop (j,imat)
   22       CONTINUE
          ENDDO   !- loop across the table

c.. now write out the 'reduced' line
          IF (IC.GE.2) THEN
            WRITE(IO,'(a,a,f4.0, 99(a,a,g11.4))')
     &      (names(j),'=',values(j),j=1,ic)
          ENDIF
        ENDIF
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_PROPS_TOKEN (IO, PROP,MPROPW,MPROPS,NPROP,IPR)
C
C     This reads in the material property values based on Keywords
c          DJK 1995
c       21-8-95  now uses STR2TOK so multi- per line
C     .. if IPR>=2 then the lines are echoed to the console
c        7-12-96 now peels off one token at a time.
C
      REAL PROP(MPROPW,MPROPS)
      CHARACTER  LINE*255                    !- a read-in line
     &        , TOKEN*40, VALUE*40           !- a token and its value
c     PARAMETER (MAXTOKENS = 50)             ! (only expect about 10)
c     INTEGER  LIST(2,MAXTOKENS)             !- store of STRING pointers
      INTEGER ISTR_END
      EXTERNAL ISTR_END

      IMAT_LO = 1                            !- assume we mean Material #1
      IMAT_HI = 1
      NPROP = MAX(NPROP,1)                   !- at least one material. ?
      ILINE = 0                              !- debug counter
      DO II=1,9999                           !- dummy loop
   1    READ (IO,'(A)',IOSTAT=IOSTAT) LINE
        IF (IOSTAT.NE.0) CALL IN_TEST(IO,IOSTAT,*1,*99)   !-- ie EOF !
        IOSTAT = INDEX('Cc!#/<*;[',LINE(1:1))           !-('c' is a problem?)
c.. I would rather call a 'is_comment()'
        IF (LINE.EQ.' ') IOSTAT=1                     !--- A blank line (?)
        IF (IOSTAT.NE.0) CALL IN_TEST(IO,IOSTAT,*1,*99)   !-- comments

        ILINE = ILINE + 1                 !( count lines or #tokens?)
        IF (IPR.GE.2) WRITE (*,'(I6,2X,A)')
     &    ILINE,LINE(1:istr_end(LINE))   !- echo

        do i=1,len(line)         !- clip line at the first comment
          if (index('!;',line(i:i)).ge.1) then
            line (i:) =' '       ! or we could stop when a token starts
            goto 21              ! with a '!'
          ENDIF
        enddo
   21   continue

c.. get the set of tokens: .. I would rather abstract one at a time.
c.. loop forever..
c      get a token  .. if EOL the EXIT
c      convert to value & act upon it (eg # of args)
c          ie. get next token(s) .. if EOL then ERROR
c          convert these to numeric values eg 'model=VM'
c
c      next loop token
c
        iend=0          !- start from the beginning of the string
        DO KKK=1,9999
c       CALL GET_TOKENS (LINE,  LIST,MAXTOKENS,NTOKS)
c       DO K = 1,NTOKS,2
c          TOKEN = LINE (LIST(1,K  ):LIST(2,K  ))
c          VALUE = LINE (LIST(1,K+1):LIST(2,K+1))

          call get_next_token(line,ibeg,iend,' =,')
          if (ibeg.lt.0) goto 92      !- EXIT : no more tokens in the line
          token = line(ibeg:iend)
c         if (token(1:1).eq'!') EXIT
          CALL TO_UPPER (TOKEN)                      !- ->uppercase
c----- check which token we have found -----
c.. try 'bits'
c      CALL get_enum (token,ival,
c     &   'MAT=0,MODEL=18, GX=7,GY=8,GZ=9,CONS=10,EPKO=11,RHO=12'//
c     &   'PERMX=15,PERMY=16,PERMZ=17, SPRINT=19,EPRINT=20'
c    can overload BW perhaps for consolidation?

c.. try 'elasto-plastic'
c      CALL get_enum (token,ival,
c     &   'E=1,v=2,c=3,phi=4,psi=5,bw=6')

c.. try 'monot'
c      CALL get_enum (token,ival,
c     &   'V=1,AA=2,AP=3,BB=4,BP=5,CC=6,CP=7,DP=9'//
C     &   'FIMU=9,FICV=10,SCV=11,VGC=12,VGP=13'//
C     &   'NU=14,EE=15,EP=16,LB=17,CN=18,CG=19,CV=20,RT=21,LV=22'//
C     &   'ATM=23,TCS=24'//
C     &   'TGAMMA=25,TEPK0=26,TPARCI=27,TPARDI=28' )
c   notes:
c     1: maybe too many .. so store in a 'new' array
c     2: should they overlap the E-P or cam-clay params?
c
c.. try 'cam-clay'
c      CALL get_enum (token,ival,
c     &   'KAPPA=99,LAMDA=99,GAMMA=99,M=99,G=99')
c.. notes:
C     1: G is standard elastic, KAPPA replaces E, M replaces v, G/v'
C     2: p401 of B+G (for undrained NC-clay):
C           K=0.062, lamda=0.161, Gamma-1=1.759, M=0.888, G/v'=0.25
C            nop=0. gammaw=100. gamma=20. kx=0. ky=0.

          CALL TOKENCODE_MATS (TOKEN, IPOS,1)        !- get which column

          IF (IPOS.EQ.-1) THEN
            CALL MYERROR (1,'Unknown Token '//TOKEN)
            GOTO 10
          ENDIF

c--------------------- read the value ----------------------
c  sometimes the value is a string (delimited by ' '?)
c  maybe parse VAL: for only '0123456789.-+ED' characters
c  if not valid as a number, try a string, if unknown then invalid
c     Note the use of extensions so 'ON' returns 1, etc.
c     Also can #define variables
c     If I am clver enough we can parse math expressions too, eg. 2.5*DX
C
          call get_next_token (line,ibeg,iend,' =,')
          value = line(ibeg:iend)
          call to_upper (value)
c         print*,'Value=',value
c         READ (VALUE,*) VAL                 ! 'dodgy' F77
          val= STR_TO_REAL (value,ifail)     !- in keyword.f

c.. or use this internaly where appropriate ?
c          if (ifail.ne.0) then
c            call myerror (1,'Invalid Material property token found')
c          endif

c----------------------- Assign the value ------------------------------
c--------- 0 = A new material so initialise (Elastic, etc.)
c.. cf here parsing a list like in Word6.0 eg. 5,7,10-14
c    so if ='MAT' then read multiple CSVs (until a token leads with (A-z)
c  (22-11-98 changed 1.e-6 to 1.e-4 so OK in single precision)
c  (note glitch on kilburn when 01.03  gave only 1->2 :-(  )
          IF (TOKEN.EQ.'MAT') THEN
            IMAT_LO = INT (VAL+1.e-4)             !-- VAL=2.05  == 'from IMAT=2-->5'
            IMAT_HI = MAX(INT ((VAL-IMAT_LO)*100.+1.e-4),IMAT_LO) !-- simple?!
            NPROP = MAX(NPROP,IMAT_HI)          !-- Max. # of Materials
            DO IMAT = IMAT_LO,IMAT_HI
c             PROP(0,IMAT) = PROP(0,IMAT) + 1
              PROP(21,IMAT) = PROP(21,IMAT) + 1
            ENDDO
            goto 10    !- jump to get the next token
         ENDIF

c-------------- handle special cases -------------------
c.. eg enumerate codes to numbers (MONOT->10)
c.. unnumerate VALUE into VALUE2 .. strip leading spaces and ''-delims
c.. If i give an explicit '2' then this should come through explicitly.

c--------- Elastic/Plastic option ------
          IF (TOKEN.eq.'MODEL') THEN   !(cf 'model=')
            IF (IFAIL.NE.0) THEN
               CALL get_enum (
     &        'ELASTIC=0,MC=1,TRESCA=2,VONMISE=3,VMO=3,DP=4,MONOT=10'
     &        //',MCNT=5'
     &        ,value,ival)
c             .. what if there is an ifail ??
              VAL = ival    !- convert to REAL
c             print*,'imat=',' is of plastic type',value,'=',ival
            ENDIF
          ENDIF
c------------------------- 6: store the values -------------------------
c.. also invoke 'triggers' such as 'G' to get 'E', w,d->area
          DO IMAT = IMAT_LO,IMAT_HI
              PROP(IPOS,IMAT) = VAL   !-- simply set the value (eg.E)
c              IF (IPOS.EQ.10.OR.IPOS.EQ.11) THEN    !- 't' triggers A,I
c              ELSEIF (IPOS.EQ.4) THEN   !-- 'Sy' triggers MP and M0
c              ENDIF
             ENDDO
c          ENDIF
c--------------------------------
   10     continue      !- jump here if we found an 'MAT=' (or unknown)
        ENDDO     !- loop the tokens on a line
   92   CONTINUE    !- when each line is exhausted
      ENDDO      !- loop the lines in this module


      CONTINUE  !-- shouldn't ever get to this line!
   99 RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_CONTROL_TOKEN (IO, IOPS,MOPS, IPR)
C
C     This reads in the job control options eg 'plasticity=off'
C     based on R_PROPS_TOKEN
C        DJK 19-11-96
C
      INTEGER IOPS(MOPS)
      CHARACTER  LINE*255                    !- a read-in line
     &        , TOKEN*40, VALUE*40           !- a token and its value
      CHARACTER logicals*60
      integer str_to_i
      external str_to_i

      LOGICALS='FALSE=0,TRUE=1,F=0,T=1,OFF=0,ON=1,NO=0,YES=1'

      DO Itoken=1,9999                           !- dummy loop
   1    READ (IO,'(A)',IOSTAT=IOSTAT) LINE

        IF (IOSTAT.NE.0) CALL IN_TEST(IO,IOSTAT,*1,*99)   !-- ie EOF !
        IOSTAT = INDEX('Cc!#/<*;[',LINE(1:1))           !-('c' is a problem?)
c.. I would rather call a 'is_comment()'
        IF (LINE.EQ.' ') IOSTAT=1                     !--- A blank line (?)
        IF (IOSTAT.NE.0) CALL IN_TEST(IO,IOSTAT,*1,*99)   !-- comments
        IF (IPR.GE.2) WRITE (*,'(I6,2X,A)') ITOKEN,LINE   !- echo

c       token = line !strip trailing comments etc. )

c------------------ find what the keyword is (token) -------------------
      iend=0
      call get_next_token (line,ibeg,iend,' =,')
      token = line(ibeg:iend)
      call to_upper (token)
c     CALL get_enum ('LOG=1,PLASTIC=2,PLOT=3,NGP=10',token,ipos)

c---- DANFE set.
      CALL GET_ENUM (
     &    'STEPS_PER_FILE=1'
     & //',NONLINEAR=2'
     & //',SOLVER=3'
     & //',PLASTIC_ITERATIONS=4'
     & //',PLASTIC_TOLERANCE=5'
     & //',NGP=10,LEGO_KM=11,PRECON=12,PRECONDITIONER=12,NGP_BULK=13'
C    & //',COORDINATES=14,PLANE_STRESS=15'         !- not yet done
     & //',MASS_MATRIX=16'
     & //',STRESSPRINT=17,STRAINPRINT=18'         !- not yet done
     & //',EIGENVECTORS=19'
     &    ,token,ipos)

c---- DANFE 'debug'  tokens
c.. there are too many of these
c.. should realy just be print control options.
c     CALL GET_ENUM ('P_GENERAL=1,P_TIMINGS=2,AUTOPLOT=3,P_COUNTERS=6'
c    & //'P_TABLES=7,P_PLASTIC_ITS=8, P_CG_ITS=9 '

c     IF (ipos.eq.-999)  then     !- was not found

c.. try to read it directly ?
c     ival = str_to_i (value, ifail)
c     if (ifail.ne.0) then ... ! try enumerated value eg 'verbose'
c       ival = -9123            ! a flag.

c--------------- handle the special cases of each keyword --------------

      call get_next_token (line,ibeg,iend,' =,')
      value = line(ibeg:iend)
      call to_upper (value)

      if (token.eq.'LOG') then
c        CALL get_enum (
c     &  'SILENT=0,OFF=0,NORMAL=2,VERBOSE=5,FULL=9', value,ival)

      elseif (token.eq.'SOLVER') then
        CALL get_enum ('SPARIN=0,PCG=1,FULL=2,BANRED=3'//
     &     ',CHOLIN=4,PCG_TRI=6', value,ival)
c       elseif (token.eq.'PRECONDITIONER') then
c        CALL get_enum ('OFF=0,NONE=0,DIAGONAL=1,CHOLESKI=2', value,ival)

c      elseif (token.eq.'PLOT') then
c        CALL get_enum (logicals, value,ival)

      elseif (token.eq.'NONLINEAR'.or.token.eq.'LEGO_KM'
     &    .or.token.eq.'PLANE_STRESS') then
        CALL get_enum (logicals, value,ival)

      elseif (token.eq.'PRECON'.or.token.eq.'PRECONDITIONER') then  !(12)
        CALL get_enum ('NONE=0,DIAG=1,CHOLESKI=2' ,value,ival)

c     elseif (token.eq.'NGP_BULK') then         !(13) - normal
c     elseif (token.eq.'COORDINATES') then             !(14)
        CALL get_enum ('CARTESIAN=1,AXISYM=2,SPHERICAL=3',value,ival)

      elseif (token.eq.'MASS_MATRIX') then             !(16)
        CALL get_enum ('HINTON=0,SIMPLEX=1,FULL=2', value,ival)

c     elseif (token.eq.'STRESS_PRINT_FREQ') then       !(17) -normal
c     elseif (token.eq.'STRAIN_PRINT_FREQ') then       !(18) -normal

c     elseif (token.eq.'STRESS_PRINT_FREQ') then       !(19) -unused
c     elseif (token.eq.'STRESS_PRINT_FREQ') then       !(20) -unused


      else            !- for 'NGP','NGP_BULK',etc.
        ival = str_to_i (value, ifail)
c       if (ifail.ne.0) then stop!
      endif

c     if (ival.eq.magic_val) then
c     call myerror (2,'CONTROL option/value not understood)'//value)
c     endif

      iops(ipos) = ival                !- hence store the value

      enddo        !- loop the keywords.
   99 continue
c     if (ipr.ge.2)
c     write(*,'()') '<> number of tokens=',itoken
      return
      end

C-----------------------------------------------------------------------
      SUBROUTINE TOKENCODE_MATS (TOKEN, IVAL,IOP)
C
C     This converts a token to its integer value (and vice versa)
C     All tokens are uppercase here (so must pre-convert)
C     IOP = 1 string->index          IOP = 2 index->string
C     returned IVAL = -1 if the token was not found (or other magic number?
C         DJK 21-8-95
c     I really need the inverse too - so I can get 32.5 when I ask for 'PHI'
c
c  17-11-96 alternative method is a 'scan-string' approach
c  22- 2-97 added 'AREA' and 'I' for rods and beams
c  23- 2-97 added 'HITS' : counts no. of time this mat has been defined to.
c  26- 6-97 added 'Crate' and 'H0' for C,phi?=f(depth)
c  15- 7-97 added 'COH' as a synonym for 'C'
c

c notes: 1/ Not necessarily 1:1 eg. KX,KY for fluids == E,v for solids ?!
c        2/ some tokens yield their own enumerated types, eg. MODEL & PRINT
c   15- 7-97 so what about 'E' as a f(depth) too?
c   15- 7-97 To be 'Bopre'-like I need HH so plastic strain makees it stronger
C              as:    C* = COH +HH*DEBARP/SQRT3 *COS(THETA)
c            (note that DEBARP is 'DSBAR' of pl.strain (=store of EVP))

      CHARACTER TOKEN*(*)
      INTEGER IVAL,IOP
      PARAMETER (NT = 25+2,lmax=6)
      CHARACTER  WORDS (NT)*(lmax)        ! list of known tokens
      INTEGER     VALS (NT)               ! list of their values
      DATA (WORDS(I),VALS(I),I=1,NT) /
     &     'MAT   ',0
     &    ,'E     ',1   ,'V     ',2                  !- elastic
     &    ,'COH   ',3       !- prefered name?
     &    ,'C     ',3   ,'PHI   ',4   ,'PSI   ',5    !- Mohr-Coulomb
     &    ,'BW    ',6                                !-  Fluid K  (usu. unused)
     &    ,'GX    ',7   ,'GY    ',8   ,'GZ    ',9    !- Gravity x-y-z (cf mass)
     &    ,'CONS  ',10  ,'EPK0  ',11  ,'RHO   ',12   !- Consol (init) /mass
     &    ,'CRATE ',13  ,'H0    ',14    !- c=f(depth)
c   (13,14 were unused)
     &    ,'PERMX ',15  ,'PERMY ',16  ,'KZ    ',17   !- Permeability x-y-z
     &    ,'MODEL ',18                               !- 0=elastic,1=MC,2=VM,..
     &    ,'SPRINT',19  ,'EPRINT',20                 !- stress/strain print-out
     &    ,'HITS'  ,21                        !- has this mateial been defined?
c   (22 is unused)
c      .. do width/depth of beams/pane-stress elems here too?
     &    ,'AREA'  ,23  ,'I',24,  'J',25             !- beams and rods.
     &    ,'ERATE'  ,26                              !- E = f(depth-H0)
     &     /
c.. I like PERMX,PERMY better than Kx,Ky
c.. xPRINT will have bit fields ORed together
c.. for MONOT, have to be careful of conflicts cg B,BP,C,CP, .. C/=cohesion
c   also cf 'v' and 'NU'
c     'KT'    = 3           !- coeff. of Thermal expansion
c     'SY'    = 4           !- Yield Stress
c     'HSEA'  = 17          !- height for initial stress calcs ?
c.......... Loadings that are element based (summed values too ?)
c. Do pore pressures here ??
c     'PI'   )   IPOS = 31  !-- Int. pressure (cf 25)
c     'WSEA' )   IPOS = 32  !-- self-weight of seawater
c     'DT'   )   IPOS = 33  !-- Temp. rise

c--------------- CBS's Monot params ------------------
c   1  V
C   8  AA,AP, BB,BP, CC,CP, DD,DP
C      FIMU,FICV, SCV, VGC,VGP
C   2  NU,EE,EP    :-)        elastic params?
c   6  LB,CN,CG,CV,RT,LV      misc. params
C   1  ATM                    atmospheric P - acts as a reference point
c      TCS                    cohesive 'shift' (not in IW's code)
c   2  PARCI,PARDI        (minimum) initial values for the hardening params ??
c                         -> YIELDC,(SLC=temp. copy), YIELDD,SLD
c
c      GAMMA,EPK0        initial stresses (as M-C)
c      PERMX,PERMY       permeability (as M-C)
c
c------------------- MONOT params --------------------------
c      ,'V     ' ,30
c      ,'AA    ' ,31,   'AP    ' ,32
c      ,'BB    ' ,33,   'BP    ' ,34       !- first the '10' basics
c      ,'CC    ' ,35,   'CP    ' ,36
c      ,'DD    ' ,37,   'DP    ' ,38       !hmm no DD :-(
c      ,'EE    ' ,39,   'EP    ' ,40
c
c      V     = PROP(1,NTYP)                 ! *not* Poisson's ratio ?
c      AA    = PROP(2,NTYP)          !(cf just an 'A')
c      AP    = PROP(3,NTYP)
c      BB    = PROP(4,NTYP)
c      BP    = PROP(5,NTYP)
c      CC    = PROP(6,NTYP)
c      CP    = PROP(7,NTYP)
c      DP    = PROP(8,NTYP)
c      FIMU  = PROP(9,NTYP)
c      FICV  = PROP(10,NTYP)
c      SCV   = PROP(11,NTYP)
c      VGC   = PROP(12,NTYP)
c      VGP   = PROP(13,NTYP)
c      NU    = PROP(14,NTYP)                ! == Poisson's ratio ?
c      EE    = PROP(15,NTYP)                ! == Young's Modulus ?
c      EP    = PROP(16,NTYP)
c      LB    = PROP(17,NTYP)
c      CN    = PROP(18,NTYP)
c      CG    = PROP(19,NTYP)
c      CV    = PROP(20,NTYP)
c      RT    = PROP(21,NTYP)
c      LV    = PROP(22,NTYP)          !- IW puts COH here
c      ATM   = PROP(23,NTYP)             !- no ATM in IW's
c      TCS   = PROP(24,NTYP)          !- cohesive strength ?

c      TGAMMA = PROP(25,NTYP)
c      TEPK0  = PROP(26,NTYP)         !- IW has KA (==BW)
c      TPARCI = PROP(27,NTYP)         !- IW has K0 (==EPK0)
c      TPARDI = PROP(28,NTYP)         !=== these 2 = initil values??

c      TPERMX = PROP(29,NTYP)         !-- Permeabilities I guess
c      TPERMY = PROP(30,NTYP)

c---------------- CBS's M-C params -------------------
c      V      = PROP(1,NMAT)    !- Poisson's Ratio in XY-plane
c      E      = PROP(2,NMAT)    !- Young's Modulus
c      C      = PROP(3,NMAT)    !- Cohesion
c      PHI    = PROP(4,NMAT)    !- Internal Friction Angle
c      PSI    = PROP(5,NMAT)    !- Dilation Angle

c      XGAMMA = PROP(25,NMAT)
c      XEPK0  = PROP(26,NMAT)      !- why Xprefix?
c      XPERMX = PROP(29,NMAT)
c      XPERMY = PROP(30,NMAT)      !- permeabilbities

c--------------------- find the token --------------------
c.. maybe call subroutines for this.
c.. to print the current options: loop, int->name, get orig_value+compare
c     , write name+value+flag('*')
c.. if COA = 2. 3. 4.  then need to parse the line(s) sequentially
c    cos the keyword indicates how many values follow (cf derived types)

c      IF (IOP.EQ.1) THEN           !--- find its code #
c        CALL FIND_TOKEN (TOKEN, WORDS,VALS,NT, IVAL ) !- or a function?
c      ELSEIF (IOP.EQ.2) THEN       !--- find its keyword name
c        CALL FIND_TOKEN_VAL (IVAL, WORDS,VALS,NT, TOKEN) !- or a function?
c      ELSE
c        CALL MYERROR (3,'Unknown get token/value (TOKENMODE_MATS)')
c      ENDIF

      IF (IOP.EQ.1) THEN           !--- find its code #
        IVAL = -1                  !(assume unfound)
        DO I=1,NT
          IF (TOKEN.EQ.WORDS(I)) THEN
            IVAL = VALS(I)
            RETURN
          ENDIF
        ENDDO

      ELSEIF (IOP.EQ.2) THEN       !--- find its keyword name
        TOKEN = ' '                !(assume unfound)
        DO I=1,NT
          IF (IVAL.EQ.VALS(I)) THEN
            TOKEN = WORDS(I)
            RETURN
          ENDIF
        ENDDO
      ELSE
        CALL MYERROR (3,'Unknown get token/value (TOKENMODE_MATS)')
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_LOAD_STEPS (IO, Q_NUMBER,Q_PERCENT,MQINC,IQ_TOT)
C       ( sort of *obs - see r_TIME_STEPS)
C     This reads in the stages of appling the load
C     as the count and the percentage each time
c     .. should I divide by 100. here ?
C        eq. 4 20. // 20 1.    == "4 of 20% then 20 of 1%"
C
      INTEGER Q_NUMBER  (MQINC)
      REAL    Q_PERCENT (MQINC)
      DO I = 1,MQINC
    1   READ(IO,*,IOSTAT=IOS) Q_NUMBER(I),Q_PERCENT(I)
        CALL IN_TEST (IO,IOS,*1,*999)
      ENDDO
      CALL MYERROR (2, 'Too many load steps (IQ_TOT>MQINC)')
  999 IQ_TOT = I-1
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_TIME_STEPS (IO, Q_NUMBER,Q_PERCENT,Q_DTIME, MQINC
     &   ,IQ_TOT)
C
C     This reads in the stages of appling the load
C     as the count and the percentage each time + Dtime too.
c         c. Dan Kidger 30-5-97
c
c     .. should I divide by 100. here ?
C        eq. 4 20. // 20 1.    == "4 of 20% then 20 of 1%"
c     30-5-97 the DTime figure is also useful in non-BIOT problems
c         as it leads animations with a pseudo-timestep.
C
      INTEGER Q_NUMBER (MQINC)      ! Number-of- this step
      REAL   Q_PERCENT (MQINC)      ! %age of the target load to apply
     &        ,Q_DTIME (MQINC)      ! increment of time (seconds)

      DO I = 1,MQINC
    1   READ(IO,*,IOSTAT=IOS) Q_NUMBER(I),Q_PERCENT(I), Q_DTIME(I)
        CALL IN_TEST (IO,IOS,*1,*999)
      ENDDO
      CALL MYERROR (2, 'Too many load steps (IQ_TOT>MQINC)')
  999 IQ_TOT = I-1
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

