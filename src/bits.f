c-----------------------------------------------------------------------
c    Extra Routines for the Danplot package
c
c    .. more of the main prog source code should end up here !
c        eg depth-sorting facets, etc.
c
c-----------------------------------------------------------------------


C-----------------------------------------------------------------------
      SUBROUTINE SHOW_STATUS (TIME,NN,NEL,NLDS,NFCETS_D,NFCETS,FILE_DAT)
C
C     This shows in the main drawing window 
C     a table of status information about the mesh (+timings)  
C     a bit obsolete now because of the permanent status-line
C
C...  show other anotation too !
C..... eg. load step # , viewing angle, light angle, disp scale, etc.
C---> 29-11-93 all the INTEGER*2 removed (for FTN90) cos' it still works

      REAL TIME(20)
c     real*4  T
      CHARACTER FILE_DAT*(80) !,LINE*80
      T = 1.
c     CALL SET_TEXT_ATTRIBUTE@ (1,T,T,T)

c     write(line,'(a)')'   < '//
c    +      FILE_DAT(1:leng(file_dat)-1)//' >'
c       call DRAW_TEXT@ (line(1:leng(line)),200, 70,14)
c     write(line,'(a,i5)')  '   NN =',NN
c       call DRAW_TEXT@ (line(1:leng(line)),200,115,14)
c     write(line,'(a,i5)')  '   NEL=',NEL
c       call DRAW_TEXT@ (line(1:leng(line)),200,130,14)
c     write(line,'(a,i5)')  '#loads=',NLDS
c       call DRAW_TEXT@ (line(1:leng(line)),200,145,14)
c     write(line,'(2(a,i4))')  '#facets=',NFCETS_D,'/',NFCETS
c       call DRAW_TEXT@ (line(1:leng(line)),200,160,1)

c     write(line,'(a,f10.3)')  '--- timings ---'
c       call DRAW_TEXT@(line(1:leng(line)),200,300,1)
c     write(line,'(a,f10.3)')  'total =',time(5)-time(1)
c       call DRAW_TEXT@(line(1:leng(line)),200,315,15)
c     write(line,'(a,f10.3)')  'transf=',time(2)-time(1)
c       call DRAW_TEXT@(line(1:leng(line)),200,330,1)
c     write(line,'(a,f10.3)')  'd.sort=',time(3)-time(2)
c       call DRAW_TEXT@(line(1:leng(line)),200,345,1)
c     write(line,'(a,f10.3)')  'draw  =',time(4)-time(3)
c       call DRAW_TEXT@(line(1:leng(line)),200,360,1)
c     write(line,'(a,f10.3)')  'buffer=',time(5)-time(4)
c       call DRAW_TEXT@(line(1:leng(line)),200,375,1)

c       write(line,'(a,f10.3)')  'reading=',time(11)-time(10)
c         call DRAW_TEXT@(line(1:leng(line)),200,390,15)
c       write(line,'(a,f10.3)')  'FSTRIP =',time(12)-time(11)
c         call DRAW_TEXT@(line(1:leng(line)),200,405,15)

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE MUNG_KEY_CODES (codes)
C
C     This simple translates a simple 'keycode' into an ascii character
C     ie. so cursor keys become normal ASCII codes.
C     .. <space> is returned as '_', CR as '~'
C     .. <nul> as 'z', the cursor-keys as 128_et_seq.
C     8-6-00 convert 2,4,6,8 to cursor keys (for PGPLOT)
c
      integer codes(*)
c     CHARACTER CKEY*1
      key = codes(1)
      codes(2) = -99                 !- ie. a non-value

      IF (KEY.EQ.0)  THEN            !- 'no-op'
        Codes(1) = ichar('z')   
      ELSEIF (KEY.EQ.32) THEN        !- alias for 'redraw' option
        Codes(1) = ichar('_')   
      ELSEIF (KEY.EQ.13) THEN        !- alias for 'redraw' option
        Codes(1) = ichar('_')   

      ELSEIF (KEY.EQ.331) THEN      ! - cursor   : l
        Codes (1) = 128          
c       Codes (1) = iCHAR('V')           
c       codes (2) = 
      ELSEIF (KEY.EQ.333) THEN      !           : right
         Codes(1) = 129           
      ELSEIF (KEY.EQ.328) THEN      !           : down
         codes(1) = 130
      ELSEIF (KEY.EQ.336) THEN      !           : up
         codes(1) = 131

      ELSEIF (key.eq.ichar('2')) THEN   !- path for keypad cursors
        codes(1) = 131
      ELSEIF (key.eq.ichar('4')) THEN
        codes(1) = 128
      ELSEIF (key.eq.ichar('6')) THEN
        codes(1) = 129
      ELSEIF (key.eq.ichar('8')) THEN
        codes(1) = 130

c      ELSEIF (KEY.GE.0.and.KEY.LE.255) THEN
c        CKEY = CHAR(KEY)
c      ELSE
c       CKEY = 'Z'        !- patch to show key is >255 (eg F9) ? obs?
c.. ** Not only obsolete but *breaks* the cursor key remapping !
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRITE_HELP ()
C
C     Just writes out the 'usage' information for DANPLOT v2
C     .. nicer to add ANSI -escape sequences
C     (.. maybe add the MENU drawing routines ??)
C
      WRITE(*,'(a)')
     + 'DANPLOT finite element visualization package. Version 4.21'
     +,'Copyright 1995-1998  Dr. Dan Kidger, all rights reserved.'
     +,' '
     +,'Usage: DANPLOT [keyfile] [-d file ] [-p#] [-PS] [-?]'
     +,' '
     +,'[keyfile]  Specifies keyword based data file'
     +,' '
     +,'-d file    Imports a mesh file where file ends in :'
     +,'               .PL    for old versions of DANPLOT' 
     +,'               .PL2   for old version containing "extra lines"'
     +,'               .GEO   for "Object File Format" '
     +,'               .NFF   for "Neutral File Format" '
     +,' '
     +,'-PS        Postscript output'
     +,'-p#        The palette number to use (1-6)'
     +,'-%         Swaps X and Z on the screen '
     +,'-A         makes Z the vertical axis'
     +,' '
     +,'To get a full copy of the DANPLOT package please contact: '
      WRITE(*,'(a,t40,a)')
     + 'Dr. Dan Kidger'
     +                    ,' Tel   : 061-275-7038'
     +,'Manchester Computing,'
     +                    ,' FAX   : 061-274-4361'  
     +,'University of Manchester, UK'
     +                    ,' E-MAIL: d.kidger@mcc.ac.uk'
      RETURN
      END

C----------------------------------------------------------------------
C        The next few subroutines are abstracts from DANLIB
c    these are only really used in the contouring of SHEAR STRAINS etc.
C----------------------------------------------------------------------

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C    The CORE routine for returning the coord/disp, etc. at a point.
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
c     SUBROUTINE SAMPLE_all (VALS_NODES ,NEN,NDIME,NDIM, VAL
c
c     Need a macro-sampler ?
C      so can sample over a facet, or over the whole element (eg strain)

C-----------------------------------------------------------------------
      SUBROUTINE SAMPLE_1 (VALS_NODES ,NEN,NDIME,NDIM, VAL
     &    , SMP,ISMP,IGP, IOP)
C
C      This is a simpliflied SAMPLE
C      just returns the value at a single point ?
C      eg. from a smoothed-stress field. (IOP is unused!)
C
      REAL VALS_NODES(NEN), VAL

      PARAMETER (IDER=3,ICOORD=20)
      REAL DER(IDER,icoord), FUN(icoord), SMP(ISMP,*)

      CALL GSF (NDIME,NEN,1, DER,IDER,FUN,SMP,ISMP,IGP)
      VAL = 0.
      DO I=1,NEN
        VAL = VAL + FUN(I) * VALS_NODES(I)
      ENDDO

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SAMPLE (SC,ISC,DISP,IDISP,NOD,NDIME,NDIM
     &    ,VAL,VEC, SMP,ISMP,IGP, IOP)
C
C      This is a general purpose SAMPLING routine to give
C      the disp,strain etc. at a given sampling point
C         Dan Kidger  1993
C
C     Input Arguements:
C       COORD(:,:)    the coords of the nodes (copied from SC(:,:)
C       DISP (:,:)    the 'disps' of the nodes
C       SMP(ISMP,*)   the table of samplig points
C       IGP           the entry on SMP to use
C       NDIM,NDIME    the DOF of the geometry (COORD) and the Element (cf SMP)
C
C     Output Arguments:
C       VAL           the returned point value
C       VEC(:)        the x/y/z 'vector' at the point (eg. disp)
C
C     Control (IOP=) :
C       10      coord in VEC  
C       14/15   normal-vector in VEC (only if a 2D elem in 3D)
C       20-23   coord in VEC  
C         20    RMS Coord in VAL
C         21..  X-Coord in VAL (et seq. for 22 & 23)   (xy-coord??)
c         29.. element area (from vector-products?)
C       30-33   disp  in VEC  
C         30    RMS Coord in VAL
C         31..   X-Disp in VAL (et seq. for 32 & 33)   (éëxy etc. ??)
C       40-49  Strain components  (plane-strain ?)
C        40    = ?
C        41    îx
C        42    îy
C        43    îxy
C        44    îz
C        45   ( îzx  )
C        46   ( îzy ?) sigm
C        47    îx      dsbar
C        48    îx      theta
C        49    DET itself :-)
C       50-59  Elasstic Stress components (E=1.,v=.3) (cf strains
C               .. really could pass E,v & do properly :-)
C
C    Thoughts:
C       Need for more components: eg. theta of the xy disp
C       Also note that :: 
C          scalars  eg. Pore Pressure, Temperature, 'smoothed stress'  
C                 - just return themselves 
C          vectors  eg. coordinates, displacements
C                  - x/y/z components (3), RMS xy,yz,zx,xyz (4)
C                    thetas xy,yz,zx (3), also x-yz,(3) etc
C          tensors  eg. stress, strain (either given or from disps?)
C                   -x/y/z components & çxy etc. too (6)
C                   -å1 etc. principal values (3) (also as vectors?)
C                   - direction cosines of the å1 etc. (eigenmodes)
C                   DSBAR,SIGM,THETA (3).. more.. ? (p/q?)
C
C.. so use 2 sub-subroutines :
C      a) Vector (3 vals) and an opcode -> rms, thetas, etc.(13)
C      b) Tensor (4/6 vals) + opcode -> å1, etc.  (14+)
C
C    .. also concept of NODOF and if /= NDIM

C---------------------------- arguements -----------------------------
      REAL VAL, VEC(*), SC(ISC,*), DISP(IDISP,*),SMP(ISMP,3)
      INTEGER NOD,NDIME,NDIM

C---------------------- FE5LIB interface ----------------------------
      PARAMETER (M_NOD =  27       !-- max # nodes per element
     &          ,M_NODOF= 4        !-- max # freedoms per node
     &          ,M_IH   = m_nodof*(m_nodof+1)/2     !-- max # stresses per GP
     &          ,MDOF   = M_NOD*M_NODOF  !-- max # freedoms per element
     &                      )

      PARAMETER (IDER=M_NODOF,IDERIV=M_NODOF,IJAC=M_IH,ICOORD=M_NOD,
     &     IBEE=M_IH,IDEE=M_IH)
      REAL    FUN (M_NOD)          !-- shape funs
     &       ,DER (IDER  ,M_NOD)   !-- derivs of shape funs
     &     ,DERIV (IDERIV,M_NOD)   !-- derivs of shape funs - global
     &       ,JAC (IJAC,IJAC)      !-- the 'Jacobian'
     &       ,BEE (IBEE,MDOF)      !-- the 'B' matrix
     &       ,DEE (IDEE,IDEE)      !-- the 'D' matrix
c    &       ,WTS (ISMP)           !-- Integration point 'weights'
      REAL 
     &   COORD(ICOORD,3)
     &  ,EPS(9), STRESS(9),DET , XY(5), eld(M_NOD*M_NODOF)

C---------------------------------------------
      IOP_2 = mod(iop,10)    !- the 'secondary' part of the code
      IOP_3 =     iop/10     !- the 'primary' part of the code

C------------- 1: get coords of this facet/element -----
      DO J=1,NDIM
        DO I=1,NOD
          COORD(I,J) = SC(I,J)       !- why do we need 2 copies ?
        ENDDO
      ENDDO

C------------ 2: Shape functions (and derivs) ------------------------
      CALL GSF (NDIME,NOD,1, DER,IDER,FUN,SMP,ISMP,IGP)

c================= Handle all the cases =====================
      IF (IOP.EQ.10) THEN                 !-- the co-ords of the point
        DO J=1,NDIM                       ! same as =20 surely
          VEC(J) = 0
          DO I=1,NOD
            VEC(J) = VEC(J) + FUN(I) * COORD(I,J)
          ENDDO
        ENDDO
        RETURN

c-----------------------------------------------------------------------
c.... normal vector to the face (X-product of 2 in-plane vectors)
c.... 'Shouldn't I be using the deformed position ??'
c  (yes you should)
      ELSEIF (IOP.EQ.14.or.iop.eq.15) THEN

        DO I=1,NDIME
          DO J=1,NDIM
            X=0.0
            DO K=1,NOD
              X=X+DER(I,K)*COORD(K,J)
            ENDDO
            JAC(I,J)=X
          ENDDO
        ENDDO
        VEC(1) = JAC(1,2)*JAC(2,3) - JAC(2,2)*JAC(1,3)
        VEC(2) = JAC(1,3)*JAC(2,1) - JAC(2,3)*JAC(1,1)
        VEC(3) = JAC(1,1)*JAC(2,2) - JAC(2,1)*JAC(1,2)
        RETURN

c-----------------------------------------------------------------------
      ELSEIF (IOP_3.eq.2.or.iop_3.eq.3) THEN     !-----coords/disps-----
c   was..     IOP .GE.20.AND.IOP.LE.39) THEN 
        DO J=1,3
          VEC(J) = 0
          DO I=1,NOD
            IF (IOP.le.29) THEN
              VEC(J) = VEC(J) + FUN(I) * SC  (I,J) ! coord
            ELSEIF (IOP.le.39) THEN
              VEC(J) = VEC(J) + FUN(I) * DISP(I,J) ! disp
            ENDIF
          ENDDO
        ENDDO
        IF(IOP_2.EQ.0) val = sqrt(vec(1)**2+vec(2)**2+vec(3)**2)
        IF(IOP_2.EQ.1) val = vec(1)
        IF(IOP_2.EQ.2) val = vec(2)
        IF(IOP_2.EQ.3) val = vec(3)

        if (vec(2).eq.0.) vec(2) = 1.e-20
        IF(IOP_2.EQ.4) val = atan2(vec(1),vec(2) )
        IF(IOP_2.EQ.5) val = atan2(vec(2),vec(3) )
        IF(IOP_2.EQ.6) val = atan2(vec(3),vec(1) )

c---- patch to return the facet area ---
        IF (IOP.eq.28.or.iop.eq.29) then
          C3 = 0.
          DO I=1,NOD               !- 10-7-97
            IP1 = MOD (I,NOD) + 1
            C3 = C3 + (SC(I,2)+SC(IP1,2)) * (SC(IP1,1)-SC(I,1))/2.
          ENDDO
          VAL = C3
c         if (iop.eq.28) val = 1./val     !- mesh 'density' 
          if (iop.eq.28) val = sqrt(abs(val))  !- edge 'spacing'
        endif   

c-------------- strains 'n elastic stresses ------------------
      ELSEIF (IOP.ge.40.and.iop.le.59) THEN  
        NODOF=NDIM            ! ie. ignore BIOT type things for now
        IH=4
        IF (NODOF.EQ.3) IH=6

        DO J=1,NODOF          !- explicit abstraction - bypasses G()
          DO I=1,NOD
            ELD(J+(I-1)*NODOF) = DISP(I,J)
          ENDDO
        ENDDO
        IDOF = NOD * NDIM        
        CALL MAT_MULT (DER,IDER,COORD,ICOORD,JAC,IJAC,NDIME,NOD,NDIME)
        CALL INVERT_JAC (JAC,IJAC,DET,NODOF)
        IF (ABS(DET).LT. 1.E-10) THEN       ! Abort if 
          VAL =0.                           ! the sampling point
          RETURN                            ! has zero area ?
        ENDIF
        CALL MAT_MULT (JAC,IJAC,DER,IDER,DERIV,IDERIV,NDIME,NDIME,NOD)
        IAXES=1               !- hack to cartesians
C      CALL NULL   (BEE,IBEE,4,2*NOD)
        CALL FORM_ANY_BEE (BEE,IBEE, DERIV,IDERIV, NOD,NDIME,
     &     XY,FUN,IAXES, IH )

       CALL MVMULT (BEE,IBEE,ELD,IH,IDOF,EPS) !- strain incrment
       if (nodof.eq.2) then    !- morph to 3d
            exy=eps(3)
         eps(3)=eps(4)     !- move ez into column 3
         eps(4)=exy
         eps(5) = 0.
         eps(5) = 0.
         eps(6) = 0.
       endif

C      DO I=1,NOD 
C        EPS(1) = EPS(1) + DERIV(1,I) * DISP(I,1)
C        EPS(2) = EPS(2) + DERIV(2,I) * DISP(I,2)
C        EPS(3) = EPS(3) + DERIV(2,I) * DISP(I,1)
C     &                  + DERIV(1,I) * DISP(I,2)
C      ENDDO

C--------------- Now get the 'elastic' stresses ------------------------
      IF (IOP.GE.50) THEN 
c       CALL FMDEPS (DEE,3,1.E6,0.3)    !-- is this plane-stress ?
        CALL FORM_DEE (DEE,idee,1.e6,.3,0.,  2,1)   ! arbitary E and v

        CALL MVMULT (DEE,idee, EPS,6,6, STRESS)
        DO J=1,3
          EPS(J) = STRESS(J)
        ENDDO
      ENDIF

C----------------------- get the principal values ----------------------
      IF (IOP_2.GE.7 .and. IOP_2.LE.9) THEN   ! if INVAR: put shear in 3
c     IF (IOP_2.GE.6 .and. IOP_2.LE.8) THEN   ! if INVAR: put shear in 3
        IF (IOP_3.eq.4) then
          EPS(4) = EPS(4) /2.  
          EPS(5) = EPS(5) /2.      !- form engineering shear strain...
          EPS(6) = EPS(6) /2.  
        ENDIF
        CALL INVARIENTS (EPS,nodof, EPS(7),EPS(8),EPS(9))
c----   CALL INVAR (EPS, sigm,  DSBAR ,THETA ) ------
c      ELSEIF (IOP_2.EQ.9) THEN
c        EPS(9) = DET
      ENDIF
      VAL = EPS (IOP_2)

c-----------------------------------------------------------------------
      ENDIF   ! end of IOP option_choice
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_DEFAULT_DISP_SCALE (GDISPS,IGD,NN,NODOF, 
     &               NLDS,DIAG,FACT)
C
C     This calculates a 'good' displacement scale factor
C     based on ALL the load steps (or just the current ?)
C     note that this sort-of uses Salford iNTL () to use long-integers
 
c----- FACT therefore is *NOT* the times-reality factor 
C      but that  * diag/25. .. so a mesh of diag=250m would
C      only show 1/10th of the correct DISP at FACT=1.  ???


      REAL GDISPS(IGD,*)

      FACT = 0.
      DISM  = 1.e-20
      IF (NLDS.LE.0) RETURN            !- no results to process
c     IBASE = INTL(NLDS-1) * NN
      IBASE = (NLDS-1) * NN
      DO I=1,NN
        DISM1 = 0
        DO J=1,NODOF
          DISM1 = DISM1 + GDISPS(J,IBASE+I)**2
        ENDDO
        DISM = MAX(DISM,DISM1)
      ENDDO
      DISM = SQRT(DISM)
      FACT = DIAG / DISM / 25.      ! should be 'device-independant'!
c       print*,'.. diag=',diag,' dism=',dism,' nlds=',nlds
      RETURN
      END


c-----------------------------------------------------------------------
c     SUBROUTINE GET_MOUSE_OR_KEY_EVENT (IXM,IYM, codes, Q_FUNC)
c     SUBROUTINE GIN (X,Y,itype, x1,y1,x2,y2, x3,y3)
c      INTERRUPT SUBROUTINE M_TRAP
c      INTERRUPT SUBROUTINE B_TRAP
c     6-6-95 All four moved to \DANLIBG\draw_sal.f

c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c
c                 FINITE ELEMENT DRAWING ROUTINES
C
c
c   DRAW_RINGS : draws a set of polygons (effectively 'patches')
c
c  etc:
c       eg. can also put here:
c          facet-Depth-sorting 
c          get-shade 0.->1. of a polygon.
c          draw_a_facets : nodes, edges, fill, node#,elem#,mat#
c          draw_contours (with sub-faceting??)
c                                                         danplot.f

c
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
               

c-----------------------------------------------------------------------
      SUBROUTINE SORT_RINGS (RINGS,jring,GC,IGC,ndim,DISPS,IDISPS
     &        ,ZBUF,PL2,PL3,IFCETS,NFCETS_D,FACT, IOP_BPC,AR,PM)
c
c     This adds the rings to the painter's depth sort lists: 
c      PL2()/PL3()/depths() 
c     jring=0 = do all rings.
c     nice to have the ability to BPC the back-planes. 
c     
c     

c     IMPLICIT  NONE
C......... geometric entities
      INTEGER   RINGS(*), jring,IGC,IDISPS
      REAL      GC(IGC,*), DISPS(IDISPS,*), FACT, ZBUF(*)
      INTEGER PL2(*),PL3(*)

c......... viewing parameters and local variables
      REAL PM(4,4)
c     REAL    X(500),Y(500)            !- each polyline
c     real  SC_X,SC_Y,SC_Z, XCEN,YCEN
      INTEGER   IRING, nrings,IC,II, I,J, NOD, IMAT,IPLANE
      REAL      GPT(3), SPT(3)

c-- need a INT_*.F call here to poll the keyboard buffer
c     LOGICAL KEY_WAITING@     !- allow an abort key.
c     EXTERNAL KEY_WAITING@


c----------- 0: initiialise ------------
c----------- 1: loop and form draw_list ------------
c.. clip off-screen, clip bpc, depth sort.
c store a pointer to each polygon record hence non-seq. access.
c----------- 2: loop and find polygon's depth ------------
c.. either call with a flag (jring):
c    1. =0 for all RINGS
c    2. >0 for a specific RING jring
c    3. <0 for a specific RING jring = a direct pointer

      IC = 3                  !- RINGS (starting) pointer
      nrings=rings(2)
      DO IRING = 1,NRINGS
        if (jring.lt.0) then
          ic=abs(jring)             !- point to a particular ring
          if (iring.gt.1) return    !- exit on second pass
        elseif (jring.gt.0) then
          if (iring.lt.jring) then      !- step round as quick as possible
             NOD    = RINGS(IC)
             IC = IC + 2+ NOD+1            !- update to the next polygon ring
             goto 21 
          endif
          if (iring.gt.jring) return    !- exit on subsequent pass
        endif
        NOD    = RINGS(IC)
        IMAT   = RINGS(IC+1)
        IPLANE = RINGS(IC+2)
        IC = IC + 2

        if (jring.gt.0.and.iring.lt.jring) goto 21 ! CYCLE

c--------------------------- build polyline ----------------------------
c.. can speed up and skip over if FACT==0.
        zval=0.

        DO I=1,NOD 
          II = RINGS(IC+I)
          DO J=1,3
            GPT (J) = GC(J,II) + FACT * DISPS(J,II)
          ENDDO
          CALL TRANSFORM2 (GPT,SPT ,PM)
c          X(I) = SPT(1)
c          Y(I) = SPT(2)
          x = spt(1)
          y = spt(2)
          Z = spt(3)
C         ZVAL=MIN(ZVAL,Z)
          ZVAL=MAX(ZVAL,Z)
C         Zval=Zval+Z/REAL(NOD)

c---- Backface Polygon culling ----
          if (i.eq.1) then
            x0=x                  !-remember first point
            y0=y
            C3 = 0.
          else
            C3 = C3 + (yl+y) * (x-xl)/2.   !- add trapezium area
          endif
          xl=x   !- store last values
          yl=y
        ENDDO
        C3 = C3 + (y+y0) * (x0-x)/2.   !- add last trapezium

        C3 = -C3 * AR               ! weight wrt. the screen topology
        if (ndim.ge.3) then
        IF (IOP_BPC.EQ.1.AND.C3.GT.0) GOTO 20  ! cull 'fronts'
        IF (IOP_BPC.EQ.2.AND.C3.LT.0) GOTO 20  ! cull 'backs'
        endif
         nfcets_d=nfcets_d+1          !-- add this ring to the list
         ZVAL=ZVAL*1.00001            ! make rings lie over facets.
         ZBUF(nfcets_d)= ZVAL
         pl3(nfcets_d) = 30           ! This is a RINGS entry
c        pl2(nfcets_d) = iring        ! pointer direct to this facet
         pl2(nfcets_d) =-(IC-2)       ! pointer direct to this facet
   20   CONTINUE
        IC = IC + NOD+1              !- update to the next polygon ring
   21   CONTINUE
      ENDDO
c  22 CONTINUE
      RETURN
      END


c-----------------------------------------------------------------------
      SUBROUTINE DRAW_RINGS (RINGS,jring,GC,IGC,DISPS,IDISPS
     &                ,FACT,code,ICOL,PM)
c
c     Draws the model boundary polygons in the colour ICOL
c     held in RINGS as a data-structure of polygons (+imats)
c        FACT = disp. scale (=0 for orig. mesh)
c        CODE = 'fill' or 'line'
c           DJK 14-3-95

c     nice to have the ability to BPC the back-planes, or even depth-sort
c     OK then, but split to get_
c     16-2-98  add a call to GET_KEY to abort if a key press
c
      IMPLICIT  NONE
C......... geometric entities
      INTEGER   RINGS(*), jring,IGC,IDISPS,  ICOL,iverb   !, IROT
      REAL      GC(IGC,*), DISPS(IDISPS,*), FACT
      CHARACTER CODE*4   !-  'fill' or 'line'

c......... viewing parameters and local variables
      REAL PM(4,4)
      REAL    X(500),Y(500)            !- each polyline
c     real  SC_X,SC_Y,SC_Z, XCEN,YCEN
      INTEGER   IRING, nrings,IC,II, I,J, NOD, IMAT,IPLANE,icol2
      character line*10
      REAL      GPT(3), SPT(3)

c     LOGICAL KEY_WAITING@     !- allow an abort key.
c     EXTERNAL KEY_WAITING@

      IF (ICOL.LT.0) RETURN   !- invisible so just exit

c----------- 0: initiialise ------------
c----------- 1: loop and form draw_list ------------
c.. clip off-screen, clip bpc, depth sort.
c store a pointer to each polygon record hence non-seq. access.
c----------- 2: loop and draw ------------


c----------------------------- loop polygons ---------------------------
c.. either call with a flag (jring):
c    1. =0 for all RINGS
c    2. >0 for a specific RING jring
c    3. <0 for a specific RING jring = a direct pointer
c     jring=0

      IC = 3                  !- RINGS pointer
      nrings=rings(2)
      iverb=2  !3
      DO IRING = 1,NRINGS
        if (iverb>=3) print*,' ring(',iring,'/',nrings,')...'
        if (jring.lt.0) then
          ic=abs(jring)             !- point to a particular ring
          if (iring.gt.1) return    !- exit on second pass
        elseif (jring.gt.0) then
          if (iring.lt.jring) then      !- step round as quick as possible
             NOD    = RINGS(IC)
             IC = IC + NOD+1            !- update to the next polygon ring
             goto 21 
          endif
          if (iring.gt.jring) return    !- exit on subsequent pass
        endif
        NOD    = RINGS(IC)
        IMAT   = RINGS(IC+1)
        IPLANE = RINGS(IC+2)
        IC = IC + 2
        if (iverb>=3) print*,'nod=',nod

c       IF (KEY_WAITING@()) RETURN             !- 16-2-98

        if (jring.gt.0.and.iring.lt.jring) goto 21 ! CYCLE

c--------------------------- build polyline ----------------------------
c.. can speed up and skip over if FACT==0.
        DO I=1,NOD 
          II = RINGS(IC+I)
          DO J=1,3
            GPT (J) = GC(J,II) + FACT * DISPS(J,II)
          ENDDO
          CALL TRANSFORM2 (GPT,SPT ,PM)
          X(I) = SPT(1)
          Y(I) = SPT(2)
        ENDDO
        ICOL2 = ICOL
        IF (ICOL2.EQ.1001) ICOL2 = IMAT         !- colour as IMAT
        IF (ICOL2.EQ.1002) ICOL2 = IPLANE       !- colour as plane # (1..6)

        IF (CODE.EQ.'line') THEN
          if (iverb>=3) print*,' call draw_prim..'
          CALL DR_PRIM (LINE,X,Y,nod,ICOL2,1)    !--- polylines
          if (iverb>=3) print*,' end call draw_prim..'
        ELSEIF (CODE.EQ.'fill') THEN
          CALL DR_PRIM (LINE,X,Y,nod,ICOL2,2)    !--- filled polygon
        ELSEIF (CODE.EQ.'imat') THEN
          WRITE (LINE,'(I4)') IMAT
c         WRITE (LINE,'(I4)') NOD
c         WRITE (LINE,'(I4)') IRING             !- maybe these too?
c         WRITE (LINE,'(I4)') IPLANE
          CALL L_JUST (LINE)                  ! right-justify (or centre?)
! F90     LINE=adjustl(line)
          DO J=2,NOD
            X(1) = X(1) + x(j)                !- find the centroid
            Y(1) = Y(1) + y(j)
          ENDDO
          X(1) = X(1) /REAL(NOD)
          Y(1) = Y(1) /REAL(NOD)
          x(2) = 7.                             !- point size
          CALL DR_PRIM (LINE,X,Y,1,ICOL2,21)

        ENDIF
        IC = IC + NOD+1              !- update to the next polygon ring
   21   CONTINUE
      ENDDO
c  22 CONTINUE
      RETURN
      END



C-----------------------------------------------------------------------
      SUBROUTINE CHANGE_VIEW_PARAMS (CKEY, CODES, SSC,FACT, COA, DEYE
     &           , IROT ,XME,YME, XML,YML, cv)

C --> menu-driven  eye-angle/eye-pan/light-source changing etc. ?
C.. can make a subroutine .. nicer if we hold ALL these real numbers in
C.. a 'viewing-array' :
C..    EYE  angles & vector
C..  LIGHT  angles and vector
C..    COA  on the object
C..    SSC  scale factor
C..   DEYE  perspective distance
C..     UP  vector
C..    PAN  screen x-y shifts (cf .. moving COA itself)
C.. really a BIG fill-in form (some affect others ?) EYE->unit vector

      REAL SSC,FACT, COA(3), XME,YME,XML,YML
      INTEGER CODES(*)    !- input 'stream' of opcodes
     &        ,IROT       !- if=1 image is rotated by 90 deg (obs?)
     &         ,CV(*)     !- only: pan% (44,45) and cursor mode (30)
      CHARACTER CKEY      !- = codes(1) .. so source code is more readable
      DATA STEP /5./      !- 5 degrees per cursor keypress ?

c     CKEY = CHAR (CODES(1))      !- as a character
      key2 = codes(2)

      IF (CKEY.EQ.'V') THEN
        IF (KEY2.EQ. 1) THEN  !-------- rotate the eye-direction --------
c.. ? maybe keep EYE as a unit vector .. so create a TR here and MVmult ! 
c.. eg. hold UP as a code (x,y,z), so view angles relative to this.
c.. hence no need for 'A' to mung the whole geometry !!
          YME = YME - step
        ELSEIF (KEY2.EQ. 2) THEN
          XME = XME + step
        ELSEIF (KEY2.EQ. 4) THEN 
          XME = XME - step
        ELSEIF (KEY2.EQ. 5) THEN
          YME = YME + step
        ELSEIF (KEY2.EQ. 3) THEN
          PRINT*,'Enter the EYE longitude and latitude'
          PRINT*,'XME,YME=',XME,YME
          READ*,  XME,YME
        ELSEIF (KEY2.GE.6.AND.KEY2.LE.9) THEN  !----- Preset Views -----
c          IF (KEY2.EQ. 6) XME = 1001.    !- 'y-z'
c          IF (KEY2.EQ. 7) XME = 1002.    !- 'x-z'
c          IF (KEY2.EQ. 8) XME = 1003.    !- 'y-z'
c          IF (KEY2.EQ. 9) XME = 1004.    !-  down 'hydrostatic' axis
          IF (KEY2.EQ. 6) THEN        !- 'y-z'
            XME = 0.
            YME = 0.
          ELSEIF (KEY2.EQ. 7) THEN    !- 'x-z'
            XME = 0.
            YME = 89.9999
          ELSEIF (KEY2.EQ. 8) THEN    !- 'y-z'
            XME = 90.
            YME = 0.
          ELSEIF (KEY2.EQ. 9) THEN    !-  down 'hydrostatic' axis
            XME = 45.
            YME = 35.264
          ENDIF
        ELSEIF (KEY2.eq.21) THEN     !-------- rotate the light --------
          YML = YML + step
        ELSEIF (KEY2.eq.22) THEN
          XML = XML - step
        ELSEIF (KEY2.eq.24) THEN
          XML = XML + step
        ELSEIF (KEY2.eq.25) THEN
          YML = YML - step
        ELSEIF (KEY2.eq.23) THEN
          PRINT*,'Enter the LIGHT longitude and latitude'
          PRINT*,'XML,YML=',XML,YML
          READ*,  XML,YML           !(Note we can overload this too!)

        ELSEIF (KEY2.eq.31) THEN       !- change the perspective -!
          DEYE = DEYE / SQRT(2.)   ! wide-angle
          SSC  = SSC / SQRT(2.)
        ELSEIF (KEY2.EQ.32) THEN  
          DEYE = DEYE * SQRT(2.)   ! narrow-angle
          SSC  = SSC * SQRT(2.)
        
        ELSEIF (KEY2.EQ.11) THEN  !--------- Image shifting -----------
          CV(45) = CV(45) + 10  !- shift the image
        ELSEIF (KEY2.EQ.12) THEN
          CV(44) = CV(44) - 10  !- by 10% screen width
        ELSEIF (KEY2.EQ.14) THEN
          CV(44) = CV(44) + 10
        ELSEIF (KEY2.EQ.15) THEN
          CV(45) = CV(45) - 10
        ELSEIF (KEY2.EQ.13) THEN           !--- ?? what is this for ?
          PRINT*,'Enter the PAN offset (0,0?)'
          PRINT*,'X_offset,Y_offset=',cv(44),cv(45)
          READ*, CV(45),CV(45)

c---------------------- Mouse -based Zooming ---------------------------
        ELSEIF (KEY2.EQ.100) THEN   !-- reset the PAN and IMAGE SCALE
          CV(44) = 0
          CV(45) = 0
          SSC    = 1.     !(I *know* this is a bit arbitary
c         goto 901     !- auto-redraw?

        ELSEIF (KEY2.EQ.101) THEN   !-- use the mouse
c          ssc = ssc / max (abs(codes(7)-codes(5)) / real(res(1)),   !- zoom factor
c     &                     abs(codes(8)-codes(6)) / real(res(2)) )
c          cv(44) = cv(44) -((codes(7)+codes(5))/2.         != delta-centre-x
c     &                    - (res(4)+res(1)/2.) ) / res(1) *100. ! / image width
c          cv(45) = cv(45) +((codes(8)+codes(6))/2.
c     &                    - (res(5)+res(2)/2.) ) / res(2) *100.
c         goto 901   !- redraw
        ELSE
          PRINT*,'** Internal : Unknown code in ''V'' = view setting'
        ENDIF       

C------------------------ typed eye-position ---------------------------
c.. This is just a sub-case of 'V' above.
      ELSEIF (CKEY.EQ.'t')THEN 
        PRINT*,'Enter the EYE longitude and latitude'
        PRINT*,'XME,YME=',xme,yme
        READ*,XME,YME

C------------------- view change by cursor-keys ------------------------
C... note:set the 'mode of action' 0=rotate,1=pan,2=light
c.. cf use of the SHIFT,CNTRL,ALT with the cursors ?
c.. also cf DOOM-like mode:
c       L,R = rotate, up=forwards (zoom), down=back
C       alt-L,R = pan ('strafe')
C       shift-L,R,U,D = fast movement
c       PgUp, PgDn = view up/down
c  .. contrast moving the object (as in Danplot) and moving the eye (Doom)

      ELSEIF (CKEY.eq.'h') THEN      !- mode
c  .. this is just a sub-set of 'V' above (to use the REAL cursor keys)
c  .. maybe put the DELTA in IOPS() so can set the 'coarseness' :-)
c.. hmm CV(30) could be held locally to this routine surely?
c.. Could add CV(30) to the bottom status-line
        CV(30) = MOD (CV(30) + 1,3)
        IF (CV(30).EQ.0) PRINT*,'rotate mode'
        IF (CV(30).EQ.1) PRINT*,'panning mode'
        IF (CV(30).EQ.2) PRINT*,'move_light mode'

      ELSEIF (CKEY.eq.CHAR(128)) THEN
        IF (cv(30).EQ.0) XME = XME + step
        IF (cv(30).EQ.1) CV(44) = nint(CV(44) -step)   !- pan is opposite ?
        IF (cv(30).EQ.2) XML = nint(XML + step)
      ELSEIF (CKEY.eq.CHAR(129)) THEN
        IF (cv(30).EQ.0) XME = nint(XME - step)
        IF (cv(30).EQ.1) CV(44) =nint( CV(44) +step)
        IF (cv(30).EQ.2) XML = int(XML - step)
      ELSEIF (CKEY.eq.CHAR(130)) THEN
        IF (cv(30).EQ.0) YME = int(YME - step)
        IF (cv(30).EQ.1) CV(45) = int(CV(45) +step)
        IF (cv(30).EQ.2) YML = YML - step
      ELSEIF (CKEY.eq.CHAR(131)) THEN
        IF (cv(30).EQ.0) YME = int(YME + step)
        IF (cv(30).EQ.1) CV(45) = int(CV(45) - step)
        IF (cv(30).EQ.2) YML = int(YML + step)

C------------------------- image scaling -------------------------------
      ELSEIF (CKEY.EQ.'+') THEN      !-- do with 'V' above ??
         SSC = SSC * SQRT(2.)
      ELSEIF (CKEY.EQ.'-') THEN  
         SSC = SSC / SQRT(2.)
      ELSEIF (CKEY.EQ.'*') THEN  
        WRITE(*,'(A,G8.2,A)') 'image scale ?(',SSC,' )'
        READ*,SSC

C---------------------- typed centre -of-attention ---------------------
      ELSEIF (CKEY.EQ.'œ') THEN  
        WRITE(*,'(A,3F10.4)') 'Type Centre of View '
     +       ,COA(1), COA(2), COA(3)
        READ*,COA(1), COA(2), COA(3)
C------------------------ (screen x,y flipping) ------------------------
      ELSEIF (CKEY.EQ.'%') THEN         ! (obs?)
        IROT = 1-IROT

C---------------------- displacement scaling ---------------------------
      ELSEIF (CKEY.EQ.'<') THEN
        FACT = FACT / SQRT(2.)
      ELSEIF (CKEY.EQ.'>') THEN 
        FACT = FACT * SQRT(2.)
      ELSEIF  (CKEY.EQ.'s') THEN
        WRITE(*,'(A,G8.2,A)') 'displacement scale ?(',FACT,' )'
        READ*,FACT
C----- typed anisotropic displacemnt scaling ---
c      ELSEIF (CKEY.EQ.'d') THEN
c        WRITE(*,'(A,3G8.2,A)') 
c     +    '(',XDS,YDS,ZDS,' ) new x,y,z disp. scale ?'
c          READ*,XDS,YDS,ZDS

      ENDIF   
      END

C-----------------------------------------------------------------------
      SUBROUTINE MUNG_MESH (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2
     &   ,GDISPS,IGDISPS,MDF,NLDS, LD 
     &   ,FACETS,IFCETS,NFCETS, RINGS,IRINGS,  NORMS, INORMS
     & ,IDIR)
C
C     This modifies the mesh and/or displacements 
C       -- like K_MESH_MUNG but based on a given opcode/data rather than
C          the input file stream IO
C
      REAL GC(IGC,*)                ! nodal coords
      INTEGER    NUMS (INUMS,*)     ! element steering
     &        ,FACETS (3,IFCETS)    !   element faces
     &         ,RINGS (IRINGS)      !   face polygons

      real ld
      REAL     GDISPS (MDF,IGDISPS)   ! nodal disps (for ALL load steps)
     &         ,NORMS (INORMS,*)    ! normal vectors at the nodes
      INTEGER PL2(*)                ! misc flags (eg. marked nodes)

      INTEGER NUM(32)               ! -- only used once!
      REAL GC_MIN(5),GC_MAX(5)      !- mesh bounding-box

c-------------------- 'standard' Mirroring -----------------------------
c ... if direction = 0 then *remove* elements !
      IF (IDIR.GE.1.AND.IDIR.LE.6) THEN
        CALL GET_MESH_RANGE (GC,IGC ,NN,NDIM, GC_MIN,GC_MAX,DIAG)
        IF (IDIR.GE.1.AND.IDIR.LE.3) THEN
          VAL = GC_MIN (IDIR)             !- min x,y,z 
        ELSEIF (IDIR.GE.4.AND.IDIR.LE.6) THEN
          IDIR = IDIR - 3                 !- restore opcode first
          VAL = GC_MAX (IDIR)             !- max x,y,z
        ENDIF

C----------- copy the displacement info
C----> assume that NN is constant(?), so copy from the top down 
C.. I don't really like this .. maybe only mirror the 'current' disps ?
        DO K=NLDS-1,0,-1             !- loop load-steps
          IB1 = K * NN * 2
          IB2 = K * NN
          DO I=NN,1,-1               !- loop nodes (why backwards?)
            DO J=1,3                 !- loop freedoms
                GDISPS (J,IB1 +I   )  =  GDISPS(J,IB2 +I)  ! move + 
              IF (J.NE.IDIR) THEN
                GDISPS (J,IB1 +I+NN)  =  GDISPS(J,IB2 +I)  ! copy
              ELSE                         
                GDISPS (J,IB1 +I+NN ) = -GDISPS(J,IB2 +I)  ! reflect
              ENDIF
            ENDDO     
          ENDDO
        ENDDO
        CALL MIRROR_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,IDIR,VAL)

c----------------  UnMirror .. ie. delete a 1/4 etc.-------------------
      ELSEIF (IDIR.GE.7.AND.IDIR.LE.9) THEN
        IF (IDIR.EQ.7) THEN     !  un-mirror.. = 'halve the model'
          FACTOR=1./2.
        ELSEIF (IDIR.EQ.8) THEN !  'quarter the model' eg. (spud-can)
          FACTOR=3./4.
        ELSEIF (IDIR.EQ.9) THEN !  'eighth the model' (eg sphere)
          FACTOR=7./8.
        ENDIF
          NEL = nint(NEL * FACTOR)
!         NN  = NN  * FACTOR
        

      ELSEIF (IDIR.EQ.10) THEN !  Typed NEL
c.. probably leave NN alone - as orphan nodes.
        print*,'NEL=',NEL
        read*,NEL

c-------------------- set mat type for selected elements -----------
c.. eg set to zero and so delete
c.. need a concept of 'current material type' for marking and
c.. also for creating new elements via mesh generation
c...  currently implimented as  'HIDE SELECTED ELEMENTS'
c... set_imat is nicer for mesh-generation
        ELSEIF (IDIR.EQ.45) THEN 
          DO IEL=1,NEL
            CALL GET_ELEMENT 
     &      (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
            DO I=1,NOD
C             IF (IFL_NODES(NUM(I)).NE.0) 
              IF (PL2(NUM(I)).NE.0) 
c    &          CALL PUT_EL_IMAT (NUMS,INUMS,IEL, -IMAT)
     &          CALL PUT_EL_IMAT (NUMS,INUMS,IEL, 0)
            ENDDO  
          ENDDO
C         CALL DEL_INVIS_MATS   !-- now delete
C     &             (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2)

c-------------------------------------------------------------
        ELSEIF (IDIR.EQ.50) THEN !  *** Connectivity ***
          CALL CONNECTIVITY_IGRP (GC,IGC,NN,NDIM,NUMS,INUMS,NEL,PL2)

c-------------------------------------------------------------
        ELSEIF (IDIR.EQ.51) THEN !  * HARDENING* : GC = GC + FACT*DISPS
          print*,'*** Hardening option temporarily unavailable'
          DO I=1,NN
            DO J=1,3
c             GC(J,I) = GC(J,I) + FACT* DISPS(J,I)
            ENDDO
          ENDDO
c         NLDS = 0     !- 'wipe-out'

C--------------------- cummulative displacements -----------------------
        ELSEIF (IDIR.eq.52) then  
          DO K=2,NLDS              !- loop load-steps (miss first :-)
            IB1 = (K-2) * NN    
            IB2 = (K-1) * NN
            DO I=1,NN               ! loop nodes
              DO J=1,3               
                GDISPS (J,IB2+I) = GDISPS(J,IB2+I)+GDISPS(J,IB1+I)  
              ENDDO     
            ENDDO
          ENDDO

C----------------------- difference displacements ----------------------
        ELSEIF (IDIR.eq.53) then  
          DO K=NLDS,2,-1            !- loop load-steps (miss first)
            IB1 = (K-2) * NN 
            IB2 = (K-1) * NN
            DO I=1,NN               ! loop nodes
              DO J=1,3               
                GDISPS (J,IB2+I) = GDISPS(J,IB2+I)-GDISPS(J,IB1+I)  
              ENDDO     
            ENDDO
          ENDDO

C---------------------- get face normals displacements -----------------
        ELSEIF (IDIR.EQ.54) THEN  
        CALL GET_NORMALS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,NORMS,MDF,PL2)
c       FACT = 0.    !- so we dont draw them !
C---------------------------- scale the mesh ---------------------------
        ELSEIF (IDIR.EQ.55) THEN  
          PRINT*,' Enter the x,y,z scale factors'
          READ*,XS,YS,ZS
          DO I=1,NN
            GC(1,I) = GC(1,I) * XS
            GC(2,I) = GC(2,I) * YS
            GC(3,I) = GC(3,I) * ZS
          ENDDO
C---------------------------- shift the mesh ---------------------------
c.. cf moving the COA or adding a second mesh on top (eg 2 3D slices
        ELSEIF (IDIR.EQ.56) THEN  
          PRINT*,' Enter the x,y,z shift amounts'
          READ*,XS,YS,ZS
          DO I=1,NN
            GC(1,I) = GC(1,I) + XS
            GC(2,I) = GC(2,I) + YS
            GC(3,I) = GC(3,I) + ZS
          ENDDO
C---------------------- condense coincident nodes ----------------------
c.. these routines are in K_MESH.FOR
      ELSEIF (IDIR.EQ.57) THEN  
          CALL DEL_COIN_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2)

C----------------------- delete orphan nodes ---------------------------
      ELSEIF (IDIR.EQ.58) THEN  
        CALL DEL_O_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2)

C------------------- delete elements of 'zero' area' -------------------
      ELSEIF (IDIR.EQ.59) THEN  
        CALL DEL_ZERO_AREA (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2)

C---------------- turn all 4nq's to 4 3nt's and vice versa -------------
      ELSEIF (IDIR.EQ.60) THEN  
c        CALL QUADRILATERALISE_MESH
c     &      (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2)
        JMAT=230964
        IOP=7
        CALL BISECT_ELEMENTS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P
     &   ,IOP, JMAT)             

      ELSEIF (IDIR.EQ.61) THEN  
C        CALL TRIANGULARISE_MESH
C     &      (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2,1)  !- 1= |/| style
        JMAT=230964
        IOP=1                         !- 1= |/| style
C       IOP=2                         !- 2= |/| style
        CALL BISECT_ELEMENTS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P
     &   ,IOP, JMAT)             

      ELSEIF (IDIR.EQ.64) THEN  
         CALL DEL_INVIS_MATS
     &             (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2)

c............... FACETS and EDGES .................
      ELSEIF (IDIR.EQ.100) THEN  
c       CALL FSTRIP2 (NUMS,INUMS,NEL,FACETS,IFCETS,NFCETS,PL2)
        CALL FSTRIP3 (NUMS,INUMS,NEL,NN,FACETS,IFCETS,NFCETS,PL2)
      ELSEIF (IDIR.EQ.101) THEN  
         CALL DEL_INT_FACETS (FACETS, NFCETS)
      ELSEIF (IDIR.EQ.102) THEN  
c     .. it would be so nice if we could keep the disps too.
C     (so PL2 returns pointers, so cf other mungings)
         CALL FACETS_TO_ELEMENTS
     &       (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2, FACETS,NFCETS)
      ELSEIF (IDIR.EQ.103) THEN  
        CALL STRIP_EDGES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &      ,FACETS,IFCETS,NFCETS,PL2,RINGS,IRINGS,     0,0) !- just corners
      ELSEIF (IDIR.EQ.104) THEN  
        CALL STRIP_EDGES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &      ,FACETS,IFCETS,NFCETS,PL2,RINGS,IRINGS,     1,0) !- all nodes

c............... volume calculations .........
c  13-8-98 for BNFL craters - hydrogen calculations
      ELSEIF (IDIR.EQ.201) THEN  
        IB1 = int(ld-1+.001) * NN      !- the 'current' loadstep
        nbins=20
        CALL CALC_VOLUMES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL, 
     &                           GDISPS(1,ib1+1),MDF,ndim, nbins)


c-------------------------------------------------------
c.. just some routines that I may want to turn DANPLOT into a mesh generator.
c     ELSEIF (KEYWORD.EQ.'*X_TO_Y') THEN
c        CALL X2Y    (GC,IGC,NN,NDIM)
c        CALL DEL_MATS (IO,GC,IGC,NN,NUMS,INUMS,NEL,P,NDIM)
c     ELSEIF (KEYWORD.EQ.'*CHANGE_MATERIALS') THEN
c        CALL CHANGE_MATS (IO,GC,IGC,NN,NDIM,NUMS,INUMS,NEL)
c     ELSEIF (KEYWORD.EQ.'*MATERIALS_BY_BOX') THEN
c        CALL MATS_BY_BOX (IO,GC,IGC,NN,NUMS,INUMS,NEL,NDIM)
c     ELSEIF (KEYWORD.EQ.'*WRAP_CIRCLE_Y') THEN       
c        CALL WRAP_CIRCLE_Y (GC,IGC,NN,NDIM)           !- all to circlar
 
c... I also would like this one .. but it will require data input 
c     ELSEIF (KEYWORD.EQ.'*SORT_NODES_FROM_POINT') THEN
c       CALL SORT_NODES_POINT (IO,GC,IGC,NN,NUMS,INUMS,NEL,PL2,NDIM)


C---------------------------
      ENDIF  !--- All possible MUNGings.
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------


