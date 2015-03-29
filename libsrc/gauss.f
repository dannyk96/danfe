c-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      > > > > >    Numerical Integration Subroutines    < < < < <
C-----------------------------------------------------------------------
C
C      Entrypoint :  GET_ANY_GAUSS   (calls all other routines)
C
C      CONTAINed subroutines :
C           GET_PROD_GAUSS (calls GAUSS) - quads, bricks, 4D etc.
C           GET_IRONS_QTURE           - 3D bricks
C           NUMINT                    - triangles
C           NUMIN3                    - tetrahedra
C           GAUSS                     - 1D
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE GET_ANY_GAUSS (SMP,ISMP,WTS,NDIME,NGP,NOD, AREA)
C
C     A header routine to GAUSS which returns the quadrature
C     points as a single list (no implied looping) ie. in the same
C     form as NUMINT, NUMIN3 and QTURE (IRONS)
C     
C                                            Dan Kidger  April '91
C
C     NOD = the 'target' element to find a rule for :-)
C
C     18-3-95   AREA now returned  = 1 for quads, .5 for tris, 1/6. for tets
C             A 'Modifier' if the area of the domain is
C             less than the full integral (eq. triangles are only 1/2, tets 1/6)
C             (cf. Axisymetry where DET is further modified by the 'circumference'

      IMPLICIT NONE
      INTEGER ISMP, NDIME,NGP,NOD
      INTEGER NGP_1D, NGP_TRI,NGP_QUAD, i,j,igp
      REAL SMP(ISMP,*), WTS(*), AREA
      real smp_base(2,9), wts_base(9)      !- for prisms&pyramids. - base
      real smp_1d(1,3), wts_1d(3)          !- for prisms&pyramids. -project

      IF (NDIME.EQ.2.AND.                  !-- triangles
     &  (NOD.EQ.3.OR.NOD.EQ.6.OR.NOD.EQ.10.OR.NOD.EQ.15)) THEN
         CALL NUMINT (SMP,ISMP,WTS,NGP)
         AREA = 0.5

      ELSEIF (NDIME.EQ.3.AND.              !-- tetrahedra
     &  (NOD.EQ.4.or.nod.eq.10)) THEN
        CALL NUMIN3 (SMP,ISMP,WTS,NGP)
        AREA = 1./6.

      ELSEIF (NDIME.EQ.3.AND.              !-- pyramids
     &  (NOD.EQ.5.or.nod.eq.13)) THEN
        ngp_1d=1               !- linear
        ngp_quad=4
c        ngp_1d=2              !- order 2 (or 3?)
c        ngp_quad=9
c ** 9-8-98 The next 2 lines must be wrong cos SMP gets overwritten
        CALL NUMIN_pyr (SMP,ISMP,WTS,NGP_tri)
        CALL GET_PROD_GAUSS (SMP,ISMP,WTS,2,NGP_quad)
c                          !.. now simply multiply in together the weights
c                          !(ref: G.Bedrosian, IJNM.Eng 1992)
        IGP= 0 
        DO J=1,NGP_1D
          DO I=1,NGP_QUAD
            IGP = IGP+1
            WTS(IGP) = WTS(I)
            SMP(IGP,1) = SMP(I,1)
            SMP(IGP,2) = SMP(I,2)
            SMP(IGP,3) = SMP_1D(J,1)
          ENDDO
            WTS(IGP) = WTS(I)*WTS_1D(J)   !- * this is wrong! *
        ENDDO
        AREA = 1./3.  ! ?check?  - it is OK

      ELSEIF (NDIME.EQ.3.AND.              !-- wedges (prisms)
     &  (NOD.EQ.6.or.nod.eq.15)) THEN
        ngp_1d=2               !-  2 points
        ngp_tri=3              !- 3 in-plane
c        ngp_1d=2              !- order 2 (or 3?)
c        ngp_tri=6
        CALL NUMINT         (SMP_base,2, WTS_base,NGP_tri)
        CALL GET_PROD_GAUSS (SMP_1d,1,   WTS_1d,1,NGP_1d)

c.. cf 1/ *replicating* SMP_tri, NGP_1d times; WTS too
c      2/ copying-in SMP_1d into the z-column
c      3/ product-in WTS_1d
        IGP= 0 
        DO J=1,NGP_1D
          DO I=1,NGP_TRI
            IGP = IGP+1
            WTS(IGP) = WTS_base(J)*WTS_1d(i)
            SMP(IGP,1) = SMP_base(I,1)
            SMP(IGP,2) = SMP_base(I,2)
            SMP(IGP,3) = SMP_1D(J,1)
          ENDDO
        ENDDO
        AREA = 1./2.

c.. note here that we have 4,5,6, 8nb, 10, 13,14nb,15, 
      ELSEIF (NDIME.EQ.3.AND.              !-- Irons rules 
     & (NGP.EQ.6.OR.NGP.EQ.13.OR.NGP.EQ.14.OR.NGP.EQ.15)) THEN
        CALL GET_IRONS_QTURE(SMP,ISMP,WTS,NGP)     
        AREA = 1.
      ELSE                                 !-- product-Gauss
        CALL GET_PROD_GAUSS (SMP,ISMP,WTS,NDIME,NGP)
        AREA = 1.
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_PROD_GAUSS (SMP,ISMP,WTS,NDIME,NGP)
C
C     This recursively call GAUSS to build a product-Gauss rule
C
      IMPLICIT NONE
      INTEGER ISMP,NDIME,NGP

      REAL     SAMP (8,2)       !- one-dimensional rules
     &         ,SMP (ISMP,*)    !- the returned product rule
     &         ,WTS (*)         !- the returned product weights  
      INTEGER POINT (5)         !- 'toggles'
     &      ,NGPI,I,IC

      NGPI = NINT (REAL(NGP)**(1./REAL(NDIME)) )

      IF (NGPI**NDIME.NE.NGP) THEN
        PRINT*,'NGP=',NGP,' is NOT a product Gauss rule for',ndime,'D!'
        CALL MYERROR (2,'in GET_PROD_GAUSS')
      ENDIF
      DO I=1,NDIME
        POINT(I)=1
      ENDDO
      CALL GAUSS (SAMP,8,NGPI)
      DO IC=1,NGP   !---- loop the points in the 'product' table

        WTS(IC)=1.
        DO I=1,NDIME      !--------- build this row -------------
          SMP(IC,I) = SAMP(POINT(I),1)
          WTS(IC)   = SAMP(POINT(I),2) * WTS(IC)
        ENDDO

        I=1                            !(was NDIME)
    1   POINT(I) = POINT(I) + 1       !-- 'toggle' up 
        IF (POINT(I).GT.NGPI)THEN
         POINT(I)=1
         I = I + 1                      !( was I-=1
         IF (I.LE.NDIME) GOTO 1         !(was gt.0
       ENDIF
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_IRONS_QTURE (SAMPL,ISAMPL,WT,NQP)
C
C     Weights and sampling points for Irons' 3D Integration rules
C         Original by IMS 1989
C     3/5/90.... 13 point rule added     (so now 6,13,14,15 rules)
C    18/3/98.... 14 point rule swapped over corner<->face points

      IMPLICIT NONE
      INTEGER ISAMPL, NQP
      REAL SAMPL(ISAMPL,*),WT(*)
      INTEGER I,J
      REAL B,C,D,E
c     CALL NULL(SAMPL,ISAMPL,NQP,3)   !- why zap .. all is explicit ??
      do j=1,3
        do i=1,nqp
          sampl(i,j) = 0. 
        enddo
      enddo

      IF (NQP.EQ.6) THEN
        DO I=1,NQP
          WT(I)=4./3.
        ENDDO
        SAMPL(1,1)=-1.
        SAMPL(2,1)= 1.
        SAMPL(3,2)=-1.
        SAMPL(4,2)= 1.
        SAMPL(5,3)=-1.
        SAMPL(6,3)= 1.

      ELSEIF (NQP.EQ.13) THEN
        B=-0.49584802
        C= 0.88030430
        D= 0.79562143
        E= 0.025293237
        DO I=1,6
          WT( I)= 0.54498736
        ENDDO
        DO I=7,12
          WT( I)= 0.507644216
        ENDDO
        WT(13)= 1.68421056
        DO I=1,6
          DO J=1,3
            SAMPL (I,J)=B
            SAMPL (I,MOD(I-1,3)+1) =C
          ENDDO
        ENDDO
        DO I=7,12
          DO J=1,3
            SAMPL(I,J)= D
            SAMPL(I,MOD(I-1,3)+1) =E
          ENDDO
        ENDDO
        DO J=1,3
          SAMPL(13,J)=0.
          DO I=4,9
            SAMPL(I,J)=-SAMPL(I,J)
          ENDDO
        ENDDO

      ELSEIF (NQP.EQ.14) THEN
        B=0.795822426       !xyz coord of a 'corner' GP
        C=0.758786911       !x00 of a mid-face GP
c        DO I=1,6
        DO I=9,14
          WT(I)=0.886426593
        ENDDO
c        DO I=7,NQP
        DO I=1,8
          WT(I)=0.335180055
        ENDDO
c       sampl(1:6,1) =(/-b, b, 0, 0, 0, 0/)
c       sampl(1:6,2) =(/ 0, 0,-b, b, 0, 0/)
c       sampl(1:6,3) =(/ 0, 0, 0, 0,-b, b/)
        SAMPL( 9,1) = -B
        SAMPL(10,1) =  B     ! cf order for my 14nb?
        SAMPL(11,2) = -B
        SAMPL(12,2) =  B
        SAMPL(13,3) = -B
        SAMPL(14,3) =  B
c.. this is 8-node Gauss-rule -like
c       sampl(7:14,1) =(/-c, c,-c, c, -c, c,-c, c/)
c       sampl(7:14,2) =(/-c,-c, c, c, -c,-c, c, c/)
c       sampl(7:14,3) =(/-c,-c,-c,-c,  c, c, c, c/)
        DO I=1,8
          DO J=1,3
            SAMPL(I,J) = C      !-- default value
          ENDDO
        ENDDO
        SAMPL( 1,1) = -C
        SAMPL( 1,2) = -C
        SAMPL( 1,3) = -C
        SAMPL( 2,2) = -C
        SAMPL( 2,3) = -C
        SAMPL( 3,1) = -C
        SAMPL( 3,3) = -C
        SAMPL( 4,3) = -C

        SAMPL( 5,1) = -C
        SAMPL( 5,2) = -C
        SAMPL( 6,2) = -C
        SAMPL( 7,1) = -C
      ELSEIF (NQP.EQ.15) THEN
c       A=0.                      !- 000 of the center-point
        B=1.                      !- x00 of the 6 mid-face points
        C=0.674199862             !- xyz of the 8 corner points
        WT(1)=1.564444444
        DO I=2,7
          WT(I)=0.355555556
        ENDDO
        DO I=8,15
          WT(I)=0.537777778
        ENDDO
        SAMPL(2,1)=-B
        SAMPL(3,1)=  B
        SAMPL(4,2)=-B
        SAMPL(5,2)=  B
        SAMPL(6,3)=-B
        SAMPL(7,3)=  B
        DO I=8,NQP
          DO J=1,3
            SAMPL(I,J)=C
          ENDDO
        ENDDO
        SAMPL(8,1) = -C
        SAMPL(8,2) = -C
        SAMPL(8,3) = -C
        SAMPL(9,2) = -C
        SAMPL(9,3) = -C
        SAMPL(10,1)= -C
        SAMPL(10,3)= -C
        SAMPL(11,3)= -C
        SAMPL(12,1)= -C
        SAMPL(12,2)= -C
        SAMPL(13,2)= -C
        SAMPL(14,1)= -C
      ELSE
        CALL MYERROR (2,'Unknown Irons type Gauss rule')
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE NUMINT (S,IS,WT,NGP)
C
C      This forms the sampling points and
C      weights for integration over a triangular area
C

c..  IN Fortran 90 I can use a set of 'internal' subroutines
c..  Each sets the constants as 'Parameters' 
c..  then uses simple 'array constructors' top build SMP and WTS
c..  eg.    REAL,PARAM, A / 0.3333/
c..          S= (/A,B,A,A,B,A, C,C,C,D,D,D/)      ok ?

      IMPLICIT NONE
      INTEGER IS, NGP
      REAL S(IS,*),WT(*)
      IF (NGP.EQ.1) THEN                 !--- order 1 ----
        S(1,1) = 1./3.
        S(1,2) = 1./3.
        WT(1) = 1.
      ELSEIF (NGP.EQ.3) THEN             !--- order 2 ----
c.. 6-5-97 note Hammer+Stroud give S(1:3,1)=.666667,.166667,.166667
c     as an alternative. (cos any 3 points along lines to the centre are valid
        S(1,1) = .5
        S(1,2) = .5
        S(2,1) = .5
        S(2,2) = 0.
        S(3,1) = 0.
        S(3,2) = .5
        WT(1) = 1./3.
        WT(2) = WT(1)
        WT(3) = WT(1)
      ELSEIF (NGP.EQ.4) THEN             !--- order ? ----
        S(1,1) = 1./3.
        S(1,2) = 1./3.
        S(2,1) = .6
        S(2,2) = .2
        S(3,1) = .2
        S(3,2) = .6
        S(4,1) = .2
        S(4,2) = .2
        WT(1) = -9./16.
        WT(2) = 25./48.
        WT(3) = WT(2)
        WT(4) = WT(2)
      ELSEIF (NGP.EQ.6) THEN             !--- order 4 ----
        S(1,1) = .816847572980459
        S(1,2) = .091576213509771
        S(2,1) = S(1,2)
        S(2,2) = S(1,1)
        S(3,1) = S(1,2)
        S(3,2) = S(1,2)
        S(4,1) = .108103018168070
        S(4,2) = .445948490915965
        S(5,1) = S(4,2)
        S(5,2) = S(4,1)
        S(6,1) = S(4,2)
        S(6,2) = S(4,2)
        WT(1) = .109951743655322
        WT(2) = WT(1)
        WT(3) = WT(1)
        WT(4) = .223381589678011
        WT(5) = WT(4)
        WT(6) = WT(4)
      ELSEIF (NGP.EQ.7) THEN             !--- order 5 ----
        S(1,1) = 1./3.
        S(1,2) = 1./3.
        S(2,1) = .797426985353087
        S(2,2) = .101286507323456
        S(3,1) = S(2,2)
        S(3,2) = S(2,1)
        S(4,1) = S(2,2)
        S(4,2) = S(2,2)
        S(5,1) = .470142064105115
        S(5,2) = .059715871789770
        S(6,1) = S(5,2)
        S(6,2) = S(5,1)
        S(7,1) = S(5,1)
        S(7,2) = S(5,1)
        WT(1) = .225
        WT(2) = .125939180544827
        WT(3) = WT(2)
        WT(4) = WT(2)
        WT(5) = .132394152788506
        WT(6) = WT(5)
        WT(7) = WT(5)
      ELSEIF (NGP.EQ.12) THEN            !--- order 6 ----
        S(1,1) = .873821971016996
        S(1,2) = .063089014491502
        S(2,1) = S(1,2)
        S(2,2) = S(1,1)
        S(3,1) = S(1,2)
        S(3,2) = S(1,2)
        S(4,1) = .501426509658179
        S(4,2) = .249286745170910
        S(5,1) = S(4,2)
        S(5,2) = S(4,1)
        S(6,1) = S(4,2)
        S(6,2) = S(4,2)
        S(7,1) = .636502499121399
        S(7,2) = .310352451033785
        S(8,1) = S(7,1)
        S(8,2) = .053145049844816
        S(9,1) = S(7,2)
        S(9,2) = S(7,1)
        S(10,1) = S(7,2)
        S(10,2) = S(8,2)
        S(11,1) = S(8,2)
        S(11,2) = S(7,1)
        S(12,1) = S(8,2)
        S(12,2) = S(7,2)
        WT(1) = .050844906370207
        WT(2) = WT(1)
        WT(3) = WT(1)
        WT(4) = .116786275726379
        WT(5) = WT(4)
        WT(6) = WT(4)
        WT(7) = .082851075618374
        WT(8) = WT(7)
        WT(9) = WT(7)
        WT(10) = WT(7)
        WT(11) = WT(7)
        WT(12) = WT(7)
      ELSEIF (NGP.EQ.16) THEN
        S(1,1) = 1./3.
        S(1,2) = 1./3.
        S(2,1) = .658861384496478
        S(2,2) = .170569307751761
        S(3,1) = S(2,2)
        S(3,2) = S(2,1)
        S(4,1) = S(2,2)
        S(4,2) = S(2,2)
        S(5,1) = .898905543365938
        S(5,2) = .050547228317031
        S(6,1) = S(5,2)
        S(6,2) = S(5,1)
        S(7,1) = S(5,2)
        S(7,2) = S(5,2)
        S(8,1) = .081414823414554
        S(8,2) = .459292588292723
        S(9,1) = S(8,2)
        S(9,2) = S(8,1)
        S(10,1) = S(8,2)
        S(10,2) = S(8,2)
        S(11,1) = .008394777409958
        S(11,2) = .263112829634638
        S(12,1) = S(11,1)
        S(12,2) = .728492392955404
        S(13,1) = S(11,2)
        S(13,2) = S(11,1)
        S(14,1) = S(11,2)
        S(14,2) = S(12,2)
        S(15,1) = S(12,2)
        S(15,2) = S(11,1)
        S(16,1) = S(12,2)
        S(16,2) = S(11,2)
        WT(1) = .144315607677787
        WT(2) = .103217370534718
        WT(3) = WT(2)
        WT(4) = WT(2)
        WT(5) = .032458497623198
        WT(6) = WT(5)
        WT(7) = WT(5)
        WT(8) = .095091634267284
        WT(9) = WT(8)
        WT(10) = WT(8)
        WT(11) = .027230314174435
        WT(12) = WT(11)
        WT(13) = WT(11)
        WT(14) = WT(11)
        WT(15) = WT(11)
        WT(16) = WT(11)
      ELSE
        CALL MYERROR (2,'Unknown Triangle type Gauss rule')
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE NUMIN_PYR (SAMP,ISAMP,WT,NGP)
C
C      This forms the sampling points and
C      weights for integration over a pyramid
C        DJK 6-5-97
C     note: we must product-in these with a 2d-quad rule before use.
C
      IMPLICIT NONE
      INTEGER ISAMP, NGP
      REAL SAMP(ISAMP,*), WT(*)
      IF (NGP.EQ.1) THEN              !- order 1-
        SAMP(1,1) = 0.75
        WT(1) = 1./3.
      ELSEIF (NGP.EQ.2) THEN          !- order 3-
        SAMP(1,1) = 0.455848155988775
        SAMP(2,1) = 0.877485177344559
        WT(1) = 0.100785882079825
        WT(2) = 0.232547451253508
      ELSEIF (NGP.EQ.3) THEN          !- order 5-
        SAMP(1,1) = 0.294997790111502
        SAMP(2,1) = 0.652996233961648
        SAMP(3,1) = 0.927005975926850
        WT(1) = 0.029950703008581
        WT(2) = 0.146246269259866
        WT(3) = 0.157136361064887
      ELSE
        CALL MYERROR (2,'Unknown Pyramid type Gauss rule')
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE NUMIN3 (SAMP,ISAMP,WT,NGP)
C
C      This forms the sampling points and
C      weights for integration over a tetrahedron
C
      IMPLICIT NONE
      INTEGER ISAMP, NGP
      REAL SAMP(ISAMP,*), WT(*)
      IF (NGP.EQ.1) THEN          !- order 1--
        SAMP(1,1) = .25
        SAMP(1,2) = .25
        SAMP(1,3) = .25
        WT(1) = 1.
      ELSEIF (NGP.EQ.4) THEN      !- order 2--
        SAMP(1,1) = .58541020
        SAMP(1,2) = .13819660
        SAMP(1,3) = SAMP(1,2)
        SAMP(2,2) = SAMP(1,1)
        SAMP(2,3) = SAMP(1,2)
        SAMP(2,1) = SAMP(1,2)
        SAMP(3,3) = SAMP(1,1)
        SAMP(3,1) = SAMP(1,2)
        SAMP(3,2) = SAMP(1,2)
        SAMP(4,1) = SAMP(1,2)
        SAMP(4,2) = SAMP(1,2)
        SAMP(4,3) = SAMP(1,2)
        WT(1) = .25
        WT(2) = .25
        WT(3) = .25
        WT(4) = .25
        
      ELSEIF (NGP.EQ.5) THEN
        SAMP(1,1) = .25
        SAMP(1,2) = .25
        SAMP(1,3) = .25
        SAMP(2,1) = .5
        SAMP(2,2) = 1./6.
        SAMP(2,3) = SAMP(2,2)
        SAMP(3,2) = .5
        SAMP(3,3) = 1./6.
        SAMP(3,1) = SAMP(3,3)
        SAMP(4,3) = .5
        SAMP(4,1) = 1./6.
        SAMP(4,2) = SAMP(4,1)
        SAMP(5,1) = 1./6.
        SAMP(5,2) = SAMP(5,1)
        SAMP(5,3) = SAMP(5,1)
        WT(1) = -.8
        WT(2) = 9./20.
        WT(3) = WT(2)
        WT(4) = WT(2)
        WT(5) = WT(2)

      ELSEIF (NGP.EQ.12) THEN      !- order 3--

      ELSE
        CALL MYERROR (2,'Unknown Tetrahedra type Gauss rule')
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GAUSS (SAMP,ISAMP,NGP)
C
C     This provides the weights and sampling points
C     for Gauss-Legendre quadrature   (ie. 1d.. hence quads & bricks)
C     .. finally changed to a BLOCK-IF   7-Mar-94 !
C     15-9-95 order all reversed so now low->hi coords.

      IMPLICIT NONE
      INTEGER ISAMP, NGP
      REAL SAMP(ISAMP,*)

      IF (NGP.EQ.1) THEN
        SAMP(1,1) = 0.
        SAMP(1,2) = 2.
      ELSEIF (NGP.EQ.2) THEN
        SAMP(1,1) = -1./SQRT(3.)
        SAMP(2,1) = -SAMP(1,1)
        SAMP(1,2) = 1.
        SAMP(2,2) = 1.
      ELSEIF (NGP.EQ.3) THEN
        SAMP(1,1) = -.2*SQRT(15.)
        SAMP(2,1) = .0
        SAMP(3,1) = -SAMP(1,1)
        SAMP(1,2) = 5./9.
        SAMP(2,2) = 8./9.
        SAMP(3,2) = SAMP(1,2)
      ELSEIF (NGP.EQ.4) THEN
        SAMP(1,1) = -.861136311594053
        SAMP(2,1) = -.339981043584856
        SAMP(3,1) = -SAMP(2,1)
        SAMP(4,1) = -SAMP(1,1)
        SAMP(1,2) = .347854845137454
        SAMP(2,2) = .652145154862546
        SAMP(3,2) = SAMP(2,2)
        SAMP(4,2) = SAMP(1,2)
      ELSEIF (NGP.EQ.5) THEN
        SAMP(1,1) = -.906179845938664
        SAMP(2,1) = -.538469310105683
        SAMP(3,1) = .0
        SAMP(4,1) = -SAMP(2,1)
        SAMP(5,1) = -SAMP(1,1)
        SAMP(1,2) = .236926885056189
        SAMP(2,2) = .478628670499366
        SAMP(3,2) = .568888888888889
        SAMP(4,2) = SAMP(2,2)
        SAMP(5,2) = SAMP(1,2)
      ELSEIF (NGP.EQ.6) THEN
        SAMP(1,1) = -.932469514203152
        SAMP(2,1) = -.661209386466265
        SAMP(3,1) = -.238619186083197
        SAMP(4,1) = -SAMP(3,1)
        SAMP(5,1) = -SAMP(2,1)
        SAMP(6,1) = -SAMP(1,1)
        SAMP(1,2) = .171324492379170
        SAMP(2,2) = .360761573048139
        SAMP(3,2) = .467913934572691
        SAMP(4,2) = SAMP(3,2)
        SAMP(5,2) = SAMP(2,2)
        SAMP(6,2) = SAMP(1,2)
      ELSEIF (NGP.EQ.7) THEN
        SAMP(1,1) = -.949107912342759
        SAMP(2,1) = -.741531185599394
        SAMP(3,1) = -.405845151377397
        SAMP(4,1) = .0
        SAMP(5,1) = -SAMP(3,1)
        SAMP(6,1) = -SAMP(2,1)
        SAMP(7,1) = -SAMP(1,1)
        SAMP(1,2) = .129484966168870
        SAMP(2,2) = .279705391489277
        SAMP(3,2) = .381830050505119
        SAMP(4,2) = .417959183673469
        SAMP(5,2) = SAMP(3,2)
        SAMP(6,2) = SAMP(2,2)
        SAMP(7,2) = SAMP(1,2)
      ELSE
        CALL MYERROR (2,'Unknown Linear-Gauss type rule')
      ENDIF
      RETURN
      END
C-----------------------------------------------------------------------
