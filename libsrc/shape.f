C-----------------------------------------------------------------------
C        - These are ALL the Shape Function subroutines
C
C   - The names have all been made consistent! eg. FM8NQ   :-)
C   - Also the Sampling-points have been made consistent (+ no weights)
C   - About half of the subroutines were also in FE5LIB.. the others
C     (eg. 1D elements, 12N-Quad,10N-triangle and the 14 N-brick are 
C      all by Dan Kidger )
C
C   - Also included are 4 general-purpose entry-points: 
C      GSF     which calls up the appropriate element subroutine and

c      GET_DEFAULT_NGP   : returns the 'usual' # of GP's to use
C      GET_BIOT_DAUGHTER : returns the NUM of the daughter
C      WTHATN  which calls SHAPE to get the local-coords of an element
C      GET_DEFAULT_NGP returns the 'normal' GP rule for this element
c
C      NUM_TO_CODE  does (2,4,1) -> ' 4nq'
C      CODE_TO_NUM  does '8nb' -> 3,8,1, etc.
C
C-----------------------------------------------------------------------
C    29-5-93 added 2 14nb's and the 16n-tesseract :-)
C          - FM8NQ changed to SF8NQ etc. :-)
C          - fixed some XI<->ETA order problems
c    26-2-97 moved GET_BIOT_DAUGHTER to here.
c    19-6-97 added 5np = a pyramid with a 4nq as a base.

C-----------------------------------------------------------------------
      SUBROUTINE GSF (NDIME,NOD,ITYPE,DER,IDER,FUN,SMP,ISMP,I)
C
C     = 'Generalised_Shape_Functions'
C     A macro header routine to call up the shape functions
C     and derivatives of ANY known element.
C     ITYPE = optional element type
C                                       Dan Kidger   April '91
c
c     This routines is an obvious candidate for a SELECT CASE construct
C
C     8-4-92 samp order reversed for 4nq,8nq,8nb,11nb etc. 
C            so that the 'first' coord is ALWAYS local-X !
C     7-4-94 NODOF-> NDIME 
C
      REAL SMP(ISMP,*), DER(IDER,*), FUN(*)
      character line*78
      FUN(1) = 230964.       !- flag
C------------------------- 0D elements ---------------------------------
      IF (NDIME.EQ.0) THEN  
        IF (NOD.EQ. 1) THEN
          FUN(1) = 1.
        ENDIF
C------------------------- 1D elements ---------------------------------
      ELSEIF (NDIME.EQ.1) THEN  
        IF (NOD.EQ. 2) THEN
          CALL SF2NL  (DER,IDER,FUN,SMP,ISMP,I)
        ELSEIF (NOD.EQ. 3) THEN
          CALL SF3NL  (DER,IDER,FUN,SMP,ISMP,I)
        ELSEIF (NOD.EQ. 5) THEN
          CALL SF5NL  (DER,IDER,FUN,SMP,ISMP,I)
        ENDIF
C------------------------- 2D elements ---------------------------------
      ELSEIF (NDIME.EQ.2) THEN
        IF (NOD.EQ. 3) THEN 
          CALL SF3NT  (DER,IDER,FUN,SMP,ISMP,I)
        ELSEIF (NOD.EQ. 6) THEN 
          CALL SF6NT  (DER,IDER,FUN,SMP,ISMP,I)
        ELSEIF (NOD.EQ.10) THEN 
          CALL SF10NT (DER,IDER,FUN,SMP,ISMP,I)
        ELSEIF (NOD.EQ.15) THEN
          CALL SF15NT (DER,IDER,FUN,SMP,ISMP,I)

        ELSEIF (NOD.EQ. 4) THEN
          CALL SF4NQ  (DER,IDER,FUN,SMP,ISMP,I)
        ELSEIF (NOD.EQ. 5) THEN
          CALL SF5NQ  (DER,IDER,FUN,SMP,ISMP,I)
        ELSEIF (NOD.EQ. 8) THEN
          CALL SF8NQ  (DER,IDER,FUN,SMP,ISMP,I)
        ELSEIF (NOD.EQ. 9) THEN
          CALL SF9NQ  (DER,IDER,FUN,SMP,ISMP,I)
        ELSEIF (NOD.EQ.12) THEN
          CALL SF12NQ (DER,IDER,FUN,SMP,ISMP,I) 
        ELSEIF (NOD.EQ.17) THEN
          CALL SF17NQ (DER,IDER,FUN,SMP,ISMP,I) 
        ENDIF
C------------------------- 3D elements ---------------------------------
      ELSEIF (NDIME.EQ.3)THEN

        IF (NOD.EQ. 4) THEN
          CALL SF4NP  (DER,IDER,FUN,SMP, ISMP,I)
        ELSEIF (NOD.EQ. 5) THEN
          CALL SF5NP  (DER,IDER,FUN,SMP, ISMP,I)
        ELSEIF (NOD.EQ. 8) THEN
          CALL SF8NB  (DER,IDER,FUN,SMP, ISMP,I)
        ELSEIF (NOD.EQ.11) THEN
          CALL SF11NB (DER,IDER,FUN,SMP, ISMP,I)
c        ELSEIF (NOD.EQ. 13) THEN
c          CALL SF13NP  (DER,IDER,FUN,SMP, ISMP,I)
        ELSEIF (NOD.EQ.14) THEN
c         IF (ITYPE.EQ.0) CALL SF14NB    (DER,IDER,FUN,SMP, ISMP,I)
          IF (ITYPE.EQ.1) CALL SF14NB_AB (DER,IDER,FUN,SMP, ISMP,I)
          IF (ITYPE.EQ.2) CALL SF14NB_SER(DER,IDER,FUN,SMP, ISMP,I)
        ELSEIF (NOD.EQ.20) THEN
          CALL SF20NB (DER,IDER,FUN,SMP, ISMP,I)
        ELSEIF (NOD.EQ.26) THEN
          CALL SF26NB (DER,IDER,FUN,SMP, ISMP,I)
        ELSEIF (NOD.EQ.27) THEN
          CALL SF27NB (DER,IDER,FUN,SMP, ISMP,I)
        ELSEIF (NOD.EQ.32) THEN                     !- 10-9-98
          CALL SF32NB (DER,IDER,FUN,SMP, ISMP,I)

        ELSEIF (NOD.EQ.6) THEN         !- 6 node wedge
          CALL SF6NW (DER,IDER,FUN,SMP, ISMP,I)        ! (no derivs yet)
        ELSEIF (NOD.EQ.15) THEN        !- 15 node wedge
          CALL SF15NW (DER,IDER,FUN,SMP, ISMP,I)       ! (not yet done)

        ENDIF
C------------------------- 4D elements ---------------------------------
c.. these are for academic studies only?
      ELSEIF (NDIME.EQ.4) THEN
        IF (NOD.EQ.16) THEN
          CALL SF16N_4D (DER,IDER,FUN,SMP, ISMP,I)
        ENDIF
      ENDIF

c-----------------------------------------------------------------------
c... check that our flag has been replaced - else ERROR
      IF (ABS(FUN(1)-230964.).lt.0.1) then
           WRITE(line,'(A,I2,A,I3,A,I3,a,i2)')
     &      'element not found: ndime=',ndime,' nod=',nod
     &     ,' type=',itype
      call myerror (2,line)
      endif
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_DEFAULT_NGP (NOD,NDIME,NGP)
C
C     This will return a suitable (full) Gauss Point rule for all
C     elements    Dan Kidger 7-3-94
C     .. Maybe can other info eg. is it a triangle?
C   * BEWARE * the 8nq us usually used with 2x2 (=R.I.) .. so the
C       default (9=3x3) is too stiff for normal use.
C

C   To save using IF..ELSEIF can have a table of 'elcode', 'ngp'
C      or:  ngp_ri,ngp_fi, ngp_defualt
C    also: is_a_triangle, eigenmode info?, 'name'(eg. 4-node quad.)
c        : #msn, 'the 6 facets'(for drawing/surface loads) <GET_FACE>
 
 
C  (neutralise ITYPE .. so handle specifics first then 'any'
C  (Buffer pointer so we only re-loop-up if nen/ndime/itype changes
C  (also select by # .. so effectively we can loop *all* elements
C   and so list/eigenmode/animate

      INTEGER NOD,NGP
      NGP = -1

C------------------------- 1D elements ---------------------------------
      IF (NDIME.EQ.1) THEN  
        IF (NOD.EQ. 2) THEN       !- for rod elements (cf Hermite beams)
        NGP = 1
        ELSEIF (NOD.EQ. 3) THEN
        NGP = 2
        ENDIF

C------------------------- 2D elements ---------------------------------
      ELSEIF (NDIME.EQ.2) THEN
        IF (NOD.EQ. 3) THEN 
        NGP = 1
        ELSEIF (NOD.EQ. 6) THEN 
        NGP = 3
        ELSEIF (NOD.EQ.10) THEN 
        NGP = 6
        ELSEIF (NOD.EQ.15) THEN
        NGP = 12

        ELSEIF (NOD.EQ. 4) THEN
        NGP = 4
        ELSEIF (NOD.EQ. 5) THEN
        NGP = 4
        ELSEIF (NOD.EQ. 8) THEN
c       NGP = 9                  !-- do we want 4 or 9 here ?
        NGP = 4
        ELSEIF (NOD.EQ. 9) THEN
        NGP = 9
        ELSEIF (NOD.EQ.12) THEN
        NGP = 9
        ELSEIF (NOD.EQ.17) THEN
        NGP = 16
        ENDIF
C------------------------- 3D elements ---------------------------------
      ELSEIF (NDIME.EQ.3)THEN
        IF (NOD.EQ. 4) THEN
        NGP = 1
        ELSEIF (NOD.EQ. 5) THEN               !- hack for a pyramid?
        NGP = 8   !4

        ELSEIF (NOD.EQ. 6) THEN               !- toblerone
        NGP = 6

        ELSEIF (NOD.EQ. 8) THEN
        NGP =  8
        ELSEIF (NOD.EQ.11) THEN
        NGP =  8
        ELSEIF (NOD.EQ.20) THEN
        NGP = 14
        ELSEIF (NOD.EQ.26) THEN
        NGP = 14
        ELSEIF (NOD.EQ.27) THEN
        NGP = 14
        ELSEIF (NOD.EQ.32) THEN             !- 10-9-98
        NGP = 27

        ELSEIF (NOD.EQ.14) THEN
        NGP =  8
        ENDIF

C------------------------- 4D elements ---------------------------------
      ELSEIF (NDIME.EQ.4) THEN
        IF (NOD.EQ.16) THEN
        NGP = 16
        ENDIF
      ENDIF

c-----------------------------------------------------------------------
      IF (ngp.lt.0) THEN
         WRITE(*,'(A,I2,A,I3,A,I3)')
     &  '** ERROR: element not found: ndime=',ndime,' nod=',nod
      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_BIOT_DAUGHTER (NOD,NDIME,ITYPE, NODF, NUM2)
c
c      Gets the daughter element used by a Biot consolidation element
c      .. should check ITYPE=5 for pore-pressure elements?
C      .. Perhaps I ought to convert NUM->NUM2 here too?
c       < but if 10nt/6nt then some nodes are not in the 10nt !!>>
c               DJK 20-2-96
c
      IMPLICIT NONE
      INTEGER NOD,NDIME,ITYPE, NODF, NUM2(*)
c     INTEGER NUM(*)
      NODF=0
      IF (NDIME.EQ.2) THEN       !-- 'SELECT CASE' is much nicer
        IF (NOD.EQ.4) THEN
          NODF=4          
c         num2=(/1,2,3,4/)       !- or let the default be epinomous
        ELSEIF (NOD.EQ.6) THEN
          NODF=3          
c         num2=(/1,3,5/)
        ELSEIF (NOD.EQ.8) THEN
          NODF=4          
c         num2=(/1,3,5,7/)
          num2(1) = 1
          num2(2) = 3
          num2(3) = 5
          num2(4) = 7
        ELSEIF (NOD.EQ.9) THEN
          NODF=4
c         num2=(/1,3,5,7/)
        ENDIF
      ELSEIF (NDIME.EQ.3) THEN
        IF (NOD.EQ.8) THEN
          NODF=8          
c         num2=(/1,2,3,4,5,6,7,8/)
        ELSEIF (NOD.EQ.14) THEN
          NODF=8          
c         num2=(/1,2,3,4,5,6,7,8/)
        ELSEIF (NOD.EQ.20) THEN
          NODF=8
c         num2=(/1,3,5,7, 13,15,17,19/)
        ENDIF
      ENDIF
      IF (NODF.EQ.0) CALL MYERROR 
     &      (3,'Oops BIOT sub-element not found')
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE WTHATN (NOD,NDIME,ITYPE,LCOORD,ILCOORD)
C
C     This is a header to WTHATN2 to avoid unnecessary work by caching
C     the last element.      Dan Kidger 1992
C     - perhaps we should also be able to write out the info as F90 
C       statments ?
C
      SAVE
      REAL LCOORD  (ILCOORD,*)    !- the returned positions
     &    ,LCOORD2 (32,4)         !- saved copy (as max. possible szie)
      INTEGER NOD,    NDIME,    ITYPE
     &       ,NOD_OLD,NDIME_OLD,ITYPE_OLD
      DATA    NOD_OLD,NDIME_OLD,ITYPE_OLD/3*-99/

      IF (     NOD.NE.NOD_OLD  
     &    .OR. NDIME.NE.NDIME_OLD 
     &    .OR. ITYPE.NE.ITYPE_OLD) THEN   !- a 'new' element type

        CALL WTHATN2 (NOD,NDIME,ITYPE,LCOORD,ILCOORD)
        DO J=1,NDIME
          DO I=1,NOD
            LCOORD2 (I,J) = LCOORD(I,J)    !- store for future use
          ENDDO
        ENDDO
c        do i=1,nod        !- HACK for 32nbs
c          write (*,'(i5,3f12.4)') i,(lcoord(i,j),j=1,ndime)
c        enddo

        NOD_OLD   = NOD      ! 2-12-95  *ARRGH* these three lines were misisng
        NDIME_OLD = NDIME    !  so we *never* were buffering!!
        ITYPE_OLD = ITYPE    ! so DANBLOCKS is slowed, and Contouring too.
      ELSE
        DO J=1,NDIME
          DO I=1,NOD
            LCOORD (I,J) = LCOORD2(I,J)     !- just recall the last
          ENDDO
        ENDDO
      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE WTHATN2 (NEN,NDIME,ITYPE,LCOORD,ILCOORD)
C
C     This recursively searches coords in the range -1 to +1 searching
C     for the local coords of an element usng SHAPE  - Dan Kidger 1992
C
      PARAMETER (MAX_LEVEL=12,IDER=5,ISMP=1, TOL=1.E-5 )

      REAL      FUN (99)           !- tested shape funs
     &         ,DER (IDER,99)      !- SF derivs (unused)
     &         ,SMP (ISMP,IDER)    !- test-point in local coords
     &      ,LCOORD (ILCOORD,*)    !- the returned positions
      INTEGER POINT (5)            !- the 'integer' grid-point
      INODE = 0                    !- the # of nodes so far found

      DO I=1,NEN             !- null FUN incase SHAPE can't find it :-)
        FUN(I) = 0.
      ENDDO

c------------------- loop the 'recursion'-search level -----------------
      DO N=1,MAX_LEVEL       !- loop the 'recursion' level 

        DO J=1,NDIME   
          POINT(J) = 0       !- set-up the initial trial point
        ENDDO

c-------------- loop through the local co-ords -------------------------
      DO LEVEL=1,99999           !(ie. do-forever)

c--------- Sieve of Erasmthus (sp?) search of the prime factors --------
c.. ie. we can skip out if we have tried this point already

      IF (N.GE.2) THEN
        DO J=2,MAX_LEVEL/2
          IF (MOD(N,J).EQ.0)THEN
            IFLAG=0
            DO IPT=1,NDIME
              IF (MOD(POINT(IPT),J).NE.0) IFLAG=1
            ENDDO  
            IF (IFLAG.EQ.0) GOTO 2      !- try another point
          ENDIF
        ENDDO
      ENDIF
c--------------- form the sampling point into SMP ----------------------
        DO J=1,NDIME
          SMP(1,J) = -1. + 2. * REAL(POINT(J)) / REAL(N) 
        ENDDO
        CALL GSF (NDIME,NEN,ITYPE,DER,IDER,FUN,SMP,ISMP,1)

C------------ loop the nodes to find which has FUN = 1.0 ---------------
        NODEPOS = 0
        NZERO  = 0
        DO I=1,NEN
          IF (ABS(FUN(I)-1.).LT.TOL) THEN           !- 'own node'
            NODEPOS = I                        
          ELSEIF (ABS(FUN(I)).LT.TOL) THEN          !- 'other node'
            NZERO = NZERO +1
          ENDIF
        ENDDO

        IF (NODEPOS.NE.0.AND.NZERO+1.EQ.NEN) THEN    !- a 'hit'
          INODE = INODE + 1
          DO J=1,NDIME
            LCOORD(NODEPOS,J) = SMP(1,J)
          ENDDO
        ENDIF
        IF (INODE.EQ.NEN)  RETURN                   !- all done

c------------------ 'toggle' up the POINT pointers ---------------------
    2   CONTINUE        !- can jump to this point
        IPT = NDIME
    1   POINT(IPT) = POINT(IPT) + 1
        IF (POINT(IPT).GT.N) THEN
          POINT(IPT) = 0
          IPT = IPT - 1              
          IF (IPT.GT.0) GOTO 1        !- push left
          GOTO 99                     !- finished all possiblilities
        ENDIF

      ENDDO        !- end of the 'pseudo' local co-ord permutation loop
c-----------------------------------------------------------------------
   99   CONTINUE
      ENDDO        !- end of the recusion 'level'
      PRINT*,'*** ERROR: coords not found after',max_level,' iterations'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE NUM_TO_CODE (NDIME,NOD,ITYPE, STRING)
C
C     Converts the 2,4,1 form of an element into its charcter code
C       DJK  13-7-96
C
      INTEGER NDIME,NOD,ITYPE
      CHARACTER STRING*8, TYP*1
      WRITE (STRING,'(i2,a1,i1,a4)') NOD,'n',ndime, '.' 

C------------- 1D elements ------------
      IF (NDIME.EQ.1) THEN  
        TYP = 'l'

C------------- 2D elements ------------
      ELSEIF (NDIME.EQ.2) THEN
        IF (NOD.EQ. 3.or. NOD.EQ.6.or.NOD.eq.10.or.NOD.eq.15) THEN 
          TYP = 't'
        ELSEIF (NOD.EQ. 4 .or.NOD.EQ.5 .or. NOD.eq.8 .or.NOD.EQ.9.or.
     &          NOD.EQ.12 .OR.NOD.EQ.17 ) THEN
          TYP = 'q'
        ENDIF
C-------------- 3D elements --------------
      ELSEIF (NDIME.EQ.3)THEN
c       !- but 20nb too ? (1+3+6+10)
        IF (NOD.EQ.4.or.NOD.eq.5.or.NOD.EQ.10.or.NOD.eq.13) THEN  
          TYP = 'p'        !- ie a 'pyramid'
        ELSEIF (NOD.EQ. 8.or. NOD.EQ.11.or. NOD.EQ.14.or. NOD.EQ.20
     &     .or. NOD.eq.26.or.NOD.EQ.27.or. NOD.EQ.32 ) THEN
          TYP = 'b'
        ENDIF
C--------------- 4D elements --------------
      ELSEIF (NDIME.EQ.4) THEN
        TYP = 'h'        !- ie a 'hypercube'
      ENDIF
      STRING(4:4) = TYP

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE CODE_TO_NUM (NDIME,NOD,ITYPE, STRING)
C
C     Converts the character form '8nb'  into the numeric form (3,8,1)
C       DJK  13-7-96
C
      INTEGER NDIME,NOD,ITYPE
      CHARACTER STRING*6, TYP*1
      READ (STRING,'(i2)') NOD
      
      ITYPE = 1                  !- hmm maybe /=1 if a BIOT flag.
      TYP = STRING (4:4)
      IF (TYP.EQ.'l') THEN
        NDIM = 1
      ELSEIF (TYP.EQ.'t'.OR.TYP.EQ.'q') THEN
        NDIM = 2
      ELSEIF (TYP.EQ.'p'.OR.TYP.EQ.'b') THEN
        NDIM = 3
      ELSEIF (TYP.EQ.'h') THEN
        NDIM = 4
      ENDIF
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE SF2NL (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for
C     a 2-noded line element
C             by  Dan Kidger, Jan 1993
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      X=SAMP(I,1)
      FUN(1) = (1.-X)/2.
      FUN(2) = (1.+X)/2.
      DER(1,1) =  -0.5
      DER(1,2) =   0.5
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF3NL (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for
C     a 3-noded line element
C             by  Dan Kidger, Jan 1993
c       9-11-96 FUNs fixed - inspired by B+G
C
      IMPLICIT NONE
      INTEGER IDER,ISAMP,I 
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*), X
      X=SAMP(I,1)
      FUN(1) = (X-1.)*X/2.
      FUN(2) = (1.-X)*(1.+X)   !/4.
      FUN(3) = (1.+X)*X/2.
      DER(1,1) =  x -.5     !- these were always OK
      DER(1,2) = -2.*x
      DER(1,3) =  x +.5
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF5NL (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for
C     a 5-noded line element
C              Dan Kidger, Sept 1998 - inspired by B+G
C
      IMPLICIT NONE
      INTEGER IDER,ISAMP,I 
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      REAL sm2,sm1,x,sp1,sp2, c1,c2,c3
      X=SAMP(I,1)

      SM2=X-1.0
      SM1=X-0.5
      SP1=X+0.5
      SP2=X+1.0
      C1=2./3.
      C2=8./3.
      C3=4.
      FUN(1)    =   C1*X*SP1*SM1*SM2
      FUN(2)   = -C2*X*SM1*SP2*SM2
      FUN(3)   =  C3*SP1*SM1*SP2*SM2
      FUN(4)   = -C2*X*SP1*SP2*SM2
      FUN(5)   =  C1*X*SP1*SM1*SP2
      DER(1,1) =  C1*(SM1*SM2*(SP1+X)  + X*SP1*(SM1+SM2))
      DER(1,2) = -C2*(SM1*SM2*(SP2+X)  + X*SP2*(SM1+SM2))
      DER(1,3) =  C3*(SP2*SM2*(SP1+SM1)+ SP1*SM1*(SP2+SM2))
      DER(1,4) = -C2*(SP2*SM2*(SP1+X)  + SP1*X*(SP2+SM2))
      DER(1,5) =  C1*(SM1*SP2*(SP1+X)  + SP1*X*(SM1+SP2))

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF3NT (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for
C     a 3-noded triangular element
C      < Jan'97  -- be careful of the order: clockwise or aclock? >
c
      REAL DER(IDER,*),SAMP(ISAMP,*),FUN(*)
      FUN(1)=SAMP(I,1)
      FUN(3)=SAMP(I,2)
      FUN(2)=1. -SAMP(I,1) -SAMP(I,2)
      DER(1,1)=1.
      DER(1,3)=0.
      DER(1,2)=-1.
      DER(2,1)=0.
      DER(2,3)=1.
      DER(2,2)=-1.
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF6NT (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for
C     a 6-noded triangular element
c       14-9-98  order reversed to clockwise (so like all other elements)
c            from 1-2-3-4-5-6 to 1-6-5-4-3-2
c            (noticed when comapring with Crisp)
C
      REAL DER(IDER,*),SAMP(ISAMP,*),FUN(*)
      C1=SAMP(I,1)
      C2=SAMP(I,2)
      C3=1.-C1-C2
      FUN(1)=(2.*C1-1.)*C1
      FUN(6)=4.*C1*C2
      FUN(5)=(2.*C2-1.)*C2
      FUN(4)=4.*C2*C3
      FUN(3)=(2.*C3-1.)*C3
      FUN(2)=4.*C3*C1
      DER(1,1)=4.*C1-1.
      DER(1,6)=4.*C2
      DER(1,5)=0.
      DER(1,4)=-4.*C2
      DER(1,3)=-(4.*C3-1.)
      DER(1,2)=4.*(C3-C1)
      DER(2,1)=0.
      DER(2,6)=4.*C1
      DER(2,5)=4.*C2-1.
      DER(2,4)=4.*(C3-C2)
      DER(2,3)=-(4.*C3-1.)
      DER(2,2)=-4.*C1
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF10NT (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for
C     a 10-noded triangular element
C          Created using REDUCE by  Dan Kidger  MAY 1990
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      X=SAMP(I,1)
      Y=SAMP(I,2)
      FUN(1)= ((3.*X-1.)*(3.*X-2.)*X)/2.
      FUN(2)=  (9.*(3.*X-1.)*X*Y)/2.
      FUN(3)=  (9.*(3.*Y-1.)*X*Y)/2.
      FUN(4)= ((3.*Y-1.)*(3.*Y-2.)*Y)/2.
      FUN(5)= -(9.*(X+Y-1.)*(3.*Y-1.)*Y)/2.
      FUN(6)=  (9.*(3.*X+3.*Y-2.)*(X+Y-1.)*Y)/2.
      FUN(7)=-((3.*X+3.*Y-1.)*(3.*X+3.*Y-2.)*(X+Y-1.))/2.
      FUN(8)=  (9.*(3.*X+3.*Y-2.)*(X+Y-1.)*X)/2.
      FUN(9)= -(9.*(3.*X-1.)*(X+Y-1.)*X)/2.
      FUN(10)=-27.*((Y-1.)+X)*X*Y
      DER(1,1)=(27.*X**2-18.*X+2.)/2.
      DER(1,2)=(9.*(6.*X-1.)*Y)/2.
      DER(1,3)=(9.*(3.*Y-1.)*Y)/2.
      DER(1,4)=0.
      DER(1,5)= -(9.*(3.*Y-1.)*Y)/2.
      DER(1,6)=  (9.*(6.*X+6.*Y-5.)*Y)/2.
      DER(1,7)=-(27.*X**2+54.*X*Y-36.*X+27.*Y**2-36.*Y+11.)/2.
      DER(1,8)=  (9.*(9.*X**2+12.*X*Y-10.*X+3.*Y**2-5.*Y+2.))/2.
      DER(1,9)= -(9.*(9.*X**2+6.*X*Y-8.*X-Y+1.))/2.
      DER(1,10)=-27.*(((Y-1.)+X)+X)*Y
      DER(2,1)=0.
      DER(2,2)=  (9.*(3.*X-1.)*X)/2.
      DER(2,3)=  (9.*(6.*Y-1.)*X)/2.
      DER(2,4)= (27.*Y**2-18.*Y+2.)/2.
      DER(2,5)= -(9.*((X+Y-1.)*(6.*Y-1.)+(3.*Y-1.)*Y))/2.
      DER(2,6)=  (9.*(3.*X**2+12.*X*Y-5.*X+9.*Y**2-10.*Y+2.))/2.
      DER(2,7)=-(27.*X**2+54.*X*Y-36.*X+27.*Y**2-36.*Y+11.)/2.
      DER(2,8)=  (9.*(6.*X+6.*Y-5.)*X)/2.
      DER(2,9)= -(9.*(3.*X-1.)*X)/2.
      DER(2,10)=-27.*(((Y-1.)+X)+Y)*X
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF15NT (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for
C     a 15-noded triangular element
C
      REAL DER(IDER,*),SAMP(ISAMP,*),FUN(*)
      C1=SAMP(I,1)
      C2=SAMP(I,2)
      C3=1.-C1-C2
      T1=C1-.25
      T2=C1-.5
      T3=C1-.75
      T4=C2-.25
      T5=C2-.5
      T6=C2-.75
      T7=C3-.25
      T8=C3-.5
      T9=C3-.75
      FUN(1)   =32./3.  *C1*T1*T2*T3
      FUN(2)   =128./3. *C1*C2*T1*T2
      FUN(3)   =64.     *C1*C2*T1*T4
      FUN(4)   =128./3. *C1*C2*T4*T5
      FUN(5)   =32./3.  *C2*T4*T5*T6
      FUN(6)   =128./3. *C2*C3*T4*T5
      FUN(7)   =64.     *C2*C3*T4*T7
      FUN(8)   =128./3. *C2*C3*T7*T8
      FUN(9)   =32./3.  *C3*T7*T8*T9
      FUN(10)  =128./3. *C3*C1*T7*T8
      FUN(11)  =64.     *C3*C1*T1*T7
      FUN(12)  =128./3. *C3*C1*T1*T2
      FUN(13)  =128.    *C1*C2*T1*C3
      FUN(14)  =128.    *C1*C2*C3*T4
      FUN(15)  =128.    *C1*C2*C3*T7

      DER(1,1) =32./3.*(T2*T3*(T1+C1)+C1*T1*(T3+T2))
      DER(1,2)=128./3.*C2*(T2*(T1+C1)+C1*T1)
      DER(1,3)=64.*C2*T4*(T1+C1)
      DER(1,4)=128./3.*C2*T4*T5
      DER(1,5)=0.
      DER(1,6)=-128./3.*C2*T4*T5
      DER(1,7)=-64.*C2*T4*(T7+C3)
      DER(1,8)=-128./3.*C2*(T8*(T7+C3)+C3*T7)
      DER(1,9)=-32./3.*(T8*T9*(T7+C3)+C3*T7*(T8+T9))
      DER(1,10)=128./3.*(C3*T7*T8-C1*(T8*(T7+C3)+C3*T7))
      DER(1,11)=64.*(C3*T7*(T1+C1)-C1*T1*(T7+C3))
      DER(1,12)=128./3.*(C3*(T2*(T1+C1)+C1*T1)-C1*T1*T2)
      DER(1,13)=128.*C2*(C3*(T1+C1)-C1*T1)
      DER(1,14)=128.*C2*T4*(C3-C1)
      DER(1,15)=128.*C2*(C3*T7-C1*(T7+C3))
      DER(2,1)=0.0
      DER(2,2)=128./3.*C1*T1*T2
      DER(2,3)=64.*C1*T1*(T4+C2)
      DER(2,4)=128./3.*C1*(T5*(T4+C2)+C2*T4)
      DER(2,5)=32./3.*(T5*T6*(T4+C2)+C2*T4*(T6+T5))
      DER(2,6)=128./3.*((C3*(T5*(T4+C2)+C2*T4))-C2*T4*T5)
      DER(2,7)=64.*(C3*T7*(T4+C2)-C2*T4*(T7+C3))
      DER(2,8)=128./3.*(C3*T7*T8-C2*(T8*(T7+C3)+C3*T7))
      DER(2,9)=-32./3.*(T8*T9*(T7+C3)+C3*T7*(T8+T9))
      DER(2,10)=-128./3.*C1*(T8*(T7+C3)+C3*T7)
      DER(2,11)=-64.*C1*T1*(T7+C3)
      DER(2,12)=-128./3.*C1*T1*T2
      DER(2,13)=128.*C1*T1*(C3-C2)
      DER(2,14)=128.*C1*(C3*(T4+C2)-C2*T4)
      DER(2,15)=128.*C1*(C3*T7-C2*(C3+T7))
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF4NQ (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and
C     their derivatives for a 4-noded quadrilateral element
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      XI =SAMP(I,1)
      ETA=SAMP(I,2)
      ETAM=.25*(1.-ETA)
      ETAP=.25*(1.+ETA)
      XIM=.25*(1.-XI)
      XIP=.25*(1.+XI)
      FUN(1)=4.*XIM*ETAM
      FUN(2)=4.*XIM*ETAP
      FUN(3)=4.*XIP*ETAP
      FUN(4)=4.*XIP*ETAM
      DER(1,1)=-ETAM
      DER(1,2)=-ETAP
      DER(1,3)=ETAP
      DER(1,4)=ETAM
      DER(2,1)=-XIM
      DER(2,2)=XIM
      DER(2,3)=XIP
      DER(2,4)=-XIP
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF5NQ_broken (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for
C     a 5-noded quadrilateral element
C         by  Dan Kidger         13th July 1992
C        .. the centre is a 'bubble function'
C   ** 6-1-97 found to be broken (sf5 /= 0 at corners **
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      X = SAMP(I,1)
      Y = SAMP(I,2)
      FUN(1)=-((4.*X*Y+4.*X+4.*Y+3.)*(X-1.)*(Y-1.))/4.
      FUN(2)=-((4.*X*Y-4.*X+4.*Y-3.)*(X-1.)*(Y+1.))/4.
      FUN(3)=-((4.*X*Y-4.*X-4.*Y+3.)*(X+1.)*(Y+1.))/4.
      FUN(4)=-((4.*X*Y+4.*X-4.*Y-3.)*(X+1.)*(Y-1.))/4.
      FUN(5)=(X+1.)*(X-1.)*(Y+1.)*(Y-1.)
      DER(1,1)=-((8.*X*Y+8.*X-1.)*(Y-1.))/4.
      DER(1,2)=-((8.*X*Y-8.*X+1.)*(Y+1.))/4.
      DER(1,3)=-((8.*X*Y-8.*X-1.)*(Y+1.))/4.
      DER(1,4)=-((8.*X*Y+8.*X+1.)*(Y-1.))/4.
      DER(1,5)=2.*(Y+1.)*(Y-1.)*X
      DER(2,1)=-((8.*X*Y+8.*Y-1.)*(X-1.))/4.
      DER(2,2)=-((8.*X*Y+8.*Y+1.)*(X-1.))/4.
      DER(2,3)=-((8.*X*Y-8.*Y-1.)*(X+1.))/4.
      DER(2,4)=-((8.*X*Y-8.*Y+1.)*(X+1.))/4.
      DER(2,5)=2.*(X+1.)*(X-1.)*Y
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF5NQ (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for
C     a 5-noded quadrilateral element
C         by  Dan Kidger         13th July 1992
C        .. the centre is a 'bubble function'
C   ** 6-1-97 partial hand-fix! **
C   Caveat: derivatives not yet fixed!

      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      X = SAMP(I,1)
      Y = SAMP(I,2)
      FUN(1) = (1.-X)*(1.-Y)/4.
      FUN(2) = (1.-X)*(1.+Y)/4.
      FUN(3) = (1.+X)*(1.+Y)/4.
      FUN(4) = (1.+X)*(1.-Y)/4.
      FUN(5) = (X+1.)*(X-1.)*(Y+1.)*(Y-1.)
      do j=1,4
        fun(j) = fun(j) - 1./4. * fun(5)
      enddo
      DER(1,1)=-((8.*X*Y+8.*X-1.)*(Y-1.))/4.
      DER(1,2)=-((8.*X*Y-8.*X+1.)*(Y+1.))/4.
      DER(1,3)=-((8.*X*Y-8.*X-1.)*(Y+1.))/4.
      DER(1,4)=-((8.*X*Y+8.*X+1.)*(Y-1.))/4.
      DER(1,5)=2.*(Y+1.)*(Y-1.)*X
      DER(2,1)=-((8.*X*Y+8.*Y-1.)*(X-1.))/4.
      DER(2,2)=-((8.*X*Y+8.*Y+1.)*(X-1.))/4.
      DER(2,3)=-((8.*X*Y-8.*Y-1.)*(X+1.))/4.
      DER(2,4)=-((8.*X*Y-8.*Y+1.)*(X+1.))/4.
      DER(2,5)=2.*(X+1.)*(X-1.)*Y
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF8NQ (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for 
C     a 8-noded quadrilateral element
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      XI =SAMP(I,1)
      ETA=SAMP(I,2)
      ETAM=.25*(1.-ETA)
      ETAP=.25*(1.+ETA)
      XIM=.25*(1.-XI)
      XIP=.25*(1.+XI)
      FUN(1)=4.*ETAM*XIM*(-XI-ETA-1.)
      FUN(2)=32.*ETAM*XIM*ETAP
      FUN(3)=4.*ETAP*XIM*(-XI+ETA-1.)
      FUN(4)=32.*XIM*XIP*ETAP
      FUN(5)=4.*ETAP*XIP*(XI+ETA-1.)
      FUN(6)=32.*ETAP*XIP*ETAM
      FUN(7)=4.*XIP*ETAM*(XI-ETA-1.)
      FUN(8)=32.*XIM*XIP*ETAM
      DER(1,1)=ETAM*(2.*XI+ETA)
      DER(1,2)=-8.*ETAM*ETAP
      DER(1,3)=ETAP*(2.*XI-ETA)
      DER(1,4)=-4.*ETAP*XI
      DER(1,5)=ETAP*(2.*XI+ETA)
      DER(1,6)=8.*ETAP*ETAM
      DER(1,7)=ETAM*(2.*XI-ETA)
      DER(1,8)=-4.*ETAM*XI
      DER(2,1)=XIM*(XI+2.*ETA)
      DER(2,2)=-4.*XIM*ETA
      DER(2,3)=XIM*(2.*ETA-XI)
      DER(2,4)=8.*XIM*XIP
      DER(2,5)=XIP*(XI+2.*ETA)
      DER(2,6)=-4.*XIP*ETA
      DER(2,7)=XIP*(2.*ETA-XI)
      DER(2,8)=-8.*XIM*XIP
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF9NQ (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for 
C     a 9-noded quadrilateral element
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      XI =SAMP(I,1)
      ETA=SAMP(I,2)
      ETAM=ETA-1.
      XIM=XI-1.
      ETAP=ETA+1.
      XIP=XI+1.
      X2P1=2.*XI+1.
      X2M1=2.*XI-1.
      E2P1=2.*ETA+1.
      E2M1=2.*ETA-1.
      FUN(1)=.25*XI*XIM*ETA*ETAM
      FUN(2)=-.5*XI*XIM*ETAP*ETAM
      FUN(3)=.25*XI*XIM*ETA*ETAP
      FUN(4)=-.5*XIP*XIM*ETA*ETAP
      FUN(5)=.25*XI*XIP*ETA*ETAP
      FUN(6)=-.5*XI*XIP*ETAP*ETAM
      FUN(7)=.25*XI*XIP*ETA*ETAM
      FUN(8)=-.5*XIP*XIM*ETA*ETAM
      FUN(9)=XIP*XIM*ETAP*ETAM
      DER(1,1)=.25*X2M1*ETA*ETAM
      DER(1,2)=-.5*X2M1*ETAP*ETAM
      DER(1,3)=.25*X2M1*ETA*ETAP
      DER(1,4)=-XI*ETA*ETAP
      DER(1,5)=.25*X2P1*ETA*ETAP
      DER(1,6)=-.5*X2P1*ETAP*ETAM
      DER(1,7)=.25*X2P1*ETA*ETAM
      DER(1,8)=-XI*ETA*ETAM
      DER(1,9)=2.*XI*ETAP*ETAM
      DER(2,1)=.25*XI*XIM*E2M1
      DER(2,2)=-XI*XIM*ETA
      DER(2,3)=.25*XI*XIM*E2P1
      DER(2,4)=-.5*XIP*XIM*E2P1
      DER(2,5)=.25*XI*XIP*E2P1
      DER(2,6)=-XI*XIP*ETA
      DER(2,7)=.25*XI*XIP*E2M1
      DER(2,8)=-.5*XIP*XIM*E2M1
      DER(2,9)=2.*XIP*XIM*ETA
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF12NQ (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for 
C     a 12-noded quadrilateral element
C         by  Dan Kidger         8th April 1992
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      X = SAMP(I,1)
      Y = SAMP(I,2)
      FUN(1)= ((9.*X**2+9.*Y**2-10.)*(X-1.)*(Y-1.))/32.
      FUN(2)=-(9.*(X-1.)*(3.*Y-1.)*(Y+1.)*(Y-1.))/32.
      FUN(3)= (9.*(X-1.)*(3.*Y+1.)*(Y+1.)*(Y-1.))/32.
      FUN(4)=-((9.*X**2+9.*Y**2-10.)*(X-1.)*(Y+1.))/32.
      FUN(5)= (9.*(3.*X-1.)*(X+1.)*(X-1.)*(Y+1.))/32.
      FUN(6)=-(9.*(3.*X+1.)*(X+1.)*(X-1.)*(Y+1.))/32.
      FUN(7)= ((9.*X**2+9.*Y**2-10.)*(X+1.)*(Y+1.))/32.
      FUN(8)=-(9.*(X+1.)*(3.*Y+1.)*(Y+1.)*(Y-1.))/32.
      FUN(9)= (9.*(X+1.)*(3.*Y-1.)*(Y+1.)*(Y-1.))/32.
      FUN(10)=-((9.*X**2+9.*Y**2-10.)*(X+1.)*(Y-1.))/32.
      FUN(11)= (9.*(3.*X+1.)*(X+1.)*(X-1.)*(Y-1.))/32.
      FUN(12)=-(9.*(3.*X-1.)*(X+1.)*(X-1.)*(Y-1.))/32.
      DER(1,1)= ((27.*X**2-18.*X+9.*Y**2-10.)*(Y-1.))/32.
      DER(1,2)=-(9.*(3.*Y-1.)*(Y+1.)*(Y-1.))/32.
      DER(1,3)= (9.*(3.*Y+1.)*(Y+1.)*(Y-1.))/32.
      DER(1,4)=-((27.*X**2-18.*X+9.*Y**2-10.)*(Y+1.))/32.
      DER(1,5)= (9.*(9.*X**2-2.*X-3.)*(Y+1.))/32.
      DER(1,6)=-(9.*(9.*X**2+2.*X-3.)*(Y+1.))/32.
      DER(1,7)= ((27.*X**2+18.*X+9.*Y**2-10.)*(Y+1.))/32.
      DER(1,8)=-(9.*(3.*Y+1.)*(Y+1.)*(Y-1.))/32.
      DER(1,9)= (9.*(3.*Y-1.)*(Y+1.)*(Y-1.))/32.
      DER(1,10)=-((27.*X**2+18.*X+9.*Y**2-10.)*(Y-1.))/32.
      DER(1,11)= (9.*(9.*X**2+2.*X-3.)*(Y-1.))/32.
      DER(1,12)=-(9.*(9.*X**2-2.*X-3.)*(Y-1.))/32.
      DER(2,1)= ((9.*X**2+27.*Y**2-18.*Y-10.)*(X-1.))/32.
      DER(2,2)=-(9.*(X-1.)*(9.*Y**2-2.*Y-3.))/32.
      DER(2,3)= (9.*(X-1.)*(9.*Y**2+2.*Y-3.))/32.
      DER(2,4)=-((9.*X**2+27.*Y**2+18.*Y-10.)*(X-1.))/32.
      DER(2,5)= (9.*(3.*X-1.)*(X+1.)*(X-1.))/32.
      DER(2,6)=-(9.*(3.*X+1.)*(X+1.)*(X-1.))/32.
      DER(2,7)= ((9.*X**2+27.*Y**2+18.*Y-10.)*(X+1.))/32.
      DER(2,8)=-(9.*(X+1.)*(9.*Y**2+2.*Y-3.))/32.
      DER(2,9)= (9.*(X+1.)*(9.*Y**2-2.*Y-3.))/32.
      DER(2,10)=-((9.*X**2+27.*Y**2-18.*Y-10.)*(X+1.))/32.
      DER(2,11)= (9.*(3.*X+1.)*(X+1.)*(X-1.))/32.
      DER(2,12)=-(9.*(3.*X-1.)*(X+1.)*(X-1.))/32.
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF17NQ (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their derivatives for 
C     a 17-noded quadrilateral element
C         by  Dan Kidger      April 1992
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      X = SAMP(I,1)
      Y = SAMP(I,2)
      FUN(1)=-((4.*X**3-3.*X*Y-4.*X+4.*Y**3-4.*Y)*(X-1.)*(Y-1.))/12.
      FUN(2)= (2.*(X-1.)*(2.*Y-1.)*(Y+1.)*(Y-1.)*Y)/3.
      FUN(3)=-((X+4.*Y**2)*(X-1.)*(Y+1.)*(Y-1.))/2.
      FUN(4)= (2.*(X-1.)*(2.*Y+1.)*(Y+1.)*(Y-1.)*Y)/3.
      FUN(5)= ((4.*X**3+3.*X*Y-4.*X-4.*Y**3+4.*Y)*(X-1.)*(Y+1.))/12.
      FUN(6)=-(2.*(2.*X-1.)*(X+1.)*(X-1.)*(Y+1.)*X)/3.
      FUN(7)= ((4.*X**2-Y)*(X+1.)*(X-1.)*(Y+1.))/2.
      FUN(8)=-(2.*(2.*X+1.)*(X+1.)*(X-1.)*(Y+1.)*X)/3.
      FUN(9)= ((4.*X**3+3.*X*Y-4.*X+4.*Y**3-4.*Y)*(X+1.)*(Y+1.))/12.
      FUN(10)=-(2.*(X+1.)*(2.*Y+1.)*(Y+1.)*(Y-1.)*Y)/3.
      FUN(11)=-((X-4.*Y**2)*(X+1.)*(Y+1.)*(Y-1.))/2.
      FUN(12)=-(2.*(X+1.)*(2.*Y-1.)*(Y+1.)*(Y-1.)*Y)/3.
      FUN(13)=-((4.*X**3-3.*X*Y-4.*X-4.*Y**3+4.*Y)*(X+1.)*(Y-1.))/
     . 12.
      FUN(14)= (2.*(2.*X+1.)*(X+1.)*(X-1.)*(Y-1.)*X)/3.
      FUN(15)=-((4.*X**2+Y)*(X+1.)*(X-1.)*(Y-1.))/2.
      FUN(16)= (2.*(2.*X-1.)*(X+1.)*(X-1.)*(Y-1.)*X)/3.
      FUN(17)= (X+1.)*(X-1.)*(Y+1.)*(Y-1.)
      DER(1,1)=-((16.*X**3-12.*X**2-6.*X*Y-8.*X+4.*Y**3-Y+4.)*(Y-1.)
     . )/12.
      DER(1,2)= (2.*(2.*Y-1.)*(Y+1.)*(Y-1.)*Y)/3.
      DER(1,3)=-(((X+4.*Y**2)+(X-1.))*(Y+1.)*(Y-1.))/2.
      DER(1,4)= (2.*(2.*Y+1.)*(Y+1.)*(Y-1.)*Y)/3.
      DER(1,5)= ((16.*X**3-12.*X**2+6.*X*Y-8.*X-4.*Y**3+Y+4.)*(Y+1.))
     . /12.
      DER(1,6)=-(2.*(8.*X**3-3.*X**2-4.*X+1.)*(Y+1.))/3.
      DER(1,7)= (8.*X**2-Y-4.)*(Y+1.)*X
      DER(1,8)=-(2.*(8.*X**3+3.*X**2-4.*X-1.)*(Y+1.))/3.
      DER(1,9)= ((16.*X**3+12.*X**2+6.*X*Y-8.*X+4.*Y**3-Y-4.)*(Y+1.))
     . /12.
      DER(1,10)=-(2.*(2.*Y+1.)*(Y+1.)*(Y-1.)*Y)/3.
      DER(1,11)=-(((X-4.*Y**2)+(X+1.))*(Y+1.)*(Y-1.))/2.
      DER(1,12)=-(2.*(2.*Y-1.)*(Y+1.)*(Y-1.)*Y)/3.
      DER(1,13)=-((16.*X**3+12.*X**2-6.*X*Y-8.*X-4.*Y**3+Y-4.)*(Y-1.
     . ))/12.
      DER(1,14)= (2.*(8.*X**3+3.*X**2-4.*X-1.)*(Y-1.))/3.
      DER(1,15)=-(8.*X**2+Y-4.)*(Y-1.)*X
      DER(1,16)= (2.*(8.*X**3-3.*X**2-4.*X+1.)*(Y-1.))/3.
      DER(1,17)=2.*(Y+1.)*(Y-1.)*X
      DER(2,1)=-((4.*X**3-6.*X*Y-X+16.*Y**3-12.*Y**2-8.*Y+4.)*(X-1.)
     . )/12.
      DER(2,2)= (2.*(X-1.)*(8.*Y**3-3.*Y**2-4.*Y+1.))/3.
      DER(2,3)=-(X+8.*Y**2-4.)*(X-1.)*Y
      DER(2,4)= (2.*(X-1.)*(8.*Y**3+3.*Y**2-4.*Y-1.))/3.
      DER(2,5)= ((4.*X**3+6.*X*Y-X-16.*Y**3-12.*Y**2+8.*Y+4.)*(X-1.))
     . /12.
      DER(2,6)=-(2.*(2.*X-1.)*(X+1.)*(X-1.)*X)/3.
      DER(2,7)= (((4.*X**2-Y)-(Y+1.))*(X+1.)*(X-1.))/2.
      DER(2,8)=-(2.*(2.*X+1.)*(X+1.)*(X-1.)*X)/3.
      DER(2,9)= ((4.*X**3+6.*X*Y-X+16.*Y**3+12.*Y**2-8.*Y-4.)*(X+1.))
     . /12.
      DER(2,10)=-(2.*(X+1.)*(8.*Y**3+3.*Y**2-4.*Y-1.))/3.
      DER(2,11)=-(X-8.*Y**2+4.)*(X+1.)*Y
      DER(2,12)=-(2.*(X+1.)*(8.*Y**3-3.*Y**2-4.*Y+1.))/3.
      DER(2,13)=-((4.*X**3-6.*X*Y-X-16.*Y**3+12.*Y**2+8.*Y-4.)*(X+1.
     . ))/12.
      DER(2,14)= (2.*(2.*X+1.)*(X+1.)*(X-1.)*X)/3.
      DER(2,15)=-(((4.*X**2+Y)+(Y-1.))*(X+1.)*(X-1.))/2.
      DER(2,16)= (2.*(2.*X-1.)*(X+1.)*(X-1.)*X)/3.
      DER(2,17)=2.*(X+1.)*(X-1.)*Y
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF4NP (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and
C     their derivatives for a 4-node tetrahedron element
C     ... 6-4-95  nodes 1 and 2 swapped to give 'right-handed elements
C
      REAL DER(IDER,*),SAMP(ISAMP,*),FUN(*)
      FUN(2)=SAMP(I,1)
      FUN(1)=SAMP(I,2)
      FUN(3)=SAMP(I,3)
      FUN(4)=1.-FUN(1)-FUN(2)-FUN(3)
      DO M=1,3
        DO N=1,4
          DER(M,N)=0.
        ENDDO
      ENDDO
      DER(1,2)=1.
      DER(2,1)=1.
      DER(3,3)=1.
      DER(1,4)=-1.
      DER(2,4)=-1.
      DER(3,4)=-1.
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF5NP (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions (and their derivatives) for 
C     a 5-node pyramid element
c      16-6-97 Dan Kidger - from paper by G. Bedrosian IJNMENG'92
C     ... 6-4-95  nodes 1 and 2 swapped to give 'right-handed elements
C
      REAL FUN(*),DER(IDER,*),SAMP(ISAMP,*)
      x1=SAMP(I,1)
      x3=SAMP(I,2)                     ! hmm what if ==1 ??
      x2=SAMP(I,3)  
c     bit = x1*x2  *x3/(1.-x3)         ! usu x3=-1->0. ?
      bit = x1*x2  *x3/(1.+abs(x3))    ! so +1==-1
      FUN(1) = 0.25*( (1+x1)*(1+x2) - x3 + bit )
      FUN(2) = 0.25*( (1-x1)*(1+x2) - x3 - bit )
      FUN(3) = 0.25*( (1-x1)*(1-x2) - x3 + bit )
      FUN(4) = 0.25*( (1+x1)*(1-x2) - x3 - bit )
      FUN(5)=x3

c... hack to all derivs to zero for now. :-(
      DO M=1,3
        DO N=1,5
          DER(M,N)=0.
        ENDDO
      ENDDO

      RETURN
      END

C-----------------------------------------------------------------------
c     SUBROUTINE SF13NP (DER,IDER,FUN,SAMP,ISAMP,I)
c     5 corner nodes + 4+4 edges nodes
c      ..to be continued (19-6-97)
c
c     END
C-----------------------------------------------------------------------
      SUBROUTINE SF8NB (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their
C     derivatives for a 8-noded brick element
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      XI  = SAMP(I,1)
      ETA = SAMP(I,2)
      ZETA= SAMP(I,3)
      ETAM=1.-ETA
      XIM=1.-XI
      ZETAM=1.-ZETA
      ETAP=ETA+1.
      XIP=XI+1.
      ZETAP=ZETA+1.
      FUN(1)=.125*XIM*ETAM*ZETAM
      FUN(2)=.125*XIM*ETAM*ZETAP
      FUN(3)=.125*XIP*ETAM*ZETAP
      FUN(4)=.125*XIP*ETAM*ZETAM
      FUN(5)=.125*XIM*ETAP*ZETAM
      FUN(6)=.125*XIM*ETAP*ZETAP
      FUN(7)=.125*XIP*ETAP*ZETAP
      FUN(8)=.125*XIP*ETAP*ZETAM
      DER(1,1)=-.125*ETAM*ZETAM
      DER(1,2)=-.125*ETAM*ZETAP
      DER(1,3)=.125*ETAM*ZETAP
      DER(1,4)=.125*ETAM*ZETAM
      DER(1,5)=-.125*ETAP*ZETAM
      DER(1,6)=-.125*ETAP*ZETAP
      DER(1,7)=.125*ETAP*ZETAP
      DER(1,8)=.125*ETAP*ZETAM
      DER(2,1)=-.125*XIM*ZETAM
      DER(2,2)=-.125*XIM*ZETAP
      DER(2,3)=-.125*XIP*ZETAP
      DER(2,4)=-.125*XIP*ZETAM
      DER(2,5)=.125*XIM*ZETAM
      DER(2,6)=.125*XIM*ZETAP
      DER(2,7)=.125*XIP*ZETAP
      DER(2,8)=.125*XIP*ZETAM
      DER(3,1)=-.125*XIM*ETAM
      DER(3,2)=.125*XIM*ETAM
      DER(3,3)=.125*XIP*ETAM
      DER(3,4)=-.125*XIP*ETAM
      DER(3,5)=-.125*XIM*ETAP
      DER(3,6)=.125*XIM*ETAP
      DER(3,7)=.125*XIP*ETAP
      DER(3,8)=-.125*XIP*ETAP
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF11NB (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their
C     derivatives for an 11-noded brick element
C        Created by Dan Kidger 1990
C
C     18-4-95 put back the missing call to SF8NB

      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)

      CALL SF8NB  (DER,IDER,FUN,SAMP, ISMP,I)
      XI  = SAMP(I,1)
      ETA = SAMP(I,2)
      ZETA= SAMP(I,3)
      FUN( 9) = 1.0-  XI*XI
      FUN(10) = 1.0- ETA*ETA
      FUN(11) = 1.0-ZETA*ZETA
      DO I=1,3
        DO J=9,11
          DER(I, J) =  0.0
        ENDDO
      ENDDO
      DER(1, 9) = -2.0*XI
      DER(2,10) = -2.0*ETA
      DER(3,11) = -2.0*ZETA
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF14NB (DER,IDER,FUN,SAMP,ISAMP,I,J,K)
C
C     This forms the shape functions and their
c     derivatives for a 14-noded brick element
c     Type A:   X*Y*Y etc terms
c
C        by Dan Kidger MAY 1989          
c
C   ***  DONT USE THIS .. use N14_AB for a better element ! (29-4-93)
c
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      X=1+SAMP(I,1)
      Y=1+SAMP(J,1)
      Z=1+SAMP(K,1)
      FUN(1)=-(2.*((Z-4.)*Y+Z**2-4.*Z+7.)*X+(2.*Y-3.)*X**2
     . -2.*(4.*Z-7.)*Y+(2.*Z-3.)*Y**2-3.*Z**2+14.*Z-16.)/16.
      FUN(2)=(2.*((Z-4.)*Y-Z**2+2.*Z+1.)*X+(2.*Y-1.)*X**2-
     . (2.*Z-3.)*Y**2+2.*Y+3.*Z**2-6.*Z)/16.
      FUN(3)=(2.*(Y*Z+Z**2-4.*Z+1.)*X-(2.*Y-3.)*X**2-(Y-2.
     . )*(2.*Z-3.)*Y-Z**2+2.*Z)/16.
      FUN(4)=-(2.*(Y*Z-Z**2+2.*Z-1.)*X-(2.*Y-1.)*X**2+(Y-
     . 2.)*(2.*Z-3.)*Y+Z**2-2.*Z)/16.
      FUN(5)=(2.*((Z+2.)*Y-Z**2-3.)*X-(2.*Y-3.)*X**2-2.*(
     . 4.*Z-1.)*Y+(2.*Z-1.)*Y**2+3.*Z**2+2.*Z)/16.
      FUN(6)=-(2.*((Z+2.)*Y+Z**2-2.*Z-1.)*X-(2.*Y-1.)*X**2
     . -(2.*Z-1.)*Y**2-2.*Y-3.*Z**2+6.*Z)/16.
      FUN(7)=-(2.*((Z-2.)*Y-Z**2+3.)*X+(2.*Y-3.)*X**2-(Y-
     . 2.)*(2.*Z-1.)*Y+Z**2-2.*Z)/16.
      FUN(8)=(2.*((Z-2.)*Y+Z**2-2.*Z+1.)*X+(2.*Y-1.)*X**2+
     . (Y-2.)*(2.*Z-1.)*Y-Z**2+2.*Z)/16.
      FUN(9)=((Y-2.)*(2.*Z-3.)*Y-X**2+2.*X+Z**2-2.*Z)/4.
      FUN(10)=-((Y-2.)*(2.*Z-1.)*Y+X**2-2.*X-Z**2+2.*Z)/4.
      FUN(11)=((2.*Y-3.)*X**2-2.*(2.*Y-3.)*X+Y**2-2.*Y-Z**2+2.*Z)/4.
      FUN(12)=-((2.*Y-1.)*X**2-2.*(2.*Y-1.)*X-Y**2+2.*Y+Z**2-2.*Z)/4.
      FUN(13)=(2.*(Z**2-2.*Z-1.)*X+X**2-Y**2+2.*Y-3.*Z**2+6.*Z)/4.
      FUN(14)=-(2.*(Z-1.)**2*X-X**2+Y**2-2.*Y-Z**2+2.*Z)/4.
      DER(1,1)=-(((Z-4.)*Y+Z**2-4.*Z+7.)+(2.*Y-3.)*X)/8.
      DER(1,2)=(((Z-4.)*Y-Z**2+2.*Z+1.)+(2.*Y-1.)*X)/8.
      DER(1,3)=((Y*Z+Z**2-4.*Z+1.)-(2.*Y-3.)*X)/8.
      DER(1,4)=-((Y*Z-Z**2+2.*Z-1.)-(2.*Y-1.)*X)/8.
      DER(1,5)=(((Z+2.)*Y-Z**2-3.)-(2.*Y-3.)*X)/8.
      DER(1,6)=-(((Z+2.)*Y+Z**2-2.*Z-1.)-(2.*Y-1.)*X)/8.
      DER(1,7)=-(((Z-2.)*Y-Z**2+3.)+(2.*Y-3.)*X)/8.
      DER(1,8)=(((Z-2.)*Y+Z**2-2.*Z+1.)+(2.*Y-1.)*X)/8.
      DER(1,9) =-(X-1.)/2.
      DER(1,10)=-(X-1.)/2.
      DER(1,11)=((X-1.)*(2.*Y-3.))/2.
      DER(1,12)=-((X-1.)*(2.*Y-1.))/2.
      DER(1,13)=((Z**2-2.*Z-1.)+X)/2.
      DER(1,14)=-((Z-1.)**2-X)/2.
      DER(2,1)=((4.*Z-7.)-(2.*Z-3.)*Y-(Z-4.)*X-X**2)/8.
      DER(2,2)=-((2.*Z-3.)*Y-(Z-4.)*X-X**2-1.)/8.
      DER(2,3)=-(X**2-X*Z+2.*Y*Z-3.*Y-2.*Z+3.)/8.
      DER(2,4)=(X**2-X*Z-2.*Y*Z+3.*Y+2.*Z-3.)/8.
      DER(2,5)=-((4.*Z-1.)-(2.*Z-1.)*Y-(Z+2.)*X+X**2)/8.
      DER(2,6)=((2.*Z-1.)*Y-(Z+2.)*X+X**2+1.)/8.
      DER(2,7)=-(X**2+X*Z-2.*X-2.*Y*Z+Y+2.*Z-1.)/8.
      DER(2,8)=(X**2+X*Z-2.*X+2.*Y*Z-Y-2.*Z+1.)/8.
      DER(2,9)=((Y-1.)*(2.*Z-3.))/2.
      DER(2,10)=-((Y-1.)*(2.*Z-1.))/2.
      DER(2,11)=(X**2-2.*X+Y-1.)/2.
      DER(2,12)=-(X**2-2.*X-Y+1.)/2.
      DER(2,13)=-(Y-1.)/2.
      DER(2,14)=-(Y-1.)/2.
      DER(3,1)=-((Y+2.*Z-4.)*X+Y**2-4.*Y-3.*Z+7.)/8.
      DER(3,2)=((Y-2.*Z+2.)*X-Y**2+3.*Z-3.)/8.
      DER(3,3)=((Y+2.*Z-4.)*X-(Y-2.)*Y-Z+1.)/8.
      DER(3,4)=-((Y-2.*Z+2.)*X+(Y-2.)*Y+Z-1.)/8.
      DER(3,5)=((Y-2.*Z)*X+Y**2-4.*Y+3.*Z+1.)/8.
      DER(3,6)=-((Y+2.*Z-2.)*X-Y**2-3.*Z+3.)/8.
      DER(3,7)=-((Y-2.*Z)*X-(Y-2.)*Y+Z-1.)/8.
      DER(3,8)=((Y+2.*Z-2.)*X+(Y-2.)*Y-Z+1.)/8.
      DER(3,9)=((Y-2.)*Y+Z-1.)/2.
      DER(3,10)=-((Y-2.)*Y-Z+1.)/2.
      DER(3,11)=-(Z-1.)/2.
      DER(3,12)=-(Z-1.)/2.
      DER(3,13)=((2.*X-3.)*(Z-1.))/2.
      DER(3,14)=-((2.*X-1.)*(Z-1.))/2.
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF20NB (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their
c     derivatives for a 20-noded brick element
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      INTEGER XII(20),ETAI(20),ZETAI(20)
      XI  = SAMP(I,1)
      ETA = SAMP(I,2)
      ZETA= SAMP(I,3)
      XII(1)=-1
      XII(2)=-1
      XII(3)=-1
      XII(9)=-1
      XII(10)=-1
      XII(13)=-1
      XII(14)=-1
      XII(15)=-1
      XII(4)=0
      XII(8)=0
      XII(16)=0
      XII(20)=0
      XII(5)=1
      XII(6)=1
      XII(7)=1
      XII(11)=1
      XII(12)=1
      XII(17)=1
      XII(18)=1
      XII(19)=1
      DO L=1,8
        ETAI(L)=-1
      ENDDO
      DO L=9,12
        ETAI(L)=0
      ENDDO
      DO L=13,20
        ETAI(L)=1
      ENDDO
      ZETAI(1)=-1
      ZETAI(7)=-1
      ZETAI(8)=-1
      ZETAI(9)=-1
      ZETAI(12)=-1
      ZETAI(13)=-1
      ZETAI(19)=-1
      ZETAI(20)=-1
      ZETAI(2)=0
      ZETAI(6)=0
      ZETAI(14)=0
      ZETAI(18)=0
      ZETAI(3)=1
      ZETAI(4)=1
      ZETAI(5)=1
      ZETAI(10)=1
      ZETAI(11)=1
      ZETAI(15)=1
      ZETAI(16)=1
      ZETAI(17)=1
      DO L=1,20
        XIO=XI*XII(L)
        ETAO=ETA*ETAI(L)
        ZETAO=ZETA*ZETAI(L)
        IF(L.EQ.4.OR.L.EQ.8.OR.L.EQ.16.OR.L.EQ.20)THEN
          FUN(L)=.25*(1.-XI*XI)*(1.+ETAO)*(1.+ZETAO)
          DER(1,L)=-.5*XI*(1.+ETAO)*(1.+ZETAO)
          DER(2,L)=.25*ETAI(L)*(1.-XI*XI)*(1.+ZETAO)
          DER(3,L)=.25*ZETAI(L)*(1.-XI*XI)*(1.+ETAO)
        ELSE IF(L.GE.9.AND.L.LE.12)THEN
          FUN(L)=.25*(1.+XIO)*(1.-ETA*ETA)*(1.+ZETAO)
          DER(1,L)=.25*XII(L)*(1.-ETA*ETA)*(1.+ZETAO)
          DER(2,L)=-.5*ETA*(1.+XIO)*(1.+ZETAO)
          DER(3,L)=.25*ZETAI(L)*(1.+XIO)*(1.-ETA*ETA)
        ELSE IF(L.EQ.2.OR.L.EQ.6.OR.L.EQ.14.OR.L.EQ.18)THEN
          FUN(L)=.25*(1.+XIO)*(1.+ETAO)*(1.-ZETA*ZETA)
          DER(1,L)=.25*XII(L)*(1.+ETAO)*(1.-ZETA*ZETA)
          DER(2,L)=.25*ETAI(L)*(1.+XIO)*(1.-ZETA*ZETA)
          DER(3,L)=-.5*ZETA*(1.+XIO)*(1.+ETAO)
        ELSE
      FUN(L)=.125*(1.+XIO)*(1.+ETAO)*(1.+ZETAO)*(XIO+ETAO+ZETAO-2.)
      DER(1,L)=.125*XII(L)*(1.+ETAO)*(1.+ZETAO)*(2.*XIO+ETAO+ZETAO-1.)
      DER(2,L)=.125*ETAI(L)*(1.+XIO)*(1.+ZETAO)*(XIO+2.*ETAO+ZETAO-1.)
      DER(3,L)=.125*ZETAI(L)*(1.+XIO)*(1.+ETAO)*(XIO+ETAO+2.*ZETAO-1.)
        END IF
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF20NB_A (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their
C     derivatives for 20-noded brick elements
C         Created using REDUCE by Dan Kidger,  March  1992
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      X = SAMP(I,1)
      Y = SAMP(I,2)
      Z = SAMP(I,3)
      FUN(1)= ((X+Y+Z+2.)*(X-1.)*(Y-1.)*(Z-1.))/8.
      FUN(2)=-((X-1.)*(Y+1.)*(Y-1.)*(Z-1.))/4.
      FUN(3)=-((X-Y+Z+2.)*(X-1.)*(Y+1.)*(Z-1.))/8.
      FUN(4)= ((X+1.)*(X-1.)*(Y+1.)*(Z-1.))/4.
      FUN(5)=-((X+Y-Z-2.)*(X+1.)*(Y+1.)*(Z-1.))/8.
      FUN(6)= ((X+1.)*(Y+1.)*(Y-1.)*(Z-1.))/4.
      FUN(7)= ((X-Y-Z-2.)*(X+1.)*(Y-1.)*(Z-1.))/8.
      FUN(8)=-((X+1.)*(X-1.)*(Y-1.)*(Z-1.))/4.
      FUN(9)=-((X-1.)*(Y-1.)*(Z+1.)*(Z-1.))/4.
      FUN(10)= ((X-1.)*(Y+1.)*(Z+1.)*(Z-1.))/4.
      FUN(11)=-((X+1.)*(Y+1.)*(Z+1.)*(Z-1.))/4.
      FUN(12)= ((X+1.)*(Y-1.)*(Z+1.)*(Z-1.))/4.
      FUN(13)=-((X+Y-Z+2.)*(X-1.)*(Y-1.)*(Z+1.))/8.
      FUN(14)= ((X-1.)*(Y+1.)*(Y-1.)*(Z+1.))/4.
      FUN(15)= ((X-Y-Z+2.)*(X-1.)*(Y+1.)*(Z+1.))/8.
      FUN(16)=-((X+1.)*(X-1.)*(Y+1.)*(Z+1.))/4.
      FUN(17)= ((X+Y+Z-2.)*(X+1.)*(Y+1.)*(Z+1.))/8.
      FUN(18)=-((X+1.)*(Y+1.)*(Y-1.)*(Z+1.))/4.
      FUN(19)=-((X-Y+Z-2.)*(X+1.)*(Y-1.)*(Z+1.))/8.
      FUN(20)= ((X+1.)*(X-1.)*(Y-1.)*(Z+1.))/4.
      DER(1,1)= ((2.*X+Y+Z+1.)*(Y-1.)*(Z-1.))/8.
      DER(1,2)=-((Y+1.)*(Y-1.)*(Z-1.))/4.
      DER(1,3)=-((2.*X-Y+Z+1.)*(Y+1.)*(Z-1.))/8.
      DER(1,4)= ((Y+1.)*(Z-1.)*X)/2.
      DER(1,5)=-((2.*X+Y-Z-1.)*(Y+1.)*(Z-1.))/8.
      DER(1,6)= ((Y+1.)*(Y-1.)*(Z-1.))/4.
      DER(1,7)= ((2.*X-Y-Z-1.)*(Y-1.)*(Z-1.))/8.
      DER(1,8)=-((Y-1.)*(Z-1.)*X)/2.
      DER(1,9)=-((Y-1.)*(Z+1.)*(Z-1.))/4.
      DER(1,10)= ((Y+1.)*(Z+1.)*(Z-1.))/4.
      DER(1,11)=-((Y+1.)*(Z+1.)*(Z-1.))/4.
      DER(1,12)= ((Y-1.)*(Z+1.)*(Z-1.))/4.
      DER(1,13)=-((2.*X+Y-Z+1.)*(Y-1.)*(Z+1.))/8.
      DER(1,14)= ((Y+1.)*(Y-1.)*(Z+1.))/4.
      DER(1,15)= ((2.*X-Y-Z+1.)*(Y+1.)*(Z+1.))/8.
      DER(1,16)=-((Y+1.)*(Z+1.)*X)/2.
      DER(1,17)= ((2.*X+Y+Z-1.)*(Y+1.)*(Z+1.))/8.
      DER(1,18)=-((Y+1.)*(Y-1.)*(Z+1.))/4.
      DER(1,19)=-((2.*X-Y+Z-1.)*(Y-1.)*(Z+1.))/8.
      DER(1,20)= ((Y-1.)*(Z+1.)*X)/2.
      DER(2,1)= ((X+2.*Y+Z+1.)*(X-1.)*(Z-1.))/8.
      DER(2,2)=-((X-1.)*(Z-1.)*Y)/2.
      DER(2,3)=-((X-2.*Y+Z+1.)*(X-1.)*(Z-1.))/8.
      DER(2,4)= ((X+1.)*(X-1.)*(Z-1.))/4.
      DER(2,5)=-((X+2.*Y-Z-1.)*(X+1.)*(Z-1.))/8.
      DER(2,6)= ((X+1.)*(Z-1.)*Y)/2.
      DER(2,7)= ((X-2.*Y-Z-1.)*(X+1.)*(Z-1.))/8.
      DER(2,8)=-((X+1.)*(X-1.)*(Z-1.))/4.
      DER(2,9)=-((X-1.)*(Z+1.)*(Z-1.))/4.
      DER(2,10)= ((X-1.)*(Z+1.)*(Z-1.))/4.
      DER(2,11)=-((X+1.)*(Z+1.)*(Z-1.))/4.
      DER(2,12)= ((X+1.)*(Z+1.)*(Z-1.))/4.
      DER(2,13)=-((X+2.*Y-Z+1.)*(X-1.)*(Z+1.))/8.
      DER(2,14)= ((X-1.)*(Z+1.)*Y)/2.
      DER(2,15)= ((X-2.*Y-Z+1.)*(X-1.)*(Z+1.))/8.
      DER(2,16)=-((X+1.)*(X-1.)*(Z+1.))/4.
      DER(2,17)= ((X+2.*Y+Z-1.)*(X+1.)*(Z+1.))/8.
      DER(2,18)=-((X+1.)*(Z+1.)*Y)/2.
      DER(2,19)=-((X-2.*Y+Z-1.)*(X+1.)*(Z+1.))/8.
      DER(2,20)= ((X+1.)*(X-1.)*(Z+1.))/4.
      DER(3,1)= ((X+Y+2.*Z+1.)*(X-1.)*(Y-1.))/8.
      DER(3,2)=-((X-1.)*(Y+1.)*(Y-1.))/4.
      DER(3,3)=-((X-Y+2.*Z+1.)*(X-1.)*(Y+1.))/8.
      DER(3,4)= ((X+1.)*(X-1.)*(Y+1.))/4.
      DER(3,5)=-((X+Y-2.*Z-1.)*(X+1.)*(Y+1.))/8.
      DER(3,6)= ((X+1.)*(Y+1.)*(Y-1.))/4.
      DER(3,7)= ((X-Y-2.*Z-1.)*(X+1.)*(Y-1.))/8.
      DER(3,8)=-((X+1.)*(X-1.)*(Y-1.))/4.
      DER(3,9)=-((X-1.)*(Y-1.)*Z)/2.
      DER(3,10)= ((X-1.)*(Y+1.)*Z)/2.
      DER(3,11)=-((X+1.)*(Y+1.)*Z)/2.
      DER(3,12)= ((X+1.)*(Y-1.)*Z)/2.
      DER(3,13)=-((X+Y-2.*Z+1.)*(X-1.)*(Y-1.))/8.
      DER(3,14)= ((X-1.)*(Y+1.)*(Y-1.))/4.
      DER(3,15)= ((X-Y-2.*Z+1.)*(X-1.)*(Y+1.))/8.
      DER(3,16)=-((X+1.)*(X-1.)*(Y+1.))/4.
      DER(3,17)= ((X+Y+2.*Z-1.)*(X+1.)*(Y+1.))/8.
      DER(3,18)=-((X+1.)*(Y+1.)*(Y-1.))/4.
      DER(3,19)=-((X-Y+2.*Z-1.)*(X+1.)*(Y-1.))/8.
      DER(3,20)= ((X+1.)*(X-1.)*(Y-1.))/4.
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF26NB (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their
C     derivatives for a 26-noded brick element
C         Created using REDUCE by Dan Kidger,  March  1992
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      X = SAMP(I,1)
      Y = SAMP(I,2)
      Z = SAMP(I,3)
      FUN(1)=-((X*Y+X*Z+X+Y*Z+Y+Z+1.)*(X-1.)*(Y-1.)*(Z-1.))/8.
      FUN(2)= ((X+Z+1.)*(X-1.)*(Y+1.)*(Y-1.)*(Z-1.))/4.
      FUN(3)=-((X*Y-X*Z-X+Y*Z+Y-Z-1.)*(X-1.)*(Y+1.)*(Z-1.))/8.
      FUN(4)= ((X+1.)*(X-1.)*(Y-Z-1.)*(Y+1.)*(Z-1.))/4.
      FUN(5)=-((X*Y-X*Z-X-Y*Z-Y+Z+1.)*(X+1.)*(Y+1.)*(Z-1.))/8.
      FUN(6)= ((X-Z-1.)*(X+1.)*(Y+1.)*(Y-1.)*(Z-1.))/4.
      FUN(7)=-((X*Y+X*Z+X-Y*Z-Y-Z-1.)*(X+1.)*(Y-1.)*(Z-1.))/8.
      FUN(8)= ((X+1.)*(X-1.)*(Y+Z+1.)*(Y-1.)*(Z-1.))/4.
      FUN(9)= ((X+Y+1.)*(X-1.)*(Y-1.)*(Z+1.)*(Z-1.))/4.
      FUN(10)=-((X-Y+1.)*(X-1.)*(Y+1.)*(Z+1.)*(Z-1.))/4.
      FUN(11)=-((X+Y-1.)*(X+1.)*(Y+1.)*(Z+1.)*(Z-1.))/4.
      FUN(12)= ((X-Y-1.)*(X+1.)*(Y-1.)*(Z+1.)*(Z-1.))/4.
      FUN(13)= ((X*Y-X*Z+X-Y*Z+Y-Z+1.)*(X-1.)*(Y-1.)*(Z+1.))/8.
      FUN(14)=-((X-Z+1.)*(X-1.)*(Y+1.)*(Y-1.)*(Z+1.))/4.
      FUN(15)= ((X*Y+X*Z-X-Y*Z+Y+Z-1.)*(X-1.)*(Y+1.)*(Z+1.))/8.
      FUN(16)=-((X+1.)*(X-1.)*(Y+Z-1.)*(Y+1.)*(Z+1.))/4.
      FUN(17)= ((X*Y+X*Z-X+Y*Z-Y-Z+1.)*(X+1.)*(Y+1.)*(Z+1.))/8.
      FUN(18)=-((X+Z-1.)*(X+1.)*(Y+1.)*(Y-1.)*(Z+1.))/4.
      FUN(19)= ((X*Y-X*Z+X+Y*Z-Y+Z-1.)*(X+1.)*(Y-1.)*(Z+1.))/8.
      FUN(20)=-((X+1.)*(X-1.)*(Y-Z+1.)*(Y-1.)*(Z+1.))/4.
      FUN(21)=-((X+1.)*(X-1.)*(Y+1.)*(Y-1.)*(Z-1.))/2.
      FUN(22)= ((X+1.)*(X-1.)*(Y+1.)*(Y-1.)*(Z+1.))/2.
      FUN(23)=-((X+1.)*(X-1.)*(Y-1.)*(Z+1.)*(Z-1.))/2.
      FUN(24)= ((X+1.)*(X-1.)*(Y+1.)*(Z+1.)*(Z-1.))/2.
      FUN(25)=-((X-1.)*(Y+1.)*(Y-1.)*(Z+1.)*(Z-1.))/2.
      FUN(26)= ((X+1.)*(Y+1.)*(Y-1.)*(Z+1.)*(Z-1.))/2.
      DER(1,1)=-((2.*X*Y+2.*X*Z+2.*X+Y*Z)*(Y-1.)*(Z-1.))/8.
      DER(1,2)= ((2.*X+Z)*(Y+1.)*(Y-1.)*(Z-1.))/4.
      DER(1,3)=-((2.*X*Y-2.*X*Z-2.*X+Y*Z)*(Y+1.)*(Z-1.))/8.
      DER(1,4)= ((Y-Z-1.)*(Y+1.)*(Z-1.)*X)/2.
      DER(1,5)=-((2.*X*Y-2.*X*Z-2.*X-Y*Z)*(Y+1.)*(Z-1.))/8.
      DER(1,6)= ((2.*X-Z)*(Y+1.)*(Y-1.)*(Z-1.))/4.
      DER(1,7)=-((2.*X*Y+2.*X*Z+2.*X-Y*Z)*(Y-1.)*(Z-1.))/8.
      DER(1,8)= ((Y+Z+1.)*(Y-1.)*(Z-1.)*X)/2.
      DER(1,9)= ((2.*X+Y)*(Y-1.)*(Z+1.)*(Z-1.))/4.
      DER(1,10)=-((2.*X-Y)*(Y+1.)*(Z+1.)*(Z-1.))/4.
      DER(1,11)=-((2.*X+Y)*(Y+1.)*(Z+1.)*(Z-1.))/4.
      DER(1,12)= ((2.*X-Y)*(Y-1.)*(Z+1.)*(Z-1.))/4.
      DER(1,13)= ((2.*X*Y-2.*X*Z+2.*X-Y*Z)*(Y-1.)*(Z+1.))/8.
      DER(1,14)=-((2.*X-Z)*(Y+1.)*(Y-1.)*(Z+1.))/4.
      DER(1,15)= ((2.*X*Y+2.*X*Z-2.*X-Y*Z)*(Y+1.)*(Z+1.))/8.
      DER(1,16)=-((Y+Z-1.)*(Y+1.)*(Z+1.)*X)/2.
      DER(1,17)= ((2.*X*Y+2.*X*Z-2.*X+Y*Z)*(Y+1.)*(Z+1.))/8.
      DER(1,18)=-((2.*X+Z)*(Y+1.)*(Y-1.)*(Z+1.))/4.
      DER(1,19)= ((2.*X*Y-2.*X*Z+2.*X+Y*Z)*(Y-1.)*(Z+1.))/8.
      DER(1,20)=-((Y-Z+1.)*(Y-1.)*(Z+1.)*X)/2.
      DER(1,21)=-(Y+1.)*(Y-1.)*(Z-1.)*X
      DER(1,22)= (Y+1.)*(Y-1.)*(Z+1.)*X
      DER(1,23)=-(Y-1.)*(Z+1.)*(Z-1.)*X
      DER(1,24)= (Y+1.)*(Z+1.)*(Z-1.)*X
      DER(1,25)=-((Y+1.)*(Y-1.)*(Z+1.)*(Z-1.))/2.
      DER(1,26)= ((Y+1.)*(Y-1.)*(Z+1.)*(Z-1.))/2.
      DER(2,1)=-((2.*X*Y+X*Z+2.*Y*Z+2.*Y)*(X-1.)*(Z-1.))/8.
      DER(2,2)= ((X+Z+1.)*(X-1.)*(Z-1.)*Y)/2.
      DER(2,3)=-((2.*X*Y-X*Z+2.*Y*Z+2.*Y)*(X-1.)*(Z-1.))/8.
      DER(2,4)= ((X+1.)*(X-1.)*(2.*Y-Z)*(Z-1.))/4.
      DER(2,5)=-((2.*X*Y-X*Z-2.*Y*Z-2.*Y)*(X+1.)*(Z-1.))/8.
      DER(2,6)= ((X-Z-1.)*(X+1.)*(Z-1.)*Y)/2.
      DER(2,7)=-((2.*X*Y+X*Z-2.*Y*Z-2.*Y)*(X+1.)*(Z-1.))/8.
      DER(2,8)= ((X+1.)*(X-1.)*(2.*Y+Z)*(Z-1.))/4.
      DER(2,9)= ((X+2.*Y)*(X-1.)*(Z+1.)*(Z-1.))/4.
      DER(2,10)=-((X-2.*Y)*(X-1.)*(Z+1.)*(Z-1.))/4.
      DER(2,11)=-((X+2.*Y)*(X+1.)*(Z+1.)*(Z-1.))/4.
      DER(2,12)= ((X-2.*Y)*(X+1.)*(Z+1.)*(Z-1.))/4.
      DER(2,13)= ((2.*X*Y-X*Z-2.*Y*Z+2.*Y)*(X-1.)*(Z+1.))/8.
      DER(2,14)=-((X-Z+1.)*(X-1.)*(Z+1.)*Y)/2.
      DER(2,15)= ((2.*X*Y+X*Z-2.*Y*Z+2.*Y)*(X-1.)*(Z+1.))/8.
      DER(2,16)=-((X+1.)*(X-1.)*(2.*Y+Z)*(Z+1.))/4.
      DER(2,17)= ((2.*X*Y+X*Z+2.*Y*Z-2.*Y)*(X+1.)*(Z+1.))/8.
      DER(2,18)=-((X+Z-1.)*(X+1.)*(Z+1.)*Y)/2.
      DER(2,19)= ((2.*X*Y-X*Z+2.*Y*Z-2.*Y)*(X+1.)*(Z+1.))/8.
      DER(2,20)=-((X+1.)*(X-1.)*(2.*Y-Z)*(Z+1.))/4.
      DER(2,21)=-(X+1.)*(X-1.)*(Z-1.)*Y
      DER(2,22)= (X+1.)*(X-1.)*(Z+1.)*Y
      DER(2,23)=-((X+1.)*(X-1.)*(Z+1.)*(Z-1.))/2.
      DER(2,24)= ((X+1.)*(X-1.)*(Z+1.)*(Z-1.))/2.
      DER(2,25)=-(X-1.)*(Z+1.)*(Z-1.)*Y
      DER(2,26)= (X+1.)*(Z+1.)*(Z-1.)*Y
      DER(3,1)=-((X*Y+2.*X*Z+2.*Y*Z+2.*Z)*(X-1.)*(Y-1.))/8.
      DER(3,2)= ((X+2.*Z)*(X-1.)*(Y+1.)*(Y-1.))/4.
      DER(3,3)=-((X*Y-2.*X*Z+2.*Y*Z-2.*Z)*(X-1.)*(Y+1.))/8.
      DER(3,4)= ((X+1.)*(X-1.)*(Y-2.*Z)*(Y+1.))/4.
      DER(3,5)=-((X*Y-2.*X*Z-2.*Y*Z+2.*Z)*(X+1.)*(Y+1.))/8.
      DER(3,6)= ((X-2.*Z)*(X+1.)*(Y+1.)*(Y-1.))/4.
      DER(3,7)=-((X*Y+2.*X*Z-2.*Y*Z-2.*Z)*(X+1.)*(Y-1.))/8.
      DER(3,8)= ((X+1.)*(X-1.)*(Y+2.*Z)*(Y-1.))/4.
      DER(3,9)= ((X+Y+1.)*(X-1.)*(Y-1.)*Z)/2.
      DER(3,10)=-((X-Y+1.)*(X-1.)*(Y+1.)*Z)/2.
      DER(3,11)=-((X+Y-1.)*(X+1.)*(Y+1.)*Z)/2.
      DER(3,12)= ((X-Y-1.)*(X+1.)*(Y-1.)*Z)/2.
      DER(3,13)= ((X*Y-2.*X*Z-2.*Y*Z-2.*Z)*(X-1.)*(Y-1.))/8.
      DER(3,14)=-((X-2.*Z)*(X-1.)*(Y+1.)*(Y-1.))/4.
      DER(3,15)= ((X*Y+2.*X*Z-2.*Y*Z+2.*Z)*(X-1.)*(Y+1.))/8.
      DER(3,16)=-((X+1.)*(X-1.)*(Y+2.*Z)*(Y+1.))/4.
      DER(3,17)= ((X*Y+2.*X*Z+2.*Y*Z-2.*Z)*(X+1.)*(Y+1.))/8.
      DER(3,18)=-((X+2.*Z)*(X+1.)*(Y+1.)*(Y-1.))/4.
      DER(3,19)= ((X*Y-2.*X*Z+2.*Y*Z+2.*Z)*(X+1.)*(Y-1.))/8.
      DER(3,20)=-((X+1.)*(X-1.)*(Y-2.*Z)*(Y-1.))/4.
      DER(3,21)=-((X+1.)*(X-1.)*(Y+1.)*(Y-1.))/2.
      DER(3,22)= ((X+1.)*(X-1.)*(Y+1.)*(Y-1.))/2.
      DER(3,23)=-(X+1.)*(X-1.)*(Y-1.)*Z
      DER(3,24)= (X+1.)*(X-1.)*(Y+1.)*Z
      DER(3,25)=-(X-1.)*(Y+1.)*(Y-1.)*Z
      DER(3,26)= (X+1.)*(Y+1.)*(Y-1.)*Z
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF27NB (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and their
C     derivatives for a 27-noded brick element
C         Created using REDUCE by Dan Kidger,  March  1992
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)
      X = SAMP(I,1)
      Y = SAMP(I,2)
      Z = SAMP(I,3)
      FUN(1)= ((X-1.)*(Y-1.)*(Z-1.)*X*Y*Z)/8.
      FUN(2)=-((X-1.)*(Y+1.)*(Y-1.)*(Z-1.)*X*Z)/4.
      FUN(3)= ((X-1.)*(Y+1.)*(Z-1.)*X*Y*Z)/8.
      FUN(4)=-((X+1.)*(X-1.)*(Y+1.)*(Z-1.)*Y*Z)/4.
      FUN(5)= ((X+1.)*(Y+1.)*(Z-1.)*X*Y*Z)/8.
      FUN(6)=-((X+1.)*(Y+1.)*(Y-1.)*(Z-1.)*X*Z)/4.
      FUN(7)= ((X+1.)*(Y-1.)*(Z-1.)*X*Y*Z)/8.
      FUN(8)=-((X+1.)*(X-1.)*(Y-1.)*(Z-1.)*Y*Z)/4.
      FUN(9)=-((X-1.)*(Y-1.)*(Z+1.)*(Z-1.)*X*Y)/4.
      FUN(10)=-((X-1.)*(Y+1.)*(Z+1.)*(Z-1.)*X*Y)/4.
      FUN(11)=-((X+1.)*(Y+1.)*(Z+1.)*(Z-1.)*X*Y)/4.
      FUN(12)=-((X+1.)*(Y-1.)*(Z+1.)*(Z-1.)*X*Y)/4.
      FUN(13)= ((X-1.)*(Y-1.)*(Z+1.)*X*Y*Z)/8.
      FUN(14)=-((X-1.)*(Y+1.)*(Y-1.)*(Z+1.)*X*Z)/4.
      FUN(15)= ((X-1.)*(Y+1.)*(Z+1.)*X*Y*Z)/8.
      FUN(16)=-((X+1.)*(X-1.)*(Y+1.)*(Z+1.)*Y*Z)/4.
      FUN(17)= ((X+1.)*(Y+1.)*(Z+1.)*X*Y*Z)/8.
      FUN(18)=-((X+1.)*(Y+1.)*(Y-1.)*(Z+1.)*X*Z)/4.
      FUN(19)= ((X+1.)*(Y-1.)*(Z+1.)*X*Y*Z)/8.
      FUN(20)=-((X+1.)*(X-1.)*(Y-1.)*(Z+1.)*Y*Z)/4.
      FUN(21)= ((X+1.)*(X-1.)*(Y+1.)*(Y-1.)*(Z-1.)*Z)/2.
      FUN(22)= ((X+1.)*(X-1.)*(Y+1.)*(Y-1.)*(Z+1.)*Z)/2.
      FUN(23)= ((X+1.)*(X-1.)*(Y-1.)*(Z+1.)*(Z-1.)*Y)/2.
      FUN(24)= ((X+1.)*(X-1.)*(Y+1.)*(Z+1.)*(Z-1.)*Y)/2.
      FUN(25)= ((X-1.)*(Y+1.)*(Y-1.)*(Z+1.)*(Z-1.)*X)/2.
      FUN(26)= ((X+1.)*(Y+1.)*(Y-1.)*(Z+1.)*(Z-1.)*X)/2.
      FUN(27)=-(X+1.)*(X-1.)*(Y+1.)*(Y-1.)*(Z+1.)*(Z-1.)
      DER(1,1)= (((X-1.)+X)*(Y-1.)*(Z-1.)*Y*Z)/8.
      DER(1,2)=-(((X-1.)+X)*(Y+1.)*(Y-1.)*(Z-1.)*Z)/4.
      DER(1,3)= (((X-1.)+X)*(Y+1.)*(Z-1.)*Y*Z)/8.
      DER(1,4)=-((Y+1.)*(Z-1.)*X*Y*Z)/2.
      DER(1,5)= (((X+1.)+X)*(Y+1.)*(Z-1.)*Y*Z)/8.
      DER(1,6)=-(((X+1.)+X)*(Y+1.)*(Y-1.)*(Z-1.)*Z)/4.
      DER(1,7)= (((X+1.)+X)*(Y-1.)*(Z-1.)*Y*Z)/8.
      DER(1,8)=-((Y-1.)*(Z-1.)*X*Y*Z)/2.
      DER(1,9)=-(((X-1.)+X)*(Y-1.)*(Z+1.)*(Z-1.)*Y)/4.
      DER(1,10)=-(((X-1.)+X)*(Y+1.)*(Z+1.)*(Z-1.)*Y)/4.
      DER(1,11)=-(((X+1.)+X)*(Y+1.)*(Z+1.)*(Z-1.)*Y)/4.
      DER(1,12)=-(((X+1.)+X)*(Y-1.)*(Z+1.)*(Z-1.)*Y)/4.
      DER(1,13)= (((X-1.)+X)*(Y-1.)*(Z+1.)*Y*Z)/8.
      DER(1,14)=-(((X-1.)+X)*(Y+1.)*(Y-1.)*(Z+1.)*Z)/4.
      DER(1,15)= (((X-1.)+X)*(Y+1.)*(Z+1.)*Y*Z)/8.
      DER(1,16)=-((Y+1.)*(Z+1.)*X*Y*Z)/2.
      DER(1,17)= (((X+1.)+X)*(Y+1.)*(Z+1.)*Y*Z)/8.
      DER(1,18)=-(((X+1.)+X)*(Y+1.)*(Y-1.)*(Z+1.)*Z)/4.
      DER(1,19)= (((X+1.)+X)*(Y-1.)*(Z+1.)*Y*Z)/8.
      DER(1,20)=-((Y-1.)*(Z+1.)*X*Y*Z)/2.
      DER(1,21)= (Y+1.)*(Y-1.)*(Z-1.)*X*Z
      DER(1,22)= (Y+1.)*(Y-1.)*(Z+1.)*X*Z
      DER(1,23)= (Y-1.)*(Z+1.)*(Z-1.)*X*Y
      DER(1,24)= (Y+1.)*(Z+1.)*(Z-1.)*X*Y
      DER(1,25)= (((X-1.)+X)*(Y+1.)*(Y-1.)*(Z+1.)*(Z-1.))/2.
      DER(1,26)= (((X+1.)+X)*(Y+1.)*(Y-1.)*(Z+1.)*(Z-1.))/2.
      DER(1,27)=-2.*(Y+1.)*(Y-1.)*(Z+1.)*(Z-1.)*X
      DER(2,1)= (((Y-1.)+Y)*(X-1.)*(Z-1.)*X*Z)/8.
      DER(2,2)=-((X-1.)*(Z-1.)*X*Y*Z)/2.
      DER(2,3)= (((Y+1.)+Y)*(X-1.)*(Z-1.)*X*Z)/8.
      DER(2,4)=-(((Y+1.)+Y)*(X+1.)*(X-1.)*(Z-1.)*Z)/4.
      DER(2,5)= (((Y+1.)+Y)*(X+1.)*(Z-1.)*X*Z)/8.
      DER(2,6)=-((X+1.)*(Z-1.)*X*Y*Z)/2.
      DER(2,7)= (((Y-1.)+Y)*(X+1.)*(Z-1.)*X*Z)/8.
      DER(2,8)=-(((Y-1.)+Y)*(X+1.)*(X-1.)*(Z-1.)*Z)/4.
      DER(2,9)=-(((Y-1.)+Y)*(X-1.)*(Z+1.)*(Z-1.)*X)/4.
      DER(2,10)=-(((Y+1.)+Y)*(X-1.)*(Z+1.)*(Z-1.)*X)/4.
      DER(2,11)=-(((Y+1.)+Y)*(X+1.)*(Z+1.)*(Z-1.)*X)/4.
      DER(2,12)=-(((Y-1.)+Y)*(X+1.)*(Z+1.)*(Z-1.)*X)/4.
      DER(2,13)= (((Y-1.)+Y)*(X-1.)*(Z+1.)*X*Z)/8.
      DER(2,14)=-((X-1.)*(Z+1.)*X*Y*Z)/2.
      DER(2,15)= (((Y+1.)+Y)*(X-1.)*(Z+1.)*X*Z)/8.
      DER(2,16)=-(((Y+1.)+Y)*(X+1.)*(X-1.)*(Z+1.)*Z)/4.
      DER(2,17)= (((Y+1.)+Y)*(X+1.)*(Z+1.)*X*Z)/8.
      DER(2,18)=-((X+1.)*(Z+1.)*X*Y*Z)/2.
      DER(2,19)= (((Y-1.)+Y)*(X+1.)*(Z+1.)*X*Z)/8.
      DER(2,20)=-(((Y-1.)+Y)*(X+1.)*(X-1.)*(Z+1.)*Z)/4.
      DER(2,21)= (X+1.)*(X-1.)*(Z-1.)*Y*Z
      DER(2,22)= (X+1.)*(X-1.)*(Z+1.)*Y*Z
      DER(2,23)= (((Y-1.)+Y)*(X+1.)*(X-1.)*(Z+1.)*(Z-1.))/2.
      DER(2,24)= (((Y+1.)+Y)*(X+1.)*(X-1.)*(Z+1.)*(Z-1.))/2.
      DER(2,25)= (X-1.)*(Z+1.)*(Z-1.)*X*Y
      DER(2,26)= (X+1.)*(Z+1.)*(Z-1.)*X*Y
      DER(2,27)=-2.*(X+1.)*(X-1.)*(Z+1.)*(Z-1.)*Y
      DER(3,1)= (((Z-1.)+Z)*(X-1.)*(Y-1.)*X*Y)/8.
      DER(3,2)=-(((Z-1.)+Z)*(X-1.)*(Y+1.)*(Y-1.)*X)/4.
      DER(3,3)= (((Z-1.)+Z)*(X-1.)*(Y+1.)*X*Y)/8.
      DER(3,4)=-(((Z-1.)+Z)*(X+1.)*(X-1.)*(Y+1.)*Y)/4.
      DER(3,5)= (((Z-1.)+Z)*(X+1.)*(Y+1.)*X*Y)/8.
      DER(3,6)=-(((Z-1.)+Z)*(X+1.)*(Y+1.)*(Y-1.)*X)/4.
      DER(3,7)= (((Z-1.)+Z)*(X+1.)*(Y-1.)*X*Y)/8.
      DER(3,8)=-(((Z-1.)+Z)*(X+1.)*(X-1.)*(Y-1.)*Y)/4.
      DER(3,9)=-((X-1.)*(Y-1.)*X*Y*Z)/2.
      DER(3,10)=-((X-1.)*(Y+1.)*X*Y*Z)/2.
      DER(3,11)=-((X+1.)*(Y+1.)*X*Y*Z)/2.
      DER(3,12)=-((X+1.)*(Y-1.)*X*Y*Z)/2.
      DER(3,13)= (((Z+1.)+Z)*(X-1.)*(Y-1.)*X*Y)/8.
      DER(3,14)=-(((Z+1.)+Z)*(X-1.)*(Y+1.)*(Y-1.)*X)/4.
      DER(3,15)= (((Z+1.)+Z)*(X-1.)*(Y+1.)*X*Y)/8.
      DER(3,16)=-(((Z+1.)+Z)*(X+1.)*(X-1.)*(Y+1.)*Y)/4.
      DER(3,17)= (((Z+1.)+Z)*(X+1.)*(Y+1.)*X*Y)/8.
      DER(3,18)=-(((Z+1.)+Z)*(X+1.)*(Y+1.)*(Y-1.)*X)/4.
      DER(3,19)= (((Z+1.)+Z)*(X+1.)*(Y-1.)*X*Y)/8.
      DER(3,20)=-(((Z+1.)+Z)*(X+1.)*(X-1.)*(Y-1.)*Y)/4.
      DER(3,21)= (((Z-1.)+Z)*(X+1.)*(X-1.)*(Y+1.)*(Y-1.))/2.
      DER(3,22)= (((Z+1.)+Z)*(X+1.)*(X-1.)*(Y+1.)*(Y-1.))/2.
      DER(3,23)= (X+1.)*(X-1.)*(Y-1.)*Y*Z
      DER(3,24)= (X+1.)*(X-1.)*(Y+1.)*Y*Z
      DER(3,25)= (X-1.)*(Y+1.)*(Y-1.)*X*Z
      DER(3,26)= (X+1.)*(Y+1.)*(Y-1.)*X*Z
      DER(3,27)=-2.*(X+1.)*(X-1.)*(Y+1.)*(Y-1.)*Z
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF6NW (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and
C     their derivatives for a 6-noded wedge element (based on 3nt)
C       Dan Kidger 16-2-97
c
      REAL DER(IDER,*),SAMP(ISAMP,*),FUN(*), C1,C2,C3
      C1 = SAMP(I,1)         !- triangle-L1
      C2 = SAMP(I,2)         !- triangle-L2
      C3 = SAMP(I,3)         !- project into 'Z'
      FUN(1) = C3*C1 
      FUN(3) = C3*C2
      FUN(2) = C3* (1.-C1-C2)
      FUN(4) = (1.-C3)*C1 
      FUN(6) = (1.-C3)*C2
      FUN(5) = (1.-C3)* (1.-C1-C2)

c      DER(1,1)=1.   !- not yet done these :-(
c      DER(1,3)=0.
c      DER(1,2)=-1.
c      DER(2,1)=0.
c      DER(2,3)=1.
c      DER(2,2)=-1.
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SF15NW (DER,IDER,FUN,SAMP,ISAMP,I)
C
C     This forms the shape functions and
C     their derivatives for a 15-noded wedge element (based on 6nw)
C       Dan Kidger 16-2-97
c
      REAL DER(IDER,*),SAMP(ISAMP,*),FUN(*) !, C1,C2,C3
      RETURN
      END

C-----------------------------------------------------------------------
C              26th April 1993      Dan Kidger
C
C     These are the shape function subroutines for the 2 'current' favoured
C     invocations of the 14 node brick element (D.Kidger 1990) :
C        1/ The 'minimum polynomial' type (was A/2+B/2)
C        2/ The 'serendipity' type (corner = f (8nb & mid-face funcs))
C
C        3/ also 29-4-93  16 node tesseract (4D!)
C-----------------------------------------------------------------------
      SUBROUTINE SF14NB_AB (DER,IDER,FUN,SAMP,ISAMP,I)   
C                                                        
C     This produces the shape functions and derivatives for an element
C      produced automaticaly by COMPUTER ALGEBRA code    
C           by Dan KIdger 1993                           
C
C    .. this is the 'Average of the LHS and RHS types' Element: 
C       *  sf = f (8nb terms, x ,y ,z ,(x y,y z,z x)+(x z,y x,z y) ) 
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)              
c     NOD=14
c     NDIME=3
c     ITYPE=99
      X = SAMP(I,1)                                      
      Y = SAMP(I,2)                                      
      Z = SAMP(I,3)                                      
      FUN(1)=(-((Y+Z-1.)*X**2+(Y+Z-2.)*(Y+Z)*X+(Z-1.)*Y**2+(Z-2.)*Y*Z-Z
     . **2+1.))/16.
      FUN(2)=(-((Y-Z+2.)*(Y-Z)*X-(Y-Z+1.)*X**2+(Z-1.)*Y**2-(Z-2.)*Y*Z-Z
     . **2+1.))/16.
      FUN(3)=(-((Y+Z-1.)*X**2-(Y+Z-2.)*(Y+Z)*X+(Z-1.)*Y**2+(Z-2.)*Y*Z-Z
     . **2+1.))/16.
      FUN(4)=((Y-Z+2.)*(Y-Z)*X+(Y-Z+1.)*X**2-(Z-1.)*Y**2+(Z-2.)*Y*Z+Z**
     . 2-1.)/16.
      FUN(5)=(-((Y-Z-1.)*X**2+(Y-Z-2.)*(Y-Z)*X+(Z+2.)*Y*Z-(Z+1.)*Y**2-Z
     . **2+1.))/16.
      FUN(6)=(-((Y+Z+2.)*(Y+Z)*X-(Y+Z+1.)*X**2-(Z+2.)*Y*Z-(Z+1.)*Y**2-Z
     . **2+1.))/16.
      FUN(7)=(-((Y-Z-1.)*X**2-(Y-Z-2.)*(Y-Z)*X+(Z+2.)*Y*Z-(Z+1.)*Y**2-Z
     . **2+1.))/16.
      FUN(8)=((Y+Z+2.)*(Y+Z)*X+(Y+Z+1.)*X**2+(Z+2.)*Y*Z+(Z+1.)*Y**2+Z**
     . 2-1.)/16.
      FUN(9) =( (X**2+Y**2+Z-1.)*(Z-1.))/4.
      FUN(10)=(-(X**2+Y**2-Z-1.)*(Z+1.))/4.
      FUN(11)=( (X**2+Y+Z**2-1.)*(Y-1.))/4.
      FUN(12)=(-(X**2-Y+Z**2-1.)*(Y+1.))/4.
      FUN(13)=( (X+Y**2+Z**2-1.)*(X-1.))/4.
      FUN(14)=( (X-Y**2-Z**2+1.)*(X+1.))/4.
      DER(1,1)=(-(2.*(Y+Z-1.)*X+(Y+Z-2.)*(Y+Z)))/16.
      DER(1,2)=(-((Y-Z+2.)*(Y-Z)-2.*(Y-Z+1.)*X))/16.
      DER(1,3)=(-(2.*(Y+Z-1.)*X-(Y+Z-2.)*(Y+Z)))/16.
      DER(1,4)=((Y-Z+2.)*(Y-Z)+2.*(Y-Z+1.)*X)/16.
      DER(1,5)=(-(2.*(Y-Z-1.)*X+(Y-Z-2.)*(Y-Z)))/16.
      DER(1,6)=(-((Y+Z+2.)*(Y+Z)-2.*(Y+Z+1.)*X))/16.
      DER(1,7)=(-(2.*(Y-Z-1.)*X-(Y-Z-2.)*(Y-Z)))/16.
      DER(1,8)=((Y+Z+2.)*(Y+Z)+2.*(Y+Z+1.)*X)/16.
      DER(1,9) =( (Z-1.)*X)/2.
      DER(1,10)=(-(Z+1.)*X)/2.
      DER(1,11)=( (Y-1.)*X)/2.
      DER(1,12)=(-(Y+1.)*X)/2.
      DER(1,13)=(2.*X+Y**2+Z**2-2.)/4.
      DER(1,14)=(2.*X-Y**2-Z**2+2.)/4.
      DER(2,1)=(-((Y+Z-2.)*X+(Y+Z)*X+2.*(Z-1.)*Y+(Z-2.)*Z+X**2))/16.
      DER(2,2)=(-((Y-Z+2.)*X+(Y-Z)*X+2.*(Z-1.)*Y-(Z-2.)*Z-X**2))/16.
      DER(2,3)=(  (Y+Z-2.)*X+(Y+Z)*X-2.*(Z-1.)*Y-(Z-2.)*Z-X**2)/16.
      DER(2,4)=(  (Y-Z+2.)*X+(Y-Z)*X-2.*(Z-1.)*Y+(Z-2.)*Z+X**2)/16.
      DER(2,5)=(-((Y-Z-2.)*X+(Y-Z)*X+(Z+2.)*Z-2.*(Z+1.)*Y+X**2))/16.
      DER(2,6)=(-((Y+Z+2.)*X+(Y+Z)*X-(Z+2.)*Z-2.*(Z+1.)*Y-X**2))/16.
      DER(2,7)=(  (Y-Z-2.)*X+(Y-Z)*X-(Z+2.)*Z+2.*(Z+1.)*Y-X**2)/16.
      DER(2,8)=(  (Y+Z+2.)*X+(Y+Z)*X+(Z+2.)*Z+2.*(Z+1.)*Y+X**2)/16.
      DER(2,9)=((Z-1.)*Y)/2.
      DER(2,10)=(-(Z+1.)*Y)/2.
      DER(2,11)=(X**2+2.*Y+Z**2-2.)/4.
      DER(2,12)=(-(X**2-2.*Y+Z**2-2.))/4.
      DER(2,13)=( (X-1.)*Y)/2.
      DER(2,14)=(-(X+1.)*Y)/2.
      DER(3,1)=(-(X**2+2.*X*Y+2.*X*Z-2.*X+Y**2+2.*Y*Z-2.*Y-2.*Z))/16.
      DER(3,2)=(-(X**2-2.*X*Y+2.*X*Z-2.*X+Y**2-2.*Y*Z+2.*Y-2.*Z))/16.
      DER(3,3)=(-(X**2-2.*X*Y-2.*X*Z+2.*X+Y**2+2.*Y*Z-2.*Y-2.*Z))/16.
      DER(3,4)=(-(X**2+2.*X*Y-2.*X*Z+2.*X+Y**2-2.*Y*Z+2.*Y-2.*Z))/16.
      DER(3,5)=  (X**2+2.*X*Y-2.*X*Z-2.*X+Y**2-2.*Y*Z-2.*Y+2.*Z) /16.
      DER(3,6)=  (X**2-2.*X*Y-2.*X*Z-2.*X+Y**2+2.*Y*Z+2.*Y+2.*Z) /16.
      DER(3,7)=  (X**2-2.*X*Y+2.*X*Z+2.*X+Y**2-2.*Y*Z-2.*Y+2.*Z) /16.
      DER(3,8)=  (X**2+2.*X*Y+2.*X*Z+2.*X+Y**2+2.*Y*Z+2.*Y+2.*Z) /16.
      DER(3,9)=  (X**2+Y**2+2.*Z-2.)/4.
      DER(3,10)=(-(X**2+Y**2-2.*Z-2.))/4.
      DER(3,11)=( (Y-1.)*Z)/2.
      DER(3,12)=(-(Y+1.)*Z)/2.
      DER(3,13)=( (X-1.)*Z)/2.
      DER(3,14)=(-(X+1.)*Z)/2.
      RETURN
      END

C--------------------------------------------------------------------
      SUBROUTINE SF14NB_SER (DER,IDER,FUN,SAMP,ISAMP,I)   
C                                                        
C     This produces the shape functions and derivatives for an element
C      produced automaticaly by COMPUTER ALGEBRA code    
C           by Dan KIdger 1993                           
C
C    .. this is the 'Serendipity' Element: 
C       *  centre = (1-x )(1-y )(1-z)
C       *  corner = (1-x)(1-y)(1-z) -   centre-funcs at this node
C
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)              
c     NOD=14
c     NDIME=3
c     ITYPE=99
      X = SAMP(I,1)                                      
      Y = SAMP(I,2)                                      
      Z = SAMP(I,3)                                      
      FUN(1) =(((Y+Z+2.)*X+(Z+2.)*Y+2.*Z+2.)*(X-1.)*(Y-1.)*(Z-1.))/8.
      FUN(2) =(((Y-Z-2.)*X+(Z+2.)*Y-2.*Z-2.)*(X-1.)*(Y+1.)*(Z-1.))/8.
      FUN(3) =(((Y+Z+2.)*X-(Z+2.)*Y-2.*Z-2.)*(X+1.)*(Y-1.)*(Z-1.))/8.
      FUN(4) =(((Y-Z-2.)*X-(Z+2.)*Y+2.*Z+2.)*(X+1.)*(Y+1.)*(Z-1.))/8.
      FUN(5) =(-((Y-Z+2.)*X-(Z-2.)*Y-2.*Z+2.)*(X-1.)*(Y-1.)*(Z+1.))/8.
      FUN(6) =(-((Y+Z-2.)*X-(Z-2.)*Y+2.*Z-2.)*(X-1.)*(Y+1.)*(Z+1.))/8.
      FUN(7) =(-((Y-Z+2.)*X+(Z-2.)*Y+2.*Z-2.)*(X+1.)*(Y-1.)*(Z+1.))/8.
      FUN(8) =(-((Y+Z-2.)*X+(Z-2.)*Y-2.*Z+2.)*(X+1.)*(Y+1.)*(Z+1.))/8.
      FUN(9) =(-(X+1.)*(X-1.)*(Y+1.)*(Y-1.)*(Z-1.))/2.
      FUN(10)=( (X+1.)*(X-1.)*(Y+1.)*(Y-1.)*(Z+1.))/2.
      FUN(11)=(-(X+1.)*(X-1.)*(Y-1.)*(Z+1.)*(Z-1.))/2.
      FUN(12)=( (X+1.)*(X-1.)*(Y+1.)*(Z+1.)*(Z-1.))/2.
      FUN(13)=(-(X-1.)*(Y+1.)*(Y-1.)*(Z+1.)*(Z-1.))/2.
      FUN(14)=( (X+1.)*(Y+1.)*(Y-1.)*(Z+1.)*(Z-1.))/2.
      DER(1,1)=( (2.*X*Y+2.*X*Z+4.*X+Y*Z+Y+Z)*(Y-1.)*(Z-1.))/8.
      DER(1,2)=( (2.*X*Y-2.*X*Z-4.*X+Y*Z+Y-Z)*(Y+1.)*(Z-1.))/8.
      DER(1,3)=( (2.*X*Y+2.*X*Z+4.*X-Y*Z-Y-Z)*(Y-1.)*(Z-1.))/8.
      DER(1,4)=( (2.*X*Y-2.*X*Z-4.*X-Y*Z-Y+Z)*(Y+1.)*(Z-1.))/8.
      DER(1,5)=(-(2.*X*Y-2.*X*Z+4.*X-Y*Z+Y-Z)*(Y-1.)*(Z+1.))/8.
      DER(1,6)=(-(2.*X*Y+2.*X*Z-4.*X-Y*Z+Y+Z)*(Y+1.)*(Z+1.))/8.
      DER(1,7)=(-(2.*X*Y-2.*X*Z+4.*X+Y*Z-Y+Z)*(Y-1.)*(Z+1.))/8.
      DER(1,8)=(-(2.*X*Y+2.*X*Z-4.*X+Y*Z-Y-Z)*(Y+1.)*(Z+1.))/8.
      DER(1,9)=  -(Y+1.)*(Y-1.)*(Z-1.)*X
      DER(1,10)=  (Y+1.)*(Y-1.)*(Z+1.)*X
      DER(1,11)= -(Y-1.)*(Z+1.)*(Z-1.)*X
      DER(1,12)=  (Y+1.)*(Z+1.)*(Z-1.)*X
      DER(1,13)=(-(Y+1.)*(Y-1.)*(Z+1.)*(Z-1.))/2.
      DER(1,14)=( (Y+1.)*(Y-1.)*(Z+1.)*(Z-1.))/2.
      DER(2,1)=( (2.*X*Y+X*Z+X+2.*Y*Z+4.*Y+Z)*(X-1.)*(Z-1.))/8.
      DER(2,2)=( (2.*X*Y-X*Z-X+2.*Y*Z+4.*Y-Z)*(X-1.)*(Z-1.))/8.
      DER(2,3)=( (2.*X*Y+X*Z+X-2.*Y*Z-4.*Y-Z)*(X+1.)*(Z-1.))/8.
      DER(2,4)=( (2.*X*Y-X*Z-X-2.*Y*Z-4.*Y+Z)*(X+1.)*(Z-1.))/8.
      DER(2,5)=(-(2.*X*Y-X*Z+X-2.*Y*Z+4.*Y-Z)*(X-1.)*(Z+1.))/8.
      DER(2,6)=(-(2.*X*Y+X*Z-X-2.*Y*Z+4.*Y+Z)*(X-1.)*(Z+1.))/8.
      DER(2,7)=(-(2.*X*Y-X*Z+X+2.*Y*Z-4.*Y+Z)*(X+1.)*(Z+1.))/8.
      DER(2,8)=(-(2.*X*Y+X*Z-X+2.*Y*Z-4.*Y-Z)*(X+1.)*(Z+1.))/8.
      DER(2,9)=  -(X+1.)*(X-1.)*(Z-1.)*Y
      DER(2,10)=  (X+1.)*(X-1.)*(Z+1.)*Y
      DER(2,11)=(-(X+1.)*(X-1.)*(Z+1.)*(Z-1.))/2.
      DER(2,12)=( (X+1.)*(X-1.)*(Z+1.)*(Z-1.))/2.
      DER(2,13)= -(X-1.)*(Z+1.)*(Z-1.)*Y
      DER(2,14)= (X+1.)*(Z+1.)*(Z-1.)*Y
      DER(3,1)=( (X*Y+2.*X*Z+X+2.*Y*Z+Y+4.*Z)*(X-1.)*(Y-1.))/8.
      DER(3,2)=( (X*Y-2.*X*Z-X+2.*Y*Z+Y-4.*Z)*(X-1.)*(Y+1.))/8.
      DER(3,3)=( (X*Y+2.*X*Z+X-2.*Y*Z-Y-4.*Z)*(X+1.)*(Y-1.))/8.
      DER(3,4)=( (X*Y-2.*X*Z-X-2.*Y*Z-Y+4.*Z)*(X+1.)*(Y+1.))/8.
      DER(3,5)=(-(X*Y-2.*X*Z+X-2.*Y*Z+Y-4.*Z)*(X-1.)*(Y-1.))/8.
      DER(3,6)=(-(X*Y+2.*X*Z-X-2.*Y*Z+Y+4.*Z)*(X-1.)*(Y+1.))/8.
      DER(3,7)=(-(X*Y-2.*X*Z+X+2.*Y*Z-Y+4.*Z)*(X+1.)*(Y-1.))/8.
      DER(3,8)=(-(X*Y+2.*X*Z-X+2.*Y*Z-Y-4.*Z)*(X+1.)*(Y+1.))/8.
      DER(3,9)=(-(X+1.)*(X-1.)*(Y+1.)*(Y-1.))/2.
      DER(3,10)=((X+1.)*(X-1.)*(Y+1.)*(Y-1.))/2.
      DER(3,11)=-(X+1.)*(X-1.)*(Y-1.)*Z
      DER(3,12)= (X+1.)*(X-1.)*(Y+1.)*Z
      DER(3,13)=-(X-1.)*(Y+1.)*(Y-1.)*Z
      DER(3,14)= (X+1.)*(Y+1.)*(Y-1.)*Z
      RETURN
      END

C--------------------------------------------------------------------
      SUBROUTINE SF16N_4D (DER,IDER,FUN,SAMP,ISAMP,I)   
C                                                        
C     This produces the shape functions and derivatives for an element
C      produced automaticaly by COMPUTER ALGEBRA code    
C           by Dan KIdger 1993                           
C
C      This is the 4D equivalent of the 8 node brick element 29-4-93
C                                                        
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)              
c      NOD=16.
c      ANS=NDIME=NDIME
c      ITYPE=99.
      X = SAMP(I,1)                                      
      Y = SAMP(I,2)                                      
      Z = SAMP(I,3)                                      
      W = SAMP(I,4)                                      
      FUN(1)=((W-1.)*(X-1.)*(Y-1.)*(Z-1.))/16.
      FUN(2)=(-(W-1.)*(X-1.)*(Y+1.)*(Z-1.))/16.
      FUN(3)=(-(W-1.)*(X+1.)*(Y-1.)*(Z-1.))/16.
      FUN(4)=((W-1.)*(X+1.)*(Y+1.)*(Z-1.))/16.
      FUN(5)=(-(W-1.)*(X-1.)*(Y-1.)*(Z+1.))/16.
      FUN(6)=((W-1.)*(X-1.)*(Y+1.)*(Z+1.))/16.
      FUN(7)=((W-1.)*(X+1.)*(Y-1.)*(Z+1.))/16.
      FUN(8)=(-(W-1.)*(X+1.)*(Y+1.)*(Z+1.))/16.
      FUN(9)=(-(W+1.)*(X-1.)*(Y-1.)*(Z-1.))/16.
      FUN(10)=((W+1.)*(X-1.)*(Y+1.)*(Z-1.))/16.
      FUN(11)=((W+1.)*(X+1.)*(Y-1.)*(Z-1.))/16.
      FUN(12)=(-(W+1.)*(X+1.)*(Y+1.)*(Z-1.))/16.
      FUN(13)=((W+1.)*(X-1.)*(Y-1.)*(Z+1.))/16.
      FUN(14)=(-(W+1.)*(X-1.)*(Y+1.)*(Z+1.))/16.
      FUN(15)=(-(W+1.)*(X+1.)*(Y-1.)*(Z+1.))/16.
      FUN(16)=((W+1.)*(X+1.)*(Y+1.)*(Z+1.))/16.
      DER(1,1)=((W-1.)*(Y-1.)*(Z-1.))/16.
      DER(1,2)=(-(W-1.)*(Y+1.)*(Z-1.))/16.
      DER(1,3)=(-(W-1.)*(Y-1.)*(Z-1.))/16.
      DER(1,4)=((W-1.)*(Y+1.)*(Z-1.))/16.
      DER(1,5)=(-(W-1.)*(Y-1.)*(Z+1.))/16.
      DER(1,6)=((W-1.)*(Y+1.)*(Z+1.))/16.
      DER(1,7)=((W-1.)*(Y-1.)*(Z+1.))/16.
      DER(1,8)=(-(W-1.)*(Y+1.)*(Z+1.))/16.
      DER(1,9)=(-(W+1.)*(Y-1.)*(Z-1.))/16.
      DER(1,10)=((W+1.)*(Y+1.)*(Z-1.))/16.
      DER(1,11)=((W+1.)*(Y-1.)*(Z-1.))/16.
      DER(1,12)=(-(W+1.)*(Y+1.)*(Z-1.))/16.
      DER(1,13)=((W+1.)*(Y-1.)*(Z+1.))/16.
      DER(1,14)=(-(W+1.)*(Y+1.)*(Z+1.))/16.
      DER(1,15)=(-(W+1.)*(Y-1.)*(Z+1.))/16.
      DER(1,16)=((W+1.)*(Y+1.)*(Z+1.))/16.
      DER(2,1)=((W-1.)*(X-1.)*(Z-1.))/16.
      DER(2,2)=(-(W-1.)*(X-1.)*(Z-1.))/16.
      DER(2,3)=(-(W-1.)*(X+1.)*(Z-1.))/16.
      DER(2,4)=((W-1.)*(X+1.)*(Z-1.))/16.
      DER(2,5)=(-(W-1.)*(X-1.)*(Z+1.))/16.
      DER(2,6)=((W-1.)*(X-1.)*(Z+1.))/16.
      DER(2,7)=((W-1.)*(X+1.)*(Z+1.))/16.
      DER(2,8)=(-(W-1.)*(X+1.)*(Z+1.))/16.
      DER(2,9)=(-(W+1.)*(X-1.)*(Z-1.))/16.
      DER(2,10)=((W+1.)*(X-1.)*(Z-1.))/16.
      DER(2,11)=((W+1.)*(X+1.)*(Z-1.))/16.
      DER(2,12)=(-(W+1.)*(X+1.)*(Z-1.))/16.
      DER(2,13)=((W+1.)*(X-1.)*(Z+1.))/16.
      DER(2,14)=(-(W+1.)*(X-1.)*(Z+1.))/16.
      DER(2,15)=(-(W+1.)*(X+1.)*(Z+1.))/16.
      DER(2,16)=((W+1.)*(X+1.)*(Z+1.))/16.
      DER(3,1)=((W-1.)*(X-1.)*(Y-1.))/16.
      DER(3,2)=(-(W-1.)*(X-1.)*(Y+1.))/16.
      DER(3,3)=(-(W-1.)*(X+1.)*(Y-1.))/16.
      DER(3,4)=((W-1.)*(X+1.)*(Y+1.))/16.
      DER(3,5)=(-(W-1.)*(X-1.)*(Y-1.))/16.
      DER(3,6)=((W-1.)*(X-1.)*(Y+1.))/16.
      DER(3,7)=((W-1.)*(X+1.)*(Y-1.))/16.
      DER(3,8)=(-(W-1.)*(X+1.)*(Y+1.))/16.
      DER(3,9)=(-(W+1.)*(X-1.)*(Y-1.))/16.
      DER(3,10)=((W+1.)*(X-1.)*(Y+1.))/16.
      DER(3,11)=((W+1.)*(X+1.)*(Y-1.))/16.
      DER(3,12)=(-(W+1.)*(X+1.)*(Y+1.))/16.
      DER(3,13)=((W+1.)*(X-1.)*(Y-1.))/16.
      DER(3,14)=(-(W+1.)*(X-1.)*(Y+1.))/16.
      DER(3,15)=(-(W+1.)*(X+1.)*(Y-1.))/16.
      DER(3,16)=((W+1.)*(X+1.)*(Y+1.))/16.
      DER(4,1)=((X-1.)*(Y-1.)*(Z-1.))/16.
      DER(4,2)=(-(X-1.)*(Y+1.)*(Z-1.))/16.
      DER(4,3)=(-(X+1.)*(Y-1.)*(Z-1.))/16.
      DER(4,4)=((X+1.)*(Y+1.)*(Z-1.))/16.
      DER(4,5)=(-(X-1.)*(Y-1.)*(Z+1.))/16.
      DER(4,6)=((X-1.)*(Y+1.)*(Z+1.))/16.
      DER(4,7)=((X+1.)*(Y-1.)*(Z+1.))/16.
      DER(4,8)=(-(X+1.)*(Y+1.)*(Z+1.))/16.
      DER(4,9)=(-(X-1.)*(Y-1.)*(Z-1.))/16.
      DER(4,10)=((X-1.)*(Y+1.)*(Z-1.))/16.
      DER(4,11)=((X+1.)*(Y-1.)*(Z-1.))/16.
      DER(4,12)=(-(X+1.)*(Y+1.)*(Z-1.))/16.
      DER(4,13)=((X-1.)*(Y-1.)*(Z+1.))/16.
      DER(4,14)=(-(X-1.)*(Y+1.)*(Z+1.))/16.
      DER(4,15)=(-(X+1.)*(Y-1.)*(Z+1.))/16.
      DER(4,16)=((X+1.)*(Y+1.)*(Z+1.))/16.
      RETURN
      END
C--------------------------------------------------------------------
C--------------------------------------------------------------------
*Return-path: <RSHANSEN2@aol.com> 
*  Received: from [198.81.17.1] (helo=imo11.mx.aol.com)
*  Received: from RSHANSEN2@aol.com
*        for <d.kidger@man.ac.uk>; Tue, 1 Sep 1998 22:40:36 +2000 (EDT)
*  From: RSHANSEN2@aol.com

      SUBROUTINE SF32NB_old (DER,IDER,FUN,SAMP,ISAMP,I)
C
C       This forms the shape functions and their 
C       derivatives for the 32-noded hexhedrahl
C     element defined in "Finite Elemental Structural
C     Analysis" by Dr. T. Y. Yang
C       The shape function equations (eq. # 10.55)were 
C     taken from section 10.5.3, page 343.  
C       The derivatives were calculate from the shape
C     function equations by Mathematica.
C     Shape functions verified by: 
C     R. Scott Hansen   August 6, 1998
C       The derivatives were verified by R. Scott Hansen
C     on August 23, 1998. 
c
c    .. built into DANLIB by DJK 10-9-98

C    The shape functions are defined by the function FUN(NN)
C     where NN is the node number in the local coordinate system.
C
C    The derivatives of the shape functions are defined by DER(IDER,NN)
C    where the IDER is the derivative to the local coordiinate system.
C           IDER = 1 when d(FUN(NN))/d(xi)
C           IDER = 2 when d(FUN(NN))/d(eta)
C           IDER = 3 when d(FUN(NN))/d(zeta) 
C*****************************************************************       
        REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)

        X = SAMP(I,1)
        Y = SAMP(I,2)
        Z = SAMP(I,3)

C*****************************************************************
C       THE FIRST EIGHT NODES (CORNER NODES)
C       The shape functions for the first eight nodes

        FUN(1)=(1.-X)*(1.-Y)*(1.-Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
        FUN(2)=(1.+X)*(1.-Y)*(1.-Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
        FUN(3)=(1.+X)*(1.-Y)*(1.+Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
        FUN(4)=(1.-X)*(1.-Y)*(1.+Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
        FUN(5)=(1.-X)*(1.+Y)*(1.-Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
        FUN(6)=(1.+X)*(1.+Y)*(1.-Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
        FUN(7)=(1.+X)*(1.+Y)*(1.+Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
        FUN(8)=(1.-X)*(1.+Y)*(1.+Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.

C*****************************************************************
C       The derivatives for the shape functions with respect
C     to XI or the first eight nodes

        DER(1,1)=9./32.*X*(1.-X)*(1.-Y)*(1.-Z)+(-1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
        DER(1,2)=9./32.*X*(1.+X)*(1.-Y)*(1.-Z)+(1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
        DER(1,3)=9./32.*X*(1.+X)*(1.-Y)*(1.+Z)+(1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
        DER(1,4)=9./32.*X*(1.-X)*(1.-Y)*(1.+Z)+(-1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
        DER(1,5)=9./32.*X*(1.-X)*(1.+Y)*(1.-Z)+(-1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
        DER(1,6)=9./32.*X*(1.+X)*(1.+Y)*(1.-Z)+(1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
        DER(1,7)= 9./32.*X*(1.+X)*(1.+Y)*(1.+Z)+(1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
      DER(1,8)=9./32.*X*(1.-X)*(1.+Y)*(1.+Z)+(-1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)

C*****************************************************************
C       The derivatives for the shape functions with respect
C     to ETA the first eight nodes

        DER(2,1)=9./32.*Y*(1.-X)*(1.-Y)*(1.-Z)+(-1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-X)
        DER(2,2)=9./32.*Y*(1.+X)*(1.-Y)*(1.-Z)+(-1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+X)
        DER(2,3)=9./32.*Y*(1.+X)*(1.-Y)*(1.+Z)+(-1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+X)
        DER(2,4)=9./32.*Y*(1.-X)*(1.-Y)*(1.+Z)+(-1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-X)
        DER(2,5)=9./32.*Y*(1.-X)*(1.+Y)*(1.-Z)+(1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-X)
        DER(2,6)=9./32.*Y*(1.+X)*(1.+Y)*(1.-Z)+(1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+X)
        DER(2,7)= 9./32.*Y*(1.+X)*(1.+Y)*(1.+Z)+(1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+X)
      DER(2,8)=9./32.*Y*(1.-X)*(1.+Y)*(1.+Z)+(1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-X)

C*****************************************************************
C       The derivatives for the shape functions with respect
C     to ZETA the first eight nodes

        DER(3,1)=9./32.*Z*(1.-X)*(1.-Y)*(1.-Z)+(-1)/64.*(1.-X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
        DER(3,2)=9./32.*Z*(1.+X)*(1.-Y)*(1.-Z)+(-1)/64.*(1.+X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
        DER(3,3)=9./32.*Z*(1.+X)*(1.-Y)*(1.+Z)+(1)/64.*(1.+X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
        DER(3,4)=9./32.*Z*(1.-X)*(1.-Y)*(1.+Z)+(1)/64.*(1.-X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
        DER(3,5)=9./32.*Z*(1.-X)*(1.+Y)*(1.-Z)+(-1)/64.*(1.-X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
        DER(3,6)=9./32.*Z*(1.+X)*(1.+Y)*(1.-Z)+(-1)/64.*(1.+X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
        DER(3,7)= 9./32.*Z*(1.+X)*(1.+Y)*(1.+Z)+(1)/64.*(1.+X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
      DER(3,8)=9./32.*Z*(1.-X)*(1.+Y)*(1.+Z)+(1)/64.*(1.-X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
C*****************************************************************
C       THE SECOND EIGHT NODES (9-16) 
C       The shape functions for the second eight nodes
        FUN(9)=9.0*(1.-X*X)*(1.-Y)*(1.-Z)*(1.+9.0*(-X/3.))/64.
        FUN(10)=9.0*(1.-X*X)*(1.+Y)*(1.-Z)*(1.+9.0*(-X/3.))/64.
        FUN(11)=9.0*(1.-X*X)*(1.+Y)*(1.+Z)*(1.+9.0*(-X/3.))/64.
        FUN(12)=9.0*(1.-X*X)*(1.-Y)*(1.+Z)*(1.+9.0*(-X/3.))/64.
        FUN(13)=9.0*(1.-X*X)*(1.-Y)*(1.-Z)*(1.+9.0*(X/3.))/64.   
        FUN(14)=9.0*(1.-X*X)*(1.+Y)*(1.-Z)*(1.+9.0*(X/3.))/64.
        FUN(15)=9.0*(1.-X*X)*(1.+Y)*(1.+Z)*(1.+9.0*(X/3.))/64.
        FUN(16)=9.0*(1.-X*X)*(1.-Y)*(1.+Z)*(1.+9.0*(X/3.))/64.   

C*****************************************************************
C       The derivatives for the shape functions with respect
C     to XI or the SECOND eight nodes (9-16)

        DER(1,9)=81./64.*(1.+(-Y))*(1.-X*X)*(1.+(-Z))*(-1./3.)-
     &9./32.*X*(1.+(-Z))*(1.+9.0*(-X/3.))*(1.+(-Y))
        DER(1,10)=81./64.*(1.+(Y))*(1.-X*X)*(1.+(-Z))*(-1./3.)-
     &9./32.*X*(1.+(-Z))*(1.+9.0*(-X/3.))*(1.+(Y))
        DER(1,11)=81./64.*(1.+(Y))*(1.-X*X)*(1.+(Z))*(-1./3.)-
     &9./32.*X*(1.+(Z))*(1.+9.0*(-X/3.))*(1.+(Y))
        DER(1,12)=81./64.*(1.+(-Y))*(1.-X*X)*(1.+(Z))*(-1./3.)-
     &9./32.*X*(1.+(Z))*(1.+9.0*(-X/3.))*(1.+(-Y))
        DER(1,13)=81./64.*(1.+(-Y))*(1.-X*X)*(1.+(-Z))*(1./3.)-
     &9./32.*X*(1.+(-Z))*(1.+9.0*(X/3.))*(1.+(-Y))
        DER(1,14)=81./64.*(1.+(Y))*(1.-X*X)*(1.+(-Z))*(1./3.)-
     &9./32.*X*(1.+(-Z))*(1.+9.0*(X/3.))*(1.+(Y))
        DER(1,15)=81./64.*(1.+(Y))*(1.-X*X)*(1.+(Z))*(1./3.)-
     &9./32.*X*(1.+(Z))*(1.+9.0*(X/3.))*(1.+(Y))
        DER(1,16)=81./64.*(1.+(-Y))*(1.-X*X)*(1.+(Z))*(1./3.)-
     &9./32.*X*(1.+(Z))*(1.+9.0*(X/3.))*(1.+(-Y))

C*****************************************************************
C       The derivatives for the shape functions with respect
C     to ETA the SECOND eight nodes (9 -16)

        DER(2,9)=(-9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(-Z))
        DER(2,10)=(9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(-Z))
        DER(2,11)=(9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(+Z))
        DER(2,12)=(-9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(+Z))
        DER(2,13)=(-9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(-Z))
        DER(2,14)=(9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(-Z))
        DER(2,15)=(9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(+Z))
        DER(2,16)=(-9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(+Z))

C*****************************************************************
C       The derivatives for the shape functions with respect
C     to ZETA the SECOND eight nodes

        DER(3,9)=(-9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(-Y))
        DER(3,10)=(-9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(Y))
        DER(3,11)=(9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(Y))
        DER(3,12)=(9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(-Y))
        DER(3,13)=(-9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(-Y))
        DER(3,14)=(-9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(Y))
        DER(3,15)=(9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(Y))
        DER(3,16)=(9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(-Y))

C*****************************************************************
C       THE THIRD EIGHT NODES (17-24) 
C       The shape functions for the THIRD eight nodes

        FUN(17)=(9./64.)*(1.-X)*(1.-(Y*Y))*(1.-Z)*(1.+9.0*(-Y/3.))
        FUN(18)=(9./64.)*(1.+X)*(1.-(Y*Y))*(1.-Z)*(1.+9.0*(-Y/3.))
        FUN(19)=(9./64.)*(1.+X)*(1.-(Y*Y))*(1.+Z)*(1.+9.0*(-Y/3.))
        FUN(20)=(9./64.)*(1.-X)*(1.-(Y*Y))*(1.+Z)*(1.+9.0*(-Y/3.))
        FUN(21)=(9./64.)*(1.-X)*(1.-(Y*Y))*(1.-Z)*(1.+9.0*(Y/3.))
        FUN(22)=(9./64.)*(1.+X)*(1.-(Y*Y))*(1.-Z)*(1.+9.0*(Y/3.))
        FUN(23)=(9./64.)*(1.+X)*(1.-(Y*Y))*(1.+Z)*(1.+9.0*(Y/3.))
        FUN(24)=(9./64.)*(1.-X)*(1.-(Y*Y))*(1.+Z)*(1.+9.0*(Y/3.))

C*****************************************************************
C       The derivatives for the shape functions with respect
C     to XI or the THIRD eight nodes (17-24)
C
        DER(1,17)=(-9.0/64.0)*(1.+(-Z))*(1.-Y*Y)*(1.+9.0*(-Y/3.0))
        DER(1,18)=(9./64.)*(1.+(-Z))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
        DER(1,19)=(9./64.)*(1.+(Z))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
        DER(1,20)=(-9./64.)*(1.+(Z))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
        DER(1,21)=(-9./64.)*(1.+(-Z))*(1.-Y*Y)*(1.+9.0*(Y/3.))
        DER(1,22)=(9./64.)*(1.+(-Z))*(1.-Y*Y)*(1.+9.0*(Y/3.))
        DER(1,23)=(9./64.)*(1.+(Z))*(1.-Y*Y)*(1.+9.0*(Y/3.))
        DER(1,24)=(-9./64.)*(1.+(Z))*(1.-Y*Y)*(1.+9.0*(Y/3.))

C*****************************************************************
C       The derivatives for the shape functions with respect
C     to ETA the THIRD eight nodes (17-24)

        DER(2,17)=(81./64.)*(1.-Y*Y)*(1.-Z)*(1.-X)*(-1./3.)+
     &(9./32)*Y*(1.-X)*(1.+9.0*(-Y/3.))*(1.-Z)
        DER(2,18)=(81./64.)*(1.-Y*Y)*(1.-Z)*(1.+X)*(-1./3.)+
     &(9./32)*Y*(1.+X)*(1.+9.0*(-Y/3.))*(1.-Z)
        DER(2,19)=(81./64.)*(1.-Y*Y)*(1.+Z)*(1.+X)*(-1./3.)+
     &(9./32)*Y*(1.+X)*(1.+9.0*(-Y/3.))*(1.+Z)
        DER(2,20)=(81./64.)*(1.-Y*Y)*(1.+Z)*(1.-X)*(-1./3.)+
     &(9./32)*Y*(1.-X)*(1.+9.0*(-Y/3.))*(1.+Z)
        DER(2,21)=(81./64.)*(1.-Y*Y)*(1.-Z)*(1.-X)*(1./3.)+
     &(9./32)*Y*(1.-X)*(1.+9.0*(Y/3.))*(1.-Z)
        DER(2,22)=(81./64.)*(1.-Y*Y)*(1.-Z)*(1.+X)*(1./3.)+
     &(9./32)*Y*(1.+X)*(1.+9.0*(Y/3.))*(1.-Z)
        DER(2,23)=(81./64.)*(1.-Y*Y)*(1.+Z)*(1.+X)*(1./3.)+
     &(9./32)*Y*(1.+X)*(1.+9.0*(Y/3.))*(1.+Z)
        DER(2,24)=(81./64.)*(1.-Y*Y)*(1.+Z)*(1.-X)*(1./3.)+
     &(9./32)*Y*(1.-X)*(1.+9.0*(Y/3.))*(1.+Z)

C*****************************************************************
C       The derivatives for the shape functions with respect
C     to ZETA the THIRD eight nodes (17-24)

        DER(3,17)=(-9./64.)*(1.+(-X))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
        DER(3,18)=(-9./64.)*(1.+(X))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
        DER(3,19)=(9./64.)*(1.+(X))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
        DER(3,20)=(9./64.)*(1.+(-X))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
        DER(3,21)=(-9./64.)*(1.+(-X))*(1.-Y*Y)*(1.+9.0*(Y/3.))
        DER(3,22)=(-9./64.)*(1.+(X))*(1.-Y*Y)*(1.+9.0*(Y/3.))
        DER(3,23)=(9./64.)*(1.+(X))*(1.-Y*Y)*(1.+9.0*(Y/3.))
        DER(3,24)=(9./64.)*(1.+(-X))*(1.-Y*Y)*(1.+9.0*(Y/3.))

C*****************************************************************
C       THE FOURTH EIGHT NODES (25-32) 
C       The shape functions for the FOURTH eight nodes

        FUN(25)=(9./64.)*(1.-X)*(1.-(Z*Z))*(1.-Y)*(1.+9.0*(-Z/3.))
        FUN(26)=(9./64.)*(1.+X)*(1.-(Z*Z))*(1.-Y)*(1.+9.0*(-Z/3.))
        FUN(27)=(9./64.)*(1.+X)*(1.-(Z*Z))*(1.+Y)*(1.+9.0*(-Z/3.))
        FUN(28)=(9./64.)*(1.-X)*(1.-(Z*Z))*(1.+Y)*(1.+9.0*(-Z/3.))
        FUN(29)=(9./64.)*(1.-X)*(1.-(Z*Z))*(1.-Y)*(1.+9.0*(Z/3.))
        FUN(30)=(9./64.)*(1.+X)*(1.-(Z*Z))*(1.-Y)*(1.+9.0*(Z/3.))
        FUN(31)=(9./64.)*(1.+X)*(1.-(Z*Z))*(1.+Y)*(1.+9.0*(Z/3.))
        FUN(32)=(9./64.)*(1.-X)*(1.-(Z*Z))*(1.+Y)*(1.+9.0*(Z/3.))

C*****************************************************************
C       The derivatives for the shape functions with respect
C     to XI or the FOURTH eight nodes (25-32)

        DER(1,25)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(-Y))
        DER(1,26)=(9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(-Y))
        DER(1,27)=(9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(Y))
        DER(1,28)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(Y))
        DER(1,29)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(-Y))
        DER(1,30)=(9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(-Y))
        DER(1,31)=(9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(Y))
        DER(1,32)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(Y))

C*****************************************************************
C       The derivatives for the shape functions with respect
C     to ETA the FOURTH eight nodes (25-32)

        DER(2,25)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(-X))
        DER(2,26)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(X))
        DER(2,27)=(9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(X))
        DER(2,28)=(9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(-X))
        DER(2,29)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(-X))
        DER(2,30)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(X))
        DER(2,31)=(9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(X))
        DER(2,32)=(9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(-X))

C*****************************************************************
C       The derivatives for the shape functions with respect
C     to ZETA the FOURTH eight nodes (25-32)

        DER(3,25)=81./64.*(1.+(-X))*(1.-Z*Z)*(1.+(-Y))*(-1./3.)-
     &9./32.*Z*(1.+(-X))*(1.+9.0*(-Z/3.))*(1.+(-Y))
        DER(3,26)=81./64.*(1.+(X))*(1.-Z*Z)*(1.+(-Y))*(-1./3.)-
     &9./32.*Z*(1.+(X))*(1.+9.0*(-Z/3.))*(1.+(-Y))
        DER(3,27)=81./64.*(1.+(X))*(1.-Z*Z)*(1.+(Y))*(-1./3.)-
     &9./32.*Z*(1.+(X))*(1.+9.0*(-Z/3.))*(1.+(Y))
        DER(3,28)=81./64.*(1.+(-X))*(1.-Z*Z)*(1.+(Y))*(-1./3.)-
     &9./32.*Z*(1.+(-X))*(1.+9.0*(-Z/3.))*(1.+(Y))

        DER(3,29)=81./64.*(1.+(-X))*(1.-Z*Z)*(1.+(-Y))*(1./3.)-
     &9./32.*Z*(1.+(-X))*(1.+9.0*(Z/3.))*(1.+(-Y))
        DER(3,30)=81./64.*(1.+(X))*(1.-Z*Z)*(1.+(-Y))*(1./3.)-
     &9./32.*Z*(1.+(X))*(1.+9.0*(Z/3.))*(1.+(-Y))
        DER(3,31)=81./64.*(1.+(X))*(1.-Z*Z)*(1.+(Y))*(1./3.)-
     &9./32.*Z*(1.+(X))*(1.+9.0*(Z/3.))*(1.+(Y))
        DER(3,32)=81./64.*(1.+(-X))*(1.-Z*Z)*(1.+(Y))*(1./3.)-
     &9./32.*Z*(1.+(-X))*(1.+9.0*(Z/3.))*(1.+(Y))
        END
C-----------------------------------------------------------------------

      SUBROUTINE SF32NB(DER,IDER,FUN,SAMP,ISAMP,I)
C     This forms the shape functions and their 
C     derivatives for the 32-noded hexhedrahl
C     element defined in "Finite Elemental Structural
C     Analysis" by Dr. T. Y. Yang
C     The shape function equations (eq. # 10.55)were 
C     taken from section 10.5.3, page 343.  
C     The derivatives were calculate from the shape
C     function equations by Mathematica.
C     Shape functions verified by: 
C     R. Scott Hansen   August 6, 1998
C     The derivatives were verified by R. Scott Hansen
C     on August 23, 1998. 
C
C     The shape functions are defined by the 
C     function
C        FUN(NN)
C      where NN is the node number in the local 
C     coordinate system.
C
C     The derivatives of the shape functions are
C     defined by DER(IDER,NN)
C     where the IDER is the derivative to the local 
C     coordiinate system.
C         IDER = 1 when d(FUN(NN))/d(xi)
C         IDER = 2 when d(FUN(NN))/d(eta)
C         IDER = 3 when d(FUN(NN))/d(zeta) 
C*****************************************************************       
      REAL DER(IDER,*),FUN(*),SAMP(ISAMP,*)

      X = SAMP(I,1)
      Y = SAMP(I,2)
      Z = SAMP(I,3)

C*****************************************************************
C     THE FIRST EIGHT NODES (CORNER NODES)
C     The shape functions for the first eight nodes

      FUN(1)=(1.-X)*(1.-Y)*(1.-Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
      FUN(2)=(1.+X)*(1.-Y)*(1.-Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
      FUN(3)=(1.+X)*(1.-Y)*(1.+Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
      FUN(4)=(1.-X)*(1.-Y)*(1.+Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
      FUN(5)=(1.-X)*(1.+Y)*(1.-Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
      FUN(6)=(1.+X)*(1.+Y)*(1.-Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
      FUN(7)=(1.+X)*(1.+Y)*(1.+Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.
      FUN(8)=(1.-X)*(1.+Y)*(1.+Z)*(-19.0+9.0*(X*X+Y*Y+Z*Z))/64.

C*****************************************************************
C     The derivatives for the shape functions with respect
C     to XI or the first eight nodes

      DER(1,1)=9./32.*X*(1.-X)*(1.-Y)*(1.-Z)+(-1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
      DER(1,2)=9./32.*X*(1.+X)*(1.-Y)*(1.-Z)+(1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
      DER(1,3)=9./32.*X*(1.+X)*(1.-Y)*(1.+Z)+(1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
      DER(1,4)=9./32.*X*(1.-X)*(1.-Y)*(1.+Z)+(-1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
      DER(1,5)=9./32.*X*(1.-X)*(1.+Y)*(1.-Z)+(-1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
      DER(1,6)=9./32.*X*(1.+X)*(1.+Y)*(1.-Z)+(1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
      DER(1,7)= 9./32.*X*(1.+X)*(1.+Y)*(1.+Z)+(1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
      DER(1,8)=9./32.*X*(1.-X)*(1.+Y)*(1.+Z)+(-1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)

C*****************************************************************
C     The derivatives for the shape functions with respect
C     to ETA the first eight nodes

      DER(2,1)=9./32.*Y*(1.-X)*(1.-Y)*(1.-Z)+(-1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-X)
      DER(2,2)=9./32.*Y*(1.+X)*(1.-Y)*(1.-Z)+(-1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+X)
      DER(2,3)=9./32.*Y*(1.+X)*(1.-Y)*(1.+Z)+(-1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+X)
      DER(2,4)=9./32.*Y*(1.-X)*(1.-Y)*(1.+Z)+(-1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-X)
      DER(2,5)=9./32.*Y*(1.-X)*(1.+Y)*(1.-Z)+(1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-X)
      DER(2,6)=9./32.*Y*(1.+X)*(1.+Y)*(1.-Z)+(1)/64.*(1.-Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+X)
      DER(2,7)= 9./32.*Y*(1.+X)*(1.+Y)*(1.+Z)+(1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+X)
      DER(2,8)=9./32.*Y*(1.-X)*(1.+Y)*(1.+Z)+(1)/64.*(1.+Z)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-X)

C*****************************************************************
C     The derivatives for the shape functions with respect
C     to ZETA the first eight nodes

      DER(3,1)=9./32.*Z*(1.-X)*(1.-Y)*(1.-Z)+(-1)/64.*(1.-X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
      DER(3,2)=9./32.*Z*(1.+X)*(1.-Y)*(1.-Z)+(-1)/64.*(1.+X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
      DER(3,3)=9./32.*Z*(1.+X)*(1.-Y)*(1.+Z)+(1)/64.*(1.+X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
      DER(3,4)=9./32.*Z*(1.-X)*(1.-Y)*(1.+Z)+(1)/64.*(1.-X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.-Y)
      DER(3,5)=9./32.*Z*(1.-X)*(1.+Y)*(1.-Z)+(-1)/64.*(1.-X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
      DER(3,6)=9./32.*Z*(1.+X)*(1.+Y)*(1.-Z)+(-1)/64.*(1.+X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
      DER(3,7)= 9./32.*Z*(1.+X)*(1.+Y)*(1.+Z)+(1)/64.*(1.+X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
      DER(3,8)=9./32.*Z*(1.-X)*(1.+Y)*(1.+Z)+(1)/64.*(1.-X)*(-19.0+
     &9.0*(X*X+Y*Y+Z*Z))*(1.+Y)
C*****************************************************************
C     THE SECOND EIGHT NODES (9-16) 
C     The shape functions for the second eight nodes
      FUN(9)=9.0*(1.-X*X)*(1.-Y)*(1.-Z)*(1.+9.0*(-X/3.))/64.
      FUN(10)=9.0*(1.-X*X)*(1.+Y)*(1.-Z)*(1.+9.0*(-X/3.))/64.
      FUN(11)=9.0*(1.-X*X)*(1.+Y)*(1.+Z)*(1.+9.0*(-X/3.))/64.
      FUN(12)=9.0*(1.-X*X)*(1.-Y)*(1.+Z)*(1.+9.0*(-X/3.))/64.
      FUN(13)=9.0*(1.-X*X)*(1.-Y)*(1.-Z)*(1.+9.0*(X/3.))/64.
      FUN(14)=9.0*(1.-X*X)*(1.+Y)*(1.-Z)*(1.+9.0*(X/3.))/64.
      FUN(15)=9.0*(1.-X*X)*(1.+Y)*(1.+Z)*(1.+9.0*(X/3.))/64.
      FUN(16)=9.0*(1.-X*X)*(1.-Y)*(1.+Z)*(1.+9.0*(X/3.))/64.

C*****************************************************************
C     The derivatives for the shape functions with respect
C     to XI or the SECOND eight nodes (9-16)

      DER(1,9)=81./64.*(1.+(-Y))*(1.-X*X)*(1.+(-Z))*(-1./3.)-
     &9./32.*X*(1.+(-Z))*(1.+9.0*(-X/3.))*(1.+(-Y))
      DER(1,10)=81./64.*(1.+(Y))*(1.-X*X)*(1.+(-Z))*(-1./3.)-
     &9./32.*X*(1.+(-Z))*(1.+9.0*(-X/3.))*(1.+(Y))
      DER(1,11)=81./64.*(1.+(Y))*(1.-X*X)*(1.+(Z))*(-1./3.)-
     &9./32.*X*(1.+(Z))*(1.+9.0*(-X/3.))*(1.+(Y))
      DER(1,12)=81./64.*(1.+(-Y))*(1.-X*X)*(1.+(Z))*(-1./3.)-
     &9./32.*X*(1.+(Z))*(1.+9.0*(-X/3.))*(1.+(-Y))
      DER(1,13)=81./64.*(1.+(-Y))*(1.-X*X)*(1.+(-Z))*(1./3.)-
     &9./32.*X*(1.+(-Z))*(1.+9.0*(X/3.))*(1.+(-Y))
      DER(1,14)=81./64.*(1.+(Y))*(1.-X*X)*(1.+(-Z))*(1./3.)-
     &9./32.*X*(1.+(-Z))*(1.+9.0*(X/3.))*(1.+(Y))
      DER(1,15)=81./64.*(1.+(Y))*(1.-X*X)*(1.+(Z))*(1./3.)-
     &9./32.*X*(1.+(Z))*(1.+9.0*(X/3.))*(1.+(Y))
      DER(1,16)=81./64.*(1.+(-Y))*(1.-X*X)*(1.+(Z))*(1./3.)-
     &9./32.*X*(1.+(Z))*(1.+9.0*(X/3.))*(1.+(-Y))

C*****************************************************************
C     The derivatives for the shape functions with respect
C     to ETA the SECOND eight nodes (9 -16)

      DER(2,9)=(-9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(-Z))
      DER(2,10)=(9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(-Z))
      DER(2,11)=(9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(+Z))
      DER(2,12)=(-9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(+Z))
      DER(2,13)=(-9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(-Z))
      DER(2,14)=(9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(-Z))
      DER(2,15)=(9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(+Z))
      DER(2,16)=(-9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(+Z))

C*****************************************************************
C     The derivatives for the shape functions with respect
C     to ZETA the SECOND eight nodes

      DER(3,9)=(-9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(-Y))
      DER(3,10)=(-9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(Y))
      DER(3,11)=(9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(Y))
      DER(3,12)=(9./64.)*(1.-X*X)*(1.+9.0*(-X/3.))*(1.+(-Y))
      DER(3,13)=(-9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(-Y))
      DER(3,14)=(-9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(Y))
      DER(3,15)=(9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(Y))
      DER(3,16)=(9./64.)*(1.-X*X)*(1.+9.0*(X/3.))*(1.+(-Y))

C*****************************************************************
C     THE THIRD EIGHT NODES (17-24) 
C     The shape functions for the THIRD eight nodes

      FUN(17)=(9./64.)*(1.-X)*(1.-(Y*Y))*(1.-Z)*(1.+9.0*(-Y/3.))
      FUN(18)=(9./64.)*(1.+X)*(1.-(Y*Y))*(1.-Z)*(1.+9.0*(-Y/3.))
      FUN(19)=(9./64.)*(1.+X)*(1.-(Y*Y))*(1.+Z)*(1.+9.0*(-Y/3.))
      FUN(20)=(9./64.)*(1.-X)*(1.-(Y*Y))*(1.+Z)*(1.+9.0*(-Y/3.))
      FUN(21)=(9./64.)*(1.-X)*(1.-(Y*Y))*(1.-Z)*(1.+9.0*(Y/3.))
      FUN(22)=(9./64.)*(1.+X)*(1.-(Y*Y))*(1.-Z)*(1.+9.0*(Y/3.))
      FUN(23)=(9./64.)*(1.+X)*(1.-(Y*Y))*(1.+Z)*(1.+9.0*(Y/3.))
      FUN(24)=(9./64.)*(1.-X)*(1.-(Y*Y))*(1.+Z)*(1.+9.0*(Y/3.))

C*****************************************************************
C     The derivatives for the shape functions with respect
C     to XI or the THIRD eight nodes (17-24)
C
      DER(1,17)=(-9.0/64.0)*(1.+(-Z))*(1.-Y*Y)*(1.+9.0*(-Y/3.0))
      DER(1,18)=(9./64.)*(1.+(-Z))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
      DER(1,19)=(9./64.)*(1.+(Z))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
      DER(1,20)=(-9./64.)*(1.+(Z))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
      DER(1,21)=(-9./64.)*(1.+(-Z))*(1.-Y*Y)*(1.+9.0*(Y/3.))
      DER(1,22)=(9./64.)*(1.+(-Z))*(1.-Y*Y)*(1.+9.0*(Y/3.))
      DER(1,23)=(9./64.)*(1.+(Z))*(1.-Y*Y)*(1.+9.0*(Y/3.))
      DER(1,24)=(-9./64.)*(1.+(Z))*(1.-Y*Y)*(1.+9.0*(Y/3.))

C*****************************************************************
C     The derivatives for the shape functions with respect
C     to ETA the THIRD eight nodes (17-24)

      DER(2,17)=(81./64.)*(1.-Y*Y)*(1.-Z)*(1.-X)*(-1./3.)+
     & (9./32)*Y*(1.-X)*(1.+9.0*(-Y/3.))*(1.-Z)
      DER(2,18)=(81./64.)*(1.-Y*Y)*(1.-Z)*(1.+X)*(-1./3.)+
     & (9./32)*Y*(1.+X)*(1.+9.0*(-Y/3.))*(1.-Z)
      DER(2,19)=(81./64.)*(1.-Y*Y)*(1.+Z)*(1.+X)*(-1./3.)+
     & (9./32)*Y*(1.+X)*(1.+9.0*(-Y/3.))*(1.+Z)
      DER(2,20)=(81./64.)*(1.-Y*Y)*(1.+Z)*(1.-X)*(-1./3.)+
     & (9./32)*Y*(1.-X)*(1.+9.0*(-Y/3.))*(1.+Z)
      DER(2,21)=(81./64.)*(1.-Y*Y)*(1.-Z)*(1.-X)*(1./3.)+
     & (9./32)*Y*(1.-X)*(1.+9.0*(Y/3.))*(1.-Z)
      DER(2,22)=(81./64.)*(1.-Y*Y)*(1.-Z)*(1.+X)*(1./3.)+
     & (9./32)*Y*(1.+X)*(1.+9.0*(Y/3.))*(1.-Z)
      DER(2,23)=(81./64.)*(1.-Y*Y)*(1.+Z)*(1.+X)*(1./3.)+
     & (9./32)*Y*(1.+X)*(1.+9.0*(Y/3.))*(1.+Z)
      DER(2,24)=(81./64.)*(1.-Y*Y)*(1.+Z)*(1.-X)*(1./3.)+
     & (9./32)*Y*(1.-X)*(1.+9.0*(Y/3.))*(1.+Z)

C*****************************************************************
C     The derivatives for the shape functions with respect
C     to ZETA the THIRD eight nodes (17-24)

      DER(3,17)=(-9./64.)*(1.+(-X))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
      DER(3,18)=(-9./64.)*(1.+(X))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
      DER(3,19)=(9./64.)*(1.+(X))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
      DER(3,20)=(9./64.)*(1.+(-X))*(1.-Y*Y)*(1.+9.0*(-Y/3.))
      DER(3,21)=(-9./64.)*(1.+(-X))*(1.-Y*Y)*(1.+9.0*(Y/3.))
      DER(3,22)=(-9./64.)*(1.+(X))*(1.-Y*Y)*(1.+9.0*(Y/3.))
      DER(3,23)=(9./64.)*(1.+(X))*(1.-Y*Y)*(1.+9.0*(Y/3.))
      DER(3,24)=(9./64.)*(1.+(-X))*(1.-Y*Y)*(1.+9.0*(Y/3.))

C*****************************************************************
C     THE FOURTH EIGHT NODES (25-32) 
C     The shape functions for the FOURTH eight nodes

      FUN(25)=(9./64.)*(1.-X)*(1.-(Z*Z))*(1.-Y)*(1.+9.0*(-Z/3.))
      FUN(26)=(9./64.)*(1.+X)*(1.-(Z*Z))*(1.-Y)*(1.+9.0*(-Z/3.))
      FUN(27)=(9./64.)*(1.+X)*(1.-(Z*Z))*(1.+Y)*(1.+9.0*(-Z/3.))
      FUN(28)=(9./64.)*(1.-X)*(1.-(Z*Z))*(1.+Y)*(1.+9.0*(-Z/3.))
      FUN(29)=(9./64.)*(1.-X)*(1.-(Z*Z))*(1.-Y)*(1.+9.0*(Z/3.))
      FUN(30)=(9./64.)*(1.+X)*(1.-(Z*Z))*(1.-Y)*(1.+9.0*(Z/3.))
      FUN(31)=(9./64.)*(1.+X)*(1.-(Z*Z))*(1.+Y)*(1.+9.0*(Z/3.))
      FUN(32)=(9./64.)*(1.-X)*(1.-(Z*Z))*(1.+Y)*(1.+9.0*(Z/3.))

C*****************************************************************
C     The derivatives for the shape functions with respect
C     to XI or the FOURTH eight nodes (25-32)

      DER(1,25)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(-Y))
      DER(1,26)=(9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(-Y))
      DER(1,27)=(9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(Y))
      DER(1,28)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(Y))
      DER(1,29)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(-Y))
      DER(1,30)=(9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(-Y))
      DER(1,31)=(9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(Y))
      DER(1,32)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(Y))

C*****************************************************************
C     The derivatives for the shape functions with respect
C     to ETA the FOURTH eight nodes (25-32)

      DER(2,25)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(-X))
      DER(2,26)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(X))
      DER(2,27)=(9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(X))
      DER(2,28)=(9./64.)*(1.-Z*Z)*(1.+9.0*(-Z/3.))*(1.+(-X))
      DER(2,29)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(-X))
      DER(2,30)=(-9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(X))
      DER(2,31)=(9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(X))
      DER(2,32)=(9./64.)*(1.-Z*Z)*(1.+9.0*(Z/3.))*(1.+(-X))

C*****************************************************************
C     The derivatives for the shape functions with respect
C     to ZETA the FOURTH eight nodes (25-32)

      DER(3,25)=81./64.*(1.+(-X))*(1.-Z*Z)*(1.+(-Y))*(-1./3.)-
     & 9./32.*Z*(1.+(-X))*(1.+9.0*(-Z/3.))*(1.+(-Y))
      DER(3,26)=81./64.*(1.+(X))*(1.-Z*Z)*(1.+(-Y))*(-1./3.)-
     & 9./32.*Z*(1.+(X))*(1.+9.0*(-Z/3.))*(1.+(-Y))
      DER(3,27)=81./64.*(1.+(X))*(1.-Z*Z)*(1.+(Y))*(-1./3.)-
     & 9./32.*Z*(1.+(X))*(1.+9.0*(-Z/3.))*(1.+(Y))
      DER(3,28)=81./64.*(1.+(-X))*(1.-Z*Z)*(1.+(Y))*(-1./3.)-
     & 9./32.*Z*(1.+(-X))*(1.+9.0*(-Z/3.))*(1.+(Y))

      DER(3,29)=81./64.*(1.+(-X))*(1.-Z*Z)*(1.+(-Y))*(1./3.)-
     & 9./32.*Z*(1.+(-X))*(1.+9.0*(Z/3.))*(1.+(-Y))
      DER(3,30)=81./64.*(1.+(X))*(1.-Z*Z)*(1.+(-Y))*(1./3.)-
     & 9./32.*Z*(1.+(X))*(1.+9.0*(Z/3.))*(1.+(-Y))
      DER(3,31)=81./64.*(1.+(X))*(1.-Z*Z)*(1.+(Y))*(1./3.)-
     & 9./32.*Z*(1.+(X))*(1.+9.0*(Z/3.))*(1.+(Y))
      DER(3,32)=81./64.*(1.+(-X))*(1.-Z*Z)*(1.+(Y))*(1./3.)-
     & 9./32.*Z*(1.+(-X))*(1.+9.0*(Z/3.))*(1.+(Y))
      END





C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
      SUBROUTINE GET_FACE (NUM,NOD,NDIME,ITYPE,IFACE, 
     &                  FACE,NN_F,NN_FT,IOP)
C      
C     This returns the nodes making up the side (face) of any element
C     usu. 2D elems have 1 face and 3D brick elems have 6 faces
C        Dan Kidger 24-2-94
C
C     .. I could split into 2+ driver routines for IOP=1 and IOP=2 ..
C
C     NUM(*) = global node numbers of this element
C     NOD,NDIME,ITYPE = element type
C     IFACE = input number of the required face / returned total #faces
C     FACE(*) = returned global node numbers of this face
C     NN_FT   = # nodes on this face
C     NN_F    = # nodes around the perimeter of this face (minus mid-face nodes)
C     IOP     = opcode : 1=just return IFACE, 2=return FACE... etc?
C
C     TABLE (-1:*,*) = table of nodes on each face of the element
C      the zero-th elements are used to store other information
C
c     IMPLICIT NONE 
      SAVE
      INTEGER  NUM(*)    !- the input node numbers
     &       ,FACE(*)    !- the output node numbers on a face

      INTEGER NOD,NDIME,ITYPE,NN_F,NN_FT,IOP
      PARAMETER (ITAB1=18,ITAB2=24)
      INTEGER TABLE (-1:ITAB1,ITAB2)     !- 1 elements worth

      INTEGER NOD_OLD, NDIME_OLD,I   !- internal variables
     &  , IFACE_MAX, NN_FT_MAX  
      DATA NOD_OLD, NDIME_OLD /0,0/ !- initialise  (ITYPE ?)

c------------------- do we already have this element ? ---------------

      IF (NOD_OLD.NE.NOD .OR. NDIME_OLD.NE.NDIME) THEN     !- if not 'current'
        NOD_OLD   = NOD
        NDIME_OLD = NDIME
        CALL GET_FACE_TABLE ( NOD,NDIME,ITYPE, TABLE,ITAB1,ITAB2, 
     &                        IFACE_MAX,NN_FT_MAX)
      ENDIF

      IF (IOP.EQ.1) THEN        
        IFACE = IFACE_MAX 
        NN_FT = NN_FT_MAX              !- return this too :-)
      ELSEIF (IOP.EQ.2.or.IOP.EQ.3) THEN    
        NN_FT= TABLE (-1, IFACE)
        NN_F = TABLE ( 0, IFACE)
        DO I=1,NN_FT
c    10-9-97 hmm I would like to keep the mid-side -ve's 
c            so we can get the edges afterwards.
          inode = (TABLE(I,IFACE))
          IF (IOP.EQ.2)
     &    FACE (I) = NUM (ABS(inode))     !- global node numbers
          IF (IOP.EQ.3)               !-preserve the sign (midside nodes)
     &    FACE (I) = SIGN(inode,NUM (inode))
        ENDDO
      ELSE
        STOP '** unknown opcode in GET_FACE' 
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_FACE2 (NOD,NDIME,ITYPE, IFACE, FACE,NN_F,NN_FT,IOP)
C      
C     same as GET_FACE but returns the local node numbers
C     This returns the nodes making up the side (face) of any element
C     usu. 2D elems have 1 face and 3D brick elems have 6 faces
C        Dan Kidger 24-2-94
C
C     .. I could split into 2+ driver routines for IOP=1 and IOP=2 ..
C
C     NUM(*) = global node numbers of this element
C     NOD,NDIME,ITYPE = element type
C     IFACE = input number of the required face / returned total #faces
C     FACE(*) = returned global node numbers of this face
C     NN_FT   = # nodes on this face
C     NN_F    = # nodes around the perimeter of this face (minus mid-face nodes)
C     IOP     = opcode : 1=just return IFACE, 2=return FACE... etc?
C
C     TABLE (-1:*,*) = table of nodes on each face of the element
C      the zero-th elements are used to store other information
C
c     IMPLICIT NONE 
      SAVE
      INTEGER  FACE(*)    !- the output node numbers on a face

      INTEGER NOD,NDIME,ITYPE,NN_F,NN_FT,IOP
      PARAMETER (ITAB1=18,ITAB2=24)
      INTEGER TABLE (-1:ITAB1,ITAB2)     !- 1 elements worth

      INTEGER NOD_OLD, NDIME_OLD,I   !- internal variables
     &  , IFACE_MAX, NN_FT_MAX  
      DATA NOD_OLD, NDIME_OLD /0,0/ !- initialise  (ITYPE ?)

c------------------- do we already have this element ? ---------------

      IF (NOD_OLD.NE.NOD .OR. NDIME_OLD.NE.NDIME) THEN     !- if not 'current'
        NOD_OLD   = NOD
        NDIME_OLD = NDIME
        CALL GET_FACE_TABLE ( NOD,NDIME,ITYPE, TABLE,ITAB1,ITAB2, 
     &                        IFACE_MAX,NN_FT_MAX)
      ENDIF

      IF (IOP.EQ.1) THEN        
        IFACE = IFACE_MAX 
        NN_FT = NN_FT_MAX              !- return this too :-)
      ELSEIF (IOP.EQ.2) THEN    
        NN_FT= TABLE (-1, IFACE)
        NN_F = TABLE ( 0, IFACE)
        DO I=1,NN_FT
C         FACE (I) = NUM (ABS(TABLE(I,IFACE)))     !- global node numbers
          FACE (I) =      ABS(TABLE(I,IFACE))      !- global node numbers
        ENDDO
      ELSE
        STOP '** unknown opcode in GET_FACE' 
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_FACE_TABLE ( NOD,NDIME,ITYPE, TABLE,ITAB1,ITAB2, 
     &  IFACE_MAX,NN_FT_MAX)
C
C         .. a daughter of GET_FACE
C     This returns a table of the nodes that make up each face of a
C     2D/3D element    (1d/4d ??)
C
C     TABLE(:,:) = returned table, column (-1 ) = NN_FT,(0) = NN_F
C     IFACE_MAX, NN_FT_MAX = overall table size
C 16-2-97 6 node wedge 'toblerone' element added (15node too?)
C   10-97? 5 node pyramid added.
C 24-2-98 16 node tesseract added.
C
      INTEGER TABLE(-1:ITAB1,ITAB2)

      INTEGER   T_2NL(-1:4,1),  T_3NL(-1:3,1)     ! 1D
      INTEGER   T_4NQ(-1:4,1),  T_5NQ(-1:5,1)     ! 2D
      INTEGER   T_8NQ(-1:8,1),  T_9NQ(-1:9,1), T_12NQ(-1:12,1),
     &          T_17NQ(-1:17,1),T_3NT(-1:3,1), 
     &          T_6NT(-1:6,1), T_10NT(-1:10,1),T_15NT(-1:15,1)
      INTEGER   T_8NB(-1:4,6), T_14NB(-1:5,6), T_20NB(-1:8,6),   ! 3D
     &          T_32NB(-1:12,6),
     &          T_4NTET(-1:3,4),T_5Np(-1:4,5),  T_6NW(-1:4,5)
      INTEGER   T_16NS(-1:4,24)    ! 4D

c.. for 4D elements: pick which cube (8) we want 
c then treat as an 8nb
C (cf 8nbs being treated as 6*4nq's)

c------------------------- the DATA tables -----------------------------
c     DATA T_2NL / 2, 2,  1,2 /         !- not realy a face?
      DATA T_2NL / 4, 4,  1,2,2,1 /     !- try as a vestigial 4nq face?
      DATA T_3NL / 3, 3,  1,-2,3 /      

      DATA T_4NQ / 4, 4,  1, 2, 3, 4 /
      DATA T_5NQ / 5, 4,  1, 2, 3, 4, 5 /
      DATA T_8NQ / 8, 8,  1,-2, 3,-4, 5,-6, 7,-8 /
      DATA T_9NQ / 9, 8,  1,-2, 3,-4, 5,-6, 7,-8, 9 /
      DATA T_12NQ/12,12,  1,-2,-3, 4,-5,-6, 7,-8,-9, 10,-11,-12 /
      DATA T_17NQ/17,16,  1,-2,-3,-4, 5,-6,-7,-8, 9,-10,-11,-12,
     &                                        13,-14,-15,-16,17 /
      DATA T_3NT / 3, 3,  1, 2, 3 /
      DATA T_6NT / 6, 6,  1,-2, 3,-4, 5,-6 /
      DATA T_10NT/10, 9,  1,-2,-3, 4,-5,-6, 7,-8,-9, 10 /
      DATA T_15NT/15,12,  1,-2,-3,-4, 5,-6,-7,-8, 9,-10,-11,-12,13,14,15
     & /

      DATA T_4NTET
     &  /3,3, 1,2,3,   3,3, 1,4,2,   3,3, 4,3,2,   3,3, 1,3,4 /
      DATA T_8NB  
c-- xyzXYZ
c     &  /4,4,  1,4,8,5,    4,4, 1,5,6,2,    4,4, 1,2,3,4,
c     &   4,4,  7,3,2,6,    4,4, 7,8,4,3,    4,4, 7,6,5,8 /
c-- or try xXyYzZ
     &  /4,4,  1,4,8,5,   4,4,  7,3,2,6, 
     &   4,4,  1,5,6,2,   4,4,  7,8,4,3,
     &   4,4,  1,2,3,4,   4,4,  7,6,5,8 /
      DATA T_14NB     !-- (are all 14nb elements like this ?)
     &  /5,4,  3,4,2,1, -9,   5,4,   1,2,6,5,-13,   5,4,  1,5,7,3,-11,
     &   5,4,  5,6,8,7,-10,   5,4,   7,8,4,3,-14,   5,4,  2,4,8,6,-12 /
      DATA T_20NB
     &  /8,8,    1, -8, 7,-12,19,-20,13, -9,
     &   8,8,    1, -9,13,-14,15,-10, 3, -2,
     &   8,8,    1, -2, 3, -4, 5, -6, 7, -8, 
     &   8,8,   17,-11, 5, -4, 3,-10,15,-16,
     &   8,8,   17,-18,19,-12, 7, -6, 5,-11,
     &   8,8,   17,-16,15,-14,13,-20,19,-18 /

       DATA T_32NB
     &  /12,12,   1,  -9,-13, 2,-18,-22, 6,-14,-10, 5,-21,-17,
     &   12,12,   4, -20,-24, 8,-11,-15, 7,-23,-19, 3,-16,-12,
     &   12,12,   1, -17,-21, 5,-28,-32, 8,-24,-20, 4,-29,-25,
     &   12,12,   3, -19,-23, 7,-31,-27, 6,-22,-18, 2,-26,-30,
     &   12,12,   1, -25,-29, 4,-12,-16, 3,-30,-26, 2,-13, -9,
     &   12,12,   5, -10,-14, 6,-27,-31, 7,-15,-11, 8,-32,-28/

      DATA T_5Np
     &  /4,4,  1,2,3,4,    3,3, 2,1,5,0,    !- base then around
     &   3,3,  3,2,5,0,    3,3, 4,3,5,0,    3,3, 1,4,5,0 /
      DATA T_6NW
     &  /3,3,  1,2,3,0,    3,3, 5,4,6,0,    !- front;back;then around 
     &   4,4,  4,5,2,1,    4,4, 6,4,1,3,    4,4, 5,6,3,2 /

      DATA T_16NS    !.. the 16 node tessaract has 24 polygons 
     &  /4,4,  1,2,6,5,    4,4, 7,8,4,3,    4,4, 3,4,2,1,          !- outer
     &   4,4,  5,6,8,7,    4,4, 2,4,8,6,    4,4, 3,1,5,7,

     &   4,4,  9,10,14,13,   4,4, 15,16,12,11,   4,4, 11,12,10, 9, !- inner
     &   4,4, 13,14,16,15,   4,4, 10,12,16,14,   4,4, 11, 9,13,15,

     &   4,4,  1, 2,10, 9,   4,4,  7, 8,16,15,   4,4,  1, 3,11, 9, !- 12 wings
     &   4,4,  2, 6,14,10,   4,4,  8, 4,12,16,   4,4,  2, 4,12,10,
     &   4,4,  6, 5,13,14,   4,4,  4, 3,11,12,   4,4,  6, 8,16,14,
     &   4,4,  5, 1, 9,13,   4,4,  3, 7,15,11,   4,4,  5, 7,15,13 /
 
C------------------------ common threads -------------------------------

      IF (NDIME.EQ.1) THEN          
c       IFACE_MAX = 0           !- treat 1Ds as NOT polygons ?
        IFACE_MAX = 1           !- 6-12-96 so DXF lines will come thru.
        NN_FT_MAX = NOD
      ELSEIF (NDIME.EQ.2) THEN          
        IFACE_MAX = 1           !- as all 2d elements are polygons
        NN_FT_MAX = NOD
      ELSEIF (NDIME.EQ.3) THEN
        IFACE_MAX = 6            !- all bricks are like this
      ENDIF
C-------------------------------- 1D -----------------------------------
      IF (NDIME.EQ.1) THEN
        IF (NOD.EQ. 2) THEN
c         CALL COPY_TABLE (T_2NL,2,1, TABLE,ITAB1,ITAB2)
          CALL COPY_TABLE (T_2NL,4,1, TABLE,ITAB1,ITAB2)
          NN_FT_MAX = 4               !- hack to 4 nodes
        ELSEIF (NOD.EQ. 3) THEN
          CALL COPY_TABLE (T_3NL,3,1, TABLE,ITAB1,ITAB2)
        ENDIF

C-------------------------------- 2D -----------------------------------
       ELSEIF (NDIME.EQ.2) THEN
        IF (ITYPE.EQ.9) THEN        !-- special case for .OFF polygons -
          TABLE(-1,1) = NOD         ! Here they all appear
          TABLE( 0,1) = NOD         ! with no mid-side or mid-face nodes
          DO I=1,NOD                !
            TABLE(I,1) = I
          ENDDO
        ELSEIF (NOD.EQ. 4) THEN
          CALL COPY_TABLE (T_4NQ,4,1, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ. 5) THEN
          CALL COPY_TABLE (T_5NQ,5,1, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ. 8) THEN
          CALL COPY_TABLE (T_8NQ,8,1, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ. 9) THEN
          CALL COPY_TABLE (T_9NQ,9,1, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ.12) THEN
          CALL COPY_TABLE (T_12NQ,12,1, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ.17) THEN
          CALL COPY_TABLE (T_17NQ,17,1, TABLE,ITAB1,ITAB2)

        ELSEIF (NOD.EQ. 3) THEN
          CALL COPY_TABLE (T_3NT,3,1, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ. 6) THEN
          CALL COPY_TABLE (T_6NT,6,1, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ.10) THEN
          CALL COPY_TABLE (T_10NT,10,1, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ.15) THEN 
          CALL COPY_TABLE (T_15NT,15,1, TABLE,ITAB1,ITAB2)
        ENDIF

C------------------------------- 3D ------------------------------------
      ELSEIF (NDIME.EQ.3) THEN
        IF (NOD.EQ. 4) THEN           !- tetrahedron
          IFACE_MAX = 4               !- has 4 faces 
          NN_FT_MAX = 3               !- of 3 node max each
          CALL COPY_TABLE (T_4NTET,3,4, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ. 8) THEN
          NN_FT_MAX = 4                !- hack as an 8nb for now ?
          CALL COPY_TABLE (T_8NB,4,6, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ.11) THEN         !-- really the same as an 8NB ?
          NN_FT_MAX = 4
          CALL COPY_TABLE (T_8NB,4,6, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ.14) THEN
          NN_FT_MAX = NOD
          CALL COPY_TABLE (T_14NB,5,6, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ.20) THEN
          NN_FT_MAX = NOD
          CALL COPY_TABLE (T_20NB,8,6, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ.26) THEN
c         .. no-op ..
        ELSEIF (NOD.EQ.27) THEN  !(hmm: node 27 is never referenced !)
c         .. no-op ..
        ELSEIF (NOD.EQ.32) THEN  !(hmm: node 27 is never referenced !)
c         .. no-op ..
           NN_FT_MAX = NOD
           CALL COPY_TABLE (T_32NB,12,6, TABLE,ITAB1,ITAB2)

        ELSEIF (NOD.EQ. 5) THEN    !- the 5-node pyramid.
          IFACE_MAX = 5               !- has 5 faces (not 6 like an 8nb)
          NN_FT_MAX = 4               !- of 4 node max each
          CALL COPY_TABLE (T_5NP,4,6, TABLE,ITAB1,ITAB2)
        ELSEIF (NOD.EQ. 6) THEN    !- the 6-node wedge element
          IFACE_MAX = 5               !- has 5 faces (not 6 like an 8nb)
          NN_FT_MAX = 4               !- of 4 node max each
          CALL COPY_TABLE (T_6NW,4,6, TABLE,ITAB1,ITAB2)

        ENDIF

C------------------------------- 4D ------------------------------------
c.. perhaps if 4d I should pre loop and convert into 8*8nbs
c.. maybe even wthatn to pick those nodes whose z=-1.
      ELSEIF (NDIME.EQ.4) THEN
        IF (NOD.EQ.16) THEN
c 8 cubes(6 sides) so 24 faces, but each is shared by two sub-cubes.
          IFACE_MAX =24                !- 6 front, 6 back + 12 sides
          NN_FT_MAX = 4                !- 
          CALL COPY_TABLE (T_16NS,4,24, TABLE,ITAB1,ITAB2)
        ENDIF
      ENDIF

C      IF (abs(FUN(1)-230964.).lt.0.1) 
C     +  WRITE(*,'(A,I2,A,I3,A,I3,a,i2)')
C     + '** WARNING: element not found: ndime=',ndime,' nod=',nod
C     +,' type=',itype
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE COPY_TABLE (TABLE_MASTER,IT1,IT2, TABLE,ITAB1,ITAB2)
C
C       >> a daughter of GET_FACE_TABLE)
C     This simply copies a table from TABLE_MASTER to TABLE
C
      INTEGER TABLE_MASTER(-1:IT1,IT2), TABLE(-1:ITAB1,ITAB2)
      DO J=1,IT2
        DO I=-1,IT1
         TABLE(I,J) = TABLE_MASTER(I,J)
        ENDDO
      ENDDO
      END
C-----------------------------------------------------------------------
      SUBROUTINE GET_EDGE (FACE,NN_F,NN_FT, IEDGE,EDGE,npts)
c
c     For the given FACE this returns the N'th edge. 
c        Dan Kidger  10-9-97
c     Notes:
c       1/ Midside nodes are marked as -ve
c       2/ The number of +ve numbers = NEDGES+1
c       3/ note that internal nodes appear at the end of the list
c

      INTEGER FACE(*)    !- the input node numbers on a face
     &       ,EDGE(*)    !- the output edge.

c------- 1: look for the desired edge -----
      jedge = 0
      DO Ibase=1,NN_F    !- loop around the boundary nodes.
        if (face(ibase).gt.0) jedge = jedge+1
        if (iedge.eq.jedge) goto 22
      ENDDO
   22 continue
c     edge (1) = num(face(ibase))
      edge (1) =    (face(ibase))

c--- 2: abstract the nodes on this edge ------
      npts = 0
      do i = ibase, nn_f
         npts = npts + 1
         edge(npts) = abs(face(i))
         if (npts.ge.2.and.face(i).ge.0) goto 33 !- all done
      enddo
   33 continue

      RETURN
      END 
c-----------------------------------------------------------------------

