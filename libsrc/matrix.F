c-----------------------------------------------------------------------
C     MATRIX.F
c
c     A library of matrix manipulation routines:
C     1/ Equation solvers                       (Solve_cg,Sparin,...)
c     2/ An Eigenvalue solver                   (Tred2 form 'Eispack')
c     3/ General (mini) matrix vector routines  (Mat_mult,Vvmult,...)
C     4/ FE-styled routines:                    (Invert_jac, Checon3,..)
c
C                    Dan Kidger 1995,96,97
C-----------------------------------------------------------------------

c   Revisions:
c      25-9-99 explicit 24x24, 60x60 versions of sub_bit
c              - fixed bug in sub_bit_upper
c              - pcg: print stats of Mflops and convergence rate
c      7-11-99 explicit BLAS where possible (-DBLAS)
c     18-11-99 work on SPARIN,SPABAC: get mflops and mflops/s
c     27-07-00 changed mflops table in PCG
c     10- 3-01 added BANDRD/ BISECT for Eigenvalues
c     12- 3-01 adding flops calcs for BANRED etc.
c
c

c^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^
c
c     "Parallelism and M.P.I."
c             Dan Kidger 14-May-1997
c
c (A) Initial Points:
c     --------------
c      1/ The Lochinver conference on Parallel F.E. was 2 weeks ago.
c      2/ I think I want a coarse grained parallelism with a 'local'
c         set of node numbers and element numbers on each processor.
c         So really a set of 'connected analyses'
c      3/ *A corner-stone will be a parallel PCG solver*
c      4/ Think about EXPLICIT methods too (cf PamCrash).
c
c (B) A Parallel PCG solver
c     ---------------------
c      1/ Needs to be fed the 'x' for all its nodes (easy)
c      2/ Do lots of Mat*vec. to produce this proc's U(?)
c      3/ send the boundary-U to our neighbours
c      4/ add-in the received boundary-nodes.
c      5/ vector-vector bit: need to (MPI_Greduce) global sum UP and DOWN
c         (hence sync. all the processors here)
c      6/ Continue vector-vectoring (inc. another Global_reduce)
c      7/ note that this means that boundary nodes will get simultaneously
c         updated on both processors. This is wasteful but OK.
c      8/ convergence checking: Maybe I should uncouple this to a local
c         check only - if 'bad' tell other processors to keep going.
c          else the algorithm naturaly terminates.
c
c (C) Preconditioning
c     ---------------
c      1/ Build the Precon on each processor seperately.
c      2/ For boundary nodes we need to add-in the diag-precon value
c         of the other proc's
c      3/ hence invert the diagonals on each proc.
c      4/ non-Diag precons: (eg. Choleski) are 10 times harder?!
c      5/
c
c^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^

C-----------------------------------------------------------------------
C     Section 1 :    Matrix solvers
C
C-----------------------------------------------------------------------
c     There should be only 5 user-callable entry-points:
c
c     0    INIT_KV           : malloc and Null
c     1    ASSEMBLE_KV   * Store a KM matrix *
c     2    APPLY_BIG_SPRINGS : adds a big # to the 'diagonal' terms
c     3    FACTOR_KV         : reduces the global matrix (apply precon?)
c     4    SOLVE_KV          : back-substitutes the global matrix
C     5    MULT_KV_VEC       : multiplies the big matrix by a vector
C
C     Each of these can apply to one of 5 possible storage stratagies:
c     istore = 0    'Sparin' skyline storage
c     istore = 1    'CG' element by element storage (upper diag only ?)
c     istore = 2    'Full KM'- global (unbanded) matrix
c     istore = 3    'Banred' Upper half (diagonal-first)
c     istore = 4    'Cholin' lower-half (in horizontal strips)
C
C    ( Note technically we can do CG with *any* of the formats cos we
c      already have a KMxV routine for each.)
C    ( Note split between direct solvers and iterative ones; maybe I
c      should have more iterative ones such as Gauss-Seidal (as in
C      Laplacian mesh generation)
C    ( Note 2nd options, eg lower tri v. full matrix store, which
C      Preconditioner, Lego storage (affects building only) )
c
c ... also 'toolbox' routines:
C      INIT_KV
C   &  'Calc storage requirements'
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

c  Notes to installers of new Solver Algorithms/Storage Schemes:
c A/ The storage scheme is coded by ISTORE - so pick a new unique number.
c B/ put your routines together in a section at the end of this file.
c C/ modify the following to make calls to your stuff.

c    Each method will need:
c     1. A data structure to store everything in (Fortran 90 would be
c          nicest - hence make it PRIVATE to a module?
c     2. A call from INIT_KV to 'clear' the arrays
c          - to allocate them; to init internal counters, etc.
c     3. A call from ASSEMBLE_KV to add an element KM (and/or G)
c         (no need for a 'remove element -  we usualy zap the lot.)
c     4. A call from APPLY_BIG_SPRINGS to add a large value to DIAG terms
c          for prescribed values (eg. disps)
c        -- hmm cf rigid links twixt nodes?
c     5. FACTOR_KV - to factorise the matrix after full assembly
c        - cf building a preconditioner (eg. incomplete Choleski)
c     6. BACSUB_KV - to 'solve' the equations ( eg. PCG)
c           < note PCG's internal Matrix*vector routine - SUB_BIT >
c     7. MULT_KV_VEC - multipies the whole KV by a RHS
c           potentailly used by a PCG solver?
c
c

C-----------------------------------------------------------------------
      SUBROUTINE INIT_KV (KV,IKV,IR,KDIAG,IKDIAG, N,ISTORE)
C
C     Initialise (ie. NULL) the arrays used for storing the stiffness matrix.
C     including storing PCG flags for
C          1:Preconditioner, 2:upper-tri only, 3:Big springs.
C     (also return IR,IR2 = working pointers for PCG (better in KDIAG itself) )
C           DJK 6-8-95
c
c      ---> really we are just NULLing KV for directs (KDIAG is done before)
C           for PCG we fill KDIAG(1:19) with control info
C           (things like which 'Precon' surely get applied later!)
C
      IMPLICIT NONE              !
      INTEGER IKV,IKDIAG         !

      REAL KV(IKV)               ! Stiffness matrix (to be nulled)
      INTEGER KDIAG(IKDIAG)      ! Pointers for Sparin/CG methods
     &       ,IR,N               ! Used length of KV, #of freedoms
     &       ,ISTORE             ! Storage method (1=PCG, etc.)
      INTEGER I,IHEAD            !- local variables

      IF (ISTORE.EQ.0 .or.              !- Sparin
     &    ISTORE.EQ.2 .or.              !- NxN
     &    ISTORE.EQ.3 .or.              !- Banred
     &    ISTORE.EQ.4 ) THEN            !- Cholin

c     or call my_reallocate_1d (kv, ir,ikv, .false.)
c     cf call my_reallocate_2d (gc, igc,igg,nn, mnn, .true.)
c  and even 'call extend_array'   usu. with a factor of sqrt(2) each time
c       if (allocated(kv)) deallocate (kv)  !- ie. on second pass.
c       allocate (kv(ir))
c- if this fails then should print a 'nice' error message
c    ie. 'This job needs xxxx Mb of storage for KV'

        CALL NULVEC (KV,IR)       !- zap for SPARIN,NxN, etc.
c                                 ... (cf init of KDIAG for SPARIN)

      ELSEIF (ISTORE.EQ.1        !------- conjugent gardient solver -------
     &   .or. ISTORE.EQ.6) THEN     !(1=if 'full', 6 if upper tri only
c  (after here, we use the internal KDIAG(6) as the flag )
        Ihead = 19                !- length of KDIAG reserved for header info
        DO I=1, Ihead
          KDIAG(I) = 0
        ENDDO
        KDIAG(1) = 12345          !- A magic Cookie ? :-)
        KDIAG(2) = ihead          !- Current end-point of KDIAG (after headers)
        KDIAG(3) = 0              !- Current end-point of KV
        KDIAG(4) = 1              !- default = store full KM's
        IF (ISTORE.EQ.6)          !
     &  KDIAG(4) = 2              !- store upper tri's only
        KDIAG(5) = 0              !- # Element matrices stored so far
        KDIAG(6) = ihead+1        !- start-point of the G's (defer?)
c ..  (7=length?)
c ..  8=precon type,   9=start pos, (10=length?)
c .. 11=#big-springs, 12=start pos, (13=length?)

      ELSE
        CALL MYERROR(3,'Unsupported KV storage method (INIT_KV)')
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE ASSEMBLE_KV (KV,IKV,IR, KDIAG,IKDIAG,N, KM,IKM,G,IDOF
     &  ,ISTORE, IPOS)
C
C     This assembles the given element stiffness matrix KM into the
C     Global Matrix KV. KDAIG is a set of integer flags that are updated
c     if necessary (currently only for PCG methods)
c     -- Should we store IEL with PCG (or allways sequential?)
c
c     ILEGO   : 0 = store all (KM and G)'s
C             : 1 = check wether 'new', if so only store G
C     ISTORE : the storage method : 0=SPARIN,1=PCG, etc.
C     IPOS   : 0 = Store KM in the nest free space  (only for PCG)
C              -1 = same as the last elem, so co-point
C              >0 here is the *explicit* place where its KM is
C         **   -2 = full scan of KV for a match! ** (not yet implimented)
C              IPOS is always set to the actual storage pos on exit
C
C        DJK  14-8-95
C
      REAL KV (IKV), KM(IKM,IKM)
      INTEGER KDIAG(IKDIAG),N, G(IDOF)

      INTEGER IPOS_OLD            !- where the *last* KM was stored in
      DATA IPOS_OLD /-123/        !- PCG's KV

C.. can even do here .. parse the table, and compare new KM with stored
C.. as soon as we get a no-match, try next KM.
c      If full-match then just store a pointer
c      If no matches at all store a new KM

      IF (ISTORE.EQ.0) THEN                     !----- Sparin -----
        CALL FORM_SPARCE_KV (KM,IKM,IDOF,G,KDIAG, KV)
        IPOS = 1
      ELSEIF (ISTORE.EQ.1.or.ISTORE.eq.6) THEN  !------ PCG -------
        IF (IPOS.EQ.0) THEN                 !- store a new one
          IR  = KDIAG(3)                    !- KV_end
          IPOS = IR                         !.. remember start-point for below
          IMETH = KDIAG(4)                  ! sq/tri
          CALL FORM_PCG_KV (KV,IKV,KM,IKM,IDOF,IR, IMETH)  !- store KM
          KDIAG(3) = IR                     ! update KV end-point
        ELSEIF (IPOS.EQ.-1) THEN            !- same as last
          IPOS = IPOS_OLD
        ELSEIF (IPOS.EQ.-2) THEN            !- auto-search
          CALL MYERROR (3,'Auto-search of KM''s not yet implimented')
        ELSEIF (IPOS.GT.0) THEN             !- explicit old one
c                                           !(no-op as we are given IPOS)
        ENDIF
        IPOS_OLD = IPOS                     !- remember where it was stored
c                                           !  for subsequent calls
        IC = KDIAG(2)                       !- current end-point
        CALL FORM_PCG_KDIAG (KDIAG,IKDIAG,G,IDOF,IC,IPOS)  !- store G's & IPOS
        KDIAG(2) = IC                       !- update KDIAG end-point
        KDIAG(5) = KDIAG (5) + 1            !- # of elements (G's) stored


      ELSEIF (ISTORE.EQ.2) THEN                            !- NxN
        CALL FORM_FULL_KV (KV,IKV,KM,IKM,IDOF,G,N)
        IPOS = 1
      ELSEIF (ISTORE.EQ.3) THEN                            !-  Banred
c       IBW = IR/N  -1
        CALL FORMKV (KV,  KM,IKM,G,N, IDOF )
        IPOS = 1
      ELSEIF (ISTORE.EQ.4) THEN                            !-  Cholin
        IBW = IR/N -1                              !26-03-99 we need -1
        CALL FORMKB (KV,N,  KM,IKM,G,IBW,IDOF )
        IPOS = 1
      ELSE
        CALL MYERROR (2,'Unknown storage method : SPARIN/PCG/?')
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE APPLY_BIG_SPRINGS (KV,IKV,IR,KDIAG,IKDIAG,N
     &         ,NF,INF,NN,NODOF, FORCES_N,MDF,BIG_SPRING,ISTORE)
c
c     This applies a big-spring to the diagonal terms of KP
c     where
c         istore = 0  'Sparin' skyline storage
c         istore = 1  'CG' element by element storage
c         istore = 2  'Full KM'- global (unbanded) matrix
c         istore = 3  'Banred' Upper half (diagonal-first)
c         istore = 4  'Cholin' lower-half (in horizontal strips)
c                = 6  'CG' -upper triangles stored
c   .. hmm, I should really scan to find my own BIG_SPRING value
C .... hmm, warning message if this not is NOT a valid freedom ?
c
      REAL KV(IKV), FORCES_N(MDF,NN)
      INTEGER NF(INF,NN), KDIAG(IKDIAG)

      IC = 0
      DO I=1,NN
        DO J=1,NODOF
          K = NF(J,I)
          IF (K.NE.0.AND.ABS(FORCES_N(J,I)).GT.1.E-8) THEN
          IC = IC + 1   !- count of loaded freedoms

      IF (ISTORE.EQ.0) THEN              !-- SPARIN method
        KV (KDIAG(K)) = BIG_SPRING
      ELSEIF (ISTORE.EQ.1.or.istore.eq.6) THEN          !-- CG by EBE
c.. new method stores a table of freedoms into KDIAg (plus a set of
c.. BIG_SPRINGS ?

        KDIAG (11) = IC
        IF (IC.eq.1) THEN                !-- for the first one:
          KDIAG (13)= KDIAG (3) +1          !- get storage point in KV
          KDIAG (3) = KDIAG(13)             !- KV gets a little longer
          KV (KDIAG(13)) = BIG_SPRING       !- just store one at the end
          KDIAG(12) = KDIAG(2)              !-- K's in KDIAG starts here
        ENDIF
        KDIAG (KDIAG(12)+IC) = K           !- store the freedom number.
        KDIAG(2) = KDIAG(2) + 1            !- KDIAG gets a little longer

      ELSEIF (ISTORE.EQ.2) THEN          !-- NxN
        KV ((K-1)*N+K)= BIG_SPRING
      ELSEIF (ISTORE.EQ.3) THEN          !-- Banred
        KV (K) = BIG_SPRING
      ELSEIF (ISTORE.EQ.4) THEN          !-- Cholin
c---    since we know IR, we can 'deduce' what the BW was!
        IBW = IR / N
c       IBW = IR/N -1                              !26-03-99 we need -1
c       KV ((K-1)*IBW+IBW) = BIG_SPRING
        KV ((K-1)*N+IBW) = BIG_SPRING
      ELSE
        CALL MYERROR (2,'Unknown KV type to apply big-springs')
      ENDIF

          ENDIF   !- only presc. disp freedoms
        ENDDO   !- loop NODOF
      ENDDO   !- loop nodes
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE FACTOR_KV (KV,IR,KDIAG,N,ISTORE, IPR)
c
c     This will factorise the stiffness matrix
c     for any storage strategy
c         istore = 0  'Sparin' skyline storage
c         istore = 1  'CG' element by element storage
c         istore = 2  'Full KM'- global (unbanded) matrix
c         istore = 3  'Banred' Upper half (diagonal-first)
c         istore = 4  'Cholin' lower-half (in horizontal strips)
c
c     IPR >=1  to print progess (CG iters / SPARIN row #)
c
      REAL KV(IR)
      INTEGER KDIAG(*)
      IF (ISTORE.EQ.0) THEN              !-- SPARIN method
        CALL SPARIN (KV,N,KDIAG, IPR)
      ELSEIF (ISTORE.EQ.1.or.ISTORE.eq.6) THEN          !-- CG by EBE
c     ---- in CG this is a no-op
      ELSEIF (ISTORE.EQ.2) THEN          !-- NxN
c     ---- n/a as we only use NxN for eigenvalues?
c     CALL SOLVE () .. but do at the BACSUB stage only?
      ELSEIF (ISTORE.EQ.3) THEN          !-- Banred
        IBW = IR/N -1                              !26-03-99 we need -1
        CALL BANRED (KV,N,IBW,IPR)
      ELSEIF (ISTORE.EQ.4) THEN          !-- Cholin
        IBW = IR/N -1                              !26-03-99 we need -1
        CALL CHOLIN (KV,N,N,IBW,IPR)      !
      ELSE
        CALL MYERROR (2,'Unknown KV type to factorise')
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE BACKSUB_KV (KV,IKV,IR,KDIAG,N,ISTORE,DISPS,LOADS
     &  ,iters_cg, IPR)
c
c     This will 'back-substitute' in KV to get the LHS (note CG)
c     for any storage strategy
c         0,2,3,4 = 'Direct Methods'
c         istore = 0  'Sparin' skyline storage
c      *  istore = 1  'CG' element by element storage     *
c         istore = 2  'Full KM'- global (unbanded) matrix
c         istore = 3  'Banred' Upper half (diagonal-first)
c         istore = 4  'Cholin' lower-half (in horizontal strips)
c         istore =10-19  All Conjugent Gradient Iterative Methods ?
c   .. really need a set of CG storage strategies (held in KDIAG(2)?)
c
c     IPR >=1  to print progess (CG iters / SPARIN row #)

      REAL KV(IKV), DISPS(0:N), LOADS(0:N)
      INTEGER KDIAG(*)
      ITERS_CG = 0
      IF (ISTORE.EQ.0) THEN              !-- SPARIN method
        DO I=1,N
          DISPS(I) = LOADS(I)
        ENDDO
c ** next line is for testing only !!!  DJK 15-11-99
c       DO I=1,1000
        CALL SPABAC (KV,DISPS,N,KDIAG,IPR)   !- careful with loads(0:n) **
c       ENDDO
      ELSEIF (ISTORE.EQ.1.or.istore.eq.6) THEN          !-- CG by EBE
c       print*,'debug: calling SOLVE_CG'
c       CALL SOLVE_CG (KV,IKV,IR,LOADS,DISPS,N,KDIAG, ITERS_CG,IPR)  !orig
        CALL solve_conjgrad(KV,IKV,IR,LOADS,DISPS,N,KDIAG, ITERS_CG,IPR)
c  =47 iters,disp=.22225
c       CALL SOLVE_CG2(KV,IKV,IR,LOADS,DISPS,N,KDIAG, ITERS_CG,IPR)  !Lucquin
c  =89 iters,disp=.
      ELSEIF (ISTORE.EQ.2) THEN          !-- NxN
c     ---- n/a as we use only NxN for eigenvalues?
c 25-03-99 since SOLVE uses pivoting it breaks KV.
        CALL SOLVE (KV,N, DISPS(1),LOADS(1),N)
      ELSEIF (ISTORE.EQ.3) THEN          !-- Banred
        DO I=1,N
          DISPS(I) = LOADS(I)
        ENDDO
        IBW = IR/N -1                              !26-03-99 we need -1
        CALL BACSUB (KV,DISPS,N,IBW,IPR)
      ELSEIF (ISTORE.EQ.4) THEN          !-- Cholin
        DO I=1,N
          DISPS(I) = LOADS(I)
        ENDDO
        IBW = IR/N -1                              !26-03-99 we need -1
c       CALL CHOBAC (KV,IBW,DISPS,N,IBW)   !- (be careful!)
        CALL CHOBAC (KV,N,DISPS,N,IBW,IPR)   !- (be careful!)
      ELSE
        CALL MYERROR (2,'Unknown KV type to factorise')
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE MULT_KV_VEC (KV,IR,KDIAG,N,ISTORE, DISPS,LOADS)
c
c     This mulitplies the global stiffness matrix KV by a vector
c     for any storage strategy
c     (15-8-95 : not yet used by my DANFE .. cf BIOT methods)
c         istore = 0  'Sparin' skyline storage
c         istore = 1  'CG' element by element storage
c         istore = 2  'Full KM'- global (unbanded) matrix
c         istore = 3  'Banred' Upper half (diagonal-first)
c         istore = 4  'Cholin' lower-half (in horizontal strips)
c
      REAL KV(IR), DISPS(0:N), LOADS(0:N)
      INTEGER KDIAG(*)

      IF (ISTORE.EQ.0) THEN              !-- SPARIN method
        CALL LINMLS (KV, DISPS(1), LOADS(1), N, KDIAG)
      ELSEIF (ISTORE.EQ.1.or.istore.eq.6) THEN          !-- CG by EBE
c ** do this here **

      ELSEIF (ISTORE.EQ.2) THEN          !-- NxN        !note skips of the zero'th
        CALL MVMULT (KV,N,DISPS(1),N,N,LOADS(1) )    !term in LOADS
      ELSEIF (ISTORE.EQ.3) THEN          !-- Banred
        IW = IR/N
        CALL LINMUL (KV,DISPS(1),LOADS(1),N,IW)
      ELSEIF (ISTORE.EQ.4) THEN          !-- Cholin
        IBW = IR/N -1                              !26-03-99 we need -1
        CALL BANMUL (KV,IBW,DISPS(1),LOADS(1),N,IBW)
      ELSE
        CALL MYERROR (2,'Unknown KV type for matrix * vector')
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
c     Individual solvers & matrix-builders:
c     SPARIN,CG,NxN,Banred,Cholin
C     Also surely some general purpose routines: eg calc bandwidth..
C-----------------------------------------------------------------------

c-----------------------------------------------------------------------
C----------------- 0: Choleski-Skyline (SPARIN) ------------------------
C-----------------------------------------------------------------------
      SUBROUTINE FORM_KDIAG (KDIAG,G,IDOF)
C
C     This finds the maximum bandwidth for each freedom (=FKDIAG)
C      - for upper triangle methods - so last row is bound to be = 1
c
C     usgae: null KDIAG, loop all elements, get G & call this.
C     At the end, KDIAG=list of bandwidths for each freedom.
C     Thence call RESEQ_KDIAG to calc the cummulative values.
C     This can be used as a precursor for BANRED and CHOLIN too:
C        just find MAXVAL(KDIAG)
C     This calc the UPPER_BW - contrast this routine with one which
c     calcs the LOWER_BW - the sum of the two would give the total BW
C     for each node.
C     For the MEAN_BW then we will need both: pick the MAX for each node.
C     < Contrast this with an algorithm to actually COUNT (and list) the
C       freedoms attached to each node. >
C           DJK 2-2-97
C
      INTEGER KDIAG(*),G(*)
      DO I=1,IDOF
        IWP1=1
        IF (G(I).NE.0) THEN          !- skip 'fixity'
          DO J=1,IDOF
            IF (G(J).NE.0) THEN      !- skip 'fixity'
              IM_UPPER = G(I)-G(J)+1   !- the Upper BW   (+1 cos of the
              IM_LOWER = G(J)-G(I)+1   !- the Lower BW   ( DIAG itself.
              IM = IM_UPPER            !- here we want the upper
              IF(IM.GT.IWP1) IWP1=IM   !- ie. 'max' value so at least=1
            ENDIF
          ENDDO
          K=G(I)                          !- 'dummy'
          IF (IWP1.GT.KDIAG(K)) KDIAG(K)=IWP1   !- this elem gives a bigger BW
        ENDIF
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
c         Notes on an Algorithm to find the list of freedoms (nodes)
c                    attached to each freedom (node)
c                              DJK 2-2-97
c      3 phases:
c       1: loop all elements and find the number of different nodes attached
c       2: malloc enough storage for each row
c       3: re-loop the elems and fill-in each row; in ascending order.
c   The problem is that it is very hard to do this in order N algorithm
c    - ie. simultaneously update each node.
c   Maybe we can find a method akin to Iron's Frontal solver
c    - for each freedom, find which element it is refered to first and
c       which, last. Hence only need to sub-loop these.
c      Whilest I am at it I can even count the number of elements this node
c      has a hit (is connected) to. < and contour > ?
c         < note mesh-quality analyses in DANPLOT>
c   - so loop freedoms:
c      - loop element range.
c         if a hit, compile list of touchers. (or count and malloc later)
c   Still not very elegant :-(
c
c----------------------------------
c   Method #2 : If we have a list of elements attached to each node...
c     1. Loop each node (1:NN)  !& loop each freedom (3)
c     2.   Consult the table and loop this node's elements.
c     3.     Loop each element's nodes and so build up a list (in ascending
c            order of all the nodes connected to 'our' node.
c     4.   So now got a list of nodes attached to my node. - now work on
c          the freedoms themselves...
c     5.   loop the nodes in this row and expand into freedoms
c          (so for BIOT some nodes have 3 some have 2, some have zero (fixities)
c     6.   store this row for all (3) freedoms
c          < since all 3 rows will be the same maybe we only need to store
c            it once ? >
c     7. so now have the full table - note the use of a pointer array
c        for the start of each row.
c
c   < alternative MATRIC-VECTOR mult. ..
c        if we just store the list of nodes connected to nodes.
c     1. Loop nodes
c     2.   Loop its touchers
c     3.     loop their NF's and list(i++) store them (hence #freedoms)
c     4.   So got the list to use so..
c     5.   Loop the (3) non-zero freedoms
c     6.   Loop across the freedoms and dot_product the KV*X terms.
c       and that is it.
c
c
c-------------------------------
c   Method 3 : Utter brute force!
c     1. Loop the nodes (or freedoms)  - and null the list.
c     2.   Loop all the elements.
c     3.     Loop its (20) nodes &loop its (3) freedoms.
c     4.       Add freedoms to the list (in ascending order)
c     5.   So save the completed list for this node (freedom)
c
c  Q. Can the partial list be of use to build the rest?
c
c  P. Upper-triangle: only keep nodes that are >= self.
c
c
C-----------------------------------------------------------------------
c                  Notes on an Algorithm to find
c            the list of elements attached to each node
c                           DJK 2-2-97
c
c  1. Zero counters P() for every node
c  2. loop elements and loop each's (20) nodes:  & p(inode)++
c  3. hence now got the number of elem hits each node has
c  4. malloc an array for all the nodes and null each row
c  5. loop elements and loop each's (20) nodes.
c     - for this node; insert its parent element's number (in numerical order)
c  6. hence now got a list of elems attached to each node.
c
c  Notes:
c    If I only need sequential access, then I can flag the first elem
c    of each node's list as -ve, so no need for a pointer array?
c
C-----------------------------------------------------------------------
      SUBROUTINE RESEQ_KDIAG (KDIAG,N,  IR,IBW_MIN,IBW_MAX)
C
C     This simply resequences the KDIAG pointers (for SPARIN)
C     and also returns the max and min. bandwidths (for BANRED,CHOLIN)
C     (if upper triangle then min. is always =1, if 0 then degenerate eqns!)
C
      IMPLICIT NONE
      INTEGER N,KDIAG(N)           !--- (only need the first N entries)
     &       ,IR,IBW_MIN,IBW_MAX
      INTEGER  IW,I                !- local variables
      IR = 0
      IBW_MIN  = 99999
      IBW_MAX  = 0
      DO I=1,N
        IW = KDIAG(I)
        IBW_MIN = MIN (IBW_MIN,IW)
        IBW_MAX = MAX (IBW_MAX,IW)
        IR = IR + IW
        KDIAG(I) = IR
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE FORM_SPARCE_KV (KM,IKM,IDOF,G,KDIAG, KV)
C
C     This assembles the element stiffness matrix into the global matrix
C     stored as a vector accounting for a variable bandwidth (was 'FSPARV')
C     Needs KDIAG to have been pre-formed (FORM_KDIAG,RESEQ_KDIAG)
C     (SPARIN style)
C     note: even though only the *lower* triangle is stored, it still loops
c     over *all* of KM(,)
C
      REAL KV(*), KM(IKM,*)
      INTEGER KDIAG(*), G(*), G1, G2, I,J,IW,ILOC

      DO I=1,IDOF
        G1 = G(I)
        IF (G1.NE.0) THEN
          DO J=1,IDOF
            G2 = G(J)
            IF (G2.NE.0) THEN
              IW = G1-G2
              IF (IW.GE.0) THEN
                ILOC = KDIAG(G1)-IW          ! minus cos below the diagonal
                KV(ILOC) = KV(ILOC)+KM(I,J)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE SPARIN (A,N,KDIAG, IPR)
C
C     This performs Choleski reduction of the
C     variable-bandwidth stiffness matrix stored as a vector
C     A() is the skyline matrix, KDIAG(1:N) points to the start of each
c     line in A().
C     IPR >=2 for a print of the progress as I/N
C
      REAL A(*)
      INTEGER KDIAG(*), NZEROS
      real ::x=0.,x1
      real flops
      save flops

c.. calc total flops needed ..
      flops = 2
      do i=2,n
        KI = KDIAG(I)-I                 !- row_length
        L  = KDIAG(I-1)-KI+1            !- start_of_row
        DO J=L,I                        !- go across
          KJ = KDIAG(J)-J               !- is this the term 'above it' ?
          IF (J.NE.1) THEN              !- skip
            LBAR = MAX (L,KDIAG(J-1)-KJ+1)
            flops = flops  + 2*(J-LBAR)
          ENDIF
        ENDDO
        flops = flops + 1               !- this is a squareroot
      enddo

      if (ipr.ge.4) write(*,'(A,F10.3,A,F15.3,A)')
     & 'SPARIN:',N/1.e6,' Mequations', flops/1.e9,' Gflop'


c----------------------- -----------------------
      call get_secs(t0)
      NZEROS=0
      A(1) = SQRT(A(1))

      IPERCENT_OLD = 0

      DO I=2,N                           !- loop the equations
        IPERCENT = nint(100.*REAL(I)/REAL(N))    !=nint(I/(.01*n))
        IF (IPR.ge.5 .and.IPERCENT.NE.IPERCENT_OLD)
     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)

        KI = KDIAG(I)-I                 !- row_length
        L  = KDIAG(I-1)-KI+1            !- start_of_row
        DO J=L,I                        !- go across
          X = A(KI+J)                   !- get this term
          X1=0.
          KJ = KDIAG(J)-J               !- is this the term 'above it' ?
          IF (J.NE.1) THEN              !- skip
            LBAR = MAX (L,KDIAG(J-1)-KJ+1)
            IF (LBAR.NE.J) THEN         !- skip
c do not use BLAS's ddot - better to have an explicit do-loop
#ifdef BLAS1
              X1= ddot (J-LBAR,A(KI+LBAR),1,A(KJ+LBAR),1)
#else
cdir$ unroll 4
              DO K=LBAR,J-1
                X = X - A(KI+K)*A(KJ+K)
              ENDDO
#endif
            ENDIF
          ENDIF
          X = X-X1
          A(KI+J) = X/A(KJ+J)
        ENDDO
        IF (X.LE.0.) THEN      !- oops!
          NZEROS=NZEROS+1     !-- just bumble along the best we can
          A(KI+I) = SQRT(ABS(X))
        ELSE
          A(KI+I) = SQRT(X)
        ENDIF
      ENDDO

      IF (NZEROS.NE.0) THEN
        write (*,'(i8,a,i8,a)')
     &  NZEROS,' of the',N,' diagonal terms are non-positive!'
        CALL MYERROR(2,'Degenerate Equations in SPARIN')
      ENDIF

c--- report Mflops/s ---
      if (ipr.ge.4) then
        call get_secs (t1)           !- time the process
        if (t1.gt.t0) then
          flop_rate = flops / (t1-t0) !- this step
        else
          flop_rate = 0.
        endif
        write(*,'(A,F10.3,A,F10.3,A)')
     & 'SPARIN: in', t1-t0,' secs :=',flop_rate/1.e6, ' Mflops/s'
      endif


      END

C-----------------------------------------------------------------------
      SUBROUTINE SPABAC (A,B,N,KDIAG,IPR)
C
C     This performs the Choleski back-substitution
c     on the variable bandwidth stiffness matrix
C     A() is the symmetric skyline (lower) matrix,
c     KDIAG(1:N) points to the end of each line in A().
C     IPR>=4 gives total flops and flop rate
C
      REAL A(*),B(0:N)          !- B is the RHS and LHS !
      INTEGER KDIAG(*)
      real x,x1, flops ,flops_last
      data flops_last/0/

c 9-11-99 can I pre-compute the recipricals ?
c   - Probably, but no real need since they only cost 2% of the CPU time
c     of SPABAC


c 9-11-99 calculate the number of flops
c     if (ipr.ge.4) then
        flops =2                 !- first and last
        do i=2,n
          KI = KDIAG(I)-I         !- this row
          L  = KDIAG(I-1)-KI+1    !- previous row
          flops = flops + 2 +2 + 4*(I-L) !- (note 2 are divs)
        enddo
c     endif


c--------------------- work down the matrix -------------------------
      call get_secs(t0)
      B(1) = B(1)/A(1)          !- the first is easy :-)
      DO I=2,N
        KI = KDIAG(I)-I         !- this row
        L  = KDIAG(I-1)-KI+1    !- previous row
        X = B(I)                !- get the 'force'  (factor ?)
        X1=0.
        IF (L.NE.I) THEN        !- skip if zero-length (irrelevant!)
#ifdef BLAS1
         X1= ddot (I-L,A(KI+L),1,B(L),1)
#else
cdir$ unroll 4
          DO J=L,(I-1)
            X1 = X1 + A(KI+J)*B(J)  !- accumulate factor
          ENDDO
#endif
        ENDIF
        B(I) = (X-x1)/A(KI+I)          !- store as a 'disp' (well almost)
      ENDDO

c--------------------- work back-up the matrix -------------------------
cdir$ unroll 4
      DO I = N,2, -1
c     DO IT=2,N                 !-  IT is NOT used !!
c       I = N+2-IT              !- 'freedom to end at' = N-->2
        KI = KDIAG(I)-I         !- 'start' of this row ?
        X  = B(I)/A(KI+I)       !- extract, factor and hold_in_X
        B(I) = X
        L = KDIAG(I-1)-KI+1     !- start of the previous row ?
        IF (L.NE.I) THEN        !- skip if zero-length (irrelevant!)
c try daxpy ? - no! seems to be much slower than doing a do-loop.
#ifdef BLAS1
          call daxpy (I-L,-X,A(KI+L),1,B(L),1)
#else
cdir$ unroll 4
          DO K=L,(I-1)            !- loop this row
            B(K) = B(K) - X*A(KI+K)     !- note X is B(I) saved
          ENDDO
#endif
        ENDIF
      ENDDO
      B(1)=B(1)/A(1)            !- and the first :-) (can do earlier?)

c--- report Mflops/s ---
      if (ipr.ge.4.and. flops.ne.flops_last) then
        flops_last=flops
        call get_secs (t1)           !- time the process
        flop_rate=0.
        if (t1.gt.t0) flop_rate = flops / (t1-t0) !- this step
        write(*,'(A,F10.3,A,F8.3,A,F8.3,A)')
     &  'SPABAC:',flops/1.e6,' Mflop in'
     &  ,t1-t0,' secs := ',  flop_rate/1.e6, 'Mflops/s'
      endif

      END

c-----------------------------------------------------------------------
      SUBROUTINE LINMLS (KV,DISPS,LOADS,N,KDIAG)
C
C     This forms the product of a matrix and a vector
c     where the matrix is stored in a skyline vector (Sparin)
C
      REAL KV(*),LOADS(*),DISPS(*)
      INTEGER KDIAG(*)

      DO I=1,N
        LUP=KDIAG(I)
        IF (I.EQ.1) LOW = LUP
        IF (I.NE.1) LOW = KDIAG(I-1)+1
        X = 0.
        DO J=LOW,LUP
          X = X + KV(J)*DISPS(I+J-LUP)
        ENDDO
        LOADS(I) = X
        IF (I.NE.1) THEN         !- first row is only a diagonal term
          LUP = LUP-1
          DO J=LOW,LUP
            K = I+J-LUP-1        !(could use IBASE=I-LUP-1)
            LOADS(K) = LOADS(K)+KV(J)*DISPS(I)
          ENDDO
        ENDIF
      ENDDO
      END

C-----------------------------------------------------------------------
C---------------------- PCG solving routines ---------------------------
c... currently these handle Conjugent gradients
c... without a pre-conditioner
c     there are 3 entry-points :
c       1/ FORM_PCG_KV     to store the Stif. matrix for each element
c       2/ FORM_PCG_KDIAG  to store the G-vector for each element
c       3/ SOLVE_CG        to execute the solver
c
c   Dan Kidger 1993    (SOLVE_CG Algorithm from Griffiths & Smith book)
C-----------------------------------------------------------------------

      subroutine print_pcg_structs(kv,ikv,ir, n,kdiag, ipr)
!
!   This routine dumps the contents of the KDIAG structure
!   This for debug
!    Dan Kidger 16-4-15
!
!    Notes: here we build a F90 struct.
!     This is a template for teh next revision where we will use this struct
!     in place of kdiag(3) etc. which is very ugly and error prone
!

      IMPLICIT NONE
      integer :: i,j, iel, ic, idof, ibase
      INTEGER IKV,IR,N,IPR !- size of KV/size of vectors/ (output #iters)
      REAL        KV (IKV)       !- all the matrices + procond.
      real   :: big_spring
      INTEGER  KDIAG (*)         !- store of 'G' vectors + extra info.

      TYPE:: pcg_struct
        integer :: cookie    ! = 12345 ??
        integer :: end       ! current end point in KDIAG
        integer :: endkv     ! current end point in KV
        integer :: istore    ! 1=full, 2=upper tri storage
        integer :: nel       !  # of KM (G's) that we have 
        integer :: istart    ! start location of teh G()s in KDIAG
        integer :: iprecon   !  >0 if a preconditioner
        integer :: ipre_1    ! pointer to the preconditioner in KV()
        integer :: n_bs      ! # of big_springs (for presc. disps.)
        integer :: ibs_start ! start of the list of big spring freedoms
        integer :: bigspring ! where the big spring value is in KV()
      end type 
      type(pcg_struct) :: pcg
        
! note that some of these are teh real start, others ar eteh base so +1,,+2 etc for content
      pcg%cookie  = KDIAG(1)      ! cookie
      pcg%end      =KDIAG(2)      ! current end point in KDIAG
      pcg%endkv    =KDIAG(3)      ! current end point in KV
      pcg%ISTORE  = KDIAG(4)      !- 1=full, 2=upper tri storage
      pcg%NEL     = KDIAG(5)      !- # of KM (G's) that we have
      pcg%ISTART  = KDIAG(6)      !- start loc of the G()'s - working pointer
      pcg%IPRECON = KDIAG(8)      ! >0 if a preconditioner
      pcg%IPRE_1   = KDIAG(9)      ! pointer to the start of the  preconditioner in KV()
      pcg%N_BS     = KDIAG(11)     ! # of big_springs (for presc. disps.)
      pcg%IBS_START = KDIAG(12)   !- start of the list of big spring freedoms
      pcg%bigspring = KDIAG(13)   !- position of the big_spring value in KV

      BIG_SPRING = KV (pcg%bigspring) !- value of the BIG_SPRING

      print*,'Structure of the KDIAG list:' 
      print*,'cookie                     =',pcg%cookie
      print*,'current end point of KDIAG =',pcg%end
      print*,'current end point of KV    =',pcg%endkv,  'ir=',ir
      print*,'storage type (1=sq,2=tri)  =',pcg%istore
      print*,'No of elems stored in KDIAG=',pcg%nel
      print*,'start loc of Gs in KDIAG   =',pcg%istart,
     &   ' val=(idof)=',kdiag(pcg%istart)

      print*,'preconditioner (1=diiag)   =',pcg%iprecon
      print*,'ptr to the precon in KV    =',pcg%ipre_1
      if (pcg%iprecon==1) then
        print*,'preconditioner (diagonal    ='
        write (*,'(12g12.3)') (kv(pcg%ipre_1+i),i=1,n) 
      endif
    
      print*,'No. of big springs         =',pcg%n_bs
      print*,'ptr to the bigspring in KV =',pcg%bigspring
     &      ,' val=', kv(pcg%bigspring)
      print*,'ptr to bs freedomsin KDIAG =',pcg%ibs_start
     &      ,' vals=',(kdiag(pcg%ibs_start+i),i=1,pcg%n_bs)

      print*,' '
      print*,'g vectors'
      ic=pcg%istart
      do iel=1,pcg%nel
        IDOF  = KDIAG(IC)
        IBASE = KDIAG(IC+1)
         write(*,'(i6,i3,i6, 60i4)') 
     &     iel,idof, ibase,(kdiag(ic+1+j),j=1,idof)
        IC = IC + IDOF + 2        !- KDIAG pointer
      enddo
      end subroutine print_pcg_structs

C-----------------------------------------------------------------------
      SUBROUTINE FORM_PCG_KV (KV,IKV, KM,IKM,IDOF,IC, itype)
C
C     This stores complete KM's into KV using IC as a pointer
C     Stored from IC+1 onwards (IC is returned as the 'new' endpoint)
C     ITYPE = 1 for full KM, = 2 for upper-tri only.
C
      IMPLICIT NONE
      INTEGER IKV,IKM,IDOF,IC,ITYPE
      REAL KV(IKV), KM(IKM,IKM)
      INTEGER I,J                   !(locals)

      IF (ITYPE.EQ.1) THEN     !--- full KM storage ----
        IF (IC+IDOF**2.GE.IKV) CALL MYERROR (3,'No more room in KV')
        DO J=1,IDOF
          DO I=1,IDOF
            IC = IC + 1
            KV(IC) = KM(I,J)    !- be careful about the order (tho' symetric)
          ENDDO
        ENDDO

      ELSEIF (ITYPE.EQ.2) THEN     !--- upper-triangle KM storage ----
        DO I=1,IDOF
          DO J=I,IDOF              !(only from the diagonal across)
            IC = IC + 1
            KV(IC) = KM(I,J)    !- be careful about the order (tho' symetric)
          ENDDO
        ENDDO

      ELSE
        CALL MYERROR (3,'Unknown full/upper-tri CG storage method')
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE FORM_PCG_KDIAG (KDIAG,IKDIAG,G,IDOF,IC,IR1)
C
C     This stores IDOF and the G vector in KDIAG -so PCG loop is 'tight'
C     IC = storage point in KDIAG
C     IR1 = cross-reference to where its KM is stored in KV
C
      INTEGER KDIAG (ikdiag), G(idof)

      IF (IC+IDOF+2.GE.IKDIAG) CALL MYERROR (3,'No more room in KDIAG')

      IC = IC + 1
      KDIAG (IC) = IDOF              !- IDOF (record is IDOF+2 long)
      IC = IC + 1
      KDIAG (IC) = IR1 +1            !- 12-3-95 pointer to this element
      DO I=1,IDOF
        IC = IC + 1
        KDIAG(IC) = G(I)             !- the G vector
      ENDDO                          ! (ie pointers to the global freedoms)
      END

C-----------------------------------------------------------------------
      SUBROUTINE FORM_PRECON_DIAG (KV,IKV,KDIAG,IKDIAG, N,BIG_SPRING)
!
!     This finds the diagonal of the Global stiffness matrix
!     and stores the reciprical of this in KV after the KM's
!     for use as a Proconditioner.
!       .. works for both full KM and upper-tri versions.
!       .. Note the big-springs are stored *after* this  - really?
!          (should it be before so the Precon uses the big_springs itself?)
!
      REAL    :: KV (IKV)
      INTEGER :: KDIAG(IKDIAG),N
      real    :: kv_max
      real    :: big_spring
      integer :: nzeros

c.. hmm nice to have a 'magic' cookie for PCG so we abort if we are given
c.. the wrong matrix to play with.
      ITYPE    = KDIAG (4)           !- 1 if full KM, 2 if upper tri
      NEL_USED = KDIAG (5)           !- # of G's that we have
      IR2      = KDIAG (6)           !- start loc of the G()'s
      IPRE_1   = KDIAG (3)           !- base-pointer to the preconditioner in KV()
c                                       (ie after the current end of KV)
!     write(*,'(10i6)') kdiag(1:20+24)
c------------------ null the diagonal ----------------
      DO I=1,N                       !- (hold N in KDIAG too?)
        KV (IPRE_1+ I) = 0.
      ENDDO

c---------------------------- build the diagonal -----------------------
      DO IEL = 1, NEL_USED
        IDOF = KDIAG(IR2)
        IR1  = KDIAG(IR2+1)             !- get KM-startpoint from KDIAG

        ipos = 1
        IF (ITYPE.EQ.1) THEN          !-- full IDOFxIDOF storage
          DO J=1,IDOF
            I = KDIAG(IR2+1 +J)           !- freedom #
c ... 10-8-97 hey - what if I=0 ?? - kills the end of KM's :-(
c .. so do we need a bit-bucket = PRECON(0) ?
            val = KV (IR1-1 +ipos)
            IF (i.gt.0)
     &      KV (IPRE_1+I) = KV (IPRE_1+I) + val
            ipos = ipos + (IDOF+1)        ! so +61,+61,+61,.. (=diagonal)
          ENDDO

        ELSEIF (ITYPE.EQ.2) THEN      !-- upper triangle storage
          DO J=1,IDOF
            I = KDIAG(IR2+1 +J)           !- freedom #
            IF (i.gt.0)                   ! fixed 21-2-98 and (.ge.)6-8-98
     &      KV (IPRE_1+I) = KV (IPRE_1+I) +  KV (IR1-1 +ipos)
            ipos = ipos + (IDOF+1-J)      !- so +60, +59, +58,...
          ENDDO
        ELSE
        call myerror(3,'KV storage format is not 1 (NxN) or 2 (diag)')
        ENDIF
        IR2 = IR2 + IDOF + 2            !- update KDIAG pointer to next elem
      ENDDO

c---------- MPI: exchange values across boundaries ------------
c--1: send
c Loop boundaries.
c   Loop nodes,
c     loop freedoms, assemble vector, then send
c   enddo
c enddo
c--2: receive
c Loop boundaries.
c   receive the 'next' message.
c   Loop nodes,
c     loop freedoms, pick-up value, then add into KV_precon
c   enddo
c enddo

!     write(*,'(a,(10g12.3))') 'before BS', (KV(IPRE_1+J),j=1,n)
!
!     Add in the big_spring values
!
      n_bs      = kdiag(11)         ! No. of Big Springs
      ibs_start = kdiag(12)         ! where these freedoms are stored
      do i=1,n_bs
        j = kdiag(ibs_start+i)               ! freedom number
        KV(IPRE_1+J) = BIG_SPRING
      enddo 
!     write(*,'(a,(10g12.3))') 'after BS', (KV(IPRE_1+J),j=1,n)

!--------- invert the diagonal --------
      nzeros=0
      kv_max=0.
      DO I=1,N
        val = KV(IPRE_1+I)
        kv_max=max(kv_max,val)
        if (val.eq.0) then
           nzeros = nzeros+1
        else
           KV (IPRE_1+ I) = 1. / val
        endif
      ENDDO
      print*,'  * Largest value found on diagonal of KV =', kv_max
      if (nzeros>0) then
         print*,'*** ', nzeros,'/',n,
     & ' zero elements found on the diagonal'
         call myerror(3,'Zero diagonal terms found in KV (PRECON)')
      endif

c----- update KDIAG info to match -----
      KDIAG(8) = 1            !- we now have a diagonal preconditioner
      KDIAG(9) = IPRE_1       !- & here is where it starts
c     KDIAG(2) = IPRE_1+N     !- new end-point of KDIAG
      KDIAG(3) = KDIAG(3)+1   !- new end-point of KV()  <KDIAG is unaffected>
c     KDIAG(10)=..            !- store PRECON length too?
      END

!----------------------------------------------------------------------
      SUBROUTINE solve_conjgrad ( KV,IKV,IR, B,X,N, KDIAG, ITERS, IPR)
!
!     This solves the equations by using the Conjugate Gradient Method
!                      KV x = B
!     19/04/15 rewritten from SOLVE_CG()  to be much cleaner
!
      IMPLICIT NONE
!
!    passed arguments
!
      INTEGER,intent(in)  :: KDIAG (*)         !- store of 'G' vectors + extra info.
      INTEGER,INTENT(IN)  :: IKV,IR,N,IPR !- size of KV/size of vectors/ (output #iters)
      INTEGER,INTENT(OUT) :: ITERS
      REAL,INTENT(IN)     :: KV (IKV)       !- all the matrices + procond.
      REAL,INTENT(INOUT)  :: B (0:N)       !- the input forces   GET MODIFIED
      REAL,INTENT(INOUT)  :: X (0:N)       !- the output disps (=inital guess on input)
!
!  Workspace
!
      real    ::  XNEW(0:n),P(0:n),U(0:n),D(0:n) !- use automatic arrays
!     REAL    :: R(0:n)    ! unused: we now use B (=loads()) as this workspace
      real    :: up,up2,down, alpha,beta
      REAL    :: BIG_SPRING

      integer :: i,j,k, iel, ic, idof, ibase
      real    :: zero=0.

! converrgence checking
      real    :: tol,big,biggest
      integer :: ilevel,ireport, iters_old, max_iters
      LOGICAL CONVERGED, report_flops


! timers
      real :: flops, flops_mv, flops_vv,flops_setup,
     & flop_rate=0., flop_ratet=0.,
     & tol_sofar, t0,t0a,t1a,t1,t_old


      TYPE:: pcg_struct
        integer :: cookie    ! = 12345 ??
        integer :: end       ! current end point in KDIAG
        integer :: endkv     ! current end point in KV
        integer :: istore    ! 1=full, 2=upper tri storage
        integer :: nel       !  # of KM (G's) that we have
        integer :: istart    ! start location of teh G()s in KDIAG
        integer :: iprecon   !  >0 if a preconditioner
        integer :: ipre_1    ! pointer to the preconditioner in KV()
        integer :: n_bs      ! # of big_springs (for presc. disps.)
        integer :: ibs_start ! start of the list of big spring freedoms
        integer :: bigspring ! where the big spring value is in KV()
      end type
      type(pcg_struct) :: pcg

! note that some of these are the real start, others are the base so +1,,+2 etc for content
      pcg%cookie  = KDIAG(1)      ! cookie
      pcg%end      =KDIAG(2)      ! current end point in KDIAG
      pcg%endkv    =KDIAG(3)      ! current end point in KV
      pcg%ISTORE  = KDIAG(4)      !- 1=full, 2=upper tri storage
      pcg%NEL     = KDIAG(5)      !- # of KM (G's) that we have
      pcg%ISTART  = KDIAG(6)      !- start loc of the G()'s - working pointer
      pcg%IPRECON = KDIAG(8)      ! >0 if a preconditioner
      pcg%IPRE_1   = KDIAG(9)      ! pointer to the start of the  preconditioner in KV()
      pcg%N_BS     = KDIAG(11)     ! # of big_springs (for presc. disps.)
      pcg%IBS_START = KDIAG(12)   !- start of the list of big spring freedoms
      pcg%bigspring = KDIAG(13)   !- position of the big_spring value in KV

      BIG_SPRING = KV (pcg%bigspring) !- value of the BIG_SPRING
!
!     Initialastion
!
      call apply_prceon(KV,b,N,D,pcg%iprecon,pcg%ipre_1)
      d(0) = zero
      p = d
!     r(1:n) = loads(1:n)
      x=zero

c     TOL = 10.**(-4)              !- adjust as necessary
      TOL = 10.**(-6)              !- adjust as necessary
      MAX_ITERS = N                !- Theoretical maximum needed  :-)
      MAX_ITERS = nint(MAX_ITERS * 1.5)  !- try this ?

      call get_secs (t0)           !- time the process, and..
      t_old=t0
      iters_old=0

      TOL_SOFAR = 0.1              !- watch current closeness
      ilevel = 0                   !- and how many so far
      ireport= 0

!     how many floating point operations per CG iteration
      CALL GET_PCG_FLOPS
     &   (KDIAG,N,pcg%NEL,flops,flops_mv, flops_vv, flops_setup)   !- for timing
      if (ipr.ge.4) write(*,'(a,a,f12.3,a,f12.3,a)')
     & 'CG solver', ':',n/1.e3, ' Kequations,', flops/1.e6,' mflops/itn'
c    & ' setup=(',flops_setup/1000000.,')'

!
!------------------------ loop the iterations --------------------------
!
      DO ITERS = 1 ,MAX_ITERS
!       print*,'iters=',iters, ' /',max_iters

      call get_secs(t0a)            !- time before matrix:vector
      u  =zero
      CALL CG_MATVEC_EBE (KV, P, U, N, KDIAG, 1,pcg%NEL)  !U =KV * P
!----- apply the BIG_SPRINGS ------
      DO I=1,pcg%N_BS                   !(=0 if no Big springs)
        K = KDIAG (pcg%IBS_START+I)
        U(K) = U(K) + BIG_SPRING * P(K)
      ENDDO
!     print*,'b=',b
!     print*,'d=',d
!     print*,'p=',p
!     print*,'u=',u
!     print*,'x=',x
!     print*,'up,down,up2,alpha,beta=', up,down,up2,alpha,beta

      up   = dot_product(b,d)
      down = dot_product(p,u)
      IF (ABS(DOWN).LT.1.E-30)
     &CALL MYERROR (2,'down=0. : Degenerate loads in Conj Grad Solver')

      alpha= up/down
      xnew  = x + alpha * p
      b     = b - alpha * u

      call apply_prceon(KV,b,N,D,pcg%iprecon,pcg%ipre_1)
      up2    = dot_product(b,d)
      beta   = up2/up
      p      = d + beta * p
      CALL CHECON3 (XNEW(1) ,X(1),N,BIG,BIGGEST)
      x = xnew
      CONVERGED = (BIGGEST.LT.TOL)         !- convergence

!
!     report Mflops and when we pass each convergence level.
!
      if (ipr.ge.4.and. biggest.lt.tol_sofar) then   ! report flop rate
        ireport=ireport+1
        ilevel = ilevel +1
        TOL_SOFAR = tol_sofar/10.    !- next one to check
        call get_secs (t1)           !- time the process
        if (t1.gt.t0) then
          flop_rate = (iters-iters_old)*flops / (t1-t_old) !- this step
          flop_ratet = iters*flops / (t1-t0)  !- overall
          t_old=t1
          iters_old=iters
        else
          flop_rate = 0.
          flop_ratet = 0.
        endif
      if (ireport.eq.1)     !- column headers --
     &write(*,'(a)')
     &'iters, log_big,%n, flops (MatVec, VecVec, mean, overall)'
      write (*,'(i6,f8.2,f5.1,a,2f9.1,2f9.1,f9.3)')
     &  iters, log10(biggest), iters/real(n)*100,'%',
     & flops_mv/max(1.e-12,(t1a-t0a))/1.e6,
     & flops_vv/max(1.e-12,(t1-t1a))/1.e6,
     & flop_rate/1.e6, flop_ratet/1.e6
     & ,t1-t0    !- timestamp
      endif


      if (ipr>=4)
     &write(*,'(i5,8g13.3)') iters, up,down,up2,alpha,beta, 
     &    big,biggest
        B(0) = 0.          !-  make sure this is zero ? why?
        IF (.not.converged) CYCLE
        x = xnew           ! copy final answer from workspace
        return
      ENDDO                              !- loop iterations
      CALL MYERROR (3,'Conjugant Gradient Method failed to converge')

      END SUBROUTINE solve_conjgrad

C-----------------------------------------------------------------------
      SUBROUTINE SOLVE_CG ( KV,IKV,IR, B,X,N, KDIAG, ITERS, IPR)
C
C     This solves the equations by using the Conjugate Gradient Method
C                      KV x = B
C
C     22-6-94 : removed NEL.. mow use loop IR2 until IR instead
C     12-3-95 : IR2 embedded in KDIAG hence support for (now IC)
C     15-3-95 : NEL restored (abstracted form KDIAG's 'header')
C     18-3-95 : support for a Preconditioner added (eg 'D' vector)
C     23-8-95 : order reworked so only one KMxV and 2 precon's
c     26-9-99 : count Mflops and report convergence rate
c
c-----------------------------------------------------------------------
C  Notes:
C   1/ For big-springs we overload some terms in KV
c      This is *INVALID* for the LEGO algorithm!
C      (need to store in KV somewhere else as 1 BIG-SPR plus NNODES pointers)
C   2/ No preconditioner yet supported (a KDIAG pointer ?)
C   3/ should vectorise very well .. just make sure that in SUB_BIT
C      the compiler doesn't kill vectorisation because of the indirect
C      addressing (only the bit-bucket is a 'conflict')
C   4/  Structures
C       KV () holds: 1) sets of KM matrices
C                    2) the preconditioner
C                    3) the 'big-spring' nodes ?
C       KDIAG (1-19) 1) holds information about the set of equations:
c                 *     <1> Magic cookie ?  (maybe at KDIAG(0) ??)
c                   /   <2>'current' end of KDIAG
c                   \   <3>'current' end of KV
C       KM's     /      <4> KM Storage type 1=full, 2=upper tri, (0=none!)
C                |      <5> NEL = # of matrices
c                |      <6> IKM_1 = loc. in KDIAG() for first elem (=20)
c                \      <7>          (length too?)
C     Precond.      /   <8> IPRECON = 0 for no P,1 = diagonal P, etc.
C                   |   <9> IPRE_1 = start loc. in KV for the precon.
C                   \  <10>         (length too? .. if diag, then ==N)
C    Big Springs  /    <11> NBIG_SPR  = # of big-spring freedoms
C                 |    <12> IBIGSPR_1  = start loc in KDIAG() of the spring freedoms
C                 \    <13>  position in KV of the BIG_SPRING  (length too? )
c  .. also need current end-points of both KDIAG and KV (for building)
c  .. also flag for full/upper-tri storage
c
C                    2) A structure for each element:
C                       <1> IDOF = size of each KM (eg. 60 for 20nb)
C                       <2> ILOC = start loc. of KM in KV()
C                       <3> G(1:IDOF) = steering vector for RHS's
C
C                    3) The set of (NBIG_SPR) freedoms for the big springs
C                    4) .. extra info for pre-conditioners ?
C

c       IPR = 1 for total # iters print-out, = 2 for progress as well
c             0 for silent
C-----------------------------------------------------------------------
c.. note IR is the used length of KV, = KDIAG(2) I think
      IMPLICIT NONE
      INTEGER IKV,IR,N,ITERS,IPR !- size of KV/size of vectors/ (output #iters)
      REAL        KV (IKV)       !- all the matrices + procond.
     +            ,B (0:N)       !- the input forces
     +            ,X (0:N)       !- the output disps (=inital guess on input)
      INTEGER  KDIAG (*)         !- store of 'G' vectors + extra info.

C------------------------ Workspace arrays -----------------------------
c I can allocate them in Fortran 90, also in most F77s I can create automatic arrays also.
c#ifdef F90
c     real, allocatable :: XNEW(:),P(:),R(:),U(:),D(:)   !- dont need this ?
c     real XNEW(n),P(n),R(n),U(n),D(n) !- use automatic arrays
c#else
c... be VERY careful that we don't get an ENORMOUS Object File from this.
c     INTEGER    MAX_N          !   MAX_N = SIZE (X) in F90
      integer iworkspace
c     parameter (iworkspace=20000)
c     PARAMETER (IWORKSPACE= 25000)  !-- for the workspace arrays (eg.19816)
c     PARAMETER (IWORKSPACE= 125000)  !-- (this gives 1Mb to each :-)
c     PARAMETER (IWORKSPACE= 350000)  !-- (enough for 30x30x30 20nbs)
      PARAMETER (IWORKSPACE= 1 060 000)  !-- (enough for 44x44x44 20nbs)
c     integer:: iworkspace=n
      REAL XNEW (0:IWORKSPACE)       !- 'new' solution  (cf 'X')
     &       ,P (0:IWORKSPACE)       !- working vector   (force?)
     &       ,R (0:IWORKSPACE)       !- working vector   (residual?)
     &       ,U (0:IWORKSPACE)       !- temp work space for (KMxP)
     &       ,D (0:IWORKSPACE)       !- [Precond]*R  (==R if no precon)
c#endif
      LOGICAL CONVERGED, report_flops
      INTEGER :: IDOF,I,K,IEL,IC,NEL,IPRECON,N_BS,IBS_START=0
     &      ,MAX_ITERS,IPRE_1, ISTORE,ISTART     !- locals
      REAL TOL,BIG,BIGGEST, ALPHA,BETA, UP,UP2,DOWN, BIG_SPRING

      character string*79, solver*7
      real :: flops, flops_mv, flops_vv,flops_setup, 
     &        flop_rate=0., flop_ratet=0.,
     &        tol_sofar, t0,t0a,t1a,t1,t_old
      integer ilevel,ireport,iters_old

!      print*,'debug: entered SOLVE_CG'
! if debug
      print*,'solve_cg,ipr=',ipr
      if (ipr.ge.2)
     &call print_pcg_structs(kv,ikv,ir, n,kdiag, ipr)

      P(0) = 0.          !-  make sure these are zero too ?
      U(0) = 0.          !  ( only need to do P(0) before mat_mul
      R(0) = 0.          !      and U(0) after .. rest are never used )
      D(0) = 0.
      B(0) = 0.
      X(0) = 0.          !- good to zero this on exit
      XNEW(0) = 0.

c---------------------- Check array sizes ------------------------------
      IF (N.GT.IWORKSPACE) THEN      !<only needed for F77, F90 ALLOCATE's
        PRINT*,'oops: n=',n,' iworkspace=',iworkspace
        CALL MYERROR (3,'Not enough workspace in SOLVE_CG')
      ENDIF

c----- set up the initial guess ----
c... should do outside .. if not use a dummy unit vector ?
c      CALL NULVEC (X,N)
c      X (1) = 1.

c-------------------- Abstract the storage info ------------------------
      ISTORE  = KDIAG(4)      !- 1=full, 2=upper tri storage
      NEL     = KDIAG(5)      !- # of KM (G's) that we have
      ISTART  = KDIAG(6)      !- start loc of the G()'s - working pointer
      IPRECON = KDIAG(8)      ! >0 if a preconditioner
      IF (IPRECON.gt.0)
     &IPRE_1  = KDIAG(9)      ! pointer to the preconditioner in KV()
      N_BS    = KDIAG(11)     ! # of big_springs (for presc. disps.)
!**FIXED 16-4-15 next line was (11) !!  which =1 not a far location in kdiag
      IF (N_BS.gt.0) THEN
        IBS_START = KDIAG(12)       !- start of the list of big spring freedoms
        BIG_SPRING = KV (KDIAG(13)) !- value of the BIG_SPRING
      ENDIF

c------------------------- set up the system ---------------------------
c.. arrgh! here we must do R = B - [KM] X
C (so if X is correct first-time R=0 !)
c-----------------------------------------------------------------------
      SOLVER='PCG-sq'
      IF (istore.eq.6) SOLVER='PCG-tri'
      CALL GET_PCG_FLOPS (KDIAG,N,NEL, flops,
     & flops_mv, flops_vv, flops_setup)   !- for timing
      if (ipr.ge.4) then
        write(*,'(a,a,f12.3,a,f12.3,a)')
     &  solver,':',n/1.e3, ' Kequations,', flops/1.e6,' mflops/itn'
c    &   ' setup=(',flops_setup/1000000.,')'
      endif

c.. multiply the (EBE) matrix by X to give U.
      write(*,'(a,10G12.3)')'before:x,u=',x(0:N),U(0:N)
      CALL CG_MATVEC_EBE (KV, X, U, N, KDIAG, 1,NEL)
      write(*,'(a,(10G12.3))')'after:x,u=',x(0:N),u(0:N)

c.. recieve the parts of U from other processors and sum them

c----- apply the BIG_SPRINGS ------
      DO I=1,N_BS   !(=0 if no Big springs)
        K = KDIAG (IBS_START+I)
        U(K) = U(K) + BIG_SPRING * X(K)
      ENDDO
!     write(*,'(a,10G12.3)')'after bs:x,u=',x(0:N),U(0:N)

c---------- MPI: exchange U() across boundaries ------------
c   send: the values of U() on the boundary
c   receive: values and add into U()
c Note here: we want to send the messages as soon as possible and
c   receive as late as possible.
c So: can start a send as soon as we have finshed its nodes
c   so perhaps for each message store (flag) the highest numbered
c     element, hence dispatch the message as soon as we hit it.
c For shared memory,  we must know the address that the other processor
c calls our nodes, hence block the += our values.
c (cf one-sided communication)

c-----------------------------------------------
!     write(*,'(a,(10G12.3))')'b=',b(0:N)
      DO I=1,N
        R(I) = B(I)  -U(I)               !- put 'loads' B into R=residual
      ENDDO                              ! (B is not used again)
!     write(*,'(a,(10G12.3))')'r=',r(0:N)

      call apply_prceon(KV,R,N,D,iprecon,ipre_1)
!     write(*,'(a,(10G12.3))')'d,r=',d(0:N),r(0:N)
c----------- need 2 copies: (one gets bigger, the other smaller ?)
c.. compute UP=R.D here too?
      DO I=1,N
        P(I) = D(I)
      ENDDO
!     write(*,'(a,(10G12.3))')'p,d=',p(0:N),d(0:N)

c------------------------ loop the iterations --------------------------
c     TOL = 10.**(-4)              !- adjust as necessary
      TOL = 10.**(-6)              !- adjust as necessary
      MAX_ITERS = N                !- Theoretical maximum needed  :-)
      MAX_ITERS = nint(MAX_ITERS * 1.5)  !- try this ?

      call get_secs (t0)           !- time the process, and..
      t_old=t0
      iters_old=0
      TOL_SOFAR = 0.1              !- watch current closeness
      ilevel = 0                   !- and how many so far
      ireport= 0
      string=" "

      DO ITERS = 1 ,MAX_ITERS
c       print*,'iters=',iters
      call get_secs(t0a)            !- time before matrix:vector
      CALL CG_MATVEC_EBE (KV, P, U, N, KDIAG, 1,NEL)  !U =KV * P
!     write(*,'(a,(10G12.3))')'p,u=',p(0:N),U(0:N)

c.. recieve the parts of U from other processors and sum them

c----- apply the BIG_SPRINGS ------
      DO I=1,N_BS   !(=0 if no Big springs)
        K = KDIAG (IBS_START+I)
        U(K) = U(K) + BIG_SPRING * P(K)
      ENDDO
!     write(*,'(a,(10G12.3))')'u,p=',u(0:N),p(0:N)

      call get_secs(t1a)            !- time after matrix:vector

c---------- MPI: exchange U() across boundaries ------------
c     CALL MPI_REDUCE (U,U,N,MPI_REAL,MPI_SUM,0,MPI_COMM_WORLD)
c-----------------------------------------------------------------------
#ifdef BLAS1
      up =ddot(n,r,1,d,1)
      down=ddot(n,p,1,u,1)
#else
      UP   = 0.
      DOWN = 0.
c$OMP PARALLEL DO
cdir$ unroll 2
      DO I=1,N
        UP   = UP   + R(I)*D(I)  !(these haven't changed since last iteration)
        DOWN = DOWN + P(I)*U(I)  ! hah=h.b
      ENDDO
#endif
c-- if MPI: global sum UP and DOWN ----
c     best to meld into an arry of two numbers
c     call MPI_reduce_all (up,MPI_REAL,1, 2, MPI_COMM_WORLD) !- fix me

      IF (ABS(DOWN).LT.1.E-30) THEN
        CALL MYERROR (2,'Degenerate loads in Conj Grad Solver')
      ENDIF

c-- all PEs can do the next
      ALPHA = UP/DOWN

c  x=x+ro*h ; g=g-ro*b
#ifdef BLAS1
c-- really it shouldn't be 1:n but rather ifrom:ito
      do i=1,n
        xnew(i)=x(i)
      enddo
      call daxpy(N,alpha,P,1,Xnew,1)
      call daxpy(N,-alpha,U,1,R,1)
#else
c$OMP PARALLEL DO
cdir$ unroll 2
      DO I=1,N
        XNEW(I) = X(I) + ALPHA * P(I)        !  X = X + à P
        R(I)    = R(I) - ALPHA * U(I)        !  R = R - à U
      ENDDO
!     write(*,'(a,(10G12.3))')'xnew,r=',xnew(0:N),r(0:N)
#endif

c------------------------- apply preconditioner ------------------------
c.. I do not know why we have to do this second-precon step
c.. Lucquin applies this to U not R
      call apply_prceon(KV,R,N,D,iprecon,ipre_1)
c     IF (IPRECON.eq.0) THEN          !-- no preconditioner --
c       DO I=1,N
c         D(I) = R(I)
c       ENDDO
c     ELSEIF (IPRECON.EQ.1) THEN      !-- diagonal preconditioner --
c       DO I=1,N
c         D(I) = KV (IPRE_1+I) * R(I)
c       ENDDO
c     ELSEIF (IPRECON.EQ.2) THEN      !-- incomplete Choleski, etc. ? --
c     ENDIF

c-----------------------------------------------------------------------
c MPI: need to MPI_ALLREDUCE (up2)
c - where nodes are common to PEs then we either:
c   1: halve their value (or 1/3 if common to 3 PEs)
c   2: only do them on one processor

#ifdef BLAS1
      up2 = ddot (N,R,1,D,1)
#else
      UP2 = 0.
c$OMP PARALLEL DO
cdir$ unroll 2
      DO I=1,N
        UP2 = UP2 + R(I)*D(I)
      ENDDO
#endif
c---- MPI: global sum UP2 ----
c    call MPI_Allreduce (UP2,up2,1,MPI_REAL,'SUM',MPI_COMM_MASTERS)

c.. perhaps I should check that UP/=0. ?
      BETA = UP2/UP

c-- update P --
c#ifdef BLAS1
c#else
c$OMP PARALLEL DO
      DO I=1,N                         !(if PCG  p = (d==) + p*beta
        P(I) = D(I) + BETA * P(I)
      ENDDO
c#endif

c---------------------- convergence check ------------------------------
c using X and XNEW (can do 'earlier'?)
c  MPI: (14-5-97) non-blocking broadcast BIGGEST, hence get the
c       VERY_BIGGEST (maybe do dx,dy,dz,pp seperately?)
c       we re-cycle if VERY_BIGGEST is > TOL
c---- MPI: global reduce the convergence test. ----
c... we want the global norm BIG so globally reduce BIG and BIGGEST ?
c Perhaps even devolve convergence checking to another processor?
c 4-11-98 Lucquin uses ALPHA/ALPHA0 as a criteria

      CALL CHECON3 (XNEW(1) ,X(1),N,BIG,BIGGEST)
      CONVERGED = (BIGGEST.LT.TOL)         !- convergence


c --- report Mflops and when we pass each convergence level.
      report_flops = ipr.ge.4.and. biggest.lt.tol_sofar
c     report_flops = ipr.ge.4.and..true.
      if (report_flops) then
        ireport=ireport+1
        ilevel = ilevel +1
        TOL_SOFAR = tol_sofar/10.    !- next one to check
        call get_secs (t1)           !- time the process
        if (t1.gt.t0) then
          flop_rate = (iters-iters_old)*flops / (t1-t_old) !- this step
          flop_ratet = iters*flops / (t1-t0)  !- overall
          t_old=t1
          iters_old=iters
        else
          flop_rate = 0.
          flop_ratet = 0.
        endif
      if (ireport.eq.1)     !- column headers --
     &write(*,'(a)')
     &'iters, log_big,%n, flops (MatVec, VecVec, mean, overall)'
      write (*,'(i6,f8.2,f5.1,a,2f9.1,2f9.1,f9.3)')
     &  iters, log10(biggest), iters/real(n)*100,'%',
     & flops_mv/max(1.e-12,(t1a-t0a))/1.e6,
     & flops_vv/max(1.e-12,(t1-t1a))/1.e6,
     & flop_rate/1.e6, flop_ratet/1.e6
     & ,t1-t0    !- timestamp
      endif

      IF (IPR.ge.6) then
          WRITE(*,'(I5,A,G18.9,A,F18.9,a,i4,A,A)')
     &      ITERS,': BIG=',BIG,' relative change =',BIGGEST !/TOL
     &     ,' ipr=',ipr,
     &     '   ',char(27)//'[A'
      ENDIF
c$OMP PARALLEL DO
      DO I=1,N
        X(I) = XNEW(I)            !- now update X
      ENDDO
c-----------------------------------------------------------------------
        IF (CONVERGED) GOTO 99             !- exit the loop
      ENDDO                              !- loop iterations
      CALL MYERROR (3,'Conjugant Gradient Method failed to converge')

C--------------------------- Ok converged ------------------------------
   99 CONTINUE
      B(0) = 0.          !-  make sure this is zero ? why?
c     IF (IPR.ge.4) WRITE(*,*) 'total CG Iterations =', ITERS

      RETURN
      END


C-----------------------------------------------------------------------
c   Notes on Storage Formats  4-11-98
c
c   1. As full NxN, or upper-triangle?
c   2. Upper triangle as strips, or DL_POLY rectangle.
c   3. RHS as indirect reference within loop, or pre-copy
c   4. Single RHS (MatxVec) or group with the LEGO algorithm (MatxMat).
c   5.  #4 as pointers rectangle-> single list??
c   6. Parallelisation issues.
c   7. should we skip in the outer loop if G()=0?

C-----------------------------------------------------------------------
      SUBROUTINE CG_MATVEC_EBE (KV, X, U, N, KDIAG,  IEL_FROM,IEL_TO)
c
c     This multiplies the global matrix by the given RHS
c     Matrix is stored EBE.
c        Dan Kidger
c
c     Elements IEL_FROM to IEL_TO are handled (eg 1,NEL)
c     for a parallel version we call this routine with different sets of
c     elements for each processor.

c
c     for efficiency, if we have the lego algorithm, then we ought to
c     assemble several LHS's at a time and so do matrx*matrix. This makes
C     much better use of the cache (ie. more flops per load/store)
c

c-----------------------------------------------------------------------
c  27-7-00 need support for ISTORE.eq.3 (say) for SAPRIN's sparce format, etc.
c.. multiply the (EBE) matrix by P to give U.

      REAL KV(*), X(0:N), U(0:N)
      INTEGER KDIAG(*)

      ISTORE  = KDIAG(4)      !- 1=full, 2=upper tri storage, (3=sparin, etc.)
      ISTART  = KDIAG(6)      !- start loc of the G()'s - working pointer

c.. hmm ISTORE could vary for each element
c (eg. if only some were non-symmetric)

c---- 1: clear the results-vector ----
c.. realy we only need to null the subset that this PE works on.
      DO I=1,N
        U(I) = 0.
      ENDDO

c--- pick up the first element that we are handling ---
      IC = ISTART                  !- start loc of the G()'s
      DO IEL=1,IEL_FROM-1          ! usu. IEL_FROM=1 so this is a no-op
        IDOF = KDIAG(IC)
        IC = IC + IDOF + 2         !- KDIAG pointer
      ENDDO

c---- loop elements and multiply ---
c. 25-9-99 could test if the next matrix is the same and so batch together.
c. ie. pass a set of 1000 G's
c.. each time matrix is same as last: no_els++
c.. if not same as last or if no_els>limit (say 1000 max?)
C   then call sub_bit :-)

      ibase_old=-1
      no_els=1
      DO IEL=IEL_FROM,IEL_TO
        IDOF  = KDIAG(IC)
        IBASE = KDIAG(IC+1)
c       IBASE_NEXT=KDIAG(IC+IDOF+2)  !(not needed?)

c       if (ibase.eq.ibase_old) then
c         no_els = no_els+1
c         do_it = .false.
c         if (no_els>1000) do_it = .true.
c       else
C         no_els=1
c         ibase_old=ibase
c         do_it = .true.
C       endif

c.. fire up sub_bit if..
C   ibase has changed,
c   or if no_els> buffer size (say 1000)
c   or if this is the very last element.

c       if (do_it.or.iel.eq.iel_to) then
        IF (ISTORE.EQ.1) THEN        !-- full IDOFxIDOF storage
            CALL SUB_BIT (KV(IBASE),IDOF, IDOF, KDIAG(IC+2),X,U, N )
        ELSEIF (ISTORE.EQ.2) THEN     !-- upper triangle storage
            CALL SUB_BIT_UPPER (KV(IBASE),IDOF, IDOF
     &                            ,KDIAG(IC+2),X,U, N )
        ENDIF
c       endif  !- multiple elements

        IC = IC + IDOF + 2        !- KDIAG pointer

      ENDDO

      U(0) = 0.                     !- & reset the bit-bucket  :-)

      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE SUB_BIT (KM,IKM,IDOF,G, LHS,RHS,N)
C
C     This multiplies a matrix by a vector (taking freedoms into account)
C     ie. given KM and G into multiples the 'bits' of LOADS by KM
C     so just loop the elements, form KM and G and call this routine :-)
C
C     1/ I could have KM being only the upper triangle to save storage ??
C        but this would probally inhibit any vectorisation
C     2/ Need to remove all IF's for vectorisation !
C
C     3/ A better version of this would be to reverse I and J so we
C        are working through contiguous KM loc's
C
C.. 1-9-99 for t3e, scatter and gather both RHS and LHS, hence inner becomes
C     SGEMV automatically
      IMPLICIT NONE
c     SAVE G1,T,I,J,lhs2,rhs2    !- force local storage of arrays
      save
      INTEGER IKM,IDOF,N                   !- usu. IKM==IDOF
      REAL KM(IKM,IKM), LHS(0:N), RHS(0:N)
      INTEGER G(IDOF), G1
      common /pcg_sub_bit/lhs2,rhs2
      real lhs2(102),rhs2(102)
      REAL T                               !- local variables
      INTEGER I,J,me, ifail

c.. first store a copy of the left hand side: LHS(G(1:idof))   3-11-98
c.. this is not nice for fast-cpu but slow-memory machines like the t3e (?)

c.. try to speed up by quoting the sizes of 8,14 and 20nbs
c.. otherwise just do genericaly

c.. multiple elements at a time ?
c.. simply add a loop around the gather operation 1:no_els
c.. and another around the matvec (to make it matmul)
c   just need to be careful of the fact that each G is padded apart with
c    2 more Integers (IDOF and IBASE=location of the target KM)

c.. cache alignment issues: should I try and pad LHS and RHS to make
c   best use of T3E cache lines? I could even tweak depending upon the
c   base address of KM

c..
c     LHS(0)=0.   !- clear bit bucket here?

!------------- generic case ----
cdir$ ivdep
cdir$ unroll 4
      DO I=1,IDOF
        LHS2(I)=LHS(G(I))
      ENDDO

#define FUJI 13
#if OS==FUJI
c  Fujitsu's SLL library -
      call dmav(km,idof,idof,idof,lhs2,rhs2,ifail)
#elif BLAS2
      call dgemv ('n',idof,idof,1.,KM,idof,lhs2,1,0.,rhs2,1)
#else
cdir$ nopattern
      DO I=1,IDOF
        T = 0.
cdir$ unroll 4
        DO J=1,IDOF
          T = T + KM(J,I) * LHS2(J)
        ENDDO
        rhs2(i)=t
      ENDDO
#endif

ccdir$ ivdep
cdir$ unroll 4
      DO I=1,IDOF       !- copy the vector(s) back again
        G1=G(I)
        RHS(G1) = RHS(G1) + RHS2(I)
      ENDDO

c     ENDIF  !- 8nb/14nb/20nb/generic



      END

C-----------------------------------------------------------------------
      SUBROUTINE SUB_BIT_upper (KM,IKM,IDOF,G, LHS,RHS,N)
C
C     Alternative to SUB_BIT .. here the KM's are stored as an upper
C     triangle only.  (so IKM is not relevant
c     Method is to loop down the real matrix
c       Then loop across, and 'trace'the position of the KM terms
C            DJK 15-8-95
c
c   .. surely there is a method where both inner loops are constant step?
c   In one loop updates a single RHS term; the second updates '60' RH terms.
c   3-11-98 perhaps there is? compare with the DL_POLY method of storing
c   the upper triangle as a rectangle.
C
c   Q: can not both inner loops be fixed stride thru KM?
c
      IMPLICIT NONE
      INTEGER IKM,IDOF,N                   !- usu. IKM==IDOF
      REAL KM(*), LHS(0:N), RHS(0:N)
      INTEGER G(IDOF)
      REAL T                             !- local variables
      INTEGER I,J, IC,G1
      real lhs2(96),rhs2(96)

c.. first store LHS(G(1:idof))   3-11-98
      DO I=1,IDOF          !- gather -
        LHS2(I)=LHS(G(I))
      ENDDO

      DO J=1,IDOF
        T = 0.
        IC = J                        !- start-point along the top-row

cdir$ unroll 8
        DO I=1,J-1                    !- go down the column to the diagonal
          T = T + KM(IC) * LHS2(I)
          IC = IC + (IDOF-I)          !- so 1 less each time
        ENDDO

cdir$ unroll 4
        DO I=J,IDOF                   !- go across this row to the RHS
          T = T + KM(IC) * LHS2(I)    !-- oops was LHS(I) fixed 25-9-99
          IC = IC + 1
        ENDDO

c       G1 = G(J)                     !- where to store this row-sum
c       RHS(G1) = RHS(G1) + T         !- store the result
        RHS2(J) = T          !- 25-9-99 defer global updates
      ENDDO

cdir$ unroll 8
      DO I=1,IDOF       !- copy the vector(s) back again  25-9-99
        G1=G(I)
        RHS(G1) = RHS(G1) + RHS2(I)
      ENDDO

      END

c-----------------------------------------------------------------------
      subroutine apply_prceon (KV,R,N,D,iprecon,ipre_1)
c
c     This applies teh preconditioner to the system of equations in the
c     PCG solver
c     if diagonal preconditioning (default) D= KV_precon * R
c        Dan Kidger : abstracted from SOLVE_CG
c
c.. if a precon.. then up = dot(r,d)  < if no precon d==r >
c     solve precon eqns. for D
      real kv(*), r(0:n), D(0:n)
      integer I,n
      IF (IPRECON.eq.0) THEN        !-- no preconditioner --
        DO I=1,N
          D(I) = R(I)
        ENDDO
      ELSEIF (IPRECON.EQ.1) THEN    !-- diagonal preconditioner --
        DO I=1,N
          D(I) = KV (IPRE_1+I) * R(I)
        ENDDO
      ELSEIF (IPRECON.EQ.2) THEN    !-- incomplete Choleski, etc.? --
      ELSE
        CALL MYERROR (2,'Unknown Preconditioner (SOLVE_CG)')
      ENDIF
      return
      end

C-----------------------------------------------------------------------
      SUBROUTINE GET_PCG_FLOPS (KDIAG,N,NEL, flops,
     &  flops_mv, flops_vv, flops_setup)
C
C     This estimates the number of floating point operations (FLOPS)
c     that are need for each iteration the conjugent gradient solver
C
       INTEGER KDIAG (*)

      ISTORE  = KDIAG(4)      !- 1=full, 2=upper tri storage
      ISTART  = KDIAG(6)      !- start loc of the G()'s - working pointer

c--------- matrix vector bit ----------
      FLOPS_mv = 0.
      IC = ISTART                  !- start loc of the G()'s
      DO IEL=1,NEL
        IDOF = KDIAG(IC)
        !- a multiply and an add for every number in an element matrix
        FLOPS_mv = FLOPS_mv + (2*IDOF*IDOF)
        IC = IC + IDOF + 2         !- KDIAG pointer
      ENDDO

c------ vector:vector operations ------
      FLOPS_VV = 0.
c   these are generaly less
c   be careful of preconditioner : assume diag precon
c ignore big_springs for now (cos unlikely to be significant.)
      flops_vv = flops_vv + N * (1+4+1+2+2+1)
c.. add up/down, etc.
      flops_vv = flops_vv + (1+1)
c.. add converegnce checking
C   (although it can be done in integer arithmatic!)
      flops_vv = flops_vv + (3) * N

c--------- set up costs ----------
c.. for first iteration only: an extra matrix-vector plus 3 v:v
      FLOPS_setup = flops_mv
      FLOPS_setup = flops_Setup + (3) * N

c----- hence total flops ------
      flops = flops_mv + flops_vv
      RETURN
      END

C-----------------------------------------------------------------------



c-----------------------------------------------------------------------
c     3: Gauss full NxN (unbanded) solver
c-----------------------------------------------------------------------
      SUBROUTINE FORM_FULL_KV (KV,IKV,KM,IKM,IDOF,G,N)
c
c     This assembles an Element into the full NxN matrix (held as a vector)
c
      REAL KM(IKM,IKM), KV(IKV)
      INTEGER G(IKM)

      DO J=1,IDOF
        IF (G(J).NE.0) THEN
          DO I=1,IDOF
            IF (G(I).NE.0) THEN
              IPOS = N*(G(I)-1) + G(J)
              KV(IPOS) = KV(IPOS) + KM(I,J)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      END

c-----------------------------------------------------------------------
      SUBROUTINE SOLVE (K,IK,U,F,N)
C
C      This performs Gaussian Elimination with partial pivoting
C      on a full N*N matrix
C      (For global stiffness matrix, pivoting should not be necessary)
C         or even desirable
C
      REAL K(IK,*),F(*),U(*)

C----------- pivoting stage -------------
      DO 1,I=1,N-1
      BIG=ABS(K(I,I))
      IHOLD=I
      DO J=I+1,N                       !- find the biggest # to pivot
        IF(ABS(K(J,I)).GT.BIG)THEN
          BIG=ABS(K(J,I))
          IHOLD=J
        END IF
      ENDDO
      IF (IHOLD.NE.I) THEN             !- if not on the diag, move it
        DO J=I,N
          HOLD       = K(I,J)
          K(I,J)     = K(IHOLD,J)
          K(IHOLD,J) = HOLD
        ENDDO
        HOLD     = F(I)                !- do the RHS too
        F(I)     = F(IHOLD)
        F(IHOLD) = HOLD
      END IF

C------------- elimination stage ----------
      DO J=I+1,N
        FAC=K(J,I)/K(I,I)
        DO L=I,N
          K(J,L)=K(J,L)-K(I,L)*FAC
        ENDDO
        F(J)=F(J)-F(I)*FAC
      ENDDO
    1 CONTINUE

C----------------------- back-substitution stage -----------------------
      DO I=N,1,-1
        SUM = 0.
        DO L=I+1,N
          SUM = SUM + K(I,L)*U(L)
        ENDDO
        U(I) = (F(I)-SUM) /K(I,I)
      ENDDO
      RETURN
      END

c-----------------------------------------------------------------------
c     4: Gauss-banded solver (BANRED)
c-----------------------------------------------------------------------
      SUBROUTINE FORMKV (BK,KM,IKM,G,N,IDOF)
C
C     This forms the global stiffness matrix
C     storing the upper triangle as a vector BK(N*(IW+1))
C      -- I would rather store BK row-wise for efficincy
C
      REAL BK(*),KM(IKM,*)
      INTEGER G(*)
      DO I=1,IDOF
        IF (G(I).gt.0) THEN          !- or just use 'cycle'
        DO J=1,IDOF
          IF (G(J).gt.0) THEN
          ICD = G(J)-G(I)+1
          IF (ICD.gt.0) THEN
            IVAL = N*(ICD-1)+G(I)        ! morph to the storage location
            BK(IVAL) = BK(IVAL)+KM(I,J)  ! and store
          ENDIF
          ENDIF
        ENDDO
        ENDIF
      ENDDO
      END

c-----------------------------------------------------------------------
      SUBROUTINE FORMKU(KU,IKU,KM,IKM,G,IW,IDOF)
C
C      THIS SUBROUTINE ASSEMBLES ELEMENT MATRICES INTO SYMMETRICAL
C      GLOBAL MATRIX(STORED AS AN UPPER RECTANGLE)
C
c      9-3-01 (DJK) added (from fe5lib) for Lamaload Eigenvalues.
c      note that is (almost) identical to FormKV above
c
      REAL KU(IKU,*),KM(IKM,*)
      INTEGER G(*)
      DO I=1,IDOF
        IF(G(I).gt.0) then
          DO J=1,IDOF
            IF(G(J).gt.0) then
              ICD=G(J)-G(I)+1
              IF(ICD.gt.0) then
                KU(G(I),ICD) = KU(G(I),ICD)+KM(I,J)
              endif
            endif
          enddo
        endif
      enddo
      RETURN
      END

c-----------------------------------------------------------------------

      SUBROUTINE BANRED (BK,N,IW,IPR)
C
C     This performs Gaussian Reduction of the stiffness matrix
C     stored as a vector: BK(N*(IW+1))
C     (IMS's FTN4 version .. too complex for me to decifer! -DJK)
C    12-3-01 (DJK) timings and mflops added
      REAL BK(*)
      real flops
      save flops

c.. calc total flops needed ..
      flops = 0
      DO I=2,N                  !- loop rows
        IL1 = I-1
        KBL = MIN(I-1+IW+1,N)
        DO J=I,KBL              !- loop down to edge of band ?
          NKB = MAX(1,J-IW)
          flops = flops + 3*(max(0,il1-nkb+1))  !- loop across (rest of?) row
        ENDDO
      ENDDO
      if (ipr.ge.4) write(*,'(A,F10.3,A,F18.6,A)')
     & 'BANRED:',N/1.e6,' Mequations', flops/1.e9,' Gflop'

!----------------------------------------------
      call get_secs(t0)

      IPERCENT_OLD = 0

      DO I=2,N                           !- loop the equations
        IPERCENT = nint(100.*REAL(I)/REAL(N))    !=nint(I/(.01*n))
        IF (IPR.ge.5 .and.IPERCENT.NE.IPERCENT_OLD)
     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)


        IL1 = I-1
        KBL = IL1+IW+1                !- last row in this 'section'
        KBL = MIN(KBL,N)              !- clip to the bottom of the matrix

        DO J=I,KBL                    !- so loop down the matrix
          IJ=(J-I)*N+I                   !- loc. of this term
          NKB=J-IW
          NKB = MAX(1,NKB)               !- clip to start of row ?
          SUM=BK(IJ)                     !- diagonal term
c         IF (NKB-IL1.gt.0) THEN
            DO M=NKB,IL1                 !-note that sometimes NKB>IL1
              NI=(I-M)*N+M               !-so zero-pass loop
              NJ=(J-M)*N+M
              SUM = SUM-BK(NI)*BK(NJ)/BK(M)
            ENDDO
c         ENDIF
          BK(IJ)=SUM                     !- store factored value.
        ENDDO
      ENDDO
c--- report Mflops/s ---
      if (ipr.ge.4) then
        call get_secs (t1)
        flop_rate=0.
        if (t1.gt.t0) flop_rate = flops / (t1-t0)
        write(*,'(A,F10.3,A,F10.3,A)')
     & 'BANRED: in', t1-t0,' secs :=',flop_rate/1.e6, 'Mflops/s'
      endif


      END

c-----------------------------------------------------------------------
      SUBROUTINE BACSUB (BK,LOADS,N,IW,ipr)
C
C     This performs the Gaussian Back-Substitution (as from BANRED)
C
      REAL BK(*),LOADS(0:n)
      real flops ,flops_last
      data flops_last/0/

      flops =1                                    !- a 
      do i=2,n
        NKB = MAX(1,I-IW)
        flops = flops + 2*(max(0,i-1-nkb+1))       ! + and * (ddot)
      enddo
      DO I=N-1,1,-1
        flops = flops + 2* (MIN(I+IW,N)-(I+1)+1)  ! + and * (ddot)
        flops = flops + 2                         ! - and /
      ENDDO
      call get_secs(t0)

c------------ work down the matrix -------------
c.. note that I would rather 'flip' BK to store row-wise
C.. note also that loads(i) once written is used again on the next pass.
c.. I *can* store into DISPS.. as only (loads(i)-sum) is a 'new' tern
      LOADS(1)=LOADS(1)/BK(1)     !- the first term is easy
      DO I=2,N                    ! loop freedoms
        NKB = MAX(1,I-IW)         !  'top' freedom of this column
        SUM = 0.
        DO K=NKB,I-1              !-- loop across the banded matrix
          SUM = SUM + BK((I-K)*N+K) * LOADS(K)    !- dot_product
        ENDDO
        LOADS(I) = (LOADS(I)-SUM) / BK(I)
      ENDDO

c----------- work back-up the matrix --------------
      DO I=N-1,1,-1
        SUM = 0.
        DO K=I+1,MIN(I+IW,N)      !- clip to the bandwidth
          SUM = SUM + BK((K-I)*N+I) * LOADS(K)     !- simple dot_product
        ENDDO
        LOADS(I) = LOADS(I) - SUM / BK(I)
      ENDDO
c--- report Mflops/s ---
      if (ipr.ge.4.and. flops.ne.flops_last) then
        flops_last=flops
        call get_secs (t1)           !- time the process
        flop_rate=0.
        if (t1.gt.t0) flop_rate = flops / (t1-t0) !- this step
        write(*,'(A,F10.3,A,F8.3,A,F8.3,A)')
     &  'BACSUB:',flops/1.e6,' Mflop in'
     &  ,t1-t0,' secs := ',  flop_rate/1.e6, 'Mflops/s'
      endif

      END

c-----------------------------------------------------------------------
         SUBROUTINE BANDRD(N,IW,A,IA,D,E,IPR)
!   This subroutine transforms a real symmetric band matrix a, of order n
!   and band width iw, to tridiagonal form by an appropriate Sequence of
!   Jacobi rotations. During the transformation the property of the band
!   matrix is maintained.
!   The method yields a tridiagonal matrix, the diagonal elements of which
!   are in D(n) and off-diagonal elements in E(n).
!
!     9-3-01 (DJK) added (from fe5lib) for Lamaload Eigenvalues.
!    12-3-01 (DJK) removed E2 as it is unused
!    13-3-01 (DJK) added and fixed flops estimate.

      INTEGER M, IW, N2, N, K, MAXR, IRR, IR, KR, J, JM, IUGL, J2,
     + L, JL, MAXL, I, IA
      REAL B, S, C, C2, S2, CS, U, U1, A(IA,*), D(*), E(*)
      real flops
      real :: g=0.    !- to avoid uninitialised gfortran warnings

c.. calc total flops needed ..
      flops = 0
      do k=1,n-2           !(160)               !- loop rows of matrix  ! O(N)
        MAXR = IW
        IF (MAXR.GE.N-K) MAXR = N - K
        DO IRR=2,MAXR      !(140)               !- loop across bw
          IR = 2 + MAXR - IRR
          KR = K + IR
          DO J=KR,N,IW     !(120)               !-  loop O(N/IW) ?
            IUGL = J - IW
            if (j.eq.kr) IUGL = K                  ! first time around
            flops = flops + 1                   ! a divide
            flops = flops + 4                   ! +, *, / and SQRT
            flops = flops + 14 + 6              ! 14 *, 3 + and 3-
            flops = flops + (4+2)*(J-2-IUGL+1)  ! * and +   ! O(N)?
            JM = J - IW
            IF (J.NE.KR) flops = flops+3        ! 2 * and a -
!           MAXL = IW - 1
!           IF (MAXL.GE.N-J) MAXL = N - J
            MAXL = MIN(IW-1, N-J)
            flops = flops + (4+2)*max(0,MAXL)   ! 4 *, + amd -  ! O(IW)
            IF (J+IW.le.N) flops = flops + 2
          enddo
        enddo
      enddo
      if (ipr.ge.4) write(*,'(A,F10.3,A,F18.6,A)')
     & 'BANDRD:',N/1.e6,' Mequations', flops/1.e9,' Gflop (estimated)'

!------------------------------
      N2 = N - 2
      IF (N-2.LT.1) GO TO 180   !- nothing to do for a 1x1 matrix.
!------------------------------
      call get_secs (t0)
      IPERCENT_OLD = 0

      DO 160 K=1,N2
        IPERCENT = nint(100.*REAL(K)/REAL(N))    !=nint(I/(.01*n))
        IF (IPR.ge.5 .and.IPERCENT.NE.IPERCENT_OLD)
     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)

         MAXR = IW
         IF (N-K.LT.IW) MAXR = N - K
         DO 140 IRR=2,MAXR
            IR = 2 + MAXR - IRR
            KR = K + IR
            DO 120 J=KR,N,IW
               IF (J.EQ.KR) GO TO 20
               IF (G.EQ.0.0) GO TO 140
               JM = J - IW
               B = -A(JM-1,IW+1)/G
               IUGL = J - IW
               GO TO 40
   20          IF (A(K,IR+1).EQ.0.0) GO TO 140
               B = -A(K,IR)/A(K,IR+1)
               IUGL = K
   40          S = 1.0/SQRT(1.0+B*B)
               C = B*S
               C2 = C*C
               S2 = S*S
               CS = C*S
               U = C2*A(J-1,1) - 2.0*CS*A(J-1,2) + S2*A(J,1)
               U1 = S2*A(J-1,1) + 2.0*CS*A(J-1,2) + C2*A(J,1)
               A(J-1,2) = CS*(A(J-1,1)-A(J,1)) + (C2-S2)*A(J-1,2)
               A(J-1,1) = U
               A(J,1) = U1
               J2 = J - 2
               DO 60 L=IUGL,J2
                  JL = J - L
                  U = C*A(L,JL) - S*A(L,JL+1)
                  A(L,JL+1) = S*A(L,JL) + C*A(L,JL+1)
                  A(L,JL) = U
   60          CONTINUE
               JM = J - IW
               IF (J.NE.KR) A(JM-1,IW+1) = C*A(JM-1,IW+1) - S*G
               MAXL = IW - 1
               IF (N-J.LT.IW-1) MAXL = N - J
               IF (MAXL.LE.0) GO TO 100
               DO 80 L=1,MAXL
                  U = C*A(J-1,L+2) - S*A(J,L+1)
                  A(J,L+1) = S*A(J-1,L+2) + C*A(J,L+1)
                  A(J-1,L+2) = U
   80          CONTINUE
  100          IF (J+IW.GT.N) GO TO 120
               G = -S*A(J,IW+1)
               A(J,IW+1) = C*A(J,IW+1)
  120       CONTINUE
  140    CONTINUE
  160 CONTINUE
  180 E(1) = 0.0    !- end of loop


c--- report Mflops/s ---
      if (ipr.ge.4) then
        call get_secs (t1)
        flop_rate=0.
        if (t1.gt.t0) flop_rate = flops / (t1-t0)
        write(*,'(A,F10.3,A,F10.3,A)')
     & 'BANDRD: in', t1-t0,' secs :=',flop_rate/1.e6, 'Mflops/s'
      endif


!-- take a copy of the diagonal (could do outside)
      DO I=1,N
        D(I) = A(I,1)
      enddo
!-- take a copy of sub-diagonal - could I not do this outside?
      IF (2.GT.N) GO TO 240
      DO I=2,N
         E(I) = A(I-1,2)
      enddo
  240 CONTINUE

!-- this next bit seems rather pointless!  DJK 11-3-00
!     DO I=1,N
!         E2(I) = E(I)*E(I)
!     enddo
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE BISECT(N,ACHEPS,D,E,IFAIL,IPR)
C
C      THIS SUBROUTINE FINDS THE EIGENVALUES OF A TRIDIAGONAL MATRIX T
C      GIVEN WITH ITS DIAGONAL ELEMENTS IN THE ARRAY D(N) AND
C      ITS SUBDIAGONAL ELEMENTS IN THE LAST N - 1 STORES OF THE
C      ARRAY E(N), USING QL TRANSFORMATIONS. THE EIGENVALUES ARE
C      OVERWRITTEN ON THE DIAGONAL ELEMENTS IN THE ARRAY D IN
C      ASCENDING ORDER. THE SUBROUTINE WILL FAIL IF ANY ONE
C      EIGENVALUE TAKES MORE THAN 30 ITERATIONS.
C
c      9-3-01 (DJK) added (from fe5lib) for Lamaload Eigenvalues.
c     13-3-01 (DJK) added flops estimate - appears O(N^2) ?
c        note that this can only be an estimate since it has internal iteration.
c
      INTEGER P01AAF, ISAVE, IFAIL, N, I, L, J, M, I1, M1, II   ! P01AAf unused!
      REAL B, F, H, ACHEPS, G, P, R, C, S, D(*), E(*)
      real flops
!      ISAVE = IFAIL  ! unused

c.. calc total flops needed ..
      flops = 0
      DO L=1,N                !(340)
         flops = flops + 4                    !- *, +, 2abs
         flops = flops + 2*(N-L+1)            ! search rest of diagonal for smallest
         M=(N-L)/2     ! guess that smallest is  half way down.
!      if found as the first number then there is nothing more to do.
!      else we iterate the next set of lines...
         flops = flops + 2 +3+3+ 1+2          ! one of these is a sqrt
         flops = flops + 1*(max(0,N-(L+1)+1))   ! simple subtract
         flops = flops + 1
!     QL Transformation
         flops = flops + 17* (max(0,M-1-L+1))   !- plenty of divides and a sqrt
         flops = flops + 2                  ! *
!        here we reiterate if (ABS(E(L)).GT.B)  (limit is 30 times)
         flops = flops + 1
         flops = flops + 1*(max(0,L-2+1))   ! insert eigenvalue into list (in order)
      ENDDO
      if (ipr.ge.4) write(*,'(A,F10.3,A,F18.6,A)')
     & 'BISECT:',N/1.e6,' Mequations', flops/1.e9,
     & ' Gflop (underestimate)'

!------------------------------

      IF (N.EQ.1) GO TO 40
      DO 20 I=2,N             !- hmm shuffle the list down
         E(I-1) = E(I)        !- throw E(1) away and put 0.0 in E(N)
   20 CONTINUE
   40 E(N) = 0.0
      B = 0.0
      F = 0.0
!------------------------------
      call get_secs (t0)

      IPERCENT_OLD = 0

      DO 340 L=1,N
        IPERCENT = nint(100.*REAL(L)/REAL(N))    !=nint(I/(.01*n))
        IF (IPR.ge.5 .and.IPERCENT.NE.IPERCENT_OLD)
     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)

         J = 0
         H = ACHEPS*(ABS(D(L))+ABS(E(L)))
         IF (B.LT.H) B = H
C     LOOK FOR SMALL SUB DIAGONAL ELEMENT
         DO 60 M=L,N
            IF (ABS(E(M)).LE.B) GO TO 80
   60    CONTINUE
   80    IF (M.EQ.L) GO TO 260
  100    IF (J.EQ.30) GO TO 360     !- loop back point when iterating
         J = J + 1
C     FORM SHIFT
         G = D(L)
         H = D(L+1) - G
         IF (ABS(H).GE.ABS(E(L))) GO TO 120
         P = H*0.5/E(L)
         R = SQRT(P*P+1.0)
         H = P + R
         IF (P.LT.0.0) H = P - R
         D(L) = E(L)/H
         GO TO 140
  120    P = 2.0*E(L)/H
         R = SQRT(P*P+1.0)
         D(L) = E(L)*P/(1.0+R)
  140    H = G - D(L)
         I1 = L + 1
         IF (I1.GT.N) GO TO 180
         DO 160 I=I1,N
            D(I) = D(I) - H
  160    CONTINUE
  180    F = F + H
C     QL TRANSFORMATION
         P = D(M)
         C = 1.0
         S = 0.0
         M1 = M - 1
         DO 240 II=L,M1
            I = M1 - II + L
            G = C*E(I)
            H = C*P
            IF (ABS(P).LT.ABS(E(I))) GO TO 200
            C = E(I)/P
            R = SQRT(C*C+1.0)
            E(I+1) = S*P*R
            S = C/R
            C = 1.0/R
            GO TO 220
  200       C = P/E(I)
            R = SQRT(C*C+1.0)
            E(I+1) = S*E(I)*R
            S = 1.0/R
            C = C/R
  220       P = C*D(I) - S*G
            D(I+1) = H + S*(C*G+S*D(I))
  240    CONTINUE
         E(L)= S*P
         D(L)= C*P
         IF (ABS(E(L)).GT.B) GO TO 100
  260    P = D(L) + F
C     ORDER EIGENVALUE
         IF (L.EQ.1) GO TO 300
         DO 280 II=2,L
            I = L - II + 2
            IF (P.GE.D(I-1)) GO TO 320
            D(I) = D(I-1)
  280    CONTINUE
  300    I = 1
  320    D(I) = P
  340 CONTINUE   !-- main loop --

c--- report Mflops/s ---
      if (ipr.ge.4) then
        call get_secs (t1)
        flop_rate=0.
        if (t1.gt.t0) flop_rate = flops / (t1-t0)
        write(*,'(A,F10.3,A,F10.3,A)')
     & 'BISECT: in', t1-t0,' secs :=',flop_rate/1.e6, 'Mflops/s'
      endif


      IFAIL = 0
      RETURN

  360 IFAIL=1    !- if > 30 iterations needed.
      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE LINMUL (BK,DISPS,LOADS,N,IW)
C
C     This multiplies a matrix by a vector
c     the matrix is symmetrical with its upper triangle stored as a vector
c     (first N elements are the diagonal, etc. = BANRED style)
c
      REAL BK(*),DISPS(*),LOADS(*)
      DO I=1,N
        X = 0.
        DO J=1,IW+1       !- not past the RH edge of the matrix
          IF (I+J.LE.N+1) X = X + BK(N*(J-1)+I)*DISPS(I+J-1)
        ENDDO
        DO J=2,IW+1       !- the reflected lower terms?
          IF (I-J+1.GE.1) X = X + BK((N-1)*(J-1)+I)*DISPS(I-J+1)
        ENDDO
        LOADS(I) = X
      ENDDO
      END

c-----------------------------------------------------------------------
c     5: Choleski-banded solver (CHOLIN)
c-----------------------------------------------------------------------
      SUBROUTINE FORMKB (KB,IKB,KM,IKM,G,IW,IDOF)
C
C     This forms the global stiffness matrix storing the lower triangle
C     as an array BK(N,IW+1) = CHOLIN style
C       -- the subscripts aught to be reversed !
      REAL KB(IKB,*),KM(IKM,*)
      INTEGER G(*)
      DO I=1,IDOF
        IF (G(I).gt.0) THEN
          DO J=1,IDOF
            IF (G(J).GT.0) THEN
              ICD = G(J)-G(I)+IW+1
              IF (ICD.LE.IW+1)
     &        KB(G(I),ICD) = KB(G(I),ICD) + KM(I,J)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      END

c-----------------------------------------------------------------------
      SUBROUTINE CHOLIN (KB,IKB,N,IW, IPR)
C
C     This performs Choleski Reduction of the stiffness matrix
C     stored as an array BK(N,IW+1)
C    12-3-01 (DJK) timings and mflops added
C
      INTEGER IKB,N,IW, I,J,K
      REAL KB(IKB,*)

c.. calc total flops needed ..
      flops = 0
      DO I=1,N
        flops = flops + 2*(IW)          ! + and * (ddot)
        flops = flops + 2               ! - and SQRT
        DO  K=1,MIN(IW,N-I)             !- loop across again
          flops = flops + 2*(max(0,iw-k)) ! + and *  (ddot)
          flops = flops + 2             ! - and /
        ENDDO
      ENDDO
      if (ipr.ge.4) write(*,'(A,F10.3,A,F15.3,A)')
     & 'CHOLIN:',N/1.e6,' Mequations', flops/1.e9,' Gflop'

!----------------------------------------------
      call get_secs(t0)
      IPERCENT_OLD = 0

      DO I=1,N                           !- loop the equations
!                                         (note BANRED and CHOLIN start at 2 !)
        IPERCENT = nint(100.*REAL(I)/REAL(N))    !=nint(I/(.01*n))
        IF (IPR.ge.5 .and.IPERCENT.NE.IPERCENT_OLD)
     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)

        X=0.
        DO J=1,IW
          X=X+KB(I,J)**2          !- sum_square this row !
        ENDDO
        KB(I,IW+1)=SQRT (KB(I,IW+1)-X)  ! hence reduce the diag term

c       DO 3 K=1,IW                     !- loop across again
        DO  K=1,MIN(IW,N-I)                     !- loop across again
          X=0.
c         IF (I+K.GT.N) GOTO 3          !- off the base-tri

          IF (K.ne.IW) THEN               !- a diag term?
            DO L=IW-K,1,-1                !-- loop back to the edge?
              X = X+KB(I+K,L)*KB(I,L+K)   !- dot_product up the terms
            ENDDO
          ENDIF

          IA = I+K                        !- a term
          IB = IW-K+1                     !- another term
          KB(IA,IB) = (KB(IA,IB)-X) / KB(I,IW+1)     !- so reduce a term
        ENDDO
c   3   CONTINUE
      ENDDO

c--- report Mflops/s ---
      if (ipr.ge.4) then
        call get_secs (t1)
        flop_rate=0.
        if (t1.gt.t0) flop_rate = flops / (t1-t0)
        write(*,'(A,F10.3,A,F10.3,A)')
     & 'CHOLIN: in', t1-t0,' secs :=',flop_rate/1.e6, 'Mflops/s'
      endif

      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE CHOBAC (KB,IKB,LOADS,N,IW, ipr)
C
C     This performs the Choleski Back-Substitution (as from CHOLIN)
C
      REAL KB(IKB,*),LOADS(0:n)
      real flops ,flops_last
      data flops_last/0/

      flops = 1                                   !- a 
      DO I=2,N
        flops = flops + 2*(MAX(1, IW+1 -I +1)-IW+1) ! + and * (ddot)
        flops = flops + 2                         ! - and /
      enddo
      flops = flops + 1
      DO I=N-1,1,-1
        flops = flops + 2* (I+1-MIN (I+IW,N)+1)  ! + and * (ddot)
        flops = flops + 1                        ! a /
      ENDDO
      call get_secs(t0)

c------------ work down the matrix ----------
      LOADS(1)=LOADS(1)/KB(1,IW+1)
      DO I=2,N
c        K = 1
c        IF (I.LE.IW+1) K = IW+1 -I +1   !- limit to the LH edge of KV
c        K = MAX(1, IW+1 -I +1)
        X = 0.0
        DO J=MAX(1, IW+1 -I +1),IW
          X = X + KB(I,J)*LOADS(I+J-IW-1)
        ENDDO
        LOADS(I) = (LOADS(I)-X)/KB(I,IW+1)
      ENDDO

c----------- work back-up the matrix ---------
      LOADS(N)=LOADS(N)/KB(N,IW+1)
      DO I=N-1,1,-1
        X = 0.0
        DO J=I+1,MIN (I+IW,N)     !-- loop down (clip to matrix_base)
          X = X + KB(J,IW+1+I-J) * LOADS(J)
        ENDDO
        LOADS(I) = (LOADS(I)-X) / KB(I,IW+1)
      ENDDO

c--- report Mflops/s ---
      if (ipr.ge.4.and. flops.ne.flops_last) then
        flops_last=flops
        call get_secs (t1)           !- time the process
        flop_rate=0.
        if (t1.gt.t0) flop_rate = flops / (t1-t0) !- this step
        write(*,'(A,F10.3,A,F8.3,A,F8.3,A)')
     &  'CHOBAC:',flops/1.e6,' Mflop in'
     &  ,t1-t0,' secs := ',  flop_rate/1.e6, 'Mflops/s'
      endif

      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE BANMUL (KB,IKB,LOADS,ANS,N,IW)
C
C     This multiplies a matrix by a vector
c     the matrix is symmetrical with its lower triangle
c     stored as a rectangle  (CHOLIN style)
C
      REAL KB(IKB,*),LOADS(*),ANS(*)
      DO 1 I=1,N
        X = 0.
        J = IW+1
    2   IF (I+J.LE.IW+1) GOTO 3
          X = X+KB(I,J)*LOADS(I+J-IW-1)
    3     J = J-1
        IF (J.NE.0) GOTO 2         !- -ve do loop
        J = IW
    6   IF (I-J.GE.N-IW) GOTO 7
          X = X+KB(I-J+IW+1,J)*LOADS(I-J+IW+1)
    7     J = J-1
        IF (J.NE.0) GOTO 6           !- -ve do-loop
        ANS(I) = X
    1 CONTINUE
      END

c-----------------------------------------------------------------------

C-------------------------------------------------------------------------
C-------------------------------------------------------------------------
C >>>> Part 3 : eigenvalue routines from 'EISPACK'                <<<<<<<<
C-------------------------------------------------------------------------

C-------------------------------------------------------------------------
c.. abstracted to EISPACK.F for now 11-3-95 (why?)
C--------------------------------------------------------------------

      SUBROUTINE TRED2 (nm,n,a,d,e,z,iz ,ipr)
c
      integer i,j,k,l,n,ii,nm,jp1
c      double precision a(nm,n),d(n),e(n),z(iz,n)
c      double precision f,g,h,hh,scale
      real a(nm,n),d(n),e(n),z(iz,n)
      real f,g,h,hh,scale

c ** 9-6-94 modified by Dan Kidger to pass 'iz' : the size of 'z'
c
c     this subroutine is a translation of the algol procedure tred2,
c     num. math. 11, 181-195(1968) by martin, reinsch, and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).
c
c     this subroutine reduces a real symmetric matrix to a
c     symmetric tridiagonal matrix using and accumulating
c     orthogonal similarity transformations.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        a contains the real symmetric input matrix.  only the
c          lower triangle of the matrix need be supplied.
c
c     on output
c
c        d contains the diagonal elements of the tridiagonal matrix.
c
c        e contains the subdiagonal elements of the tridiagonal
c          matrix in its last n-1 positions.  e(1) is set to zero.
c
c        z contains the orthogonal transformation matrix
c          produced in the reduction.
c
c        a and z may coincide.  if distinct, a is unaltered.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      do i = 1, n              !--- 1: take a copy of the matrix ----
         do J = i, n
            z(j,i) = a(j,i)
         ENDDO
         d(i) = a(n,i)         !- copy the diagonal too.
      ENDDO

      if (n .eq. 1) go to 510      !- a 1x1 matrix!
c     .......... for i=n step -1 until 2 do -- ..........
c ** 2: loop down the matrix ***
      IPERCENT_OLD = 0
      do 300 ii = 2, n
        i = n + 2 - ii
c       IPR=2   !hack
c       IF (IPR.GE.2) CALL PR_EL_COUNT ('Freedom',I,N,IPR)    !- echo progress
        IPERCENT = nint(100.*REAL(II)/REAL(N))
c        IF (IPR.ge.2 .and.IPERCENT.NE.IPERCENT_OLD)
c     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)

         l = i - 1
         h = 0.0d0
         scale = 0.0d0
         if (l .lt. 2) go to 130
c     .......... scale row (algol tol then not needed) ..........
         do 120 k = 1, l
  120    scale = scale + abs(d(k))
c
         if (scale .ne. 0.0d0) go to 140
  130    e(i) = d(l)
c
         do 135 j = 1, l
            d(j) = z(l,j)
            z(i,j) = 0.0d0
            z(j,i) = 0.0d0
  135    continue
c
         go to 290
c
  140    do 150 k = 1, l
            d(k) = d(k) / scale
            h = h + d(k) * d(k)
  150    continue
c
         f = d(l)
         g = -sign(sqrt(h),f)
         e(i) = scale * g
         h = h - f * g
         d(l) = f - g
c     .......... form a*u ..........
         do 170 j = 1, l
  170    e(j) = 0.0d0
c
         do 240 j = 1, l
            f = d(j)
            z(j,i) = f
            g = e(j) + z(j,j) * f
            jp1 = j + 1
            if (l .lt. jp1) go to 220
c
            do 200 k = jp1, l
               g = g + z(k,j) * d(k)
               e(k) = e(k) + z(k,j) * f
  200       continue
c
  220       e(j) = g
  240    continue
c     .......... form p ..........
         f = 0.0d0
c
         do 245 j = 1, l
            e(j) = e(j) / h
            f = f + e(j) * d(j)
  245    continue
c
         hh = f / (h + h)
c     .......... form q ..........
         do 250 j = 1, l
  250    e(j) = e(j) - hh * d(j)
c     .......... form reduced a ..........
         do 280 j = 1, l
            f = d(j)
            g = e(j)
c
            do 260 k = j, l
  260       z(k,j) = z(k,j) - f * e(k) - g * d(k)
c
            d(j) = z(l,j)
            z(i,j) = 0.0d0
  280    continue
c
  290    d(i) = h
  300 continue


c     .......... accumulation of transformation matrices ..........

      IPERCENT_OLD=0
      do 500 i = 2, n
        IPERCENT = nint(100.*REAL(I)/REAL(N))
        IF (IPR.ge.2 .and.IPERCENT.NE.IPERCENT_OLD)
     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)
         l = i - 1
         z(n,l) = z(l,l)
         z(l,l) = 1.0d0
         h = d(i)
         if (h .eq. 0.0d0) go to 380
c
         do 330 k = 1, l
  330    d(k) = z(k,i) / h
c
         do 360 j = 1, l
            g = 0.0d0
c
            do 340 k = 1, l
  340       g = g + z(k,i) * z(k,j)
c
            do 360 k = 1, l
               z(k,j) = z(k,j) - g * d(k)
  360    continue
c
  380    do 400 k = 1, l
  400    z(k,i) = 0.0d0
c
  500 continue

c---------------------------------------
  510 do 520 i = 1, n
         d(i) = z(n,i)
         z(n,i) = 0.0d0
  520 continue
c
      z(n,n) = 1.0d0
      e(1) = 0.0d0
      return
      end

C-------------------------------------------------------------------------
      subroutine tql2 (nm,n,d,e,z,iz,ierr)
c
      integer i,j,k,l,m,n,ii,l1,l2,nm,mml,ierr
c      double precision d(n),e(n),z(iz,n)
c      double precision c,c2,c3,dl1,el1,f,g,h,p,r,s,tst1,tst2  !,pythag
      real d(n),e(n),z(iz,n)
      real c,c2,dl1,el1,f,g,h,p,r,s,tst1,tst2  !,pythag
      ! to avoid compile warning about uninitialised and type convertion
      real:: zero=0. , one=1.  , two=2.
      real:: s2=0., c3=0.

c ** 9-6-94 modified by Dan Kidger to pass 'iz' : the size of 'z'
c
c     this subroutine is a translation of the algol procedure tql2,
c     num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and
c     wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 227-240(1971).
c
c     this subroutine finds the eigenvalues and eigenvectors
c     of a symmetric tridiagonal matrix by the ql method.
c     the eigenvectors of a full symmetric matrix can also
c     be found if  tred2  has been used to reduce this
c     full matrix to tridiagonal form.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        d contains the diagonal elements of the input matrix.
c
c        e contains the subdiagonal elements of the input matrix
c          in its last n-1 positions.  e(1) is arbitrary.
c
c        z contains the transformation matrix produced in the
c          reduction by  tred2, if performed.  if the eigenvectors
c          of the tridiagonal matrix are desired, z must contain
c          the identity matrix.
c
c      on output
c
c        d contains the eigenvalues in ascending order.  if an
c          error exit is made, the eigenvalues are correct but
c          unordered for indices 1,2,...,ierr-1.
c
c        e has been destroyed.
c
c        z contains orthonormal eigenvectors of the symmetric
c          tridiagonal (or full) matrix.  if an error exit is made,
c          z contains the eigenvectors associated with the stored
c          eigenvalues.
c
c        ierr is set to
c          zero       for normal return,
c          j          if the j-th eigenvalue has not been
c                     determined after 30 iterations.
c
c     calls pythag for  dsqrt(a*a + b*b) .
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      ierr = 0
      if (n .eq. 1) go to 1001
c
      do 100 i = 2, n               !-- shuffle down --
  100 e(i-1) = e(i)
c
      f = 0.0d0
      tst1 = 0.0d0
      e(n) = 0.0d0
c
      IPERCENT_OLD = 0
      do 240 l = 1, n

c       IPR=2   !hack
c       IF (IPR.GE.2) CALL PR_EL_COUNT ('Freedom',I,N,IPR)    !- echo progress
        IPERCENT = nint(100.*REAL(I)/REAL(N))
c        IF (IPR.ge.2 .and.IPERCENT.NE.IPERCENT_OLD)
c     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)

         j = 0
         h = abs(d(l)) + abs(e(l))
         if (tst1 .lt. h) tst1 = h
c     .......... look for small sub-diagonal element ..........
         do 110 m = l, n
            tst2 = tst1 + abs(e(m))
            if (tst2 .eq. tst1) go to 120
c     .......... e(n) is always zero, so there is no exit
c                through the bottom of the loop ..........
  110    continue
c
  120    if (m .eq. l) go to 220
  130    if (j .eq. 30) go to 1000
         j = j + 1
c     .......... form shift ..........
         l1 = l + 1
         l2 = l1 + 1
         g = d(l)
         p = (d(l1) - g) / (two * e(l))
          r = sqrt(p**2+one)
c         r = pythag(p,1.0d0)
         d(l) = e(l) / (p + sign(r,p))
         d(l1) = e(l) * (p + sign(r,p))
         dl1 = d(l1)
         h = g - d(l)
         if (l2 .gt. n) go to 145
c
         do 140 i = l2, n
  140    d(i) = d(i) - h
c
  145    f = f + h
c     .......... ql transformation ..........
         p = d(m)
         c = 1.0d0
         c2 = c
         el1 = e(l1)
         s = 0.0d0
         mml = m - l
c     .......... for i=m-1 step -1 until l do -- ..........
         do 200 ii = 1, mml
            c3 = c2
            c2 = c
            s2 = s
            i = m - ii
            g = c * e(i)
            h = c * p

             r = sqrt(p**2+e(i)**2)

c            r = pythag(p,e(i))
            e(i+1) = s * r
            s = e(i) / r
            c = p / r
            p = c * d(i) - s * g
            d(i+1) = h + s * (c * g + s * d(i))
c     .......... form vector ..........
            do 180 k = 1, n
               h = z(k,i+1)
               z(k,i+1) = s * z(k,i) + c * h
               z(k,i) = c * z(k,i) - s * h
  180       continue
c
  200    continue
c
         p = -s * s2 * c3 * el1 * e(l) / dl1
         e(l) = s * p
         d(l) = c * p
         tst2 = tst1 + abs(e(l))
         if (tst2 .gt. tst1) go to 130
  220    d(l) = d(l) + f
  240 continue
c     .......... order eigenvalues and eigenvectors ..........
      IPERCENT_OLD = 0
      do 300 ii = 2, n
         i = ii - 1
        IPERCENT = nint(100.*REAL(II)/REAL(N))
c        IF (IPR.ge.2 .and.IPERCENT.NE.IPERCENT_OLD)
c     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)


         k = i
         p = d(i)
c
         do 260 j = ii, n
            if (d(j) .ge. p) go to 260
            k = j
            p = d(j)
  260    continue
c
         if (k .eq. i) go to 300
         d(k) = d(i)
         d(i) = p
c
         do 280 j = 1, n
            p = z(j,i)
            z(j,i) = z(j,k)
            z(j,k) = p
  280    continue
c
  300 continue
c
      go to 1001
c     .......... set error -- no convergence to an
c                eigenvalue after 30 iterations ..........
 1000 ierr = l
 1001 return
      end

c.. only build in eispack if really needed (cos of compiler warnings)
c#endif

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      > > > > >      Basic Matrix Algebra Subroutines    < < < < <
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c.. much of this is obsolete under Fortran 90's array intrinsics.
c.. and anyway there are BLAS equivalents
c - although if I can inline my code it would be faster - particularly
c   for small matrices
C-----------------------------------------------------------------------
      SUBROUTINE NULL2D (A,IA,M,N)
C
C     This nulls a 2-d array
C     01-12-99 renamed from simply 'NULL' becuase of FTN95 conflicts
c
      IMPLICIT NONE
      INTEGER IA, M,N, I,J
      REAL A(IA,*)

      DO J=1,N
cdir$ unroll 8
ccdir$ cache_bypass A
        DO I=1,M              !- note: reversed loops
          A(I,J)=0.0
        ENDDO
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE NULVEC (VEC,N)
C
C     This nulls a column vector
C
      IMPLICIT NONE
      INTEGER I,N
      REAL VEC(*)
      DO I=1,N
        VEC(I) = 0.
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE MATCOP (A,IA,B,IB,M,N)
C
C     This copies matrix A to matrix B
C
      IMPLICIT NONE
      INTEGER IA,IB, M,N, I,J
      REAL A(IA,*), B(IB,*)

      DO J=1,N
        DO I=1,M                    ! note : reversed order
          B(I,J) = A(I,J)
        ENDDO
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE MSMULT (A,IA,C,M,N)
C
C     This multiplies a matrix by a scalar
C
      IMPLICIT NONE
      INTEGER IA, M,N, I,J
      REAL A(IA,*),  C
      DO J=1,N
        DO I=1,M                !- note: reversed loops
          A(I,J) = A(I,J)*C
        ENDDO
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE MVMULT (M,IM,V,K,L,Y)
C
C     This multiplies a matrix by a vector
C
      IMPLICIT NONE
      INTEGER IM, K,L
      REAL M(IM,*),V(*),Y(*)
#ifdef BLAS2
      CALL dgemv ('n',K,L,1.,M,IM,V,1,0.,Y,1)
#else
c hmm should I not reverse the loops ?
      INTEGER I,J
      REAL X
      DO I=1,K
        X = 0.
        DO J=1,L
          X = X + M(I,J)*V(J)      !-- reverse order ??
        ENDDO
        Y(I) = X
      ENDDO
#endif
      END

C-----------------------------------------------------------------------
      SUBROUTINE MTVMULT (M,IM,V,K,L,Y)
C
C     This multiplies a matrix-transposed by a vector
C
      IMPLICIT NONE
      INTEGER IM, K,L
      REAL M(IM,*),V(*),Y(*)

#ifdef BLAS2
      CALL dgemv ('t',L,K,1.,M,IM,V,1,0.,Y,1)
#else
      INTEGER I,J
      REAL X
      DO I=1,K
        X = 0.
        DO J=1,L
          X = X + M(J,I)*V(J)      !-- reverse order ??
        ENDDO
        Y(I) = X
      ENDDO
#endif
      END

C-----------------------------------------------------------------------
      SUBROUTINE MAT_MULT (A,IA,B,IB,C,IC,L,M,N)
C
C     This forms the product of two matrices
C     .. need inner-loop optimisation for vectorisation (cf Cray version)
C
      IMPLICIT NONE
      INTEGER IA, IB, IC, L,M,N
      REAL A(IA,*),B(IB,*),C(IC,*)

#ifdef BLAS2
      CALL DGEMM ('n','n',L,N,M,1.,A,IA,B,IB,0.,C,IC)
#else
      INTEGER I,J,K
      REAL X
      DO I=1,L
        DO J=1,N
          X = 0.0
          DO K=1,M
            X = X+A(I,K)*B(K,J)
          ENDDO
          C(I,J) = X
        ENDDO
      ENDDO
#endif
      END

C-----------------------------------------------------------------------
      SUBROUTINE VECCOP (A,B,N)
C
C     Copies vector A into vector B
C
      IMPLICIT NONE
      INTEGER I,N
      REAL A(*), B(*)

      DO I = 1,N
        B(I) = A(I)
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE VECADD (A,B,C,N)
C
C     Adds vectors    A + B = C
C
      IMPLICIT NONE
      INTEGER I,N
      REAL A(*),B(*),C(*)
      DO I = 1,N
        C(I) = A(I) + B(I)
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE VECSUB (A,B,C,N)
C
C     Vector subtract   :   C := A - B
C
      IMPLICIT NONE
      INTEGER I,N
      REAL A(*), B(*), C(*)

      DO I = 1,N
        C(I) = A(I) - B(I)
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE VSMULT (V,SCAL,N)
C
C     multiply a vector by a scalar
C
      IMPLICIT NONE
      INTEGER I,N
      REAL V(*), SCAL

      DO I = 1,N
        V(I) = V(I) * SCAL
      ENDDO
      END
C-----------------------------------------------------------------------
      SUBROUTINE VDOTV (V1,V2,DOTPR,N)
C
C     dot product  :  V1 * V2
C
      IMPLICIT NONE
      INTEGER I,N
      REAL V1(*),V2(*), DOTPR

      DOTPR = 0.0
      DO I = 1,N
        DOTPR = DOTPR + V1(I)*V2(I)
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE VVMULT (V1,V2,PROD,IPROD,M,N)
C
C     This forms a vector product
C
      IMPLICIT NONE
      INTEGER IPROD, M,N, I,J
      REAL V1(*),V2(*),PROD(IPROD,*)
      DO J=1,N
        DO I=1,M
          PROD(I,J) = V1(I)*V2(J)
        ENDDO
      ENDDO
      END

c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c      > > > > > > >  Toolbox routines : JAC,BTDB   < < < < < < <
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------

c-----------------------------------------------------------------------
      SUBROUTINE INVERT_JAC (JAC,IJAC,DET,NODOF)
C
C     This inverts a Jacobian (small square) matrix in 1D,2D,3D
C     this calls TWOBY2,TREEX3 as needed, putting JAC1 back into JAC
C     (I dont really like this copying!)
C
      IMPLICIT NONE
      INTEGER IJAC, NODOF, ijac1
      PARAMETER (IJAC1=6)
      REAL JAC(IJAC,*), JAC1(IJAC1,IJAC1), DET

      IF (NODOF.EQ.1) THEN
        DET = JAC(1,1)
        JAC(1,1) = 1./DET
      ELSEIF (NODOF.EQ.2) THEN
        CALL TWOBY2 (JAC,IJAC,JAC1,IJAC1,DET)
        CALL MATCOP (JAC1,IJAC1,JAC,IJAC,NODOF,NODOF)
      ELSEIF (NODOF.EQ.3) THEN
        CALL TREEX3 (JAC,IJAC,JAC1,IJAC1,DET)
        CALL MATCOP (JAC1,IJAC1,JAC,IJAC,NODOF,NODOF)
      ELSE
        CALL MATINV (JAC,IJAC,NODOF)
c       PRINT*,'** WARNING, DET not calculated (INVERT) .. set to 1.'
c  .. hmm but if I multiply JAC1*JAC I will get 'I'
c   what about if I just dot_product the diagonals?
        DET = 1.
      ENDIF
      IF (DET.LT.0.) CALL MYERROR (1,'Negative Jacobian (INVERT)')
      END

c-----------------------------------------------------------------------
      SUBROUTINE TWOBY2 (JAC,IJAC,JAC1,IJAC1,DET)
C
C     This forms the inverse of a 2 by 2 matrix
C
      IMPLICIT NONE
      INTEGER IJAC, IJAC1
      REAL JAC(IJAC,*),JAC1(IJAC1,*), DET

      DET = JAC(1,1)*JAC(2,2) - JAC(1,2)*JAC(2,1)
      if (det.eq.0.) return          !- abandon work.

      JAC1(1,1) = JAC(2,2) /DET
      JAC1(1,2) =-JAC(1,2) /DET
      JAC1(2,1) =-JAC(2,1) /DET
      JAC1(2,2) = JAC(1,1) /DET
      END

C-----------------------------------------------------------------------
      SUBROUTINE TREEX3 (JAC,IJAC,JAC1,IJAC1,DET)
C
C     This forms the inverse of a 3 by 3 matrix
C
      IMPLICIT NONE
      INTEGER IJAC, IJAC1
      REAL JAC(IJAC,*),JAC1(IJAC1,*), DET

      DET =  JAC(1,1) * (JAC(2,2)*JAC(3,3)-JAC(3,2)*JAC(2,3))
     &     - JAC(1,2) * (JAC(2,1)*JAC(3,3)-JAC(3,1)*JAC(2,3))
     &     + JAC(1,3) * (JAC(2,1)*JAC(3,2)-JAC(3,1)*JAC(2,2))
      if (det.eq.0.) return          !- abandon work.

      JAC1(1,1) = ( JAC(2,2)*JAC(3,3) - JAC(3,2)*JAC(2,3)) /DET
      JAC1(2,1) = (-JAC(2,1)*JAC(3,3) + JAC(3,1)*JAC(2,3)) /DET
      JAC1(3,1) = ( JAC(2,1)*JAC(3,2) - JAC(3,1)*JAC(2,2)) /DET
      JAC1(1,2) = (-JAC(1,2)*JAC(3,3) + JAC(3,2)*JAC(1,3)) /DET
      JAC1(2,2) = ( JAC(1,1)*JAC(3,3) - JAC(3,1)*JAC(1,3)) /DET
      JAC1(3,2) = (-JAC(1,1)*JAC(3,2) + JAC(3,1)*JAC(1,2)) /DET
      JAC1(1,3) = ( JAC(1,2)*JAC(2,3) - JAC(2,2)*JAC(1,3)) /DET
      JAC1(2,3) = (-JAC(1,1)*JAC(2,3) + JAC(2,1)*JAC(1,3)) /DET
      JAC1(3,3) = ( JAC(1,1)*JAC(2,2) - JAC(2,1)*JAC(1,2)) /DET
      END

C-----------------------------------------------------------------------
      SUBROUTINE MATINV (A,IA,N)
C
C     This forms the inverse of a square-matrix
C      by Gauss-Jordan transformation
C       .. cf. Inversion of Jacobian matrices
C
      IMPLICIT NONE
      INTEGER IA, N, I,J,K
      REAL A(IA,*), CON
      DO K=1,N
        CON=A(K,K)
        A(K,K) = 1.
        DO J=1,N
          A(K,J) = A(K,J)/CON
        ENDDO

        DO I=1,N
          IF (I.NE.K) THEN
            CON=A(I,K)
            A(I,K) = 0.
            DO J=1,N
              A(I,J) = A(I,J)-A(K,J)*CON
            ENDDO
          ENDIF
        ENDDO
      ENDDO
      END

c-----------------------------------------------------------------------
c     3/ Some other Matrix routines:
C        FMBTDB, CHECON3
c-----------------------------------------------------------------------
      SUBROUTINE FMBTDB (BEE,IBEE,DEE,IDEE,KM,IKM,FACT,IH,IDOF)
C
C     ** this is a mega-simplification of the long BT * DEE * BEE
C        and MSMULT, MATADD proceedures. Simply pass it the BEE and
C        DEE matrices and this willl do the rest.
C     It has condensed looping, it avoids unnecessary work when
C     zero terms are found in DEE ( hence not much slow-down when doing
C     SRI). It also exploits the symmetry of KM thus halving the effort.
C
C                                         Dan KIdger    May 1991
C
C     21-12-92 modified to use its own workspace
C
      IMPLICIT NONE
      INTEGER IBEE, IDEE, IKM, IH,IDOF
      REAL  BEE(IBEE,*), DEE(IDEE,*), KM(IKM,*), WORK(500)
      REAL F, FACT
      INTEGER I,J,K, IOP

C--------------- loop across the BEE matrix -------
      DO K=1,IH

C-------- first form one column of BT * DEE in the workspace -----------
        CALL NULVEC (WORK,IDOF)
        DO I=1,IH
          F = DEE(I,K) * FACT
          IF (ABS(F).GT. 1.E-12) THEN     !- skip blanks for speed
            DO J=1,IDOF
              WORK(J) =  WORK(J) + F * BEE(I,J)
            ENDDO
          ENDIF
        ENDDO

C--------  now product this column with BEE and sum into KM ------------
        IOP = 1
c       IOP = 2
c............................. Method 1 : good for vectorization
        IF (IOP.EQ.1) THEN
          DO I=1,IDOF         !(try swapping the loop-order ?)
            DO J=1,IDOF
              KM(I,J) = KM(I,J) + WORK(I) * BEE(K,J)
            ENDDO
          ENDDO
c............................. Method 2 : better for scalar machines ?
c.. Tho' strangely on the PC, Method 1 also runs faster
       ELSEIF (IOP.EQ.2) THEN
         DO I=1,IDOF
           DO J=I,IDOF
             KM(J,I) = KM(J,I) + WORK(I) * BEE(K,J)
          ENDDO
c.. I can hack this next bit off if we only want the upper triangle
           DO J=I+1,IDOF             !- Symmetry
             KM(I,J) = KM(J,I)
           ENDDO
         ENDDO
       ENDIF

      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE CHECON3 (DISPS,DISPS_OLD,N,BIG,BIGGEST)
C
C     This find the largest displacement: BIG      and
C     the largest change in displacement: BIGGEST
C
      IMPLICIT NONE
      REAL DISPS(*),DISPS_OLD(*), BIG,BIGGEST
      INTEGER I,N
      BIG = 0.
      BIGGEST = 0.
      DO I=1,N
        BIG     = MAX (BIG     ,ABS(DISPS(I)))
        BIGGEST = MAX (BIGGEST, ABS(DISPS(I)-DISPS_OLD(I)))
      ENDDO
      if (abs(big).gt.1.e-33) then
        BIGGEST = BIGGEST / BIG
      else
        BIGGEST = 0.
        CALL MYERROR(1,'All Displacements are Zero (CHECON3)')
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
C      3:   Maths functions:
C           (maybe these ought to be in MATRIX.F)
C           - ranking real numbers
C           - ranking an integer array
C           - ranking character arrays ?
C             and inverting a index list to a rank list
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE INDEX_REALS (ARR,N, INDX)
C
C     Indexes an array of N real numbers in ARR()
C     so the first element in INDX() points to the *smallest* number in A etc.
C       - from 'indexx' in 'Numerical recipes in Fortran'
C
      IMPLICIT NONE
      INTEGER N,INDX(N)
      REAL ARR(N)
      INTEGER M,NSTACK               !- local variables ..
      PARAMETER (M=7,NSTACK=50)      !- M = threshhold to just Bubble-sort
      INTEGER ISTACK(NSTACK)         !- workspace (2* 25 entries)
      INTEGER I,INDXT,IR,ITEMP,J,JSTACK,K,L
      REAL A

      DO J=1,N                       !--- assume list is already in order
        INDX(J) = J
      ENDDO
      JSTACK = 0
      L = 1
      IR = N
    1 IF (IR-L.LT.M) THEN    !-- Bubble-sort the small (<7) sub-list --
        DO J=L+1,IR
          INDXT = INDX(J)
          A = ARR(INDXT)
          DO I=J-1,1,-1
            IF (ARR(INDX(I)).LE.A) GOTO 2
            INDX(I+1) = INDX(I)
          ENDDO
          I=0
    2     INDX(I+1) = INDXT
        ENDDO
        IF (JSTACK.EQ.0) RETURN      !--> (this is the *only* exitpoint)
        IR = ISTACK(JSTACK)
        L  = ISTACK(JSTACK-1)           !- Pop the new IR,L off the stack
        JSTACK = JSTACK-2

      ELSE                   !------ Quicksort a longer list -------

        K = (L+IR)/2                ! midpoint of list
        ITEMP = INDX(K)
        INDX(K) = INDX(L+1)         !> swap the 2 pointers
        INDX(L+1) = ITEMP

        IF (ARR(INDX(L+1)).GT.ARR(INDX(IR))) THEN   !- sort the triplet
          ITEMP = INDX(L+1)
          INDX(L+1) = INDX(IR)
          INDX(IR) = ITEMP
        ENDIF
        IF (ARR(INDX(L)).GT.ARR(INDX(IR))) THEN
          ITEMP = INDX(L)
          INDX(L) = INDX(IR)
          INDX(IR) = ITEMP
        ENDIF
        IF (ARR(INDX(L+1)).GT.ARR(INDX(L))) THEN
          ITEMP = INDX(L+1)
          INDX(L+1) = INDX(L)
          INDX(L) = ITEMP
        ENDIF

        I = L+1
        J = IR
        INDXT = INDX(L)
        A = ARR(INDXT)              !- get a value to compare
    3   CONTINUE                      !- search down list
          I = I+1                     !- to find the first mis-order
        IF (ARR(INDX(I)).LT.A) GOTO 3
    4   CONTINUE
          J = J-1                     !- search up list
        IF (ARR(INDX(J)).GT.A) GOTO 4  !- to find the first mis-order

        IF (J.LT.I) GOTO 5            !-- skip over
        ITEMP = INDX(I)
        INDX(I) = INDX(J)             !> swap the 2 pointers over
        INDX(J) = ITEMP
        goto 3
c   5   CONTINUE

    5   INDX(L) = INDX(J)
        INDX(J) = INDXT              !(INDXT was = INDX(L)

        JSTACK = JSTACK+2       !---- push the larger half onto the stack ----
        IF (JSTACK.GT.NSTACK)
     &  CALL MYERROR (3,'Stack size too small (INDEX_REALS)')
        IF (IR-I+1.GE.J-L) THEN       !- which is bigger?
          ISTACK(JSTACK)   = IR
          ISTACK(JSTACK-1) = I
          IR = J-1
        ELSE
          ISTACK(JSTACK)   = J-1
          ISTACK(JSTACK-1) = L
          L = I                         !                           ^
        ENDIF                           !                           |
      ENDIF
      GOTO 1                          !--- jump back to the top  -+
      END

C-----------------------------------------------------------------------
      SUBROUTINE INDEX_INTEGERS (IARR,N, INDX)
C
C     Indexes an array of N real numbers in IARR()
C     so the first element in INDX() points to the *smallest* number in A etc.
C       - from 'indexx' in 'Numerical recipes in Fortran'
C
      IMPLICIT NONE
      INTEGER N,INDX(N),IARR(N)

      INTEGER M,NSTACK               !- local variables ..
      PARAMETER (M=7,NSTACK=50)      !- M = threshhold to just Bubble-sort
      INTEGER ISTACK(NSTACK)         !- workspace (2* 25 entries)
      INTEGER I,INDXT,IR,ITEMP,J,JSTACK,K,L
      REAL IA

      DO J=1,N                       !--- assume list is already in order
        INDX(J) = J
      ENDDO
      JSTACK = 0
      L = 1
      IR = N
    1 IF (IR-L.LT.M) THEN    !-- Bubble-sort the small (<7) sub-list --
        DO J=L+1,IR
          INDXT = INDX(J)
          IA = IARR(INDXT)
          DO I=J-1,1,-1
            IF (IARR(INDX(I)).LE.IA) GOTO 2
            INDX(I+1) = INDX(I)
          ENDDO
          I=0
    2     INDX(I+1) = INDXT
        ENDDO
        IF (JSTACK.EQ.0) RETURN      !--> (this is the *only* exitpoint)
        IR = ISTACK(JSTACK)
        L  = ISTACK(JSTACK-1)           !- Pop the new IR,L off the stack
        JSTACK = JSTACK-2

      ELSE                   !------ Quicksort a longer list -------

        K = (L+IR)/2                ! midpoint of list
        ITEMP = INDX(K)
        INDX(K) = INDX(L+1)         !> swap the 2 pointers
        INDX(L+1) = ITEMP

        IF (IARR(INDX(L+1)).GT.IARR(INDX(IR))) THEN   !- sort the triplet
          ITEMP = INDX(L+1)
          INDX(L+1) = INDX(IR)
          INDX(IR) = ITEMP
        ENDIF
        IF (IARR(INDX(L)).GT.IARR(INDX(IR))) THEN
          ITEMP = INDX(L)
          INDX(L) = INDX(IR)
          INDX(IR) = ITEMP
        ENDIF
        IF (IARR(INDX(L+1)).GT.IARR(INDX(L))) THEN
          ITEMP = INDX(L+1)
          INDX(L+1) = INDX(L)
          INDX(L) = ITEMP
        ENDIF

        I = L+1
        J = IR
        INDXT = INDX(L)
        IA = IARR(INDXT)              !- get a value to compare
    3   CONTINUE                      !- search down list
          I = I+1                     !- to find the first mis-order
        IF (IARR(INDX(I)).LT.IA) GOTO 3
    4   CONTINUE
          J = J-1                     !- search up list
        IF (IARR(INDX(J)).GT.IA) GOTO 4  !- to find the first mis-order

        IF (J.LT.I) GOTO 5            !-- skip over
        ITEMP = INDX(I)
        INDX(I) = INDX(J)             !> swap the 2 pointers over
        INDX(J) = ITEMP
        goto 3
c   5   CONTINUE

    5   INDX(L) = INDX(J)
        INDX(J) = INDXT              !(INDXT was = INDX(L)

        JSTACK = JSTACK+2       !---- push the larger half onto the stack ----
        IF (JSTACK.GT.NSTACK)
     &  CALL MYERROR (3,'Stack size too small (INDEX_REALS)')
        IF (IR-I+1.GE.J-L) THEN       !- which is bigger?
          ISTACK(JSTACK)   = IR
          ISTACK(JSTACK-1) = I
          IR = J-1
        ELSE
          ISTACK(JSTACK)   = J-1
          ISTACK(JSTACK-1) = L
          L = I                         !                           ^
        ENDIF                           !                           |
      ENDIF
      GOTO 1                            !--- jump back to the top  -+
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

