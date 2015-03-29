c-----------------------------------------------------------------------
c    MPI_NUL.F
c      This is is 'nul' implimentation of MPI
c          Dan Kidger  6-4-98
c
c      It allows my MPI codes to run on non-MPI machines (eg the PC)
c      it (naturaly) tells the world that there is only one processor
c      messages go to the great bit-bucket in the sky
c      received messages never arrive ??
c
c-----------------------------------------------------------------------

c-----------------------------------------------------------------------
      SUBROUTINE MPI_INIT (ifail)
C
C     This is always the first call to MPI
C
      ifail=0
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE MPI_FINALIZE (ifail)
C
C     This is always the last call to MPI
C
      ifail=0
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine MPI_Comm_rank (MPI_COMM_WORLD, irank,ifail) 
c
c     This returns the number of the current process
c        counting 0,1,2,3,...
c
      integer MPI_COMM_WORLD
      irank=0
      ifail=0
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine MPI_Comm_size (MPI_COMM_WORLD, nprocs,ifail)
c
c     This returns the total number of processors
c
      integer MPI_COMM_WORLD
      nprocs=1
      ifail=0
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine MPI_Type_extent (MPI_INTEGER, iextent, ifail)
c
c     Something to do with derived types for F77 ?
c
      integer MPI_INTEGER
      ifail=0
      RETURN
      END

c-----------------------------------------------------------------------
c     subroutine MPI_type_struct (2, list_exten, list_disps, list_types,
c     &    mytype, ifail)
c     subroutine MPI_Type_commit (mytype, ifail)
c
c
c
c      ifail=0
c      RETURN
c      END

c-----------------------------------------------------------------------
      subroutine MPI_send (isum,ifoo,mytype, iright,itag,
     &     MPI_COMM_WORLD, ifail) 
c
c     Sends a (non-blocking?) message
c
      integer MPI_COMM_WORLD
      ifail=0
      RETURN
      END

c-----------------------------------------------------------------------
      subroutine MPI_Isend (isum,ifoo,mytype, iright,itag,
     &     MPI_COMM_WORLD, ifail) 
c
c     Sends a (non-blocking?) message
c
      integer MPI_COMM_WORLD
      ifail=0
      RETURN
      END

c-----------------------------------------------------------------------
      subroutine MPI_recv (isum,ifoo,mytype, ileft,MPI_ANY_TAG,
     &     MPI_COMM_WORLD, status, ifail)
c
c     Receives a (non-blocking?) message
c
      integer MPI_COMM_WORLD, MPI_ANY_TAG, status(*)
      ifail=0
      RETURN
      END
c-----------------------------------------------------------------------
      subroutine MPI_Wait (irequest,status,ifail)
c
c     A barrier of some sort.
c
      integer status(*)
      ifail=0
      RETURN
      END
c-----------------------------------------------------------------------

