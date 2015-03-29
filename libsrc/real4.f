c-----------------------------------------------------------------------
C   Currently these routines are just for handling REAL*4 calls
C   when the program is compilied with /DREAL
C   .. perhaps DSORT@() and DTIME@() should be passed thru here too ?
C   .. perhaps we should call these the DBOS interface ??
c-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE SET_FONT (IFONT,SIZE1,ROTATION,ITALICS)
C
C     Just a wrapper for SET_TEXT_ATTRRIBUTE
C     to convert REAL*8 to REAL*4 arguements
C
      INTEGER IFONT
      REAL*4  SIZE,ROTATION,ITALICS, size1 

      size = size1

c      CALL GET_IDEST (IDEST)
c      if (idest.eq.50) then      !- Postscript
c         size = size / 40.            !- try this ?
c      elseif (idest.le.29) then
c         size = size / (1024./210.)  * 2.   !- hack
c      ENDIF

c     CALL SET_TEXT_ATTRIBUTE@ ( IFONT, SIZE,ROTATION,ITALICS )
      RETURN
      END

C-----------------------------------------------------------------------


