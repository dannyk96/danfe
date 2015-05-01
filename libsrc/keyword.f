C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C   > > > > > > >  Basic File Handling routines < < < < < < < <  
C-----------------------------------------------------------------------
c.. lowest level library .. never calls anything else.
c
c    (GET_A_DATA_FILE)
C    GET_KEYWORD   - returns the next *KEYWORD
c    IN_TEST       - tests for comments and #included files.
c    MYERROR       - funnels all error message thru here
c    PASS_THRU     - simply copies 'stdin' to 'stdout'
C    PRINT_KEYWORD - simply echoes the keyword.
C      PRINT_KEYWORD_ANSI - with ANSI-colour codes
C    R_OPTIONS     - simply read pairs of IOP,IVAL pairs
C    R_DEBUG       - like R_OPTIONS ?
c    PR_EL_COUNT   - show the percentage done
c      PR_EL_COUNT_ANSI - using ANSI cursor codes
c    WRVAL         - print 'eigenvalues' lumped consecutive values togther
c    TO_UPPER      - convert string to CAPITAL LETTERS
c    TO_LOWER      - convert string to small letters
c    L_JUST        - removes leading spaces from a string
c    EXTRACT_PARAM - finds a string within a record  and retunds its code
c  < COUNT_TOKENS  - counts the number of 'words' in a string >
c       cf count the # of numbers on a line. 
c    GET_NEXT_TOKEN- returns the IBEG:IEND of the next word.
c    GET_TOKENS    - returns ALL words from a string cf GET_NEXT_TOKEN
c    STR_TO_I      - 'reads' an integer from a string - Hex too
c    STR_TO_REAL   - really READS a real from a string 
c    STR_TO_F      - 'reads' a real from a string 
c
c
C-----------------------------------------------------------------------
c... also provide routines for getting the command-line arguements
c... and using SELECT_FILE() if desired
c
c  2-4-95  This is a good home for all non-FE routines, 
c            eg. ANSI code routines
C   cf (matrix.f for sorting routines etc., draw.f for general graphics)
c

C-----------------------------------------------------------------------
c Notes on 'stopwatches' 15-11-98
c   time=STOPWATCH (-1)     - 'starts' sw #1 (time is n/a)
c   time=STOPWATCH (1)      - returns elapsed time of sw #1
c   time=STOPWATCH (0)      - returns elapsed time, since start of program
c
c but what about >1PE ? and getting sum of all PEs?
c also default is to measure CPU, but wallclock is often useful too.

C-----------------------------------------------------------------------

      SUBROUTINE KEY_MISC (FOUND,IPR,KEYWORD,IO)
C
C     A Keyword handler for various non-FE bits and pieces
C         Dan Kidger   16-3-98
C
      INTEGER IO, IPR
      LOGICAL FOUND 
      CHARACTER KEYWORD*(*)
c     DATA IO2/80/            !- this is for file writes I think

c------------- init -----------
      FOUND = .TRUE.

c------------------------ find the '*keyword' --------------------------
c     IF (KEYWORD.EQ.'*EOF') THEN
c      - perhaps handle this here ?

      IF (KEYWORD.EQ.'DUMMY') THEN
c.. simpy scan for the next keyword ?
      ELSEIF (KEYWORD.EQ.'*DUMMY') THEN      !-- just a dummy :-)
c      - should I now scan ahead to the next *KEYWORD line?

c----------------          --------------------
c.. echoes text to stdout. cf use of '!'
      ELSEIF (KEYWORD.EQ.'*ECHO_TEXT') THEN
         CALL R_ECHO_TEXT (IO)                 !- in KEYWORD.F
      ELSEIF (KEYWORD.EQ.'*PAUSE') THEN
        PRINT*,'*PAUSE*, press <Enter> to continue'
c        .or allow a mouse click ?
        READ*

c----------------          --------------------
      ELSEIF (KEYWORD.EQ.'*SKIP') THEN     !- looping-back up the input file
        CALL R_SKIP (IO)                   ! 

c----------------          --------------------
c   Maybe I should hold the printing options internally
c     so IPR is inquired from an object, rather than passsing it though
c     the system
c   I could also hold other flags - eg. if PR_BARLINE goes to ASCII 
c      or to MS-WINDOZE etc. ?
c

c---------------- Variables --------------------
c  Use a C like method?
c    eg. #define WIDTH=2.81
c  then if a Read fails, then in_test could look for $(WIDTH)
c   - replace with 2.81 and write to a buffer so that it can be read.
c
c--- modifying variables
c  can I do imat++. or even imat=2*imat+1
c   so I can make loops more powerful
c  (compare with my knowledge of Basic *interpreters* like the ZX-Spectrum.
c   - the loop-control variable stores its current, max, and step values.
c
c
c

c----------------          --------------------


c----------------          --------------------

C------------------------- else unknown --------------------------------
      ELSE
        FOUND = .FALSE.
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE MY_NARG (IARG)
C  ** OBSOLETE? **
C     This simply returns the number of arguments on the command line
C     hmm maybe forget this for now and just use getarg/argc ?
C
c     CHARACTER value*255
      INTEGER IARG
      do i=1,999
c        call my_getarg(i,value)
c        if (value.eq.' ') goto 99
      enddo
      iarg=i-1
      return
      end

C----------------------------------------------------------------------
c     SUBROUTINE GET_A_DATA_FILE (IO,FILE)
c.. moved to INT_DBOS 'cos of CMNAM() etc.
c
c  18-5-98 scanning order:
c     1/ command-line (if present)  (cf PERL-scripts too)
c     2/ file called FILE
c     3/ a file-browser (eg SELECT_FILE)
c     4/ a raw PRINT*/READ*  (3&4 are n/a for batch-mode)
c
c  notes 
c   1:  Parallel processing where we auto-generate suffixes
c   2:  what happens when we have exhausted the first file and came back
c     looking for another? (only accept 1/ or 2/ here ?)

C-----------------------------------------------------------------------
      SUBROUTINE GET_KEYWORD (IO,Idummy,KEYWORD)
C
C     This gets the next KEYWORD from the data file on unit IO
C     '*EOF' is returned when all data is exhausted
C     IDUMMY is not used anymore
C       .. maybe force the user to use upper-case ? (eg. on the cray)
C       .. nope better to use my own case-conversion

c 20-9-98 maybe we should store LINE in COMMON, so that later routines 
c         can ask what the (optional) arguments were


      IMPLICIT NONE
      SAVE
      CHARACTER KEYWORD*(*), LINE*255, KEY*1
      INTEGER IO,IDUMMY,IPASS,IOSTAT, ibeg,iend

c--------  overload to return optional arguments
c. just requires that the main prog to init KEYWORD to ' '
c..12-2-99 hmm shouldn't IEND be reset to 0 ?)
      if (io.eq.-123) then
        CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,',= ')
        if (ibeg.lt.0) then              !- none found
          keyword=' '
        else
          keyword=line(ibeg:iend)        !- return next
        endif
        return
      endif
      iend=0                                 !- for arguments

      do ipass =1, 99999
    1   READ (IO,'(A)',IOSTAT=IOSTAT) LINE
        CALL IN_TEST (IO,IOSTAT,*1,*999)
        CALL TO_UPPER (LINE)                 !- Uppercase is easier to handle
c.. should I shift_left LINE to get the first non-blank character ?
c       CALL LJUST (LINE)                    
c       LINE = LINE(ISTR_BEG(LINE):)              !- remove leading blanks

c.. really on the next I want to find the first non blank character
c.. so things like '*KEYWORD' can be indented/nested
c (perhaps like F77 'c'/'C' aught to be column '1' only?)

        KEY = LINE(1:1)
        IOSTAT = INDEX('Cc!%#/<;',KEY)           !-('c' is a problem?)
        IF (IOSTAT.ge.1) call IN_TEST (IO,IOSTAT,*1,*999)

c        IF (IS_COMMENT(LINE)) THEN 
c          IOSTAT = 1                            !- force as if an error
c          CALL IN_TEST (IO,IOSTAT,*1,*999)      !- so '!' is echoed.
c          GOTO 1
c        ENDIF

c.. why can't I just call IN_TEST always ?

c.. note here on 'Support for DO LOOPS .
c .. if a *DO is found, read the params eg. Cu=30., 10. , -2.
c    then store Cu in a table with first,last&increment values too
C     (see ZX-Spectrum manual for details!)
c    each time READ() fails, we call IN_TEST (say). this looks for a 
c     makefile style $(Cu) token and replaces it with the *real* value.
c     The problem then is how to let the application read this value ??
c      READ(LINE,*) is obvious but not F77 standard (eg fails on the CRAY)
c      ? can a fake an #include line so control will return to the main flow ??
c       .. ie. write new line to a scratch file & IO++
c
c       ? should the Cu=28.3 type lines go in *CONTROL only ?
c         hence a pair of PUT_VAR(), GET_VAR(char VAR, float VALUE
c
c     - a related issue is effiency .. cf binary files for large tables.
c
c
        IF (      KEY.EQ.'<'         .or.
     &      LINE(1:8).eq.'#include') THEN       !- 'look-for' a redirect
          IOSTAT = 1                            !- force as if an error
          CALL IN_TEST (IO,IOSTAT,*1,*999)      !- so IO = IO +1
          GOTO 1
        ENDIF
        IF (KEY.eq.'*'.or.KEY.eq.'[') GOTO 2  !- got a 'hit'
      ENDDO
c.. if no-hit then we must still process the event:
C     comments are OK .. some get echoed.
c     include files are opened
c     ? 'token=value' are recorded ?
c     unknown strings should be echoed with a warning message.

    2 CONTINUE  !   =='EXIT'

c.. note that we mung [windows] style keywords into *DANFE style
      IF (KEY.EQ.'*')  THEN        
        KEYWORD  = LINE(1:INDEX(LINE,' '))      !- strip off any comments
c       CALL UPCASE (KEYWORD)                   !- Uppercase is easier to handle
c       CALL TO_UPPER (LINE)                 !- Uppercase is easier to handle
      ELSEIF (KEY.EQ.'[')  THEN          ! 'Windows(tm)' style :-)
        KEYWORD  = '*'//LINE(2:INDEX(LINE,']')-1)  !- within [pair]
      ENDIF
      RETURN

c--- error exit
 999  KEYWORD ='*EOF'         !- end of the all the data
      END

c-----------------------------------------------------------------------
      SUBROUTINE IN_TEST (IO,IOSTAT,*,*)
C
C     This tests the nature of a data read error
C     return to *1 if 'soft', and *2 if 'fatal'
C     21-9-93 fixed '< file', io status !
C     22-8-94 Patched Salford BUG where IOSTAT=104 at (subsequent) EOFs
C
      IMPLICIT NONE
      CHARACTER LINE*80, file*80
      INTEGER IO,IOSTAT, IFR,ITO, I, ios
      LOGICAL FILE_OPEN

      IF (IOSTAT.EQ.0) THEN
        RETURN                          !-------- No error! -----------
      ELSEIF (IOSTAT.LT.0.or.iostat.eq.104) THEN
        INQUIRE (IO-1,OPENED=FILE_OPEN)  !- is previous unit a file ?  
        IF (FILE_OPEN) THEN   !------------ 'end-of-file'----------------
          CLOSE(IO)
          IO = IO - 1           ! Ok so return to
          RETURN 1              ! the previous unit and continue
        ELSE                  !---------- 'file was not open'------------
          RETURN 2              !ie. ALL DONE
        ENDIF
      ELSE
c----------------- check other possible read-errors --------------------
        BACKSPACE (IO)           !(should never get an error here)
        READ (IO,'(A)',IOSTAT=ios) LINE     !(or here either)
        if (ios.ne.0) print*,'ios=',ios, "(IN_TEST)"
        if (ios.ne.0) return 2

        IF (LINE(1:1).eq.'*') THEN !------- A new Keyword -----------
          BACKSPACE (IO)           !- point back to the '*'
          RETURN 2
        ELSEIF (LINE(1:1).eq.'[') THEN !------- A new Keyword -----------
          BACKSPACE (IO)           !- point back to the '['
          RETURN 2
        ELSEIF (LINE(1:1).EQ.'!') THEN !----An 'echoed' comment line -----
C         WRITE (*,'(A)') LINE
!         CALL WR_LINE_RED (line)
          CALL WR_LINE (line)
          RETURN 1
        ELSEIF (LINE(1:1).EQ.'%') THEN !----An 'echoed' comment line -----
          WRITE (80,'(A)') LINE(2:)    !       to the '.KEY' file
          RETURN 1                     !      (strip off the '!' ?)

!---------- emulate cpp preprocessing (with my extensions) ---------------
c.. if '#' I should really sub call to handle the various possibilities.
c-------------------- GOTO, etc. ------------------
c.. Store which labels we have passed - so we no which direction to look in
        ELSEIF (LINE(1:6).eq.'#label') THEN
          ifr =     index(line      ,'"')+1     ! first '
          ito = ifr+index(line(ifr:),'"')-2     !  ending '
          IF (IFR.lt.1.or.ito.lt.1) THEN  
            CALL MYERROR (2,'invalid #label format')
            RETURN 2                        !-- crash out 
          ENDIF
c         LABEL = LINE(ifr:ito)

c.. now look downwards (or up?) for the corresponding label.
        ELSEIF (LINE(1:5).eq.'#goto') THEN
          ifr =     index(line      ,'"')+1     ! first '
          ito = ifr+index(line(ifr:),'"')-2     !  ending '
          IF (IFR.lt.1.or.ito.lt.1) THEN  
            CALL MYERROR (2,'invalid #goto format')
            RETURN 2                        !-- crash out 
          ENDIF
c         LABEL = LINE(ifr:ito)

c-------------------- read from another file ---------------------------
c (maybe allow 'standard' includes in the form: <file> (mat props?)
c.. should allow both single ' and double " ??
c either by munging the line or using my GET_NEXT_TOKEN
        ELSEIF (LINE(1:8).eq.'#include') THEN
c         (convert " and < to ' here ?)
          ifr =     index(line      ,'"')+1     ! first '
          ito = ifr+index(line(ifr:),'"')-2     !  ending '
          IF (IFR.lt.1.or.ito.lt.1) THEN  
            CALL MYERROR (2,'missing quotes<"> in #include format')
            RETURN 2                        !-- crash out 
          ENDIF
          FILE = LINE(ifr:ito)
          IO = IO + 1
          OPEN (IO,FILE = FILE, IOSTAT=IOSTAT,STATUS='old')
          IF (IOSTAT.NE.0) THEN  
            CALL MYERROR (2,'missing include file-'//FILE)
            IO=IO-1
            RETURN 2                        !-- crash out 
          ENDIF
          RETURN 1            !-- this line was missing !!!  14-9-92

c---------------- read from another file (old) -------------------------
        ELSEIF (LINE(1:1).eq.'<') THEN 
          IO = IO + 1
          FILE = LINE (3:)
          OPEN (IO,FILE = FILE, IOSTAT=IOSTAT,STATUS='old')
          IF (IOSTAT.NE.0) THEN        !--- 'new data file not found'
            CALL MYERROR (1,'missing include file-'//FILE)
            RETURN 2    !-- crash out 
          ENDIF
          RETURN 1      !-- this line was missing !!!  14-9-92

c------------------ allowable comment characters -----------------------
c '!' is also handled above 'cos it is echod to stdout too.
c for /* we oght to keep reading lines until we find a correspondiong */
! should we allow leading whitespace if '!' or /* ..*/ ? 

c       ISCOMMENT=IS_COMMENT (LINE)
c       IF (ISCOMMENT) 
        ELSEIF (INDEX('#!Cc ;',LINE(1:1)).GE.1) THEN
          RETURN 1
        ELSEIF (LINE(1:2).EQ.'/*') THEN    !- this too :-)
          DO I=1, 9999
            IF (INDEX ('*/', LINE) .GE.1) RETURN 1
            READ(IO,*) LINE                   !- what if EOF ?
          ENDDO
          CALL MYERROR(2,' Matching */ not found')     !- 

c-------------------- rubbish found in the file -----------------------
        ELSE    
          print*,'>>LINE was :',LINE(1:60)
          CALL MYERROR (2,'>>Error in data file format ')
          RETURN 2
        ENDIF
      ENDIF      !- test different values of IOSTAT
c  99 CONTINUE  
      CALL MYERROR(3,'shouldn''t have got to this line in IN_TEST')
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_LINE (LINE)
C
C     Simply writes the given line out to stdout
C         Dan Kidger  7-06-04
C
! TODO 290315  using F90 intrinsic len_trim() here
!
      CHARACTER*(*) LINE
      INTEGER ISTR_END
      EXTERNAL ISTR_END
      WRITE(*,'(A)') LINE(1:istr_end(line))
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_LINE_RED (LINE)
C  **UNSUED**
C     Simply writes the given line out to stdout with a RED colour
C      (then resets to white)
C         Dan Kidger 22-2-98
C
      CHARACTER*(*) LINE
      INTEGER ISTR_END
      EXTERNAL ISTR_END
      WRITE(*,'(3A)') CHAR(27)//'[;33m',
     &   LINE(1:istr_end(line)),  CHAR(27)//'[0m'
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE MYERROR (ILEVEL,STRING)
C
C     This outputs a warning/error message to stdout 
C      1 = WARNING   - minor mistakes (eg k*KEYWORDS that contain no data)
C      2 = ERROR     - fatal eg too many nodes
C      3 = INTERNAL  - catching programmers errors
C      4 = COMMENT   - (or INFORMATION) - cf Micro$oft
C
C.... maybe extend with further options..
C     . eg. GINO errors,  e-mail form to report errors :-)
C      options:  1/ # of warnings before failure
C                2/ pause for "press <CR> to continue"
c     19- 2-97 changed 'ERROR' to 'FATAL'
c     19- 2-97 Added control words: '#INIT','#TOTAL_WARNINGS'
c     22-11-98
C
C  exit the program via the FTN77 error hander?
c
      IMPLICIT NONE
      SAVE
      CHARACTER STRING*(*)
      CHARACTER WARNINGS(20)*79
      INTEGER ILEVEL, I
      integer ercount, maxerr
      data ercount,maxerr/0,10/

c--------- handle specical control words -------------
      IF (STRING.EQ.'#INIT') THEN   !- reset the totals (why?)
        ERCOUNT=0
        RETURN
      ELSEIF (STRING.EQ.'#MAX_WARINIGS') THEN
        MAXERR=ILEVEL       !- hack the error code?
        RETURN
      ELSEIF (STRING.EQ.'#TOTAL_WARNINGS') THEN  !- print summary
        IF (ERCOUNT.GT.0) THEN
C-- old style
C          write (*,'(10(''****''))')
C          if (ercount.eq.1)
C     &    write (*,'(a,i3,a,t39,a)') 
C     &     '*    There was' , ercount, ' Warning' ,' *'
C          if (ercount.gt.1)
C     &    write (*,'(a,i3,a,t40,a)') 
C     &     '*    There were', ercount, ' Warnings','*'
C          write (*,'(a,t40,a)')  '*    Please check your data!','*'
C          write (*,'(10(''****''))')
C-- new style (22-11-98)

           if (ercount.eq.1)
     &     write (*,'(A,A)') '**** The following', 
     &     ' warning was reported:'
           if (ercount.gt.1)
     &     write (*,'(A,I3,A)') '**** The following ',ercount, 
     &     ' warnings were reported:'
           do i=1,ercount
             write (*,'(i3,a,a)') i,': ', warnings(i)
           enddo
           write (*,'(A)') '**** please check your data'
         ENDIF
        RETURN
      ENDIF

c--------- Handle the error condition -------------
c.. I could also have an 'Information' ilevel
c.. If under windows then I simply call the appropriate dialog box
c   Windows has Icons for most of these.

      IF (ILEVEL.EQ.1) THEN
        ercount=ercount+1
        WRITE (*,'(A,I3,a,2X,A)') '<WARNING> :',ercount,': ',STRING
        if (ercount.gt.maxerr) then
          WRITE (*,'(A,2X,A)') '**FATAL** :',    !- it is a pity that we
     &     'Maximum No. of Warnings exceeded'    !- cannot call ourselves
          stop
        endif
        WARNINGS(ERCOUNT)=STRING                 !- save for summary
      ELSEIF (ILEVEL.EQ.2) THEN
        WRITE (*,'(A,2X,A)') '**FATAL** :',STRING
        CALL EXIT_PROG()
      ELSEIF (ILEVEL.EQ.3) THEN
        WRITE (*,'(A,2X,A)') '**INTERNAL ERROR** :',STRING
        CALL EXIT_PROG()
      ELSE
        WRITE (*,'(A,I2)') 'hmm.. unknown error level =',ILEVEL
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE EXIT_PROG ()
C
C     This is simply the common exit point for all threads in the package
C
!     call MPI_FINALIZE(ifail)
!     if (ifail.ne.0) print*,'** problem with MPI_FINALIZE'

      STOP
      END !subroutine exit_prog


C-----------------------------------------------------------------------
      SUBROUTINE PASS_THRU (I_IN,I_OUT)
C
C     This simply copies data from U1 to U9 until the next '*KEYWORD'
C     is encounterd
C     ( The first characeter in each line is trimmed off) <- why?
C
      IMPLICIT NONE
      CHARACTER LINE*255
      INTEGER I_IN,I_OUT

  1   READ  (I_IN,'(A)') LINE
      IF (LINE(1:1).NE.'*') THEN         ! 29/03/15 Handle [ too?
        WRITE (I_OUT,'(A)') LINE (2:)    !was(2:LENG(LINE))
        GOTO 1
      ELSE 
        BACKSPACE (I_OUT)
        RETURN
      ENDIF
      END

c-----------------------------------------------------------------------
      SUBROUTINE PRINT_KEYWORD (IKEYWORD,KEYWORD,colorise)
c
c     Simply writes the current keyword
c
c     If text-enviroment (eg. Dos) then, use ANSI colours
c     If under Windows, then ...
c     If under Danplot, write to the screen in a pop-up

      IMPLICIT NONE
      INTEGER IKEYWORD
      CHARACTER KEYWORD*(*)
      logical :: colorise
      integer,external :: istr_end
      if (colorise) then 
        WRITE (*,'(A,I3,1X,A,2X,A,A)') 
     &    CHAR(27)//'[1;33m  ',IKEYWORD
     &   ,CHAR(27)//'[1;32m  ',KEYWORD(1:ISTR_END(KEYWORD))
     &   ,CHAR(27)//'[0m.'
      else
      WRITE (*,'(/A,I3,1X,3A)') '#####', IKEYWORD,
     &    '[',KEYWORD(2:ISTR_END(KEYWORD)),']'
      endif

c--- my graphics ---
c     CALL PRINT_KEYWORD_MYGAPHICS (IKEYWORD,KEYWORD)
c--- Msoft Windows ---
c     CALL PRINT_KEYWORD_WINDOWS (IKEYWORD,KEYWORD)
c--- Motif / X11 / Tcl ---
c     CALL PRINT_KEYWORD_UNIX (IKEYWORD,KEYWORD)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_OPTIONS (IO,IOPS)
C
C     This reads in various program control options as an integer array
C     ..  flag >limit / echo 'old'/'new' ??
c     .. cf a general 'read_real_values' too
!TODO 29/03/15  support token-value syntax
C
      IMPLICIT NONE
      INTEGER IOPS(*)
      INTEGER IO, IOSTAT ,I,IOP,IVAL

      DO I=1,9999
    1   READ(IO,*,IOSTAT=IOSTAT) IOP,IVAL
        CALL IN_TEST(IO,IOSTAT,*1,*999)
! if recode== goto 1 
! if retcode==999 return
        IOPS(IOP) = IVAL
      ENDDO
      CALL MYERROR (2,'-- should never get to this line') !-code = 3 ?
  999 RETURN
      END

C-----------------------------------------------------------------------
C      SUBROUTINE R_DEBUG (IO,IOPS)
c
c
C      RETURN
C      END
C-----------------------------------------------------------------------
       SUBROUTINE R_ECHO_TEXT (IO)
c
c      This should read lines of text and echo them to stdout.
c      until another *Keyword is found.
c      eg. a 'commentry' on an example data file.
c      cf '!' prefix which does the same
c      cf writing a block of keywords to the output stream
c      eg *RAW 2 found dump the next 2 blocks verbatim ?
c

c .. loop and read_line, if line(1:1) /= '*' echo to stdout & continue
c   This is used typical to pass through a table that the verification
c   example should produce.  I guess '<' should be respected.

      INTEGER IO

C    2 READ (IO,*,IOSTAT=IOS) LINE
C       IF (LINE(1:1).EQ.'*') RETURN  .. etc.
C      GOTO 2
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_SKIP (IO)
C
C     This reads in a number, then 'jumps' that number of keywords up or 
c     down the file.
c         Dan Kidger 11-3-98
C     Typicaly we use it in an analysis to re-perform the last loadcase
c     (inspired by Shape Optimisation - remove 5% of elements each time)
c     - contrast with proper loop constructs.
c
c     IMPLICIT NONE
      INTEGER IO, IOSTAT ,I,Ival,ijump
      character line*255
   1  READ(IO,*,IOSTAT=IOSTAT) Ival
      CALL IN_TEST(IO,IOSTAT,*1,*999)

      IJUMP=Ival-1          ! so istep=-1 runs the last keyword again

      IF (IJUMP.eq.0) THEN
        RETURN    !- nothing to do
      elseif (IJUMP.eq.-1) THEN
        call myerror (2,' self-recursive *JUMP detected!')
      endif
      ic =0
      do i=1,999999
        if (ijump.lt.0) then
          backspace(io)          ! Ugly, but the only way
          backspace(io)          ! for backward jumps :-(
        endif
        READ(IO,'(a)',IOSTAT=IOSTAT) LINE
c       CALL IN_TEST(IO,IOSTAT,*1,*999)    !- detect #include ?
        if (line(1:1).eq.'*'.or.line(1:1).eq.'[') then
          ic=ic+1
          if (ic.eq.abs(ijump)) then    ! completed?
            write(*,'(A,A)')   '<> Jumping back to',LINE(1:32)
            backspace (io)
            return
          endif
        endif
      enddo

  999 RETURN
      END

C-----------------------------------------------------------------------

! These next routines are misplaced hjere in Keyword.f
! suggest they are moved to general.f ?

c-----------------------------------------------------------------------
      SUBROUTINE PR_EL_COUNT (WHAT,I,N,IOP)
C      
C     This simply prints out the element counter (or nodes, etc.)
C     .. IOP will be used for different output styles
c       IOP = 0  : disable (eg for batch mode)
c       IOP = 1  : screen using ANSI codes
c             2  : as a coloured 80-column bar
c             3  : to the Windows API
c
C        DJK 4-4-95
C
      IMPLICIT NONE
      CHARACTER WHAT*(*)
      INTEGER I,N,IOP
C     IF (IOP.EQ.1)     !.. IOP=1  is for an ANSI pair
      CALL PR_EL_COUNT_ANSI (WHAT,I,N,IOP)
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE PR_EL_COUNT_ANSI (WHAT,I,N,IOP)
C      
C     This simply prints out the element counter (or nodes, etc.)
C     .. nice to do graphicaly as a %-done line . so use a *different*
C     .. binding (ie. a *graphical* version of this, MYERROR, etc.)
c     -- also can skip a few if N>1000 (say)
c     -- or show as a %
C        DJK 4-4-95
C
!     use, intrinsic :: iso_fortran_env, only : error_unit=>stderr
      IMPLICIT NONE
      CHARACTER WHAT*(*)
      INTEGER:: I,N,IOP, stderr=-1

      if (stderr==-1) call my_getstderr(stderr)   ! first pass

!     PRINT*,WHAT,I,' /',N,CHAR(27)//'[A'
! try stderr
      write(stderr,'(a,i8,a,i8,a)') WHAT,I,' /',N,CHAR(27)//'[A'
      IF (I.EQ.N) PRINT*              !- (optional step over)
      RETURN
      END

C-------------------------------------------------------------------------
      SUBROUTINE WRVAL (IO,R,IDOF)
C
C     this prints the number of eigenmodes sharing the same eigenvalue
C     R= list of Eigenvales, V1 = reduced list (I1=pointers)
C     .. really this is more general .. write a vector with repeat-counts
C        if consequtive values differ by less then TOL
! Only called from danfe.f
c
      IMPLICIT NONE
      REAL R(*), TOL
      PARAMETER (TOL=.001)
      REAL    V1 (999)
      INTEGER I1 (999)
      INTEGER IO ,IC,K,IDOF,I

c     R(IDOF+1) = 0.    !-- flag the end :-)   (why)

      IC    = 1
      I1(1) = 1
      V1(1) = R(1)
      DO K=1,IDOF
        IF (K.LT.IDOF.AND.ABS(R(K)-R(K+1)).LT.TOL) THEN     !- 'tol'
          I1(IC) = I1(IC) + 1
        ELSE
          V1(IC) = R(K)
          IC     = IC + 1
          I1(IC) = 1
        ENDIF
      ENDDO
      WRITE (IO,'(4(3X,I2,A,f9.4,A))') 
     &        (I1(I),'* (',V1(I),' )',I=1,IC-1)
      END

C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
C     String handling functions:
C       1/ Case conversion (all to UPPER/;lower)
C       2/ Length of a string (position of the last non-space) (cf F90)
C       3/ Start of a string (position of the first non-space)
C       4/ Extract tokens (as list of pointers)
C       5/ .. extract tokens, including what the seperator was
C                so check for '=', and even '!' comment marker
C-----------------------------------------------------------------------
      SUBROUTINE TO_UPPER (STRING)
C
C     Converts the given string to Upper case (ie. 'FredA' to FREDA'
C
      IMPLICIT NONE
      INTEGER I
      CHARACTER  STRING*(*), CH*1
      DO I=1,LEN(STRING)   !- len_trim is a bit faster 
        CH = STRING (I:I)
        IF (CH.GE.'a'.and.CH.LE.'z') STRING (I:I) = char (ichar(CH)-32)
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE TO_LOWER (STRING)
C
C     Converts the given string to LOWER case (ie. 'FredA' to freda')
C
      IMPLICIT NONE
      INTEGER I
      CHARACTER  STRING*(*), CH*1
      DO I=1,LEN(STRING)
        CH = STRING (I:I)
        IF (CH.GE.'A'.and.CH.LE.'Z') STRING (I:I) = char (ichar(CH)+32)
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE L_JUST (STRING)
C
C     Removes leading spaces from the given string =='Left Justify'
!     USed eg when ploting node number next to nodes oin Danplot
C     (be careful about STRING on both sides of the equation.)
C     cf TRIM () in F90  but that only removes trailing not leading blanks
!**OSBOLETE** as adjustl() in F90 does exactly the same
C        DJK 6-9-95
C
      IMPLICIT NONE
      INTEGER ISTR_BEG
      EXTERNAL ISTR_BEG
      CHARACTER  STRING*(*)
      STRING = STRING(ISTR_BEG(STRING):LEN(STRING))
      RETURN
      END

C-----------------------------------------------------------------------
      FUNCTION ISTR_BEG (STRING)
C
C     Returns the start of the given string (the first non-blank character)
! *probably *OBSOLETE** as we can use L_JUST() in almost all use cases
C
      IMPLICIT NONE
      INTEGER ISTR_BEG, I
      CHARACTER  STRING*(*)
      DO I=1,LEN(STRING)
        IF (STRING (I:I).NE.' ') THEN
          ISTR_BEG = I
          RETURN
        ENDIF
      ENDDO
      ISTR_BEG = 0      !- no start (eg a blank line)
      RETURN
      END

C-----------------------------------------------------------------------
      FUNCTION ISTR_END (STRING)
C
C     Returns the end of the given string (the last non-blank character)
C     - does this actualy work?
c possibly **OBSOLETE**  same as F90 Intrisic len_trim()
C
      IMPLICIT NONE
      INTEGER ISTR_END, I, ilength
      CHARACTER STRING*(*)
      ilength = len(string)     !- debug
c      print*,' string_length=',ilength,' string=',string
      DO I=ilength,1,-1
        IF (STRING (I:I).NE.' ') THEN
          ISTR_END = I
          RETURN
        ENDIF
      ENDDO
      ISTR_END = 0      !- no end (eg a blank line)
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE get_enum (LIST,TOKEN, num)
c  aka. FIND_STR_VAL
c
c     This returns the numeric value for the given token
c     typicaly used when parsing a data file
c     eg 'model=monot' when called here with 
C       'elastic=0,vonmise=1,MC=2, monot=10,cam-clay=15'
c        Dan Kidger 16-9-96
c
c.. hence call another routine to extract the RHS
c
      IMPLICIT NONE
      CHARACTER LIST*(*), token*(*)
c     character delims*3
      INTEGER IBEG,IEND 
      integer str_to_i, ifail, num,i, istr_end
      external str_to_i, istr_end

c     print*,':token=',token
      num = -1       !- 'default' value.
      Iend = 0       !- start at the beginning of the string

c--- if the given token is a valid integer then we return that
c    and so forget any string lookups
      num = str_to_i (token, ifail)
      IF (ifail.eq.0) return       !- was OK as a number.

      num = 0
      DO I=1,9999
c       print*,'::lhs=',token(1:istr_end(token))
        CALL GET_NEXT_TOKEN (LIST, IBEG,IEND, '=, ')
        IF (IBEG.LT.0) GOTO 99          !- token not found -> error
c       print*,'::rhs=',LIST(IBEG:IEND)
        IF (token(1:istr_end(token)).eq.LIST(IBEG:IEND)) then
c... got a 'hit' so pick up its numeric value
          CALL GET_NEXT_TOKEN (LIST, IBEG,IEND, ' =,')
c         print*,'::num=',LIST(IBEG:IEND)
          IF (IBEG.LT.0) GOTO 1        !- list exhausted (internal error)
            num = str_to_i (LIST(IBEG:IEND), ifail)
            if (ifail.ne.0) call myerror(3,'Invalid Integer found')
          RETURN
        ENDIF
        CALL GET_NEXT_TOKEN (LIST, IBEG,IEND, '=, ') !- skip over the number
      ENDDO

c----------- error conditions ------
    1 CONTINUE
C     ... token was not found .. so need to return an error condition?
      CALL MYERROR (3,
     &  'problem with finding the enumerated token(EXTRACT_PARAM)')
      RETURN
c-------------------------
   99 CONTINUE
C     ... token was not found .. so need to return an error condition?
c .. as is return num=-1 ?
c.. or return the error back to the application ?

      CALL MYERROR (1,'token not found (GET_ENUM)')
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE COUNT_TOKENS (STRING, ic, DELIMS)
c
c     This simply counts the number of 'words' in the given STRING
c     typicaly used when parsing the command-line or a data-file
c        Dan Kidger 16-9-96
c
      IMPLICIT NONE
      CHARACTER STRING*(*), DELIMS*(*)
      INTEGER IBEG,IEND,IC 

c     DELIMS = ', ='        !- or should I set the delims in the application?
      Iend = 0
      DO IC = 0,9999          !- form '0' cos maybe no tokens at all
        CALL GET_NEXT_TOKEN (STRING, IBEG,IEND, delims)
        if (ibeg.lt.0) return
      ENDDO
C.. here if an error or > 9999 tokens in this line!
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_NEXT_TOKEN (STRING, ibeg,iend,delims)
c
c     This returns the begin&end (IBEG,IEND) of the next 'word' 
C     in the given line (STRING).
c        Dan Kidger 16-9-96
c     Multiple calls are needed to extract all of the  TOKENs
c     IEND=-1 is returned when the list is exhasuted
c     the string is searched from IEND+1 onwards so we must set IEND=0
c     before the first call to this routine
c
      IMPLICIT NONE
      CHARACTER STRING*(*), DELIMS*(*), CH*1
      INTEGER IBEG,IEND,L 

      L = len(string)
      DO ibeg = iend+1,L    !-- find the start of the token --
        ch = string(ibeg:ibeg)
        if (index (delims,ch).lt.1) then    ! DELIM is usualy '= ,'
          goto 1                            !- EXIT if not a seperator
        ENDIF
      ENDDO
c.. so here if it never starts.. hence error return
      IBEG = -1
      RETURN

    1 CONTINUE

c.. check the following characters to look for the end of the token...
      DO iend = ibeg, L-1  
        ch = string(iend+1:iend+1)
        if (index (delims,ch).ge.1) goto 2     !- 
      ENDDO
    2 CONTINUE

      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_TOKENS (STRING, IARRAY, MAXTOKEN, NUMTOK)
C       *OBSOLETE* ?
C     From: STR2TOK by Kurt Kallblad <kurt.kallblad@bkl.lth.se>
C
C     Returns a list of pairs of start/end points of 
C     space or '=' seperated 'words' found within STRING.
C     (maybe pass the list of allowed seperators)
C       and also the comment character (=';' in .INI files)
C
c   Alternative stratagy:
c       - so the driving routine doesn't need an array of workspace 
c       - before first call set iend=1
c        routine returns ibeg and iend that delimit the substring
c        also flags if no more tokens
c       -- nice to pass it its set of seperators (ie. ' ' and '=', comma too
c       - also nice to pass it the comment character = '!' or ';'
c      ? should I allow embedded spaces within quotes - so return
c         the quote-stripped substring.
C
      IMPLICIT NONE
      character STRING*(*),cc          !- input
      integer MAXTOKEN
     &       ,IARRAY (2,maxtoken)      !- output as from/to
     &       ,NUMTOK,I                 !- # of tokens found
      logical intok                    !- local flag

      numtok = 0                       !- none found yet
      intok = .false.                  !- not in a token yet
      do i = 1, len(string)
         cc = string(i:i)
         if (index( ' =,',cc ).le.0) then  !- if still within a token
            if (intok) then                    !- still going?
               iarray(2,numtok) = i               ! mark end-point?
            elseif ( numtok .ge. maxtoken ) then
               numtok = -1         
               return                        !- too many tokens
            else
c               if (cc.eq.'!') return         !--> comment character <-- ?
c               if (cc.eq.';') return         !--> comment character <-- ?
               numtok = numtok + 1           !- start a new token.
               iarray(1,numtok) = i
               iarray(2,numtok) = i
               intok  = .true.               !- got start point
            endif
         elseif (intok) then
            intok = .false.
         endif    
      enddo
      return   
      end

c-----------------------------------------------------------------------
      function str_to_i (string, ifail)
c
c//   was SUBROUTINE GETINT()                                          //
c//   Purpose:               Retrieve an integer number from a string  //
c//   Reference:             Proprietory to I.J. Hutchinson            //
c.. note to allow hexadecimal too eg. 0xff00aa  good for colours.
c   Dan Kidger 15-9-96
c                                                                             
      integer str_to_i, num, radix, ifail
      character         string*(*), ch*1, symbols*17
      data symbols/'0123456789abcdef '/          ! valid digits.

c-------- Initialise values -----------
      isign  = 1           ! default is +ve
      radix = 10           ! assume decimal .. may be hex, binary,octal
      num  = 0             ! where we build the number

c-------- loop the string --------

      l    = len(string)         !- =LENG
      do i=1, l
        ch = string(i:i)
        IF (CH.GE.'A'.and.CH.LE.'Z') CH = char (ichar(CH)+32)
        if (ch.eq.' '.or.ch.eq.char(0)) goto 100   !- end of string

        ival = index(symbols(1:radix),ch) -1    !- so 0->9, =-1 if invalid)
        if (i.eq.1.and.ch.eq.'+') then          ! +ve sign
          isign = +1
        elseif (i.eq.1.and.ch.eq.'-') then      ! -ve sign
          isign = -1
        elseif (i.eq.1.and.ch.eq.'z') then      ! binary prefix ?
          radix = 2
        elseif (i.eq.1.and.ch.eq.'h') then      ! hexadecimal
          radix = 16
        elseif (i.eq.1.and.ch.eq.'#') then      ! hexadecimal
          radix = 16
        elseif (i.eq.2.and.ch.eq.'x'.and.num.eq.0) then ! hex '0x' prefix
          radix = 16

        elseif (ival.ge.0) then          !- numeric symbol found.
          num = num*radix + ival
        else
          ifail = 1
c         num=0    ?   or leave at what we managed to get ?
          str_to_i=0
         return
        endif
      enddo

  100 ifail=0
      str_to_i = isign * num
      return    !- if an erro, jump to here
      end

c-----------------------------------------------------------------------
      function str_to_real (string,ifail)
c
c     Extracts a real number from a string
c      .. becomes a no-op in Fortran90 'cos we are allowed to do a READ
c       DJK 11-19-96
C
      real str_to_real
      character string*(*)
      real value
      integer ifail

      value=0.     !- dummy value
      read (string,*,iostat=ifail) value     !- non-standard conforming F77
      str_to_real = value
      end !function str_to_real

c-----------------------------------------------------------------------
      function str_to_f (string, value, found)
c
c Purpose:               Retrieve a double precision number from a string  
c Reference:             Proprietory to I.J. Hutchinson                    
c < from FRAME program -1988 ?>
c
      real str_to_f, value
      character*(*)     string
      logical           found
      integer istack(75)            !- buffer space
      real x
      logical           exp,dot

      found=.false.
      value= 0.
      l    = len(string)
c     l    = length(string) 
      if (l.eq.0) goto 999      !- string was empty

c---------- Copy number into string ---------
c.. realy just prefilters out embedded spaces and commas
      ic=0
      do 10 i=1,l
         n          =ichar(string(i:i)) 
         string(i:i)=' '                     ! why zap.. never used again ???
         if (n.eq.32.or.n.eq.44) goto 20     ! space or comma = e_o_string
         ic           =ic+1
         istack(ic)   =n
   10 continue
c                           -- so now istack is an (integer) set of digits
20    if(ic.eq.0)goto999

c---- Initialise values ------ 

      exp    =.false.            !- no 'E' found
      dot    =.false.            !- no '.' found
      isign  = 1                 !- mantisa is+ve
      jsign  = 1                 !- exponent is +ve
      icount = 0
      x      = 0.                !- the number
      m      = 0
c     
c.. mote that we can be in one of 2 modes : first 'mantisaa'
c.. and then optionally 'exponent'
c
c--- if string = $(foo) then sub-call the recover_variable routine

      i=0 
 40   i=i+1         !--- loop ---
      if (i.gt.ic) goto 100                   ! EXIT
      n = istack(i)
      if (n.eq.32.or.n.eq.0) goto 100         !- EXIT on end of string
      if (n.eq.46) then              !- '.' goes into the fractional part
         dot = .true.
         goto 40                 
      endif
      if (n.eq.43) goto 40           !- '+' sign is a no-op
      if (n.eq.45) then              !- '-' change sign of the :
         if (     exp) isign=-1      !-     - the exponent
         if (.not.exp) jsign=-1      !-     - or the mantissa
         goto 40
      endif
      if (n.eq.69.or.n.eq.101) then  !   'e' or 'E'
         exp = .true.
         goto 40
      endif
      if (exp.and.(n.eq.88.or.n.eq.120)) goto 40   !- skip on  'X'or 'x'
      if (exp.and.(n.eq.80.or.n.eq.112)) goto 40   !- 'Pp'  (but why ? 
      if (n.lt.48.or.n.gt.57           ) goto 999  !- not 0->9 =invalid
c
      if (exp) then
         m = (n-48)+m*10                 !- build up exponent
      else
         x = real(n-48)+x*10.            !- build up mantissa
         if (dot) icount=icount+1        !- count dec. places
      endif
      goto 40
c---- end of 40 loop back ----

c--- end of string ---
100   value = jsign * (x * 10.**(isign*m-icount)) 
      found=.true.

c.. if jump to 999 then an error so value is junk!
999   str_to_f = value     
      return
      end
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------


C-----------------------------------------------------------------------
      SUBROUTINE BAR_LINE (IPERCENT,IPERCENT_OLD, iwidth )
C
C     Shows the status bar 0 to 100%
c        eg. ********.......27%............
c..   cf simple ASCII v. ANSI v. Windoze
C         Dan Kidger 10-3-97

c.. hmm nicer to only write where the strings *change*
c..  ie. only need to write the last *, then write the %number

      INTEGER ::  IPERCENT,IPERCENT_OLD, IWIDTH, stderr=-1
      CHARACTER LINE*255
      if (stderr==-1) call my_getstderr(stderr)   ! first pass

      IFRAC = NINT (IWIDTH * (IPERCENT/100.))
      DO I=1,IFRAC
        LINE(I:I) = '*'
      ENDDO
      DO I=IFRAC+1,IWIDTH
        LINE(I:I) = '.'
      ENDDO
      ip = iwidth/2+1
      line(ip:ip)='%'

      ival = ipercent
  12  ip = ip -1
        line(ip:ip) = char(ichar('0')+mod(ival,10) )
        ival = ival/10
      if (ival.ne.0) goto 12
!     WRITE (*,'(A,A)') LINE(1:IWIDTH), char(27)//'[A'
      WRITE (stderr,'(A,A)') LINE(1:IWIDTH), char(27)//'[A'
      if (ipercent.eq.100) write(stderr,*)
      IPERCENT_OLD= IPERCENT
      RETURN
      END
c-----------------------------------------------------------------------
