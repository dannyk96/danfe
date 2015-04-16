program publish
!
!  This parses the file 'MASTER1.TXT' and creates the various
!   documentation files.
!     Dan Kidger 26-8-98

  integer :: ndata,ndescription, nexample,nadvanced,ncaveats,nseealso, &
             nrevision
  character :: keyword*50,    & !- the *Keyword itself
        summary*255,          & !- one line summary
        data(50)*255,         & !- list of data items (fixed format)
        description (400)*255,& !- how to use it.
        example(50)*255,      & !- short example (fixed format)
        advanced (400)*255,   & !- more complex instructions
        caveats (50)*255,     & !- limitations on use
        seealso (50)*255,     & !- cross references
        flags(10)*255,        & !- various internal flags
        revision(20)*255        !- revision history

  character :: keynames(800)*50   !- complete list
  character :: file*80,       &   !- generic filename
               page*50            !- Each page
!              link*50            !- and its cross reference(s)
   character :: token*255         !- generic read-in line
!  character ::keyword_last*50,  & !- cross-references
!              keyword_next*50,  &
!              page_last*40,     &
!              page_next*40

!----- 0: initialise --------
open (10,file='keywords.txt', action='read')   !- database
open (20,file='keywords/index.html' , action='write')  !- master index

!----- 0a: count the number of keywords --------
!  should also store the summary etc. ? so we can write the index.html
!  sorted by any column eg. alphebeticaly
  nkeywords=0
  do ikeyword=1,999
  do
    read (10,'(a)',iostat=ios) token
    if (ios.ne.0) exit            !- end of file (w. empty keyword)
    if (token(1:3)=='.TH') then
      nkeywords = nkeywords+1
      keynames(nkeywords)=trim(token(5:))   !- includes '*'
    endif  !- found a word
  enddo
  enddo
  rewind(10)
  print*,nkeywords,' Keywords were found'

!------- 0b: write the output file headers ----------------
   write(20,'(a)') '<html>','<head><title>KEYWORD descriptions</title>'
   write(20,'(a)') '<link rel="stylesheet" type="text/css" href="keywords.css">'
   write(20,'(a)') '</head>'
   write(20,'(a)') '<body bgcolor="#efefff" text="#000000">'

   write(20,'(a,a,a)') '<font face="arial" size=+2 color="FF0000">', &
      'Dansoft Keyword definitions</font><p>'
   write(20,'(a)') '<br><hr>'
   write(20,'(a)') '<table bgcolor="#ffffff">'
   write(20,'(a)') '<tr bgcolor="ff7f7f">'
   write(20,'(a)') '<td>No.</td>'
   write(20,'(a)') '<td>Keyword</td>'
   write(20,'(a)') '<td>Summary</td>'
   write(20,'(a)') '<td>#lines</td>'
   write(20,'(a)') '</tr>'


!keyword=' '
!keyword_next=' '
!page=' '
!page_next=' '
nkeywords_only= nkeywords
!nkeywords_only= 25
keywords: do ikeyword=1,nkeywords_only


!---- 2: read a complete record ---------
  call r_record ()              !- returns SUMMARY,DATA,DESCRIPTION, etc.
  print*,'<>',ikeyword,trim(keyword )
  if (keyword==' ') exit    !- all done
!---- 3: build the html filename --------
  call make_file(keyword,ikeyword,page)

!---- 4: write out the index record ----
  write (20,'(a,i4,a,  5a,   3a)') &
   '<tr><td><font size=-1>',ikeyword,'</font></td>',&
   '<td><a href="',trim(page),'"><font size=-1 color="#7f0000"><b>',&   !- dark red
    trim(keyword),'</b></font></a></td>',&

!.. write the scope flags too?

!-- write the entry length
   '<td><font size=-1 >',trim(summary),'</font></td>'
  write(20,'(a)') '<td>'
  write(20,'(i4)') nDATA + nDESCRIPTION + nEXAMPLE + nADVANCED + nCAVEATS
  write(20,'(a)') '</td>'
  write(20,'(a)') '</tr>'


!---- 5: write out the indiviual manual entry ----
  open (25,file="keywords/"//trim(page),action='write',iostat=ios)
  if (ios.ne.0) stop 'failed to open HTML file'
  call wr_entry ()
  close(25)

enddo keywords
  write(20,'(a)')'</table>'

contains
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------
subroutine r_record ()
!--- This extracts the next keyword and its associated data ----
  character :: code*3, token*255
  keyword=' '                      !- assume nothing

  do irecord=1,99

  do
    read (10,'(a)',iostat=ios) token
    if (ios.ne.0) return            !- end of file (w. empty keyword)
    if (token/=' ') exit            !- found a word
  enddo
  if (token(1:1).ne.'.') then
    print*,'token (',token(1:len_trim(token)),') does not start with a <.>'
    stop
  endif
! print*,'token=',trim(token)

!---- 2: handle each token seperately ------
  code=token(1:3)
  if (code=='.HR') then          !- horizontal rule - just skip?

  elseif (code=='.TH') then
    keyword=token(5:)
    read(10,'(A)') summary
  elseif (code=='.SH') then
    select case(token(5:))
   case ('DATA')                 !---
     call r_block (DATA,nDATA)
   case ('DESCRIPTION')          !---
     call r_block (DESCRIPTION,nDESCRIPTION)
   case ('EXAMPLE')              !---
     call r_block (EXAMPLE,nEXAMPLE)
   case ('ADVANCED')             !---
     call r_block (ADVANCED,nADVANCED)
   case ('CAVEATS')              !---
     call r_block (CAVEATS,nCAVEATS)
   case ('SEEALSO')              !---
     call r_block (SEEALSO,nSEEALSO)
   case ('FLAGS')                !---
     call r_block (FLAGS,nFLAGS)
   case ('REVISION')             !---
     call r_block (REVISION,nREVISION)
   case ('END')                  !- end so record so we can write it out.
     return
   case default
     stop 'Unknown ".SH,.TH,.HR" token found'
   end select
  endif
  enddo    !- loop each section of this page
end subroutine r_record
!-----------------------------------------------------------------------------
subroutine r_block (record, nrecord)
!
!  This reads a record entry into a character array
!
character :: record(:)*255    !(*)*(*)
nrecord=0
do irecord = 1, 999
  read(10,'(a)') record(irecord)
  if (record(irecord)(1:1)=='.') then
    nrecord=irecord-1
    backspace(10)
    return
  endif
enddo
stop ' >999 lines found in this record!'

end subroutine r_block

!-----------------------------------------------------------------------------
subroutine wr_entry ()
!
!  This write out the record as a html file.
!
! ? write the title in a table so can colour the background ?
! ? write the DANFE logo as a GIF too?
!
   write(25,'(a)') '<html>','<head><title>',trim(keyword),'</title>'
   write(25,'(a)') '<link rel="stylesheet" type="text/css" href="horse.css">'
   write(25,'(a)') '</head>'

   write(25,'(a)') '<body bgcolor="#efefff" text="#000000">'

   write(25,'(a)') '<a href=index.html>Index</a>'
   if (ikeyword>1) then
     call make_file (keynames(ikeyword-1), ikeyword-1,file)
     write(25,'(5a)') '| Previous <a href="',file,'">', &
     keynames(ikeyword-1),'</a>'
   else                                         !- gray control
     write(25,'(a)') '| <font color="7f7f7f">Previous</font>'
   endif
   if (ikeyword<nkeywords) then
     call make_file (keynames(ikeyword+1), ikeyword+1,file)
     write(25,'(5a)') '| Next <a href="',file,'">', &
     keynames(ikeyword+1),'</a>'
   else                                         !- gray control
     write(25,'(a)') '| <font color="7f7f7f">Next</font>'
   endif


   write(25,'(a)') '<br>'
   write(25,'(a)') '<table width="100%" cellpadding=5 celllspacing=0>',&
      '<tr><td align="right" bgcolor="#ffafaf">'
   write(25,'(a,a,a)') '<font face="arial" size=+2 color="000040">', &
      trim(keyword),'</font>'
   write(25,'(a)') '</td></tr>'
   write(25,'(a,a,a)') '<tr><td bgcolor="#ffdfdf"><font face="arial" size=+1 color="000000">', &
      '<i>',trim(summary),'</i></font></td>'
   write(25,'(a)') '</table>'
!   write(25,'(a)') '<hr>'

   write(25,'(a)')
   write(25,'(a)') '<h3>DATA</h3><p>'
   write(25,'(a)') '<pre>'
   write(25,'(a)') (trim(data(i)),i=1,ndata)
   write(25,'(a)') '</pre>'

   write(25,'(a)')
   write(25,'(a)') '<h3>DESCRIPTION</h3><p>'
   do i=1,ndescription
     if (description(i)==' ') then         !- blank lines between paragraphs
       write(25,'(a)') '<p>'
     elseif (description(i)(1:1)==' ') then !- preserve line breaks if indented
       write(25,'(a)') '<br>'
     endif
     write(25,'(a)') trim(description(i))
   enddo
   write(25,'(a)') '<p>'

   write(25,'(a)')
   write(25,'(a)') '<h3>EXAMPLE</h3><p>'
   write(25,'(a)') '<pre>'
   write(25,'(a)') (trim(example(i)),i=1,nexample)
   write(25,'(a)') '</pre>'

   IF (NADVANCED>0) THEN
   write(25,'(a)')
   write(25,'(a)') '<h3>ADVANCED</h3><p>'
   write(25,'(a)') (trim(advanced(i)),i=1,nadvanced)
   write(25,'(a)') '<p>'
   ENDIF

   IF (NCAVEATS>0) THEN
   write(25,'(a)')
   write(25,'(a)') '<h3>CAVEATS</h3><p>'
   write(25,'(a)') (trim(caveats(i)),i=1,ncaveats)
   write(25,'(a)') '<p>'
   ENDIF

!.. This section needs to split records at commas, then write each as a
!.. <A HREF=''></a>

   write(25,'(a)')
   write(25,'(a)') '<h3>SEE ALSO</h3><p>'
!  write(25,'(a)') (trim(seealso(i)),i=1,nseealso)
!  write(25,'(a)') '<p>'
   do isee=1,nseealso        !- usu. only 1 line
!    print*,seealso(isee)
!    seealso(isee) = '   '//seealso(isee)(1:)
!   iend=0                    !- scan from the begining of it.
!   do j=1,99
!     call get_next_token(seealso(isee),ibeg,iend,'= ,')
!     if (ibeg.le.0) exit   !- all done
!     print*,ibeg,iend,seealso(isee)(ibeg:iend)
!     call find_keyword(seealso(isee)(ibeg:iend),ikey2)
!     if (ikey2<=0) print*,'reference not found'
!     call make_file (seealso(isee)(ibeg:iend),ikey2,file)
!    write(25,'(4a)') '<a href="',file,'">',seealso(isee)(ibeg:iend),'</a>, '
!   enddo
!   write(25,'(4a)') '<a href="',file,'">',seealso(isee)(ibeg:iend),'</a>, '

     call find_keyword (trim(seealso(isee)),ikey2)
     if (ikey2>0.and.ikey2<=nkeywords_only) then
       call make_file (trim(seealso(isee)),ikey2,file)
       write(25,'(4a)') '<a href="',file,'">',trim(seealso(isee)),'</a>, '
     else        !- broken link  (gray?)
       write(*,'(a,a,a)') &
         trim(keyword),': Link not found:',trim(seealso(isee))
       write(25,'(a)') '<font color="#7F7F7F">'//trim(seealso(isee))//'</font>'
     endif
   enddo
  write(25,'(a)') '<p>'
   write(25,'(a)') '<hr><font size=-3>please contact Dr. Dan Kidger', &
    '<a href=mailto:d.kidger@man.ac.uk>d.kidger@man.ac.uk</a> if you find', &
    ' any omissions or corrections</font>'

   write(25,'(a)') '</body></html>'

end subroutine wr_entry

!-----------------------------------------------------------------------------
subroutine find_keyword (keyword,ikey2)
!
! This finds the position of the given keyword in the long table
!  - here we do linear search - binary chop would be better
 implicit none
 character :: keyword*(*)
 integer :: i, ikey2
 do i=1, nkeywords
   if (keyword==keynames(i)) then
     ikey2=i
     return
   endif
 enddo
 ikey2=0   !- not found (should never happen)

end subroutine find_keyword

!-----------------------------------------------------------------------------
subroutine make_file (keyword,ikeyword,file)
!
!   form A is suitable for 8+3 DOS machines
!   form B is for generic Unix/winNT
!   form C is purely numeric - this lets Next+Previous work
!   form D encode the longer name into a unique (CRC?) code
!
  implicit none
  integer :: i, icode, icode2, ikeyword
  character :: keyword*(*), file*(*)
! form A
! write (file,'(a,i3.3,a)')  keyword(2:6),ikeyword,'.html'
! form B
  write (file,'(a,a)')  trim(keyword(2:)),'.html'
! form C
! write (file,'(a,i3.3,a)')  'danfe',ikeyword,'.html'
! form D
!  icode=0
!  do i=2,len(keyword)
!    icode2= (ichar(keyword(i:i))-ichar('A'))
!    icode=26*(i-2)+icode2
!  enddo
! write (file,'(a,i6.6,a)') 'D',icode,'.html'

end subroutine make_file
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

end program publish
!-----------------------------------------------------------------------------
      SUBROUTINE GET_NEXT_TOKEN (STRING, ibeg,iend,delims)
!
!     This returns the begin & end (IBEG,IEND) of the next 'word'
!     in the given line (STRING).
!        Dan Kidger 16-9-96
!     Multiple calls are needed to extract all of the  TOKENs
!     IEND=-1 is returned when the list is exhasuted
!     the string is searched from IEND+1 onwards so we must set IEND=0
!     before the first call to this routine
!
      IMPLICIT NONE
      CHARACTER :: STRING*(*), DELIMS*(*), CH*1
      INTEGER :: IBEG,IEND,L
      character :: delims2*5, string2*40='cats, dogs, *mice, *men'

      delims2=delims   ! F90 hacks

      L = len(string2)
      print*,'L=',L,' delims=',delims,len(delims),delims2,len(delims2)
      DO ibeg = iend+1,L    !-- find the start of the token --
        print*,ibeg,'..'
        ch = string2(ibeg:ibeg)
        if (index (delims,ch).lt.1) then    ! DELIM is usualy '= ,'
!       if (index (delims2,ch).lt.1) then    ! DELIM is usualy '= ,'
!       if (index (' ,',ch).lt.1) then    ! DELIM is usualy '= ,'
          goto 1                            !- EXIT if not a seperator
        ENDIF
      ENDDO
!.. so here if it never starts.. hence error return
      IBEG = -1
      RETURN

    1 CONTINUE

!.. check the following characters to look for the end of the token...
      DO iend = ibeg, L-1
        print*,iend,'..'
        ch = string2(iend+1:iend+1)
        print*,'<',ch,'>',delims,'ee', index(', ',ch),'ff'
!       if (index (delims,ch).ge.1) goto 2     !-
!       if (index (delims2,ch).ge.1) goto 2     !-
        if (index (' ,',ch).ge.1) goto 2     !-
      ENDDO
    2 CONTINUE
      print*,'ibeg,iend=',ibeg,iend
      RETURN
      END
!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

