!$Procedure RDNBL ( Read non-blank line )

      subroutine rdnbl ( file, line, eof )

! $ Abstract
!
!     Read the next non-blank line of text from a text file.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     FILES
!     TEXT
!
! $ Declarations

      implicit none

      character*(*)       file
      character*(*)       line
      logical             eof

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FILE       I   Input text file.
!     LINE       O   Next non-blank line from the input text file.
!     EOF        O   End-of-file indicator.
!
! $ Detailed_Input
!
!     FILE     is the name of the text file from which the next
!              line is to be read. If the file is not currently
!              open, it is opened with a logical unit determined
!              at run time, and the first line of the file is
!              returned. Otherwise, the next line not yet read
!              from the file is read and returned.
!
! $ Detailed_Output
!
!     LINE     is next non-blank line of text in the specified file.
!
!     EOF      is .TRUE. when the end of the file is reached, and is
!              otherwise .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If either the end of the file is reached or an error occurs
!         before a non-blank line is found, LINE is blank.
!
! $ Files
!
!     See input FILES.
!
! $ Particulars
!
!     RDNBL simply calls RDTEXT until one of two things happens:
!
!        1. A non-blank line is found (in which case the line
!           is returned).
!
!        2. The end of the file is reached (in which case the
!           file is closed, a blank line is returned, and the
!           end-of-file indicator becomes .TRUE.)
!
! $ Examples
!
!     Let FILE.1 contain the following lines.
!
!        Mary had a little lamb
!
!        Everywhere that Mary went
!
!
!
!        Its fleece was white as snow.
!        The lamb was sure to go.
!
!     Then the code fragment
!
!        DO I = 1, 4
!           CALL RDNBL ( 'FILE.1', LINE, EOF )
!           WRITE (*,*) LINE
!        END DO
!
!     produces the following output:
!
!        Mary had a little lamb
!        Everywhere that Mary went
!        Its fleece was white as snow.
!        The lamb was sure to go.
!
!     In fact, the following code fragment removes all of the blank
!     lines from an arbitrary text file (FILE).
!
!        CALL RDNBL ( FILE, LINE, EOF )
!
!        DO WHILE ( .NOT. EOF )
!           WRITE (*,*) LINE( : RTRIM(LINE) )
!
!           CALL RDNBL ( FILE, LINE, EOF )
!        END DO
!
!     Note that because RDNBL calls RDTEXT, calls to either routine
!     can be interspersed. For example, RDNBL can be used to skip
!     blank lines at the beginning of the file, leaving the rest to
!     be processed:
!
!        CALL RDNBL ( FILE, LINE, EOF )
!
!        DO WHILE ( .NOT. EOF )
!           < do something with LINE >
!
!           CALL RDTEXT ( FILE, LINE, EOF )
!        END DO
!
! $ Restrictions
!
!     1)  Any restrictions that apply to RDTEXT apply to RDNBL as well.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 07-AUG-1994 (IMU)
!
!-&


! $ Index_Entries
!
!     read a non-blank line from a text file
!
!-&


!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'RDNBL' )
      end if

!
!     Return as soon as a non-blank line is found. Otherwise, keep
!     looking until either the end of the file is reached or RDTEXT
!     manages to fail.
!
      call rdtext ( file, line, eof )

      do while ( ( .not. eof )  .and.  ( .not. failed() ) )

         if ( line .ne. ' ' ) then
            call chkout ( 'RDNBL' )
            return

         else
            call rdtext ( file, line, eof )
         end if

      end do

!
!     Didn't find anything?
!
      line = ' '

      call chkout ( 'RDNBL' )
      return
      end
