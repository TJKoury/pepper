!$Procedure TOSTDO ( To Standard Output)

      subroutine tostdo ( line )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Write a line of text to standard output.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      implicit none

      character(*)          line

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LINE       I   is a line of text to be written to standard output
!
! $ Detailed_Input
!
!     LINE     is a character string containing text to be written
!              to standard output.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while attempting to write to the standard
!         output, the error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is a macro for the subroutine call
!
!        CALL WRITLN ( LINE, STDOUT )
!
!     Where STDOUT is the logical unit connected to standard output.
!
! $ Examples
!
!     Suppose you need to create a message to be printed on the
!     user's terminal. Here is how to use TOSTDO to handle this
!     task.
!
!        CALL TOSTDO ( 'Hello. '         )
!        CALL TOSTDO ( 'My Name is HAL.' )
!        CALL TOSTDO ( 'I became operational January 12, 1997 on the ' )
!        CALL TOSTDO ( 'campus of the University of Illinois in '      )
!        CALL TOSTDO ( 'Urbana, Illinois.' )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 18-SEP-1996 (WLT)
!
!-&

! $ Index_Entries
!
!     Write a line of text to standard output.
!
!-&

      integer(ip)           stdout
      logical               first
      save

      data                  first  / .true. /

      if ( first ) then
         call stdio ( 'STDOUT', stdout )
         first = .false.
      end if

      call writln ( line, stdout )
      return

      end
