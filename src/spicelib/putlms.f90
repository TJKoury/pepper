!$Procedure PUTLMS ( Store Long Error Message )

      subroutine putlms ( msg )

! $ Abstract
!
!     Store a long error message.
!
!     PUTLMS is a low-level data structure access routine.
!     DO NOT CALL THIS ROUTINE. USE SETMSG, NOT PUTLMS, TO SET THE
!     CURRENT LONG ERROR MESSAGE.
!
! $ Required_Reading
!
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations

      implicit none

      include 'errhnd.inc'

      character(*)           msg

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MSG        I   A long error message.
!
! $ Detailed_Input
!
!     MSG      is the current long error message. This value will be
!              saved.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     LMSGLN   is the maximum length of the long error message. See
!              the include file errhnd.inc for the value of LMSGLN.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     DO NOT CALL THIS ROUTINE.
!
!     This routine should be used only by routines within the SPICELIB
!     error handling system. Other routines should use SETMSG to set
!     the long error message.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  Calls to this routine by routines outside of the SPICELIB
!         error handling system may interfere with error processing.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 27-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added
!        $Index_Entries entry.
!
!-    SPICELIB Version 1.1.0, 29-JUL-1997 (NJB)
!
!        Maximum length of the long error message is now represented
!        by the parameter LMSGLN. Miscellaneous header fixes were
!        made. Some indentation and vertical white space abnormalities
!        in the code were fixed. Some dubious comments were deleted
!        from the code.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&

! $ Index_Entries
!
!     store long error message
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 29-JUL-1997 (NJB)
!
!        Maximum length of the long error message is now represented
!        by the parameter LMSGLN. Miscellaneous header fixes were
!        made. Some indentation and vertical white space abnormalities
!        in the code were fixed. Some dubious comments were deleted
!        from the code.
!
!-    Beta Version 1.0.1, 08-FEB-1989 (NJB)
!
!        Warnings added to discourage use of this routine in
!        non-error-handling code.
!
!-&

!
!     Local Variables:
!

!
!     The current long error message:
!
      character(lmsgln)     savmsg

      save                  savmsg

!
!     Initial values:
!
      data    savmsg      / ' ' /

!
!     Executable Code:
!
      savmsg = msg

      return

!$Procedure GETLMS ( Get Long Error Message )

      entry getlms ( msg )

! $ Abstract
!
!     Return the value of the current long error message.
!
! $ Required_Reading
!
!     ERROR
!
! $ Keywords
!
!     ERROR
!
! $ Declarations
!
!    CHARACTER*(*)          MSG
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MSG        O   The current long error message.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     MSG      is the current long error message. See the
!              "required reading" file for a detailed discussion
!              of error messages.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  However, this routine is part of the SPICELIB error
!         handling mechanism.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See the required reading file for details of error
!     processing.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.3, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added
!        $Index_Entries entry. Removed unnecessary $Revisions section.
!
!-    SPICELIB Version 1.0.2, 29-JUL-1997 (NJB)
!
!        Maximum length of the long error message is now represented
!        by the parameter LMSGLN. Miscellaneous header fixes were
!        made. Some indentation and vertical white space abnormalities
!        in the code were fixed. Some dubious comments were deleted
!        from the code.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB)
!
!-&

! $ Index_Entries
!
!     get current long error message
!
!-&

!
!     Grab the saved long message:
!
      msg = savmsg

      end
