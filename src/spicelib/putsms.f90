!$Procedure PUTSMS ( Store Short Error Message )

      SUBROUTINE PUTSMS ( MSG )

! $ Abstract
!
!     Store a short error message.
!
!     PUTSMS is a low-level data structure access routine.
!     DO NOT CALL THIS ROUTINE. USE SIGERR, NOT PUTSMS, TO SIGNAL
!     ERRORS.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      IMPLICIT NONE

      INCLUDE 'errhnd.inc'

      CHARACTER*(*)          MSG

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MSG        I   A short error message.
!
! $ Detailed_Input
!
!     MSG      is the current short error message. This value will be
!              saved.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     SMSGLN   is the maximum length of the short error message. See
!              the include file errhnd.inc for the value of SMSGLN.
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
!     This is a data structure access routine for the
!     toolkit short error message. This routine should be
!     used for no other purpose; in particular, it should
!     not be used to signal errors. Use SIGERR for that.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  Calls to this routine by routines other than the
!         SPICELIB error handling routines may interfere
!         with error processing.
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
!-    SPICELIB Version 1.2.0, 27-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added
!        $Index_Entries entry.
!
!-    SPICELIB Version 1.1.0, 29-JUL-1997 (NJB)
!
!        Maximum length of the short error message is now represented
!        by the parameter SMSGLN. Miscellaneous header fixes were
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
!     store short error message
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 29-JUL-1997 (NJB)
!
!        Maximum length of the short error message is now represented
!        by the parameter SMSGLN. Miscellaneous header fixes were
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
!     The current short error message:
!
      CHARACTER*(SMSGLN)    SAVMSG

      SAVE                  SAVMSG

!
!     Initial values:
!
      DATA    SAVMSG      / ' ' /

!
!     Executable Code:
!
      SAVMSG = MSG

      RETURN




!$Procedure GETSMS ( Get Short Error Message )

       ENTRY GETSMS ( MSG )

! $ Abstract
!
!     Return the value of the current short error message.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
!     IMPLICIT NONE
!
!     CHARACTER*(*)          MSG
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MSG        O   The current short error message.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     MSG      is the current short error message. See the
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
!     See the required reading file for details of error processing.
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
!-    SPICELIB Version 1.2.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added
!        $Index_Entries entry. Removed unnecessary $Revisions section.
!
!-    SPICELIB Version 1.1.0, 29-JUL-1997 (NJB)
!
!        Maximum length of the short error message is now represented
!        by the parameter SMSGLN. Miscellaneous header fixes were
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
!     get current short error message
!
!-&


!
!     Grab saved short message:
!
      MSG = SAVMSG

      END
