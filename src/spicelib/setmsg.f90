!$Procedure SETMSG  ( Set Long Error Message )

      SUBROUTINE SETMSG ( MSG )

! $ Abstract
!
!     Set the value of the current long error message.
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

      CHARACTER*(*)                 MSG

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MSG        I   A long error message.
!
! $ Detailed_Input
!
!     MSG      is a "long" error message.
!
!              MSG is a detailed description of the error.
!              MSG is supposed to start with the name of the
!              module which detected the error, followed by a
!              colon. Example:
!
!                 'RDTEXT:  There are no more free logical units'
!
!              Only the first LMSGLN characters of MSG are stored;
!              any further characters are truncated.
!
!              Generally, MSG will be stored internally by the SPICELIB
!              error handling mechanism. The only exception
!              is the case in which the user has commanded the
!              toolkit to ``ignore'' the error indicated by MSG.
!
!              As a default, MSG will be output to the screen.
!              See the required reading file for a discussion of how
!              to customize toolkit error handling behavior, and
!              in particular, the disposition of MSG.
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
!     Error free.
!
!     1)  This routine does not detect any errors.
!
!         However, this routine is part of the interface to the
!         SPICELIB error handling mechanism. For this reason,
!         this routine does not participate in the trace scheme,
!         even though it has external references.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The SPICELIB routine SIGERR should always be called
!     AFTER this routine is called, when an error is detected.
!
!     The effects of this routine are:
!
!        1. If acceptance of a new long error message is
!            allowed:
!
!            MSG will be stored internally. As a result,
!            The SPICELIB routine, GETMSG, will be able to
!            retrieve MSG, until MSG has been ``erased''
!            by a call to RESET, or overwritten by another
!            call to SETMSG.
!
!
!        2. If acceptance of a new long error message is not allowed,
!            a call to this routine has no effect.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a user-defined error message, including both the
!        short and long messages, providing the value of an integer
!        and a double precision variables within the long message,
!        and signal the error.
!
!
!        Example code begins here.
!
!
!              PROGRAM SETMSG_EX1
!              IMPLICIT NONE
!
!        C
!        C     Set long error message, with two different MARKER
!        C     strings where the value of the variables will go.
!        C     Our markers are '#' and 'XX'.
!        C
!              CALL SETMSG ( 'LONG MESSAGE. Invalid operation value. '
!             .         //   '  The value was #.  Left endpoint '
!             .         //   'exceeded right endpoint.  The left '
!             .         //   'endpoint was:  XX.'                     )
!
!        C
!        C     Insert the integer number where the # is now.
!        C
!              CALL ERRINT ( '#',  5  )
!
!        C
!        C     Insert a double precision number where the XX is now.
!        C
!              CALL ERRDP  ( 'XX', 910.26111991D0 )
!
!        C
!        C     Signal the error.
!        C
!              CALL SIGERR ( 'SPICE(USERDEFINED)' )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        ============================================================***
!
!        Toolkit version: N0066
!
!        SPICE(USERDEFINED) --
!
!        LONG MESSAGE. Invalid operation value. The value was 5. Left***
!        exceeded right endpoint. The left endpoint was: 9.1026111991***
!
!        Oh, by the way:  The SPICELIB error handling actions are USER-
!        TAILORABLE.  You can choose whether the Toolkit aborts or co***
!        when errors occur, which error messages to output, and where***
!        the output.  Please read the ERROR "Required Reading" file, ***
!        the routines ERRACT, ERRDEV, and ERRPRT.
!
!        ============================================================***
!
!
!        Warning: incomplete output. 7 lines extended past the right
!        margin of the header and have been truncated. These lines are
!        marked by "***" at the end of each line.
!
!
!        Note that the execution of this program produces the error
!        SPICE(USERDEFINED), which follows the NAIF standard as
!        described in the ERROR required reading.
!
! $ Restrictions
!
!     1)  SIGERR must be called once after each call to this routine.
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
!-    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example based on existing fragments.
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
!     set long error message
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.0.2, 29-JUL-1997 (NJB)
!
!        Maximum length of the long error message is now represented
!        by the parameter LMSGLN. Miscellaneous header fixes were
!        made. Some indentation and vertical white space abnormalities
!        in the code were fixed. Some dubious comments were deleted
!        from the code.
!
!-     Beta Version 1.1.0, 17-FEB-1989 (NJB)
!
!         Declarations of the unused variable STAT and unused function
!         ACCEPT removed.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               ALLOWD

!
!     We store the long error message only when updates
!     of the long message are allowed:
!
      IF ( ALLOWD() ) THEN

         CALL PUTLMS ( MSG )

      END IF

      END
