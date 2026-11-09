!$Procedure ACCEPT ( Accept New Long Error Message )

      logical function accept ( ok )

! $ Abstract
!
!     Indicate to the SPICELIB error handling mechanism whether or not
!     a replacement or modification of the long error message can be
!     accepted.
!
!     DO NOT CALL THIS ROUTINE.
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

      logical               ok

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         I   Indicates whether long error msg changes are ok.
!
!     The function takes an UNSPECIFIED value on exit.
!
! $ Detailed_Input
!
!     OK       indicates to the error handling mechanism whether
!              replacement of or changes to the long error message
!              are to be allowed; for them to be allowed,
!              both of the following must be true:
!
!              1. No error condition exists, or the error response
!                 action is not 'RETURN'.
!
!              2. The current error response mode is not 'IGNORE'.
!
! $ Detailed_Output
!
!     The function is assigned a value on output, but the
!     value is not meaningful.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  This routine does not detect any errors.
!
!         However, this routine is part of the SPICELIB error handling
!         mechanism.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     DO NOT CALL THIS ROUTINE.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  DO NOT CALL THIS ROUTINE.
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
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
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
!     None.
!
!-&

! $ Revisions
!
!-     Beta Version 1.1.0, 13-DEC-1989 (NJB)
!
!         ACCEPT must return a value, in order to comply with the
!         Fortran standard. So, now it does. The value has no
!         meaning, as far as the specification of ACCEPT is
!         concerned.
!
!-     Beta Version 1.0.1, 08-FEB-1989 (NJB)
!
!         Warnings added to discourage use of this routine in
!         non-error-handling code.
!
!-&

!
!     SPICELIB functions:
!

      logical               allowd

!
!     Local Variables:
!

      logical               savok

      save                  savok

!
!     Initial Values:
!

      data       savok  /  .true.  /

!
!     Executable Code:
!

      savok  =  ok

      accept = .false.

      return

!$Procedure ALLOWD    (Are Changes of Long Error Message Allowed?)

       entry  allowd ()

! $ Abstract
!
!     Return .TRUE. if replacement or modification of the long error
!     message is allowed.
!
!     DO NOT CALL THIS ROUTINE.
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
!      None.
!
! $ Brief_I/O
!
!     The function takes the value, .TRUE., if replacement or
!     modification of the long error message is currently allowed.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function takes the value, .TRUE., if replacement of or
!     changes to the long error message are to be allowed; for them
!     to be allowed, both of the following must be true:
!
!     1. No error condition exists, or the error response
!        action is not 'RETURN'.
!
!     2. The current error response mode is not 'IGNORE'.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not detect any errors.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     DO NOT CALL THIS ROUTINE.
!
!     Non-error handling routines should not call this routine. Such
!     routines can set the long error message using SETMSG, which
!     itself calls this routine to test whether an update is allowed.
!
!     The initial value returned by ALLOWD is .FALSE.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  DO NOT CALL THIS ROUTINE.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (NJB) (HAN)
!
!-&

! $ Index_Entries
!
!     allow changes of long error message
!
!-&

! $ Revisions
!
!-     Beta Version 1.1.0, 18-DEC-1989 (HAN)
!
!         Empty parentheses added to the ENTRY statement in order to
!         comply with the ANSI Fortran 77 Standard.
!
!-     Beta Version 1.0.1, 08-FEB-1989 (NJB)
!
!         Warnings added to discourage use of this routine in
!         non-error-handling code.
!
!-&

!
!     Executable Code:
!

      allowd = savok

      end
