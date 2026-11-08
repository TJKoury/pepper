!$Procedure SMSGND  ( Same Sign Double Precision Numbers )

      logical function smsgnd ( x, y )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Return .TRUE. if two given double precision numbers have the same
!     sign.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     NUMBERS
!
! $ Declarations

      implicit none

      real(wp)           x
      real(wp)           y

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   A double precision number
!     Y          I   A double precision number
!
!     The function returns .TRUE. if the input arguments have the same
!     sign.
!
! $ Detailed_Input
!
!     X        is any double precision number.
!
!     Y        is any double precision number.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if X and Y are both positive or both
!     negative. Otherwise, it returns .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine returns the value:
!
!           (      (( X .GT. 0) .AND. (Y .GT. 0))
!             .OR. (( X .LT. 0) .AND. (Y .LT. 0)) )
!
!     This is a more stable value than
!
!           ( X*Y .GT. 0 )
!
!     Note: If either of the to inputs is zero. The result returned
!     will be .FALSE.
!
! $ Examples
!
!     This routine can be used whenever a decision depends upon two
!     Double Precision values having the same sign.
!
!     IF ( SMSGND ( F(X1), F(X2) ) ) THEN
!           .
!           .
!        do something
!           .
!           .
!     ELSE
!           .
!           .
!        find a root of F lying between X1 and X2
!           .
!           .
!     END IF
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     same sign d.p. numbers
!
!-&


      smsgnd = (      (( x .gt. 0 ) .and. ( y .gt. 0 )) &
                 .or. (( x .lt. 0 ) .and. ( y .lt. 0 )) &
               )

      return
      end
