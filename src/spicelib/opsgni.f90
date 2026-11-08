!$Procedure OPSGNI  ( Opposite Sign Integers )

      logical function opsgni  ( x, y )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return .TRUE. if two given integer numbers have opposite signs.
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

      integer(ip)      x
      integer(ip)      y

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   An integer.
!     Y          I   An integer.
!
!     The function returns .TRUE. when the integer numbers X and Y have
!     opposite signs.
!
! $ Detailed_Input
!
!     X        is any integer number.
!
!     Y        is any integer number.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if one of the pair X,Y is positive and
!     the other is negative. If either of the two values is zero, OPSGNI
!     will be .FALSE.
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
!           (      (( X .GT. 0) .AND. (Y .LT. 0))
!             .OR. (( X .LT. 0) .AND. (Y .GT. 0)) )
!
!     This is a more stable value than
!
!           ( X*Y .LT. 0 )
!
!     Note that if either of the two values is zero, OPSGNI will be
!     false.
!
! $ Examples
!
!     This routine can be used whenever a decision depends upon two
!     integer values having opposite signs.
!
!     IF ( OPSGNI ( F(X1), F(X2) ) ) THEN
!           .
!           .
!        find the value of F closest to zero.
!           .
!           .
!     ELSE
!           .
!           .
!        do something
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Extended
!        $Detailed_Output section to indicate the output value for
!        the case of either input being zero.
!
!-    SPICELIB Version 1.0.2, 07-NOV-2005 (BVS)
!
!        Fixed cut-and-paste errors in the header.
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
!     opposite sign integers
!
!-&


      opsgni = (      (( x .gt. 0 ) .and. ( y .lt. 0 )) &
                 .or. (( x .lt. 0 ) .and. ( y .gt. 0 )) &
               )

      return
      end
