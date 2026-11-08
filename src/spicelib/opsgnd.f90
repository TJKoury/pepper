!$Procedure OPSGND ( Opposite Sign Double Precision Numbers )

      logical function opsgnd  ( x, y )

! $ Abstract
!
!     Return .TRUE. if two given double precision numbers have opposite
!     signs.
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

      double precision   x
      double precision   y

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   A double precision number
!     Y          I   A double precision number
!
!     The function returns .TRUE. when the double precision numbers X
!     and Y have opposite signs.
!
! $ Detailed_Input
!
!     X        is any double precision number.
!
!     Y        is any double precision number.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if one of the pair X,Y is positive and
!     the other is negative. If either of the two values is zero, OPSGND
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
!     Note that if either of the two values is zero, OPSGND will be
!     false.
!
! $ Examples
!
!     This routine can be used whenever a decision depends upon two
!     Double Precision values having opposite signs.
!
!     IF ( OPSGND ( F(X1), F(X2) ) ) THEN
!           .
!           .
!        find a root of F lying between X1 and X2
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
!     opposite sign d.p. numbers
!
!-&


      opsgnd = (      (( x .gt. 0 ) .and. ( y .lt. 0 )) &
                 .or. (( x .lt. 0 ) .and. ( y .gt. 0 )) &
               )

      return
      end
