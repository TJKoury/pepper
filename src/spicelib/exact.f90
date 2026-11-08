!$Procedure EXACT ( Round to exact value )

      real(wp) function exact ( number, value, tol )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Round an input double precision number to a specified exact value
!     if the number and the value are equal to within some tolerance.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     COMPARE
!     NUMBERS
!
! $ Declarations

      implicit none

      real(wp)              number
      real(wp)              value
      real(wp)              tol

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NUMBER     I   Double precision number.
!     VALUE      I   Target value.
!     TOL        I   Tolerance.
!
!     The function returns VALUE whenever |NUMBER - VALUE| < TOL.
!                                                          -
!
! $ Detailed_Input
!
!     NUMBER   is an arbitrary double precision number.
!
!     VALUE    is a target value.
!
!     TOL      is a tolerance. NUMBER and VALUE are considered to
!              be equal if they differ by no more than this amount.
!              If TOL is negative, they are never considered equal.
!
! $ Detailed_Output
!
!     The function returns VALUE whenever |NUMBER - VALUE| < TOL, and
!     otherwise returns NUMBER.                            -
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
!     None.
!
! $ Examples
!
!     C
!     C     If the eccentricity is near one, make this a parabola.
!     C
!           ECC = EXACT ( ECC, 1.D0, 10.D-12 )
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
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     round to exact value
!
!-&


!
!     Just shorthand, really.
!
      if ( abs ( number - value ) .le. tol ) then
         exact = value

      else
         exact = number
      end if

      return
      end
