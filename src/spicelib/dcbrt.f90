!$Procedure DCBRT ( Double precision cube root )

      double precision function  dcbrt ( x )

! $ Abstract
!
!     Return the cube root of a double precision number.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!     ROOT
!
! $ Declarations

      implicit none

      double precision   x

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I     Number whose cube root is desired.
!
!     The function returns the cube root of the input value.
!
! $ Detailed_Input
!
!     X        is a double precision value.
!
! $ Detailed_Output
!
!     The function returns the cube root of the input value.
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
!     DCBRT calculates the cube root of the input value by using the
!     exponentiation operator to raise the input value to the 1/3
!     power. This operation, however, is performed on the absolute
!     value of the input variable, and then the sign of the input
!     is transferred to the output value.
!
!     All values of the input variable X should be acceptable to the
!     DCBRT.
!
! $ Examples
!
!     The following table gives sample values of the variable X and
!     DCBRT(X)
!
!      X                        DCBRT(X)
!     --------------------------------------------------------------
!      0.0D0                    0.0D0
!      8.0D0                    2.0D0
!     -1.0D3                   -1.0D1
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
!     W.M. Owen          (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
!
!-&


! $ Index_Entries
!
!     d.p. cube root
!
!-&


      double precision third
      parameter (third = 1.d0/3.d0)
!
      dcbrt = dsign (dabs(x)**third, x)
!
      return
      end
