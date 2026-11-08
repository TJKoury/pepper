!$Procedure DACOSH ( Double precision arc hyperbolic cosine )

      real(wp) function  dacosh ( x )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Return the inverse hyperbolic cosine of a double precision
!     argument.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     HYPERBOLIC
!     MATH
!
! $ Declarations

      implicit none

      real(wp)           x

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!      X         I   Number whose inverse hyperbolic cosine is desired.
!
!     The function returns the inverse hyperbolic cosine of a double
!     precision number.
!
! $ Detailed_Input
!
!     X        is any double precision number greater than or equal to
!              1.
!
! $ Detailed_Output
!
!     The function returns the inverse hyperbolic cosine of the double
!     precision number X.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If X is less than 1.0D0, the error SPICE(INVALIDARGUMENT) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This function simply implements the definition of the inverse
!     hyperbolic cosine as follows:
!
!        DACOSH = DLOG (X + DSQRT (X*X-1.D0))
!
!     If the input value is not valid, an error is signaled.
!
! $ Examples
!
!     The following table gives a few values for X and the resulting
!     value of DACOSH.
!
!     X                       DACOSH(X)
!     ----------------------------------------------
!     1.000000000000000      0.0000000000000000E+00
!     10.00000000000000       2.993222846126381
!     100.0000000000000       5.298292365610485
!     1000.000000000000       7.600902209541989
!
! $ Restrictions
!
!     1)  The value of the input variable X must be greater than or
!         equal to 1.0d0.
!
! $ Literature_References
!
!     [1]  W.H. Beyer, "CRC Standard Mathematical Tables," CRC Press,
!          1987.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.M. Owen          (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        Set the default function value to either 0, 0.0D0, .FALSE.,
!        or blank depending on the type of the function.
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
!     d.p. arc hyperbolic_cosine
!
!-&


!
!     SPICELIB functions
!

      logical               return



!
!   Set up the error processing.
!
      if ( return() ) then
         dacosh = 0.0_wp
         return
      else
         call chkin ( 'DACOSH' )
         dacosh = 0.0_wp
      end if

!
!  Check that X >= 1.
!

      if ( x < 1.0_wp ) then

         call setmsg ( 'DACOSH: Invalid argument, X is less than one.' )
         call sigerr ( 'SPICE(INVALIDARGUMENT)' )

         call chkout ( 'DACOSH' )
         return
      end if
!
!  Abiding by the order implied by the parentheses in the expression
!  (1.0D0/X)/X prevents floating point overflow that might occur for
!  large values of X if the equivalent expression, 1.0D0/(X*X), were
!  used.
!
      dacosh = dlog (x + x * dsqrt (1.0_wp - (1.0_wp/x)/x) )


      call chkout ( 'DACOSH' )
      return
      end
