!$Procedure QDERIV ( Quadratic derivative )

      subroutine qderiv ( ndim, f0, f2, delta, dfdt )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Estimate the derivative of a function by finding the derivative
!     of a quadratic approximating function. This derivative estimate
!     is equivalent to that found by computing the average of forward
!     and backward differences.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!     UTILITY
!
! $ Declarations

      implicit none

      integer(ip)           ndim
      real(wp)              f0    ( ndim )
      real(wp)              f2    ( ndim )
      real(wp)              delta
      real(wp)              dfdt  ( ndim )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     NDIM       I   Dimension of function to be differentiated.
!     F0         I   Function values at left endpoint.
!     F2         I   Function values at right endpoint.
!     DELTA      I   Separation of abscissa points.
!     DFDT       O   Derivative vector.
!
! $ Detailed_Input
!
!     NDIM     is the dimension of the function to be
!              differentiated. The derivative of each
!              function component will be found.
!
!     F0       is an array of NDIM function values at a point on
!              the real line; we'll refer to this point as X0.
!
!     F2       is an array of NDIM function values at a second
!              point on the real line; we'll refer to this point
!              as X2. The points X0 and X2 must satisfy
!
!                 X2 = X0 + 2 * DELTA
!
!
!     DELTA    is one half of the difference between X2 and X0:
!
!                 DELTA = ( X2 - X0 ) / 2
!
!              DELTA may be negative but must be non-zero.
!
! $ Detailed_Output
!
!     DFDT     is an N-dimensional vector representing an estimate
!              of the derivative of the input function at the
!              midpoint X1 of the interval between X0 and X2.
!
!              The Ith component of DFDT is
!
!                 ( 1 / (2*DELTA) ) * ( F2(I) - F0(I) )
!
!              We may regard this estimate as the derivative
!              at X1 of a parabola fitted to the points
!
!                  ( X0, F0(I) ),  ( X2, F2(I) )
!
!              We may also regard this derivative as the average
!              of the forward and backward first-order
!              differences of the input function defined by
!              F0(I), F2(I), and DELTA.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If DELTA is zero, the error SPICE(DIVIDEBYZERO) is signaled.
!
!     2)  If NDIM is less than 1, this routine will fail in a
!         system-dependent manner.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine estimates the derivative of a vector-valued function
!     using the average of forward and backward differences.
!
!     The derivative estimate computed by this routine is equivalent to
!     that obtained by fitting each component of the function with a
!     parabola at the points
!
!        (X0, f(X0)), (X1, f(X1)), (X2, f(X2))
!
!     where
!
!         X0  =  X1 - DELTA
!         X2  =  X1 + DELTA
!
!     and finding the derivative of the parabolas at X1.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!
!     1) Estimate the derivative of x**2 at x = 2.
!
!        Example code begins here.
!
!
!              PROGRAM QDERIV_EX1
!              IMPLICIT NONE
!
!              DOUBLE PRECISION     DELTA
!              DOUBLE PRECISION     DFDT  (1)
!              DOUBLE PRECISION     F0    (1)
!              DOUBLE PRECISION     F2    (1)
!              INTEGER              N
!
!              N     = 1
!              DELTA = 1.D-3
!              F0(1) = ( 2.D0 - DELTA ) ** 2.D0
!              F2(1) = ( 2.D0 + DELTA ) ** 2.D0
!
!              CALL QDERIV ( N, F0, F2, DELTA, DFDT )
!
!              WRITE ( *, '(1X,A,E25.16)'  ) '4 - DFDT(1) = ',
!             .                               4 - DFDT(1)
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         4 - DFDT(1) =    0.4547473508864641E-12
!
!
!        Note that the difference displayed is platform-dependent, but
!        should be on the order of 1.E-12.
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
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 05-AUG-2020 (JDR)
!
!        Changed input argument name "N" to "NDIM" for consistency with
!        other routines.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
!
!-&


! $ Index_Entries
!
!     Estimate function derivative using quadratic fit
!
!-&


!
!     Use discovery check-in.
!
      if ( delta == 0.0_wp ) then

         call setmsg ( 'Delta abscissa value is zero; a non-zero ' // &
                       'value is required.'                        )
         call sigerr ( 'SPICE(DIVIDEBYZERO)'                       )
         return

      end if
!
!
!     Our derivative estimate is
!
!            1/2 * (   Backward_difference / DELTA
!                    + Forward_difference  / DELTA )
!
!        =   ( 1/(2*DELTA) ) * ( ( F(X2) - F(X1) ) +  ( F(X1) - F(X0) )
!
!        =   ( 1/(2*DELTA) ) * ( ( F(X2) - F(X0) )
!
!        =    (0.5/DELTA) * F(X2)  +  (-0.5/DELTA) * F(X0)
!
!
      call vlcomg (  ndim, 0.5_wp/delta, f2, -0.5_wp/delta, f0, dfdt )

      return
      end
