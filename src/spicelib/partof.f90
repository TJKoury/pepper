!$Procedure PARTOF ( Parabolic time of flight )

      subroutine partof ( ma, d )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Solve the time of flight equation MA = D + (D**3) / 3
!     for the parabolic eccentric anomaly D, given mean anomaly.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONIC
!
! $ Declarations

      implicit none

      real(wp)              ma
      real(wp)              d

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MA         I   Mean anomaly at epoch.
!     D          O   Parabolic eccentric anomaly.
!
! $ Detailed_Input
!
!     MA       is the parabolic mean anomaly of an orbiting body at
!              some epoch t,
!
!                                   3  1/2
!                 MA = (t-T) (mu/(2q ))
!
!              where T is the time of periapsis passage, mu is
!              the gravitational parameter of the primary body,
!              and q is the perifocal distance.
!
! $ Detailed_Output
!
!     D        is the corresponding parabolic anomaly. This is the
!              solution to the time of flight equation
!
!                           3
!                 MA = D + D / 3
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
!     Iterate to solve
!
!                         3
!        f(D,MA,p) = D + D / 3 - MA = 0
!
! $ Examples
!
!     ELLTOF, HYPTOF, and PARTOF are used by CONICS.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
!          Astrodynamics," Dover Publications Inc., 1971.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 19-APR-1991 (WLT)
!
!        A write statement left over from debugging days was removed.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&

! $ Index_Entries
!
!     parabolic time of flight
!
!-&

!
!     SPICELIB functions
!
      logical               return
      real(wp)              dcbrt

!
!     Local parameters
!
      real(wp), parameter :: tol = 1.0e-13_wp

!
!     Local variables
!
      real(wp)              m
      real(wp)              fn
      real(wp)              deriv
      real(wp)              deriv2
      real(wp)              change

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     If the mean anomaly is zero, the eccentric anomaly is also zero
!     (by inspection). If the mean anomaly is negative, we can pretend
!     that it's positive (by symmetry).
!

      if ( ma == 0.0_wp ) then
         d = 0.0_wp
         return
      else
         m = abs ( ma )
      end if

!
!     We need an initial guess for the eccentric anomaly D. The function
!     is well behaved, so just about any guess will do.
!
      d = dcbrt ( 3.0_wp * m )

!
!     Use the Newton second-order method,
!
!                                           2
!          F    = F  - (f/f')*(1 + f*f''/2f' )
!           i+1    i
!
!     where
!
!                     3
!          f   = D + D / 3 - M
!
!                     2
!          f'  = 1 + D
!
!          f'' = 2 D
!

      change = 1.0_wp

      do while ( abs ( change ) > tol )

         fn     = d    + d**3/3.0_wp - m
         deriv  = 1.0_wp + d**2
         deriv2 = 2.0_wp * d

         change = (fn/deriv) * ( 1.0_wp + (fn*deriv2) / (2.0_wp*deriv**2) )
         d      = d - change

      end do

      if ( ma < 0.0_wp ) then
         d = -d
      end if

      return
      end
