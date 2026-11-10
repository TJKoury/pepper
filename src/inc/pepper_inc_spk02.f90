!> The parameters of SPICELIB's include file spk02.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_spk02

   implicit none

   public

! $ Abstract
!
!     Declare parameters specific to SPK type 2.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 30-DEC-2013 (NJB)
!

!     MAXDEG         is the maximum allowed degree of the input
!                    Chebyshev expansions. If the value of MAXDEG is
!                    increased, the SPICELIB routine SPKPVN must be
!                    changed accordingly. In particular, the size of
!                    the record passed to SPKRnn and SPKEnn must be
!                    increased, and comments describing the record size
!                    must be changed.
!
!                    The record size requirement is
!
!                       MAXREC = 3 * ( MAXDEG + 2 )
!
!
   integer               maxdeg
   parameter           ( maxdeg = 27 )

!
!     TOLSCL         is a tolerance scale factor (also called a
!                    "relative tolerance") used for time coverage
!                    bound checking. TOLSCL is unitless. TOLSCL
!                    produces a tolerance value via the formula
!
!                       TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) )
!
!                    where FIRST and LAST are the coverage time bounds
!                    of a type 2 segment, expressed as seconds past
!                    J2000 TDB.
!
!                    The resulting parameter TOL is used as a tolerance
!                    for comparing the input segment descriptor time
!                    bounds to the first and last epoch covered by the
!                    sequence of time intervals defined by the inputs
!                    to SPKW02:
!
!                       BTIME
!                       INTLEN
!                       N

!
!     Tolerance scale for coverage gap at the endpoints
!     of the segment coverage interval:
!
   double precision      tolscl
   parameter           ( tolscl = 1.d-13 )

!
!     End of include file spk02.inc.
!

end module pepper_inc_spk02
