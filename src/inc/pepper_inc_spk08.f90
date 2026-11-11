!> Declare parameters specific to SPK type 8.
!>
!>## Restrictions
!>
!>     None.
!>
!>## Author and Institution
!>
!>     N.J. Bachman      (JPL)
!>
!>## Literature References
!>
!>     None.
!>
!>## Version
!>
!>-    SPICELIB Version 1.0.0, 11-JAN-2014 (NJB)
module pepper_inc_spk08

   use pepper_kinds, only: ip, wp

   implicit none

   public

!
!     MAXDEG         is the maximum allowed degree of type 8
!                    interpolating polynomials. If the value of MAXDEG
!                    is increased, the SPICELIB routine SPKPVN must be
!                    changed accordingly. In particular, the size of
!                    the record passed to SPKRnn and SPKEnn must be
!                    increased, and comments describing the record size
!                    must be changed.
!
!                    The record size requirement is
!
!                       MAXREC = ( 3 * ( MAXDEG + 1 ) )  +  3
!
!
   integer(ip), parameter :: maxdeg = 27

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
!                    to SPKW08:
!
!                       EPOCH1
!                       STEP
!                       N

!
!     Tolerance scale for coverage gap at the endpoints
!     of the segment coverage interval:
!
   real(wp), parameter :: tolscl = 1.0e-13_wp

end module pepper_inc_spk08
