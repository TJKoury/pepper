!> Declare parameters specific to SPK type 20.
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
!>-    SPICELIB Version 1.0.0, 30-DEC-2013 (NJB)
module pepper_inc_spk20

   use pepper_kinds, only: ip, wp

   implicit none

   public

   integer(ip), parameter :: maxdeg = 50  !! MAXDEG is the maximum allowed degree of the input
                                          !! Chebyshev expansions. If the value of MAXDEG is
                                          !! increased, the SPICELIB routine SPKPVN must be
                                          !! changed accordingly. In particular, the size of
                                          !! the record passed to SPKRnn and SPKEnn must be
                                          !! increased, and comments describing the record size
                                          !! must be changed.
                                          !!
                                          !! The record size requirement is
                                          !!
                                          !!     MAXREC = 3 * ( MAXDEG + 3 )

   real(wp), parameter :: tolscl = 1.0e-13_wp  !! Tolerance scale for coverage gap at the endpoints
                                               !! of the segment coverage interval:
                                               !!
                                               !! TOLSCL is a tolerance scale factor (also called a
                                               !! "relative tolerance") used for time coverage
                                               !! bound checking. TOLSCL is unitless. TOLSCL
                                               !! produces a tolerance value via the formula
                                               !!
                                               !!     TOL = TOLSCL * MAX( ABS(FIRST), ABS(LAST) )
                                               !!
                                               !! where FIRST and LAST are the coverage time bounds
                                               !! of a type 20 segment, expressed as seconds past
                                               !! J2000 TDB.
                                               !!
                                               !! The resulting parameter TOL is used as a tolerance
                                               !! for comparing the input segment descriptor time
                                               !! bounds to the first and last epoch covered by the
                                               !! sequence of time intervals defined by the inputs
                                               !! to SPKW20:
                                               !!
                                               !!     INITJD
                                               !!     INITFR
                                               !!     INTLEN
                                               !!     N

end module pepper_inc_spk20
