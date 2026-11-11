!> Declare parameters specific to PCK type 20.
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
!>-    SPICELIB Version 1.0.0, 03-DEC-2013 (NJB)
module pepper_inc_pck20

   use pepper_kinds, only: ip, wp

   implicit none

   public

   integer(ip), parameter :: maxdeg = 50  !! MAXDEG is the maximum allowed degree of the input
                                          !! Chebyshev expansions. If the value of MAXDEG is
                                          !! increased, the SPICELIB routine PCKMAT must be
                                          !! changed accordingly. In particular, the size of
                                          !! the record passed to PCKRnn and PCKEnn must be
                                          !! increased, and comments describing the record size
                                          !! must be changed.

   real(wp), parameter :: dtol = 1.0e-4_wp  !! Absolute tolerance for coverage gap at endpoints
                                            !! of segment coverage interval, in seconds:
                                            !!
                                            !! DTOL,
                                            !! TOLSCL         are, respectively, an absolute tolerance and
                                            !! a tolerance scale used for time coverage bound
                                            !! checking. DTOL has units of TDB seconds. TOLSCL
                                            !! is unitless. These two parameters are combined
                                            !! to produce a tolerance value via the formula
                                            !!
                                            !!        TOL = MAX ( DTOL, TOLSCL * MAX( ABS(FIRST),
                                            !!                                        ABS(LAST) ) )
                                            !!
                                            !!     where FIRST and LAST are the coverage time bounds
                                            !!     of a type 20 segment.
                                            !!
                                            !!     The resulting parameter TOL is used as a tolerance
                                            !!     for comparing the input segment descriptor time
                                            !!     bounds to the first and last epoch covered by the
                                            !!     sequence of time intervals defined by the inputs
                                            !!     to PCKW20:
                                            !!
                                            !!        INITJD
                                            !!        INITFR
                                            !!        INTLEN
                                            !!        N

   real(wp), parameter :: tolscl = 1.0e-12_wp  !! Relative tolerance for coverage gap at endpoints
                                               !! of segment coverage interval:

end module pepper_inc_pck20
