!$Procedure F_ZZINLAT ( ZZINLAT tests )

      SUBROUTINE F_ZZINLAT ( OK )

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZINLAT.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE
      INCLUDE 'dsktol.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the private SPICELIB routine ZZINLAT.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 12-FEB-2017 (NJB)
!
!        Updated test cases for near-polar cases: now these cases
!        exclude coordinates from testing in the same way as the
!        complementary latitude cases.
!
!        Added test cases for longitude out of range by less than the
!        alias limit.
!
!
!        Previous version 10-OCT-2016 (NJB)
!
!           Removed non-error case with matching minimum and maximum
!           longitudes.
!
!           Removed debugging output and STOP statements.
!
!           Previous version 25-MAY-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD

!
!     Local Parameters
!
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )

      INTEGER               LONIDX
      PARAMETER           ( LONIDX = 1 )

      INTEGER               LATIDX
      PARAMETER           ( LATIDX = 2 )

      INTEGER               RADIDX
      PARAMETER           ( RADIDX = 3 )

      DOUBLE PRECISION      POLMRG
      PARAMETER           ( POLMRG = 1.D-4 )

      INTEGER               NLAT
      PARAMETER           ( NLAT   = 6 )

      INTEGER               NLON
      PARAMETER           ( NLON   = 9 )

      INTEGER               NR
      PARAMETER           ( NR     = 4 )

      INTEGER               TITLSZ
      PARAMETER           ( TITLSZ = 240 )

!
!     Local Variables
!
      CHARACTER*(TITLSZ)    STEM
      CHARACTER*(TITLSZ)    TITLE

      DOUBLE PRECISION      AEPS
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      EPS
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      MAXLAT ( NLAT )
      DOUBLE PRECISION      MAXLON ( NLON )
      DOUBLE PRECISION      MAXR   ( NR   )
      DOUBLE PRECISION      MIDLAT
      DOUBLE PRECISION      MIDLON
      DOUBLE PRECISION      MIDR
      DOUBLE PRECISION      MINLAT ( NLAT )
      DOUBLE PRECISION      MINLON ( NLON )
      DOUBLE PRECISION      MINR   ( NR   )
      DOUBLE PRECISION      NRMMAX
      DOUBLE PRECISION      NRMMIN
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      TOL

      INTEGER               EXCLUD
      INTEGER               I
      INTEGER               L
      INTEGER               LATIX1
      INTEGER               LATIX2
      INTEGER               LONIX
      INTEGER               M
      INTEGER               N
      INTEGER               RIX1
      INTEGER               RIX2

      LOGICAL               INSIDE
      LOGICAL               LATLB
      LOGICAL               LATUB
      LOGICAL               LONBDS
      LOGICAL               RLB
      LOGICAL               XIN



!
!     Saved values
!
      SAVE

!
!     Initial values
!

!
!     Set the initial angular bounds in degrees, since we can
!     do this without function calls. The bounds will be
!     converted to radians at run time.
!

!
!     For the latitude boundaries, every valid combination of
!     minimum and maximum will be tested.
!
      DATA                  MINLAT / -90.D0, &
                                     -89.999D0, &
                                     -45.D0, &
                                       0.D0, &
                                      45.D0, &
                                      89.999D0 /

      DATA                  MAXLAT / -89.999D0, &
                                     -45.D0, &
                                       0.D0, &
                                      45.D0, &
                                      89.999D0, &
                                      90.D0       /


!
!     For the longitude boundaries, each pair of bounds
!     consisting of the Ith minimum and Ith maximum will
!     be tested.
!
      DATA                  MINLON /  -10.D0, &
                                      -10.D0, &
                                     -180.D0, &
                                     -360.D0, &
                                       10.D0, &
                                      179.999D0, &
                                     -179.999D0, &
                                     -260.D0, &
                                      350.D0       /

      DATA                  MAXLON /   -5.D0, &
                                       20.D0, &
                                      180.D0, &
                                        0.D0, &
                                      -10.D0, &
                                     -179.999D0, &
                                      179.999D0, &
                                      200.D0, &
                                     -350.D0       /

!
!     For the radius boundaries, every valid combination of
!     minimum and maximum will be tested.
!
      DATA                  MINR    /   0.D0, &
                                        1.D-3, &
                                        1.D3, &
                                        9.999999D3 /

      DATA                  MAXR    /   4 * 1.D4   /



!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZINLAT' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize coordinate bounds' )

!
!     Convert angular bounds to radians.
!
      DO I = 1, NLAT

         MINLAT(I) = MINLAT(I) * RPD()
         MAXLAT(I) = MAXLAT(I) * RPD()

      END DO


      DO I = 1, NLON

         MINLON(I) = MINLON(I) * RPD()
         MAXLON(I) = MAXLON(I) * RPD()

      END DO




!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-origin case: input point is near ' &
      //           'the origin and the minimum radius bound ' &
      //           'nearly zero. Interior case.'             )

!
!     Longitude bounds:
!
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

!
!     The longitude is out of range.
!
      LON         = PI()

!
!     Latitude bounds:
!
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = PI()/4

!
!     Point's latitude is out of range.
!
      LAT         = HALFPI()

!
!     Radius bounds:
!
      BOUNDS(1,3) = 1.D-8
      BOUNDS(2,3) = 1.D4

      R           = 9.D-9

!
!     Margin:
!
      MARGIN = 1.D-8

!
!     Excluded coordinate: none.
!
      EXCLUD = NONE

!
!     Create the input point and test it.
!
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-origin case: input point is near ' &
      //           'the origin and the minimum radius bound ' &
      //           'nearly zero. Exterior case.'            )

!
!     Longitude bounds:
!
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

!
!     The longitude is out of range.
!
      LON         = PI()

!
!     Latitude bounds:
!
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = PI()/4

!
!     Point's latitude is out of range.
!
      LAT         = HALFPI()

!
!     Radius bounds:
!
      BOUNDS(1,3) = 1.D-8
      BOUNDS(2,3) = 1.D4

      R           = 9.D-9

!
!     Margin:
!
      MARGIN = 1.D-11

!
!     Excluded coordinate: none.
!
      EXCLUD = NONE

!
!     Create the input point and test it.
!
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-polar latitude (North) case: radius, ' &
      //           'latitude, and longitude are within bounds ' &
      //           '(taking margin into account). No coordinate ' &
      //           'is excluded.'                                )

!
!     Excluded coordinate: none.
!
      EXCLUD = NONE

!
!     "Standard" margin:
!
      MARGIN = 1.D-8

!
!     Longitude bounds:
!
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()/4

!
!     Latitude bounds:
!
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = HALFPI() - 1.D-9

!
!     Point's latitude exceeds upper bound by less than MARGIN.
!
      LAT         = HALFPI()

!
!     Radius bounds:
!
      BOUNDS(1,3) = 5.D3
      BOUNDS(2,3) = 1.D4

      R           = 1.D4

!
!     Margin:
!
      MARGIN = 1.D-8

!
!     Excluded coordinate: none.
!
      EXCLUD = NONE

!
!     Create the input point and test it.
!
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Near-polar latitude (South) case: radius, ' &
      //           'latitude, and longitude are within bounds ' &
      //           '(taking margin into account). No coordinate ' &
      //           'is excluded.'                                )

!
!     Excluded coordinate: none.
!
      EXCLUD = NONE

!
!     "Standard" margin:
!
      MARGIN = 1.D-8
!
!     Latitude bounds:
!
      BOUNDS(1,2) = -HALFPI() + 1.D-9
      BOUNDS(2,2) = 0.D0
!
!     Point's latitude exceeds lower bound by less than MARGIN.
!
      LAT         = -HALFPI()

!
!     Create the input point and test it.
!
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-polar latitude (North) case: radius and ' &
      //           'latitude are within bounds; longitude is not. ' &
      //           'No coordinate is excluded.'                    )

!
!     Excluded coordinate: none.
!
      EXCLUD = NONE

!
!     "Standard" margin:
!
      MARGIN = 1.D-8

!
!     Latitude bounds:
!
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = HALFPI() - 1.D-9

!
!     Point's latitude exceeds lower bound by less than MARGIN.
!
      LAT         = HALFPI()

!
!     Set longitude to a value that's out of bounds. This should
!     not affect the comparison, due to the high latitude bound.
!
      LON = PI()
!
!     Create the input point and test it.
!
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-polar latitude (South) case: radius and ' &
      //           'latitude are within bounds; longitude is not. ' &
      //           'No coordinate is excluded.'                    )


!
!     Excluded coordinate: none.
!
      EXCLUD = NONE
!
!     "Standard" margin:
!
      MARGIN = 1.D-8

!
!     Latitude bounds:
!
      BOUNDS(1,2) = -HALFPI() + 1.D-9
      BOUNDS(2,2) = 0.D0
!
!     Point's latitude exceeds lower bound by less than MARGIN.
!
      LAT         = -HALFPI()

!
!     Set longitude to a value that's out of bounds. This should
!     not affect the comparison, due to the high latitude bound.
!
      LON = PI()
!
!     Create the input point and test it.
!
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-polar latitude (North) case: radius is ' &
      //           'out of bounds; latitude and longitude are not. ' &
      //           'No coordinate is excluded.'                    )


!
!     Set EXCLUD to indicate no exclusion.
!
      EXCLUD = NONE

!
!     "Standard" margin:
!
      MARGIN = 1.D-8

!
!     Latitude bounds:
!
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = HALFPI() - 1.D-9

!
!     Point's latitude exceeds lower bound by less than MARGIN.
!
      LAT         = HALFPI()

!
!     Set longitude to a value that's in bounds.
!
      LON = PI()/4

!
!     Radius bounds:
!
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4

!
!     Create the input point and test it.
!

!
!     R is too large.
!
      R = BOUNDS(2,3) * ( 1.D0  + (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE (large)', INSIDE, XIN, OK )

!
!     R is too small.
!
      R = BOUNDS(1,3) * ( 1.D0  - (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE (small)', INSIDE, XIN, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-polar latitude (South) case: radius is ' &
      //           'out of bounds; latitude and longitude are not. ' &
      //           'No coordinate is excluded.'                    )


!
!     Set EXCLUD to indicate no exclusion.
!
      EXCLUD = NONE

!
!     "Standard" margin:
!
      MARGIN = 1.D-8

!
!     Latitude bounds:
!
      BOUNDS(1,2) = -HALFPI() + 1.D-9
      BOUNDS(2,2) = 0.D0

!
!     Point's latitude exceeds lower bound by less than MARGIN.
!
      LAT         = -HALFPI()

!
!     Set longitude to a value that's in bounds.
!
      LON = PI()/4

!
!     Radius bounds:
!
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4

!
!     Create the input point and test it.
!

!
!     R is too large.
!
      R = BOUNDS(2,3) * ( 1.D0  + (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE (large)', INSIDE, XIN, OK )

!
!     R is too small.
!
      R = BOUNDS(1,3) * ( 1.D0  - (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE (small)', INSIDE, XIN, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-polar latitude (North) case: radius and ' &
      //           'longitude are within bounds; latitude is not. ' &
      //           'No coordinate is excluded.'                    )

!
!     Longitude bounds:
!
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()/4

!
!     Margin:
!
      MARGIN = 1.D-8

!
!     Latitude bounds:
!
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = HALFPI() - 1.D-7

!
!     Point's latitude exceeds upper bound by more than MARGIN.
!
      LAT         = HALFPI() - 1.D-10

!
!     Radius bounds:
!
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4

      R           = 2.D4

!
!     Set EXCLUD to indicate latitude no exclusion.
!
      EXCLUD = NONE

!
!     Create the input point and test it.
!
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-polar latitude (South) case: radius and ' &
      //           'longitude are within bounds; latitude is not. ' &
      //           'No coordinate is excluded.'                     )

!
!     Longitude bounds:
!
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()/4

!
!     Margin:
!
      MARGIN = 1.D-8

!
!     Latitude bounds:
!
      BOUNDS(1,2) = -HALFPI() + 1.D-7
      BOUNDS(2,2) = 0.D0


!
!     Point's latitude exceeds lower bound by more than MARGIN.
!
      LAT         = -HALFPI() + 1.D-10

!
!     Radius bounds:
!
      BOUNDS(1,3) = 5.D3
      BOUNDS(2,3) = 1.D4

      R           = 1.D4


!
!     Set EXCLUD to indicate latitude no exclusion.
!
      EXCLUD = NONE

!
!     Create the input point and test it.
!
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-polar latitude (North) case: radius and ' &
      //           'longitude are out of bounds; latitude is not. ' &
      //           'Radius is excluded.'                           )

!
!     Set EXCLUD to exclude radius testing.
!
      EXCLUD = RADIDX

!
!     Margin:
!
      MARGIN = 1.D-8

!
!     Latitude bounds:
!
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = HALFPI() - 1.D-9

!
!     Point's latitude exceeds lower bound by less than MARGIN.
!
      LAT         = HALFPI()

!
!     Longitude bounds:
!
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()

!
!     Radius bounds:
!
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4


!
!     Create the input point and test it.
!


!
!     R is too large.
!
      R = BOUNDS(2,3) * ( 1.D0  + (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE (large)', INSIDE, XIN, OK )

!
!     R is too small.
!
      R = BOUNDS(1,3) * ( 1.D0  - (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE (small)', INSIDE, XIN, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-polar latitude (South) case: radius and ' &
      //           'longitude are out of bounds; latitude is not. ' &
      //           'Radius is excluded.'                           )


!
!     Set EXCLUD to exclude radius testing.
!
      EXCLUD = RADIDX

!
!     Latitude bounds:
!
      BOUNDS(1,2) = -HALFPI() + 1.D-9
      BOUNDS(2,2) = 0.D0

!
!     Point's latitude is less than lower bound by less than MARGIN.
!
      LAT         = -HALFPI()

!
!     Longitude bounds:
!
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()

!
!     Radius bounds:
!
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4


!
!     Create the input point and test it.
!

!
!     R is too large.
!
      R = BOUNDS(2,3) * ( 1.D0  + (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE (large)', INSIDE, XIN, OK )

!
!     R is too small.
!
      R = BOUNDS(1,3) * ( 1.D0  - (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE (small)', INSIDE, XIN, OK )





!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-polar latitude (North) case: latitude is ' &
      //           'out of bounds; radius and longitude are not. ' &
      //           'Latitude is excluded.'                         )

!
!     Set EXCLUD to exclude latitude testing.
!
      EXCLUD = LATIDX

!
!     Margin:
!
      MARGIN = 1.D-8

!
!     Latitude bounds:
!
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = HALFPI() - 2.D-8

!
!     Point's latitude exceeds upper bound by more than MARGIN.
!
      LAT         = HALFPI()

!
!     Longitude bounds:
!
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()/4

!
!     R is in range.
!
      R = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2
!
!     Create the input point and test it.
!

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )





!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Near-polar latitude (South) case: latitude is ' &
      //           'out of bounds; radius and longitude are not. ' &
      //           'Latitude is excluded.'                         )

!
!     Set EXCLUD to exclude latitude testing.
!
      EXCLUD = LATIDX

!
!     Margin:
!
      MARGIN = 1.D-8

!
!     Latitude bounds:
!
      BOUNDS(1,2) = -HALFPI() + 2.D-8
      BOUNDS(2,2) = 0.D0

!
!     Point's latitude is below lower bound by more than MARGIN.
!
      LAT         = -HALFPI()

!
!     Longitude bounds:
!
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()/4

!
!     R is in range.
!
      R = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2
!
!     Create the input point and test it.
!

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )




!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Longitude acceptance test for longitude ' &
      //           'lower than lower bound by less than ANGMRG. ' &
      //           'Other coordinates are in range. Non-polar case.' )

!
!     Margin: positive but small enough so that ANGMRG is larger.
!
      MARGIN = 1.D-14

!
!     Longitude bounds:
!
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

!
!     The point's longitude is outside the element by more than ANGMRG
!     but by less than (ANGMRG+LONALI).
!
      LON         = BOUNDS(1,1) - ANGMRG - 1.D-13

!
!     Latitude bounds:
!
      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) =  PI()/4

!
!     Point's latitude is in range.
!
      LAT         = 0.D0

!
!     Radius bounds:
!
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4

!
!     Point's radius is in range.
!
      R           = 2.D4

!
!     Check all coordinates.
!
      EXCLUD = NONE

!
!     Create the input point and test it.
!
      CALL LATREC ( R, LON, LAT, P )


      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Longitude acceptance test for longitude ' &
      //           'greater than upper bound by less than ANGMRG. ' &
      //           'Other coordinates are in range. Non-polar case.' )

!
!     Margin: positive but small enough so that ANGMRG is larger.
!
      MARGIN = 1.D-14

!
!     Longitude bounds:
!
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

!
!     The point's longitude is outside the element by more than ANGMRG
!     but by less than (ANGMRG+LONALI).
!
      LON         = BOUNDS(2,1) + ANGMRG + 1.D-13

!
!     Latitude bounds:
!
      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) =  PI()/4

!
!     Point's latitude is in range.
!
      LAT         = 0.D0

!
!     Radius bounds:
!
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4

!
!     Point's radius is in range.
!
      R           = 2.D4

!
!     Check all coordinates.
!
      EXCLUD = NONE

!
!     Create the input point and test it.
!
      CALL LATREC ( R, LON, LAT, P )


      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )




!***********************************************************************
!
!
!     Systematic cases:
!
!
!***********************************************************************



!
! --- Case: ------------------------------------------------------
!


!
!     Choose a positive margin.
!
      MARGIN = 1.D-6


!
!     Loop over the volume element cases.
!
      DO LONIX = 1, NLON

         BOUNDS(1,1) = MINLON(LONIX)
         BOUNDS(2,1) = MAXLON(LONIX)

!
!        Normalize the element's longitude bounds.
!
         TOL = 1.D-13

         CALL ZZNRMLON ( BOUNDS(1,1), BOUNDS(2,1), TOL, &
                         NRMMIN,      NRMMAX            )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Indicate whether the longitude boundaries exist (vs
!        2*pi longitude extent).
!
         LONBDS = ( NRMMAX - NRMMIN ) .LT. (2*PI())- LONALI


         DO LATIX1 = 1, NLAT

            BOUNDS(1,2) = MINLAT(LATIX1)
!
!           Indicate whether the lower latitude boundary is a surface.
!
            LATLB = ( MINLAT(LATIX1) .GT. -HALFPI() )


            DO LATIX2 = LATIX1, NLAT

               BOUNDS(2,2) = MAXLAT(LATIX2)

!
!              Indicate whether the upper latitude boundary is a
!              surface.
!
               LATUB = ( MAXLAT(LATIX2) .LT. HALFPI() )


               DO RIX1 = 1, NR

                  BOUNDS(1,3) = MINR(RIX1)
!
!                 Indicate whether the lower radius boundary is a
!                 surface.
!
                  RLB = ( MINR(RIX1) .GT. 0.D0 )


                  DO RIX2 = RIX1, NR

                     BOUNDS(2,3) = MAXR(RIX2)

                     DO EXCLUD = 0, 3

!
!-- Case: ------------------------------------------------------
!

!
!                       Set the input point so that each coordinate
!                       is the midpoint of the element's range for
!                       that coordinate.
!

                        STEM = 'Lon #:#; Lat #:#; Rad #:#; ' &
                        //     'EXCLUD = #;'

                        CALL REPMD ( STEM, '#', BOUNDS(1,1)*DPR(), &
                                     9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(2,1)*DPR(), &
                                     9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(1,2)*DPR(), &
                                     9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(2,2)*DPR(), &
                                     9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(1,3), &
                                     9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(2,3), &
                                     9,     STEM                  )
                        CALL REPMI ( STEM, '#', EXCLUD, STEM )

                        CALL CHCKXC ( .FALSE., ' ', OK )


!
!-- Case: ------------------------------------------------------
!

                        TITLE = STEM
                        CALL SUFFIX ( 'Midpoint case', 1, TITLE )
                        CALL TCASE(TITLE)

!
!                        CALL TOSTDO ( TITLE )
!
                        MIDLON = ( NRMMIN      + NRMMAX      ) / 2
                        MIDLAT = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
                        MIDR   = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

                        CALL LATREC ( MIDR, MIDLON, MIDLAT, P )


                        CALL ZZINLAT ( P,      BOUNDS, MARGIN, &
                                       EXCLUD, INSIDE          )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        XIN = .TRUE.

                        CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



!
!-- Case: ------------------------------------------------------
!
!
!                       Check interior points near each corner of
!                       the volume element.
!
                        DO L = 1, 2

                           DO M = 1, 2

                              DO N = 1, 2
!
!-- Case: ------------------------------------------------------
!
                                 TITLE = STEM

                                 CALL SUFFIX ( 'Point near corner ' &
                                 //            '# # #; interior', &
                                               1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

!
!                                Set incremental offsets.
!
                                 AEPS = ( 1.D0 + 1.D-6 ) * MARGIN
                                 EPS  = ( 1.D0 + 1.D-6 ) * MARGIN

!
!                                Multiply the small increments by 1 or
!                                -1 as needed, depending on whether they
!                                are used as offsets from upper or lower
!                                bounds.
!
                                 LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )
                                 R   = BOUNDS(N,3)+  EPS*( (-1)**(N+1) )


                                 IF ( L .EQ. 1 ) THEN

                                    LON = NRMMIN  + AEPS
                                 ELSE
                                    LON = NRMMAX  - AEPS
                                 END IF


                                 CALL LATREC ( R, LON, LAT, P )


                                 CALL ZZINLAT ( P,      BOUNDS, MARGIN, &
                                                EXCLUD, INSIDE         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )


                                 XIN = .TRUE.

                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)




!
!-- Case: ------------------------------------------------------
!
                                 TITLE = STEM
                                 CALL SUFFIX ( 'Point near corner ' &
                                 //            '# # #; interior; ' &
                                 //            'excluded coordinate ' &
                                 //            'out of range.', &
                                               1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

!
!                                Set incremental offsets.
!
                                 AEPS = ( 1.D0 + 1.D-3 ) * MARGIN
                                 EPS  = ( 1.D0 + 1.D-3 ) * MARGIN

!
!                                Multiply the small increments by 1 or
!                                -1 as needed, depending on whether they
!                                are used as offsets from upper or lower
!                                bounds.
!
                                 LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )

                                 IF ( EXCLUD .EQ. 2 ) THEN
!
!                                   Latitude is not considered in
!                                   the bounds comparison performed
!                                   by ZZINLAT.
!
                                    IF ( M .EQ. 1 ) THEN

                                       LAT = -HALFPI() + 10*POLMRG
                                    ELSE
                                       LAT =  HALFPI() - 10*POLMRG
                                    END IF

                                 END IF


                                 R   = BOUNDS(N,3)+  EPS*( (-1)**(N+1) )


                                 IF ( EXCLUD .EQ. 3 ) THEN
!
!                                   Radius is not considered in
!                                   the bounds comparison performed
!                                   by ZZINLAT.
!
                                    IF ( N .EQ. 1 ) THEN
!
!                                      Set R to a small value, but
!                                      not to zero, because in that
!                                      case the point's longitude
!                                      and latitude won't be reliably
!                                      recoverable.
!
                                       R   =  1.D-6
                                    ELSE
                                       R   =  BOUNDS(2,3) * 2
                                    END IF

                                 END IF



                                 IF ( L .EQ. 1 ) THEN

                                    LON = NRMMIN  + AEPS
                                 ELSE
                                    LON = NRMMAX  - AEPS
                                 END IF


                                 IF ( EXCLUD .EQ. 1 ) THEN
!
!                                   Longitude is not considered in
!                                   the bounds comparison performed
!                                   by ZZINLAT.
!
                                    IF ( N .EQ. 1 ) THEN

                                       LON =  NRMMIN - AEPS
                                    ELSE
                                       LON =  NRMMAX + AEPS
                                    END IF

                                 END IF


                                 CALL LATREC ( R, LON, LAT, P )


                                 CALL ZZINLAT ( P,      BOUNDS, MARGIN, &
                                                EXCLUD, INSIDE         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )


                                 XIN = .TRUE.

                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)



!
!-- Case: ------------------------------------------------------
!
                                 TITLE = STEM
                                 CALL SUFFIX ( 'Point near corner ' &
                                 //            '# # #; exterior; ' &
                                 //            'successor of excluded ' &
                                 //            'coordinate ' &
                                 //            'out of range.', &
                                               1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

!
!                                Give XIN its default value. In most
!                                cases, we'll reset it to .FALSE.
!
                                 XIN = .TRUE.

!
!                                Set incremental offsets.
!
                                 AEPS = ( 1.D0 + 1.D-3 ) * MARGIN
                                 EPS  = ( 1.D0 + 1.D-3 ) * MARGIN

                                 IF ( EXCLUD .EQ. 1 ) THEN
!
!                                   Set the latitude value out of range.

                                    IF ( M .EQ. 1 ) THEN

                                       LAT = -HALFPI()
!
!                                      If we managed to set the
!                                      latitude to a value below the
!                                      lower latitude bound, then we
!                                      have an exterior point.
!
                                       XIN = .NOT. LATLB

                                    ELSE

                                       LAT =  HALFPI()

                                       XIN = .NOT. LATUB

                                    END IF

                                 ELSE

                                    LAT = MIDLAT

                                 END IF



                                 IF ( EXCLUD .EQ. 2 ) THEN
!
!                                   Set the radius value out of range.
!
                                    IF ( N .EQ. 1 ) THEN
!
!                                      Set R to a small value, but
!                                      not to zero, because in that
!                                      case the point's longitude
!                                      and latitude won't be reliably
!                                      recoverable.
!
                                       R   =  1.D-6

                                       XIN = .NOT. RLB

                                    ELSE

                                       R   =  BOUNDS(2,3) * 2

                                       XIN = .FALSE.

                                    END IF

                                 ELSE

                                    R = MIDR

                                 END IF


                                 IF ( EXCLUD .EQ. 3 ) THEN
!
!                                   Set the longitude value out of
!                                   range.
!
                                    IF ( L .EQ. 1 ) THEN

                                       IF ( ABS(LAT) .LT. &
                                            HALFPI() - POLMRG ) THEN

                                          LON = NRMMIN - AEPS/COS(LAT)

                                          XIN = .NOT. LONBDS

                                       ELSE
                                          XIN = .TRUE.
                                       END IF

                                    ELSE


                                       IF ( ABS(LAT) .LT. &
                                            HALFPI() - POLMRG ) THEN

                                          LON = NRMMAX + AEPS/COS(LAT)

                                          XIN = .NOT. LONBDS

                                       ELSE
                                          XIN = .TRUE.
                                       END IF

                                    END IF

                                 ELSE

                                    LON = MIDLON

                                 END IF


                                 CALL LATREC ( R, LON, LAT, P )


                                 CALL ZZINLAT ( P,      BOUNDS, MARGIN, &
                                                EXCLUD, INSIDE         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                XIN has been set.
!
                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)


!
!-- Case: ------------------------------------------------------
!
                                 TITLE = STEM
                                 CALL SUFFIX ( 'Point near corner ' &
                                 //            '# # #; exterior; ' &
                                 //            'predecessor of ' &
                                 //            'excluded coordinate ' &
                                 //            'out of range.', &
                                               1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

!
!                                Give XIN its default value. In most
!                                cases, we'll reset it to .FALSE.
!
                                 XIN = .TRUE.

!
!                                Set incremental offsets.
!
                                 AEPS = ( 1.D0 + 1.D-3 ) * MARGIN
                                 EPS  = ( 1.D0 + 1.D-3 ) * MARGIN

                                 IF ( EXCLUD .EQ. 3 ) THEN
!
!                                   Set the latitude value out of range.

                                    IF ( M .EQ. 1 ) THEN

                                       LAT = -HALFPI()
!
!                                      If we managed to set the
!                                      latitude to a value below the
!                                      lower latitude bound, then we
!                                      have an exterior point.
!
                                       XIN = .NOT. LATLB

                                    ELSE

                                       LAT =  HALFPI()

                                       XIN = .NOT. LATUB

                                    END IF

                                 ELSE

                                    LAT = MIDLAT

                                 END IF


                                 IF ( EXCLUD .EQ. 1 ) THEN
!
!                                   Set the radius value out of range.
!
                                    IF ( N .EQ. 1 ) THEN
!
!                                      Set R to a small value, but
!                                      not to zero, because in that
!                                      case the point's longitude
!                                      and latitude won't be reliably
!                                      recoverable.
!
                                       R   =  1.D-6

                                       XIN = .NOT. RLB

                                    ELSE

                                       R   =  BOUNDS(2,3) * 2

                                       XIN = .FALSE.

                                    END IF

                                 ELSE

                                    R = MIDR

                                 END IF


                                 IF ( EXCLUD .EQ. 2 ) THEN
!
!                                   Set the longitude value out of
!                                   range.
!
                                    IF ( L .EQ. 1 ) THEN

                                       IF ( ABS(LAT) .LT. &
                                            HALFPI() - POLMRG ) THEN

                                          LON = NRMMIN - AEPS/COS(LAT)

                                          XIN = .NOT. LONBDS

                                       ELSE
                                          XIN = .TRUE.
                                       END IF

                                    ELSE


                                       IF ( ABS(LAT) .LT. &
                                            HALFPI() - POLMRG ) THEN

                                          LON = NRMMAX + AEPS/COS(LAT)

                                          XIN = .NOT. LONBDS

                                       ELSE
                                          XIN = .TRUE.
                                       END IF

                                    END IF

                                 ELSE

                                    LON = MIDLON

                                 END IF


                                 CALL LATREC ( R, LON, LAT, P )

                                 CALL ZZINLAT ( P,      BOUNDS, MARGIN, &
                                                EXCLUD, INSIDE         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                XIN has been set.
!
                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)


                              END DO
!
!                             End of "N" loop. N selects the radius
!                             upper/lower bound.
!
                           END DO
!
!                          End of "M" loop. M selects the radius
!                          upper/lower bound.
!
                        END DO
!
!                       End of "L" loop. L selects the longitude
!                       upper/lower bound.

                     END DO
!
!                    End of coordinate exclusion (EXCLUD) loop.
!
                  END DO
!
!                 End of upper radius bound loop.
!
               END DO
!
!              End of lower radius bound loop.
!
            END DO
!
!           End of upper latitude bound loop.
!
         END DO
!
!        End of lower latitude bound loop.
!
      END DO
!
!     End of longitude loop.
!


!***********************************************************************
!
!     Error cases
!
!***********************************************************************

!
!     ZZINLAT is "error free."
!



!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


