C$Procedure F_ZZINLAT ( ZZINLAT tests )
 
      SUBROUTINE F_ZZINLAT ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB routine ZZINLAT.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE
      INCLUDE 'dsktol.inc'

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the private SPICELIB routine ZZINLAT.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 12-FEB-2017 (NJB)
C
C        Updated test cases for near-polar cases: now these cases
C        exclude coordinates from testing in the same way as the
C        complementary latitude cases.
C
C        Added test cases for longitude out of range by less than the
C        alias limit.
C
C
C        Previous version 10-OCT-2016 (NJB)
C
C           Removed non-error case with matching minimum and maximum
C           longitudes.
C
C           Removed debugging output and STOP statements.
C
C           Previous version 25-MAY-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C 
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD

C
C     Local Parameters
C
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

C
C     Local Variables 
C
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
      

      
C
C     Saved values
C
      SAVE

C
C     Initial values
C

C
C     Set the initial angular bounds in degrees, since we can
C     do this without function calls. The bounds will be 
C     converted to radians at run time.
C

C
C     For the latitude boundaries, every valid combination of
C     minimum and maximum will be tested.
C
      DATA                  MINLAT / -90.D0,
     .                               -89.999D0,
     .                               -45.D0,
     .                                 0.D0,
     .                                45.D0,
     .                                89.999D0 /

      DATA                  MAXLAT / -89.999D0,
     .                               -45.D0,
     .                                 0.D0,
     .                                45.D0,
     .                                89.999D0,
     .                                90.D0       /


C
C     For the longitude boundaries, each pair of bounds
C     consisting of the Ith minimum and Ith maximum will
C     be tested.
C
      DATA                  MINLON /  -10.D0,
     .                                -10.D0,
     .                               -180.D0,
     .                               -360.D0,
     .                                 10.D0,
     .                                179.999D0,
     .                               -179.999D0,
     .                               -260.D0,
     .                                350.D0       /

      DATA                  MAXLON /   -5.D0,
     .                                 20.D0,
     .                                180.D0,
     .                                  0.D0,
     .                                -10.D0,
     .                               -179.999D0,
     .                                179.999D0,
     .                                200.D0,
     .                               -350.D0       /

C
C     For the radius boundaries, every valid combination of
C     minimum and maximum will be tested.
C
      DATA                  MINR    /   0.D0,
     .                                  1.D-3,
     .                                  1.D3,
     .                                  9.999999D3 / 

      DATA                  MAXR    /   4 * 1.D4   / 



C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZINLAT' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Initialize coordinate bounds' )
 
C
C     Convert angular bounds to radians.
C
      DO I = 1, NLAT

         MINLAT(I) = MINLAT(I) * RPD()
         MAXLAT(I) = MAXLAT(I) * RPD()
     
      END DO


      DO I = 1, NLON

         MINLON(I) = MINLON(I) * RPD()
         MAXLON(I) = MAXLON(I) * RPD()
     
      END DO




C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-origin case: input point is near '
     .//           'the origin and the minimum radius bound '
     .//           'nearly zero. Interior case.'             )

C
C     Longitude bounds:
C     
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

C
C     The longitude is out of range.
C
      LON         = PI()

C
C     Latitude bounds:
C
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = PI()/4

C
C     Point's latitude is out of range.
C
      LAT         = HALFPI() 

C
C     Radius bounds:
C     
      BOUNDS(1,3) = 1.D-8
      BOUNDS(2,3) = 1.D4

      R           = 9.D-9

C
C     Margin:
C
      MARGIN = 1.D-8

C
C     Excluded coordinate: none.
C
      EXCLUD = NONE

C
C     Create the input point and test it.
C
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-origin case: input point is near '
     .//           'the origin and the minimum radius bound '
     .//           'nearly zero. Exterior case.'            )

C
C     Longitude bounds:
C     
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

C
C     The longitude is out of range.
C
      LON         = PI()

C
C     Latitude bounds:
C
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = PI()/4

C
C     Point's latitude is out of range.
C
      LAT         = HALFPI() 

C
C     Radius bounds:
C     
      BOUNDS(1,3) = 1.D-8
      BOUNDS(2,3) = 1.D4

      R           = 9.D-9

C
C     Margin:
C
      MARGIN = 1.D-11

C
C     Excluded coordinate: none.
C
      EXCLUD = NONE

C
C     Create the input point and test it.
C
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-polar latitude (North) case: radius, '
     .//           'latitude, and longitude are within bounds ' 
     .//           '(taking margin into account). No coordinate '
     .//           'is excluded.'                                )

C
C     Excluded coordinate: none.
C
      EXCLUD = NONE

C
C     "Standard" margin:
C
      MARGIN = 1.D-8

C
C     Longitude bounds:
C     
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()/4

C
C     Latitude bounds:
C
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = HALFPI() - 1.D-9

C
C     Point's latitude exceeds upper bound by less than MARGIN.
C
      LAT         = HALFPI() 

C
C     Radius bounds:
C     
      BOUNDS(1,3) = 5.D3
      BOUNDS(2,3) = 1.D4

      R           = 1.D4

C
C     Margin:
C
      MARGIN = 1.D-8

C
C     Excluded coordinate: none.
C
      EXCLUD = NONE

C
C     Create the input point and test it.
C
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Near-polar latitude (South) case: radius, '
     .//           'latitude, and longitude are within bounds ' 
     .//           '(taking margin into account). No coordinate '
     .//           'is excluded.'                                )
 
C
C     Excluded coordinate: none.
C
      EXCLUD = NONE

C
C     "Standard" margin:
C
      MARGIN = 1.D-8
C
C     Latitude bounds:
C
      BOUNDS(1,2) = -HALFPI() + 1.D-9
      BOUNDS(2,2) = 0.D0
C
C     Point's latitude exceeds lower bound by less than MARGIN.
C
      LAT         = -HALFPI() 

C
C     Create the input point and test it.
C
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-polar latitude (North) case: radius and '
     .//           'latitude are within bounds; longitude is not. '
     .//           'No coordinate is excluded.'                    )

C
C     Excluded coordinate: none.
C
      EXCLUD = NONE

C
C     "Standard" margin:
C
      MARGIN = 1.D-8

C
C     Latitude bounds:
C
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = HALFPI() - 1.D-9

C
C     Point's latitude exceeds lower bound by less than MARGIN.
C
      LAT         = HALFPI() 

C
C     Set longitude to a value that's out of bounds. This should
C     not affect the comparison, due to the high latitude bound.
C
      LON = PI()
C
C     Create the input point and test it.
C
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-polar latitude (South) case: radius and '
     .//           'latitude are within bounds; longitude is not. '
     .//           'No coordinate is excluded.'                    )


C
C     Excluded coordinate: none.
C
      EXCLUD = NONE
C
C     "Standard" margin:
C
      MARGIN = 1.D-8

C
C     Latitude bounds:
C
      BOUNDS(1,2) = -HALFPI() + 1.D-9
      BOUNDS(2,2) = 0.D0
C
C     Point's latitude exceeds lower bound by less than MARGIN.
C
      LAT         = -HALFPI() 

C
C     Set longitude to a value that's out of bounds. This should
C     not affect the comparison, due to the high latitude bound.
C
      LON = PI()
C
C     Create the input point and test it.
C
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-polar latitude (North) case: radius is '
     .//           'out of bounds; latitude and longitude are not. '
     .//           'No coordinate is excluded.'                    )


C
C     Set EXCLUD to indicate no exclusion.
C
      EXCLUD = NONE

C
C     "Standard" margin:
C
      MARGIN = 1.D-8

C
C     Latitude bounds:
C
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = HALFPI() - 1.D-9

C
C     Point's latitude exceeds lower bound by less than MARGIN.
C
      LAT         = HALFPI() 

C
C     Set longitude to a value that's in bounds. 
C
      LON = PI()/4

C
C     Radius bounds:
C     
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4

C
C     Create the input point and test it.
C

C
C     R is too large.
C
      R = BOUNDS(2,3) * ( 1.D0  + (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE (large)', INSIDE, XIN, OK )

C
C     R is too small.
C
      R = BOUNDS(1,3) * ( 1.D0  - (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE (small)', INSIDE, XIN, OK )


C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-polar latitude (South) case: radius is '
     .//           'out of bounds; latitude and longitude are not. '
     .//           'No coordinate is excluded.'                    )


C
C     Set EXCLUD to indicate no exclusion.
C
      EXCLUD = NONE

C
C     "Standard" margin:
C
      MARGIN = 1.D-8

C
C     Latitude bounds:
C
      BOUNDS(1,2) = -HALFPI() + 1.D-9
      BOUNDS(2,2) = 0.D0

C
C     Point's latitude exceeds lower bound by less than MARGIN.
C
      LAT         = -HALFPI() 

C
C     Set longitude to a value that's in bounds. 
C
      LON = PI()/4

C
C     Radius bounds:
C     
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4

C
C     Create the input point and test it.
C
 
C
C     R is too large.
C
      R = BOUNDS(2,3) * ( 1.D0  + (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE (large)', INSIDE, XIN, OK )

C
C     R is too small.
C
      R = BOUNDS(1,3) * ( 1.D0  - (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE (small)', INSIDE, XIN, OK )



C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-polar latitude (North) case: radius and '
     .//           'longitude are within bounds; latitude is not. '
     .//           'No coordinate is excluded.'                    )

C
C     Longitude bounds:
C     
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()/4

C
C     Margin:
C
      MARGIN = 1.D-8

C
C     Latitude bounds:
C
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = HALFPI() - 1.D-7

C
C     Point's latitude exceeds upper bound by more than MARGIN.
C
      LAT         = HALFPI() - 1.D-10

C
C     Radius bounds:
C     
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4

      R           = 2.D4

C
C     Set EXCLUD to indicate latitude no exclusion. 
C
      EXCLUD = NONE

C
C     Create the input point and test it.
C
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-polar latitude (South) case: radius and '
     .//           'longitude are within bounds; latitude is not. '
     .//           'No coordinate is excluded.'                     )

C
C     Longitude bounds:
C     
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()/4

C
C     Margin:
C
      MARGIN = 1.D-8

C
C     Latitude bounds:
C
      BOUNDS(1,2) = -HALFPI() + 1.D-7
      BOUNDS(2,2) = 0.D0


C
C     Point's latitude exceeds lower bound by more than MARGIN.
C
      LAT         = -HALFPI() + 1.D-10

C
C     Radius bounds:
C     
      BOUNDS(1,3) = 5.D3
      BOUNDS(2,3) = 1.D4

      R           = 1.D4


C
C     Set EXCLUD to indicate latitude no exclusion.
C
      EXCLUD = NONE

C
C     Create the input point and test it.
C
      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .FALSE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )



C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-polar latitude (North) case: radius and '
     .//           'longitude are out of bounds; latitude is not. '
     .//           'Radius is excluded.'                           )

C
C     Set EXCLUD to exclude radius testing.
C
      EXCLUD = RADIDX

C
C     Margin:
C
      MARGIN = 1.D-8

C
C     Latitude bounds:
C
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = HALFPI() - 1.D-9

C
C     Point's latitude exceeds lower bound by less than MARGIN.
C
      LAT         = HALFPI() 

C
C     Longitude bounds:
C     
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()

C
C     Radius bounds:
C     
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4

 
C
C     Create the input point and test it.
C


C
C     R is too large.
C
      R = BOUNDS(2,3) * ( 1.D0  + (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE (large)', INSIDE, XIN, OK )

C
C     R is too small.
C
      R = BOUNDS(1,3) * ( 1.D0  - (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE (small)', INSIDE, XIN, OK )


C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-polar latitude (South) case: radius and '
     .//           'longitude are out of bounds; latitude is not. '
     .//           'Radius is excluded.'                           )


C
C     Set EXCLUD to exclude radius testing.
C
      EXCLUD = RADIDX
   
C
C     Latitude bounds:
C
      BOUNDS(1,2) = -HALFPI() + 1.D-9
      BOUNDS(2,2) = 0.D0

C
C     Point's latitude is less than lower bound by less than MARGIN.
C
      LAT         = -HALFPI() 

C
C     Longitude bounds:
C     
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()

C
C     Radius bounds:
C     
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4


C
C     Create the input point and test it.
C

C
C     R is too large.
C
      R = BOUNDS(2,3) * ( 1.D0  + (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE (large)', INSIDE, XIN, OK )

C
C     R is too small.
C
      R = BOUNDS(1,3) * ( 1.D0  - (1.1D0 * MARGIN) )

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE (small)', INSIDE, XIN, OK )





C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-polar latitude (North) case: latitude is '
     .//           'out of bounds; radius and longitude are not. '
     .//           'Latitude is excluded.'                         )

C
C     Set EXCLUD to exclude latitude testing.
C
      EXCLUD = LATIDX

C
C     Margin:
C
      MARGIN = 1.D-8

C
C     Latitude bounds:
C
      BOUNDS(1,2) = 0.D0
      BOUNDS(2,2) = HALFPI() - 2.D-8

C
C     Point's latitude exceeds upper bound by more than MARGIN.
C
      LAT         = HALFPI() 

C
C     Longitude bounds:
C     
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()/4

C
C     R is in range.
C
      R = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2
C
C     Create the input point and test it.
C

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )





C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Near-polar latitude (South) case: latitude is '
     .//           'out of bounds; radius and longitude are not. '
     .//           'Latitude is excluded.'                         )

C
C     Set EXCLUD to exclude latitude testing.
C
      EXCLUD = LATIDX

C
C     Margin:
C
      MARGIN = 1.D-8

C
C     Latitude bounds:
C
      BOUNDS(1,2) = -HALFPI() + 2.D-8
      BOUNDS(2,2) = 0.D0

C
C     Point's latitude is below lower bound by more than MARGIN.
C
      LAT         = -HALFPI() 

C
C     Longitude bounds:
C     
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

      LON         = PI()/4

C
C     R is in range.
C
      R = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2
C
C     Create the input point and test it.
C

      CALL LATREC ( R, LON, LAT, P )

      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )




C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Longitude acceptance test for longitude '
     .//           'lower than lower bound by less than ANGMRG. '
     .//           'Other coordinates are in range. Non-polar case.' )

C
C     Margin: positive but small enough so that ANGMRG is larger.
C
      MARGIN = 1.D-14

C
C     Longitude bounds:
C     
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

C
C     The point's longitude is outside the element by more than ANGMRG
C     but by less than (ANGMRG+LONALI).
C
      LON         = BOUNDS(1,1) - ANGMRG - 1.D-13

C
C     Latitude bounds:
C
      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) =  PI()/4

C
C     Point's latitude is in range.
C
      LAT         = 0.D0

C
C     Radius bounds:
C     
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4

C
C     Point's radius is in range.
C
      R           = 2.D4

C 
C     Check all coordinates.
C
      EXCLUD = NONE

C
C     Create the input point and test it. 
C
      CALL LATREC ( R, LON, LAT, P )


      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


C
C --- Case: ------------------------------------------------------
C
 
      CALL TCASE ( 'Longitude acceptance test for longitude '
     .//           'greater than upper bound by less than ANGMRG. '
     .//           'Other coordinates are in range. Non-polar case.' )

C
C     Margin: positive but small enough so that ANGMRG is larger.
C
      MARGIN = 1.D-14

C
C     Longitude bounds:
C     
      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = HALFPI()

C
C     The point's longitude is outside the element by more than ANGMRG
C     but by less than (ANGMRG+LONALI).
C
      LON         = BOUNDS(2,1) + ANGMRG + 1.D-13

C
C     Latitude bounds:
C
      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) =  PI()/4

C
C     Point's latitude is in range.
C
      LAT         = 0.D0

C
C     Radius bounds:
C     
      BOUNDS(1,3) = 1.D4
      BOUNDS(2,3) = 3.D4

C
C     Point's radius is in range.
C
      R           = 2.D4

C 
C     Check all coordinates.
C
      EXCLUD = NONE

C
C     Create the input point and test it. 
C
      CALL LATREC ( R, LON, LAT, P )


      CALL ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XIN = .TRUE.

      CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


      

C***********************************************************************
C
C
C     Systematic cases:
C
C
C***********************************************************************



C
C --- Case: ------------------------------------------------------
C


C
C     Choose a positive margin.
C     
      MARGIN = 1.D-6


C
C     Loop over the volume element cases.
C
      DO LONIX = 1, NLON         

         BOUNDS(1,1) = MINLON(LONIX)
         BOUNDS(2,1) = MAXLON(LONIX)

C
C        Normalize the element's longitude bounds.
C
         TOL = 1.D-13
 
         CALL ZZNRMLON ( BOUNDS(1,1), BOUNDS(2,1), TOL,
     .                   NRMMIN,      NRMMAX            )
         CALL CHCKXC ( .FALSE., ' ', OK )         
C
C        Indicate whether the longitude boundaries exist (vs 
C        2*pi longitude extent).
C
         LONBDS = ( NRMMAX - NRMMIN ) .LT. (2*PI())- LONALI


         DO LATIX1 = 1, NLAT
 
            BOUNDS(1,2) = MINLAT(LATIX1)
C
C           Indicate whether the lower latitude boundary is a surface.
C
            LATLB = ( MINLAT(LATIX1) .GT. -HALFPI() )

 
            DO LATIX2 = LATIX1, NLAT
 
               BOUNDS(2,2) = MAXLAT(LATIX2)
 
C
C              Indicate whether the upper latitude boundary is a
C              surface.
C
               LATUB = ( MAXLAT(LATIX2) .LT. HALFPI() )


               DO RIX1 = 1, NR
 
                  BOUNDS(1,3) = MINR(RIX1)
C
C                 Indicate whether the lower radius boundary is a
C                 surface.
C
                  RLB = ( MINR(RIX1) .GT. 0.D0 )

 
                  DO RIX2 = RIX1, NR
 
                     BOUNDS(2,3) = MAXR(RIX2)
 
                     DO EXCLUD = 0, 3
  
C
C-- Case: ------------------------------------------------------
C
 
C
C                       Set the input point so that each coordinate
C                       is the midpoint of the element's range for
C                       that coordinate.
C

                        STEM = 'Lon #:#; Lat #:#; Rad #:#; '
     .                  //     'EXCLUD = #;'
 
                        CALL REPMD ( STEM, '#', BOUNDS(1,1)*DPR(),
     .                               9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(2,1)*DPR(),
     .                               9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(1,2)*DPR(),
     .                               9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(2,2)*DPR(),
     .                               9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(1,3),
     .                               9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(2,3),
     .                               9,     STEM                  )
                        CALL REPMI ( STEM, '#', EXCLUD, STEM )
 
                        CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C-- Case: ------------------------------------------------------
C
 
                        TITLE = STEM
                        CALL SUFFIX ( 'Midpoint case', 1, TITLE )
                        CALL TCASE(TITLE)
 
C
C                        CALL TOSTDO ( TITLE )
C 
                        MIDLON = ( NRMMIN      + NRMMAX      ) / 2
                        MIDLAT = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
                        MIDR   = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2
 
                        CALL LATREC ( MIDR, MIDLON, MIDLAT, P )
                        

                        CALL ZZINLAT ( P,      BOUNDS, MARGIN,
     .                                 EXCLUD, INSIDE          )
                        CALL CHCKXC ( .FALSE., ' ', OK )
 
                        XIN = .TRUE.
 
                        CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )
 
                        

C
C-- Case: ------------------------------------------------------
C
C
C                       Check interior points near each corner of
C                       the volume element.
C
                        DO L = 1, 2

                           DO M = 1, 2

                              DO N = 1, 2
C
C-- Case: ------------------------------------------------------
C
                                 TITLE = STEM

                                 CALL SUFFIX ( 'Point near corner '
     .                           //            '# # #; interior',
     .                                         1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

C
C                                Set incremental offsets.
C                                
                                 AEPS = ( 1.D0 + 1.D-6 ) * MARGIN
                                 EPS  = ( 1.D0 + 1.D-6 ) * MARGIN
 
C
C                                Multiply the small increments by 1 or 
C                                -1 as needed, depending on whether they
C                                are used as offsets from upper or lower
C                                bounds.
C
                                 LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )
                                 R   = BOUNDS(N,3)+  EPS*( (-1)**(N+1) )


                                 IF ( L .EQ. 1 ) THEN

                                    LON = NRMMIN  + AEPS
                                 ELSE
                                    LON = NRMMAX  - AEPS
                                 END IF


                                 CALL LATREC ( R, LON, LAT, P )
                        

                                 CALL ZZINLAT ( P,      BOUNDS, MARGIN,
     .                                          EXCLUD, INSIDE         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )


                                 XIN = .TRUE.
 
                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)


                                  

C
C-- Case: ------------------------------------------------------
C
                                 TITLE = STEM
                                 CALL SUFFIX ( 'Point near corner '
     .                           //            '# # #; interior; '
     .                           //            'excluded coordinate '
     .                           //            'out of range.',
     .                                         1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

C
C                                Set incremental offsets.
C                                
                                 AEPS = ( 1.D0 + 1.D-3 ) * MARGIN
                                 EPS  = ( 1.D0 + 1.D-3 ) * MARGIN
 
C
C                                Multiply the small increments by 1 or 
C                                -1 as needed, depending on whether they
C                                are used as offsets from upper or lower
C                                bounds.
C
                                 LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )

                                 IF ( EXCLUD .EQ. 2 ) THEN
C
C                                   Latitude is not considered in
C                                   the bounds comparison performed
C                                   by ZZINLAT.
C
                                    IF ( M .EQ. 1 ) THEN

                                       LAT = -HALFPI() + 10*POLMRG
                                    ELSE
                                       LAT =  HALFPI() - 10*POLMRG
                                    END IF
                                    
                                 END IF


                                 R   = BOUNDS(N,3)+  EPS*( (-1)**(N+1) )


                                 IF ( EXCLUD .EQ. 3 ) THEN
C
C                                   Radius is not considered in
C                                   the bounds comparison performed
C                                   by ZZINLAT.
C
                                    IF ( N .EQ. 1 ) THEN
C
C                                      Set R to a small value, but 
C                                      not to zero, because in that
C                                      case the point's longitude
C                                      and latitude won't be reliably
C                                      recoverable.
C
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
C
C                                   Longitude is not considered in
C                                   the bounds comparison performed
C                                   by ZZINLAT.
C
                                    IF ( N .EQ. 1 ) THEN

                                       LON =  NRMMIN - AEPS
                                    ELSE
                                       LON =  NRMMAX + AEPS
                                    END IF
                                    
                                 END IF


                                 CALL LATREC ( R, LON, LAT, P )
                        

                                 CALL ZZINLAT ( P,      BOUNDS, MARGIN,
     .                                          EXCLUD, INSIDE         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

 
                                 XIN = .TRUE.
 
                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)



C
C-- Case: ------------------------------------------------------
C
                                 TITLE = STEM
                                 CALL SUFFIX ( 'Point near corner '
     .                           //            '# # #; exterior; '
     .                           //            'successor of excluded '
     .                           //            'coordinate '
     .                           //            'out of range.',
     .                                         1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

C
C                                Give XIN its default value. In most
C                                cases, we'll reset it to .FALSE.
C
                                 XIN = .TRUE.

C
C                                Set incremental offsets.
C                                
                                 AEPS = ( 1.D0 + 1.D-3 ) * MARGIN
                                 EPS  = ( 1.D0 + 1.D-3 ) * MARGIN
 
                                 IF ( EXCLUD .EQ. 1 ) THEN
C
C                                   Set the latitude value out of range.

                                    IF ( M .EQ. 1 ) THEN

                                       LAT = -HALFPI()
C
C                                      If we managed to set the
C                                      latitude to a value below the
C                                      lower latitude bound, then we
C                                      have an exterior point.
C
                                       XIN = .NOT. LATLB

                                    ELSE

                                       LAT =  HALFPI()

                                       XIN = .NOT. LATUB

                                    END IF

                                 ELSE

                                    LAT = MIDLAT 

                                 END IF



                                 IF ( EXCLUD .EQ. 2 ) THEN
C
C                                   Set the radius value out of range.
C
                                    IF ( N .EQ. 1 ) THEN
C
C                                      Set R to a small value, but 
C                                      not to zero, because in that
C                                      case the point's longitude
C                                      and latitude won't be reliably
C                                      recoverable.
C
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
C
C                                   Set the longitude value out of
C                                   range.
C
                                    IF ( L .EQ. 1 ) THEN

                                       IF ( ABS(LAT) .LT. 
     .                                      HALFPI() - POLMRG ) THEN

                                          LON = NRMMIN - AEPS/COS(LAT)
                                       
                                          XIN = .NOT. LONBDS

                                       ELSE
                                          XIN = .TRUE.
                                       END IF

                                    ELSE


                                       IF ( ABS(LAT) .LT. 
     .                                      HALFPI() - POLMRG ) THEN

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
                        

                                 CALL ZZINLAT ( P,      BOUNDS, MARGIN,
     .                                          EXCLUD, INSIDE         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C                                XIN has been set.
C 
                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)

                                  
C
C-- Case: ------------------------------------------------------
C
                                 TITLE = STEM
                                 CALL SUFFIX ( 'Point near corner '
     .                           //            '# # #; exterior; '
     .                           //            'predecessor of '
     .                           //            'excluded coordinate '
     .                           //            'out of range.',
     .                                         1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

C
C                                Give XIN its default value. In most
C                                cases, we'll reset it to .FALSE.
C
                                 XIN = .TRUE.

C
C                                Set incremental offsets.
C                                
                                 AEPS = ( 1.D0 + 1.D-3 ) * MARGIN
                                 EPS  = ( 1.D0 + 1.D-3 ) * MARGIN
 
                                 IF ( EXCLUD .EQ. 3 ) THEN
C
C                                   Set the latitude value out of range.

                                    IF ( M .EQ. 1 ) THEN

                                       LAT = -HALFPI()
C
C                                      If we managed to set the
C                                      latitude to a value below the
C                                      lower latitude bound, then we
C                                      have an exterior point.
C
                                       XIN = .NOT. LATLB

                                    ELSE

                                       LAT =  HALFPI()

                                       XIN = .NOT. LATUB

                                    END IF

                                 ELSE

                                    LAT = MIDLAT 

                                 END IF


                                 IF ( EXCLUD .EQ. 1 ) THEN
C
C                                   Set the radius value out of range.
C
                                    IF ( N .EQ. 1 ) THEN
C
C                                      Set R to a small value, but 
C                                      not to zero, because in that
C                                      case the point's longitude
C                                      and latitude won't be reliably
C                                      recoverable.
C
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
C
C                                   Set the longitude value out of
C                                   range.
C
                                    IF ( L .EQ. 1 ) THEN

                                       IF ( ABS(LAT) .LT. 
     .                                      HALFPI() - POLMRG ) THEN

                                          LON = NRMMIN - AEPS/COS(LAT)
                                       
                                          XIN = .NOT. LONBDS

                                       ELSE
                                          XIN = .TRUE.
                                       END IF

                                    ELSE


                                       IF ( ABS(LAT) .LT. 
     .                                      HALFPI() - POLMRG ) THEN

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

                                 CALL ZZINLAT ( P,      BOUNDS, MARGIN,
     .                                          EXCLUD, INSIDE         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C                                XIN has been set.
C                                  
                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)


                              END DO
C
C                             End of "N" loop. N selects the radius
C                             upper/lower bound.
C                             
                           END DO
C
C                          End of "M" loop. M selects the radius
C                          upper/lower bound.
C
                        END DO
C
C                       End of "L" loop. L selects the longitude
C                       upper/lower bound.

                     END DO
C
C                    End of coordinate exclusion (EXCLUD) loop.
C 
                  END DO
C
C                 End of upper radius bound loop.
C
               END DO
C
C              End of lower radius bound loop.
C 
            END DO
C
C           End of upper latitude bound loop. 
C
         END DO
C
C        End of lower latitude bound loop.
C
      END DO
C
C     End of longitude loop.
C
 

C***********************************************************************
C
C     Error cases
C
C***********************************************************************

C
C     ZZINLAT is "error free."
C

 

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


