!$Procedure F_ZZINPDT ( ZZINPDT tests )

      SUBROUTINE F_ZZINPDT ( OK )

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZINPDT.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
      INCLUDE 'dskdsc.inc'
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
!     This routine tests the private SPICELIB routine ZZINPDT.
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
!-    TSPICE Version 1.0.0, 13-FEB-2017 (NJB)
!
!        Updated tests for revised near-polar latitude margin
!        and inclusion logic.
!
!        10-OCT-2016 (NJB)
!
!        Removed non-error case with matching minimum and
!        maximum longitudes.
!
!        Previous version 09-AUG-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI

!
!     Local Parameters
!
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )

      INTEGER               LONIDX
      PARAMETER           ( LONIDX = 1 )

      INTEGER               LATIDX
      PARAMETER           ( LATIDX = 2 )

      INTEGER               ALTIDX
      PARAMETER           ( ALTIDX = 3 )


      DOUBLE PRECISION      LATMRG
      PARAMETER           ( LATMRG = 1.D-8 )

      DOUBLE PRECISION      PRVMRG
      PARAMETER           ( PRVMRG = 1.D-6 )

      INTEGER               NLAT
      PARAMETER           ( NLAT   = 6 )

      INTEGER               NLON
      PARAMETER           ( NLON   = 9 )

      INTEGER               NELL
      PARAMETER           ( NELL   = 3 )

      INTEGER               TITLSZ
      PARAMETER           ( TITLSZ = 300 )

!
!     Local Variables
!
      CHARACTER*(TITLSZ)    STEM
      CHARACTER*(TITLSZ)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      AEPS
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      EPS
      DOUBLE PRECISION      EQHI
      DOUBLE PRECISION      EQLOW
      DOUBLE PRECISION      EQRAD  ( NELL )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      HIALT
      DOUBLE PRECISION      HIGHPT ( 3 )
      DOUBLE PRECISION      HILAT
      DOUBLE PRECISION      HILON
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LOWALT
      DOUBLE PRECISION      LOWLAT
      DOUBLE PRECISION      LOWLON
      DOUBLE PRECISION      LOWPT  ( 3 )
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      MAXALT ( NELL )
      DOUBLE PRECISION      MAXLAT ( NLAT )
      DOUBLE PRECISION      MAXLON ( NLON )
      DOUBLE PRECISION      MIDLAT
      DOUBLE PRECISION      MIDLON
      DOUBLE PRECISION      MIDALT
      DOUBLE PRECISION      MINALT ( NELL )
      DOUBLE PRECISION      MINLAT ( NLAT )
      DOUBLE PRECISION      MINLON ( NLON )
      DOUBLE PRECISION      MODBDS ( 2, 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      NRMMAX
      DOUBLE PRECISION      NRMMIN
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      P2     ( 3 )
      DOUBLE PRECISION      P3     ( 3 )
      DOUBLE PRECISION      POLHI
      DOUBLE PRECISION      POLLOW
      DOUBLE PRECISION      POLRAD ( NELL )
      DOUBLE PRECISION      POSMRG
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )

      INTEGER               EXCLUD
      INTEGER               I
      INTEGER               L
      INTEGER               LATIX1
      INTEGER               LATIX2
      INTEGER               LONIX
      INTEGER               M
      INTEGER               N
      INTEGER               SHAPIX

      LOGICAL               FIRST
      LOGICAL               FOUND
      LOGICAL               INSIDE
      LOGICAL               LATLB
      LOGICAL               LATUB
      LOGICAL               LONBDS
      LOGICAL               XIN


!
!     Saved values
!
      SAVE

!
!     Initial values
!
      DATA                  FIRST  / .TRUE. /

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
                                     -89.999999D0, &
                                     -45.D0, &
                                       0.D0, &
                                      45.D0, &
                                      89.999999D0 /

      DATA                  MAXLAT / -89.999999D0, &
                                     -45.D0, &
                                       0.D0, &
                                      45.D0, &
                                      89.999999D0, &
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
                                      179.999999D0, &
                                     -179.999999D0, &
                                     -260.D0, &
                                      350.D0       /

      DATA                  MAXLON /   -5.D0, &
                                       20.D0, &
                                      180.D0, &
                                        0.D0, &
                                      -10.D0, &
                                     -179.999999D0, &
                                      179.999999D0, &
                                      200.D0, &
                                     -350.D0       /

!
!     For the altitude boundaries, there is one valid combination of
!     minimum and maximum for each shape.
!
      DATA                  MINALT  /  -2.0D2,  -1.5D2,  2.0D2 /
      DATA                  MAXALT  /  -1.0D2,   0.5D2,  3.0D2 /

      DATA                  EQRAD   /   1.0D4,   0.8D4,  2.0D4 /
      DATA                  POLRAD  /   0.8D4,   1.0D4,  0.5D4 /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZINPDT' )



!***********************************************************************
!
!     Setup
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize coordinate bounds' )

!
!     Convert angular bounds to radians.
!

!
!     First pass only!
!
      IF ( FIRST ) THEN

         DO I = 1, NLAT

            MINLAT(I) = MINLAT(I) * RPD()
            MAXLAT(I) = MAXLAT(I) * RPD()

         END DO


         DO I = 1, NLON

            MINLON(I) = MINLON(I) * RPD()
            MAXLON(I) = MAXLON(I) * RPD()

         END DO

         FIRST = .FALSE.

      END IF

!
!     Initialize CORPAR.
!
      CALL CLEARD ( NSYPAR, CORPAR )



!***********************************************************************
!
!     Error cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: negative margin.' )

      RE = 3000.D0
      F  =    0.1D0

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  TWOPI()

      BOUNDS(1,2) = -PI()/2
      BOUNDS(2,2) =  PI()/2

      BOUNDS(1,3) = -RE/2.D0
      BOUNDS(2,3) =  RE/2.D0

      CORPAR(1)   =  RE
      CORPAR(2)   =  F

      CALL VPACK ( 3010.D0, 3020.D0, 2800.D0, P )

      MARGIN = -1.D-6

      EXCLUD =  NONE

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC  ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: invalid value of EXCLUD' )

      MARGIN = 1.D-6

      EXCLUD = -1

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC  ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      EXCLUD =  4

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC  ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: longitude bounds out of range.' )

      MARGIN = 1.D-6

      EXCLUD = ALTIDX

      BOUNDS(1,1) = -3*PI()
      BOUNDS(2,1) = -TWOPI()

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC  ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC  ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!***********************************************************************
!
!
!     Special non-error cases
!
!
!***********************************************************************


!
!     Test special logic for positions near the poles.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment includes north pole. Point''s longitude ' &
      //           'is out of range; altitude is in range.'         )

      RE = 3000.D0
      F  =    0.1D0

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  PI()/4

      BOUNDS(1,2) =  0.D0
      BOUNDS(2,2) =  PI()/2

      BOUNDS(1,3) = -100.D0
      BOUNDS(2,3) =  100.D0

      CORPAR(1)   =  RE
      CORPAR(2)   =  F

      LAT = BOUNDS(2,2) - 1.D-8
      LON = PI()/2
      ALT = 0.D0

      CALL GEOREC ( LON, LAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )


      MARGIN =  1.D-7

      EXCLUD =  NONE

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'INSIDE', INSIDE, .TRUE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment includes north pole. Point''s longitude ' &
      //           'is out of range; altitude is out of range.'      )

      MARGIN =  1.D-7

      A = RE
      B = RE * ( 1.D0-F )

      CALL ZZELLBDS ( A, &
                      B, &
                      BOUNDS(2,3)*(1+MARGIN), &
                      BOUNDS(1,3)*(1-MARGIN), &
                      EQHI, &
                      POLHI, &
                      EQLOW, &
                      POLLOW       )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Start with a point on the reference ellipsoid. Find the point
!     directly above this on the outer bounding ellipsoid.
!
      LAT = BOUNDS(2,2) - 1.D-8
      LON = PI()/2
      ALT = 0.D0

      CALL GEOREC ( LON, LAT, ALT, RE, F, VERTEX )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SURFNM ( A, A, B, VERTEX, NORMAL )

!
!     Find the surface point on the lower bounding ellipsoid, at the
!     given planetodetic longitude and latitude (relative to the
!     reference ellipsoid).
!

      CALL SURFPT ( VERTEX, NORMAL, &
                    EQHI,   EQHI, POLHI, &
                    HIGHPT, FOUND         )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We should never fail to find an intercept.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

      CALL VLCOM ( 1.D0, HIGHPT, 1.D-6, NORMAL, P )


      EXCLUD =  NONE

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'INSIDE', INSIDE, .FALSE., OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment''s upper latitude bound is near ' &
      //           'north pole. Point''s longitude ' &
      //           'is out of range; altitude and latitude ' &
      //           'are in range.'                          )

      MARGIN = 1.D-8


      RE = 3000.D0
      F  =    0.1D0

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  PI()/4

      BOUNDS(1,2) =  0.D0
      BOUNDS(2,2) =  ( PI()/2 ) - 1.5D0*MARGIN

      BOUNDS(1,3) = -100.D0
      BOUNDS(2,3) =  100.D0

      CORPAR(1)   =  RE
      CORPAR(2)   =  F

      LAT = BOUNDS(2,2) + MARGIN/2
      LON = PI()/2
      ALT = 0.D0

      CALL GEOREC ( LON, LAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )


      EXCLUD =  NONE

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'INSIDE', INSIDE, .TRUE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment''s upper latitude bound is near ' &
      //           'north pole. Point''s longitude ' &
      //           'is out of range; altitude is in range; ' &
      //           'latitude is out of range.'              )

      MARGIN = 1.D-8


      RE = 3000.D0
      F  =    0.1D0

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  PI()/4

      BOUNDS(1,2) =  0.D0
      BOUNDS(2,2) =  ( PI()/2 ) - 1.D-7

      BOUNDS(1,3) = -100.D0
      BOUNDS(2,3) =  100.D0

      CORPAR(1)   =  RE
      CORPAR(2)   =  F

      LAT = BOUNDS(2,2) + MARGIN*2
      LON = PI()/2
      ALT = 0.D0

      CALL GEOREC ( LON, LAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )


      EXCLUD =  NONE

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'INSIDE', INSIDE, .FALSE., OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment''s upper latitude bound is near ' &
      //           'north pole. Point''s longitude ' &
      //           'is out of range; latitude is in range; ' &
      //           'altitude is out of range.'              )

      MARGIN =  1.D-7

      A = RE
      B = RE * ( 1.D0-F )

      CALL ZZELLBDS ( A, &
                      B, &
                      BOUNDS(2,3)*(1+MARGIN), &
                      BOUNDS(1,3)*(1-MARGIN), &
                      EQHI, &
                      POLHI, &
                      EQLOW, &
                      POLLOW       )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Start with a point on the reference ellipsoid. Find the point
!     directly above this on the outer bounding ellipsoid.
!
      LAT = BOUNDS(2,2) + MARGIN/2
      LON = PI()/2
      ALT = 0.D0

      CALL GEOREC ( LON, LAT, ALT, RE, F, VERTEX )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SURFNM ( A, A, B, VERTEX, NORMAL )

!
!     Find the surface point on the lower bounding ellipsoid, at the
!     given planetodetic longitude and latitude (relative to the
!     reference ellipsoid).
!

      CALL SURFPT ( VERTEX, NORMAL, &
                    EQHI,   EQHI, POLHI, &
                    HIGHPT, FOUND         )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We should never fail to find an intercept.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

      CALL VLCOM ( 1.D0, HIGHPT, 1.D-6, NORMAL, P )


      EXCLUD =  NONE

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'INSIDE', INSIDE, .FALSE., OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment includes south pole. Point''s longitude ' &
      //           'is out of range; altitude is in range.'         )

      RE = 3000.D0
      F  =    0.1D0

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  PI()/4

      BOUNDS(1,2) = -PI()/2
      BOUNDS(2,2) =  0.D0

      BOUNDS(1,3) = -100.D0
      BOUNDS(2,3) =  100.D0

      CORPAR(1)   =  RE
      CORPAR(2)   =  F

      LAT = BOUNDS(1,2) + 1.D-8
      LON = PI()/2
      ALT = 0.D0

      CALL GEOREC ( LON, LAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )


      MARGIN =  1.D-7

      EXCLUD =  NONE

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'INSIDE', INSIDE, .TRUE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment includes south pole. Point''s longitude ' &
      //           'is out of range; altitude is out of range.'      )

      MARGIN =  1.D-7

      A = RE
      B = RE * ( 1.D0-F )

      CALL ZZELLBDS ( A, &
                      B, &
                      BOUNDS(2,3)*(1+MARGIN), &
                      BOUNDS(1,3)*(1-MARGIN), &
                      EQHI, &
                      POLHI, &
                      EQLOW, &
                      POLLOW       )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Start with a point on the reference ellipsoid. Find the point
!     directly above this on the outer bounding ellipsoid.
!
      LAT = BOUNDS(1,2) + 1.D-8
      LON = PI()/2
      ALT = 0.D0

      CALL GEOREC ( LON, LAT, ALT, RE, F, VERTEX )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SURFNM ( A, A, B, VERTEX, NORMAL )

!
!     Find the surface point on the lower bounding ellipsoid, at the
!     given planetodetic longitude and latitude (relative to the
!     reference ellipsoid).
!

      CALL SURFPT ( VERTEX, NORMAL, &
                    EQHI,   EQHI, POLHI, &
                    HIGHPT, FOUND         )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We should never fail to find an intercept.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

      CALL VLCOM ( 1.D0, HIGHPT, 1.D-6, NORMAL, P )


      EXCLUD =  NONE

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'INSIDE', INSIDE, .FALSE., OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment''s lower latitude bound is near ' &
      //           'south pole. Point''s longitude ' &
      //           'is out of range; altitude and latitude ' &
      //           'are in range.'                          )

      MARGIN = 1.D-8


      RE = 3000.D0
      F  =    0.1D0

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  PI()/4

      BOUNDS(1,2) =  -PI()/2 + 1.5D0*MARGIN
      BOUNDS(2,2) =  0.D0

      BOUNDS(1,3) = -100.D0
      BOUNDS(2,3) =  100.D0

      CORPAR(1)   =  RE
      CORPAR(2)   =  F

      LAT = BOUNDS(1,2) - MARGIN/2
      LON = PI()/2
      ALT = 0.D0

      CALL GEOREC ( LON, LAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )


      EXCLUD =  NONE

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'INSIDE', INSIDE, .TRUE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment''s lower latitude bound is near ' &
      //           'north pole. Point''s longitude ' &
      //           'is out of range; altitude is in range; ' &
      //           'latitude is out of range.'              )

      MARGIN = 1.D-8


      RE = 3000.D0
      F  =    0.1D0

      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  PI()/4

      BOUNDS(1,2) =  0.D0
      BOUNDS(2,2) =  ( PI()/2 ) - 1.D-7

      BOUNDS(1,3) = -100.D0
      BOUNDS(2,3) =  100.D0

      CORPAR(1)   =  RE
      CORPAR(2)   =  F

      LAT = BOUNDS(2,2) + MARGIN*2
      LON = PI()/2
      ALT = 0.D0

      CALL GEOREC ( LON, LAT, ALT, RE, F, P )
      CALL CHCKXC ( .FALSE., ' ', OK )


      EXCLUD =  NONE

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'INSIDE', INSIDE, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Segment''s lower latitude bound is near ' &
      //           'south pole. Point''s longitude ' &
      //           'is out of range; latitude is in range; ' &
      //           'altitude is out of range.'              )

      MARGIN =  1.D-7

      A = RE
      B = RE * ( 1.D0-F )

      CALL ZZELLBDS ( A, &
                      B, &
                      BOUNDS(2,3)*(1+MARGIN), &
                      BOUNDS(1,3)*(1-MARGIN), &
                      EQHI, &
                      POLHI, &
                      EQLOW, &
                      POLLOW       )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Start with a point on the reference ellipsoid. Find the point
!     directly above this on the outer bounding ellipsoid.
!
      LAT = BOUNDS(1,2) - MARGIN/2
      LON = PI()/2
      ALT = 0.D0

      CALL GEOREC ( LON, LAT, ALT, RE, F, VERTEX )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SURFNM ( A, A, B, VERTEX, NORMAL )

!
!     Find the surface point on the lower bounding ellipsoid, at the
!     given planetodetic longitude and latitude (relative to the
!     reference ellipsoid).
!

      CALL SURFPT ( VERTEX, NORMAL, &
                    EQHI,   EQHI, POLHI, &
                    HIGHPT, FOUND         )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We should never fail to find an intercept.
!
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

      CALL VLCOM ( 1.D0, HIGHPT, 1.D-6, NORMAL, P )


      EXCLUD =  NONE

      CALL ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'INSIDE', INSIDE, .FALSE., OK )



!***********************************************************************
!
!     Normal cases
!
!***********************************************************************



!
! --- Case: ------------------------------------------------------
!

!
!     Choose a positive margin. We'll vary the margin in some
!     test cases, so we use POSMRG to save the original value.
!
      POSMRG = 1.D-6
      MARGIN = POSMRG

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


               DO SHAPIX = 1, NELL

                  A           = EQRAD (SHAPIX)
                  B           = POLRAD(SHAPIX)
                  F           = ( A - B ) / A

                  CORPAR(1)   = A
                  CORPAR(2)   = F

                  BOUNDS(1,3) = MINALT(SHAPIX)
                  BOUNDS(2,3) = MAXALT(SHAPIX)

                  DO EXCLUD = 0, 3

!
! --- Case: ------------------------------------------------------
!

!
!                    Set the input point so that each coordinate
!                    is the midpoint of the element's range for
!                    that coordinate.
!

                     STEM = 'Lon #:#; Lat #:#; Alt #:#; ' &
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


                     I = I + 1


!
! --- Case: ------------------------------------------------------
!

                     TITLE = STEM
                     CALL SUFFIX ( 'Midpoint case, positive margin ', &
                                   1, TITLE )
                     CALL TCASE(TITLE)

                     MIDLON = ( NRMMIN      + NRMMAX      ) / 2
                     MIDLAT = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
                     MIDALT = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2


                     CALL GEOREC ( MIDLON, MIDLAT, MIDALT, A, F, P )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL ZZINPDT( P,      BOUNDS, CORPAR, &
                                   MARGIN, EXCLUD, INSIDE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     XIN = .TRUE.

                     CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )


!
! --- Case: ------------------------------------------------------
!

                     TITLE = STEM
                     CALL SUFFIX ( 'Midpoint case, zero margin', &
                                   1, TITLE )
                     CALL TCASE(TITLE)


                     MARGIN = 0.D0

                     MIDLON = ( NRMMIN      + NRMMAX      ) / 2
                     MIDLAT = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
                     MIDALT = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2


                     CALL GEOREC ( MIDLON, MIDLAT, MIDALT, A, F, P )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL ZZINPDT( P,      BOUNDS, CORPAR, &
                                   MARGIN, EXCLUD, INSIDE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     XIN = .TRUE.

                     CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

!
!                    Restore margin.
!
                     MARGIN = POSMRG

!
! --- Case: ------------------------------------------------------
!
!
!                    Check interior points near each corner of
!                    the volume element.
!
                     DO L = 1, 2

                        DO M = 1, 2

                           DO N = 1, 2


                              I = I + 1
!
! --- Case: ------------------------------------------------------
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
!                             Set incremental offsets. AEPS is the
!                             "angular epsilon." EPS is scaled to
!                             yield a "distance epsilon."
!
                              AEPS = 1.D-11
                              EPS  = 1.D-6 * A

!
!                             Obtain the ellipsoid radii of the lower
!                             and upper bounding surfaces. It is these,
!                             not the surfaces of minimum and maximum
!                             altitude relative to the body's reference
!                             ellipsoid, that are used for comparison.
!
                              IF ( A .GT. B ) THEN

                                 CALL ZZELLBDS ( A, &
                                                 B, &
                                                 BOUNDS(2,3)*(1+MARGIN), &
                                                 BOUNDS(1,3)*(1-MARGIN), &
                                                 EQHI, &
                                                 POLHI, &
                                                 EQLOW, &
                                                 POLLOW       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )


                              ELSE
!
!                                This is the prolate case. ZZELLBDS
!                                works only with oblate spheroids,
!                                so swap A and B on input.
!
                                 CALL ZZELLBDS ( B, &
                                                 A, &
                                                 BOUNDS(2,3)*(1+MARGIN), &
                                                 BOUNDS(1,3)*(1-MARGIN), &
                                                 EQHI, &
                                                 POLHI, &
                                                 EQLOW, &
                                                 POLLOW       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                The EQ* outputs are actually for
!                                the polar radii of the bounding
!                                ellipsoids, and the POL* outputs
!                                are for the equatorial radii of
!                                the bounding ellipsoids.
!
                                 CALL SWAPD ( EQHI,  POLHI  )
                                 CALL SWAPD ( EQLOW, POLLOW )

                              END IF
!
!                             Multiply the small increments by 1 or
!                             -1 as needed, depending on whether they
!                             are used as offsets from upper or lower
!                             bounds.
!
                              LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )


                              IF ( L .EQ. 1 ) THEN

                                 LON = NRMMIN  + AEPS
                              ELSE
                                 LON = NRMMAX  - AEPS
                              END IF

!
!                             Find the point at the given longitude
!                             and latitude on the reference ellipsoid.
!                             We'll use this below.
!
                              CALL GEOREC ( LON,    LAT, 0.D0, A, F, &
                                            VERTEX                  )
                              CALL CHCKXC ( .FALSE., ' ', OK )


                              CALL SURFNM ( A, A, B, VERTEX, NORMAL )


                              IF ( N .EQ. 1 ) THEN
!
!                                Find the surface point on the lower
!                                bounding ellipsoid, at the given
!                                planetodetic longitude and latitude
!                                (relative to the reference ellipsoid).

                                 IF ( BOUNDS(1,3) .LT. 0.D0 ) THEN
!
!                                   The direction to the inner bounding
!                                   ellipsoid is "down."
!
                                    CALL VMINUS ( NORMAL, DIR )
                                 ELSE
                                    CALL VEQU   ( NORMAL, DIR )
                                 END IF

                                 CALL SURFPT ( VERTEX, DIR, &
                                               EQLOW,  EQLOW, POLLOW, &
                                               LOWPT,  FOUND         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                We should never fail to find an
!                                intercept.
!
                                 CALL CHCKSL ( 'FOUND', FOUND, &
                                               .TRUE.,  OK     )

!
!                                Find the geodetic coordinates of LOWPT
!                                with respect to the reference
!                                ellipsoid.
!
                                 CALL RECGEO ( LOWPT,  A,      F, &
                                               LOWLON, LOWLAT, LOWALT )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                Adjust the altitude of LOWPT by a
!                                small increment.
!
                                 ALT = LOWALT +  EPS*( (-1)**(N+1) )


                              ELSE
!
!                                This is the case for the outer
!                                bounding ellipsoid (N = 2).
!
                                 IF ( BOUNDS(2,3) .LT. 0.D0 ) THEN
!
!                                   The direction to the outer bounding
!                                   ellipsoid is "down."
!
                                    CALL VMINUS ( NORMAL, DIR )
                                 ELSE
                                    CALL VEQU   ( NORMAL, DIR )
                                 END IF

                                 CALL SURFPT ( VERTEX, DIR, &
                                               EQHI,   EQHI, POLHI, &
                                               HIGHPT, FOUND       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                We should never fail to find an
!                                intercept.
!
                                 CALL CHCKSL ( 'FOUND', FOUND, &
                                               .TRUE.,  OK     )
!
!                                Find the geodetic coordinates of
!                                HIGHPT with respect to the reference
!                                ellipsoid.
!
                                 CALL RECGEO ( HIGHPT, A,     F, &
                                               HILON,  HILAT, HIALT )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                Adjust the altitude of HIGHPT by a
!                                small increment.
!
                                 ALT = HIALT +  EPS*( (-1)**(N+1) )

                              END IF

!
!                             Produce the perturbed input point using
!                             the perturbed geodetic coordinates.
!

                              CALL GEOREC ( LON, LAT, ALT, A, F, P2 )
                              CALL CHCKXC ( .FALSE., ' ', OK )


                              CALL ZZINPDT( P2,     BOUNDS, CORPAR, &
                                            MARGIN, EXCLUD, INSIDE )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              XIN = .TRUE.

                              CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)

!
! --- Case: ------------------------------------------------------
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
!                             Set initial coordinates of the input
!                             point.
!
                              CALL RECGEO ( P2, A, F, LON, LAT, ALT )
                              CALL CHCKXC ( .FALSE., ' ', OK )

!
!                             Use the perturbed point produced for
!                             the previous test case. Modify the
!                             excluded coordinate of the point.
!
                              IF ( EXCLUD .EQ. 2 ) THEN
!
!                                Latitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT.
!
!                                However, we can't move the latitude
!                                too far out of range, or else the
!                                relationship between the altitude
!                                of the point and the bounding ellipsoid
!                                will change too much.
!
                                 IF ( M .EQ. 1 ) THEN

                                    LAT = MAX( -HALFPI(), LAT-2*MARGIN )
                                 ELSE
                                    LAT = MIN(  HALFPI(), LAT+2*MARGIN )
                                 END IF

                              END IF


                              IF ( EXCLUD .EQ. 3 ) THEN
!
!                                Altitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT.
!
                                 IF ( N .EQ. 1 ) THEN
                                    ALT = ALT  -  0.1D0*ABS(ALT)
                                 ELSE
                                    ALT = ALT  +  0.1D0*ABS(ALT)
                                 END IF

                              END IF


                              IF ( EXCLUD .EQ. 1 ) THEN
!
!                                Longitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT.
!
                                 IF ( N .EQ. 1 ) THEN

                                    LON =  NRMMIN - AEPS
                                 ELSE
                                    LON =  NRMMAX + AEPS
                                 END IF

                              END IF




                              CALL GEOREC ( LON, LAT, ALT, A, F, P3 )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL ZZINPDT( P3,     BOUNDS, CORPAR, &
                                            MARGIN, EXCLUD, INSIDE )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              XIN = .TRUE.

                              CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)


!
! --- Case: ------------------------------------------------------
!
                              TITLE = STEM
                              CALL SUFFIX ( 'Point near corner ' &
                              //            '# # #; exterior; ' &
                              //            'successor of excluded ' &
                              //            'coordinate out of range.', &
                                            1, TITLE            )
                              CALL REPMI ( TITLE, '#', L, TITLE )
                              CALL REPMI ( TITLE, '#', M, TITLE )
                              CALL REPMI ( TITLE, '#', N, TITLE )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL TCASE(TITLE)

!
!                             Set initial coordinates of the input
!                             point.
!
                              CALL RECGEO ( P2, A, F, LON, LAT, ALT )
                              CALL CHCKXC ( .FALSE., ' ', OK )


!
!                             We're going to work with a modified
!                             version of the input bounds. This will
!                             make it easier to create coordinates that
!                             are slightly out of range.
!
                              CALL MOVED ( BOUNDS, 6, MODBDS )

!
!                             Use the perturbed point produced for
!                             the previous test case. Modify the
!                             excluded coordinate of the point.
!
                              IF ( EXCLUD .EQ. 1 ) THEN
!
!                                Longitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT. We'll move the latitude
!                                out of bounds.
!
!                                However, we can't move the latitude
!                                too far out of range, or else the
!                                relationship between the altitude
!                                of the point and the bounding ellipsoid
!                                will change too much.
!
                                 IF ( M .EQ. 1 ) THEN

                                    LAT = MAX( -HALFPI(), LAT-2*MARGIN )
                                 ELSE
                                    LAT = MIN(  HALFPI(), LAT+2*MARGIN )
                                 END IF

                              END IF


                              IF (      ( EXCLUD .EQ. 2 ) &
                                   .OR. ( EXCLUD .EQ. 0 ) ) THEN
!
!                                Either all coordinates are considered,
!                                or latitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT. We'll move the altitude
!                                out of bounds.
!

                                 IF ( N .EQ. 1 ) THEN
                                    ALT = ALT  -  0.1D0*ABS(ALT)
                                 ELSE
                                    ALT = ALT  +  0.1D0*ABS(ALT)
                                 END IF

                              END IF


                              IF ( EXCLUD .EQ. 3 ) THEN
!
!                                Altitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT. We'll move the
!                                longitude out of bounds.
!
!                                To make it possible to create
!                                longitude values that are out
!                                of bounds, adjust the longitude
!                                bounds. We'll nudge the lower
!                                longitude bound upwards for this
!                                test.
!
!                                The magnitude of the "nudge" must
!                                be great enough to overcome the
!                                rounding margin used (even) by the
!                                "no margin" routine ZZINPDT for
!                                longitude comparisons.
!
                                 MODBDS(1,1) = MODBDS(1,1) + 4*MARGIN
                                 MODBDS(2,1) = MODBDS(2,1) - 4*MARGIN

                                 IF ( N .EQ. 1 ) THEN

                                    LON =  NRMMIN - 2*MARGIN
                                 ELSE
                                    LON =  NRMMAX + 2*MARGIN
                                 END IF

                              END IF

                              CALL GEOREC ( LON, LAT, ALT, A, F, P3 )
                              CALL CHCKXC ( .FALSE., ' ', OK )

!
!                             Perform the test using modified bounds.
!                             See the EXCLUD = 3 case above.
!

                              CALL ZZINPDT( P3,     MODBDS, CORPAR, &
                                            MARGIN, EXCLUD, INSIDE )
                              CALL CHCKXC ( .FALSE., ' ', OK )

!
!                             Except for some special cases, the
!                             point should be outside the element.
!
                              XIN = .FALSE.
!
!                             Our latitude modifications won't work for
!                             the cases where the latitude is already
!                             at an extreme value.
!
                              IF ( EXCLUD .EQ. 1 ) THEN
!
!                                Longitude is the excluded coordinate;
!                                this is a case for which we try to
!                                modify latitude. However, the
!                                attempted modification may have no
!                                effect if a boundary is already at
!                                the extreme value.
!
                                 IF (      (  M    .EQ.  1 ) &
                                     .AND. ( .NOT. LATLB   ) ) THEN

                                    XIN = .TRUE.

                                 ELSE IF (      (  M    .EQ.  2 ) &
                                          .AND. ( .NOT. LATUB   ) ) THEN

                                    XIN = .TRUE.

                                 END IF

!
!                                If the latitude is within the latitude
!                                range extended by the margin, the
!                                point is in the element.
!
                                 IF (    (LAT .GE. BOUNDS(1,2)-MARGIN ) &
                                    .AND.(LAT .LE. BOUNDS(2,2)+MARGIN )) &
                                 THEN

                                    XIN = .TRUE.

                                 END IF


                              ELSE IF ( EXCLUD .EQ. 3 ) THEN
!
!                                Longitude will be set to an
!                                out-of-range value. This won't
!                                be considered if we're in the
!                                "high latitude" case. We use the
!                                old value of LATMRG here, because
!                                with the new value one cannot create
!                                longitudes that are out of range, due
!                                to the longitude margin scaling.
!
                                 IF (    (LAT .GT. HALFPI()-PRVMRG ) &
                                    .AND.(LAT .LE. BOUNDS(2,2)     ) ) &
                                 THEN

                                    XIN = .TRUE.

                                 END IF

                                 IF (    (LAT .LT. -HALFPI()+PRVMRG ) &
                                    .AND.(LAT .GE.  BOUNDS(1,2)     ) ) &
                                 THEN

                                    XIN = .TRUE.

                                 END IF

                              END IF


                              CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)



!***********************************************************************
!
!
!     ZERO MARGIN CASES
!
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
                              MARGIN = 0.D0



                              TITLE = STEM

                              CALL SUFFIX ( 'Point near corner ' &
                              //            '# # #; interior; ' &
                              //            'zero margin ', &
                                            1, TITLE            )
                              CALL REPMI ( TITLE, '#', L, TITLE )
                              CALL REPMI ( TITLE, '#', M, TITLE )
                              CALL REPMI ( TITLE, '#', N, TITLE )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL TCASE(TITLE)

!
!                             Set incremental offsets. AEPS is the
!                             "angular epsilon." EPS is scaled to
!                             yield a "distance epsilon."
!
                              AEPS = 1.D-9
                              EPS  = 1.D-9 * A

!
!                             Obtain the ellipsoid radii of the lower
!                             and upper bounding surfaces. It is these,
!                             not the surfaces of minimum and maximum
!                             altitude relative to the body's reference
!                             ellipsoid, that are used for comparison.
!
                              IF ( A .GT. B ) THEN

                                 CALL ZZELLBDS ( A, &
                                                 B, &
                                                 BOUNDS(2,3), &
                                                 BOUNDS(1,3), &
                                                 EQHI, &
                                                 POLHI, &
                                                 EQLOW, &
                                                 POLLOW       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )


                              ELSE
!
!                                This is the prolate case. ZZELLBDS
!                                works only with oblate spheroids,
!                                so swap A and B on input.
!
                                 CALL ZZELLBDS ( B, &
                                                 A, &
                                                 BOUNDS(2,3), &
                                                 BOUNDS(1,3), &
                                                 EQHI, &
                                                 POLHI, &
                                                 EQLOW, &
                                                 POLLOW       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                The EQ* outputs are actually for
!                                the polar radii of the bounding
!                                ellipsoids, and the POL* outputs
!                                are for the equatorial radii of
!                                the bounding ellipsoids.
!
                                 CALL SWAPD ( EQHI,  POLHI  )
                                 CALL SWAPD ( EQLOW, POLLOW )

                              END IF
!
!                             Multiply the small increments by 1 or
!                             -1 as needed, depending on whether they
!                             are used as offsets from upper or lower
!                             bounds.
!
!                             Note the scaled increment for latitude.
!
                              LAT = BOUNDS(M,2)+ AEPS*((-1)**(M+1))


                              IF ( L .EQ. 1 ) THEN

                                 LON = NRMMIN  + AEPS
                              ELSE
                                 LON = NRMMAX  - AEPS
                              END IF


!
!                             Find the point at the given longitude
!                             and latitude on the reference ellipsoid.
!                             We'll use this below.
!
                              CALL GEOREC ( LON,    LAT, 0.D0, A, F, &
                                            VERTEX                  )
                              CALL CHCKXC ( .FALSE., ' ', OK )


                              CALL SURFNM ( A, A, B, VERTEX, NORMAL )


                              IF ( N .EQ. 1 ) THEN
!
!                                Find the surface point on the lower
!                                bounding ellipsoid, at the given
!                                planetodetic longitude and latitude
!                                (relative to the reference ellipsoid).

                                 IF ( BOUNDS(1,3) .LT. 0.D0 ) THEN
!
!                                   The direction to the inner bounding
!                                   ellipsoid is "down."
!
                                    CALL VMINUS ( NORMAL, DIR )
                                 ELSE
                                    CALL VEQU   ( NORMAL, DIR )
                                 END IF

                                 CALL SURFPT ( VERTEX, DIR, &
                                               EQLOW,  EQLOW, POLLOW, &
                                               LOWPT,  FOUND         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                We should never fail to find an
!                                intercept.
!
                                 CALL CHCKSL ( 'FOUND', FOUND, &
                                               .TRUE.,  OK     )

!
!                                Find the geodetic coordinates of LOWPT
!                                with respect to the reference
!                                ellipsoid.
!
                                 CALL RECGEO ( LOWPT,  A,      F, &
                                               LOWLON, LOWLAT, LOWALT )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                Adjust the altitude of LOWPT by a
!                                small increment.
!
                                 ALT = LOWALT +  EPS*( (-1)**(N+1) )


                              ELSE
!
!                                This is the case for the outer
!                                bounding ellipsoid (N = 2).
!
                                 IF ( BOUNDS(2,3) .LT. 0.D0 ) THEN
!
!                                   The direction to the outer bounding
!                                   ellipsoid is "down."
!
                                    CALL VMINUS ( NORMAL, DIR )
                                 ELSE
                                    CALL VEQU   ( NORMAL, DIR )
                                 END IF

                                 CALL SURFPT ( VERTEX, DIR, &
                                               EQHI,   EQHI, POLHI, &
                                               HIGHPT, FOUND       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                We should never fail to find an
!                                intercept.
!
                                 CALL CHCKSL ( 'FOUND', FOUND, &
                                               .TRUE.,  OK     )
!
!                                Find the geodetic coordinates of
!                                HIGHPT with respect to the reference
!                                ellipsoid.
!
                                 CALL RECGEO ( HIGHPT, A,     F, &
                                               HILON,  HILAT, HIALT )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                Adjust the altitude of HIGHPT by a
!                                small increment.
!
                                 ALT = HIALT +  EPS*( (-1)**(N+1) )

                              END IF

!
!                             Produce the perturbed input point using
!                             the perturbed geodetic coordinates.
!
                              CALL GEOREC ( LON, LAT, ALT, A, F, P2 )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL ZZINPDT( P2,     BOUNDS, CORPAR, &
                                            MARGIN, EXCLUD, INSIDE )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              XIN = .TRUE.

                              CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)

!
! --- Case: ------------------------------------------------------
!
                              MARGIN = 0.D0



                              TITLE = STEM

                              CALL SUFFIX ( 'Point near corner ' &
                              //            '# # #; exterior; ' &
                              //            'successor of excluded ' &
                              //            'coordinate out of ' &
                              //            'range; zero margin ', &
                                            1, TITLE            )
                              CALL REPMI ( TITLE, '#', L, TITLE )
                              CALL REPMI ( TITLE, '#', M, TITLE )
                              CALL REPMI ( TITLE, '#', N, TITLE )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL TCASE(TITLE)

!                             Set initial coordinates of the input
!                             point.
!
                              CALL RECGEO ( P2, A, F, LON, LAT, ALT )
                              CALL CHCKXC ( .FALSE., ' ', OK )

!
!                             Set incremental offsets. AEPS is the
!                             "angular epsilon." EPS is scaled to
!                             yield a "distance epsilon."
!
                              AEPS = 1.D-7 * RPD()
                              EPS  = 1.D-9 * A

!
!                             Multiply the small increments by 1 or
!                             -1 as needed, depending on whether they
!                             are used as offsets from upper or lower
!                             bounds.
!
!                             Note the scaled increment for latitude.
!
                              IF ( EXCLUD .EQ. LONIDX ) THEN

                                 IF ( M .EQ. 1 ) THEN

                                    LAT = MAX ( BOUNDS(M,2) - 2 * AEPS, &
                                                -HALFPI()              )
                                 ELSE
                                    LAT = MIN ( BOUNDS(M,2) + 2 * AEPS, &
                                                HALFPI()               )
                                 END IF

                              ELSE

                                 LAT = BOUNDS(M,2) +  AEPS*((-1)**(M+1))

                              END IF

                              IF ( EXCLUD .EQ. ALTIDX ) THEN

                                 IF ( L .EQ. 1 ) THEN

                                    LON = NRMMIN  - AEPS
                                 ELSE
                                    LON = NRMMAX  + AEPS
                                 END IF

                              ELSE

                                 IF ( L .EQ. 1 ) THEN

                                    LON = NRMMIN  + AEPS
                                 ELSE
                                    LON = NRMMAX  - AEPS
                                 END IF

                              END IF

!
!                             Find the point at the given longitude
!                             and latitude on the reference ellipsoid.
!                             We'll use this below.
!
                              CALL GEOREC ( LON,    LAT, 0.D0, A, F, &
                                            VERTEX                  )
                              CALL CHCKXC ( .FALSE., ' ', OK )


                              CALL SURFNM ( A, A, B, VERTEX, NORMAL )


                              IF ( N .EQ. 1 ) THEN
!
!                                Find the surface point on the lower
!                                bounding ellipsoid, at the given
!                                planetodetic longitude and latitude
!                                (relative to the reference ellipsoid).

                                 IF ( BOUNDS(1,3) .LT. 0.D0 ) THEN
!
!                                   The direction to the inner bounding
!                                   ellipsoid is "down."
!
                                    CALL VMINUS ( NORMAL, DIR )
                                 ELSE
                                    CALL VEQU   ( NORMAL, DIR )
                                 END IF

                                 CALL SURFPT ( VERTEX, DIR, &
                                               EQLOW,  EQLOW, POLLOW, &
                                               LOWPT,  FOUND         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                We should never fail to find an
!                                intercept.
!
                                 CALL CHCKSL ( 'FOUND', FOUND, &
                                               .TRUE.,  OK     )

!
!                                Find the geodetic coordinates of LOWPT
!                                with respect to the reference
!                                ellipsoid.
!
                                 CALL RECGEO ( LOWPT,  A,      F, &
                                               LOWLON, LOWLAT, LOWALT )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                Adjust the altitude of LOWPT by a
!                                small increment.
!
                                 IF (      ( EXCLUD .EQ. LATIDX ) &
                                      .OR. ( EXCLUD .EQ. NONE   ) ) THEN

                                    ALT = LOWALT + EPS*( (-1)**(N  ) )
                                 ELSE
                                    ALT = LOWALT + EPS*( (-1)**(N+1) )
                                 END IF


                              ELSE
!
!                                This is the case for the outer
!                                bounding ellipsoid (N = 2).
!
                                 IF ( BOUNDS(2,3) .LT. 0.D0 ) THEN
!
!                                   The direction to the outer bounding
!                                   ellipsoid is "down."
!
                                    CALL VMINUS ( NORMAL, DIR )
                                 ELSE
                                    CALL VEQU   ( NORMAL, DIR )
                                 END IF

                                 CALL SURFPT ( VERTEX, DIR, &
                                               EQHI,   EQHI, POLHI, &
                                               HIGHPT, FOUND       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                We should never fail to find an
!                                intercept.
!
                                 CALL CHCKSL ( 'FOUND', FOUND, &
                                               .TRUE.,  OK     )
!
!                                Find the geodetic coordinates of
!                                HIGHPT with respect to the reference
!                                ellipsoid.
!
                                 CALL RECGEO ( HIGHPT, A,     F, &
                                               HILON,  HILAT, HIALT )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                Adjust the altitude of HIGHPT by a
!                                small increment.
!
                                 IF (      ( EXCLUD .EQ. LATIDX ) &
                                      .OR. ( EXCLUD .EQ. NONE   ) ) THEN

                                    ALT = HIALT +  EPS*( (-1)**(N  ) )
                                 ELSE
                                    ALT = HIALT +  EPS*( (-1)**(N+1) )
                                 END IF

                              END IF


                              IF (      ( EXCLUD .EQ. 2 ) &
                                   .OR. ( EXCLUD .EQ. 0 ) ) THEN
!
!                                Either all coordinates are considered,
!                                or latitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT. We'll move the altitude
!                                out of bounds.
!

                                 IF ( N .EQ. 1 ) THEN
                                    ALT = ALT  -  0.1D0*ABS(ALT)
                                 ELSE
                                    ALT = ALT  +  0.1D0*ABS(ALT)
                                 END IF

                              END IF


!
!                             Produce the perturbed input point using
!                             the perturbed geodetic coordinates.
!
                              CALL GEOREC ( LON, LAT, ALT, A, F, P2 )

                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL ZZINPDT( P2,     BOUNDS, CORPAR, &
                                            MARGIN, EXCLUD, INSIDE )
                              CALL CHCKXC ( .FALSE., ' ', OK )


                              XIN = .FALSE.


                              IF ( EXCLUD .EQ. 1 ) THEN
!
!                                Longitude is the excluded coordinate;
!                                this is a case for which we try to
!                                modify latitude. However, the
!                                attempted modification may have no
!                                effect if a boundary is already at
!                                the extreme value.
!
                                 IF (     (LAT .GE. BOUNDS(1,2)-ANGMRG) &
                                    .AND. (LAT .LE. BOUNDS(2,2)+ANGMRG)) &
                                 THEN


                                    XIN = .TRUE.

                                 END IF



                              ELSE IF ( EXCLUD .EQ. 3 ) THEN


                                 IF (     (LON .GE. BOUNDS(1,1)-ANGMRG) &
                                    .AND. (LON .LE. BOUNDS(2,1)+ANGMRG)) &
                                 THEN

                                    XIN = .TRUE.

                                 END IF

                                 IF ( BOUNDS(1,1) .EQ. BOUNDS(2,1) ) &
                                 THEN

                                    XIN = .TRUE.

                                 END IF

                                 IF ( .NOT. LONBDS ) THEN

                                    XIN = .TRUE.

                                 END IF


                                 IF ( LONIX .EQ. NLON-1 ) THEN

                                    IF ( L .EQ. 2 ) THEN
!
!                                      In this special case, the input
!                                      longitude will be outside the
!                                      longitude boundaries.
!
                                       XIN = .FALSE.

                                    END IF

                                 END IF

                              END IF


                              CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)


!
!                             Restore the positive margin.
!
                              MARGIN = POSMRG

                           END DO
!
!                          End of "N" loop. N selects the altitude
!                          upper/lower bound.
!
                        END DO
!
!                       End of "M" loop. M selects the altitude
!                       upper/lower bound.
!
                     END DO
!
!                    End of "L" loop. L selects the longitude
!                    upper/lower bound.
!

                  END DO
!
!                 End of coordinate exclusion (EXCLUD) loop.
!
               END DO
!
!              End of altitude bound loop.
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


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


