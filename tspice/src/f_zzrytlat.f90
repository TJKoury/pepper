!$Procedure F_ZZRYTLAT ( ZZRYTLAT tests )

      SUBROUTINE F_ZZRYTLAT ( OK )
      use pepper_inc_dsktol

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZRYTLAT.
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
!     This routine tests the private SPICELIB routine ZZRYTLAT.
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
!     E.D. Wright      (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 12-FEB-2017 (NJB)
!
!        Previous version 06-JUL-2016 (NJB) (EDW)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )

!
!     This value must be kept in sync with the parameter
!     LATMRG in ZZINLAT.
!
      DOUBLE PRECISION      POLMRG
      PARAMETER           ( POLMRG = 1.D-8 )

!
!     Local Variables
!

!      CHARACTER*(TITLSZ)    TITLE

      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MAG
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      MIDLAT
      DOUBLE PRECISION      MIDLON
      DOUBLE PRECISION      MIDR
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

!      INTEGER               I
      INTEGER               NXPTS



!
!     Saved values
!
      SAVE

!
!     Initial values
!
      DATA                  X / 1.D0, 0.D0, 0.D0 /
      DATA                  Y / 0.D0, 1.D0, 0.D0 /
      DATA                  Z / 0.D0, 0.D0, 1.D0 /





!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZRYTLAT' )



!***********************************************************************
!
!     Normal cases
!
!***********************************************************************


!
!     The following tests use a volume element with a longitude extent
!     of less than 90 degrees and a latitude extent of 15 degrees. The
!     depth of the element is 50 km. The element is located above the
!     equator.
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Hit on north polar cap. Margin = 0.'   )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = 2*PI()

      BOUNDS(1,2) = PI()/4
      BOUNDS(2,2) = PI()/2

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50


      CALL VPACK ( 0.D0, 0.D0, BOUNDS(2,3), XXPT )


      MARGIN = 0.D0

      MAG    = 1.D10

      CALL VLCOM  ( 1.D0, XXPT, MAG, Z, VERTEX )
      CALL VMINUS ( Z,                  RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Hit on south polar cap. Margin = 0.'   )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = 2*PI()

      BOUNDS(1,2) = -PI()/2
      BOUNDS(2,2) = -PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50


      CALL VPACK ( 0.D0, 0.D0, -BOUNDS(2,3), XXPT )


      MARGIN = 0.D0

      MAG    = 1.D10

      CALL VLCOM ( 1.D0, XXPT, -MAG, Z, VERTEX )
      CALL VEQU  ( Z,                   RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Vertex is inside element. Margin = 0.'   )


      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = 2*PI()

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, MIDLON, MIDLAT, XXPT )


      MARGIN = 0.D0

      CALL VEQU ( XXPT, VERTEX )

      CALL CLEARD ( 3, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on outer sphere. Margin = 0. Longitude ' &
      //           'wrap-around case.'                          )




      BOUNDS(1,1) =  5*PI()/6
      BOUNDS(2,1) = -5*PI()/6

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( 2*PI() + BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      CALL LATREC ( R, MIDLON, MIDLAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )

      MARGIN = 0.D0

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on inner sphere. Vertex is outside ' &
      //           'outer sphere. Margin = 0.'             )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(1,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      CALL LATREC ( R, MIDLON, MIDLAT, XXPT )

      MAG = 1.D3

      CALL VSCL   ( -MAG,   XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )

      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on inner sphere. Vertex is inside ' &
      //           'inner sphere. Margin = 0.'             )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(1,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      CALL LATREC ( R, MIDLON, MIDLAT, XXPT )

      MAG = 1.D3

      CALL CLEARD ( 3,    VERTEX )
      CALL VEQU   ( XXPT, RAYDIR )

      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on inner sphere. Margin = 0. Longitude ' &
      //           'wrap-around case.'                          )




      BOUNDS(1,1) =  5*PI()/6
      BOUNDS(2,1) = -5*PI()/6

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(1,3)

      MIDLON      = ( 2*PI() + BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      CALL LATREC ( R, MIDLON, MIDLAT, XXPT )

      MAG = 1.D3

      CALL VSCL   ( -MAG,   XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )

      MARGIN = 0.D0



      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on upper latitude boundary. Margin = 0.' )

!
!     This case produces a single intercept of the ray with
!     the upper latitude cone.
!
      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, MIDLON, BOUNDS(2,2), XXPT )


      MAG = 1.D3


      CALL VLCOM  ( 1.D0, XXPT, MAG, Z, VERTEX )
      CALL VMINUS ( Z,                  RAYDIR )

      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on upper latitude boundary. Intercept ' &
      //           'is second solution. Margin = 0.' )

!
!     This case produces a single intercept of the ray with
!     the upper latitude cone.
!
      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, MIDLON, BOUNDS(2,2), XXPT )

!
!     The magnitude of the vertex offset must be large
!     enough to place the vertex outside of the upper
!     latitude cone.
!
      MAG = 1.D4

!
!     Pick the ray's direction vector so that we can make the
!     ray hit the upper latitude cone once at pi radians away
!     from XXPT, then hit again at XXPT.
!
      CALL LATREC ( MAG,  MIDLON, -PI()/5, RAYDIR )
      CALL VSUB   ( XXPT, RAYDIR, VERTEX          )


      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on lower latitude boundary. Margin = 0.' )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, MIDLON, BOUNDS(1,2), XXPT )


      MAG = 1.D3


      CALL VLCOM ( 1.D0, XXPT, -MAG, Z, VERTEX )
      CALL VEQU  ( Z,                   RAYDIR )

      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!

!
!     Note: there is no corresponding "second solution" case
!     for the lower latitude boundary of an element in the
!     northern hemisphere.
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on west longitude boundary. Margin = 0.' )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, BOUNDS(1,1), MIDLAT, XXPT )


      MAG = 1.D3

      CALL VLCOM ( 1.D0, XXPT, -MAG, Y, VERTEX )
      CALL VEQU  ( Y,                   RAYDIR )

      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on east longitude boundary. Margin = 0.' )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, BOUNDS(2,1), MIDLAT, XXPT )


      MAG = 1.D3

      CALL VLCOM ( 1.D0, XXPT, -MAG, X, VERTEX )
      CALL VEQU  ( X,                   RAYDIR )

      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
!     The following tests use a volume element with a longitude extent
!     of less than degrees and a latitude extent of 15 degrees. The
!     depth of the element is 50 km. The element is located below the
!     equator.
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Vertex is inside element. Margin = 0.'   )


      BOUNDS(1,1) =  PI()/4
      BOUNDS(2,1) =  PI()/2

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) =  3.D3
      BOUNDS(2,3) =  3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, MIDLON, MIDLAT, XXPT )


      MARGIN = 0.D0

      CALL VEQU ( XXPT, VERTEX )

      CALL CLEARD ( 3, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on outer sphere. Margin = 0. Longitude ' &
      //           'wrap-around case.'                          )




      BOUNDS(1,1) =  5*PI()/6
      BOUNDS(2,1) = -5*PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( 2*PI() + BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      CALL LATREC ( R, MIDLON, MIDLAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )

      MARGIN = 0.D0

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on inner sphere. Vertex is outside ' &
      //           'outer sphere. Margin = 0.'             )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(1,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      CALL LATREC ( R, MIDLON, MIDLAT, XXPT )

      MAG = 1.D3

      CALL VSCL   ( -MAG,   XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )

      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on inner sphere. Vertex is inside ' &
      //           'inner sphere. Margin = 0.'             )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(1,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      CALL LATREC ( R, MIDLON, MIDLAT, XXPT )

      MAG = 1.D3

      CALL CLEARD ( 3,    VERTEX )
      CALL VEQU   ( XXPT, RAYDIR )

      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on inner sphere. Margin = 0. Longitude ' &
      //           'wrap-around case.'                          )




      BOUNDS(1,1) =  5*PI()/6
      BOUNDS(2,1) = -5*PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(1,3)

      MIDLON      = ( 2*PI() + BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      CALL LATREC ( R, MIDLON, MIDLAT, XXPT )

      MAG = 1.D3

      CALL VSCL   ( -MAG,   XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )

      MARGIN = 0.D0



      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on upper latitude boundary. Margin = 0.' )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, MIDLON, BOUNDS(2,2), XXPT )


      MAG = 1.D3


      CALL VLCOM  ( 1.D0, XXPT, MAG, Z, VERTEX )
      CALL VMINUS ( Z,                  RAYDIR )

      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on lower latitude boundary. Margin = 0.' )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, MIDLON, BOUNDS(1,2), XXPT )


      MAG = 1.D3


      CALL VLCOM ( 1.D0, XXPT, -MAG, Z, VERTEX )
      CALL VEQU  ( Z,                   RAYDIR )

      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on lower latitude boundary. Intercept ' &
      //           'is second solution. Margin = 0.' )


!
!     This case produces a single intercept of the ray with
!     the upper latitude cone.
!
      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) =  PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, MIDLON, BOUNDS(1,2), XXPT )

!
!     The magnitude of the vertex offset must be large
!     enough to place the vertex outside of the lower
!     latitude cone.
!
      MAG = 1.D4

!
!     Pick the ray's direction vector so that we can make the
!     ray hit the lower latitude cone once at pi radians away
!     from XXPT, then hit again at XXPT.
!
      CALL LATREC ( MAG,  MIDLON, -PI()/5, RAYDIR )
      CALL VSUB   ( XXPT, RAYDIR, VERTEX          )


      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!

!
!     Note: there is no corresponding "second solution" case
!     for the upper latitude boundary of an element in the
!     southern hemisphere.
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on west longitude boundary. Margin = 0.' )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, BOUNDS(1,1), MIDLAT, XXPT )


      MAG = 1.D3


      CALL VLCOM ( 1.D0, XXPT, -MAG, Y, VERTEX )
      CALL VEQU  ( Y,                   RAYDIR )

      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on east longitude boundary. Margin = 0.' )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, BOUNDS(2,1), MIDLAT, XXPT )


      MAG = 1.D3


      CALL VLCOM( 1.D0, XXPT, -MAG, X, VERTEX )
      CALL VEQU ( X,                   RAYDIR )

      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
!
!     Following are tests for some cases in which the volume
!     element has special properties.
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element with southern boundary on the ' &
      //           'equator. Margin = 0.' )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) =  0.D0
      BOUNDS(2,2) =  PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, MIDLON, BOUNDS(1,2), XXPT )


      MAG = 1.D3


      CALL VLCOM ( 1.D0, XXPT, -MAG, Z, VERTEX )
      CALL VEQU  ( Z,                   RAYDIR )


      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element with northern boundary on the ' &
      //           'equator. Margin = 0.' )


      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =  0.D0

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      CALL LATREC ( MIDR, MIDLON, BOUNDS(2,2), XXPT )


      MAG = 1.D3


      CALL VLCOM ( 1.D0, XXPT, MAG, Z, VERTEX )
      CALL VMINUS( Z,                  RAYDIR )


      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Equatorial band; longitude extent = 2pi. ' &
      //           'Margin = 0. Vertex is on +X axis.' )


      BOUNDS(1,1) = -PI()
      BOUNDS(2,1) =  PI()

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      CALL LATREC ( R, 0.D0, MIDLAT, XXPT )

      MAG = 1.D3

      CALL VLCOM ( 1.D0, XXPT, MAG, X, VERTEX )
      CALL VMINUS( X,                  RAYDIR )


      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Equatorial band; longitude extent = 2pi. ' &
      //           'Margin = 0. Vertex is on +Y axis.' )


      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  2*PI()

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      XXPT(1)     = R - 1.D0
      XXPT(2)     = SQRT( R**2 - (R-1.D0)**2 )
      XXPT(3)     = 0.D0


      CALL LATREC ( R, 0.D0, MIDLAT, XXPT )


      MAG = 1.D3

      CALL VLCOM ( 1.D0, XXPT, MAG, Y, VERTEX )
      CALL VMINUS( Y,                  RAYDIR )


      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Equatorial band; longitude extent = 2pi. ' &
      //           'Margin = 0. Vertex is on +Z axis.' )


      BOUNDS(1,1) =  0.D0
      BOUNDS(2,1) =  2*PI()

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      XXPT(1)     = R - 1.D0
      XXPT(2)     = 0.D0
      XXPT(3)     = SQRT( R**2 - (R-1.D0)**2 )


      CALL LATREC ( R, 0.D0, MIDLAT, XXPT )


      MAG = 1.D3

      CALL VLCOM ( 1.D0, XXPT, MAG, Z, VERTEX )
      CALL VMINUS( Y,                  RAYDIR )


      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element with wrap-around longitude. Margin ' &
      //           '= 0. Hit on west (left) longitude boundary.'     )


      BOUNDS(1,1) =   5 * PI()/6
      BOUNDS(2,1) =  -5 * PI()/6

      BOUNDS(1,2) =  -PI()/3
      BOUNDS(2,2) =   PI()/3

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2



      CALL LATREC ( R, BOUNDS(2,1), MIDLAT, XXPT )


      MAG = 1.D3

      CALL VLCOM ( 1.D0, XXPT, -MAG, X, VERTEX )
      CALL VEQU  ( X,                   RAYDIR )


      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element with wrap-around longitude. Margin ' &
      //           '= 0. Hit on east (right) longitude boundary.'     )


      BOUNDS(1,1) =   5 * PI()/6
      BOUNDS(2,1) =  -5 * PI()/6

      BOUNDS(1,2) =  -PI()/3
      BOUNDS(2,2) =   PI()/3

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2



      CALL LATREC ( R, BOUNDS(2,1), MIDLAT, XXPT )


      MAG = 1.D3

      CALL VLCOM ( 1.D0, XXPT, -MAG, X, VERTEX )
      CALL VEQU  ( X,                   RAYDIR )


      MARGIN = 0.D0


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
!
!     The following cases use positive margins.
!
!        POSITIVE MARGINS
!
!

!
!     We start with cases involving hits on the outer sphere.
!
!        OUTER SPHERE
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Vertex is inside element. Margin > 0. ' &
      //           'Interior case.'                         )


      MARGIN      = 1.D-8

      BOUNDS(1,1) = PI()/4
      BOUNDS(2,1) = PI()/2

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3) + ( ( MARGIN / 2 ) * R )

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2
      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      CALL LATREC ( R, MIDLON, MIDLAT, XXPT )


      CALL VEQU ( XXPT, VERTEX )

      CALL CLEARD ( 3, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Interior ' &
      //           'case. Hit is beyond east longitude boundary.' )


      MARGIN = 1.D-8


      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =  PI()/6

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(2,1) +  ( MARGIN / 2 )

      CALL LATREC ( R, LON, MIDLAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )



      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Exterior case. ' &
      //           'Hit is beyond east longitude boundary.' )


      MARGIN = 1.D-8

      LON         = BOUNDS(2,1) +  ( 2*MARGIN )

      CALL LATREC ( R, LON, MIDLAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )



      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Longitude ' &
      //           'wrap-around case. Interior case. Hit is ' &
      //           'beyond east longitude boundary.'           )


      MARGIN = 1.D-8


      BOUNDS(1,1) =  5*PI()/6
      BOUNDS(2,1) = -5*PI()/6

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(2,1) +  ( MARGIN / 2 )

      CALL LATREC ( R, LON, MIDLAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )



      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )






!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Longitude ' &
      //           'wrap-around case. Exterior case. Hit is ' &
      //           'beyond east longitude boundary.'           )


      MARGIN = 1.D-8


      BOUNDS(1,1) =  5*PI()/6
      BOUNDS(2,1) = -5*PI()/6

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(2,1) +  ( MARGIN * 2 )

      CALL LATREC ( R, LON, MIDLAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )



      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Interior ' &
      //           'case. Hit is beyond west longitude boundary.' )


      MARGIN = 1.D-8



      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =  PI()/6

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(1,1) -  ( MARGIN / 2 )

      CALL LATREC ( R, LON, MIDLAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )



      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )


      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Longitude ' &
      //           'wrap-around case. Interior case. Hit is ' &
      //           'beyond west longitude boundary.'           )


      MARGIN = 1.D-8


      BOUNDS(1,1) =  5*PI()/6
      BOUNDS(2,1) = -5*PI()/6

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(1,1) -  ( MARGIN / 2 )

      CALL LATREC ( R, LON, MIDLAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )



      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )


      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Longitude ' &
      //           'wrap-around case. Exterior case. Hit is ' &
      //           'beyond west longitude boundary.'           )



      LON         = BOUNDS(1,1) -  ( MARGIN * 2 )

      CALL LATREC ( R, LON, MIDLAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )



      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Interior ' &
      //           'case. Hit is beyond north latitude boundary.' )


      MARGIN = 1.D-8


      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =  PI()/6

      BOUNDS(1,2) = PI()/6
      BOUNDS(2,2) = PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(2,2) +  ( MARGIN / 2 )

      CALL LATREC ( R, MIDLON, LAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Exterior ' &
      //           'case. Hit is beyond north latitude boundary.' )


      LAT         = BOUNDS(2,2) +  ( MARGIN * 2 )

      CALL LATREC ( R, MIDLON, LAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Interior ' &
      //           'case. Hit is beyond north latitude boundary.' )


      MARGIN = 1.D-8


      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =  PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(2,2) +  ( MARGIN / 2 )

      CALL LATREC ( R, MIDLON, LAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Exterior ' &
      //           'case. Hit is beyond north latitude boundary.' )



      LAT         = BOUNDS(2,2) +  ( MARGIN * 2 )

      CALL LATREC ( R, MIDLON, LAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Interior ' &
      //           'case. Hit is beyond south latitude boundary.' )


      MARGIN = 1.D-8


      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =  PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(1,2) -  ( MARGIN / 2 )

      CALL LATREC ( R, MIDLON, LAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on outer sphere. Margin > 0. Exterior ' &
      //           'case. Hit is beyond south latitude boundary.' )



      LAT         = BOUNDS(1,2) -  ( MARGIN * 2 )

      CALL LATREC ( R, MIDLON, LAT, XXPT )


      MAG = 1.D3

      CALL VSCL   ( MAG,    XXPT, VERTEX )
      CALL VMINUS ( VERTEX,       RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )




!
!     The following cases deal with hits on longitude boundaries.
!
!        EAST LONGITUDE
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on east longitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond south latitude ' &
      //           'boundary.'                                   )





      MARGIN = 1.D-8


      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =  PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      LON         = BOUNDS(2,1)

      LAT         = BOUNDS(1,2) -  ( MARGIN / 2 )

      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, MAG, Y, VERTEX )
      CALL VMINUS ( Y,                  RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on east longitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond south latitude ' &
      //           'boundary.'                                   )


      LAT         = BOUNDS(1,2) -  ( MARGIN * 2 )

      CALL LATREC ( MIDR, LON, LAT, XXPT )


      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, MAG, Y, VERTEX )
      CALL VMINUS ( Y,                  RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on east longitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond north latitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

!
!     Note the upper longitude bound. This value was selected to
!     simplify the choice of a ray that would not hit other
!     bounding surfaces before hitting the expected intercept.
!

      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =   0.D0

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      LON         = BOUNDS(2,1)

      LAT         = BOUNDS(2,2) +  ( MARGIN / 2 )

      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, MAG, Y, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,       RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on east longitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond north latitude ' &
      //           'boundary.'                                   )

      LAT         = BOUNDS(2,2) +  ( MARGIN * 2 )

      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, MAG, Y, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,       RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on east longitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond outer radius ' &
      //           'boundary.'                                   )

!
!     This is a place holder. There is no possibility of a hit
!     outside of the outer bounding sphere (radius BOUNDS(2,3)).
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on east longitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond outer radius ' &
      //           'boundary.'                                   )



      MARGIN = 1.D-8

!
!     Note the upper longitude bound. This value was selected to
!     simplify the choice of a ray that would not hit other
!     bounding surfaces before hitting the expected intercept.
!

      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =   0.D0

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3) + ( BOUNDS(2,3)*MARGIN / 2 )

      LON         = BOUNDS(2,1)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      CALL LATREC ( R, LON, MIDLAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, MAG, Y, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,       RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on east longitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond inner radius ' &
      //           'boundary.'                                   )



      MARGIN = 1.D-8

!
!     Note the upper longitude bound. This value was selected to
!     simplify the choice of a ray that would not hit other
!     bounding surfaces before hitting the expected intercept.
!

      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =   0.D0

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      LON         = BOUNDS(2,1)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      R           = BOUNDS(1,3) - ( BOUNDS(1,3)*MARGIN / 2 )

      CALL LATREC ( R, LON, MIDLAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, MAG, Y, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,       RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on east longitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond inner radius ' &
      //           'boundary.'                                   )

!
!     Pick a radius value that makes it easy to miss the inner
!     bounding sphere.
!
      R           = BOUNDS(1,3) / 2

      CALL LATREC ( R, LON, MIDLAT, XXPT )

!
!     Choose MAG so that the vertex is inside the inner bounding
!     sphere.
!
      MAG = 1.D0

      CALL VLCOM  ( 1.D0, XXPT, MAG, Y, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,       RAYDIR )


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
!        WEST LONGITUDE
!
!     The cases below are mirror images of the east longitude
!     cases.
!
!
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on west longitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond south latitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8


      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =  PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      LON         = BOUNDS(1,1)

      LAT         = BOUNDS(1,2) -  ( MARGIN / 2 )

      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, -MAG, Y, VERTEX )
      CALL VEQU   ( Y,                   RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on west longitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond south latitude ' &
      //           'boundary.'                                   )


      LAT         = BOUNDS(1,2) -  ( MARGIN * 2 )

      CALL LATREC ( MIDR, LON, LAT, XXPT )


      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, -MAG, Y, VERTEX )
      CALL VEQU   ( Y,                   RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on west longitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond north latitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

!
!     Note the upper longitude bound. This value was selected to
!     simplify the choice of a ray that would not hit other
!     bounding surfaces before hitting the expected intercept.
!

      BOUNDS(1,1) =   0.D0
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      LON         = BOUNDS(1,1)

      LAT         = BOUNDS(2,2) +  ( MARGIN / 2 )

      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, -MAG, Y, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on west longitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond north latitude ' &
      //           'boundary.'                                   )

      LAT         = BOUNDS(2,2) +  ( MARGIN * 2 )

      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, -MAG, Y, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on west longitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond outer radius ' &
      //           'boundary.'                                   )

!
!     This is a place holder. There is no possibility of a hit
!     outside of the outer bounding sphere (radius BOUNDS(2,3)).
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on west longitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond outer radius ' &
      //           'boundary.'                                   )



      MARGIN = 1.D-8

!
!     Note the upper longitude bound. This value was selected to
!     simplify the choice of a ray that would not hit other
!     bounding surfaces before hitting the expected intercept.
!

      BOUNDS(1,1) =   0.D0
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3) + ( BOUNDS(2,3)*MARGIN / 2 )

      LON         = BOUNDS(1,1)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      CALL LATREC ( R, LON, MIDLAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, -MAG, Y, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on west longitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond inner radius ' &
      //           'boundary.'                                   )



      MARGIN = 1.D-8

!
!     Note the upper longitude bound. This value was selected to
!     simplify the choice of a ray that would not hit other
!     bounding surfaces before hitting the expected intercept.
!

      BOUNDS(1,1) =   0.D0
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      LON         = BOUNDS(1,1)

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      R           = BOUNDS(1,3) - ( BOUNDS(1,3)*MARGIN / 2 )

      CALL LATREC ( R, LON, MIDLAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, -MAG, Y, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on west longitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond inner radius ' &
      //           'boundary.'                                   )

!
!     Pick a radius value that makes it easy to miss the inner
!     bounding sphere.
!
      R           = BOUNDS(1,3) / 2

      CALL LATREC ( R, LON, MIDLAT, XXPT )

!
!     Choose MAG so that the vertex is inside the inner bounding
!     sphere.
!
      MAG = 1.D0

      CALL VLCOM  ( 1.D0, XXPT, -MAG, Y, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )





!
!     The following cases deal with hits on latitude boundaries.
!
!        Volume element is below equator.
!
!        SOUTHERN HEMISPHERE
!
!        NORTH LATITUDE
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond west longitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      LAT         = BOUNDS(2,2)

      LON         = BOUNDS(1,1) - ( MARGIN/2 )



      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond west longitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

      LON         = BOUNDS(1,1) - ( MARGIN*2*BOUNDS(2,3) )


      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond east longitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8


      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      LAT         = BOUNDS(2,2)

      LON         = BOUNDS(2,1) + ( MARGIN/2 )



      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond east longitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

      LON         = BOUNDS(2,1) + ( MARGIN*2*BOUNDS(2,3) )


      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond outer radius ' &
      //           'boundary.'                                   )

!
!     This is a place holder. There's no such hit case.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond outer radius ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

!
!     Note the upper longitude bound. This value was selected to
!     simplify the choice of a ray that would not hit other
!     bounding surfaces before hitting the expected intercept.
!

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3) + ( BOUNDS(2,3) * MARGIN * 2 )

      LAT         = BOUNDS(2,2)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2



      CALL LATREC ( R, MIDLON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond inner radius ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8


      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50


      R           = BOUNDS(1,3) - ( BOUNDS(1,3) * MARGIN / 2 )

      LAT         = BOUNDS(2,2)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2



      CALL LATREC ( R, MIDLON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, X, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond inner radius ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8


      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

!
!     Choose a radius that precludes a valid hit on the inner
!     sphere.
!
      R           = BOUNDS(1,3) / 2

      LAT         = BOUNDS(2,2)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2



      CALL LATREC ( R, MIDLON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
!        SOUTHERN HEMISPHERE
!
!        SOUTH LATITUDE
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond west longitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8


      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      LAT         = BOUNDS(1,2)

      LON         = BOUNDS(1,1) - ( MARGIN/2 )



      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond west longitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

      LON         = BOUNDS(1,1) - ( MARGIN*2*BOUNDS(2,3) )


      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond east longitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      LAT         = BOUNDS(1,2)

      LON         = BOUNDS(2,1) + ( MARGIN/2 )



      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond east longitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

      LON         = BOUNDS(2,1) + ( MARGIN*2*BOUNDS(2,3) )


      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond outer radius ' &
      //           'boundary.'                                   )

!
!     This is a place holder. There's no such hit case.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond outer radius ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8


      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3) + ( BOUNDS(2,3) * MARGIN * 2 )

      LAT         = BOUNDS(1,2)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2



      CALL LATREC ( R, MIDLON, LAT, XXPT )

      MAG = 1.D3

!
!     Choose the ray direction so that the ray skims past the
!     volume element.
!
      CALL VPACK ( 1.D0, 0.D0, 1.D0, RAYDIR )

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, RAYDIR, VERTEX )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond inner radius ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8


      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50


      R           = BOUNDS(1,3) - ( BOUNDS(1,3) * MARGIN / 2 )

      LAT         = BOUNDS(1,2)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2



      CALL LATREC ( R, MIDLON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, X, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in southern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond inner radius ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8


      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) = -PI()/4
      BOUNDS(2,2) = -PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(1,3) / 2

      LAT         = BOUNDS(2,2)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2



      CALL LATREC ( R, MIDLON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
!
!        Volume element is above equator.
!
!        NORTHERN HEMISPHERE
!
!        NORTH LATITUDE
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond west longitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      LAT         = BOUNDS(2,2)

      LON         = BOUNDS(1,1) - ( MARGIN/2 )



      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond west longitude ' &
      //           'boundary.'                                   )



      LON         = BOUNDS(1,1) - ( MARGIN * 2 )


      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond east longitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      LAT         = BOUNDS(2,2)

      LON         = BOUNDS(2,1) + ( MARGIN/2 )



      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond east longitude ' &
      //           'boundary.'                                   )



      LON         = BOUNDS(2,1) + ( MARGIN * 2 )


      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond outer radius ' &
      //           'boundary.'                                   )


!
!     This is a place holder. There's no such hit case.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond outer radius ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      LAT         = BOUNDS(2,2)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      R           = BOUNDS(2,3) + ( BOUNDS(2,3) * MARGIN / 2 )


      CALL LATREC ( R, MIDLON, LAT, XXPT )

      MAG = 1.D3

!
!     Choose the ray direction so that the ray skims past the
!     volume element.
!
      CALL VPACK ( 1.D0, 0.D0, -1.D0, RAYDIR )

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, RAYDIR, VERTEX )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond inner radius ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8


      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50


      R           = BOUNDS(1,3) - ( BOUNDS(1,3) * MARGIN / 2 )

      LAT         = BOUNDS(2,2)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2



      CALL LATREC ( R, MIDLON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, X, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on north latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond inner radius ' &
      //           'boundary.'                                   )


!
!     Choose R to ensure a miss.
!
      R           = BOUNDS(1,3) / 2

      LAT         = BOUNDS(2,2)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2



      CALL LATREC ( R, MIDLON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
!
!        Volume element is above equator.
!
!        NORTHERN HEMISPHERE
!
!        SOUTH LATITUDE
!



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond west longitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      LAT         = BOUNDS(1,2)

      LON         = BOUNDS(1,1) - ( MARGIN/2 )



      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond west longitude ' &
      //           'boundary.'                                   )



      LON         = BOUNDS(1,1) - ( MARGIN * 2 )


      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond east longitude ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDR        = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

      LAT         = BOUNDS(1,2)

      LON         = BOUNDS(2,1) + ( MARGIN/2 )



      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond east longitude ' &
      //           'boundary.'                                   )



      LON         = BOUNDS(2,1) + ( MARGIN * 2 )


      CALL LATREC ( MIDR, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond outer radius ' &
      //           'boundary.'                                   )


!
!     This is a place holder. There's no such hit case.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond outer radius ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  PI()/6
      BOUNDS(2,2) =  PI()/4

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      LAT         = BOUNDS(1,2)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      R           = BOUNDS(2,3) + ( BOUNDS(2,3) * MARGIN * 2 )


      CALL LATREC ( R, MIDLON, LAT, XXPT )

      MAG = 1.D3


      CALL VLCOM  ( 1.D0, XXPT, MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,       RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond inner radius ' &
      //           'boundary.'                                   )


      MARGIN = 1.D-8


      R           = BOUNDS(1,3) - ( BOUNDS(1,3) * MARGIN / 2 )

      LAT         = BOUNDS(1,2)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2



      CALL LATREC ( R, MIDLON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  -MAG, X, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,         RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element in northern hemisphere. ' &
      //           'Hit on south latitude boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond inner radius ' &
      //           'boundary.'                                   )


!
!     Choose R to ensure a miss.
!
      R           = BOUNDS(1,3) / 2

      LAT         = BOUNDS(1,2)

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2



      CALL LATREC ( R, MIDLON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT,  MAG, Z, VERTEX )
      CALL VSUB   ( XXPT, VERTEX,        RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )




!
!
!     The following cases deal with hits on the inner bounding sphere.
!
!
!        INNER RADIUS BOUNDARY
!
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond east longitude ' &
      //           'boundary. Vertex is inside inner sphere; ' &
      //           'this is the "first endpoint" case.'    )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(2,1) + ( MARGIN/2 )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, LON, MIDLAT, XXPT )

      CALL CLEARD ( 3,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond east longitude ' &
      //           'boundary. Vertex is inside inner sphere; ' &
      //           'this is the "first endpoint" case.'    )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(2,1) + ( 2 * BOUNDS(2,3) * MARGIN )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, LON, MIDLAT, XXPT )

      CALL CLEARD ( 3,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond west longitude ' &
      //           'boundary. Vertex is inside inner sphere; ' &
      //           'this is the "first endpoint" case.'    )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(1,1) - ( MARGIN/2 )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, LON, MIDLAT, XXPT )

      CALL CLEARD ( 3,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond west longitude ' &
      //           'boundary. Vertex is inside inner sphere; ' &
      //           'this is the "first endpoint" case.'    )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(1,1) - ( 2 * BOUNDS(2,3) * MARGIN )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, LON, MIDLAT, XXPT )

      CALL CLEARD ( 3,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond north latitude ' &
      //           'boundary. Vertex is inside inner sphere; ' &
      //           'this is the "first endpoint" case.'    )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(2,2) + ( MARGIN/2 )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, MIDLON, LAT, XXPT )

      CALL CLEARD ( 3,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond north latitude ' &
      //           'boundary. Vertex is inside inner sphere; ' &
      //           'this is the "first endpoint" case.'    )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(2,2) + ( MARGIN*2 )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, MIDLON, LAT, XXPT )

      CALL CLEARD ( 3,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond south latitude ' &
      //           'boundary. Vertex is inside inner sphere; ' &
      //           'this is the "first endpoint" case.'    )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(1,2) - ( MARGIN/2 )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, MIDLON, LAT, XXPT )

      CALL CLEARD ( 3,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond south latitude ' &
      //           'boundary. Vertex is inside inner sphere; ' &
      //           'this is the "first endpoint" case.'    )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(1,2) - ( MARGIN*2 )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, MIDLON, LAT, XXPT )

      CALL CLEARD ( 3,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond east longitude ' &
      //           'boundary. Vertex is outside inner sphere; ' &
      //           'this is the "second endpoint" case.'    )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(2,1) + ( MARGIN/2 )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, LON, MIDLAT, XXPT )

      VERTEX(1) = -2 * XXPT(1)
      VERTEX(2) = -2 * XXPT(2)
      VERTEX(3) =      XXPT(3)

      CALL VSUB ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond east longitude ' &
      //           'boundary. Vertex is outside inner sphere; ' &
      //           'this is the "second endpoint" case.'    )


      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(2,1) + ( 2 * BOUNDS(2,3) * MARGIN )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, LON, MIDLAT, XXPT )

      VERTEX(1) = -2 * XXPT(1)
      VERTEX(2) = -2 * XXPT(2)
      VERTEX(3) =      XXPT(3)

      CALL VSUB ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond west longitude ' &
      //           'boundary. Vertex is outside inner sphere; ' &
      //           'this is the "second endpoint" case.'    )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(1,1) - ( MARGIN/2 )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, LON, MIDLAT, XXPT )

      VERTEX(1) = -2 * XXPT(1)
      VERTEX(2) = -2 * XXPT(2)
      VERTEX(3) =      XXPT(3)
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond west longitude ' &
      //           'boundary. Vertex is outside inner sphere; ' &
      //           'this is the "second endpoint" case.'    )



      MIDLAT      = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2

      LON         = BOUNDS(1,1) - ( 2 * BOUNDS(2,3) * MARGIN )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, LON, MIDLAT, XXPT )

      VERTEX(1) = -2 * XXPT(1)
      VERTEX(2) = -2 * XXPT(2)
      VERTEX(3) =      XXPT(3)
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond north latitude ' &
      //           'boundary. Vertex is outside inner sphere; ' &
      //           'this is the "second endpoint" case.'    )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(2,2) + ( MARGIN/2 )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, MIDLON, LAT, XXPT )

      VERTEX(1) = -2 * XXPT(1)
      VERTEX(2) = -2 * XXPT(2)
      VERTEX(3) =      XXPT(3)
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond north latitude ' &
      //           'boundary. Vertex is outside inner sphere; ' &
      //           'this is the "second endpoint" case.'    )



      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(2,2) + ( MARGIN*2 )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, MIDLON, LAT, XXPT )

      VERTEX(1) = -2 * XXPT(1)
      VERTEX(2) = -2 * XXPT(2)
      VERTEX(3) = -2 * XXPT(3)
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Interior case. Hit is beyond south latitude ' &
      //           'boundary. Vertex is outside inner sphere; ' &
      //           'this is the "second endpoint" case.'    )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =  -PI()/6
      BOUNDS(2,2) =   PI()/6

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(1,2) - ( MARGIN/2 )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, MIDLON, LAT, XXPT )

      VERTEX(1) = -2 * XXPT(1)
      VERTEX(2) = -2 * XXPT(2)
      VERTEX(3) = -2 * XXPT(3)
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Small element that spans the equator. ' &
      //           'Hit on inner radius boundary. Margin > 0. ' &
      //           'Exterior case. Hit is beyond south latitude ' &
      //           'boundary. Vertex is outside inner sphere; ' &
      //           'this is the "second endpoint" case.'    )



      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(1,2) - ( MARGIN*2 )

      R           = BOUNDS(1,3)


      CALL LATREC ( R, MIDLON, LAT, XXPT )

      VERTEX(1) = -2 * XXPT(1)
      VERTEX(2) = -2 * XXPT(2)
      VERTEX(3) = -2 * XXPT(3)
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
!
!     SPECIAL CASES
!
!
!     The following cases deal with hits near the poles
!
!
!        NEAR NORTH POLE, OUTER SPHERE
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element ranging from equator to near ' &
      //           'north pole. Hit on outer sphere. Margin > 0. ' &
      //           'Interior case. Hit is beyond north latitude ' &
      //           'boundary.'   )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =   0.D0
      BOUNDS(2,2) =   HALFPI() - (POLMRG/1.001D0)

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50


      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(2,2) + ( MARGIN/2 )

      R           = BOUNDS(2,3)

      CALL LATREC ( R, MIDLON, LAT, XXPT )



      MAG = 1.D3

      CALL VSCL   ( MAG, XXPT,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element ranging from equator to near ' &
      //           'north pole. Hit on outer sphere. Margin > 0. ' &
      //           'Interior case. Hit is below north latitude ' &
      //           'boundary, but above high latitude limit. ' &
      //           'Longitude is off by pi radians.'           )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =   0.D0
      BOUNDS(2,2) =   HALFPI() - (POLMRG/4)

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50


      MIDLON      = PI() + ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(2,2) - ( MARGIN/4 )

      R           = BOUNDS(2,3)

      CALL LATREC ( R, MIDLON, LAT, XXPT )



      MAG = 1.D3

      CALL VSCL   ( MAG, XXPT,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element ranging from equator to near ' &
      //           'north pole. Hit on outer sphere. Margin > 0. ' &
      //           'Exterior case. Hit is below north latitude ' &
      //           'boundary, and below high latitude limit. ' &
      //           'Longitude is off by pi radians.'           )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =   0.D0
      BOUNDS(2,2) =   HALFPI() - (POLMRG/2)

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50


      MIDLON      = PI() + ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(2,2) - ( 2 * POLMRG )

      R           = BOUNDS(2,3)

      CALL LATREC ( R, MIDLON, LAT, XXPT )



      MAG = 1.D3

      CALL VSCL   ( MAG, XXPT,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element ranging from equator to near ' &
      //           'south pole. Hit on outer sphere. Margin > 0. ' &
      //           'Interior case. Hit is beyond south latitude ' &
      //           'boundary.'   )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =   -HALFPI() + (POLMRG/1.001D0)
      BOUNDS(2,2) =   0.D0

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50


      MIDLON      = ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(1,2) - ( MARGIN/2 )

      R           = BOUNDS(2,3)

      CALL LATREC ( R, MIDLON, LAT, XXPT )



      MAG = 1.D3

      CALL VSCL   ( MAG, XXPT,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element ranging from equator to near ' &
      //           'south pole. Hit on outer sphere. Margin > 0. ' &
      //           'Interior case. Hit is above south latitude ' &
      //           'boundary, but below low latitude limit. ' &
      //           'Longitude is off by pi radians.'           )



      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =   -HALFPI() + (POLMRG/4)
      BOUNDS(2,2) =   0.D0

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50


      MIDLON      = PI() + ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(1,2) + ( MARGIN/4 )

      R           = BOUNDS(2,3)

      CALL LATREC ( R, MIDLON, LAT, XXPT )



      MAG = 1.D3

      CALL VSCL   ( MAG, XXPT,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element ranging from equator to near ' &
      //           'north pole. Hit on outer sphere. Margin > 0. ' &
      //           'Exterior case. Hit is above south latitude ' &
      //           'boundary, and above low latitude limit. ' &
      //           'Longitude is off by pi radians.'           )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =   -HALFPI() + (POLMRG/2)
      BOUNDS(2,2) =   0.D0

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50


      MIDLON      = PI() + ( BOUNDS(1,1) + BOUNDS(2,1) ) / 2

      LAT         = BOUNDS(1,2) + ( 2 * POLMRG )

      R           = BOUNDS(2,3)

      CALL LATREC ( R, MIDLON, LAT, XXPT )



      MAG = 1.D3

      CALL VSCL   ( MAG, XXPT,    VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is "orange slice" ranging from south to ' &
      //           'north pole. Vertex is on +Z axis; ray points ' &
      //           'in -Z direction.'                              )


      MARGIN = 1.D-8

      BOUNDS(1,1) =  -PI()/6
      BOUNDS(2,1) =   PI()/6

      BOUNDS(1,2) =   -HALFPI()
      BOUNDS(2,2) =    HALFPI()

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50


      MAG = 1.D10

      CALL VPACK  ( 0.D0, 0.D0, BOUNDS(2,3), XXPT   )
      CALL VPACK  ( 0.D0, 0.D0, MAG,         VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is "orange slice" ranging from south to ' &
      //           'north pole. Vertex is on -Z axis; ray points ' &
      //           'in +Z direction.'                              )


      CALL VPACK  ( 0.D0, 0.D0, -BOUNDS(2,3), XXPT   )
      CALL VPACK  ( 0.D0, 0.D0, -MAG,         VERTEX )
      CALL VSUB   ( XXPT, VERTEX, RAYDIR )


      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )


!
!     The following cases deal with hits near the poles but outside
!     of the latitude margins.
!
!
!        HIGH LATITUDE, OUTER SPHERE
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is high latitude patch. Ray hits beyond ' &
      //           'east longitude boundary. Test of longitude margin ' &
      //           'scaling. Interior case.'                           )


      MARGIN      = 1.D-8

      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =  PI()/6

      BOUNDS(1,2) = 5*PI()/12
      BOUNDS(2,2) = (PI()/2) - (2*POLMRG)

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)
      LAT         = BOUNDS(2,2) - MARGIN
      LON         = BOUNDS(2,1) + ( (MARGIN/2) / COS(LAT) )


      CALL LATREC ( R, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, MAG, Z, VERTEX )
      CALL VMINUS ( Z,                  RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is high latitude patch. Ray hits beyond ' &
      //           'east longitude boundary. Test of longitude margin ' &
      //           'scaling. Exterior case.'                           )



      R           = BOUNDS(2,3)
      LAT         = BOUNDS(2,2) - MARGIN
      LON         = BOUNDS(2,1) + ( (MARGIN*2) / COS(LAT) )


      CALL LATREC ( R, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, MAG, Z, VERTEX )
      CALL VMINUS ( Z,                  RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is high latitude patch. Ray hits beyond ' &
      //           'west longitude boundary. Test of longitude margin ' &
      //           'scaling. Interior case.'                           )


      MARGIN      = 1.D-8

      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =  PI()/6

      BOUNDS(1,2) = 5*PI()/12
      BOUNDS(2,2) = (PI()/2) - (2*POLMRG)

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)
      LAT         = BOUNDS(2,2) - MARGIN
      LON         = BOUNDS(1,1) - ( (MARGIN/2) / COS(LAT) )


      CALL LATREC ( R, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, MAG, Z, VERTEX )
      CALL VMINUS ( Z,                  RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is high latitude patch. Ray hits beyond ' &
      //           'west longitude boundary. Test of longitude margin ' &
      //           'scaling. Exterior case.'                           )



      R           = BOUNDS(2,3)
      LAT         = BOUNDS(2,2) - MARGIN
      LON         = BOUNDS(1,1) - ( (MARGIN*2) / COS(LAT) )


      CALL LATREC ( R, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, MAG, Z, VERTEX )
      CALL VMINUS ( Z,                  RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
!        LOW LATITUDE, OUTER SPHERE
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is low latitude patch. Ray hits beyond ' &
      //           'east longitude boundary. Test of longitude margin ' &
      //           'scaling. Interior case.'                           )


      MARGIN      = 1.D-8

      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =  PI()/6

      BOUNDS(1,2) = (-PI()/2) + (2*POLMRG)
      BOUNDS(2,2) = -5*PI()/12

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)
      LAT         = BOUNDS(1,2) + MARGIN
      LON         = BOUNDS(2,1) + ( (MARGIN/2) / COS(LAT) )


      CALL LATREC ( R, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, -MAG, Z, VERTEX )
      CALL VEQU   ( Z,                   RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is low latitude patch. Ray hits beyond ' &
      //           'east longitude boundary. Test of longitude margin ' &
      //           'scaling. Exterior case.'                           )



      R           = BOUNDS(2,3)
      LAT         = BOUNDS(1,2) + MARGIN
      LON         = BOUNDS(2,1) + ( (MARGIN*2) / COS(LAT) )


      CALL LATREC ( R, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, -MAG, Z, VERTEX )
      CALL VEQU   ( Z,                   RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is low latitude patch. Ray hits beyond ' &
      //           'west longitude boundary. Test of longitude margin ' &
      //           'scaling. Interior case.'                           )


      MARGIN      = 1.D-8

      BOUNDS(1,1) = -PI()/6
      BOUNDS(2,1) =  PI()/6

      BOUNDS(1,2) = (-PI()/2) + (2*POLMRG)
      BOUNDS(2,2) = -5*PI()/12

      BOUNDS(1,3) = 3.D3
      BOUNDS(2,3) = 3.D3 + 50

      R           = BOUNDS(2,3)
      LAT         = BOUNDS(1,2) + MARGIN
      LON         = BOUNDS(1,1) - ( (MARGIN/2) / COS(LAT) )


      CALL LATREC ( R, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, -MAG, Z, VERTEX )
      CALL VEQU   ( Z,                   RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  1, 0, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'XPT',   XPT, '~~/', XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Element is low latitude patch. Ray hits beyond ' &
      //           'west longitude boundary. Test of longitude margin ' &
      //           'scaling. Exterior case.'                           )



      R           = BOUNDS(2,3)
      LAT         = BOUNDS(1,2) + MARGIN
      LON         = BOUNDS(1,1) - ( (MARGIN*2) / COS(LAT) )

      CALL LATREC ( R, LON, LAT, XXPT )

      MAG = 1.D3

      CALL VLCOM  ( 1.D0, XXPT, -MAG, Z, VERTEX )
      CALL VEQU   ( Z,                   RAYDIR )

      CALL ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, MARGIN, NXPTS, XPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=',  0, 0, OK )







!***********************************************************************
!
!     Error cases
!
!***********************************************************************

!
!     ZZRYTLAT is declared to be error free, in the interest of
!     speed. This decision may need to be revisited.
!

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


