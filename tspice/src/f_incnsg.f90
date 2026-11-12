!$Procedure F_INCNSG ( INCNSG tests )

      SUBROUTINE F_INCNSG ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the line segment-cone intersection routine INCNSG.
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
!     This routine tests the line segment-cone intersection
!     routine INCNSG.
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
!-    TSPICE Version 1.0.0, 30-SEP-2016 (NJB)
!
!        Added test cases.
!
!        18-MAR-2016 (NJB)
!
!           Removed unneeded declarations.
!
!        24-SEP-2014 (NJB)
!
!           Original version.
!-&

!
!     SPICELIB functions
!


!
!     Local Parameters
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-12 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-10 )

      DOUBLE PRECISION      MED
      PARAMETER           ( MED    = 1.D-9 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    TITLE


      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      APEX   ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      CONDIR ( 3 )
      DOUBLE PRECISION      CONPT1 ( 3 )
      DOUBLE PRECISION      CONPT2 ( 3 )
      DOUBLE PRECISION      CONVTX ( 3 )
      DOUBLE PRECISION      CONX   ( 3 )
      DOUBLE PRECISION      CONZ   ( 3 )
      DOUBLE PRECISION      DALPHA
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DP
      DOUBLE PRECISION      DSPAN
      DOUBLE PRECISION      ENDPT1 ( 3 )
      DOUBLE PRECISION      ENDPT2 ( 3 )
      DOUBLE PRECISION      EP1    ( 3 )
      DOUBLE PRECISION      EP1OFF ( 3 )
      DOUBLE PRECISION      EP2    ( 3 )
      DOUBLE PRECISION      EP2OFF ( 3 )
      DOUBLE PRECISION      H
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SPAN
      DOUBLE PRECISION      TARG   ( 3 )
      DOUBLE PRECISION      TARGY
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XPT1   ( 3 )
      DOUBLE PRECISION      XPT2   ( 3 )
      DOUBLE PRECISION      XVEC   ( 3 )
      DOUBLE PRECISION      XXPT1  ( 3 )
      DOUBLE PRECISION      XXPT2  ( 3 )
      DOUBLE PRECISION      XZ
      DOUBLE PRECISION      YVEC   ( 3 )
      DOUBLE PRECISION      Z

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NCONE
      INTEGER               NLAT
      INTEGER               NSPAN
      INTEGER               NXPTS
      INTEGER               XNXPTS

!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_INCNSG' )




!**********************************************************************
!
!     Error cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Axis is zero vector.' )


      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, AXIS )

      ANGLE = 30.D0 * RPD()

      XZ    = SQRT( 3.D0 )

      CALL VPACK ( -1.D0, 0.D0, XZ/2, ENDPT1 )
      CALL VPACK ( -1.D0, 0.D0, 2.D0, ENDPT2 )

      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Angular radius of cone is negative.' )



      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )

      ANGLE = -1.D-12

      XZ    = SQRT( 3.D0 )

      CALL VPACK ( -1.D0, 0.D0, XZ/2, ENDPT1 )
      CALL VPACK ( -1.D0, 0.D0, 2.D0, ENDPT2 )

      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDANGLE)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Line segment has zero length.' )



      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )

      ANGLE = 30.D0 * RPD()

      XZ    = SQRT( 3.D0 )

      CALL VPACK ( -1.D0, 0.D0, XZ/2, ENDPT1 )
      CALL VEQU  ( ENDPT1, ENDPT2 )

      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .TRUE., 'SPICE(ENDPOINTSMATCH)', OK )



!
!     To be completed:
!
! --- Case: ------------------------------------------------------
!
!      TITLE =  'Coefficients have absolute values that are too large.'
!
!      CALL TCASE ( TITLE )





!**********************************************************************
!
!     Non-error exceptional cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Cone is the half-space {Z>=0}. Segment is ' &
      //           'vertical and intersects cone.'             )

      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )

      ANGLE = 90.D0 * RPD()


      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0,  3.D0, ENDPT2 )


      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL VPACK ( -1.D0, 2.D0, 0.D0, XXPT1 )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

!
!     Repeat call with segment endpoints reversed.
!
      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, &
                    ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )


      CALL VPACK ( -1.D0, 2.D0, 0.D0, XXPT1 )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Cone is the half-space {Z>=0}. Segment is ' &
      //           'vertical and misses cone.'                )

      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0, -2.D0, ENDPT2 )


      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Stretch case: cone is ALMOST the half-space ' &
      //           '{Z>=0}; cone angle is pi/2 - 1.e-9. Segment is ' &
      //           'vertical and intersects cone.'                  )

      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )

      ANGLE = ( 90.D0 * RPD() ) - 1.D-9


      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0,  3.D0, ENDPT2 )


      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL VPACK ( -1.D0, 2.D0, 0.D0, XXPT1 )

      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

!
!     Repeat call with segment endpoints reversed.
!
      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, &
                    ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )


      CALL VPACK ( -1.D0, 2.D0, 0.D0, XXPT1 )

      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Stretch case: cone is ALMOST the half-space ' &
      //           '{Z>=0}; cone angle is pi/2 - 1.e-9. Segment is ' &
      //           'vertical and misses cone.'                      )

      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0, -2.D0, ENDPT2 )


      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Stretch case: cone is ALMOST the half-space ' &
      //           '{ X: <X-APEX,AXIS>  >  0}; ' &
      //           'cone angle is pi/2 - 1.e-9. Segment is ' &
      //           'parallel to AXIS and intersects cone.' )


      CALL VPACK ( 100.D0, -200.D0, 300.D0, APEX )

      CALL VPACK  ( -7.D0, 5.D0, 1.D0, AXIS )

      CALL FRAME ( AXIS, XVEC, YVEC )

      ANGLE = ( 90.D0 * RPD() ) - 1.D-9


      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC, -3.D0, AXIS, VTEMP  )

      CALL VADD ( VTEMP, APEX, ENDPT1 )

      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC,  3.D0, AXIS, VTEMP )

      CALL VADD ( VTEMP, APEX, ENDPT2 )



      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (up)', NXPTS, '=', 1, 0, OK )

      Z = SQRT(5.D0) * TAN( HALFPI() - ANGLE )


      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT1 )

      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (up)', XPT1, '~~/', XXPT1, 3, TOL, OK )

!
!     Repeat call with segment endpoints reversed.
!
      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, &
                    ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (down)', NXPTS, '=', 1, 0, OK )


      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC, -Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT1 )


      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (down)', XPT1, '~~/', XXPT1, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Stretch case: cone is ALMOST the half-space ' &
      //           '{ X: <X-APEX,AXIS>  <  0}; ' &
      //           'cone angle is pi/2 + 1.e-9. Segment is ' &
      //           'parallel to AXIS and intersects cone.' )


      CALL VPACK ( 100.D0, -200.D0, 300.D0, APEX )

      CALL VPACK  ( -7.D0, 5.D0, 1.D0, AXIS )

      CALL FRAME ( AXIS, XVEC, YVEC )

      ANGLE = ( 90.D0 * RPD() ) + 1.D-9


      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC, -3.D0, AXIS, VTEMP  )

      CALL VADD ( VTEMP, APEX, ENDPT1 )

      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC,  3.D0, AXIS, VTEMP )

      CALL VADD ( VTEMP, APEX, ENDPT2 )



      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (up)', NXPTS, '=', 1, 0, OK )

      Z = SQRT(5.D0) * TAN( HALFPI() - ANGLE )


      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT1 )

      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (up)', XPT1, '~~/', XXPT1, 3, TOL, OK )

!
!     Repeat call with segment endpoints reversed.
!
      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, &
                    ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (down)', NXPTS, '=', 1, 0, OK )


      CALL VLCOM3 ( -1.D0, XVEC, 2.D0, YVEC, -Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT1 )


      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (down)', XPT1, '~~/', XXPT1, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Stretch case: cone is ALMOST the half-space ' &
      //           '{ X: <X-APEX,AXIS>  <  0}; ' &
      //           'cone angle is pi/2 - 1.e-9. Segment is ' &
      //           'orthogonal to cone''s axis and intersects cone ' &
      //           'twice.'                                       )


      CALL VPACK ( 100.D0, -200.D0, 300.D0, APEX )

      CALL VPACK  ( -7.D0, 5.D0, 1.D0, AXIS )

      CALL FRAME ( AXIS, XVEC, YVEC )

      ANGLE = ( 90.D0 * RPD() ) - 1.D-9

!
!     Z is the magnitude of the projections of the endpoints
!     onto the cone's axis.
!
      Z = 1.D-10

      CALL VLCOM3 (  1.D0, XVEC, -2.D0, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, ENDPT1 )

      CALL VLCOM3 ( -1.D0, XVEC,  2.D0, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, ENDPT2 )


      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (1)', NXPTS, '=', 2, 0, OK )



!
!     Compute expected value of XPT1.
!
      R = SQRT(5.D0)
      Z = 1.D-10
      S = Z / TAN( ABS(HALFPI()-ANGLE) )


      CALL VLCOM3 (  1.D0*S/R, XVEC, -2.D0*S/R, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT1 )


      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (1)', XPT1, '~~/', XXPT1, 3, TOL, OK )

      CALL VLCOM3 ( -1.D0*S/R, XVEC,  2.D0*S/R, YVEC, Z, AXIS, VTEMP  )
      CALL VADD ( VTEMP, APEX, XXPT2 )


      TOL = 1.D-7

      CALL CHCKAD ( 'XPT2 (1)', XPT2, '~~/', XXPT2, 3, TOL, OK )

!
!     Repeat call with segment endpoints reversed.
!
      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, &
                    ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (2)', NXPTS, '=', 2, 0, OK )


      CALL VLCOM3 ( -1.D0*S/R, XVEC,  2.D0*S/R, YVEC, Z, AXIS, VTEMP  )
      CALL VADD ( VTEMP, APEX, XXPT1 )


      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (2)', XPT1, '~~/', XXPT1, 3, TOL, OK )

      CALL VLCOM3 (  1.D0*S/R, XVEC, -2.D0*S/R, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT2 )

      CALL CHCKAD ( 'XPT2 (2)', XPT2, '~~/', XXPT2, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Stretch case: cone is ALMOST the half-space ' &
      //           '{ X: <X-APEX,AXIS>  <  0}; ' &
      //           'cone angle is pi/2 + 1.e-9. Segment is ' &
      //           'orthogonal to cone''s axis and intersects cone ' &
      //           'twice.'                                       )


      CALL VPACK ( 100.D0, -200.D0, 300.D0, APEX )

      CALL VPACK  ( -7.D0, 5.D0, 1.D0, AXIS )

      CALL FRAME ( AXIS, XVEC, YVEC )

      ANGLE = ( 90.D0 * RPD() ) + 1.D-9

!
!     Z is the magnitude of the projections of the endpoints
!     onto the cone's axis.
!
      Z = -1.D-10

      CALL VLCOM3 (  1.D0, XVEC, -2.D0, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, ENDPT1 )

      CALL VLCOM3 ( -1.D0, XVEC,  2.D0, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, ENDPT2 )


      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (1)', NXPTS, '=', 2, 0, OK )



!
!     Compute expected value of XPT1.
!
      R = SQRT(5.D0)
      Z = 1.D-10
      S = ABS(Z) / TAN( ABS(HALFPI()-ANGLE) )


      CALL VLCOM3 (  1.D0*S/R, XVEC, -2.D0*S/R, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT1 )


      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (1)', XPT1, '~~/', XXPT1, 3, TOL, OK )

      CALL VLCOM3 ( -1.D0*S/R, XVEC,  2.D0*S/R, YVEC, Z, AXIS, VTEMP  )
      CALL VADD ( VTEMP, APEX, XXPT2 )


      TOL = 1.D-7

      CALL CHCKAD ( 'XPT2 (1)', XPT2, '~~/', XXPT2, 3, TOL, OK )

!
!     Repeat call with segment endpoints reversed.
!
      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, &
                    ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS (2)', NXPTS, '=', 2, 0, OK )


      CALL VLCOM3 ( -1.D0*S/R, XVEC,  2.D0*S/R, YVEC, Z, AXIS, VTEMP  )
      CALL VADD ( VTEMP, APEX, XXPT1 )


      TOL = 1.D-7

      CALL CHCKAD ( 'XPT1 (2)', XPT1, '~~/', XXPT1, 3, TOL, OK )

      CALL VLCOM3 (  1.D0*S/R, XVEC, -2.D0*S/R, YVEC, Z, AXIS, VTEMP )
      CALL VADD ( VTEMP, APEX, XXPT2 )

      CALL CHCKAD ( 'XPT2 (2)', XPT2, '~~/', XXPT2, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Cone is ALMOST the half-space {Z>=0}; ' &
      //           'cone angle is pi/2 - 1.e-15. Segment is ' &
      //           'vertical and intersects cone.'             )

      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )

      ANGLE = ( 90.D0 * RPD() ) - 1.D-15


      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0,  3.D0, ENDPT2 )


      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL VPACK ( -1.D0, 2.D0, 0.D0, XXPT1 )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

!
!     Repeat call with segment endpoints reversed.
!
      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, &
                    ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )


      CALL VPACK ( -1.D0, 2.D0, 0.D0, XXPT1 )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Cone is ALMOST the half-space {Z>=0}; ' &
      //           'cone angle is pi/2 - 1.e-15. Segment is ' &
      //           'vertical and misses cone.'             )

      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0, -2.D0, ENDPT2 )


      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Cone is the half-space {X<=5}. Segment is ' &
      //           'parallel to X axis and intersects cone.'   )

      CALL VPACK  (  5.D0, 3.D0, 4.D0, APEX )
      CALL VPACK  ( -1.D0, 0.D0, 0.D0, AXIS )

      ANGLE = 90.D0 * RPD()


      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK (  6.D0, 2.D0, -3.D0, ENDPT2 )


      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL VPACK ( 5.D0, 2.D0, -3.D0, XXPT1 )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

!
!     Repeat call with segment endpoints reversed.
!
      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, &
                    ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL VPACK ( 5.D0, 2.D0, -3.D0, XXPT1 )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Cone is the half-space {X<=5}. Segment is ' &
      //           'parallel to X axis and misses cone.'   )

      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -4.D0, 2.D0, -3.D0, ENDPT2 )


      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Cone is ALMOST the half-space {Z>=0}; ' &
      //           'cone angle is pi/2 - 1.e-7. Segment is ' &
      //           'vertical and intersects cone.'             )

      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )

      ANGLE = ( 90.D0 * RPD() ) - 1.D-7

      CALL VPACK ( -1.D0, 2.D0, -3.D0, ENDPT1 )
      CALL VPACK ( -1.D0, 2.D0,  3.D0, ENDPT2 )


      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL VPACK ( -1.D0, 2.D0, TAN(HALFPI()-ANGLE)*SQRT(5.D0), XXPT1 )

      TOL = 1.D-8

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )

!
!     Repeat call with segment endpoints reversed.
!
      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, &
                    ENDPT1, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, TOL, OK )



!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Trivial case: cone apex is origin; axis is +Z; ' &
      //       'angular radius is 30 degrees; segment''s ' &
      //       'first endpoint is (-1,0,sqrt(3)/2); second endpoint is ' &
      //       '(0,0,sqrt(3)/2).'

      CALL TCASE ( TITLE )


      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )

      ANGLE = 30.D0 * RPD()

      XZ    = SQRT( 3.D0 ) / 2

      CALL VPACK ( -1.D0, 0.D0, XZ, ENDPT1 )
      CALL VPACK (  0.D0, 0.D0, XZ, ENDPT2 )

      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect one point of intersection to be found.
!
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

      IF ( OK ) THEN

         CALL VPACK  ( -0.5D0, 0.D0, XZ, XXPT1 )
         CALL CLEARD ( 3, XXPT2 )

         CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, VTIGHT, OK )

      END IF


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Trivial case: cone apex is origin; axis is +Z; ' &
      //       'angular radius is 30 degrees; segment''s ' &
      //       'first endpoint is (-1,0,sqrt(3)/2); second endpoint is ' &
      //       '(-1,0,2).'

      CALL TCASE ( TITLE )


      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )

      ANGLE = 30.D0 * RPD()

      XZ    = SQRT( 3.D0 )

      CALL VPACK ( -1.D0, 0.D0, XZ/2, ENDPT1 )
      CALL VPACK ( -1.D0, 0.D0, 2.D0, ENDPT2 )

      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect one point of intersection to be found.
!
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

      IF ( OK ) THEN

         CALL VPACK  ( -1.D0, 0.D0, XZ, XXPT1 )
         CALL CLEARD ( 3, XXPT2 )

         CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, VTIGHT, OK )

      END IF


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Trivial case: cone apex is origin; axis is +Z; ' &
      //       'angular radius is 30 degrees; segment''s ' &
      //       'first endpoint is (-1,0,sqrt(3)/2); second endpoint is ' &
      //       '(1,0,sqrt(3)/2).'

      CALL TCASE ( TITLE )


      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )

      ANGLE = 30.D0 * RPD()

      XZ    = SQRT( 3.D0 ) / 2

      CALL VPACK ( -1.D0, 0.D0, XZ, ENDPT1 )
      CALL VPACK (  1.D0, 0.D0, XZ, ENDPT2 )

      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect two points of intersection to be found.
!
      XNXPTS = 2

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

      IF ( OK ) THEN

         CALL VPACK ( -0.5D0, 0.D0, XZ, XXPT1 )
         CALL VPACK (  0.5D0, 0.D0, XZ, XXPT2 )

         CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, VTIGHT, OK )
         CALL CHCKAD ( 'XPT2', XPT2, '~~/', XXPT2, 3, VTIGHT, OK )

      END IF


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Repeat previous case but offset both the apex ' &
      //       'and ray''s vertex.'

      CALL TCASE ( TITLE )


      CALL VPACK  ( 3.D0, 4.D0, -5.D0, APEX )

      ANGLE = 30.D0 * RPD()

      XZ    = SQRT( 3.D0 ) / 2

      CALL VPACK ( -1.D0,  0.D0, XZ, ENDPT1 )
      CALL VADD  ( ENDPT1, APEX,     VTEMP  )
      CALL VEQU  ( VTEMP,            ENDPT1 )

      CALL VPACK (  1.D0, 0.D0, XZ, ENDPT2 )
      CALL VADD  ( ENDPT2, APEX,     VTEMP  )
      CALL VEQU  ( VTEMP,            ENDPT2 )


      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect two points of intersection to be found.
!
      XNXPTS = 2

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

      IF ( OK ) THEN

         CALL VPACK ( -0.5D0, 0.D0, XZ, XXPT1 )
         CALL VADD  ( XXPT1,  APEX,     VTEMP )
         CALL VEQU  ( VTEMP,            XXPT1 )

         CALL VPACK (  0.5D0, 0.D0, XZ, XXPT2 )
         CALL VADD  ( XXPT2,  APEX,     VTEMP )
         CALL VEQU  ( VTEMP,            XXPT2 )

         CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, VTIGHT, OK )
         CALL CHCKAD ( 'XPT2', XPT2, '~~/', XXPT2, 3, VTIGHT, OK )

      END IF


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Cone angle exceeds pi/2: cone apex is origin; axis ' &
      //       'is +Z; angular radius is 150 degrees; segment''s ' &
      //       'first endpoint is (-1,0,-sqrt(3)/2); second endpoint ' &
      //       'is (1,0,-sqrt(3)/2).'

      CALL TCASE ( TITLE )


      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )

      ANGLE = 150.D0 * RPD()

      XZ    = -SQRT( 3.D0 ) / 2

      CALL VPACK ( -1.D0, 0.D0, XZ, ENDPT1 )
      CALL VPACK (  1.D0, 0.D0, XZ, ENDPT2 )

      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect two points of intersection to be found.
!
      XNXPTS = 2

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

      IF ( OK ) THEN

         CALL VPACK ( -0.5D0, 0.D0, XZ, XXPT1 )
         CALL VPACK (  0.5D0, 0.D0, XZ, XXPT2 )

         CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, VTIGHT, OK )
         CALL CHCKAD ( 'XPT2', XPT2, '~~/', XXPT2, 3, VTIGHT, OK )

      END IF


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Cone angle exceeds pi/2: cone apex is origin; axis ' &
      //       'is +Z; angular radius is 150 degrees; segment''s ' &
      //       'first endpoint is (-1,0,-sqrt(3)/2); second endpoint ' &
      //       'is (0,0,-sqrt(3)/2).'

      CALL TCASE ( TITLE )


      CALL CLEARD ( 3, APEX )
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, AXIS )

      ANGLE = 150.D0 * RPD()

      XZ    = -SQRT( 3.D0 ) / 2

      CALL VPACK ( -1.D0, 0.D0, XZ, ENDPT1 )
      CALL VPACK (  0.D0, 0.D0, XZ, ENDPT2 )

      CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                    ENDPT2, NXPTS, XPT1,  XPT2   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect one point of intersection to be found.
!
      XNXPTS = 1

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

      IF ( OK ) THEN

         CALL VPACK ( -0.5D0, 0.D0, XZ, XXPT1 )

         CALL CHCKAD ( 'XPT1', XPT1, '~~/', XXPT1, 3, VTIGHT, OK )

      END IF




!**********************************************************************
!
!     Normal cases --- varied geometry
!
!**********************************************************************

!
!     The following are intersection cases.
!
!     All intersection cases for lines that are not orthogonal to the
!     cone's axis can be characterized by:
!
!        1) The line's intersection with the plane containing the apex
!           and normal to the axis. This point of intersection is the
!           vertex of a ray contained in the line and intersecting the
!           cone.
!
!        2) The ray's angle relative to the plane described in (1).
!
!        3) The azimuth of the ray relative to the line segment starting
!           at the ray's vertex and ending at the cone's apex.
!
!
!     Pick an arbitrary cone axis and apex.
!
!
      CALL VPACK ( 3.D0, -2.D0,  7.D0, APEX )

      CALL VPACK ( 2.D0,  3.D0, -2.D0, AXIS )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )

      CALL VHAT  ( AXIS,  CONZ )

!
!     Pick an arbitrary ray vertex in the X-Y plane.
!
      THETA  = 27.D0 * RPD()

      R      = 1000.D0

      CALL VPACK ( R*COS(THETA), R*SIN(THETA), 0.D0, VTEMP )
!
!     Create the transformation matrix for mapping vectors
!     from the standard frame to the cone frame.
!
      CALL TWOVEC ( CONZ, 3, VTEMP, 1, XFORM )

      DO I = 1, 3

         CONX(I) = XFORM(1,I)

      END DO

      CALL VSCL ( R, CONX, CONVTX )

!
!     Loop over cone angles.
!
      NCONE  = 10

      DALPHA = HALFPI() / (NCONE+1)


!
!     Note: NLAT can be increased to 100 without an excessive
!     increase in run time. However, the value 10 exercises a
!     difficult numerical case: that in which the ray is normal
!     to the cone at the first intercept.
!
      NLAT   = 10

      DLAT   = HALFPI() / (NLAT+1)


!
!     Let the cone angles range from DALPHA to pi/2 - DALPHA.
!
      DO I = 1, NCONE

         ANGLE = I * DALPHA

!
!        Let the ray "latitude" vary from DLAT to pi/2 - DLAT. Note
!        that this is an approximation unless the ray's direction
!        vector has a zero Y component.
!
         DO J = 1, NLAT

            LAT = J * DLAT

!
!           Let H be the Z intercept of a ray emanating from CONVTX,
!           pointing toward the +Z axis, and making angle LAT relative
!           to the X-Y plane.
!
            H = R * TAN( LAT )
!
!           Let SPAN be the length of a line segment parallel to the Y
!           axis and passing through the +Z axis at height H.
!
            SPAN = 2.D0 * H * TAN( ANGLE )

!
!           We'll place a "target" on the segment from which SPAN was
!           derived.
!
            NSPAN = 11

            DSPAN = SPAN / (NSPAN-1)

            DO K = 1, NSPAN

               TARGY = (-SPAN/2) + (K-1)*DSPAN

               IF ( ( K .EQ. 1 ) .OR. ( K .EQ. NSPAN ) ) THEN
!
!                 Contract TARGY a bit, since these are formally
!                 tangent cases, and we want to ensure a hit.
!
                  TARGY = TARGY * ( 1.D0 - 1.D-3 )

               END IF

               CALL VPACK ( 0.D0, TARGY,  H, TARG )

               CALL VSUB  ( TARG, CONVTX, CONDIR )

               CALL VHATIP( CONDIR )

               CALL VEQU  ( CONVTX, CONPT1 )

!
!              Let R2 be the distance of the second endpoint from
!              the vertex.
!
               R2 = 1.D5
               CALL VLCOM ( 1.D0, CONPT1, R2, CONDIR, CONPT2 )

!
!              Shift the endpoints to make them relative to the origin
!              of the standard frame. Convert the endpoints back to the
!              standard basis.
!
               CALL MTXV ( XFORM,  CONPT1, VTEMP  )
               CALL VADD ( VTEMP,  APEX,   ENDPT1 )

               CALL MTXV ( XFORM,  CONPT2, VTEMP  )
               CALL VADD ( VTEMP,  APEX,   ENDPT2 )

               CALL MTXV ( XFORM,  CONVTX, VTEMP  )
               CALL VADD ( VTEMP,  APEX,   VERTEX )

               CALL MTXV ( XFORM,  TARG,   VTEMP  )
               CALL VADD ( VTEMP,  APEX,   TARG   )

               CALL MTXV ( XFORM,  CONDIR, VTEMP )
               CALL VEQU ( VTEMP,          RAYDIR )



!
! --- Case: ------------------------------------------------------
!
               TITLE = 'Intercept case: I = #; J = #; K = #'
               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL REPMI ( TITLE, '#', K, TITLE )

               CALL TCASE ( TITLE )


               CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                             ENDPT2, NXPTS, XPT1,  XPT2   )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSI ( 'NXPTS', NXPTS, '>', 0, 0, OK )

               IF ( NXPTS .GE. 1 ) THEN
!
!                 Check XPT1.
!
!                 Make sure XPT1 is on the ray containing the
!                 line segment.
!
                  CALL NPLNPT ( VERTEX, RAYDIR, XPT1, PNEAR, DIST )

                  TOL = TIGHT

                  CALL CHCKAD( 'PNEAR', PNEAR, '~~/', XPT1, 3, TOL, OK )

!
!                 Make sure XPT1 is on the correct side of the ray's
!                 vertex.
!
                  CALL VSUB ( XPT1, VERTEX, EP1OFF )

                  DP = VDOT ( EP1OFF, RAYDIR )

                  CALL CHCKSD ( 'DP 1', DP, '>', 0.D0, 0.D0, OK )

!
!                 Make sure XPT1 is on the cone's surface.
!
                  CALL VSUB ( XPT1, APEX, OFFSET )

                  SEP = VSEP( OFFSET, AXIS )

                  TOL = TIGHT

                  CALL CHCKSD ( 'XPT1 SEP', SEP, '~', ANGLE, TOL, OK )


                  IF ( NXPTS .EQ. 2 ) THEN

!
!                    Check XPT2.
!
!                    Make sure XPT2 is on the ray containing the
!                    line segment.
!
                     CALL NPLNPT ( VERTEX, RAYDIR, XPT2, PNEAR, DIST )

                     TOL = TIGHT

                     CALL CHCKAD( 'PNEAR', PNEAR, '~~/', &
                                           XPT2, 3, TOL, OK )

!
!                    Make sure XPT2 is on the correct side of the ray's
!                    vertex.
!
                     CALL VSUB ( XPT2, VERTEX, EP2OFF )

                     DP = VDOT ( EP2OFF, RAYDIR )

                     CALL CHCKSD ( 'DP 2', DP, '>', 0.D0, 0.D0, OK )

!
!                    Check that XPT2 is farther from VERTEX than XPT1.
!
                     CALL CHCKSD ( 'XPT2 offset', VNORM(EP2OFF), '>', &
                                   VNORM(EP1OFF), 0.D0,          OK )

!
!                    Make sure XPT2 is on the cone's surface.
!
                     CALL VSUB ( XPT2, APEX, OFFSET )

                     SEP = VSEP( OFFSET, AXIS )

                     TOL = TIGHT

                     CALL CHCKSD ( 'XPT2 SEP', SEP, '~', &
                                               ANGLE, TOL, OK )

                  END IF

               END IF


!
! --- Case: ------------------------------------------------------
!
               TITLE = 'Intercept case with reversed endpoints: ' &
               //      'I = #; J = #; K = #'
               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL REPMI ( TITLE, '#', K, TITLE )

               CALL TCASE ( TITLE )


               CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT2, &
                             ENDPT1, NXPTS, XPT1,  XPT2   )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSI ( 'NXPTS', NXPTS, '>', 0, 0, OK )

               IF ( NXPTS .GE. 1 ) THEN
!
!                 Check XPT1.
!
!                 Make sure XPT1 is on the ray containing the
!                 line segment.
!
                  CALL NPLNPT ( VERTEX, RAYDIR, XPT1, PNEAR, DIST )

                  TOL = MED

                  CALL CHCKAD( 'PNEAR', PNEAR, '~~/', XPT1, 3, TOL, OK )

!
!                 Make sure XPT1 is on the correct side of the ray's
!                 vertex.
!
                  CALL VSUB ( XPT1, VERTEX, EP1OFF )

                  DP = VDOT ( EP1OFF, RAYDIR )

                  CALL CHCKSD ( 'DP 1', DP, '>', 0.D0, 0.D0, OK )

!
!                 Make sure XPT1 is on the cone's surface.
!
                  CALL VSUB ( XPT1, APEX, OFFSET )

                  SEP = VSEP( OFFSET, AXIS )

                  TOL = MED

                  CALL CHCKSD ( 'XPT1 SEP', SEP, '~', ANGLE, TOL, OK )


                  IF ( NXPTS .EQ. 2 ) THEN

!
!                    Check XPT2.
!
!                    Make sure XPT2 is on the ray containing the
!                    line segment.
!
                     CALL NPLNPT ( VERTEX, RAYDIR, XPT2, PNEAR, DIST )

                     TOL = TIGHT

                     CALL CHCKAD( 'PNEAR', PNEAR, '~~/', &
                                           XPT2, 3, TOL, OK )

!
!                    Make sure XPT2 is on the correct side of the ray's
!                    vertex.
!
                     CALL VSUB ( XPT2, VERTEX, EP2OFF )

                     DP = VDOT ( EP2OFF, RAYDIR )

                     CALL CHCKSD ( 'DP 2', DP, '>', 0.D0, 0.D0, OK )

!
!                    Check that XPT1 is farther from VERTEX than XPT2.
!
                     CALL CHCKSD ( 'XPT1 offset', VNORM(EP1OFF), '>', &
                                   VNORM(EP2OFF), 0.D0,          OK )

!
!                    Make sure XPT2 is on the cone's surface.
!
                     CALL VSUB ( XPT2, APEX, OFFSET )

                     SEP = VSEP( OFFSET, AXIS )

                     TOL = MED

                     CALL CHCKSD ( 'XPT2 SEP', SEP, '~', &
                                               ANGLE, TOL, OK )

                  END IF


               END IF


!
! --- Case: ------------------------------------------------------
!
               TITLE = 'Intercept case with second endpoint in ' &
               //      'cone''s interior. ' &
               //      'I = #; J = #; K = #'
               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL REPMI ( TITLE, '#', K, TITLE )

               CALL TCASE ( TITLE )

!
!              Find the first intercept. Extend the segment slightly
!              to create the second endpoint.
!
               CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                             ENDPT2, NXPTS, XPT1,  XPT2   )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSI ( '(0) NXPTS', NXPTS, '>', 0, 0, OK )

               CALL VSUB ( XPT1, ENDPT1, EP1OFF )

               CALL VLCOM ( 1.D0, ENDPT1, (1.D0 + 1.D-3), EP1OFF, EP2 )

!
!              On this second call to INCNSG, use the new value
!              of the second endpoint.
!
               CALL INCNSG ( APEX, AXIS,  ANGLE, ENDPT1, &
                             EP2,  NXPTS, XPT1,  XPT2   )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              We expect exactly one hit.
!
               CALL CHCKSI ( '(1) NXPTS', NXPTS, '=', 1, 0, OK )


!
! --- Case: ------------------------------------------------------
!
               TITLE = 'Intercept case with first endpoint in ' &
               //      'cone''s interior. The endpoints are ' &
               //      'reversed relative to those of the previous ' &
               //      'case. I = #; J = #; K = #.'
               CALL REPMI ( TITLE, '#', I, TITLE )
               CALL REPMI ( TITLE, '#', J, TITLE )
               CALL REPMI ( TITLE, '#', K, TITLE )

               CALL TCASE ( TITLE )

!
!              Find the first intercept. Extend the segment slightly
!              to create the second endpoint.
!
               CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                             ENDPT2, NXPTS, XPT1,  XPT2   )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSI ( '(0) NXPTS', NXPTS, '>', 0, 0, OK )

               CALL VSUB ( XPT1, ENDPT1, EP1OFF )

               CALL VLCOM ( 1.D0, ENDPT1, (1.D0 + 1.D-3), EP1OFF, EP2 )

!
!              On this second call to INCNSG, use the new value
!              of the second endpoint as the first endpoint.
!
               CALL INCNSG ( APEX,   AXIS,  ANGLE, EP2, &
                             ENDPT1, NXPTS, XPT1,  XPT2   )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              We expect exactly one hit.
!
               CALL CHCKSI ( '(1) NXPTS', NXPTS, '=', 1, 0, OK )


!
! --- Case: ------------------------------------------------------
!
               IF ( K .EQ. ((NSPAN+1)/2) ) THEN
!
!                 Perform this test only for rays contained in the
!                 plane containing ENDPT1 and AXIS; this simplifies
!                 the geometry.
!

                  TITLE = 'Intercept case with first endpoint in ' &
                  //      'cone''s interior. ' &
                  //      'I = #; J = #; K = #'
                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL REPMI ( TITLE, '#', K, TITLE )

                  CALL TCASE ( TITLE )

!
!                 Find the first intercept. Extend the segment slightly
!                 to create the new value of the first endpoint.
!
                  CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                                ENDPT2, NXPTS, XPT1,  XPT2   )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL CHCKSI ( '(0) NXPTS', NXPTS, '>', 0, 0, OK )

                  CALL VSUB ( XPT1, ENDPT1, EP1OFF )

                  CALL VLCOM ( 1.D0,          ENDPT1, &
                              (1.D0 + 1.D-3), EP1OFF, EP1 )

!
!                 On this second call to INCNSG, use the new value of
!                 the first endpoint.
!
                  CALL INCNSG ( APEX,   AXIS,  ANGLE, EP1, &
                                ENDPT2, NXPTS, XPT1,  XPT2   )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  MARGIN = 1.D-10

                  IF ( LAT .LT. (HALFPI()-ANGLE)-MARGIN ) THEN
!
!                    We expect exactly one hit.
!
                     CALL CHCKSI ( '(1) NXPTS', NXPTS, '=', 1, 0, OK )

                  ELSE IF ( LAT .GT. (HALFPI()-ANGLE)+MARGIN ) THEN
!
!                    There should be no hits; the ray rises more steeply
!                    than the cone.
!
                     CALL CHCKSI ( '(1) NXPTS', NXPTS, '=', 0, 0, OK )

                  END IF


!
! --- Case: ------------------------------------------------------
!
!                 Perform this test only for rays contained in the
!                 plane containing ENDPT1 and AXIS; this simplifies
!                 the geometry.
!

                  TITLE = 'Intercept case with second endpoint in ' &
                  //      'cone''s interior. This is a repeat of ' &
                  //      'the previous case, with endpoints switched. ' &
                  //      'I = #; J = #; K = #'
                  CALL REPMI ( TITLE, '#', I, TITLE )
                  CALL REPMI ( TITLE, '#', J, TITLE )
                  CALL REPMI ( TITLE, '#', K, TITLE )

                  CALL TCASE ( TITLE )

!
!                 On this second call to INCNSG, use the new value of
!                 the first endpoint.
!
                  CALL INCNSG ( APEX, AXIS,  ANGLE, ENDPT2, &
                                EP1,  NXPTS, XPT1,  XPT2   )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  MARGIN = 1.D-10

                  IF ( LAT .LT. (HALFPI()-ANGLE)-MARGIN ) THEN
!
!                    We expect exactly one hit.
!
                     CALL CHCKSI ( '(1) NXPTS', NXPTS, '=', 1, 0, OK )

                  ELSE IF ( LAT .GT. (HALFPI()-ANGLE)+MARGIN ) THEN
!
!                    There should be no hits; the ray rises more steeply
!                    than the cone.
!
                     CALL CHCKSI ( '(1) NXPTS', NXPTS, '=', 0, 0, OK )

                  END IF

               END IF

            END DO

!
! --- Case: ------------------------------------------------------
!
!           Miss case: make the target too far off in the Y direction
!           for an intercept to exist.
!
            K     = 0
            TARGY = (-SPAN/2) + (K-1)*DSPAN

            TITLE = 'Non-intercept case: Y < -SPAN/2. ' &
            //      'I = #; J = #; K = #'
            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL REPMI ( TITLE, '#', K, TITLE )

            CALL TCASE ( TITLE )

            CALL VPACK ( 0.D0, TARGY,  H, TARG )

            CALL VSUB  ( TARG, CONVTX, CONDIR )

            CALL VHATIP( CONDIR )

            CALL VEQU  ( CONVTX, CONPT1 )

!
!           Let R2 be the distance of the second endpoint from
!           the vertex.
!
            R2 = 1.D5
            CALL VLCOM ( 1.D0, CONPT1, R2, CONDIR, CONPT2 )

!
!           Shift the endpoints to make them relative to the origin
!           of the standard frame. Convert the endpoints back to the
!           standard basis.
!
            CALL MTXV ( XFORM,  CONPT1, VTEMP  )
            CALL VADD ( VTEMP,  APEX,   ENDPT1 )

            CALL MTXV ( XFORM,  CONPT2, VTEMP  )
            CALL VADD ( VTEMP,  APEX,   ENDPT2 )

            CALL MTXV ( XFORM,  TARG,   VTEMP  )
            CALL VADD ( VTEMP,  APEX,   TARG   )

            CALL MTXV ( XFORM,  CONVTX, VTEMP  )
            CALL VADD ( VTEMP,  APEX,   VERTEX )

            CALL MTXV ( XFORM,  RAYDIR, VTEMP )
            CALL VEQU ( VTEMP,          RAYDIR )

            CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                          ENDPT2, NXPTS, XPT1,  XPT2   )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           We expect a miss.
!
            CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
!           Miss case: make the target too far off in the Y direction
!           for an intercept to exist.
!
            K     = NSPAN+1
            TARGY = DBLE(NSPAN/2) * DSPAN

            TITLE = 'Non-intercept case: Y > SPAN/2. ' &
            //      'I = #; J = #; K = #'
            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL REPMI ( TITLE, '#', K, TITLE )

            CALL TCASE ( TITLE )

            CALL VPACK ( 0.D0, TARGY,  H, TARG )

            CALL VSUB  ( TARG, CONVTX, RAYDIR )

            CALL VHATIP( RAYDIR )

            CALL VEQU  ( CONVTX, CONPT1 )

!
!           Let R2 be the distance of the second endpoint from
!           the vertex.
!
            R2 = 1.D5
            CALL VLCOM ( 1.D0, CONPT1, R2, CONDIR, CONPT2 )

!
!           Shift the endpoints to make them relative to the origin
!           of the standard frame. Convert the endpoints back to the
!           standard basis.
!
            CALL MTXV ( XFORM,  CONPT1, VTEMP  )
            CALL VADD ( VTEMP,  APEX,   ENDPT1 )

            CALL MTXV ( XFORM,  CONPT2, VTEMP  )
            CALL VADD ( VTEMP,  APEX,   ENDPT2 )

            CALL MTXV ( XFORM,  TARG,   VTEMP  )
            CALL VADD ( VTEMP,  APEX,   TARG   )

            CALL MTXV ( XFORM,  CONVTX, VTEMP  )
            CALL VADD ( VTEMP,  APEX,   VERTEX )

            CALL MTXV ( XFORM,  RAYDIR, VTEMP )
            CALL VEQU ( VTEMP,          RAYDIR )

            CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                          ENDPT2, NXPTS, XPT1,  XPT2   )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           We expect a miss.
!
            CALL CHCKSI ( 'NXPTS', NXPTS, '=', 0, 0, OK )

         END DO

      END DO


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


