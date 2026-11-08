!$Procedure      F_ZZCXBRUT ( Test brute force cone-segment intercept )

      SUBROUTINE F_ZZCXBRUT ( OK )
      IMPLICIT NONE

! $ Abstract
!
!     Test the SPICELIB brute force line segment-cone intersection
!     routine ZZCXBRUT.
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
!     This routine tests the routine ZZCXBRUT.
!
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
!-    TSPICE Version 1.0.0, 20-JAN-2017 (NJB)
!
!        Previous version 30-SEP-2016 (NJB)
!
!-&

!
!     Test Utility Functions
!


!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI

!
!     Local parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

!      DOUBLE PRECISION      TIGHT
!      PARAMETER           ( TIGHT  = 1.D-12 )

!
!     Local Variables
!
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      APEX   ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      ENDPT1 ( 3 )
      DOUBLE PRECISION      ENDPT2 ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )

      LOGICAL               ISBRCK


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZCXBRUT' )




!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'AXIS is +Z; line segment is ' &
      //           'parallel to X-Y plane. First endpoint ' &
      //           'is outside cone.'                      )


      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )

      ANGLE = PI()/4


      CALL VPACK ( 2.D0, 0.D0, 1.D0, ENDPT1 )
      CALL VPACK ( 0.D0, 0.D0, 1.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL VPACK ( 1.D0, 0.D0, 1.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'AXIS is +Z; line segment is ' &
      //           'parallel to X-Y plane. First endpoint ' &
      //           'is inside cone.'                       )


      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )

      ANGLE = PI()/4


      CALL VPACK ( 2.D0, 0.D0, 1.D0, ENDPT2 )
      CALL VPACK ( 0.D0, 0.D0, 1.D0, ENDPT1 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )


      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL VPACK ( 1.D0, 0.D0, 1.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'AXIS is +Z; line segment is ' &
      //           'parallel to Z axis. First endpoint ' &
      //           'is outside cone.'                    )


      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )

      ANGLE = PI()/4


      CALL VPACK ( 2.D0, 0.D0, 0.D0, ENDPT1 )
      CALL VPACK ( 2.D0, 0.D0, 3.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )


      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 2.D0, 0.D0, 2.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'AXIS is +Z; line segment is ' &
      //           'parallel to Z axis. First endpoint ' &
      //           'is inside cone.'                    )


      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )

      ANGLE = PI()/4


      CALL VPACK ( 2.D0, 0.D0, 0.D0, ENDPT2 )
      CALL VPACK ( 2.D0, 0.D0, 3.D0, ENDPT1 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )


      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 2.D0, 0.D0, 2.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'AXIS is +Z; line segment is ' &
      //           'parallel to Z axis; first endpoint is far ' &
      //           'below X-Y plane.'                          )


      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )

      ANGLE = PI()/4


      CALL VPACK ( 2.D0, 0.D0, -1.D10, ENDPT1 )
      CALL VPACK ( 2.D0, 0.D0,  3.D0,  ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )


      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 2.D0, 0.D0, 2.D0, XXPT )

      TOL = 1.D-5

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'AXIS is +Z; line segment is on the Z axis;' &
      //           'first endpoint is below X-Y plane.' )


      CALL VPACK ( 0.D0, 0.D0, 1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )

      ANGLE = PI()/4


      CALL VPACK ( 0.D0, 0.D0, -1.D0, ENDPT1 )
      CALL VPACK ( 0.D0, 0.D0,  3.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )


      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 0.D0, XXPT )

      TOL = VTIGHT

!
!     Use absolute test since the expected result is the
!     zero vector.
!
      CALL CHCKAD ( 'XPT', XPT, '~~', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'AXIS is +Z; line segment is ' &
      //           'parallel to X-Y plane. First endpoint ' &
      //           'is outside cone. Angle > pi/2.'         )


      CALL VPACK ( 0.D0, 0.D0,  1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0,  0.D0, APEX )

      ANGLE = 3*PI()/4


      CALL VPACK ( 2.D0, 0.D0, -1.D0, ENDPT1 )
      CALL VPACK ( 0.D0, 0.D0, -1.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL VPACK ( 1.D0, 0.D0, -1.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'AXIS is +Z; line segment is ' &
      //           'parallel to X-Y plane. First endpoint ' &
      //           'is outside cone. Angle > pi/2. ' &
      //           'APEX is non-zero.'                    )


      CALL VPACK ( 0.D0, 0.D0,  1.D0, AXIS )
      CALL VPACK ( 3.D0, 4.D0,  5.D0, APEX )

      ANGLE = 3*PI()/4


      CALL VPACK ( 5.D0, 4.D0,  4.D0, ENDPT1 )
      CALL VPACK ( 3.D0, 4.D0,  4.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL VPACK ( 4.D0, 4.D0, 4.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'AXIS is -Z; line segment is ' &
      //           'parallel to X-Y plane. First endpoint ' &
      //           'is outside cone. Angle > pi/2. ' &
      //           'APEX is non-zero.'                    )


      CALL VPACK ( 0.D0, 0.D0, -1.D0, AXIS )
      CALL VPACK ( 3.D0, 4.D0,  5.D0, APEX )

      ANGLE = 3*PI()/4


      CALL VPACK ( 5.D0, 4.D0,  6.D0, ENDPT1 )
      CALL VPACK ( 3.D0, 4.D0,  6.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL VPACK ( 4.D0, 4.D0, 6.D0, XXPT )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .TRUE., OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Root is not bracketed.'  )


      CALL VPACK ( 0.D0, 0.D0,  1.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0,  0.D0, APEX )

      ANGLE = PI()/4


      CALL VPACK ( 2.D0, 0.D0,  1.D0, ENDPT1 )
      CALL VPACK ( 3.D0, 0.D0,  1.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ISBRCK', ISBRCK, .FALSE., OK )




!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error: axis is zero vector.' )


      CALL VPACK ( 0.D0, 0.D0,  0.D0, AXIS )
      CALL VPACK ( 0.D0, 0.D0,  0.D0, APEX )

      ANGLE = PI()/4


      CALL VPACK ( 2.D0, 0.D0,  1.D0, ENDPT1 )
      CALL VPACK ( 3.D0, 0.D0,  1.D0, ENDPT2 )

      CALL ZZCXBRUT ( APEX, AXIS, ANGLE, ENDPT1, ENDPT2, XPT, ISBRCK )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )




      CALL T_SUCCESS ( OK )
      RETURN
      END
