!$Procedure F_ZZELNAXX ( ZZELNAXX tests )

      SUBROUTINE F_ZZELNAXX ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZELNAXX.
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
!     This routine tests the private SPICELIB routine ZZELNAXX.
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
!-    TSPICE Version 1.0.0, 19-MAY-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

!
!     Local Variables
!
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      EPS
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      M
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      X
      DOUBLE PRECISION      XX
      DOUBLE PRECISION      XXPT
      DOUBLE PRECISION      XY
      DOUBLE PRECISION      Y
      DOUBLE PRECISION      YXPT

!
!     Saved values
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZELNAXX' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Oblate case, LAT = pi/6' )

      A   = 2.D0
      B   = 1.D0
      LAT = PI()/6

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     M is the slope of the normal vector at (x,y).
!     This vector is the gradient of
!
!                   2   2      2
!        f(x,y) =  x / a   +  y / b
!
!
      M = TAN( LAT )

!
!     Let T = (y/x) (the tangent of the latitudinal latitude)
!
      T = (B**2)/(A**2) * M

!
!     Since
!
!        y = T*x
!
!     we have
!
!         2   2       2 2   2
!        x / a    +  T x / b    =   1
!
!     So
!
!         2        2     2   2  -1       2    2 2
!        x  = ( 1/a  +  T / b  )  ,     y  = T x
!
!     Both x, y are >= 0.
!
!
!     XX is the expected X-axis intercept, which is
!
!                 2 2
!         ( 1 - (b/a ) ) * x
!
!
      X   = SQRT( 1.D0 / ( (1/A**2) + (T**2/B**2) ) )

      Y   = SQRT( (T**2) * (X**2) )

      XX  = ( 1.D0 - (B**2/A**2) ) * X


      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )

!
!     XY is the expected Y-axis intercept, which is
!
!                2   2
!        ( 1 - (a / b ) ) y
!

      XY  = ( 1.D0 - (A**2/B**2) ) * Y

      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Oblate case, LAT = pi/3' )

      A   = 2.D0
      B   = 1.D0
      LAT = PI()/3

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     See the pi/6 case for a discussion of what follows.
!
      M = TAN( LAT )
!
      T = (B**2)/(A**2) * M

      X   = SQRT( 1.D0 / ( (1/A**2) + (T**2/B**2) ) )

      Y   = SQRT( (T**2) * (X**2) )

      XX  = ( 1.D0 - (B**2/A**2) ) * X


      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )

      XY  = ( 1.D0 - (A**2/B**2) ) * Y

      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Oblate case, LAT << 1' )

      A   = 2.D0
      B   = 1.D0
      LAT = 1.D-13

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     See the pi/3 case for a discussion of what follows.
!
      M = TAN( LAT )
!
      T = (B**2)/(A**2) * M

      X   = SQRT( 1.D0 / ( (1/A**2) + (T**2/B**2) ) )

      Y   = SQRT( (T**2) * (X**2) )

      XX  = ( 1.D0 - (B**2/A**2) ) * X


      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )

      XY  = ( 1.D0 - (A**2/B**2) ) * Y

      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Oblate case, LAT = 0' )

      A   = 2.D0
      B   = 1.D0
      LAT = 0.D0

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     See the pi/3 case for a discussion of what follows.
!
      M = TAN( LAT )
!
      T = (B**2)/(A**2) * M

      X   = SQRT( 1.D0 / ( (1/A**2) + (T**2/B**2) ) )

      Y   = SQRT( (T**2) * (X**2) )

      XX  = ( 1.D0 - (B**2/A**2) ) * X


      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )

      XY  = ( 1.D0 - (A**2/B**2) ) * Y

      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Oblate case, LAT = pi/2' )

      A   = 2.D0
      B   = 1.D0
      LAT = PI()/2

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     See the pi/3 case for a discussion of what follows.
!
      X   = 0.D0

      Y   = B

      XX  = ( 1.D0 - (B**2/A**2) ) * X


      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )

      XY  = ( 1.D0 - (A**2/B**2) ) * Y

      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Oblate case, LAT = (pi/2) - EPS, EPS << 1' )

      A   = 2.D0
      B   = 1.D0
      EPS = 1.D-13
      LAT = ( PI()/2 ) - EPS

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     In principle, tan(LAT) can be very large. We want to
!     work with cot(LAT) and x/y instead. Let T = x/y.
!
!                     2   2
!        tan(LAT) = (a / b ) * y/x
!
!     so
!
!              2   2
!        T = (a / b ) * cot(LAT)
!
!
      T = ( (A**2)/(B**2) ) * ( COS(LAT)/SIN(LAT) )

!
!     x = T*y, so
!
!              2        2
!        (Ty/a)  + (y/b)  = 1
!
!     Then
!
!          2      2  2       2  -1
!         y  = ( T /a  +  1/b  )
!
!
!
      Y   = SQRT(  1.D0 / ( (T**2/A**2) + (1.D0/B**2) )  )

      X   = T * Y

      XX  = ( 1.D0 - (B**2/A**2) ) * X

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )

      XY  = ( 1.D0 - (A**2/B**2) ) * Y

      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Prolate case, LAT = pi/6' )

      A   = 1.D0
      B   = 2.D0
      LAT = PI()/6

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The prolate geometry is just a reflection of an oblate case
!     in which the latitude is complemented and the axis lengths
!     are swapped. The X intercept in the reflected case is the
!     Y intercept of the original case; the relation is the same
!     for the other intercepts.
!
      CALL ZZELNAXX ( B, A, (PI()/2)-LAT, XY, XX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )
      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Prolate case, LAT = pi/3' )

      A   = 1.D0
      B   = 2.D0
      LAT = PI()/3

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELNAXX ( B, A, (PI()/2)-LAT, XY, XX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )
      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Prolate case, LAT << 1' )


      A   = 1.D0
      B   = 2.D0
      EPS = 1.D-13
      LAT = EPS

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELNAXX ( B, A, (PI()/2)-LAT, XY, XX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )
      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Prolate case, LAT = 0' )


      A   = 1.D0
      B   = 2.D0
      LAT = 0.D0

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELNAXX ( B, A, (PI()/2)-LAT, XY, XX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )
      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Prolate case, LAT = pi/2' )


      A   = 1.D0
      B   = 2.D0
      LAT = PI()/2

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELNAXX ( B, A, (PI()/2)-LAT, XY, XX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )
      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Prolate case, LAT = (pi/2) - EPS, EPS << 1' )


      A   = 1.D0
      B   = 2.D0
      EPS = 1.D-13
      LAT = ( PI()/2 ) - EPS

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELNAXX ( B, A, (PI()/2)-LAT, XY, XX )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'X intercept', XXPT, '~', XX, TOL, OK )
      CALL CHCKSD ( 'Y intercept', YXPT, '~', XY, TOL, OK )



!***********************************************************************
!
!     Error cases
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid axis length.' )


      A   = 0.D0
      B   = 1.D0
      LAT = PI()/2

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEAXIS)', OK )

      A   = -1.D0

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEAXIS)', OK )


      A   = 1.D0
      B   = 0.D0

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEAXIS)', OK )

      B   = -1.D0

      CALL ZZELNAXX ( A, B, LAT, XXPT, YXPT )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVEAXIS)', OK )

!
! ---------------------------------------------------------
!

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


