!$Procedure F_ZZBQUAD ( ZZBQUAD tests )

      SUBROUTINE F_ZZBQUAD ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the bounded quadratic solution routine ZZBQUAD.
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
!     This routine tests the bounded quadratic solution
!     routine ZZBQUAD.
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
!        Previous version 20-SEP-2014 (NJB)
!
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
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    TITLE


      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      UB
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XR1
      DOUBLE PRECISION      XR2

      INTEGER               N
      INTEGER               NX
      INTEGER               XN
      INTEGER               XNX

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
      CALL TOPEN ( 'F_ZZBQUAD' )




!**********************************************************************
!
!     Error cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Coefficients have absolute values that are too large.'

      CALL TCASE ( TITLE )


      A  =   1.D160
      B  =   1.D0
      C  =   1.D0
      UB =  10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      A  =  -1.D160
      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      A  =   1.D0
      B  =   1.D160
      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      B  =  -1.D160
      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      B  =   1.D0
      C  =   1.D160
      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      B  =   1.D0
      C  =   1.D160
      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      C  =  -1.D160
      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      A  =   1.D160
      B  =   1.D0
      C  =   1.D0
      UB =  10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Non-postive upper bound.'
      CALL TCASE ( TITLE )

      A  =   1.D0
      B  =   1.D0
      C  =  -1.D0

      UB = 0.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      UB = -1.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Bound has absolute values that are too large.'
      CALL TCASE ( TITLE )

      A  =   1.D0
      B  =   1.D0
      C  =   1.D0
      UB =   1.D160

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )





!**********************************************************************
!
!     Non-error exceptions
!
!**********************************************************************



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Complex roots'

      CALL TCASE ( TITLE )


!
!     The equation is
!
!        ( X + 2i ) * ( X - 2i ) = 0
!
!     or
!
!         2
!        X  + 4 = 0
!
!
!     Use an upper bound of 10.
!

      A  =   1.D0
      B  =   0.D0
      C  =   4.D0
      UB =  10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 0
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0

      CALL CHCKSD ( 'R1', R1, '=', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=', XR2, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'First order equation, no roots excluded'

      CALL TCASE ( TITLE )


!
!     The equation is
!
!
!        X + 4 = 0
!
!
!     Use an upper bound of 10.
!

      A  =   0.D0
      B  =   1.D0
      C  =   4.D0
      UB =  10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 0
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = -4.D0
      XR2 = -4.D0

      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '~/', XR2, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'First order equation, both roots excluded'

      CALL TCASE ( TITLE )


!
!     The equation is
!
!
!        X + 4 = 0
!
!
!     Use an upper bound of 4 - 1e-12.
!

      A  =   0.D0
      B  =   1.D0
      C  =   4.D0
      UB =   4.D0 - 1.D-12

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 1
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0

      CALL CHCKSD ( 'R1', R1, '=', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=', XR2, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Degenerate equation; form is 0 = 0. N should ' &
      //       'be set to -1.'

      CALL TCASE ( TITLE )


!
!     The equation is
!
!           2
!        0*X  + 0*X + 0 = 0
!
!
!     Use an upper bound of 10.
!

      A  =   0.D0
      B  =   0.D0
      C  =   0.D0
      UB =   10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = -1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 0
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0

      CALL CHCKSD ( 'R1', R1, '=', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=', XR2, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Degenerate equation; form is 1 = 0. N should ' &
      //       'be set to -2.'

      CALL TCASE ( TITLE )


!
!     The equation is
!
!           2
!        0*X  + 0*X + 1 = 0
!
!
!     Use an upper bound of 10.
!

      A  =   0.D0
      B  =   0.D0
      C  =   1.D0
      UB =   10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = -2
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 0
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0

      CALL CHCKSD ( 'R1', R1, '=', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=', XR2, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Single root of multiplicity 2; root is too ' &
      //       'large to compute.'

      CALL TCASE ( TITLE )

!
!     Set coefficients to produce the root -4, having multiplicity 2.
!
      A  =    4.D0
      B  =   32.D0
      C  =   64.D0
      UB =    3.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 1
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )



!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Trivial case: equation with two small real roots. ' &
      //       'The upper bound does not exclude roots.'

      CALL TCASE ( TITLE )

!
!     The equation is
!
!        ( X - 1 ) * ( X - 2 ) = 0
!
!     or
!
!         2
!        X  - 3X + 2 = 0
!
!
!     Use an upper bound of 10.
!
      A  =   1.D0
      B  =  -3.D0
      C  =   2.D0
      UB =  10.D0

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 2
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 0
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 1.D0
      XR2 = 2.D0

      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '~/', XR2, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation with two small real roots; the root ' &
      //       'of larger magnitude is positive. ' &
      //       'The upper bound excludes the larger root.'

      CALL TCASE ( TITLE )

!
!     The equation is
!
!        ( X - 1 ) * ( X - 200 ) = 0
!
!     or
!
!         2
!        X  - 201X + 200 = 0
!
!
!     Use an upper bound of 200 - 1e-12
!
      A  =     1.D0
      B  =  -201.D0
      C  =   200.D0
      UB =   200.D0 - 1.D-12

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 1
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 1.D0
      XR2 = 0.D0

      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation with two small real roots; the root ' &
      //       'of larger magnitude is positive. ' &
      //       'The upper bound excludes both roots.'

      CALL TCASE ( TITLE )

!
!     Use the equation above but change UB.
!
      UB = 1.D0 - 1.D-12

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 2
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0

      CALL CHCKSD ( 'R1', R1, '=', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=', XR2, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation with two small real roots; the root ' &
      //       'of larger magnitude is negative. ' &
      //       'No roots are excluded.'

      CALL TCASE ( TITLE )

!
!     The equation is
!
!        ( X - 1 ) * ( X + 200 ) = 0
!
!     or
!
!         2
!        X  + 199 X - 200 = 0
!
!
!     Use an upper bound of 200 + 1e-12
!
      A  =     1.D0
      B  =   199.D0
      C  =  -200.D0
      UB =   200.D0 + 1.D-12

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 2
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 0
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 =    1.D0
      XR2 = -200.D0

      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '~/', XR2, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation with two small real roots; the root ' &
      //       'of larger magnitude is negative. ' &
      //       'The upper bound excludes the root of ' &
      //       'larger magnitude.'

      CALL TCASE ( TITLE )

!
!     The equation is
!
!        ( X - 1 ) * ( X + 200 ) = 0
!
!     or
!
!         2
!        X  + 199 X - 200 = 0
!
!
!     Use an upper bound of 200 - 1e-12
!
      A  =     1.D0
      B  =   199.D0
      C  =  -200.D0
      UB =   200.D0 - 1.D-12

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 1
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 1.D0
      XR2 = 0.D0

      TOL = 0.D0

      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation with two small real roots; the root ' &
      //       'of larger magnitude is negative. ' &
      //       'The upper bound excludes both roots.'

      CALL TCASE ( TITLE )

!
!     The equation is
!
!        ( X - 1 ) * ( X + 200 ) = 0
!
!     or
!
!         2
!        X  + 199 X - 200 = 0
!
!
!     Use an upper bound of 1 - 1e-12
!
      A  =     1.D0
      B  =   199.D0
      C  =  -200.D0
      UB =     1.D0 - 1.D-12

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 2
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0

      CALL CHCKSD ( 'R1', R1, '=',  XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation with two large real roots. ' &
      //       'The upper bound excludes the larger root.'

      CALL TCASE ( TITLE )

!
!     The equation is
!
!        ( X - 1.D70 ) * ( X - 2.D70 ) = 0
!
!     or
!
!         2
!        X  - 3.D70 X + 2.D140 = 0
!
!
!     Use an upper bound of 1.5D70
!
      A  =   1.D0
      B  =  -3.D70
      C  =   2.D140
      UB =   1.5D70

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 1
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 1.D70
      XR2 = 0.D0

      TOL = TIGHT

      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL,  OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, 0.D0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation with two large real roots. ' &
      //       'The upper bound excludes both roots.'

      CALL TCASE ( TITLE )

!
!     The equation is
!
!        ( X - 1.D70 ) * ( X - 2.D70 ) = 0
!
!     or
!
!         2
!        X  - 3.D70 X + 2.D140 = 0
!
!
!     Use an upper bound of 1.D70 - 1.D-56
!
      A  =   1.D0
      B  =  -3.D70
      C  =   2.D140
      UB =   1.0D70 - 1.0D56

      CALL ZZBQUAD ( A, B, C, UB, N, NX, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XNX = 2
      CALL CHCKSI ( 'NX', NX, '=', XNX, 0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0

      CALL CHCKSD ( 'R1', R1, '=',  XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, TOL, OK )





!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


