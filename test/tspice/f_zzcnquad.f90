!$Procedure F_ZZCNQUAD ( ZZCNQUAD tests )

      SUBROUTINE F_ZZCNQUAD ( OK )

! $ Abstract
!
!     Exercise the cone intercept quadratic solution routine ZZCNQUAD.
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
!     This routine tests the cone intercept quadratic solution
!     routine ZZCNQUAD.
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
!-    TSPICE Version 1.0.0, 07-FEB-2017 (NJB)
!
!        Previous version 23-SEP-2014 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      TOUCHD

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
      DOUBLE PRECISION      C1 ( 2 )
      DOUBLE PRECISION      C2 ( 2 )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      UB
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XR1
      DOUBLE PRECISION      XR2
      DOUBLE PRECISION      XVAL1
      DOUBLE PRECISION      XVAL2

      INTEGER               N
      INTEGER               XN

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
      CALL TOPEN ( 'F_ZZCNQUAD' )




!**********************************************************************
!
!     Error cases
!
!**********************************************************************




!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Bound has absolute values that are too large.'
      CALL TCASE ( TITLE )

      A  =   1.D0
      B  =   1.D0
      C  =   1.D0
      UB =   1.D160

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      UB =   -1.D160

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = -4.D0
      XR2 = -4.D0

      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '~/', XR2, TOL, OK )

!
!     Make sure the root of larger magnitude is the second
!     one.
!
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )


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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = -1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = -2
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0

      CALL CHCKSD ( 'R1', R1, '=', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=', XR2, TOL, OK )



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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 2
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 1.D0
      XR2 = 2.D0

      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '~/', XR2, TOL, OK )

!
!     Make sure the root of larger magnitude is the second
!     one.
!
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 2
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 =    1.D0
      XR2 = -200.D0

      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '~/', XR2, TOL, OK )

!
!     Make sure the root of larger magnitude is the second
!     one.
!
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 1.D0
      XR2 = 0.D0

      TOL = TIGHT
      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL, OK )

      TOL = 0.D0
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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 1.D70
      XR2 = 0.D0

      TOL = TIGHT

      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL,  OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation with one small real root, one root ' &
      //       'having magnitude near 1. ' &
      //       'The upper bound excludes the larger root. ' &
      //       'The quadratic and zero-order coefficients ' &
      //       'are small; the linear coefficient has ' &
      //       'magnitude near 1.'

      CALL TCASE ( TITLE )

!
!     The equation is
!
!        ( 1.D-5 X - 1.D-10 ) * ( 1.D-5 X - 1.D5 ) = 0
!
!     or
!
!                2
!        1.D-10 X  -(1+1.D-15) X + 1.D-5 =  0
!
!
!     Use an upper bound of 1.D9
!
      A  =   1.D-10
      B  =   - ( 1.D0 + 1.D-15 )
      C  =   1.D-5
      UB =   1.D9

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 1.D-5
      XR2 = 0.D0

      TOL = TIGHT

      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL,  OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, 0.D0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation with one small real root, one root ' &
      //       'having magnitude near 1. ' &
      //       'The upper bound excludes both roots. ' &
      //       'The quadratic and zero-order coefficients ' &
      //       'are small; the linear coefficient has ' &
      //       'magnitude near 1.'

      CALL TCASE ( TITLE )

!
!     The equation is
!
!        ( 1.D-5 X - 1.D-10 ) * ( 1.D-5 X - 1.D5 ) = 0
!
!     or
!
!                2
!        1.D-10 X  -(1+1.D-15) X + 1.D-5 =  0
!
!
!     Use an upper bound of 1.D9
!
      A  =   1.D-10
      B  =   - ( 1.D0 + 1.D-15 )
      C  =   1.D-5
      UB =   1.D-6

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = TIGHT

      CALL CHCKSD ( 'R1', R1, '~/', XR1, TOL,  OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Reciprocal solution case: negative discriminant.'

      CALL TCASE ( TITLE )

      A  =   1.D-10
      B  =   1.D0
      C  =   1.D11
      UB =   1.D9

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Reciprocal solution case: zero discriminant. ' &
      //       'Root has multiplicity 2. Root magnitude is ' &
      //       'less than bound.'

      CALL TCASE ( TITLE )

      A  =   2.D0 ** (-37.D0 )
      B  =   1.D0
      C  =   2.D0 **  35.D0
      UB =   1.D15

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 1
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Reciprocal solution case: zero discriminant. ' &
      //       'Root has multiplicity 2. Root is too large ' &
      //       'to accept.'

      CALL TCASE ( TITLE )

      A  =   2.D0 ** (-37.D0 )
      B  =   1.D0
      C  =   2.D0 **  35.D0
      UB =   1.D9

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )





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

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XN  = 0
      CALL CHCKSI ( 'N',  N,  '=', XN,  0, OK )

      XR1 = 0.D0
      XR2 = 0.D0

      TOL = 0.D0

      CALL CHCKSD ( 'R1', R1, '=',  XR1, TOL, OK )
      CALL CHCKSD ( 'R2', R2, '=',  XR2, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation has very large B compared to both A and C.'
      CALL TCASE ( TITLE )


      A  =   3.D0
      B  =   4.D11
      C  =   5.D2
      UB =   1.D20

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )


      F  = TOUCHD( A * R1**2  +  B * R1  +  C )

      TOL = 1.D-6

      CALL CHCKSD ( 'F(R1)', F, '~', 0.D0, TOL, OK )

!      WRITE (*,*) 'f(R1): ', F

!
!     Compare R1 and R2 to results from RQUAD:
!
      CALL RQUAD ( A, B, C, C1, C2 )

      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', C1(1), TOL, OK )

      CALL CHCKSD ( 'R2', R2, '~/', C2(1), TOL, OK )


!
!     Make sure the root of larger magnitude is the second
!     one.
!
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )




!     F  = TOUCHD( A * R2**2  +  B * R2  +  C )

!      WRITE (*,*) 'f(R2): ', F

!      WRITE (*,*) 'C1 = ', C1
!      WRITE (*,*) 'C2 = ', C2

!      WRITE (*,*) 'C1(1) - R1 = ', C1(1) - R1
!      WRITE (*,*) 'C2(1) - R2 = ', C2(1) - R2




!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation has very large B compared to both A and C; ' &
      //       'B is negative.'
      CALL TCASE ( TITLE )


      A  =    3.D0
      B  =   -4.D11
      C  =    5.D2
      UB =   1.D20

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )


      F  = TOUCHD( A * R1**2  +  B * R1  +  C )

      TOL = 1.D-6

      CALL CHCKSD ( 'F(R1)', F, '~', 0.D0, TOL, OK )

!      WRITE (*,*) 'f(R1): ', F

!
!     Compare R1 and R2 to results from RQUAD:
!
      CALL RQUAD ( A, B, C, C1, C2 )

      TOL = VTIGHT

      IF (  ABS( C1(1) ) .LT. ABS( C2(1) )  ) THEN

         XVAL1 = C1(1)
         XVAL2 = C2(1)

      ELSE

         XVAL2 = C1(1)
         XVAL1 = C2(1)

      END IF

      CALL CHCKSD ( 'R1', R1, '~/', XVAL1, TOL, OK )

      CALL CHCKSD ( 'R2', R2, '~/', XVAL2, TOL, OK )

!
!     Make sure the root of larger magnitude is the second
!     one.
!
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )



      F  = TOUCHD( A * R2**2  +  B * R2  +  C )

!      WRITE (*,*) 'f(R2): ', F

!      WRITE (*,*) 'C1 = ', C1
!      WRITE (*,*) 'C2 = ', C2

!      WRITE (*,*) 'C1(1) - R1 = ', C1(1) - R1
!      WRITE (*,*) 'C2(1) - R2 = ', C2(1) - R2



!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation has very large B compared to A; ' &
      //       'C has the same scale as B.'
      CALL TCASE ( TITLE )

      A  =   2.D0
      B  =   3.D12
      C  =   4.D12
      UB =   1.D40

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

!
!     Compare R1 and R2 to results from RQUAD:
!
      CALL RQUAD ( A, B, C, C1, C2 )

      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', C1(1), TOL, OK )

      CALL CHCKSD ( 'R2', R2, '~/', C2(1), TOL, OK )


!
!     Make sure the root of larger magnitude is the second
!     one.
!
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Equation has very large C compared to A,B.'

      CALL TCASE ( TITLE )

!
!     This is a case that led to severe loss of precision
!     in ZZBQUAD:
!
      A =  -1.11022302462515654D-016
      B =   518.74106559784411D0
      C =  -920626.76641559054D0

      CALL ZZCNQUAD ( A, B, C, UB, N, R1, R2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )


      F  = TOUCHD( A * R1**2  +  B * R1  +  C )

      TOL = 1.D-6

      CALL CHCKSD ( 'F(R1)', F, '~', 0.D0, TOL, OK )


!
!     Compare R1 and R2 to results from RQUAD:
!
      CALL RQUAD ( A, B, C, C1, C2 )

      TOL = VTIGHT

      CALL CHCKSD ( 'R1', R1, '~/', C1(1), TOL, OK )

      CALL CHCKSD ( 'R2', R2, '~/', C2(1), TOL, OK )

!
!     Make sure the root of larger magnitude is the second
!     one.
!
      CALL CHCKSD ( '|R1|', ABS(R1), '<=', ABS(R2), TOL, OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


