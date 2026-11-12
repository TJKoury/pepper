!$Procedure F_LAGRNG ( Lagrange interpolation )

      SUBROUTINE F_LAGRNG ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the Lagrange polynomial interpolation routines LGRESP,
!     LGRIND and LGRINT.
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the routines LGRESP, LGRIND and LGRINT.
!     The tests performed check
!
!     1) That the routines correctly interpolate data sets plucked
!        from known polynomials.
!
!     2) That all three interpolators perform error handling as
!        specified.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TSPICE Version 2.0.0, 19-JUL-2021 (JDR)
!
!        Added test cases for LGRESP and LGRINT routines.
!
!        Updated the header and comments to comply to NAIF standard.
!
!-    TSPICE Version 1.0.0, 28-AUG-2002 (NJB)
!
!-&


! $ Index_Entries
!
!     test Lagrange polynomial interpolation routines
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!

!
!     Local Parameters
!
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 15 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-12 )

!
!     Local Variables
!
      DOUBLE PRECISION      EVAL
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      P
      DOUBLE PRECISION      DP
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      WORK    ( MAXDEG, 2 )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      XVALS   ( MAXDEG )
      DOUBLE PRECISION      YVALS   ( MAXDEG )

      INTEGER               I
      INTEGER               N

!
!     Statement function for (a particular) polynomial derivative:
!
      DOUBLE PRECISION      DF
      DF ( X ) = 3.D0*X**2 + 4.D0*X - 4.D0

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_LAGRNG' )


!
!     *****************************************************************
!
!     Normal cases: LGRESP
!
!     *****************************************************************
!
      N         =   4
      FIRST     =  -1.D0
      STEP      =   2.D0

      YVALS(1)  =  -2.D0
      YVALS(2)  =  -8.D0
      YVALS(3)  =  26.D0
      YVALS(4)  = 148.D0

!
!     --- Case: -------------------------------------------------------
!
!        The unique cubic polynomial that fits these points is
!
!                   3      2
!           f(x) = x  + 2*x  - 4*x - 7
!
      CALL TCASE ( 'LGRESP normal case #1: f(x) = x^3 + 2*x^2 '  // &
                   '-4*x - 7.  Evaluate at input x=2.'           )

      X    = 2.D0
      EVAL = LGRESP (  N, FIRST, STEP, YVALS, WORK, X )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'EVAL', EVAL,  '~/', 1.D0, TIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
!        The unique cubic polynomial that fits these points is
!
!                   3      2
!           f(x) = x  + 2*x  - 4*x - 7
!
      CALL TCASE ( 'LGRESP normal case #2: f(x) = x^3 + 2*x^2 '      // &
                   '-4*x - 7.  Evaluate at FIRST + N * STEP values.' )

      DO I = 1, N

         X = FIRST + (I-1) * STEP

         EVAL = LGRESP (  N, FIRST, STEP, YVALS, WORK, X )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSD ( 'EVAL', EVAL, '~/', YVALS(I), TIGHT, OK )

      END DO

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRESP normal case #3: linear polynomial.'  )

      N         =   2

      FIRST     =  -1.D0
      STEP      =   1.D0

      YVALS(1)  =  -2.D0
      YVALS(2)  =  -7.D0

      EVAL = LGRESP (  N, FIRST, STEP, YVALS, WORK, 2.D0 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'Y(2)', EVAL,  '~/', -1.7D1, TIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRESP normal case #4: constant polynomial.'  )

      N         =   1

      FIRST     =  -1.D0
      STEP      =   3.D0

      YVALS(1)  =  -2.D0

      EVAL = LGRESP (  N, FIRST, STEP, YVALS, WORK, 2.D0 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'Y(2)', EVAL, '~/', -2.D0, TIGHT, OK )

!
!     *****************************************************************
!
!     Normal cases: LGRIND
!
!     *****************************************************************
!
      N         =   4

      XVALS(1)  =  -1
      XVALS(2)  =   0
      XVALS(3)  =   1
      XVALS(4)  =   3

      YVALS(1)  =  -2
      YVALS(2)  =  -7
      YVALS(3)  =  -8
      YVALS(4)  =  26

!
!     --- Case: -------------------------------------------------------
!
!
!        The unique cubic polynomial that fits these points is
!
!                   3      2
!           f(x) = x  + 2*x  - 4*x - 7
!
!
!        The derivative of f(x) is
!
!            '         2
!           f (x) = 3*x  + 4*x - 4
!
      CALL TCASE ( 'LGRIND normal case #1: f(x) = x^3 + 2*x^2 ' // &
                   '-4*x - 7.  Evaluate at input x values.'    )


      DO I = 1, N

         X = XVALS(I)

         CALL LGRIND ( N, XVALS, YVALS, WORK, X, P, DP )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSD ( 'y(xvals(i))',   P,  '~/', YVALS(I), TIGHT, OK )
         CALL CHCKSD ( 'y''(xvals(i))', DP, '~',  DF(X),    TIGHT, OK )

      END DO

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRIND normal case #2: f(x) = x^3 + 2*x^2 '  // &
                   '-4*x - 7.  Evaluate at x = 2.'              )


      CALL LGRIND ( N, XVALS, YVALS, WORK, 2.D0, P, DP )
      CALL CHCKXC ( .FALSE., ' ',  OK )

!
!        The returned value of P should be 1.D0.
!
!        The returned value of DP should be 1.6D1.
!
      CALL CHCKSD ( 'y(2)',   P,  '~/', 1.0D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(2)', DP, '~',  1.6D1, TIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRIND normal case #3: linear polynomial.'  )


      N         =   2

      XVALS(1)  =  -1
      XVALS(2)  =   0

      YVALS(1)  =  -2
      YVALS(2)  =  -7

      CALL LGRIND ( N, XVALS, YVALS, WORK, 2.D0, P, DP )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'y(2)',   P,  '~/', -1.7D1, TIGHT, OK )
      CALL CHCKSD ( 'y''(2)', DP, '~',   -5.D0, TIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRIND normal case #4: constant polynomial.'  )


      N         =   1

      XVALS(1)  =  -1

      YVALS(1)  =  -2

      CALL LGRIND ( N, XVALS, YVALS, WORK, 2.D0, P, DP )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'y(2)',   P,  '~/', -2.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(2)', DP, '~',   0.D0, TIGHT, OK )

!
!     *****************************************************************
!
!     Normal cases: LGRINT
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!
!        The unique cubic polynomial that fits these points is
!
!                   3      2
!           f(x) = x  + 2*x  - 4*x - 7
!
      CALL TCASE ( 'LGRINT normal case #1: f(x) = x^3 + 2*x^2 ' // &
                   '-4*x - 7. Evaluate at input x values.'     )

      N         =   4

      XVALS(1)  =  -1
      XVALS(2)  =   0
      XVALS(3)  =   1
      XVALS(4)  =   3

      YVALS(1)  =  -2
      YVALS(2)  =  -7
      YVALS(3)  =  -8
      YVALS(4)  =  26

      DO I = 1, N

         X = XVALS(I)

         EVAL = LGRINT ( N, XVALS, YVALS, WORK, X )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSD ( 'y(xvals(i))', EVAL, '~/', YVALS(I), TIGHT, OK )

      END DO

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRINT normal case #2: f(x) = x^3 + 2*x^2 ' // &
                   '-4*x - 7. Evaluate at x = 2.'              )

      EVAL = LGRINT ( N, XVALS, YVALS, WORK, 2.D0 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

!
!        The returned value of P should be 1.D0.
!
      CALL CHCKSD ( 'y(2)', EVAL, '~/', 1.0D0, TIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRINT normal case #3: linear polynomial.'  )

      N         =   2

      XVALS(1)  =  -1
      XVALS(2)  =   0

      YVALS(1)  =  -2
      YVALS(2)  =  -7

      EVAL = LGRINT ( N, XVALS, YVALS, WORK, 2.D0 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'y(2)', EVAL, '~/', -1.7D1, TIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRINT normal case #4: constant polynomial.'  )

      N         =   1

      XVALS(1)  =  -1

      YVALS(1)  =  -2

      EVAL = LGRINT ( N, XVALS, YVALS, WORK, 2.D0 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'y(2)', EVAL,  '~/', -2.D0, TIGHT, OK )

!     *****************************************************************
!
!     Error handling tests follow.
!
!     *****************************************************************
!
!     LGRESP error cases.
!
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRESP error case:  array size non-positive.' )

      EVAL = LGRESP ( 0, FIRST, STEP, YVALS, WORK, X    )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)',    OK )
      CALL CHCKSD ( 'EVAL', EVAL,  '=', 0.D0, TIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRESP error case:  step zero.' )

      EVAL = LGRESP ( N, FIRST, 0.D0, YVALS, WORK, X     )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEPSIZE)', OK )
      CALL CHCKSD ( 'EVAL', EVAL,  '=', 0.D0, TIGHT,  OK )

!
!     LGRIND error cases.
!
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRIND error case:  equal abscissa values.'  )

      XVALS(2)  =      -1.D0

      CALL LGRIND ( 4, XVALS, YVALS, WORK, 3.D0, P, DP )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRIND error case:  array size non-positive.'  )

      CALL LGRIND ( 0, XVALS, YVALS, WORK, 3.D0, P, DP )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)',  OK  )

!
!     LGRINT error cases.
!
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRINT error case:  equal abscissa values.'  )

      XVALS(2)  =      -1.D0

      EVAL = LGRINT ( 4, XVALS, YVALS, WORK, X )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)',   OK )
      CALL CHCKSD ( 'EVAL', EVAL,  '=', 0.D0, TIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'LGRINT error case:  array size non-positive.'  )

      EVAL = LGRINT ( 0, XVALS, YVALS, WORK, X )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)',    OK )
      CALL CHCKSD ( 'EVAL', EVAL,  '=', 0.D0, TIGHT, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
