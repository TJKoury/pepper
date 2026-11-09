!$Procedure F_POLYDS ( POLYDS tests )

      SUBROUTINE F_POLYDS ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine POLYDS.
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
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the routine POLYDS. The tests performed check
!
!     1) That the routine correctly computes the value of a polynomial
!        and it's first n derivatives for data sets plucked
!        from known polynomials.
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
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 23-MAR-2020 (JDR)
!
!-&


! $ Index_Entries
!
!     test POLYDS routine
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


!
!     Local Variables
!
      DOUBLE PRECISION      COEFFS ( 0:MAXDEG )
      DOUBLE PRECISION      P      ( 0:MAXDEG )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      X

      INTEGER               DEG
      INTEGER               I
      INTEGER               NDERIV

!
!     Function for (a particular) polynomial:
!
!                              2    3        4    5    6
!        f(x) = 1 + 3*x + 0.5*x  + x  + 0.5*x  - x  + x
!
      DOUBLE PRECISION      F
      F ( X ) = 1.D0 + 3.D0*X + 0.5D0*X**2 + X**3 &
              + 0.5D0*X**4 - X**5 + X**6

!
!     Function for (a particular) polynomial first derivative:
!
      DOUBLE PRECISION      DF1
      DF1 ( X ) = 3.D0 + X + 3.D0*X**2 + 2.D0*X**3 &
                - 5.D0*X**4 + 6.D0*X**5

!
!     Function for (a particular) polynomial second derivative:
!
      DOUBLE PRECISION      DF2
      DF2 ( X ) = 1.D0 + 6.D0*X + 6.D0*X**2 - 20.D0*X**3 + 30.D0*X**4

!
!     Function for (a particular) polynomial third derivative:
!
      DOUBLE PRECISION      DF3
      DF3 ( X ) = 6.D0 + 12.D0*X - 60.D0*X**2 + 120.D0*X**3


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_POLYDS' )


!
!     *****************************************************************
!
!        Normal cases: POLYDS
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     For the polynomial
!
!                              2    3        4    5    6
!        f(x) = 1 + 3*x + 0.5*x  + x  + 0.5*x  - x  + x
!
!     calculate the value of the polynomial and its first 3
!     derivatives at input COEFF values.
!
      CALL TCASE ( 'POLYDS normal case #1: Calculate the value of ' // &
                   'the polynomial and its first 3 derivatives. '   // &
                   'Evaluate at input COEFF values.'               )

      COEFFS(0) =  1.D0
      COEFFS(1) =  3.D0
      COEFFS(2) =  0.5D0
      COEFFS(3) =  1.D0
      COEFFS(4) =  0.5D0
      COEFFS(5) = -1.D0
      COEFFS(6) =  1.D0

      DEG       =  6
      NDERIV    =  3

      DO I = 0, DEG

         T = COEFFS(I)

         CALL POLYDS ( COEFFS, DEG, NDERIV, T, P )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSD ( 'f(coeffs(i))',   P(0), '~', F(T),   TIGHT, OK )
         CALL CHCKSD ( 'df1(coeffs(i))', P(1), '~', DF1(T), TIGHT, OK )
         CALL CHCKSD ( 'df2(coeffs(i))', P(2), '~', DF2(T), TIGHT, OK )
         CALL CHCKSD ( 'df3(coeffs(i))', P(3), '~', DF3(T), TIGHT, OK )

      END DO

!
!     --- Case: -------------------------------------------------------
!
!     For the polynomial
!
!                              2    3        4    5    6
!        f(x) = 1 + 3*x + 0.5*x  + x  + 0.5*x  - x  + x
!
!     calculate the value of the polynomial and its first 3
!     derivatives at input T = 0.3D0.
!
      CALL TCASE ( 'POLYDS normal case #2: Calculate the value of ' // &
                   'the polynomial and its first 3 derivatives. '   // &
                   'Evaluate at input T = 0.3D0.'                  )

      T = 0.3D0

      CALL POLYDS ( COEFFS, DEG, NDERIV, T, P )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'f(0.3)',   P(0), '~',  F(T),   TIGHT, OK )
      CALL CHCKSD ( 'df1(0.3)', P(1), '~',  DF1(T), TIGHT, OK )
      CALL CHCKSD ( 'df2(0.3)', P(2), '~',  DF2(T), TIGHT, OK )
      CALL CHCKSD ( 'df3(0.3)', P(3), '~',  DF3(T), TIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'POLYDS normal case #3: linear polynomial.'  )

      COEFFS(0) =  1.D0
      COEFFS(1) =  3.D0
      DEG       =  1
      NDERIV    =  3

      T = 0.3D0

      CALL POLYDS ( COEFFS, DEG, NDERIV, T, P )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'f(0.3)',   P(0), '=', 1.9D0, TIGHT, OK )
      CALL CHCKSD ( 'df1(0.3)', P(1), '=', 3.D0,  TIGHT, OK )
      CALL CHCKSD ( 'df2(0.3)', P(2), '=', 0.D0,  TIGHT, OK )
      CALL CHCKSD ( 'df3(0.3)', P(3), '=', 0.D0,  TIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'POLYDS normal case #4: constant polynomial.'  )

      COEFFS(0) =  1.D0
      DEG       =  0
      NDERIV    =  3

      T = 0.3D0

      CALL POLYDS ( COEFFS, DEG, NDERIV, T, P )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'f(0.3)',   P(0), '=', 1.D0, TIGHT, OK )
      CALL CHCKSD ( 'df1(0.3)', P(1), '=', 0.D0, TIGHT, OK )
      CALL CHCKSD ( 'df2(0.3)', P(2), '=', 0.D0, TIGHT, OK )
      CALL CHCKSD ( 'df3(0.3)', P(3), '=', 0.D0, TIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'POLYDS exceptional case #1: NDERIV less than ' // &
                   'zero.'  )

      COEFFS(0) =  1.D0
      DEG       =  0
      NDERIV    =  -1
      P(0)      =  10.D0
      P(1)      =   7.D0
      P(2)      =  -2.D0
      P(3)      =  13.D0

      T = 0.3D0

      CALL POLYDS ( COEFFS, DEG, NDERIV, T, P )
      CALL CHCKXC ( .FALSE., ' ',  OK )

!
!     In this case, the routine is expected to return without
!     doing anything. The values that were set in P should be
!     not modified.
!
      CALL CHCKSD ( 'f(0.3)',   P(0), '=', 10.D0, TIGHT, OK )
      CALL CHCKSD ( 'df1(0.3)', P(1), '=',  7.D0, TIGHT, OK )
      CALL CHCKSD ( 'df2(0.3)', P(2), '=', -2.D0, TIGHT, OK )
      CALL CHCKSD ( 'df3(0.3)', P(3), '=', 13.D0, TIGHT, OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'POLYDS exceptional case #2: DEG less than ' // &
                   'zero.'  )

      COEFFS(0) =  1.D0
      DEG       =  -1
      NDERIV    =  3
      P(0)      =  10.D0
      P(1)      =   7.D0
      P(2)      =  -2.D0
      P(3)      =  13.D0

      T = 0.3D0

      CALL POLYDS ( COEFFS, DEG, NDERIV, T, P )
      CALL CHCKXC ( .FALSE., ' ',  OK )

!
!     In this case, the routine is expected to return but the
!     array P should be initialized to zeros.
!
      CALL CHCKSD ( 'f(0.3)',   P(0), '=', 0.D0, TIGHT, OK )
      CALL CHCKSD ( 'df1(0.3)', P(1), '=', 0.D0, TIGHT, OK )
      CALL CHCKSD ( 'df2(0.3)', P(2), '=', 0.D0, TIGHT, OK )
      CALL CHCKSD ( 'df3(0.3)', P(3), '=', 0.D0, TIGHT, OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
