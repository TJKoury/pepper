C$Procedure F_POLYDS ( POLYDS tests )

      SUBROUTINE F_POLYDS ( OK )

C$ Abstract
C
C     Exercise the SPICELIB routine POLYDS.
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the routine POLYDS. The tests performed check
C
C     1) That the routine correctly computes the value of a polynomial
C        and it's first n derivatives for data sets plucked
C        from known polynomials.
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
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 23-MAR-2020 (JDR)
C
C-&


C$ Index_Entries
C
C     test POLYDS routine
C
C-&


C
C     Test Utility Functions
C

C
C     SPICELIB Functions
C

C
C     Local Parameters
C
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 15 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )


C
C     Local Variables
C
      DOUBLE PRECISION      COEFFS ( 0:MAXDEG )
      DOUBLE PRECISION      P      ( 0:MAXDEG )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      X

      INTEGER               DEG
      INTEGER               I
      INTEGER               NDERIV

C
C     Function for (a particular) polynomial:
C
C                              2    3        4    5    6
C        f(x) = 1 + 3*x + 0.5*x  + x  + 0.5*x  - x  + x
C
      DOUBLE PRECISION      F
      F ( X ) = 1.D0 + 3.D0*X + 0.5D0*X**2 + X**3
     .        + 0.5D0*X**4 - X**5 + X**6

C
C     Function for (a particular) polynomial first derivative:
C
      DOUBLE PRECISION      DF1
      DF1 ( X ) = 3.D0 + X + 3.D0*X**2 + 2.D0*X**3
     .          - 5.D0*X**4 + 6.D0*X**5

C
C     Function for (a particular) polynomial second derivative:
C
      DOUBLE PRECISION      DF2
      DF2 ( X ) = 1.D0 + 6.D0*X + 6.D0*X**2 - 20.D0*X**3 + 30.D0*X**4

C
C     Function for (a particular) polynomial third derivative:
C
      DOUBLE PRECISION      DF3
      DF3 ( X ) = 6.D0 + 12.D0*X - 60.D0*X**2 + 120.D0*X**3


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_POLYDS' )


C
C     *****************************************************************
C
C        Normal cases: POLYDS
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     For the polynomial
C
C                              2    3        4    5    6
C        f(x) = 1 + 3*x + 0.5*x  + x  + 0.5*x  - x  + x
C
C     calculate the value of the polynomial and its first 3
C     derivatives at input COEFF values.
C
      CALL TCASE ( 'POLYDS normal case #1: Calculate the value of ' //
     .             'the polynomial and its first 3 derivatives. '   //
     .             'Evaluate at input COEFF values.'               )

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

C
C     --- Case: -------------------------------------------------------
C
C     For the polynomial
C
C                              2    3        4    5    6
C        f(x) = 1 + 3*x + 0.5*x  + x  + 0.5*x  - x  + x
C
C     calculate the value of the polynomial and its first 3
C     derivatives at input T = 0.3D0.
C
      CALL TCASE ( 'POLYDS normal case #2: Calculate the value of ' //
     .             'the polynomial and its first 3 derivatives. '   //
     .             'Evaluate at input T = 0.3D0.'                  )

      T = 0.3D0

      CALL POLYDS ( COEFFS, DEG, NDERIV, T, P )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'f(0.3)',   P(0), '~',  F(T),   TIGHT, OK )
      CALL CHCKSD ( 'df1(0.3)', P(1), '~',  DF1(T), TIGHT, OK )
      CALL CHCKSD ( 'df2(0.3)', P(2), '~',  DF2(T), TIGHT, OK )
      CALL CHCKSD ( 'df3(0.3)', P(3), '~',  DF3(T), TIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C
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

C
C     --- Case: -------------------------------------------------------
C
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

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'POLYDS exceptional case #1: NDERIV less than ' //
     .             'zero.'  )

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

C
C     In this case, the routine is expected to return without
C     doing anything. The values that were set in P should be
C     not modified.
C
      CALL CHCKSD ( 'f(0.3)',   P(0), '=', 10.D0, TIGHT, OK )
      CALL CHCKSD ( 'df1(0.3)', P(1), '=',  7.D0, TIGHT, OK )
      CALL CHCKSD ( 'df2(0.3)', P(2), '=', -2.D0, TIGHT, OK )
      CALL CHCKSD ( 'df3(0.3)', P(3), '=', 13.D0, TIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'POLYDS exceptional case #2: DEG less than ' //
     .             'zero.'  )

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

C
C     In this case, the routine is expected to return but the
C     array P should be initialized to zeros.
C
      CALL CHCKSD ( 'f(0.3)',   P(0), '=', 0.D0, TIGHT, OK )
      CALL CHCKSD ( 'df1(0.3)', P(1), '=', 0.D0, TIGHT, OK )
      CALL CHCKSD ( 'df2(0.3)', P(2), '=', 0.D0, TIGHT, OK )
      CALL CHCKSD ( 'df3(0.3)', P(3), '=', 0.D0, TIGHT, OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
