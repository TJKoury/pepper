C$Procedure F_LAGRNG ( Lagrange interpolation )

      SUBROUTINE F_LAGRNG ( OK )

C$ Abstract
C
C     Exercise the Lagrange polynomial interpolation routines LGRESP,
C     LGRIND and LGRINT.
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
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the routines LGRESP, LGRIND and LGRINT.
C     The tests performed check
C
C     1) That the routines correctly interpolate data sets plucked
C        from known polynomials.
C
C     2) That all three interpolators perform error handling as
C        specified.
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TSPICE Version 2.0.0, 19-JUL-2021 (JDR)
C
C        Added test cases for LGRESP and LGRINT routines.
C
C        Updated the header and comments to comply to NAIF standard.
C
C-    TSPICE Version 1.0.0, 28-AUG-2002 (NJB)
C
C-&


C$ Index_Entries
C
C     test Lagrange polynomial interpolation routines
C
C-&


C
C     Test Utility Functions
C


C
C     SPICELIB Functions
C
      DOUBLE PRECISION      LGRESP
      DOUBLE PRECISION      LGRINT

C
C     Local Parameters
C
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 15 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-12 )

C
C     Local Variables
C
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

C
C     Statement function for (a particular) polynomial derivative:
C
      DOUBLE PRECISION      DF
      DF ( X ) = 3.D0*X**2 + 4.D0*X - 4.D0

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_LAGRNG' )


C
C     *****************************************************************
C
C     Normal cases: LGRESP
C
C     *****************************************************************
C
      N         =   4
      FIRST     =  -1.D0
      STEP      =   2.D0

      YVALS(1)  =  -2.D0
      YVALS(2)  =  -8.D0
      YVALS(3)  =  26.D0
      YVALS(4)  = 148.D0

C
C     --- Case: -------------------------------------------------------
C
C        The unique cubic polynomial that fits these points is
C
C                   3      2
C           f(x) = x  + 2*x  - 4*x - 7
C
      CALL TCASE ( 'LGRESP normal case #1: f(x) = x^3 + 2*x^2 '  //
     .             '-4*x - 7.  Evaluate at input x=2.'           )

      X    = 2.D0
      EVAL = LGRESP (  N, FIRST, STEP, YVALS, WORK, X )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'EVAL', EVAL,  '~/', 1.D0, TIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C
C        The unique cubic polynomial that fits these points is
C
C                   3      2
C           f(x) = x  + 2*x  - 4*x - 7
C
      CALL TCASE ( 'LGRESP normal case #2: f(x) = x^3 + 2*x^2 '      //
     .             '-4*x - 7.  Evaluate at FIRST + N * STEP values.' )

      DO I = 1, N

         X = FIRST + (I-1) * STEP

         EVAL = LGRESP (  N, FIRST, STEP, YVALS, WORK, X )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSD ( 'EVAL', EVAL, '~/', YVALS(I), TIGHT, OK )

      END DO

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRESP normal case #3: linear polynomial.'  )

      N         =   2

      FIRST     =  -1.D0
      STEP      =   1.D0

      YVALS(1)  =  -2.D0
      YVALS(2)  =  -7.D0

      EVAL = LGRESP (  N, FIRST, STEP, YVALS, WORK, 2.D0 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'Y(2)', EVAL,  '~/', -1.7D1, TIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRESP normal case #4: constant polynomial.'  )

      N         =   1

      FIRST     =  -1.D0
      STEP      =   3.D0

      YVALS(1)  =  -2.D0

      EVAL = LGRESP (  N, FIRST, STEP, YVALS, WORK, 2.D0 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'Y(2)', EVAL, '~/', -2.D0, TIGHT, OK )

C
C     *****************************************************************
C
C     Normal cases: LGRIND
C
C     *****************************************************************
C
      N         =   4

      XVALS(1)  =  -1
      XVALS(2)  =   0
      XVALS(3)  =   1
      XVALS(4)  =   3

      YVALS(1)  =  -2
      YVALS(2)  =  -7
      YVALS(3)  =  -8
      YVALS(4)  =  26

C
C     --- Case: -------------------------------------------------------
C
C
C        The unique cubic polynomial that fits these points is
C
C                   3      2
C           f(x) = x  + 2*x  - 4*x - 7
C
C
C        The derivative of f(x) is
C
C            '         2
C           f (x) = 3*x  + 4*x - 4
C
      CALL TCASE ( 'LGRIND normal case #1: f(x) = x^3 + 2*x^2 ' //
     .             '-4*x - 7.  Evaluate at input x values.'    )


      DO I = 1, N

         X = XVALS(I)

         CALL LGRIND ( N, XVALS, YVALS, WORK, X, P, DP )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CHCKSD ( 'y(xvals(i))',   P,  '~/', YVALS(I), TIGHT, OK )
         CALL CHCKSD ( 'y''(xvals(i))', DP, '~',  DF(X),    TIGHT, OK )

      END DO

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRIND normal case #2: f(x) = x^3 + 2*x^2 '  //
     .             '-4*x - 7.  Evaluate at x = 2.'              )


      CALL LGRIND ( N, XVALS, YVALS, WORK, 2.D0, P, DP )
      CALL CHCKXC ( .FALSE., ' ',  OK )

C
C        The returned value of P should be 1.D0.
C
C        The returned value of DP should be 1.6D1.
C
      CALL CHCKSD ( 'y(2)',   P,  '~/', 1.0D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(2)', DP, '~',  1.6D1, TIGHT, OK )


C
C     --- Case: -------------------------------------------------------
C
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

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRIND normal case #4: constant polynomial.'  )


      N         =   1

      XVALS(1)  =  -1

      YVALS(1)  =  -2

      CALL LGRIND ( N, XVALS, YVALS, WORK, 2.D0, P, DP )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'y(2)',   P,  '~/', -2.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(2)', DP, '~',   0.D0, TIGHT, OK )

C
C     *****************************************************************
C
C     Normal cases: LGRINT
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C
C        The unique cubic polynomial that fits these points is
C
C                   3      2
C           f(x) = x  + 2*x  - 4*x - 7
C
      CALL TCASE ( 'LGRINT normal case #1: f(x) = x^3 + 2*x^2 ' //
     .             '-4*x - 7. Evaluate at input x values.'     )

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

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRINT normal case #2: f(x) = x^3 + 2*x^2 ' //
     .             '-4*x - 7. Evaluate at x = 2.'              )

      EVAL = LGRINT ( N, XVALS, YVALS, WORK, 2.D0 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

C
C        The returned value of P should be 1.D0.
C
      CALL CHCKSD ( 'y(2)', EVAL, '~/', 1.0D0, TIGHT, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRINT normal case #3: linear polynomial.'  )

      N         =   2

      XVALS(1)  =  -1
      XVALS(2)  =   0

      YVALS(1)  =  -2
      YVALS(2)  =  -7

      EVAL = LGRINT ( N, XVALS, YVALS, WORK, 2.D0 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'y(2)', EVAL, '~/', -1.7D1, TIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRINT normal case #4: constant polynomial.'  )

      N         =   1

      XVALS(1)  =  -1

      YVALS(1)  =  -2

      EVAL = LGRINT ( N, XVALS, YVALS, WORK, 2.D0 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'y(2)', EVAL,  '~/', -2.D0, TIGHT, OK )

C     *****************************************************************
C
C     Error handling tests follow.
C
C     *****************************************************************
C
C     LGRESP error cases.
C
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRESP error case:  array size non-positive.' )

      EVAL = LGRESP ( 0, FIRST, STEP, YVALS, WORK, X    )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)',    OK )
      CALL CHCKSD ( 'EVAL', EVAL,  '=', 0.D0, TIGHT, OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRESP error case:  step zero.' )

      EVAL = LGRESP ( N, FIRST, 0.D0, YVALS, WORK, X     )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEPSIZE)', OK )
      CALL CHCKSD ( 'EVAL', EVAL,  '=', 0.D0, TIGHT,  OK )

C
C     LGRIND error cases.
C
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRIND error case:  equal abscissa values.'  )

      XVALS(2)  =      -1.D0

      CALL LGRIND ( 4, XVALS, YVALS, WORK, 3.D0, P, DP )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)',  OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRIND error case:  array size non-positive.'  )

      CALL LGRIND ( 0, XVALS, YVALS, WORK, 3.D0, P, DP )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)',  OK  )

C
C     LGRINT error cases.
C
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRINT error case:  equal abscissa values.'  )

      XVALS(2)  =      -1.D0

      EVAL = LGRINT ( 4, XVALS, YVALS, WORK, X )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)',   OK )
      CALL CHCKSD ( 'EVAL', EVAL,  '=', 0.D0, TIGHT, OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'LGRINT error case:  array size non-positive.'  )

      EVAL = LGRINT ( 0, XVALS, YVALS, WORK, X )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)',    OK )
      CALL CHCKSD ( 'EVAL', EVAL,  '=', 0.D0, TIGHT, OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
