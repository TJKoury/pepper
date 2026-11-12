!$Procedure F_HRMITE ( Hermite interpolation )

      SUBROUTINE F_HRMITE ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the Hermite interpolation routines HRMINT and HRMESP.
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
!     This routine tests the routines HRMINT and HRMESP.
!     The tests performed check
!
!     1)  That the routines correctly interpolate data sets plucked
!         from known polynomials.
!
!     2)  That both interpolators perform error handling as specified.
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
!-    TSPICE Version 3.0.1, 29-MAY-2021 (JDR)
!
!        Updated header and comments to comply with NAIF standard.
!
!-    TSPICE Version 3.0.0, 09-NOV-2005 (NJB)
!
!        Changed comparison type from relative to absolute for the
!        quantity y'(0) in the HRMINT test case using abscissa points
!        -1000, 0, 1, 1.000001. (Yes, the same case for which the
!        previous update was made.)
!
!-    TSPICE Version 2.2.0, 27-FEB-2003 (NJB)
!
!        Changed tolerance from TIGHT to LOOSE for HRMINT test
!        case using abscissa points -1000, 0, 1, 1.000001.
!
!-    TSPICE Version 2.0.0, 04-SEP-2002 (NJB)
!
!        Test cases for handling of non-standard adjustable
!        array dimension 0 were removed; these tests cause the
!        calling program to crash on VAX environments.
!
!-    TSPICE Version 1.0.0, 01-MAR-2000 (NJB)
!
!-&


! $ Index_Entries
!
!     test Hermite interpolation routines
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

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-11 )

!
!     Local Variables
!
      DOUBLE PRECISION      F
      DOUBLE PRECISION      DF
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      WORK    ( 2*MAXDEG, 2 )
      DOUBLE PRECISION      XVALS   ( MAXDEG )
      DOUBLE PRECISION      YVALS   ( 2*MAXDEG )




!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_HRMITE' )

!
!     *****************************************************************
!
!     Normal cases: HRMINT
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMINT test:  interpolate the constant function ' // &
                   'y == 1.  The function satisfies y(0) = 1, '       // &
                   'y''(0) = 0, y(1) = 1, y''(1) = 0.  Evaluate the ' // &
                   'function at x = -5, x = -1, x = 0, x = 1, x = 5.'  )


      XVALS(1)   =  0.D0
      XVALS(2)   =  1.D0

      YVALS(1)   =  1.D0
      YVALS(2)   =  0.D0
      YVALS(3)   =  1.D0
      YVALS(4)   =  0.D0

      CALL HRMINT ( 2, XVALS, YVALS, -5.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-5)',   F,  '~/', 1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(-5)', DF, '~',  0.D0, TIGHT, OK )


      CALL HRMINT ( 2, XVALS, YVALS, -1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-1)',   F,  '~/', 1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(-1)', DF, '~',  0.D0, TIGHT, OK )


      CALL HRMINT ( 2, XVALS, YVALS, 0.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(0)',   F,  '~/', 1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(0)', DF, '~',  0.D0, TIGHT, OK )


      CALL HRMINT ( 2, XVALS, YVALS, 1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(1)',   F,  '~/', 1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(1)', DF, '~',  0.D0, TIGHT, OK )


      CALL HRMINT ( 2, XVALS, YVALS, 5.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(5)',   F,  '~/', 1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(5)', DF, '~',  0.D0, TIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMINT test:  interpolate the linear function ' // &
                   'y == 1.  The function satisfies y(0) = 0, '       // &
                   'y''(0) = 1, y(1) = 1, y''(1) = 1.  Evaluate the ' // &
                   'function at x = -5, x = -1, x = 0, x = 1, x = 5.'  )


      XVALS(1)   =  0.D0
      XVALS(2)   =  1.D0

      YVALS(1)   =  0.D0
      YVALS(2)   =  1.D0
      YVALS(3)   =  1.D0
      YVALS(4)   =  1.D0

      CALL HRMINT ( 2, XVALS, YVALS, -5.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-5)',   F,  '~/', -5.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(-5)', DF, '~/',  1.D0, TIGHT, OK )


      CALL HRMINT ( 2, XVALS, YVALS, -1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-1)',   F,  '~/', -1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(-1)', DF, '~/',  1.D0, TIGHT, OK )


      CALL HRMINT ( 2, XVALS, YVALS, 0.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(0)',   F,  '~',   0.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(0)', DF, '~/',  1.D0, TIGHT, OK )


      CALL HRMINT ( 2, XVALS, YVALS, 1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(1)',   F,  '~/',  1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(1)', DF, '~/',  1.D0, TIGHT, OK )


      CALL HRMINT ( 2, XVALS, YVALS, 5.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(5)',   F,  '~/',  5.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(5)', DF, '~/',  1.D0, TIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMINT test:  interpolate the 3rd degree '        // &
                   'polynomial y == -x**3 + 12*x**2 -7*x -6. '        // &
                   'The function y and its derivative take the '      // &
                   'values y(-6) = 684, y''(-6) = -259, '             // &
                   'values y(-1) = 14,  y''(-1) = -34.'       )


      XVALS(1)  =      -6.D0
      XVALS(2)  =      -1.D0

      YVALS(1)  =     684.D0
      YVALS(2)  =    -259.D0
      YVALS(3)  =      14.D0
      YVALS(4)  =     -34.D0

      CALL HRMINT ( 2, XVALS, YVALS, -6.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-6)',   F,  '~/',  YVALS(1), TIGHT, OK )
      CALL CHCKSD ( 'y''(-6)', DF, '~/',  YVALS(2), TIGHT, OK )


      CALL HRMINT ( 2, XVALS, YVALS, -1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-1)',   F,  '~/',  YVALS(3), TIGHT, OK )
      CALL CHCKSD ( 'y''(-1)', DF, '~/',  YVALS(4), TIGHT, OK )


      CALL HRMINT ( 2, XVALS, YVALS, 6.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(6)',   F,  '~/',  168.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(6)', DF, '~/',  29.D0, TIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMINT test:  interpolate the 7th degree '        // &
                   'polynomial y == x**7 + 2*x**2 +5.  The function ' // &
                   'y(-1) = 6, y''(-1) = 3, y(0) = 5, y''(0) = 0, '   // &
                   'y(3) = 2210, y''(3) = 5115, y(5) = 78180, y''(5) '// &
                   '= 109395.'                                         )

      XVALS(1)  =      -1.D0
      XVALS(2)  =       0.D0
      XVALS(3)  =       3.D0
      XVALS(4)  =       5.D0

      YVALS(1)  =       6.D0
      YVALS(2)  =       3.D0
      YVALS(3)  =       5.D0
      YVALS(4)  =       0.D0
      YVALS(5)  =    2210.D0
      YVALS(6)  =    5115.D0
      YVALS(7)  =   78180.D0
      YVALS(8)  =  109395.D0

      CALL HRMINT ( 4, XVALS, YVALS, -1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-1)',   F,  '~/',  6.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(-1)', DF, '~/',  3.D0, TIGHT, OK )


      CALL HRMINT ( 4, XVALS, YVALS, 0.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(0)',   F,  '~/',  5.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(0)', DF, '~',   0.D0, TIGHT, OK )


      CALL HRMINT ( 4, XVALS, YVALS, 2.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(2)',   F,  '~/',  141.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(2)', DF, '~/',  456.D0, TIGHT, OK )


      CALL HRMINT ( 4, XVALS, YVALS, 3.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(3)',   F,  '~/',  2210.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(3)', DF, '~/',  5115.D0, TIGHT, OK )


      CALL HRMINT ( 4, XVALS, YVALS, 5.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(5)',   F,  '~/',   78180.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(5)', DF, '~/',  109395.D0, TIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMINT test:  interpolate the 7th degree '        // &
                   'polynomial y == x**7 + 2*x**2 +5 at a variety '   // &
                   'of abscissa values.'                              )

      CALL HRMINT ( 4, XVALS, YVALS, -15.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-15)',   F,  '~/',  -0.17085892D9, TIGHT, OK )
      CALL CHCKSD ( 'y''(-15)', DF, '~/',   0.79734315D8, TIGHT, OK )

      CALL HRMINT ( 4, XVALS, YVALS, 100.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(100)',   F,  '~/',  0.1000000000200050D+15, &
                    TIGHT,    OK                                )
      CALL CHCKSD ( 'y''(100)', DF, '~/',  0.7000000000400000D+13, &
                    TIGHT,    OK                                )


      CALL HRMINT ( 4, XVALS, YVALS, -1000.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-1000)',   F,  '~/', -0.9999999999999980D+21, &
                    TIGHT,    OK                                )
      CALL CHCKSD ( 'y''(-1000)', DF, '~/',  0.6999999999999996D+19, &
                    TIGHT,    OK                                )

      CALL HRMINT ( 4, XVALS, YVALS, 2000.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(2000)',   F,  '~/', 0.1280000000000000D+24, &
                    TIGHT,    OK                                )
      CALL CHCKSD ( 'y''(2000)', DF, '~/', 0.4480000000000000D+21, &
                    TIGHT,    OK                                )

      CALL HRMINT ( 4, XVALS, YVALS, 1.D-3, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(1.D-3)',   F,  '~/',  0.500000200000000028D+01, &
                    MEDIUM,   OK                                  )
      CALL CHCKSD ( 'y''(1.D-3)', DF, '~/',  0.400000000000000702D-02, &
                    MEDIUM,   OK                                  )


!
!     *****************************************************************
!
!     HRMINT error cases.
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMINT error case:  equal abscissa values.'  )

      XVALS(2)  =      -1.D0

      CALL HRMINT ( 4, XVALS, YVALS, 3.D0, WORK, F, DF )
      CALL CHCKXC ( .TRUE., 'SPICE(DIVIDEBYZERO)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMINT error case:  number of points = 0.'  )

      XVALS(2)  =       0.D0

      CALL HRMINT ( 0, XVALS, YVALS, 3.D0, WORK, F, DF )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)',  OK )

!
!     --- Case: -------------------------------------------------------
!
!
!     Further HRMINT non-error tests:
!
!     Using the same polynomial as above
!
!             7      2
!        y = x  + 2*x  + 5
!
!     define the polynomial on a set of abscissa values having
!     spacing of widely varying magnitudes.
!
      CALL TCASE ( 'HRMINT test:  interpolate the 7th degree '        // &
                   'polynomial y = x**7 + 2*x**2 +5.  Use  '          // &
                   'abscissa points -1000, 0, 1, 1.000001.'            )

      XVALS(1)  =      -100.D0
      XVALS(2)  =       0.D0
      XVALS(3)  =       1.D0
      XVALS(4)  =       1.1D0

      YVALS(1)  =      -0.999999999799950000D+14
      YVALS(2)  =       0.699999999960000000D+13
      YVALS(3)  =       5.D0
      YVALS(4)  =       0.D0
      YVALS(5)  =       8.D0
      YVALS(6)  =      11.D0
      YVALS(7)  =       0.936871710000000135D+01
      YVALS(8)  =       0.168009270000000086D+02

!
!     Make sure we can recover the interpolated values.
!
      CALL HRMINT ( 4, XVALS, YVALS, XVALS(1), WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-100)',   F,  '~/',  YVALS(1), LOOSE, OK )
      CALL CHCKSD ( 'y''(-100)', DF, '~/',  YVALS(2), LOOSE, OK )

      CALL HRMINT ( 4, XVALS, YVALS, XVALS(2), WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(0)',   F,  '~/',  YVALS(3), LOOSE, OK )
      CALL CHCKSD ( 'y''(0)', DF, '~',   YVALS(4), LOOSE, OK )

      CALL HRMINT ( 4, XVALS, YVALS, XVALS(3), WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(1)',   F,  '~/',  YVALS(5), LOOSE, OK )
      CALL CHCKSD ( 'y''(1)', DF, '~/',  YVALS(6), LOOSE, OK )

      CALL HRMINT ( 4, XVALS, YVALS, XVALS(4), WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(1.1)',   F,  '~/',  YVALS(7), LOOSE, OK )
      CALL CHCKSD ( 'y''(1.1)', DF, '~/',  YVALS(8), LOOSE, OK )


!
!     Try a variety of abscissa values.
!
      CALL HRMINT ( 4, XVALS, YVALS, -15.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-15)',   F,  '~/',  -0.17085892D9, LOOSE, OK )
      CALL CHCKSD ( 'y''(-15)', DF, '~/',   0.79734315D8, LOOSE, OK )

      CALL HRMINT ( 4, XVALS, YVALS, 100.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(100)',   F,  '~/',  0.1000000000200050D+15, &
                    LOOSE,    OK                                )
      CALL CHCKSD ( 'y''(100)', DF, '~/',  0.7000000000400000D+13, &
                    LOOSE,    OK                                )


      CALL HRMINT ( 4, XVALS, YVALS, -1000.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-1000)',   F,  '~/', -0.9999999999999980D+21, &
                    LOOSE,    OK                                )
      CALL CHCKSD ( 'y''(-1000)', DF, '~/',  0.6999999999999996D+19, &
                    LOOSE,    OK                                )

      CALL HRMINT ( 4, XVALS, YVALS, 2000.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(2000)',   F,  '~/', 0.1280000000000000D+24, &
                    LOOSE,    OK                                )
      CALL CHCKSD ( 'y''(2000)', DF, '~/', 0.4480000000000000D+21, &
                    LOOSE,    OK                                )

      CALL HRMINT ( 4, XVALS, YVALS, 1.D-3, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(1.D-3)',   F,  '~/',  0.500000200000000028D+01, &
                    LOOSE,   OK                                  )
      CALL CHCKSD ( 'y''(1.D-3)', DF, '~/',  0.400000000000000702D-02, &
                    LOOSE,   OK                                  )


!
!     *****************************************************************
!
!     Normal cases: HRMESP
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMESP test:  interpolate the constant function ' // &
                   'y == 1.  The function satisfies y(0) = 1, '       // &
                   'y''(0) = 0, y(1) = 1, y''(1) = 0.  Evaluate the ' // &
                   'function at x = -5, x = -1, x = 0, x = 1, x = 5.'  )


      START      =  0.D0
      STEP       =  1.D0

      YVALS(1)   =  1.D0
      YVALS(2)   =  0.D0
      YVALS(3)   =  1.D0
      YVALS(4)   =  0.D0

      CALL HRMESP ( 2, START, STEP, YVALS, -5.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-5)',   F,  '~/', 1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(-5)', DF, '~',  0.D0, TIGHT, OK )


      CALL HRMESP ( 2, START, STEP, YVALS, -1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(1-)',   F,  '~/', 1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(-1)', DF, '~',  0.D0, TIGHT, OK )


      CALL HRMESP ( 2, START, STEP, YVALS, 0.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(0)',   F,  '~/', 1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(0)', DF, '~',  0.D0, TIGHT, OK )


      CALL HRMESP ( 2, START, STEP, YVALS, 1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(1)',   F,  '~/', 1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(1)', DF, '~',  0.D0, TIGHT, OK )


      CALL HRMESP ( 2, START, STEP, YVALS, 5.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(5)',   F,  '~/', 1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(5)', DF, '~',  0.D0, TIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMESP test:  interpolate the linear function ' // &
                   'y == 1.  The function satisfies y(0) = 0, '       // &
                   'y''(0) = 1, y(1) = 1, y''(1) = 1.  Evaluate the ' // &
                   'function at x = -5, x = -1, x = 0, x = 1, x = 5.'  )


      START      =  0.D0
      STEP       =  1.D0

      YVALS(1)   =  0.D0
      YVALS(2)   =  1.D0
      YVALS(3)   =  1.D0
      YVALS(4)   =  1.D0

      CALL HRMESP ( 2, START, STEP, YVALS, -5.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-5)',   F,  '~/', -5.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(-5)', DF, '~/',  1.D0, TIGHT, OK )


      CALL HRMESP ( 2, START, STEP, YVALS, -1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-1)',   F,  '~/', -1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(-1)', DF, '~/',  1.D0, TIGHT, OK )


      CALL HRMESP ( 2, START, STEP, YVALS, 0.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(0)',   F,  '~',   0.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(0)', DF, '~/',  1.D0, TIGHT, OK )


      CALL HRMESP ( 2, START, STEP, YVALS, 1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(1)',   F,  '~/',  1.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(1)', DF, '~/',  1.D0, TIGHT, OK )


      CALL HRMESP ( 2, START, STEP, YVALS, 5.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(5)',   F,  '~/',  5.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(5)', DF, '~/',  1.D0, TIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMESP test:  interpolate the 7th degree '        // &
                   'polynomial y == x**7 + 2*x**2 +5.  The function ' // &
                   'y(-1) = 6, y''(-1) = 3, y(1) = 8, y''(1) = 11, '  // &
                   'y(3) = 2210, y''(3) = 5115, y(5) = 78180, y''(5) '// &
                   '= 109395.'                                         )

      START     =      -1.D0
      STEP      =       2.D0

      YVALS(1)  =       6.D0
      YVALS(2)  =       3.D0
      YVALS(3)  =       8.D0
      YVALS(4)  =      11.D0
      YVALS(5)  =    2210.D0
      YVALS(6)  =    5115.D0
      YVALS(7)  =   78180.D0
      YVALS(8)  =  109395.D0

      CALL HRMESP ( 4, START, STEP,  YVALS, -1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-1)',   F,  '~/',  6.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(-1)', DF, '~/',  3.D0, TIGHT, OK )


      CALL HRMESP ( 4, START, STEP,  YVALS, 1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(1)',   F,  '~/',  8.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(1)', DF, '~',  11.D0, TIGHT, OK )


      CALL HRMESP ( 4, START, STEP,  YVALS, 2.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(2)',   F,  '~/',  141.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(2)', DF, '~/',  456.D0, TIGHT, OK )


      CALL HRMESP ( 4, START, STEP,  YVALS, 3.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(3)',   F,  '~/',  2210.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(3)', DF, '~/',  5115.D0, TIGHT, OK )


      CALL HRMESP ( 4, START, STEP,  YVALS, 5.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(5)',   F,  '~/',   78180.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(5)', DF, '~/',  109395.D0, TIGHT, OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMESP test:  interpolate the 7th degree '        // &
                   'polynomial y == x**7 + 2*x**2 +5.  The function ' // &
                   'y(-1) = 6, y''(-1) = 3, y(1) = 8, y''(1) = 11, '  // &
                   'y(3) = 2210, y''(3) = 5115, y(5) = 78180, y''(5) '// &
                   '= 109395.  This time use a NEGATIVE step.'        )

      START     =       5.D0
      STEP      =      -2.D0

      YVALS(7)  =       6.D0
      YVALS(8)  =       3.D0
      YVALS(5)  =       8.D0
      YVALS(6)  =      11.D0
      YVALS(3)  =    2210.D0
      YVALS(4)  =    5115.D0
      YVALS(1)  =   78180.D0
      YVALS(2)  =  109395.D0

      CALL HRMESP ( 4, START, STEP,  YVALS, -1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(-1)',   F,  '~/',  6.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(-1)', DF, '~/',  3.D0, TIGHT, OK )


      CALL HRMESP ( 4, START, STEP,  YVALS, 1.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(1)',   F,  '~/',  8.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(1)', DF, '~',  11.D0, TIGHT, OK )


      CALL HRMESP ( 4, START, STEP,  YVALS, 2.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(2)',   F,  '~/',  141.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(2)', DF, '~/',  456.D0, TIGHT, OK )


      CALL HRMESP ( 4, START, STEP,  YVALS, 3.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(3)',   F,  '~/',  2210.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(3)', DF, '~/',  5115.D0, TIGHT, OK )


      CALL HRMESP ( 4, START, STEP,  YVALS, 5.D0, WORK, F, DF )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'y(5)',   F,  '~/',   78180.D0, TIGHT, OK )
      CALL CHCKSD ( 'y''(5)', DF, '~/',  109395.D0, TIGHT, OK )



!
!     *****************************************************************
!
!     HRMESP error cases.
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMESP error case:  step size zero.'  )

      START     =      -1.D0
      STEP      =       0.D0

      CALL HRMESP ( 4, START, STEP,  YVALS, 5.D0, WORK, F, DF )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEPSIZE)',  OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'HRMESP error case:  number of points = 0.'  )

      START     =      -1.D0
      STEP      =       2.D0

      CALL HRMESP ( 0, START, STEP,  YVALS, 5.D0, WORK, F, DF )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)',  OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
