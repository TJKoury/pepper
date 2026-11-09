!$Procedure F_NPEDLN ( NPEDLN tests )

      SUBROUTINE F_NPEDLN ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine NPEDLN. NPEDLN find the nearest
!     point to a specified line on the surface of an ellipsoid.
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
!     This routine tests the SPICELIB routine NPEDLN.
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
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 30-MAY-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 1.0.0, 15-NOV-2005 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD
      LOGICAL               T_ISNPLN
      LOGICAL               T_ISEDPT

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 400 )

!      INTEGER               NEASY
!      PARAMETER           ( NEASY  =  3 )

      INTEGER               NSIMPL
      PARAMETER           ( NSIMPL =  3 )

      INTEGER               NRANDM
      PARAMETER           ( NRANDM = 5000 )

!      INTEGER               NUMSCL
!      PARAMETER           ( NUMSCL =  5 )


!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      LINEDR ( 3 )
      DOUBLE PRECISION      LINEPT ( 3 )
      DOUBLE PRECISION      NEGDIR ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      SFACTR
      DOUBLE PRECISION      SMPA   (     NSIMPL )
      DOUBLE PRECISION      SMPB   (     NSIMPL )
      DOUBLE PRECISION      SMPC   (     NSIMPL )
      DOUBLE PRECISION      SMPDIR ( 3,  NSIMPL )
      DOUBLE PRECISION      SMPPT  ( 3,  NSIMPL )
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               I
      INTEGER               SEED

      LOGICAL               FOUND
      LOGICAL               FOUND2
      LOGICAL               ORTHOG
      LOGICAL               VALPT

!
!     Saved values
!
      SAVE                  SMPA
      SAVE                  SMPB
      SAVE                  SMPC
      SAVE                  SMPPT
      SAVE                  SMPDIR

!
!     Initial values
!

!
!     The simple test cases.
!
      DATA SMPA (1), SMPB(1), SMPC(1)  /   1.D1 , 2.D2,  3.D3  /

      DATA ( SMPPT (I,1), I = 1, 3 )   /   2.D1,  0.D0,  0.D0  /
      DATA ( SMPDIR(I,1), I = 1, 3 )   /   0.D0,  0.D0, -1.D0  /


      DATA SMPA (2), SMPB(2), SMPC(2)  /   3.D30 , 3.D30,  3.D30  /

      DATA ( SMPPT (I,2), I = 1, 3 )   /   4.D30,  0.D30,  4.D30  /
      DATA ( SMPDIR(I,2), I = 1, 3 )   /  -1.D0,   0.D0,   2.D0   /


      DATA SMPA (3), SMPB(3), SMPC(3)  /    1.D-2 , 1.D-2,  5.D-3  /

      DATA ( SMPPT (I,3), I = 1, 3 )   /   0.D0,    1.D0,    1.D0     /
      DATA ( SMPDIR(I,3), I = 1, 3 )   /   0.D0,   -2.D0,   -1.D0     /




!
!     In this test family, we encapsulate some of the geometric
!     tests within functions defined in this file. The function
!
!        T_ISEDPT
!
!     tests a point is on a specified ellipsoid:
!
!        - A point P = (x,y,z) is "on" the ellipsoid with semi-axis
!          lengths a, b, c if the "level surface parameter"
!
!             lambda = x**2/a**2 + y**2/b**2 + z**2/c**2
!
!          is sufficiently close to 1.
!
!
!     The function
!
!        T_ISNPLN
!
!     test whether the near point found by NPEDLN has the property
!     that the outward normal at the near point can be extended to
!     intersect the input line at right angles.
!
!     T_ISNPLN also checks the distance of the near point from the line:
!     the distance is checked against the value obtained from NPLNPT.
!
!




!
!     Open the test family.
!
      CALL TOPEN ( 'F_NPEDLN' )


!
!     Run some simple tests where the correct results can be
!     determined by inspection.
!
      DO I = 1, NSIMPL
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'NPEDLN simple case #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE  ( TITLE )


         CALL NPEDLN ( SMPA (  I),  SMPB  (  I),  SMPC(I), &
                       SMPPT(1,I),  SMPDIR(1,I),  PNEAR,   DIST )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure the intersection ellipse we found is contained in
!        both the plane and the surface of the ellipsoid.
!
         VALPT  = T_ISEDPT ( PNEAR, SMPA(I), SMPB(I), SMPC(I), OK )

         CALL CHCKSL ( 'Is near point on surface?', VALPT, .TRUE., OK )

         ORTHOG = T_ISNPLN ( SMPA (  I), SMPB  (  I), SMPC(I), &
                           SMPPT(1,I), SMPDIR(1,I), PNEAR,   DIST, OK )

         CALL CHCKSL ( 'Does extension of outward normal hit '   // &
                       'line orthogonally?  Is distance correct?', &
                       ORTHOG, .TRUE., OK )

      END DO





!
!     Now for some more difficult cases.  We'll generate the ellipsoids
!     and lines using random numbers.  There are ten components to
!     generate:
!
!        - random line direction vectors
!        - random line points
!        - random ellipsoid axes
!        - random scale factors for the ellipsoid and plane; these are
!          used to create a wide range of scales
!

      SEED = -1

      DO I = 1, NRANDM
!
! --- Case: ------------------------------------------------------
!

!
!        Get a scale factor.
!
         SFACTR    =  10.D0 ** T_RANDD ( -290.D0, 290.D0, SEED )

!
!        Make up ellipsoid axes and plane constant.
!
         A         =  SFACTR  *  T_RANDD (  1.D0,   2.D0, SEED )
         B         =  SFACTR  *  T_RANDD (  1.D0,   2.D0, SEED )
         C         =  SFACTR  *  T_RANDD (  1.D0,   2.D0, SEED )

!
!        We gotta have a line direction vector.
!
         LINEDR(1) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEDR(2) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEDR(3) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )

         CALL VHATIP ( LINEDR )

!
!        We also need a point on the line.  Scale the point up to
!        increase the likelihood of a non-intercept case.
!
         LINEPT(1) =  10 * SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEPT(2) =  10 * SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEPT(3) =  10 * SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )

         TITLE = 'NPEDLN Random case #.  A, B, C = # # #; ' // &
                 'LINEDR = (#, #, #); LINEPT = (#, #, #)'
         CALL REPMI ( TITLE, '#', I,             TITLE )
         CALL REPMD ( TITLE, '#', A,         14, TITLE )
         CALL REPMD ( TITLE, '#', B,         14, TITLE )
         CALL REPMD ( TITLE, '#', C,         14, TITLE )
         CALL REPMD ( TITLE, '#', LINEDR(1), 14, TITLE )
         CALL REPMD ( TITLE, '#', LINEDR(2), 14, TITLE )
         CALL REPMD ( TITLE, '#', LINEDR(3), 14, TITLE )
         CALL REPMD ( TITLE, '#', LINEPT(1), 14, TITLE )
         CALL REPMD ( TITLE, '#', LINEPT(2), 14, TITLE )
         CALL REPMD ( TITLE, '#', LINEPT(3), 14, TITLE )

         CALL TCASE ( TITLE )

!
!        Cross our fingers and toes and let 'er rip.
!
         CALL NPEDLN ( A, B, C, LINEPT, LINEDR, PNEAR, DIST )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure the intersection ellipse we found is contained in
!        both the plane and the surface of the ellipsoid.
!
         VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

         CALL CHCKSL ( 'Is near point on surface?', VALPT, .TRUE., OK )

!
!        Check for surface intercept.
!
         CALL SURFPT ( LINEPT, LINEDR, A, B, C, XPT, FOUND  )

         IF ( .NOT. FOUND ) THEN
!
!           See whether the opposite ray hits the ellipsoid.
!
            CALL VMINUS ( LINEDR, NEGDIR )

            CALL SURFPT ( LINEPT, NEGDIR, A, B, C, XPT, FOUND2 )

         END IF

!
!        If we have an intersection case, test the near point
!        and distance.
!
         IF ( FOUND .OR. FOUND2 ) THEN

            CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPT, 3, TIGHT, OK )

            CALL CHCKSD ( 'DIST',  DIST,  '=',   0.D0,   0.D0,  OK )

         ELSE
!
!           If we have a non-intersection case, test the solution
!           for the orthogonality condition.  Also check the distance
!           of the near point from the line.
!
            ORTHOG = T_ISNPLN ( A, B, C, LINEPT, LINEDR, &
                                         PNEAR,  DIST,   OK )

            CALL CHCKSL ( 'Does extension of outward normal hit '   // &
                          'line orthogonally?  Is distance correct?', &
                          ORTHOG, .TRUE., OK )

         END IF


      END DO


!
!     NPEDLN error cases:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NPEDLN: zero direction vector,' )

      CALL FILLD  ( 1.D1, 3,  LINEPT )
      CALL CLEARD (       3,  LINEDR )

      CALL NPEDLN ( 1.D0, 1.D0, 1.D0,  LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NPEDLN: ellipsoid has one zero-length axis.' )

      CALL FILLD (   1.D1, 3, LINEPT )
      CALL FILLD (  -1.D0, 3, LINEDR )

      CALL NPEDLN ( 0.D0, 1.D0, 1.D0, LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL NPEDLN ( 1.D0, 0.D0, 1.D0, LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL NPEDLN ( 1.D0, 1.D0, 0.D0, LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NPEDLN: ellipsoid has one negative-length axis.' )

      CALL NPEDLN ( -1.D0, 1.D0,   1.D0,  LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL NPEDLN (  1.D0, -1.D0,  1.D0,  LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL NPEDLN (  1.D0,  1.D0, -1.D0,  LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

!
! --- Case: ------------------------------------------------------
!
!
!     This error may not be detected on some systems.  We exclude
!     it for now.

!CC    CALL TCASE  ( 'NPEDLN: axis length underflow after squaring.' )



!CC    CALL NPEDLN (  1.D255, 1.D0, 1.D0,  LINEPT, LINEDR, PNEAR, DIST )
!CC    CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

!CC    CALL NPEDLN (  1.D0, 1.D255, 1.D0,  LINEPT, LINEDR, PNEAR, DIST )
!CC    CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

!CC    CALL NPEDLN (  1.D0, 1.D0, 1.D255,  LINEPT, LINEDR, PNEAR, DIST )
!CC    CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END





!
!     The utility function T_ISNPLN tests whether the "near point" found
!     by NPEDLN satisfies the criterion that the outward surface normal
!     at this point can be extended to intersect the input line
!     orthogonally.
!
      LOGICAL FUNCTION T_ISNPLN ( A, B, C,  LINEPT, LINEDR, X, D, OK )
      IMPLICIT NONE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      LINEPT ( 3 )
      DOUBLE PRECISION      LINEDR ( 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      D
      LOGICAL               OK

!
!     SPICELIB functions
!

!
!     Local parameters
!
      DOUBLE PRECISION      DTOL
      PARAMETER           ( DTOL   = 1.D-12 )

      DOUBLE PRECISION      NPTOL
      PARAMETER           ( NPTOL  = 1.D-12 )

!
!     Local variables
!
      DOUBLE PRECISION      APPROX ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      LINMIN ( 3 )
      DOUBLE PRECISION      DIST2

!
!     Executable code.
!

!
!     The point on the line closest to the ellipsoid ( LINMIN ).
!
      CALL NPLNPT ( LINEPT, LINEDR, X, LINMIN, DIST2 )

!
!     Check the distance.  This is pretty easy to get right.
!
      CALL CHCKSD ( 'D', D, '~/', DIST2, DTOL, OK )

      IF ( .NOT. OK ) THEN

         T_ISNPLN = .FALSE.
         RETURN

      END IF

!
!     Obtain ellipsoid surface unit normal at the near point.
!
      CALL SURFNM ( A, B, C, X, NORMAL )

!
!     Approximation to LINMIN using X and D.
!
      CALL VSCLIP ( D, NORMAL )
      CALL VADD   ( X, NORMAL, APPROX )

!
!     Check the relative error in the approximation to LINMIN.
!
      CALL CHCKAD ( 'APPROX', APPROX, '~~/', LINMIN, 3, NPTOL, OK )

      T_ISNPLN = OK

      RETURN
      END




!
!     Supporting function T_ISEDPT
!
      LOGICAL FUNCTION T_ISEDPT ( PT, A, B, C, OK )
      IMPLICIT NONE
!
!     Test whether a point is on a specified ellipsoid.
!
      DOUBLE PRECISION      PT ( 3 )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      LOGICAL               OK

!
!     SPICELIB functions
!

!
!     Local parameters
!
      DOUBLE PRECISION      LVLTOL
      PARAMETER           ( LVLTOL = 1.D-12 )

!
!     Local variables
!
      DOUBLE PRECISION      ELPT   ( 3 )
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SCLA
      DOUBLE PRECISION      SCLB
      DOUBLE PRECISION      SCLC

!
!     Scale ellipse and plane.
!
      SCALE =  MAX ( DABS(A), DABS(B), DABS(C) )

      SCLA  =  A / SCALE
      SCLB  =  B / SCALE
      SCLC  =  C / SCALE

      CALL VSCL ( 1.D0 / SCALE, PT, ELPT )


!
!     Check the `level' of the point.
!
      LEVEL  =    ( ELPT(1)**2 / SCLA**2 ) &
                + ( ELPT(2)**2 / SCLB**2 ) &
                + ( ELPT(3)**2 / SCLC**2 )

      CALL CHCKSD ( 'LEVEL', LEVEL, '~', 1.D0, LVLTOL, OK )

      T_ISEDPT = OK

      RETURN
      END
