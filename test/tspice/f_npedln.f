C$Procedure F_NPEDLN ( NPEDLN tests )

      SUBROUTINE F_NPEDLN ( OK )

C$ Abstract
C
C     Exercise the SPICELIB routine NPEDLN. NPEDLN find the nearest
C     point to a specified line on the surface of an ellipsoid.
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
C     This routine tests the SPICELIB routine NPEDLN.
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
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 30-MAY-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Edited the header to comply with NAIF standard.
C
C-    TSPICE Version 1.0.0, 15-NOV-2005 (NJB)
C
C-&


C$ Index_Entries
C
C     None.
C
C-&


C
C     Other functions
C
      DOUBLE PRECISION      T_RANDD
      LOGICAL               T_ISNPLN
      LOGICAL               T_ISEDPT

C
C     Local Parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 400 )

C      INTEGER               NEASY
C      PARAMETER           ( NEASY  =  3 )

      INTEGER               NSIMPL
      PARAMETER           ( NSIMPL =  3 )

      INTEGER               NRANDM
      PARAMETER           ( NRANDM = 5000 )

C      INTEGER               NUMSCL
C      PARAMETER           ( NUMSCL =  5 )


C
C     Local Variables
C
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

C
C     Saved values
C
      SAVE                  SMPA
      SAVE                  SMPB
      SAVE                  SMPC
      SAVE                  SMPPT
      SAVE                  SMPDIR

C
C     Initial values
C

C
C     The simple test cases.
C
      DATA SMPA (1), SMPB(1), SMPC(1)  /   1.D1 , 2.D2,  3.D3  /

      DATA ( SMPPT (I,1), I = 1, 3 )   /   2.D1,  0.D0,  0.D0  /
      DATA ( SMPDIR(I,1), I = 1, 3 )   /   0.D0,  0.D0, -1.D0  /


      DATA SMPA (2), SMPB(2), SMPC(2)  /   3.D30 , 3.D30,  3.D30  /

      DATA ( SMPPT (I,2), I = 1, 3 )   /   4.D30,  0.D30,  4.D30  /
      DATA ( SMPDIR(I,2), I = 1, 3 )   /  -1.D0,   0.D0,   2.D0   /


      DATA SMPA (3), SMPB(3), SMPC(3)  /    1.D-2 , 1.D-2,  5.D-3  /

      DATA ( SMPPT (I,3), I = 1, 3 )   /   0.D0,    1.D0,    1.D0     /
      DATA ( SMPDIR(I,3), I = 1, 3 )   /   0.D0,   -2.D0,   -1.D0     /




C
C     In this test family, we encapsulate some of the geometric
C     tests within functions defined in this file. The function
C
C        T_ISEDPT
C
C     tests a point is on a specified ellipsoid:
C
C        - A point P = (x,y,z) is "on" the ellipsoid with semi-axis
C          lengths a, b, c if the "level surface parameter"
C
C             lambda = x**2/a**2 + y**2/b**2 + z**2/c**2
C
C          is sufficiently close to 1.
C
C
C     The function
C
C        T_ISNPLN
C
C     test whether the near point found by NPEDLN has the property
C     that the outward normal at the near point can be extended to
C     intersect the input line at right angles.
C
C     T_ISNPLN also checks the distance of the near point from the line:
C     the distance is checked against the value obtained from NPLNPT.
C
C




C
C     Open the test family.
C
      CALL TOPEN ( 'F_NPEDLN' )


C
C     Run some simple tests where the correct results can be
C     determined by inspection.
C
      DO I = 1, NSIMPL
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'NPEDLN simple case #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE  ( TITLE )


         CALL NPEDLN ( SMPA (  I),  SMPB  (  I),  SMPC(I),
     .                 SMPPT(1,I),  SMPDIR(1,I),  PNEAR,   DIST )

         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Make sure the intersection ellipse we found is contained in
C        both the plane and the surface of the ellipsoid.
C
         VALPT  = T_ISEDPT ( PNEAR, SMPA(I), SMPB(I), SMPC(I), OK )

         CALL CHCKSL ( 'Is near point on surface?', VALPT, .TRUE., OK )

         ORTHOG = T_ISNPLN ( SMPA (  I), SMPB  (  I), SMPC(I),
     .                     SMPPT(1,I), SMPDIR(1,I), PNEAR,   DIST, OK )

         CALL CHCKSL ( 'Does extension of outward normal hit '   //
     .                 'line orthogonally?  Is distance correct?',
     .                 ORTHOG, .TRUE., OK )

      END DO





C
C     Now for some more difficult cases.  We'll generate the ellipsoids
C     and lines using random numbers.  There are ten components to
C     generate:
C
C        - random line direction vectors
C        - random line points
C        - random ellipsoid axes
C        - random scale factors for the ellipsoid and plane; these are
C          used to create a wide range of scales
C

      SEED = -1

      DO I = 1, NRANDM
C
C --- Case: ------------------------------------------------------
C

C
C        Get a scale factor.
C
         SFACTR    =  10.D0 ** T_RANDD ( -290.D0, 290.D0, SEED )

C
C        Make up ellipsoid axes and plane constant.
C
         A         =  SFACTR  *  T_RANDD (  1.D0,   2.D0, SEED )
         B         =  SFACTR  *  T_RANDD (  1.D0,   2.D0, SEED )
         C         =  SFACTR  *  T_RANDD (  1.D0,   2.D0, SEED )

C
C        We gotta have a line direction vector.
C
         LINEDR(1) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEDR(2) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEDR(3) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )

         CALL VHATIP ( LINEDR )

C
C        We also need a point on the line.  Scale the point up to
C        increase the likelihood of a non-intercept case.
C
         LINEPT(1) =  10 * SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEPT(2) =  10 * SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         LINEPT(3) =  10 * SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )

         TITLE = 'NPEDLN Random case #.  A, B, C = # # #; ' //
     .           'LINEDR = (#, #, #); LINEPT = (#, #, #)'
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

C
C        Cross our fingers and toes and let 'er rip.
C
         CALL NPEDLN ( A, B, C, LINEPT, LINEDR, PNEAR, DIST )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Make sure the intersection ellipse we found is contained in
C        both the plane and the surface of the ellipsoid.
C
         VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

         CALL CHCKSL ( 'Is near point on surface?', VALPT, .TRUE., OK )

C
C        Check for surface intercept.
C
         CALL SURFPT ( LINEPT, LINEDR, A, B, C, XPT, FOUND  )

         IF ( .NOT. FOUND ) THEN
C
C           See whether the opposite ray hits the ellipsoid.
C
            CALL VMINUS ( LINEDR, NEGDIR )

            CALL SURFPT ( LINEPT, NEGDIR, A, B, C, XPT, FOUND2 )

         END IF

C
C        If we have an intersection case, test the near point
C        and distance.
C
         IF ( FOUND .OR. FOUND2 ) THEN

            CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPT, 3, TIGHT, OK )

            CALL CHCKSD ( 'DIST',  DIST,  '=',   0.D0,   0.D0,  OK )

         ELSE
C
C           If we have a non-intersection case, test the solution
C           for the orthogonality condition.  Also check the distance
C           of the near point from the line.
C
            ORTHOG = T_ISNPLN ( A, B, C, LINEPT, LINEDR,
     .                                   PNEAR,  DIST,   OK )

            CALL CHCKSL ( 'Does extension of outward normal hit '   //
     .                    'line orthogonally?  Is distance correct?',
     .                    ORTHOG, .TRUE., OK )

         END IF


      END DO


C
C     NPEDLN error cases:
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NPEDLN: zero direction vector,' )

      CALL FILLD  ( 1.D1, 3,  LINEPT )
      CALL CLEARD (       3,  LINEDR )

      CALL NPEDLN ( 1.D0, 1.D0, 1.D0,  LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NPEDLN: ellipsoid has one zero-length axis.' )

      CALL FILLD (   1.D1, 3, LINEPT )
      CALL FILLD (  -1.D0, 3, LINEDR )

      CALL NPEDLN ( 0.D0, 1.D0, 1.D0, LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL NPEDLN ( 1.D0, 0.D0, 1.D0, LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL NPEDLN ( 1.D0, 1.D0, 0.D0, LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NPEDLN: ellipsoid has one negative-length axis.' )

      CALL NPEDLN ( -1.D0, 1.D0,   1.D0,  LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL NPEDLN (  1.D0, -1.D0,  1.D0,  LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL NPEDLN (  1.D0,  1.D0, -1.D0,  LINEPT, LINEDR, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

C
C --- Case: ------------------------------------------------------
C
C
C     This error may not be detected on some systems.  We exclude
C     it for now.

CCC    CALL TCASE  ( 'NPEDLN: axis length underflow after squaring.' )



CCC    CALL NPEDLN (  1.D255, 1.D0, 1.D0,  LINEPT, LINEDR, PNEAR, DIST )
CCC    CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

CCC    CALL NPEDLN (  1.D0, 1.D255, 1.D0,  LINEPT, LINEDR, PNEAR, DIST )
CCC    CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

CCC    CALL NPEDLN (  1.D0, 1.D0, 1.D255,  LINEPT, LINEDR, PNEAR, DIST )
CCC    CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END





C
C     The utility function T_ISNPLN tests whether the "near point" found
C     by NPEDLN satisfies the criterion that the outward surface normal
C     at this point can be extended to intersect the input line
C     orthogonally.
C
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

C
C     SPICELIB functions
C

C
C     Local parameters
C
      DOUBLE PRECISION      DTOL
      PARAMETER           ( DTOL   = 1.D-12 )

      DOUBLE PRECISION      NPTOL
      PARAMETER           ( NPTOL  = 1.D-12 )

C
C     Local variables
C
      DOUBLE PRECISION      APPROX ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      LINMIN ( 3 )
      DOUBLE PRECISION      DIST2

C
C     Executable code.
C

C
C     The point on the line closest to the ellipsoid ( LINMIN ).
C
      CALL NPLNPT ( LINEPT, LINEDR, X, LINMIN, DIST2 )

C
C     Check the distance.  This is pretty easy to get right.
C
      CALL CHCKSD ( 'D', D, '~/', DIST2, DTOL, OK )

      IF ( .NOT. OK ) THEN

         T_ISNPLN = .FALSE.
         RETURN

      END IF

C
C     Obtain ellipsoid surface unit normal at the near point.
C
      CALL SURFNM ( A, B, C, X, NORMAL )

C
C     Approximation to LINMIN using X and D.
C
      CALL VSCLIP ( D, NORMAL )
      CALL VADD   ( X, NORMAL, APPROX )

C
C     Check the relative error in the approximation to LINMIN.
C
      CALL CHCKAD ( 'APPROX', APPROX, '~~/', LINMIN, 3, NPTOL, OK )

      T_ISNPLN = OK

      RETURN
      END




C
C     Supporting function T_ISEDPT
C
      LOGICAL FUNCTION T_ISEDPT ( PT, A, B, C, OK )
      IMPLICIT NONE
C
C     Test whether a point is on a specified ellipsoid.
C
      DOUBLE PRECISION      PT ( 3 )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      LOGICAL               OK

C
C     SPICELIB functions
C

C
C     Local parameters
C
      DOUBLE PRECISION      LVLTOL
      PARAMETER           ( LVLTOL = 1.D-12 )

C
C     Local variables
C
      DOUBLE PRECISION      ELPT   ( 3 )
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SCLA
      DOUBLE PRECISION      SCLB
      DOUBLE PRECISION      SCLC

C
C     Scale ellipse and plane.
C
      SCALE =  MAX ( DABS(A), DABS(B), DABS(C) )

      SCLA  =  A / SCALE
      SCLB  =  B / SCALE
      SCLC  =  C / SCALE

      CALL VSCL ( 1.D0 / SCALE, PT, ELPT )


C
C     Check the `level' of the point.
C
      LEVEL  =    ( ELPT(1)**2 / SCLA**2 )
     .          + ( ELPT(2)**2 / SCLB**2 )
     .          + ( ELPT(3)**2 / SCLC**2 )

      CALL CHCKSD ( 'LEVEL', LEVEL, '~', 1.D0, LVLTOL, OK )

      T_ISEDPT = OK

      RETURN
      END
