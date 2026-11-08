C$Procedure F_NPELPT ( NPELPT tests )

      SUBROUTINE F_NPELPT ( OK )

C$ Abstract
C
C     Exercise the SPICELIB routine NPELPT. NPELPT find the nearest
C     point on an ellipse to a specified point.
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
C     This routine tests the SPICELIB routine NPELPT.
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
C-    TSPICE Version 1.2.0, 30-MAY-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Edited the header to comply with NAIF standard.
C
C-    TSPICE Version 1.1.0, 22-FEB-2008 (NJB)
C
C        Loosened tolerances in test function T_ISNPEL.
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
      LOGICAL               T_ISNPEL

C
C     Local Parameters
C
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 400 )

      INTEGER               NRANDM
      PARAMETER           ( NRANDM = 5000 )

C
C     Local Variables
C
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      SFACTR
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      V1     ( 3 )
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      XDIST
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               SEED

      LOGICAL               VALPT

C
C     Saved values
C

C
C     Initial values
C



C
C     In this test family, we encapsulate some of the geometric
C     tests within functions defined in this file. The function
C
C        T_ISNPEL
C
C     tests whether a 3-dpoint is on a specified ellipse embedded in
C     3-dimensional space:
C
C        - A point P = (p1,p2,p3) is "on" the ellipse with center C
C          and semi-axes SMAJOR and SMINOR if P lies in the plane
C          of the ellipse and if the "level surface parameter"
C
C             lambda =  x**2/A**2 + y**2/B**2
C
C          is sufficiently close to 1, where
C
C             A = ||SMAJOR||
C             B = ||SMINOR||
C
C             x = <P-C, SMAJOR/A> * SMAJOR / A
C             y = <P-C, SMINOR/B> * SMINOR / B
C
C
C     The function
C
C        T_ISNPEL
C
C     test whether the near point found by NPELPT has the property
C     that the outward normal at the near point can be extended to
C     intersect the input line at right angles.
C
C     T_ISNPEL also checks the distance of the near point from the line:
C     the distance is checked against the value obtained from NPLNPT.
C
C




C
C     Open the test family.
C
      CALL TOPEN ( 'F_NPELPT' )


C
C     Run some simple tests where the correct results can be
C     determined by inspection.
C
C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Ellipse is unit circle in x-y plane.  Point is '//
     .        '(1,0,1)'
      CALL REPMI ( TITLE, '#', I, TITLE )
      CALL TCASE ( TITLE )

      CALL VPACK  ( 1.D0,    0.D0,  1.D0,   POINT )
      CALL VPACK  ( 1.D0,    0.D0,  0.D0,   V1     )
      CALL VPACK  ( 0.D0,    1.D0,  0.D0,   V2     )
      CALL VPACK  ( 0.D0,    0.D0,  0.D0,   CENTER )
      CALL CGV2EL ( CENTER,  V1,    V2,     ELLIPS )

      CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Test results.
C
      CALL VEQU ( V1, XPT )
      XDIST = 1.D0

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPT,   3, TIGHT, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,    0.D0,  OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Ellipse is unit circle in z=5 plane.  Point is '//
     .        '(4,0,1)'
      CALL REPMI ( TITLE, '#', I, TITLE )
      CALL TCASE ( TITLE )

      CALL VPACK  ( 4.D0,    0.D0,  1.D0,   POINT )
      CALL VPACK  ( 1.D0,    0.D0,  0.D0,   V1     )
      CALL VPACK  ( 0.D0,    1.D0,  0.D0,   V2     )
      CALL VPACK  ( 0.D0,    0.D0,  5.D0,   CENTER )
      CALL CGV2EL ( CENTER,  V1,    V2,     ELLIPS )

      CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Test results.
C
      CALL VPACK  ( 1.D0,    0.D0,  5.D0,  XPT   )
      XDIST = 5.D0

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPT,   3, TIGHT, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,    0.D0,  OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Ellipse is unit circle in z=5 plane.  Point is '//
     .        '(0,0,5)'
      CALL REPMI ( TITLE, '#', I, TITLE )
      CALL TCASE ( TITLE )

      CALL VPACK  ( 0.D0,    0.D0,  5.D0,   POINT )
      CALL VPACK  ( 1.D0,    0.D0,  0.D0,   V1     )
      CALL VPACK  ( 0.D0,    1.D0,  0.D0,   V2     )
      CALL VPACK  ( 0.D0,    0.D0,  5.D0,   CENTER )
      CALL CGV2EL ( CENTER,  V1,    V2,     ELLIPS )

      CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Test results.
C
C     For this case, we don't know which point will be selected
C     as the near point.  Just check that it's a valid solution.
C
C
      VALPT = T_ISNPEL ( POINT, ELLIPS, PNEAR, DIST, OK )

      CALL CHCKSL ( 'Is near point on ellipse?', VALPT, .TRUE., OK )



C
C     Now for some more difficult cases.  We'll generate the ellipses
C     and planes using random numbers.  There are four components to
C     generate:
C
C        - random points
C        - random ellipse axes
C        - random ellipse centers
C        - random scale factors for the ellipse and point; these are
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
         SFACTR  =  10.D0 ** T_RANDD ( -290.D0, 290.D0, SEED )

C
C        Make up ellipse vectors.
C
         DO J = 1, 3

            CENTER(J) =  SFACTR  *   T_RANDD (  -1.D0,  1.D0,  SEED )
            V1(J)     =  SFACTR  *   T_RANDD (  -1.D0,  1.D0,  SEED )
            V2(J)     =  SFACTR  *   T_RANDD (  -1.D0,  1.D0,  SEED )

         END DO

C
C        Pack the ellipse.
C
         CALL CGV2EL ( CENTER, V1, V2, ELLIPS )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Find the semi-axes of this ellipse.
C
         CALL SAELGV ( V1, V2, SMAJOR, SMINOR )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We also need a point.
C
         POINT(1) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         POINT(2) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         POINT(3) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )


         TITLE = 'NPELPT Random case #.  CENTER = (#, #, #); ' //
     .           'V1 = (#, #, #); V2 = (#, #, #); POINT = '    //
     .           '(#, #, #)'

         CALL REPMI ( TITLE, '#', I,             TITLE )
         CALL REPMD ( TITLE, '#', CENTER(1), 14, TITLE )
         CALL REPMD ( TITLE, '#', CENTER(2), 14, TITLE )
         CALL REPMD ( TITLE, '#', CENTER(3), 14, TITLE )
         CALL REPMD ( TITLE, '#', V1(1),     14, TITLE )
         CALL REPMD ( TITLE, '#', V1(2),     14, TITLE )
         CALL REPMD ( TITLE, '#', V1(3),     14, TITLE )
         CALL REPMD ( TITLE, '#', V2(1),     14, TITLE )
         CALL REPMD ( TITLE, '#', V2(2),     14, TITLE )
         CALL REPMD ( TITLE, '#', V2(3),     14, TITLE )
         CALL REPMD ( TITLE, '#', POINT(1),  14, TITLE )
         CALL REPMD ( TITLE, '#', POINT(2),  14, TITLE )
         CALL REPMD ( TITLE, '#', POINT(3),  14, TITLE )

         CALL TCASE ( TITLE )
C
C        Cross our fingers and toes and let 'er rip.
C
         CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Make sure point we found lies on the ellipse.
C
         VALPT = T_ISNPEL ( POINT, ELLIPS, PNEAR, DIST, OK )

         CALL CHCKSL ( 'Is near point on ellipse?', VALPT, .TRUE., OK )

      END DO


C
C     NPELPT error cases:
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'NPELPT: ellipse has one zero-length axis.' )

      CALL FILLD (   1.D1, 3, POINT )

      CALL VPACK  ( 0.D0,    0.D0,  0.D0,   V1     )
      CALL VPACK  ( 0.D0,    1.D0,  0.D0,   V2     )
      CALL VPACK  ( 2.D0,    2.D0,  2.D0,   CENTER )
      CALL CGV2EL ( CENTER,  V1,    V2,     ELLIPS )

      CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

C
C     Swap the semi-axes and repeat.
C
      CALL CGV2EL ( CENTER,  V2,    V1,     ELLIPS )
      CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END





C
C
C     Supporting function T_ISNPEL
C
      LOGICAL FUNCTION T_ISNPEL ( VIEWPT, ELLIPS, PNEAR, D, OK )
      IMPLICIT NONE
C
C     The utility function T_ISNPEL tests whether the "near point"
C     found by NPELPT satisfies the criterion that the line segment
C     connecting this point to the viewing point is orthogonal to the
C     ellipse's tangent vector at the near point.  The function also
C     test the viewpoint-ellipse distance and verifies that the
C     near point lies on the ellipse.

      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      DOUBLE PRECISION      VIEWPT ( 3 )
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      D
      LOGICAL               OK

C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

C
C     Local parameters
C
      DOUBLE PRECISION      LVLTOL
      PARAMETER           ( LVLTOL = 1.D-11 )


      DOUBLE PRECISION      DSTTOL
      PARAMETER           ( DSTTOL = 1.D-11 )

C
C     Local variables
C
      DOUBLE PRECISION      AXMAT  ( 3, 3 )
      DOUBLE PRECISION      AXPT   ( 3 )
      DOUBLE PRECISION      AXVIEW ( 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SCLA
      DOUBLE PRECISION      SCLB
      DOUBLE PRECISION      SCLPT  ( 3 )
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      TANGNT ( 3 )
      DOUBLE PRECISION      VPAX   ( 3 )
      DOUBLE PRECISION      VPSCL  ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )

C
C     Set an initial return value.
C
      T_ISNPEL = .FALSE.

C
C     We're going to express the vector from the ellipse's center
C     to the near point in a frame whose axes are parallel to the
C     ellipse's semi-axes.
C
C     Unpack ellipse.
C
      CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )

C
C     Scale view point, ellipse and near point.
C
      SCALE =  MAX ( VNORM(CENTER), VNORM(SMAJOR), VNORM(SMINOR) )

      CALL VSCL   ( 1.D0 / SCALE, VIEWPT, VPSCL )

      CALL VSCLIP ( 1.D0 / SCALE, CENTER        )
      CALL VSCLIP ( 1.D0 / SCALE, SMAJOR        )
      CALL VSCLIP ( 1.D0 / SCALE, SMINOR        )

      CALL VSCL   ( 1.D0 / SCALE, PNEAR,  SCLPT )

      SCLA  =  VNORM(SMAJOR)
      SCLB  =  VNORM(SMINOR)

C
C     Find the vector from the near point to the center.
C
      CALL VSUB ( SCLPT, CENTER, OFFSET )

C
C     Create the matrix that maps from the base frame to
C     a frame where the semi-axes are the x and y axes.
C
      CALL TWOVEC ( SMAJOR, 1, SMINOR, 2, AXMAT )

C
C     Map the offset vector into the semi-axis frame.
C
      CALL MXV ( AXMAT, OFFSET, AXPT )


C
C     Check the `level' of the near point.
C
      LEVEL  =    ( AXPT(1)**2 / SCLA**2 )
     .          + ( AXPT(2)**2 / SCLB**2 )

      CALL CHCKSD ( 'LEVEL', LEVEL, '~', 1.D0, LVLTOL, OK )

      IF ( .NOT. OK ) THEN
         RETURN
      END IF

C
C     The z-component of AXPT should be zero.
C
      CALL CHCKSD ( 'AXPT(3)', AXPT(3), '~', 0.D0, LVLTOL, OK )

      IF ( .NOT. OK ) THEN
         RETURN
      END IF

C
C     If we're still here, the near point is considered to be on
C     the ellipse.
C
C     Next step:  find the tangent direction at the near point.
C
      TANGNT(1) =  - AXPT(2) / SCLB**2
      TANGNT(2) =    AXPT(1) / SCLA**2
      TANGNT(3) =    0.D0

C
C     Map the scaled view point into the semi-axis frame.  To do
C     this we must shift the origin to the ellipse's center, then
C     rotate the vector.
C
      CALL VSUB ( VPSCL, CENTER, VTEMP )
      CALL MXV  ( AXMAT, VTEMP, VPAX   )

C
C     Find the vector from the near point to the view point.
C
      CALL VSUB ( VPAX,  AXPT,  AXVIEW )

C
C     AXVIEW should be orthogonal to the tangent vector.
C
      CALL CHCKSD ( '<AXVIEW,TANGNT>',
     .              VDOT(AXVIEW,TANGNT),
     .              '~',
     .              0.D0,
     .              DSTTOL,
     .              OK                  )

      IF ( .NOT. OK ) THEN
         RETURN
      END IF

C
C     Check the distance to the near point.
C
      CALL CHCKSD ( 'Distance',           D,       '~/',
     .              VNORM(AXVIEW)*SCALE,  DSTTOL,  OK    )

      IF ( .NOT. OK ) THEN
         RETURN
      END IF

C
C     The tests passed if we made it this far.
C
      T_ISNPEL = .TRUE.

      RETURN
      END
