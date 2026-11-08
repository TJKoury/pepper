!$Procedure F_NPELPT ( NPELPT tests )

      SUBROUTINE F_NPELPT ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine NPELPT. NPELPT find the nearest
!     point on an ellipse to a specified point.
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
!     This routine tests the SPICELIB routine NPELPT.
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
!-    TSPICE Version 1.2.0, 30-MAY-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 1.1.0, 22-FEB-2008 (NJB)
!
!        Loosened tolerances in test function T_ISNPEL.
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
      LOGICAL               T_ISNPEL

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 400 )

      INTEGER               NRANDM
      PARAMETER           ( NRANDM = 5000 )

!
!     Local Variables
!
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

!
!     Saved values
!

!
!     Initial values
!



!
!     In this test family, we encapsulate some of the geometric
!     tests within functions defined in this file. The function
!
!        T_ISNPEL
!
!     tests whether a 3-dpoint is on a specified ellipse embedded in
!     3-dimensional space:
!
!        - A point P = (p1,p2,p3) is "on" the ellipse with center C
!          and semi-axes SMAJOR and SMINOR if P lies in the plane
!          of the ellipse and if the "level surface parameter"
!
!             lambda =  x**2/A**2 + y**2/B**2
!
!          is sufficiently close to 1, where
!
!             A = ||SMAJOR||
!             B = ||SMINOR||
!
!             x = <P-C, SMAJOR/A> * SMAJOR / A
!             y = <P-C, SMINOR/B> * SMINOR / B
!
!
!     The function
!
!        T_ISNPEL
!
!     test whether the near point found by NPELPT has the property
!     that the outward normal at the near point can be extended to
!     intersect the input line at right angles.
!
!     T_ISNPEL also checks the distance of the near point from the line:
!     the distance is checked against the value obtained from NPLNPT.
!
!




!
!     Open the test family.
!
      CALL TOPEN ( 'F_NPELPT' )


!
!     Run some simple tests where the correct results can be
!     determined by inspection.
!
!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Ellipse is unit circle in x-y plane.  Point is '// &
              '(1,0,1)'
      CALL REPMI ( TITLE, '#', I, TITLE )
      CALL TCASE ( TITLE )

      CALL VPACK  ( 1.D0,    0.D0,  1.D0,   POINT )
      CALL VPACK  ( 1.D0,    0.D0,  0.D0,   V1     )
      CALL VPACK  ( 0.D0,    1.D0,  0.D0,   V2     )
      CALL VPACK  ( 0.D0,    0.D0,  0.D0,   CENTER )
      CALL CGV2EL ( CENTER,  V1,    V2,     ELLIPS )

      CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Test results.
!
      CALL VEQU ( V1, XPT )
      XDIST = 1.D0

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPT,   3, TIGHT, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,    0.D0,  OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Ellipse is unit circle in z=5 plane.  Point is '// &
              '(4,0,1)'
      CALL REPMI ( TITLE, '#', I, TITLE )
      CALL TCASE ( TITLE )

      CALL VPACK  ( 4.D0,    0.D0,  1.D0,   POINT )
      CALL VPACK  ( 1.D0,    0.D0,  0.D0,   V1     )
      CALL VPACK  ( 0.D0,    1.D0,  0.D0,   V2     )
      CALL VPACK  ( 0.D0,    0.D0,  5.D0,   CENTER )
      CALL CGV2EL ( CENTER,  V1,    V2,     ELLIPS )

      CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Test results.
!
      CALL VPACK  ( 1.D0,    0.D0,  5.D0,  XPT   )
      XDIST = 5.D0

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPT,   3, TIGHT, OK )
      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,    0.D0,  OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Ellipse is unit circle in z=5 plane.  Point is '// &
              '(0,0,5)'
      CALL REPMI ( TITLE, '#', I, TITLE )
      CALL TCASE ( TITLE )

      CALL VPACK  ( 0.D0,    0.D0,  5.D0,   POINT )
      CALL VPACK  ( 1.D0,    0.D0,  0.D0,   V1     )
      CALL VPACK  ( 0.D0,    1.D0,  0.D0,   V2     )
      CALL VPACK  ( 0.D0,    0.D0,  5.D0,   CENTER )
      CALL CGV2EL ( CENTER,  V1,    V2,     ELLIPS )

      CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Test results.
!
!     For this case, we don't know which point will be selected
!     as the near point.  Just check that it's a valid solution.
!
!
      VALPT = T_ISNPEL ( POINT, ELLIPS, PNEAR, DIST, OK )

      CALL CHCKSL ( 'Is near point on ellipse?', VALPT, .TRUE., OK )



!
!     Now for some more difficult cases.  We'll generate the ellipses
!     and planes using random numbers.  There are four components to
!     generate:
!
!        - random points
!        - random ellipse axes
!        - random ellipse centers
!        - random scale factors for the ellipse and point; these are
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
         SFACTR  =  10.D0 ** T_RANDD ( -290.D0, 290.D0, SEED )

!
!        Make up ellipse vectors.
!
         DO J = 1, 3

            CENTER(J) =  SFACTR  *   T_RANDD (  -1.D0,  1.D0,  SEED )
            V1(J)     =  SFACTR  *   T_RANDD (  -1.D0,  1.D0,  SEED )
            V2(J)     =  SFACTR  *   T_RANDD (  -1.D0,  1.D0,  SEED )

         END DO

!
!        Pack the ellipse.
!
         CALL CGV2EL ( CENTER, V1, V2, ELLIPS )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Find the semi-axes of this ellipse.
!
         CALL SAELGV ( V1, V2, SMAJOR, SMINOR )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We also need a point.
!
         POINT(1) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         POINT(2) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )
         POINT(3) =  SFACTR  * T_RANDD ( -1.D0, 1.D0, SEED )


         TITLE = 'NPELPT Random case #.  CENTER = (#, #, #); ' // &
                 'V1 = (#, #, #); V2 = (#, #, #); POINT = '    // &
                 '(#, #, #)'

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
!
!        Cross our fingers and toes and let 'er rip.
!
         CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure point we found lies on the ellipse.
!
         VALPT = T_ISNPEL ( POINT, ELLIPS, PNEAR, DIST, OK )

         CALL CHCKSL ( 'Is near point on ellipse?', VALPT, .TRUE., OK )

      END DO


!
!     NPELPT error cases:
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NPELPT: ellipse has one zero-length axis.' )

      CALL FILLD (   1.D1, 3, POINT )

      CALL VPACK  ( 0.D0,    0.D0,  0.D0,   V1     )
      CALL VPACK  ( 0.D0,    1.D0,  0.D0,   V2     )
      CALL VPACK  ( 2.D0,    2.D0,  2.D0,   CENTER )
      CALL CGV2EL ( CENTER,  V1,    V2,     ELLIPS )

      CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

!
!     Swap the semi-axes and repeat.
!
      CALL CGV2EL ( CENTER,  V2,    V1,     ELLIPS )
      CALL NPELPT ( POINT, ELLIPS, PNEAR, DIST )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END





!
!
!     Supporting function T_ISNPEL
!
      LOGICAL FUNCTION T_ISNPEL ( VIEWPT, ELLIPS, PNEAR, D, OK )
      IMPLICIT NONE
!
!     The utility function T_ISNPEL tests whether the "near point"
!     found by NPELPT satisfies the criterion that the line segment
!     connecting this point to the viewing point is orthogonal to the
!     ellipse's tangent vector at the near point.  The function also
!     test the viewpoint-ellipse distance and verifies that the
!     near point lies on the ellipse.

      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      DOUBLE PRECISION      VIEWPT ( 3 )
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      D
      LOGICAL               OK

!
!     SPICELIB functions
!
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

!
!     Local parameters
!
      DOUBLE PRECISION      LVLTOL
      PARAMETER           ( LVLTOL = 1.D-11 )


      DOUBLE PRECISION      DSTTOL
      PARAMETER           ( DSTTOL = 1.D-11 )

!
!     Local variables
!
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

!
!     Set an initial return value.
!
      T_ISNPEL = .FALSE.

!
!     We're going to express the vector from the ellipse's center
!     to the near point in a frame whose axes are parallel to the
!     ellipse's semi-axes.
!
!     Unpack ellipse.
!
      CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )

!
!     Scale view point, ellipse and near point.
!
      SCALE =  MAX ( VNORM(CENTER), VNORM(SMAJOR), VNORM(SMINOR) )

      CALL VSCL   ( 1.D0 / SCALE, VIEWPT, VPSCL )

      CALL VSCLIP ( 1.D0 / SCALE, CENTER        )
      CALL VSCLIP ( 1.D0 / SCALE, SMAJOR        )
      CALL VSCLIP ( 1.D0 / SCALE, SMINOR        )

      CALL VSCL   ( 1.D0 / SCALE, PNEAR,  SCLPT )

      SCLA  =  VNORM(SMAJOR)
      SCLB  =  VNORM(SMINOR)

!
!     Find the vector from the near point to the center.
!
      CALL VSUB ( SCLPT, CENTER, OFFSET )

!
!     Create the matrix that maps from the base frame to
!     a frame where the semi-axes are the x and y axes.
!
      CALL TWOVEC ( SMAJOR, 1, SMINOR, 2, AXMAT )

!
!     Map the offset vector into the semi-axis frame.
!
      CALL MXV ( AXMAT, OFFSET, AXPT )


!
!     Check the `level' of the near point.
!
      LEVEL  =    ( AXPT(1)**2 / SCLA**2 ) &
                + ( AXPT(2)**2 / SCLB**2 )

      CALL CHCKSD ( 'LEVEL', LEVEL, '~', 1.D0, LVLTOL, OK )

      IF ( .NOT. OK ) THEN
         RETURN
      END IF

!
!     The z-component of AXPT should be zero.
!
      CALL CHCKSD ( 'AXPT(3)', AXPT(3), '~', 0.D0, LVLTOL, OK )

      IF ( .NOT. OK ) THEN
         RETURN
      END IF

!
!     If we're still here, the near point is considered to be on
!     the ellipse.
!
!     Next step:  find the tangent direction at the near point.
!
      TANGNT(1) =  - AXPT(2) / SCLB**2
      TANGNT(2) =    AXPT(1) / SCLA**2
      TANGNT(3) =    0.D0

!
!     Map the scaled view point into the semi-axis frame.  To do
!     this we must shift the origin to the ellipse's center, then
!     rotate the vector.
!
      CALL VSUB ( VPSCL, CENTER, VTEMP )
      CALL MXV  ( AXMAT, VTEMP, VPAX   )

!
!     Find the vector from the near point to the view point.
!
      CALL VSUB ( VPAX,  AXPT,  AXVIEW )

!
!     AXVIEW should be orthogonal to the tangent vector.
!
      CALL CHCKSD ( '<AXVIEW,TANGNT>', &
                    VDOT(AXVIEW,TANGNT), &
                    '~', &
                    0.D0, &
                    DSTTOL, &
                    OK                  )

      IF ( .NOT. OK ) THEN
         RETURN
      END IF

!
!     Check the distance to the near point.
!
      CALL CHCKSD ( 'Distance',           D,       '~/', &
                    VNORM(AXVIEW)*SCALE,  DSTTOL,  OK    )

      IF ( .NOT. OK ) THEN
         RETURN
      END IF

!
!     The tests passed if we made it this far.
!
      T_ISNPEL = .TRUE.

      RETURN
      END
