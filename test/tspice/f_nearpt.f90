!$Procedure F_NEARPT ( NEARPT tests )

      SUBROUTINE F_NEARPT ( OK )

! $ Abstract
!
!     Exercise the SPICELIB routine NEARPT. NEARPT finds the nearest
!     point on the surface of an ellipsoid to a specified point.
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
!     This routine tests the SPICELIB routine NEARPT.
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
!-    TSPICE Version 2.0.0, 26-JUN-2014 (NJB)
!
!        Removed duplicate test case for input point (1,1,1)
!        and sphere. Added case for input point at center of
!        sphere. Removed declaration of unused parameter NSIMPL.
!
!     Last update was 08-SEP-2010 (NJB)
!
!        Added simple test cases, in particular to
!        check prolate case with on-long-axis view point.
!
!-    TSPICE Version 1.1.0, 24-FEB-2008 (NJB)
!
!        Loosened tolerance for initial NEARPT interior
!        random cases. Changed ATOL from 1.D-15 to 1.D-14.
!
!-    TSPICE Version 1.0.0, 14-NOV-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VNORM

!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD
      LOGICAL               T_ISNPPT
      LOGICAL               T_ISEDPT


!
!     Local parameters
!
!
!     SPICELIB ellipse upper bound.
!
      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )

!
!     Altitude tolerance:  this is applied to the sum of
!     altitude of the view point and the maximum of the
!     ellipsoid's axis lengths.  The reason the tolerance is not
!     applied to the altitude directly is that the altitude may have
!     large relative errors, but still be accurate, when the view point
!     is close to the surface.
!
      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL   = 1.D-14 )

!
!     Angular separation tolerance:  this applies to the angular
!     separation between the near point-to-view point vector and
!     the properly signed normal vector at the near point.
!
      DOUBLE PRECISION      STOL
      PARAMETER           ( STOL   = 1.D-11 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-13 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-9 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 400 )

      INTEGER               NXRAND
      PARAMETER           ( NXRAND = 4000 )

!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      GV1    ( 3 )
      DOUBLE PRECISION      GV2    ( 3 )
      DOUBLE PRECISION      IPALT
      DOUBLE PRECISION      IPNEAR ( 3 )
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SFACTR
      DOUBLE PRECISION      SRFX   ( 3 )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      VIEWPT ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               I
      INTEGER               PINDEX
      INTEGER               SEED

      LOGICAL               FOUND
      LOGICAL               ISNEAR
      LOGICAL               VALPT

!
!     Saved values
!
      SAVE                  ORIGIN

!
!     Initial values
!
      DATA                  ORIGIN / 3 * 0.D0 /





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
!        T_ISNPPT
!
!     test whether the near point found by NEARPT has the property
!     that the outward normal at the near point can be extended to
!     touch the input point.
!
!     T_ISNPPT also checks the distance of the near point from the
!     point: the distance is checked against the value obtained from
!     NPLNPT.

!




!
!     Open the test family.
!
      CALL TOPEN ( 'F_NEARPT' )


!
!     NEARPT simple cases:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: ellipsoid is sphere; point is at origin.' )

      CALL CLEARD ( 3, VIEWPT )

      CALL NEARPT ( VIEWPT, 1.D0, 1.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSD ( 'ALT', ALT, '~', -1.D0, ATOL, OK )

      CALL CHCKSD ( 'VNORM(PNEAR)', VNORM(PNEAR), '~', 1.D0, ATOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: ellipsoid is sphere; point is exterior ' &
      //            'on +Z axis.'                                   )

      CALL VPACK ( 0.D0, 0.D0, 5.D0, VIEWPT )
      CALL VPACK ( 0.D0, 0.D0, 1.D0, XPT    )


      CALL NEARPT ( VIEWPT, 1.D0, 1.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~', XPT, 3, ATOL, OK )

      CALL CHCKSD ( 'ALT', ALT, '~', 4.D0, ATOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: ellipsoid is sphere; point is interior ' &
      //            'on ray generated by (1,1,1)'                    )

      S = 1/SQRT(3.D0)

      CALL VPACK ( S/2.D0, S/2.D0, S/2.D0, VIEWPT )


      CALL VPACK ( S, S, S, XPT    )


      CALL NEARPT ( VIEWPT, 1.D0, 1.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'PNEAR', PNEAR, '~~', XPT, 3, ATOL, OK )

      CALL CHCKSD ( 'ALT',   ALT,   '~',  -0.5D0, ATOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: ellipsoid is triaxial; point is on ' &
      //            'selected outward normal.'                   )
!
!     Start with the unit sphere and a point on the ray (1,1,1);
!     multiply all participants by diag[3,2,1].
!
      A = 3.D0
      B = 2.D0
      C = 1.D0

      S = 1/SQRT(3.D0)

      CALL VPACK ( A*S, B*S, C*S, SRFX )

!
!     Create the outward unit normal at SRFX.
!
      CALL VPACK  ( SRFX(1)/A**2, SRFX(2)/B**2, SRFX(3)/C**2, NORMAL )

      CALL VHATIP ( NORMAL )

!
!     Create a view point at altitude 1 along the normal.
!
      CALL VADD   ( SRFX, NORMAL, VIEWPT )

      CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSD ( 'ALT', ALT, '~', 1.D0, ATOL, OK )

!
!     The near point should be the surface point at which the
!     normal was generated.
!
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~', SRFX, 3, ATOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: ellipsoid is triaxial; point is on ' &
      //            'selected inward normal.'                   )
!
!     Start with the unit sphere and a point on the ray (1,1,1);
!     multiply all participants by diag[3,2,1].
!
      A = 3.D0
      B = 2.D0
      C = 1.D0

      S = 1/SQRT(3.D0)

      CALL VPACK ( A*S, B*S, C*S, SRFX )

!
!     Create the outward unit normal at SRFX.
!
      CALL VPACK  ( SRFX(1)/A**2, SRFX(2)/B**2, SRFX(3)/C**2, NORMAL )

      CALL VHATIP ( NORMAL )

!
!     Create a view point at altitude 1 along the normal.
!
      CALL VLCOM   ( 1.D0, SRFX, -1.D-1, NORMAL, VIEWPT )

      CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSD ( 'ALT', ALT, '~', -1.D-1, ATOL, OK )

!
!     The near point should be the surface point at which the
!     normal was generated.
!
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~', SRFX, 3, ATOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: ellipsoid is bi-axial; point is at ' // &
                    'origin.'                                     )

      CALL CLEARD ( 3, VIEWPT )

      CALL NEARPT ( VIEWPT, 2.D0, 2.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSD ( 'ALT', ALT, '~', -1.D0, ATOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: ellipsoid is tri-axial; point is at ' // &
                    'origin.'                                     )

      CALL CLEARD ( 3, VIEWPT )

      CALL NEARPT ( VIEWPT, 3.D0, 2.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSD ( 'ALT', ALT, '~', -1.D0, ATOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: interior prolate case. Viewpoint is ' &
      //            'on the long axis; solution is off-axis.'      )


      A = 10.D0
      B =  5.D0
      C =  5.D0

      CALL VPACK  ( 7.D0, 0.D0, 0.D0, VIEWPT )

      CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure the solution is on the ellipsoid.
!
      VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

      CALL CHCKSL ( 'Is on ellipsoid', VALPT, .TRUE., OK )

!
!     Make sure the solution satisfies the orthogonality
!     condition.
!
      ISNEAR =  T_ISNPPT ( VIEWPT, A,   B,    C, &
                           PNEAR,  ALT, ATOL, STOL, OK )

      CALL CHCKSL ( 'Is critical point', ISNEAR, .TRUE., OK )

!
!     Make sure altitude is greater than -2.95.
!
      CALL CHCKSD ( 'ALT', ALT, '>', -2.95D0, ATOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: interior prolate case. Repeat previous ' &
      //            'case with point closer to the origin.'          )


      A = 10.D0
      B =  5.D0
      C =  5.D0

      CALL VPACK  ( 1.D0, 0.D0, 0.D0, VIEWPT )

      CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure the solution is on the ellipsoid.
!
      VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

      CALL CHCKSL ( 'Is on ellipsoid', VALPT, .TRUE., OK )

!
!     Make sure the solution satisfies the orthogonality
!     condition.
!
      ISNEAR =  T_ISNPPT ( VIEWPT, A,   B,    C, &
                           PNEAR,  ALT, ATOL, STOL, OK )

      CALL CHCKSL ( 'Is critical point', ISNEAR, .TRUE., OK )

!
!     Make sure altitude is greater than -5.
!
      CALL CHCKSD ( 'ALT', ALT, '>', -5.D0, ATOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: interior prolate case. Repeat previous ' &
      //            'case with point near axis endpoint.'           )


      A = 10.D0
      B =  5.D0
      C =  5.D0

      CALL VPACK  ( 9.D0, 0.D0, 0.D0, VIEWPT )

      CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure the solution is on the ellipsoid.
!
      VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

      CALL CHCKSL ( 'Is on ellipsoid', VALPT, .TRUE., OK )

!
!     Make sure the solution satisfies the orthogonality
!     condition.
!
      ISNEAR =  T_ISNPPT ( VIEWPT, A,   B,    C, &
                           PNEAR,  ALT, ATOL, STOL, OK )

      CALL CHCKSL ( 'Is critical point', ISNEAR, .TRUE., OK )

!
!     Make sure near point is axis endpoint.
!
      CALL CHCKSD ( 'ALT', ALT, '>', -1.D0, ATOL, OK )

      CALL VPACK  ( A, 0.D0, 0.D0, XPT )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~', XPT, 3, ATOL, OK )




!
! --- Case: ------------------------------------------------------
!



!
!     First set of random test cases:  random ellipsoids and random
!     *exterior* viewing points.
!
      SEED = -1

      DO I = 1, NXRAND
!
! --- Case: ------------------------------------------------------
!
!
!        Make up ellipsoid axis lengths.
!
         A =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
         B =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
         C =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )

!
!        Create viewing point.
!
         VIEWPT(1) = 10.D0 ** T_RANDD ( -3.D0,  100.D0, SEED )
         VIEWPT(2) = 10.D0 ** T_RANDD ( -3.D0,  100.D0, SEED )
         VIEWPT(3) = 10.D0 ** T_RANDD ( -3.D0,  100.D0, SEED )

!
!        Find the level surface parameter of the view point.
!
         LEVEL =    (VIEWPT(1)/A)**2.D0 &
                  + (VIEWPT(2)/B)**2.D0 &
                  + (VIEWPT(3)/C)**2.D0

!
!        If the viewing point is inside the ellipsoid, replace it
!        with the surface point found by scaling up the viewing
!        point.
!
         IF ( LEVEL .LT. 1.D0 ) THEN
!
!           FOUND will always be .TRUE. in this case.
!
            CALL SURFPT ( ORIGIN, VIEWPT, A, B, C, SRFX, FOUND )
!
!           Scale the point up a tad for safety.
!
            CALL VSCL ( (1.D0 + 1.D-15),   SRFX,   VIEWPT )

         END IF


         TITLE = 'Initial NEARPT exterior random case #. '   // &
                 'For this set of cases, we don''t use '     // &
                 'extreme scale differences for the axis '   // &
                 'lengths:  range is [0.1 : 10]. VIEWPT '    // &
                 'components are in range [0.001 : 1.e150]. '// &
                 'A = #; B = #; C = #; VIEWPT = (#,#,#).'

         CALL REPMI ( TITLE,  '#', I,             TITLE )
         CALL REPMD ( TITLE,  '#', A,         14, TITLE )
         CALL REPMD ( TITLE,  '#', B,         14, TITLE )
         CALL REPMD ( TITLE,  '#', C,         14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(1), 14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(2), 14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(3), 14, TITLE )

         CALL TCASE ( TITLE )

!
!        Cross our fingers and toes and let 'er rip.
!
         CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( OK ) THEN
!
!           Make sure the near point we found belongs to
!           the surface of the ellipsoid.
!
            VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

            CALL CHCKSL ( 'Is near point on surface?', &
                          VALPT, .TRUE., OK           )

!
!           The outward normal at the near point should point towards
!           the view point.
!
            ISNEAR = T_ISNPPT ( VIEWPT, A,   B,    C, &
                                PNEAR,  ALT, ATOL, STOL, OK  )

            CALL CHCKSL ( 'Is signed outward normal parallel to  ' // &
                          'near point-view point vector?  Is '     // &
                          'distance correct?', &
                          ISNEAR, .TRUE., OK )

         END IF

      END DO




!
!     Second set of random test cases:  random ellipsoids and random
!     *interior* viewing points.  At this point, we don't verify
!     that the solutions are absolute minima, but we do verify
!     that they are critical points.
!
      SEED = -1

      DO I = 1, NXRAND
!
! --- Case: ------------------------------------------------------
!
!
!        Make up ellipsoid axis lengths.
!
         A =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
         B =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
         C =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )

!
!        Create viewing point.
!
         VIEWPT(1) = T_RANDD ( -1.D0,  1.D0, SEED )
         VIEWPT(2) = T_RANDD ( -1.D0,  1.D0, SEED )
         VIEWPT(3) = T_RANDD ( -1.D0,  1.D0, SEED )

!
!        Find the level surface parameter of the view point.
!
         LEVEL =    (VIEWPT(1)/A)**2.D0 &
                  + (VIEWPT(2)/B)**2.D0 &
                  + (VIEWPT(3)/C)**2.D0

!
!        If the viewing point is outside the ellipsoid, replace it
!        with an interior point found by scaling down the surface
!        point corresponding to the viewing point.
!
         IF ( LEVEL .GE. 1.D0 ) THEN
!
!           FOUND will always be .TRUE. in this case.
!
            CALL SURFPT ( ORIGIN, VIEWPT, A, B, C, SRFX, FOUND )
!
!           Scale the point down to make it usable.
!
            SFACTR = T_RANDD ( 0.D0,  1.D0-1.D-6, SEED )

            CALL VSCL ( SFACTR, SRFX, VIEWPT )

         END IF


         TITLE = 'Initial NEARPT interior random case #. '   // &
                 'For this set of cases, we don''t use '     // &
                 'extreme scale differences for the axis '   // &
                 'lengths:  range is [0.1 : 10]. VIEWPT '    // &
                 'components are in range [0.001 : 1.e150]. '// &
                 'A = #; B = #; C = #; VIEWPT = (#,#,#).'

         CALL REPMI ( TITLE,  '#', I,             TITLE )
         CALL REPMD ( TITLE,  '#', A,         14, TITLE )
         CALL REPMD ( TITLE,  '#', B,         14, TITLE )
         CALL REPMD ( TITLE,  '#', C,         14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(1), 14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(2), 14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(3), 14, TITLE )

         CALL TCASE ( TITLE )

!
!        Cross our fingers and toes and let 'er rip.
!
         CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure the near point we found is contained in
!        both the plane and the surface of the ellipsoid.
!
         IF ( OK ) THEN
!
!           Make sure the near point we found belongs to
!           the surface of the ellipsoid.
!
            VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

            CALL CHCKSL ( 'Is near point on surface?', &
                          VALPT, .TRUE., OK           )

!
!           The inward normal at the near point should point towards
!           the view point.
!
            ISNEAR = T_ISNPPT ( VIEWPT, A,   B,    C, &
                                PNEAR,  ALT, ATOL, STOL, OK  )

            CALL CHCKSL ( 'Is signed normal parallel to  ' // &
                          'near point-view point vector?  Is '     // &
                          'distance correct?', &
                          ISNEAR, .TRUE., OK )

         END IF

      END DO



!
!     Third set of random test cases:  random ellipsoids and random
!     viewing points lying on principal planes.
!
      SEED = -1

      DO I = 1, NXRAND
!
! --- Case: ------------------------------------------------------
!
!
!        Make up ellipsoid axis lengths.
!
         A =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
         B =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )
         C =  10.D0 ** T_RANDD ( -1.D0,   1.D0, SEED )

!
!        Do a few tests to check for problems in the special
!        cases where the axes are not distinct.
!
         IF ( I .LT. 200 ) THEN
!
!           Turn the ellipsoid into a sphere.
!
            B = A
            C = A

         ELSE IF ( I .LT. 400 ) THEN
!
!           The ellipsoid is symmetric about X.
!
            B = C

         ELSE IF ( I .LT. 600 ) THEN
!
!           The ellipsoid is symmetric about Y.
!
            C = A

         ELSE IF ( I .LT. 800 ) THEN
!
!           The ellipsoid is symmetric about Z.
!
            B = A

         END IF

!
!        Create viewing point.  We pick a plane index, a scale factor
!        to give us the radius of the point, and an angular argument.
!
         PINDEX = NINT (  T_RANDD ( 1.D0,  3.D0, SEED )  )
         SCALE  =         T_RANDD ( 0.D0,  1.D0, SEED )
         THETA  =         T_RANDD ( -PI(), PI(), SEED )


         IF ( PINDEX .EQ. 1 ) THEN
!
!           The point lies in the y-z plane.
!
            VIEWPT(1) = 0.D0
            VIEWPT(2) = SCALE * B * COS(THETA)
            VIEWPT(3) = SCALE * C * SIN(THETA)

!
!           In addition to creating a viewing point, create generating
!           vectors for the intersection ellipse defined by the
!           ellipsoid and the y-z plane.  We'll use this ellipse later.
!
            CALL VPACK  ( 0.D0, B,    0.D0, GV1 )
            CALL VPACK  ( 0.D0, 0.D0, C,    GV2 )


         ELSE IF ( PINDEX .EQ. 2 ) THEN
!
!           The point lies in the x-z plane.
!
            VIEWPT(1) = SCALE * A * COS(THETA)
            VIEWPT(2) = 0.D0
            VIEWPT(3) = SCALE * C * SIN(THETA)

            CALL VPACK  ( A,    0.D0, 0.D0, GV1 )
            CALL VPACK  ( 0.D0, 0.D0, C,    GV2 )

         ELSE
!
!           The point lies in the x-Y plane.
!
            VIEWPT(1) = SCALE * A * COS(THETA)
            VIEWPT(2) = SCALE * B * SIN(THETA)
            VIEWPT(3) = 0.D0

            CALL VPACK  ( A,    0.D0, 0.D0, GV1 )
            CALL VPACK  ( 0.D0, B,    0.D0, GV2 )

         END IF

!
!        Create the ellipse from the generating vectors.
!
         CALL CGV2EL ( ORIGIN, GV1, GV2, ELLIPS )


!
!        If the viewing point is outside the ellipsoid, replace it
!        with an interior point found by scaling down the surface
!        point corresponding to the viewing point.
!
!        Find the level surface parameter of the view point.
!
         LEVEL =    (VIEWPT(1)/A)**2.D0 &
                  + (VIEWPT(2)/B)**2.D0 &
                  + (VIEWPT(3)/C)**2.D0

         IF ( LEVEL .GE. 1.D0 ) THEN

            SFACTR = 1.D0 - 1.D-6

            CALL VSCLIP ( SFACTR, VIEWPT )

         END IF


         TITLE = 'NEARPT interior principal plane random '      // &
                 'case #.  For this set of cases, we don''t '   // &
                 'use extreme scale differences for the axis '  // &
                 'lengths:  range is [0.1 : 10]. VIEWPT '       // &
                 'components are in range [0.001 : 1.e150]. '   // &
                 'A = #; B = #; C = #; VIEWPT = (#,#,#).'

         CALL REPMI ( TITLE,  '#', I,             TITLE )
         CALL REPMD ( TITLE,  '#', A,         14, TITLE )
         CALL REPMD ( TITLE,  '#', B,         14, TITLE )
         CALL REPMD ( TITLE,  '#', C,         14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(1), 14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(2), 14, TITLE )
         CALL REPMD ( TITLE,  '#', VIEWPT(3), 14, TITLE )

         CALL TCASE ( TITLE )

!
!        Cross our fingers and toes and let 'er rip.
!
         CALL NEARPT ( VIEWPT, A, B, C, PNEAR, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Make sure the near point we found is contained in
!        both the plane and the surface of the ellipsoid.
!
         IF ( OK ) THEN
!
!           Make sure the near point we found belongs to
!           the surface of the ellipsoid.
!
            VALPT = T_ISEDPT ( PNEAR, A, B, C, OK )

            CALL CHCKSL ( 'Is near point on surface?', &
                          VALPT, .TRUE., OK           )

!
!           The inward normal at the near point should point towards
!           the view point.  For these cases we use a more lenient
!           tolerance for the angular separation.  We also use a
!           slightly looser altitude tolerance.
!
            ISNEAR = T_ISNPPT ( VIEWPT, A,   B,     C, &
                                PNEAR,  ALT, TIGHT, MEDIUM, OK  )

            CALL CHCKSL ( 'Is signed normal parallel to  ' // &
                          'near point-view point vector?  Is '     // &
                          'distance correct?', &
                          ISNEAR, .TRUE., OK )

            IF ( PNEAR(PINDEX) .NE. 0.D0 ) THEN
!
!              We have a solution that doesn't lie in the same plane
!              as the viewing point.  This may well be correct, but
!              we'll check it.  Find the in-plane solution and
!              compare altitudes.
!
               CALL NPELPT ( VIEWPT, ELLIPS, IPNEAR, IPALT )

!
!              We expect the in-plane "altitude" to be greater than
!              or equal to that of the out-of-plane near point.
!
               CALL CHCKSD ( 'IPALT', IPALT, '>=', ALT, 0.D0, OK )

!               WRITE (*,*) 'IPALT - ALT = ', IPALT - ALT

!               WRITE (*,*) 'Out-of-plane solution'
!               WRITE (*,*) 'view point = ', VIEWPT
!               WRITE (*,*) 'near point = ', PNEAR
!               WRITE (*,*) 'altitude   = ', ALT
!               WRITE (*,*) '======================='

            END IF

         END IF

      END DO







!
!     NEARPT error cases:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: ellipsoid has one zero-length axis.' )

      CALL FILLD (   1.D1, 3, VIEWPT )

      CALL NEARPT ( VIEWPT, 0.D0, 1.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL NEARPT ( VIEWPT, 1.D0, 0.D0, 1.D0, PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL NEARPT ( VIEWPT, 1.D0, 1.D0, 0.D0, PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: ellipsoid has one negative-length axis.' )

      CALL NEARPT ( VIEWPT, -1.D0, 1.D0,   1.D0,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL NEARPT ( VIEWPT,  1.D0, -1.D0,  1.D0,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL NEARPT ( VIEWPT,  1.D0,  1.D0, -1.D0,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: ratio of longest to shortest axes ' // &
                    'is too large.'                              )

      A = 1.D-6
      B = 1.D150
      C = 1.D0

      CALL NEARPT ( VIEWPT,  A, B, C,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL NEARPT ( VIEWPT,  C, A, B,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL NEARPT ( VIEWPT,  B, C, A,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'NEARPT: product of scaled axis magnitude ' // &
                    'and scaled POSITN component is too large.' )

      A = 1.D-6
      B = 1.D75
      C = 1.D0

      CALL VPACK  ( A, B**3.D0, C, VIEWPT )
      CALL NEARPT ( VIEWPT,  A, B, C,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(INPUTSTOOLARGE)', OK )

      CALL VPACK  ( C, A, B**3.D0, VIEWPT )
      CALL NEARPT ( VIEWPT,  C, A, B,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(INPUTSTOOLARGE)', OK )

      CALL VPACK  ( B**3.D0, C, A, VIEWPT )
      CALL NEARPT ( VIEWPT,  B, C, A,  PNEAR, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(INPUTSTOOLARGE)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END





!
!     The utility function T_ISNPPT tests whether the "near point"
!     found by NEARPT satisfies the criterion that the appropriately
!     signed surface normal at the near point has small angular
!     separation from the vector from the near point to view point.
!
      LOGICAL FUNCTION T_ISNPPT ( V, A, B, C, X, ALT, ATOL, STOL, OK )
      IMPLICIT NONE

      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      ATOL
      DOUBLE PRECISION      STOL
      LOGICAL               OK

!
!     SPICELIB functions
!
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

!
!     Local parameters
!

!
!     Local variables
!
      DOUBLE PRECISION      ALTSGN
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      MAXAX
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PVEC   ( 3 )
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      XALT

!
!     Executable code.
!

!
!     Find the level surface parameter of the view point.
!
      LEVEL = (V(1)/A)**2.D0 + (V(2)/B)**2.D0 + (V(3)/C)**2.D0

!
!     Set the sign of the altitude of the near point.
!
      IF ( LEVEL .GE. 1.D0 ) THEN
         ALTSGN =  1.D0
      ELSE
         ALTSGN = -1.D0
      END IF

!
!     Find the vector from the near point to the view point.
!
      CALL VSUB ( V, X, PVEC )

!
!     Checking the altitude can be problematic, since small altitudes
!     can be accurate but have a large relative error.  Instead,
!     check the relative error in the sum of the altitude and
!     the maximum axis.
!
      MAXAX = MAX ( A, B, C )

      XALT  = MAXAX + ALTSGN * VNORM( PVEC )

      CALL CHCKSD ( 'MAXAX+ALT', &
                     MAXAX+ALT,   '~/',  XALT, ATOL,   OK )

      IF ( .NOT. OK ) THEN

         T_ISNPPT = .FALSE.
         RETURN

      END IF

!
!     Obtain outward ellipsoid surface unit normal at the near point.
!
      CALL SURFNM ( A, B, C, X, NORMAL )

!
!     Adjust sign of normal vector according to whether view point is
!     inside or outside the ellipsoid.
!
      CALL VSCLIP ( ALTSGN, NORMAL )

!
!     Find angular separation of sign-adjusted normal and PVEC.
!
      SEP = VSEP ( NORMAL, PVEC )

!
!     Check the angular separation.
!
      CALL CHCKSD ( 'SEP', SEP, '~', 0.D0 , STOL, OK )

      T_ISNPPT = OK

      RETURN
      END




!
!     Supporting function T_ISEDPT
!
!     This function is defined in the test family F_NPEDLN.
!     The calling sequence is
!
!        LOGICAL FUNCTION T_ISEDPT ( PT, A, B, C, OK )
!
!
!        Test whether a point is on a specified ellipsoid.
!
!        DOUBLE PRECISION      PT ( 3 )
!        DOUBLE PRECISION      A
!        DOUBLE PRECISION      B
!        DOUBLE PRECISION      C
!        LOGICAL               OK
!
