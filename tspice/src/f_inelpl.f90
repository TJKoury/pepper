!$Procedure F_INELPL ( Ellipse/plane intersection routine tests )

      SUBROUTINE F_INELPL ( OK )

! $ Abstract
!
!     Exercise INELPL, the SPICELIB routine that slices ellipses
!     with planes.
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
!     This routine tests the SPICELIB routine INELPL.
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
!-    TSPICE Version 2.0.0, 22-FEB-2008 (NJB)
!
!        Added test cases to exercise new exception-handling
!        logic of INELPL. Loosened tolerances in test function
!        T_ISPXEL.
!
!-    TSPICE Version 1.0.0, 15-NOV-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!

!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD
      LOGICAL               T_ISPXEL

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 800 )

      INTEGER               NRANDM
      PARAMETER           ( NRANDM = 2000 )

      INTEGER               UBEL
      PARAMETER           ( UBEL   =  9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   =  4 )

!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      BETA
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      PPT    ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SFACTR
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      V1     ( 3 )
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      XPT    ( 3, 2 )
      DOUBLE PRECISION      XPTS   ( 3, 2 )

      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               SEED

      LOGICAL               VALXPT

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
!        T_ISPXEL
!
!     test whether a set of points constitute the intersection of a
!     specified plane and ellipse.  It does this by testing the points
!     to see whether they are
!
!        - On the ellipse:  a 2-d point P = (x,y) is "on" the
!          ellipse with semi-axis lengths a, b if
!          the "level surface parameter"
!
!             lambda = x**2/a**2 + y**2/b**2
!
!          is sufficiently close to 1.  In three dimensions, the
!          point must lie in the plane of the ellipse and must
!          satisfy the above equation when mapped to a basis
!          spanned by the ellipse's semi-axes.
!
!
!        - On the plane:  a point P = (x,y,z) is "on" the
!          plane with normal vector N and constant C if
!          the dot product
!
!             < N, P >
!
!          is sufficiently close to C.
!
!




!
!     Open the test family.
!
      CALL TOPEN ( 'F_INELPL' )



!
!     Perform a number of simple INELPL tests for which we can compute
!     the intersection points in advance.
!
! --- Case: ------------------------------------------------------
!
!
      TITLE = 'Ellipse and plane are parallel and disjoint.'
      CALL TCASE ( TITLE )

      CALL CLEARD ( 3, CENTER )
      CALL VPACK  ( 2.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 1.D0, 0.D0, SMINOR )

!
!     Create the ellipse.
!
      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 1.D0

!
!     Create the plane.
!
      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Find the intersection.
!
      CALL INELPL (  ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intersection points.
!
      CALL CHCKSI ( 'N', N, '=', 0, 0, OK )

!
! --- Case: ------------------------------------------------------
!
!
      TITLE = 'Ellipse and plane are orthogonal and disjoint.'
      CALL TCASE ( TITLE )

      CALL CLEARD ( 3, CENTER )
      CALL VPACK  ( 2.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 1.D0, 0.D0, SMINOR )

!
!     Create the ellipse.
!
      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL VPACK  ( 1.D0, 0.D0, 0.D0, NORMAL )
      CONST = 3.D0

!
!     Create the plane.
!
      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Find the intersection.
!
      CALL INELPL (  ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intersection points.
!
      CALL CHCKSI ( 'N', N, '=', 0, 0, OK )


!
! --- Case: ------------------------------------------------------
!
!
      TITLE = 'Plane contains ellipse.'
      CALL TCASE ( TITLE )

      CALL CLEARD ( 3, CENTER )
      CALL VPACK  ( 2.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 1.D0, 0.D0, SMINOR )

!
!     Create the ellipse.
!
      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

!
!     Create the plane.
!
      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Find the intersection.
!
      CALL INELPL (  ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intersection points.
!
      CALL CHCKSI ( 'N', N, '=', -1, 0, OK )


!
! --- Case: ------------------------------------------------------
!
!
      TITLE = 'Plane and ellipse intersect in a single point.'
      CALL TCASE ( TITLE )

      CALL CLEARD ( 3, CENTER )
      CALL VPACK  ( 1.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 1.D0, 0.D0, SMINOR )

!
!     Create the ellipse.
!
      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL VPACK  ( 1.D0, 0.D0, 0.D0, NORMAL )
      CONST = 1.D0

!
!     Create the plane.
!
      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Find the intersection.
!
      CALL INELPL (  ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intersection points.
!
      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Check the intersection points.
!
      CALL VPACK  ( 1.D0, 0.D0, 0.D0, XPT )

      CALL CHCKAD ( 'XPT1', XPTS(1,1), '~', XPT, 3, TIGHT, OK )

      CALL CHCKAD ( 'XPT2', XPTS(1,2), '~', XPT, 3, TIGHT, OK )


!
! --- Case: ------------------------------------------------------
!
!
      TITLE = 'Plane and ellipse intersect along the line x = 0.5.'
      CALL TCASE ( TITLE )

      CALL CLEARD ( 3, CENTER )
      CALL VPACK  ( 1.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 1.D0, 0.D0, SMINOR )

!
!     Create the ellipse.
!
      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL VPACK  ( 1.D0, 0.D0, 0.D0, NORMAL )
      CONST = 5.D-1

!
!     Create the plane.
!
      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Find the intersection.
!
      CALL INELPL (  ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intersection points.
!
      CALL CHCKSI ( 'N', N, '=', 2, 0, OK )

!
!     Check the intersection points.
!
      IF ( XPTS(2,1) .LT. 0.D0 ) THEN
         S = -1.D0
      ELSE
         S =  1.D0
      END IF

      CALL VPACK  ( 5.D-1,  S*SQRT(3.D0)/2.D0, 0.D0, XPT )

      CALL CHCKAD ( 'XPT1', XPTS(1,1), '~', XPT, 3, TIGHT, OK )


      CALL VPACK  ( 5.D-1, -S*SQRT(3.D0)/2.D0, 0.D0, XPT )

      CALL CHCKAD ( 'XPT2', XPTS(1,2), '~', XPT, 3, TIGHT, OK )



!
!     Now for some more difficult cases.  We'll generate the ellipses
!     and planes using random numbers.  There are four components to
!     generate:
!
!        - random plane normal vectors
!        - random ellipse axes
!        - random plane constants
!        - random scale factors for the ellipse and plane; these are
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
!        Generate a random point in the rectangle spanned by
!        the linear combinations
!
!           CENTER + alpha*SMAJOR + beta*SMINOR,
!
!               -1 <  alpha, beta  <  1
!                  -               -
!
         ALPHA  =  T_RANDD (  -1.D0,  1.D0,  SEED )
         BETA   =  T_RANDD (  -1.D0,  1.D0,  SEED )


         CALL VLCOM3 ( 1.D0, CENTER, ALPHA, SMAJOR, BETA, SMINOR, PPT )

!
!        We gotta have a plane normal vector.
!
         NORMAL(1) =  T_RANDD ( -1.D0, 1.D0, SEED )
         NORMAL(2) =  T_RANDD ( -1.D0, 1.D0, SEED )
         NORMAL(3) =  T_RANDD ( -1.D0, 1.D0, SEED )

         CALL VHATIP ( NORMAL )

         TITLE = 'INELPL Random case #.  CENTER = (#, #, #); ' // &
                 'V1 = (#, #, #); V2 = (#, #, #); PPT = '      // &
                 '(#, #, #); NORMAL = (#, #, #)'

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
         CALL REPMD ( TITLE, '#', PPT(1),    14, TITLE )
         CALL REPMD ( TITLE, '#', PPT(2),    14, TITLE )
         CALL REPMD ( TITLE, '#', PPT(3),    14, TITLE )
         CALL REPMD ( TITLE, '#', NORMAL(1), 14, TITLE )
         CALL REPMD ( TITLE, '#', NORMAL(2), 14, TITLE )
         CALL REPMD ( TITLE, '#', NORMAL(3), 14, TITLE )

         CALL TCASE ( TITLE )

!
!        Form the plane.
!
         CALL NVP2PL ( NORMAL, PPT, PLANE )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Cross our fingers and toes and let 'er rip.
!
         CALL INELPL ( ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( N .GT. 0 ) THEN
!
!           Make sure the intersection points we found are
!           contained in both the plane and the the
!           ellipse.
!
            VALXPT  = T_ISPXEL ( ELLIPS, PLANE, N, XPTS, OK )

            CALL CHCKSL ( 'Is intersection valid?', &
                           VALXPT, .TRUE., OK       )

         END IF


      END DO


!
!     INELPL non-error exception cases:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INELPL: ellipse has one zero-length axis. ' &
      //           'and intersects plane in one point.'        )

!
!     Create the plane.
!
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the ellipse.
!
      CALL VPACK  ( 1.D0, 2.D0, 3.D0, CENTER )
      CALL VPACK  ( 0.D0, 0.D0, 4.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL INELPL (ELLIPS, PLANE,  N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', N, '=', 1, 0, OK )
!
!     Check intersection points directly (T_ISPXEL isn't
!     designed to work with degenerate ellipses).
!
      CALL VPACK ( 1.D0, 2.D0, 0.D0, XPT )

      CALL CHCKAD ( 'XPTS 1', XPTS(1,1), '~', XPT, 3, TIGHT, OK )
      CALL CHCKAD ( 'XPTS 2', XPTS(1,2), '~', XPT, 3, TIGHT, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INELPL: ellipse has one zero-length axis. ' &
      //           'and lies in plane.'                        )

!
!     Create the plane.
!
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the ellipse.
!
!     The ellipse and plane are chosen so that round-off
!     errors should not prevent INELPL from deciding that
!     the ellipse lies in the plane.
!
      CALL VPACK  ( 1.D0, 2.D0, 0.D0, CENTER )
      CALL VPACK  ( 4.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL INELPL ( ELLIPS, PLANE,  N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', N, '=', -1, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INELPL: ellipse has one zero-length axis. ' &
      //           'and is disjoint from, but parallel to, ' &
      //           'the plane.'                                )

!
!     Create the plane.
!
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the ellipse.
!
      CALL VPACK  ( 1.D0, 2.D0, -5.D0, CENTER )
      CALL VPACK  ( 4.D0, 0.D0,  0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 0.D0,  0.D0, SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL INELPL ( ELLIPS, PLANE,  N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', N, '=', 0, 0, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INELPL: ellipse has one zero-length axis. ' &
      //           'and is disjoint from, and not parallel to, ' &
      //           'the plane.'                                )

!
!     Create the plane.
!
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the ellipse.
!
      CALL VPACK  ( 1.D0, 2.D0, -5.D0, CENTER )
      CALL VPACK  ( 4.D0, 0.D0,  1.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 0.D0,  0.D0, SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL INELPL ( ELLIPS, PLANE,  N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', N, '=', 0, 0, OK )





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INELPL: ellipse is a single point ' &
      //           'and intersects plane in one point.'        )

!
!     Create the plane.
!
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the ellipse.
!
      CALL VPACK  ( 1.D0, 2.D0, 0.D0, CENTER )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL INELPL ( ELLIPS, PLANE,  N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', N, '=', 1, 0, OK )
!
!     Check intersection points directly (T_ISPXEL isn't
!     designed to work with degenerate ellipses).
!
      CALL VPACK ( 1.D0, 2.D0, 0.D0, XPT )

      CALL CHCKAD ( 'XPTS 1', XPTS(1,1), '~', XPT, 3, TIGHT, OK )
      CALL CHCKAD ( 'XPTS 2', XPTS(1,2), '~', XPT, 3, TIGHT, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INELPL: ellipse is a single point ' &
      //           'and does not intersect plane.'        )

!
!     Create the plane.
!
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the ellipse.
!
      CALL VPACK  ( 1.D0, 2.D0, 3.D0, CENTER )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, SMAJOR )
      CALL VPACK  ( 0.D0, 0.D0, 0.D0, SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )

      CALL INELPL ( ELLIPS, PLANE,  N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', 0, '=', 0, 0, OK )

!
!     INELPL error cases:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'INELPL: invalid plane' )

      CALL CLEARD ( 4, PLANE )

      CALL INELPL ( ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDPLANE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'INELPL: invalid ellipse having ' &
      //            'non-orthogonal axes.'           )

!
!     Create the plane.
!
      CALL VPACK  ( 0.D0, 0.D0, 1.D0, NORMAL )
      CONST = 0.D0

      CALL NVC2PL ( NORMAL, CONST, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the ellipse. Note that we can't use
!     CGV2EL for this purpose, since it will create
!     proper, orthogonal semi-axis vectors.
!
      CALL VPACK  ( 1.D0, 2.D0, 3.D0, ELLIPS(1) )
      CALL VPACK  ( 0.D0, 1.D0, 1.D0, ELLIPS(4) )
      CALL VPACK  ( 0.D0, 1.D0, 0.D0, ELLIPS(7) )

      CALL INELPL ( ELLIPS, PLANE, N, XPTS(1,1), XPTS(1,2) )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDELLIPSE)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END





!
!     Supporting function T_ISPXEL
!
      LOGICAL FUNCTION T_ISPXEL ( ELLIPS, PLANE, N, XPTS, OK )
      IMPLICIT NONE
!
!     Test whether a set of points constitute the intersection of a
!     specified plane and ellipse.
!
      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL = 4 )

      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      PLANE  ( UBPL )
      INTEGER               N
      DOUBLE PRECISION      XPTS   ( 3, 2 )
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
      PARAMETER           ( LVLTOL = 1.D-12 )

      DOUBLE PRECISION      PLNTOL
      PARAMETER           ( PLNTOL = 1.D-11 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

!
!     Local variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OFFSET ( 3, 2 )
      DOUBLE PRECISION      PC
      DOUBLE PRECISION      RMAT   ( 3, 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SCLA
      DOUBLE PRECISION      SCLB
      DOUBLE PRECISION      SCLX   ( 3, 2 )
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      XAX    ( 3, 2 )

      INTEGER               I


!
!     Give the function an initial return value.
!
      T_ISPXEL = .FALSE.

!
!     Scale ellipse and plane.
!
      CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A     =  VNORM(SMAJOR)
      B     =  VNORM(SMINOR)
      C     =  VNORM(CENTER)

      CALL PL2NVC ( PLANE, NORMAL, CONST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( .NOT. OK ) THEN
         RETURN
      END IF

!
!     Scale the ellipse and plane constant for safer computation.
!
      SCALE = MAX ( ABS(A), ABS(B), ABS(C), ABS(CONST) )

      CALL VSCLIP ( 1.D0 / SCALE, CENTER )
      CALL VSCLIP ( 1.D0 / SCALE, SMAJOR )
      CALL VSCLIP ( 1.D0 / SCALE, SMINOR )

      SCLA  = A     / SCALE
      SCLB  = B     / SCALE

      CONST = CONST / SCALE

!
!     Scale the input points.
!
      DO I = 1, N
         CALL VSCL (  1.D0 / SCALE, XPTS(1,I), SCLX(1,I) )
      END DO

!
!     Subtract the scaled ellipse center from the intersection
!     points.
!
      DO I = 1, N
         CALL VSUB ( SCLX(1,I), CENTER, OFFSET(1,I) )
      END DO

!
!     Transform the input points into the semi-axis frame.
!
      CALL TWOVEC ( SMAJOR, 1, SMINOR, 2, RMAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, N
         CALL MXV ( RMAT, OFFSET(1,I), XAX(1,I) )
      END DO

!
!     Check the `level' of each point.
!
      DO I = 1, N

         LEVEL  =    ( XAX(1,I)**2 / SCLA**2 ) &
                   + ( XAX(2,I)**2 / SCLB**2 )

         TITLE = 'Level of point #'
         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL CHCKSD ( TITLE, LEVEL, '~', 1.D0, LVLTOL, OK )

         IF  ( .NOT. OK ) THEN

            T_ISPXEL = .FALSE.
            RETURN

         END IF

      END DO

!
!     Check each point to see if it's in the plane.
!
      CALL PL2NVC ( PLANE, NORMAL, CONST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CONST = CONST / SCALE

      DO I = 1, N

         PC = VDOT ( SCLX(1,I), NORMAL )

         TITLE = 'Plane constant of point #'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL CHCKSD ( TITLE, PC, '~/', CONST, PLNTOL, OK )

         IF ( .NOT. OK  ) THEN

            T_ISPXEL = .FALSE.
            RETURN

         END IF

      END DO

      T_ISPXEL = .TRUE.

      RETURN
      END
