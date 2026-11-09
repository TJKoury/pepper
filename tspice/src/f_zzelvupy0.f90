!$Procedure      F_ZZELVUPY0 ( Test ZZELVUPY )

      SUBROUTINE F_ZZELVUPY0 ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        ZZELVUPY
!
! $ Version
!
!-    TSPICE Version 1.0.0, 11-AUG-2005 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local parameters
!

      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      INTEGER               NCASE
      PARAMETER           ( NCASE  = 5 )

      INTEGER               NCOMBO
      PARAMETER           ( NCOMBO = 7 )

      INTEGER               MAXN
      PARAMETER           ( MAXN   = 100 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      ECTR   ( 3 )
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      ESMAJ  ( 3 )
      DOUBLE PRECISION      ESMIN  ( 3 )
      DOUBLE PRECISION      FOV    ( 3, MAXN )
      DOUBLE PRECISION      HAFRT2
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               N

      LOGICAL               FOUND
      LOGICAL               XFOUND

!
!     Saved variables
!

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZELVUPY0' )

!
!     Test all ellipse/FOV combinations.
!
      DO I = 1, NCOMBO
!
!        Set the default FOV shape.
!

!
!        WRITE (*,*) '================================================'
!        WRITE (*,*) '================================================'
!        WRITE (*,*) '================================================'
!        WRITE (*,*) '================================================'

         IF ( I .EQ. 1 ) THEN
!
!           The first case is a square FOV.
!
            N = 4
            CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,1) )
            CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,2) )
            CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,3) )
            CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,4) )

            CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS     )
            CALL VPACK (  1.D0,   0.D0,  0.D0,  VERTEX   )

!
!           The default ellipse is oriented with the major axis
!           vertical and is parallel to the x-z plane.
!
            CALL VPACK (  0.D0,   0.D0,  1.D0,   SMAJOR )
            CALL VPACK (  0.D0,   5.D-1, 0.D0,   SMINOR )
            CALL VPACK ( -1.D0,   0.D0,  0.D0,   CENTER )

         ELSE IF ( I .EQ. 2 ) THEN
!
!           Rotate the FOV starting index counterclockwise by pi/2.
!
            CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,1) )
            CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,2) )
            CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,3) )
            CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,4) )

         ELSE IF ( I .EQ. 3 ) THEN
!
!           Rotate the FOV starting index clockwise by pi/2.
!
            CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,1) )
            CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,2) )
            CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,3) )
            CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,4) )

         ELSE IF ( I .EQ. 4 ) THEN
!
!           Rotate the FOV starting index clockwise by pi.
!
            CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,1) )
            CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,2) )
            CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,3) )
            CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,4) )

         ELSE IF ( I .EQ. 5 ) THEN
!
!           Reverse ordering of FOV boundary vectors.
!
            CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,4) )
            CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,3) )
            CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,2) )
            CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,1) )

         ELSE IF ( I .EQ. 6 ) THEN
!
!           For this case, we use an ellipse that is seen
!           edge-on from the apex of the pyramid.  This
!           ellipse lies in the x-y plane.
!
            CALL VPACK (  1.D0,   0.D0,  0.D0,   SMAJOR )
            CALL VPACK (  0.D0,   5.D-1, 0.D0,   SMINOR )
            CALL VPACK ( -1.D0,   0.D0,  0.D0,   CENTER )

         ELSE IF ( I .EQ. 7 ) THEN
!
!           Now we make the FOV into a four-pointed star.  The
!           star is formed by taking our original square FOV and
!           "pinching" it at the midpoints of each edge.
!
            N = 8

            CALL VPACK ( -1.D0,  -1.D0,  -1.D0,   FOV(1,1) )
            CALL VPACK ( -1.D0,   0.D0,  -5.D-1,  FOV(1,2) )
            CALL VPACK ( -1.D0,   1.D0,  -1.D0,   FOV(1,3) )
            CALL VPACK ( -1.D0,   5.D-1,  0.D0,   FOV(1,4) )
            CALL VPACK ( -1.D0,   1.D0,   1.D0,   FOV(1,5) )
            CALL VPACK ( -1.D0,   0.D0,   5.D-1,  FOV(1,6) )
            CALL VPACK ( -1.D0,  -1.D0,   1.D0,   FOV(1,7) )
            CALL VPACK ( -1.D0,  -5.D-1,  0.D0,   FOV(1,8) )

!
!           Use the same axis and vertex as before.
!
            CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS     )
            CALL VPACK (  1.D0,   0.D0,  0.D0,  VERTEX   )

!
!           The default ellipse is oriented with the major axis slanted
!           in the z=y direction and is parallel to the x-z plane.
!
            HAFRT2 = SQRT( 2.D0 ) / 2.D0

            CALL VPACK (  0.D0,   HAFRT2,    HAFRT2,   SMAJOR )
            CALL VPACK (  0.D0,  -HAFRT2/2,  HAFRT2,   SMINOR )
            CALL VPACK ( -1.D0,   0.D0,      0.D0,     CENTER )

         END IF

!
!        Test for geometric cases:
!
!           1) Ellipse inside FOV
!           2) FOV inside ellipse
!           3) FOV chops ellipse
!           4) No intersection
!

         DO J = 1, NCASE


            IF ( J .EQ. 1 ) THEN
!
! --- Case: ------------------------------------------------------
!
               TITLE = 'Combo = #; geometric case = ellipse in FOV.'

               CALL REPMI  ( TITLE, '#', I,   TITLE )
               CALL CHCKXC ( .FALSE.,    ' ', OK    )
               CALL TCASE  ( TITLE )

!
!              The ellipse is defined by the default components.
!
               CALL VEQU ( CENTER, ECTR  )
               CALL VEQU ( SMAJOR, ESMAJ )
               CALL VEQU ( SMINOR, ESMIN )

!
!              We expect the intersection to be found.
!
               XFOUND = .TRUE.



            ELSE IF ( J .EQ. 2 ) THEN
!
! --- Case: ------------------------------------------------------
!
               TITLE = 'Combo = #; geometric case = ellipse '  // &
                       'is not contained in FOV but contains ' // &
                       'center of FOV.'

               CALL REPMI  ( TITLE, '#', I,   TITLE )
               CALL CHCKXC ( .FALSE.,    ' ', OK    )
               CALL TCASE  ( TITLE )


               IF ( I .LE. 5 ) THEN
!
!                 The ellipse is scaled up by a factor of 10 and
!                 shifted in the -z direction so the center of the
!                 ellipse is not in the FOV.
!
                  CALL VPACK ( 0.D0, 0.D0, -9.5D0, OFFSET )

                  CALL VADD  ( CENTER,  OFFSET, ECTR  )
                  CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
                  CALL VSCL  ( 10.D0,   SMINOR, ESMIN )

               ELSE IF ( I .EQ. 6 ) THEN

                  CALL VPACK ( 0.D0, -9.5D0, 0.D0, OFFSET )

                  CALL VADD  ( CENTER,  OFFSET, ECTR  )
                  CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
                  CALL VSCL  ( 10.D0,   SMINOR, ESMIN )

               END IF

!
!              We expect the intersection to be found.
!
               XFOUND = .TRUE.


            ELSE IF ( J .EQ. 3 ) THEN
!
! --- Case: ------------------------------------------------------
!
               TITLE = 'Combo = #; geometric case = ellipse '  // &
                       'is chopped by FOV.'

               CALL REPMI  ( TITLE, '#', I,   TITLE )
               CALL CHCKXC ( .FALSE.,    ' ', OK    )
               CALL TCASE  ( TITLE )

               IF ( I .LE. 5 ) THEN
!
!                 The ellipse is scaled up by a factor of 10 and
!                 shifted in the -z direction so the center is not in
!                 the FOV and the center of the FOV is not in the
!                 ellipse.
!
                  CALL VPACK ( 0.D0, 0.D0, -10.999D0, OFFSET )

                  CALL VADD  ( CENTER,  OFFSET, ECTR  )
                  CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
                  CALL VSCL  ( 10.D0,   SMINOR, ESMIN )

               ELSE IF ( I .EQ. 6 ) THEN

!                 The ellipse is scaled up by a factor of 10 and
!                 shifted in the -y direction so the center is not in
!                 the FOV and the center of the FOV is not in the
!                 ellipse.
!
                  CALL VPACK ( 0.D0, -5.5D0, 0.D0, OFFSET )

                  CALL VADD  ( CENTER,  OFFSET, ECTR  )
                  CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
                  CALL VSCL  ( 10.D0,   SMINOR, ESMIN )

               END IF

!
!              We expect the intersection to be found.
!
               XFOUND = .TRUE.


            ELSE IF ( J .EQ. 4 ) THEN
!
! --- Case: ------------------------------------------------------
!
               TITLE = 'Combo = #; geometric case = bounding '  // &
                       'cones of ellipse and FOV are disjoint.'

               CALL REPMI  ( TITLE, '#', I,   TITLE )
               CALL CHCKXC ( .FALSE.,    ' ', OK    )
               CALL TCASE  ( TITLE )

!
!              The ellipse is scaled up by a factor of 10 and
!              shifted in the -z direction so the bounding cones
!              are disjoint.
!
               CALL VPACK ( 0.D0, 0.D0, -30.D0, OFFSET )

               CALL VADD  ( CENTER,  OFFSET, ECTR  )
               CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
               CALL VSCL  ( 10.D0,   SMINOR, ESMIN )

!
!              We expect the intersection NOT to be found.
!
               XFOUND = .FALSE.


            ELSE IF ( J .EQ. 5 ) THEN
!
! --- Case: ------------------------------------------------------
!
               TITLE = 'Combo = #; geometric case = bounding '  // &
                       'cones of ellipse and FOV are not '      // &
                       'disjoint, but there is no intersection.'

               CALL REPMI  ( TITLE, '#', I,   TITLE )
               CALL CHCKXC ( .FALSE.,    ' ', OK    )
               CALL TCASE  ( TITLE )


               IF ( I .LE. 5 ) THEN
!
!                 Make the ellipse very thin and position it
!                 so that it doesn't intersect the FOV, but so
!                 that the plane containing the apex and the top
!                 edge of the FOV does intersect the ellipse.
!
                  IF ( I .LE. 3 ) THEN
                     CALL VPACK (  0.D0,  2.00050001D0, 2.D0, OFFSET )
                  ELSE
                     CALL VPACK (  0.D0, -2.00050001D0, 2.D0, OFFSET )
                  END IF

                  CALL VADD  ( CENTER,  OFFSET, ECTR  )
                  CALL VEQU  ( SMAJOR,          ESMAJ )
                  CALL VSCL  ( 1.D-3,   SMINOR, ESMIN )

               ELSE
!
!                 The ellipse is scaled up by a factor of 10 and
!                 shifted in the -z direction so the bounding cones
!                 are non disjoint, but the ellipse is outside the
!                 FOV.
!
                  CALL VPACK ( 0.D0, 0.D0, -15.D0, OFFSET )

                  CALL VADD  ( CENTER,  OFFSET, ECTR  )
                  CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
                  CALL VSCL  ( 10.D0,   SMINOR, ESMIN )

               END IF

!
!              We expect the intersection NOT to be found.
!
               XFOUND = .FALSE.

            END IF

!
!           Pack the ellipse components.
!
            CALL CGV2EL ( ECTR, ESMAJ, ESMIN, ELLIPS )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND', FOUND, XFOUND, OK )

         END DO


      END DO


!
!     Test error cases.
!
      N = 4
      CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,1) )
      CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,2) )
      CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,3) )
      CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,4) )

      CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS     )
      CALL VPACK (  1.D0,   0.D0,  0.D0,  VERTEX   )

!
!     The default ellipse is oriented with the major axis
!     vertical and is parallel to the x-z plane.
!
      CALL VPACK (  0.D0,   0.D0,  1.D0,   SMAJOR )
      CALL VPACK (  0.D0,   5.D-1, 0.D0,   SMINOR )
      CALL VPACK ( -1.D0,   0.D0,  0.D0,   CENTER )


      CALL TCASE ( 'Axis is the zero vector.' )

      CALL VPACK ( 0.D0,   0.D0,  0.D0,  AXIS )

      CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )




      CALL TCASE ( 'Second and third boundary vectors are the same.' )
!
!     Restore the orginal axis vector.
!
      CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS )

      CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,3) )

      CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFOV)', OK )



      CALL TCASE ( 'Third boundary vector is the zero vector.' )

      CALL VPACK ( 0.D0,   0.D0,  0.D0,  FOV(1,3) )

      CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )



      CALL TCASE ( 'Ellipse generating vectors are linearly ' // &
                   'dependent.  The semi-minor axis is '      // &
                   'therefore zero.'                          )

      N = 4
      CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,1) )
      CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,2) )
      CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,3) )
      CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,4) )

      CALL VPACK (  0.D0,   0.D0,   1.D0,   SMAJOR )
      CALL VPACK (  0.D0,   0.D0,   1.D0,   SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


      CALL TCASE ( 'Ellipse semi-minor axis is zero.' )

      N = 4
      CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,1) )
      CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,2) )
      CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,3) )
      CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,4) )

      CALL VPACK (  0.D0,   0.D0,   1.D0,   SMAJOR )
      CALL VPACK (  0.D0,   0.D0,   0.D0,   SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

!
      CALL TCASE ( 'Ellipse semi-axes are both zero.' )

      N = 4
      CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,1) )
      CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,2) )
      CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,3) )
      CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,4) )

      CALL VPACK (  0.D0,   0.D0,   0.D0,   SMAJOR )
      CALL VPACK (  0.D0,   0.D0,   0.D0,   SMINOR )

      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!     This commented-out block is used for timing tests.
!     We set up the square FOV and ellipse in the x-z plane.
!
!     CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,1) )
!     CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,2) )
!     CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,3) )
!     CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,4) )
!
!     CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS     )
!     CALL VPACK (  1.D0,   0.D0,  0.D0,  VERTEX   )
!
!     The default ellipse is oriented with the major axis
!     vertical and is parallel to the x-z plane.
!
!     CALL VPACK (  0.D0,   0.D0,  1.D0,   SMAJOR )
!     CALL VPACK (  0.D0,   5.D-1, 0.D0,   SMINOR )
!     CALL VPACK ( -1.D0,   0.D0,  0.D0,   CENTER )
!
!     This  block is used for the case where the bounding cones are
!     disjoint. This is the fastest case, but the one that should be
!     encountered most often.
!
!     CALL VPACK ( 0.D0, 0.D0, -30.D0, OFFSET )
!
!     CALL VADD  ( CENTER,  OFFSET, ECTR  )
!     CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
!     CALL VSCL  ( 10.D0,   SMINOR, ESMIN )
!
!     This commented-out block is for the fall-through
!     non-intersection case, which is the slowest.
!
!     CALL VPACK ( 0.D0, 0.D0, -15.D0, OFFSET )
!
!     CALL VADD  ( CENTER,  OFFSET, ECTR  )
!     CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
!     CALL VSCL  ( 10.D0,   SMINOR, ESMIN )
!
!
!     CALL CGV2EL ( ECTR, ESMAJ, ESMIN, ELLIPS )
!
!
!     DO I = 1, 100000
!        CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
!     END DO
!


      CALL T_SUCCESS ( OK )
      RETURN
      END
