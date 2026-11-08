C$Procedure      F_ZZELVUPY0 ( Test ZZELVUPY )

      SUBROUTINE F_ZZELVUPY0 ( OK )
      IMPLICIT NONE

      LOGICAL               OK

C$ Abstract
C
C     This routine tests the SPICELIB routine
C     
C        ZZELVUPY
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
C$ Version
C
C-    TSPICE Version 1.0.0, 11-AUG-2005 (NJB)
C
C-&
 
C
C     SPICELIB functions
C

C
C     Local parameters
C
      
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

C
C     Local variables
C
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

C
C     Saved variables
C

C
C     Initial values
C


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZELVUPY0' )

C
C     Test all ellipse/FOV combinations.
C
      DO I = 1, NCOMBO
C
C        Set the default FOV shape.
C

C
C        WRITE (*,*) '================================================'
C        WRITE (*,*) '================================================'
C        WRITE (*,*) '================================================'
C        WRITE (*,*) '================================================'

         IF ( I .EQ. 1 ) THEN
C
C           The first case is a square FOV.
C
            N = 4
            CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,1) )
            CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,2) )
            CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,3) )
            CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,4) )

            CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS     )
            CALL VPACK (  1.D0,   0.D0,  0.D0,  VERTEX   )            

C
C           The default ellipse is oriented with the major axis
C           vertical and is parallel to the x-z plane.
C
            CALL VPACK (  0.D0,   0.D0,  1.D0,   SMAJOR )
            CALL VPACK (  0.D0,   5.D-1, 0.D0,   SMINOR )
            CALL VPACK ( -1.D0,   0.D0,  0.D0,   CENTER )

         ELSE IF ( I .EQ. 2 ) THEN
C
C           Rotate the FOV starting index counterclockwise by pi/2.
C
            CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,1) )
            CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,2) )
            CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,3) )
            CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,4) )

         ELSE IF ( I .EQ. 3 ) THEN
C
C           Rotate the FOV starting index clockwise by pi/2.
C
            CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,1) )
            CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,2) )
            CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,3) )
            CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,4) )

         ELSE IF ( I .EQ. 4 ) THEN
C
C           Rotate the FOV starting index clockwise by pi.
C
            CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,1) )
            CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,2) )
            CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,3) )
            CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,4) )

         ELSE IF ( I .EQ. 5 ) THEN
C
C           Reverse ordering of FOV boundary vectors.
C
            CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,4) )
            CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,3) )
            CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,2) )
            CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,1) )

         ELSE IF ( I .EQ. 6 ) THEN
C
C           For this case, we use an ellipse that is seen
C           edge-on from the apex of the pyramid.  This
C           ellipse lies in the x-y plane.
C
            CALL VPACK (  1.D0,   0.D0,  0.D0,   SMAJOR )
            CALL VPACK (  0.D0,   5.D-1, 0.D0,   SMINOR )
            CALL VPACK ( -1.D0,   0.D0,  0.D0,   CENTER )

         ELSE IF ( I .EQ. 7 ) THEN
C
C           Now we make the FOV into a four-pointed star.  The
C           star is formed by taking our original square FOV and
C           "pinching" it at the midpoints of each edge.
C
            N = 8

            CALL VPACK ( -1.D0,  -1.D0,  -1.D0,   FOV(1,1) )
            CALL VPACK ( -1.D0,   0.D0,  -5.D-1,  FOV(1,2) )
            CALL VPACK ( -1.D0,   1.D0,  -1.D0,   FOV(1,3) )
            CALL VPACK ( -1.D0,   5.D-1,  0.D0,   FOV(1,4) )
            CALL VPACK ( -1.D0,   1.D0,   1.D0,   FOV(1,5) )
            CALL VPACK ( -1.D0,   0.D0,   5.D-1,  FOV(1,6) )
            CALL VPACK ( -1.D0,  -1.D0,   1.D0,   FOV(1,7) )
            CALL VPACK ( -1.D0,  -5.D-1,  0.D0,   FOV(1,8) )

C
C           Use the same axis and vertex as before.
C
            CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS     )
            CALL VPACK (  1.D0,   0.D0,  0.D0,  VERTEX   )            

C
C           The default ellipse is oriented with the major axis slanted
C           in the z=y direction and is parallel to the x-z plane.
C           
            HAFRT2 = SQRT( 2.D0 ) / 2.D0

            CALL VPACK (  0.D0,   HAFRT2,    HAFRT2,   SMAJOR )
            CALL VPACK (  0.D0,  -HAFRT2/2,  HAFRT2,   SMINOR )
            CALL VPACK ( -1.D0,   0.D0,      0.D0,     CENTER )

         END IF

C
C        Test for geometric cases:
C
C           1) Ellipse inside FOV
C           2) FOV inside ellipse
C           3) FOV chops ellipse
C           4) No intersection
C

         DO J = 1, NCASE


            IF ( J .EQ. 1 ) THEN
C
C --- Case: ------------------------------------------------------
C
               TITLE = 'Combo = #; geometric case = ellipse in FOV.'

               CALL REPMI  ( TITLE, '#', I,   TITLE )
               CALL CHCKXC ( .FALSE.,    ' ', OK    )
               CALL TCASE  ( TITLE )

C
C              The ellipse is defined by the default components.
C
               CALL VEQU ( CENTER, ECTR  )
               CALL VEQU ( SMAJOR, ESMAJ )
               CALL VEQU ( SMINOR, ESMIN )

C
C              We expect the intersection to be found.
C
               XFOUND = .TRUE.

            

            ELSE IF ( J .EQ. 2 ) THEN
C
C --- Case: ------------------------------------------------------
C
               TITLE = 'Combo = #; geometric case = ellipse '  //
     .                 'is not contained in FOV but contains ' //
     .                 'center of FOV.'

               CALL REPMI  ( TITLE, '#', I,   TITLE )
               CALL CHCKXC ( .FALSE.,    ' ', OK    )
               CALL TCASE  ( TITLE )


               IF ( I .LE. 5 ) THEN
C
C                 The ellipse is scaled up by a factor of 10 and
C                 shifted in the -z direction so the center of the
C                 ellipse is not in the FOV.
C
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

C
C              We expect the intersection to be found.
C
               XFOUND = .TRUE.


            ELSE IF ( J .EQ. 3 ) THEN
C
C --- Case: ------------------------------------------------------
C
               TITLE = 'Combo = #; geometric case = ellipse '  //
     .                 'is chopped by FOV.'

               CALL REPMI  ( TITLE, '#', I,   TITLE )
               CALL CHCKXC ( .FALSE.,    ' ', OK    )
               CALL TCASE  ( TITLE )

               IF ( I .LE. 5 ) THEN
C
C                 The ellipse is scaled up by a factor of 10 and
C                 shifted in the -z direction so the center is not in
C                 the FOV and the center of the FOV is not in the
C                 ellipse.
C
                  CALL VPACK ( 0.D0, 0.D0, -10.999D0, OFFSET )

                  CALL VADD  ( CENTER,  OFFSET, ECTR  )
                  CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
                  CALL VSCL  ( 10.D0,   SMINOR, ESMIN )

               ELSE IF ( I .EQ. 6 ) THEN

C                 The ellipse is scaled up by a factor of 10 and
C                 shifted in the -y direction so the center is not in
C                 the FOV and the center of the FOV is not in the
C                 ellipse.
C
                  CALL VPACK ( 0.D0, -5.5D0, 0.D0, OFFSET )

                  CALL VADD  ( CENTER,  OFFSET, ECTR  )
                  CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
                  CALL VSCL  ( 10.D0,   SMINOR, ESMIN )

               END IF

C
C              We expect the intersection to be found.
C
               XFOUND = .TRUE.


            ELSE IF ( J .EQ. 4 ) THEN
C
C --- Case: ------------------------------------------------------
C
               TITLE = 'Combo = #; geometric case = bounding '  //
     .                 'cones of ellipse and FOV are disjoint.'

               CALL REPMI  ( TITLE, '#', I,   TITLE )
               CALL CHCKXC ( .FALSE.,    ' ', OK    )
               CALL TCASE  ( TITLE )

C
C              The ellipse is scaled up by a factor of 10 and
C              shifted in the -z direction so the bounding cones
C              are disjoint.
C
               CALL VPACK ( 0.D0, 0.D0, -30.D0, OFFSET )

               CALL VADD  ( CENTER,  OFFSET, ECTR  )
               CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
               CALL VSCL  ( 10.D0,   SMINOR, ESMIN )

C
C              We expect the intersection NOT to be found.
C
               XFOUND = .FALSE.


            ELSE IF ( J .EQ. 5 ) THEN
C
C --- Case: ------------------------------------------------------
C
               TITLE = 'Combo = #; geometric case = bounding '  //
     .                 'cones of ellipse and FOV are not '      //
     .                 'disjoint, but there is no intersection.'

               CALL REPMI  ( TITLE, '#', I,   TITLE )
               CALL CHCKXC ( .FALSE.,    ' ', OK    )
               CALL TCASE  ( TITLE )


               IF ( I .LE. 5 ) THEN
C
C                 Make the ellipse very thin and position it
C                 so that it doesn't intersect the FOV, but so
C                 that the plane containing the apex and the top
C                 edge of the FOV does intersect the ellipse.
C
                  IF ( I .LE. 3 ) THEN
                     CALL VPACK (  0.D0,  2.00050001D0, 2.D0, OFFSET )
                  ELSE
                     CALL VPACK (  0.D0, -2.00050001D0, 2.D0, OFFSET )
                  END IF

                  CALL VADD  ( CENTER,  OFFSET, ECTR  )
                  CALL VEQU  ( SMAJOR,          ESMAJ )
                  CALL VSCL  ( 1.D-3,   SMINOR, ESMIN )

               ELSE
C
C                 The ellipse is scaled up by a factor of 10 and
C                 shifted in the -z direction so the bounding cones
C                 are non disjoint, but the ellipse is outside the
C                 FOV.
C
                  CALL VPACK ( 0.D0, 0.D0, -15.D0, OFFSET )

                  CALL VADD  ( CENTER,  OFFSET, ECTR  )
                  CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
                  CALL VSCL  ( 10.D0,   SMINOR, ESMIN )

               END IF

C
C              We expect the intersection NOT to be found.
C
               XFOUND = .FALSE.

            END IF

C
C           Pack the ellipse components.
C
            CALL CGV2EL ( ECTR, ESMAJ, ESMIN, ELLIPS )            
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND', FOUND, XFOUND, OK )

         END DO


      END DO


C
C     Test error cases.
C
      N = 4
      CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,1) )
      CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,2) )
      CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,3) )
      CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,4) )

      CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS     )
      CALL VPACK (  1.D0,   0.D0,  0.D0,  VERTEX   )            

C
C     The default ellipse is oriented with the major axis
C     vertical and is parallel to the x-z plane.
C
      CALL VPACK (  0.D0,   0.D0,  1.D0,   SMAJOR )
      CALL VPACK (  0.D0,   5.D-1, 0.D0,   SMINOR )
      CALL VPACK ( -1.D0,   0.D0,  0.D0,   CENTER )


      CALL TCASE ( 'Axis is the zero vector.' )

      CALL VPACK ( 0.D0,   0.D0,  0.D0,  AXIS )

      CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )




      CALL TCASE ( 'Second and third boundary vectors are the same.' )
C
C     Restore the orginal axis vector.
C
      CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS )

      CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,3) )

      CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFOV)', OK )



      CALL TCASE ( 'Third boundary vector is the zero vector.' )

      CALL VPACK ( 0.D0,   0.D0,  0.D0,  FOV(1,3) )

      CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )



      CALL TCASE ( 'Ellipse generating vectors are linearly ' //
     .             'dependent.  The semi-minor axis is '      //
     .             'therefore zero.'                          )

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
      
C
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
      

C     This commented-out block is used for timing tests.
C     We set up the square FOV and ellipse in the x-z plane. 
C
C     CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,1) )
C     CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,2) )
C     CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,3) )
C     CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,4) )
C
C     CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS     )
C     CALL VPACK (  1.D0,   0.D0,  0.D0,  VERTEX   )
C
C     The default ellipse is oriented with the major axis
C     vertical and is parallel to the x-z plane.
C
C     CALL VPACK (  0.D0,   0.D0,  1.D0,   SMAJOR )
C     CALL VPACK (  0.D0,   5.D-1, 0.D0,   SMINOR )
C     CALL VPACK ( -1.D0,   0.D0,  0.D0,   CENTER )
C
C     This  block is used for the case where the bounding cones are
C     disjoint. This is the fastest case, but the one that should be
C     encountered most often.
C
C     CALL VPACK ( 0.D0, 0.D0, -30.D0, OFFSET )
C
C     CALL VADD  ( CENTER,  OFFSET, ECTR  )
C     CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
C     CALL VSCL  ( 10.D0,   SMINOR, ESMIN )
C
C     This commented-out block is for the fall-through
C     non-intersection case, which is the slowest.
C 
C     CALL VPACK ( 0.D0, 0.D0, -15.D0, OFFSET )
C
C     CALL VADD  ( CENTER,  OFFSET, ECTR  )
C     CALL VSCL  ( 10.D0,   SMAJOR, ESMAJ )
C     CALL VSCL  ( 10.D0,   SMINOR, ESMIN )
C
C
C     CALL CGV2EL ( ECTR, ESMAJ, ESMIN, ELLIPS )
C
C
C     DO I = 1, 100000
C        CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
C     END DO
C


      CALL T_SUCCESS ( OK )
      RETURN
      END
