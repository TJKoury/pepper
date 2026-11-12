!$Procedure F_XDDA ( XDDA tests )

      SUBROUTINE F_XDDA ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the ray-voxel grid intersection routine XDDA.
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
!     This routine tests the DSKLIB type 2 ray-voxel grid
!     intersection routine XDDA.
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
!-    TSPICE Version 1.0.0, 10-JUL-2014 (NJB)
!
!        Updated to text XDDA bug fix. Logic
!        for vertices slightly outside the grid
!        or just inside it is now tested.
!
!-    TSPICE Version 1.0.0, 05-JUN-2014 (NJB)
!
!-&

!
!     SPICELIB functions
!



!
!     Local Parameters
!

!
!     The parameter GRDTOL must be kept in sync with
!     the value used in XDDA.
!
      DOUBLE PRECISION      GRDTOL
      PARAMETER           ( GRDTOL = 1.D-12 )

      INTEGER               MAXLST
      PARAMETER           ( MAXLST = 10000 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 320 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    LABEL
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      BOXVTX ( 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      DXTENT ( 3 )
      DOUBLE PRECISION      EGRESS ( 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      VOXORI ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               EXTENT ( 3 )
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               M
      INTEGER               MAXNVX
      INTEGER               NEXT   ( 3 )
      INTEGER               NVX
      INTEGER               S
      INTEGER               UB
      INTEGER               VC
      INTEGER               VCOORD ( 2 )
      INTEGER               VOXLST ( 3,MAXLST )
      INTEGER               VR
      INTEGER               XC
      INTEGER               XCOORD ( 2 )
      INTEGER               XR
      INTEGER               XVOX   ( 3 )

      LOGICAL               FOUND

!
!     Saved values
!
      SAVE


!
!     Initial values
!
      DATA                  NEXT   / 2, 3, 1  /
      DATA                  VOXORI / 3 * 0.D0 /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_XDDA' )

!
!     Test XDDA:  start out with error handling.
!

!**********************************************************************
!
!     Error cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Ray direction is zero vector.' )

!
!     Default inputs:
!
!
!     EXTENT is of integer type, so we can't use VPACK.
!
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      CALL VPACK (  0.D0,  0.D0,  10.D0, VERTEX )
      CALL VPACK (  0.D0,  0.D0,  -1.D0, RAYDIR )

      MAXNVX = MAXLST

!
!     Zero out the direction vector.
!
      CALL CLEARD ( 3, RAYDIR )

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
      CALL CHCKXC   ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

      CALL VPACK (  0.D0,  0.D0,  -1.D0, RAYDIR )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-positive extents' )

!
!     Default inputs:
!
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      CALL VPACK (  0.D0,  0.D0,  40.D0, VERTEX )
      CALL VPACK (  0.D0,  0.D0,  -1.D0, RAYDIR )

      MAXNVX = MAXLST

!
!     Set an invalid extent value.
!
      EXTENT(1) =  0
      EXTENT(2) = 20
      EXTENT(3) = 10

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
      CALL CHCKXC   ( .TRUE., 'SPICE(BADDIMENSIONS)', OK )

      EXTENT(1) = -1
      EXTENT(2) = 20
      EXTENT(3) = 10

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
      CALL CHCKXC   ( .TRUE., 'SPICE(BADDIMENSIONS)', OK )


      EXTENT(1) = 10
      EXTENT(2) =  0
      EXTENT(3) = 10

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
      CALL CHCKXC   ( .TRUE., 'SPICE(BADDIMENSIONS)', OK )


      EXTENT(1) = 10
      EXTENT(2) = -1
      EXTENT(3) = 10

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
      CALL CHCKXC   ( .TRUE., 'SPICE(BADDIMENSIONS)', OK )


      EXTENT(1) = 10
      EXTENT(2) = 20
      EXTENT(3) =  0

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
      CALL CHCKXC   ( .TRUE., 'SPICE(BADDIMENSIONS)', OK )


      EXTENT(1) = 10
      EXTENT(2) = 20
      EXTENT(3) = -1

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
      CALL CHCKXC   ( .TRUE., 'SPICE(BADDIMENSIONS)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Vertex is not in voxel grid' )

!
!     Default inputs:
!
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      MAXNVX = MAXLST

      CALL VPACK (  0.D0,   0.D0,  -1.D0, RAYDIR )

!
!     Vertex is outside of grid by one unit.
!
      DO I = 1, 3

         DO J = 0, 1

            S = 2*J - 1

            CALL VPACK (  15.D0,  10.D0,  5.D0, VERTEX )

            VERTEX(I) = VERTEX(I) +  S * ((EXTENT(I)/2.D0) + 1)

            CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
            CALL CHCKXC   ( .TRUE., 'SPICE(VERTEXNOTINGRID)', OK )

         END DO

      END DO

!
!     Vertex is outside of grid by less than the limit.
!
      DO I = 1, 3

         DO J = 0, 1

            S = 2*J - 1

            CALL VPACK (  15.D0,  10.D0,  5.D0, VERTEX )

            VERTEX(I) =   VERTEX(I) + &
                        S * ( EXTENT(I) * ( 1.D0 + GRDTOL ) / 2 )

            CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
            CALL CHCKXC   ( .FALSE., ' ', OK )

         END DO

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Output array size is non-positive' )

!
!     Default inputs:
!
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      CALL VPACK (  0.D0,  0.D0,  10.D0, VERTEX )
      CALL VPACK (  0.D0,  0.D0,  -1.D0, RAYDIR )

!
!     Set an invalid array size value.
!
      MAXNVX = 0

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
      CALL CHCKXC   ( .TRUE., 'SPICE(INVALIDSIZE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Output array size is too small' )

!
!     Default inputs:
!
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      CALL VPACK (  15.D0,  10.D0,  10.D0, VERTEX )
      CALL VPACK (  0.D0,    0.D0,  -1.D0, RAYDIR )

!
!     Set an invalid array size value.
!
      MAXNVX = 9

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
      CALL CHCKXC   ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )





!**********************************************************************
!
!     Non-error exception cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!

!
!     In the following cases, the vertex is slightly outside of the
!     voxel grid, and an intersection exists. Rays are chosen so
!     that incorrect determination of the vertex offset should lead
!     to incorrect determination of the second or third intersected
!     voxels.
!
!
!     Grid extents:
!
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      MAXNVX = MAXLST

      DO I = 1, 3

         XCOORD(1) = NEXT(I)
         XCOORD(2) = NEXT( XCOORD(1) )


         DO J = 0, 1

!
! --- Case: ------------------------------------------------------
!
            TITLE = 'Vertex is not in voxel grid; intersection ' &
            //      'exists. I = #; J = #.'

            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )


            S = 2*J - 1

!
!           Vertex is outside of grid by less than the limit.
!

            VERTEX(I) =     ( EXTENT(I) / 2.D0 ) + &
                        S * ( EXTENT(I) * ( 1.D0 + GRDTOL ) / 2.D0 )

            VERTEX( XCOORD(1) ) = ( 1.D0 + EXTENT( XCOORD(1) ) ) / 2.D0
            VERTEX( XCOORD(2) ) = ( 1.D0 + EXTENT( XCOORD(2) ) ) / 2.D0


!            WRITE (*,*) 'VERTEX(i)         = ', VERTEX(I)
!            WRITE (*,*) 'VERTEX(xcoord(1)) = ', VERTEX(XCOORD(1))
!            WRITE (*,*) 'VERTEX(xcoord(2)) = ', VERTEX(XCOORD(2))

            CALL CLEARD ( 3, RAYDIR )
            RAYDIR(I)           =   -S
            RAYDIR( XCOORD(1) ) =  2 * ABS(S)
            RAYDIR( XCOORD(2) ) =  3 * ABS(S)


            CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
            CALL CHCKXC   ( .FALSE., ' ', OK )

!
!           In all cases at least 3 voxels should be hit.
!
            LABEL = 'NVX (#,#)'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, NVX, '>=', 3, 0, OK )

!
!           Check the first voxel returned. First compute
!           the expected values of its components.
!
            IF ( S .LT. 0 ) THEN
               XVOX(I) = 1
            ELSE
               XVOX(I) = EXTENT(I)
            END IF

            XVOX( XCOORD(1) ) = BRCKTI ( 1 + INT(VERTEX(XCOORD(1))), &
                                         1, &
                                         EXTENT( XCOORD(1) )         )

            XVOX( XCOORD(2) ) = BRCKTI ( 1 + INT(VERTEX(XCOORD(2))), &
                                         1, &
                                         EXTENT( XCOORD(2) )         )

            LABEL = 'VOXEL 1 (#,#)'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKAI ( LABEL, VOXLST(1,1), '=', XVOX, 3, OK )

!            WRITE (*,*) '1 XVOX = ', XVOX

!
!           We expect the coordinates of the second voxel to match,
!           except for coordinate indexed XCOORD(2), which should
!           be incremented.
!
            XVOX( XCOORD(2) ) = XVOX( XCOORD(2) ) + 1

!
!           Instead of calling CHCKAI, we'll check each voxel
!           element individually.
!
!            WRITE (*,*) '2 XVOX = ', XVOX

            DO K = 1, 3

               LABEL = 'VOXEL 2 (#) '
               CALL REPMI ( LABEL, '#', K, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKSI ( LABEL, VOXLST(K,2), '=', XVOX(K), 0, OK )

            END DO
!
!           We expect the coordinates of the third voxel to match those
!           of the second, except for coordinate indexed XCOORD(1),
!           which should be incremented.
!
            XVOX( XCOORD(1) ) = XVOX( XCOORD(1) ) + 1

            DO K = 1, 3

               LABEL = 'VOXEL 3 (#) '
               CALL REPMI ( LABEL, '#', K, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKSI ( LABEL, VOXLST(K,3), '=', XVOX(K), 0, OK )

            END DO

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!

!
!     In the following cases, the vertex is slightly inside of the
!     voxel grid. Rays are chosen so that incorrect determination of
!     the vertex offset should lead to incorrect determination of the
!     second or third intersected voxels.
!
!
!     Grid extents:
!
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      MAXNVX = MAXLST

      DO I = 1, 3

         XCOORD(1) = NEXT(I)
         XCOORD(2) = NEXT( XCOORD(1) )


         DO J = 0, 1

!
! --- Case: ------------------------------------------------------
!
            TITLE = 'Vertex is just inside voxel grid; intersection ' &
            //      'exists. I = #; J = #.'

            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )


            S = 2*J - 1

!
!           Vertex is inside of grid and separated from the
!           nearest boundary plane by a small amount.
!

            VERTEX(I) =     ( EXTENT(I) / 2.D0 ) + &
                        S * ( EXTENT(I) * ( 1.D0 - GRDTOL ) / 2.D0 )

            VERTEX( XCOORD(1) ) = ( 1.D0 + EXTENT( XCOORD(1) ) ) / 2.D0
            VERTEX( XCOORD(2) ) = ( 1.D0 + EXTENT( XCOORD(2) ) ) / 2.D0


!            WRITE (*,*) 'VERTEX(i)         = ', VERTEX(I)
!            WRITE (*,*) 'VERTEX(xcoord(1)) = ', VERTEX(XCOORD(1))
!            WRITE (*,*) 'VERTEX(xcoord(2)) = ', VERTEX(XCOORD(2))

            CALL CLEARD ( 3, RAYDIR )
            RAYDIR(I)           =   -S
            RAYDIR( XCOORD(1) ) =  2 * ABS(S)
            RAYDIR( XCOORD(2) ) =  3 * ABS(S)


            CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
            CALL CHCKXC   ( .FALSE., ' ', OK )

!
!           In all cases at least 3 voxels should be hit.
!
            LABEL = 'NVX (#,#)'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, NVX, '>=', 3, 0, OK )

!
!           Check the first voxel returned. First compute
!           the expected values of its components.
!
            IF ( S .LT. 0 ) THEN
               XVOX(I) = 1
            ELSE
               XVOX(I) = EXTENT(I)
            END IF

            XVOX( XCOORD(1) ) = BRCKTI ( 1 + INT(VERTEX(XCOORD(1))), &
                                         1, &
                                         EXTENT( XCOORD(1) )         )

            XVOX( XCOORD(2) ) = BRCKTI ( 1 + INT(VERTEX(XCOORD(2))), &
                                         1, &
                                         EXTENT( XCOORD(2) )         )

            LABEL = 'VOXEL 1 (#,#)'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKAI ( LABEL, VOXLST(1,1), '=', XVOX, 3, OK )

!            WRITE (*,*) '1 XVOX = ', XVOX

!
!           We expect the coordinates of the second voxel to match,
!           except for coordinate indexed XCOORD(2), which should
!           be incremented.
!
            XVOX( XCOORD(2) ) = XVOX( XCOORD(2) ) + 1

!
!           Instead of calling CHCKAI, we'll check each voxel
!           element individually.
!
!            WRITE (*,*) '2 XVOX = ', XVOX

            DO K = 1, 3

               LABEL = 'VOXEL 2 (#) '
               CALL REPMI ( LABEL, '#', K, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKSI ( LABEL, VOXLST(K,2), '=', XVOX(K), 0, OK )

            END DO
!
!           We expect the coordinates of the third voxel to match those
!           of the second, except for coordinate indexed XCOORD(1),
!           which should be incremented.
!
            XVOX( XCOORD(1) ) = XVOX( XCOORD(1) ) + 1

            DO K = 1, 3

               LABEL = 'VOXEL 3 (#) '
               CALL REPMI ( LABEL, '#', K, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKSI ( LABEL, VOXLST(K,3), '=', XVOX(K), 0, OK )

            END DO

         END DO

      END DO




!
! --- Case: ------------------------------------------------------
!

!
!     The following tests exercise logic that deals with rays that
!     are nearly parallel to a coordinate axis.
!

      DO I = 1, 3

         XCOORD(1) = NEXT( I )
         XCOORD(2) = NEXT( XCOORD(1) )

!
!        The selected value of EXTENT(I) is less than MAXLST.
!
         EXTENT( I         ) = 9999
         EXTENT( XCOORD(1) ) = 2
         EXTENT( XCOORD(2) ) = 2

         DO J = 0, 1

            DO K = 0, 1

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'Ray is nearly parallel to  ' &
               //      'coordinate axis test: I = #; ' &
               //      'J = #; K = #. Vertex is at face center.'

               CALL REPMI ( TITLE, '#', I,  TITLE )
               CALL REPMI ( TITLE, '#', J,  TITLE )
               CALL REPMI ( TITLE, '#', K,  TITLE )
               CALL CHCKXC( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               VERTEX( I         ) = 0.D0
               VERTEX( XCOORD(1) ) = 1.D0
               VERTEX( XCOORD(2) ) = 1.D0

!
!              Use small magnitudes for ray components that
!              are orthogonal to the Ith component.
!
               RAYDIR( I         ) = 1.D0
               RAYDIR( XCOORD(1) ) = (2*J - 1) * 1.D-300
               RAYDIR( XCOORD(2) ) = (2*K - 1) * 1.D-30

               CALL XDDA ( VERTEX, RAYDIR, EXTENT, &
                           MAXLST, NVX,    VOXLST )
               CALL CHCKXC   ( .FALSE., ' ', OK )

!
!              Check the list.
!
               CALL T_CHKVOX ( VERTEX, RAYDIR, EXTENT, NVX, VOXLST )
               CALL CHCKXC   ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
               TITLE = 'Ray is nearly parallel to  ' &
               //      'coordinate axis test: I = #; ' &
               //      'J = #; K = #. Vertex is at face edge.'

               CALL REPMI ( TITLE, '#', I,  TITLE )
               CALL REPMI ( TITLE, '#', J,  TITLE )
               CALL REPMI ( TITLE, '#', K,  TITLE )
               CALL CHCKXC( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               VERTEX( I         ) = 0.D0
               VERTEX( XCOORD(1) ) = 2.D0
               VERTEX( XCOORD(2) ) = 2.D0

!
!              Use small magnitudes for ray components that
!              are orthogonal to the Ith component.
!
               RAYDIR( I         ) = 1.D0
               RAYDIR( XCOORD(1) ) = (2*J - 1) * 1.D-300
               RAYDIR( XCOORD(2) ) = (2*K - 1) * 1.D-30

               CALL XDDA ( VERTEX, RAYDIR, EXTENT, &
                           MAXLST, NVX,    VOXLST )
               CALL CHCKXC   ( .FALSE., ' ', OK )

!
!              Check the list. In this case the checking code
!              may fail to correctly compute the expected point
!              of exit from the grid. However, we can get the
!              checking algorithm to work by passing in a ray direction
!              for which the components orthogonal to the Ith axis
!              have been zeroed out.
!
               CALL CLEARD( 3, VTEMP )
               VTEMP(I) = 1.D0

               CALL T_CHKVOX ( VERTEX, VTEMP, EXTENT, NVX, VOXLST )
               CALL CHCKXC   ( .FALSE., ' ', OK )

            END DO

         END DO

      END DO



!**********************************************************************
!
!     Normal cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Trivial case: grid has one voxel. Vertex is in ' &
      //           'the middle of the X = 1 face. Ray points inward.' )

      CALL VPACK (  1.D0, 0.5D0, 0.5D0, VERTEX )
      CALL VPACK ( -1.D0, 0.D0,  0.D0,  RAYDIR )

      EXTENT(1) = 1
      EXTENT(2) = 1
      EXTENT(3) = 1

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, 10, NVX, VOXLST )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NVX', NVX, '=', 1, 0, OK )

      CALL T_CHKVOX ( VERTEX, RAYDIR, EXTENT, NVX, VOXLST )
      CALL CHCKXC   ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!

!
!     Using a small but non-trivial grid, place vertices of rays
!     at the midpoint of each exterior voxel face. Place exit points
!     at the midpoint of each exterior voxel face on grid surfaces
!     other than the one containing the vertex.
!
      EXTENT(1) = 4
      EXTENT(2) = 5
      EXTENT(3) = 6

      MAXNVX    = MAXLST

      CALL CLEARD ( 3, RAYDIR )
      CALL CLEARD ( 3, VERTEX )
      CALL CLEARD ( 3, EGRESS )

      DO I = 1, 3
!
!        Set the "horizontal" coordinates on the vertex face
!        of the grid: these are orthogonal to the Ith axis.
!
         VCOORD(1) = NEXT( I )
         VCOORD(2) = NEXT( VCOORD(1) )

!
!        Loop over both vertex faces orthogonal to axis I.
!
         DO J = 0, 1
!
!           Loop over the exit faces. There's one face on
!           the opposite side from the vertex face, and four
!           faces normal to the horizontal coordinate axes.
!
            DO K = 1, 3
!
!              Set the "horizontal" coordinates on the exit face
!              of the grid: these are orthogonal to the Kth axis.
!
               XCOORD(1) = NEXT( K )
               XCOORD(2) = NEXT( XCOORD(1) )

               IF ( K .EQ. I ) THEN
                  UB = 0
               ELSE
                  UB = 1
               END IF

!
!              Loop over both exit faces orthogonal to axis K,
!              except when K = I. In the latter case, the exit
!              face must be on the opposite side of the grid
!              from the vertex.
!
               DO M = 0, UB
!
!                 S is (1-J) on the first pass; J on the second.
!
                  S = (1-M) + ( 2*M - 1 ) * J

!
!                 Loop over the horizontal coordinates on the grid face
!                 containing the vertex. VR is the row index for the
!                 vertex; VC is the column index.
!
                  DO VR = 1, EXTENT( VCOORD(1) )

                     DO VC = 1, EXTENT( VCOORD(2) )

                        DO XR = 1, EXTENT( XCOORD(1) )

                           DO XC = 1, EXTENT( XCOORD(2) )


!
! --- Case: ------------------------------------------------------
!
                              TITLE = 'Voxel face center to voxel ' &
                              //      'face center test: I = #; ' &
                              //      'J = #; K = #; M = #; VR = #; ' &
                              //      'VC = #; XR = #; XC = #.'

                              CALL REPMI ( TITLE, '#', I,  TITLE )
                              CALL REPMI ( TITLE, '#', J,  TITLE )
                              CALL REPMI ( TITLE, '#', K,  TITLE )
                              CALL REPMI ( TITLE, '#', M,  TITLE )
                              CALL REPMI ( TITLE, '#', VR, TITLE )
                              CALL REPMI ( TITLE, '#', VC, TITLE )
                              CALL REPMI ( TITLE, '#', XR, TITLE )
                              CALL REPMI ( TITLE, '#', XC, TITLE )
                              CALL CHCKXC   ( .FALSE., ' ', OK )

                              CALL TCASE ( TITLE )
!
!                             Loop over the horizontal coordinates on
!                             the face containing the exit. XR is the
!                             row index for the vertex; XC is the
!                             column index.
!
!                             Set the vertex and exit points. It's
!                             inefficient to set coordinates here that
!                             could be set in outer loops, but the code
!                             is easier to read this way.
!
                              VERTEX( I         ) = J  * EXTENT(I)
                              VERTEX( VCOORD(1) ) = VR - 0.5D0
                              VERTEX( VCOORD(2) ) = VC - 0.5D0

                              EGRESS( K         ) = S  * EXTENT(K)
                              EGRESS( XCOORD(1) ) = XR - 0.5D0
                              EGRESS( XCOORD(2) ) = XC - 0.5D0

                              CALL VSUB ( EGRESS, VERTEX, RAYDIR )
!
!                             Compute the list of intersected voxels.
!
                              CALL XDDA ( VERTEX, RAYDIR, EXTENT, &
                                          MAXNVX, NVX,    VOXLST )
                              CALL CHCKXC   ( .FALSE., ' ', OK )

!
!                             Check the list.
!
                              CALL T_CHKVOX ( VERTEX, RAYDIR, &
                                              EXTENT, NVX,   VOXLST )
                              CALL CHCKXC   ( .FALSE., ' ', OK )

                           END DO

                        END DO

                     END DO

                  END DO

               END DO

            END DO

         END DO

      END DO





!
! --- Case: ------------------------------------------------------
!

!
!     Using a small but non-trivial grid, place vertices of rays
!     at the corners of each exterior voxel face. Place exit points
!     at the corners of each exterior voxel face on grid surfaces
!     other than the one containing the vertex.
!
      EXTENT(1) = 4
      EXTENT(2) = 4
      EXTENT(3) = 6

      MAXNVX    = MAXLST

      CALL CLEARD ( 3, RAYDIR )
      CALL CLEARD ( 3, VERTEX )
      CALL CLEARD ( 3, EGRESS )

      DO I = 1, 3
!
!        Set the "horizontal" coordinates on the vertex face
!        of the grid: these are orthogonal to the Ith axis.
!
         VCOORD(1) = NEXT( I )
         VCOORD(2) = NEXT( VCOORD(1) )

!
!        Loop over both vertex faces orthogonal to axis I.
!
         DO J = 0, 1
!
!           Loop over the exit faces. There's one face on
!           the opposite side from the vertex face, and four
!           faces normal to the horizontal coordinate axes.
!
            DO K = 1, 3
!
!              Set the "horizontal" coordinates on the exit face
!              of the grid: these are orthogonal to the Kth axis.
!
               XCOORD(1) = NEXT( K )
               XCOORD(2) = NEXT( XCOORD(1) )

               IF ( K .EQ. I ) THEN
                  UB = 0
               ELSE
                  UB = 1
               END IF

!
!              Loop over both exit faces orthogonal to axis K,
!              except when K = I. In the latter case, the exit
!              face must be on the opposite side of the grid
!              from the vertex.
!
               DO M = 0, UB
!
!                 S is (1-J) on the first pass; J on the second.
!
                  S = (1-M) + ( 2*M - 1 ) * J

!
!                 Loop over the horizontal coordinates on the grid face
!                 containing the vertex. VR is the row index for the
!                 vertex; VC is the column index.
!
                  DO VR = 0, EXTENT( VCOORD(1) )

                     DO VC = 0, EXTENT( VCOORD(2) )

                        DO XR = 0, EXTENT( XCOORD(1) )

                           DO XC = 0, EXTENT( XCOORD(2) )


!
! --- Case: ------------------------------------------------------
!
                              TITLE = 'Voxel face corner to voxel ' &
                              //      'face corner test: I = #; ' &
                              //      'J = #; K = #; M = #; VR = #; ' &
                              //      'VC = #; XR = #; XC = #.'

                              CALL REPMI ( TITLE, '#', I,  TITLE )
                              CALL REPMI ( TITLE, '#', J,  TITLE )
                              CALL REPMI ( TITLE, '#', K,  TITLE )
                              CALL REPMI ( TITLE, '#', M,  TITLE )
                              CALL REPMI ( TITLE, '#', VR, TITLE )
                              CALL REPMI ( TITLE, '#', VC, TITLE )
                              CALL REPMI ( TITLE, '#', XR, TITLE )
                              CALL REPMI ( TITLE, '#', XC, TITLE )
                              CALL CHCKXC   ( .FALSE., ' ', OK )

                              CALL TCASE ( TITLE )
!
!                             Loop over the horizontal coordinates on
!                             the face containing the exit. XR is the
!                             row index for the vertex; XC is the
!                             column index.
!
!                             Set the vertex and exit points. It's
!                             inefficient to set coordinates here that
!                             could be set in outer loops, but the code
!                             is easier to read this way.
!
                              VERTEX( I         ) = J  * EXTENT(I)
                              VERTEX( VCOORD(1) ) = VR
                              VERTEX( VCOORD(2) ) = VC

                              EGRESS( K         ) = S  * EXTENT(K)
                              EGRESS( XCOORD(1) ) = XR
                              EGRESS( XCOORD(2) ) = XC

                              CALL VSUB ( EGRESS, VERTEX, RAYDIR )

                              IF ( .NOT. VZERO( RAYDIR ) ) THEN
!
!                                Compute the list of intersected voxels.
!
                                 CALL XDDA ( VERTEX, RAYDIR, EXTENT, &
                                             MAXNVX, NVX,    VOXLST )
                                 CALL CHCKXC   ( .FALSE., ' ', OK )

!
!                                Check the list.
!
                                 CALL T_CHKVOX ( VERTEX, RAYDIR, &
                                                 EXTENT, NVX,   VOXLST )
                                 CALL CHCKXC   ( .FALSE., ' ', OK )

                              END IF

                           END DO

                        END DO

                     END DO

                  END DO

               END DO

            END DO

         END DO

      END DO



!
! --- Case: ------------------------------------------------------
!

!
!     Using a realistic, large grid, use rays pointed toward the
!     origin. This is the classic "spear test."
!
      EXTENT(1) = 151
      EXTENT(2) = 180
      EXTENT(3) = 119

      DO I = 1, 3
         DXTENT(I) = DBLE( EXTENT(I) )
         CENTER(I) = DXTENT(I) / 2
      END DO

      D = VNORM( DXTENT )

      DLAT =  1.D0
      DLON =  2.D0

      LAT  = 90.D0

      DO WHILE ( LAT .GE. -90.D0 )

         LON  =  0.D0

         DO WHILE ( LON .LT. 360.D0 )

!
! --- Case: ------------------------------------------------------
!
            TITLE = 'Spear test: lon = # deg.; lat = # deg., ' &
            //      'extents are # # #.'

            CALL REPMD ( TITLE, '#', LON,       14, TITLE )
            CALL REPMD ( TITLE, '#', LAT,       14, TITLE )
            CALL REPMI ( TITLE, '#', EXTENT(1),     TITLE )
            CALL REPMI ( TITLE, '#', EXTENT(2),     TITLE )
            CALL REPMI ( TITLE, '#', EXTENT(3),     TITLE )
            CALL CHCKXC( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )



            CALL LATREC ( D,      LON*RPD(),  LAT*RPD(), VERTEX )
            CALL VMINUS ( VERTEX,                        RAYDIR )

            CALL VADD ( VERTEX, CENTER, VTEMP )
!
!           Unlike DSKX02, we need the vertex on the grid surface
!           for XDDA to work properly.
!
            CALL ZZRAYBOX ( VTEMP, RAYDIR, VOXORI, DXTENT, XPT, FOUND )
            CALL CHCKXC( .FALSE., ' ', OK )

            CALL VEQU ( XPT, BOXVTX )

            CALL XDDA ( BOXVTX, RAYDIR, EXTENT, MAXLST, NVX, VOXLST )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL T_CHKVOX ( BOXVTX, RAYDIR, EXTENT, NVX, VOXLST )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            IF ( .NOT. OK ) STOP

            LON = LON + DLON

         END DO

         LAT = LAT - DLAT

      END DO



!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END







!**********************************************************************
!
!     U T I L I T Y   R O U T I N E S
!
!**********************************************************************


!
!     Routine: T_CHKVOX
!
!     Check a solution produced by XDDA.
!
!     Note that, in the event that the ray intersects a voxel
!     edge or corner, the solution is not unique. Therefore
!     computing an expected solution is not a general approach
!     to validating XDDA.
!
!     A solution must have the following properties:
!
!        1)  The first voxel contains the ray's vertex.
!
!        2)  The last voxel contains the ray's point of exit from the
!            grid.
!
!        3)  All voxels have coordinates in within the ranges defined
!            by the grid extents.
!
!        4)  The (i+1)st voxel differs from the ith voxel in only one
!            coordinate, and the difference has absolute value equal to
!            1.
!
!        5)  The ray intersects each voxel. The intersection test
!            must be carried out using a tolerance.
!
!
!     This routine signals an error if any of the above conditions
!     are not met. The calling test family should call CHCKXC
!     immediately following each call to this routine.
!

      SUBROUTINE T_CHKVOX ( VERTEX, RAYDIR, EXTENT, N, VOXLST )
      use pepper_legacy
      IMPLICIT NONE

      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      INTEGER               EXTENT ( 3 )
      INTEGER               N
      INTEGER               VOXLST ( 3, * )

!
!     SPICELIB functions
!

!
!     Other functions
!

!
!     Local parameters
!

!
!     Tolerance used for testing whether a point is inside a
!     given voxel:
!
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL    = 1.D-11 )

!
!     Expansion amount used for ray-voxel and ray-grid
!     intersection testing:
!
      DOUBLE PRECISION      EXPAND
      PARAMETER           ( EXPAND = 1.D-12 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 320 )

!
!     Local Variables
!
      CHARACTER*(MSGLEN)    LNGMSG
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DXTENT ( 3 )
      DOUBLE PRECISION      EGRESS ( 3 )
      DOUBLE PRECISION      EGRVTX ( 3 )
      DOUBLE PRECISION      NEGDIR ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      PRVXPT ( 3 )
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               I
      INTEGER               IDELTA
      INTEGER               J

      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE

!
!     Initial values
!


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_CHKVOX' )

!
!     Make sure the solution has at least one voxel, so we
!     can safely refer the first and last voxel in the list.
!
      IF ( N .LT. 1 ) THEN

         CALL SETMSG ( 'Voxel list size is #; size must be positive.' )
         CALL ERRINT ( '#', N                                         )
         CALL SIGERR ( 'SPICE(BADLISTSIZE)'                           )
         CALL CHKOUT ( 'T_CHKVOX'                                     )
         RETURN

      END IF

!
!     Make sure the voxel coordinates are in range.
!
      DO I = 1, N

         DO J = 1, 3

            IF (      ( VOXLST(J,I) .LT. 1         ) &
                 .OR. ( VOXLST(J,I) .GT. EXTENT(J) )  ) THEN

               CALL SETMSG ( 'Coordinate # of voxel # was #; valid ' &
               //            'range is 1:#.'                        )
               CALL ERRINT ( '#', J                                 )
               CALL ERRINT ( '#', I                                 )
               CALL ERRINT ( '#', VOXLST(J,I)                       )
               CALL ERRINT ( '#', EXTENT(J)                         )
               CALL SIGERR ( 'SPICE(BADVOXCOORD)'                   )
               CALL CHKOUT ( 'T_CHKVOX'                             )
               RETURN

            END IF

         END DO

      END DO

!
!     Check the first voxel.
!
      DO I = 1, 3
!
!        Do not use a tolerance for this check. Bracketing should
!        ensure the voxel is inside the grid (this includes the
!        surface). Computation of voxel coordinates for the voxel
!        containing the vertex should be portable.
!

         IF (     ( VERTEX(I) .LT. (VOXLST(I,1) - 1)  ) &
             .OR. ( VERTEX(I) .GT.  VOXLST(I,1)       )  ) THEN

            CALL SETMSG ( 'Coordinate # of vertex was #; valid ' &
               //         'range, given by voxel 1, is #:#.'       )
            CALL ERRINT ( '#', I                                   )
            CALL ERRDP  ( '#', VERTEX(I)                           )
            CALL ERRINT ( '#', VOXLST(I,1) - 1                     )
            CALL ERRINT ( '#', VOXLST(I,1)                         )
            CALL SIGERR ( 'SPICE(BADFIRSTVOX)'                     )
            CALL CHKOUT ( 'T_CHKVOX'                               )
            RETURN

         END IF

      END DO


!
!     Check the last voxel. In order to do this, we'll need to
!     find the ray's point of exit from the grid.
!
!     Start by computing the vertex of a ray pointing in the opposite
!     direction as the input ray and passing through the input vertex.
!     The vertex of this ray is beyond the exit point. Call this vertex
!     EGRVTX.
!
!     We'll expand the grid slightly before computing the exit point,
!     since the ray may lie along the grid's surface.. The extents will
!     be lengthened by 2*EXPAND, and the "origin" of the grid will be
!     shifted by -EXPAND in each coordinate. This will give us an
!     expanded grid concentric with the original. Let D be the diameter
!     of the grid (the supremum of the distances between any two points
!     in the grid).
!
      DO I = 1, 3
         DXTENT(I) = DBLE( EXTENT(I) )
      END DO

      D = VNORM( DXTENT )

      CALL VHAT   ( RAYDIR, UDIR )

      CALL VLCOM  ( 1.D0, VERTEX, 2.D0 * D, UDIR, EGRVTX )

      CALL VMINUS ( UDIR, NEGDIR )

      DO I = 1, 3
         DXTENT(I) = DBLE(EXTENT(I))  +  2 * EXPAND
      END DO

      DO I = 1, 3
         ORIGIN(I) =  -EXPAND
      END DO

      CALL ZZRAYBOX ( EGRVTX, NEGDIR, ORIGIN, DXTENT, EGRESS, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'Uh-oh. Intersection of reversed ray with ' &
         //            'voxel grid was not found. Input ray vertex: ' &
         //            '# # #; direction: # # #; grid extents: # # #.' )
         CALL ERRDP  ( '#', VERTEX(1)                                  )
         CALL ERRDP  ( '#', VERTEX(2)                                  )
         CALL ERRDP  ( '#', VERTEX(3)                                  )
         CALL ERRDP  ( '#', RAYDIR(1)                                  )
         CALL ERRDP  ( '#', RAYDIR(2)                                  )
         CALL ERRDP  ( '#', RAYDIR(3)                                  )
         CALL ERRINT ( '#', EXTENT(1)                                  )
         CALL ERRINT ( '#', EXTENT(2)                                  )
         CALL ERRINT ( '#', EXTENT(3)                                  )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'T_CHKVOX'                                      )
         RETURN

      END IF

!
!     Bracket the egress within the voxel grid.
!
      DO I = 1, 3
         EGRESS(I) = BRCKTD(  EGRESS(I),  0.D0,  DBLE(EXTENT(I))  )
      END DO

!
!     Ok, check the last voxel to see whether it contains EGRESS.
!
      DO I = 1, 3
!
!        Use a tolerance for this check.
!
         IF (     ( EGRESS(I) .LT. (VOXLST(I,N) - 1) - TOL ) &
             .OR. ( EGRESS(I) .GT.  VOXLST(I,N)      + TOL )  ) THEN

            CALL SETMSG ( 'Coordinate # of egress was #; valid ' &
               //         'range, given by voxel N, is #:#.'       )
            CALL ERRINT ( '#', I                                   )
            CALL ERRDP  ( '#', EGRESS(I)                           )
            CALL ERRDP  ( '#', VOXLST(I,N) - 1 - TOL               )
            CALL ERRDP  ( '#', VOXLST(I,N)     + TOL               )
            CALL SIGERR ( 'SPICE(BADLASSTVOX)'                     )
            CALL CHKOUT ( 'T_CHKVOX'                               )
            RETURN

         END IF

      END DO

!
!     Check the coordinate differences of successive voxels in the
!     list. Each voxel should be different from the previous one
!     in one coordinate, and by a value of 1. A way of checking
!     this is to sum the absolute values of the differences of the
!     coordinates.
!
      DO I = 2, N
!
!        Note DELTA has INTEGER type.
!
         IDELTA = 0

         DO J = 1, 3
            IDELTA = IDELTA + ABS( VOXLST(J,I) - VOXLST(J,I-1) )
         END DO

         IF ( IDELTA .NE. 1 ) THEN

            CALL SETMSG ( 'Voxel at index # is # # #; voxel at ' &
            //            'index # is # # #. Absolute values of  ' &
            //            'coordinate differences should sum to 1.' )
            CALL ERRINT ( '#', I                                    )
            CALL ERRINT ( '#', VOXLST(1,I)                          )
            CALL ERRINT ( '#', VOXLST(2,I)                          )
            CALL ERRINT ( '#', VOXLST(3,I)                          )
            CALL ERRINT ( '#', I - 1                                )
            CALL ERRINT ( '#', VOXLST(1,I-1)                        )
            CALL ERRINT ( '#', VOXLST(2,I-1)                        )
            CALL ERRINT ( '#', VOXLST(3,I-1)                        )
            CALL SIGERR ( 'SPICE(BADVOXELDIFF)'                     )
            CALL CHKOUT ( 'T_CHKVOX'                                )
            RETURN

         END IF

      END DO

!
!     Make sure the ray hits each voxel in the list. We'll expand the
!     voxels slightly before testing them. The extents will be
!     lengthened by 2*EXPAND, and the "origin" of each voxel will be
!     shifted by -EXPAND in each coordinate. This will give us an
!     expanded voxel concentric with the original.
!
      CALL CLEARD ( 3, PRVXPT )

      DO I = 1, N

         DO J = 1, 3
            DXTENT(J) = DBLE(EXTENT(J))  +  2 * EXPAND
         END DO

         DO J = 1, 3
            ORIGIN(J) = ( VOXLST(J,I) - 1.D0 ) - EXPAND
         END DO

         CALL ZZRAYBOX ( VERTEX, RAYDIR, ORIGIN, DXTENT, XPT, FOUND )

         IF ( .NOT. FOUND ) THEN

            IF ( I .GT. 1 ) THEN

               LNGMSG = 'Intersection of ray with voxel # ' &
               //       '(list size = #) was not  ' &
               //       'found. Input ray vertex: # # #; direction: ' &
               //       '# # #; grid extents: # # #. Voxel ' &
               //       'coordinates: # # #. Coordinates of ray ' &
               //       'intercept with previous voxel are # # #.'

            ELSE

               LNGMSG = 'Intersection of ray with voxel # ' &
               //       '(list size = #) was not  ' &
               //       'found. Input ray vertex: # # #; direction: ' &
               //       '# # #; grid extents: # # #. Voxel ' &
               //       'coordinates: # # #.'

            END IF

            CALL SETMSG ( LNGMSG               )
            CALL ERRINT ( '#', I               )
            CALL ERRINT ( '#', N               )
            CALL ERRDP  ( '#', VERTEX(1)       )
            CALL ERRDP  ( '#', VERTEX(2)       )
            CALL ERRDP  ( '#', VERTEX(3)       )
            CALL ERRDP  ( '#', RAYDIR(1)       )
            CALL ERRDP  ( '#', RAYDIR(2)       )
            CALL ERRDP  ( '#', RAYDIR(3)       )
            CALL ERRINT ( '#', EXTENT(1)       )
            CALL ERRINT ( '#', EXTENT(2)       )
            CALL ERRINT ( '#', EXTENT(3)       )
            CALL ERRINT ( '#', VOXLST(1,I)     )
            CALL ERRINT ( '#', VOXLST(2,I)     )
            CALL ERRINT ( '#', VOXLST(3,I)     )

            IF ( I .GT. 1 ) THEN

               CALL ERRDP ( '#', PRVXPT(1) )
               CALL ERRDP ( '#', PRVXPT(2) )
               CALL ERRDP ( '#', PRVXPT(3) )

            END IF

            CALL SIGERR ( 'SPICE(VOXELNOTHIT)' )
            CALL CHKOUT ( 'T_CHKVOX'           )
            RETURN

         END IF

         CALL VEQU ( XPT, PRVXPT )

      END DO


      CALL CHKOUT ( 'T_CHKVOX' )
      RETURN
      END





