C$Procedure F_XDDA ( XDDA tests )
 
      SUBROUTINE F_XDDA ( OK )
 
C$ Abstract
C
C     Exercise the ray-voxel grid intersection routine XDDA.
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
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the DSKLIB type 2 ray-voxel grid
C     intersection routine XDDA.
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
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 10-JUL-2014 (NJB)
C
C        Updated to text XDDA bug fix. Logic
C        for vertices slightly outside the grid
C        or just inside it is now tested. 
C
C-    TSPICE Version 1.0.0, 05-JUN-2014 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      VNORM

      INTEGER               BRCKTI

      LOGICAL               VZERO

C
C     Local Parameters
C 

C
C     The parameter GRDTOL must be kept in sync with
C     the value used in XDDA.
C
      DOUBLE PRECISION      GRDTOL
      PARAMETER           ( GRDTOL = 1.D-12 )

      INTEGER               MAXLST
      PARAMETER           ( MAXLST = 10000 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 320 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Local Variables  
C
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

C
C     Saved values
C
      SAVE


C
C     Initial values
C
      DATA                  NEXT   / 2, 3, 1  /
      DATA                  VOXORI / 3 * 0.D0 /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_XDDA' )

C
C     Test XDDA:  start out with error handling.
C     

C**********************************************************************
C
C     Error cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Ray direction is zero vector.' )

C
C     Default inputs:
C
C
C     EXTENT is of integer type, so we can't use VPACK.
C
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      CALL VPACK (  0.D0,  0.D0,  10.D0, VERTEX )
      CALL VPACK (  0.D0,  0.D0,  -1.D0, RAYDIR )

      MAXNVX = MAXLST

C
C     Zero out the direction vector.
C
      CALL CLEARD ( 3, RAYDIR )

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
      CALL CHCKXC   ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

      CALL VPACK (  0.D0,  0.D0,  -1.D0, RAYDIR )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Non-positive extents' )

C
C     Default inputs:
C
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      CALL VPACK (  0.D0,  0.D0,  40.D0, VERTEX )
      CALL VPACK (  0.D0,  0.D0,  -1.D0, RAYDIR )

      MAXNVX = MAXLST

C
C     Set an invalid extent value.
C
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
 
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Vertex is not in voxel grid' )

C
C     Default inputs:
C
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      MAXNVX = MAXLST

      CALL VPACK (  0.D0,   0.D0,  -1.D0, RAYDIR )

C
C     Vertex is outside of grid by one unit.
C
      DO I = 1, 3

         DO J = 0, 1

            S = 2*J - 1

            CALL VPACK (  15.D0,  10.D0,  5.D0, VERTEX )

            VERTEX(I) = VERTEX(I) +  S * ((EXTENT(I)/2.D0) + 1)

            CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
            CALL CHCKXC   ( .TRUE., 'SPICE(VERTEXNOTINGRID)', OK )
  
         END DO

      END DO

C
C     Vertex is outside of grid by less than the limit.
C
      DO I = 1, 3

         DO J = 0, 1

            S = 2*J - 1

            CALL VPACK (  15.D0,  10.D0,  5.D0, VERTEX )

            VERTEX(I) =   VERTEX(I) +  
     .                  S * ( EXTENT(I) * ( 1.D0 + GRDTOL ) / 2 )

            CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
            CALL CHCKXC   ( .FALSE., ' ', OK )
  
         END DO

      END DO       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Output array size is non-positive' )

C
C     Default inputs:
C
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      CALL VPACK (  0.D0,  0.D0,  10.D0, VERTEX )
      CALL VPACK (  0.D0,  0.D0,  -1.D0, RAYDIR )

C
C     Set an invalid array size value.
C
      MAXNVX = 0

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
      CALL CHCKXC   ( .TRUE., 'SPICE(INVALIDSIZE)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Output array size is too small' )

C
C     Default inputs:
C
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      CALL VPACK (  15.D0,  10.D0,  10.D0, VERTEX )
      CALL VPACK (  0.D0,    0.D0,  -1.D0, RAYDIR )

C
C     Set an invalid array size value.
C
      MAXNVX = 9

      CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
      CALL CHCKXC   ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )





C**********************************************************************
C
C     Non-error exception cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 

C
C     In the following cases, the vertex is slightly outside of the
C     voxel grid, and an intersection exists. Rays are chosen so
C     that incorrect determination of the vertex offset should lead
C     to incorrect determination of the second or third intersected
C     voxels.
C     
C
C     Grid extents:
C
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      MAXNVX = MAXLST

      DO I = 1, 3

         XCOORD(1) = NEXT(I)
         XCOORD(2) = NEXT( XCOORD(1) )

         
         DO J = 0, 1

C
C --- Case: ------------------------------------------------------
C 
            TITLE = 'Vertex is not in voxel grid; intersection ' 
     .      //      'exists. I = #; J = #.' 

            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL CHCKXC   ( .FALSE., ' ', OK )
            
            CALL TCASE ( TITLE )


            S = 2*J - 1

C
C           Vertex is outside of grid by less than the limit.
C

            VERTEX(I) =     ( EXTENT(I) / 2.D0 ) +  
     .                  S * ( EXTENT(I) * ( 1.D0 + GRDTOL ) / 2.D0 )

            VERTEX( XCOORD(1) ) = ( 1.D0 + EXTENT( XCOORD(1) ) ) / 2.D0
            VERTEX( XCOORD(2) ) = ( 1.D0 + EXTENT( XCOORD(2) ) ) / 2.D0


C            WRITE (*,*) 'VERTEX(i)         = ', VERTEX(I)
C            WRITE (*,*) 'VERTEX(xcoord(1)) = ', VERTEX(XCOORD(1))
C            WRITE (*,*) 'VERTEX(xcoord(2)) = ', VERTEX(XCOORD(2))

            CALL CLEARD ( 3, RAYDIR )
            RAYDIR(I)           =   -S
            RAYDIR( XCOORD(1) ) =  2 * ABS(S)
            RAYDIR( XCOORD(2) ) =  3 * ABS(S)


            CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
            CALL CHCKXC   ( .FALSE., ' ', OK )

C           
C           In all cases at least 3 voxels should be hit.
C
            LABEL = 'NVX (#,#)'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, NVX, '>=', 3, 0, OK )

C
C           Check the first voxel returned. First compute
C           the expected values of its components.
C
            IF ( S .LT. 0 ) THEN
               XVOX(I) = 1
            ELSE
               XVOX(I) = EXTENT(I)
            END IF

            XVOX( XCOORD(1) ) = BRCKTI ( 1 + INT(VERTEX(XCOORD(1))), 
     .                                   1,
     .                                   EXTENT( XCOORD(1) )         )

            XVOX( XCOORD(2) ) = BRCKTI ( 1 + INT(VERTEX(XCOORD(2))), 
     .                                   1,
     .                                   EXTENT( XCOORD(2) )         )

            LABEL = 'VOXEL 1 (#,#)'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKAI ( LABEL, VOXLST(1,1), '=', XVOX, 3, OK )

C            WRITE (*,*) '1 XVOX = ', XVOX

C
C           We expect the coordinates of the second voxel to match,
C           except for coordinate indexed XCOORD(2), which should 
C           be incremented.
C
            XVOX( XCOORD(2) ) = XVOX( XCOORD(2) ) + 1

C
C           Instead of calling CHCKAI, we'll check each voxel
C           element individually.
C
C            WRITE (*,*) '2 XVOX = ', XVOX

            DO K = 1, 3

               LABEL = 'VOXEL 2 (#) '
               CALL REPMI ( LABEL, '#', K, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKSI ( LABEL, VOXLST(K,2), '=', XVOX(K), 0, OK )

            END DO
C
C           We expect the coordinates of the third voxel to match those
C           of the second, except for coordinate indexed XCOORD(1),
C           which should be incremented.
C
            XVOX( XCOORD(1) ) = XVOX( XCOORD(1) ) + 1

            DO K = 1, 3

               LABEL = 'VOXEL 3 (#) '
               CALL REPMI ( LABEL, '#', K, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKSI ( LABEL, VOXLST(K,3), '=', XVOX(K), 0, OK )

            END DO
   
         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C 

C
C     In the following cases, the vertex is slightly inside of the
C     voxel grid. Rays are chosen so that incorrect determination of
C     the vertex offset should lead to incorrect determination of the
C     second or third intersected voxels.
C     
C
C     Grid extents:
C
      EXTENT(1) = 30
      EXTENT(2) = 20
      EXTENT(3) = 10

      MAXNVX = MAXLST

      DO I = 1, 3

         XCOORD(1) = NEXT(I)
         XCOORD(2) = NEXT( XCOORD(1) )

         
         DO J = 0, 1

C
C --- Case: ------------------------------------------------------
C 
            TITLE = 'Vertex is just inside voxel grid; intersection ' 
     .      //      'exists. I = #; J = #.' 

            CALL REPMI ( TITLE, '#', I, TITLE )
            CALL REPMI ( TITLE, '#', J, TITLE )
            CALL CHCKXC   ( .FALSE., ' ', OK )
            
            CALL TCASE ( TITLE )


            S = 2*J - 1

C
C           Vertex is inside of grid and separated from the 
C           nearest boundary plane by a small amount.
C

            VERTEX(I) =     ( EXTENT(I) / 2.D0 ) +  
     .                  S * ( EXTENT(I) * ( 1.D0 - GRDTOL ) / 2.D0 )

            VERTEX( XCOORD(1) ) = ( 1.D0 + EXTENT( XCOORD(1) ) ) / 2.D0
            VERTEX( XCOORD(2) ) = ( 1.D0 + EXTENT( XCOORD(2) ) ) / 2.D0


C            WRITE (*,*) 'VERTEX(i)         = ', VERTEX(I)
C            WRITE (*,*) 'VERTEX(xcoord(1)) = ', VERTEX(XCOORD(1))
C            WRITE (*,*) 'VERTEX(xcoord(2)) = ', VERTEX(XCOORD(2))

            CALL CLEARD ( 3, RAYDIR )
            RAYDIR(I)           =   -S
            RAYDIR( XCOORD(1) ) =  2 * ABS(S)
            RAYDIR( XCOORD(2) ) =  3 * ABS(S)


            CALL XDDA ( VERTEX, RAYDIR, EXTENT, MAXNVX, NVX, VOXLST )
            CALL CHCKXC   ( .FALSE., ' ', OK )

C           
C           In all cases at least 3 voxels should be hit.
C
            LABEL = 'NVX (#,#)'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKSI ( LABEL, NVX, '>=', 3, 0, OK )

C
C           Check the first voxel returned. First compute
C           the expected values of its components.
C
            IF ( S .LT. 0 ) THEN
               XVOX(I) = 1
            ELSE
               XVOX(I) = EXTENT(I)
            END IF

            XVOX( XCOORD(1) ) = BRCKTI ( 1 + INT(VERTEX(XCOORD(1))), 
     .                                   1,
     .                                   EXTENT( XCOORD(1) )         )

            XVOX( XCOORD(2) ) = BRCKTI ( 1 + INT(VERTEX(XCOORD(2))), 
     .                                   1,
     .                                   EXTENT( XCOORD(2) )         )

            LABEL = 'VOXEL 1 (#,#)'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC   ( .FALSE., ' ', OK )

            CALL CHCKAI ( LABEL, VOXLST(1,1), '=', XVOX, 3, OK )

C            WRITE (*,*) '1 XVOX = ', XVOX

C
C           We expect the coordinates of the second voxel to match,
C           except for coordinate indexed XCOORD(2), which should 
C           be incremented.
C
            XVOX( XCOORD(2) ) = XVOX( XCOORD(2) ) + 1

C
C           Instead of calling CHCKAI, we'll check each voxel
C           element individually.
C
C            WRITE (*,*) '2 XVOX = ', XVOX

            DO K = 1, 3

               LABEL = 'VOXEL 2 (#) '
               CALL REPMI ( LABEL, '#', K, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKSI ( LABEL, VOXLST(K,2), '=', XVOX(K), 0, OK )

            END DO
C
C           We expect the coordinates of the third voxel to match those
C           of the second, except for coordinate indexed XCOORD(1),
C           which should be incremented.
C
            XVOX( XCOORD(1) ) = XVOX( XCOORD(1) ) + 1

            DO K = 1, 3

               LABEL = 'VOXEL 3 (#) '
               CALL REPMI ( LABEL, '#', K, LABEL )
               CALL CHCKXC   ( .FALSE., ' ', OK )

               CALL CHCKSI ( LABEL, VOXLST(K,3), '=', XVOX(K), 0, OK )

            END DO
   
         END DO

      END DO




C
C --- Case: ------------------------------------------------------
C 

C
C     The following tests exercise logic that deals with rays that
C     are nearly parallel to a coordinate axis.
C

      DO I = 1, 3

         XCOORD(1) = NEXT( I )
         XCOORD(2) = NEXT( XCOORD(1) )

C
C        The selected value of EXTENT(I) is less than MAXLST.
C        
         EXTENT( I         ) = 9999
         EXTENT( XCOORD(1) ) = 2
         EXTENT( XCOORD(2) ) = 2

         DO J = 0, 1

            DO K = 0, 1

C
C --- Case: ------------------------------------------------------
C 
               TITLE = 'Ray is nearly parallel to  '
     .         //      'coordinate axis test: I = #; '
     .         //      'J = #; K = #. Vertex is at face center.'

               CALL REPMI ( TITLE, '#', I,  TITLE )
               CALL REPMI ( TITLE, '#', J,  TITLE )
               CALL REPMI ( TITLE, '#', K,  TITLE )
               CALL CHCKXC( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               VERTEX( I         ) = 0.D0
               VERTEX( XCOORD(1) ) = 1.D0
               VERTEX( XCOORD(2) ) = 1.D0

C
C              Use small magnitudes for ray components that
C              are orthogonal to the Ith component.
C
               RAYDIR( I         ) = 1.D0
               RAYDIR( XCOORD(1) ) = (2*J - 1) * 1.D-300
               RAYDIR( XCOORD(2) ) = (2*K - 1) * 1.D-30

               CALL XDDA ( VERTEX, RAYDIR, EXTENT,
     .                     MAXLST, NVX,    VOXLST )
               CALL CHCKXC   ( .FALSE., ' ', OK )

C
C              Check the list.
C
               CALL T_CHKVOX ( VERTEX, RAYDIR, EXTENT, NVX, VOXLST )
               CALL CHCKXC   ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
               TITLE = 'Ray is nearly parallel to  '
     .         //      'coordinate axis test: I = #; '
     .         //      'J = #; K = #. Vertex is at face edge.'

               CALL REPMI ( TITLE, '#', I,  TITLE )
               CALL REPMI ( TITLE, '#', J,  TITLE )
               CALL REPMI ( TITLE, '#', K,  TITLE )
               CALL CHCKXC( .FALSE., ' ', OK )

               CALL TCASE ( TITLE )

               VERTEX( I         ) = 0.D0
               VERTEX( XCOORD(1) ) = 2.D0
               VERTEX( XCOORD(2) ) = 2.D0

C
C              Use small magnitudes for ray components that
C              are orthogonal to the Ith component.
C
               RAYDIR( I         ) = 1.D0
               RAYDIR( XCOORD(1) ) = (2*J - 1) * 1.D-300
               RAYDIR( XCOORD(2) ) = (2*K - 1) * 1.D-30

               CALL XDDA ( VERTEX, RAYDIR, EXTENT,
     .                     MAXLST, NVX,    VOXLST )
               CALL CHCKXC   ( .FALSE., ' ', OK )

C
C              Check the list. In this case the checking code
C              may fail to correctly compute the expected point
C              of exit from the grid. However, we can get the
C              checking algorithm to work by passing in a ray direction
C              for which the components orthogonal to the Ith axis
C              have been zeroed out.
C
               CALL CLEARD( 3, VTEMP ) 
               VTEMP(I) = 1.D0

               CALL T_CHKVOX ( VERTEX, VTEMP, EXTENT, NVX, VOXLST )
               CALL CHCKXC   ( .FALSE., ' ', OK )

            END DO

         END DO

      END DO
      


C**********************************************************************
C
C     Normal cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Trivial case: grid has one voxel. Vertex is in '
     .//           'the middle of the X = 1 face. Ray points inward.' )
 
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


C
C --- Case: ------------------------------------------------------
C 

C
C     Using a small but non-trivial grid, place vertices of rays
C     at the midpoint of each exterior voxel face. Place exit points
C     at the midpoint of each exterior voxel face on grid surfaces
C     other than the one containing the vertex.
C
      EXTENT(1) = 4
      EXTENT(2) = 5
      EXTENT(3) = 6

      MAXNVX    = MAXLST

      CALL CLEARD ( 3, RAYDIR )
      CALL CLEARD ( 3, VERTEX )
      CALL CLEARD ( 3, EGRESS )
      
      DO I = 1, 3 
C
C        Set the "horizontal" coordinates on the vertex face
C        of the grid: these are orthogonal to the Ith axis.
C
         VCOORD(1) = NEXT( I )
         VCOORD(2) = NEXT( VCOORD(1) )

C
C        Loop over both vertex faces orthogonal to axis I.
C        
         DO J = 0, 1
C
C           Loop over the exit faces. There's one face on
C           the opposite side from the vertex face, and four
C           faces normal to the horizontal coordinate axes.
C           
            DO K = 1, 3 
C
C              Set the "horizontal" coordinates on the exit face
C              of the grid: these are orthogonal to the Kth axis.
C
               XCOORD(1) = NEXT( K )
               XCOORD(2) = NEXT( XCOORD(1) )

               IF ( K .EQ. I ) THEN
                  UB = 0
               ELSE
                  UB = 1 
               END IF

C
C              Loop over both exit faces orthogonal to axis K,
C              except when K = I. In the latter case, the exit
C              face must be on the opposite side of the grid
C              from the vertex.
C        
               DO M = 0, UB
C
C                 S is (1-J) on the first pass; J on the second.
C            
                  S = (1-M) + ( 2*M - 1 ) * J

C
C                 Loop over the horizontal coordinates on the grid face
C                 containing the vertex. VR is the row index for the
C                 vertex; VC is the column index.
C
                  DO VR = 1, EXTENT( VCOORD(1) )

                     DO VC = 1, EXTENT( VCOORD(2) )

                        DO XR = 1, EXTENT( XCOORD(1) )

                           DO XC = 1, EXTENT( XCOORD(2) )


C
C --- Case: ------------------------------------------------------
C 
                              TITLE = 'Voxel face center to voxel '
     .                        //      'face center test: I = #; '
     .                        //      'J = #; K = #; M = #; VR = #; '
     .                        //      'VC = #; XR = #; XC = #.'

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
C
C                             Loop over the horizontal coordinates on
C                             the face containing the exit. XR is the
C                             row index for the vertex; XC is the
C                             column index.
C
C                             Set the vertex and exit points. It's
C                             inefficient to set coordinates here that
C                             could be set in outer loops, but the code
C                             is easier to read this way.
C
                              VERTEX( I         ) = J  * EXTENT(I) 
                              VERTEX( VCOORD(1) ) = VR - 0.5D0
                              VERTEX( VCOORD(2) ) = VC - 0.5D0

                              EGRESS( K         ) = S  * EXTENT(K) 
                              EGRESS( XCOORD(1) ) = XR - 0.5D0
                              EGRESS( XCOORD(2) ) = XC - 0.5D0

                              CALL VSUB ( EGRESS, VERTEX, RAYDIR )
C                             
C                             Compute the list of intersected voxels.
C
                              CALL XDDA ( VERTEX, RAYDIR, EXTENT,
     .                                    MAXNVX, NVX,    VOXLST )
                              CALL CHCKXC   ( .FALSE., ' ', OK )

C
C                             Check the list.
C
                              CALL T_CHKVOX ( VERTEX, RAYDIR, 
     .                                        EXTENT, NVX,   VOXLST )
                              CALL CHCKXC   ( .FALSE., ' ', OK )

                           END DO

                        END DO

                     END DO

                  END DO

               END DO

            END DO

         END DO

      END DO





C
C --- Case: ------------------------------------------------------
C 

C
C     Using a small but non-trivial grid, place vertices of rays
C     at the corners of each exterior voxel face. Place exit points
C     at the corners of each exterior voxel face on grid surfaces
C     other than the one containing the vertex.
C
      EXTENT(1) = 4
      EXTENT(2) = 4
      EXTENT(3) = 6

      MAXNVX    = MAXLST

      CALL CLEARD ( 3, RAYDIR )
      CALL CLEARD ( 3, VERTEX )
      CALL CLEARD ( 3, EGRESS )
      
      DO I = 1, 3 
C
C        Set the "horizontal" coordinates on the vertex face
C        of the grid: these are orthogonal to the Ith axis.
C
         VCOORD(1) = NEXT( I )
         VCOORD(2) = NEXT( VCOORD(1) )

C
C        Loop over both vertex faces orthogonal to axis I.
C        
         DO J = 0, 1
C
C           Loop over the exit faces. There's one face on
C           the opposite side from the vertex face, and four
C           faces normal to the horizontal coordinate axes.
C           
            DO K = 1, 3 
C
C              Set the "horizontal" coordinates on the exit face
C              of the grid: these are orthogonal to the Kth axis.
C
               XCOORD(1) = NEXT( K )
               XCOORD(2) = NEXT( XCOORD(1) )

               IF ( K .EQ. I ) THEN
                  UB = 0
               ELSE
                  UB = 1 
               END IF

C
C              Loop over both exit faces orthogonal to axis K,
C              except when K = I. In the latter case, the exit
C              face must be on the opposite side of the grid
C              from the vertex.
C        
               DO M = 0, UB
C
C                 S is (1-J) on the first pass; J on the second.
C            
                  S = (1-M) + ( 2*M - 1 ) * J

C
C                 Loop over the horizontal coordinates on the grid face
C                 containing the vertex. VR is the row index for the
C                 vertex; VC is the column index.
C
                  DO VR = 0, EXTENT( VCOORD(1) )

                     DO VC = 0, EXTENT( VCOORD(2) )

                        DO XR = 0, EXTENT( XCOORD(1) )

                           DO XC = 0, EXTENT( XCOORD(2) )


C
C --- Case: ------------------------------------------------------
C 
                              TITLE = 'Voxel face corner to voxel '
     .                        //      'face corner test: I = #; '
     .                        //      'J = #; K = #; M = #; VR = #; '
     .                        //      'VC = #; XR = #; XC = #.'

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
C
C                             Loop over the horizontal coordinates on
C                             the face containing the exit. XR is the
C                             row index for the vertex; XC is the
C                             column index.
C
C                             Set the vertex and exit points. It's
C                             inefficient to set coordinates here that
C                             could be set in outer loops, but the code
C                             is easier to read this way.
C
                              VERTEX( I         ) = J  * EXTENT(I) 
                              VERTEX( VCOORD(1) ) = VR 
                              VERTEX( VCOORD(2) ) = VC 

                              EGRESS( K         ) = S  * EXTENT(K) 
                              EGRESS( XCOORD(1) ) = XR 
                              EGRESS( XCOORD(2) ) = XC 

                              CALL VSUB ( EGRESS, VERTEX, RAYDIR )

                              IF ( .NOT. VZERO( RAYDIR ) ) THEN
C                             
C                                Compute the list of intersected voxels.
C
                                 CALL XDDA ( VERTEX, RAYDIR, EXTENT,
     .                                       MAXNVX, NVX,    VOXLST )
                                 CALL CHCKXC   ( .FALSE., ' ', OK )
  
C
C                                Check the list.
C
                                 CALL T_CHKVOX ( VERTEX, RAYDIR, 
     .                                           EXTENT, NVX,   VOXLST )
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



C
C --- Case: ------------------------------------------------------
C 

C
C     Using a realistic, large grid, use rays pointed toward the
C     origin. This is the classic "spear test."
C
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

C
C --- Case: ------------------------------------------------------
C 
            TITLE = 'Spear test: lon = # deg.; lat = # deg., '
     .      //      'extents are # # #.'

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
C
C           Unlike DSKX02, we need the vertex on the grid surface
C           for XDDA to work properly. 
C
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



C     
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END







C**********************************************************************
C
C     U T I L I T Y   R O U T I N E S
C
C**********************************************************************


C
C     Routine: T_CHKVOX
C
C     Check a solution produced by XDDA.
C
C     Note that, in the event that the ray intersects a voxel
C     edge or corner, the solution is not unique. Therefore 
C     computing an expected solution is not a general approach
C     to validating XDDA.
C
C     A solution must have the following properties:
C
C        1)  The first voxel contains the ray's vertex.
C
C        2)  The last voxel contains the ray's point of exit from the
C            grid.
C
C        3)  All voxels have coordinates in within the ranges defined
C            by the grid extents.
C
C        4)  The (i+1)st voxel differs from the ith voxel in only one
C            coordinate, and the difference has absolute value equal to
C            1.
C
C        5)  The ray intersects each voxel. The intersection test
C            must be carried out using a tolerance.
C
C
C     This routine signals an error if any of the above conditions
C     are not met. The calling test family should call CHCKXC 
C     immediately following each call to this routine.
C

      SUBROUTINE T_CHKVOX ( VERTEX, RAYDIR, EXTENT, N, VOXLST )
      IMPLICIT NONE

      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      INTEGER               EXTENT ( 3 )
      INTEGER               N
      INTEGER               VOXLST ( 3, * )

C
C     SPICELIB functions
C     
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN
C
C     Other functions
C

C
C     Local parameters
C

C
C     Tolerance used for testing whether a point is inside a 
C     given voxel:
C
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL    = 1.D-11 )

C
C     Expansion amount used for ray-voxel and ray-grid
C     intersection testing:
C
      DOUBLE PRECISION      EXPAND
      PARAMETER           ( EXPAND = 1.D-12 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 320 )

C
C     Local Variables  
C
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

C
C     Saved variables
C     
      SAVE                  

C
C     Initial values
C


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_CHKVOX' )

C
C     Make sure the solution has at least one voxel, so we 
C     can safely refer the first and last voxel in the list.
C      
      IF ( N .LT. 1 ) THEN

         CALL SETMSG ( 'Voxel list size is #; size must be positive.' )
         CALL ERRINT ( '#', N                                         )
         CALL SIGERR ( 'SPICE(BADLISTSIZE)'                           )
         CALL CHKOUT ( 'T_CHKVOX'                                     )
         RETURN

      END IF

C
C     Make sure the voxel coordinates are in range.
C
      DO I = 1, N

         DO J = 1, 3
            
            IF (      ( VOXLST(J,I) .LT. 1         ) 
     .           .OR. ( VOXLST(J,I) .GT. EXTENT(J) )  ) THEN
               
               CALL SETMSG ( 'Coordinate # of voxel # was #; valid '
     .         //            'range is 1:#.'                        )
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

C
C     Check the first voxel. 
C
      DO I = 1, 3
C
C        Do not use a tolerance for this check. Bracketing should
C        ensure the voxel is inside the grid (this includes the
C        surface). Computation of voxel coordinates for the voxel
C        containing the vertex should be portable.
C

         IF (     ( VERTEX(I) .LT. (VOXLST(I,1) - 1)  ) 
     .       .OR. ( VERTEX(I) .GT.  VOXLST(I,1)       )  ) THEN

            CALL SETMSG ( 'Coordinate # of vertex was #; valid '
     .         //         'range, given by voxel 1, is #:#.'       )
            CALL ERRINT ( '#', I                                   )
            CALL ERRDP  ( '#', VERTEX(I)                           )
            CALL ERRINT ( '#', VOXLST(I,1) - 1                     )
            CALL ERRINT ( '#', VOXLST(I,1)                         )
            CALL SIGERR ( 'SPICE(BADFIRSTVOX)'                     )
            CALL CHKOUT ( 'T_CHKVOX'                               )
            RETURN     

         END IF

      END DO


C
C     Check the last voxel. In order to do this, we'll need to
C     find the ray's point of exit from the grid.
C
C     Start by computing the vertex of a ray pointing in the opposite
C     direction as the input ray and passing through the input vertex.
C     The vertex of this ray is beyond the exit point. Call this vertex
C     EGRVTX.
C     
C     We'll expand the grid slightly before computing the exit point,
C     since the ray may lie along the grid's surface.. The extents will
C     be lengthened by 2*EXPAND, and the "origin" of the grid will be
C     shifted by -EXPAND in each coordinate. This will give us an
C     expanded grid concentric with the original. Let D be the diameter
C     of the grid (the supremum of the distances between any two points
C     in the grid).
C
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

         CALL SETMSG ( 'Uh-oh. Intersection of reversed ray with '
     .   //            'voxel grid was not found. Input ray vertex: '
     .   //            '# # #; direction: # # #; grid extents: # # #.' )
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

C
C     Bracket the egress within the voxel grid.
C
      DO I = 1, 3 
         EGRESS(I) = BRCKTD(  EGRESS(I),  0.D0,  DBLE(EXTENT(I))  )
      END DO

C
C     Ok, check the last voxel to see whether it contains EGRESS.
C
      DO I = 1, 3
C
C        Use a tolerance for this check.
C
         IF (     ( EGRESS(I) .LT. (VOXLST(I,N) - 1) - TOL ) 
     .       .OR. ( EGRESS(I) .GT.  VOXLST(I,N)      + TOL )  ) THEN

            CALL SETMSG ( 'Coordinate # of egress was #; valid '
     .         //         'range, given by voxel N, is #:#.'       )
            CALL ERRINT ( '#', I                                   )
            CALL ERRDP  ( '#', EGRESS(I)                           )
            CALL ERRDP  ( '#', VOXLST(I,N) - 1 - TOL               )
            CALL ERRDP  ( '#', VOXLST(I,N)     + TOL               )
            CALL SIGERR ( 'SPICE(BADLASSTVOX)'                     )
            CALL CHKOUT ( 'T_CHKVOX'                               )
            RETURN     

         END IF

      END DO
      
C
C     Check the coordinate differences of successive voxels in the
C     list. Each voxel should be different from the previous one
C     in one coordinate, and by a value of 1. A way of checking
C     this is to sum the absolute values of the differences of the
C     coordinates.
C
      DO I = 2, N
C
C        Note DELTA has INTEGER type.
C
         IDELTA = 0

         DO J = 1, 3
            IDELTA = IDELTA + ABS( VOXLST(J,I) - VOXLST(J,I-1) )
         END DO

         IF ( IDELTA .NE. 1 ) THEN

            CALL SETMSG ( 'Voxel at index # is # # #; voxel at '
     .      //            'index # is # # #. Absolute values of  '
     .      //            'coordinate differences should sum to 1.' )
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

C
C     Make sure the ray hits each voxel in the list. We'll expand the
C     voxels slightly before testing them. The extents will be
C     lengthened by 2*EXPAND, and the "origin" of each voxel will be
C     shifted by -EXPAND in each coordinate. This will give us an
C     expanded voxel concentric with the original.
C
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

               LNGMSG = 'Intersection of ray with voxel # '
     .         //       '(list size = #) was not  '
     .         //       'found. Input ray vertex: # # #; direction: '
     .         //       '# # #; grid extents: # # #. Voxel '
     .         //       'coordinates: # # #. Coordinates of ray '
     .         //       'intercept with previous voxel are # # #.'

            ELSE

               LNGMSG = 'Intersection of ray with voxel # '
     .         //       '(list size = #) was not  '
     .         //       'found. Input ray vertex: # # #; direction: '
     .         //       '# # #; grid extents: # # #. Voxel '
     .         //       'coordinates: # # #.'

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





