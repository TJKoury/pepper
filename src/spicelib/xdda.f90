!$Procedure XDDA  ( list voxels intersected by a ray )

      SUBROUTINE XDDA ( VERTEX, RAYDIR, GRDEXT, MAXNVX, NVX, VOXLST )

! $ Abstract
!
!     Return a list of voxels that a given ray intersects in a given
!     voxel grid.
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
!     GRID
!     INTERSECTION
!     PLATE
!     VOXEL
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      GRDTOL
      PARAMETER           ( GRDTOL = 1.D-12 )

      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      INTEGER               GRDEXT ( 3 )
      INTEGER               MAXNVX
      INTEGER               NVX
      INTEGER               VOXLST ( 3, * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     GRDTOL     P   Tolerance for vertex distance from grid.
!     VERTEX     I   Voxel grid coordinates of ray's vertex.
!     RAYDIR     I   Direction vector of ray.
!     GRDEXT     I   Dimensions of grid in voxel units.
!     MAXNVX     I   Maximum value of VOXLST.
!     NVX        O   Number of voxels in the VOXLST list.
!     VOXLST     O   List of voxels intersected by ray.
!
! $ Detailed_Input
!
!     VERTEX   is the voxel grid coordinates of ray's vertex. These
!              coordinates are zero-based, double precision offsets from
!              the grid's origin. The units of the coordinates are
!              voxels, that is, voxel edge lengths.
!
!     RAYDIR   is the direction vector of ray from VERTEX.
!
!     GRDEXT   is the integer 3-vector containing the voxel grid
!              extents. These are the dimensions of the voxel grid in
!              voxel units, in the X, Y, and Z directions respectively.
!
!     MAXNVX   is the maximum number of voxel coordinate sets that can
!              be stored in VOXLST.
!
! $ Detailed_Output
!
!     NVX      is the number of voxel coordinate sets contained in
!              VOXLST.
!
!     VOXLST   is the list of coordinate sets of voxels intersected by
!              ray. Elements
!
!                 VOXLST(J,I), J = 1, 3
!
!              are the coordinates of the Ith voxel in the list. These
!              coordinates are 1-based integer values.
!
!              The voxels in the output list are ordered by increasing
!              distance from the ray's vertex.
!
! $ Parameters
!
!     GRDTOL   is a tolerance value used to determine whether
!              VERTEX is too far from the voxel grid. The Ith
!              component of VERTEX must not differ from the
!              Ith coordinate of the nearest grid point by more
!              than
!
!                  GRDTOL * EXTENT(I)
!
! $ Exceptions
!
!     1)  If the input RAYDIR has all zero components, the error
!         SPICE(ZEROVECTOR) is signaled.
!
!     2)  If the maximum output list size MAXNVX is non-positive, the
!         error SPICE(INVALIDSIZE) is signaled.
!
!     3)  If any element of the grid extents array GRDEXT is
!         non-positive, the error SPICE(BADDIMENSIONS) is signaled.
!
!     4)  If the ray's vertex is neither inside, nor within a small
!         distance from, the voxel grid, the error
!         SPICE(VERTEXNOTINGRID) is signaled. See the description of the
!         parameter GRDTOL.
!
!     5)  If the value of the NVX counter (number of intersected voxels)
!         exceeds the size of the VOXLST input vector, the error
!         SPICE(ARRAYTOOSMALL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports use of a spatial index for rapid
!     selection of plates that could be hit by a specified ray.
!
! $ Examples
!
!     See the routine DSKX02 for a usage example.
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
!     J.A. Bytof         (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.1, 26-OCT-2021 (NJB) (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Typo correction: the description of the error denoted
!        by the short message SPICE(VERTEXNOTINGRID) had been
!        the negative of what was intended.
!
!-    SPICELIB Version 3.1.0, 02-FEB-2016 (NJB)
!
!        Updated to call ZZINGRD rather than INGRD.
!        Minor updates were made to header I/O sections.
!
!-    SPICELIB Version 3.0.0, 11-JUL-2014 (NJB) (EDW) (BVS) (JAB)
!
!        Previously released as DSKLIB:
!
!        DSKLIB Version 3.0.0, 11-JUL-2014 (NJB)
!
!           Bug fix: a correction was made to the computation of
!           the vertex offset from the bounding planes of the
!           voxel containing the vertex.
!
!           Minor edits were made to comments.
!
!        Last update was 05-JUN-2014 (NJB)
!
!           Bug fix: the use of the MOD function led to a 1-voxel
!           size error when the input ray's vertex was on the
!           voxel grid boundary.
!
!           An error check for invalid grid dimensions was added.
!
!           Code to prevent arithmetic overflow was added.
!
!           Code was added to prevent the values AX2ERR and AX3ERR from
!           ever becoming negative when the components of the ray's
!           direction vector in the corresponding directions are zero or
!           too small for a voxel step in those directions to occur.
!
!           Renamed the routine's arguments, except for NVX.
!
!           Detailed output descriptions were updated to refer to
!           voxel coordinates rather than IDs. References to sorting
!           were deleted.
!
!           In-line comments now explain the routine's algorithm.
!           Old comments that are no longer applicable were deleted.
!
!        DSKLIB Version 2.1.0, 26-JUL-2010 (NJB)
!
!           Bug fix: voxel space coordinates of input
!           vertex are now bracketed within the voxel
!           grid.
!
!           This prevents round-off errors from occurring
!           when the vertex is slightly outside the grid,
!           but may not be appropriate for all applications.
!           Therefore it may make sense to make this a
!           private routine.
!
!        DSKLIB Version 2.0.0, 20-APR-2010 (NJB)
!
!           Removed commented out lines declaring and calling VOX2ID.
!
!        DSKLIB Version 1.1.0, 08-OCT-2009 (NJB)
!
!           Updated header.
!
!           Bug fix: driving axis for intercept computation is
!           now determined by largest component of ray direction vector.
!           This fix was made long before this header update.
!
!        DSKLIB Version 1.1.0, 19-OCT-2004 (EDW)
!
!           Added logic to remove duplicate voxel IDs from
!           the return list. Extended programming comments.
!
!        DSKLIB Version 1.0.1, 26-AUG-2002 (BVS)
!
!           Replaced WRITE with normal error reporting calls.
!
!        DSKLIB Version 1.0.0, 03-FEB-1999 (JAB)
!
!-&


! $ Index_Entries
!
!     list voxels intersected by a ray
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      DPMAX

      INTEGER               BRCKTI

      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!
      DOUBLE PRECISION      EPS
      PARAMETER           ( EPS = 1.D-20 )

!
!     Local variables
!
      DOUBLE PRECISION      AX2ERR
      DOUBLE PRECISION      AX3ERR
      DOUBLE PRECISION      LIMIT
      DOUBLE PRECISION      MAXCMP
      DOUBLE PRECISION      VTXOFF ( 3 )
      DOUBLE PRECISION      S12
      DOUBLE PRECISION      S13

      INTEGER               I
      INTEGER               IAXIS  ( 3 )
      INTEGER               ICOORD ( 3 )
      INTEGER               INTVTX ( 3 )
      INTEGER               NEXT   ( 3 )
      INTEGER               STEP   ( 3 )

      LOGICAL               ZZINGRD

!
!     Saved variables
!
      SAVE                  NEXT

!
!     Initial values
!
      DATA                  NEXT   /  2,   3,   1   /

!
!     Use discovery check-in.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

!
!     The algorithm below efficiently determines the set of voxels
!     intersected by the input ray.
!
!     This algorithm doesn't compute the intersections of the ray
!     with the boundaries of the voxels, nor does it ever compute
!     the coordinates of any point on the ray. Instead, it keeps
!     track of the voxel boundary planes that the ray passes through.
!
!     The algorithm starts out by determining which voxel contains the
!     ray's vertex. It computes the distances from the vertex to the
!     "next" voxel boundary planes---those that the ray is headed
!     towards. It maintains measurements that enable it to determine
!     which boundary plane is hit next. The voxel on the other side of
!     that intersection point is the next voxel the ray goes through.
!     In the case of ties, any of the candidate "next" voxels may be
!     selected. The "next" voxel is added to the output voxel list, the
!     measurements of relative distances to the next boundaries are
!     updated, and the algorithm continues in this fashion until a
!     voxel outside the grid is detected.
!
!     The relative distance measurements from the ray's vertex to
!     the "next" boundary planes are defined as follows:
!
!        -  For the primary ray direction---this is the direction
!           corresponding to the component of largest magnitude of the
!           ray's direction vector---the distance is just the
!           difference of the primary coordinates of the next plane and
!           of the ray's vertex.
!
!        -  For each axis orthogonal to the primary one, the algorithm
!           computes the length of the projection onto the primary axis
!           of the portion of the ray extending from the vertex to the
!           "next" voxel boundary plane orthogonal to that non-primary
!           axis. From that projection length the distance from the
!           vertex to the boundary in the primary direction is
!           subtracted.
!
!           For the non-primary axes, these differences are stored in
!           the respective variables
!
!              AX2ERR
!              AX3ERR
!
!           When AX2ERR is negative, the ray will hit the next voxel
!           boundary orthogonal to the "second" axis (having its
!           index stored in the variable IAXIS(2)) before it hits
!           the next boundary orthogonal to the primary axis. The
!           quantity AX3ERR behaves similarly.
!
!           If both AX2ERR and AX3ERR are negative, the more negative
!           value marks the boundary plane that is hit first.
!
!     The axes will be re-labeled using the variable IAXIS. IAXIS(1)
!     will be the index of the primary axis.
!
!     There are a few numeric issues to consider:
!
!        1)  The ratios of the components of the ray's direction vector
!            are computed and stored in the variables S12 and S13. Very
!            small components acting as denominators could cause
!            arithmetic overflow.
!
!        2)  The quantities S12 and S13, while representable as double
!            precision numbers, can be quite large. These quantities
!            may be added repeatedly to the terms AX2ERR and AX3ERR,
!            respectively. These additions could potentially result
!            in arithmetic overflow.
!
!     Both of these problems are addressed by the following observation:
!
!        If a component of the ray direction vector is small enough, and
!        the corresponding component of the ray's vertex is not on a
!        voxel boundary, the ray will exit the grid before reaching a
!        bounding plane orthogonal to that component of the direction
!        vector.
!
!        If the above situation holds, but the ray's vertex is already
!        on a boundary plane orthogonal to the small component, then
!        the ray will exit the grid before hitting a parallel boundary
!        plane.
!
!     So we can safely treat very small direction components as zero.
!

!
!     Check if ray direction vector is a zero vector.
!
      IF ( VZERO( RAYDIR ) ) THEN

         CALL CHKIN  ( 'XDDA'                     )
         CALL SETMSG ( 'Ray is the zero vector.'  )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'        )
         CALL CHKOUT ( 'XDDA'                     )
         RETURN

      END IF

!
!     Check the voxel grid dimensions.
!
      IF ( MIN( GRDEXT(1), GRDEXT(2), GRDEXT(3) ) .LT. 1 ) THEN

         CALL CHKIN  ( 'XDDA'                                    )
         CALL SETMSG ( 'Voxel grid dimensions must be strictly ' &
         //            'positive but are # # #.'                 )
         CALL ERRINT ( '#', GRDEXT(1)                            )
         CALL ERRINT ( '#', GRDEXT(2)                            )
         CALL ERRINT ( '#', GRDEXT(3)                            )
         CALL SIGERR ( 'SPICE(BADDIMENSIONS)'                    )
         CALL CHKOUT ( 'XDDA'                                    )
         RETURN

      END IF

!
!     Make sure the vertex is not too far from the voxel grid.
!
      DO I = 1, 3

         IF (     ( VERTEX(I) .LT.     -GRDTOL  * GRDEXT(I) ) &
             .OR. ( VERTEX(I) .GT. (1 + GRDTOL) * GRDEXT(I) )  ) THEN

            CALL CHKIN  ( 'XDDA'                                  )
            CALL SETMSG ( 'Vertex # # # is outside of voxel grid ' &
            //            'defined by extents # # #.'             )
            CALL ERRDP  ( '#', VERTEX(1)                          )
            CALL ERRDP  ( '#', VERTEX(2)                          )
            CALL ERRDP  ( '#', VERTEX(3)                          )
            CALL ERRINT ( '#', GRDEXT(1)                          )
            CALL ERRINT ( '#', GRDEXT(2)                          )
            CALL ERRINT ( '#', GRDEXT(3)                          )
            CALL SIGERR ( 'SPICE(VERTEXNOTINGRID)'                )
            CALL CHKOUT ( 'XDDA'                                  )
            RETURN

         END IF

      END DO

!
!     The maximum output voxel array size must be positive.
!
      IF ( MAXNVX .LT. 1 ) THEN

         CALL CHKIN  ( 'XDDA'                     )
         CALL SETMSG ( 'Maximum voxel list size must be positive ' &
         //            'but was #.'                               )
         CALL ERRINT ( '#', MAXNVX                                )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                       )
         CALL CHKOUT ( 'XDDA'                                     )
         RETURN

      END IF

!
!     Find the largest component of the direction vector.
!
      IAXIS(1) = 1
      MAXCMP   = ABS( RAYDIR(1) )

      DO I = 2, 3

         IF ( ABS( RAYDIR(I) ) .GT. MAXCMP  ) THEN
            IAXIS(1) = I
            MAXCMP   = ABS( RAYDIR(I) )
         END IF

      END DO

!
!     Set the indices of the orthogonal components of the direction
!     vector.  We maintain a right-handed relationship between the axes
!     labeled by IAXIS(1), IAXIS(2), and IAXIS(3):  the third axis is
!     the cross product of the first and second.
!
      IAXIS(2) = NEXT ( IAXIS(1) )
      IAXIS(3) = NEXT ( IAXIS(2) )

!
!     Which voxel contains the vertex? Truncate the vertex
!     coordinates to integers. Add 1 to each coord to compensate
!     for 1 based counting.
!
      DO I = 1, 3

         INTVTX(I) = INT ( VERTEX(IAXIS(I)) )

         ICOORD(I) = 1 + INTVTX(I)

         ICOORD(I) = BRCKTI (  ICOORD(I),  1,  GRDEXT( IAXIS(I) )  )

         VOXLST( IAXIS(I), 1 ) = ICOORD(I)

      END DO

!
!     Initialize the counter for number of voxels the ray intercepts.
!     The bracketing done above ensures that the coordinates ICOORD of
!     the voxel considered to contain ray's vertex (there is a choice
!     to be made if the vertex lies on a voxel boundary) are within the
!     grid.

      NVX = 1

!
!     Calculate the relative location of vertex within the voxel. The
!     coordinates of a voxel's corners are integer values with each
!     voxel side length 1 (in voxel coords).
!
!     The variable VTXOFF usually has components equal to the
!     fractional parts of the corresponding components of VERTEX(
!     IAXIS(I) ), but the components of VTXOFF may be as large as 1 and
!     are never less than 0.
!
      DO I = 1, 3

         VTXOFF(I) = BRCKTD( VERTEX( IAXIS(I) ) - (ICOORD(I)-1), &
                             0.D0, &
                             1.D0                               )
      END DO

!
!     Compute the lower limit on the magnitudes of RAYDIR( IAXIS(2) )
!     and of RAYDIR( IAXIS(3) ) for which we'll treat those components
!     of the direction vector as non-zero.
!
      LIMIT = ( EPS / GRDEXT( IAXIS(1) ) )  *  ABS( RAYDIR(IAXIS(1)) )
!
!     If the magnitude of RAYDIR( IAXIS(J) ), J = 2 or 3, is below
!     LIMIT, then the ray can pass through the entire grid in the
!     IAXIS(1) direction without its IAXIS(J) component changing by
!     more than EPS. We'll treat this case as though the IAXIS(J)
!     component of the ray were 0.
!
!
!     Determine the error term initial values and increments.
!
!
      AX2ERR = DPMAX()
      AX3ERR = AX2ERR
      S12    = 0.D0
      S13    = 0.D0

!
!     Compute the initial relative distance measurement AX2ERR
!     for the non-primary axis IAXIS(2).
!
      IF (   ABS(  RAYDIR( IAXIS(2) )  ) .GT.  LIMIT  ) THEN
!
!        For any line segment along the ray, S12 is the ratio of the
!        magnitudes of the projections of the segment in the primary
!        and the IAXIS(2) directions.
!
         S12 = ABS (  RAYDIR( IAXIS(1) ) / RAYDIR( IAXIS(2) )  )


         IF ( RAYDIR(IAXIS(1)) .GT. 0.D0 ) THEN
!
!           The primary component of the ray's direction is positive.
!           The distance to the next boundary plane in the primary
!           direction is
!
!              1.D0 - VTXOFF( IAXIS(1) )
!
            IF( RAYDIR(IAXIS(2)) .GT. 0.D0 ) THEN
!
!              The IAXIS(2) component of the ray's direction is
!              positive. The distance to the next boundary plane for
!              the that axis is
!
!                 1.D0 - VTXOFF(2)
!
!              The corresponding change along the primary axis is
!
!                 S12 * ( 1.D0 - VTXOFF(2) )
!
!              The "error" term for IAXIS(2) is this value minus the
!              distance from the vertex to the next boundary in the
!              primary direction.
!
               AX2ERR = S12*(1.D0 - VTXOFF(2)) + VTXOFF(1) - 1.D0

            ELSE
!
!              The IAXIS(2) component of the ray's direction is
!              negative. The distance to the next boundary plane for
!              the that axis is
!
!                 VTXOFF(2)
!
!              The corresponding change along the primary axis is
!
!                 S12 * VTXOFF(2)
!
!              The "error" term for IAXIS(2) is this value minus the
!              distance from the vertex to the next boundary in the
!              primary direction.
!
               AX2ERR = S12*VTXOFF(2) + VTXOFF(1) - 1.D0

            END IF

         ELSE
!
!           The primary component of the ray's direction is negative.
!           The distance to the next boundary plane in the primary
!           direction is
!
!              VTXOFF( IAXIS(1) )

            IF( RAYDIR(IAXIS(2)) .GT. 0.D0 ) THEN
!
!              The IAXIS(2) component of the ray's direction is
!              positive. The distance to the next boundary plane for
!              the that axis is
!
!                 1.D0 - VTXOFF(2)
!
!              The corresponding change along the primary axis is
!
!                 S12 * ( 1.D0 - VTXOFF(2) )
!
!              The "error" term for IAXIS(2) is this value minus the
!              distance from the vertex to the next boundary in the
!              primary direction.

               AX2ERR = S12*(1.D0 - VTXOFF(2)) - VTXOFF(1)

            ELSE
!
!              The IAXIS(2) component of the ray's direction is
!              negative. The distance to the next boundary plane for
!              the that axis is
!
!                 VTXOFF(2)
!
!              The corresponding change along the primary axis is
!
!                 S12 * VTXOFF(2)
!
!              The "error" term for IAXIS(2) is this value minus the
!              distance from the vertex to the next boundary in the
!              primary direction.

               AX2ERR = S12*VTXOFF(2) - VTXOFF(1)

            END IF

         END IF

      END IF


!
!     Computations of AX3ERR are analogous to those of AX2ERR.
!     See the comments above.
!
      IF (  ABS(  RAYDIR( IAXIS(3) )  )  .GT.  LIMIT  ) THEN
!
!        For any line segment along the ray, S13 is the ratio of the
!        magnitudes of the projections of the segment in the primary
!        and the IAXIS(3) directions.
!
         S13 = ABS(  RAYDIR( IAXIS(1) ) / RAYDIR( IAXIS(3) )  )

         IF ( RAYDIR( IAXIS(1) ) .GT. 0.D0 ) THEN

            IF( RAYDIR( IAXIS(3) ) .GT. 0.D0 ) THEN
               AX3ERR = S13*(1.D0 - VTXOFF(3) ) + VTXOFF(1) - 1.D0
            ELSE
               AX3ERR = S13*VTXOFF(3) + VTXOFF(1) - 1.D0
            END IF

         ELSE

            IF(  RAYDIR( IAXIS(3) ) .GT. 0.D0  ) THEN
               AX3ERR = S13*(1.D0 - VTXOFF(3)) - VTXOFF(1)
            ELSE
               AX3ERR = S13*VTXOFF(3) - VTXOFF(1)
            END IF

         END IF

      END IF

!
!     The "steps" set below are the amounts by which any voxel
!     coordinate changes when the "next" voxel is identified. Only one
!     coordinate changes at a time. The magnitude of each coordinate
!     step is always an integer. The signs of the steps are those of
!     the corresponding components of the ray's direction vector.
!
!     We treat direction components smaller than LIMIT as though
!     they were zero. Note that the IAXIS(1) component of the
!     ray will always have magnitude greater than LIMIT.
!
      DO I = 1, 3

         IF ( RAYDIR( IAXIS(I) ) .GT. LIMIT ) THEN
!
!           Positive component direction, positive step.
!
            STEP(I) =  1

         ELSE IF ( RAYDIR( IAXIS(I) ) .LT. -LIMIT ) THEN
!
!           Negative component direction, negative step.
!
            STEP(I) = -1

         ELSE
!
!           No component in this direction, no step.
!
            STEP(I) =  0

         END IF

      END DO

!
!     Follow the ray until it exits the voxel grid.
!
      DO WHILE ( ZZINGRD( GRDEXT, VOXLST(1,NVX) ) )

         IF ( (AX2ERR .LT. 0.D0) .OR. (AX3ERR .LT. 0.D0) ) THEN
!
!           Ray has crossed over into the next voxel in IAXIS(2) or
!           IAXIS(3)
!
            IF ( AX2ERR .LT. AX3ERR ) THEN
!
!              The boundary plane orthogonal to axis IAXIS(2) was hit.
!
               ICOORD(2)  = ICOORD(2) + STEP(2)
               AX2ERR     = AX2ERR    + S12
               NVX        = NVX       + 1

            ELSE
!
!              The boundary plane orthogonal to axis IAXIS(3) was hit.
!
               ICOORD(3)  = ICOORD(3) + STEP(3)
               AX3ERR     = AX3ERR    + S13
               NVX        = NVX       + 1

            END IF

         ELSE
!
!           No change in IAXIS(2) or IAXIS(3), step in IAXIS(1).
!
            ICOORD(1) = ICOORD(1) + STEP(1)
            NVX       = NVX       + 1

            IF ( STEP(2) .NE. 0 ) THEN
               AX2ERR = AX2ERR - 1.D0
            END IF

            IF ( STEP(3) .NE. 0 ) THEN
               AX3ERR = AX3ERR - 1.D0
            END IF

         END IF

!
!        Check we have room in VOXLST.
!
         IF( NVX .GT. MAXNVX ) THEN

            CALL CHKIN  ( 'XDDA' )
            CALL SETMSG ( 'Index larger than array. ' &
            //            'Index = #1. Array size = #2.' )
            CALL ERRINT ( '#1', NVX                      )
            CALL ERRINT ( '#2', MAXNVX                   )
            CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'         )

            CALL CHKOUT ( 'XDDA' )
            RETURN

         END IF

!
!        Pack the voxel indices into VOXLST using
!        the values calculated in this loop pass.
!
         DO I = 1, 3
            VOXLST ( IAXIS(I), NVX ) = ICOORD(I)
         END DO

      END DO

!
!     Subtract one off the voxel count since the final voxel
!     exists outside the grid.
!
      NVX = NVX - 1

      RETURN
      END
