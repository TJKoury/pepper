!$Procedure ZZRAYBOX ( Ray-box intercept )

      SUBROUTINE ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Find the surface intercept of a ray on a specified box in
!     three-dimensional space.
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
!     DSK
!
! $ Keywords
!
!     MATH
!     GEOMETRY
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      BOXORI ( 3 )
      DOUBLE PRECISION      EXTENT ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     VERTEX     I   Vertex of ray.
!     RAYDIR     I   Direction vector of ray.
!     BOXORI     I   Box origin: corner having minimum coordinates.
!     EXTENT     I   Box extents in X, Y, Z directions.
!     XPT        O   Ray-box intercept.
!     FOUND      O   Flag indicating whether intercept was found.
!
! $ Detailed_Input
!
!     VERTEX     is the vertex of a ray in three dimensional space.
!
!     RAYDIR     is the direction vector of the input ray.
!
!     BOXORI     is a vector representing the "origin" of a box in
!                three-dimensional space. BOXORI is the corner of
!                the box at which each coordinate attains its
!                minimum value.
!
!     EXTENT     is an array containing the box's edge lengths in
!                the X, Y, and Z directions. All lengths must be
!                positive.
!
! $ Detailed_Output
!
!     XPT        is the point of intersection closest to VERTEX
!                of the ray and the box. If VERTEX lies within the
!                box, XPT is set equal to VERTEX.
!
!                XPT is undefined if FOUND is .FALSE.
!
!
!     FOUND      is set to .TRUE. if and only if the ray intersects
!                the box.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input ray is the zero vector, the error
!         SPICE(ZEROVECTOR) is signaled.
!
!     2)  If any element of EXTENT is non-positive, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See use in DSKX02.
!
! $ Restrictions
!
!     This is a private routine; its interface or functionality
!     may be changed without notice. SPICE user applications
!     should not call this routine directly.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     J.A. Bytof      (JPL)
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 06-JUN-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     compute ray-box intercept
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!
      INTEGER               LEFT
      PARAMETER           ( LEFT   = 1 )

      INTEGER               MIDDLE
      PARAMETER           ( MIDDLE = 2 )

      INTEGER               RIGHT
      PARAMETER           ( RIGHT  = 3 )

!
!     Local variables
!
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LIMIT
      DOUBLE PRECISION      MAXT
      DOUBLE PRECISION      NEAR   ( 3 )
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      PLNDST ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      SPHXPT ( 3 )
      DOUBLE PRECISION      SPHVTX ( 3 )
      DOUBLE PRECISION      T      ( 3 )
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )

      INTEGER               SECTOR ( 3 )
      INTEGER               I
      INTEGER               MAXIDX

      LOGICAL               SPHFND


!
!     Use discovery check-in.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     No intercept has been found yet.
!
      FOUND = .FALSE.

!
!     Check for a zero ray direction vector.
!
      IF ( VZERO(RAYDIR) ) THEN

         CALL CHKIN  ( 'ZZRAYBOX'                                      )
         CALL SETMSG ( 'Input ray direction was the zero vector; this ' &
         //            'vector must be non-zero.'                      )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                             )
         CALL CHKOUT ( 'ZZRAYBOX'                                      )
         RETURN

      END IF

      CALL VHAT ( RAYDIR, UDIR )

!
!     Check the box extents.
!
      IF (  MIN( EXTENT(1), EXTENT(2), EXTENT(3) ) .LE. 0.D0 ) THEN

         CALL CHKIN  ( 'ZZRAYBOX'                                    )
         CALL SETMSG ( 'All box extents should be strictly positive ' &
         //            'but the extents were #, #, #.'               )
         CALL ERRDP  ( '#',  EXTENT(1)                               )
         CALL ERRDP  ( '#',  EXTENT(2)                               )
         CALL ERRDP  ( '#',  EXTENT(3)                               )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                      )
         CALL CHKOUT ( 'ZZRAYBOX'                                    )
         RETURN

      END IF

!
!     Compute the coordinates of the center of the box, and compute
!     the offset of the ray's vertex from the center.
!
      DO I = 1, 3
         CENTER(I) = BOXORI(I) + EXTENT(I)/2
      END DO

      CALL VSUB  ( VERTEX, CENTER, OFFSET )

!
!     If the ray's vertex is inside the box, we consider the
!     vertex to be the intercept.
!
      IF (       ( ABS( OFFSET(1) ) .LE. EXTENT(1)/2 ) &
           .AND. ( ABS( OFFSET(2) ) .LE. EXTENT(2)/2 ) &
           .AND. ( ABS( OFFSET(3) ) .LE. EXTENT(3)/2 )  ) THEN

         CALL VEQU ( VERTEX, XPT )
         FOUND = .TRUE.

         RETURN

      END IF
!
!     Compute the intercept of the ray on the surface of a bounding
!     sphere that contains the box. Let R be the radius of this sphere.
!
      R = 0.5D0 * (1.D0 + 1.D-3) * VNORM( EXTENT )

      IF ( VNORM(OFFSET) .LT. R ) THEN
!
!        The vertex is already inside the bounding sphere.
!
         CALL VEQU ( OFFSET, SPHXPT )

      ELSE

         CALL SURFPT ( OFFSET, UDIR, R, R, R, SPHXPT, SPHFND )

         IF ( .NOT. SPHFND ) THEN
!
!           The ray misses the bounding sphere.
!
            RETURN

         END IF

      END IF

!
!     Shift the sphere intercept so as to be relative to the
!     box's origin. From this point on, we'll treat BOXORI
!     as the origin of the reference frame.
!
      DO I = 1, 3
         SPHVTX(I) = SPHXPT(I) + CENTER(I) - BOXORI(I)
      END DO

!
!     Classify the position of the vertex relative to the planes
!     bounding the box: each coordinate will be classified as
!     "left," "middle," or "right" depending on whether it is
!     less than the lower bound for that coordinate, between
!     the bounds, or greater than the upper bound.
!
      DO I = 1, 3

         IF ( SPHVTX(I) .LT. 0.D0 ) THEN

            SECTOR(I) = LEFT
            NEAR(I)   = 0.D0

         ELSE IF ( SPHVTX(I) .GT. EXTENT(I) ) THEN

            SECTOR(I) = RIGHT
            NEAR(I)   = EXTENT(I)

         ELSE
            SECTOR(I) = MIDDLE
            NEAR(I)   = 0.D0
         END IF

      END DO

!
!     At this point, SPHVTX is a point on the ray that is outside,
!     but close to, the box. SPHVTX is an offset from BOXORI we'll
!     need to add BOXORI to it to obtain the corresponding point in
!     the input reference frame.
!
!     We'll use SPHVTX as the new ray vertex.
!
!     Find the distances of the vertex's components from the nearest
!     bounding planes of the box; find the corresponding distances
!     along the ray that would be traveled in order to move each
!     component from the vertex to the nearest bounding plane. Call the
!     latter distance for the Ith coordinate T(I). We're only
!     interested in the vertex components that are "outside" the
!     bounding planes. Mark the values of T(I) for components in the
!     "middle" using the value -1.
!
!     Find the index of the maximum T value while we're at it. If
!     there's an intercept, it occurs at the point on the ray
!     corresponding to the maximum value of T.
!
      MAXIDX =  1
      MAXT   = -1.D0


      DO I = 1, 3

         T(I) = -1.D0

         IF ( SECTOR(I) .NE. MIDDLE ) THEN

            PLNDST(I) = NEAR(I) - SPHVTX(I)

!
!           Prepare for a "safe" division.
!
            LIMIT = 2 * R * ABS( UDIR(I) )

            IF ( ABS( PLNDST(I) ) .GT. LIMIT ) THEN
!
!              The ray can't get to the nearest bounding plane
!              before exiting the bounding sphere. No intersection
!              is possible.
!
               RETURN

            END IF
!
!           The magnitude of the following quotient is bounded by 2R.
!
            T(I) = PLNDST(I) / UDIR(I)

            IF ( T(I) .LT. 0.D0 ) THEN
!
!              This component of the ray is going in the wrong
!              direction. No intersection is possible.
!
               RETURN

            END IF

            IF ( T(I) .GT. MAXT ) THEN

               MAXIDX = I
               MAXT   = T(I)

            END IF

         END IF

      END DO

!
!     We should have a positive value of T for at least one
!     coordinate. However, if we don't, there's no intersection.
!
      IF ( MAXT .LT. 0.D0 ) THEN
         RETURN
      END IF

!
!     Compute the candidate intercept. Note that we're now working
!     in a frame centered at the box origin.
!
      CALL VLCOM ( 1.D0, SPHVTX, MAXT, UDIR, XPT )

!
!     Decide whether XPT is actually on the surface of the box.
!     Sharpen XPT as part of the process.
!
      DO I = 1, 3

         IF ( I .EQ. MAXIDX ) THEN
!
!           XPT is supposed to lie exactly on the bounding plane
!           orthogonal to the Ith axis and nearest to SPHVTX.
!
            XPT(I) = NEAR(I)

         ELSE

            IF ( SECTOR(I) .EQ. MIDDLE ) THEN
!
!              The Ith component of the vertex is between the
!              bounding planes for the Ith coordinate. If the
!              Ith component of XPT is outside these bounds,
!              the ray misses the box.
!
               IF (      ( XPT(I) .LT. 0.D0      ) &
                    .OR. ( XPT(I) .GT. EXTENT(I) ) ) THEN

                  RETURN

               END IF

            ELSE
!
!              The Ith component of the vertex SPHVTX is outside of the
!              bounding planes for the Ith coordinate. Since T(MAXIDX)
!              is greater than or equal to T(I), XPT(I) should be on or
!              past the bounding plane closest to SPHVTX(I). Sharpen
!              XPT(I) if necessary. If XPT(I) is beyond the bounding
!              plane farthest from SPHVTX(I), no intersection can
!              exist.
!
               IF ( SECTOR(I) .EQ. LEFT ) THEN
!
!                 Sharpen the Ith component of XPT.
!
                  XPT(I) = MAX( XPT(I), 0.D0 )

                  IF ( XPT(I) .GT. EXTENT(I) ) THEN
!
!                    The ray hits the MAXIDX face too far away from
!                    SPHVTX(I). There's no intersection with the box.
!
                     RETURN

                  END IF


               ELSE
!
!                 SECTOR(I) .EQ. RIGHT
!
!                 Sharpen the Ith component of XPT.
!
                  XPT(I) = MIN( XPT(I), EXTENT(I) )

                  IF ( XPT(I) .LT. 0.D0 ) THEN
!
!                    The ray hits the MAXIDX face too far away from
!                    SPHVTX(I). There's no intersection with the box.
!
                     RETURN

                  END IF

               END IF
!
!              End of block in which the Ith component of XPT is
!              either sharpened or found to be off the surface of the
!              box. This block deals with the components other than
!              MAXIDX.
!
            END IF
!
!           End of block in which the Ith component of XPT is either
!           sharpened or found to be off the surface of the box. This
!           block deals with all components.
!
         END IF

      END DO
!
!     End of loop in which XPT is either sharpened or found to be off
!     the surface of the box. Getting here means XPT is valid.
!
!     Shift XPT to the input reference frame.
!
      CALL VADD ( XPT,   BOXORI, VTEMP )
      CALL VEQU ( VTEMP,         XPT   )

      FOUND = .TRUE.

      RETURN
      END
