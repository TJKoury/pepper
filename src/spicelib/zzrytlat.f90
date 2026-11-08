!$Procedure ZZRYTLAT ( DSK, ray touches latitudinal element )

      SUBROUTINE ZZRYTLAT ( VERTEX, RAYDIR, BOUNDS, &
                            MARGIN, NXPTS,  XPT    )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Find nearest intersection to a given ray's vertex of that ray and
!     at latitudinal volume element. If the vertex is inside the
!     element, the vertex is considered to be the solution.
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
!     GEOMETRY
!     INTERCEPT
!     INTERSECTION
!     RAY
!     SURFACE
!     TOPOGRAPHY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dsktol.inc'

      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      MARGIN
      INTEGER               NXPTS
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               LONIDX
      PARAMETER           ( LONIDX = 1 )

      INTEGER               LATIDX
      PARAMETER           ( LATIDX = 2 )

      INTEGER               RADIDX
      PARAMETER           ( RADIDX = 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     BOUNDS     I   Bounds of latitudinal volume element.
!     MARGIN     I   Margin used for element expansion.
!     NXPTS      O   Number of intercept points.
!     XPT        O   Intercept.
!     LONIDX     P   Longitude index.
!     LATIDX     P   Latitude index.
!     RADIDX     P   Radius index.
!
! $ Detailed_Input
!
!     VERTEX,
!     RAYDIR     are, respectively, the vertex and direction vector of
!                the ray to be used in the intercept computation.
!
!                Both the vertex and ray direction must be represented
!                in the reference frame to which the volume element
!                boundaries correspond. The vertex is considered to be
!                an offset from the center of the reference frame
!                associated with the volume element.
!
!     BOUNDS     is a 2x3 array containing the bounds of a latitudinal
!                volume element. Normally this is the coverage boundary
!                of a DSK segment. In the element
!
!                   BOUNDS(I,J)
!
!                J is the coordinate index. J is one of
!
!                   { LONIDX, LATIDX, RADIDX }
!
!                I is the bound index.
!
!                   I = 1   ->   lower bound
!                   I = 2   ->   upper bound
!
!                If the longitude upper bound is not greater than the
!                longitude lower bound, a value greater than the upper
!                bound by 2*pi is used for the comparison.
!
!
!     MARGIN     is a scale factor used to effectively expand the
!                segment boundaries so as to include intersections
!                that lie slightly outside the volume element.
!
!
! $ Detailed_Output
!
!     XPT        is the intercept of the ray on the surface described
!                by the input coordinate bounds, if such an intercept
!                exists. If the ray intersects the surface at multiple
!                points, the one closest to the ray's vertex is
!                selected. XPT is valid if and only if FOUND is .TRUE.
!
!                XPT is expressed in the reference frame associated
!                with the inputs VERTEX and RAYDIR. XPT represents
!                an offset from the origin of the coordinate system.
!
!                XPT is valid only if NXPTS is set to 1.
!
!
!     NXPTS      is the number of intercept points of the ray and
!                the volume element.
!
!                Currently there are only two possible values for
!                NXPTS:
!
!                   1 for an intersection
!                   0 for no intersection
!
!                If the vertex is inside the element, NXPTS is
!                set to 1.
!
! $ Parameters
!
!
!     LONIDX     is the index of longitude in the second dimension of
!                BOUNDS.
!
!     LATIDX     is the index of latitude in the second dimension of
!                BOUNDS.
!
!     RADIDX     is the index of radius in the second dimension of
!                BOUNDS.
! $ Exceptions
!
!     For the sake of run-time efficiency, this routine does not
!     participate in call tracing. However, it's possible for
!     routines called by this routine to signal errors.
!
! $ Files
!
!     None. However, the input volume element boundaries normally have
!     been obtained from a segment in a loaded DSK file.
!
! $ Particulars
!
!     This routine sits on top of data DSK type-specific ray-segment
!     intercept routines such as DSKX02.
!
! $ Examples
!
!     See usage in ZZDSKBUX.
!
! $ Restrictions
!
!     This is a private routine. It is meant to be used only by the DSK
!     subsystem.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 18-JAN-2017 (NJB)
!
!        Based on ZZRYXLAT 20-MAR-2015 (NJB)
!
!        Original 01-OCT-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     find intercept of ray on latitudinal volume element
!
!-&



!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               FAILED

!
!     Local parameters
!

!
!     Radius expansion factor:
!
      DOUBLE PRECISION      RADFAC
      PARAMETER           ( RADFAC = 1.1D0 )


!
!     Element boundary indices:
!
      INTEGER               WEST
      PARAMETER           ( WEST   = 1 )

      INTEGER               EAST
      PARAMETER           ( EAST   = 2 )

      INTEGER               SOUTH
      PARAMETER           ( SOUTH  = 1 )

      INTEGER               NORTH
      PARAMETER           ( NORTH  = 2 )

      INTEGER               INNER
      PARAMETER           ( INNER  = 1 )

      INTEGER               OUTER
      PARAMETER           ( OUTER  = 2 )


!
!     Local variables
!
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      EASTB  ( 3 )
      DOUBLE PRECISION      EBACK  ( 3 )
      DOUBLE PRECISION      ENDPT2 ( 3 )
      DOUBLE PRECISION      LONCOV
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MAXR
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MINR
      DOUBLE PRECISION      MNDIST
      DOUBLE PRECISION      NEGDIR ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SRFX   ( 3 )
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      VLAT
      DOUBLE PRECISION      VLON
      DOUBLE PRECISION      VR
      DOUBLE PRECISION      WBACK  ( 3 )
      DOUBLE PRECISION      WESTB  ( 3 )
      DOUBLE PRECISION      XPT2   ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               NX

      LOGICAL               FOUND
      LOGICAL               INSIDE
      LOGICAL               XIN

!
!     Saved variables
!
      SAVE                  ORIGIN
      SAVE                  Z

!
!     Initial values
!
      DATA                  ORIGIN / 3 *   0.D0 /
      DATA                  Z      / 0.D0, 0.D0, 1.D0 /


!
!     CAUTION: this routine doesn't check in, so it won't
!     appear in the traceback if a lower-level routine
!     signals an error.
!

!
!     Determine whether the vertex is inside the element.
!     Use double the margin for this test, since we don't
!     want to have false negative tests for rays having
!     vertices lying on the expanded element boundary.
!
      CALL ZZINLAT ( VERTEX, BOUNDS, 2*MARGIN, 0, INSIDE )

      IF ( FAILED() ) THEN
         RETURN
      END IF

      IF ( INSIDE ) THEN
!
!        We know the answer.
!
         NXPTS = 1

         CALL VEQU ( VERTEX, XPT )

         RETURN

      END IF

!
!     Extract the segment's coordinate bounds into easily
!     readable variables.
!
      MINR = BOUNDS( INNER, RADIDX )
      MAXR = BOUNDS( OUTER, RADIDX )
!
!     Normalize the longitude bounds. After this step, the bounds will
!     be in order and differ by no more than 2*pi.
!
      CALL ZZNRMLON( BOUNDS(WEST,LONIDX), BOUNDS(EAST,LONIDX), ANGMRG, &
                     MINLON,              MAXLON                       )

      IF ( FAILED() ) THEN
         RETURN
      END IF

      MINLAT = BOUNDS( SOUTH, LATIDX )
      MAXLAT = BOUNDS( NORTH, LATIDX )

!
!     The vertex is outside the element.
!
!     Indicate no intersection to start.
!
      NXPTS  = 0
!
!     We'll use a unit length copy of the ray's direction vector.
!
      CALL VHAT ( RAYDIR, UDIR )
!
!     Initialize the distance to the closest solution. We'll keep track
!     of this quantity in order to compare competing solutions.
!
      MNDIST = DPMAX()
!
!     Find the intersection of the ray and outer bounding sphere, if
!     possible. Often this intersection is the closest to the vertex.
!     If the intersection exists and is on the boundary of the element,
!     it's a winner.
!
      CALL ZZRYXSPH ( VERTEX, UDIR, MAXR, SRFX, FOUND )

      IF ( .NOT. FOUND ) THEN
!
!        There are no intersections. The ray cannot hit the
!        volume element.
!
         RETURN

      END IF
!
!     Get the latitudinal coordinates of the ray's vertex.
!
      CALL RECLAT ( VERTEX, VR, VLON, VLAT )

!
!     The ray hits the outer bounding sphere. See whether
!     the longitude and latitude are within bounds, taking
!     the margin into account. Exclude the radius coordinate
!     from testing.
!
      CALL ZZINLAT ( SRFX, BOUNDS, MARGIN, RADIDX, XIN )

      IF ( FAILED() ) THEN
         RETURN
      END IF


      IF ( XIN ) THEN
!
!        This solution is a candidate.
!
         CALL VEQU ( SRFX, XPT )
         NXPTS  = 1

         IF ( VR .GT. MAXR ) THEN
!
!           The vertex is outside this sphere, and the segment
!           lies within the sphere.
!
!           No other intersection can be closer to the vertex;
!           we don't need to check the other surfaces.
!
            RETURN

         ELSE
!
!           We have a possible solution.
!
            MNDIST = VDIST( VERTEX, XPT )

         END IF

      END IF

!
!     So far there may be a candidate solution. We'll try the latitude
!     boundaries next.
!
!     For testing intersections with the latitude boundaries, we'll
!     need a far endpoint for the line segment on which to perform the
!     test.
!
      S = VNORM(VERTEX) + RADFAC * MAXR

      CALL VLCOM ( 1.D0, VERTEX, S, UDIR, ENDPT2 )

!
!     Now try the upper latitude bound. We can skip this test
!     if the upper bound is pi/2 radians.
!
      IF ( MAXLAT .LT. HALFPI() ) THEN
!
!        Let ANGLE be the angular separation of the surface of latitude
!        MAXLAT and the +Z axis. Note that the surface might be the
!        lower nappe of the cone.
!
         ANGLE = MAX ( 0.D0,  HALFPI() - MAXLAT )

         CALL INCNSG ( ORIGIN, Z,  ANGLE, VERTEX, &
                       ENDPT2, NX, SRFX,  XPT2    )

         IF ( FAILED() ) THEN
            RETURN
         END IF

         IF ( NX .GE. 1 ) THEN
!
!           See whether SRFX is in the element.
!
            CALL ZZINLAT ( SRFX, BOUNDS, MARGIN, LATIDX, XIN )

            IF ( FAILED() ) THEN
               RETURN
            END IF

            IF ( XIN ) THEN
!
!              SRFX is a candidate solution.
!
               DIST = VDIST( VERTEX, SRFX )

               IF ( DIST .LT. MNDIST ) THEN

                  CALL VEQU ( SRFX, XPT )

                  NXPTS  = 1

                  IF ( VLAT .GT. MAXLAT ) THEN
!
!                    Since the latitude of the vertex is greater than
!                    MAXLAT, this is the best solution, since the
!                    volume element is on the other side of the maximum
!                    latitude boundary.
!
                     RETURN

                  END IF
!
!                 This is the best solution seen so far.
!
                  MNDIST = DIST

               END IF

            END IF

            IF ( NX .EQ. 2 ) THEN
!
!              Check the second solution as well.
!
               CALL ZZINLAT ( XPT2, BOUNDS, MARGIN, LATIDX, XIN )

               IF ( FAILED() ) THEN
                  RETURN
               END IF


               IF ( XIN ) THEN
!
!                 XPT2 is a candidate solution.
!
                  DIST = VDIST( VERTEX, XPT2 )

                  IF ( DIST .LT. MNDIST ) THEN

                     CALL VEQU ( XPT2, XPT )

                     NXPTS  = 1
                     MNDIST = DIST
!
!                    This is the best solution seen so far.
!                    However, it's not necessarily the best
!                    solution. So we continue.
!
                  END IF

               END IF

            END IF
!
!           We've handled the second root, if any.
!
         END IF
!
!        We're done with the upper latitude boundary.
!
      END IF

!
!     Try the lower latitude bound. We can skip this test if the lower
!     bound is -pi/2 radians.
!
      IF (  MINLAT .GT. -HALFPI() ) THEN
!
!        Let ANGLE be the angular separation of the surface
!        of latitude MINLAT and the +Z axis. Note that the
!        surface might be the lower nappe of the cone.
!
         ANGLE = HALFPI() - MINLAT

         CALL INCNSG ( ORIGIN, Z,  ANGLE, VERTEX, &
                       ENDPT2, NX, SRFX,  XPT2    )

         IF ( FAILED() ) THEN
            RETURN
         END IF


         IF ( NX .GE. 1 ) THEN
!
!           See whether SRFX is in the element.
!
            CALL ZZINLAT ( SRFX, BOUNDS, MARGIN, LATIDX, XIN )

            IF ( FAILED() ) THEN
               RETURN
            END IF

            IF ( XIN ) THEN
!
!              SRFX is a candidate solution.
!
               DIST = VDIST( VERTEX, SRFX )

               IF ( DIST .LT. MNDIST ) THEN

                  CALL VEQU ( SRFX, XPT )

                  NXPTS  = 1

                  IF ( VLAT .LT. MINLAT ) THEN
!
!                    Since the latitude of the vertex is less than
!                    MINLAT, this is the best solution, since the
!                    volume element is on the other side of the minimum
!                    latitude boundary.
!
                     RETURN

                  END IF
!
!                 This is the best solution seen so far.
!
                  MNDIST = DIST

               END IF

            END IF

            IF ( NX .EQ. 2 ) THEN
!
!              Check the second solution as well.
!
               CALL ZZINLAT ( XPT2, BOUNDS, MARGIN, LATIDX, XIN )

               IF ( FAILED() ) THEN
                  RETURN
               END IF

               IF ( XIN ) THEN
!
!                 XPT2 is a candidate solution.
!
                  DIST = VDIST( VERTEX, XPT2 )

                  IF ( DIST .LT. MNDIST ) THEN

                     CALL VEQU ( XPT2, XPT )

                     NXPTS  = 1
                     MNDIST = DIST
!
!                    This is the best solution seen so far.
!                    However, it's not necessarily the best
!                    solution. So we continue.
!
                     RETURN

                  END IF

               END IF

            END IF

         END IF
!
!        We're done with the lower latitude boundary.
!
      END IF

!
!     Perform longitude boundary checks if the coverage is not
!     2*pi radians. Note that MAXLON > MINLON at this point.
!
      LONCOV = MAXLON - MINLON

      IF ( COS(LONCOV) .LT. 1.D0 ) THEN
!
!        We have distinct longitude boundaries. Go to work.
!
!
!        Check the longitude boundaries. Try the plane of western
!        longitude first.
!
!        The vector WESTB is an outward-facing vector normal to
!        the west boundary.
!
         CALL VPACK ( SIN(MINLON), -COS(MINLON), 0.D0, WESTB )


         S = RADFAC * ( VNORM(VERTEX) + MAXR )


         CALL ZZINRYPL ( VERTEX, UDIR, WESTB, 0.D0, S, NX, SRFX )

         IF ( NX .EQ. 1 ) THEN
!
!           We have one point of intersection. Determine whether it's a
!           candidate solution.  Don't use longitude in the following
!           inclusion test. Note that we'll perform a separate check
!           later in place of the longitude check.
!
            CALL ZZINLAT ( SRFX, BOUNDS, MARGIN, LONIDX, XIN )

            IF ( FAILED() ) THEN
               RETURN
            END IF


            IF ( XIN ) THEN
!
!              Make sure the intercept is not too far on the
!              "wrong" side of the Z axis.
!
               CALL UCRSS ( WESTB, Z, WBACK )

               IF (  VDOT(SRFX, WBACK) .LT. (MARGIN*MAXR)  ) THEN
!
!                 The intercept is either on the same side of the Z
!                 axis as the west face of the segment, or is very
!                 close to the Z axis.
!
                  DIST = VDIST( VERTEX, SRFX )

                  IF ( DIST .LT. MNDIST ) THEN
!
!                    Record the intercept, distance, and surface index.
!
                     CALL VEQU ( SRFX, XPT )

                     NXPTS  = 1
                     MNDIST = DIST

                  END IF

               END IF

            END IF

         END IF
!
!        We're done with the western boundary.
!
!
!        Try the plane of eastern longitude next.
!
!        The vector EASTB is an outward-facing vector normal to
!        the east boundary.

         CALL VPACK ( -SIN(MAXLON), COS(MAXLON), 0.D0, EASTB )

         CALL ZZINRYPL ( VERTEX, UDIR, EASTB, 0.D0, S, NX, SRFX )

         IF ( NX .EQ. 1 ) THEN
!
!           We have one point of intersection. Determine whether it's a
!           candidate solution.
!
            CALL ZZINLAT ( SRFX, BOUNDS, MARGIN, LONIDX, XIN )

            IF ( FAILED() ) THEN
               RETURN
            END IF

            IF ( XIN ) THEN
!
!              Make sure the intercept is not too far on the "wrong"
!              side of the Z axis.
!
               CALL UCRSS ( Z, EASTB, EBACK )

               IF (  VDOT(SRFX, EBACK) .LT. (MARGIN*MAXR)  ) THEN
!
!                 The intercept is either on the same side of the Z
!                 axis as the east face of the segment, or is very
!                 close to the Z axis.
!
                  DIST = VDIST( VERTEX, SRFX )

                  IF ( DIST .LT. MNDIST ) THEN
!
!                    Record the intercept, distance, and surface index.
!
                     CALL VEQU ( SRFX, XPT )

                     NXPTS  = 1
                     MNDIST = DIST

                  END IF

               END IF

            END IF

         END IF

      END IF
!
!     End of longitude boundary checks.
!

!
!     Find the intersection of the ray and inner bounding
!     sphere, if possible.
!
      IF ( MINR .GT. 0.D0 ) THEN

         CALL ZZRYXSPH ( VERTEX, UDIR, MINR, SRFX, FOUND )

         IF ( FOUND ) THEN
!
!           See whether this solution is in the element.
!
            CALL ZZINLAT ( SRFX, BOUNDS, MARGIN, RADIDX, XIN )

            IF ( FAILED() ) THEN
               RETURN
            END IF

            IF ( XIN ) THEN

               DIST = VDIST( VERTEX, SRFX )

               IF ( DIST .LT. MNDIST ) THEN
!
!                 Record the intercept, distance, and surface index.
!
                  CALL VEQU ( SRFX, XPT )

                  NXPTS  = 1
                  MNDIST = DIST

               END IF

            END IF

         END IF

!
!        Unlike the outer sphere, either intersection of the ray with
!        the inner sphere can be an optimal solution. We'll test for
!        the case where the intercept further from the ray's vertex is
!        the correct solution.
!
         CALL VMINUS( UDIR, NEGDIR )

         CALL ZZRYXSPH ( ENDPT2, NEGDIR, MINR, SRFX, FOUND )

         IF ( FOUND ) THEN

            CALL ZZINLAT ( SRFX, BOUNDS, MARGIN, RADIDX, XIN )

            IF ( FAILED() ) THEN
               RETURN
            END IF

            IF ( XIN ) THEN

               DIST = VDIST( VERTEX, SRFX )

               IF ( DIST .LT. MNDIST ) THEN
!
!                 Record the intercept, distance, and surface index.
!
                  CALL VEQU ( SRFX, XPT )

                  NXPTS  = 1
                  MNDIST = DIST

               END IF

            END IF

         END IF

      END IF

!
!     NXPTS and XPT are set.
!
      END
