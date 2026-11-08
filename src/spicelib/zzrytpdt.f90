!$Procedure ZZRYTPDT ( DSK, ray touches planetodetic element )

      SUBROUTINE ZZRYTPDT ( VERTEX, RAYDIR, BOUNDS, &
                            CORPAR, MARGIN, NXPTS,  XPT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Find nearest intersection to a given ray's vertex of the ray and
!     a planetodetic volume element. If the vertex is inside the
!     element, the vertex is considered to be the solution.
!
!     In the computation performed by this routine, ellipsoidal
!     surfaces are used, instead of surfaces of constant altitude, to
!     define boundaries of planetodetic volume elements. The element
!     defined by the input boundaries is contained in the element
!     bounded by the input latitude and longitude boundaries and by the
!     ellipsoidal surfaces.
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
      DOUBLE PRECISION      CORPAR ( * )
      DOUBLE PRECISION      MARGIN
      INTEGER               NXPTS
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               LONIDX
      PARAMETER           ( LONIDX = 1 )

      INTEGER               LATIDX
      PARAMETER           ( LATIDX = 2 )

      INTEGER               ALTIDX
      PARAMETER           ( ALTIDX = 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     BOUNDS     I   Bounds of planetodetic volume element.
!     CORPAR     I   Coordinate parameters.
!     MARGIN     I   Margin used for element expansion.
!     NXPTS      O   Number of intercept points.
!     XPT        O   Intercept.
!     LONIDX     P   Longitude index.
!     LATIDX     P   Latitude index.
!     ALTIDX     P   Altitude index.
!
! $ Detailed_Input
!
!     VERTEX,
!     RAYDIR     are, respectively, the vertex and direction vector of
!                the ray to be used in the intercept computation.
!
!                Both the vertex and ray direction must be represented
!                in the reference frame to which the planetodetic
!                volume element boundaries correspond. The vertex is
!                considered to be an offset from the center of the
!                reference frame associated with the element.
!
!     BOUNDS     is a 2x3 array containing the bounds of a planetodetic
!                volume element. Normally this is the coverage boundary
!                of a DSK segment. In the element
!
!                   BOUNDS(I,J)
!
!                J is the coordinate index. J is one of
!
!                   { LONIDX, LATIDX, ALTIDX }
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
!     RE,
!     F          are, respectively, the equatorial radius and
!                flattening coefficient associated with the
!                planetodetic coordinate system in which the input
!                volume element is described.
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
!                by the segment, if such an intercept exists. If the
!                ray intersects the surface at multiple points, the
!                one closest to the ray's vertex is selected. XPT is
!                valid if and only if FOUND is .TRUE.
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
!     LONIDX     is the index of longitude in the second dimension of
!                BOUNDS.
!
!     LATIDX     is the index of latitude in the second dimension of
!                BOUNDS.
!
!     ALTIDX     is the index of altitude in the second dimension of
!                BOUNDS.
! $ Exceptions
!
!     1)  If MARGIN is negative, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
!     2)  If the input ray direction vector is zero, the error
!         SPICE(ZEROVECTOR) will be signaled.
!
!     3)  Any errors that occur while calculating the ray-surface
!         intercept will be signaled by routines in the call tree
!         of this routine.
!
! $ Files
!
!     None. However, the input segment boundaries normally have
!     been obtained from a loaded DSK file.
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
!-    SPICELIB Version 1.0.0, 19-JAN-2017 (NJB)
!
!-&

! $ Index_Entries
!
!     find intercept of ray on planetodetic volume element
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
      DOUBLE PRECISION      VSEP

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO
      LOGICAL               ZZPDPLTC

!
!     Local parameters
!

!
!     Altitude expansion factor:
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

      INTEGER               LOWER
      PARAMETER           ( LOWER  = 1 )

      INTEGER               UPPER
      PARAMETER           ( UPPER  = 2 )

      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )

!
!     Local variables
!
      DOUBLE PRECISION      AMNALT
      DOUBLE PRECISION      AMXALT
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      APEX   ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      EASTB  ( 3 )
      DOUBLE PRECISION      EBACK  ( 3 )
      DOUBLE PRECISION      EMAX
      DOUBLE PRECISION      EMIN
      DOUBLE PRECISION      ENDPT2 ( 3 )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LONCOV
      DOUBLE PRECISION      MAXALT
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MAXR
      DOUBLE PRECISION      MINALT
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MNDIST
      DOUBLE PRECISION      NEGDIR ( 3 )
      DOUBLE PRECISION      PMAX
      DOUBLE PRECISION      PMIN
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SRFX   ( 3 )
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      VTXANG
      DOUBLE PRECISION      VTXLVL
      DOUBLE PRECISION      VTXOFF ( 3 )
      DOUBLE PRECISION      WBACK  ( 3 )
      DOUBLE PRECISION      WESTB  ( 3 )
      DOUBLE PRECISION      XPT2   ( 3 )
      DOUBLE PRECISION      XINCPT
      DOUBLE PRECISION      YINCPT
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               NX

      LOGICAL               FOUND
      LOGICAL               INSIDE
      LOGICAL               XIN
      LOGICAL               XVAL1
      LOGICAL               XVAL2

!
!     Saved variables
!
      SAVE                  Z

!
!     Initial values
!
      DATA                  Z      / 0.D0, 0.D0, 1.D0 /



      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZRYTPDT' )


      IF ( MARGIN .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Margin must be non-negative but was #.' )
         CALL ERRDP  ( '#', MARGIN                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                 )
         CALL CHKOUT ( 'ZZRYTPDT'                               )
         RETURN

      END IF

      IF ( VZERO(RAYDIR) ) THEN

         CALL SETMSG ( 'The ray''s direction was the zero vector.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                         )
         CALL CHKOUT ( 'ZZRYTPDT'                                  )
         RETURN

      END IF


!
!     Determine whether the vertex is inside the element.
!
      CALL ZZINPDT ( VERTEX, BOUNDS, CORPAR, MARGIN, NONE, INSIDE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZRYTPDT' )
         RETURN
      END IF

      IF ( INSIDE ) THEN
!
!        We know the answer.
!
         NXPTS = 1

         CALL VEQU ( VERTEX, XPT )

         CALL CHKOUT ( 'ZZRYTPDT' )
         RETURN

      END IF

!
!     Get semi-axis lengths of the reference spheroid.
!
      RE = CORPAR(1)
      F  = CORPAR(2)
      RP = RE * ( 1.D0 - F )

!
!     Extract the segment's coordinate bounds into easily
!     readable variables.
!
      MINALT = BOUNDS( LOWER, ALTIDX )
      MAXALT = BOUNDS( UPPER, ALTIDX )
!
!     Normalize the longitude bounds. After this step, the bounds will
!     be in order and differ by no more than 2*pi.
!
      CALL ZZNRMLON( BOUNDS(WEST,LONIDX), BOUNDS(EAST,LONIDX), ANGMRG, &
                     MINLON,              MAXLON                      )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZRYTPDT' )
         RETURN
      END IF

      MINLAT = BOUNDS( SOUTH, LATIDX )
      MAXLAT = BOUNDS( NORTH, LATIDX )

!
!     Compute adjusted altitude bounds, taking margin into
!     account.
!
      AMNALT = MINALT  -  MARGIN * ABS(MINALT)
      AMXALT = MAXALT  +  MARGIN * ABS(MAXALT)

!
!     Generate semi-axis lengths of inner and outer bounding
!     ellipsoids.
!
      IF ( RE .GE. RP ) THEN
!
!        The reference spheroid is oblate.
!
         CALL ZZELLBDS ( RE,   RP,   AMXALT, AMNALT, &
                         EMAX, PMAX, EMIN,   PMIN   )

      ELSE
!
!        The reference spheroid is prolate.
!
         CALL ZZELLBDS ( RP,   RE,   AMXALT, AMNALT, &
                         PMAX, EMAX, PMIN,   EMIN   )

      END IF

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZRYTPDT' )
         RETURN
      END IF

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
!     Find the intersection of the ray and outer bounding ellipsoid, if
!     possible. Often this intersection is the closest to the vertex.
!     If the intersection exists and is on the boundary of the element,
!     it's a winner.
!
      CALL SURFPT ( VERTEX, UDIR, EMAX, EMAX, PMAX, SRFX, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZRYTPDT' )
         RETURN
      END IF

      IF ( .NOT. FOUND ) THEN
!
!        There are no intersections. The ray cannot hit the volume
!        element.
!
         CALL CHKOUT ( 'ZZRYTPDT' )
         RETURN

      END IF

!
!     The ray hits the outer bounding ellipsoid. See whether
!     the longitude and latitude are within bounds, taking
!     the margin into account. Exclude the altitude coordinate
!     from testing.
!
      CALL ZZINPDT ( SRFX, BOUNDS, CORPAR, MARGIN, ALTIDX, XIN )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZRYTPDT' )
         RETURN
      END IF

      IF ( XIN ) THEN
!
!        This solution is a candidate.
!
         CALL VEQU ( SRFX, XPT )
         NXPTS  = 1

!
!        Find the level surface parameter of the vertex relative
!        to the adjusted outer bounding ellipsoid.
!
         VTXLVL =     ( VERTEX(1)/EMAX )**2 &
                   +  ( VERTEX(2)/EMAX )**2 &
                   +  ( VERTEX(3)/PMAX )**2

         IF ( VTXLVL .GT. 1.D0 ) THEN
!
!           The vertex is outside this ellipsoid, and the DSK segment
!           lies within the ellipsoid.
!
!           No other intersection can be closer to the vertex;
!           we don't need to check the other surfaces.
!
            CALL CHKOUT ( 'ZZRYTPDT' )
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
      MAXR = MAX ( EMAX, PMAX )

      S    = VNORM(VERTEX) + RADFAC * MAXR

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

!
!        Compute the Z coordinate of the apex of the latitude cone.
!
         CALL ZZELNAXX ( RE, RP, MAXLAT, XINCPT, YINCPT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZRYTPDT' )
            RETURN
         END IF

         APEX(1) = 0.D0
         APEX(2) = 0.D0
         APEX(3) = YINCPT

!
!        Find the offset of the ray's vertex from the cone's apex,
!        and find the angular separation of the offset from the +Z
!        axis. This separation enables us to compare the latitude of
!        the vertex to the latitude boundary without making a RECGEO
!        call to compute the planetodetic coordinates of the vertex.
!
!        (The comparison will be done later.)
!
         CALL VSUB ( VERTEX, APEX, VTXOFF )

         VTXANG = VSEP ( VTXOFF, Z )

!
!        Check for intersection of the ray with the latitude cone.
!
         CALL INCNSG ( APEX, Z, ANGLE, VERTEX, ENDPT2, NX, SRFX, XPT2 )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZRYTPDT' )
            RETURN
         END IF

!
!        Unlike the case of latitudinal coordinates, for planetodetic
!        coordinates, the surface of the latitude cone does not
!        coincide with the set of points having that latitude (which is
!        equal to pi/2 - the cone's angular separation from the +Z
!        axis). The subset of the cone having the specified latitude is
!        truncated by the X-Y plane. If we ignore round-off errors, we
!        can assert that the Z-coordinate of a point having the given
!        planetodetic latitude must match the direction of the nappe of
!        the cone: positive if ANGLE < pi/2, negative if ANGLE > pi/2,
!        and 0 if ANGLE = pi/2.
!
!        However, we cannot ignore round-off errors. For a cone having
!        angle from its central axis of nearly pi/2, it's possible for
!        a valid ray-cone intercept to be on the "wrong" side of the
!        X-Y plane due to round-off errors. So we use a more robust
!        check to determine whether an intercept should be considered
!        to have the same latitude as the cone.
!
!        Check all intercepts.
!
         IF ( NX .GT. 0 ) THEN
!
!           Check the first intercept.
!
            XVAL1 =  ZZPDPLTC( RE, F, SRFX, MAXLAT )
            XVAL2 =  .FALSE.

            IF ( NX .EQ. 2 ) THEN
!
!              Check the second intercept.
!
               XVAL2 =  ZZPDPLTC( RE, F, XPT2, MAXLAT )

            END IF

            IF ( XVAL1 .AND. ( .NOT. XVAL2 ) ) THEN

               NX = 1

            ELSE IF (  XVAL2  .AND.  (.NOT. XVAL1 )  ) THEN
!
!              Only the second solution is valid. Overwrite
!              the first.
!
               NX = 1

               CALL VEQU( XPT2, SRFX )

            ELSE IF (  ( .NOT. XVAL1 )  .AND.  ( .NOT. XVAL2 )  ) THEN
!
!              Neither solution is valid.
!
               NX = 0

            END IF

         END IF




         IF ( NX .GE. 1 ) THEN
!
!           The ray intercept SRFX lies on the upper latitude boundary.
!
!           See whether SRFX meets the longitude and proxy altitude
!           constraints.
!
            CALL ZZINPDT ( SRFX, BOUNDS, CORPAR, MARGIN, LATIDX, XIN )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZRYTPDT' )
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


                  IF ( VTXANG .LT. ANGLE ) THEN

                     IF (      ( MAXLAT    .LT. 0.D0 ) &
                          .OR. ( VERTEX(3) .GT. 0.D0 )  ) THEN
!
!                       If MAXLAT is negative, the vertex offset
!                       being outside the cone is enough to
!                       guarantee the planetodetic latitude of the
!                       vertex is greater than that of the cone.
!
!                       If MAXLAT is non-negative, the angle of the
!                       vertex offset relative to the +Z axis is not
!                       enough; we need the vertex to lie above the
!                       X-Y plane as well.
!
!                       Getting here means one of these conditions
!                       was met.
!
!                       Since the latitude of the vertex is greater
!                       than MAXLAT, this is the best solution, since
!                       the volume element is on the other side of the
!                       maximum latitude boundary.
!
                        CALL CHKOUT ( 'ZZRYTPDT' )
                        RETURN

                     END IF

                  END IF
!
!                 This is the best solution seen so far, but we
!                 need to check the remaining boundaries.
!
                  MNDIST = DIST

               END IF

            END IF

            IF ( NX .EQ. 2 ) THEN
!
!              Check the second solution as well.
!
               CALL ZZINPDT ( XPT2,   BOUNDS, CORPAR, &
                              MARGIN, LATIDX, XIN    )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZRYTPDT' )
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


!        Compute the Z coordinate of the apex of the latitude cone.
!
         CALL ZZELNAXX ( RE, RP, MINLAT, XINCPT, YINCPT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZRYTPDT' )
            RETURN
         END IF

         APEX(1) = 0.D0
         APEX(2) = 0.D0
         APEX(3) = YINCPT

         CALL INCNSG ( APEX,   Z,  ANGLE, VERTEX, &
                       ENDPT2, NX, SRFX,  XPT2    )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZRYTPDT' )
            RETURN
         END IF

!
!        Find the offset of the ray's vertex from the cone's apex,
!        and find the angular separation of the offset from the +Z
!        axis. This separation enables us to compare the latitude of
!        the vertex to the latitude boundary without making a RECGEO
!        call to compute the planetodetic coordinates of the vertex.
!
!        (The comparison will be done later.)
!
         CALL VSUB ( VERTEX, APEX, VTXOFF )

         VTXANG = VSEP ( VTXOFF, Z )

!
!        Check whether the latitude of the intercept can be
!        considered to match that of the cone.
!
         IF ( NX .GT. 0 ) THEN
!
!           Check the first intercept.
!
            XVAL1 =  ZZPDPLTC( RE, F, SRFX, MINLAT )
            XVAL2 =  .FALSE.

            IF ( NX .EQ. 2 ) THEN
!
!              Check the second intercept.
!
               XVAL2 =  ZZPDPLTC( RE, F, XPT2, MINLAT )

            END IF

            IF ( XVAL1 .AND. ( .NOT. XVAL2 ) ) THEN

               NX = 1

            ELSE IF (  XVAL2  .AND.  (.NOT. XVAL1 )  ) THEN
!
!              Only the second solution is valid. Overwrite
!              the first.
!
               NX = 1

               CALL VEQU( XPT2, SRFX )

            ELSE IF (  ( .NOT. XVAL1 )  .AND.  ( .NOT. XVAL2 )  ) THEN
!
!              Neither solution is valid.
!
               NX = 0

            END IF

         END IF


         IF ( NX .GE. 1 ) THEN
!
!           The ray intercept SRFX lies on the lower latitude boundary.
!
!           See whether SRFX meets the longitude and proxy altitude
!           constraints.
!
            CALL ZZINPDT ( SRFX, BOUNDS, CORPAR, MARGIN, LATIDX, XIN )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZRYTPDT' )
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

                  IF ( VTXANG .GT. ANGLE ) THEN

                     IF (      ( MINLAT    .GT. 0.D0 ) &
                          .OR. ( VERTEX(3) .LT. 0.D0 )  ) THEN
!
!                       If MINLAT is positive, the vertex offset
!                       being outside the cone is enough to
!                       guarantee the planetodetic latitude of the
!                       vertex is less than that of the cone.
!
!                       If MINLAT is non-positive, the angle of the
!                       vertex offset relative to the +Z axis is not
!                       enough; we need the vertex to lie below the
!                       X-Y plane as well.
!
!                       Getting here means one of these conditions
!                       was met.
!
!                       Since the latitude of the vertex is less than
!                       than MINLAT, this is the best solution, since
!                       the volume element is on the other side of the
!                       minimum latitude boundary.
!
                        CALL CHKOUT ( 'ZZRYTPDT' )
                        RETURN

                     END IF

                  END IF
!
!                 This is the best solution seen so far, but we
!                 need to check the remaining boundaries.

                  MNDIST = DIST

               END IF

            END IF

            IF ( NX .EQ. 2 ) THEN
!
!              Check the second solution as well.
!
               CALL ZZINPDT ( XPT2,   BOUNDS, CORPAR, &
                              MARGIN, LATIDX, XIN    )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZRYTPDT' )
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
                     CALL CHKOUT ( 'ZZRYTPDT' )
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
            CALL ZZINPDT ( SRFX, BOUNDS, CORPAR, MARGIN, LONIDX, XIN )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZRYTPDT' )
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
         CALL VPACK ( -SIN(MAXLON), COS(MAXLON), 0.D0, EASTB )

         CALL ZZINRYPL ( VERTEX, UDIR, EASTB, 0.D0, S, NX, SRFX )

         IF ( NX .EQ. 1 ) THEN
!
!           We have one point of intersection. Determine whether it's a
!           candidate solution.
!
            CALL ZZINPDT ( SRFX, BOUNDS, CORPAR, MARGIN, LONIDX, XIN )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZRYTPDT' )
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
!     Find the intersection of the ray and lower bounding
!     ellipsoid, if possible.
!
      CALL SURFPT ( VERTEX, UDIR, EMIN, EMIN, PMIN, SRFX, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZRYTPDT' )
         RETURN
      END IF


      IF ( FOUND ) THEN
!
!        See whether this solution is in the element.
!
         CALL ZZINPDT ( SRFX, BOUNDS, CORPAR, MARGIN, ALTIDX, XIN )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZRYTPDT' )
            RETURN
         END IF


         IF ( XIN ) THEN

            DIST = VDIST( VERTEX, SRFX )

            IF ( DIST .LT. MNDIST ) THEN
!
!              Record the intercept, distance, and surface index.
!
               CALL VEQU ( SRFX, XPT )

               NXPTS  = 1
               MNDIST = DIST

            END IF

         END IF

      END IF

!
!     Unlike the outer ellipsoid, either intersection of the ray with
!     the inner ellipsoid might be a valid solution. We'll test for the
!     case where the intersection farther from the ray's vertex is the
!     correct one.
!
      CALL VMINUS( UDIR, NEGDIR )

      CALL SURFPT ( ENDPT2, NEGDIR, EMIN, EMIN, PMIN, SRFX, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZRYTPDT' )
         RETURN
      END IF


      IF ( FOUND ) THEN

         CALL ZZINPDT ( SRFX, BOUNDS, CORPAR, MARGIN, ALTIDX, XIN )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZRYTPDT' )
            RETURN
         END IF


         IF ( XIN ) THEN

            DIST = VDIST( VERTEX, SRFX )

            IF ( DIST .LT. MNDIST ) THEN
!
!              Record the intercept, distance, and surface index.
!
               CALL VEQU ( SRFX, XPT )

               NXPTS  = 1
!
!              There's no need to update MNDIST at this point.
!
            END IF

         END IF

      END IF

!
!     NXPTS and XPT are set.
!
      CALL CHKOUT ( 'ZZRYTPDT' )
      RETURN
      END
