!$Procedure ZZINLAT ( DSK, in latitudinal element? )

      SUBROUTINE ZZINLAT ( P, BOUNDS, MARGIN, EXCLUD, INSIDE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Test a point represented by a set of latitudinal coordinates for
!     inclusion in a specified latitudinal volume element. The test is
!     performed using margins for the element.
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
!     DSK
!     GEOMETRY
!     INTERSECTION
!     SURFACE
!     TOPOGRAPHY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'dsktol.inc'

      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      MARGIN
      INTEGER               EXCLUD
      LOGICAL               INSIDE

      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )

      INTEGER               LONIDX
      PARAMETER           ( LONIDX = 1 )

      INTEGER               LATIDX
      PARAMETER           ( LATIDX = 2 )

      INTEGER               RADIDX
      PARAMETER           ( RADIDX = 3 )

      DOUBLE PRECISION      LATMRG
      PARAMETER           ( LATMRG = 1.D-8 )


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     P          I   Input point.
!     BOUNDS     I   Coordinate bounds of element.
!     MARGIN     I   Margin used for inclusion testing.
!     EXCLUD     I   Index of coordinate to exclude from test.
!     INSIDE     O   Flag indicating whether point is in element.
!     LATMRG     P   Margin for latitude near the poles.
!     LONALI     P   Longitude shift margin.
!     ANGMRG     P   Angular rounding margin.
!     NONE       P   Meaning: exclude nothing.
!     LONIDX     P   Longitude index. Also used for exclusion.
!     LATIDX     P   Latitude index. Also used for exclusion.
!     RADIDX     P   Radius index. Also used for exclusion.
!
! $ Detailed_Input
!
!     P          is a point expressed in Cartesian coordinates. The
!                point is to be checked to determine whether it is
!                inside the volume element specified by BOUNDS.
!
!     BOUNDS     is an 2x3 array containing the bounds of a volume
!                element expressed in latitudinal coordinates. BOUNDS
!                defines the volume element used in the comparison. In
!                the element
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
!                If necessary, the longitude bounds are normalized
!                for the comparison. The normalized bounds differ
!                by no more than 2*pi and are in increasing order.
!                See ZZNRMLON for details.
!
!                The aliasing margin LONALI, the greedy margin, and the
!                angular rounding margin ANGMRG are used for longitude
!                comparisons. ANGMRG is used if its magnitude exceeds
!                that of the greedy margin. See the Parameters section
!                below for further information.
!
!
!     MARGIN     is a fraction used to expand the volume element for
!                inclusion testing.
!
!                   - The outer radius is scaled by (1+MARGIN).
!                     The inner radius is scaled by (1-MARGIN).
!
!                   - Latitude bounds are extended by MARGIN radians,
!                     within the interval -pi/2 : pi/2.
!
!                   - For any input point having latitude "not close"
!                     to pi/2 or -pi/2, the element's longitude
!                     interval is extended by
!
!                        MARGIN / cos(LAT)
!
!                     where LAT is the latitude of the input point.
!
!                     Here "close to" means "within LATMRG radians of."
!
!                   - For any input point having latitude close to
!                     pi/2 or -pi/2, comparison against the longitude
!                     bounds of the volume element is omitted if
!                     MARGIN > 0.
!
!
!
!     EXCLUD     is either a coordinate index or the parameter NONE.
!
!                If EXCLUD is set to one of
!
!                   { LONIDX, LATIDX, RADIDX }
!
!                then the indicated coordinate is excluded from
!                comparison with the corresponding volume element
!                boundaries.
!
!                If EXCLUD is set to NONE, all coordinates are
!                compared.
!
!                Exclusion of coordinates is used in cases where a
!                point is known to be on a level surface of a given
!                coordinate. For example, if a point is on the sphere
!                of radius equal to the upper radius bound, radius need
!                not be used in the comparison and in fact can't be
!                meaningfully compared, due to round-off errors.
!
! $ Detailed_Output
!
!     INSIDE     is a logical flag that is set to .TRUE. if and
!                only if the input coordinates represent a
!                point inside or on the surface of the volume
!                element, according to the comparisons that are
!                performed.
!
!                The value of INSIDE is not affected by the value
!                of any excluded coordinate.
!
! $ Parameters
!
!
!     LATMRG     Margin for determining whether the input latitude is
!                "close" to the poles. Units are radians. This value
!                should be kept in sync with the default "greedy"
!                margin.
!
!     LONALI     is a longitude shift margin. If the input longitude
!                coordinate LON is less than the longitude lower bound
!                by more than this amount, the value LON + 2*pi is
!                used in the longitude comparison, provided that the
!                comparison is not excluded.
!
!                See dsktol.inc for details.
!
!     ANGMRG     is an angular rounding margin. If the input longitude
!                is outside of the longitude bounds by less than this
!                amount, the longitude is considered to be within the
!                bounds. ANGMRG is used when the magnitude of the
!                "greedy" margin is less than ANGMRG. ANGMRG is not
!                used for computations involving coordinates other than
!                longitude.
!
!                This margin is distinct from the "greedy" segment
!                selection margin, which is used to expand a volume
!                element for the purpose of determining whether a given
!                point lies in that element.
!
!                See dsktol.inc for details.
!
!     NONE       when used as a value of the input argument EXCLUD,
!                indicates that no coordinates should be excluded from
!                comparison.
!
!     LONIDX     is the index of longitude in the second dimension of
!                BOUNDS. When used as a value of the input argument
!                EXCLUD, indicates that longitude should be excluded
!                from comparison.
!
!     LATIDX     is the index of latitude in the second dimension of
!                BOUNDS. When used as a value of the input argument
!                EXCLUD, indicates that latitude should be excluded
!                from comparison.
!
!
!     RADIDX     is the index of radius in the second dimension of
!                BOUNDS. When used as a value of the input argument
!                EXCLUD, indicates that radius should be excluded
!                from comparison.
!
! $ Exceptions
!
!     1)  If MARGIN is negative, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
!     2)  If EXCLUD is outside of the range 0:3, the error
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
!     See usage in ZZRYTLAT.
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
!-    SPICELIB Version 1.0.0, 03-JUN-2016 (NJB)
!
!        Original version 03-OCT-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     test point against latitudinal element using margin
!
!-&




!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      TWOPI

      LOGICAL               RETURN

!
!     Local parameters
!

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
      DOUBLE PRECISION      AMAXR
      DOUBLE PRECISION      AMINR
      DOUBLE PRECISION      AMAXLT
      DOUBLE PRECISION      AMINLT
      DOUBLE PRECISION      AMAXLO
      DOUBLE PRECISION      AMINLO
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      HPI
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LONMRG
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MAXR
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MINR
      DOUBLE PRECISION      PI2
      DOUBLE PRECISION      R
      DOUBLE PRECISION      SMAX
      DOUBLE PRECISION      SMIN

      LOGICAL               FIRST


      SAVE                  FIRST
      SAVE                  PI2
      SAVE                  HPI

      DATA                  FIRST / .TRUE. /
      DATA                  PI2   / -1.D0  /
      DATA                  HPI   / -1.D0  /

!
!     Use discovery check-in.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      IF ( FIRST ) THEN

         PI2   = TWOPI()
         HPI   = HALFPI()

         FIRST = .FALSE.

      END IF

!
!     Get the latitudinal coordinates of the input point.
!
      CALL RECLAT ( P, R, LON, LAT )


!
!     Handle the simpler zero-margin case separately.
!
      IF ( MARGIN .EQ. 0.D0 ) THEN

         CALL ZZINLAT0 ( R, LON, LAT, BOUNDS, EXCLUD, INSIDE )

         RETURN

      ELSE IF ( MARGIN .LT. 0.D0 ) THEN

         CALL CHKIN  ( 'ZZINLAT'                                )
         CALL SETMSG ( 'Margin must be non-negative but was #.' )
         CALL ERRDP  ( '#', MARGIN                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                 )
         CALL CHKOUT ( 'ZZINLAT'                                )
         RETURN

      END IF


      IF (  ( EXCLUD .LT. 0 ) .OR. ( EXCLUD .GT. 3 )  ) THEN

         CALL CHKIN  ( 'ZZINLAT'                                    )
         CALL SETMSG ( 'EXCLUD must be in the range 0:3 but was #.' )
         CALL ERRINT ( '#', EXCLUD                                  )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                     )
         CALL CHKOUT ( 'ZZINLAT'                                    )
         RETURN

      END IF

!
!     Special case: if the input point is within distance MARGIN
!     from the origin, and the minimum radius of the volume element
!     is less than or equal to MARGIN, the point is inside.
!
      IF ( R .LE. MARGIN ) THEN

         IF ( BOUNDS(INNER, RADIDX) .LE. MARGIN ) THEN

            INSIDE = .TRUE.

            RETURN

         END IF

      END IF


!
!     Assume the point is outside to start. This allows us
!     to skip setting INSIDE when we find a boundary test
!     failure.
!
      INSIDE = .FALSE.

!
!     Get local copies of the coordinate bounds. Don't normalize the
!     longitude bounds until we know we need them.
!
      MINR   = BOUNDS( INNER, RADIDX )
      MAXR   = BOUNDS( OUTER, RADIDX )

      MINLAT = BOUNDS( SOUTH, LATIDX )
      MAXLAT = BOUNDS( NORTH, LATIDX )

!
!     Compare coordinates to adjusted latitude and radius
!     boundaries.
!
      IF ( EXCLUD .NE. RADIDX ) THEN
!
!        Create adjusted radius bounds.
!
         SMAX  = 1.D0 + MARGIN
         SMIN  = 1.D0 - MARGIN

         AMINR = MAX( 0.D0,  MINR*SMIN )
         AMAXR = MAXR * SMAX

         IF ( ( R .LT. AMINR ) .OR. ( R .GT. AMAXR ) ) THEN
             RETURN
         END IF

      END IF


      IF ( EXCLUD .NE. LATIDX ) THEN
!
!        Create adjusted latitude bounds.
!
         AMINLT = MAX( -HPI, MINLAT - MARGIN )
         AMAXLT = MIN(  HPI, MAXLAT + MARGIN )

         IF ( ( LAT .LT. AMINLT ) .OR. ( LAT .GT. AMAXLT ) ) THEN
            RETURN
         END IF

      END IF

!
!     At this point, the input radius and latitude are within the
!     adjusted bounds, if their tests haven't been excluded by
!     the caller.
!
!     Perform longitude tests, unless they're excluded by the
!     caller.
!
      IF ( EXCLUD .NE. LONIDX ) THEN

         CALL ZZNRMLON ( BOUNDS(WEST,LONIDX), BOUNDS(EAST,LONIDX), &
                         ANGMRG,              MINLON, &
                         MAXLON                                   )
!
!        Set the margin to be used for longitude interval
!        inclusion tests.
!
         LONMRG = MAX( ABS(ANGMRG), ABS(MARGIN) )

!
!        We have a special case for segments that include the poles. If
!        the input point is close enough to a pole contained in the
!        segment, we consider the point to be included in the segment,
!        regardless of the point's longitude. All other points get the
!        normal longitude test.
!
         IF (       ( LAT .LE. (  HPI - LATMRG ) ) &
              .AND. ( LAT .GE. ( -HPI + LATMRG ) )  ) THEN
!
!           This is the usual case: the latitude of the input point
!           is bounded away from the poles.
!
!           Check the point's longitude against the segment's
!           longitude bounds.
!
!           We'll scale the longitude margin to compensate for the
!           latitude of the input point. Note that the division
!           below is safe; presuming a reasonable value of MARGIN;
!           we know that
!
!              DLON << 1
!
            DLON   = LONMRG / MAX( ABS(COS(LAT)), LATMRG )

            AMINLO = MINLON - DLON
            AMAXLO = MAXLON + DLON

!
!           Now move the input point's longitude into range, if
!           necessary.
!
            IF ( LON .LT. AMINLO ) THEN

               IF (  LON  .LT. ( AMINLO - LONALI )  ) THEN
!
!                 See whether an aliased version of LON is a match.
!
                  LON = LON + PI2

               ELSE
!
!                 Consider LON to be a match with the lower bound.
!
                  LON = AMINLO

               END IF

            ELSE IF ( LON .GT. AMAXLO ) THEN

               IF (  LON  .GT. ( AMAXLO + LONALI )  ) THEN
!
!                 See whether an aliased version of LON is a match.
!
                  LON = LON - PI2

               ELSE
!
!                 Consider LON to be a match with the upper bound.
!
                  LON = AMAXLO

               END IF

            END IF

!
!           Compare the adjusted longitude of the input point to the
!           adjusted longitude bounds.
!
            IF ( ( LON .LT. AMINLO ) .OR. ( LON .GT. AMAXLO ) ) THEN

               RETURN

            END IF


         ELSE
!
!           The latitude of the input point is close to one of the
!           poles.
!
!           This is a no-op case.
!
!           The input point has already passed whichever of the radius
!           and latitude tests that were not excluded.
!
!           If the element has a non-degenerate latitude boundary
!           having the same sign as the latitude of the input point,
!           and if latitude is excluded because the input point is
!           already nominally on that boundary, then passing the radius
!           check implies that the point is close to the element.
!
!           If the element has a degenerate latitude boundary having
!           the same sign as the latitude of the input point---namely,
!           the element contains the pole, and latitude is excluded,
!           then then passing the radius check implies that the point
!           is close to the portion of the Z-axis contained in the
!           element.
!
!           If the radius check has been excluded because the point is
!           already nominally on one of the element's radius boundaries,
!           the passing the latitude test implies the point is close
!           to the element.
!
!           In all cases, as long as EXCLUD has been set appropriately,
!           the point is close to the element. We consider the point to
!           be in the expanded element.
!
         END IF

      END IF

!
!     All tests that were commanded have been passed. The input
!     point is considered to be contained in the expanded volume
!     element.
!
      INSIDE = .TRUE.

      END
