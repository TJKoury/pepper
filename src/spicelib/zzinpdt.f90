!$Procedure ZZINPDT ( DSK, in planetodetic element? )

      SUBROUTINE ZZINPDT ( P, BOUNDS, CORPAR, MARGIN, EXCLUD, INSIDE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Test a point represented by a set of planetodetic coordinates for
!     inclusion in a specified planetodetic volume element. Ellipsoidal
!     surfaces are used in place of surfaces of constant altitude. The
!     test is performed using margins for the element.
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
      DOUBLE PRECISION      CORPAR ( * )
      DOUBLE PRECISION      MARGIN
      INTEGER               EXCLUD
      LOGICAL               INSIDE

      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )

      INTEGER               LONIDX
      PARAMETER           ( LONIDX = 1 )

      INTEGER               LATIDX
      PARAMETER           ( LATIDX = 2 )

      INTEGER               ALTIDX
      PARAMETER           ( ALTIDX = 3 )


      DOUBLE PRECISION      LATMRG
      PARAMETER           ( LATMRG = 1.D-8 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     P          I   Input point.
!     BOUNDS     I   Coordinate bounds of element.
!     CORPAR     I   Coordinate system parameters.
!     MARGIN     I   Margin used for inclusion testing.
!     EXCLUD     I   Index of coordinate to exclude from test.
!     INSIDE     O   Flag indicating whether point is in element.
!     LONALI     P   Longitude shift margin.
!     NONE       P   Meaning: exclude nothing.
!     LONIDX     P   Longitude index. Also used for exclusion.
!     LATIDX     P   Latitude index. Also used for exclusion.
!     ALTIDX     P   Altitude index. Also used for exclusion.
!     LATMRG     P   High/low latitude margin.
!
! $ Detailed_Input
!
!     P          is a point expressed in Cartesian coordinates. The
!                point is to be checked to determine whether it is
!                inside the volume element specified by BOUNDS.
!
!     BOUNDS     is an 2x3 array containing the bounds of a volume
!                element expressed in planetodetic coordinates. BOUNDS
!                defines the volume element used in the comparison. In
!                the element
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
!
!     CORPAR     is an array of coordinate parameters; normally these
!                are obtained from a DSK descriptor. The first element
!                is the equatorial radius of the reference spheroid.
!                The second element is the flattening coefficient.
!                Any other elements are ignored by this routine.
!
!
!     MARGIN     is a fraction used to expand the volume element for
!                inclusion testing.
!
!                   - The upper altitude bound is increased by
!
!                        MARGIN*ABS( BOUNDS(2,3) )
!
!                     The lower altitude bound is decreased by
!
!                        MARGIN*ABS( BOUNDS(1,3) )
!
!                     Note that these bounds are used as inputs to
!                     compute the radii of upper and lower bounding
!                     ellipsoids. The input point's position is tested
!                     for inclusion within or exclusion from these
!                     ellipsoids.
!
!
!                   - Latitude bounds are extended by MARGIN radians,
!                     within the interval -pi/2 : pi/2.
!
!                   - For any input point having latitude "not close"
!                     to pi/2 or -pi/2, the element's longitude
!                     interval is extended by
!
!                        MARGIN / cos(LAT) radians
!
!                     where LAT is the planetocentric [sic] latitude of
!                     the input point.
!
!                     Here "close to" means "within LATMRG radians of."
!
!                   - For any input point having planetocentric
!                     latitude close to (within LATMRG radians of) pi/2
!                     or -pi/2, comparison against the longitude bounds
!                     of the volume element is omitted if MARGIN > 0.
!
!
!     EXCLUD     is either a coordinate index or the parameter NONE.
!
!                If EXCLUD is set to one of
!
!                   { LONIDX, LATIDX, ALTIDX }
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
!                coordinate. For example, if a point is on the outer
!                bounding ellipsoid, the level surface parameter of the
!                point with respect to the ellipsoid need not be used
!                in the comparison and in fact can't be meaningfully
!                compared, due to round-off errors.
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
!     LONALI     is a longitude shift margin. If the input longitude
!                coordinate LON is less than the longitude lower bound
!                by more than this amount, the value LON + 2*pi is
!                used in the longitude comparison, provided that the
!                comparison is not excluded.
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
!     ALTIDX     is the index of altitude in the second dimension of
!                BOUNDS. When used as a value of the input argument
!                EXCLUD, indicates that altitude should be excluded
!                from comparison.
!
!
!     LATMRG     is the margin used to determine whether the input
!                latitude is "close" to the poles. Units are radians.
!                The value of this parameter must be kept in sync
!                with the "greedy" margin declared in dsktol.inc.
!
! $ Exceptions
!
!     1)  If MARGIN is negative, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
!     2)  If EXCLUD is outside of the range 0:3, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     3)  If an error occurs while determining the planetodetic
!         coordinates of the input point, the error will be signaled by
!         a routine in the call tree of this routine.
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
!     See usage in ZZRYTPDT.
!
! $ Restrictions
!
!     1)  This is a private routine. It is meant to be used only by the
!         DSK subsystem.
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
!-    SPICELIB Version 1.0.0, 13-FEB-2017 (NJB)
!
!        03-JUN-2016 (NJB)
!
!           Added standard traceback participation. Added check for
!           invalid values of EXCLUD.
!
!        05-MAR-2016 (NJB)
!
!           Original version 03-OCT-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     test point against planetodetic element using margin
!
!-&




!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

      LOGICAL               FAILED
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

      INTEGER               LOWER
      PARAMETER           ( LOWER  = 1 )

      INTEGER               UPPER
      PARAMETER           ( UPPER  = 2 )


!
!     Numeric relation codes returned by ZZPDCMPL:
!
      INTEGER               LT
      PARAMETER           ( LT = -1 )

      INTEGER               GT
      PARAMETER           ( GT =  1 )

!
!     The code EQ can be returned by ZZPDCMPL, but we make no
!     references to it, so it's not declared here. For the
!     record, EQ is set to 0.
!

!
!     Local variables
!
      DOUBLE PRECISION      ALTBDS ( 2, 3 )
      DOUBLE PRECISION      AMNALT
      DOUBLE PRECISION      AMNLAT
      DOUBLE PRECISION      AMNLON
      DOUBLE PRECISION      AMXALT
      DOUBLE PRECISION      AMXLAT
      DOUBLE PRECISION      AMXLON
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      F
      DOUBLE PRECISION      HPI
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LONMRG
      DOUBLE PRECISION      MAXALT
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MINALT
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      PCNLAT
      DOUBLE PRECISION      PI2
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RE

      INTEGER               RELMIN
      INTEGER               RELMAX

      LOGICAL               ALPASS
      LOGICAL               FIRST

!
!     Saved variables
!
      SAVE                  ALTBDS
      SAVE                  FIRST
      SAVE                  HPI
      SAVE                  PI2

!
!     Initial values
!
      DATA                  FIRST / .TRUE. /
      DATA                  HPI   / -1.D0  /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZINPDT' )

      IF ( FIRST ) THEN

         HPI   = HALFPI()
         PI2   = TWOPI()
!
!        Initialize the local array used for altitude checks.
!
         ALTBDS( SOUTH, LATIDX ) =  - HALFPI()
         ALTBDS( NORTH, LATIDX ) =    HALFPI()
         ALTBDS( WEST,  LONIDX ) =  - PI()
         ALTBDS( EAST,  LONIDX ) =    PI()
         ALTBDS( LOWER, ALTIDX ) =    0.D0
         ALTBDS( UPPER, ALTIDX ) =    0.D0

         FIRST = .FALSE.

      END IF


      IF (  ( EXCLUD .LT. 0 ) .OR. ( EXCLUD .GT. 3 )  ) THEN

         CALL SETMSG ( 'EXCLUD must be in the range 0:3 but was #.' )
         CALL ERRINT ( '#', EXCLUD                                  )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                     )
         CALL CHKOUT ( 'ZZINPDT'                                    )
         RETURN

      END IF

!
!     Get the planetocentric [sic] coordinates of the input point. The
!     latitude we obtain will be planetocentric. To emphasize this, we
!     use the name "PCNLAT."
!
      CALL RECLAT ( P, R, LON, PCNLAT )
!
!     RECLAT is error free, so we don't call FAILED() here.
!

      IF ( MARGIN .EQ. 0.D0 ) THEN
!
!        ZZINPDT0 contains the logic required to determine whether
!        the input point is contained in the element.
!
         CALL ZZINPDT0 ( P,      LON, &
                         BOUNDS, CORPAR, EXCLUD, INSIDE )

         CALL CHKOUT ( 'ZZINPDT' )
         RETURN

      ELSE IF ( MARGIN .LT. 0.D0 ) THEN

         CALL SETMSG ( 'Margin must be non-negative but was #.' )
         CALL ERRDP  ( '#', MARGIN                              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'                 )
         CALL CHKOUT ( 'ZZINPDT'                                )
         RETURN

      END IF

!
!     At this point a more detailed analysis is needed.
!
!
!     Assume the point is outside to start. This allows us
!     to skip setting INSIDE when we find a boundary test
!     failure.
!
      INSIDE = .FALSE.

!
!     We'll use the shape parameters for latitude and altitude
!     comparisons.
!
      RE = CORPAR(1)
      F  = CORPAR(2)

!
!     Get local copies of the coordinate bounds. Don't normalize the
!     longitude bounds until we know we need them.
!
      MINLAT = BOUNDS( SOUTH, LATIDX )
      MAXLAT = BOUNDS( NORTH, LATIDX )

      MINALT = BOUNDS( LOWER, ALTIDX )
      MAXALT = BOUNDS( UPPER, ALTIDX )

!
!     Compare coordinates to adjusted latitude boundaries.
!
      IF ( EXCLUD .NE. LATIDX ) THEN
!
!        Create adjusted latitude bounds.
!
         AMNLAT = MAX( -HPI-ANGMRG, MINLAT - MARGIN )
         AMXLAT = MIN(  HPI+ANGMRG, MAXLAT + MARGIN )

!
!        Compare the latitude of the input point to the bounds.
!
         CALL ZZPDCMPL ( RE, F, P, AMNLAT, RELMIN )
         CALL ZZPDCMPL ( RE, F, P, AMXLAT, RELMAX )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZINPDT' )
            RETURN
         END IF


         IF ( ( RELMIN .EQ. LT ) .OR. ( RELMAX .EQ. GT ) ) THEN
!
!           The latitude of P is strictly outside of the element's
!           latitude bounds.
!
            CALL CHKOUT ( 'ZZINPDT' )
            RETURN
         END IF

      END IF

!
!     Test the point for inclusion in the region between the bounding
!     ellipsoids that act as proxies for altitude boundaries.
!
      IF ( EXCLUD .NE. ALTIDX ) THEN
!
!        Extract altitude bounds from the segment descriptor.
!
         MINALT = BOUNDS(LOWER, ALTIDX)
         MAXALT = BOUNDS(UPPER, ALTIDX)
!
!        Adjust altitude bounds to account for the margin.
!
         AMNALT = MINALT  -  MARGIN * ABS(MINALT)
         AMXALT = MAXALT  +  MARGIN * ABS(MAXALT)
!
!        Set up a "boundary" array so that we can use ZZINPDT0
!        to do the altitude check for us. We'll exclude longitude
!        tests; the latitude test is set up for an automatic pass
!        (the latitude range of ALTBDS is [-pi/2, pi/2]).
!
         ALTBDS( LOWER, ALTIDX ) = AMNALT
         ALTBDS( UPPER, ALTIDX ) = AMXALT

         CALL ZZINPDT0 ( P, LON, ALTBDS, CORPAR, LONIDX, ALPASS )

         IF ( .NOT. ALPASS ) THEN

            CALL CHKOUT ( 'ZZINPDT' )
            RETURN

         END IF

      END IF

!
!     At this point, the input altitude and latitude are within the
!     adjusted bounds, if their tests haven't been excluded by
!     the caller.
!
!     Perform longitude tests, unless they're excluded by the
!     caller.
!
      IF ( EXCLUD .NE. LONIDX ) THEN
!
!        Start out by normalizing the element's longitude bounds.
!
         CALL ZZNRMLON ( BOUNDS(LOWER,LONIDX),  BOUNDS(UPPER,LONIDX), &
                         ANGMRG,       MINLON,  MAXLON               )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZINPDT' )
            RETURN
         END IF
!
!        Set the margin to be used for longitude interval
!        inclusion tests.
!
         LONMRG = MAX( ABS(ANGMRG), ABS(MARGIN) )

!
!        We have a special case for segments that include the poles. If
!        the latitude and altitude of the input point are within
!        bounds, and if the latitude of the point is close enough to a
!        pole we consider the point to be included in the segment,
!        regardless of the point's longitude. All other points get the
!        normal longitude test.
!
!        We use planetocentric latitude to determine whether the point
!        is "close" to a pole.
!
         CALL ZZPDCMPL ( RE, F, P,  HPI-LATMRG, RELMAX )
         CALL ZZPDCMPL ( RE, F, P, -HPI+LATMRG, RELMIN )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZINPDT' )
            RETURN
         END IF

         IF (       ( RELMAX .NE. GT ) &
              .AND. ( RELMIN .NE. LT )  ) THEN
!
!           This is the usual case: the latitude of the input point is
!           bounded away from the poles.
!
!           Check the point's longitude against the segment's longitude
!           bounds.
!
!           We'll scale the longitude margin to compensate for the
!           latitude of the input point. Note that the division
!           below is safe; presuming a reasonable value of MARGIN,
!           we know that
!
!              DLON << 1
!
!           Note that we use planetocentric latitude for scaling the
!           longitude margin. This substitution (for planetodetic
!           latitude) is adequate for this purpose.
!
            DLON   =  LONMRG / MAX( ABS(COS(PCNLAT)), LATMRG )

            AMNLON = MINLON - DLON
            AMXLON = MAXLON + DLON

!
!           Now move the input point's longitude into range, if
!           necessary, so we can make a valid comparison against
!           the longitude bounds.
!
            IF ( LON .LT. AMNLON ) THEN

               IF (  LON  .LT. ( AMNLON - LONALI )  ) THEN
!
!                 See whether an aliased version of LON is a match.
!
                  LON = LON + PI2

               ELSE
!
!                 Consider LON to be a match with the lower bound.
!
                  LON = AMNLON

               END IF

            ELSE IF ( LON .GT. AMXLON ) THEN

               IF (  LON  .GT. ( AMXLON + LONALI )  ) THEN
!
!                 See whether an aliased version of LON is a match.
!
                  LON = LON - PI2

               ELSE
!
!                 Consider LON to be a match with the upper bound.
!
                  LON = AMXLON

               END IF

            END IF


            IF ( ( LON .LT. AMNLON ) .OR. ( LON .GT. AMXLON ) ) THEN

               CALL CHKOUT ( 'ZZINPDT' )
               RETURN

            END IF

         ELSE

!
!           The latitude of the input point is close to one of the
!           poles.
!
!           This is a no-op case.
!
!           The input point has already passed whichever of the
!           altitude and latitude tests that were not excluded.
!
!           If the element has a non-degenerate latitude boundary
!           having the same sign as the latitude of the input point,
!           and if latitude is excluded because the input point is
!           already nominally on that boundary, then passing the
!           altitude check implies that the point is close to the
!           element.
!
!           If the element has a degenerate latitude boundary having
!           the same sign as the latitude of the input point---namely,
!           the element contains the pole, and latitude is excluded,
!           then then passing the altitude check implies that the point
!           is close to the portion of the Z-axis contained in the
!           element.
!
!           If the altitude check has been excluded because the point
!           is already nominally on one of the element's altitude
!           boundaries, the passing the latitude test implies the point
!           is close to the element.
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

      CALL CHKOUT ( 'ZZINPDT' )
      RETURN
      END
