!$Procedure ZZSGLATX ( Line segment latitude extent )

      SUBROUTINE ZZSGLATX ( P1, P2, MINLAT, MINP, MAXLAT, MAXP )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Find the latitude extent (extrema of latitude) of a line segment.
!     Latitude is defined in the planetocentric sense.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     EXTREMA
!     GEOMETRY
!     LATITUDE
!     LINE
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      P1     ( 3 )
      DOUBLE PRECISION      P2     ( 3 )
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINP   ( 3 )
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXP   ( 3 )

      INTEGER               UBPL
      PARAMETER           ( UBPL = 4 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     P1         I   First line segment endpoint.
!     P2         I   Second line segment endpoint.
!     MINLAT     O   Minimum latitude on segment.
!     MINP       O   Point where minimum latitude is attained.
!     MAXLAT     O   Maximum latitude on segment.
!     MAXP       O   Point where maximum latitude is attained.
!     UBPL       P   SPICE plane upper bound.
!
! $ Detailed_Input
!
!     P1,
!     P2         are endpoints of a given line segment.
!
! $ Detailed_Output
!
!
!     MINLAT     is the minimum planetocentric (latitudinal) latitude
!                that occurs on the line segment defined by the input
!                endpoints. Units are radians.
!
!     MINP       is a point on the line segment at which  the latitude
!                MINLAT is attained. Note that in some cases, the
!                minimum latitude can occur at more than one point.
!
!     MAXLAT     is the maximum planetocentric (latitudinal) latitude
!                that occurs on the line segment defined by the input
!                endpoints. Units are radians.
!
!     MAXP       is a point on the line segment at which the latitude
!                MAXLAT is attained. Note that in some cases, the
!                maximum latitude can occur at more than one point.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs when this routine attempts to construct a
!         SPICE plane, the error will be diagnosed by routines in the
!         call tree of this routine.
!
!     2)  If an error occurs when this routine attempts to compute a
!         ray-plane intersection, the error will be diagnosed by
!         routines in the call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!
!     In this routine, "latitude" of a point refers to planetocentric
!     latitude: the angle between the X-Y plane and the ray emanating
!     from the origin and passing through the point.
!
!     The term "latitude extents" on a line segment refers to the
!     minimum and maximum values of latitude attained on that segment.
!     The subset of the line segment at which a latitude extremum is
!     attained can be an endpoint, a single interior point of the
!     segment, or the whole segment.
!
!     There are several geometric cases that can occur:
!
!        Endpoints of segment are linearly dependent
!        ===========================================
!
!        The latitude extrema occur at the segment endpoints. Note that
!        in this case the entire segment is tangent to one or both
!        nappes of a vertical cone. The latitude values attained on the
!        segment can be any non-empty subset of
!
!           { minimum latitude, 0, maximum latitude }
!
!
!        Segment lies in X-Y plane
!        =========================
!
!        The latitude extrema are both zero.
!
!
!        Segment intersects Z-axis in a single non-origin point
!        ======================================================
!
!        One latitude extremum will occur on the Z-axis; the other
!        extremum will occur at a segment endpoint. This case may be
!        viewed as a degenerate version of the "local extremum exists
!        in segment" case.
!
!
!        Local extremum exists in segment
!        ================================
!
!        If an extremum occurs at a single point T in the interior of
!        the line segment, we call this a local extremum. Presuming the
!        segment does not intersect the Z-axis, then T is a tangent
!        point on a vertical cone C0 having its apex at the origin. All
!        points, excluding the origin, on the nappe of the cone
!        containing T have latitude equal to that of T.
!
!        Let P0 be the plane that contains the line segment and the
!        origin. If the endpoints of the line segment are linearly
!        independent, P0 is uniquely defined, and P0 is tangent to the
!        cone C0 at T.
!
!        Let N0 be a normal vector of P0. Let P1 be a plane containing
!        N0 and the Z-axis. If N0 is not parallel to the Z-axis, P1 is
!        uniquely defined, and the point T lies in the plane P1
!        containing the Z-axis and N0: the intersection of the segment
!        with P1 is T.
!
!        Three of the cases excluded here
!
!           - Segment endpoints are linearly dependent
!
!           - Segment intersects the Z-axis, not at the origin
!
!           - Normal to P0 is parallel to Z-axis (segment lies in
!             X-Y plane)
!
!        are all discussed above. The remaining case is discussed
!        below.
!
!
!        Local extremum occurs in extension of segment
!        =============================================
!
!        A local extremum may exist on the line containing the segment,
!        outside of the segment. In this case the segment cannot
!        contain a point where a local extremum occurs. The extrema
!        of latitude on the segment occur at the endpoints.
!
!
! $ Examples
!
!     1)  One application of this routine is to assist in subsetting
!         a type 2 DSK segment; this task requires determination of
!         a set of triangular plates that intersect a given
!         longitude/latitude rectangle. Note that this set cannot be
!         found by examination of vertex latitudes alone; latitude
!         extents of edges are needed.
!
!     2)  This routine is also used to find the maximum latitude
!         change that can occur between two points on a given ray.
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
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 29-SEP-2016 (NJB)
!
!        Original version 06-DEC-2012 (NJB)
!
!-&

! $ Index_Entries
!
!     find planetocentric latitude extent of a line segment
!     find planetocentric latitude range of a line segment
!     find planetocentric latitude extrema of a line segment
!
!-&



!
!     SPICELIB functions
!
      DOUBLE PRECISION      VDOT

      LOGICAL               FAILED
      LOGICAL               OPSGND
      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local variables
!
      DOUBLE PRECISION      CREASE ( 3 )
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      DP1
      DOUBLE PRECISION      DP2
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LAT1
      DOUBLE PRECISION      LAT2
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PLANE2 ( UBPL )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      T      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               NXPTS
!
!     Saved variables
!
      SAVE                  Z

!
!     Initial values
!
      DATA                  Z / 0.D0, 0.D0, 1.D0 /

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZSGLATX' )

!
!     Start by computing latitude at the segment's endpoints.
!
      CALL RECLAT ( P1, R, LON, LAT1 )
      CALL RECLAT ( P2, R, LON, LAT2 )

!
!     Initialize the outputs using latitudes of the endpoints.
!     If there are interior extrema, we'll update these outputs
!     as needed.
!
      IF ( LAT1 .LE. LAT2 ) THEN

         MINLAT = LAT1
         MAXLAT = LAT2

         CALL VEQU ( P1, MINP )
         CALL VEQU ( P2, MAXP )

      ELSE

         MINLAT = LAT2
         MAXLAT = LAT1

         CALL VEQU ( P2, MINP )
         CALL VEQU ( P1, MAXP )

      END IF

!
!     We want to work with the plane containing the origin, P1, and P2.
!     We'll call this plane PLANE1. First see whether P1 and P2 are
!     linearly independent.
!
      CALL VCRSS ( P1, P2, NORMAL )

      IF ( VZERO(NORMAL) ) THEN
!
!        We have a special case: P1 and P2 define a line passing
!        through the origin. The latitude extrema lie on the
!        segment endpoints, and possibly at every point on the
!        segment. We've already computed the outputs.
!
         CALL CHKOUT ( 'ZZSGLATX' )
         RETURN

      END IF

!
!     At this point we know that NORMAL is non-zero. Convert it
!     to a unit vector.
!
      CALL VHATIP ( NORMAL )

!
!     Let ALPHA be the non-negative angle between PLANE1 and the X-Y
!     plane. Then ALPHA and -ALPHA are, respectively, the maximum and
!     minimum possible latitudes attained on the input segment.
!     However, these values are not necessarily attained on the
!     segment; we'll need to perform further analysis to find out. We
!     don't need to compute ALPHA, but we'll refer to it in the
!     discussion below.
!
!     The next step is to find the normal vector to the plane defined
!     by Z and NORMAL. We'll call this plane PLANE2. This plane might
!     not exist if NORMAL and Z are linearly dependent. If PLANE2
!     does exist, the X-Y plane and PLANE1 intersect in a "crease"
!     that is normal to PLANE2.
!
      CALL VCRSS ( Z, NORMAL, CREASE )

      IF ( VZERO(CREASE) ) THEN
!
!        Z and NORMAL are linearly dependent; PLANE1 coincides (up to
!        round-off error) with the X-Y plane. We've already computed
!        the outputs.
!
         CALL CHKOUT ( 'ZZSGLATX' )
         RETURN

      END IF

!
!     At this point we know CREASE is non-zero. Convert
!     it to a unit vector.
!
      CALL VHATIP ( CREASE )

!
!     By construction, CREASE is orthogonal to NORMAL. PLANE2
!     cuts PLANE1 in a line L passing through the origin. If
!     the line segment has an interior latitude extremum,
!     the point T where that extremum is attained lies on L.
!     The segment is tangent at T to a nappe of a cone, centered on
!     the Z-axis and having its apex at the origin, for which
!     the half-angle is (pi/2)-ALPHA. The point T lies in PLANE2
!     since L is contained in PLANE2.
!
!     If a single tangent point T exists in the interior of the
!     segment, then the endpoints must be on opposite sides of PLANE2.
!     See whether this is the case.
!
      DP1 = VDOT(P1,CREASE)
      DP2 = VDOT(P2,CREASE)

      IF (  OPSGND( DP1, DP2 )  ) THEN
!
!        The segment crosses PLANE2 at an interior point; this
!        point is where the extremum occurs. Solve for the
!        intersection.
!
!        CREASE is guaranteed to be a unit vector. A zero input
!        vector is the only cause for which NVC2PL will signal
!        an error. Therefore we don't check FAILED after the
!        following call.
!
         CALL NVC2PL ( CREASE, 0.D0, PLANE2 )

         CALL VSUB ( P2, P1, DIR )

         CALL INRYPL ( P1, DIR, PLANE2, NXPTS, T )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZSGLATX' )
            RETURN
         END IF

         IF ( NXPTS .EQ. 1 ) THEN
!
!           This is the normal case: we have one intersection of the
!           segment with PLANE2. Update whichever of the extrema is
!           superseded by the interior value.
!
!           Note that this case can occur when NORMAL is orthogonal to
!           Z, making ALPHA equal to pi/2. The nappes are degenerate in
!           this case, consisting of the positive and negative Z-axes.
!           This degenerate case occurs when the segment intersects the
!           Z-axis in a point other than the origin, and the endpoints
!           are linearly independent.
!
!           This is not a special case computationally.
!
            CALL RECLAT ( T, R, LON, LAT )

            IF ( LAT .GT. MAXLAT ) THEN

               MAXLAT = LAT
               CALL VEQU ( T, MAXP )

            ELSE IF ( LAT .LT. MINLAT ) THEN

               MINLAT = LAT
               CALL VEQU ( T, MINP )

            END IF
!
!           There can be only one local extremum, so we're done.
!
         END IF
!
!        If NXPTS is not 1, then even though the endpoints are on
!        opposite sides of PLANE2, either the segment was found to lie
!        in PLANE2 or no intersection was found. This situation must be
!        due to finite precision arithmetic error. We'll make do with
!        the extrema already found.

      END IF

!
!     We reach this point if we found a local extremum or if any of the
!     following are true:
!
!        1)  The segment misses PLANE2 altogether, in which case
!            there's no tangency point.
!
!        2)  One endpoint lies on PLANE2 and one endpoint does not.
!
!        3)  Both endpoints lie in PLANE2. Then both endpoints lie
!            in L, so we should have found them to be linearly
!            dependent. This situation must be due to finite precision
!            arithmetic error.
!
!     In all of the numbered cases the extrema occur at the endpoints.
!     and have been found already. In all cases, the outputs are set.
!
      CALL CHKOUT ( 'ZZSGLATX' )
      RETURN
      END
