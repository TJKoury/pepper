!$Procedure ZZINPDT0 ( DSK, in planetodetic element, w/o margin? )

      SUBROUTINE ZZINPDT0 ( P, LON, BOUNDS, CORPAR, EXCLUD, INSIDE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Test a point represented by a set of planetodetic coordinates for
!     inclusion in a specified planetodetic volume element. Ellipsoidal
!     surfaces are used in place of surfaces of constant altitude. The
!     test is performed without using the "greedy" margins for the
!     element. The built-in angular rounding tolerance is still used
!     for the longitude comparison.
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
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CORPAR ( * )
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

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     P          I   Cartesian coordinates of point.
!     LON        I   Planetocentric longitude of point.
!     BOUNDS     I   Coordinate bounds of element.
!     CORPAR     I   Coordinate parameters.
!     EXCLUD     I   Index of coordinate to exclude from test.
!     INSIDE     O   Flag indicating whether point is in element.
!     ANGMRG     P   Angular rounding margin.
!     LONALI     P   Longitude alias margin.
!     NONE       P   Meaning: exclude nothing.
!     LONIDX     P   Longitude index. Also used for exclusion.
!     LATIDX     P   Latitude index. Also used for exclusion.
!     ALTIDX     P   Altitude index. Also used for exclusion.
!
! $ Detailed_Input
!
!     P          is a point expressed in Cartesian coordinates. The
!                reference frame is that of the segment of interest.
!                The point is to be checked to determine whether it is
!                inside the volume element specified by BOUNDS.
!
!
!     LON        is the planetocentric longitude of the point P.
!
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
!                If necessary, the longitude bounds are normalized
!                for the comparison. The normalized bounds differ
!                by no more than 2*pi and are in increasing order.
!                See ZZNRMLON for details.
!
!                The aliasing margin LONALI and the angular rounding
!                margin ANGMRG are used for longitude comparisons.
!                However, the greedy margin is not used. See the
!                Parameters section below for further information.
!
!
!     CORPAR     is an array of coordinate parameters; normally these
!                are obtained from a DSK descriptor. The first element
!                is the equatorial radius of the reference spheroid.
!                The second element is the flattening coefficient.
!                Any other elements are ignored by this routine.
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
!                coordinate. For example, if a point is on the surface
!                of altitude equal to the upper altitude bound,
!                altitude need not be used in the comparison and in
!                fact can't be meaningfully compared, due to round-off
!                errors.
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
!                by more than this amount, the value LON + 2*pi is used
!                in the longitude comparison. If the input longitude
!                exceeds the upper longitude bound by this amount, the
!                value LON - 2*pi is used in the comparison.
!
!     ANGMRG     is an angular rounding margin. If the input longitude
!                is outside of the longitude bounds by less than this
!                amount, the longitude is considered to be within the
!                bounds. The margin is used analogously for latitude.
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
!     ALTIDX     is the index of altitude in the second dimension of
!                BOUNDS. When used as a value of the input argument
!                EXCLUD, indicates that altitude should be excluded
!                from comparison.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine uses ellipsoidal bounding surfaces, in place of
!     surfaces of constant altitude, to represent the "outer" and
!     "inner" segment boundaries. These surfaces must be consistent
!     with those used by the ray-planetodetic volume element intercept
!     routine ZZRYTPDT.
!
!     The radii of these ellipsoidal surfaces are those produced by
!     the routine ZZELLBDS.
!
!     Note that a margin is used for longitude shifting, although
!     none is used to expand the volume element.
!
! $ Examples
!
!     See usage in ZZINPDT.
!
! $ Restrictions
!
!     1)  Validity of the ellipsoid shape parameters must be ensured by
!         the caller.
!
!     2)  This is a private routine. It is meant to be used only by the
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
!-    SPICELIB Version 1.0.0, 16-AUG-2016 (NJB)
!
!        Now uses ANGMRG for latitude comparisons.
!
!        25-MAY-2016 (NJB)
!
!           Original version.
!
!-&

! $ Index_Entries
!
!     test point against planetodetic element without margin
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      TWOPI

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               LOWER
      PARAMETER           ( LOWER = 1 )

      INTEGER               UPPER
      PARAMETER           ( UPPER = 2 )

!
!     Numeric relation codes returned by ZZPDCMPL:
!
      INTEGER               LT
      PARAMETER           ( LT = -1 )

      INTEGER               EQ
      PARAMETER           ( EQ =  0 )

      INTEGER               GT
      PARAMETER           ( GT =  1 )


!
!     Local variables
!
      DOUBLE PRECISION      EMAX
      DOUBLE PRECISION      EMIN
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      LOCLON
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MAXALT
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MINALT
      DOUBLE PRECISION      PI2
      DOUBLE PRECISION      PMAX
      DOUBLE PRECISION      PMIN
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP

      INTEGER               MAXREL
      INTEGER               MINREL

      LOGICAL               FIRST



!
!     Saved variables
!
      SAVE                  FIRST
      SAVE                  PI2

!
!     Initial values
!
      DATA                  FIRST / .TRUE. /
      DATA                  PI2   / -1.D0  /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZINPDT0' )

      IF ( FIRST ) THEN

         PI2   = TWOPI()
         FIRST = .FALSE.

      END IF

!
!     Unpack the shape parameters. Set the polar axis length for
!     later use.
!
      RE = CORPAR(1)
      F  = CORPAR(2)
      RP = RE * ( 1.D0 - F )

!
!     Assume the point is outside to start. This allows us
!     to skip setting INSIDE when we find a boundary test
!     failure.
!
      INSIDE = .FALSE.
!
!     Compare coordinates of the input point and the segment
!     bounds. Deal with altitude last, since it involves the
!     most work. We may be able to exit before performing the
!     altitude tests.
!
      IF ( EXCLUD .NE. LATIDX ) THEN
!
!        Compare the point's latitude to the segment's latitude bounds.
!
         MINLAT = MAX( BOUNDS(LOWER, LATIDX) - ANGMRG, -HALFPI() )
         MAXLAT = MIN( BOUNDS(UPPER, LATIDX) + ANGMRG,  HALFPI() )

         CALL ZZPDCMPL ( RE, F, P, MINLAT, MINREL )
         CALL ZZPDCMPL ( RE, F, P, MAXLAT, MAXREL )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZINPDT0' )
            RETURN
         END IF


         IF (      ( MINREL .EQ. LT ) &
              .OR. ( MAXREL .EQ. GT )  ) THEN
!
!           The point's latitude is outside of the segment's range.
!
            CALL CHKOUT ( 'ZZINPDT0' )
            RETURN

         END IF

      END IF

!
!     Move the longitude of the input point into the interval
!
!        [ MINLON, MAXLON ]
!
!     if necessary and if possible.
!
      IF ( EXCLUD .NE. LONIDX ) THEN
!
!        Put the local longitude bounds in order, if necessary.
!
         CALL ZZNRMLON ( BOUNDS(LOWER,LONIDX),  BOUNDS(UPPER,LONIDX), &
                         ANGMRG,       MINLON,  MAXLON               )
!
!        Compare the point's longitude to the segment's longitude
!        bounds.
!
         LOCLON = LON

         IF ( LON .LT. ( MINLON - LONALI ) ) THEN
!
!           If the point's longitude is less than the segment's
!           longitude by more than a small margin, shift the longitude
!           right by 2*pi.

            LOCLON = LON + PI2

         ELSE IF ( LON .GT. ( MAXLON + LONALI ) ) THEN
!
!           If the point's longitude is greater than the segment's
!           longitude by more than a small margin, shift the longitude
!           left by 2*pi.

            LOCLON = LON - PI2

         END IF


         IF (        ( LOCLON .LT. (MINLON-ANGMRG) ) &
                .OR. ( LOCLON .GT. (MAXLON+ANGMRG) )  ) THEN
!
!           The point's longitude, adjusted if necessary for
!           comparison, is outside of the segment's range.
!
            CALL CHKOUT ( 'ZZINPDT0' )
            RETURN

         END IF


      END IF

      IF ( EXCLUD .NE. ALTIDX ) THEN
!
!        Extract altitude bounds from the segment descriptor.
!
         MINALT = BOUNDS(LOWER, ALTIDX)
         MAXALT = BOUNDS(UPPER, ALTIDX)
!
!        Find the semi-axes of the bounding spheroids.
!
         IF ( F .GE. 0.D0 ) THEN
!
!           This is the oblate case. Get the semi-axis lengths
!           for the inner and outer bounding spheroids.
!
            CALL ZZELLBDS ( RE,   RP,   MAXALT, MINALT, &
                            EMAX, PMAX, EMIN,   PMIN   )

         ELSE
!
!           This is the prolate case. RP is the longer axis. Get the
!           semi-axis lengths for the inner and outer bounding
!           spheroids.
!
!           In this call, we'll store the radii associated with
!           the longer axis in PMAX and PMIN.
!
            CALL ZZELLBDS ( RP,   RE,   MAXALT, MINALT, &
                            PMAX, EMAX, PMIN,   EMIN   )

         END IF

!
!        Compute the input point's level surface parameters
!        for the inner and outer bounding ellipsoids. Do these
!        computations one at a time, so the second one can be
!        skipped if the first one shows the point is outside
!        the outer ellipsoid.
!
         LEVEL = ( P(1)/EMAX )**2 + ( P(2)/EMAX )**2 + ( P(3)/PMAX )**2

         IF ( LEVEL .GT. 1.D0 ) THEN
!
!           The point is outside of the outer bounding ellipsoid.
!
            CALL CHKOUT ( 'ZZINPDT0' )
            RETURN

         END IF


         LEVEL = ( P(1)/EMIN )**2 + ( P(2)/EMIN )**2 + ( P(3)/PMIN )**2

         IF ( LEVEL .LT. 1.D0 ) THEN
!
!           The point is inside the inner bounding ellipsoid, which
!           implies it is outside of the segment's boundaries.
!
            CALL CHKOUT ( 'ZZINPDT0' )
            RETURN

         END IF

      END IF

!
!     Getting to this point means the input point is inside
!     the segment. Being on the boundary counts as inside.
!
      INSIDE = .TRUE.


      CALL CHKOUT ( 'ZZINPDT0' )
      RETURN
      END
