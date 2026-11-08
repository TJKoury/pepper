!$Procedure ZZINLAT0 ( DSK, in latitudinal element, w/o margin? )

      SUBROUTINE ZZINLAT0 ( R, LON, LAT, BOUNDS, EXCLUD, INSIDE )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Test a point represented by a set of latitudinal coordinates for
!     inclusion in a specified latitudinal volume element. The test is
!     performed without using the "greedy" margins for the element.
!     The built-in angular rounding tolerance is still used for the
!     longitude comparison.
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

      DOUBLE PRECISION      R
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
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

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     R          I   Radius of point.
!     LON        I   Planetocentric longitude of point.
!     LAT        I   Planetocentric latitude of point.
!     BOUNDS     I   Coordinate bounds of element.
!     EXCLUD     I   Index of coordinate to exclude from test.
!     INSIDE     O   Flag indicating whether point is in element.
!     ANGMRG     P   Angular rounding margin.
!     LONALI     P   Longitude shift margin.
!     NONE       P   Meaning: exclude nothing.
!     LONIDX     P   Longitude index. Also used for exclusion.
!     LATIDX     P   Latitude index. Also used for exclusion.
!     RADIDX     P   Radius index. Also used for exclusion.
!
! $ Detailed_Input
!
!     R,
!     LON,
!     LAT        are, respectively, the radius, longitude, and
!                latitude of a point. The point is to be checked
!                to determine whether it is inside the volume
!                element specified by BOUNDS.
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
!                The aliasing margin LONALI and the angular rounding
!                margin ANGMRG are used for longitude comparisons.
!                However, the greedy margin is not used. See the
!                Parameters section below for further information.
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
!     INSIDE     is a logical flag that is set to .TRUE. if and only if
!                the input coordinates represent a point inside or on
!                the surface of the volume element, according to the
!                comparisons that are performed.
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
!                See dsktol.inc for details.
!
!     ANGMRG     is an angular rounding margin. If the input longitude
!                is outside of the longitude bounds by less than this
!                amount, the longitude is considered to be within the
!                bounds.
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
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Note that a margin is used for longitude shifting, although
!     none is used to expand the volume element.
!
! $ Examples
!
!     See usage in ZZINLAT.
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
!-    SPICELIB Version 1.0.0, 07-AUG-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     test point against latitudinal element without margin
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      TWOPI

!
!     Local parameters
!
      INTEGER               LOWER
      PARAMETER           ( LOWER = 1 )

      INTEGER               UPPER
      PARAMETER           ( UPPER = 2 )

!
!     Local variables
!
      DOUBLE PRECISION      LOCLON
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MAXR
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MINR
      DOUBLE PRECISION      PI2

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



      IF ( FIRST ) THEN

         PI2   = TWOPI()
         FIRST = .FALSE.

      END IF

!
!     Assume the point is outside to start. This allows us
!     to skip setting INSIDE when we find a boundary test
!     failure.
!
      INSIDE = .FALSE.
!
!     Compare coordinates of the input point and the segment
!     bounds.
!
!     Special case: if the input point is at the origin, and the
!     volume element contains the origin, the point is inside.
!
      IF ( R .EQ. 0.D0 ) THEN

         IF ( BOUNDS(LOWER, RADIDX) .EQ. 0.D0 ) THEN

            INSIDE = .TRUE.

            RETURN

         END IF

      END IF



      IF ( EXCLUD .NE. RADIDX ) THEN
!
!        Compare the point's radius to the segment's radius bounds.
!
         MINR = BOUNDS(LOWER, RADIDX)
         MAXR = BOUNDS(UPPER, RADIDX)

         IF ( ( R .LT. MINR ) .OR. ( R .GT. MAXR ) ) THEN
!
!           The point's radius is outside of the segment's range.
!
            RETURN

         END IF

      END IF


      IF ( EXCLUD .NE. LATIDX ) THEN
!
!        Compare the point's latitude to the segment's latitude bounds.
!
         MINLAT = BOUNDS(LOWER, LATIDX)
         MAXLAT = BOUNDS(UPPER, LATIDX)

         IF ( ( LAT .LT. MINLAT ) .OR. ( LAT .GT. MAXLAT ) ) THEN
!
!           The point's latitude is outside of the segment's range.
!
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
         CALL ZZNRMLON ( BOUNDS(1,LONIDX),  BOUNDS(2,LONIDX), &
                         ANGMRG,   MINLON,  MAXLON           )
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
            RETURN

         END IF


      END IF
!
!     Getting to this point means the input point is inside
!     the segment. Being on the boundary counts as inside.
!
      INSIDE = .TRUE.

      RETURN
      END
