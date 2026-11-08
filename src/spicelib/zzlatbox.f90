!$Procedure ZZLATBOX (Bounding box for latitudinal volume element)

      SUBROUTINE ZZLATBOX ( BOUNDS, CENTER, LR, LT, LZ, RADIUS )

! $ Abstract
!
!     Create a bounding box for a latitudinal volume element.
!
!     The outputs are the box's center, dimensions in the radial,
!     tangential, and vertical directions, and the box's radius.
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
!     GEOMETRY
!     MATH
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS

      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.D-12 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BOUNDS     I   Array of coordinate bounds for segment.
!     CENTER     O   Center of bounding box.
!     LR         O   Extent of box in the cylindrical radial direction.
!     LT         O   Extent of box in the tangential direction.
!     LZ         O   Extent of box in the Z direction.
!     RADIUS     O   Radius of box.
!     MARGIN     P   Angular margin.
!
! $ Detailed_Input
!
!     BOUNDS     is a 2 x 3 double precision array containing bounds
!                on the latitudinal coordinates of the spatial region
!                (volume element) covered by a DSK segment. The
!                contents of BOUNDS are:
!
!                   BOUNDS(*,1)              Longitude bounds
!                   BOUNDS(*,2)              Latitude bounds
!                   BOUNDS(*,3)              Radius bounds
!
!                Elements (1,*) are lower bounds; elements (2,*) are
!                upper bounds. Angular units are radians.
!
! $ Detailed_Output
!
!     CENTER     is a double precision 3-vector representing the center
!                of a box tangent to and containing the volume
!                specified by BOUNDS. The sides of the box are parallel
!                to the radial, tangential, and Z directions, where the
!                radial direction is a vector orthogonal to the Z axis,
!                having longitude equal to the midpoint of the
!                segment's longitude range. The tangential direction is
!                orthogonal to the radial and Z directions.
!
!     LR,
!     LT,
!     LZ         are, respectively, the extents (edge lengths) of the
!                bounding box in the radial, tangential, and Z
!                directions.
!
!     RADIUS     is the radius of the sphere that circumscribes the
!                box. RADIUS is equal to the length of a line segment
!                connecting the center of the box to any corner.
!
! $ Parameters
!
!     MARGIN     is used for latitude range validity tests. Latitude
!                values can be out of range by as much as MARGIN.
!
! $ Exceptions
!
!     1) If the minimum longitude exceeds the maximum by more than
!        2*pi, the error SPICE(BADLONGITUDERANGE) is signaled.
!
!     2) If the latitude bounds are out of order, the error
!        SPICE(BADLATITUDEBOUNDS) is signaled.
!
!     3) If the minimum latitude is less than -pi/2 - MARGIN, or
!        if the maximum latitude is greater than pi/2 + MARGIN,
!        the error SPICE(BADLATITUDERANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides a simple way to compute the center and
!     radius for an outer bounding sphere for the volume covered
!     by a DSK segment using latitudinal coordinates.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 03-FEB-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     bounding box for latitudinal volume element
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN

!
!     Local parameters
!

!
!     Local variables
!
      DOUBLE PRECISION      DIAG   ( 3 )
      DOUBLE PRECISION      HDLON
      DOUBLE PRECISION      INRAD
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXLON
      DOUBLE PRECISION      MAXR
      DOUBLE PRECISION      MAXZ
      DOUBLE PRECISION      MIDLON
      DOUBLE PRECISION      MIDR
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINLON
      DOUBLE PRECISION      MINR
      DOUBLE PRECISION      MINZ
      DOUBLE PRECISION      OUTRAD

!
!     This routine uses discovery check-in. We check RETURN in order to
!     avoid performing math operations using invalid operands.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     Get local copies of the bounds of the volume element.
!
      MINLON = BOUNDS(1,1)
      MAXLON = BOUNDS(2,1)

      IF ( MAXLON .LE. MINLON ) THEN
         MAXLON = MAXLON + TWOPI()
      END IF

      IF ( MAXLON .LE. MINLON) THEN

         CALL CHKIN  ( 'ZZLATBOX'                               )
         CALL SETMSG ( 'Longitude bounds are #:#. Minimum ' &
         //            'longitude exceeds maximum by more than ' &
         //            '2 pi.'                                  )
         CALL ERRDP  ( '#', MINLON                              )
         CALL ERRDP  ( '#', BOUNDS(2,1)                         )
         CALL SIGERR ( 'SPICE(BADLONGITUDERANGE)'               )
         CALL CHKOUT ( 'ZZLATBOX'                               )
         RETURN

      END IF

      MINLAT = BOUNDS(1,2)
      MAXLAT = BOUNDS(2,2)

      MINR   = BOUNDS(1,3)
      MAXR   = BOUNDS(2,3)

      IF ( MINLAT .GT. MAXLAT ) THEN

         CALL CHKIN  ( 'ZZLATBOX'                              )
         CALL SETMSG ( 'Latitude bounds #:# are out of order.' )
         CALL ERRDP  ( '#', MINLAT                             )
         CALL ERRDP  ( '#', MAXLAT                             )
         CALL SIGERR ( 'SPICE(BADLATITUDEBOUNDS)'              )
         CALL CHKOUT ( 'ZZLATBOX'                              )
         RETURN

      END IF

      IF ( MINLAT .LT. ( -HALFPI() - MARGIN )  ) THEN

         CALL CHKIN  ( 'ZZLATBOX'                               )
         CALL SETMSG ( 'Minimum latitude # is less than -pi/2.' )
         CALL ERRDP  ( '#', MINLAT                              )
         CALL SIGERR ( 'SPICE(BADLATITUDERANGE)'                )
         CALL CHKOUT ( 'ZZLATBOX'                               )
         RETURN

      END IF

      IF ( MAXLAT .GT. ( HALFPI() + MARGIN )  ) THEN

         CALL CHKIN  ( 'ZZLATBOX'                               )
         CALL SETMSG ( 'Maximum latitude # is more than -pi/2.' )
         CALL ERRDP  ( '#', MAXLAT                              )
         CALL SIGERR ( 'SPICE(BADLATITUDERANGE)'                )
         CALL CHKOUT ( 'ZZLATBOX'                               )
         RETURN

      END IF

      MINLAT = MAX( MINLAT, -HALFPI() )
      MAXLAT = MIN( MAXLAT,  HALFPI() )

!
!     Let INRAD and OUTRAD be, respectively, the radii of the
!     orthogonal projections onto the X-Y plane of the element's arcs
!     of minimum and maximum distance from the Z axis.
!
!     If the element lies on or above the X-Y plane, the outer radius
!     is that of the lower latitude bound on the outer bounding sphere,
!     and the inner radius is that of the upper latitude bound on the
!     inner bounding sphere.
!
!     These relationships are reversed for elements that lie on or
!     below the X-Y plane.
!
!     For elements that span the X-Y plane, the outer radius is that of
!     the outer bounding sphere. The inner radius is that of the
!     latitude circle on the inner bounding sphere for which the
!     absolute value of the latitude is maximum.
!
      IF ( MINLAT .GE. 0.D0 ) THEN

         OUTRAD = COS( MINLAT ) * MAXR
         INRAD  = COS( MAXLAT ) * MINR

      ELSE IF ( MAXLAT .LE. 0.D0 ) THEN

         OUTRAD = COS( MAXLAT ) * MAXR
         INRAD  = COS( MINLAT ) * MINR

      ELSE

         OUTRAD = MAXR
         INRAD  = COS(  MAX( ABS(MAXLAT), ABS(MINLAT) )  ) * MINR

      END IF

!
!     Let MIDLON be the longitude of the midpoint of the element's
!     longitude coverage. Let HDLON be one half of the extent of the
!     longitude coverage.
!
      HDLON  = ( MAXLON - MINLON ) / 2
      MIDLON =   MINLON + HDLON

!
!     LR is the length of the bounding box in the radial direction,
!     where "radius" is defined as the distance from the Z axis.
!
      IF ( HDLON .LE. HALFPI() ) THEN

         LR = OUTRAD - ( INRAD * COS(HDLON) )

      ELSE

         LR = ( 1.D0 - COS(HDLON) ) * OUTRAD

      END IF

!
!     The tangential length of bounding box depends on the longitude
!     extent. For any extent larger than Pi radians, the width
!     is just that of the outer radius.
!
      IF ( HDLON .GT. HALFPI() ) THEN

         LT = 2.D0 * OUTRAD
      ELSE
         LT = 2.D0 * OUTRAD * SIN( HDLON )
      END IF

!
!     The height bounds are derived from the lowest and highest points
!     on the element.
!
      IF ( MINLAT .GE. 0.D0 ) THEN

         MAXZ = MAXR * SIN(MAXLAT)
         MINZ = MINR * SIN(MINLAT)

      ELSE IF ( MAXLAT .LE. 0.D0 ) THEN

         MAXZ = MINR * SIN(MAXLAT)
         MINZ = MAXR * SIN(MINLAT)

      ELSE

         MAXZ = MAXR * SIN(MAXLAT)
         MINZ = MAXR * SIN(MINLAT)

      END IF

      LZ = MAXZ - MINZ

!
!     Make sure all dimensions are non-negative.
!
      LR = MAX( 0.D0, LR )
      LT = MAX( 0.D0, LT )
      LZ = MAX( 0.D0, LZ )

!
!     Compute the coordinates of the center of the box.
!
!     The box center lies on the meridian of central
!     longitude. The outer tangential edge is at radius
!     OUTRAD. Let MIDR be the radius of the center.
!
      MIDR = OUTRAD - (LR/2)

      CALL CYLREC ( MIDR, MIDLON, MINZ+(LZ/2), CENTER )

!
!     The radius is the distance from the center of the box
!     to any corner.
!
      CALL VPACK ( LR/2, LT/2, LZ/2, DIAG )

      RADIUS = VNORM( DIAG )

      RETURN
      END
