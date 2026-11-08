!$Procedure ZZTANGNT ( DSK, find target tangent rays in half-plane )

      SUBROUTINE ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                            SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                            SCHSTP, SOLTOL, RESULT, POINTS        )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Solve for tangent points on a target surface of rays contained
!     within a specified half-plane. The rays may emanate from a
!     specified vertex or may be tangent to a "source" sphere; the
!     first case supports limb finding and the second terminator
!     finding.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DSK
!     LIMB
!     RAY
!     TANGENT
!     TERMINATOR
!     TOPOGRAPHY
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzdsk.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               CURVE
      DOUBLE PRECISION      SRCRAD
      INTEGER               SHAPE
      INTEGER               TRGCDE
      INTEGER               NSURF
      INTEGER               SRFLST ( * )
      INTEGER               FIXFID
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      PLNVEC ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      SCHSTP
      DOUBLE PRECISION      SOLTOL
      DOUBLE PRECISION      RESULT ( LBCELL : * )
      DOUBLE PRECISION      POINTS ( 3, * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     CURVE      I   Type of curve: limb or terminator type.
!     SRCRAD     I   Radius of illumination source.
!     SHAPE      I   Target shape.
!     TRGCDE     I   Target body ID code.
!     NSURF      I   Number of surfaces in list.
!     SRFLST     I   Surface ID list.
!     FIXFID     I   Frame ID of target body-fixed frame.
!     ET         I   Epoch, expressed as seconds past J2000 TDB.
!     PLNVEC     I   Reference vector contained in cutting half-plane.
!     AXIS       I   Axis vector: edge of cutting half-plane.
!     SCHSTP     I   Angular step size for searching.
!     SOLTOL     I   Solution convergence tolerance.
!     RESULT     O   Cell (not window) containing transition angles.
!     POINTS     O   Array of tangent points.
!     ELLSHP     P   Ellipsoid shape code.
!     DSKSHP     P   DSK shape code.
!     LMBCRV     P   Limb code.
!     UMBRAL     P   Umbral terminator code.
!     PNMBRL     P   Penumbral terminator code.
!
! $ Detailed_Input
!
!     CURVE      is an integer code indicating the type of set on the
!                target body on which tangent points are to be found.
!                When the target is an ellipsoid, the set is literally
!                a curve. When the target is a DSK shape, the set is
!                often not connected; so it is not actually a curve;
!                still, we retain the familiar terminology for the
!                ellipsoid case.
!
!                Possible values and meanings are:
!
!                   LMBCRV            limb
!                   UMBRAL            umbral terminator
!                   PNMBRL            penumbral terminator
!
!                Terminator computations are performed assuming
!                the light source is spherical.
!
!
!     SRCRAD     is the radius of the illumination source. This
!                value is used for terminator computations only.
!                When used, SRCRAD must be strictly positive.
!                Units are km.
!
!
!     SHAPE      is an integer code indicating the target body shape.
!
!                Possible values and meanings are:
!
!                   ELLSHP            shape is modeled as an ellipsoid
!                   DSKSHP            shape is provided by DSK data
!
!
!     TRGCDE     is the body ID code of the target body.
!
!
!     NSURF,
!     SRFLST     are, respectively, the count of surface IDs and the
!                surface ID list.
!
!
!     FIXFID     is the frame ID code of a body-fixed frame centered
!                on the target body. The output tangent points will
!                be expressed in this frame.
!
!
!     ET         is the computation epoch, expressed as seconds past
!                J2000 TDB. ET is used for DSK segment selection.
!                If the target shape is modeled as an ellipsoid, ET
!                is ignored.
!
!
!     PLNVEC     is a vector used to define a half-plane in which
!                to search for tangent rays. The half-plane contains
!                PLNVEC, the target body center, and AXIS.
!
!                For limb and umbral terminator computations,
!                tangent rays lie in the half-plane containing PLNVEC.
!
!                For penumbral terminator computations, tangent rays
!                touch the target in the half-plane containing PLNVEC,
!                but touch the light source in the complementary
!                half-plane bounded by the line containing AXIS.
!
!
!     AXIS       is a second vector used to define a half-plane in
!                which to search for tangent vectors. AXIS lies in
!                the line forming the boundary of the half-plane.
!
!                For limb computations, AXIS points from the target
!                toward the observation point.
!
!                For terminator computations, AXIS points from the
!                target toward the center of the illumination source.
!
!
!     SCHSTP,
!     SOLTOL     are, respectively, the search angular step size and
!                solution convergence tolerance used to find tangent
!                rays and associated limb points within each cutting
!                half plane.
!
! $ Detailed_Output
!
!     RESULT     is a cell (not a window) containing ray-axis angular
!                separation values at which ray-surface tangencies
!                occur.
!
!                Here "angular separation" refers to the angle between
!                a ray and the input vector AXIS. AXIS points away from
!                the target.
!
!                The vertex of the vector depends on the curve type,
!                which is indicated by the input code CURVE.
!
!                For a limb computation, the vertex is the observer's
!                location.
!
!                For an umbral terminator computation, the vertex is on
!                the surface of the light source, in the half- plane
!                defined by PLNDEF and AXIS. The line containing the
!                ray is tangent to the light source at the vertex.
!
!                For a penumbral terminator computation, the vertex is
!                on the surface of the light source, in the half-plane
!                complementary to that defined by PLNDEF and AXIS. The
!                line containing the ray is tangent to the light source
!                at the vertex.
!
!                Units are radians.
!
!
!     POINTS     is an array of tangent points on the target body's
!                surface. Elements
!
!                   POINTS(J,I), J = 1, 3
!
!                constitute the tangent point for the ray having
!                angular separation
!
!                   RESULT(I)
!
!                from AXIS. Here the ray's vertex is as described
!                above in the discussion of the RESULT output cell.
!
!
! $ Parameters
!
!     See the INCLUDE file zzdsk.inc for declarations of these
!     parameters.
!
!
!     ELLSHP     is a code specifying an ellipsoidal shape model.
!
!     DSKSHP     is a code specifying a DSK-based shape model.
!
!     LMBCRV     is a code specifying a limb computation.
!
!     UMBRAL     is a code specifying an umbral terminator computation.
!
!                The umbral terminator is the boundary of the portion
!                of the target surface that receives no light from the
!                illumination source.
!
!     PNMBRL     is a code specifying an penumbral terminator
!                computation.
!
!                The penumbral terminator is the boundary of the
!                portion of the target surface that is not subject to
!                self-occultation of light from the illumination
!                source. Given that the light source is modeled as
!                a sphere, from any target surface point nearer to
!                the source than the penumbral terminator, the source
!                appears to be a lit disc.
!
!
! $ Exceptions
!
!     1)  If AXIS is the zero vector, the error SPICE(ZEROVECTOR) is
!         signaled.
!
!     2)  If PLNDEF is the zero vector, the error SPICE(ZEROVECTOR) is
!         signaled.
!
!     3)  If the curve type code is unrecognized, the error
!         SPICE(BADCURVETYPE) is signaled.
!
!     4)  If a terminator curve is specified by the source radius
!         is non-positive, the error SPICE(BADSOURCERADIUS) is
!         signaled.
!
!     5)  If AXIS and PLNDEF are linearly dependent, the error
!         SPICE(DEGENERATECASE) is signaled.
!
!     6)  If the target shape code is unrecognized, the error
!         SPICE(BADSHAPE) is signaled.
!
! $ Files
!
!     This routine makes use of DSK files loaded by the ZZDSKBSR
!     subsystem.
!
!     If any loaded DSK segment has a reference frame that is not
!     centered at the segment's central (target) body, SPK data are
!     required to compute the offset between the frame's center and
!     the segment's center.
!
!     Frame kernels may be required in order to look up a segment's
!     frame center offset. In some cases, additional kernels such
!     as CK kernels and SCLK kernels could be required to support
!     the offset vector lookup.
!
!     This routine uses PCK data for target body reference
!     ellipsoids.
!
! $ Particulars
!
!     This routine is meant to be used only by the DSK subsystem.
!
!     This routine computes target tangent points for limb or
!     terminator computations.
!
! $ Examples
!
!     See usage in LIMBPT and TERMPT.
!
! $ Restrictions
!
!     1) This is a private routine.
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
!-     SPICELIB Version 1.1.0, 17-OCT-2021 (NJB)
!
!         Deleted unnecessary declarations of external routines
!         GFBAIL and GFREPU.
!
!-     SPICELIB Version 1.0.0, 24-AUG-2016 (NJB)
!
!           10-FEB-2016 (NJB)
!
!           Removed VERTEX from argument list.
!
!           Original version 26-OCT-2015 (NJB)
!-&

! $ Index_Entries
!
!     find target tangent rays in specified half-plane
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VNORM

      INTEGER               CARDD

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     EXTERNAL routines
!
      EXTERNAL              ZZTANSTA
      EXTERNAL              GFSTEP
      EXTERNAL              GFREFN

!
!     Local parameters
!
      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.D-12 )

!
!     Local variables
!
      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      START
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      MINDST
      DOUBLE PRECISION      R
      DOUBLE PRECISION      REFVEC ( 3 )

      INTEGER               I
      INTEGER               N

      LOGICAL               CSTEP
      LOGICAL               ENDFLG ( 2 )



      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZTANGNT' )

!
!     Empty the result window.
!
      CALL SCARDD ( 0, RESULT )

!
!     Rotate the plane definition vector by pi about AXIS if
!     we're generating penumbral terminator points.
!
      IF ( CURVE .EQ. PNMBRL ) THEN

         CALL VROTV ( PLNVEC, AXIS, PI(), REFVEC )
      ELSE
         CALL VEQU  ( PLNVEC,             REFVEC )
      END IF

!
!     Prepare the tangent finding utilities.
!
      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, SRFLST, &
                      FIXFID, ET,     REFVEC, AXIS                  )
!
!     Fetch a maximum bounding radius for the target.
!
!     Caution: we assume ZZTANINI has initialized the
!     SINCPT utility subsystem by calling one of
!
!        ZZSUELIN
!        ZZSUDSKI
!
      CALL ZZMAXRAD ( MAXRAD )

!
!     Scale up MAXRAD slightly to ensure bracketing.
!
      MAXRAD = MAXRAD * 1.001D0

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZTANGNT' )
         RETURN
      END IF

      IF ( MAXRAD .LE. 0.D0 ) THEN

         CALL SETMSG ( 'Target maximum radius # is non-positive.' )
         CALL ERRDP  ( '#',  MAXRAD                               )
         CALL SIGERR ( 'SPICE(INVALIDRADIUS)'                     )
         CALL CHKOUT ( 'ZZTANGNT'                                 )
         RETURN

      END IF



      IF ( CURVE .EQ. LMBCRV ) THEN
!
!        We're looking for limb points.
!
!        Set the initial ray-axis separation.
!
         START  = 0.D0
!
!        If the vertex is outside of the bounding sphere,
!        set the initial angle to the supplement of
!        the angular radius of the target, based on
!        its maximum radius.
!
         R      = VNORM( AXIS )
         MINDST = MAXRAD * ( 1.D0 + MARGIN )

         IF ( R .GT. MINDST ) THEN

            START = PI() -  DASINE( MAXRAD/R, MARGIN )

        END IF

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZTANGNT' )
            RETURN
         END IF
!
!        Set the final ray-axis separation.
!
         FINISH = PI()


      ELSE
!
!        For the terminator cases, check for an invalid source radius.
!
         IF ( SRCRAD .LE. 0.D0 ) THEN

            CALL SETMSG ( 'Source radius # is non-positive.' )
            CALL ERRDP  ( '#',  SRCRAD                       )
            CALL SIGERR ( 'SPICE(INVALIDRADIUS)'             )
            CALL CHKOUT ( 'ZZTANGNT'                         )
            RETURN

         END IF

!
!        Make sure the source and outer bounding sphere of the
!        target don't intersect.
!
         R = VNORM( AXIS )

         IF ( (SRCRAD + MAXRAD) .GT. R ) THEN

            CALL SETMSG ( 'Source radius # and target maximum ' &
            //            'radius # sum to #; distance between ' &
            //            'source and target centers is #. Source ' &
            //            'and target are too close together.'     )
            CALL ERRDP  ( '#',  SRCRAD                             )
            CALL ERRDP  ( '#',  MAXRAD                             )
            CALL ERRDP  ( '#',  R                                  )
            CALL SIGERR ( 'SPICE(OBJECTSTOOCLOSE)'                 )
            CALL CHKOUT ( 'ZZTANGNT'                               )
            RETURN

         END IF


         IF ( CURVE .EQ. UMBRAL ) THEN
!
!           We'll search for a point on the umbral terminator.
!
!           For this search, the angle we measure is that between a ray
!           tangent to the source and a ray emanating from the tangent
!           point and parallel to the axis, pointing in the
!           target-source direction. The ray lies in a plane containing
!           the source-target axis.
!
!           The minimum angle is achieved when the ray is tangent to
!           the target sphere; the maximum angle is achieved when the
!           ray intersects the target center.
!
!
!           The following equation is valid regardless of whether
!           or not SRCRAD > MAXRAD.
!
            START = PI() + DASINE( (SRCRAD-MAXRAD)/R, MARGIN )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZTANGNT' )
               RETURN
            END IF

!
!           Set the final ray-axis separation.
!
            FINISH = PI() +  DASINE( SRCRAD/R, MARGIN )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZTANGNT' )
               RETURN
            END IF


         ELSE IF ( CURVE .EQ. PNMBRL ) THEN
!
!           We'll search for a point on the umbral terminator.
!
!           We measure the ray's angle from the axis, but in this case,
!           the angle increases in the clockwise (negative sense about
!           the normal to the cutting half-plane defined by the cross
!           product of the axis and the reference vector. Each ray
!           emanating from, and tangent to, the source's surface passes
!           through the axis at a point between the source and target.
!
!           In order to use the root-finding utilities, we treat the
!           angle as a positive quantity.
!
!           The initial ray contains a line segment connecting
!           tangency points on each sphere. The segment, axis,
!           and radii of the spheres form two similar triangles.
!           Below, ALPHA is the fraction of the source-target
!           distance belonging to the triangle having a vertex
!           at the center of the source.
!
            ALPHA = SRCRAD / ( SRCRAD + MAXRAD )

            START = PI() -  DASINE( SRCRAD / (ALPHA*R), MARGIN )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZTANGNT' )
               RETURN
            END IF

!
!           We stop looking when the ray intersects the target center.
!
            FINISH = PI() - DASINE( SRCRAD / R, MARGIN )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZTANGNT' )
               RETURN
            END IF

         ELSE

            CALL SETMSG ( 'Input curve code # was not recognized.' )
            CALL ERRINT ( '#', CURVE                               )
            CALL SIGERR ( 'SPICE(BUG)'                             )
            CALL CHKOUT ( 'ZZTANGNT'                               )
            RETURN

         END IF

      END IF

!
!     Search for ray occultations. The endpoints of the occultation
!     intervals are angles at which tangency occurs.
!
!     We consider the angle to be measured from the AXIS vector.
!     The initial and final values START and FINISH have been set
!     above.
!
      CSTEP  = .TRUE.

      CALL ZZTANSLV ( ZZTANSTA, GFSTEP, GFREFN, &
                      CSTEP,    SCHSTP, START,  FINISH, &
                      SOLTOL,   RESULT, POINTS, ENDFLG )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZTANGNT' )
         RETURN
      END IF

!
!     If the first endpoint of RESULT is the interval start but is not
!     a point of transition, delete it from RESULT. Note that RESULT
!     becomes a cell rather than a window. We must delete the
!     corresponding point from the POINTS array as well.
!
      IF ( CARDD(RESULT) .GT. 0 ) THEN

         IF ( ( RESULT(1) .EQ. START ) .AND. ( .NOT. ENDFLG(1) ) ) THEN

            N = CARDD( RESULT )

            DO I = 2, N

               RESULT(I-1) = RESULT(I)

               CALL VEQU ( POINTS(1,I), POINTS(1,I-1) )

            END DO

            CALL SCARDD ( N-1, RESULT )

         END IF

      END IF

!
!     If the final endpoint of RESULT is not a transition, delete
!     it as well. In this case decrementing the cardinality of
!     RESULT suffices.
!
      N = CARDD(RESULT)

      IF ( N .GT. 0 ) THEN

         IF ( ( RESULT(N) .EQ. FINISH ) .AND. ( .NOT. ENDFLG(2) ) ) THEN

            CALL SCARDD ( N-1, RESULT )

         END IF

      END IF

      CALL CHKOUT ( 'ZZTANGNT' )
      RETURN
      END
