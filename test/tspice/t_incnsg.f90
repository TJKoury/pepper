!$Procedure   T_INCNSG ( Test intersection of cone and line segment )

      SUBROUTINE T_INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                            ENDPT2, NXPTS, XPT1,  XPT2   )

! $ Abstract
!
!     Test utility: exercise the "brute force" logic of INCNSG.
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
!     CONE
!     GEOMETRY
!     INTERSECTION
!     LINE
!     MATH
!     SEGMENT
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      APEX   ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      ENDPT1 ( 3 )
      DOUBLE PRECISION      ENDPT2 ( 3 )
      INTEGER               NXPTS
      DOUBLE PRECISION      XPT1   ( 3 )
      DOUBLE PRECISION      XPT2   ( 3 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     APEX       I   Apex of cone.
!     AXIS       I   Axis of cone.
!     ANGLE      I   Angle of cone.
!     ENDPT1,
!     ENDPT2     I   Endpoints of line segment.
!     NXPTS      O   Number of intersection points.
!     XPT1       O   First intersection point, if it exists.
!     XPT2       O   Second intersection point, if it exists.
!
! $ Detailed_Input
!
!     APEX       is the apex (tip) of the cone. In this routine's
!                documentation, we'll consider the cone to be a
!                semi-infinite pyramid with circular cross-section. In
!                some contexts, this object is called one "nappe" of
!                the complete cone.
!
!     AXIS       is an axis vector of the cone.
!
!     ANGLE      is the angular separation from AXIS of the rays
!                comprising the cone. Let the notation
!
!                   < A, B >
!
!                denote the dot product of vectors A and B, and let
!
!                   ||A||
!
!                denote the norm of vector A. Then the cone is the set
!                of points
!
!                             X-APEX       AXIS
!                   { X:  < ----------,  -------- >  =  cos(ANGLE) }
!                           ||X-APEX||   ||AXIS||
!
!
!     ENDPT1,
!     ENDPT2     are endpoints of a line segment. These points
!                must be distinct.
!
! $ Detailed_Output
!
!     NXPTS      is the number of points of intersection of the input
!                line segment and cone.
!
!     XPT1       is the point of intersection of the segment and cone
!                that is closest to ENDPT1, if an intersection exists.
!                If there are no intersections, XPT1 is undefined.
!
!     XPT2       is the point of intersection of the segment and cone
!                that is farthest from ENDPT1, if two points of
!                intersection exist. If there are not two
!                intersections, XPT2 is undefined.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If AXIS is the zero vector, the error SPICE(ZEROVECTOR)
!         will be signaled.
!
!     2)  If ANGLE is less than zero, the error SPICE(INVALIDANGLE)
!         will be signaled.
!
!     3)  If ENDPT1 and ENDPT2 coincide, the error
!         SPICE(ENDPOINTSMATCH) will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is used by the SPICELIB DSK subsystem. In
!     particular, it is used to determine whether a ray contacts a
!     latitude boundary of a volume element in either planetocentric
!     latitudinal or planetodetic coordinates.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as input
!     (if any), the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Compute the intersection of a line segment and cone in
!        a simple case for which the results can easily be checked.
!
!        Let the apex of the cone be at the origin. Let the axis
!        of the cone lie on the +X axis. Let the angle of the cone
!        be 45 degrees. Let the line segment have endpoints
!
!           ENDPT1 = ( 1,   -2, sqrt(3)/2 )
!           ENDPT2 = ( 1,    2, sqrt(3)/2 )
!
!        We expect there to be two points of intersection:
!
!           XPT1   = ( 1, -1/2, sqrt(3)/2 )
!           XPT2   = ( 1,  1/2, sqrt(3)/2 )
!
!
!        Example code begins here.
!
!
!              PROGRAM EX1
!              IMPLICIT NONE
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      RPD
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         FMT1
!              PARAMETER           ( FMT1 = '(A,3F13.8)' )
!
!              CHARACTER*(*)         FMT2
!              PARAMETER           ( FMT2 = '(A,I2)' )
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      ANGLE
!              DOUBLE PRECISION      APEX   ( 3 )
!              DOUBLE PRECISION      AXIS   ( 3 )
!              DOUBLE PRECISION      ENDPT1 ( 3 )
!              DOUBLE PRECISION      ENDPT2 ( 3 )
!              DOUBLE PRECISION      SQ3
!              DOUBLE PRECISION      XPT1   ( 3 )
!              DOUBLE PRECISION      XPT2   ( 3 )
!
!              INTEGER               NXPTS
!
!        C
!        C     Set up the cone's geometric attributes.
!        C
!              CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )
!              CALL VPACK ( 1.D0, 0.D0, 0.D0, AXIS )
!
!              ANGLE = 45.D0 * RPD()
!        C
!        C     Initialize the line segment's endpoints.
!        C
!              SQ3 = SQRT( 3.D0  )
!
!              CALL VPACK ( 1.D0, -2.D0, SQ3/2, ENDPT1 )
!              CALL VPACK ( 1.D0,  2.D0, SQ3/2, ENDPT2 )
!        C
!        C     Find the points of intersection.
!        C
!              CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1,
!             .              ENDPT2, NXPTS, XPT1,  XPT2   )
!
!              WRITE (*,*) ' '
!              WRITE (*,FMT1) 'Apex:        ', APEX
!              WRITE (*,FMT1) 'Axis:        ', AXIS
!              WRITE (*,FMT1) 'Angle (deg): ', ANGLE/RPD()
!              WRITE (*,FMT1) 'Endpoint 1:  ', ENDPT1
!              WRITE (*,FMT1) 'Endpoint 2:  ', ENDPT2
!              WRITE (*,*) ' '
!              WRITE (*,FMT2) 'Number of intersection points: ',
!             .            NXPTS
!              WRITE (*,*) ' '
!              WRITE (*,FMT1) 'Point 1:    ', XPT1
!              WRITE (*,FMT1) 'Point 2:    ', XPT2
!              WRITE (*,*) ' '
!
!              END
!
!
!     When this program was executed on a PC/Linux/gfortran/64-bit
!     platform, the output was:
!
!
!        Apex:           0.00000000   0.00000000   0.00000000
!        Axis:           1.00000000   0.00000000   0.00000000
!        Angle (deg):   45.00000000
!        Endpoint 1:     1.00000000  -2.00000000   0.86602540
!        Endpoint 2:     1.00000000   2.00000000   0.86602540
!
!        Number of intersection points:  2
!
!        Point 1:       1.00000000  -0.50000000   0.86602540
!        Point 2:       1.00000000   0.50000000   0.86602540
!
!
! $ Restrictions
!
!     1)  If the sine of the input cone angle is 1, this routine treats
!         the cone as though it were a plane normal to the input axis
!         and containing the apex.
!
!     2)  This routine is designed to avoid arithmetic overflow in
!         normal cases, such as those in which the line segment is
!         nearly parallel to the cone. However, it is possible to cause
!         arithmetic overflow by using input vectors with extremely
!         large magnitudes.
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
!-    SPICELIB Version 1.1.0 14-OCT-2021 (BVS)
!
!        Bug fix: fixed routine name in CHKIN/CHKOUT calls
!        (INCNSG -> T_INCNSG).
!
!-    SPICELIB Version 1.0.0 30-SEP-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     intersection of line segment and cone
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )
!
!     Local variables
!
      DOUBLE PRECISION      AXMAG
      DOUBLE PRECISION      COLAT
      DOUBLE PRECISION      COSANG
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      DMAG
      DOUBLE PRECISION      DP1
      DOUBLE PRECISION      DP2
      DOUBLE PRECISION      LOCANG
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXP   ( 3 )
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINP   ( 3 )
      DOUBLE PRECISION      NRMPLN ( UBPL )
      DOUBLE PRECISION      OFF1   ( 3 )
      DOUBLE PRECISION      OFF2   ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      PLNX   ( 3 )
      DOUBLE PRECISION      UAXIS  ( 3 )
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      UOFF1  ( 3 )
      DOUBLE PRECISION      UOFF2  ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      VTEMP2 ( 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      XOFF1  ( 3 )
      DOUBLE PRECISION      XOFF2  ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               I
      INTEGER               NPLNX

      LOGICAL               IN1
      LOGICAL               IN2
      LOGICAL               ISBRCK
      LOGICAL               NEG1
      LOGICAL               NEG2

!
!     Saved values
!
      SAVE                  ORIGIN
      SAVE                  Y
      SAVE                  Z

!
!     Initial values
!
      DATA                  ORIGIN / 3 * 0.D0         /
      DATA                  Y      / 0.D0, 1.D0, 0.D0 /
      DATA                  Z      / 0.D0, 0.D0, 1.D0 /

!
!     Use quasi-discovery check-in. We'll check in before
!     code sections that can generate SPICE errors, and check
!     out afterwards. When those code sections are skipped,
!     we avoid traceback participation.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     No intersection was found so far.
!
      NXPTS = 0

!
!     The cone's axis vector must be non-zero.
!
      CALL UNORM ( AXIS, UAXIS, AXMAG )

      IF ( AXMAG .EQ. 0.D0 ) THEN

         CALL CHKIN  ( 'T_INCNSG'                                     )
         CALL SETMSG ( 'The cone''s axis vector must be non-zero ' &
         //            'but sadly, it failed to meet this criterion.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                            )
         CALL CHKOUT ( 'T_INCNSG'                                     )
         RETURN

      END IF

!
!     The cone's angular radius must be non-negative.
!
      IF ( ANGLE .LT. 0.D0 ) THEN

         CALL CHKIN  ( 'T_INCNSG'                           )
         CALL SETMSG ( 'The cone''s angular radius must be ' &
         //            ' non-negative but was # (radians).' )
         CALL ERRDP  ( '#', ANGLE                           )
         CALL SIGERR ( 'SPICE(INVALIDANGLE)'                )
         CALL CHKOUT ( 'T_INCNSG'                           )
         RETURN

      END IF

!
!     The endpoints of the segment must be distinct. Check this after
!     computing a unit direction vector for the line segment.
!
      CALL VSUB  ( ENDPT2, ENDPT1, DIR )

      CALL UNORM ( DIR, UDIR, DMAG )

      IF ( DMAG .EQ. 0.D0 ) THEN

         CALL CHKIN  ( 'T_INCNSG'                            )
         CALL SETMSG ( 'The distance between the segment''s ' &
         //            'endpoints was zero. First endpoint: ' &
         //            '(# # #).'                            )
         CALL ERRDP  ( '#', ENDPT1(1)                        )
         CALL ERRDP  ( '#', ENDPT1(2)                        )
         CALL ERRDP  ( '#', ENDPT1(3)                        )
         CALL SIGERR ( 'SPICE(ENDPOINTSMATCH)'               )
         CALL CHKOUT ( 'T_INCNSG'                            )
         RETURN

      END IF

!
!     Store the cosine of the cone's angular radius. We'll treat all
!     cases with COSANG equal to 0 as though the cone is actually a
!     plane normal to the axis and containing the apex.
!
      COSANG = COS( ANGLE )
      LOCANG = ANGLE

!
!     We'll work with a local axis that has angular separation of
!     no more than pi/2 from the nappe.
!
      IF ( COSANG .LT. 0.D0 ) THEN

         LOCANG   = PI() - ANGLE
         COSANG   = - COSANG

         UAXIS(1) = - UAXIS(1)
         UAXIS(2) = - UAXIS(2)
         UAXIS(3) = - UAXIS(3)

      END IF

!
!     Compute the offsets of the endpoints of the segment from
!     the cone's apex.
!
      CALL VSUB ( ENDPT1, APEX, OFF1 )
      CALL VSUB ( ENDPT2, APEX, OFF2 )

!
!     Deal with some of the simple cases first.
!
      CALL VHAT ( OFF1, UOFF1 )
      CALL VHAT ( OFF2, UOFF2 )

      DP1  = VDOT( UOFF1, UAXIS )
      DP2  = VDOT( UOFF2, UAXIS )

!
!     The given axis is inside the nappe defined by the angular radius.
!
!     There's no intersection if both endpoints are in the interior of
!     the nappe of the cone (since the nappe is convex).
!
      IN1  = DP1 .GE. COSANG
      IN2  = DP2 .GE. COSANG
!
!     If the line segment lies on the far side of the plane that
!     contains the apex and is orthogonal to the axis, there's no
!     intersection.
!
      NEG1 = DP1 .LT. 0.D0
      NEG2 = DP2 .LT. 0.D0

      IF (      ( IN1  .AND. IN2  ) &
           .OR. ( NEG1 .AND. NEG2 ) ) THEN
!
!        The segment is in the interior of the cone or
!        on the far side of the plane.
!
         NXPTS = 0

         RETURN

      END IF

!
!     Here's where we handle the half-space case.
!
      IF ( COSANG .EQ. 0.D0 ) THEN
!
!        See whether the ray emanating from the first endpoint and
!        having direction UDIR hits the plane normal to the axis and
!        containing the apex. We'll call this plane NRMPLN.
!
!        NVP2PL can signal an error only if the input axis is the
!        zero vector. We've ensured that it isn't.
!
         CALL NVP2PL ( UAXIS,  APEX, NRMPLN )
         CALL INRYPL ( ENDPT1, UDIR, NRMPLN, NPLNX, PLNX )

!
!        If the ray doesn't hit the plane, we're done. Otherwise,
!        check the intercept.
!
         IF ( NPLNX .EQ. 1 ) THEN
!
!           The ray does hit the plane. If the intersection is on the
!           line segment, we have a solution.
!
            IF ( VDIST(PLNX, ENDPT1) .LE. DMAG ) THEN
!
!              The intercept is not further along the ray than the
!              second endpoint. It's a valid solution.
!
               NXPTS = 1
               CALL VEQU ( PLNX, XPT1 )

            END IF

         END IF
!
!        This is the end of the half-space case.
!
         RETURN

      END IF


      IF ( NXPTS .LT. 2 ) THEN
!
!        We must determine the expected number of roots, and if
!        we didn't come up with them, we must find the roots
!        by an alternate method.
!
!        We'll examine the containment of the endpoints within the
!        cone.
!
!        The case where both endpoints are inside the cone was handled
!        earlier.
!
!        If one endpoint is inside the cone and one is outside,
!        we expect to have one root.
!
         IF (     ( IN1 .AND. ( .NOT. IN2 ) ) &
             .OR. ( IN2 .AND. ( .NOT. IN1 ) )  ) THEN
!
!           There's supposed to be one root. If we found none, find one
!           now.
!
            IF ( NXPTS .EQ. 0 ) THEN
!
!              ZZCXBRUT signals an error if the axis is the zero
!              vector, but not otherwise. We've already ruled out this
!              situation. Therefore, we don't check in before the
!              following call.
!
               CALL ZZCXBRUT ( APEX,   UAXIS,  LOCANG, &
                               ENDPT1, ENDPT2, XPT1,  ISBRCK )

               IF ( ISBRCK ) THEN
!
!                 As long as the root was bracketed, XPT1 is a
!                 solution.
!
                  NXPTS = 1

               END IF

            END IF


         ELSE

            CALL CHKIN ( 'T_INCNSG' )
!
!           Both endpoints are outside the cone. We could have zero to
!           two roots. If the minimum angular separation of the segment
!           from the axis is less than ANGLE, we expect to find two
!           roots; if it's equal to ANGLE, we expect to find one, and
!           if it's greater than ANGLE, none.
!
!           We'll transform OFF1 and OFF2 into a reference frame in
!           which angular separation from the axis is equivalent to
!           colatitude. Then we'll find the maximum latitude attained
!           on the segment.
!
!           We'll count the roots we find, so we'll start at zero.
!
            NXPTS = 0

            CALL FRAME ( UAXIS, X, Y )

            DO I = 1, 3

               XFORM(1,I) = X(I)
               XFORM(2,I) = Y(I)
               XFORM(3,I) = UAXIS(I)

            END DO

            CALL MXV ( XFORM, OFF1, XOFF1 )
            CALL MXV ( XFORM, OFF2, XOFF2 )

            CALL ZZSGLATX ( XOFF1, XOFF2, MINLAT, MINP, MAXLAT, MAXP )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'T_INCNSG' )
               RETURN
            END IF

!
!           COLAT is the colatitude of the point of maximum latitude.
!
            COLAT = HALFPI() - MAXLAT


            IF ( COLAT .LT. LOCANG ) THEN


!
!              MAXP is inside the cone. There should be an intersection
!              on the segment between XOFF1 and MAXP and another
!              between MAXP and XOFF2.
!
               CALL ZZCXBRUT ( ORIGIN, Z,    LOCANG, &
                               XOFF1,  MAXP, VTEMP,  ISBRCK )

               IF ( ISBRCK ) THEN
!
!                 Convert VTEMP to the original frame, then translate
!                 it so that it's represented as an offset from the
!                 origin.
!
                  CALL MTXV ( XFORM,  VTEMP, VTEMP2 )
                  CALL VADD ( VTEMP2, APEX,  XPT1   )

                  NXPTS = 1

               END IF

               CALL ZZCXBRUT ( ORIGIN, Z,     LOCANG, &
                               MAXP,   XOFF2, VTEMP,  ISBRCK )

               IF ( ISBRCK ) THEN
!
!                 Convert VTEMP to the original frame, then translate
!                 it so that it's represented as an offset from the
!                 origin.
!
                  CALL MTXV ( XFORM,  VTEMP, VTEMP2 )
                  CALL VADD ( VTEMP2, APEX,  XPT2   )

                  IF ( NXPTS .EQ. 1 ) THEN
!
!                    Both roots are valid.
!
                     NXPTS = 2

                  ELSE
!
!                    The second root is the only valid root. Move it
!                    into XPT1.
!
                     CALL VEQU ( XPT2, XPT1 )

                     NXPTS = 1

                  END IF

               END IF


            ELSE IF ( COLAT .EQ. LOCANG ) THEN
!
!              The root corresponds to a point of tangency of
!              the segment and cone. This occurs at the point
!              having maximum latitude: MAXP.
!
               CALL VEQU ( MAXP, XPT1 )

               NXPTS = 1

!
!           Note that if COLAT > LOCANG, there are no roots.
!
            END IF


            CALL CHKOUT ( 'T_INCNSG' )

         END IF
!
!        This is the end of the "brute force" case with both endpoints
!        outside the cone.
!
      END IF

!
!     NXPTS  has been set.
!
!     If NXPTS is 1, then XPT1 is set.
!
!     If NXPTS is 2, then both XPT1 and XPT2 are set.
!

      RETURN
      END




