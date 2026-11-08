!$Procedure ZZEDTERM ( Ellipsoid terminator )

      SUBROUTINE ZZEDTERM ( TYPE,   A,      B,     C, &
                            SRCRAD, SRCPOS, NPTS,  TRMPTS )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute a set of points on the umbral or penumbral terminator of
!     a specified ellipsoid, given a spherical light source.
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
!     ELLIPSES
!
! $ Keywords
!
!     BODY
!     GEOMETRY
!     MATH
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         TYPE
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      SRCRAD
      DOUBLE PRECISION      SRCPOS ( 3 )
      INTEGER               NPTS
      DOUBLE PRECISION      TRMPTS ( 3, NPTS )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TYPE       I   Terminator type.
!     A          I   Length of ellipsoid semi-axis lying on the x-axis.
!     B          I   Length of ellipsoid semi-axis lying on the y-axis.
!     C          I   Length of ellipsoid semi-axis lying on the z-axis.
!     SRCRAD     I   Radius of light source.
!     SRCPOS     I   Position of center of light source.
!     NPTS       I   Number of points in terminator point set.
!     TRMPTS     O   Terminator point set.
!
! $ Detailed_Input
!
!     TYPE           is a string indicating the type of terminator to
!                    compute:  umbral or penumbral.  The umbral
!                    terminator is the boundary of the portion of the
!                    ellipsoid surface in total shadow.  The penumbral
!                    terminator is the boundary of the portion of the
!                    surface that is completely illuminated.  Possible
!                    values of TYPE are
!
!                       'UMBRAL'
!                       'PENUMBRAL'
!
!                    Case and leading or trailing blanks in TYPE are
!                    not significant.
!
!     A,
!     B,
!     C              are the lengths of the semi-axes of a triaxial
!                    ellipsoid.  The ellipsoid is centered at the
!                    origin and oriented so that its axes lie on the
!                    x, y and z axes.  A, B, and C are the lengths of
!                    the semi-axes that point in the x, y, and z
!                    directions respectively.
!
!                    Length units associated with A, B, and C must
!                    match those associated with SRCRAD, SRCPOS,
!                    and the output TRMPTS.
!
!     SRCRAD         is the radius of the spherical light source.
!
!     SRCPOS         is the position of the center of the light source
!                    relative to the center of the ellipsoid.
!
!     NPTS           is the number of terminator points to compute.
!
!
! $ Detailed_Output
!
!     TRMPTS         is an array of points on the umbral or penumbral
!                    terminator of the ellipsoid, as specified by the
!                    input argument TYPE.  The Ith point is contained
!                    in the array elements
!
!                        TRMPTS(J,I),  J = 1, 2, 3
!
!                    The terminator points are expressed in the
!                    body-fixed reference frame associated with the
!                    ellipsoid.  Units are those associated with
!                    the input axis lengths.
!
!                    Each terminator point is the point of tangency of
!                    a plane that is also tangent to the light source.
!                    These associated points of tangency on the light
!                    source have uniform distribution in longitude when
!                    expressed in a cylindrical coordinate system whose
!                    Z-axis is SRCPOS.  The magnitude of the separation
!                    in longitude between these tangency points on the
!                    light source is
!
!                       2*Pi / NPTS
!
!                    If the target is spherical, the terminator points
!                    also are uniformly distributed in longitude in the
!                    cylindrical system described above.  If the target
!                    is non-spherical, the longitude distribution of
!                    the points generally is not uniform.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the terminator type is not recognized, the error
!         SPICE(NOTSUPPORTED) is signaled.
!
!     2)  If the set size NPTS is not at least 1, the error
!         SPICE(INVALIDSIZE) is signaled.
!
!     3)  If any of the ellipsoid's semi-axis lengths is non-positive,
!         the error SPICE(INVALIDAXISLENGTH) is signaled.
!
!     4)  If the light source has non-positive radius, the error
!         SPICE(INVALIDRADIUS) is signaled.
!
!     5)  If the light source intersects the smallest sphere
!         centered at the origin and containing the ellipsoid, the
!         error SPICE(OBJECTSTOOCLOSE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine models the boundaries of shadow regions on an
!     ellipsoid "illuminated" by a spherical light source.  Light rays
!     are assumed to travel along straight lines; refraction is not
!     modeled.
!
!     Points on the ellipsoid at which the entire cap of the light
!     source is visible are considered to be completely illuminated.
!     Points on the ellipsoid at which some portion (or all) of the cap
!     of the light source are blocked are considered to be in partial
!     (or total) shadow.
!
!     In this routine, we use the term "umbral terminator" to denote
!     the curve usually called the "terminator":  this curve is the
!     boundary of the portion of the surface that lies in total shadow.
!     We use the term "penumbral terminator" to denote the boundary of
!     the completely illuminated portion of the surface.
!
!     In general, the terminator on an ellipsoid is a more complicated
!     curve than the limb (which is always an ellipse).  Aside from
!     various special cases, the terminator does not lie in a plane.
!
!     However, the condition for a point X on the ellipsoid to lie on
!     the terminator is simple:  a plane tangent to the ellipsoid at X
!     must also be tangent to the light source.  If this tangent plane
!     does not intersect the vector from the center of the ellipsoid to
!     the center of the light source, then X lies on the umbral
!     terminator; otherwise X lies on the penumbral terminator.
!
! $ Examples
!
!     See the SPICELIB routine EDTERM.
!
! $ Restrictions
!
!     This is a private SPICELIB routine.  User applications should not
!     call this routine.
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
!-    SPICELIB Version 1.1.1, 02-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 1.1.0, 24-APR-2012 (NJB)
!
!        Deleted computations of unused quantities
!        MAXANG and MINANG.
!
!-    SPICELIB Version 1.0.0, 03-FEB-2007 (NJB)
!
!-&

! $ Index_Entries
!
!     find terminator on ellipsoid
!     find umbral terminator on ellipsoid
!     find penumbral terminator on ellipsoid
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      INTEGER               MAXITR
      PARAMETER           ( MAXITR = 10 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 50 )

!
!     Local variables
!
      CHARACTER*(TYPLEN)    LOCTYP

      DOUBLE PRECISION      ANGERR
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      E      ( 3 )
      DOUBLE PRECISION      INANG
      DOUBLE PRECISION      LAMBDA
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      MINRAD
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      OUTANG
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      PLCONS
      DOUBLE PRECISION      PRVANG
      DOUBLE PRECISION      PRVDIF
      DOUBLE PRECISION      RMAX
      DOUBLE PRECISION      RMIN
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SRCPT  ( 3 )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TRANS  ( 3, 3 )
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      VTX    ( 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               I
      INTEGER               NITR

      LOGICAL               UMBRAL


!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEDTERM' )

!
!     Check the terminator type.
!
      CALL LJUST ( TYPE,   LOCTYP )
      CALL UCASE ( LOCTYP, LOCTYP )

      IF (  LOCTYP .EQ. 'UMBRAL' ) THEN

         UMBRAL = .TRUE.

      ELSE IF ( LOCTYP .EQ. 'PENUMBRAL' ) THEN

         UMBRAL = .FALSE.

      ELSE

         CALL SETMSG ( 'Terminator type must be UMBRAL or ' &
         //            'PENUMBRAL but was actually #.'     )
         CALL ERRCH  ( '#',  TYPE                          )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'               )
         CALL CHKOUT ( 'ZZEDTERM'                          )
         RETURN

      END IF

!
!     Check the terminator set dimension.
!
      IF ( NPTS .LT. 1 ) THEN

         CALL SETMSG ( 'Set must contain at least one point; NPTS ' &
         //            ' = #.'                                        )
         CALL ERRINT ( '#',  NPTS                                     )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                           )
         CALL CHKOUT ( 'ZZEDTERM'                                     )
         RETURN

      END IF

!
!     The ellipsoid semi-axes must have positive length.
!
      IF (       ( A .LE. 0.D0 ) &
           .OR.  ( B .LE. 0.D0 ) &
           .OR.  ( C .LE. 0.D0 )   )   THEN

         CALL SETMSG ( 'Semi-axis lengths:  A = #, B = #, C = #. ' )
         CALL ERRDP  ( '#', A                                      )
         CALL ERRDP  ( '#', B                                      )
         CALL ERRDP  ( '#', C                                      )
         CALL SIGERR ( 'SPICE(INVALIDAXISLENGTH)'                  )
         CALL CHKOUT ( 'ZZEDTERM'                                  )
         RETURN

      END IF

!
!     Check the input light source radius.
!
      IF ( SRCRAD .LE. 0.D0 ) THEN

         CALL SETMSG ( 'Light source must have positive radius; ' &
         //            'actual radius was #.'                    )
         CALL ERRDP  ( '#', SRCRAD                               )
         CALL SIGERR ( 'SPICE(INVALIDRADIUS)'                    )
         CALL CHKOUT ( 'ZZEDTERM'                                )
         RETURN

      END IF

!
!     The light source must not intersect the outer bounding
!     sphere of the ellipsoid.
!
      D    =  VNORM ( SRCPOS )

      RMAX =  MAX ( A, B, C )
      RMIN =  MIN ( A, B, C )

      IF (  ( SRCRAD + RMAX )  .GE.  D  ) THEN
!
!        The light source is too close.
!
         CALL SETMSG ( 'Light source intersects outer bounding ' &
         //            'sphere of the ellipsoid.  Light source ' &
         //            'radius = #; ellipsoid''s longest axis = ' &
         //            '#; sum = #; distance between centers = #.' )
         CALL ERRDP  ( '#', SRCRAD                                 )
         CALL ERRDP  ( '#', RMAX                                   )
         CALL ERRDP  ( '#', SRCRAD + RMAX                          )
         CALL ERRDP  ( '#', D                                      )
         CALL SIGERR ( 'SPICE(OBJECTSTOOCLOSE)'                    )
         CALL CHKOUT ( 'ZZEDTERM'                                  )
         RETURN

      END IF

!
!     Let the negative of the ellipsoid-light source vector be the
!     Z-axis of a frame we'll use to generate the terminator set.
!
      CALL VMINUS ( SRCPOS, Z )

      CALL FRAME  ( Z,  X,  Y )

!
!     Create the rotation matrix required to convert vectors
!     from the source-centered frame back to the target body-fixed
!     frame.
!
      CALL VEQU ( X, TRANS(1,1) )
      CALL VEQU ( Y, TRANS(1,2) )
      CALL VEQU ( Z, TRANS(1,3) )

!
!     Find the maximum and minimum target radii.
!
      MAXRAD = MAX ( A, B, C )
      MINRAD = MIN ( A, B, C )

      IF ( UMBRAL ) THEN
!
!        Compute the angular offsets from the axis of rays tangent to
!        both the source and the bounding spheres of the target, where
!        the tangency points lie in a half-plane bounded by the line
!        containing the origin and SRCPOS.  (We'll call this line
!        the "axis.")
!
!        OUTANG corresponds to the target's outer bounding sphere;
!        INANG to the inner bounding sphere.
!
         OUTANG = ASIN ( (SRCRAD - MAXRAD) / D )
         INANG  = ASIN ( (SRCRAD - MINRAD) / D )

      ELSE
!
!        Compute the angular offsets from the axis of rays tangent to
!        both the source and the bounding spheres of the target, where
!        the tangency points lie in opposite half-planes bounded by the
!        axis (compare the case above).
!
!        OUTANG corresponds to the target's outer bounding sphere;
!        INANG to the inner bounding sphere.
!
         OUTANG = ASIN (  ( SRCRAD + MAXRAD )  /  D  )
         INANG  = ASIN (  ( SRCRAD + MINRAD )  /  D  )

      END IF

!
!     Compute the angular delta we'll use for generating
!     terminator points.
!
      DELTA = TWOPI() / NPTS

!
!     Generate the terminator points.
!
      DO I = 1, NPTS

         THETA = (I-1)*DELTA

!
!        Let SRCPT be the surface point on the source lying in
!        the X-Y plane of the frame produced by FRAME
!        and corresponding to the angle THETA.
!
         CALL LATREC ( SRCRAD,  THETA,  0.D0,  SRCPT )

!
!        Now solve for the angle by which SRCPT must be rotated (toward
!        +Z in the umbral case, away from +Z in the penumbral case)
!        so that a plane tangent to the source at SRCPT is also tangent
!        to the target. The rotation is bracketed by OUTANG on the low
!        side and INANG on the high side in the umbral case; the
!        bracketing values are reversed in the penumbral case.
!
         IF ( UMBRAL ) THEN
            ANGLE  = OUTANG
         ELSE
            ANGLE  = INANG
         END IF

         PRVDIF = TWOPI()
         PRVANG = ANGLE + HALFPI()
         NITR   = 0

         DO WHILE (       ( NITR                        .LE. MAXITR ) &
                    .AND. ( TOUCHD( ABS(ANGLE-PRVANG) ) .LT. PRVDIF )  )

            NITR    =   NITR  + 1
            PRVDIF  =   TOUCHD ( ABS(ANGLE-PRVANG) )
            PRVANG  =   ANGLE
!
!           Find the closest point on the ellipsoid to the plane
!           corresponding to "ANGLE".
!
!           The tangent point on the source is obtained by rotating
!           SRCPT by ANGLE towards +Z.  The plane's normal vector is
!           parallel to VTX in the source-centered frame.
!
            CALL LATREC ( SRCRAD, THETA, ANGLE, VTX )

            CALL VEQU   ( VTX, DIR )

!
!           VTX and DIR are expressed in the source-centered frame.  We
!           must translate VTX to the target frame and rotate both
!           vectors into that frame.
!
            CALL MXV  ( TRANS,  VTX,   VTEMP )
            CALL VADD ( SRCPOS, VTEMP, VTX   )

            CALL MXV  ( TRANS,  DIR,   VTEMP )
            CALL VEQU ( VTEMP,         DIR   )

!
!           Create the plane defined by VTX and DIR.
!
            CALL NVP2PL ( DIR, VTX, PLANE )
!
!           Find the closest point on the ellipsoid to the plane. At
!           the point we seek, the outward normal on the ellipsoid is
!           parallel to the choice of plane normal that points away
!           from the origin.  We can always obtain this choice from
!           PL2NVC.
!
            CALL PL2NVC ( PLANE, DIR, PLCONS )

!
!           At the point
!
!               E = (x, y, z)
!
!           on the ellipsoid's surface, an outward normal
!           is
!
!               N = ( x/A**2, y/B**2, z/C**2 )
!
!           which is also
!
!               lambda * ( DIR(1), DIR(2), DIR(3) )
!
!           Equating components in the normal vectors yields
!
!               E = lambda * ( DIR(1)*A**2, DIR(2)*B**2, DIR(3)*C**2 )
!
!           Taking the inner product with the point E itself and
!           applying the ellipsoid equation, we find
!
!               lambda * <DIR, E>  =  < N, E >  =  1
!
!           The first term above is
!
!               lambda**2 * || ( A*DIR(1), B*DIR(2), C*DIR(3) ) ||**2
!
!           So the positive root lambda is
!
!               1 / || ( A*DIR(1), B*DIR(2), C*DIR(3) ) ||
!
!           Having lambda we can compute E.
!

            CALL VPACK ( A*DIR(1), B*DIR(2), C*DIR(3), V )

            LAMBDA = 1.D0 / VNORM(V)

            CALL VPACK ( A*V(1), B*V(2), C*V(3), E )

            CALL VSCL  ( LAMBDA, E, TRMPTS(1,I) )

!
!           Make a new estimate of the plane rotation required to touch
!           the target.
!
            CALL VSUB ( TRMPTS(1,I), VTX, OFFSET )

!
!           Let ANGERR be an estimate of the magnitude of angular error
!           between the plane and the terminator.
!
            ANGERR = VSEP ( DIR, OFFSET ) - HALFPI()

!
!           Let S indicate the sign of the altitude error:  where
!           S is positive, the plane is above E.
!
            S = SIGN ( 1.D0, VDOT(E,DIR) )

            IF ( UMBRAL ) THEN
!
!              If the plane is above the target, increase the
!              rotation angle; otherwise decrease the angle.
!
               ANGLE = ANGLE  +  S * ANGERR

            ELSE
!
!              This is the penumbral case; decreasing the angle
!              "lowers" the plane toward the target.
!
               ANGLE = ANGLE  -  S * ANGERR

            END IF

         END DO

      END DO

      CALL CHKOUT ( 'ZZEDTERM' )
      RETURN
      END
