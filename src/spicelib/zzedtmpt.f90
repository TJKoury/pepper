!$Procedure ZZEDTMPT ( Ellipsoid terminator point in half-plane )

      SUBROUTINE ZZEDTMPT ( UMBRAL, A, B, C, R, AXIS, PLNVEC, POINT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Compute an umbral or penumbral terminator point on an ellipsoidal
!     target. The point is confined to a specified half-plane. The
!     illumination source is spherical.
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
!     ILLUMINATION
!     TERMINATOR
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               UMBRAL
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      R
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      PLNVEC ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UMBRAL     I   Flag indicating whether terminator is umbral.
!     A          I   Semi-axis length in the X direction.
!     B          I   Semi-axis length in the Y direction.
!     C          I   Semi-axis length in the Z direction.
!     R          I   Radius of illumination source.
!     AXIS       I   Axis vector from target to source.
!     PLNVEC     I   Vector in cutting half-plane.
!     POINT      O   Terminator point.
!
! $ Detailed_Input
!
!     UMBRAL     is a logical flag that must be set by the caller
!                to .TRUE. if an umbral terminator point is to be
!                computed, and set to .FALSE. if a penumbral
!                terminator point is to be computed.
!
!     A,
!     B,
!     C          are, respectively, the semi-axis lengths of a
!                triaxial ellipsoid representing a target object.
!                The axes of the ellipsoid are aligned with the
!                axes of the Cartesian coordinate system.
!
!
!     R          is the radius of the spherical model of the
!                illumination source.
!
!
!     AXIS       is the position of the center of the illumination
!                source relative to the center of the target. AXIS is
!                contained in the line bounding cutting half-planes in
!                which terminator points are found.
!
!
!     PLNVEC     is a vector that, together with AXIS, defines
!                a cutting half-plane. The half-plane contains
!                PLNVEC and has the line containing AXIS as a
!                boundary.
!
!                The terminator point that is sought lies within
!                the half-plane.
!
! $ Detailed_Output
!
!     POINT      is a terminator point lying within the half-plane
!                defined by AXIS and PLNVEC.
!
!                When UMBRAL is set to .TRUE., POINT lies on the
!                umbral terminator: the plane tangent to the target
!                ellipsoid at POINT is also tangent to the illumination
!                source. This plane does not intersect the vector
!                AXIS.
!
!                When UMBRAL is set to .FALSE., POINT lies on the
!                penumbral terminator: the plane tangent to the target
!                ellipsoid at POINT is also tangent to the illumination
!                source. This plane intersects the vector AXIS.
!
!                Unless the target is spherical, the plane tangent
!                to the target at POINT is usually tangent to the
!                illumination source at a point outside the plane
!                containing the cutting half-plane.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If any of the target ellipsoid's semi-axis lengths is
!         non-positive, the error SPICE(INVALIDAXISLENGTH) is signaled.
!
!     2)  If the radius of the illumination source is non-positive, the
!         error SPICE(INVALIDRADIUS) is signaled.
!
!     3)  If AXIS is the zero vector, the error SPICE(ZEROVECTOR) is
!         signaled.
!
!     4)  If the target and illumination source are separated by
!         less than the sum of the radius of the source sphere
!         and the maximum radius of the target ellipsoid, the
!         error SPICE(OBJECTSTOOCLOSE) is signaled.
!
!     5)  If PLNVEC is the zero vector, the error SPICE(ZEROVECTOR) is
!         signaled.
!
!     6)  If AXIS and PLNVEC are linearly dependent, the error
!         SPICE(DEGENERATECASE) is signaled.
!
!     7)  If the terminator solution doesn't converge, the error
!         SPICE(NOCONVERGENCE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The "umbral" terminator on an ellipsoid is the boundary of the
!     region that is in total shadow. At any point P on this boundary,
!     the illumination source is below and tangent to the local
!     horizontal plane at P.
!
!     The "penumbral" terminator on an ellipsoid is the boundary of the
!     region that totally illuminated. At any point P on this boundary,
!     the illumination source is above and tangent to the local
!     horizontal plane at P.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     This is a SPICELIB private routine.
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
!-    SPICELIB Version 2.0.0, 30-JUN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     compute ellipsoid terminator point in half-plane
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!
!
!     Tolerance used for arcsine arguments:
!
      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL = 1.D-14 )

!
!     Angular error used to determine convergence:
!
      DOUBLE PRECISION      CNVLIM
      PARAMETER           ( CNVLIM = 1.D-15 )

!
!     Maximum number of iterations allowed for root finding:
!
      INTEGER               MAXITR
      PARAMETER           ( MAXITR = 20 )

!
!     Local variables
!
      DOUBLE PRECISION      ANGERR
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      D
      DOUBLE PRECISION      H
      DOUBLE PRECISION      HPLNML ( 3 )
      DOUBLE PRECISION      MAXR
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PROJ   ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SGNNML ( 3 )
      DOUBLE PRECISION      SRCPNT ( 3 )
      DOUBLE PRECISION      TA
      DOUBLE PRECISION      TARGPT ( 3 )
      DOUBLE PRECISION      TAXIS  ( 3 )
      DOUBLE PRECISION      TB
      DOUBLE PRECISION      TC
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TMPVEC ( 3 )
      DOUBLE PRECISION      TPLNVC ( 3 )
      DOUBLE PRECISION      TRANS  ( 3, 3 )
      DOUBLE PRECISION      UTAXIS ( 3 )
      DOUBLE PRECISION      XA
      DOUBLE PRECISION      XB
      DOUBLE PRECISION      XC

      INTEGER               NITR


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEDTMPT' )

!
!     Check A, B, C, and R.
!
      IF (      ( A .LE. 0.D0 ) &
           .OR. ( B .LE. 0.D0 ) &
           .OR. ( C .LE. 0.D0 )  ) THEN

         CALL SETMSG ( 'Target radii must be strictly positive ' &
         //            'but were #, #, #.'                      )
         CALL ERRDP  ( '#',  A                                  )
         CALL ERRDP  ( '#',  B                                  )
         CALL ERRDP  ( '#',  C                                  )
         CALL SIGERR ( 'SPICE(INVALIDAXISLENGTH)'               )
         CALL CHKOUT ( 'ZZEDTMPT'                               )
         RETURN

      END IF

      IF ( R .LE. 0.D0 ) THEN

         CALL SETMSG ( 'Source radius must be strictly positive ' &
         //            'but was #.'                             )
         CALL ERRDP  ( '#',  R                                  )
         CALL SIGERR ( 'SPICE(INVALIDRADIUS)'                   )
         CALL CHKOUT ( 'ZZEDTMPT'                               )
         RETURN

      END IF

!
!     Check AXIS and PLNVEC.
!
      IF ( VZERO( AXIS ) ) THEN

         CALL SETMSG ( 'AXIS must be a non-zero vector but is ' &
         //            'in fact zero.'                         )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                     )
         CALL CHKOUT ( 'ZZEDTMPT'                              )
         RETURN

      END IF

      IF (  R + MAX(A,B,C)  .GE.  VNORM(AXIS)  ) THEN

         CALL SETMSG ( 'Centers of source and target are too ' &
         //            'close together; distance is #. Radius ' &
         //            'of source is #; semi-axis lengths are ' &
         //            '#, #, #.'                               )
         CALL ERRDP  ( '#',  VNORM(AXIS)                        )
         CALL ERRDP  ( '#',  R                                  )
         CALL ERRDP  ( '#',  A                                  )
         CALL ERRDP  ( '#',  B                                  )
         CALL ERRDP  ( '#',  C                                  )
         CALL SIGERR ( 'SPICE(OBJECTSTOOCLOSE)'                 )
         CALL CHKOUT ( 'ZZEDTMPT'                               )
         RETURN

      END IF


      IF ( VZERO( PLNVEC ) ) THEN

         CALL SETMSG ( 'PLNVEC must be a non-zero vector but is ' &
         //            'in fact zero.'                           )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                       )
         CALL CHKOUT ( 'ZZEDTMPT'                                )
         RETURN

      END IF

!
!     Transform the source, target, axis, and plane vector
!     so that the target becomes a unit sphere.
!
      CALL CLEARD ( 9, TRANS )

      TA         = 1.D0 / A
      TB         = 1.D0 / B
      TC         = 1.D0 / C

      XA         = TA * R
      XB         = TB * R
      XC         = TC * R

      TRANS(1,1) = TA
      TRANS(2,2) = TB
      TRANS(3,3) = TC

!
!     TNEGAX is the negative of the transformed axis.
!     UTAXIS is the unit vector in the direction of TNEGAX.
!
      CALL MXV    ( TRANS,  PLNVEC, TPLNVC )
      CALL MXV    ( TRANS,  AXIS,   TAXIS )
      CALL VHAT   ( TAXIS,  UTAXIS )

!
!     Let HPLNML be a normal vector to the plane containing
!     the transformed axis and plane vectors.
!
      CALL VCRSS ( TPLNVC, TAXIS, HPLNML )

      IF ( VZERO(HPLNML) ) THEN

         CALL SETMSG ( 'Plane reference vector and axis are ' &
         //            'linearly dependent.'                 )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'               )
         CALL CHKOUT ( 'ZZEDTMPT'                            )
         RETURN

      END IF

!
!     Let MAXR be an outer bounding radius for the transformed
!     source sphere.
!
      MAXR = MAX ( XA, XB, XC )

      D    = VNORM ( TAXIS )

      IF ( UMBRAL ) THEN
!
!        Find the angle between the negative axis and a ray tangent to
!        both the transformed target and the outer bounding sphere of
!        the transformed source. Here a tangent point on the
!        transformed target is the vertex, and the tangent ray is
!        confined to the half-plane normal to HPLNML, containing
!        TPLNVC, and bounded by the line containing TNEGAX. The
!        tangent ray does not cross the line containing TNEGAX.
!
         ANGLE = DASINE ( (MAXR-1.D0) / D,  ATOL  )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZEDTMPT' )
            RETURN
         END IF

!
!        Create the tangent point on the transformed target.
!
         THETA = - ( HALFPI() + ANGLE )

         CALL VROTV ( UTAXIS, HPLNML, THETA, TARGPT )

!
!        S is the sign applied to pi/2 - ANGLE.
!
         S = 1.D0

      ELSE
!
!        This is the penumbral case. The tangent ray crosses
!        the line containing TNEGAX.
!
!
!        The tangent line always slopes downward (toward AXIS)
!        toward the light source.
!
         ANGLE = DASINE ( (MAXR+1.D0) / D,  ATOL  )


         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZEDTMPT' )
            RETURN
         END IF

!
!        Create the tangent point on the transformed target.
!
         THETA = ANGLE - HALFPI()

         CALL VROTV ( UTAXIS, HPLNML, THETA, TARGPT )

         S = -1.D0

      END IF

!
!     The tangent point is also a normal direction for the plane
!     tangent to both objects. Get the corresponding unit normal and
!     the plane constant.
!
      CALL VHAT ( TARGPT, NORMAL )

      CONST = VDOT( NORMAL, TARGPT )
!
!     Find the height of the plane relative to the transformed source.
!     We'll find the unique point on the transformed source where the
!     outward normal is parallel to NORMAL and find the height of this
!     point relative to the plane.
!
!     Let SGNNML be the "signed" normal which is parallel to NORMAL
!     in the umbral case and anti-parallel otherwise.
!
      CALL VSCL   ( S,  NORMAL, SGNNML )

      CALL EDNMPT ( XA, XB, XC, SGNNML, SRCPNT )

!
!     Express the source point as an offset from the transformed
!     target center.
!
      CALL VADD ( SRCPNT, TAXIS, TMPVEC )
      CALL VEQU ( TMPVEC,        SRCPNT )

!
!     H is the height of the surface point on the source, relative
!     to the plane tangent to the target at TARGPT. ANGERR is the
!     corresponding angular error estimate: an estimate of the
!     amount by which TARGPT needs to be rotated in the positive
!     sense about HPLNML to make the plane contain SRCPNT.
!
      H      = VDOT( SRCPNT, NORMAL ) - CONST

      ANGERR = TOUCHD( -H / D )

      NITR   = 0

!
!     The loop terminates when the angular error magnitude
!     stops decreasing. If the iteration count exceeds the
!     limit, an error will be signaled.
!
      DO WHILE (       ( ABS(ANGERR) .GT. CNVLIM ) &
                 .AND. ( NITR        .LE. MAXITR )  )
!
!        Rotate the target point about HPLNML in the positive sense
!        by the angular error. This should make the tangent plane
!        closer to the source point.
!
         CALL VROTV ( TARGPT, HPLNML, ANGERR, TMPVEC )
         CALL VEQU  ( TMPVEC,                 TARGPT )
         CALL VHAT  ( TARGPT, NORMAL )

!
!        Re-compute the normal and constant of the tangent plane.
!
         CONST = VDOT( NORMAL, TARGPT )

         CALL VSCL   ( S,  NORMAL, SGNNML )

!
!        Find the near point on the source to the tangent plane.
!
         CALL EDNMPT ( XA, XB, XC, SGNNML, SRCPNT )

         CALL VADD ( SRCPNT, TAXIS, TMPVEC )
         CALL VEQU ( TMPVEC,        SRCPNT )

!
!        Re-compute the height error and angular error.
!
         H      = VDOT  ( SRCPNT, NORMAL ) - CONST

         CALL VPERP ( SRCPNT, HPLNML, PROJ )

         D      = VDIST ( PROJ, TARGPT )
         ANGERR = TOUCHD( -H / D )

         NITR   = NITR + 1

         IF ( NITR .GT. MAXITR ) THEN

            CALL SETMSG ( 'Tangent finding loop failed to ' &
            //            'converge. Iteration count = #.' )
            CALL ERRINT ( '#', NITR                        )
            CALL SIGERR ( 'SPICE(NOCONVERGENCE)'           )
            CALL CHKOUT ( 'ZZEDTMPT'                       )
            RETURN

         END IF

      END DO

!
!     Apply the inverse distortion transformation to TARGPT in order to
!     obtain the tangent point on the original, ellipsoidal target.
!
      POINT(1) = A * TARGPT(1)
      POINT(2) = B * TARGPT(2)
      POINT(3) = C * TARGPT(3)

      CALL CHKOUT ( 'ZZEDTMPT' )
      RETURN
      END
