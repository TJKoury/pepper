!$Procedure ZZEDTMPT ( Ellipsoid terminator point in half-plane )

      subroutine zzedtmpt ( umbral, a, b, c, r, axis, plnvec, point )
      use pepper_kinds, only: ip, wp

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

      implicit none

      logical               umbral
      real(wp)              a
      real(wp)              b
      real(wp)              c
      real(wp)              r
      real(wp)              axis   ( 3 )
      real(wp)              plnvec ( 3 )
      real(wp)              point  ( 3 )

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
      real(wp)              dasine
      real(wp)              halfpi
      real(wp)              touchd
      real(wp)              vdot
      real(wp)              vdist
      real(wp)              vnorm

      logical               failed
      logical               return
      logical               vzero

!
!     Local parameters
!
!
!     Tolerance used for arcsine arguments:
!
      real(wp)              atol
      parameter           ( atol = 1.0e-14_wp )

!
!     Angular error used to determine convergence:
!
      real(wp)              cnvlim
      parameter           ( cnvlim = 1.0e-15_wp )

!
!     Maximum number of iterations allowed for root finding:
!
      integer(ip)           maxitr
      parameter           ( maxitr = 20 )

!
!     Local variables
!
      real(wp)              angerr
      real(wp)              angle
      real(wp)              const
      real(wp)              d
      real(wp)              h
      real(wp)              hplnml ( 3 )
      real(wp)              maxr
      real(wp)              normal ( 3 )
      real(wp)              proj   ( 3 )
      real(wp)              s
      real(wp)              sgnnml ( 3 )
      real(wp)              srcpnt ( 3 )
      real(wp)              ta
      real(wp)              targpt ( 3 )
      real(wp)              taxis  ( 3 )
      real(wp)              tb
      real(wp)              tc
      real(wp)              theta
      real(wp)              tmpvec ( 3 )
      real(wp)              tplnvc ( 3 )
      real(wp)              trans  ( 3, 3 )
      real(wp)              utaxis ( 3 )
      real(wp)              xa
      real(wp)              xb
      real(wp)              xc

      integer(ip)           nitr


      if ( return() ) then
         return
      end if

      call chkin ( 'ZZEDTMPT' )

!
!     Check A, B, C, and R.
!
      if (      ( a .le. 0.0_wp ) &
           .or. ( b .le. 0.0_wp ) &
           .or. ( c .le. 0.0_wp )  ) then

         call setmsg ( 'Target radii must be strictly positive ' &
         //            'but were #, #, #.'                      )
         call errdp  ( '#',  a                                  )
         call errdp  ( '#',  b                                  )
         call errdp  ( '#',  c                                  )
         call sigerr ( 'SPICE(INVALIDAXISLENGTH)'               )
         call chkout ( 'ZZEDTMPT'                               )
         return

      end if

      if ( r .le. 0.0_wp ) then

         call setmsg ( 'Source radius must be strictly positive ' &
         //            'but was #.'                             )
         call errdp  ( '#',  r                                  )
         call sigerr ( 'SPICE(INVALIDRADIUS)'                   )
         call chkout ( 'ZZEDTMPT'                               )
         return

      end if

!
!     Check AXIS and PLNVEC.
!
      if ( vzero( axis ) ) then

         call setmsg ( 'AXIS must be a non-zero vector but is ' &
         //            'in fact zero.'                         )
         call sigerr ( 'SPICE(ZEROVECTOR)'                     )
         call chkout ( 'ZZEDTMPT'                              )
         return

      end if

      if (  r + max(a,b,c)  .ge.  vnorm(axis)  ) then

         call setmsg ( 'Centers of source and target are too ' &
         //            'close together; distance is #. Radius ' &
         //            'of source is #; semi-axis lengths are ' &
         //            '#, #, #.'                               )
         call errdp  ( '#',  vnorm(axis)                        )
         call errdp  ( '#',  r                                  )
         call errdp  ( '#',  a                                  )
         call errdp  ( '#',  b                                  )
         call errdp  ( '#',  c                                  )
         call sigerr ( 'SPICE(OBJECTSTOOCLOSE)'                 )
         call chkout ( 'ZZEDTMPT'                               )
         return

      end if


      if ( vzero( plnvec ) ) then

         call setmsg ( 'PLNVEC must be a non-zero vector but is ' &
         //            'in fact zero.'                           )
         call sigerr ( 'SPICE(ZEROVECTOR)'                       )
         call chkout ( 'ZZEDTMPT'                                )
         return

      end if

!
!     Transform the source, target, axis, and plane vector
!     so that the target becomes a unit sphere.
!
      call cleard ( 9, trans )

      ta         = 1.0_wp / a
      tb         = 1.0_wp / b
      tc         = 1.0_wp / c

      xa         = ta * r
      xb         = tb * r
      xc         = tc * r

      trans(1,1) = ta
      trans(2,2) = tb
      trans(3,3) = tc

!
!     TNEGAX is the negative of the transformed axis.
!     UTAXIS is the unit vector in the direction of TNEGAX.
!
      call mxv    ( trans,  plnvec, tplnvc )
      call mxv    ( trans,  axis,   taxis )
      call vhat   ( taxis,  utaxis )

!
!     Let HPLNML be a normal vector to the plane containing
!     the transformed axis and plane vectors.
!
      call vcrss ( tplnvc, taxis, hplnml )

      if ( vzero(hplnml) ) then

         call setmsg ( 'Plane reference vector and axis are ' &
         //            'linearly dependent.'                 )
         call sigerr ( 'SPICE(DEGENERATECASE)'               )
         call chkout ( 'ZZEDTMPT'                            )
         return

      end if

!
!     Let MAXR be an outer bounding radius for the transformed
!     source sphere.
!
      maxr = max ( xa, xb, xc )

      d    = vnorm ( taxis )

      if ( umbral ) then
!
!        Find the angle between the negative axis and a ray tangent to
!        both the transformed target and the outer bounding sphere of
!        the transformed source. Here a tangent point on the
!        transformed target is the vertex, and the tangent ray is
!        confined to the half-plane normal to HPLNML, containing
!        TPLNVC, and bounded by the line containing TNEGAX. The
!        tangent ray does not cross the line containing TNEGAX.
!
         angle = dasine ( (maxr-1.0_wp) / d,  atol  )

         if ( failed() ) then
            call chkout ( 'ZZEDTMPT' )
            return
         end if

!
!        Create the tangent point on the transformed target.
!
         theta = - ( halfpi() + angle )

         call vrotv ( utaxis, hplnml, theta, targpt )

!
!        S is the sign applied to pi/2 - ANGLE.
!
         s = 1.0_wp

      else
!
!        This is the penumbral case. The tangent ray crosses
!        the line containing TNEGAX.
!
!
!        The tangent line always slopes downward (toward AXIS)
!        toward the light source.
!
         angle = dasine ( (maxr+1.0_wp) / d,  atol  )


         if ( failed() ) then
            call chkout ( 'ZZEDTMPT' )
            return
         end if

!
!        Create the tangent point on the transformed target.
!
         theta = angle - halfpi()

         call vrotv ( utaxis, hplnml, theta, targpt )

         s = -1.0_wp

      end if

!
!     The tangent point is also a normal direction for the plane
!     tangent to both objects. Get the corresponding unit normal and
!     the plane constant.
!
      call vhat ( targpt, normal )

      const = vdot( normal, targpt )
!
!     Find the height of the plane relative to the transformed source.
!     We'll find the unique point on the transformed source where the
!     outward normal is parallel to NORMAL and find the height of this
!     point relative to the plane.
!
!     Let SGNNML be the "signed" normal which is parallel to NORMAL
!     in the umbral case and anti-parallel otherwise.
!
      call vscl   ( s,  normal, sgnnml )

      call ednmpt ( xa, xb, xc, sgnnml, srcpnt )

!
!     Express the source point as an offset from the transformed
!     target center.
!
      call vadd ( srcpnt, taxis, tmpvec )
      call vequ ( tmpvec,        srcpnt )

!
!     H is the height of the surface point on the source, relative
!     to the plane tangent to the target at TARGPT. ANGERR is the
!     corresponding angular error estimate: an estimate of the
!     amount by which TARGPT needs to be rotated in the positive
!     sense about HPLNML to make the plane contain SRCPNT.
!
      h      = vdot( srcpnt, normal ) - const

      angerr = touchd( -h / d )

      nitr   = 0

!
!     The loop terminates when the angular error magnitude
!     stops decreasing. If the iteration count exceeds the
!     limit, an error will be signaled.
!
      do while (       ( abs(angerr) .gt. cnvlim ) &
                 .and. ( nitr        .le. maxitr )  )
!
!        Rotate the target point about HPLNML in the positive sense
!        by the angular error. This should make the tangent plane
!        closer to the source point.
!
         call vrotv ( targpt, hplnml, angerr, tmpvec )
         call vequ  ( tmpvec,                 targpt )
         call vhat  ( targpt, normal )

!
!        Re-compute the normal and constant of the tangent plane.
!
         const = vdot( normal, targpt )

         call vscl   ( s,  normal, sgnnml )

!
!        Find the near point on the source to the tangent plane.
!
         call ednmpt ( xa, xb, xc, sgnnml, srcpnt )

         call vadd ( srcpnt, taxis, tmpvec )
         call vequ ( tmpvec,        srcpnt )

!
!        Re-compute the height error and angular error.
!
         h      = vdot  ( srcpnt, normal ) - const

         call vperp ( srcpnt, hplnml, proj )

         d      = vdist ( proj, targpt )
         angerr = touchd( -h / d )

         nitr   = nitr + 1

         if ( nitr .gt. maxitr ) then

            call setmsg ( 'Tangent finding loop failed to ' &
            //            'converge. Iteration count = #.' )
            call errint ( '#', nitr                        )
            call sigerr ( 'SPICE(NOCONVERGENCE)'           )
            call chkout ( 'ZZEDTMPT'                       )
            return

         end if

      end do

!
!     Apply the inverse distortion transformation to TARGPT in order to
!     obtain the tangent point on the original, ellipsoidal target.
!
      point(1) = a * targpt(1)
      point(2) = b * targpt(2)
      point(3) = c * targpt(3)

      call chkout ( 'ZZEDTMPT' )
      return
      end
