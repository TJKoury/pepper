!$Procedure ZZEDTERM ( Ellipsoid terminator )

      subroutine zzedterm ( type,   a,      b,     c, &
                            srcrad, srcpos, npts,  trmpts )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute a set of points on the umbral or penumbral terminator of
!     a specified ellipsoid, given a spherical light source.
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

      implicit none

      character*(*)         type
      double precision      a
      double precision      b
      double precision      c
      double precision      srcrad
      double precision      srcpos ( 3 )
      integer               npts
      double precision      trmpts ( 3, npts )

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
      double precision      halfpi
      double precision      touchd
      double precision      twopi
      double precision      vdot
      double precision      vnorm
      double precision      vsep

      logical               return

!
!     Local parameters
!
      integer               ubel
      parameter           ( ubel   = 9 )

      integer               ubpl
      parameter           ( ubpl   = 4 )

      integer               maxitr
      parameter           ( maxitr = 10 )

      integer               typlen
      parameter           ( typlen = 50 )

!
!     Local variables
!
      character*(typlen)    loctyp

      double precision      angerr
      double precision      angle
      double precision      d
      double precision      delta
      double precision      dir    ( 3 )
      double precision      e      ( 3 )
      double precision      inang
      double precision      lambda
      double precision      maxrad
      double precision      minrad
      double precision      offset ( 3 )
      double precision      outang
      double precision      plane  ( ubpl )
      double precision      plcons
      double precision      prvang
      double precision      prvdif
      double precision      rmax
      double precision      rmin
      double precision      s
      double precision      srcpt  ( 3 )
      double precision      theta
      double precision      trans  ( 3, 3 )
      double precision      v      ( 3 )
      double precision      vtemp  ( 3 )
      double precision      vtx    ( 3 )
      double precision      x      ( 3 )
      double precision      y      ( 3 )
      double precision      z      ( 3 )

      integer               i
      integer               nitr

      logical               umbral


!
!     Standard SPICELIB error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'ZZEDTERM' )

!
!     Check the terminator type.
!
      call ljust ( type,   loctyp )
      call ucase ( loctyp, loctyp )

      if (  loctyp .eq. 'UMBRAL' ) then

         umbral = .true.

      else if ( loctyp .eq. 'PENUMBRAL' ) then

         umbral = .false.

      else

         call setmsg ( 'Terminator type must be UMBRAL or ' &
         //            'PENUMBRAL but was actually #.'     )
         call errch  ( '#',  type                          )
         call sigerr ( 'SPICE(NOTSUPPORTED)'               )
         call chkout ( 'ZZEDTERM'                          )
         return

      end if

!
!     Check the terminator set dimension.
!
      if ( npts .lt. 1 ) then

         call setmsg ( 'Set must contain at least one point; NPTS ' &
         //            ' = #.'                                        )
         call errint ( '#',  npts                                     )
         call sigerr ( 'SPICE(INVALIDSIZE)'                           )
         call chkout ( 'ZZEDTERM'                                     )
         return

      end if

!
!     The ellipsoid semi-axes must have positive length.
!
      if (       ( a .le. 0.d0 ) &
           .or.  ( b .le. 0.d0 ) &
           .or.  ( c .le. 0.d0 )   )   then

         call setmsg ( 'Semi-axis lengths:  A = #, B = #, C = #. ' )
         call errdp  ( '#', a                                      )
         call errdp  ( '#', b                                      )
         call errdp  ( '#', c                                      )
         call sigerr ( 'SPICE(INVALIDAXISLENGTH)'                  )
         call chkout ( 'ZZEDTERM'                                  )
         return

      end if

!
!     Check the input light source radius.
!
      if ( srcrad .le. 0.d0 ) then

         call setmsg ( 'Light source must have positive radius; ' &
         //            'actual radius was #.'                    )
         call errdp  ( '#', srcrad                               )
         call sigerr ( 'SPICE(INVALIDRADIUS)'                    )
         call chkout ( 'ZZEDTERM'                                )
         return

      end if

!
!     The light source must not intersect the outer bounding
!     sphere of the ellipsoid.
!
      d    =  vnorm ( srcpos )

      rmax =  max ( a, b, c )
      rmin =  min ( a, b, c )

      if (  ( srcrad + rmax )  .ge.  d  ) then
!
!        The light source is too close.
!
         call setmsg ( 'Light source intersects outer bounding ' &
         //            'sphere of the ellipsoid.  Light source ' &
         //            'radius = #; ellipsoid''s longest axis = ' &
         //            '#; sum = #; distance between centers = #.' )
         call errdp  ( '#', srcrad                                 )
         call errdp  ( '#', rmax                                   )
         call errdp  ( '#', srcrad + rmax                          )
         call errdp  ( '#', d                                      )
         call sigerr ( 'SPICE(OBJECTSTOOCLOSE)'                    )
         call chkout ( 'ZZEDTERM'                                  )
         return

      end if

!
!     Let the negative of the ellipsoid-light source vector be the
!     Z-axis of a frame we'll use to generate the terminator set.
!
      call vminus ( srcpos, z )

      call frame  ( z,  x,  y )

!
!     Create the rotation matrix required to convert vectors
!     from the source-centered frame back to the target body-fixed
!     frame.
!
      call vequ ( x, trans(1,1) )
      call vequ ( y, trans(1,2) )
      call vequ ( z, trans(1,3) )

!
!     Find the maximum and minimum target radii.
!
      maxrad = max ( a, b, c )
      minrad = min ( a, b, c )

      if ( umbral ) then
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
         outang = asin ( (srcrad - maxrad) / d )
         inang  = asin ( (srcrad - minrad) / d )

      else
!
!        Compute the angular offsets from the axis of rays tangent to
!        both the source and the bounding spheres of the target, where
!        the tangency points lie in opposite half-planes bounded by the
!        axis (compare the case above).
!
!        OUTANG corresponds to the target's outer bounding sphere;
!        INANG to the inner bounding sphere.
!
         outang = asin (  ( srcrad + maxrad )  /  d  )
         inang  = asin (  ( srcrad + minrad )  /  d  )

      end if

!
!     Compute the angular delta we'll use for generating
!     terminator points.
!
      delta = twopi() / npts

!
!     Generate the terminator points.
!
      do i = 1, npts

         theta = (i-1)*delta

!
!        Let SRCPT be the surface point on the source lying in
!        the X-Y plane of the frame produced by FRAME
!        and corresponding to the angle THETA.
!
         call latrec ( srcrad,  theta,  0.d0,  srcpt )

!
!        Now solve for the angle by which SRCPT must be rotated (toward
!        +Z in the umbral case, away from +Z in the penumbral case)
!        so that a plane tangent to the source at SRCPT is also tangent
!        to the target. The rotation is bracketed by OUTANG on the low
!        side and INANG on the high side in the umbral case; the
!        bracketing values are reversed in the penumbral case.
!
         if ( umbral ) then
            angle  = outang
         else
            angle  = inang
         end if

         prvdif = twopi()
         prvang = angle + halfpi()
         nitr   = 0

         do while (       ( nitr                        .le. maxitr ) &
                    .and. ( touchd( abs(angle-prvang) ) .lt. prvdif )  )

            nitr    =   nitr  + 1
            prvdif  =   touchd ( abs(angle-prvang) )
            prvang  =   angle
!
!           Find the closest point on the ellipsoid to the plane
!           corresponding to "ANGLE".
!
!           The tangent point on the source is obtained by rotating
!           SRCPT by ANGLE towards +Z.  The plane's normal vector is
!           parallel to VTX in the source-centered frame.
!
            call latrec ( srcrad, theta, angle, vtx )

            call vequ   ( vtx, dir )

!
!           VTX and DIR are expressed in the source-centered frame.  We
!           must translate VTX to the target frame and rotate both
!           vectors into that frame.
!
            call mxv  ( trans,  vtx,   vtemp )
            call vadd ( srcpos, vtemp, vtx   )

            call mxv  ( trans,  dir,   vtemp )
            call vequ ( vtemp,         dir   )

!
!           Create the plane defined by VTX and DIR.
!
            call nvp2pl ( dir, vtx, plane )
!
!           Find the closest point on the ellipsoid to the plane. At
!           the point we seek, the outward normal on the ellipsoid is
!           parallel to the choice of plane normal that points away
!           from the origin.  We can always obtain this choice from
!           PL2NVC.
!
            call pl2nvc ( plane, dir, plcons )

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

            call vpack ( a*dir(1), b*dir(2), c*dir(3), v )

            lambda = 1.d0 / vnorm(v)

            call vpack ( a*v(1), b*v(2), c*v(3), e )

            call vscl  ( lambda, e, trmpts(1,i) )

!
!           Make a new estimate of the plane rotation required to touch
!           the target.
!
            call vsub ( trmpts(1,i), vtx, offset )

!
!           Let ANGERR be an estimate of the magnitude of angular error
!           between the plane and the terminator.
!
            angerr = vsep ( dir, offset ) - halfpi()

!
!           Let S indicate the sign of the altitude error:  where
!           S is positive, the plane is above E.
!
            s = sign ( 1.d0, vdot(e,dir) )

            if ( umbral ) then
!
!              If the plane is above the target, increase the
!              rotation angle; otherwise decrease the angle.
!
               angle = angle  +  s * angerr

            else
!
!              This is the penumbral case; decreasing the angle
!              "lowers" the plane toward the target.
!
               angle = angle  -  s * angerr

            end if

         end do

      end do

      call chkout ( 'ZZEDTERM' )
      return
      end
