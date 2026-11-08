!$Procedure NPELPT  ( Nearest point on ellipse to point )

      subroutine npelpt ( point, ellips, pnear, dist )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Find the nearest point on an ellipse to a specified point, both
!     in three-dimensional space, and find the distance between the
!     ellipse and the point.
!
! $ Required_Reading
!
!     ELLIPSES
!
! $ Keywords
!
!     CONIC
!     ELLIPSE
!     GEOMETRY
!     MATH
!
! $ Declarations

      implicit none

      integer(ip), parameter :: ubel = 9

      real(wp)              ellips ( ubel )
      real(wp)              point  (    3 )
      real(wp)              pnear  (    3 )
      real(wp)              dist

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     POINT      I   Point whose distance to an ellipse is to be found.
!     ELLIPS     I   A SPICE ellipse.
!     PNEAR      O   Nearest point on ellipse to input point.
!     DIST       O   Distance of input point to ellipse.
!
! $ Detailed_Input
!
!     POINT    is a point in 3-dimensional space.
!
!     ELLIPS   is a SPICE ellipse that represents an ellipse
!              in three-dimensional space.
!
! $ Detailed_Output
!
!     PNEAR    is the nearest point on ELLIPS to POINT.
!
!     DIST     is the distance between POINT and PNEAR. This is
!              the distance between POINT and the ellipse.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input ellipse ELLIPS has one or both semi-axis lengths
!         equal to zero, the error SPICE(DEGENERATECASE) is signaled.
!
!     2)  If the geometric ellipse represented by ELLIPS does not
!         have a unique point nearest to the input point, any point
!         at which the minimum distance is attained may be returned
!         in PNEAR.
!
!     3)  If a ratio of non-zero ellipse radii violates the constraints
!         imposed by NEARPT, an error is signaled by a routine in the
!         call tree of this routine.
!
!     4)  The routine does not check for overflow when scaling or
!         translating the input point.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Given an ellipse and a point in 3-dimensional space, if the
!     orthogonal projection of the point onto the plane of the ellipse
!     is on or outside of the ellipse, then there is a unique point on
!     the ellipse closest to the original point. This routine finds
!     that nearest point on the ellipse. If the projection falls inside
!     the ellipse, there may be multiple points on the ellipse that are
!     at the minimum distance from the original point. In this case,
!     one such closest point will be returned.
!
!     This routine returns a distance, rather than an altitude, in
!     contrast to the SPICELIB routine NEARPT. Because our ellipse is
!     situated in 3-space and not 2-space, the input point is not
!     "inside" or "outside" the ellipse, so the notion of altitude does
!     not apply to the problem solved by this routine. In the case of
!     NEARPT, the input point is on, inside, or outside the ellipsoid,
!     so it makes sense to speak of its altitude.
!
! $ Examples
!
!     1)  For planetary rings that can be modeled as flat disks with
!         elliptical outer boundaries, the distance of a point in
!         space from a ring's outer boundary can be computed using this
!         routine. Suppose CENTER, SMAJOR, and SMINOR are the center,
!         semi-major axis, and semi-minor axis of the ring's boundary.
!         Suppose also that SCPOS is the position of a spacecraft.
!         SCPOS, CENTER, SMAJOR, and SMINOR must all be expressed in
!         the same coordinate system. We can find the distance from
!         the spacecraft to the ring using the code fragment
!
!            C
!            C     Make a SPICE ellipse representing the ring,
!            C     then use NPELPT to find the distance between
!            C     the spacecraft position and RING.
!            C
!                  CALL CGV2EL ( CENTER, SMAJOR, SMINOR, RING )
!                  CALL NPELPT ( SCPOS,  RING,   PNEAR,  DIST )
!
!
!     2)  The problem of finding the distance of a line from a tri-axial
!         ellipsoid can be reduced to the problem of finding the
!         distance between the same line and an ellipse; this problem in
!         turn can be reduced to the problem of finding the distance
!         between an ellipse and a point. The routine NPEDLN carries
!         out this process and uses NPELPT to find the ellipse-to-point
!         distance.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.3.0, 24-AUG-2021 (JDR) (NJB)
!
!        Added IMPLICT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Added entries #3 and #4 to $Exceptions section.
!
!-    SPICELIB Version 1.2.0, 02-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VADD, VSCL, MTXV and MXV calls.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     nearest point on ellipse to point
!
!-&


!
!     SPICELIB functions
!
      real(wp)              vnorm
      real(wp)              vdist

      logical               return

!
!     Local variables
!
      real(wp)              center ( 3 )
      real(wp)              majlen
      real(wp)              minlen
      real(wp)              rotate ( 3, 3 )
      real(wp)              scale
      real(wp)              smajor ( 3 )
      real(wp)              sminor ( 3 )
      real(wp)              tempv  ( 3 )
      real(wp)              tmppnt ( 3 )
      real(wp)              prjpnt ( 3 )



!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Here's an overview of our solution:
!
!        Let ELPL be the plane containing the ELLIPS, and let PRJ be
!        the orthogonal projection of the POINT onto ELPL.  Let X be
!        any point in the plane ELPL.  According to the Pythagorean
!        Theorem,
!
!                           2                       2                  2
!           || POINT - X ||    =   || POINT - PRJ ||   +  || PRJ - X ||.
!
!        Then if we can find a point X on ELLIPS that minimizes the
!        rightmost term, that point X is the closest point on the
!        ellipse to POINT.
!
!        So, we find the projection PRJ, and then solve the problem of
!        finding the closest point on ELLIPS to PRJ.  To solve this
!        problem, we find a triaxial ellipsoid whose intersection with
!        the plane ELPL is precisely ELLIPS, and two of whose axes lie
!        in the plane ELPL.  The closest point on ELLIPS to PRJ is also
!        the closest point on the ellipsoid to ELLIPS.  But we have the
!        SPICELIB routine NEARPT on hand to find the closest point on an
!        ellipsoid to a specified point, so we've reduced our problem to
!        a solved problem.
!
!        There is a subtle point to worry about here:  if PRJ is outside
!        of ELLIPS (PRJ is in the same plane as ELLIPS, so `outside'
!        does make sense here), then the closest point on ELLIPS to PRJ
!        coincides with the closest point on the ellipsoid to PRJ,
!        regardless of how we choose the z-semi-axis length of the
!        ellipsoid.  But the correspondence may be lost if PRJ is inside
!        the ellipse, if we don't choose the z-semi-axis length
!        correctly.
!
!        Though it takes some thought to verify this (and we won't prove
!        it here), making the z-semi-axis of the ellipsoid longer than
!        the other two semi-axes is sufficient to maintain the
!        coincidence of the closest point on the ellipsoid to PRJPNT and
!        the closest point on the ellipse to PRJPNT.
!

!
!     Find the ellipse's center and semi-axes.
!
      call el2cgv ( ellips, center, smajor, sminor )

!
!     Find the lengths of the semi-axes, and scale the vectors to try
!     to prevent arithmetic unpleasantness.  Degenerate ellipses are
!     turned away at the door.
!
      minlen = vnorm (sminor)
      majlen = vnorm (smajor)

      if (   min ( majlen, minlen )  ==  0.0_wp  ) then

         call setmsg ( 'Semi-axis lengths: # #. ' )
         call errdp  ( '#', majlen                )
         call errdp  ( '#', minlen                )
         call sigerr ( 'SPICE(DEGENERATECASE)'    )
         return

      end if


      scale = 1.0_wp / majlen

      call vsclip ( scale, smajor )
      call vsclip ( scale, sminor )

!
!     Translate ellipse and point so that the ellipse is centered at
!     the origin.  Scale the point's coordinates to maintain the
!     correct relative position to the scaled ellipse.
!
      call vsub   ( point, center, tmppnt )
      call vsclip ( scale,         tmppnt )

!
!     We want to reduce the problem to a two-dimensional one.  We'll
!     work in a coordinate system whose x- and y- axes are aligned with
!     the semi-major and semi-minor axes of the input ellipse.  The
!     z-axis is picked to give us a right-handed system.  We find the
!     matrix that transforms coordinates to our new system using TWOVEC.
!
      call twovec ( smajor, 1, sminor, 2, rotate )

!
!     Apply the coordinate transformation to our scaled input point.
!
      call mxv  ( rotate, tmppnt, tempv  )
      call vequ ( tempv,          tmppnt )

!
!     We must find the distance between the orthogonal projection of
!     TMPPNT onto the x-y plane and the ellipse.  The projection is
!     just
!
!        ( TMPPNT(1), TMPPNT(2), 0 );
!
!     we'll call this projection PRJPNT.
!
!
      call vpack ( tmppnt(1),  tmppnt(2),  0.0_wp,  prjpnt )

!
!     Now we're ready to find the distance between and a triaxial
!     ellipsoid whose intersection with the x-y plane is the ellipse
!     and whose third semi-axis lies on the z-axis.
!
!     Because we've scaled the ellipse's axes so as to give the longer
!     axis length 1, a length of 2.D0 suffices for the ellipsoid's
!     z-semi-axis.
!

!
!     Find the nearest point to PRJPNT on the ellipsoid, PNEAR.
!
      call nearpt ( prjpnt, 1.0_wp, minlen/majlen, 2.0_wp, pnear, dist )

!
!     Scale the near point coordinates back to the original scale.
!
      call vsclip ( majlen, pnear )

!
!     Apply the required inverse rotation and translation to PNEAR.
!     Compute the point-to-ellipse distance.
!
      call mtxv ( rotate, pnear,  tempv )
      call vadd ( tempv,  center, pnear )

      dist = vdist ( pnear, point )


      return
      end
