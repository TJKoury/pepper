!$Procedure NPELPT  ( Nearest point on ellipse to point )

      SUBROUTINE NPELPT ( POINT, ELLIPS, PNEAR, DIST )

! $ Abstract
!
!     Find the nearest point on an ellipse to a specified point, both
!     in three-dimensional space, and find the distance between the
!     ellipse and the point.
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
!     CONIC
!     ELLIPSE
!     GEOMETRY
!     MATH
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               UBEL
      PARAMETER           ( UBEL    =   9 )

      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      POINT  (    3 )
      DOUBLE PRECISION      PNEAR  (    3 )
      DOUBLE PRECISION      DIST

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
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VDIST

      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      MAJLEN
      DOUBLE PRECISION      MINLEN
      DOUBLE PRECISION      ROTATE ( 3, 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      TEMPV  ( 3 )
      DOUBLE PRECISION      TMPPNT ( 3 )
      DOUBLE PRECISION      PRJPNT ( 3 )



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NPELPT' )
      END IF

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
      CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )

!
!     Find the lengths of the semi-axes, and scale the vectors to try
!     to prevent arithmetic unpleasantness.  Degenerate ellipses are
!     turned away at the door.
!
      MINLEN = VNORM (SMINOR)
      MAJLEN = VNORM (SMAJOR)

      IF (   MIN ( MAJLEN, MINLEN )  .EQ.  0.D0  ) THEN

         CALL SETMSG ( 'Semi-axis lengths: # #. ' )
         CALL ERRDP  ( '#', MAJLEN                )
         CALL ERRDP  ( '#', MINLEN                )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'    )
         CALL CHKOUT ( 'NPELPT'                   )
         RETURN

      END IF


      SCALE = 1.D0 / MAJLEN

      CALL VSCLIP ( SCALE, SMAJOR )
      CALL VSCLIP ( SCALE, SMINOR )

!
!     Translate ellipse and point so that the ellipse is centered at
!     the origin.  Scale the point's coordinates to maintain the
!     correct relative position to the scaled ellipse.
!
      CALL VSUB   ( POINT, CENTER, TMPPNT )
      CALL VSCLIP ( SCALE,         TMPPNT )

!
!     We want to reduce the problem to a two-dimensional one.  We'll
!     work in a coordinate system whose x- and y- axes are aligned with
!     the semi-major and semi-minor axes of the input ellipse.  The
!     z-axis is picked to give us a right-handed system.  We find the
!     matrix that transforms coordinates to our new system using TWOVEC.
!
      CALL TWOVEC ( SMAJOR, 1, SMINOR, 2, ROTATE )

!
!     Apply the coordinate transformation to our scaled input point.
!
      CALL MXV  ( ROTATE, TMPPNT, TEMPV  )
      CALL VEQU ( TEMPV,          TMPPNT )

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
      CALL VPACK ( TMPPNT(1),  TMPPNT(2),  0.D0,  PRJPNT )

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
      CALL NEARPT ( PRJPNT, 1.D0, MINLEN/MAJLEN, 2.D0, PNEAR, DIST )

!
!     Scale the near point coordinates back to the original scale.
!
      CALL VSCLIP ( MAJLEN, PNEAR )

!
!     Apply the required inverse rotation and translation to PNEAR.
!     Compute the point-to-ellipse distance.
!
      CALL MTXV ( ROTATE, PNEAR,  TEMPV )
      CALL VADD ( TEMPV,  CENTER, PNEAR )

      DIST = VDIST ( PNEAR, POINT )


      CALL CHKOUT  ( 'NPELPT' )
      RETURN
      END
