!$Procedure      F_ZZOCCED3 ( Test ellipsoid occultation routine )

      SUBROUTINE F_ZZOCCED3 ( OK )
      IMPLICIT NONE

      LOGICAL               OK

      INCLUDE 'zzocced.inc'

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        ZZOCCED
!
!     by searching for occultation state transitions and comparing
!     them to those found using the alternate occultation classification
!     routine
!
!        T_OCCED
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
! $ Version
!
!-    TSPICE Version 1.0.0, 20-NOV-2006 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      INTEGER               ZZOCCED

!
!     Other functions
!
      DOUBLE PRECISION      T_RANDD

      INTEGER               T_OCCED

!
!     Local parameters
!
      DOUBLE PRECISION      ANGTOL
      PARAMETER           ( ANGTOL = 1.D-12 )

      DOUBLE PRECISION      CNVTOL
      PARAMETER           ( CNVTOL = 1.D-14 )

      DOUBLE PRECISION      ADJTOL
      PARAMETER           ( ADJTOL = 1.D-14 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      INTEGER               MAXITR
      PARAMETER           ( MAXITR = 2048 )

      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

!
!     Recommendation:  increase the parameters NCASE1 and NCASE2
!     to 1000 for robust testing.
!
      INTEGER               NCASE1
      PARAMETER           ( NCASE1 = 200 )

      INTEGER               NCASE2
      PARAMETER           ( NCASE2 = 200 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      BASIS  ( 3, 3, 2 )
      DOUBLE PRECISION      BETA
      DOUBLE PRECISION      BIGSEP
      DOUBLE PRECISION      CENTR1 ( 3 )
      DOUBLE PRECISION      CENTR2 ( 3 )
      DOUBLE PRECISION      CTRSEP
      DOUBLE PRECISION      CHOP   ( UBPL )
      DOUBLE PRECISION      DIFF
      DOUBLE PRECISION      DIST   ( 2 )
      DOUBLE PRECISION      GAMMA
      DOUBLE PRECISION      HIGH
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LIMIT
      DOUBLE PRECISION      LMBCTR ( 3 )
      DOUBLE PRECISION      LMBMAJ ( 3 )
      DOUBLE PRECISION      LMBMIN ( 3 )
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LOW
      DOUBLE PRECISION      LVIEW  ( 3 )
      DOUBLE PRECISION      MAXANG ( 2 )
      DOUBLE PRECISION      MAXRAD ( 2 )
      DOUBLE PRECISION      MIDPT
      DOUBLE PRECISION      MINANG ( 2 )
      DOUBLE PRECISION      MINRAD ( 3 )
      DOUBLE PRECISION      NEWCTR ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      R      ( 3, 2 )
      DOUBLE PRECISION      RADSUM
      DOUBLE PRECISION      RMAT   ( 3, 3 )
      DOUBLE PRECISION      RSIGN
      DOUBLE PRECISION      RVEC   ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SEMAX1 ( 3, 3 )
      DOUBLE PRECISION      SEMAX2 ( 3, 3 )
      DOUBLE PRECISION      SRFDIR ( 3 )
      DOUBLE PRECISION      TSTVEC ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      UVPOFF ( 3 )
      DOUBLE PRECISION      V1     ( 3 )
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      V3     ( 3 )
      DOUBLE PRECISION      VIEWPT ( 3 )
      DOUBLE PRECISION      VPCTR1 ( 3 )
      DOUBLE PRECISION      VPCTR2 ( 3 )
      DOUBLE PRECISION      VPOFF  ( 3 )
      DOUBLE PRECISION      VPOFF2 ( 3 )
      DOUBLE PRECISION      VPXPT1 ( 3 )
      DOUBLE PRECISION      XLIMB  ( UBEL )
      DOUBLE PRECISION      XPT1   ( 3 )
      DOUBLE PRECISION      XPT2   ( 3 )
      DOUBLE PRECISION      XSEP
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               CODE
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NITR
      INTEGER               NXPTS
      INTEGER               SEED

      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE                  ORIGIN

!
!     Initial values
!
      DATA                  ORIGIN /  3 * 0.D0 /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZOCCED3' )


!
!     The initial set of cases tests the accuracy with which
!     ZZOCCED can detect transitions between total occultation
!     and partial occultation.
!
!     In the discussion below, we use the terms "target" and
!     "ellipsoid" interchangeably
!
!     These cases attempt to cover a broad set of geometric cases.
!     In order to do this efficiently, random numbers are used
!     to generate most inputs:
!
!        - A random scale factor is applied uniformly to all
!          participating objects.
!
!        - The principal axis matrices of both targets are
!          generated from random Euler angles.
!
!        - The radii of each target are chosen randomly, with
!          each radius in the range of 1:100 prior to scaling.
!
!        - The location of the observer's sub-point on the first
!          target is selected using a direction vector with random
!          components.  The ray emanating from the center of the
!          first target and parallel to this direction vector
!          defines the sub-point.
!
!        - The altitude of the observer above its sub-point on the
!          first target is selected randomly.
!
!        - The original position of the center of the second target
!          is on the ray emanating from the observer and passing
!          through the center of the first target.  The distance
!          between the target centers is chosen randomly.
!
!          The second target is scaled to make sure it is in total
!          occultation by the first target.
!
!        - To search for state transitions, the second target is
!          displaced from its original position.  The displacement
!          is accomplished by rotating the vector from the observer
!          to the center of the second target.  This rotated vector
!          is constrained to lie in a plane whose normal vector is
!          orthogonal to the vector from the observer to the center
!          of the first target.  The orientation of this normal vector
!          is selected randomly.
!
!
!
!
!     Initialize the random number seed.
!
      SEED = -1

      DO I = 1, NCASE1

!        WRITE (*,*) '=========================='

!
! --- Case: ------------------------------------------------------
!
         TITLE = '"General" case:  search for transitions from total '// &
                 'to partial occultation for ellipsoids of '  // &
                 'different shape, size and orientation.  Loop '      // &
                 'iteration = #.'

         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )

!
!        Pick a scale factor; we'll scale all of the participating
!        objects using this factor.
!
         SCALE  = 10.D0 ** T_RANDD ( -10.D0, 10.D0, SEED )

!
!        Create random orientation matrices for both ellipsoids.  We
!        start with three Euler angles.  Also pick random radii for
!        the ellipsoids.
!
         DO J = 1, 2

            ALPHA = T_RANDD ( -PI(),     PI(),     SEED )
            BETA  = T_RANDD ( -HALFPI(), HALFPI(), SEED )
            GAMMA = T_RANDD ( -PI(),     PI(),     SEED )

            CALL EUL2M ( ALPHA, BETA, GAMMA, 1, 2, 3, RMAT )

            CALL XPOSE ( RMAT, BASIS(1,1,J) )

!
!           Pick unscaled radius values in the range 1:10 for the
!           Jth ellipsoid.
!
            R(1,J) = T_RANDD ( 1.D0, 1.D2, SEED )
            R(2,J) = T_RANDD ( 1.D0, 1.D2, SEED )
            R(3,J) = T_RANDD ( 1.D0, 1.D2, SEED )


!
!           Scale the radii using our global scale factor.
!
            CALL VSCLIP ( SCALE, R(1,J) )

!
!           Save the minimum and maximum radii of each ellipsoid.
!
            MINRAD(J) = MIN ( R(1,J), R(2,J), R(3,J) )
            MAXRAD(J) = MAX ( R(1,J), R(2,J), R(3,J) )

!           WRITE (*,*) 'MINRAD, MAXRAD = ', MINRAD(J), MAXRAD(J)

!
!           Create the Jth semi-axis matrix by scaling the column
!           vectors of RMAT.
!
            DO K = 1, 3

               IF ( J .EQ. 1 ) THEN
                  CALL VSCL ( R(K,J), BASIS(1,K,1), SEMAX1(1,K) )
               ELSE
                  CALL VSCL ( R(K,J), BASIS(1,K,2), SEMAX2(1,K) )
               END IF

            END DO

         END DO

!
!        Pick a center for the first ellipsoid.
!
         DO J = 1, 3
            CENTR1(J) = SCALE * T_RANDD ( -1.D3, 1.D3, SEED )
         END DO

!
!        Pick a random viewing point.  Start by picking longitude
!        and latitude of a surface point on the first ellipsoid.
!
         LON = T_RANDD ( -PI(),     PI(),     SEED )
         LAT = T_RANDD ( -HALFPI(), HALFPI(), SEED )

         CALL LATREC ( 1.D0, LON, LAT, SRFDIR )

!
!        Since the origin of this ray is the center of the
!        ellipsoid, we don't have to check the found flag.
!
         CALL SURFPT ( ORIGIN,  SRFDIR, &
                       R(1,1),  R(2,1),  R(3,1), XPT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Pick a random altitude.
!
         ALT  = SCALE * (  10.D0 ** T_RANDD( -1.D0, 4.D0, SEED )  )

!
!        Find the local outward unit surface normal, scale it by
!        the altitude, and add it to XPT to obtain the view point.
!
         CALL SURFNM ( R(1,1),  R(2,1),  R(3,1), XPT, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VLCOM  ( 1.D0, XPT, ALT, NORMAL, VIEWPT )

!
!
!        We want to find the limb of the first ellipsoid as seen
!        from the viewing point.
!
!        Express the viewing point as an offset from the center
!        of the first ellipsoid.
!
         CALL VSUB ( VIEWPT, CENTR1, VPOFF )

!
!        Transform the viewing point into the principal axis frame
!        of the first ellipsoid.
!
         CALL MTXV ( BASIS(1,1,1), VPOFF, LVIEW )

!
!        Find the limb of the ellipsoid.  Rotate the limb back to the
!        original reference frame and shift the limb center to reflect
!        the offset of the first ellipsoid's center from the origin.
!
         CALL EDLIMB ( R(1,1),  R(2,1),  R(3,1), LVIEW, XLIMB )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL EL2CGV ( XLIMB,   V1, V2, V3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MXV ( BASIS(1,1,1), V1, LMBCTR )
         CALL MXV ( BASIS(1,1,1), V2, LMBMAJ )
         CALL MXV ( BASIS(1,1,1), V3, LMBMIN )

         CALL VADD ( CENTR1, LMBCTR, V1     )
         CALL VEQU ( V1,             LMBCTR )

!
!        Construct the limb in the original frame.
!
         CALL CGV2EL ( LMBCTR, LMBMAJ, LMBMIN, LIMB )

!
!        Determine the center of the second ellipsoid.  This ellipsoid
!        is centered on the ray from the view point through the center
!        of the first ellipsoid.
!
!        The second ellipsoid must be placed far enough from the first
!        so that no overlap of the ellipsoids occurs.
!
         DIST(1)    =  VNORM(VPOFF)

!
!        Let UVPOFF be the unit vector pointing from the view point
!        to the center of the first ellipsoid.
!
         CALL VMINUS ( VPOFF, UVPOFF )
         CALL VHATIP ( UVPOFF )

!
!        Pick a random distance between the centers; the distance
!        must be at least 1.25 * the sum of the maximum radii of
!        the ellipsoids.  Here 1.25 is an arbitrary factor "slightly"
!        greater than 1.
!
         RADSUM = MAXRAD(1) + MAXRAD(2)

         CTRSEP = T_RANDD ( 1.25D0*RADSUM, 100.D0*RADSUM, SEED )

!
!        The second center is placed at distance CTRSEP along
!        the ray emanating from the first center in direction UVPOFF.
!
         CALL VLCOM ( 1.D0, CENTR1, CTRSEP, UVPOFF, CENTR2 )

!
!        Now make sure the second ellipsoid is occulted.  We'll
!        adjust its size if necessary.
!
!        Find a lower bound on the angular radius, as seen from the
!        view point, of the first ellipsoid.
!
         MINANG(1)  =  DASINE ( MINRAD(1)/DIST(1),  0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Find the maximum angular radius of the second ellipsoid,
!        based on its nominal radii.
!
         DIST(2) = DIST(1) + CTRSEP

         MAXANG(2) = DASINE ( MAXRAD(2)/DIST(2), 0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         MINANG(2) = 0.D0

!         MINANG(2) = DASINE ( MINRAD(2)/DIST(2), 0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        If we can't guarantee the second ellipsoid is occulted based
!        on its maximum angular radius, scale it down until it "fits."
!
         IF ( MAXANG(2) .GE. MINANG(1) ) THEN
!
!           Find the limit on MAXRAD(2).  We pick LIMIT so that
!
!              ASIN ( LIMIT/DIST(2) ) < MINANG(1)
!
            LIMIT = 0.5D0 * DIST(2) * SIN ( MINANG(1) )

!
!           Scale down the radii of the second ellipsoid.
!
            CALL VSCLIP (  LIMIT/MAXRAD(2),  R(1,2)  )

            MINRAD(2) = MIN ( R(1,2), R(2,2), R(3,2) )
            MAXRAD(2) = MAX ( R(1,2), R(2,2), R(3,2) )

!
!           We must re-create the second semi-axis matrix too.
!
            DO J = 1, 3
               CALL VSCL ( R(J,2), BASIS(1,J,2), SEMAX2(1,J) )
            END DO

         END IF

!
!        Sanity check:  validate MAXANG(2).
!
         MAXANG(2) = DASINE ( MAXRAD(2)/DIST(2), 0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( 'MAXANG(2)',  MAXANG(2),  '<', &
                       MINANG(1),    0.D0,       OK   )

!
!        The second ellipsoid should be in total occultation
!        by the first.  Verify this.
!
         CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE (initial total occultation)', &
                        CODE, '=', TOTAL2, 0, OK                   )


         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'T_OCCED CODE (initial total occultation)', &
                        CODE, '=', TOTAL2, 0, OK                   )


!
!        Repeat with arguments switched.
!
         CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE (initial total occ/switched)', &
                        CODE, '=', TOTAL1, 0, OK                   )


         CODE = T_OCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'T_OCCED CODE (initial total occ/switched)', &
                        CODE, '=', TOTAL1, 0, OK                   )



!
!        Now we're going to displace the second ellipsoid in
!        a direction orthogonal to the vector UVPOFF.  We'll
!        do this by selecting a rotation axis and rotating the
!        vector from the viewpoint to the center of the second
!        ellipsoid about this axis until we detect a change of
!        occultation classification (from total to partial).
!        We'll then make sure that both ZZOCCED and our alternative
!        computation performed by T_OCCED agree that we have a
!        transition at the same angle.
!
!        We're now going to pick a random vector orthogonal to
!        UVPOFF.  First pick an orthogonal basis, with UVPOFF the
!        first vector of the basis.
!
         CALL FRAME ( UVPOFF, Y, Z )

!
!        Pick a random rotation angle; we'll rotate Y about X by
!        this angle to create a rotation axis.
!
         ANGLE = T_RANDD ( -PI(), PI(), SEED )

         CALL VROTV ( Y, UVPOFF, ANGLE, AXIS )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We're now going to bracket the rotation angle needed to
!        find the occultation state transition from total to partial.
!        If the vector from the view point to the center of the
!        second ellipsoid intersects the limb of the first ellipsoid,
!        we definitely have a partial occultation, so determine
!        the angular displacement required to make this happen.
!
!        Create the plane containing the second center and normal
!        to the rotation axis.
!
         CALL NVP2PL ( AXIS, CENTR2, CHOP )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Chop the first limb with the plane.
!
         CALL INELPL ( LIMB, CHOP, NXPTS, XPT1, XPT2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        If we don't have two points of intersection, something's
!        wrong.
!
         CALL CHCKSI ( 'NXPTS', NXPTS, '=', 2, 0, OK )

!
!        Find the vector from the view point to the first
!        intersection point XPT1; find the angular separation
!        of this vector from UVPOFF.
!
         CALL VSUB ( XPT1, VIEWPT, VPXPT1 )

         XSEP = VSEP ( VPXPT1, UVPOFF )

!
!        Determine the sign of the rotation angle by which
!        we rotate UVPOFF to align it with VPXPT1.
!
         CALL VROTV ( UVPOFF, AXIS, XSEP, TSTVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( VSEP(TSTVEC, VPXPT1) .GT. ANGTOL ) THEN

            XSEP = -XSEP
            CALL VROTV ( UVPOFF, AXIS, XSEP, TSTVEC )

         END IF

         CALL CHCKSD ( 'VSEP(TSTVEC, VPXPT1)', VSEP(TSTVEC, VPXPT1), &
                       '~', 0.D0, ANGTOL, OK )

!
!        Time for another sanity check:  rotate the vector from
!        the viewpoint to the center of the second ellipsoid about
!        AXIS by XSEP, then add this to the view point,
!        yielding a new center vector for the second ellipsoid.
!        verify that the ellipsoid is in partial occultation.
!
         CALL VSUB  ( CENTR2, VIEWPT, VPCTR2 )

         CALL VROTV ( VPCTR2, AXIS, XSEP, RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Sanity check:  make sure RVEC points from the view point
!        to XPT1.
!
         CALL CHCKSD ( 'VSEP(RVEC, VPXPT1)', VSEP(RVEC, VPXPT1), &
                       '~', 0.D0, ANGTOL, OK )


         CALL VADD  ( VIEWPT, RVEC, NEWCTR )


!         CALL VSUB  ( NEWCTR, VIEWPT, TSTVEC )
!         WRITE (*,*) 'VPXPT1 sep = ', VSEP ( TSTVEC, VPXPT1 )
!         WRITE (*,*) 'VPCTR2 sep = ', VSEP ( TSTVEC, VPCTR2 )

!
!        Check for partial occultation of the second ellipsoid.
!
         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSI ( 'T_OCCED CODE, (initial partial occultation)', &
                       CODE, '=', PARTL2, 0, OK )

         CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE, (initial partial occultation)', &
                       CODE, '=', PARTL2, 0, OK )


!
!        Repeat with arguments switched.
!
         CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE (initial partial occ/switched)', &
                        CODE, '=', TOTAL1, 0, OK                   )


         CODE = T_OCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'T_OCCED CODE (initial partial occ/switched)', &
                        CODE, '=', TOTAL1, 0, OK                   )

!
!        We now know that a rotation of the vector from the
!        view point to the center of the second ellipsoid about
!        AXIS by an angle between 0 and XSEP radians should yield
!        a state transition.  Find the angle via binary search.
!
         IF ( XSEP .GT. 0.D0 ) THEN

            RSIGN =  1.D0
         ELSE
            RSIGN = -1.D0
         END IF

         LOW  = 0.D0
         HIGH = ABS( XSEP )
         DIFF = HIGH - LOW
         NITR = 0

         DO WHILE ( ( DIFF .GT. CNVTOL ) .AND. OK )

            NITR  = NITR + 1

            CALL CHCKSI ( 'NITR', NITR, '<', MAXITR, 0, OK )

            MIDPT = ( HIGH + LOW ) / 2.D0

            CALL VROTV ( VPCTR2, AXIS, RSIGN*MIDPT, RVEC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VADD  ( VIEWPT, RVEC, NEWCTR )

            CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( CODE .EQ. PARTL2 ) THEN

               HIGH = MIDPT

            ELSE
!
!              The code had better be TOTAL2.
!
               CALL CHCKSI ( '(bisection) CODE', CODE, '=', &
                             TOTAL2, 0, OK )

               LOW  = MIDPT

            END IF

            DIFF = HIGH - LOW

         END DO

!
!        Now that we've dropped out of the loop, verify that T_OCCED
!        says the occultation is total at rotation angle RSIGN*LOW and
!        partial at rotation angle RSIGN*HIGH.  We adjust each
!        of these angles by ADJTOL to allow for differences in
!        round-off between ZZOCCED and T_OCCED.
!
!
!        Verify that when we switch the order of the ellipsoids,
!        we see a transition from total occultation of ellipsoid 1
!        to partial occultation of ellipsoid 1.
!
!
!        Check for total occultation:
!
         CALL VROTV ( VPCTR2, AXIS, RSIGN*(LOW-ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (total)', CODE, '=', &
                       TOTAL2, 0, OK )


         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (total, args switched)', &
                       CODE, '=', &
                       TOTAL1, 0, OK )



!
!        Check for partial occultation:
!
         CALL VROTV ( VPCTR2, AXIS, RSIGN*(HIGH+ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (partial 0)', CODE, '=', &
                       PARTL2, 0, OK )


         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (partial 0, args switched)', &
                       CODE, '=', &
                       PARTL1, 0, OK )


!
! --- Case: ------------------------------------------------------
!
         TITLE = '"General" case:  search for transitions from ' // &
                 'partial occultation to no occultation, '       // &
                 'for ellipsoids of different shape, size and '  // &
                 'orientation.  Loop iteration = #.'

         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )
!
!        The next step is to test for transitions from partial
!        occultation to no occultation.
!
!        Let BIGSEP be a displacement angle large enough to guarantee
!        that no occultation will be found.
!
         BIGSEP = PI()

         CALL VROTV ( VPCTR2, AXIS, RSIGN*BIGSEP, RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE', CODE, '=', NOOCC, 0, OK )

!
!        We now know that a rotation of the vector from the
!        view point to the center of the second ellipsoid about
!        AXIS by an angle between 0 and XSEP radians should yield
!        a state transition.  Find the angle via binary search.
!
         LOW  = ABS(XSEP)
         HIGH = BIGSEP
         DIFF = HIGH - LOW
         NITR = 0

         DO WHILE ( ( DIFF .GT. CNVTOL ) .AND. OK )

            NITR  = NITR + 1

            CALL CHCKSI ( 'NITR', NITR, '<', MAXITR, 0, OK )

            MIDPT = ( HIGH + LOW ) / 2.D0

            CALL VROTV ( VPCTR2, AXIS, RSIGN*MIDPT, RVEC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VADD  ( VIEWPT, RVEC, NEWCTR )

            CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( CODE .EQ. NOOCC ) THEN

               HIGH = MIDPT

            ELSE
!
!              The code had better be PARTL2.
!
               CALL CHCKSI ( '(bisection) CODE', CODE, '=', &
                             PARTL2, 0, OK )

               LOW  = MIDPT

            END IF

            DIFF = HIGH - LOW

         END DO

!
!        Now that we've dropped out of the loop, verify that T_OCCED
!        says the occultation is partial at rotation angle RSIGN*LOW and
!        "none" at rotation angle RSIGN*HIGH.  We adjust each
!        of these angles by CNVTOL to allow for differences in
!        round-off between ZZOCCED and T_OCCED.
!
!
!        Check for partial occultation:
!
         CALL VROTV ( VPCTR2, AXIS, RSIGN*(LOW-ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (partial 1)', CODE, '=', &
                       PARTL2, 0, OK )

!
!        Check for no occultation:
!
         CALL VROTV ( VPCTR2, AXIS, RSIGN*(HIGH+ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (none)', CODE, '=', &
                       NOOCC, 0, OK )


      END DO


!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************
!
!
!         Transit cases follow.
!
!
!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************
!*****************************************************************


!
!
!     The following tests are quite similar to the preceding ones,
!     with the difference that now we're going to have the first
!     ellipsoid start out in annular transit across the second one.
!
      DO I = 1, NCASE2

!        WRITE (*,*) '=========================='

!
! --- Case: ------------------------------------------------------
!
         TITLE = '"General" case:  search for transitions from '      // &
                 'annular to partial transit for ellipsoids of '  // &
                 'different shape, size and orientation.  Loop '      // &
                 'iteration = #.'

         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )


!
!        Pick a scale factor; we'll scale all of the participating
!        objects using this factor.
!
         SCALE  = 10.D0 ** T_RANDD ( -10.D0, 10.D0, SEED )

!
!        Create random orientation matrices for both ellipsoids.  We
!        start with three Euler angles.  Also pick random radii for
!        the ellipsoids.
!
         DO J = 1, 2

            ALPHA = T_RANDD ( -PI(),     PI(),     SEED )
            BETA  = T_RANDD ( -HALFPI(), HALFPI(), SEED )
            GAMMA = T_RANDD ( -PI(),     PI(),     SEED )

            CALL EUL2M ( ALPHA, BETA, GAMMA, 1, 2, 3, RMAT )

            CALL XPOSE ( RMAT, BASIS(1,1,J) )

!
!           Pick unscaled radius values in the range 1:10 for the
!           Jth ellipsoid.
!
            R(1,J) = T_RANDD ( 1.D0, 1.D2, SEED )
            R(2,J) = T_RANDD ( 1.D0, 1.D2, SEED )
            R(3,J) = T_RANDD ( 1.D0, 1.D2, SEED )


!
!           Scale the radii using our global scale factor.
!
            CALL VSCLIP ( SCALE, R(1,J) )

!
!           Save the minimum and maximum radii of each ellipsoid.
!
            MINRAD(J) = MIN ( R(1,J), R(2,J), R(3,J) )
            MAXRAD(J) = MAX ( R(1,J), R(2,J), R(3,J) )

!           WRITE (*,*) 'MINRAD, MAXRAD = ', MINRAD(J), MAXRAD(J)

!
!           Create the Jth semi-axis matrix by scaling the column
!           vectors of RMAT.
!
            DO K = 1, 3

               IF ( J .EQ. 1 ) THEN
                  CALL VSCL ( R(K,J), BASIS(1,K,1), SEMAX1(1,K) )
               ELSE
                  CALL VSCL ( R(K,J), BASIS(1,K,2), SEMAX2(1,K) )
               END IF

            END DO

         END DO

!
!        Pick a center for the first ellipsoid.
!
         DO J = 1, 3
            CENTR1(J) = SCALE * T_RANDD ( -1.D3, 1.D3, SEED )
         END DO

!
!        Pick a random viewing point.  Start by picking longitude
!        and latitude of a surface point on the first ellipsoid.
!
         LON = T_RANDD ( -PI(),     PI(),     SEED )
         LAT = T_RANDD ( -HALFPI(), HALFPI(), SEED )

         CALL LATREC ( 1.D0, LON, LAT, SRFDIR )

!
!        Since the origin of this ray is the center of the
!        ellipsoid, we don't have to check the found flag.
!
         CALL SURFPT ( ORIGIN,  SRFDIR, &
                       R(1,1),  R(2,1),  R(3,1), XPT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Pick a random altitude.  Note the we move the observer
!        farther away from the target than in the total occultation
!        cases.  For those, the minimum exponent was -1.
!
         ALT  = SCALE * (  10.D0 ** T_RANDD( 1.D0, 4.D0, SEED )  )

!
!        Find the local outward unit surface normal, scale it by
!        the altitude, and add it to XPT to obtain the view point.
!
         CALL SURFNM ( R(1,1),  R(2,1),  R(3,1), XPT, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VLCOM  ( 1.D0, XPT, ALT, NORMAL, VIEWPT )

!
!        Find the offset from the viewing point to the center of the
!        first ellipsoid.
!
         CALL VSUB ( VIEWPT, CENTR1, VPOFF )
         DIST(1)    =  VNORM(VPOFF)

!
!        Let UVPOFF be the unit vector pointing from the view point
!        to the center of the first ellipsoid.
!
         CALL VMINUS ( VPOFF, UVPOFF )
         CALL VHATIP ( UVPOFF )


!        Determine the center of the second ellipsoid.  This ellipsoid
!        is centered on the ray from the view point through the center
!        of the first ellipsoid.
!
!        The second ellipsoid must be placed far enough from the first
!        so that no overlap of the ellipsoids occurs.
!
!        Pick a random distance between the centers; the distance
!        must be at least 1.25 * the sum of the maximum radii of
!        the ellipsoids.  Here 1.25 is an arbitrary factor "slightly"
!        larger than 1.
!
         RADSUM = MAXRAD(1) + MAXRAD(2)

         CTRSEP = T_RANDD ( 1.25D0*RADSUM, 100.D0*RADSUM, SEED )

!
!        The second center is placed at distance CTRSEP along
!        the ray emanating from the first center in direction UVPOFF.
!
         CALL VLCOM ( 1.D0, CENTR1, CTRSEP, UVPOFF, CENTR2 )


!
!        Now make sure the second ellipsoid is in annular occultation
!        by the first.  We'll adjust the size of the first
!        ellipsoid if necessary.
!
!        Find a upper bound on the angular radius, as seen from the
!        view point, of the first ellipsoid.
!
         MAXANG(1)  =  DASINE ( MAXRAD(1)/DIST(1),  0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Find the minimum angular radius of the second ellipsoid,
!        based on its nominal radii.
!
         DIST(2) = DIST(1) + CTRSEP

         MAXANG(2) = 0.D0

!         MAXANG(2) = DASINE ( MAXRAD(2)/DIST(2), 0.D0 )
!         CALL CHCKXC ( .FALSE., ' ', OK )

         MINANG(1) = 0.D0
         MINANG(2) = DASINE ( MINRAD(2)/DIST(2), 0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        If we can't guarantee the second ellipsoid is in annular
!        occultation by the first, based on the second ellipsoid's
!        minimum angular radius, shrink the first ellipsoid until this
!        condition is met.
!
         IF ( MINANG(2) .LE. MAXANG(1) ) THEN
!
!           Find the limit on MAXRAD(1).  We pick LIMIT so that
!
!              ASIN ( LIMIT/DIST(1) ) = MINANG(2)
!
            LIMIT = DIST(1) * SIN ( MINANG(2) )

!
!           Scale down the radii of the first ellipsoid.  Use the
!           arbitrary safety factor 0.75.
!
            CALL VSCLIP (  0.75D0 * LIMIT/MAXRAD(1),  R(1,1)  )

            MINRAD(1) = MIN ( R(1,1), R(2,1), R(3,1) )
            MAXRAD(1) = MAX ( R(1,1), R(2,1), R(3,1) )

!
!           We must re-create the first semi-axis matrix too.
!
            DO J = 1, 3
               CALL VSCL ( R(J,1), BASIS(1,J,1), SEMAX1(1,J) )
            END DO

         END IF

!
!        Sanity check:  validate MAXANG(1).
!
         MAXANG(1) = DASINE ( MAXRAD(1)/DIST(1), 0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( 'MAXANG(1)',  MAXANG(1),  '<', &
                       MINANG(2),    0.D0,       OK   )

!
!
!        We want to find the limb of the *second* ellipsoid as seen
!        from the viewing point.
!
!        Express the viewing point as an offset from the center
!        of the first ellipsoid.
!
         CALL VSUB ( VIEWPT, CENTR2, VPOFF2 )

!
!        Transform the viewing point into the principal axis frame
!        of the second ellipsoid.
!
         CALL MTXV ( BASIS(1,1,2), VPOFF2, LVIEW )

!
!        Find the limb of the second ellipsoid. Rotate the limb back to
!        the original reference frame and shift the limb center to
!        reflect the offset of the second ellipsoid's center from the
!        origin.
!
         CALL EDLIMB ( R(1,2),  R(2,2),  R(3,2), LVIEW, XLIMB )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL EL2CGV ( XLIMB,   V1, V2, V3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MXV ( BASIS(1,1,2), V1, LMBCTR )
         CALL MXV ( BASIS(1,1,2), V2, LMBMAJ )
         CALL MXV ( BASIS(1,1,2), V3, LMBMIN )

         CALL VADD ( CENTR2, LMBCTR, V1     )
         CALL VEQU ( V1,             LMBCTR )

!
!        Construct the limb of the second ellipsoid
!        in the original frame.
!
         CALL CGV2EL ( LMBCTR, LMBMAJ, LMBMIN, LIMB )

!
!        The second ellipsoid should be in annular occultation
!        by the first.  Verify this.
!
         CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE (initial annular occultation)', &
                        CODE, '=', ANNLR2, 0, OK                   )


         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'T_OCCED CODE (initial annular occultation)', &
                        CODE, '=', ANNLR2, 0, OK                   )


!
!        Repeat with arguments switched.
!
         CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE (initial annular occ/switched)', &
                        CODE, '=', ANNLR1, 0, OK                   )


         CODE = T_OCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'T_OCCED CODE (initial annular occ/switched)', &
                        CODE, '=', ANNLR1, 0, OK                   )



!
!        Now we're going to displace the first ellipsoid in
!        a direction orthogonal to the vector UVPOFF.  We'll
!        do this by selecting a rotation axis and rotating the
!        vector from the viewpoint to the center of the first
!        ellipsoid about this axis until we detect a change of
!        transit classification (from annular to partial).
!        We'll then make sure that both ZZOCCED and our alternative
!        computation performed by T_OCCED agree that we have a
!        transition at the same angle.
!
!        We're now going to pick a random vector orthogonal to
!        UVPOFF.  First pick an orthogonal basis, with UVPOFF the
!        first vector of the basis.
!
         CALL FRAME ( UVPOFF, Y, Z )

!
!        Pick a random rotation angle; we'll rotate Y about X by
!        this angle to create a rotation axis.
!
         ANGLE = T_RANDD ( -PI(), PI(), SEED )

         CALL VROTV ( Y, UVPOFF, ANGLE, AXIS )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We're now going to bracket the rotation angle needed to find
!        the transit state transition from annular to partial. If
!        the vector from the view point to the center of the *first*
!        ellipsoid intersects the limb of the *second* ellipsoid, we
!        definitely have a partial transit, so determine the
!        angular displacement required to make this happen.
!
!        Create the plane containing the second center and normal
!        to the rotation axis.
!
         CALL NVP2PL ( AXIS, CENTR2, CHOP )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Chop the second limb with the plane.
!
         CALL INELPL ( LIMB, CHOP, NXPTS, XPT1, XPT2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        If we don't have two points of intersection, something's
!        wrong.
!
         CALL CHCKSI ( 'NXPTS', NXPTS, '=', 2, 0, OK )

!
!        Find the vector from the view point to the first
!        intersection point XPT1; find the angular separation
!        of this vector from UVPOFF.
!
         CALL VSUB ( XPT1, VIEWPT, VPXPT1 )

         XSEP = VSEP ( VPXPT1, UVPOFF )

!
!        Determine the sign of the rotation angle by which
!        we rotate UVPOFF to align it with VPXPT1.
!
         CALL VROTV ( UVPOFF, AXIS, XSEP, TSTVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( VSEP(TSTVEC, VPXPT1) .GT. ANGTOL ) THEN

            XSEP = -XSEP
            CALL VROTV ( UVPOFF, AXIS, XSEP, TSTVEC )

         END IF

         CALL CHCKSD ( 'VSEP(TSTVEC, VPXPT1)', VSEP(TSTVEC, VPXPT1), &
                       '~', 0.D0, ANGTOL, OK )

!
!        Time for another sanity check:  rotate the vector from
!        the viewpoint to the center of the first ellipsoid about
!        AXIS by XSEP, then add this to the view point,
!        yielding a new center vector for the first ellipsoid.
!        Verify that the first ellipsoid is in partial transit.
!
         CALL VSUB  ( CENTR1, VIEWPT, VPCTR1 )

         CALL VROTV ( VPCTR1, AXIS, XSEP, RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Sanity check:  make sure RVEC points from the view point
!        to XPT1.
!
         CALL CHCKSD ( 'VSEP(RVEC, VPXPT1)', VSEP(RVEC, VPXPT1), &
                       '~', 0.D0, ANGTOL, OK )


         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

!         CALL VSUB  ( NEWCTR, VIEWPT, TSTVEC )
!         WRITE (*,*) 'VPXPT1 sep = ', VSEP ( TSTVEC, VPXPT1 )
!         WRITE (*,*) 'VPCTR2 sep = ', VSEP ( TSTVEC, VPCTR2 )

!
!        Check for partial transit of the first ellipsoid across
!        the second.
!
         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSI ( 'T_OCCED CODE, (initial partial transit)', &
                       CODE, '=', PARTL2, 0, OK )

         CODE = ZZOCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE, (initial partial transit)', &
                       CODE, '=', PARTL2, 0, OK )


!
!        Repeat with arguments switched.
!
         CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, NEWCTR, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE (initial partial occ/switched)', &
                        CODE, '=', PARTL1, 0, OK                   )


         CODE = T_OCCED ( VIEWPT, CENTR2, SEMAX2, NEWCTR, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'T_OCCED CODE (initial partial occ/switched)', &
                        CODE, '=', PARTL1, 0, OK                   )

!
!        We now know that a rotation of the vector from the
!        view point to the center of the second ellipsoid about
!        AXIS by an angle between 0 and XSEP radians should yield
!        a state transition.  Find the angle via binary search.
!
         IF ( XSEP .GT. 0.D0 ) THEN

            RSIGN =  1.D0
         ELSE
            RSIGN = -1.D0
         END IF

         LOW  = 0.D0
         HIGH = ABS( XSEP )
         DIFF = HIGH - LOW
         NITR = 0

         DO WHILE ( ( DIFF .GT. CNVTOL ) .AND. OK )

            NITR  = NITR + 1

            CALL CHCKSI ( 'NITR', NITR, '<', MAXITR, 0, OK )

            MIDPT = ( HIGH + LOW ) / 2.D0

            CALL VROTV ( VPCTR1, AXIS, RSIGN*MIDPT, RVEC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VADD  ( VIEWPT, RVEC, NEWCTR )

!XXX
            CODE = ZZOCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( CODE .EQ. PARTL2 ) THEN

               HIGH = MIDPT

            ELSE
!
!              The code had better be ANNLR2.
!
               CALL CHCKSI ( '(bisection) CODE', CODE, '=', &
                             ANNLR2, 0, OK )

               LOW  = MIDPT

            END IF

            DIFF = HIGH - LOW

         END DO

!
!        Now that we've dropped out of the loop, verify that T_OCCED
!        says the transit is total at rotation angle RSIGN*LOW and
!        partial at rotation angle RSIGN*HIGH.  We adjust each
!        of these angles by ADJTOL to allow for differences in
!        round-off between ZZOCCED and T_OCCED.
!
!
!        Verify that when we switch the order of the ellipsoids,
!        we see a transition from total transit of ellipsoid 1
!        to partial transit of ellipsoid 1.
!
!
!        Check for annular transit:
!
         CALL VROTV ( VPCTR1, AXIS, RSIGN*(LOW-ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (annular)', CODE, '=', &
                       ANNLR2, 0, OK )


!
!        IF ( .NOT. OK ) THEN
!
!           WRITE (*,*) '============='
!           WRITE (*,*) 'XSEP = ', XSEP
!           WRITE (*,*) 'MINANG: ', MINANG
!           WRITE (*,*) 'MAXANG: ', MAXANG
!           WRITE (*,*) 'LIMB   = ', LIMB
!           WRITE (*,*) 'VIEWPT = ', VIEWPT
!           WRITE (*,*) 'LOW, HIGH = ', LOW, HIGH
!        END IF



         CODE = T_OCCED ( VIEWPT, CENTR2, SEMAX2, NEWCTR, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (annular, args switched)', &
                       CODE, '=', &
                       ANNLR1, 0, OK )



!
!        Check for partial transit:
!
         CALL VROTV ( VPCTR1, AXIS, RSIGN*(HIGH+ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (partial 0)', CODE, '=', &
                       PARTL2, 0, OK )

!
!        IF ( .NOT. OK ) THEN
!
!           WRITE (*,*) '============='
!           WRITE (*,*) 'XSEP = ', XSEP
!           WRITE (*,*) 'MINANG: ', MINANG
!           WRITE (*,*) 'MAXANG: ', MAXANG
!           WRITE (*,*) 'LIMB   = ', LIMB
!           WRITE (*,*) 'VIEWPT = ', VIEWPT
!           WRITE (*,*) 'LOW, HIGH = ', LOW, HIGH
!        END IF
!
         CODE = T_OCCED ( VIEWPT, CENTR2, SEMAX2, NEWCTR, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (partial 0, args switched)', &
                       CODE, '=', &
                       PARTL1, 0, OK )


!
! --- Case: ------------------------------------------------------
!
         TITLE = '"General" case:  search for transitions from ' // &
                 'partial transit to no occultation, '       // &
                 'for ellipsoids of different shape, size and '  // &
                 'orientation.  Loop iteration = #.'

         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )
!
!        The next step is to test for transitions from partial
!        transit to no occultation.
!
!        Let BIGSEP be a displacement angle large enough to guarantee
!        that no occultation will be found.
!
         BIGSEP = PI()

         CALL VROTV ( VPCTR1, AXIS, RSIGN*BIGSEP, RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = ZZOCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE', CODE, '=', NOOCC, 0, OK )

!
!        We now know that a rotation of the vector from the
!        view point to the center of the first ellipsoid about
!        AXIS by an angle between 0 and XSEP radians should yield
!        a state transition.  Find the angle via binary search.
!
         LOW  = ABS(XSEP)
         HIGH = BIGSEP
         DIFF = HIGH - LOW
         NITR = 0

         DO WHILE ( ( DIFF .GT. CNVTOL ) .AND. OK )

            NITR  = NITR + 1

            CALL CHCKSI ( 'NITR', NITR, '<', MAXITR, 0, OK )

            MIDPT = ( HIGH + LOW ) / 2.D0

            CALL VROTV ( VPCTR1, AXIS, RSIGN*MIDPT, RVEC )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VADD  ( VIEWPT, RVEC, NEWCTR )

            CODE = ZZOCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( CODE .EQ. NOOCC ) THEN

               HIGH = MIDPT

            ELSE
!
!              The code had better be PARTL2.
!
               CALL CHCKSI ( '(bisection) CODE', CODE, '=', &
                             PARTL2, 0, OK )

               LOW  = MIDPT

            END IF

            DIFF = HIGH - LOW

         END DO

!
!        Now that we've dropped out of the loop, verify that T_OCCED
!        says the transit is partial at rotation angle RSIGN*LOW and
!        "none" at rotation angle RSIGN*HIGH.  We adjust each
!        of these angles by CNVTOL to allow for differences in
!        round-off between ZZOCCED and T_OCCED.
!
!
!        Check for partial transit:
!
         CALL VROTV ( VPCTR1, AXIS, RSIGN*(LOW-ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (partial 1)', CODE, '=', &
                       PARTL2, 0, OK )

!
!        Check for no occultation:
!
         CALL VROTV ( VPCTR1, AXIS, RSIGN*(HIGH+ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (none)', CODE, '=', &
                       NOOCC, 0, OK )


      END DO




      CALL T_SUCCESS ( OK )
      RETURN
      END
