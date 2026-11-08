C$Procedure      F_ZZOCCED3 ( Test ellipsoid occultation routine )

      SUBROUTINE F_ZZOCCED3 ( OK )
      IMPLICIT NONE

      LOGICAL               OK

      INCLUDE 'zzocced.inc'

C$ Abstract
C
C     This routine tests the SPICELIB routine
C     
C        ZZOCCED
C
C     by searching for occultation state transitions and comparing
C     them to those found using the alternate occultation classification
C     routine 
C
C        T_OCCED
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Version
C
C-    TSPICE Version 1.0.0, 20-NOV-2006 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      INTEGER               ZZOCCED

C
C     Other functions
C
      DOUBLE PRECISION      T_RANDD

      INTEGER               T_OCCED

C
C     Local parameters
C
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

C
C     Recommendation:  increase the parameters NCASE1 and NCASE2
C     to 1000 for robust testing.
C
      INTEGER               NCASE1
      PARAMETER           ( NCASE1 = 200 )

      INTEGER               NCASE2
      PARAMETER           ( NCASE2 = 200 )

C
C     Local variables
C
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

C
C     Saved variables
C
      SAVE                  ORIGIN

C
C     Initial values
C
      DATA                  ORIGIN /  3 * 0.D0 /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZOCCED3' )


C
C     The initial set of cases tests the accuracy with which 
C     ZZOCCED can detect transitions between total occultation
C     and partial occultation.
C
C     In the discussion below, we use the terms "target" and 
C     "ellipsoid" interchangeably
C
C     These cases attempt to cover a broad set of geometric cases.
C     In order to do this efficiently, random numbers are used
C     to generate most inputs:
C
C        - A random scale factor is applied uniformly to all
C          participating objects.
C
C        - The principal axis matrices of both targets are 
C          generated from random Euler angles.
C
C        - The radii of each target are chosen randomly, with
C          each radius in the range of 1:100 prior to scaling.
C
C        - The location of the observer's sub-point on the first
C          target is selected using a direction vector with random
C          components.  The ray emanating from the center of the
C          first target and parallel to this direction vector
C          defines the sub-point.
C       
C        - The altitude of the observer above its sub-point on the
C          first target is selected randomly.
C
C        - The original position of the center of the second target
C          is on the ray emanating from the observer and passing
C          through the center of the first target.  The distance
C          between the target centers is chosen randomly.
C
C          The second target is scaled to make sure it is in total
C          occultation by the first target.
C
C        - To search for state transitions, the second target is 
C          displaced from its original position.  The displacement
C          is accomplished by rotating the vector from the observer
C          to the center of the second target.  This rotated vector
C          is constrained to lie in a plane whose normal vector is
C          orthogonal to the vector from the observer to the center
C          of the first target.  The orientation of this normal vector
C          is selected randomly.
C
C
C
C
C     Initialize the random number seed.
C
      SEED = -1
     
      DO I = 1, NCASE1

C        WRITE (*,*) '=========================='

C
C --- Case: ------------------------------------------------------
C
         TITLE = '"General" case:  search for transitions from total '//
     .           'to partial occultation for ellipsoids of '  //
     .           'different shape, size and orientation.  Loop '      //
     .           'iteration = #.'

         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )
      
C
C        Pick a scale factor; we'll scale all of the participating
C        objects using this factor.
C
         SCALE  = 10.D0 ** T_RANDD ( -10.D0, 10.D0, SEED )

C
C        Create random orientation matrices for both ellipsoids.  We
C        start with three Euler angles.  Also pick random radii for
C        the ellipsoids.
C
         DO J = 1, 2

            ALPHA = T_RANDD ( -PI(),     PI(),     SEED )
            BETA  = T_RANDD ( -HALFPI(), HALFPI(), SEED )
            GAMMA = T_RANDD ( -PI(),     PI(),     SEED )

            CALL EUL2M ( ALPHA, BETA, GAMMA, 1, 2, 3, RMAT )

            CALL XPOSE ( RMAT, BASIS(1,1,J) )
 
C
C           Pick unscaled radius values in the range 1:10 for the
C           Jth ellipsoid.
C
            R(1,J) = T_RANDD ( 1.D0, 1.D2, SEED )
            R(2,J) = T_RANDD ( 1.D0, 1.D2, SEED )
            R(3,J) = T_RANDD ( 1.D0, 1.D2, SEED )


C
C           Scale the radii using our global scale factor.
C
            CALL VSCLIP ( SCALE, R(1,J) )

C
C           Save the minimum and maximum radii of each ellipsoid.
C
            MINRAD(J) = MIN ( R(1,J), R(2,J), R(3,J) )
            MAXRAD(J) = MAX ( R(1,J), R(2,J), R(3,J) )

C           WRITE (*,*) 'MINRAD, MAXRAD = ', MINRAD(J), MAXRAD(J)

C
C           Create the Jth semi-axis matrix by scaling the column
C           vectors of RMAT.
C
            DO K = 1, 3

               IF ( J .EQ. 1 ) THEN
                  CALL VSCL ( R(K,J), BASIS(1,K,1), SEMAX1(1,K) )
               ELSE
                  CALL VSCL ( R(K,J), BASIS(1,K,2), SEMAX2(1,K) )
               END IF

            END DO

         END DO

C
C        Pick a center for the first ellipsoid.
C
         DO J = 1, 3
            CENTR1(J) = SCALE * T_RANDD ( -1.D3, 1.D3, SEED )
         END DO

C
C        Pick a random viewing point.  Start by picking longitude
C        and latitude of a surface point on the first ellipsoid.
C                
         LON = T_RANDD ( -PI(),     PI(),     SEED )
         LAT = T_RANDD ( -HALFPI(), HALFPI(), SEED )

         CALL LATREC ( 1.D0, LON, LAT, SRFDIR )

C
C        Since the origin of this ray is the center of the
C        ellipsoid, we don't have to check the found flag.
C
         CALL SURFPT ( ORIGIN,  SRFDIR, 
     .                 R(1,1),  R(2,1),  R(3,1), XPT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Pick a random altitude.
C
         ALT  = SCALE * (  10.D0 ** T_RANDD( -1.D0, 4.D0, SEED )  )

C
C        Find the local outward unit surface normal, scale it by
C        the altitude, and add it to XPT to obtain the view point.
C
         CALL SURFNM ( R(1,1),  R(2,1),  R(3,1), XPT, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VLCOM  ( 1.D0, XPT, ALT, NORMAL, VIEWPT )

C
C
C        We want to find the limb of the first ellipsoid as seen
C        from the viewing point.
C
C        Express the viewing point as an offset from the center
C        of the first ellipsoid.
C
         CALL VSUB ( VIEWPT, CENTR1, VPOFF )

C
C        Transform the viewing point into the principal axis frame
C        of the first ellipsoid.
C
         CALL MTXV ( BASIS(1,1,1), VPOFF, LVIEW )

C
C        Find the limb of the ellipsoid.  Rotate the limb back to the
C        original reference frame and shift the limb center to reflect
C        the offset of the first ellipsoid's center from the origin.
C
         CALL EDLIMB ( R(1,1),  R(2,1),  R(3,1), LVIEW, XLIMB )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL EL2CGV ( XLIMB,   V1, V2, V3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MXV ( BASIS(1,1,1), V1, LMBCTR )
         CALL MXV ( BASIS(1,1,1), V2, LMBMAJ )
         CALL MXV ( BASIS(1,1,1), V3, LMBMIN )

         CALL VADD ( CENTR1, LMBCTR, V1     )
         CALL VEQU ( V1,             LMBCTR )

C
C        Construct the limb in the original frame.
C
         CALL CGV2EL ( LMBCTR, LMBMAJ, LMBMIN, LIMB )
 
C
C        Determine the center of the second ellipsoid.  This ellipsoid
C        is centered on the ray from the view point through the center
C        of the first ellipsoid.  
C
C        The second ellipsoid must be placed far enough from the first
C        so that no overlap of the ellipsoids occurs. 
C
         DIST(1)    =  VNORM(VPOFF)

C
C        Let UVPOFF be the unit vector pointing from the view point
C        to the center of the first ellipsoid.
C
         CALL VMINUS ( VPOFF, UVPOFF )
         CALL VHATIP ( UVPOFF )
         
C
C        Pick a random distance between the centers; the distance
C        must be at least 1.25 * the sum of the maximum radii of
C        the ellipsoids.  Here 1.25 is an arbitrary factor "slightly"
C        greater than 1.
C        
         RADSUM = MAXRAD(1) + MAXRAD(2)

         CTRSEP = T_RANDD ( 1.25D0*RADSUM, 100.D0*RADSUM, SEED )

C
C        The second center is placed at distance CTRSEP along
C        the ray emanating from the first center in direction UVPOFF.
C
         CALL VLCOM ( 1.D0, CENTR1, CTRSEP, UVPOFF, CENTR2 )

C
C        Now make sure the second ellipsoid is occulted.  We'll
C        adjust its size if necessary.
C
C        Find a lower bound on the angular radius, as seen from the
C        view point, of the first ellipsoid.
C
         MINANG(1)  =  DASINE ( MINRAD(1)/DIST(1),  0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Find the maximum angular radius of the second ellipsoid,
C        based on its nominal radii.
C
         DIST(2) = DIST(1) + CTRSEP

         MAXANG(2) = DASINE ( MAXRAD(2)/DIST(2), 0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         MINANG(2) = 0.D0

C         MINANG(2) = DASINE ( MINRAD(2)/DIST(2), 0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        If we can't guarantee the second ellipsoid is occulted based
C        on its maximum angular radius, scale it down until it "fits."
C
         IF ( MAXANG(2) .GE. MINANG(1) ) THEN
C
C           Find the limit on MAXRAD(2).  We pick LIMIT so that
C 
C              ASIN ( LIMIT/DIST(2) ) < MINANG(1)
C
            LIMIT = 0.5D0 * DIST(2) * SIN ( MINANG(1) )

C
C           Scale down the radii of the second ellipsoid. 
C          
            CALL VSCLIP (  LIMIT/MAXRAD(2),  R(1,2)  )

            MINRAD(2) = MIN ( R(1,2), R(2,2), R(3,2) )
            MAXRAD(2) = MAX ( R(1,2), R(2,2), R(3,2) )

C
C           We must re-create the second semi-axis matrix too.
C
            DO J = 1, 3
               CALL VSCL ( R(J,2), BASIS(1,J,2), SEMAX2(1,J) )
            END DO

         END IF

C
C        Sanity check:  validate MAXANG(2).
C
         MAXANG(2) = DASINE ( MAXRAD(2)/DIST(2), 0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( 'MAXANG(2)',  MAXANG(2),  '<', 
     .                 MINANG(1),    0.D0,       OK   )

C
C        The second ellipsoid should be in total occultation
C        by the first.  Verify this.
C        
         CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE (initial total occultation)', 
     .                  CODE, '=', TOTAL2, 0, OK                   )


         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'T_OCCED CODE (initial total occultation)', 
     .                  CODE, '=', TOTAL2, 0, OK                   )
         

C
C        Repeat with arguments switched.
C
         CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE (initial total occ/switched)', 
     .                  CODE, '=', TOTAL1, 0, OK                   )


         CODE = T_OCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'T_OCCED CODE (initial total occ/switched)', 
     .                  CODE, '=', TOTAL1, 0, OK                   )
         


C
C        Now we're going to displace the second ellipsoid in
C        a direction orthogonal to the vector UVPOFF.  We'll
C        do this by selecting a rotation axis and rotating the
C        vector from the viewpoint to the center of the second
C        ellipsoid about this axis until we detect a change of
C        occultation classification (from total to partial).
C        We'll then make sure that both ZZOCCED and our alternative
C        computation performed by T_OCCED agree that we have a 
C        transition at the same angle.
C
C        We're now going to pick a random vector orthogonal to 
C        UVPOFF.  First pick an orthogonal basis, with UVPOFF the
C        first vector of the basis.
C
         CALL FRAME ( UVPOFF, Y, Z )

C
C        Pick a random rotation angle; we'll rotate Y about X by
C        this angle to create a rotation axis.
C
         ANGLE = T_RANDD ( -PI(), PI(), SEED )

         CALL VROTV ( Y, UVPOFF, ANGLE, AXIS )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We're now going to bracket the rotation angle needed to
C        find the occultation state transition from total to partial.
C        If the vector from the view point to the center of the 
C        second ellipsoid intersects the limb of the first ellipsoid,
C        we definitely have a partial occultation, so determine
C        the angular displacement required to make this happen.
C
C        Create the plane containing the second center and normal
C        to the rotation axis.
C
         CALL NVP2PL ( AXIS, CENTR2, CHOP )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Chop the first limb with the plane.
C
         CALL INELPL ( LIMB, CHOP, NXPTS, XPT1, XPT2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        If we don't have two points of intersection, something's
C        wrong.
C
         CALL CHCKSI ( 'NXPTS', NXPTS, '=', 2, 0, OK )

C
C        Find the vector from the view point to the first
C        intersection point XPT1; find the angular separation
C        of this vector from UVPOFF.
C
         CALL VSUB ( XPT1, VIEWPT, VPXPT1 )

         XSEP = VSEP ( VPXPT1, UVPOFF )

C
C        Determine the sign of the rotation angle by which
C        we rotate UVPOFF to align it with VPXPT1.
C        
         CALL VROTV ( UVPOFF, AXIS, XSEP, TSTVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( VSEP(TSTVEC, VPXPT1) .GT. ANGTOL ) THEN
            
            XSEP = -XSEP
            CALL VROTV ( UVPOFF, AXIS, XSEP, TSTVEC )

         END IF

         CALL CHCKSD ( 'VSEP(TSTVEC, VPXPT1)', VSEP(TSTVEC, VPXPT1),
     .                 '~', 0.D0, ANGTOL, OK )

C
C        Time for another sanity check:  rotate the vector from
C        the viewpoint to the center of the second ellipsoid about
C        AXIS by XSEP, then add this to the view point,
C        yielding a new center vector for the second ellipsoid.
C        verify that the ellipsoid is in partial occultation.
C
         CALL VSUB  ( CENTR2, VIEWPT, VPCTR2 )

         CALL VROTV ( VPCTR2, AXIS, XSEP, RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Sanity check:  make sure RVEC points from the view point
C        to XPT1.
C        
         CALL CHCKSD ( 'VSEP(RVEC, VPXPT1)', VSEP(RVEC, VPXPT1),
     .                 '~', 0.D0, ANGTOL, OK )


         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         
C         CALL VSUB  ( NEWCTR, VIEWPT, TSTVEC )
C         WRITE (*,*) 'VPXPT1 sep = ', VSEP ( TSTVEC, VPXPT1 )
C         WRITE (*,*) 'VPCTR2 sep = ', VSEP ( TSTVEC, VPCTR2 )

C
C        Check for partial occultation of the second ellipsoid.
C
         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSI ( 'T_OCCED CODE, (initial partial occultation)', 
     .                 CODE, '=', PARTL2, 0, OK )

         CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE, (initial partial occultation)', 
     .                 CODE, '=', PARTL2, 0, OK )


C
C        Repeat with arguments switched.
C
         CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE (initial partial occ/switched)', 
     .                  CODE, '=', TOTAL1, 0, OK                   )


         CODE = T_OCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'T_OCCED CODE (initial partial occ/switched)', 
     .                  CODE, '=', TOTAL1, 0, OK                   )
         
C
C        We now know that a rotation of the vector from the 
C        view point to the center of the second ellipsoid about
C        AXIS by an angle between 0 and XSEP radians should yield
C        a state transition.  Find the angle via binary search.
C
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
C
C              The code had better be TOTAL2.
C
               CALL CHCKSI ( '(bisection) CODE', CODE, '=', 
     .                       TOTAL2, 0, OK )

               LOW  = MIDPT

            END IF

            DIFF = HIGH - LOW

         END DO
         
C
C        Now that we've dropped out of the loop, verify that T_OCCED
C        says the occultation is total at rotation angle RSIGN*LOW and
C        partial at rotation angle RSIGN*HIGH.  We adjust each 
C        of these angles by ADJTOL to allow for differences in 
C        round-off between ZZOCCED and T_OCCED.
C
C
C        Verify that when we switch the order of the ellipsoids,
C        we see a transition from total occultation of ellipsoid 1
C        to partial occultation of ellipsoid 1.
C
C
C        Check for total occultation:
C
         CALL VROTV ( VPCTR2, AXIS, RSIGN*(LOW-ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (total)', CODE, '=', 
     .                 TOTAL2, 0, OK )


         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (total, args switched)', 
     .                 CODE, '=', 
     .                 TOTAL1, 0, OK )



C
C        Check for partial occultation:
C
         CALL VROTV ( VPCTR2, AXIS, RSIGN*(HIGH+ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (partial 0)', CODE, '=', 
     .                 PARTL2, 0, OK )


         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (partial 0, args switched)', 
     .                 CODE, '=', 
     .                 PARTL1, 0, OK )


C
C --- Case: ------------------------------------------------------
C
         TITLE = '"General" case:  search for transitions from ' //
     .           'partial occultation to no occultation, '       //
     .           'for ellipsoids of different shape, size and '  //
     .           'orientation.  Loop iteration = #.'

         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )
C
C        The next step is to test for transitions from partial 
C        occultation to no occultation.
C
C        Let BIGSEP be a displacement angle large enough to guarantee
C        that no occultation will be found.
C
         BIGSEP = PI()

         CALL VROTV ( VPCTR2, AXIS, RSIGN*BIGSEP, RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE', CODE, '=', NOOCC, 0, OK )

C
C        We now know that a rotation of the vector from the 
C        view point to the center of the second ellipsoid about
C        AXIS by an angle between 0 and XSEP radians should yield
C        a state transition.  Find the angle via binary search.
C
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
C
C              The code had better be PARTL2.
C
               CALL CHCKSI ( '(bisection) CODE', CODE, '=', 
     .                       PARTL2, 0, OK )

               LOW  = MIDPT

            END IF

            DIFF = HIGH - LOW

         END DO
         
C
C        Now that we've dropped out of the loop, verify that T_OCCED
C        says the occultation is partial at rotation angle RSIGN*LOW and
C        "none" at rotation angle RSIGN*HIGH.  We adjust each 
C        of these angles by CNVTOL to allow for differences in 
C        round-off between ZZOCCED and T_OCCED.
C
C
C        Check for partial occultation:
C
         CALL VROTV ( VPCTR2, AXIS, RSIGN*(LOW-ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (partial 1)', CODE, '=', 
     .                 PARTL2, 0, OK )

C
C        Check for no occultation:
C
         CALL VROTV ( VPCTR2, AXIS, RSIGN*(HIGH+ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, NEWCTR, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (none)', CODE, '=', 
     .                 NOOCC, 0, OK )


      END DO


C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************
C
C
C         Transit cases follow.
C
C
C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************
C*****************************************************************


C
C
C     The following tests are quite similar to the preceding ones,
C     with the difference that now we're going to have the first
C     ellipsoid start out in annular transit across the second one.
C     
      DO I = 1, NCASE2

C        WRITE (*,*) '=========================='

C
C --- Case: ------------------------------------------------------
C
         TITLE = '"General" case:  search for transitions from '      //
     .           'annular to partial transit for ellipsoids of '  //
     .           'different shape, size and orientation.  Loop '      //
     .           'iteration = #.'

         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )
      

C
C        Pick a scale factor; we'll scale all of the participating
C        objects using this factor.
C
         SCALE  = 10.D0 ** T_RANDD ( -10.D0, 10.D0, SEED )

C
C        Create random orientation matrices for both ellipsoids.  We
C        start with three Euler angles.  Also pick random radii for
C        the ellipsoids.
C
         DO J = 1, 2

            ALPHA = T_RANDD ( -PI(),     PI(),     SEED )
            BETA  = T_RANDD ( -HALFPI(), HALFPI(), SEED )
            GAMMA = T_RANDD ( -PI(),     PI(),     SEED )

            CALL EUL2M ( ALPHA, BETA, GAMMA, 1, 2, 3, RMAT )

            CALL XPOSE ( RMAT, BASIS(1,1,J) )
 
C
C           Pick unscaled radius values in the range 1:10 for the
C           Jth ellipsoid.
C
            R(1,J) = T_RANDD ( 1.D0, 1.D2, SEED )
            R(2,J) = T_RANDD ( 1.D0, 1.D2, SEED )
            R(3,J) = T_RANDD ( 1.D0, 1.D2, SEED )


C
C           Scale the radii using our global scale factor.
C
            CALL VSCLIP ( SCALE, R(1,J) )

C
C           Save the minimum and maximum radii of each ellipsoid.
C
            MINRAD(J) = MIN ( R(1,J), R(2,J), R(3,J) )
            MAXRAD(J) = MAX ( R(1,J), R(2,J), R(3,J) )

C           WRITE (*,*) 'MINRAD, MAXRAD = ', MINRAD(J), MAXRAD(J)

C
C           Create the Jth semi-axis matrix by scaling the column
C           vectors of RMAT.
C
            DO K = 1, 3

               IF ( J .EQ. 1 ) THEN
                  CALL VSCL ( R(K,J), BASIS(1,K,1), SEMAX1(1,K) )
               ELSE
                  CALL VSCL ( R(K,J), BASIS(1,K,2), SEMAX2(1,K) )
               END IF

            END DO

         END DO

C
C        Pick a center for the first ellipsoid.
C
         DO J = 1, 3
            CENTR1(J) = SCALE * T_RANDD ( -1.D3, 1.D3, SEED )
         END DO

C
C        Pick a random viewing point.  Start by picking longitude
C        and latitude of a surface point on the first ellipsoid.
C                
         LON = T_RANDD ( -PI(),     PI(),     SEED )
         LAT = T_RANDD ( -HALFPI(), HALFPI(), SEED )

         CALL LATREC ( 1.D0, LON, LAT, SRFDIR )

C
C        Since the origin of this ray is the center of the
C        ellipsoid, we don't have to check the found flag.
C
         CALL SURFPT ( ORIGIN,  SRFDIR, 
     .                 R(1,1),  R(2,1),  R(3,1), XPT, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C        Pick a random altitude.  Note the we move the observer 
C        farther away from the target than in the total occultation
c        cases.  For those, the minimum exponent was -1.
C
         ALT  = SCALE * (  10.D0 ** T_RANDD( 1.D0, 4.D0, SEED )  )

C
C        Find the local outward unit surface normal, scale it by
C        the altitude, and add it to XPT to obtain the view point.
C
         CALL SURFNM ( R(1,1),  R(2,1),  R(3,1), XPT, NORMAL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VLCOM  ( 1.D0, XPT, ALT, NORMAL, VIEWPT )

C
C        Find the offset from the viewing point to the center of the
C        first ellipsoid.
C
         CALL VSUB ( VIEWPT, CENTR1, VPOFF )
         DIST(1)    =  VNORM(VPOFF)

C
C        Let UVPOFF be the unit vector pointing from the view point
C        to the center of the first ellipsoid.
C
         CALL VMINUS ( VPOFF, UVPOFF )
         CALL VHATIP ( UVPOFF )

         
C        Determine the center of the second ellipsoid.  This ellipsoid
C        is centered on the ray from the view point through the center
C        of the first ellipsoid.  
C
C        The second ellipsoid must be placed far enough from the first
C        so that no overlap of the ellipsoids occurs. 
C
C        Pick a random distance between the centers; the distance
C        must be at least 1.25 * the sum of the maximum radii of
C        the ellipsoids.  Here 1.25 is an arbitrary factor "slightly"
C        larger than 1.
C        
         RADSUM = MAXRAD(1) + MAXRAD(2)

         CTRSEP = T_RANDD ( 1.25D0*RADSUM, 100.D0*RADSUM, SEED )

C
C        The second center is placed at distance CTRSEP along
C        the ray emanating from the first center in direction UVPOFF.
C
         CALL VLCOM ( 1.D0, CENTR1, CTRSEP, UVPOFF, CENTR2 )

 
C
C        Now make sure the second ellipsoid is in annular occultation
C        by the first.  We'll adjust the size of the first 
C        ellipsoid if necessary.
C
C        Find a upper bound on the angular radius, as seen from the
C        view point, of the first ellipsoid.
C
         MAXANG(1)  =  DASINE ( MAXRAD(1)/DIST(1),  0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Find the minimum angular radius of the second ellipsoid,
C        based on its nominal radii.
C
         DIST(2) = DIST(1) + CTRSEP

         MAXANG(2) = 0.D0

C         MAXANG(2) = DASINE ( MAXRAD(2)/DIST(2), 0.D0 )
C         CALL CHCKXC ( .FALSE., ' ', OK )

         MINANG(1) = 0.D0
         MINANG(2) = DASINE ( MINRAD(2)/DIST(2), 0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        If we can't guarantee the second ellipsoid is in annular
C        occultation by the first, based on the second ellipsoid's
C        minimum angular radius, shrink the first ellipsoid until this
C        condition is met.
C
         IF ( MINANG(2) .LE. MAXANG(1) ) THEN
C
C           Find the limit on MAXRAD(1).  We pick LIMIT so that
C 
C              ASIN ( LIMIT/DIST(1) ) = MINANG(2)
C
            LIMIT = DIST(1) * SIN ( MINANG(2) )

C
C           Scale down the radii of the first ellipsoid.  Use the
C           arbitrary safety factor 0.75.
C          
            CALL VSCLIP (  0.75D0 * LIMIT/MAXRAD(1),  R(1,1)  )

            MINRAD(1) = MIN ( R(1,1), R(2,1), R(3,1) )
            MAXRAD(1) = MAX ( R(1,1), R(2,1), R(3,1) )

C
C           We must re-create the first semi-axis matrix too.
C
            DO J = 1, 3
               CALL VSCL ( R(J,1), BASIS(1,J,1), SEMAX1(1,J) )
            END DO

         END IF

C
C        Sanity check:  validate MAXANG(1).
C
         MAXANG(1) = DASINE ( MAXRAD(1)/DIST(1), 0.D0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSD ( 'MAXANG(1)',  MAXANG(1),  '<', 
     .                 MINANG(2),    0.D0,       OK   )

C
C
C        We want to find the limb of the *second* ellipsoid as seen
C        from the viewing point.
C
C        Express the viewing point as an offset from the center
C        of the first ellipsoid.
C
         CALL VSUB ( VIEWPT, CENTR2, VPOFF2 )

C
C        Transform the viewing point into the principal axis frame
C        of the second ellipsoid.
C
         CALL MTXV ( BASIS(1,1,2), VPOFF2, LVIEW )

C
C        Find the limb of the second ellipsoid. Rotate the limb back to
C        the original reference frame and shift the limb center to
C        reflect the offset of the second ellipsoid's center from the
C        origin.
C
         CALL EDLIMB ( R(1,2),  R(2,2),  R(3,2), LVIEW, XLIMB )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL EL2CGV ( XLIMB,   V1, V2, V3 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL MXV ( BASIS(1,1,2), V1, LMBCTR )
         CALL MXV ( BASIS(1,1,2), V2, LMBMAJ )
         CALL MXV ( BASIS(1,1,2), V3, LMBMIN )

         CALL VADD ( CENTR2, LMBCTR, V1     )
         CALL VEQU ( V1,             LMBCTR )

C
C        Construct the limb of the second ellipsoid 
C        in the original frame.
C
         CALL CGV2EL ( LMBCTR, LMBMAJ, LMBMIN, LIMB )

C
C        The second ellipsoid should be in annular occultation
C        by the first.  Verify this.
C        
         CODE = ZZOCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE (initial annular occultation)', 
     .                  CODE, '=', ANNLR2, 0, OK                   )


         CODE = T_OCCED ( VIEWPT, CENTR1, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'T_OCCED CODE (initial annular occultation)', 
     .                  CODE, '=', ANNLR2, 0, OK                   )
         

C
C        Repeat with arguments switched.
C
         CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE (initial annular occ/switched)', 
     .                  CODE, '=', ANNLR1, 0, OK                   )


         CODE = T_OCCED ( VIEWPT, CENTR2, SEMAX2, CENTR1, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'T_OCCED CODE (initial annular occ/switched)', 
     .                  CODE, '=', ANNLR1, 0, OK                   )
         


C
C        Now we're going to displace the first ellipsoid in
C        a direction orthogonal to the vector UVPOFF.  We'll
C        do this by selecting a rotation axis and rotating the
C        vector from the viewpoint to the center of the first
C        ellipsoid about this axis until we detect a change of
C        transit classification (from annular to partial).
C        We'll then make sure that both ZZOCCED and our alternative
C        computation performed by T_OCCED agree that we have a 
C        transition at the same angle.
C
C        We're now going to pick a random vector orthogonal to 
C        UVPOFF.  First pick an orthogonal basis, with UVPOFF the
C        first vector of the basis.
C
         CALL FRAME ( UVPOFF, Y, Z )

C
C        Pick a random rotation angle; we'll rotate Y about X by
C        this angle to create a rotation axis.
C
         ANGLE = T_RANDD ( -PI(), PI(), SEED )

         CALL VROTV ( Y, UVPOFF, ANGLE, AXIS )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We're now going to bracket the rotation angle needed to find
C        the transit state transition from annular to partial. If
C        the vector from the view point to the center of the *first*
C        ellipsoid intersects the limb of the *second* ellipsoid, we
C        definitely have a partial transit, so determine the
C        angular displacement required to make this happen.
C
C        Create the plane containing the second center and normal
C        to the rotation axis.
C
         CALL NVP2PL ( AXIS, CENTR2, CHOP )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Chop the second limb with the plane.
C
         CALL INELPL ( LIMB, CHOP, NXPTS, XPT1, XPT2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        If we don't have two points of intersection, something's
C        wrong.
C
         CALL CHCKSI ( 'NXPTS', NXPTS, '=', 2, 0, OK )

C
C        Find the vector from the view point to the first
C        intersection point XPT1; find the angular separation
C        of this vector from UVPOFF.
C
         CALL VSUB ( XPT1, VIEWPT, VPXPT1 )

         XSEP = VSEP ( VPXPT1, UVPOFF )

C
C        Determine the sign of the rotation angle by which
C        we rotate UVPOFF to align it with VPXPT1.
C        
         CALL VROTV ( UVPOFF, AXIS, XSEP, TSTVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( VSEP(TSTVEC, VPXPT1) .GT. ANGTOL ) THEN
            
            XSEP = -XSEP
            CALL VROTV ( UVPOFF, AXIS, XSEP, TSTVEC )

         END IF

         CALL CHCKSD ( 'VSEP(TSTVEC, VPXPT1)', VSEP(TSTVEC, VPXPT1),
     .                 '~', 0.D0, ANGTOL, OK )

C
C        Time for another sanity check:  rotate the vector from
C        the viewpoint to the center of the first ellipsoid about
C        AXIS by XSEP, then add this to the view point,
C        yielding a new center vector for the first ellipsoid.
C        Verify that the first ellipsoid is in partial transit.
C
         CALL VSUB  ( CENTR1, VIEWPT, VPCTR1 )

         CALL VROTV ( VPCTR1, AXIS, XSEP, RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Sanity check:  make sure RVEC points from the view point
C        to XPT1.
C        
         CALL CHCKSD ( 'VSEP(RVEC, VPXPT1)', VSEP(RVEC, VPXPT1),
     .                 '~', 0.D0, ANGTOL, OK )


         CALL VADD  ( VIEWPT, RVEC, NEWCTR )
         
C         CALL VSUB  ( NEWCTR, VIEWPT, TSTVEC )
C         WRITE (*,*) 'VPXPT1 sep = ', VSEP ( TSTVEC, VPXPT1 )
C         WRITE (*,*) 'VPCTR2 sep = ', VSEP ( TSTVEC, VPCTR2 )

C
C        Check for partial transit of the first ellipsoid across
C        the second.
C
         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSI ( 'T_OCCED CODE, (initial partial transit)', 
     .                 CODE, '=', PARTL2, 0, OK )

         CODE = ZZOCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE, (initial partial transit)', 
     .                 CODE, '=', PARTL2, 0, OK )


C
C        Repeat with arguments switched.
C
         CODE = ZZOCCED ( VIEWPT, CENTR2, SEMAX2, NEWCTR, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'ZZOCCED CODE (initial partial occ/switched)', 
     .                  CODE, '=', PARTL1, 0, OK                   )


         CODE = T_OCCED ( VIEWPT, CENTR2, SEMAX2, NEWCTR, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'T_OCCED CODE (initial partial occ/switched)', 
     .                  CODE, '=', PARTL1, 0, OK                   )
         
C
C        We now know that a rotation of the vector from the 
C        view point to the center of the second ellipsoid about
C        AXIS by an angle between 0 and XSEP radians should yield
C        a state transition.  Find the angle via binary search.
C
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

CXXX
            CODE = ZZOCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            IF ( CODE .EQ. PARTL2 ) THEN

               HIGH = MIDPT

            ELSE
C
C              The code had better be ANNLR2.
C
               CALL CHCKSI ( '(bisection) CODE', CODE, '=', 
     .                       ANNLR2, 0, OK )

               LOW  = MIDPT

            END IF

            DIFF = HIGH - LOW

         END DO
         
C
C        Now that we've dropped out of the loop, verify that T_OCCED
C        says the transit is total at rotation angle RSIGN*LOW and
C        partial at rotation angle RSIGN*HIGH.  We adjust each 
C        of these angles by ADJTOL to allow for differences in 
C        round-off between ZZOCCED and T_OCCED.
C
C
C        Verify that when we switch the order of the ellipsoids,
C        we see a transition from total transit of ellipsoid 1
C        to partial transit of ellipsoid 1.
C
C
C        Check for annular transit:
C
         CALL VROTV ( VPCTR1, AXIS, RSIGN*(LOW-ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (annular)', CODE, '=', 
     .                 ANNLR2, 0, OK )


C
C        IF ( .NOT. OK ) THEN
C
C           WRITE (*,*) '============='
C           WRITE (*,*) 'XSEP = ', XSEP
C           WRITE (*,*) 'MINANG: ', MINANG
C           WRITE (*,*) 'MAXANG: ', MAXANG
C           WRITE (*,*) 'LIMB   = ', LIMB
C           WRITE (*,*) 'VIEWPT = ', VIEWPT
C           WRITE (*,*) 'LOW, HIGH = ', LOW, HIGH
C        END IF



         CODE = T_OCCED ( VIEWPT, CENTR2, SEMAX2, NEWCTR, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (annular, args switched)', 
     .                 CODE, '=', 
     .                 ANNLR1, 0, OK )



C
C        Check for partial transit:
C
         CALL VROTV ( VPCTR1, AXIS, RSIGN*(HIGH+ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (partial 0)', CODE, '=', 
     .                 PARTL2, 0, OK )

C
C        IF ( .NOT. OK ) THEN
C
C           WRITE (*,*) '============='
C           WRITE (*,*) 'XSEP = ', XSEP
C           WRITE (*,*) 'MINANG: ', MINANG
C           WRITE (*,*) 'MAXANG: ', MAXANG
C           WRITE (*,*) 'LIMB   = ', LIMB
C           WRITE (*,*) 'VIEWPT = ', VIEWPT
C           WRITE (*,*) 'LOW, HIGH = ', LOW, HIGH
C        END IF
C
         CODE = T_OCCED ( VIEWPT, CENTR2, SEMAX2, NEWCTR, SEMAX1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (partial 0, args switched)', 
     .                 CODE, '=', 
     .                 PARTL1, 0, OK )


C
C --- Case: ------------------------------------------------------
C
         TITLE = '"General" case:  search for transitions from ' //
     .           'partial transit to no occultation, '       //
     .           'for ellipsoids of different shape, size and '  //
     .           'orientation.  Loop iteration = #.'

         CALL REPMI ( TITLE, '#', I, TITLE )
         CALL TCASE ( TITLE )
C
C        The next step is to test for transitions from partial 
C        transit to no occultation.
C
C        Let BIGSEP be a displacement angle large enough to guarantee
C        that no occultation will be found.
C
         BIGSEP = PI()

         CALL VROTV ( VPCTR1, AXIS, RSIGN*BIGSEP, RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = ZZOCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE', CODE, '=', NOOCC, 0, OK )

C
C        We now know that a rotation of the vector from the 
C        view point to the center of the first ellipsoid about
C        AXIS by an angle between 0 and XSEP radians should yield
C        a state transition.  Find the angle via binary search.
C
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
C
C              The code had better be PARTL2.
C
               CALL CHCKSI ( '(bisection) CODE', CODE, '=', 
     .                       PARTL2, 0, OK )

               LOW  = MIDPT

            END IF

            DIFF = HIGH - LOW

         END DO
         
C
C        Now that we've dropped out of the loop, verify that T_OCCED
C        says the transit is partial at rotation angle RSIGN*LOW and
C        "none" at rotation angle RSIGN*HIGH.  We adjust each 
C        of these angles by CNVTOL to allow for differences in 
C        round-off between ZZOCCED and T_OCCED.
C
C
C        Check for partial transit:
C
         CALL VROTV ( VPCTR1, AXIS, RSIGN*(LOW-ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (partial 1)', CODE, '=', 
     .                 PARTL2, 0, OK )

C
C        Check for no occultation:
C
         CALL VROTV ( VPCTR1, AXIS, RSIGN*(HIGH+ADJTOL), RVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VADD  ( VIEWPT, RVEC, NEWCTR )

         CODE = T_OCCED ( VIEWPT, NEWCTR, SEMAX1, CENTR2, SEMAX2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSI ( 'CODE from T_OCCED (none)', CODE, '=', 
     .                 NOOCC, 0, OK )


      END DO




      CALL T_SUCCESS ( OK )
      RETURN
      END
