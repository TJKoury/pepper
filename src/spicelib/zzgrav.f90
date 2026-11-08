!$Procedure ZZGRAV ( SGP4 gravitational constants )

      subroutine zzgrav ( grav )
      use pepper_kinds, only: wp

! $ Abstract
!
!      Constants assignments equivalent to 21 July 2006 "getgravconst"
!      by David Vallado, AIAA-2006-6753.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations

      implicit none

      include               'zzsgp4.inc'

      real(wp)              grav( ngravs, ngravc )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     GRAV       O   Array of geophysical constants.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     GRAV       An array of three geophysical constants vectors.
!
! $ Parameters
!
!     Refer to include file zzsgp4.inc.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!           INCLUDE               'zzsgp4.inc'
!
!           CALL ZZGRAV ( GRAV )
!
!     C
!     C     Retrieve the constants set based on the WGS ID parameter.
!     C
!
!           MU   = GRAV( P_WGS, P_MU)
!           RAD  = GRAV( P_WGS, P_RAD)
!           XKE  = GRAV( P_WGS, P_XKE)
!           TUMN = GRAV( P_WGS, P_TUMN)
!           J2   = GRAV( P_WGS, P_J2)
!           J3   = GRAV( P_WGS, P_J3)
!           J4   = GRAV( P_WGS, P_J4)
!           J3OJ2= GRAV( P_WGS, P_J3J2)
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
!       Propagation of the NORAD Element Sets." Spacetrack Report #3.
!       U.S. Air Force: Aerospace Defense Command.
!
!   [2] Hoots, Felix R. "Spacetrack Report #6: Models for Propagation
!       of Space Command Element Sets." Space Command,
!       U. S. Air Force, CO.
!
!   [3] Hoots, Felix R., P. W. Schumacher, and R. A. Glover. 2004.
!       History of Analytical Orbit Modeling in the U. S. Space
!       Surveillance System. Journal of Guidance, Control, and
!       Dynamics. 27(2):174-185.
!
!   [4] Vallado, David, Crawford, Paul, Hujsak, Richard,
!       and Kelso, T.S. 2006. Revisiting Spacetrack Report #3. Paper
!       AIAA 2006-6753 presented at the AIAA/AAS Astrodynamics
!       Specialist Conference, August 21-24, 2006. Keystone, CO.
!
! $ Author_and_Institution
!
!     David Vallado   (AGI)
!     E. D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0 22-JUL-2014 (EDW)
!
!-&

! $ Index_Entries
!
!   SGP4
!
!-&

!
!     The definition of the MU constant is to allow calculation of
!     those constants which are functions of MU in the WGS72 and WGS84
!     assignments.
!


!
!     WGS72 low precision Spacetrack #3 constants. These values should
!     correspond to those in geophysical.ker. If not, something is
!     very wrong.
!

      grav( wgs721, p_rad)  =  6378.135_wp

      grav( wgs721, p_xke)  =  0.0743669161_wp

      grav( wgs721, p_mu)   =  398600.79964_wp

      grav( wgs721, p_tumn) =  1.0_wp / grav(wgs721, p_xke)

      grav( wgs721, p_j2)   =  0.001082616_wp

      grav( wgs721, p_j3)   =  -0.00000253881_wp

      grav( wgs721, p_j4)   =  -0.00000165597_wp

      grav( wgs721, p_j3j2) =  grav(wgs721, p_j3) / grav(wgs721, p_j2)


!
!     WGS 72 constants.
!

      grav( wgs72, p_mu)   = 398600.8_wp

      grav( wgs72, p_rad)  =  6378.135_wp

      grav( wgs72, p_xke)  =  60.0_wp / &
                       dsqrt(grav(wgs72, p_rad)**3/grav(wgs72, p_mu))

      grav( wgs72, p_tumn) =  1.0_wp / grav(wgs72, p_xke)

      grav( wgs72, p_j2)   =  0.001082616_wp

      grav( wgs72, p_j3)   =  -0.00000253881_wp

      grav( wgs72, p_j4)   =  -0.00000165597_wp

      grav( wgs72, p_j3j2) =  grav(wgs72, p_j3) / grav(wgs72, p_j2)


!
!     WGS 84 constants.
!

      grav( wgs84, p_mu)   =  398600.5_wp

      grav( wgs84, p_rad)  =  6378.137_wp

      grav( wgs84, p_xke)  =  60.0_wp / &
                       dsqrt(grav(wgs84, p_rad)**3/grav(wgs84, p_mu))

      grav( wgs84, p_tumn) =  1.0_wp / grav(wgs84, p_xke)

      grav( wgs84, p_j2)   =  0.00108262998905_wp

      grav( wgs84, p_j3)   =  -0.00000253215306_wp

      grav( wgs84, p_j4)   =  -0.00000161098761_wp

      grav( wgs84, p_j3j2) =  grav(wgs84, p_j3) / grav(wgs84, p_j2)

      end

