!$Procedure ZZGRAV ( SGP4 gravitational constants )

      SUBROUTINE ZZGRAV ( GRAV )

! $ Abstract
!
!      Constants assignments equivalent to 21 July 2006 "getgravconst"
!      by David Vallado, AIAA-2006-6753.
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
!     None.
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'zzsgp4.inc'

      DOUBLE PRECISION      GRAV( NGRAVS, NGRAVC )

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

      GRAV( WGS721, P_RAD)  =  6378.135D0

      GRAV( WGS721, P_XKE)  =  0.0743669161D0

      GRAV( WGS721, P_MU)   =  398600.79964D0

      GRAV( WGS721, P_TUMN) =  1.0D0 / GRAV(WGS721, P_XKE)

      GRAV( WGS721, P_J2)   =  0.001082616D0

      GRAV( WGS721, P_J3)   =  -0.00000253881D0

      GRAV( WGS721, P_J4)   =  -0.00000165597D0

      GRAV( WGS721, P_J3J2) =  GRAV(WGS721, P_J3) / GRAV(WGS721, P_J2)


!
!     WGS 72 constants.
!

      GRAV( WGS72, P_MU)   = 398600.8D0

      GRAV( WGS72, P_RAD)  =  6378.135D0

      GRAV( WGS72, P_XKE)  =  60.0D0 / &
                       DSQRT(GRAV(WGS72, P_RAD)**3/GRAV(WGS72, P_MU))

      GRAV( WGS72, P_TUMN) =  1.0D0 / GRAV(WGS72, P_XKE)

      GRAV( WGS72, P_J2)   =  0.001082616D0

      GRAV( WGS72, P_J3)   =  -0.00000253881D0

      GRAV( WGS72, P_J4)   =  -0.00000165597D0

      GRAV( WGS72, P_J3J2) =  GRAV(WGS72, P_J3) / GRAV(WGS72, P_J2)


!
!     WGS 84 constants.
!

      GRAV( WGS84, P_MU)   =  398600.5D0

      GRAV( WGS84, P_RAD)  =  6378.137D0

      GRAV( WGS84, P_XKE)  =  60.0D0 / &
                       DSQRT(GRAV(WGS84, P_RAD)**3/GRAV(WGS84, P_MU))

      GRAV( WGS84, P_TUMN) =  1.0D0 / GRAV(WGS84, P_XKE)

      GRAV( WGS84, P_J2)   =  0.00108262998905D0

      GRAV( WGS84, P_J3)   =  -0.00000253215306D0

      GRAV( WGS84, P_J4)   =  -0.00000161098761D0

      GRAV( WGS84, P_J3J2) =  GRAV(WGS84, P_J3) / GRAV(WGS84, P_J2)

      END

