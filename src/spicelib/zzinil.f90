!$Procedure ZZINIL ( SGP4 initializer )

      subroutine zzinil ( geophs, opmode, ecco, &
                          epoch,  inclo,  no,     ainv, &
                          ao,     con41,  con42,  cosio, &
                          cosio2, eccsq,  omeosq, posq, &
                          rp,     rteosq, sinio,  gsto )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     This subroutine initializes the SGP4 propagator. All the
!     initialization is consolidated here instead of having multiple
!     loops inside other routines.
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

      include 'zzsgp4.inc'

      real(wp)                 geophs    ( 8 )
      integer(ip)              opmode

      real(wp)                 ecco
      real(wp)                 epoch
      real(wp)                 inclo
      real(wp)                 no
      real(wp)                 ainv
      real(wp)                 ao
      real(wp)                 con41
      real(wp)                 con42
      real(wp)                 cosio
      real(wp)                 cosio2
      real(wp)                 eccsq
      real(wp)                 omeosq
      real(wp)                 posq
      real(wp)                 rp
      real(wp)                 rteosq
      real(wp)                 sinio
      real(wp)                 gsto

! $ Brief_I/O
!
!    See Detailed_input and Detailed_Output.
!
! $ Detailed_Input
!
!     GEOPYS         Gephysical constants array.
!
!     ECCO           Eccentricity.
!
!     EPOCH          TLE epoch time in days from
!                    1950-00-00 00:00:00.000000 TDB, i.e.
!                    1949-12-31 00:00:00.000000 TDB.
!
!     INCLO          Inclination
!
!     OPMODE         Flag indicating which technique
!                    to use to calculate sidereal time.
!
!     NO             Mean motion.
!
! $ Detailed_Output
!
!     NO             Mean motion.
!
!     AINV           1.0/A0
!
!     AO             Semi major axis.
!
!     CON41          Value -CON42-COSIO2-COSIO2.
!
!     CON42          1.0 - 5.0*cos(inclination).
!
!     COSIO          Cosine of inclination.
!
!     COSIO2         COSIO squared.
!
!     ECCSQ          Eccentricity squared.
!
!     OMEOSQ         1.0 - ECCO * ECCO.
!
!     POSQ           Semi-parameter squared.
!
!     RP             Radius of perigee.
!
!     RTEOSQ         Square root of (1.0 - ECCO*ECCO).
!
!     SINIO          Sine of inclination.
!
!     GSTO           GST at time of observation.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(UNKNOWNMODE) signals when the value of OPMODE
!        does not equal an assigned OPMODE value listed in
!        zzsgp4.inc.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is based on the INITL code by David Vallado
!     corresponding to "Revisiting Spacetrack Report #3" [4].
!     The intent is to maintain the original Vallado algorithm,
!     changing code only to meet NAIF format standards and to
!     integrate with SPICELIB.
!
!        Removed getgravconst call, replaced with GEOPHS array.
!
!        Capitalize all variables.
!
!        ENDIF replaced with END IF.
!
!        RadPerDay     replaced with RADDAY
!
!        Opsmode       replaced with OPMODE
!
!        whichconst    eliminated, function provided by GEOPHS
!
! $ Examples
!
!     None.
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
!-    SPICELIB Version 1.0.0, OCT-09-2014 (EDW)
!
!        Based on routine INITL, 28-JUN-2005, Vallado 2006 [4].
!
!-&

! $ Index_Entries
!
!   SGP4
!
!-&

!
!     Local Variables
!

      real(wp)                 adel
      real(wp)                 ak
      real(wp)                 c1
      real(wp)                 c1p2p
      real(wp)                 d1
      real(wp)                 del
      real(wp)                 fk5r
      real(wp)                 j2
      real(wp)                 po
      real(wp)                 radday
      real(wp)                 temp
      real(wp)                 tfrac
      real(wp)                 thgr70
      real(wp)                 ts70
      real(wp)                 tut1
      real(wp)                 x2o3
      real(wp)                 xke

      integer(ip)              ids70

!
!     SPICELIB routines.
!
      real(wp)                 twopi
      logical                  return

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     This code block replaces the call:
!
!     sgp4fix identify constants and allow alternate values.
!
!     CALL getgravconst( whichconst, tumin,
!     .                  mu, radiusearthkm, xke,
!     .                  j2, j3, j4, j3oj2 )
!
      j2    = geophs(k_j2)
      xke   = geophs(k_ke)
      x2o3  = 2.0_wp/3.0_wp

!
!     Calculate auxillary epoch quantities
!
      eccsq  = ecco*ecco
      omeosq = 1.0_wp - eccsq
      rteosq = dsqrt(omeosq)
      cosio  = dcos(inclo)
      cosio2 = cosio*cosio

!
!     Un-KOZAI the mean motion
!
      ak   =  (xke/no)**x2o3
      d1   =  0.75_wp*j2* (3.0_wp*cosio2-1.0_wp) / (rteosq*omeosq)
      del  =  d1/(ak*ak)
      adel =  ak * ( 1.0_wp - del*del - del* &
                       (1.0_wp/3.0_wp + 134.0_wp*del*del / 81.0_wp) )
      del  =  d1/(adel*adel)
      no   =  no/(1.0_wp + del)

      ao   =  (xke/no)**x2o3
      sinio=  dsin(inclo)
      po   =  ao*omeosq
      con42=  1.0_wp-5.0_wp*cosio2
      con41=  -con42-cosio2-cosio2
      ainv =  1.0_wp/ao
      posq =  po*po
      rp   =  ao*(1.0_wp-ecco)

!
!     Calculate greenwich location at epoch
!

!
!     sgp4fix Modern approach to finding sidereal time
!

      if (opmode == imprvd ) then

!
!        Radians per day, earth rotation, 6.30038809866574D0.
!
         radday = twopi() * 1.002737909350795_wp
         temp   = epoch + 2433281.5_wp
         tut1= ( dint(temp-0.5_wp) + 0.5_wp - 2451545.0_wp ) / 36525.0_wp
         gsto= 1.75336855923327_wp + 628.331970688841_wp*tut1 &
                   + 6.77071394490334e-06_wp*tut1*tut1 &
                   - 4.50876723431868e-10_wp*tut1*tut1*tut1 &
                   + radday*( temp-0.5_wp-dint(temp-0.5_wp) )

      else if (opmode == afspc ) then

!
!        sgp4fix Use old way of finding GST
!
!        Count integer number of days from 0 jan 1970
!
         ts70  = epoch-7305.0_wp
         ids70 = int(ts70 + 1.0e-8_wp)
         tfrac = ts70-ids70

!
!        Find greenwich location at epoch
!
         c1     = 1.72027916940703639e-2_wp
         thgr70 = 1.7321343856509374_wp
         fk5r   = 5.07551419432269442e-15_wp
         c1p2p  = c1+twopi()
         gsto   = thgr70+c1*ids70+c1p2p*tfrac+ts70*ts70*fk5r

      else

         call setmsg ( 'Unknown value for OPMODE. Value # not ' &
          //           'coded in zzsgp4.inc.'                         )
         call errint ( '#', opmode                                    )
         call sigerr ( 'SPICE(UNKNOWNMODE)'                           )
         return

      end if

!
!     Check quadrants
!
      gsto = dmod( gsto,twopi() )

      if ( gsto < 0.0_wp ) then
         gsto= gsto + twopi()
      end if

      return
      end
