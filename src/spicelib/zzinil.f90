!$Procedure ZZINIL ( SGP4 initializer )

      subroutine zzinil ( geophs, opmode, ecco, &
                          epoch,  inclo,  no,     ainv, &
                          ao,     con41,  con42,  cosio, &
                          cosio2, eccsq,  omeosq, posq, &
                          rp,     rteosq, sinio,  gsto )

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

      double precision         geophs    ( 8 )
      integer                  opmode

      double precision         ecco
      double precision         epoch
      double precision         inclo
      double precision         no
      double precision         ainv
      double precision         ao
      double precision         con41
      double precision         con42
      double precision         cosio
      double precision         cosio2
      double precision         eccsq
      double precision         omeosq
      double precision         posq
      double precision         rp
      double precision         rteosq
      double precision         sinio
      double precision         gsto

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

      double precision         adel
      double precision         ak
      double precision         c1
      double precision         c1p2p
      double precision         d1
      double precision         del
      double precision         fk5r
      double precision         j2
      double precision         po
      double precision         radday
      double precision         temp
      double precision         tfrac
      double precision         thgr70
      double precision         ts70
      double precision         tut1
      double precision         x2o3
      double precision         xke

      integer                  ids70


!
!     SPICELIB routines.
!
      double precision         twopi
      logical                  return

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'ZZINIL' )


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
      x2o3  = 2.0d0/3.0d0

!
!     Calculate auxillary epoch quantities
!
      eccsq  = ecco*ecco
      omeosq = 1.0d0 - eccsq
      rteosq = dsqrt(omeosq)
      cosio  = dcos(inclo)
      cosio2 = cosio*cosio

!
!     Un-KOZAI the mean motion
!
      ak   =  (xke/no)**x2o3
      d1   =  0.75d0*j2* (3.0d0*cosio2-1.0d0) / (rteosq*omeosq)
      del  =  d1/(ak*ak)
      adel =  ak * ( 1.0d0 - del*del - del* &
                       (1.0d0/3.0d0 + 134.0d0*del*del / 81.0d0) )
      del  =  d1/(adel*adel)
      no   =  no/(1.0d0 + del)

      ao   =  (xke/no)**x2o3
      sinio=  dsin(inclo)
      po   =  ao*omeosq
      con42=  1.0d0-5.0d0*cosio2
      con41=  -con42-cosio2-cosio2
      ainv =  1.0d0/ao
      posq =  po*po
      rp   =  ao*(1.0d0-ecco)

!
!     Calculate greenwich location at epoch
!

!
!     sgp4fix Modern approach to finding sidereal time
!

      if (opmode .eq. imprvd ) then

!
!        Radians per day, earth rotation, 6.30038809866574D0.
!
         radday = twopi() * 1.002737909350795d0
         temp   = epoch + 2433281.5d0
         tut1= ( dint(temp-0.5d0) + 0.5d0 - 2451545.0d0 ) / 36525.0d0
         gsto= 1.75336855923327d0 + 628.331970688841d0*tut1 &
                   + 6.77071394490334d-06*tut1*tut1 &
                   - 4.50876723431868d-10*tut1*tut1*tut1 &
                   + radday*( temp-0.5d0-dint(temp-0.5d0) )

      else if (opmode .eq. afspc ) then

!
!        sgp4fix Use old way of finding GST
!
!        Count integer number of days from 0 jan 1970
!
         ts70  = epoch-7305.0d0
         ids70 = int(ts70 + 1.0d-8)
         tfrac = ts70-ids70

!
!        Find greenwich location at epoch
!
         c1     = 1.72027916940703639d-2
         thgr70 = 1.7321343856509374d0
         fk5r   = 5.07551419432269442d-15
         c1p2p  = c1+twopi()
         gsto   = thgr70+c1*ids70+c1p2p*tfrac+ts70*ts70*fk5r

      else

         call setmsg ( 'Unknown value for OPMODE. Value # not ' &
          //           'coded in zzsgp4.inc.'                         )
         call errint ( '#', opmode                                    )
         call sigerr ( 'SPICE(UNKNOWNMODE)'                           )
         call chkout ( 'ZZINIL'                                       )
         return

      end if

!
!     Check quadrants
!
      gsto = dmod( gsto,twopi() )

      if ( gsto .lt. 0.0d0 ) then
         gsto= gsto + twopi()
      end if

      call chkout ( 'ZZINIL' )

      return
      end
