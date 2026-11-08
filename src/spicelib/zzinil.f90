!$Procedure ZZINIL ( SGP4 initializer )

      SUBROUTINE ZZINIL ( GEOPHS, OPMODE, ECCO, &
                          EPOCH,  INCLO,  NO,     AINV, &
                          AO,     CON41,  CON42,  COSIO, &
                          COSIO2, ECCSQ,  OMEOSQ, POSQ, &
                          RP,     RTEOSQ, SINIO,  GSTO )

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

      IMPLICIT NONE

      INCLUDE 'zzsgp4.inc'

      DOUBLE PRECISION         GEOPHS    ( 8 )
      INTEGER                  OPMODE

      DOUBLE PRECISION         ECCO
      DOUBLE PRECISION         EPOCH
      DOUBLE PRECISION         INCLO
      DOUBLE PRECISION         NO
      DOUBLE PRECISION         AINV
      DOUBLE PRECISION         AO
      DOUBLE PRECISION         CON41
      DOUBLE PRECISION         CON42
      DOUBLE PRECISION         COSIO
      DOUBLE PRECISION         COSIO2
      DOUBLE PRECISION         ECCSQ
      DOUBLE PRECISION         OMEOSQ
      DOUBLE PRECISION         POSQ
      DOUBLE PRECISION         RP
      DOUBLE PRECISION         RTEOSQ
      DOUBLE PRECISION         SINIO
      DOUBLE PRECISION         GSTO

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

      DOUBLE PRECISION         ADEL
      DOUBLE PRECISION         AK
      DOUBLE PRECISION         C1
      DOUBLE PRECISION         C1P2P
      DOUBLE PRECISION         D1
      DOUBLE PRECISION         DEL
      DOUBLE PRECISION         FK5R
      DOUBLE PRECISION         J2
      DOUBLE PRECISION         PO
      DOUBLE PRECISION         RADDAY
      DOUBLE PRECISION         TEMP
      DOUBLE PRECISION         TFRAC
      DOUBLE PRECISION         THGR70
      DOUBLE PRECISION         TS70
      DOUBLE PRECISION         TUT1
      DOUBLE PRECISION         X2O3
      DOUBLE PRECISION         XKE

      INTEGER                  IDS70


!
!     SPICELIB routines.
!
      DOUBLE PRECISION         TWOPI
      LOGICAL                  RETURN

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZINIL' )


!
!     This code block replaces the call:
!
!     sgp4fix identify constants and allow alternate values.
!
!     CALL getgravconst( whichconst, tumin,
!     .                  mu, radiusearthkm, xke,
!     .                  j2, j3, j4, j3oj2 )
!
      J2    = GEOPHS(K_J2)
      XKE   = GEOPHS(K_KE)
      X2O3  = 2.0D0/3.0D0

!
!     Calculate auxillary epoch quantities
!
      ECCSQ  = ECCO*ECCO
      OMEOSQ = 1.0D0 - ECCSQ
      RTEOSQ = DSQRT(OMEOSQ)
      COSIO  = DCOS(INCLO)
      COSIO2 = COSIO*COSIO

!
!     Un-KOZAI the mean motion
!
      AK   =  (XKE/NO)**X2O3
      D1   =  0.75D0*J2* (3.0D0*COSIO2-1.0D0) / (RTEOSQ*OMEOSQ)
      DEL  =  D1/(AK*AK)
      ADEL =  AK * ( 1.0D0 - DEL*DEL - DEL* &
                       (1.0D0/3.0D0 + 134.0D0*DEL*DEL / 81.0D0) )
      DEL  =  D1/(ADEL*ADEL)
      NO   =  NO/(1.0D0 + DEL)

      AO   =  (XKE/NO)**X2O3
      SINIO=  DSIN(INCLO)
      PO   =  AO*OMEOSQ
      CON42=  1.0D0-5.0D0*COSIO2
      CON41=  -CON42-COSIO2-COSIO2
      AINV =  1.0D0/AO
      POSQ =  PO*PO
      RP   =  AO*(1.0D0-ECCO)

!
!     Calculate greenwich location at epoch
!

!
!     sgp4fix Modern approach to finding sidereal time
!

      IF (OPMODE .EQ. IMPRVD ) THEN

!
!        Radians per day, earth rotation, 6.30038809866574D0.
!
         RADDAY = TWOPI() * 1.002737909350795D0
         TEMP   = EPOCH + 2433281.5D0
         TUT1= ( DINT(TEMP-0.5D0) + 0.5D0 - 2451545.0D0 ) / 36525.0D0
         GSTO= 1.75336855923327D0 + 628.331970688841D0*TUT1 &
                   + 6.77071394490334D-06*TUT1*TUT1 &
                   - 4.50876723431868D-10*TUT1*TUT1*TUT1 &
                   + RADDAY*( TEMP-0.5D0-DINT(TEMP-0.5D0) )

      ELSE IF (OPMODE .EQ. AFSPC ) THEN

!
!        sgp4fix Use old way of finding GST
!
!        Count integer number of days from 0 jan 1970
!
         TS70  = EPOCH-7305.0D0
         IDS70 = INT(TS70 + 1.0D-8)
         TFRAC = TS70-IDS70

!
!        Find greenwich location at epoch
!
         C1     = 1.72027916940703639D-2
         THGR70 = 1.7321343856509374D0
         FK5R   = 5.07551419432269442D-15
         C1P2P  = C1+TWOPI()
         GSTO   = THGR70+C1*IDS70+C1P2P*TFRAC+TS70*TS70*FK5R

      ELSE

         CALL SETMSG ( 'Unknown value for OPMODE. Value # not ' &
          //           'coded in zzsgp4.inc.'                         )
         CALL ERRINT ( '#', OPMODE                                    )
         CALL SIGERR ( 'SPICE(UNKNOWNMODE)'                           )
         CALL CHKOUT ( 'ZZINIL'                                       )
         RETURN

      END IF

!
!     Check quadrants
!
      GSTO = DMOD( GSTO,TWOPI() )

      IF ( GSTO .LT. 0.0D0 ) THEN
         GSTO= GSTO + TWOPI()
      END IF

      CALL CHKOUT ( 'ZZINIL' )

      RETURN
      END
