!$Procedure ZZENUT80 ( Earth nutation transformation, IAU 1980 model )

      SUBROUTINE ZZENUT80 ( ET, NUTXF )
      IMPLICIT NONE

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute the state transformation matrix implementing the IAU 1980
!     nutation model.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     FRAMES
!     MATRIX
!     PRIVATE
!     TRANSFORMATION
!     UTILITY
!
! $ Declarations

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      NUTXF  ( 6, 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     ET         I   Ephemeris time, seconds past J2000.
!     NUTXF      O   Nutation transformation matrix.
!
! $ Detailed_Input
!
!     ET             is an epoch, expressed as seconds past J2000 TDB.
!
! $ Detailed_Output
!
!     NUTXF          is a state transformation matrix that maps states
!                    from the earth mean equator and equinox of date
!                    frame (based on the 1976 IAU precession model) to
!                    the earth true equator and equinox frame of date
!                    (based on the 1980 IAU nutation model).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See the private SPICELIB routine ZZWAHR for a discussion
!     of the implementation of the 1980 IAU nutation model.
!
!     See the private SPICELIB routine ZZMOBLIQ for a discussion
!     of the implementation of the 1980 IAU earth mean obliquity
!     of date model.
!
! $ Examples
!
!     See ZZDYNFRM.
!
! $ Restrictions
!
!     1) This is a SPICE private routine; the routine is subject
!        to change without notice.  User applications should not
!        call this routine.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Literature_References
!
!     [1] "Explanatory Supplement to the Astronomical Almanac"
!          edited by P. Kenneth Seidelmann. University Science
!          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992)
!
!     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and
!         Acceleration Vectors of Tracking Station" by T. D. Moyer.
!         Draft of JPL Publication documenting the JPL navigation
!         program "Regres."
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      DOUBLE PRECISION      DMOB
      DOUBLE PRECISION      DVNUT  ( 4 )
      DOUBLE PRECISION      EULANG ( 6 )
      DOUBLE PRECISION      MOB


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZENUT80' )

!
!      Get nutation angles and their rates.  We're expecting
!
!         DVNUT(1) = Psi------nutation in longitude (radians)
!         DVNUT(2) = Epsilon--nutation in obliquity (radians)
!         DVNUT(3) = dPsi/dt     (radians/second)
!         DVNUT(4) = dEpsilon/dt (radians/second)
!
      CALL ZZWAHR ( ET, DVNUT )

!
!     Get the mean obliquity of date.
!
!     We're expecting the outputs to be as follows:
!
!         MOB      is the mean obliquity of the ecliptic at epoch
!                  ET. The mean obliquity of the ecliptic is the
!                  inclination of the ecliptic of date to the
!                  mean Earth equator of date.  Output units are
!                  radians.
!
!         DMOB     is the time derivative of MOB at ET, expressed
!                        in radians per second.

      CALL ZZMOBLIQ ( ET, MOB, DMOB )

!
!     The nutation rotation N is defined by
!
!
!         N = [ -MOB - NUOBL ]  [ -NULON ]   [ MOB ]
!                             1           3         1
!
!     where MOBLIQ is the mean obliquity of the earth's ecliptic
!     at epoch, NUOB is nutation in obliquity at epoch, and
!     NULONG is nutation in longitude at epoch.  Using our
!     variable names, the Euler angle sequence is
!
!        [ -MOB - DVNUT(2) ]  [ -DVNUT(1) ]  [ MOB ]
!                           1              3        1
!
!     The rates corresponding to these angles are:
!
!        -DMOB - DVNUT(4),  -DVNUT(3),  DMOB
!
!     We can use EUL2XF to form the state transformation from
!     the nutation base frame to the nutation frame.
!

      EULANG(1) =  -MOB  - DVNUT(2)
      EULANG(2) =        - DVNUT(1)
      EULANG(3) =   MOB
      EULANG(4) =  -DMOB - DVNUT(4)
      EULANG(5) =        - DVNUT(3)
      EULANG(6) =   DMOB

      CALL EUL2XF ( EULANG, 1, 3, 1, NUTXF )

      CALL CHKOUT ( 'ZZENUT80' )
      RETURN
      END
