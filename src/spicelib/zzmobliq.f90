!$Procedure   ZZMOBLIQ   ( Mean obliquity of date )

      SUBROUTINE ZZMOBLIQ ( ET, MOB, DMOB )

! $ Abstract
!
!     Return the mean obliquity of the ecliptic, and its time
!     derivative, at a specified epoch.
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     GEOMETRY
!
! $ Declarations

      IMPLICIT NONE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      MOB
      DOUBLE PRECISION      DMOB

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris time, in seconds past J2000.
!     MOB        O   Mean obliquity of the ecliptic at ET.
!     DMOB       O   Time derivative of the mean obliquity.
!
! $ Detailed_Input
!
!     ET             is the epoch at which the obliquity of the ecliptic
!                    is to be computed.  ET is barycentric dynamical
!                    time, expressed as seconds past J2000.
!
! $ Detailed_Output
!
!     MOB            is the mean obliquity of the ecliptic at epoch ET.
!                    The mean obliquity of the ecliptic is the
!                    inclination of the ecliptic of date to the mean
!                    Earth equator of date.  Output units are radians.
!
!     DMOB           is the time derivative of MOB at ET, expressed in
!                    radians per second.
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
!     The expression for mean is obliquity is
!
!                          ''        ''            ''         2
!        MOBLIQ   =   84381 .448 - 46 .8150 * T - 0 .00059 * T
!
!                      ''          3
!                   + 0 .001813 * T
!
!     where T indicates Julian centuries past J2000.  This is from
!     equation 5-153 of reference [2].
!
! $ Examples
!
!     See the routine ENUTAT for an example of usage.
!
! $ Restrictions
!
!     1)  This is a preliminary version of the routine.
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
!
! $ Author_and_Institution
!
!     W.L. Taber         (JPL)
!     N.J. Bachman       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1 02-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 1.0.0 18-JUL-1997 (WLT)
!
!        Adapted Nat's routine to private version making output
!        rate be radians/sec.
!
!-    Beta Version 1.0.0, 29-SEP-1996 (NJB)
!
!-&

! $ Index_Entries
!
!     compute mean obliquity of date of the ecliptic
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD

!
!     Local parameters
!
!
!     Coefficients for the mean obliquity:
!
      DOUBLE PRECISION      C0
      PARAMETER           ( C0 = 84381.448D0 )

      DOUBLE PRECISION      C1
      PARAMETER           ( C1 = -46.8150D0 )

      DOUBLE PRECISION      C2
      PARAMETER           ( C2 = -0.00059D0 )

      DOUBLE PRECISION      C3
      PARAMETER           ( C3 = 0.001813D0 )

!
!     Local variables
!
      DOUBLE PRECISION      T
      DOUBLE PRECISION      RAD
      DOUBLE PRECISION      YEAR
      DOUBLE PRECISION      PERSEC

      LOGICAL               FIRST
      SAVE

      DATA                  FIRST / .TRUE. /

      IF ( FIRST ) THEN

         FIRST  = .FALSE.
         YEAR   = JYEAR()
         RAD    = RPD  ()
         PERSEC = 1.0D0 / ( YEAR * 100.0D0 )

      END IF

!
!     Convert the input epoch to Julian centuries past J2000:
!
      T  =  ( ET / YEAR )  / 100.D0

!
!     Compute the obliquity at epoch.  The polynomial yields arcseconds;
!     convert the units to radians.
!
      MOB  = ( RAD / 3.6D3 ) * (  C0 + T*( C1   + T*( C2 + T*C3 ) )  )

      DMOB = ( RAD / 3.6D3 ) * (  C1 + T*( 2*C2 + T*3*C3 )  ) * PERSEC

      END
