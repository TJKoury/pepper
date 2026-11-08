!$Procedure   ZZMOBLIQ   ( Mean obliquity of date )

      subroutine zzmobliq ( et, mob, dmob )

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

      implicit none
      double precision      et
      double precision      mob
      double precision      dmob

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
      double precision      jyear
      double precision      rpd

!
!     Local parameters
!
!
!     Coefficients for the mean obliquity:
!
      double precision      c0
      parameter           ( c0 = 84381.448d0 )

      double precision      c1
      parameter           ( c1 = -46.8150d0 )

      double precision      c2
      parameter           ( c2 = -0.00059d0 )

      double precision      c3
      parameter           ( c3 = 0.001813d0 )

!
!     Local variables
!
      double precision      t
      double precision      rad
      double precision      year
      double precision      persec

      logical               first
      save

      data                  first / .true. /

      if ( first ) then

         first  = .false.
         year   = jyear()
         rad    = rpd  ()
         persec = 1.0d0 / ( year * 100.0d0 )

      end if

!
!     Convert the input epoch to Julian centuries past J2000:
!
      t  =  ( et / year )  / 100.d0

!
!     Compute the obliquity at epoch.  The polynomial yields arcseconds;
!     convert the units to radians.
!
      mob  = ( rad / 3.6d3 ) * (  c0 + t*( c1   + t*( c2 + t*c3 ) )  )

      dmob = ( rad / 3.6d3 ) * (  c1 + t*( 2*c2 + t*3*c3 )  ) * persec

      end
