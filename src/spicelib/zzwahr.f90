!$Procedure      ZZWAHR ( SPICELIB private version of Newhalls' WAHR )

      subroutine zzwahr ( et, dvnut )

! $ Abstract
!
!     Calculates nutation angles delta psi and delta epsilon,  and
!     their rates of change, referred to the ecliptic of date, from
!     the Wahr series (Table 1,'Proposal to the IAU Working Group
!     on Nutation', John M. Wahr and Martin L. Smith 1979)
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     NUTATIONS
!
! $ Declarations

      implicit none
      double precision      et
      double precision      dvnut(4)

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris Time for which nutations are sought
!     DVNUT      O   Nutation angles and their rates.
!
! $ Detailed_Input
!
!     ET         is the epoch for which nutation angles are being
!                requested expressed in TDB seconds past the epoch
!                of J2000.
!
! $ Detailed_Output
!
!     DVNUT      are the nutation angles and their derivatives.
!                Following the notation on page 112 of the
!                Explanatory Supplement to the Astronomical
!                Almanac we have
!
!                DVNUT(1) = Psi------nutation in longitude (radians)
!                DVNUT(2) = Epsilon--nutation in obliquity (radians)
!                DVNUT(3) = dPsi/dt     (radians/second)
!                DVNUT(4) = dEpsilon/dt (radians/second)
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
!     This routine computes the angles required for computing the
!     transformation from the mean of date frame for the earth
!     to the true of date frame of the earth.
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
!     Explanatory Supplement to the Astronomical Almanac edited
!     by P. Kenneth Siedelmann. (1992) (University Science
!     Books, Mill Valley CA) pp. 111-116
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 02-OCT-2021 (NJB)
!
!        Corrected typos in comments. Reordered header sections.
!
!-    SPICELIB Version 1.0.0, 15-JUL-1997 (WLT)
!
!        This routine was adapted from a routine provided by
!        Skip Newhall.  Skip's notes indicate that he obtained this
!        from Jay Lieske and Mylse Standish.  The actual notes
!        from the original routine WAHR are given here.
!
!           Lieske 3/91.  NUTATION in the IAU J2000 system.  Univac
!           version obtained from Myles Standish, (subroutine WAHR)
!           who had obtained it from USNO.  Re-ordered terms to match
!           Astronomical Almanac 1984 table S23-S25 and corrected
!           the rate for dPsi in the 0 0 2 -2 2 term.  Eliminated
!           the equivalences, common block and added necessary SAVEs.
!           Corrected the fundamental angles (L, L', F, D, Node) to
!           match Almanac.
!
!        In the current routine the various angles L, L', F, D, and
!        Node (MG) are computed using the actual values given
!        in the Explanatory Supplement.
!
!        Note that there is an error in the Explanatory supplement
!        for the Node term.  The Explanatory Supplement (page 114) has
!
!          OMEGA = 135 degrees 2 minutes 40.280 seconds
!                +  etc.
!
!        The correct formulation should be:
!
!          OMEGA = 125 degrees 2 minutes 40.280 seconds
!                +  etc.
!
!        This is the value used in this routine.  The verification of
!        this error is courtesy of Myles Standish.
!
!
!-&
!
!     SPICELIB Functions
!
      double precision      pi
      double precision      twopi
      double precision      spd

!
!     Parameters
!
!     NTERM is the number of SIN and COSINE terms used in the
!     computation of Delta Psi and Delta epsilon
!
      integer               nterm
      parameter           ( nterm = 106 )

      double precision      zero
      parameter           ( zero  = 0.0d0 )

!
!     The parameters below stand for
!
!        revolutions
!        degrees
!        minutes
!        seconds
!        julian century
!        julian century ** 2
!        julian century ** 3
!
!     These parameters are needed for converting the quantities
!     on page 114 of the Explanatory supplement from revolutions,
!     degrees, minutes and seconds / century, century**2 and century**3
!     to degrees, degrees/day, degrees/(0.0001 days)**2 and
!     degrees/(0.0001 days)**3.
!
      double precision      rev
      parameter           ( rev   = 360.0d0           )

      double precision      deg
      parameter           ( deg   = 1.0d0             )

      double precision      min
      parameter           ( min   = 1.0d0 / 60.0d0    )

      double precision      sec
      parameter           ( sec   = 1.0d0 / 3600.0d0  )

      double precision      cent1
      parameter           ( cent1 = 36525.0d0         )

      double precision      cent2
      parameter           ( cent2 = cent1*cent1*1.0d-8 )

      double precision      cent3
      parameter           ( cent3 = cent1*cent2*1.0d-4 )


!
!     The next set of parameters is an enumeration of the various
!     angles needed in the computation of nutations.
!
      integer               l
      parameter           ( l      = 1 )

      integer               lp
      parameter           ( lp     = l      + 1 )

      integer               f
      parameter           ( f      = lp     + 1 )

      integer               d
      parameter           ( d      = f      + 1 )

      integer               mg
      parameter           ( mg     = d      + 1 )

!
!     Local Variables.
!
      double precision      ce
      double precision      cl
      double precision      cosang
      double precision      angle(5)
      double precision      angrt(5)
      double precision      arg
      double precision      argrt
      double precision      dd
      double precision      dd2
      double precision      dddj
      double precision      dj
      double precision      dpi
      double precision      dtwopi
      double precision      factr
      double precision      oneday
      double precision      radian
      double precision      rasec
      double precision      sinang
      double precision      t

      double precision      d0
      double precision      d1
      double precision      d2
      double precision      d3
      double precision      f0
      double precision      f1
      double precision      f2
      double precision      f3
      double precision      l0
      double precision      l1
      double precision      l2
      double precision      l3
      double precision      lp0
      double precision      lp1
      double precision      lp2
      double precision      lp3
      double precision      mg0
      double precision      mg1
      double precision      mg2
      double precision      mg3

      integer               i
      integer               j
      integer               matrix(9,nterm)

      logical               first
      save

      data                  first / .true. /
!
!     Below are the coefficients for the various periods of the
!     nutation model.  There does not appear to be any particular reason
!     for the ordering selected.  The n'th row corresponds to the n'th
!     period listed above each data statement.
!
!>> Periods: 6798.4, 3399.2, 1305.5, 1095.2, 1615.7, 3232.9, 6786.3,
!             943.2,  182.6,  365.3,  121.7,  365.2,  177.8,  205.9,
!             173.3,  182.6,  386.0,   91.3,  346.6
!
      data ((matrix(i,j),i=1,9),j=1,19)/ &
            0,     0,     0,     0,     1, -171996, -1742, 92025,    89, &
            0,     0,     0,     0,     2,    2062,     2,  -895,     5, &
           -2,     0,     2,     0,     1,      46,     0,   -24,     0, &
            2,     0,    -2,     0,     0,      11,     0,     0,     0, &
           -2,     0,     2,     0,     2,      -3,     0,     1,     0, &
            1,    -1,     0,    -1,     0,      -3,     0,     0,     0, &
            0,    -2,     2,    -2,     1,      -2,     0,     1,     0, &
            2,     0,    -2,     0,     1,       1,     0,     0,     0, &
            0,     0,     2,    -2,     2,  -13187,   -16,  5736,   -31, &
            0,     1,     0,     0,     0,    1426,   -34,    54,    -1, &
            0,     1,     2,    -2,     2,    -517,    12,   224,    -6, &
            0,    -1,     2,    -2,     2,     217,    -5,   -95,     3, &
            0,     0,     2,    -2,     1,     129,     1,   -70,     0, &
            2,     0,     0,    -2,     0,      48,     0,     1,     0, &
            0,     0,     2,    -2,     0,     -22,     0,     0,     0, &
            0,     2,     0,     0,     0,      17,    -1,     0,     0, &
            0,     1,     0,     0,     1,     -15,     0,     9,     0, &
            0,     2,     2,    -2,     2,     -16,     1,     7,     0, &
            0,    -1,     0,     0,     1,     -12,     0,     6,     0/

!
!     Periods: 199.8, 346.6, 212.3, 119.6, 411.8, 131.7, 169.0, 329.8,
!              409.2, 388.3, 117.5,  13.7,  27.6,  13.6,   9.1,  31.8,
!               27.1,  14.8,  27.7
!
      data ((matrix(i,j),i=1,9),j=20,38)/ &
           -2,     0,     0,     2,     1,      -6,     0,     3,     0, &
            0,    -1,     2,    -2,     1,      -5,     0,     3,     0, &
            2,     0,     0,    -2,     1,       4,     0,    -2,     0, &
            0,     1,     2,    -2,     1,       4,     0,    -2,     0, &
            1,     0,     0,    -1,     0,      -4,     0,     0,     0, &
            2,     1,     0,    -2,     0,       1,     0,     0,     0, &
            0,     0,    -2,     2,     1,       1,     0,     0,     0, &
            0,     1,    -2,     2,     0,      -1,     0,     0,     0, &
            0,     1,     0,     0,     2,       1,     0,     0,     0, &
           -1,     0,     0,     1,     1,       1,     0,     0,     0, &
            0,     1,     2,    -2,     0,      -1,     0,     0,     0, &
            0,     0,     2,     0,     2,   -2274,    -2,   977,    -5, &
            1,     0,     0,     0,     0,     712,     1,    -7,     0, &
            0,     0,     2,     0,     1,    -386,    -4,   200,     0, &
            1,     0,     2,     0,     2,    -301,     0,   129,    -1, &
            1,     0,     0,    -2,     0,    -158,     0,    -1,     0, &
           -1,     0,     2,     0,     2,     123,     0,   -53,     0, &
            0,     0,     0,     2,     0,      63,     0,    -2,     0, &
            1,     0,     0,     0,     1,      63,     1,   -33,     0/
!
!     Periods: 27.4, 9.6,  9.1,  7.1, 13.8, 23.9, 6.9, 13.6, 27.0, 32.0,
!              31.7, 9.5, 34.8, 13.2, 14.2,  5.6, 9.6, 12.8, 14.8
!
      data ((matrix(i,j),i=1,9),j=39,57)/ &
           -1,     0,     0,     0,     1,     -58,    -1,    32,     0, &
           -1,     0,     2,     2,     2,     -59,     0,    26,     0, &
            1,     0,     2,     0,     1,     -51,     0,    27,     0, &
            0,     0,     2,     2,     2,     -38,     0,    16,     0, &
            2,     0,     0,     0,     0,      29,     0,    -1,     0, &
            1,     0,     2,    -2,     2,      29,     0,   -12,     0, &
            2,     0,     2,     0,     2,     -31,     0,    13,     0, &
            0,     0,     2,     0,     0,      26,     0,    -1,     0, &
           -1,     0,     2,     0,     1,      21,     0,   -10,     0, &
           -1,     0,     0,     2,     1,      16,     0,    -8,     0, &
            1,     0,     0,    -2,     1,     -13,     0,     7,     0, &
           -1,     0,     2,     2,     1,     -10,     0,     5,     0, &
            1,     1,     0,    -2,     0,      -7,     0,     0,     0, &
            0,     1,     2,     0,     2,       7,     0,    -3,     0, &
            0,    -1,     2,     0,     2,      -7,     0,     3,     0, &
            1,     0,     2,     2,     2,      -8,     0,     3,     0, &
            1,     0,     0,     2,     0,       6,     0,     0,     0, &
            2,     0,     2,    -2,     2,       6,     0,    -3,     0, &
            0,     0,     0,     2,     1,      -6,     0,     3,     0/

!
!     Periods: 7.1, 23.9, 14.7, 29.8, 6.9, 15.4, 26.9, 29.5, 25.6, 9.1,
!              9.4,  9.8, 13.7,  5.5, 7.2,  8.9, 32.6, 13.8, 27.8

      data ((matrix(i,j),i=1,9),j=58,76)/ &
            0,     0,     2,     2,     1,      -7,     0,     3,     0, &
            1,     0,     2,    -2,     1,       6,     0,    -3,     0, &
            0,     0,     0,    -2,     1,      -5,     0,     3,     0, &
            1,    -1,     0,     0,     0,       5,     0,     0,     0, &
            2,     0,     2,     0,     1,      -5,     0,     3,     0, &
            0,     1,     0,    -2,     0,      -4,     0,     0,     0, &
            1,     0,    -2,     0,     0,       4,     0,     0,     0, &
            0,     0,     0,     1,     0,      -4,     0,     0,     0, &
            1,     1,     0,     0,     0,      -3,     0,     0,     0, &
            1,     0,     2,     0,     0,       3,     0,     0,     0, &
            1,    -1,     2,     0,     2,      -3,     0,     1,     0, &
           -1,    -1,     2,     2,     2,      -3,     0,     1,     0, &
           -2,     0,     0,     0,     1,      -2,     0,     1,     0, &
            3,     0,     2,     0,     2,      -3,     0,     1,     0, &
            0,    -1,     2,     2,     2,      -3,     0,     1,     0, &
            1,     1,     2,     0,     2,       2,     0,    -1,     0, &
           -1,     0,     2,    -2,     1,      -2,     0,     1,     0, &
            2,     0,     0,     0,     1,       2,     0,    -1,     0, &
            1,     0,     0,     0,     2,      -2,     0,     1,     0/

!
!      Periods: 9.2,  9.3, 27.3, 10.1, 14.6,  5.8, 15.9, 22.5,  5.6,
!               7.3,  9.1, 29.3, 12.8,  4.7,  9.6, 12.7,  8.7, 23.8,
!              13.1
      data ((matrix(i,j),i=1,9),j=77,95)/ &
            3,     0,     0,     0,     0,       2,     0,     0,     0, &
            0,     0,     2,     1,     2,       2,     0,    -1,     0, &
           -1,     0,     0,     0,     2,       1,     0,    -1,     0, &
            1,     0,     0,    -4,     0,      -1,     0,     0,     0, &
           -2,     0,     2,     2,     2,       1,     0,    -1,     0, &
           -1,     0,     2,     4,     2,      -2,     0,     1,     0, &
            2,     0,     0,    -4,     0,      -1,     0,     0,     0, &
            1,     1,     2,    -2,     2,       1,     0,    -1,     0, &
            1,     0,     2,     2,     1,      -1,     0,     1,     0, &
           -2,     0,     2,     4,     2,      -1,     0,     1,     0, &
           -1,     0,     4,     0,     2,       1,     0,     0,     0, &
            1,    -1,     0,    -2,     0,       1,     0,     0,     0, &
            2,     0,     2,    -2,     1,       1,     0,    -1,     0, &
            2,     0,     2,     2,     2,      -1,     0,     0,     0, &
            1,     0,     0,     2,     1,      -1,     0,     0,     0, &
            0,     0,     4,    -2,     2,       1,     0,     0,     0, &
            3,     0,     2,    -2,     2,       1,     0,     0,     0, &
            1,     0,     2,    -2,     0,      -1,     0,     0,     0, &
            0,     1,     2,     0,     1,       1,     0,     0,     0/

!
!     Periods: 35.0, 13.6, 25.4, 14.2, 9.5, 14.2, 34.7, 32.8, 7.1, 4.8,
!              27.3
      data ((matrix(i,j),i=1,9),j=96,nterm)/ &
           -1,    -1,     0,     2,     1,       1,     0,     0,     0, &
            0,     0,    -2,     0,     1,      -1,     0,     0,     0, &
            0,     0,     2,    -1,     2,      -1,     0,     0,     0, &
            0,     1,     0,     2,     0,      -1,     0,     0,     0, &
            1,     0,    -2,    -2,     0,      -1,     0,     0,     0, &
            0,    -1,     2,     0,     1,      -1,     0,     0,     0, &
            1,     1,     0,    -2,     1,      -1,     0,     0,     0, &
            1,     0,    -2,     2,     0,      -1,     0,     0,     0, &
            2,     0,     0,     2,     0,       1,     0,     0,     0, &
            0,     0,     2,     4,     2,      -1,     0,     0,     0, &
            0,     1,     0,     1,     0,       1,     0,     0,     0/


      if (first) then

         first  = .false.
         dpi    = pi()
         dtwopi = twopi()
         radian =  180.0d0 / dpi
         rasec  = 3600.0d0 * radian
         factr  =    1.0d4 * rasec
         oneday = spd()



!
!        The following values are direct conversions to degrees from
!        page 114 of the Explanatory Supplement to the Astronomical
!        Almanac.
!
!        L0 through L3 are the coefficients for l---the mean longitude
!        of the Moon minus the mean longitude of the Moon's perigee.
!        Units for the various terms:
!
!           L0      degrees
!           L1      degrees/day
!           L2      degrees/(0.0001 days)**2
!           L3      degrees/(0.0001 days)**3
!
         l0 =                134.0d0*deg + 57.0d0*min + 46.733d0*sec
         l1 =(1325.0d0*rev + 198.0d0*deg + 52.0d0*min +  2.633d0*sec) &
                             / cent1
         l2 =   31.310d0*sec / cent2
         l3 =    0.064d0*sec / cent3



!
!        LP0 through LP3 are the coefficients for l'---the mean
!        longitude of the Sun minus the mean longitude of the Sun's
!        perigee. Units for the various terms:
!
!           LP0      degrees
!           LP1      degrees/day
!           LP2      degrees/(0.0001 days)**2
!           LP3      degrees/(0.0001 days)**3
!
         lp0 =               357.0d0*deg + 31.0d0*min + 39.804d0*sec
         lp1 = (99.0d0*rev + 359.0d0*deg +  3.0d0*min +  1.224d0*sec) &
                              / cent1
         lp2 =   -0.577d0*sec / cent2
         lp3 =   -0.012d0*sec / cent3


!
!        F0 through F3 are the coefficients for F---the mean longitude
!        of the Moon minus the mean longitude of the Moon's node. Units
!        for the various terms:
!
!           F0      degrees
!           F1      degrees/day
!           F2      degrees/(0.0001 days)**2
!           F3      degrees/(0.0001 days)**3
!
         f0 =                 93.0d0*deg + 16.0d0*min + 18.877d0*sec
         f1 = (1342.0d0*rev + 82.0d0*deg +  1.0d0*min +  3.137d0*sec) &
                              / cent1
         f2 =   -13.257d0*sec / cent2
         f3 =     0.011d0*sec / cent3

!
!        D0 through D3 are the coefficients for D---the mean longitude
!        of the Moon minus the mean longitude of the Sun. Units
!        for the various terms:
!
!           D0      degrees
!           D1      degrees/day
!           D2      degrees/(0.0001 days)**2
!           D3      degrees/(0.0001 days)**3
!
         d0 =                297.0d0*deg + 51.0d0*min +  1.307d0*sec
         d1 =(1236.0d0*rev + 307.0d0*deg +  6.0d0*min + 41.328d0*sec) &
                             / cent1
         d2 =   -6.891d0*sec / cent2
         d3 =    0.019d0*sec / cent3

!
!        MG0 through MG3 are the coefficients for Omega---the longitude
!        of the mean ascending node of the lunar orbit on the ecliptic
!        measured from the mean equinox of date.  NOTE: The constant
!        term MG0 is correct.  The value
!               o
!            135 02' 40".280
!
!        given in the Explanatory Supplement page 114 has a typo.  The
!        correct value is the one used here:
!
!               o
!            125 02' 40".280
!
!           MG0      degrees
!           MG1      degrees/day
!           MG2      degrees/(0.0001 days)**2
!           MG3      degrees/(0.0001 days)**3
!
         mg0 =               125.0d0*deg + 2.0d0*min + 40.280d0*sec
         mg1 =-( 5.0d0*rev + 134.0d0*deg + 8.0d0*min + 10.539d0*sec) &
                              / cent1
         mg2 =    7.455d0*sec / cent2
         mg3 =    0.008d0*sec / cent3



      end if

!
!     Compute all of the various time components.  DJ is the delta
!     in the Julian date from the J2000 epoch.
!
      dj   = et/oneday
      dd   = dj/1.0d4
      dddj = dd/1.0d4
      dd2  = dd*dd
      t    = dj/365250.d0

!
!     Now compute all of the various angles and their rates
!     at the current epoch
!
      angle(l)  = l0   +  dj*l1   +  (l2  + dd*l3 )*dd2
      angle(lp) = lp0  +  dj*lp1  +  (lp2 + dd*lp3)*dd2
      angle(f)  = f0   +  dj*f1   +  (f2  + dd*f3 )*dd2
      angle(d)  = d0   +  dj*d1   +  (d2  + dd*d3 )*dd2
      angle(mg) = mg0  +  dj*mg1  +  (mg2 + dd*mg3)*dd2

      angrt(l ) = l1   +  dddj * (2.0d0*l2  + 3.0d0*dd*l3 )
      angrt(lp) = lp1  +  dddj * (2.0d0*lp2 + 3.0d0*dd*lp3)
      angrt(f ) = f1   +  dddj * (2.0d0*f2  + 3.0d0*dd*f3 )
      angrt(d ) = d1   +  dddj * (2.0d0*d2  + 3.0d0*dd*d3 )
      angrt(mg) = mg1  +  dddj * (2.0d0*mg2 + 3.0d0*dd*mg3)

!
!     Wrap all of the angles and rates to range from 0 to 360, then
!     convert to radians.
!
      do j = 1,5
         angle(j) = dmod(angle(j), 360.d0)
         angrt(j) = dmod(angrt(j), 360.d0)

         angle(j) = angle(j)/radian
         angrt(j) = angrt(j)/radian
      end do
!
!     Zero out the components of the nutation array
!
      do j = 1,4
         dvnut(j) = zero
      end do
!
!     Now we accumulate the various terms of Delta Psi and Delta
!     epsilon as expressed on page 115 of the Green Book
!     (Explanatory Supplement to the Astronomical Almanac).
!
      do i = 1,nterm

         arg   = zero
         argrt = zero

         do j = 1,5
            if ( matrix(j,i) .ne. 0 ) then
               arg   = arg   + matrix(j,i)*angle(j)
               argrt = argrt + matrix(j,i)*angrt(j)
               arg   = mod(arg, dtwopi)
            end if
         end do

         cl = matrix(6,i)

         if ( matrix(7,i) .ne. 0 ) then
            cl = cl + matrix(7,i)*t
         end if

         ce = matrix(8,i)

         if ( matrix(9,i) .ne. 0 ) then
            ce = ce + matrix(9,i)*t
         end if

         cosang   = dcos(arg)
         sinang   = dsin(arg)

         dvnut(1) = dvnut(1) + cl*sinang/factr
         dvnut(2) = dvnut(2) + ce*cosang/factr
         dvnut(3) = dvnut(3) + cl*cosang*argrt/factr
         dvnut(4) = dvnut(4) - ce*sinang*argrt/factr

      end do

!
!     Finally convert DVNUT(3) and DVNUT(4) to radians/second
!
      dvnut(3) = dvnut(3)/oneday
      dvnut(4) = dvnut(4)/oneday


      return
      end
