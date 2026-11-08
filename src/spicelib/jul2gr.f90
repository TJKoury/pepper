!$Procedure JUL2GR (Julian to Gregorian Calendar)

      subroutine jul2gr (  year, month, day, doy )

! $ Abstract
!
!     Convert Year Month and Day on the Julian Calendar
!     to the Gregorian Calendar
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TIME
!
! $ Declarations

      implicit none

      integer               year
      integer               month
      integer               day
      integer               doy

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     YEAR      I-O  Year  of Julian Calendar/Gregorian Calendar
!     MONTH     I-O  Month of Julian Calendar/Gregorian Calendar
!     DAY       I-O  Day of Month in Julian Calendar/Gregorian Calendar
!     DOY        O   Day of Year in Gregorian Calendar
!
! $ Detailed_Input
!
!     YEAR     is an integer representing the year of an epoch, E, in
!              the Julian proleptic calendar. Note that the year 0
!              and negative values are required to represent
!              years in the pre-Christian era (B.C.) A year, Y B.C.,
!              should be represented as -(Y-1).  For example the year
!              435 B.C. should be input as -434.
!
!     MONTH    is an integer representing the month of some epoch, E,
!              in the Julian proleptic calendar. Months
!              outside the usual range from 1 to 12 are converted
!              to the standard range using modular arithmetic and
!              the input year is adjusted appropriately.
!
!
!     DAY      is the day of the month of some epoch, E, in the Julian
!              proleptic calendar.
!
!              Note to input an epoch as the day of a year, set MONTH
!              to 1 and DAY to the day of the year.
!
! $ Detailed_Output
!
!     YEAR     is an integer representing the year of the epoch, E,
!              above in the Gregorian calendar. Note that the year
!              0 (zero) and negative values are used to represent
!              years in the pre-Christian era (B.C.) A year, Y B.C.,
!              is be represented as -(Y-1).  For example the year
!              435 B.C. will be returned as -434.
!
!     MONTH    is an integer representing the month of the epoch, E,
!              above in the Gregorian Calendar calendar.
!
!     DAY      is the day of the month of the epoch, E, above in the
!              Gregorian Calendar
!
!     DOY      is the day of the year of the epoch, E, above in the
!              Gregorian Calendar.
!
! $ Parameters
!
!     None.
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
!     This is a mid-level utility routine to assist in the assignment
!     and presentation of ancient epochs.
!
!     The SPICE software deals primarily with epochs represented on
!     in the Gregorian Calendar. However, the Gregorian calendar
!     was not adopted until October 15, 1582. As a result, epochs
!     prior to that time are usually represented in the Julian
!     proleptic calendar.
!
!     Formally, both calendars can be extended indefinitely forward
!     and backward in time due the algorithmic nature of the
!     determination of calendar representation.
!
!     When converting "parsed" calendar epochs in the SPICE system,
!     you need to first convert to the Gregorian Calendar. From that
!     point the SPICE toolkit can easily convert the epoch to Julian
!     date or seconds past the J2000 epoch.
!
!     This routine allows you to take a numeric representation of
!     an epoch represented in the Julian proleptic calendar and
!     convert that to an epoch in the Gregorian calendar.
!
!     To convert from Gregorian Calendar to Julian proleptic
!     calendar, use the entry point GR2JUL.
!
! $ Examples
!
!     Suppose you need to find the epoch (in seconds past the
!     J2000) of some ancient epoch that occurred at
!     3:00 on March 4 of the year 121 B.C. And that this epoch
!     is based on the Julian proleptic calendar. We first need
!     to convert the Julian Calendar date to the Gregorian Calendar.
!
!     Here's the declarations we'll need
!
!        INTEGER               YEAR
!        INTEGER               MONTH
!        INTEGER               DAY
!        INTEGER               DOY
!
!        DOUBLE PRECISION      TVEC ( 6 )
!        DOUBLE PRECISION      TDB
!
!     You first need to convert the calendar date of this epoch
!     integers. (We don't need to worry about the hours for a moment).
!
!        YEAR  = -120
!        MONTH =  3
!        DAY   =  4
!
!     Convert this Year, Month and Day to the Gregorian Calendar.
!
!        CALL JUL2GR ( YEAR, MONTH, DAY, DOY )
!
!     Now construct a time vector for use in the routine TTRANS.
!     Note now we use the hour component of the epoch (the fourth
!     component of the time vector TVEC).
!
!        TVEC(1) = DBLE( YEAR )
!        TVEC(2) = DBLE( MONTH )
!        TVEC(3) = DBLE( DAY )
!        TVEC(4) = 3.0D0
!        TVEC(5) = 0.0D0
!        TVEC(6) = 0.0D0
!
!     Now the routine TTRANS can convert the time vector from
!     the input YMD format to barycentric dynamical time.
!
!        CALL TTRANS ( 'YDM', 'TDB', TVEC )
!
!        TDB = TVEC(1)
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 02-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.0, 26-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in RMAINI calls.
!
!-    SPICELIB Version 1.1.1, 23-SEP-1999 (WLT)
!
!        Removed the unused variable DPMON.
!
!-    SPICELIB Version 1.1.0, 23-FEB-1998 (WLT)
!
!        The routine was upgraded so that it will handle without
!        error months that are outside the range from 1 to 12.
!
!-    SPICELIB Version 1.0.0, 13-MAR-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Convert from Julian proleptic to Gregorian Calendar
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 23-FEB-1998 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in RMAINI calls.
!
!-&


!
!     Spicelib Functions
!
      integer               lstlti

!
!     Local (in-line) Functions
!
      integer               divble
      integer               gdoy
      integer               gdp001
      integer               gldays
      integer               gydays
      integer               jdoy
      integer               jdp001
      integer               jldays
      integer               jydays

!
!     Local parameters
!
!     We declare the variables that contain the number of days in
!     400 years (Gregorian), 100 years (Gregorian), 4 years and 1 year.
!
      integer               dp400y
      parameter           ( dp400y = 365*400 + 97 )

      integer               dp100y
      parameter           ( dp100y = 365*100 + 24 )

      integer               dp4y
      parameter           ( dp4y   = 365*4   + 1  )

      integer               dp1y
      parameter           ( dp1y   = 365 )


!
!     The following integers give the number of days during the
!     associated month of a non-leap year.
!
      integer               jan
      parameter           ( jan = 31 )

      integer               feb
      parameter           ( feb = 28 )

      integer               mar
      parameter           ( mar = 31 )

      integer               apr
      parameter           ( apr = 30 )

      integer               may
      parameter           ( may = 31 )

      integer               jun
      parameter           ( jun = 30 )

      integer               jul
      parameter           ( jul = 31 )

      integer               aug
      parameter           ( aug = 31 )

      integer               sep
      parameter           ( sep = 30 )

      integer               oct
      parameter           ( oct = 31 )

      integer               nov
      parameter           ( nov = 30 )

      integer               dec
      parameter           ( dec = 31 )

!
!     The integers that follow give the number of days in a normal
!     year that precede the first of the month.
!
      integer               jan0
      parameter           ( jan0 = 0  )

      integer               feb0
      parameter           ( feb0 = jan + jan0 )

      integer               mar0
      parameter           ( mar0 = feb + feb0 )

      integer               apr0
      parameter           ( apr0 = mar + mar0 )

      integer               may0
      parameter           ( may0 = apr + apr0 )

      integer               jun0
      parameter           ( jun0 = may + may0 )

      integer               jul0
      parameter           ( jul0 = jun + jun0 )

      integer               aug0
      parameter           ( aug0 = jul + jul0 )

      integer               sep0
      parameter           ( sep0 = aug + aug0 )

      integer               oct0
      parameter           ( oct0 = sep + sep0 )

      integer               nov0
      parameter           ( nov0 = oct + oct0 )

      integer               dec0
      parameter           ( dec0 = nov + nov0 )

!
!     The integers that follow give the number of days in a leap
!     year that precede the first of the month.
!
      integer               janl0
      parameter           ( janl0 = jan0  )

      integer               febl0
      parameter           ( febl0 = feb0 )

      integer               marl0
      parameter           ( marl0 = mar0 + 1 )

      integer               aprl0
      parameter           ( aprl0 = apr0 + 1 )

      integer               mayl0
      parameter           ( mayl0 = may0 + 1 )

      integer               junl0
      parameter           ( junl0 = jun0 + 1 )

      integer               jull0
      parameter           ( jull0 = jul0 + 1 )

      integer               augl0
      parameter           ( augl0 = aug0 + 1 )

      integer               sepl0
      parameter           ( sepl0 = sep0 + 1 )

      integer               octl0
      parameter           ( octl0 = oct0 + 1 )

      integer               novl0
      parameter           ( novl0 = nov0 + 1 )

      integer               decl0
      parameter           ( decl0 = dec0 + 1 )


!
!     Local variables
!
      integer               dayg
      integer               dayj
      integer               dofyr
      integer               dy
      integer               m
      integer               m100
      integer               m4
      integer               m400
      integer               mon
      integer               offset
      integer               offstg
      integer               offstj
      integer               rdayg
      integer               rdayj
      integer               tmpday
      integer               tmpyr
      integer               yr
      integer               yroff

!
!     The array EXTRA contains the number of additional days that
!     appear before the first of a month during a leap year (as opposed
!     to a non-leap year).
!
      integer               extra  ( 12 )
      integer               dpbegl ( 12 )

!
!     DPJAN0(I) gives the number of days that occur before the I'th
!     month of a normal year.
!
      integer               dpjan0 ( 12 )

      logical               first

!
!     Saved variables
!
      save

!
!     Initial values
!
      data                  first   / .true. /

      data                  extra   /  0,     0,     1,     1, &
                                       1,     1,     1,     1, &
                                       1,     1,     1,     1    /

      data                  dpjan0  /  jan0,  feb0,  mar0,  apr0, &
                                       may0,  jun0,  jul0,  aug0, &
                                       sep0,  oct0,  nov0,  dec0 /

      data                  dpbegl  /  janl0, febl0, marl0, aprl0, &
                                       mayl0, junl0, jull0, augl0, &
                                       sepl0, octl0, novl0, decl0 /
!
!     Definitions of statement functions.
!
!     The number of days elapsed since Gregorian Jan 1, of year 1 A.D.
!     to Jan 1 of YR is given by:
!
      gydays(yr) = 365*( yr-1) &
                   +   ((yr-1)/4) &
                   -   ((yr-1)/100) &
                   +   ((yr-1)/400)

!
!     The number of days elapsed since Julian Jan 1, of year 1 A.D.
!     to Jan 1 of YR is given by:
!
      jydays(yr) = 365*(yr-1) &
                   +  ((yr-1)/4)

!
!     Return 1 if YR is divisible by M, otherwise return 0.
!
      divble(yr,m) = max ( 0, 1 + ((iabs(yr)/m)*m) - iabs(yr) )

!
!     The number of leap days in a Gregorian year is given by:
!
      gldays(yr) = divble( yr, 4   ) &
                 - divble( yr, 100 ) &
                 + divble( yr, 400 )

!
!     The number of leap days in a Julian year is given by:
!
      jldays(yr) = divble( yr, 4   )

!
!     To compute the day of the year we
!
!        look up the number of days to the beginning of the month,
!
!        add on the number leap days that occurred prior to that
!        time
!
!        add on the number of days into the month
!
      gdoy(yr,mon,dy) = dpjan0 ( mon ) &
                      + extra  ( mon ) * gldays(yr) &
                      + dy

      jdoy(yr,mon,dy) = dpjan0 ( mon ) &
                      + extra  ( mon ) * jldays(yr) &
                      + dy

!
!     The number of days since 1 Jan 1 A.D. (Gregorian) is given by:
!
      gdp001(yr,mon,dy) = gydays(yr) &
                        + gdoy  (yr, mon, dy) &
                        - 1

!
!     The number of days since 1 Jan 1 A.D. (Julianis given by:
!
      jdp001(yr,mon,dy) = jydays(yr) &
                        + jdoy  (yr, mon, dy) &
                        - 1

!
!     If this is the first pass through this entry point (or the
!     companion entry point) we need to set up some reference points.
!
!     RDAYG   is the number of days past 1 A.D. Jan 1 of the Gregorian
!             calendar of the date Oct 15, 1582
!
!     RDAYJ   is the number of days past 1 A.D. Jan 1 of the Julian
!             calendar of the date Oct 5, 1582.
!
!     OFFSTJ and OFFSTG are just the offset from one count of days
!     to the other.
!
      if ( first ) then

         first  = .false.

         rdayg  = gdp001 ( 1582, 10, 15 )
         rdayj  = jdp001 ( 1582, 10,  5 )
         offstj = rdayj - rdayg
         offstg = rdayg - rdayj

      end if

!
!     Make local copies of the year, month and day.  Then get the
!     YEARs into a positive range.
!
      call rmaini ( month-1, 12, yroff, mon )

      yr  = year + yroff
      mon = mon + 1
      dy  = day

      if ( yr .le. 0 ) then

         call rmaini ( yr, 4, m4, tmpyr )
         yr = tmpyr

         if ( yr .eq. 0 ) then
            yr = yr + 4
            m4   = m4   - 1
         end if

         offset = m4 * dp4y

      else

         offset = 0

      end if

!
!     First get the day number (Julian) for the input
!     year month and day.
!
      dayj = jdp001 ( yr, mon, dy  ) + offset

!
!     This day is DAYJ - RDAYJ days after 1582 Oct 5 on the
!     julian calendar.  But this is the same as the number
!     of days past 1582 Oct 15 on the Gregorian Calendar
!     So the Gregorian day number is DAYJ - RDAYJ + RDAYG
!     which is the same as DAYJ + OFFSTG.
!
      dayg = dayj + offstg
!
!     Now that we have the Gregorian day number it's a fairly
!     straight forward task to get the year, month and day
!     on the Gregorian calendar.
!

      call rmaini ( dayg, dp400y, m400, tmpday )
      dayg = tmpday

      m100 = min ( 3, dayg/dp100y )
      dayg = dayg -   m100*dp100y

      m4   = min ( 24, dayg/dp4y  )
      dayg = dayg  -     m4*dp4y

      m    = min (  3, dayg/dp1y )
      dayg = dayg   -     m*dp1y


      dofyr  = dayg + 1
      yr     = 400 * m400 &
             + 100 * m100 &
             +   4 * m4 &
             +       m &
             +       1

!
!     Now look up the month number and compute the day of the month.
!     How we do this depends on whether or not this is a leap year.
!
      if ( gldays(yr) .eq. 0 ) then
         mon  = lstlti ( dofyr, 12, dpjan0 )
         dy   = dofyr  -            dpjan0(mon)
      else
         mon  = lstlti ( dofyr, 12, dpbegl )
         dy   = dofyr  -            dpbegl(mon)
      end if

      year  = yr
      month = mon
      day   = dy
      doy   = dofyr

      return


!$Procedure GR2JUL (Gregorian to Julian Calendar)

      entry gr2jul (  year, month, day, doy )

! $ Abstract
!
!     Convert Year Month and Day on the  Gregorian Calendar
!     to the Julian Calendar
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TIME
!
! $ Declarations
!
!     INTEGER               YEAR
!     INTEGER               MONTH
!     INTEGER               DAY
!     INTEGER               DOY
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     YEAR      I-O  Year  of Gregorian Calendar/Julian Calendar
!     MONTH     I-O  Month of Gregorian Calendar/Julian Calendar
!     DAY       I-O  Day of Month in Gregorian Calendar/Julian Calendar
!     DOY        O   Day of Year in Julian Calendar
!
! $ Detailed_Input
!
!     YEAR     is an integer representing the year of an epoch, E, in
!              the Gregorian calendar. Note that the year 0 (zero)
!              and negative values are required to represent
!              years in the pre-Christian era (B.C.) A year, Y B.C.
!              should be represented as -(Y-1).  For example the year
!              435 B.C. should be input as -434.
!
!     MONTH    is an integer representing the month of some epoch, E,
!              in the Gregorian calendar. Months
!              outside the usual range from 1 to 12 are converted
!              to the standard range using modular arithmetic and
!              the input year is adjusted appropriately.
!
!     DAY      is the day of the month of some epoch, E, in the
!              Gregorian calendar.
!
!              Note to input an epoch as the day of a year, set MONTH
!              to 1 and DAY to the day of the year.
!
! $ Detailed_Output
!
!     YEAR     is an integer representing the year of the epoch, E,
!              above in the Julian calendar. Note that the year 0
!              (zero) and negative values are used to represent
!              years in the pre-Christian era (B.C.) A year, Y B.C.,
!              is be represented as -(Y-1).  For example the year
!              435 B.C. will be returned as -434.
!
!     MONTH    is an integer representing the month of the epoch, E,
!              above in the Julian Calendar calendar.
!
!     DAY      is the day of the month of the epoch, E, above in the
!              Julian Calendar
!
!     DOY      is the day of the year of the epoch, E, above in the
!              Julian Calendar.
!
! $ Parameters
!
!     None.
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
!     This is a mid-level utility routine to assist in the assignment
!     and presentation of Ancient epochs.
!
!     The SPICE software deals primarily with epochs represented on
!     in the Gregorian Calendar. However, the Gregorian calendar
!     was not adopted until October 15, 1582. As a result, epochs
!     prior to that time are usually represented in the Julian
!     proleptic calendar.
!
!     Formally, both calendars can be extended indefinitely forward
!     and backward in time due the algorithmic nature of the
!     determination of calendar representation.
!
!     This routine allows you to take a numeric representation of
!     an epoch represented in the Gregorian calendar and
!     convert that to an epoch in the Julian calendar.
!
!     To convert from Julian Calendar to Gregorian
!     calendar, use the entry point JUL2GR.
!
! $ Examples
!
!     Suppose you need to print an epoch (given in seconds past the
!     J2000 epoch) of some ancient epoch that occurred during
!     pre-Christian era, and that you want to represent this epoch
!     using the Julian proleptic calendar.
!
!     Here's the declarations we'll need
!
!        INTEGER               YEAR
!        INTEGER               MONTH
!        INTEGER               DAY
!        INTEGER               DOY
!
!        DOUBLE PRECISION      TVEC ( 6 )
!        DOUBLE PRECISION      TDB
!
!     You first need to convert TDB (the epoch in Seconds past J2000)
!     to a calendar representation.
!
!        TVEC(1) = TDB.
!
!        CALL TTRANS ( 'TDB', 'YMD', TVEC )
!
!     The output time vector will be relative to the Gregorian
!     Calendar. Collect the year, month and day from the time
!     vector.
!
!        YEAR    = INT ( TVEC(1) )
!        MONTH   = INT ( TVEC(2) )
!        DAY     = INT ( TVEC(3) )
!
!     The hours, minutes and seconds appear in components 4 through 6
!     of the time vector. We can ignore them in the conversion
!     of the calendar from Gregorian to Julian.
!
!        CALL GR2JUL ( YEAR, MONTH, DAY, DOY )
!
!     Now create a string from the YEAR, MONTH, DAY and TVEC(4)
!     through TVEC(6).
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 02-OCT-2021 (JDR)
!
!        Reordered header sections. Edited the header to comply with
!        NAIF standard.
!
!-    SPICELIB Version 1.1.0, 23-FEB-1998 (WLT)
!
!        The routine was upgraded so that it will handle without
!        error months that are outside the range from 1 to 12.
!
!-    SPICELIB Version 1.0.0, 13-MAR-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Convert from Gregorian to Julian Calendar
!
!-&


!
!     If this is the first pass through this entry point (or the
!     companion entry point) we need to set up some reference points.
!
!     RDAYG   is the number of days past 1 A.D. Jan 1 of the Gregorian
!             calendar of the date Oct 15, 1582
!
!     RDAYJ   is the number of days past 1 A.D. Jan 1 of the Julian
!             calendar of the date Oct 5, 1582.
!
!     OFFSTJ and OFFSTG are just the offset from one count of days
!     to the other.
!
      if ( first ) then

         first  = .false.

         rdayg  = gdp001 ( 1582, 10, 15 )
         rdayj  = jdp001 ( 1582, 10,  5 )
         offstj = rdayj - rdayg
         offstg = rdayg - rdayj

      end if
!
!     Make Local Copies of YEAR, MONTH and DAY and get YEAR into
!     a positive range.
!
      call rmaini ( month-1, 12, yroff, mon )

      yr  = year + yroff
      mon = mon  + 1
      dy  = day

      if ( yr .le. 0 ) then

         call rmaini ( yr, 400, m400, tmpyr )
         yr = tmpyr

         if ( yr .eq. 0 ) then
            yr   = yr + 400
            m400 = m400 - 1
         end if

         offset = m400 * dp400y

      else

         offset = 0

      end if

!
!     First get the day number (Gregorian) for the input
!     year month and day.
!
      dayg = gdp001 ( yr, mon, dy  ) + offset

!
!     This day is DAYG - RDAYG days after 1582 Oct 15 on the
!     Gregorian calendar.  But this is the same as the number
!     of days past 1582 Oct 5 on the Julian Calendar
!     So the Julian day number is DAYG - RDAYG + RDAYJ
!     which is the same as DAYG + OFFSTJ.
!
      dayj = dayg + offstj
!
!     Now that we have the Julian day number it's a fairly
!     straight forward task to get the year, month and day
!     on the Julian calendar.
!

      call rmaini ( dayj, dp4y,  m4,  tmpday )
      dayj = tmpday

      m    = min ( 3, dayj/dp1y )
      dayj = dayj -      m*dp1y


      dofyr  = dayj + 1
      yr     = 4 * m4 &
             +     m &
             +     1

!
!     Now look up the month number and compute the day of the month.
!     How we do this depends on whether or not this is a leap year.
!
      if ( jldays(yr) .eq. 0 ) then
         mon  = lstlti ( dofyr, 12, dpjan0 )
         dy   = dofyr  -            dpjan0(mon)
      else
         mon  = lstlti ( dofyr, 12, dpbegl )
         dy   = dofyr  -            dpbegl(mon)
      end if

      year    = yr
      month   = mon
      day     = dy
      doy     = dofyr

      return
      end
