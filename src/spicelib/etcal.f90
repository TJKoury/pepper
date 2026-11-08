!$Procedure ETCAL ( Convert ET to Calendar format )

      subroutine etcal ( et, calstr )

! $ Abstract
!
!     Convert from an ephemeris epoch measured in seconds past
!     the epoch of J2000 to a calendar string format using a
!     formal calendar free of leapseconds.
!
! $ Required_Reading
!
!     TIME
!
! $ Keywords
!
!     TIME
!
! $ Declarations

      implicit none

      double precision      et
      character*(*)         calstr

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris time measured in seconds past J2000.
!     CALSTR     O   A standard calendar representation of ET.
!
! $ Detailed_Input
!
!     ET       is an epoch measured in ephemeris seconds
!              past the epoch of J2000.
!
! $ Detailed_Output
!
!     CALSTR   is a calendar string representing the input ephemeris
!              epoch. This string is based upon extending the
!              Gregorian Calendar backward and forward indefinitely
!              keeping the same rules for determining leap years.
!              Moreover, there is no accounting for leapseconds.
!
!              To be sure that all of the date can be stored in
!              CALSTR, it should be declared to have length at
!              least 48 characters.
!
!              The string will have the following format
!
!                 year (era) mon day hr:mn:sc.sss
!
!              Where:
!
!                 year --- is the year
!                 era  --- is the chronological era associated with
!                          the date. For years after 999 A.D.
!                          the era is omitted. For years
!                          between 1 A.D. and 999 A.D. (inclusive)
!                          era is the string 'A.D.' For epochs
!                          before 1 A.D. Jan 1 00:00:00, era is
!                          given as 'B.C.' and the year is converted
!                          to years before the "Christian Era".
!                          The last B.C. epoch is
!
!                            1 B.C. DEC 31 23:59:59.999
!
!                          The first A.D. epoch (which occurs .001
!                          seconds after the last B.C. epoch) is:
!
!                             1 A.D. JAN 1 00:00:00.000
!
!                          Note: there is no year 0 A.D. or 0 B.C.
!                 mon  --- is a 3-letter abbreviation for the month
!                          in all capital letters.
!                 day  --- is the day of the month
!                 hr   --- is the hour of the day (between 0 and 23)
!                          leading zeros are added to hr if the
!                          numeric value is less than 10.
!                 mn   --- is the minute of the hour (0 to 59)
!                          leading zeros are added to mn if the
!                          numeric value is less than 10.
!                 sc.sss   is the second of the minute to 3 decimal
!                          places ( 0 to 59.999). Leading zeros
!                          are added if the numeric value is less
!                          than 10. Seconds are truncated, not
!                          rounded.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the input ET is so large that the corresponding
!         number of days since 1 A.D. Jan 1, 00:00:00 is
!         within 1 of overflowing or underflowing an integer,
!         ET will not be converted to the correct string
!         representation rather, the string returned will
!         state that the epoch was before or after the day
!         that is INTMIN +1 or INTMAX - 1 days after
!         1 A.D. Jan 1, 00:00:00.
!
!     2)  If the output string is not sufficiently long to hold
!         the full date, it will be truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is an error free routine for converting ephemeris epochs
!     represented as seconds past the J2000 epoch to formal
!     calendar strings based upon the Gregorian Calendar. This formal
!     time is often useful when one needs a human recognizable
!     form of an ephemeris epoch. There is no accounting for leap
!     seconds in the output times produced.
!
!     Note: The calendar epochs produced are not the same as the
!           UTC calendar epochs that correspond to ET. The strings
!           produced by this routine may vary from the corresponding
!           UTC epochs by more than 1 minute.
!
!     This routine can be used in creating error messages or
!     in routines and programs in which one prefers to report
!     times without employing leapseconds to produce exact UTC
!     epochs.
!
! $ Examples
!
!     Suppose you wish to  report that no data is
!     available at a particular ephemeris epoch ET. The following
!     code shows how you might accomplish this task:
!
!           CALL DPSTRF ( ET,  6, 'F', ETSTR  )
!           CALL ETCAL  ( ET,          CALSTR )
!
!           E1 = RTRIM   (             CALSTR )
!           E2 = RTRIM   (             ETSTR  )
!
!           WRITE (*,*) 'There is no data available for the body '
!           WRITE (*,*) 'at requested time: '
!           WRITE (*,*) '   ', CALSTR(1:E1), ' (', ETSTR(1:E2), ')'
!
! $ Restrictions
!
!     1)  One must keep in mind when using this routine that
!         ancient times are not based upon the Gregorian
!         calendar. For example the 0 point of the Julian
!         Date system is 4713 B.C. Jan 1, 12:00:00 on the Julian
!         Calendar. If one formalized the Gregorian calendar
!         and extended it indefinitely, the zero point of the Julian
!         date system corresponds to 4714 B.C. NOV 24 12:00:00 on
!         the Gregorian calendar. There are several reasons for this.
!         Leap years in the Julian calendar occur every
!         4 years (including *all* centuries). Moreover,  the
!         Gregorian calendar "effectively" begins on 15 Oct, 1582 A.D.
!         which is 5 Oct, 1582 A.D. in the Julian Calendar.
!
!         Therefore you must be careful in your interpretation
!         of ancient dates produced by this routine.
!
! $ Literature_References
!
!     [1]  J. Jespersen and J. Fitz-Randolph, "From Sundials to Atomic
!          Clocks, Understanding Time and Frequency," Dover
!          Publications, Inc. New York, 1982.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.3.0, 17-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement. Changed output argument name
!        STRING to CALSTR for consistency with other routines.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section. Added TIME required reading.
!
!-    SPICELIB Version 2.2.0, 05-MAR-1998 (WLT)
!
!        The documentation concerning the appearance of the output
!        time string was corrected so that it does not suggest
!        a comma is inserted after the day of the month. The
!        comma was removed from the output string in Version 2.0.0
!        (see the note below) but the documentation was not upgraded
!        accordingly.
!
!-    SPICELIB Version 2.1.0, 20-MAY-1996 (WLT)
!
!        Two arrays that were initialized but never used were
!        removed.
!
!-    SPICELIB Version 2.0.0, 16-AUG-1995 (KRG)
!
!        If the day number was less than 10, the spacing was off for
!        the rest of the time by one space, that for the "tens" digit.
!        This has been fixed by using a leading zero when the number of
!        days is < 10.
!
!        Also, the comma that appeared between the month/day/year
!        and the hour:minute:seconds tokens has been removed. This was
!        done in order to make the calendar date format of ETCAL
!        consistent with the calendar date format of ET2UTC.
!
!
!-    SPICELIB Version 1.0.0, 14-DEC-1993 (WLT)
!
!-&


! $ Index_Entries
!
!     Convert ephemeris time to a formal calendar date
!
!-&


!
!     SPICELIB Functions.
!
      double precision      spd
      integer               intmax
      integer               intmin
      integer               lstlti



!
!     We declare the variables that contain the number of days in
!     400 years, 100 years, 4 years and 1 year.
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

      integer               strsiz
      parameter           ( strsiz = 16 )

      integer               lngsiz
      parameter           ( lngsiz = 180 )


!
!     The variables below hold the components of the output string
!     before they are put together.
!
      character*(strsiz)    era
      character*(strsiz)    ystr
      character*(strsiz)    dstr
      character*(strsiz)    hstr
      character*(strsiz)    mstr
      character*(strsiz)    sstr
      character*(strsiz)    messge

!
!     We will construct our string using the local variable DATE
!     and transfer the results to the output CALSTR when we are
!     done.
!
      character*(lngsiz)    date

!
!     MONTHS contains 3-letter abbreviations for the months of the year
!
      character*(3)         months ( 12 )

      double precision      dmnint
      double precision      dmxint
      double precision      dp2000
      double precision      frac
      double precision      halfd
      double precision      mydnom
      double precision      mynum
      double precision      q
      double precision      remd
      double precision      secs
      double precision      secspd

      integer               bh
      integer               bm
      integer               day
      integer               daynum
      integer               dn2000
      integer               dofyr
      integer               doy
      integer               dp0001
      integer               hours
      integer               iq
      integer               ldays
      integer               mins
      integer               month
      integer               offset
      integer               rem
      integer               tsecs
      integer               ydays
      integer               year
      integer               yr1
      integer               yr100
      integer               yr4
      integer               yr400

      logical               first
      logical               adjust

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

      save

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

      data                  months  / 'JAN',  'FEB', 'MAR', 'APR', &
                                      'MAY',  'JUN', 'JUL', 'AUG', &
                                      'SEP',  'OCT', 'NOV', 'DEC' /



!
!     Definitions of statement functions.
!
!
!     The number of days elapsed since Jan 1, of year 1 A.D. to
!     Jan 1 of YEAR is given by:
!
      ydays(year) = 365*(year-1) &
                    +  ((year-1)/4) &
                    -  ((year-1)/100) &
                    +  ((year-1)/400)

!
!     The number of leap days in a year is given by:
!
      ldays(year) =   ( (year/4  ) * 4   ) / year &
                    - ( (year/100) * 100 ) / year &
                    + ( (year/400) * 400 ) / year

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
      doy(year,month,day) =   dpjan0 ( month ) &
                            + extra  ( month ) * ldays(year) &
                            + day


!
!     The number of days since 1 Jan 1 A.D. is given by:
!
      dp0001(year,month,day) = ydays(year) &
                             + doy  (year, month, day) &
                             - 1




      if ( first ) then

         first  = .false.
         halfd  = spd()/2.0d0
         secspd = spd()
         dn2000 = dp0001 ( 2000, 1, 1 )
         dmxint = intmax()
         dmnint = intmin()

      end if
!
!     Now we "in-line" compute the following call.
!
!        call rmaind ( et + halfd, secspd, dp2000, secs )
!
!     because we can't make a call to rmaind.
!
!     The reader may wonder why we use et + halfd.  The value
!     et is seconds past the ephemeris epoch of J2000 which
!     is at 2000 Jan 1, 12:00:00.  We want to compute days past
!     2000 Jan 1, 00:00:00.  The seconds past THAT epoch is et + halfd.
!     We add on 0.0005 seconds so that the string produced will be
!     rounded to the nearest millisecond.
!
      mydnom = secspd
      mynum  = et + halfd

      q    = dint ( mynum / mydnom )
      remd = mynum -    q * mydnom

      if ( remd .lt. 0.0d0 ) then
         q    = q   - 1.0d0
         remd = remd + mydnom
      end if

      secs   = remd
      dp2000 = q

!
!     Do something about the problem when ET is vastly
!     out of range.  (Day number outside MAX and MIN integer).
!
      if      ( dp2000 + dn2000 .lt. dmnint + 1) then

         dp2000 = dmnint - dn2000 + 1
         messge = 'Epoch before '
         secs   = 0.0d0

      else if ( dp2000 + dn2000 .gt. dmxint - 1 ) then

         dp2000 = dmxint - dn2000 - 1
         messge = 'Epoch after '
         secs   = 0.0d0

      else

         messge = ' '

      end if

!
!     Compute the number of days since 1 .A.D. Jan 1, 00:00:00.
!     From the tests in the previous IF-ELSE IF-ELSE block this
!     addition is guaranteed not to overflow.
!
      daynum = int( dp2000 + dble(dn2000) )

!
!     If the number of days is negative, we need to do a little
!     work so that we can represent the date in the B.C. era.
!     We add enough multiples of 400 years so that the year will
!     be positive and then we subtract off the appropriate multiple
!     of 400 years later.
!
      if ( daynum .lt. 0 ) then

!
!        Since we can't make the call below and remain
!        error free, we compute it ourselves.
!
!        call rmaini ( daynum, dp400y, offset, daynum )
!
         iq   = daynum / dp400y
         rem  = daynum - dp400y*iq

         if ( rem .lt. 0 ) then
            iq   = iq   - 1
            rem  = rem  + dp400y
         end if

         offset = iq
         daynum = rem

         adjust = .true.

      else

         adjust = .false.

      end if

!
!     Next we compute the year.  Divide out multiples of 400, 100
!     4 and 1 year.  Finally combine these to get the correct
!     value for year.  (Note this is all integer arithmetic.)
!
!     Recall that DP1Y   =    365
!                 DP4Y   =  4*DPY    + 1
!                 DP100Y = 25*DP4Y   - 1
!                 DP400Y =  4*DP100Y + 1
!
      yr400  =  daynum / dp400y
      rem    =  daynum - dp400y*yr400

      yr100  =  min ( 3, rem   / dp100y )
      rem    =  rem    - yr100 * dp100y

      yr4    =  min (24, rem / dp4y )
      rem    =  rem    - yr4 * dp4y

      yr1    =  min (3,  rem / dp1y )
      rem    =  rem    - yr1 * dp1y

      dofyr  =  rem    + 1

      year   = yr400*400 + yr100*100 + yr4*4 + yr1 + 1
!
!     Get the month, and day of month (depending upon whether
!     we have a leap year or not).
!
      if ( ldays(year) .eq. 0 ) then
         month  = lstlti ( dofyr, 12, dpjan0 )
         day    = dofyr - dpjan0(month)
      else
         month  = lstlti ( dofyr, 12, dpbegl )
         day    = dofyr - dpbegl(month)
      end if
!
!     If we had to adjust the year to make it positive, we now
!     need to correct it and then convert it to a B.C. year.
!
      if ( adjust ) then

         year   =  year + offset*400
         year   = -year + 1
         era    = ' B.C. '

      else
!
!        If the year is less than 1000, we can't just write it
!        out.  We need to add the era.  If we don't do this
!        the dates look very confusing.
!
         if ( year .lt. 1000 ) then
            era   = ' A.D. '
         else
            era   = ' '
         end if

      end if
!
!     Convert Seconds to Hours, Minute and Seconds.
!     We work with thousandths of a second in integer arithmetic
!     so that all of the truncation work with seconds will already
!     be done.  (Note that we already know that SECS is greater than
!     or equal to zero so we'll have no problems with HOURS, MINS
!     or SECS becoming negative.)
!
      tsecs  = int(secs*1000.0d0)
      frac   = secs - dble(tsecs)

      hours  = tsecs / 3600000
      tsecs  = tsecs - 3600000 * hours

      mins   = tsecs / 60000
      tsecs  = tsecs - 60000 * mins

      secs   = dble ( tsecs ) / 1000.0d0
!
!     We round seconds if we can do so without getting seconds to be
!     bigger than 60.
!
      if ( secs + 0.0005d0 .lt. 60.0d0 ) then
         secs = secs + 0.0005d0
      end if

!
!     Finally, get the components of our date string.
!
      call intstr ( year,   ystr )
      if ( day .ge. 10 ) then
         call intstr ( day, dstr )
      else
         dstr = '0'
         call intstr( day, dstr(2:) )
      end if

!
!     We want to zero pad the hours minutes and seconds.
!
      if ( hours .lt. 10 ) then
         bh = 2
      else
         bh = 1
      end if

      if ( mins .lt. 10 ) then
         bm = 2
      else
         bm = 1
      end if


      mstr = '00'
      hstr = '00'
      sstr = ' '

!
!     Now construct the string components for hours, minutes and
!     seconds.
!
      secs = int(secs*1000.0d0) / 1000.0d0

      call intstr ( hours,         hstr(bh:) )
      call intstr ( mins,          mstr(bm:) )
      call dpstrf ( secs,  6, 'F', sstr      )
!
!     The form of the output for SSTR has a leading blank followed by
!     the first significant digit.  If a decimal point is in the
!     third slot, then SSTR is of the form ' x.xxxxx'  and we need
!     to insert a leading zero.
!
      if ( sstr(3:3) .eq. '.' ) then
         sstr(1:1) = '0'
      end if
!
!     We don't want any leading spaces in SSTR, (HSTR and MSTR don't
!     have leading spaces by construction.
!
      call ljust ( sstr, sstr )
!
!     Now form the date string, squeeze out extra spaces and
!     left justify the whole thing.
!
      date = messge &
      //     ystr &
      //     era &
      //     months(month) &
      //     ' ' &
      //     dstr(1:3) &
      //     ' ' &
      //     hstr(1:2) &
      //     ':' &
      //     mstr(1:2) &
      //     ':' &
      //     sstr(1:6)


      call cmprss ( ' ', 1, date, date )
      call ljust  (         date, date )

      calstr = date

      return
      end
