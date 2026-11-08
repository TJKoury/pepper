!$Procedure TTRANS ( Time transformation )

      subroutine ttrans ( from, to, tvec )

! $ Abstract
!
!     Transform a time vector from one representation and system
!     to another.
!
! $ Required_Reading
!
!     TIME
!
! $ Keywords
!
!     PARSING
!     TIME
!
! $ Declarations

      implicit none

      include               'zzctr.inc'

      integer               mxcomp
      parameter           ( mxcomp = 10 )

      character*(*)         from
      character*(*)         to
      double precision      tvec ( mxcomp )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MXCOMP     P    maximum number of components allowed for TVEC.
!     TO         I    description of a time vector.
!     FROM       I    description of a time vector.
!     TVEC      I-O   time vector representing an epoch.
!
! $ Detailed_Input
!
!     TVEC     is called a time vector. It is an array of double
!              precision numbers that represent some epoch. To
!              determine its meaning you must examine the string
!              FROM. Note that the number of significant entries
!              in TVEC is implied by FROM.
!
!     FROM,
!     TO       are two strings used to describe the type of time vector
!              TVEC. FROM is the type of the input vector TVEC and
!              TO is the type of the output TVEC
!
!              The interpretation of TVEC  is as follows:
!
!                 TYPE      Interpretation of TVEC
!                 ------    -------------------------------------------
!                 YMD(F)  - year, month, day,   hour, minutes, seconds
!                 YD(F)   - year,  day-of-year, hour, minutes, seconds
!                 YD.D(F) - year, number of days past beginning of year
!                 DAYSEC  - calendar days past 1 jan 1 AD,
!                           seconds past beg day
!                 DP2000  - calendar days past 1 jan 2000,
!                           seconds past beg day
!                 JDUTC   - julian date UTC.
!                 FORMAL  - seconds in the formal calendar since J2000.
!                 YWD(F)  - year, week, day, hour, minutes, seconds
!                 YMWD(F) - year, month, week, day, hour, minutes,
!                           seconds
!                 TAI     - atomic seconds past Atomic J2000.
!                 TDT     - Terrestrial Dynamical Time
!                 TDB     - Barycentric Dynamical Time
!                 JED     - Julian Ephemeris Date (based on TDB)
!                 ET      - Ephemeris time (same as TDB)
!                 JDTDB   - Julian Date based on TDB (same as JED)
!                 JDTDT   - Julian Date based on TDT
!
!              The number of components of TVEC implied by TYPE is
!              as follows:
!
!                 YMD     - 6
!                 YD      - 5
!                 JDUTC   - 1
!                 FORMAL  - 1
!                 YD.D    - 2
!                 DAYSEC  - 2
!                 DP2000  - 2
!                 YWD     - 6
!                 YMWD    - 7
!                 TAI     - 1
!                 TDT     - 1
!                 TDB     - 1
!                 JED     - 1
!                 ET      - 1
!                 JDTDB   - 1
!                 JDTDT   - 1
!
!
!              For all types, only the last component of the
!              time vector may be non-integer. If other components
!              have fractional parts only their truncated integer
!              components will be recognized.
!
!              YMD and YD
!
!                 These types are assumed to be different
!                 representations on UTC time markers. Thus
!                 the hour, minutes and seconds portions all
!                 represent time elapsed
!                 since the beginning of a day. As such the
!                 seconds portion of HMS may range up to (but
!                 not include) 61 on days when positive leap
!                 seconds occur and may range up to (but not
!                 include) 59 on days during which negative
!                 leapseconds occur.
!
!              YD.D type.
!
!                 Y is the calendar year used in civil time keeping
!                 D is the day of the calendar year --- for any time
!                   during the first of January, the integer portion
!                   of the day will be 1.
!
!                   The fractional portion is the fractional part of
!                   the specific day. Thus the amount of time
!                   specified by the fractional portion of the day
!                   depends upon whether or not the day has a leap
!                   second.  ".D" can be computed from the formula
!
!                         number of seconds past beginning of day
!                   .D = ---------------------------------------
!                            number of UTC seconds in the day.
!
!              FORMAL type.
!
!                 The FORMAL type for TVEC gives the number of
!                 seconds past the epoch J2000 (noon Jan 1 2000)
!                 on the formal calendar (no leap seconds ---
!                 all days contain 86400 seconds)  The formal clock
!                 is simply held still for one second during
!                 positive leap seconds. Times during leap seconds
!                 cannot be represented in this system.
!
!                 This system is converted internally to a
!                 calendar days past epoch and seconds
!                 past beginning of day form. For this reason,
!                 times that occur during a positive leap second
!                 can never be represented. Moreover, if a negative
!                 leapsecond occurs, times that occur during the
!                 ``missing'' leapsecond will simply be placed
!                 at the beginning of the next day. Thus two
!                 different FORMAL times can represent the
!                 same time around a negative leap second.
!
!                 FORMAL time is equivalent to somewhat parochial
!                 ``UTC seconds past J2000'' that is produced
!                 by the SPICE routine TPARSE.
!
!              JDUTC type.
!
!                 This system is similar to the FORMAL system
!                 described above. All days are assumed to have
!                 86400 seconds. All numbers of the form
!
!                    integer + 0.5
!
!                 fall at the beginning of calendar UTC days.
!
!                 There is no way to represent times during a
!                 positive leapsecond. Times during missing
!                 negative leap seconds are represented in two ways.
!
!              DAYSEC type.
!
!                 This time vector has the form of calendar
!                 days since January 1, of the year 1 A.D.
!                 and number of seconds past the beginning of the
!                 calendar day.
!                 (January 2 of the year 1 A.D. is 1 calendar
!                 day past January 1, 1 A.D.)
!
!              DP2000 type.
!
!                 This time vector has the same form as DAYSEC
!                 time vectors. The only difference is that
!                 the reference epoch is JAN 1, 2000.
!
!              YWD and YMWD types.
!
!                 These time vectors are used to specify a time
!                 that are most conveniently expressed by phrases
!                 such as "the third Monday of every month" or
!                 "Beginning with the second Wednesday of the new
!                 year and every 4th Wednesday thereafter."
!
!                 The hours, minutes and seconds components of
!                 these time vectors are the
!                 same as for the Year-Month-Day and Year-Day UTC
!                 time vectors.
!
!                 The Y component refers to the calendar year, and
!                 in the YMWD vector, the M component refers to
!                 the calendar month.
!
!                 The W component refers to the week of the
!                 Year (YWD) or Month (YMWD).  The first week
!                 begins on the first day of the year or the first
!                 day of the month. The D component is the day of the
!                 week with 1 corresponding to Sunday, 2 to Monday,
!                 and so on with 7 corresponding to Saturday.
!
!                 Thus the YMWD time vector
!
!                    1991
!                      11
!                       3
!                       5
!                      12
!                       0
!                       0
!
!                 refers to 12:00:00 on the third Thursday of
!                 November of 1991.
!
!                 The YWD time vector
!
!                    1997
!                      11
!                       4
!                      13
!                       5
!                      11
!
!                 refers to 12:05:11 on the eleventh Wednesday
!                 of 1997.
!
!              Formal Calendar Time Vectors
!              ============================
!              The types YMDF, YDF, YD.D(F), YWDF, YMWDF are similar
!              to the corresponding base types: YMD, YD, YD.D, YWD
!              and YMWD. However, these types represent formal
!              time vectors. Each day contains exactly 86400 seconds.
!              The difference between formal and non-formal systems
!              can only be seen during a positive leapsecond or
!              during the second following a negative leapsecond.
!
!              Epochs during a positive leapsecond on input are
!              placed in the first second of the next day. Epochs
!              during a positive leapsecond on output are held
!              at 00:00:00 of the next day.
!
!              Epochs during the first second following a negative
!              leapsecond are counted as belonging to the previous
!              day if both the input and output types are formal
!              types.
!
!
!              Calendars
!              =====================
!              In all time vectors for which a year is specified,
!              the year is assumed to belong to the Gregorian
!              Calendar---every 4th year is a leapyear except
!              for centuries (such as 1900) that are not divisible
!              by 400. This calendar is formally extended
!              indefinitely backward and forward in time.
!
!              Note that the Gregorian Calendar did not
!              formally exist prior to October 15, 1582. Prior to
!              that time the Julian Calendar was used (in the
!              Julian Calendar every 4th year is a leapyear, including
!              all centuries).
!
!              If you have epochs relative to the Julian calendar,
!              the SPICE routine JUL2GR is available for converting
!              to the formal Gregorian Calendar.
!
!
!              Epochs Prior to 1972
!              =====================
!              UTC as it exists today, was adopted in 1972. For
!              epochs prior to 1972, it is assumed that the difference
!              between TAI and UTC is a constant value.
!
!              Years prior to 1 A.D.
!              =====================
!              A year belonging to the B.C. era,  may be
!              represented by subtracting the year from 1.
!              Thus to specify 27 B.C (Gregorian) set the
!              year component of the time vector to -26.
!
!
!              Notes:
!              ======
!              The FORMAL and JDUTC types should not be used
!              for times near a leap second. However, for times
!              removed from leap seconds they pose no problems.
!
!              The DAYSEC and DP2000 are useful for representing
!              times that are given in atomic seconds past some
!              reference epoch other than J2000.
!
! $ Detailed_Output
!
!     TVEC     is the time vector corresponding to the input
!              time vector but with components consistent with
!              the type specified by input variable TO.
!
! $ Parameters
!
!     MXCOMP   is the maximum number of components that can appear in
!              TVEC.
!
! $ Exceptions
!
!     1)  If the type of either FROM or TO is not recognized, the
!         error SPICE(UNKNONWNTIMESYSTEM) is signaled.
!
!     2)  If a leapseconds kernel has not been loaded prior a call
!         to TTRANS, the error  SPICE(NOLEAPSECONDS) is signaled.
!
!     3)  If epochs associated with leapseconds in the leapseconds
!         kernel are not in increasing order, the error
!         SPICE(BADLEAPSECONDS) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is the fundamental translator between various
!     representations of time in the SPICE system. However, it
!     is intended to be a mid-level routine that few user's should
!     have need of calling.
!
!     In addition to translating between time systems, this routine
!     can be used to normalize the components of a time string
!     so that they are in the normal range for a particular
!     representation. This allows you to easily do arithmetic
!     with epochs.
!
! $ Examples
!
!     Suppose you need to convert a time expressed as seconds
!     past J2000 (TDB) to Pacific Daylight time. The following
!     example shows how you might use TTRANS to accomplish this
!     task.
!
!      TVEC(1) = ET
!
!      CALL TTRANS ( 'TDB', 'YMD', TVEC )
!
!      The seconds component of PDT is the same as the seconds
!      component of UTC. We save and add the UTC-PDT offset
!      to the hours and minutes component of the time vector.
!
!      SECNDS  = TVEC(6)
!      TVEC(6) = 0.0D0
!
!      TVEC(4) = TVEC(4) - 7.0D0
!      TVEC(5) = TVEC(5) + 0.0D0
!
!      CALL TTRANS ( 'YMDF', 'YMDF', TVEC )
!
!      Now reset the seconds component to the original value
!      and pass the time vector to some formatting routine.
!
!      TVEC(6) = SECNDS
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.6.0, 05-SEP-2021 (EDW) (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!        Removed INT casts in HMSSEC calls. The casts prevent
!        correct calculation of TDB time for non integer hour
!        and minute values in time strings from STR2ET.
!
!        Removed reference to FURNSH from the "LSK variable
!        not present" long error message.
!
!-    SPICELIB Version 1.5.0, 09-SEP-2013 (BVS)
!
!        Updated to keep track of the POOL counter and call ZZCVPOOL.
!
!-    SPICELIB Version 1.4.0, 05-MAR-2009 (NJB)
!
!        Bug fix: this routine now keeps track of whether its
!        kernel pool look-up succeeded. If not, a kernel pool
!        lookup is attempted on the next call to this routine.
!
!-    SPICELIB Version 1.3.0, 15-NOV-2006 (NJB)
!
!        A reference to RTPOOL was replaced by a reference
!        to GDPOOL.
!
!-    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in RMAIND and RMAINI calls. Changed reference to LDPOOL to
!        reference to FURNSH in an error message.
!
!-    SPICELIB Version 1.1.0, 09-JUN-1999 (WLT)
!
!        The routine was modified so that uniform time system
!        transformations (see UNITIM) are handled without
!        performing intermediate computations. This gives a slight
!        improvement in the accuracy of some computations.
!
!        In addition, two unused variables were removed.
!
!-    SPICELIB Version 1.0.0, 17-SEP-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Convert from one time vector to another
!     Convert between various parsed time representations
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.2.0, 24-OCT-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in RMAIND and RMAINI calls. Changed reference to LDPOOL to
!        reference to FURNSH in an error message.
!
!-&


!
!     SPICELIB functions
!
      logical               return

      integer               bsrchc
      integer               lstlei
      integer               lstled
      integer               lstlti

      double precision      j2000
      double precision      spd
      double precision      unitim

      logical               odd
      logical               elemc


!
!     Local (in-line) functions
!
      integer               divble
      integer               doy
      integer               dp0001
      integer               ldays
      integer               ydays

      double precision      hmssec

!
!     Local parameters
!
!
!     Parameters
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

!
!     MAXLP is the maximum number of leap seconds that can be
!     stored internally.   The value of 140 should be sufficient
!     to store leap seconds through the year 2100.
!
      integer               maxlp
      parameter           ( maxlp = 140 )

!
!     MAXVAR is the number of kernel pool variables required by this
!     routine.
!
      integer               maxvar
      parameter           ( maxvar = 1 )

!
!
!     The following gives us an "enumeration" for all of the
!     various types of time vectors that are recognized.
!
!     DAYSEC
!     DAYP2
!     ET
!     FRML
!     JDTDB
!     JDTDT
!     JDUTC
!     JED
!     TAI
!     TDB
!     TDT
!     YD
!     YDD
!     YDDF
!     YDF
!     YMD
!     YMDF
!     YMWD
!     YMWDF
!     YWD
!     YWDF
!

      integer               daysec
      parameter           ( daysec = 1 )

      integer               dayp2
      parameter           ( dayp2 = daysec + 1 )

      integer               et
      parameter           ( et     = dayp2 + 1 )

      integer               frml
      parameter           ( frml = et     + 1 )

      integer               jdtdb
      parameter           ( jdtdb  = frml + 1 )

      integer               jdtdt
      parameter           ( jdtdt  = jdtdb  + 1 )

      integer               jdutc
      parameter           ( jdutc  = jdtdt  + 1 )

      integer               jed
      parameter           ( jed    = jdutc  + 1 )

      integer               ttai
      parameter           ( ttai   = jed    + 1 )

      integer               tdb
      parameter           ( tdb    = ttai   + 1 )

      integer               tdt
      parameter           ( tdt    = tdb    + 1 )

      integer               yd
      parameter           ( yd     = tdt    + 1 )

      integer               ydd
      parameter           ( ydd    = yd     + 1 )

      integer               yddf
      parameter           ( yddf   = ydd    + 1 )

      integer               ydf
      parameter           ( ydf    = yddf   + 1 )

      integer               ymd
      parameter           ( ymd    = ydf    + 1 )

      integer               ymdf
      parameter           ( ymdf   = ymd    + 1 )

      integer               ymwd
      parameter           ( ymwd   = ymdf   + 1 )

      integer               ymwdf
      parameter           ( ymwdf  = ymwd   + 1 )

      integer               ywd
      parameter           ( ywd    = ymwdf  + 1 )

      integer               ywdf
      parameter           ( ywdf   = ywd    + 1 )

      integer               nrec
      parameter           ( nrec   = ywdf )
!
!     The following parameters just make the code seem a bit
!     more natural.
!
      logical               no
      parameter           ( no  = .false. )

      logical               yes
      parameter           ( yes = .true. )

!
!     Local variables
!
      integer               lbcell
      parameter           ( lbcell = -5 )


      integer               typlen
      parameter           ( typlen =  8 )

      character*(typlen)    unifrm (lbcell : nrec )

      character*(typlen)    recog   ( nrec )
      integer               parsed  ( nrec )
      logical               needy   ( nrec )
      logical               forml   ( nrec )
      integer               ordvec  ( nrec )

      character*(32)        rest
      character*(32)        myfrom
      character*(32)        myto
      character*(32)        vars   ( maxvar )

      double precision      daydp
      double precision      daylen
      double precision      dp2000
      double precision      dt
      double precision      exsecs
      double precision      formal
      double precision      frac
      double precision      halfd
      double precision      hours
      double precision      jd1101
      double precision      jdsecs
      double precision      lastdt
      double precision      mins
      double precision      secs
      double precision      secspd
      double precision      tai
      double precision      taitab ( 2*maxlp )
      double precision      tempd
      double precision      tsecs

      integer               day
      integer               daynum
      integer               dayptr
      integer               daytab ( 2*maxlp )
      integer               dn2000
      integer               doffst
      integer               dofyr
      integer               dpsun
      integer               dyear
      integer               fmday
      integer               fyrday
      integer               i
      integer               month
      integer               nref
      integer               offset
      integer               pfrom
      integer               pto
      integer               qint
      integer               refptr
      integer               rem
      integer               sunday
      integer               taiptr
      integer               tempi
      integer               usrctr ( ctrsiz )
      integer               week
      integer               wkday
      integer               year
      integer               yr1
      integer               yr100
      integer               yr4
      integer               yr400

      logical               first
      logical               found
      logical               nodata
      logical               update

!
!     The array EXTRA contains the number of many additional days that
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

      data                  nodata  / .true. /


!
!     Definitions of statement functions.
!
!     The number of days elapsed since Jan 1, of year 1 A.D. to
!     Jan 1 of YEAR is given by:
!
      ydays(year) = 365*(year-1) &
                  +    ((year-1)/4) &
                  -    ((year-1)/100) &
                  +    ((year-1)/400)

!
!     Return 1 if YEAR is divisible by N, otherwise return 0.
!
      divble(year,i) = max ( 0, 1 + ((iabs(year)/i)*i) - iabs(year) )

!
!     The number of leap days in a year is given by:
!
      ldays(year) = divble( year, 4   ) &
                  - divble( year, 100 ) &
                  + divble( year, 400 )

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
      doy(year,month,day) = dpjan0 ( month ) &
                          + extra  ( month ) * ldays(year) &
                          + day


!
!     The number of days since 1 Jan 1 A.D. is given by:
!
      dp0001(year,month,day) = ydays(year) &
                             + doy  (year, month, day) &
                             - 1


!
!     The number of seconds represented by HOURS hours MINS minutes
!     and SECS seconds.
!
      hmssec ( hours, mins, secs ) = hours * 3600.0d0 &
                                   + mins  *   60.0d0 &
                                   + secs





      if ( return() ) then
         return
      else
         call chkin ( 'TTRANS' )
      end if

!
!     The first time any of the entry points are called we
!     must set up the "watcher" for the kernel pool variables
!     that will be needed by this routine.
!
      if ( first ) then

         first   = .false.

         secspd  = spd()
         halfd   = spd() / 2.0d0

         dn2000  = dp0001 ( 2000, 1, 1 )
         sunday  = dp0001 ( 1991, 1, 6 )

         jd1101  = j2000() - dble(dn2000) - 0.5d0

!
!        Initialize the list of Uniform time systems.
!
         call ssizec ( nrec, unifrm )

!
!        Set up the set of recognized time vectors.
!
!        The following 4 parallel arrays are here
!        to assist in the task of classifying the
!        FROM and TO time representations. The arrays
!        contain:
!
!        RECOG   the strings that are recognized as legitimate
!                time representations
!
!        PARSED  a unique integer that can be used to stand
!                for each recognized format.  This is used
!                in the various IF THEN blocks to decide
!                how a time vector should be processed instead
!                of the name because integer compares are
!                much faster than string comparisons.
!
!        FORML   is a logical that indicates whether or not the
!                corresponding time system is a formal system
!                or UTC based system.  FORML(I) = YES implies
!                the time system is formal.  FORML(I) means it
!                isn't.
!
!        NEEDY   is a logical that indicates whether or not
!                there is a YEAR in the time system.  It should
!                be read "NEED Y" for "need year"  not "needy"
!                as when you are destitute.  NEEDY(I) = YES means
!                the time system has a year.  NEEDY(I) = NO means
!                it doesn't
!
         recog ( 01 ) = 'DAYSEC '
         parsed( 01 ) =  daysec
         forml ( 01 ) =  no
         needy ( 01 ) =  no

         recog ( 02 ) = 'DP2000 '
         parsed( 02 ) =  dayp2
         forml ( 02 ) =  no
         needy ( 02 ) =  no

         recog ( 03 ) = 'ET '
         parsed( 03 ) =  et
         forml ( 03 ) =  no
         needy ( 03 ) =  no

         call insrtc ( 'ET', unifrm )

         recog ( 04 ) = 'FORMAL '
         parsed( 04 ) =  frml
         forml ( 04 ) =  yes
         needy ( 04 ) =  no

         recog ( 05 ) = 'JDTDB '
         parsed( 05 ) =  jdtdb
         forml ( 05 ) =  no
         needy ( 05 ) =  no

         call insrtc ( 'JDTDB', unifrm )

         recog ( 06 ) = 'JDTDT '
         parsed( 06 ) =  jdtdt
         forml ( 06 ) =  no
         needy ( 06 ) =  no

         call insrtc ( 'JDTDT', unifrm )

         recog ( 07 ) = 'JDUTC '
         parsed( 07 ) =  jdutc
         forml ( 07 ) =  yes
         needy ( 07 ) =  no

         recog ( 08 ) = 'JED '
         parsed( 08 ) =  jed
         forml ( 08 ) =  no
         needy ( 08 ) =  no

         call insrtc ( 'JED', unifrm )

         recog ( 09 ) = 'TAI '
         parsed( 09 ) =  ttai
         forml ( 09 ) =  no
         needy ( 09 ) =  no

         call insrtc ( 'TAI', unifrm )

         recog ( 10 ) = 'TDB '
         parsed( 10 ) =  tdb
         forml ( 10 ) =  no
         needy ( 10 ) =  no

         call insrtc ( 'TDB', unifrm )

         recog ( 11 ) = 'TDT '
         parsed( 11 ) =  tdt
         forml ( 11 ) =  no
         needy ( 11 ) =  no

         call insrtc ( 'TDT', unifrm )

         recog ( 12 ) = 'YD '
         parsed( 12 ) =  yd
         forml ( 12 ) =  no
         needy ( 12 ) =  yes

         recog ( 13 ) = 'YD.D '
         parsed( 13 ) =  ydd
         forml ( 13 ) =  no
         needy ( 13 ) =  yes

         recog ( 14 ) = 'YD.DF '
         parsed( 14 ) =  yddf
         forml ( 14 ) =  yes
         needy ( 14 ) =  yes

         recog ( 15 ) = 'YDF '
         parsed( 15 ) =  ydf
         forml ( 15 ) =  yes
         needy ( 15 ) =  yes

         recog ( 16 ) = 'YMD '
         parsed( 16 ) =  ymd
         forml ( 16 ) =  no
         needy ( 16 ) =  yes

         recog ( 17 ) = 'YMDF '
         parsed( 17 ) =  ymdf
         forml ( 17 ) =  yes
         needy ( 17 ) =  yes

         recog ( 18 ) = 'YMWD '
         parsed( 18 ) =  ymwd
         forml ( 18 ) =  no
         needy ( 18 ) =  yes

         recog ( 19 ) = 'YMWDF '
         parsed( 19 ) =  ymwdf
         forml ( 19 ) =  yes
         needy ( 19 ) =  yes

         recog ( 20 ) = 'YWD '
         parsed( 20 ) =  ywd
         forml ( 20 ) =  no
         needy ( 20 ) =  yes

         recog ( 21 ) = 'YWDF '
         parsed( 21 ) =  ywdf
         forml ( 21 ) =  yes
         needy ( 21 ) =  yes

         call orderc ( recog,  nrec, ordvec )
         call reordc ( ordvec, nrec, recog  )
         call reordi ( ordvec, nrec, parsed )
         call reordl ( ordvec, nrec, forml  )
         call reordl ( ordvec, nrec, needy  )

!
!        Initialize the local POOL counter to user value.
!
         call zzctruin( usrctr )

!
!        Set up the kernel pool watchers
!
         vars(1) = 'DELTET/DELTA_AT'

         call swpool ( 'TTRANS', 1, vars )

      end if

!
!     Check to see if any of the kernel items required by this
!     routine have been updated since the last call to this
!     entry point.
!
      call zzcvpool ( 'TTRANS', usrctr, update )

      if ( update .or. nodata ) then
!
!        We load the TAI-UTC offsets and formal leapsecond epochs
!        into the TAITAB.  (We will modify this array in a minute).
!
         call gdpool ( 'DELTET/DELTA_AT',  1,       2*maxlp, &
                       nref,               taitab,  found    )

!
!        Make sure all of the requested data was there.
!
         if ( .not. found ) then

            nodata = .true.

            call setmsg ( 'The variable that points to the '          // &
                          'leapseconds (DELTET/DELTA_AT) could not '  // &
                          'be located in the kernel pool.  It is '    // &
                          'likely that the leapseconds kernel has '   // &
                          'not been loaded.'                          )
            call sigerr ( 'SPICE(NOLEAPSECONDS)'                      )
            call chkout ( 'TTRANS'                                    )
            return

         end if


!
!        Transform the TAITAB in place to give the TAI time tag
!        at the beginning of the UTC day in which a leap
!        second occurred and the TAI time tag at the beginning
!        of the next day.  Pictorially, the table is transformed
!
!               +----------------------+         +-------------------+
!               | DELTA_1 (TAI to UTC) |         | TAI at start of   |
!               |                      |         | day before TAI-UTC|
!               |                      |         | change occurred   |
!               +----------------------+         +-------------------+
!        from:  | First Formal time    |     to: | TAI time at start |
!               | associated with      |         | of next day UTC.  |
!               | DELTA_1              |         | after DELTA_1 jump|
!               +----------------------+         +-------------------+
!               | DELTA_2 (TAI to UTC) |         | TAI at start of   |
!               |                      |         | day before TAI-UTC|
!               |                      |         | jump occurred     |
!               +----------------------+         +-------------------+
!               | First Formal time    |         | TAI time at start |
!               | associated with      |         | of next day UTC.  |
!               | DELTA_2              |         | after DELTA_2 jump|
!               +----------------------+         +-------------------+
!                        .                                .
!                        .                                .
!                        .                                .
!
!
!        At the same time, load the table DAYTAB. It contains the
!        the day number past 1 Jan 1 AD for the beginning of the
!        days loaded in TAITAB.
!
         lastdt = taitab(1) - 1.0d0

         do i = 1, nref, 2

            offset         = i
            refptr         = i + 1

            dt             = taitab(offset)
            formal         = taitab(refptr)
            taitab(offset) = formal - secspd + lastdt
            taitab(refptr) = formal          + dt

            daynum         = int ( ( formal + halfd ) / secspd ) &
                           + dn2000

            daytab(offset) = daynum - 1
            daytab(refptr) = daynum

            lastdt         = dt

         end do

!
!        Since we don't have to do it very often, make sure the
!        times in the TAI table are in increasing order.
!
         do i = 2, nref

            nodata = .true.

            if ( taitab(i-1) .ge. taitab(i) ) then
               call setmsg ( 'Either the leapsecond epochs taken '    // &
                             'from the kernel pool are not properly ' // &
                             'ordered or the UTC - TAI offsets '      // &
                             'are completely out of range. '          )
               call sigerr ( 'SPICE(BADLEAPSECONDS)'                  )
               call chkout ( 'TTRANS'                                 )
               return
            end if

         end do

!
!        At this point, we've completed all checks on kernel data.
!
         nodata = .false.

      end if

!
!     Make local normalized copies of FROM and TO.
!
      call nextwd ( from,   myfrom, rest )
      call nextwd ( to,     myto,   rest )
      call ucase  ( myfrom, myfrom       )
      call ucase  ( myto,   myto         )

!
!     Make sure that the FROM and TO are recognized time types.
!
      pto   = bsrchc ( myto,   nrec, recog )
      pfrom = bsrchc ( myfrom, nrec, recog )

!
!     Eventually, we need to handle SCLKs.  When that happens
!     we will do it here and in a similarly marked spot at
!     the end of this routine.  First see if we know how to
!     handle the FROM system.
!
!     IF ( PFROM .EQ. 0 ) THEN
!
!        CALL ISSCLK ( FROM,ERROR, FOUND )
!
!        IF ( .NOT. FOUND ) THEN
!           IF ( ERROR .NE. ' ' ) THEN
!              CALL SETMSG ( ERROR )
!              CALL SIGERR ( 'SPICE(TIMESYSTEMPROBLEM)' )
!              CALL CHKOUT ( 'TTRANS' )
!              RETURN
!           END IF
!        ELSE
!           CALL SCLKTV ( FROM, TVEC )
!           PFROM = TDB
!        END IF
!
!     END IF
!
!     Now check to see if we know how to handle the  TO system.
!
!     IF ( PTO .EQ. 0 ) THEN
!
!        CALL ISSCLK ( TO, ERROR, FOUND )
!
!        IF ( .NOT. FOUND ) THEN
!
!           IF ( ERROR .NE. ' ' ) THEN
!              CALL SETMSG ( ERROR )
!              CALL SIGERR ( 'SPICE(TIMESYSTEMPROBLEM)' )
!              CALL CHKOUT ( 'TTRANS' )
!           END IF
!
!        ELSE
!
!           MKSCLK = .TRUE.
!           PTO    =  TDB
!
!        END IF
!
!     END IF
!
!
!     For now we are NOT going to deal with SCLK so if something
!     isn't recognized, we can just signal an error and quit.
!
      if ( pfrom .eq. 0 ) then

         call setmsg ( 'The FROM time representation ''#'' is ' &
         //            'not recognized. ' )
         call errch  ( '#', from                   )
         call sigerr ( 'SPICE(UNKNONWNTIMESYSTEM)' )
         call chkout ( 'TTRANS'                    )
         return

      else if ( pto .eq. 0 ) then

         call setmsg ( 'The TO time representation ''#'' is ' &
         //            'not recognized. ' )
         call errch  ( '#', from                   )
         call sigerr ( 'SPICE(UNKNONWNTIMESYSTEM)' )
         call chkout ( 'TTRANS'                    )
         return

      end if

!
!     OK.  We have made our last attempt at diagnosing a user error.
!     From this point on we assume that the user input exactly what
!     was intended.
!
!     We convert the time vector to days past 1 jan 01 and seconds
!     past the beginning of the day.  None of the cases below
!     are particularly tricky.  There's just a lot of cases.
!
      if ( (pfrom .eq. ymd) .or. (pfrom .eq. ymdf) ) then

         year   = int( tvec(1) )
         month  = int( tvec(2) )
         day    = int( tvec(3) )

         call rmaini ( month-1, 12, dyear, month )

         year   = year  + dyear
         month  = month + 1
         doffst = 0

         if ( year .le. 0 ) then

            call rmaini ( year, 400, yr400, tempi )
            year = tempi

            if ( year .eq. 0 ) then
               year  = year + 400
               yr400 = yr400 - 1
            end if

            doffst = dp400y * yr400

         end if

         daynum = dp0001 ( year,          month,         day     ) &
                + doffst

!
!        Calculate seconds from midnight, 00:00:00.
!
         secs   = hmssec( tvec(4), tvec(5), tvec(6) )

      else if ( pfrom .eq. yd  .or. pfrom .eq. ydf ) then

         year   = int( tvec(1) )
         day    = int( tvec(2) )
         month  = 1

         doffst = 0

         if ( year .le. 0 ) then

            call rmaini ( year, 400, yr400, tempi )
            year = tempi

            if ( year .eq. 0 ) then
               year  = year + 400
               yr400 = yr400 - 1
            end if

            doffst = dp400y * yr400

         end if

         daynum = dp0001 ( year,          month,         day     ) &
                + doffst

!
!        Calculate seconds from midnight, 00:00:00.
!
         secs   = hmssec ( tvec(3), tvec(4), tvec(5) )

      else if ( pfrom .eq. ydd .or. pfrom .eq. yddf   ) then

         year   = int(tvec(1))
         day    = int(tvec(2))
         month  = 1

         doffst = 0

         if ( year .le. 0 ) then

            call rmaini ( year, 400, yr400, tempi )
            year = tempi

            if ( year .eq. 0 ) then
               year  = year + 400
               yr400 = yr400 - 1
            end if

            doffst = dp400y * yr400

         end if

         frac   = tvec(2) - dble(day)
         daynum = dp0001  ( year, month, day ) &
                + doffst

!
!        Normally the length of a day is 86400 seconds, but this day
!        might be a leapsecond day.  We will set DAYLEN to SECSPD and
!        change it if it turns out this is a day with a leapsecond.
!
         if ( pfrom .eq. yddf ) then
            secs = frac * secspd
         else
            daylen = secspd
            dayptr = lstlei ( daynum, nref, daytab )

            if ( odd(dayptr) ) then
               daylen = taitab(dayptr+1) - taitab(dayptr)
            end if

            secs = frac * daylen
         end if


      else if ( pfrom .eq. frml ) then

!
!        First lets get the number of days since 1-Jan-2000 00:00:00
!
         call rmaind ( tvec(1) + halfd, secspd, dp2000, secs )

         daynum = int(dp2000) + dn2000



      else if ( pfrom .eq. jdutc  ) then

!
!        JD1101 is the julian date UTC of Jan 1, 1 AD.
!
         jdsecs = (tvec(1) - jd1101)*secspd

         call rmaind ( jdsecs, secspd, daydp, secs )

         daynum = int(daydp)



      else if ( pfrom .eq. daysec ) then

         daynum = int(tvec(1))
         secs   = tvec(2)



      else if ( pfrom .eq. dayp2 ) then

         daynum = int(tvec(1)) + dn2000
         secs   = tvec(2)



      else if ( (pfrom .eq. ywd) .or. (pfrom .eq. ywdf) ) then

         year   = int( tvec(1) )
         week   = int( tvec(2) ) - 1
         wkday  = int( tvec(3) )
         month  = 1

!
!        Compute the days past 1 jan 1 of the beginning of this
!        year and month.
!

         doffst = 0

         if ( year .le. 0 ) then

            call rmaini ( year, 400, yr400, tempi )
            year = tempi

            if ( year .eq. 0 ) then
               year  = year + 400
               yr400 = yr400 - 1
            end if

            doffst = dp400y * yr400

         end if

         daynum = dp0001 ( year, month, 1 ) &
                + doffst

         call rmaini ( daynum - sunday, 7, qint, dpsun )

         fyrday = dpsun + 1

         call rmaini ( wkday - fyrday, 7,  qint, offset )

         daynum = daynum + week*7 + offset

!
!        Calculate seconds from midnight, 00:00:00.
!
         secs   = hmssec ( tvec(4), tvec(5), tvec(6) )

      else if ( pfrom .eq. ymwd .or. pfrom .eq. ymwdf ) then

         year   = int( tvec(1) )
         month  = int( tvec(2) )
         week   = int( tvec(3) ) - 1
         day    = int( tvec(4) )

         doffst = 0

         if ( year .le. 0 ) then

            call rmaini ( year, 400, yr400, tempi )
            year = tempi

            if ( year .eq. 0 ) then
               year  = year + 400
               yr400 = yr400 - 1
            end if

            doffst = dp400y * yr400

         end if

         daynum = dp0001 ( year, month, 1 ) &
                + doffst

         call rmaini ( daynum - sunday, 7, qint, dpsun  )
         fmday  = dpsun + 1

         call rmaini ( day - fmday, 7, qint, offset )

         daynum = daynum + week*7 + offset

!
!        Calculate seconds from midnight, 00:00:00.
!
         secs   = hmssec ( tvec(5), tvec(6), tvec(7) )




!
!     If we get to this point the type must be one of the continuous
!     time types: 'TAI', 'TDT', 'TDB', 'JED', 'ET', 'JDTDT', 'JDTDB'.
!
      else
!
!        If the output time is one of the continuous time systems
!        we can take a short cut and just perform the computation
!        directly.
!
         if ( elemc ( myto, unifrm ) ) then

            tvec(1) = unitim ( tvec(1), myfrom, myto )
            call chkout ( 'TTRANS' )
            return

         end if

!
!        The output time system isn't one of the uniform time systems.
!        Convert what we have to TAI and then to the DAYNUM, SECOND
!        representation.
!
         tai    = unitim ( tvec(1), myfrom, 'TAI'   )
         taiptr = lstled ( tai,     nref,    taitab )

!
!        If the TAIPTR value is odd, then the TAI time falls during
!        a day with a leap second.  We can just look up the day
!        number and compute the number of seconds into that
!        day directly ...
!
         if ( odd(taiptr) ) then

            daynum = daytab (taiptr)
            secs   = tai &
                   - taitab (taiptr)

!
!        ...Otherwise, all days since the reference TAI time have
!        the same number of seconds (SECSPD).  (This statement applies
!        to days that precede the first reference TAI time too.)
!        Thus we can simply compute the number of days and seconds
!        that have elapsed since the reference TAI time.
!
         else

!
!           If TAI is before the first time in the table, we can
!           compute the number of days and seconds before the first
!           entry in the TAI table.
!
            taiptr = max ( taiptr, 1)

            call  rmaind ( tai     - taitab(taiptr), secspd, &
                           daydp,                    secs    )

            daynum = int ( daydp ) + daytab(taiptr)

         end if


      end if

      if ( forml ( pfrom )  ) then

         call rmaind ( secs, secspd, daydp, tsecs )
         daynum = daynum + int(daydp)
         secs   = tsecs

      end if


!     ==================================================================
!
!     Force the seconds into the range 0 to 86401 or 86400
!     depending upon whether or not the output system is a formal
!     time system or not.
!
      if ( forml(pto) .and. forml(pfrom) ) then
!
!        We don't have to do anything here.
!
      else

         if (      (secs .gt. (secspd - 1.0d0)) &
              .or. (secs .lt. 0.0d0)          ) then

!
!           First convert to TAI...
!
            dayptr = max( 1, lstlei ( daynum, nref, daytab ) )
            secs   = secs &
                   + dble ( daynum - daytab(dayptr) ) * secspd
            tai    = taitab(dayptr) + secs

!
!           ...then back to DAYNUM and SECS
!
            taiptr = lstled ( tai, nref, taitab )

            if ( odd(taiptr) ) then

               daynum = daytab ( taiptr )
               secs   = tai &
                      - taitab ( taiptr )

            else

               taiptr = max ( 1, taiptr )
               daynum = daytab ( taiptr )

               call rmaind ( tai - taitab(taiptr), secspd, daydp, secs )

               daynum = daynum + int ( daydp )

            end if
         end if

      end if
!
!     One last thing.  If we are going to a formal time vector,
!     we want to ignore positive leapseconds. (Negative ones
!     were handled above, the clock jumped ahead one second
!     when the second hand got to 59.)
!
!     The idea is that we want the clock
!     to stand still during the leapsecond.  Yeah this is bogus,
!     but people with analog clocks don't have any other choice.
!
!     We are in a positive leapsecond only if SECS is greater than
!     the number of seconds in a normal day.  In that case we
!     increment the day number by one and set SECS to zero.
!

      if ( forml(pto) .and. secs .gt. secspd ) then
         daynum = daynum + 1
         secs   = 0
      end if
!
!     OK. Now we have DAYNUM and SECS,  convert this form to the
!     one requested.
!
!     If there is a 'Y' in the form we are to convert to, then we
!     will need some form of year, etc.  Do the work now and sort it
!     it all out at the appropriate time later on.
!
      if ( needy ( pto )  ) then

         yr400  =  daynum / dp400y
         rem    =  daynum - dp400y*yr400
!
!        We want to be able to deal with years prior to  1 Jan 1
!        So we make sure the remainder is positive.
!
         if ( rem .lt. 0 ) then
            yr400 = yr400 - 1
            rem   = rem + dp400y
         end if

         yr100  =  min ( 3, rem   / dp100y )
         rem    =  rem    - yr100 * dp100y

         yr4    =  min (24, rem / dp4y )
         rem    =  rem    - yr4 * dp4y

         yr1    =  min (3,  rem / dp1y )
         rem    =  rem    - yr1 * dp1y

         dofyr  =  rem    + 1

         year   = yr400*400 + yr100*100 + yr4*4 + yr1 + 1

         if ( ldays(year) .eq. 0 ) then
            month  = lstlti ( dofyr, 12, dpjan0 )
            day    = dofyr - dpjan0(month)
         else
            month  = lstlti ( dofyr, 12, dpbegl )
            day    = dofyr - dpbegl(month)
         end if


!
!        We only want to convert that portion of seconds less than
!        86399 to hours, minutes and seconds.  Take anything extra
!        and put it in EXSECS.
!
         exsecs = max ( 0.0d0, secs - secspd + 1 )
         tsecs  = secs - exsecs

         call rmaind ( tsecs, 3600.0d0, hours, tempd )
         call rmaind ( tempd,   60.0d0, mins,  tsecs )

         tsecs  = tsecs + exsecs

      end if

!=====================================================================
!
!     Finally, we convert to the requested output.
!
      if      ( pto .eq. ymd .or. pto .eq. ymdf ) then

         tvec(1) = dble ( year  )
         tvec(2) = dble ( month )
         tvec(3) = dble ( day   )
         tvec(4) = hours
         tvec(5) = mins
         tvec(6) = tsecs

      else if ( pto .eq. yd .or. pto .eq. ydf  ) then

         tvec(1) = dble ( year  )
         tvec(2) = dble ( dofyr )
         tvec(3) = hours
         tvec(4) = mins
         tvec(5) = tsecs

      else if ( pto .eq. ydd .or. pto .eq. yddf   ) then

         tvec(1) = dble   ( year  )

         if ( pto .eq. ydd ) then
            dayptr  = lstlei ( daynum, nref, daytab )
            daylen  = secspd

            if ( odd(dayptr) ) then
               daylen = taitab(dayptr+1) - taitab(dayptr)
            end if

            tvec(2) = dble(dofyr) + secs/daylen
         else
            tvec(2) = dble(dofyr) + secs/secspd
         end if


      else if ( pto .eq. frml ) then

         tvec(1) = dble ( daynum - dn2000 ) * secspd &
                 - halfd &
                 + secs

      else if ( pto .eq. jdutc  ) then

         tvec(1) = jd1101 + dble( daynum ) + secs/secspd

      else if ( pto .eq. daysec ) then

         tvec(1) = dble(daynum)
         tvec(2) = secs

      else if ( pto .eq. dayp2 ) then

         tvec(1) = dble (daynum - dn2000)
         tvec(2) = secs

      else if ( pto  .eq. ywd .or. pto .eq. ywdf ) then
!
!        First compute the day of the week, and the week number
!
         call   rmaini  ( daynum - sunday, 7, qint, day )
         week    = 1 + (( dofyr  - 1 )   / 7)

!
!        Now just put everything where it belongs.
!
         tvec(1) = dble(year)
         tvec(2) = dble(week)
         tvec(3) = dble(day) + 1.0d0
         tvec(4) = hours
         tvec(5) = mins
         tvec(6) = tsecs

      else if ( pto .eq. ymwd .or. pto .eq. ymwdf ) then

!
!        First compute how many weeks into the month DAYNUM is,
!        and compute the day of week number.
!
         tvec(1) = dble (year)
         doffst  = 0

         if ( year .le. 0 ) then

            call rmaini ( year, 400, yr400, tempi )
            year = tempi

            if ( year .eq. 0 ) then
               year  = year + 400
               yr400 = yr400 - 1
            end if

            doffst = dp400y * yr400

         end if

         week    = 1 + ( daynum - dp0001(year, month, 1) - doffst ) / 7
         call rmaini   ( daynum - sunday,   7, qint, day )

!
!        Now just move the remaining stuff into TVEC.
!
         tvec(2) = dble (month)
         tvec(3) = dble (week)
         tvec(4) = dble (day) + 1.0d0
         tvec(5) = hours
         tvec(6) = mins
         tvec(7) = tsecs

!
!     If we get to this point the type must be one of the continuous
!     time types: 'TAI', 'TDT', 'TDB', 'JED', 'ET', 'JDTDT', 'JDTDB'.
!
!     First convert to TAI and then to the appropriate output type.
!
      else

         dayptr  = max( 1, lstlei ( daynum, nref, daytab ) )
         secs    = secs &
                 + dble ( daynum - daytab(dayptr) ) * secspd
         tai     = taitab(dayptr) + secs

         tvec(1) = unitim ( tai, 'TAI', myto )

      end if


!
!     Here's where we will handle conversion to SCLK when
!     we get around to implementing that portion of TTRANS
!
!
!     IF ( MKSCLK ) THEN
!        CALL TVSCLK ( TO, TVEC )
!     END IF
!
!     END IF
!

      call chkout ( 'TTRANS' )
      return
      end
