!$Procedure TCHECK ( Time Check )

      subroutine tcheck ( tvec, type, mods, modify, ok, error )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Determine whether the components of a time vector are in the
!     "usual" range for the components, if component checking is
!     enabled.
!
!     If component checking is not enabled, this routine simply
!     returns after setting the outputs.
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

      real(wp)              tvec   ( * )
      character(*)          type
      logical               mods
      character(*)          modify ( * )
      logical               ok
      character(*)          error

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TVEC       I   A vector of time components
!     TYPE       I   The type of time vector.
!     MODS       I   A logical indicating the presence of modifiers
!     MODIFY     I   The values of the modifiers
!     OK         O   Indicates success or failure of component checks.
!     ERROR      O   Diagnostic message if .NOT. OK.
!
! $ Detailed_Input
!
!     TVEC     is an array of double precision numbers that
!              represent the components of some calendar epoch.
!
!     TYPE     is kind of calendar epoch represented by TVEC
!              legitimate values are 'YMD' and 'YD'
!
!     MODS     is a logical flag indicating whether any of the
!              items in MODIFY are non-blank. If some item
!              in MODIFY is non-blank, MODS will be .TRUE. If
!              all items in MODIFY are blank, MODS will be .FALSE.
!
!     MODIFY   is an array of strings indicating how the
!              interpretation of the various components of TVEC
!              should be modified. Blank values indicate that
!              the default interpretation should be applied.
!              Non-blank components will have the following values
!              and meanings.
!
!
!               Component   Meaning   Possible Non-blank Modifier Values
!               ---------   -------   ----------------------------------
!               1           ERA       'A.D.', 'B.C.'
!               2           Weekday   'SUN',  'MON', ... etc.
!               3           AM/PM     'A.M.', 'P.M.'
!               4           System    'UTC',  'TDB', 'TDT'
!               5           Time Zone 'UTC+i:i', 'UTC-i:i'
!
! $ Detailed_Output
!
!     OK       is returned .TRUE. if all components of TVEC are within
!              the normal range of values. If some problem arises,
!              OK will be returned with the value .FALSE. Note that
!              component checking has not been enabled by a call
!              to TPARCH, the value of OK is automatically set to
!              .TRUE.
!
!     ERROR    if OK is returned with the value .TRUE., ERROR will be
!              returned as a blank. However, if OK is .FALSE., ERROR
!              will contain a diagnostic indicating what was wrong
!              with the components of TVEC. Note that
!              component checking has not been enabled by a call
!              to TPARCH, the value of ERROR is automatically set to
!              a blank.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  All problems with TVEC are reported via the logical OK
!         and the message ERROR.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine works in conjunction with the entry point TPARCH.
!     If TPARCH has not been called with the input value 'YES' this
!     routine simply sets the outputs as indicated above and returns.
!
!     Usually strings such as February 32, 1997 are regarded as
!     erroneous. However, the SPICE time subsystem is capable
!     of attaching meaning to such strings. The routines TPARCH and
!     TCHECK allow you to treat such strings as erroneous throughout
!     the SPICE time sub-system.
!
!     This routine examines the components of a time vector and
!     determines whether or not all of the values in the vector
!     are within the normal bounds.
!
!     To pass inspection:
!
!        Years must be integers.
!
!        Months must be in the range from 1 to 12 and must be integers.
!
!        Days of the month must be in the normal ranges. For example
!             if the month specified is January, the day of the month
!             must be greater than or equal to 1.0D0 and strictly less
!             than 32.0D0 (The normal range for February is a function
!             of whether the year specified is a leap year. The
!             Gregorian calendar is used to determine leap years.)
!
!        Day of the year must be greater than or equal to 1.0D0
!             and strictly less than 366.0D0  (367.0D0 in a leap year.
!             The Gregorian calendar is used to determine leap years.)
!
!        Hours must be greater than or equal to 0.0D0 and strictly
!             less than 24.0D0. If the AMPM modifier is included
!             hours must be greater than or equal to 1.0D0 and strictly
!             less than 13.0D0.
!
!        Minutes must be greater than or equal to 0.0D0 and must
!             be strictly less than 60.0D0
!
!        Seconds must be greater than or equal to 0.0D0 and strictly
!             less than 60.0D0 (61.0D0 during the last minute of the
!             30th of June and the 31st of December).
!
!        If some component other than the seconds component is
!        not an integer, all components of lesser significance must
!        be zero.
!
!     This routine  is designed to work in conjunction
!     with the SPICE routine TPARTV and it is anticipated that
!     it will be called in the following fashion
!
!        CALL TPARTV ( STRING, TVEC, NTVEC,  TYPE,
!       .              MODIFY, MODS, YABBRV, SUCCES, ERROR )
!
!        IF ( .NOT. SUCCES ) THEN
!
!           communicate the diagnostic message and
!           take other actions as appropriate
!
!           RETURN
!
!        END IF
!
!        IF ( SUCCES .AND. CHECK ) THEN
!            CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
!        END IF
!
!        IF ( .NOT. OK ) THEN
!
!           communicate the diagnostic message and
!           take other actions as appropriate
!
!           RETURN
!
!        END IF
!
! $ Examples
!
!     Suppose that you have parsed a string (via TPARTV) and want
!     to enforce normal ranges of the components. The following
!     sequence of calls will perform the checks on components.
!
!        get the current checking setting
!
!        CALL TCHCKD ( CURNT )
!
!        turn on component checking.
!
!        CALL TPARCH ( 'YES' )
!
!        Check the components.
!
!        CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
!
!        Reset the checking setting to the original value.
!
!        CALL TPARCH ( CURNT )
!
!
!        Now handle any problems that were diagnosed by TCHECK
!
!        IF ( .NOT. OK ) THEN
!
!           do something
!
!        END IF
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
!-    SPICELIB Version 1.2.0, 01-NOV-2021 (JDR) (EDW)
!
!        Added logic to prevent evaluation of MODIFY when MODS false.
!
!        Added text listing routines affected and not affected by
!        explicit assignments to TPARCH.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 31-JAN-2017 (NJB)
!
!        Bug fix: updated logic so that B.C. leap years are recognized.
!
!-    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS)
!
!        Fixed typo in the $Declarations section in the TPARCH header:
!        STRING -> TYPE.
!
!-    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Check the components of a time vector
!
!-&


!
!     SPICELIB functions
!
      logical               eqstr

!
!     In-line Functions
!
      integer(ip)           divble


      integer(ip)           era
      parameter           ( era    =  1 )

      integer(ip)           wday
      parameter           ( wday   = era    + 1 )

      integer(ip)           zone
      parameter           ( zone   = wday   + 1 )

      integer(ip)           ampm
      parameter           ( ampm   = zone   + 1 )

      integer(ip)           system
      parameter           ( system = ampm   + 1 )

      integer(ip)           nmods
      parameter           ( nmods = system )


!
!     Local Variables
!
      integer(ip)           lnsize
      parameter           ( lnsize = 200 )

      character(lnsize)     messge

      character(10)         mnames ( 12 )
      character(7)          cname  (  4 )

      real(wp)              dinmon ( 12 )

      real(wp)              doy
      real(wp)              dinyr
      real(wp)              jun30
      real(wp)              hubnd
      real(wp)              hlbnd

      integer(ip)           comp
      integer(ip)           day
      integer(ip)           hour
      integer(ip)           i
      integer(ip)           j
      integer(ip)           k
      integer(ip)           leapdy
      integer(ip)           minute
      integer(ip)           month
      integer(ip)           myear
      integer(ip)           second
      integer(ip)           year

      logical               dochck
      logical               modtru

      save

      data                  dochck / .false. /

      data                  dinmon / 31.0_wp,  28.0_wp, &
                                     31.0_wp,  30.0_wp, &
                                     31.0_wp,  30.0_wp, &
                                     31.0_wp,  31.0_wp, &
                                     30.0_wp,  31.0_wp, &
                                     30.0_wp,  31.0_wp  /

      data                  mnames / 'January',    'February', &
                                     'March',      'April', &
                                     'May',        'June', &
                                     'July',       'August', &
                                     'September',  'October', &
                                     'November',   'December' /

      data                  cname / 'days',        'hours', &
                                    'minutes',     'seconds' /
!
!     The in-line function DIVBLE returns 1 if YEAR is divisible
!     by I,  it returns 0 otherwise.
!
      divble(year,i) = max ( 0, 1 + ((iabs(year)/i)*i) - iabs(year) )
!
!     If checking isn't enabled, there is nothing to do.
!
      if ( .not. dochck ) then
         ok    = .true.
         error = ' '
         return
      end if
!
!     Ok.  Checking has been enabled.  Proceed with the various
!     checks.
!
      year = nint( tvec(1) )

      modtru = .false.
      if ( mods ) then
         modtru = (modify(era) == 'B.C.')
      end if

      if ( modtru ) then
         myear = 1 - year
      else
         myear = year
      end if

      leapdy    =  divble( myear, 4   ) &
                -  divble( myear, 100 ) &
                +  divble( myear, 400 )

      dinmon(2) = 28.0_wp  + real(leapdy, wp)
      dinyr     = 365.0_wp + real(leapdy, wp)
      jun30     = 181.0_wp + real(leapdy, wp)
!
!     The error message that will be attached to an out of range
!     problem for hours depends upon whether the AMPM modifier
!     was specified.  We set up valid range as well as the out
!     of range messages here.
!
      modtru = .false.
      if ( mods ) then
         modtru = (modify(ampm) /= ' ')
      end if

      if ( modtru ) then

         hubnd  = 13.0_wp
         hlbnd  =  1.0_wp
         messge = 'The hours component of the time specified was ' &
         //       '#. When either A.M. or P.M. is specified with ' &
         //       'the time the hours component must be at least ' &
         //       '1.0D0 and less than 13.0D0. '

      else

         hubnd  = 24.0_wp
         hlbnd  =  0.0_wp
         messge = 'The hours component of the time specified was ' &
         //       '#.  The hours component must be greater than ' &
         //       'or equal to 0.0D0 and less than 24.0D0. '

      end if
!
!     We only check YD and YMD anything else is out of the
!     province of this routine.
!
      if ( type /= 'YD' .and. type /= 'YMD' ) then

         ok    = .false.
         error = 'The type of the time vector specified was #, ' &
         //      'only ''YD'' and ''YMD'' are recognized. '
         call repmc ( error, '#', type, error )
         return

      end if


!
!     First check.  The year must be an integer.
!
      if ( tvec(1) /= real(year, wp) ) then
         ok    = .false.
         error = 'The year value was #.  This must be an ' &
         //      'integral value. '
         call repmd ( error,  '#', tvec(1), 8, error )

         return
      end if


      if      ( type == 'YD' ) then

         day    = 2
         hour   = 3
         minute = 4
         second = 5
         doy    = tvec(2)

         if (      tvec(2) >= dinyr + 1.0_wp &
             .or.  tvec(2) < 1.0_wp       ) then

            ok    = .false.
            error = 'Day # has been specified for the year #. ' &
            //      'The correct range for the day of year for ' &
            //      'this year is from 1 to #. '

            call repmd ( error, '#', tvec(2), 8, error )
            call repmi ( error, '#', year,       error )
            call repmi ( error, '#', 365+leapdy, error )
            return

         end if


      else if ( type == 'YMD' ) then

         month  = nint( tvec(2) )
         day    = 3
         hour   = 4
         minute = 5
         second = 6
         doy    = 0.0_wp

         if ( tvec(2) /= real(month, wp) ) then

            ok    = .false.
            error = 'The month specified, #, was not an integer. ' &
            //      'The month must be an integer in the range ' &
            //      'from 1 to 12. '
            call repmd ( error,  '#', tvec(2), 3, error )
            return

         else if (      tvec(2) <   1.0_wp &
                   .or. tvec(2) >  12.0_wp ) then

            ok    = .false.
            error = 'The month specified was #.  The month must ' &
            //      'be an integer in the range from 1 to 12 ' &
            //      '(inclusive). '
            call repmi ( error, '#', month, error )
            return

         else if (      tvec(3) < 1.0_wp &
                   .or. tvec(3) >= dinmon(month) + 1.0_wp ) then

            ok    = .false.
            error = 'The day of the month specified for the ' &
            //      'month of # was #.  For # the day must be at ' &
            //      'least 1.0D0 and less than #. '
            call repmc ( error, '#', mnames(month), error )
            call repmd ( error, '#', tvec(3), 3,    error )
            call repmc ( error, '#', mnames(month), error )
            call repmd ( error, '#', dinmon(month)+1.0_wp, 2, error )
            return

         end if

         do i = 1, month -1
            doy = doy + dinmon(i)
         end do

         doy = doy + tvec(3)


      end if


!
!     Make sure the hours, minutes and seconds are all in range.
!
      if (     tvec(hour) >= hubnd &
          .or. tvec(hour) < hlbnd ) then

         ok    = .false.
         error = messge
         call repmd ( error,  '#', tvec(hour), 2, error )
         return

      else if (     tvec(minute) >= 60.0_wp &
               .or. tvec(minute) <  0.0_wp ) then

         ok    = .false.
         error = 'The minutes component of the time specified ' &
         //      'was #. This value must be greater than or ' &
         //      'equal to 0.0 and less than 60.0. '
         call repmd ( error,  '#', tvec(minute), 2, error )
         return

      end if

      if (     tvec(second) >= 60.0_wp &
          .or. tvec(second) <  0.0_wp ) then
!
!        We allow for the possibility that we might have a leapsecond.
!
         modtru = .false.
         if ( mods ) then
            modtru = ( modify(ampm) == 'P.M.')
         end if

         if (        tvec(second) < 61.0_wp &
              .and.  tvec(second) >  0.0_wp &
              .and.  tvec(minute) == 59.0_wp &
              .and.  tvec(hour)   == 23.0_wp &
              .and.( doy == dinyr .or. doy == jun30 ) ) then
!
!           Don't do anything.
!
         else if (        tvec(second) < 61.0_wp &
              .and.  tvec(second) >  0.0_wp &
              .and.  tvec(minute) == 59.0_wp &
              .and.  tvec(hour)   == 11.0_wp &
              .and.  modtru &
              .and.( doy == dinyr .or. doy == jun30 ) ) then
!
!           Don't do anything.
!
         else

            ok    = .false.
            error = 'The seconds component of time must be at least ' &
            //      '0.0D0 and less than 60.0D0 (61.0D0 during the ' &
            //      'last minute of June ' &
            //      '30 and December 31). The value supplied was #. '

            call repmd ( error,  '#', tvec(second), 8, error )
            return

         end if
      end if

!
!     One final check.  If some component is not an integer
!     the remaining components must be zero.
!
      comp     = 0

      do i = day, minute

         comp = comp + 1
         k    = comp

         if ( tvec(i) /= nint(tvec(i)) ) then

            do j = i+1, second

               k = k+1

               if ( tvec(j) /= 0.0_wp ) then
                  ok    = .false.
                  error = 'The ''#'' component of the date has a ' &
                  //      'fractional component.  This is ' &
                  //      'allowed only if all components of ' &
                  //      'lesser significance have value 0.0D0. ' &
                  //      'However the ''#'' component has value ' &
                  //      '#. '


                  call repmc ( error, '#', cname(comp), error )
                  call repmc ( error, '#', cname(k),    error )
                  call repmd ( error, '#', tvec(j), 2,  error )
                  return
               end if

            end do

         end if

      end do

!
!     If we make it this far, all components pass the reasonableness
!     tests.
!
      ok    = .true.
      error = ' '
      return




!$Procedure TPARCH ( Parse check---check format of strings )

      entry tparch ( type )

! $ Abstract
!
!     Restrict the set of strings that are recognized by SPICE time
!     parsing routines to those that have standard values for all time
!     components.
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
!
!     CHARACTER*(*)         TYPE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TYPE       I   String: Use 'YES' to restrict time inputs.
!
! $ Detailed_Input
!
!     TYPE     is a character string that is used to adjust the set of
!              strings that will be regarded as valid time strings by
!              SPICE time parsing routines.
!
!              The default behavior of SPICE time software is to allow
!              an extended range of values for the various components
!              (tokens) of a time string. For example, using its default
!              behavior, TPARSE would regard 1993 JAN 367 as a valid
!              time string and return the UTC seconds past the J2000
!              epoch value that corresponds to Jan 2, 1994.
!
!              While this is a "reasonable" interpretation of such a
!              string, there may be occasions when such a string should
!              be regarded as an error.
!
!              By calling TPARCH with a value of 'YES', the action of
!              the time software will be modified. Strings that have
!              components that are out of the range of values used in
!              most English discourse will be regarded as errors. Thus
!              the numeric values of MONTH, DAY, HOUR, MINUTE, and
!              SECOND must satisfy the following conditions to be
!              regarded as legitimate calendar time strings.
!
!                 ITEM     Valid Range
!                 ------   -----------------------------------------
!                 MONTH    1 to 13
!                 DAY      1 to 365 (366 for leap years) when
!                          DAY is interpreted as the day of year
!                          i.e. the month token is empty.
!                          1 to 31  if month is January
!                          1 to 28  (29 in leap years) if month is
!                                   February
!                          1 to 31  if month is March
!                          1 to 30  if month is April
!                          1 to 31  if month is May
!                          1 to 31  if month is June
!                          1 to 30  if month is July
!                          1 to 31  if month is August
!                          1 to 30  if month is September
!                          1 to 31  if month is October
!                          1 to 30  if month is November
!                          1 to 31  if month is December
!                 HOUR     0 to 23
!                 MINUTE   0 to 59
!                 SECOND   0 up to but not including 60 on days that
!                          can not have a leapsecond.
!                          0 up to but not including 61 for times
!                          that are the last second of June or
!                          December. In other words,
!                               JUN 30, 23:59:60.xxxxxx...x
!                          and  DEC 31, 23:59:60.xxxxxx...x
!
!              To reset the action of time software to the default
!              action, set TYPE to a value that is not equivalent to
!              'YES' when case and spaces are ignored.
!
! $ Detailed_Output
!
!     None.
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
!     This routine is used to alter the collections of strings
!     that SPICE software regards as legitimate calendar strings. The
!     default behavior of SPICE software is to accept strings such
!     as FEB 34, 1993 and to interpret these in a "natural way"
!     (FEB 34, 1993 is regarded as MARCH 6, 1993.) This behavior
!     is sometimes useful for "private" programs that you write.
!     However, such a string may be a typo (a finger accidentally hit
!     two keys for the day instead of one). Given that this string
!     does not appear in common usage, you may want to consider
!     that it is more likely the result of erroneous input. You
!     can alter the behavior of SPICE software so that it will
!     treat such a string as an error. To do this call this entry
!     point with TYPE having the value 'YES'.
!
!        CALL TPARCH ( 'YES' )
!
!     Until the behavior is reset by calling TPARCH with a value
!     other than 'YES' (such as 'NO'), SPICE software will treat all
!     out-of-bound components of time strings as errors.
!
!     If you are happy with the SPICE default interpretation of
!     strings, you do not need to make any calls to TPARCH.
!
!     All time parsing routines --including the top-level APIs TPARSE
!     and UTC2ET-- respect the setting assigned by TPARCH, except the
!     SPICELIB routine STR2ET.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) When accepting times as input interactively, you usually
!        read a string typed at a keyboard and then pass that string
!        to the SPICE time system to convert it to an ephemeris time.
!        The default behavior of SPICE software is to accept strings
!        such as FEB 34, 1993 and to interpret these in a "natural way"
!        (FEB 34, 1993 is regarded as MARCH 6, 1993.) The following
!        example code demonstrates how to modify this behavior.
!
!
!        Example code begins here.
!
!
!              PROGRAM TPARCH_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         TIMSTR
!              PARAMETER           ( TIMSTR = 'FEB 34, 1993' )
!
!              INTEGER               ERRMLN
!              PARAMETER           ( ERRMLN = 1000 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(ERRMLN)    ERRMSG
!
!              DOUBLE PRECISION      SP2000
!
!        C
!        C     First, demonstrate the default behavior of SPICE.
!        C     Let's get the number of UTC seconds past J2000 epoch.
!        C
!              CALL TPARSE ( TIMSTR, SP2000, ERRMSG )
!
!              IF ( ERRMSG .EQ. ' ' ) THEN
!
!                 WRITE(*,'(A,F18.6)') 'UTC (s): ', SP2000
!
!              ELSE
!
!                 WRITE(*,'(2A)') 'Error  : ', ERRMSG
!
!              END IF
!
!        C
!        C     Now, turn error checking on and parse the time string
!        C     again.
!        C
!              CALL TPARCH ( 'YES' )
!              CALL TPARSE ( TIMSTR, SP2000, ERRMSG )
!
!              IF ( ERRMSG .EQ. ' ' ) THEN
!
!                 WRITE(*,'(A,F18.6)') 'UTC (s): ', SP2000
!
!              ELSE
!
!                 WRITE(*,'(2A)') 'Error  : ', ERRMSG
!
!              END IF
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        UTC (s):  -215352000.000000
!        Error  : The day of the month specified for the month of Feb***
!
!
!        Warning: incomplete output. 1 line extended past the right
!        margin of the header and has been truncated. This line is
!        marked by "***" at the end of the line.
!
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.2, 01-NOV-2021 (JDR) (EDW)
!
!        Added text listing routines affected and not affected by
!        explicit assignments to TPARCH.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Added TIME to $Required_Reading list.
!
!-    SPICELIB Version 1.0.1, 10-FEB-2014 (BVS)
!
!        Fixed typo in the $Declarations section: STRING -> TYPE.
!
!-    SPICELIB Version 1.0.0, 07-APR-1996 (WLT)
!
!        The entry point TPARCH was moved from TPARSE to the routine
!        TCHECK so that all time parsing actions could be centralized.
!
!-&


! $ Index_Entries
!
!     Restrict time strings to proper form
!
!-&


      dochck = eqstr( type, 'YES' )

      return



!$Procedure TCHCKD ( Time components are checked )

      entry tchckd ( type )

! $ Abstract
!
!     Determine whether component checking is enabled for time strings.
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
!     IMPLICIT NONE
!     CHARACTER*(*)         TYPE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TYPE       O   Answer to the question: "Is checking enabled?"
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     TYPE     is a string that gives the answer to the question
!              "Is checking of components enabled?"  If checking
!              is enabled, the value returned will be "YES" if
!              checking is not enabled, the value returned will
!              be "NO".
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
!     This entry point allows you to "fetch" the current settings
!     regarding the checking of components of a time string. This
!     allows you to temporarily set the action to whatever is desired
!     in a particular piece of code and then reset the action to
!     the setting in effect prior to the routines activities.
!
! $ Examples
!
!     Suppose you'd like to write a routine that always applies
!     component checking to the components of a time string.
!
!     Use this entry point together with TPARCH and TCHECK to
!     make use of the built-in SPICE capabilities
!
!        get the current setting.
!
!        CALL TCHCKD ( CURNT )
!        CALL TPARCH ( 'YES' )
!
!           perform some time
!           parsing activities.
!
!           check the components.
!
!        CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
!
!        Set the checking activity back to the value prior
!        to the work done here.
!
!        CALL TPARCH ( CURNT )
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
!-    SPICELIB Version 1.0.1, 17-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 07-APR-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Get the current time component checking status
!
!-&


      if ( dochck ) then
         type = 'YES'
      else
         type = 'NO'
      end if

      return
      end
