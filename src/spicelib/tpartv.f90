!$Procedure TPARTV ( Time string ---parse to a time vector)

      subroutine tpartv ( string, &
                          tvec,   ntvec, type, &
                          modify, mods,  yabbrv, succes, &
                          pictur, error )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Parse the components of a time string and return a vector of the
!     components of that string. Also return an array of any modifiers
!     present in the input string; these may alter the interpretation
!     of the components.
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

      character(*)          string
      real(wp)              tvec   ( * )
      integer(ip)           ntvec
      character(*)          type
      character(*)          modify ( * )
      logical               mods
      logical               yabbrv
      logical               succes
      character(*)          pictur
      character(*)          error

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   A string to be parsed as a time
!     TVEC       O   A vector giving the components of the time.
!     NTVEC      O   The number of components supplied for TVEC
!     TYPE       O   The type of the "time vector" TVEC
!     MODIFY     O   A list of modifiers present in STRING.
!     MODS       O   A logical indicating the presence of a modifier
!     YABBRV     O   A logical indicating that a year was abbreviated
!     SUCCES     O   A logical indicating whether STRING was parsed.
!     PICTUR     O   A time format picture associated with STRING
!     ERROR      O   A diagnostic message if STRING couldn't be parsed
!
!     The function returns
!
! $ Detailed_Input
!
!     STRING   is a character string that represents some
!              julian or calendar epoch.
!
! $ Detailed_Output
!
!     TVEC     is a vector of double precision numbers that represent
!              the input string. The number and meaning of the
!              components of TVEC depend upon the input string. This
!              meaning can be determined from the output variable
!              TYPE.
!
!                 TYPE     NTVEC     TVEC Components
!                 -----------------------------------------------------
!                 YMD      3 to 6    TVEC(1) is the calendar year
!                                    TVEC(2) is the numeric value of the
!                                            month (1-12)
!                                    TVEC(3) is the day of the month
!                                    TVEC(4) is the hour of the day
!                                    TVEC(5) is the minute of the hour
!                                    TVEC(6) is the second of the minute
!
!                 YD       2 to 5    TVEC(1) is the calendar year
!                                    TVEC(2) is the day of the year
!                                    TVEC(3) is the hour of the day
!                                    TVEC(4) is the minute of the hour
!                                    TVEC(5) is the second of the minute
!
!                 JD       1         TVEC(1) is the julian date
!
!              Note that the values of TVEC are not forced into the
!              normal ranges used in daily conversation.  TPARTV
!              simply reports what's found in the string and does
!              not pass judgement on the "correctness" of these
!              components.
!
!     NTVEC    is the actual number of components that were present
!              in the string. For example a user might have
!              supplied only year, month and day of an epoch.
!              In such a case NTVEC will be set to 3. The components
!              actually supplied will be 1 through NTVEC. Values
!              not supplied are set to zero.
!
!     TYPE     is the type of time string supplied. This is a function
!              of whether the string contains year, month and day,
!              day of year, or julian date.
!
!     MODIFY   is an array of character strings that indicate
!              whether a modifier to the calendar string was supplied.
!              If a particular modifier was not supplied, the
!              value of that component of MODIFY will be set to
!              a blank. Modifiers are used to change the meaning
!              of time strings.
!
!              For example 12:12:29 Jan 1, 1996  means 12 hours past
!              midnight on Jan 1, 1996 in the UTC time system. But
!              if we modify the string to be:
!
!                 12:12:29 A.M. Jan 1, Tuesday PDT 1996 B.C.
!
!              the string takes on an entirely different meaning.
!
!              Five different modifiers are recognized by TPARTV:
!              the era associated with the epoch, day of week of
!              the epoch, time zone of an epoch,  AM/PM used in
!              daily time usage, and the system (UTC, TDB, TT, or TDT).
!
!              Again whether or not modifiers are compatible with the
!              time and date components or with each other is not
!              determined by TPARTV. TPARTV simply reports what is
!              present in the string, leaving the task of deciding
!              the meaning of the string to the calling routine.
!
!              The components of MODIFY, their meaning and possible
!              values are given below.
!
!                                        Possible
!                 Component   Meaning    Non-blank Modifier Values
!                 ---------   ---------  -------------------------
!                 1           ERA        'A.D.', 'B.C.'
!                 2           Weekday    'SUN', 'MON', ... etc.
!                 3           Time Zone  'UTC+i:i', 'UTC-i:i'
!                 4           AM/PM      'A.M.', 'P.M.'
!                 5           System     'UTC', 'TDB', 'TT', 'TDT'
!
!              TPARTV recognizes the standard abbreviations of
!              all continental U.S. time zones.
!
!                 PDT --- Pacific  Daylight Time  (UTC-07:00)
!                 PST --- Pacific  Standard Time  (UTC-08:00)
!                 MDT --- Mountain Daylight Time  (UTC-06:00)
!                 MST --- Mountain Standard Time  (UTC-07:00)
!                 CDT --- Central  Daylight Time  (UTC-05:00)
!                 CST --- Central  Standard Time  (UTC-06:00)
!                 EDT --- Eastern  Daylight Time  (UTC-04:00)
!                 EST --- Eastern  Standard Time  (UTC-05:00)
!
!              In addition it recognizes offsets from UTC expressed
!              as UTC+/-HR:MN. Note that through out SPICELIB
!              the minutes component of the UTC offset are always
!              regarded as positive offsets from the hour offset.
!
!              All Time zones are returned in MODIFY as UTC offsets
!              as indicated in the table above.
!
!     MODS     is .TRUE. if some non-blank modifier was supplied.
!
!     YABBRV   is .TRUE. if a year was supplied in the abbreviated
!              form 'YR  where YR is a two digit integer.
!
!     SUCCES   is .TRUE. if the string was successfully parsed.
!              Otherwise it is set to .FALSE. and a diagnostic
!              is supplied in the argument ERROR.
!
!     PICTUR   is a string that gives a format picture that can
!              be used by the routine TIMOUT to construct a time
!              string of the same form as the input time string.
!
!              If some component of the input string could not be
!              identified, PICTUR is returned as a blank. However,
!              if all components of the input string could be
!              identified and the string is simply ambiguous, PICTUR
!              will contain a format picture that corresponds to
!              the ambiguous input. Consequently, you must check
!              the value of PICTUR to determine if TPARTV has
!              been able to construct a format picture.
!
!     ERROR    is blank if the string was successfully parsed.
!              Otherwise a human readable diagnostic is returned
!              in ERROR.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  All problems detected by this routine are reported via the
!         variables SUCCES and ERROR.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine parses in input string that represents some
!     epoch in some time system. In addition it constructs a
!     format picture that describes the position and meaning
!     of the various components of the string.
!
!     This routine is intended to be used in close conjunction with
!     the routines TTRANS and TIMOUT.
!
!     The string is parsed by first determining its recognizable
!     substrings (integers, punctuation marks, names of months,
!     names of weekdays, time systems, time zones, etc.) These
!     recognizable substrings are called the tokens of the input
!     string. The meaning of some tokens are immediately determined.
!     For example named months, weekdays, time systems have clear
!     meanings. However, the meanings of numeric components must
!     be deciphered from their magnitudes and location in
!     the string relative to the immediately recognized components
!     of the input string.
!
!     To determine the meaning of the numeric tokens in the input
!     string, a set of "productions rules" and transformations are
!     applied to the full set of tokens in the string. These
!     transformations are repeated until the meaning of every token
!     has been determined or until further transformations yield
!     no new clues into the meaning of the numeric tokens.
!
!     1)  Unless the substring 'JD' or 'jd' is present, the string is
!         assumed to be a calendar format (day-month-year or year and
!         day of year). If the substring JD or jd is present, the
!         string is assumed to represent a Julian date.
!
!     2)  If the Julian date specifier is not present, any integer
!         greater than 999 is regarded as being a year specification.
!
!     3)  A dash '-' can represent a minus sign only if it precedes
!         the first digit in the string and the string contains
!         the Julian date specifier (JD). (No negative years,
!         months, days, etc. are allowed).
!
!     4)  Numeric components of a time string must be separated
!         by a character that is not a digit or decimal point.
!         Only one decimal component is allowed. For example
!         1994219.12819 is sometimes interpreted as the
!         219th day of 1994 + 0.12819 days. TPARTV does not
!         support such strings.
!
!     5)  No exponential components are allowed. For example you
!         can't specify the Julian date of J2000 as 2.451545E6.
!         You also can't input 1993 Jun 23 23:00:01.202E-4 and have
!         to explicitly list all zeros that follow the decimal
!         point: i.e. 1993 Jun 23 23:00:00.0001202.
!
!     6)  The single colon (:) when used to separate numeric
!         components of a string is interpreted as separating
!         Hours, Minutes, and Seconds of time.
!
!     7)  If a double slash (//) or double colon (::) follows
!         a pair of integers, those integers are assumed  to
!         represent the year and day of year.
!
!     8)  A quote followed by an integer less than 100 is regarded
!         as an abbreviated year. For example: '93 would be regarded
!         as the 93rd year of the reference century. See the SPICELIB
!         routine TEXPYR for further discussion of abbreviated years.
!
!     9)  An integer followed by 'B.C.' or 'A.D.' is regarded as
!         a year in the era associated with that abbreviation.
!
!     10) All dates are regarded as belonging to the extended
!         Gregorian Calendar (the Gregorian calendar is the calendar
!         currently used by western society). See the SPICELIB routine
!         JUL2GR for converting from Julian Calendar to the Gregorian
!         Calendar.
!
!     11) If the ISO date-time separator (T) is present in the string
!         ISO allowed token patterns are examined for a match
!         with the current token list. If no match is found the
!         search is abandoned and appropriate diagnostic messages
!         are generated. Historically the interpretation of ISO
!         formatted time strings deviates from the ISO standard in
!         allowing two digit years and expanding years in the 0 to 99
!         range the same way as is done for non ISO formatted strings.
!         Due to this interpretation it is impossible to specify
!         times in years in the 0 A.D. to 99 A.D. range using ISO
!         formatted strings on the input.
!
!     12) If two delimiters are found in succession in the time
!         string, the time string is diagnosed as an erroneous string.
!         (Delimiters are comma, white space, dash, slash, period, or
!         day of year mark. The day of year mark is a pair of forward
!         slashes or a pair of colons.)
!
!         Note the delimiters do not have to be the same. The pair
!         of characters ",-" counts as two successive delimiters.
!
!     13) White space and commas serve only to delimit tokens in the
!         input string. They do not affect the meaning of any
!         of the tokens.
!
!     14) If an integer is greater than 1000 (and the 'JD' label
!         is not present, the integer is regarded as a year.
!
!     15) When the size of the integer components does not clearly
!         specify a year the following patterns are assumed
!
!         Calendar Format
!
!            Year Month Day
!            Month Day Year
!            Year Day Month
!
!            where Month is the name of a month, not its numeric
!            value.
!
!            When integer components are separated by slashes (/)
!            as in 3/4/5. Month, Day, Year is assumed (2005 March 4)
!
!         Day of Year Format.
!
!            If a day of year marker is present (// or ::) the
!            pattern
!
!              I-I// or I-I:: (where I stands for an integer)
!
!            is interpreted as Year Day-of-Year. However, I-I/ is
!            regarded as ambiguous.
!
!     The table below gives a list of abbreviations used to
!     classify tokens.
!
!                /   ---  slash punctuation mark
!                H   ---  hour
!                M   ---  Minute
!                S   ---  Second
!                Y   ---  year
!                d   ---  day of year marker
!                i   ---  unsigned integer
!                m   ---  month
!                n   ---  unsigned decimal number
!                y   ---  day of year
!                -   ---  dash punctuation mark
!                D   ---  day of month
!                :   ---  colon punctuation mark
!
!      Given these abbreviations the following (rather lengthy)
!      table gives the set of built in token patterns that
!      are recognized and the associated interpretation of that
!      pattern.
!
!         Pattern         Meaning         Pattern         Meaning
!         ------------------------        -------------------------
!         Y-i-it......... YmD             i/i/ii:i:n..... mDYHMS
!         Y-i-iti........ YmDH            i/i/ii:n....... mDYHM
!         Y-i-iti:i...... YmDHM           i/i/ii:n....... mDYHM
!         Y-i-iti:i:i.... YmDHMS          i:i:ii-i-Y..... HMSmDY
!         Y-i-iti:i:n.... YmDHMS          i:i:ii/i/Y..... HMSmDY
!         Y-i-iti:n...... YmDHM           i:i:ii/i/i..... HMSmDY
!         Y-i-itn........ YmDH            i:i:iimY....... HMSDmY
!         Y-i/........... Yy              i:i:imiY....... HMSmDY
!         Y-i/i:i........ YyHM            i:i:ni-i-Y..... HMSmDY
!         Y-i/i:i:i...... YyHMS           i:i:ni/i/Y..... HMSmDY
!         Y-i/i:i:n...... YyHMS           i:i:ni/i/i..... HMSmDY
!         Y-i/i:n........ YyHM            i:i:nimY....... HMSDmY
!         Y-id........... Yy              i:i:nmiY....... HMSmDY
!         Y-idi:i........ YyHM            i:ii-i-Y....... HMmDY
!         Y-idi:i:i...... YyHMS           i:ii/i/Y....... HMmDY
!         Y-idi:i:n...... YyHMS           i:ii/i/i....... HMmDY
!         Y-idi:n........ YyHM            i:iimY......... HMDmY
!         Y-it........... Yy              i:imiY......... HMmDY
!         Y-iti.......... YyH             i:ni-i-Y....... HMmDY
!         Y-iti:i........ YyHM            i:ni/i/Y....... HMmDY
!         Y-iti:i:i...... YyHMS           i:ni/i/i....... HMmDY
!         Y-iti:i:n...... YyHMS           i:nimY......... HMDmY
!         Y-iti:n........ YyHM            i:nmiY......... HMmDY
!         Y-itn.......... YyH             iYd............ yY
!         Yid............ Yy              iYdi:i......... yYHM
!         Yidi:i......... YyHM            iYdi:i:i....... yYHMS
!         Yidi:i:i....... YyHMS           iYdi:i:n....... yYHMS
!         Yidi:i:n....... YyHMS           iYdi:n......... yYHM
!         Yidi:n......... YyHM            iiY............ mDY
!         Yii............ YmD             iiYi........... mDYH
!         Yiii........... YmDH            iiYi:i......... mDYHM
!         Yiii:i......... YmDHM           iiYi:i:i....... mDYHMS
!         Yiii:i:i....... YmDHMS          iiYi:i:n....... mDYHMS
!         Yiii:i:n....... YmDHMS          iiYi:n......... mDYHM
!         Yiii:n......... YmDHM           iiYn........... mDYH
!         Yiiii.......... YmDHM           iid............ Yy
!         Yiiiii......... YmDHMS          iidi:i......... YyHM
!         Yiiiin......... YmDHMS          iidi:i:i....... YyHMS
!         Yiiin.......... YmDHM           iidi:i:n....... YyHMS
!         Yiin........... YmDH            iidi:n......... YyHM
!         Yim............ YDm             iim............ YDm
!         Yimi........... YDmH            iimi........... YDmH
!         Yimi:i......... YDmHM           iimi:i......... YDmHM
!         Yimi:i:i....... YDmHMS          iimi:i:i....... YDmHMS
!         Yimi:i:n....... YDmHMS          iimi:i:n....... YDmHMS
!         Yimi:n......... YDmHM           iimi:n......... YDmHM
!         Yimn........... YDmH            iimii.......... YDmHM
!         Yin............ YmD             iimiii......... YDmHMS
!         Ymi............ YmD             iimiin......... YDmHMS
!         Ymii........... YmDH            iimin.......... YDmHM
!         Ymii:i......... YmDHM           iimn........... YDmH
!         Ymii:i:i....... YmDHMS          imY............ DmY
!         Ymii:i:n....... YmDHMS          imYi........... DmYH
!         Ymii:n......... YmDHM           imYi:i......... DmYHM
!         Ymin........... YmDH            imYi:i:i....... DmYHMS
!         Ymn............ YmD             imYi:i:n....... DmYHMS
!         Ynm............ YDm             imYi:n......... DmYHM
!         i-Y/........... yY              imYn........... DmYH
!         i-Y/i:i........ yYHM            imi............ YmD
!         i-Y/i:i:i...... yYHMS           imi:i:iY....... DmHMSY
!         i-Y/i:i:n...... yYHMS           imi:i:nY....... DmHMSY
!         i-Y/i:n........ yYHM            imi:iY......... DmHMY
!         i-Yd........... yY              imi:nY......... DmHMY
!         i-Ydi:i........ yYHM            imii........... YmDH
!         i-Ydi:i:i...... yYHMS           imii:i......... YmDHM
!         i-Ydi:i:n...... yYHMS           imii:i:i....... YmDHMS
!         i-Ydi:n........ yYHM            imii:i:n....... YmDHMS
!         i-i-Y.......... mDY             imii:n......... YmDHM
!         i-i-Yi:i....... mDYHM           imiii.......... YmDHM
!         i-i-Yi:i:i..... mDYHMS          imiiii......... YmDHMS
!         i-i-Yi:i:n..... mDYHMS          imiiin......... YmDHMS
!         i-i-Yi:n....... mDYHM           imiin.......... YmDHM
!         i-i-it......... YmD             imin........... YmDH
!         i-i-iti........ YmDH            imn............ YmD
!         i-i-iti:i...... YmDHM           inY............ mDY
!         i-i-iti:i:i.... YmDHMS          inm............ YDm
!         i-i-iti:i:n.... YmDHMS          miY............ mDY
!         i-i-iti:n...... YmDHM           miYi........... mDYH
!         i-i-itn........ YmDH            miYi:i......... mDYHM
!         i-i/i:i........ YyHM            miYi:i:i....... mDYHMS
!         i-i/i:i:i...... YyHMS           miYi:i:n....... mDYHMS
!         i-i/i:i:n...... YyHMS           miYi:n......... mDYHM
!         i-i/i:n........ YyHM            miYn........... mDYH
!         i-idi:i........ YyHM            mii............ mDY
!         i-idi:i:i...... YyHMS           mii:i:iY....... mDHMSY
!         i-idi:i:n...... YyHMS           mii:i:nY....... mDHMSY
!         i-idi:n........ YyHM            mii:iY......... mDHMY
!         i-it........... Yy              mii:nY......... mDHMY
!         i-iti.......... YyH             miii........... mDYH
!         i-iti:i........ YyHM            miii:i......... mDYHM
!         i-iti:i:i...... YyHMS           miii:i:i....... mDYHMS
!         i-iti:i:n...... YyHMS           miii:i:n....... mDYHMS
!         i-iti:n........ YyHM            miii:n......... mDYHM
!         i-itn.......... YyH             miiii.......... mDYHM
!         i/i/Y.......... mDY             miiiii......... mDYHMS
!         i/i/Y/i:n...... mDYHM           miiiin......... mDYHMS
!         i/i/Yi:i....... mDYHM           miiin.......... mDYHM
!         i/i/Yi:i:i..... mDYHMS          miin........... mDYH
!         i/i/Yi:i:n..... mDYHMS          mnY............ mDY
!         i/i/i.......... mDY             mni............ mDY
!         i/i/ii:i....... mDYHM           nmY............ DmY
!         i/i/ii:i:i..... mDYHMS
!
! $ Examples
!
!     Suppose you need to convert various time strings to ephemeris
!     seconds past J2000. The following pair of calls shows
!     how you would use this routine together with the routines
!     TCHECK and TTRANS to perform this task.
!
!
!         CALL TPARTV ( STRING,
!        .              TVEC,   NTVEC, TYPE,
!        .              MODIFY, MODS,  YABBRV, SUCCES,
!        .              PICTUR, ERROR )
!
!
!         IF ( .NOT. SUCCES ) THEN
!
!            Use the SPICE error handling facility to post an
!            error message and signal an error.
!
!            CALL SETMSG ( ERROR )
!            CALL SIGERR ( 'MYCHECK(BADTIME)' )
!            CALL CHKOUT ( 'MYROUTINE' )
!            RETURN
!         END IF
!
!         Check the components of TVEC to make sure everything
!         makes sense.
!
!         CALL TCHECK( TVEC, TYPE, MODS, MODIFY, OK, ERROR )
!
!         IF ( .NOT. OK ) THEN
!
!            Use the SPICE error handling facility to post an
!            error message and signal an error.
!
!            CALL SETMSG ( ERROR )
!            CALL SIGERR ( 'MYCHECK(BADTIME)' )
!            CALL CHKOUT ( 'MYROUTINE' )
!            RETURN
!         END IF
!
!         CALL TTRANS ( TYPE, 'ET', TVEC )
!
!         ET = TVEC(1)
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
!-    SPICELIB Version 3.2.0, 23-DEC-2021 (EDW) (BVS) (JDR)
!
!        ISO format logic recognizes/evaluates ISO time strings
!        with or without trailing 'Z'.
!
!        Reordered header sections. Edited the header to comply with
!        NAIF standard.
!
!        Updated $Exceptions entry #1 wording.
!
!-    SPICELIB Version 3.1.0, 15-AUG-2002 (WLT)
!
!        Replaced the call to INSSUB with ZZINSSUB so that this
!        routine can legitimately be called error free.
!
!-    SPICELIB Version 3.0.0, 10-MAY-1999 (WLT)
!
!        The routine was modified so that weekday followed by a comma
!        is recognized as a legitimate pattern when parsing.
!
!-    SPICELIB Version 2.0.0, 16-APR-1997 (WLT)
!
!        The routine was modified so that last-chance removal of
!        delimiters ',', '-', and '/' are removed one at a time
!        (instead of all at once as in version 1.0.0) and the
!        resulting representation checked against
!        the built-in list.
!
!        In addition the set of built-in patterns was increased
!        from 185 to 203. See ZZTPATS for more details.
!
!-    SPICELIB Version 1.0.0, 10-AUG-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Parse a time string into a vector of components
!
!-&


!
!     SPICELIB Functions
!
      integer(ip)           bsrchc
      integer(ip)           isrchc
      integer(ip)           intmax
      integer(ip)           rtrim

!
!     Private Functions
!
      logical               zztokns
      logical               zzcmbt
      logical               zzremt
      logical               zzsubt
      logical               zzrept
      logical               zznote
      logical               zzist
      logical               zzispt
      logical               zzvalt
      logical               zzgrep
      logical               zztpats
      logical               zzunpck

!
!     Parameters
!
!
!     ERA
!     WDAY
!     ZONE
!     AMPM
!     SYSTEM
!

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

      integer(ip)           begs ( nmods )
      integer(ip)           ends ( nmods )


      logical               havera
      logical               havwdy
      logical               havzon
      logical               havapm
      logical               havsys
!
!     Local Variables.
!
!     The number of known time patterns NKNOWN comes from the include
!     file timepars.inc
!
      integer(ip)           wdsize
      parameter           ( wdsize = 12 )

      integer(ip)           room
      parameter           ( room = 300 )

      integer(ip)           nknown
      character(wdsize)     known ( room )
      character(wdsize)     meanng( room )
      character(wdsize)     rep


!
!     Time Zone Variables
!
      integer(ip)           zsize
      parameter           ( zsize = 3 )

      integer(ip)           osize
      parameter           ( osize = 6 )

      integer(ip)           nzones
      parameter           ( nzones = 8 )

      character(zsize)      zones ( nzones )
      character(osize)      offset( nzones )
      character(1)          delim ( 3 )



      integer(ip)           b
      integer(ip)           b1
      integer(ip)           b2
      integer(ip)           e
      integer(ip)           e1
      integer(ip)           e2
      integer(ip)           from
      integer(ip)           i
      integer(ip)           mapto
      integer(ip)           to
      integer(ip)           use
      integer(ip)           r




      logical               resolv
      logical               l2r
      logical               r2l

      logical               first

      save

      data                  first   / .true. /

      data                  zones   / 'EST',    'EDT', &
                                      'CST',    'CDT', &
                                      'MST',    'MDT', &
                                      'PST',    'PDT'  /

      data                  offset  / 'UTC-5',  'UTC-4', &
                                      'UTC-6',  'UTC-5', &
                                      'UTC-7',  'UTC-6', &
                                      'UTC-8',  'UTC-7'  /

!
!     Standard SPICE error handling.
!
!
!     So far there are no modifiers to the time string.
!
      mods   = .false.
      yabbrv = .false.

      do i = 1, nmods
         modify(i) = ' '
      end do

!
!     On the first call to this routine we load the built in
!     representation patterns.
!
      if ( first ) then

         if ( zztpats( room, nknown, known, meanng ) ) then
            first = .false.
         else
            pictur = ' '
            succes = .false.
            error  = 'There is an incompatibility between ZZTPATS ' &
            //       'and the room allocated for KNOWN in TPARTV.'
            return
         end if

      end if

!
!     First step is to tokenize the string.  The new representation
!     is maintained in ZZTIME.  We'll get it later if we need it.
!
      resolv = zztokns ( string, error )

      if ( .not. resolv ) then
         succes = .false.
         ntvec  =  0
         type   = ' '
         pictur = ' '
         return
      end if

!
!     The result of tokenizing the string will be a representation
!     that contains the following letters.
!
!           '        The quote character
!           [        The left parenthesis
!           ]        The right parenthesis
!           ,        The comma
!           -        The dash
!           .        The decimal point
!           /        The slash---used to separate date components.
!           :        The colon (used to separate time components)
!           N  ---   stands for one of the symbols A.M. or P.M.
!           O        stands for the symbol UTC+
!           Z  ---   stands for a time zone such as PDT, PSD, CDT, etc.
!           b        stands for a block of white space
!           d        stands for the day of year marker (// or ::)
!           e  ---   stands for the era (B.C. or A.D.)
!           j        stands for julian date
!           m        stands for a month
!           o        stands for the symbol UTC-
!           s  ---   stands for a time system (UTC, TDB, TT, TDT)
!           t        stands the ISO date-T-time separator.
!           w  ---   stands for the day of the week.
!           i        stands for a sequence of digits
!
!     We will gradually remove many of these and replace the i, i.
!     and i.i with the following items
!
!           n       stands for a decimal number
!           Y       stands for a year
!           D       stands for the day in a month
!           y       stands for the day of the year
!           H       stands for hours
!           M       stands for minutes
!           S       stands for seconds
!
!
!     We will use the following logical functions to modify
!     the tokenized representation:
!
!        ZZTOKNS --- breaks the string down into a list of recognized
!                    tokens and stores an internal model for this
!                    list.  The begins and ends of the substrings
!                    associated with the tokenization are maintained
!                    inside the routine ZZTIME (which ZZTOKNS is an
!                    entry point to).  If some substring cannot be
!                    recognized, ZZTOKNS returns the value FALSE
!                    together with a diagnostic indicating what
!                    was wrong with the input string.
!
!        ZZCMBT  --- combines one or more tokens into a single token.
!                    this is performed only once and is done either
!                    scanning left to right or right to left.
!                    It returns TRUE if a combination is performed.
!
!        ZZREMT  --- removes all instances of a token from the tokenized
!                    representation.  It returns TRUE is an item
!                    is removed.
!
!        ZZSUBT  --- substitutes the first occurrence of a
!                    subpattern (scanning left to right or right to
!                    left) with another pattern of the same length.
!                    This is where we attach new meaning to the
!                    tokenized pattern.  It returns TRUE if a
!                    substitution is performed.
!
!        ZZREPT  --- is a combination of the ZZSUBT and ZZREMT
!                    This performs ZZSUBT on the string, but then
!                    remove all occurrences of the special character
!                    * from the tokenized list. It returns TRUE
!                    is a substitution is performed.
!
!        ZZNOTE  --- returns the begin and end of the first occurrence
!                    of some token, and then removes the token
!                    from the tokenized representation.  We use this
!                    primarily to extract modifiers from the tokenized
!                    string.  These should occur only once and once
!                    removed allow us to more easily attach meaning
!                    to the remaining tokens. The value of ZZNOTE
!                    is true if the requested item could be found,
!                    otherwise it is false and the begin and end
!                    of the requested substring are set to 0.
!
!        ZZIST   --- returns TRUE if the specified token is present
!                    in the tokenized substring.
!
!        ZZISPT  --- returns true is a pair of consecutive tokens
!                    from a list are located in the representation
!                    of the tokenized string.  This is used to
!                    locate consecutive pairs of delimiters in the
!                    input string. It returns TRUE if a pair of
!                    consecutive items is located.  Otherwise
!                    it returns FALSE.
!
!        ZZVALT  --- allows you to substitute a new token for any
!                    integer (i) that lies within a specified range
!                    of values.  This is primarily used to recognize
!                    years in the input string.
!
!        ZZGREP  --- is used to get the current representation of the
!                    tokenized string (with all processing resulting
!                    from use of the manipulation routines taken into
!                    account).
!
!        ZZTPATS --- is used to set up the large list of canned patterns
!                    that are recognized as legitimate tokenizations.
!                    Almost all legitimate time strings when tokenized
!                    will match one of these patterns.
!
!        ZZUNPCK --- uses STRING together with the current
!                    representation of it's tokens to return a
!                    time vector.  If a problem is encountered with
!                    the current tokens, it returns a diagnostic
!                    message that indicates why the string
!                    could not be parsed.  Note ZZUNPCK should be
!                    called only after all string modifiers have
!                    been retrieved via a call to ZZNOTE (or by
!                    manually removing them).
!
!     Next Step is to combine some tokens so that we won't run
!     into problems later on.  We may introduce some new components
!     in the process.
!
      l2r = .true.
      r2l = .not. l2r

      if ( zzcmbt ( 'Oi', 'z', l2r ) ) then

         resolv = zzcmbt( 'z:i', 'Z', l2r )
         resolv = zzsubt( 'z',   'Z', l2r )

      end if

      if ( zzcmbt ( 'oi', 'z', l2r ) ) then

         resolv = zzcmbt( 'z:i', 'Z', l2r )
         resolv = zzsubt( 'z',   'Z', l2r )

      end if

!
!     Next we resolve any months, or weekdays that are followed
!     by periods.
!
      resolv = zzrept ( 'm.', 'm*', l2r )
      resolv = zzrept ( 'w.', 'w*', l2r )
      resolv = zzrept ( 'w,', 'w*', l2r )

!
!     Now convert the right most integer-decimal-point pair to the
!     number representation.
!
      if      ( zzcmbt ( 'i.i', 'n', r2l ) ) then
!
!        We aren't going to do anything here.  We are simply
!        using the IF-THEN...ELSE IF ... ENDIF  to make sure
!        we only replace one decimal place.
!
      else if ( zzcmbt ( 'i.', 'n',  r2l  ) ) then
!
!        Same as the previous comment.
!
      end if
!
!     Remove any white space from the tokenization.
!
      resolv = zzremt ( 'b' )

!
!     User Custom Formats (this still needs a modicum of work).
!     ----------------------------------------------------------------
!     ================================================================
!
!
!     RESOLV = ZZGREP ( REP )
!     USE    = ISRCHC ( REP, NCUSTM, CUSTOM )
!
!     IF ( USE .GT. 0 ) THEN
!        RESOLV = ZZREPT ( CUSTM(USE), CMEANS(USE), L2R )
!     ELSE
!        RESOLV =  .FALSE.
!     END IF
!
!     IF ( RESOLV ) THEN
!
!        SUCCES = ZZUNPCK ( STRING, YABBRV, ...
!                           TVEC,   NTVEC, TYPE, PICTUR, ERROR )
!        ERROR  = ' '
!
!        RETURN
!     END IF
!
!



!
!     Julian Date
!     ----------------------------------------------------------------
!     ================================================================
!
      if ( zzist ( 'j' ) ) then
!
!        This is some form of Julian Date. Handle this case
!        right here and return.
!
         resolv = zzrept ( '[s]', '*s*', l2r )
         mods   = mods .or. zznote ( 's', b, e )

         if ( mods ) then
            call ucase ( string(b:e), modify(system) )
         end if

         resolv = zzrept ( '[j]', '*j*', l2r )
         resolv = zzremt ( 'j' )

         if ( .not. zzist ( 'n' ) ) then
            resolv = zzsubt( 'i', 'n', l2r )
         end if

         resolv = zzcmbt ( '-n',  'n', l2r )
         resolv = zzsubt ( 'n',   'J', l2r )


!
!        We let ZZUNPK handle the parsing or diagnosis of any problems.
!
         succes = zzunpck ( string, yabbrv, &
                            tvec,   ntvec, type, pictur, error )

         if ( index( pictur, 'JULIAND.' ) > 0 ) then
            call suffix ( '::RND', 1, pictur )
         end if

         if ( modify(system) /= ' ' ) then
            call suffix ( '::',           1, pictur  )
            call suffix ( modify(system), 0, pictur  )
         end if

         return

      end if




!
!     Calendar Date Formats.
!     ----------------------------------------------------------------
!     ================================================================
!
!     Replace any integers greater than 1000 by Y.
!
      b      = 1000
      e      = intmax()
      resolv = zzvalt ( string, b, e, 'Y' )

!
!     If the ISO time delimiter 't' is present we don't perform
!     any further simplifications.
!
      if ( zzist ( 't' ) ) then

         resolv = zzgrep ( rep )
         use    = bsrchc ( rep, nknown, known )

         if ( use /= 0 ) then

            resolv = zzrept  ( known(use), meanng(use), l2r )

            succes = zzunpck ( string, yabbrv, &
                               tvec,   ntvec, type, pictur, error)

!
!           If you tag an ISO as A.D., other logic in the Time
!           subsystem will not intepret the two digit year value as a
!           two digit abbreviation.
!
!            MODIFY(ERA) = 'A.D.'
!

            if ( index ( pictur, '.#' ) /=0 ) then
               call suffix ( '::RND', 1, pictur )
            end if

            if ( modify(zone) /= ' ' ) then
               call suffix ( '::',         1, pictur )
               call suffix ( modify(zone), 0, pictur )
            end if

            if ( modify(system) /= ' ' ) then
               call suffix ( '::',           1, pictur )
               call suffix ( modify(system), 0, pictur )
            end if

         else

            succes = .false.
            ntvec  =  0
            mods   = .false.
            type   = ' '
            pictur = ' '

            error  = 'The input string uses the ISO  "T" ' &
            //       'date/time delimiter but does not match any ' &
            //       'of the accepted ISO formats. '

         end if

         return

      end if

!
!     If we reach this point, either we didn't have any custom
!     formats supplied or we didn't match any of them.
!     Resolve any abbreviated years.  We've already set integers
!     that are 1000 or greater to 'Y'  Only 1 or 2 digit integers
!     can be year abbreviations.  We replace the 3 digit integers
!     with I temporarily; locate any abbreviated years; reset all
!     the 3-digit back to 'i'.  (Note 3-digit means value between
!     100 and 999.  003 is not regarded as a 3 digit number).
!
      b      = 100
      e      = 1000
      resolv = zzvalt ( string, b, e, 'I' )
      yabbrv = zzrept ( '''i', '*Y',  l2r )

      do while ( zzsubt ( 'I', 'i', l2r ) )
         b = b+1
      end do



!
!     Resolve the system, and other text components.
!
      resolv = zzrept ( '[e]', '*e*', l2r )
      resolv = zzrept ( '[w]', '*w*', l2r )
      resolv = zzrept ( '[N]', '*N*', l2r )
      resolv = zzrept ( '[Z]', '*Z*', l2r )
      resolv = zzrept ( '[s]', '*s*', l2r )
      resolv = zzsubt ( 'ie',  'Ye',  l2r )
!
!     Note the positions of ERA, WEEKDAY, TIME-ZONE, AMPM marker
!     and time SYSTEM.
!

      havera = zznote ( 'e', begs(era),    ends(era)    )
      havwdy = zznote ( 'w', begs(wday),   ends(wday)   )
      havzon = zznote ( 'Z', begs(zone),   ends(zone)   )
      havapm = zznote ( 'N', begs(ampm),   ends(ampm)   )
      havsys = zznote ( 's', begs(system), ends(system) )

      mods   = havera .or. havwdy .or. havzon .or. havapm .or. havsys

      if ( mods ) then

         do i = 1, nmods
            if ( begs(i) /= 0 ) then
               call ucase( string(begs(i):ends(i)),modify(i))
            end if
         end do

         if ( havera ) then
            if ( modify(era)(1:1) == 'A' ) then
               modify(era) = 'A.D.'
            else
               modify(era) = 'B.C.'
            end if
         end if

         if ( havapm ) then
            if ( modify(ampm)(1:1) == 'A' ) then
               modify(ampm) = 'A.M.'
            else
               modify(ampm) = 'P.M.'
            end if
         end if

         modify(wday)(4:) = ' '

         if ( havzon ) then

            mapto = isrchc ( modify(zone), nzones, zones )

            if ( mapto /= 0 ) then
               modify(zone) = offset(mapto)
            end if

         end if

      end if

!
!     Try our built in formats without any further substitution.
!
      resolv = zzgrep ( rep )
      use    = bsrchc ( rep, nknown, known )

      if ( use > 0 ) then

         resolv = zzrept  ( known(use),   meanng(use), l2r   )
         succes = zzunpck ( string, yabbrv, &
                            tvec,   ntvec, type, pictur, error )

         if ( index ( pictur, '.#' ) /=0 ) then
            call suffix ( '::RND', 1, pictur )
         end if

         if ( modify(zone) /= ' ' ) then
            call suffix ( '::',         1, pictur )
            call suffix ( modify(zone), 0, pictur )
         end if

         if ( modify(system) /= ' ' ) then
            call suffix ( '::',           1, pictur )
            call suffix ( modify(system), 0, pictur )
         end if

         return

      end if
!
!     Make sure we don't have a pair of successive delimiters
!     or a delimiter at either end of the input string.
!
      if (  zzispt ( ',/-:d.', from, to ) ) then

         succes = .false.
         ntvec  =  0
         type   = ' '


         error = string
         call zzinssub ( error, '>', to+1, error )
         call zzinssub ( error, '<', from, error )
         call prefix ( 'There are two successive delimiters <#> ' &
         //            'in the input string.  This is an ' &
         //            'ambiguous input. '' ', &
                        0, error)
         call repmc  ( error, '#', string(from:to), error )
         call suffix ( '''', 0, error )
         pictur = ' '
         return

      end if

!
!     A delimiter hanging at either end of the string shall be
!     regarded as an error.
!
      resolv = zzgrep ( rep )
      r      = rtrim  ( rep )

      if ( index( ',/-:.', rep(1:1) ) > 0 ) then

         resolv = zzsubt ( rep(1:1), 'Q', l2r )
         resolv = .false.

      else if ( index( ',/-:.', rep(r:r) ) > 0 ) then

         resolv = zzsubt ( rep(r:r), 'Q', l2r )
         resolv = .false.

      end if

      if ( .not. resolv ) then

         resolv = zznote ( 'Q', from, to )
         error  = string
         call zzinssub ( error, '>', to+1, error )
         call zzinssub ( error, '<', from, error )
         call prefix ( 'An unexpected delimiter (''#'') was ' &
         //            'encountered in the input string. '' ', &
                             0, error )
         call suffix ( '''', 0, error )
         call repmc  ( error, '#', string(from:to), error )
         pictur = ' '
         succes = .false.
         return
      end if
!
!     We probably made it unscathed through the check above.
!     Remove delimiters ',', '/', and '-' and retry the built-in
!     patterns.
!
      delim(1) = ','
      delim(2) = '-'
      delim(3) = '/'

      do i = 1, 3

         resolv = zzremt ( delim(i) )

         resolv = zzgrep ( rep )
         use    = bsrchc ( rep, nknown, known )

         if ( use > 0 ) then

            resolv = zzrept  ( known(use),   meanng(use), l2r   )
            succes = zzunpck ( string, yabbrv, &
                               tvec,   ntvec, type, pictur, error )

            if ( index ( pictur, '.#' ) /=0 ) then
               call suffix ( '::RND', 1, pictur )
            end if

            if ( modify(zone) /= ' ' ) then
               call suffix ( '::',         1, pictur )
               call suffix ( modify(zone), 0, pictur )
            end if

            if ( modify(system) /= ' ' ) then
               call suffix ( '::',           1, pictur )
               call suffix ( modify(system), 0, pictur )
            end if

            return

         end if

      end do

!
!     If we make it to this point, we must have a pretty funky
!     time string.  There are some obvious incompatibilities. We
!     check them now
!

      if      ( zznote ( 'e', b, e ) ) then
      else if ( zznote ( 's', b, e ) ) then
      else if ( zznote ( 'Z', b, e ) ) then
      else if ( zznote ( 'w', b, e ) ) then
      else if ( zznote ( 'N', b, e ) ) then
      end if
!
!     If B is non-zero the item in question is a duplicate
!     modifier.
!
      if ( b > 0 ) then
         succes = .false.
         ntvec  =  0
         type   = ' '
         error  =  string

         call zzinssub ( error, '>', e+1, error )
         call zzinssub ( error, '<', b,   error )
         call prefix ( 'The substring "#" is a duplicate ' &
         //            'modifier of the input string: '' ', &
                               0,               error )
         call suffix ( '''',   0,               error )
         call repmc  ( error, '#', string(b:e), error )
         pictur = ' '
         return
      end if
!
!     Look for unresolved markers
!
      if      ( zznote ( '[', b, e ) ) then
      else if ( zznote ( ']', b, e ) ) then
      else if ( zznote ( 'O', b, e ) ) then
      else if ( zznote ( 'o', b, e ) ) then
      else if ( zznote ( 'z', b, e ) ) then
      end if

      if ( b > 0 ) then

         succes = .false.
         ntvec  =  0
         type   = ' '
         error  =  string

         call zzinssub ( error, '>', e+1, error )
         call zzinssub ( error, '<', b,   error )
         call prefix ( 'The substring "#" could not ' &
         //            'be resolved in the input string: '' ', &
                               0,               error )
         call suffix ( '''',   0,               error )
         call repmc  ( error, '#', string(b:e), error )
         pictur = ' '
         return

      end if

      if ( zzist ( 'm' ) .and. zzist ( 'd' ) ) then

         succes = .false.
         ntvec  =  0
         type   = ' '
         error  = string
         resolv = zznote ( 'm', b1, e1 )
         resolv = zznote ( 'd', b2, e2 )

         b     = max(b1, b2)
         e     = max(e1, e2)

         call zzinssub ( error, '>', e+1, error )
         call zzinssub ( error, '<', b, error )

         b     = min(b1, b2)
         e     = min(e1, e2)

         call zzinssub ( error, '>', e+1, error )
         call zzinssub ( error, '<', b, error )

         call prefix ( 'Both a month "#" and day of year ' &
         //            'delimiter "#" appear in the input ' &
         //            'string: '' ', 0, error )
         call suffix ( '''',          0, error )
         call repmc  ( error, '#', string(b1:e1), error )
         call repmc  ( error, '#', string(b2:e2), error )
         pictur = ' '
         return

      end if



!
!     Make the remaining obvious substitutions for hours,
!     minutes, and seconds
!
      if      ( zzrept ( 'i:i:i:n', 'D*H*M*S', r2l ) ) then
      else if ( zzrept ( 'i:i:i:i', 'D*H*M*S', r2l ) ) then
      else if ( zzrept ( 'i:i:n',   'H*M*S',   r2l ) ) then
      else if ( zzrept ( 'i:i:i',   'H*M*S',   r2l ) ) then
      else if ( zzrept ( 'i:n',     'H*M',     r2l ) ) then
      else if ( zzrept ( 'i:i',     'H*M',     r2l ) ) then
      end if

      resolv = zzremt ( ':' )
!
!     Handle the obvious substitutions of an integer next to
!     a Month.
!
      if      ( zzsubt ( '<miiH', 'mDY', l2r ) ) then
      else if ( zzsubt ( '<mi',   'mD',  l2r ) ) then
      else if ( zzsubt ( 'Siim>', 'SYDm',l2r ) ) then
      else if ( zzsubt ( 'im>',   'Dm',  l2r ) ) then
      else if ( zzsubt ( 'miY>',  'mDY', l2r ) ) then
      else if ( zzsubt ( 'Ymi',   'YmD', l2r ) ) then
      else if ( zzsubt ( 'Smi',   'SmD', l2r ) ) then
      else if ( zzsubt ( 'Mmi',   'MmD', l2r ) ) then
      else if ( zzsubt ( 'imY',   'DmY', l2r ) ) then
      else if ( zzsubt ( 'imH',   'DmH', l2r ) ) then
      else if ( zzrept ( 'Yid',   'Yy*', l2r ) ) then
      else if ( zzrept ( 'iYd',   'yY*', l2r ) ) then
      else if ( zzrept ( 'Ydi',   'Y*y', l2r ) ) then
      end if

!
!     That's it we let ZZUNPCK handle the problem of diagnosing
!     or decoding the current representation.
!

      succes = zzunpck ( string, yabbrv, &
                         tvec,   ntvec, type, pictur, error )

      if ( pictur /= ' ' ) then

         if ( index ( pictur, '.#' ) /=0 ) then
            call suffix ( '::RND', 1, pictur )
         end if

         if ( modify(zone) /= ' ' ) then
            call suffix ( '::',         1, pictur )
            call suffix ( modify(zone), 0, pictur )
         end if

         if ( modify(system) /= ' ' ) then
            call suffix ( '::',           1, pictur )
            call suffix ( modify(system), 0, pictur )
         end if

      end if

      return


      end
