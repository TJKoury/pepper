!$Procedure STR2ET ( String to ET )

      SUBROUTINE STR2ET ( TIMSTR, ET )

! $ Abstract
!
!     Convert a string representing an epoch to a double precision
!     value representing the number of TDB seconds past the J2000
!     epoch corresponding to the input epoch.
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

      IMPLICIT NONE

      CHARACTER*(*)         TIMSTR
      DOUBLE PRECISION      ET

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TIMSTR     I   A string representing an epoch.
!     ET         O   The equivalent value in seconds past J2000, TDB.
!
! $ Detailed_Input
!
!     TIMSTR   is a string representing an epoch. Virtually all
!              common calendar representations are allowed. You may
!              specify a time string belonging to any of the
!              systems TDB, TDT, UTC. Moreover, you may specify a
!              time string relative to a specific UTC based time
!              zone.
!
!              The rules used in the parsing of TIMSTR are spelled out
!              in great detail in the reference document time.req. The
!              basics are given in the $Particulars section below.
!
! $ Detailed_Output
!
!     ET       is the double precision number of TDB seconds past the
!              J2000 epoch that corresponds to the input TIMSTR.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the TIMSTR input string cannot be recognized as a
!         legitimate time string, the error SPICE(UNPARSEDTIME) is
!         signaled.
!
!     2)  If more than one time system is specified as part of the
!         input time string, the error SPICE(TIMECONFLICT) is signaled.
!
!     3)  If any component of the input time string is outside the
!         normal range of usage, the error SPICE(BADTIMESTRING) is
!         signaled. For example, the day January 35 is outside the
!         normal range of days in January. The checks applied are
!         spelled out in the routine TCHECK.
!
!     4)  If a time zone is specified with hours or minutes components
!         that are outside of the normal range, the error
!         SPICE(TIMEZONEERROR) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine computes the ephemeris epoch corresponding to an
!     input string. The ephemeris epoch is represented as seconds
!     past the J2000 epoch in the time system known as Barycentric
!     Dynamical Time (TDB). This time system is also referred to as
!     Ephemeris Time (ET) throughout the SPICE Toolkit.
!
!     The variety of ways people have developed for representing
!     times is enormous. It is unlikely that any single subroutine
!     can accommodate the wide variety of custom time formats that
!     have arisen in various computing contexts. However, we
!     believe that this routine will correctly interpret most time
!     formats used throughout the planetary science community.
!     For example this routine supports ISO time formats and UNIX
!     `date` output formats. One obvious omission from the strings
!     recognized by this routine are strings of the form
!
!          93234.1829  or 1993234.1829
!
!     Some readers may recognize this as the epoch that is 0.1829
!     days past the beginning of the 234'th day of 1993. However,
!     many other readers may regard this interpretation as a bit
!     obscure.
!
!     Below we outline some of the rules used in the interpretation
!     of strings. A more complete discussion of the interpretation
!     of strings is given in the reference document time.req.
!
!
!     Default Behavior
!     ----------------
!
!     Consider the string
!
!        1988 June 13, 3:29:48
!
!     There is nothing in this string to indicate what time system
!     the date and time belong to. Moreover, there is nothing to
!     indicate whether the time is based on a 24-hour clock or
!     twelve hour clock.
!
!     In the absence of such indicators, the default interpretation
!     of this string is to regard the time of day to be a time on
!     a 24-hour clock in the UTC time system. The date is a date
!     on the Gregorian Calendar (this is the calendar used in nearly
!     all western societies).
!
!     Labels
!     ------
!
!     If you add more information to the string, STR2ET can make a
!     more informed interpretation of the time string. For example:
!
!        1988 June 13, 3:29:48 P.M.
!
!     is still regarded as a UTC epoch. However, with the addition
!     of the 'P.M.' label it is now interpreted as the same epoch
!     as the unlabeled epoch 1988 June 13, 15:29:48. Similarly
!
!        1988 June 13, 12:29:48 A.M.
!
!     is interpreted as
!
!        1988 June 13, 00:29:48
!
!     For the record: 12:00 A.M. corresponds to Midnight (00:00 on the
!     24 hour clock.  12:00 P.M. corresponds to Noon. (12:00) on the
!     24 hour clock.
!
!     You may add still further indicators to the string. For example
!
!        1988 June 13, 3:29:48 P.M. PST
!
!     is interpreted as an epoch in the Pacific Standard Time system.
!     This is equivalent to
!
!        1988 June 13, 07:29:48 UTC
!
!     The following U.S. time zones are recognized.
!
!        EST   --- Eastern Standard Time  ( UTC-5:00 )
!        CST   --- Central Standard Time  ( UTC-6:00 )
!        MST   --- Mountain Standard Time ( UTC-7:00 )
!        PST   --- Pacific Standard Time  ( UTC-8:00 )
!
!        EDT   --- Eastern Daylight Time  ( UTC-4:00 )
!        CDT   --- Central Daylight Time  ( UTC-5:00 )
!        MDT   --- Mountain Daylight Time ( UTC-6:00 )
!        PDT   --- Pacific Daylight Time  ( UTC-7:00 )
!
!     In addition any other time zone may be specified by representing
!     its offset from UTC. This notation starts with the letters 'UTC'
!     followed by a '+' for time zones east of Greenwich and '-' for
!     time zones west of Greenwich. This is followed by the number of
!     hours to add or subtract from UTC. This is optionally followed
!     by a colon ':' and the number of minutes to add or subtract to
!     get the local time zone. Thus to specify the time zone of
!     Calcutta (which is 5 and 1/2 hours ahead of UTC) you would
!     specify the time zone to be UTC+5:30. To specify the time zone
!     of Newfoundland (which is 3 and 1/2 hours behind UTC) use the
!     offset notation UTC-3:30.
!
!     For the Record:  Leapseconds occur at the same time in all
!     time zones. In other words, the seconds component of a time
!     string is the same for any time zone as is the seconds
!     component of UTC. Thus the following are all legitimate
!     ways to represent an epoch of some event that occurred
!     in the leapsecond
!
!        1995 December 31  23:59:60.5  (UTC)
!
!
!        1996 January   1, 05:29:60.5  (UTC+5:30 --- Calcutta Time)
!        1995 December 31, 20:29:60.5  (UTC-3:30 --- Newfoundland)
!        1995 December 31  18:59:60.5  (EST)
!        1995 December 31  17:59:60.5  (CST)
!        1995 December 31  16:59:60.5  (MST)
!        1995 December 31  15:59:60.5  (PST)
!
!
!     In addition to specifying time zones, you may specify that the
!     string be interpreted as a formal calendar representation in
!     either the Barycentric Dynamical Time system (TDB) or the
!     Terrestrial Dynamical Time system (TDT).  In These systems there
!     are no leapseconds. Times in TDB are written as
!
!        1988 June 13, 12:29:48 TDB
!
!     TDT times are written as:
!
!        1988 June 13, 12:29:48 TDT
!
!     Finally, you may explicitly state that the time system is UTC
!
!        1988 June 13, 12:29:48 UTC.
!
!
!     Abbreviating Years
!     ------------------
!
!     Although it can lead to confusion, many people are in the
!     habit of abbreviating years when they write them in dates.
!     For example
!
!        99 Jan 13,  12:28:24
!
!     Upon seeing such a string, most of us would regard this
!     as being 1999 January 13, 12:28:24 and not January 13 of
!     the year 99. This routine interprets years that are less
!     than 100 as belonging either to the 1900's or 2000's. Years
!     greater than 68 ( 69 - 99 ) are regarded as being an
!     abbreviation with the '19' suppressed (1969 - 1999). Years
!     smaller than 69 ( 00 - 68 ) are regarded as being an
!     abbreviation with the '20' suppressed (2000 - 2068).
!
!     Note that in general it is usually a good idea to write
!     out the year. Or if you'd like to save some typing
!     abbreviate 1999 as '99.
!
!     If you need to specify an epoch whose year
!     is less than 1000, we recommend that you specify the era
!     along with the year. For example if you want to specify
!     the year 13 A.D. write it as
!
!        13 A.D. Jan 12
!
!     When specifying the era it should immediately follow the year.
!     Both the A.D. and B.C. eras are supported.
!
!
!     Changing Default Behavior
!     -------------------------
!
!     As discussed above, if a string is unlabeled, it is regarded
!     as representing a string in the UTC time system on the
!     Gregorian calendar. In addition abbreviated years are
!     regarded as abbreviations of the years from 1969 to 2068.
!
!     You may modify these defaults through the routines TIMDEF
!     and TSETYR.
!
!     You may:
!
!        Set the calendar to be Gregorian, Julian or a mixture of
!        two via the TIMDEF;
!
!        Set the time system to be UTC, TDB, TDT or any time zone
!        via the routine TIMDEF;
!
!        Set the range of year abbreviations to be any 100 year
!        interval via the routine TSETYR.
!
!     See the SPICELIB routine TEXPYR and TIMDEF for details on changing
!     defaults.
!
!     These alterations affect only the interpretation of unlabeled
!     strings. If an input string is labeled the specification
!     in the label is used.
!
!
!     If any component of a date or time is out of range, STR2ET
!     regards the string as erroneous. Below is a list of
!     erroneous strings and why they are regarded as such.
!
!        1997 Jan 32 12:29:29     --- there are only 31 days in January
!
!        '98 Jan 12 13:29:29 A.M. --- Hours must be between 1 and 12
!                                     inclusive when A.M. or P.M. is
!                                     specified.
!
!        1997 Feb 29, 12:29:20.0  --- February has only 29 days in
!                                     1997. This would be ok if the
!                                     year was 1996.
!
!
!        1992 Mar 12 12:62:20     --- Minutes must be between 0 and 59
!                                     inclusive.
!
!        1993 Mar 18 15:29:60.5   --- Seconds is out of range for this
!                                     date. It would not be out of
!                                     range for Dec 31 23:59:60.5 or
!                                     Jun 30 23:59:60.5 because these
!                                     can be leapseconds (UTC).
!
!     Specifics On Interpretation of the Input String
!     -----------------------------------------------
!
!     The process of examining the string to determine its meaning is
!     called "parsing" the string. The string is parsed by first
!     determining its recognizable substrings (integers, punctuation
!     marks, names of months, names of weekdays, time systems, time
!     zones, etc.) These recognizable substrings are called the tokens
!     of the input string. The meaning of some tokens are immediately
!     determined. For example named months, weekdays, time systems have
!     clear meanings. However, the meanings of numeric components must
!     be deciphered from their magnitudes and location in the string
!     relative to the immediately recognized components of the input
!     string.
!
!     To determine the meaning of the numeric tokens in the input
!     string, a set of "production rules" and transformations are
!     applied to the full set of tokens in the string. These
!     transformations are repeated until the meaning of every token
!     has been determined, or until further transformations yield
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
!         219th day of 1994 + 0.12819 days. STR2ET does not
!         support such strings.
!
!     5)   No exponential components are allowed. For example you
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
!         currently used by western society). See the routine TIMDEF
!         to modify this behavior.
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
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Suppose you would like to determine whether your favorite
!        time representation is supported by STR2ET. The small
!        program below gives you a simple way to experiment with
!        STR2ET. (Note that erroneous inputs will be flagged by
!        signaling an error.)
!
!        Example code begins here.
!
!
!              PROGRAM STR2ET_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(64)        TIMSTR
!              CHARACTER*(64)        CALDR
!              CHARACTER*(64)        DAYOFY
!              CHARACTER*(127)       FILNAM
!
!              DOUBLE PRECISION      ET
!
!        C
!        C     First get the name of a leapseconds kernel, and load it.
!        C
!              CALL PROMPT ( 'Leapseconds kernel: ', FILNAM )
!              CALL FURNSH ( FILNAM )
!
!        C
!        C     Get the time string.
!        C
!              CALL PROMPT ( 'Time string: ', TIMSTR )
!
!        C
!        C     Convert the string to ET and then back to UTC calendar
!        C     and day-of-year formats.
!        C
!              CALL STR2ET ( TIMSTR, ET )
!              CALL ET2UTC ( ET, 'C', 0, CALDR  )
!              CALL ET2UTC ( ET, 'D', 0, DAYOFY )
!
!        C
!        C     Print the results.
!        C
!              WRITE (*,*)
!              WRITE (*,*) 'TBD seconds from J2000 epoch: ', ET
!              WRITE (*,*) 'Calendar    Format:           ', CALDR
!              WRITE (*,*) 'Day of year Format:           ', DAYOFY
!
!
!              END
!
!
!        When this program was executed on a PC/Linux/gfortran/64-bit
!        platform, using the LCK file named naif0012.tls and the time
!        string '2017-07-14T19:46:00', the output was:
!
!
!        Leapseconds kernel: naif0012.tls
!        Time string: 2017-07-14T19:46:00
!
!         TBD seconds from J2000 epoch:    553333629.18372738
!         Calendar    Format:           2017 JUL 14 19:46:00
!         Day of year Format:           2017-195 // 19:46:00
!
!
!     2) Below is a sampling of some of the time formats that are
!        acceptable as inputs to STR2ET. A complete discussion of
!        permissible formats is given in the reference document
!        time.req.
!
!        ISO (T) Formats.
!
!        String                        Year Mon  DOY DOM  HR Min Sec
!        ----------------------------  ---- ---  --- ---  -- --- ------
!        1996-12-18T12:28:28           1996 Dec   na  18  12  28 28
!        1986-01-18T12                 1986 Jan   na  18  12  00 00
!        1986-01-18T12:19              1986 Jan   na  18  12  19 00
!        1986-01-18T12:19:52.18        1986 Jan   na  18  12  19 52.18
!        1986-01-18T12:19:52.18Z       1986 Jan   na  18  12  19 52.18
!        1995-08T18:28:12              1995  na  008  na  18  28 12
!        1995-08T18:28:12Z             1995  na  008  na  18  28 12
!        1995-18T                      1995  na  018  na  00  00 00
!        0000-01-01T                   1 BC Jan   na  01  00  00 00
!
!
!        Calendar Formats.
!
!        String                        Year   Mon DOM  HR Min  Sec
!        ----------------------------  ----   --- ---  -- ---  ------
!        Tue Aug  6 11:10:57  1996     1996   Aug  06  11  10  57
!        1 DEC 1997 12:28:29.192       1997   Dec  01  12  28  29.192
!        2/3/1996 17:18:12.002         1996   Feb  03  17  18  12.002
!        Mar 2 12:18:17.287 1993       1993   Mar  02  12  18  17.287
!        1992 11:18:28  3 Jul          1992   Jul  03  11  18  28
!        June 12, 1989 01:21           1989   Jun  12  01  21  00
!        1978/3/12 23:28:59.29         1978   Mar  12  23  28  59.29
!        17JUN1982 18:28:28            1982   Jun  17  18  28  28
!        13:28:28.128 1992 27 Jun      1992   Jun  27  13  28  28.128
!        1972 27 jun 12:29             1972   Jun  27  12  29  00
!        '93 Jan 23 12:29:47.289       1993*  Jan  23  12  29  47.289
!        27 Jan 3, 19:12:28.182        2027*  Jan  03  19  12  28.182
!        23 A.D. APR 4, 18:28:29.29    0023** Apr  04  18  28  29.29
!        18 B.C. Jun 3, 12:29:28.291   -017** Jun  03  12  29  28.291
!        29 Jun  30 12:29:29.298       2029+  Jun  30  12  29  29.298
!        29 Jun '30 12:29:29.298       2030*  Jun  29  12  29  29.298
!
!        Day of Year Formats
!
!        String                        Year  DOY HR Min Sec
!        ----------------------------  ----  --- -- --- ------
!        1997-162::12:18:28.827        1997  162 12  18 28.827
!        162-1996/12:28:28.287         1996  162 12  28 28.287
!        1993-321/12:28:28.287         1993  231 12  28 28.287
!        1992 183// 12:18:19           1992  183 12  18 19
!        17:28:01.287 1992-272//       1992  272 17  28 01.287
!        17:28:01.282 272-1994//       1994  272 17  28 01.282
!        '92-271/ 12:28:30.291         1992* 271 12  28 30.291
!        92-182/ 18:28:28.281          1992* 182 18  28 28.281
!        182-92/ 12:29:29.192          0182+ 092 12  29 29.192
!        182-'92/ 12:28:29.182         1992  182 12  28 29.182
!
!
!        Julian Date Strings
!
!        jd 28272.291                  Julian Date   28272.291
!        2451515.2981 (JD)             Julian Date 2451515.2981
!        2451515.2981 JD               Julian Date 2451515.2981
!
!                                      Abbreviations Used in Tables
!
!                                        na    --- Not Applicable
!                                        Mon   --- Month
!                                        DOY   --- Day of Year
!                                        DOM   --- Day of Month
!                                        Wkday --- Weekday
!                                        Hr    --- Hour
!                                        Min   --- Minutes
!                                        Sec   --- Seconds
!
!        *  The default interpretation of a year that has been
!           abbreviated to two digits with or without a leading quote
!           as in 'xy or xy (such as '92 or 92) is to treat the year as
!           19xy if xy > 68 and to treat it as 20xy otherwise. Thus '70
!           is interpreted as 1970 and '67 is treated as 2067. However,
!           you may change the "split point" and centuries through use
!           of the SPICE routine TSETYR. See that routine for a
!           discussion of how you may reset the split point.
!
!        ** All epochs are regarded as belonging to the Gregorian
!           calendar. We formally extend the Gregorian calendar backward
!           and forward in time for all epochs. If you have epochs
!           belonging to the Julian Calendar, consult the SPICELIB
!           routines TPARTV and JUL2GR for a discussion concerning
!           conversions to the Gregorian calendar and ET. The routines
!           TIMDEF and STR2ET, used together, also support conversions
!           from Julian Calendar epochs to ET.
!
!        +  When a day of year format or calendar format string is
!           input and neither of the integer components of the date is
!           greater than 1000, the first integer is regarded as being
!           the year.
!
!        Any integer greater than 1000 is regarded as a year
!        specification. Thus 1001-1821//12:28:28 is interpreted as
!        specifying two years and will be rejected as ambiguous.
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
!     C.H. Acton         (JPL)
!     N.J. Bachman       (JPL)
!     M. Costa Sitja     (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.4.0, 23-DEC-2021 (JDR) (EDW) (MCS)
!
!        Changed the input argument name STRING to TIMSTR for
!        consistency with other routines.
!
!        Header edits to expand description of ISO format.
!
!        Edited the header to comply with NAIF standard.
!        Added comments and removed do-loop from code example.
!
!        Replaced references to TPARTV by time.req.
!
!-    SPICELIB Version 1.3.1, 02-NOV-2009 (CHA)
!
!        A few minor grammar errors were fixed in the header.
!        The header sections were reordered.
!
!-    SPICELIB Version 1.3.0, 31-AUG-2006 (NJB) (EDW)
!
!        Bug fix: routine formerly returned incorrect results
!        in some cases on calls following calls for which a time
!        zone was specified.
!
!        Replaced reference to LDPOOL in header $Examples section
!        with reference to FURNSH.
!
!-    SPICELIB Version 1.2.2, 29-JUL-2003 (NJB)
!
!        Various minor header corrections were made
!
!-    SPICELIB Version 1.2.1, 10-FEB-2003 (NJB)
!
!        Corrected header typo.
!
!-    SPICELIB Version 1.2.0, 11-NOV-1997 (WLT)
!
!        The previous versions of this routine did not correctly
!        convert day-of-year strings in the TDB or TDT systems.
!        They treated the day of year as year, month, day giving
!        spectacularly wrong answers.
!
!        In addition, comments concerning the default century for
!        abbreviated years were updated to reflect changes to TEXPYR
!
!-    SPICELIB Version 1.1.0, 10-FEB-1997 (WLT)
!
!        In the case that a time zone could not be parsed,
!        this routine signaled an error and checked out without
!        then returning. This error has been corrected.
!
!-    SPICELIB Version 1.0.0, 15-NOV-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Convert a string to TDB seconds past the J2000 epoch
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.3.0, 31-AUG-2006 (NJB)
!
!        Bug fix: routine formerly returned incorrect results
!        in some cases on calls following calls for which a time
!        zone was specified.
!
!        The problem was caused by the variable ZONED not being
!        properly set when a time system was specified
!        in the input string. In such cases, ZONED retained the
!        value from the previous call.
!
!-&


!
!     SPICELIB Functions.
!
      LOGICAL               RETURN


!
!     Local (in-line) Functions
!
      INTEGER               SMWDSZ
      PARAMETER           ( SMWDSZ = 16 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MSGSIZ
      PARAMETER           ( MSGSIZ = 400 )

      INTEGER               ERA
      PARAMETER           ( ERA    = 1 )

      INTEGER               WKDAY
      PARAMETER           ( WKDAY  = ERA    + 1 )

      INTEGER               ZONE
      PARAMETER           ( ZONE   = WKDAY  + 1 )

      INTEGER               AMPM
      PARAMETER           ( AMPM   = ZONE   + 1 )

      INTEGER               SYSTEM
      PARAMETER           ( SYSTEM = AMPM   + 1 )

      CHARACTER*(LNSIZE)    PICTUR
      CHARACTER*(MSGSIZ)    ERROR

      CHARACTER*(SMWDSZ)    CALNDR
      CHARACTER*(SMWDSZ)    CHECK
      CHARACTER*(SMWDSZ)    DEFSYS
      CHARACTER*(SMWDSZ)    DEFZON
      CHARACTER*(SMWDSZ)    FORML
      CHARACTER*(SMWDSZ)    JULN
      CHARACTER*(SMWDSZ)    MIXED
      CHARACTER*(SMWDSZ)    GREGRN
      CHARACTER*(SMWDSZ)    MODIFY ( SYSTEM )
      CHARACTER*(SMWDSZ)    TYPE

      CHARACTER*(2)         HSTR
      CHARACTER*(2)         MSTR

      CHARACTER*(SMWDSZ)    MNAME  ( 12 )

      DOUBLE PRECISION      DHOFF
      DOUBLE PRECISION      DMOFF
      DOUBLE PRECISION      EXTRA
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      HOFF
      DOUBLE PRECISION      HOUR
      DOUBLE PRECISION      MDY    ( 2 )
      DOUBLE PRECISION      MINUTE
      DOUBLE PRECISION      MOFF
      DOUBLE PRECISION      MON    ( 2 )
      DOUBLE PRECISION      SECS
      DOUBLE PRECISION      TVEC   ( 8 )
      DOUBLE PRECISION      TVECM  ( 8 )

      INTEGER               CYEAR
      INTEGER               GYEAR

      INTEGER               DAY
      INTEGER               DOY
      INTEGER               I
      INTEGER               LAST
      INTEGER               MONTH
      INTEGER               NTVEC
      INTEGER               ORGNYR
      INTEGER               YEAR

!
!     The following integers are pointers to the
!     locations of various components in a time vector.
!
      INTEGER               DY
      INTEGER               HR
      INTEGER               MM
      INTEGER               MN
      INTEGER               SC
      INTEGER               YR

      LOGICAL               ADJUST
      LOGICAL               DOJUL
      LOGICAL               MODS
      LOGICAL               OK
      LOGICAL               OK1
      LOGICAL               OK2
      LOGICAL               SUCCES
      LOGICAL               YABBRV
      LOGICAL               ZONED

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  DEFZON  / ' '         /
      DATA                  DEFSYS  / 'UTC'       /
      DATA                  MIXED   / 'MIXED'     /
      DATA                  JULN    / 'JULIAN'    /
      DATA                  GREGRN  / 'GREGORIAN' /
      DATA                  DHOFF   /  0.0D0      /
      DATA                  DMOFF   /  0.0D0      /

      DATA                  MNAME   /   'January',   'February', &
                                        'March',     'April', &
                                        'May',       'June', &
                                        'July',      'August', &
                                        'September', 'October', &
                                        'November',  'December'  /




      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'STR2ET' )

!
!     Collect the current defaults.
!
      CALL TIMDEF ( 'GET', 'SYSTEM',   DEFSYS )
      CALL TIMDEF ( 'GET', 'ZONE',     DEFZON )
      CALL TIMDEF ( 'GET', 'CALENDAR', CALNDR )

      IF ( DEFZON .NE. ' ' ) THEN
         CALL PREFIX  ( '::', 0, DEFZON )
         CALL ZZUTCPM ( DEFZON, 1, DHOFF, DMOFF, LAST, SUCCES )
      ELSE
         DHOFF = 0.0D0
         DMOFF = 0.0D0
      END IF

!
!     See if TPARTV can recognize what the user has supplied.
!
      CALL TPARTV ( TIMSTR, &
                    TVEC,   NTVEC, TYPE, &
                    MODIFY, MODS,  YABBRV, SUCCES, &
                    PICTUR, ERROR )


      IF ( .NOT. SUCCES ) THEN

         CALL SETMSG ( ERROR )
         CALL SIGERR ( 'SPICE(UNPARSEDTIME)' )
         CALL CHKOUT ( 'STR2ET' )
         RETURN

      END IF

!
!     A system and time zone are incompatible components in a
!     time string.
!
      IF (       MODIFY(ZONE)   .NE. ' ' &
           .AND. MODIFY(SYSTEM) .NE. ' ' ) THEN

         CALL SETMSG ( 'Both a time system and time zone have ' &
         //            'been specified in the input string (# ' &
         //            'and #). These are inconsistent. A ' &
         //            'time zone is a fixed offset from UTC. ' )

         CALL ERRCH  ( '#', MODIFY(SYSTEM)   )
         CALL ERRCH  ( '#', MODIFY(ZONE)     )
         CALL SIGERR ( 'SPICE(TIMECONFLICT)' )
         CALL CHKOUT ( 'STR2ET'              )
         RETURN

      END IF
!
!     If both the zone and system are empty, we can replace them
!     with the default zone and system values (only one of which
!     can be non-blank).
!
      ZONED = .FALSE.

      IF ( MODIFY(ZONE) .EQ. ' ' .AND. MODIFY(SYSTEM) .EQ. ' ' ) THEN

         MODIFY(ZONE)   = DEFZON
         MODIFY(SYSTEM) = DEFSYS
         HOFF           = DHOFF
         MOFF           = DMOFF
         ZONED          = MODIFY(ZONE) .NE. ' '


      ELSE IF ( MODIFY(ZONE) .NE. ' ' ) THEN
!
!        Parse the time zone specification.  If we don't succeed
!        in the parsing, signal an error.
!
         ZONED = .TRUE.
         CALL PREFIX  ( '::', 0, MODIFY(ZONE) )
         CALL ZZUTCPM ( MODIFY(ZONE), 1, HOFF, MOFF, LAST, SUCCES )

         IF ( .NOT. SUCCES ) THEN
            CALL SETMSG ( '# is not a legitimate time zone ' &
            //            'specification. '       )
            CALL ERRCH  ( '#', MODIFY(ZONE)(3:)   )
            CALL SIGERR ( 'SPICE(TIMEZONEERROR)'  )
            CALL CHKOUT ( 'STR2ET' )
            RETURN
         END IF

      END IF

!
!     We handle the julian date case now.  It doesn't have the
!     complications associated with it that the calendar strings
!     have.
!
      IF ( TYPE .EQ. 'JD' ) THEN

         IF      ( MODIFY(SYSTEM) .EQ. 'UTC' ) THEN
            TYPE = 'JDUTC'
         ELSE IF ( MODIFY(SYSTEM) .EQ. 'TDB' ) THEN
            TYPE = 'JDTDB'
         ELSE IF ( MODIFY(SYSTEM) .EQ. 'TDT' ) THEN
            TYPE = 'JDTDT'
         ELSE
            TYPE = 'JDUTC'
         END IF

         CALL TTRANS ( TYPE, 'TDB', TVEC )
         ET = TVEC(1)

         CALL CHKOUT ( 'STR2ET' )
         RETURN

      END IF

!
!     Set the indexes of the hours, minutes, seconds, etc. components
!     of the time vector.
!
      IF ( TYPE .EQ. 'YD' ) THEN
         YR    =  1
         DY    =  2
         HR    =  3
         MN    =  4
         SC    =  5
         FORML = 'YDF'
      ELSE
         YR    =  1
         MM    =  2
         DY    =  3
         HR    =  4
         MN    =  5
         SC    =  6
         FORML = 'YMDF'
      END IF


!
!     Check the components for reasonableness.
!
      CALL TCHCKD ( CHECK )
      CALL TPARCH ( 'YES' )

!
!     If the calendar is NOT Gregorian, or if we have a time zone
!     present, we avoid the problem of checking for legitimate
!     leapseconds (at least we avoid this problem for the moment).
!
      ADJUST = .FALSE.

      IF ( ZONED .OR.  CALNDR .NE. GREGRN ) THEN

         IF (       TVEC(SC) .GE. 60.0D0 &
              .AND. TVEC(SC) .LT. 61.0D0 ) THEN

            ADJUST   = .TRUE.
            TVEC(SC) = TVEC(SC) - 1.0D0

         END IF

      END IF



      IF ( CALNDR .EQ. MIXED  ) THEN
!
!        This is a bit awkward, but here's what's going on.
!        If the input calendar is part of the Julian calendar
!        it might be Feb 29 on a century such as 1500.  These
!        are not legitimate dates on the Gregorian calendar.
!        But they are ok on the Julian calendar.
!
!        However, one of the year numbers YEAR or YEAR + 4 will
!        be a leap year on both the Julian and Gregorian calendar.
!        If we have just a century problem, it will be a problem
!        for only one of the years.  So in the range where we could
!        have a problem we call TCHECK twice and .OR. the results
!        of the checks to see if we have a legitimate time vector.
!
         IF ( TVEC(YR) .LT. 1580.0D0 ) THEN

            CALL MOVED  ( TVEC, 6, TVECM )

            TVECM(1) = TVECM(1) + 4.0D0

            CALL TCHECK ( TVECM, TYPE, MODS, MODIFY, OK1, ERROR )
            CALL TCHECK ( TVEC,  TYPE, MODS, MODIFY, OK2, ERROR )

            OK = OK1 .OR. OK2

         ELSE

            CALL TCHECK ( TVEC,  TYPE, MODS, MODIFY, OK, ERROR )

         END IF

      ELSE IF ( CALNDR .EQ. JULN  ) THEN
!
!        Basically, this is the same story as before, but there
!        are no bounds in the years where we might be on a century.
!        So we just check twice for each time vector.
!
         CALL MOVED  ( TVEC, 6, TVECM )

         TVECM(1) = TVECM(1) + 4.0D0

         CALL TCHECK ( TVECM, TYPE, MODS, MODIFY, OK1, ERROR )
         CALL TCHECK ( TVEC,  TYPE, MODS, MODIFY, OK2, ERROR )

         OK = OK1 .OR. OK2

      ELSE
!
!        TCHECK was designed for the Gregorian Calendar,  So we
!        don't have much to do.
!
         CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, OK, ERROR )

      END IF
!
!     Reset the checking status.
!
      CALL TPARCH ( CHECK  )

!
!     If we didn't get an OK from the inspection above,
!     say so and signal an error.
!
      IF ( .NOT. OK ) THEN

         CALL SETMSG ( ERROR )
         CALL SIGERR ( 'SPICE(BADTIMESTRING)' )
         CALL CHKOUT ( 'STR2ET' )
         RETURN

      END IF
!
!     Reset TVEC(SC) if it was adjusted earlier.
!
      IF ( ADJUST ) THEN
         TVEC(SC) = TVEC(SC) + 1.0D0
      END IF

!
!     There are no leapseconds in the TDT and TDB time systems
!     This means that the seconds component must be less than 60.
!
      IF (      MODIFY(SYSTEM) .EQ. 'TDT' &
           .OR. MODIFY(SYSTEM) .EQ. 'TDB' ) THEN

         IF ( TVEC(SC) .GE. 60.0D0 ) THEN

            CALL SETMSG ( 'The seconds component of time must ' &
            //            'be less than 60 for any ' &
            //            'calendar representation of #. ' )
            CALL ERRCH  ( '#', MODIFY(SYSTEM) )
            CALL SIGERR ( 'SPICE(BADTIMESTRING)' )
            CALL CHKOUT ( 'STR2ET' )
            RETURN

         END IF

      END IF


!
!     If a B.C. era marker is present we can't have a year abbreviation
!
      IF ( MODIFY(ERA) .EQ. 'B.C.' .AND. YABBRV ) THEN

         CALL SETMSG ( 'The Year may be abbreviated only if the ' &
         //            'year belongs to the Christian Era (A.D.) ' )
         CALL SIGERR ( 'SPICE(BADTIMESTRING)' )
         CALL CHKOUT ( 'STR2ET' )
         RETURN

      END IF
!
!     If the era is B.C. we need to reset the year.
!
      IF ( MODIFY(ERA) .EQ. 'B.C.' ) THEN
         TVEC(YR) = 1.0D0 - TVEC(YR)
      END IF

!
!     If there is a A.M. or P.M. time string modifier, we need to adjust
!     the hours component of the time.
!
      IF     ( MODIFY(AMPM) .EQ. 'P.M.'   ) THEN

         IF ( TVEC(HR) .LT. 12.0D0 ) THEN
            TVEC(HR) = TVEC(HR) + 12.0D0
         END IF

      ELSE IF ( MODIFY(AMPM) .EQ. 'A.M.' ) THEN

         IF ( TVEC(HR) .GE. 12.0D0 ) THEN
            TVEC(HR) = TVEC(HR) - 12.0D0
         END IF

      END IF

!
!     If the year has been abbreviated, we need to convert it
!     to the proper range.  In addition we assume a year less
!     than 100 that is not qualified with the B.C. or A.D. era
!     string is in fact an abbreviated year.
!
      YEAR = NINT( TVEC(YR) )

      IF ( YABBRV ) THEN

         CALL       TEXPYR ( YEAR )
         TVEC(YR) = DBLE   ( YEAR )

      ELSE IF (       YEAR        .LT. 100 &
                .AND. MODIFY(ERA) .EQ. ' ' ) THEN

         CALL       TEXPYR ( YEAR )
         TVEC(YR) = DBLE   ( YEAR )

      END IF


!
!     We may need to convert to the Gregorian Calendar, now is
!     the time to do so.
!
      IF ( CALNDR .EQ. MIXED ) THEN
!
!        We need to check the components.
!
         IF ( TYPE .EQ. 'YD' ) THEN

            DOJUL =            TVEC(YR) .LT. 1582.0D0 &
                   .OR. (      TVEC(YR) .EQ. 1582.0D0 &
                         .AND. TVEC(DY) .LT.  279.0D0 )

         ELSE

            DOJUL =             TVEC(YR) .LT. 1582.0D0 &
                   .OR. (       TVEC(YR) .LE. 1582.0D0 &
                          .AND. TVEC(MM) .LT.   10.0D0 ) &
                   .OR. (       TVEC(YR) .LE. 1582.0D0 &
                          .AND. TVEC(MM) .LE.   10.0D0 &
                          .AND. TVEC(DY) .LT.    6.0D0 )

         END IF

      ELSE IF ( CALNDR .EQ. JULN ) THEN

         DOJUL = .TRUE.

      ELSE

         DOJUL = .FALSE.

      END IF
!
!     If the input string is from the julian calendar, we need
!     to convert it to Gregorian.  We also need to save the original
!     year value in the unlikely event it is needed for a later
!     diagnostic message.
!
      IF ( DOJUL ) THEN

         IF ( TYPE .EQ. 'YD' ) THEN

            YEAR   = DINT( TVEC(YR) )
            MONTH  = 1
            DAY    = DINT( TVEC(DY) )
            FRAC   = TVEC(DY) - DBLE(DAY)
            ORGNYR = YEAR

            CALL JUL2GR ( YEAR, MONTH, DAY, DOY )

            TVEC(YR) = DBLE(YEAR)
            TVEC(DY) = DBLE(DOY ) + FRAC

         ELSE

            YEAR   = DINT ( TVEC(YR) )
            MONTH  = DINT ( TVEC(MM) )
            DAY    = DINT ( TVEC(DY) )
            FRAC   = TVEC(DY) - DBLE(DAY)
            ORGNYR = YEAR

            CALL JUL2GR ( YEAR, MONTH, DAY, DOY )

            TVEC(YR) = DBLE(YEAR)
            TVEC(MM) = DBLE(MONTH)
            TVEC(DY) = DBLE(DAY) + FRAC

         END IF

      ELSE

         ORGNYR = DINT( TVEC(YR) )

      END IF

!
!     The TDT and TDB calendars don't need to worry about time
!     zone adjustments.
!
      IF      ( MODIFY(SYSTEM) .EQ. 'TDT' ) THEN

         CALL TTRANS ( FORML, 'FORMAL', TVEC )
         CALL TTRANS ( 'TDT',  'TDB',    TVEC )
         ET = TVEC(1)
         CALL CHKOUT ( 'STR2ET' )
         RETURN

      ELSE IF ( MODIFY(SYSTEM) .EQ. 'TDB' ) THEN

         CALL TTRANS ( FORML, 'FORMAL', TVEC(1) )
         ET = TVEC(1)
         CALL CHKOUT ( 'STR2ET' )
         RETURN

      END IF


!
!     If a time zone has been specified, we need to convert
!     from the time zone components to UTC components.
!
      IF ( ZONED  ) THEN
!
!        A time zone was specified explicitly in the input
!        string.  We need to compute the hour and minute offsets
!        associated with the time zone.
!
         TVEC(HR) = TVEC(HR) - HOFF
         TVEC(MN) = TVEC(MN) - MOFF
         SECS     = TVEC(SC)
         TVEC(SC) = 0.0D0

         CALL TTRANS ( FORML, FORML, TVEC )

         TVEC(SC) = SECS

      END IF
!
!     If we decided to forgo the leapseconds check earlier
!     now is the time to do it.  We've now got Gregorian UTC
!     time components.
!

      IF ( ADJUST ) THEN

         CALL TCHCKD ( CHECK )
         CALL TPARCH ( 'YES' )

         MODS         = .FALSE.
         MODIFY(AMPM) = ' '

         CALL TCHECK ( TVEC,  TYPE, MODS, MODIFY, OK, ERROR )

      ELSE

         OK = .TRUE.

      END IF


      IF ( OK ) THEN
!
!        That's it we are ready to rumble.
!
         CALL TTRANS ( TYPE, 'TDB', TVEC )
         ET = TVEC(1)
         CALL CHKOUT ( 'STR2ET' )
         RETURN

      END IF

!     ===============================================================
!     If you are still here, it is because OK was .FALSE. in the test
!     above.  The only way this can happen is if the seconds were
!     not in the expected range.  The rest of the code is a diagnosis
!     of this problem.  (This is a nuisance case that is
!     unlikely to occur very often.)
!

      IF ( ZONED .AND. DOJUL ) THEN

         ERROR  = 'The seconds component of ''#'' is out of range. ' &
         //       'On the Julian Calendar ' &
         //       'in the specified time zone  (#) leapseconds ' &
         //       'can occur during the year # only in the ' &
         //       'second that immediately follows the time #:#:59 ' &
         //       'on  # # and # #. '

         CALL REPMC ( ERROR,  '#', TIMSTR,           ERROR  )
         CALL REPMC ( ERROR , '#', MODIFY(ZONE)(3:), ERROR  )


      ELSE IF ( ZONED ) THEN
!
!        If we had a time zone, we want to say what time zone
!        in the output string.
!
         ERROR  = 'The seconds component of ''#'' is out of range. ' &
         //       'In the specified time zone  (#) leapseconds ' &
         //       'can occur during the year # only in the ' &
         //       'second that immediately follows the time #:#:59 ' &
         //       'on  # # and # #.'

         CALL REPMC ( ERROR,  '#', TIMSTR,           ERROR  )
         CALL REPMC ( ERROR , '#', MODIFY(ZONE)(3:), ERROR  )

      ELSE
!
!        No time zone, this case can only occur if we interpreted
!        the input string as a date on the Julian Calendar
!
         ERROR  = 'The seconds component of ''#'' is out of range. ' &
         //       'Leapseconds can occur during the year # of ' &
         //       'the Julian calendar only in the second that ' &
         //       'immediately follows the time #:#:59  on # # ' &
         //       'and # #.'' '

         CALL REPMC ( ERROR,  '#', TIMSTR,           ERROR  )

      END IF

!
!     First fill in the year portion of the error message.
!
      CALL REPMI ( ERROR , '#', ORGNYR, ERROR  )

      MON(1) =  6.0D0
      MON(2) = 12.0D0

      MDY(1) = 30.0D0
      MDY(2) = 31.0D0
!
!     Next Fill in the hours and minutes. Recall that leapseconds
!     occur during the last second of the 59'th minute of the 23'rd
!     hour UTC.  So in the new time zone, it occurs in the 59'th + MOFF
!     minute of the 23'rd + HOFF hour of the time zone.  We adjust
!     these to account for hour roll over and day roll over.
!
      MINUTE = 59.0D0 + MOFF

      IF ( MINUTE .GT. 59.0D0 ) THEN
         MINUTE = MINUTE - 60.0D0
         EXTRA  = 1.0D0
      ELSE IF ( MINUTE .LT. 0.0D0 ) THEN
         MINUTE = MINUTE + 60.0D0
         EXTRA  = -1.0D0
      ELSE
         EXTRA  = 0.0D0
      END IF


      HOUR = 23.0D0 + HOFF + EXTRA

      IF ( HOUR .GT. 23 ) THEN
         HOUR = HOUR - 24
      END IF

!
!     Convert the hours and minutes to strings and place the
!     strings in the message.
!
      CALL DPFMT ( HOUR,   '0x', HSTR )
      CALL DPFMT ( MINUTE, '0x', MSTR )

      CALL REPMC ( ERROR , '#', HSTR, ERROR  )
      CALL REPMC ( ERROR , '#', MSTR, ERROR  )

!
!     Last step we generate the month and day corresponding
!     to Dec 31, 23:59, and Jun 30, 23:59.  We only want the
!     dates that belong to the original year.  We could
!     probably figure out the right year to use, but with Julian
!     date possibly messing everything up, we just use the
!     current year and the one before.  If you keep in mind that
!     the Julian Year is always less than the Gregorian year and
!     that the offsets can only push you into the next year, you
!     can determine that we want to start with what ever current
!     UTC year we have and work backwards until we have the
!     year corresponding to the original year.  Since the current
!     UTC year was constructed from the input original year, we
!     only have to step back at most 1 year to get all the dates
!     that might have leapseconds in the user specified year
!     of whatever calendar happens to be in use.
!
      CYEAR = DINT( TVEC(YR) )

      DO GYEAR = CYEAR, CYEAR-1, -1

         DO I = 1, 2

            TVEC(1) = DBLE(GYEAR)
            TVEC(2) = MON(I)
            TVEC(3) = MDY(I)
            TVEC(4) = 23.0D0 + HOFF
            TVEC(5) = 59.0D0 + MOFF
            TVEC(6) = 0.0D0
!
!           Normalize the time vector.
!
            CALL TTRANS ( 'YMDF', 'YMDF', TVEC )

            YEAR   = NINT(TVEC(1))
            MONTH  = NINT(TVEC(2))
            DAY    = NINT(TVEC(3))

            IF ( DOJUL ) THEN
               CALL GR2JUL ( YEAR, MONTH, DAY, DOY )
            END IF


            IF ( YEAR .EQ. ORGNYR ) THEN

               CALL REPMC ( ERROR, '#', MNAME(MONTH), ERROR  )
               CALL REPMI ( ERROR, '#', DAY,          ERROR  )

            END IF

         END DO

      END DO

      CALL SETMSG ( ERROR  )
      CALL SIGERR ( 'SPICE(BADTIMESTRING)' )
      CALL CHKOUT ( 'STR2ET' )
      RETURN


      END
