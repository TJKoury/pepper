!$Procedure TIMOUT ( Time Output )

      subroutine  timout ( et, pictur, output )

! $ Abstract
!
!     Convert an input epoch represented in TDB seconds past the TDB
!     epoch of J2000 to a character string formatted to the
!     specifications of a user's format picture.
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
      character*(*)         pictur
      character*(*)         output

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch in seconds past the ephemeris epoch J2000.
!     PICTUR     I   A format specification for the output string.
!     OUTPUT     O   A string representation of the input epoch.
!
! $ Detailed_Input
!
!     ET       is a double precision representation of time in seconds
!              past the ephemeris epoch J2000.
!
!     PICTUR   is a string that specifies how the output should be
!              presented. The string is made up of various markers
!              that stand for various components associated with
!              a time.
!
!              There are five types of markers that may appear in a
!              format picture. These are String Markers, Numeric
!              Markers, Meta markers, Modifier Markers and Literal
!              Markers.
!
!              The PICTUR string is examined and the various markers
!              are identified. The output time string is constructed
!              by replacing each of the identified markers with
!              an appropriate time component.
!
!              The various markers and their meanings are discussed
!              in the $Particulars section below.
!
!              Note that leading and trailing blanks in PICTUR are
!              ignored.
!
! $ Detailed_Output
!
!     OUTPUT   is a time string equivalent to the input epoch ET,
!              matching the format specified by PICTUR.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  A leapseconds kernel must be loaded via the routine FURNSH
!         before calling this routine. If a leapsecond kernel has not
!         been loaded, an error is signaled by a routine in the call
!         tree of this routine.
!
!     2)  If PICTUR contains the numeric marker 'YYYY' and the
!         magnitude of year is too large to be displayed as a four-digit
!         integer, TIMOUT will replace it by '****'.
!
!     3)  If the requested precision is higher than 12 decimal places,
!         TIMOUT will truncate the decimal part down to 12, and OUTPUT
!         will have all the remaining digits in the decimal part set to
!         zero.
!
!     4)  Double colon (::), when is not part of one of the supported
!         markers, has no effect and will be presented as is on the
!         output string.
!
! $ Files
!
!     A leapseconds kernel must be "loaded" via the routine FURNSH
!     prior to calling TIMOUT.
!
! $ Particulars
!
!     A format picture is simply a string of letters that lets
!     TIMOUT know where various components of a time representation
!     should be placed during creation of the time string.
!     Here's an example of such a picture:
!
!        MON DD,YYYY  HR:MN:SC.#### (TDB) ::TDB
!
!     Here is a sample of times that would be created by using this
!     format.
!
!        JAN 12,1992  12:28:18.2772 (TDB)
!        FEB 13,1994  23:18:25.2882 (TDB)
!        AUG 21,1995  00:02:00.1881 (TDB)
!
!     As you can see from the samples above, the format picture
!     specifies that every time string created should begin with a
!     three-letter abbreviation for the month, followed by a space and
!     the day of the month. The day of month is followed immediately by
!     a comma and the year. The year component is followed by two
!     spaces. The next outputs are hours represented as a two digit
!     integer, a colon, minutes represented as a two digit integer,
!     another colon, and seconds truncated to 4 decimal places and
!     having a two digit integer part (rounding can be commanded; see
!     the discussion of truncation and rounding below). This is
!     followed by a space and the string '(TDB)'. The special marker
!     '::TDB' in the time picture is an ``invisible'' marker. It is
!     used to specify the time system that should be used in creating
!     the time string (in this case Barycentric Dynamical Time).
!
!     TIMOUT does not recognize all of the parts of the time format
!     picture in the example above. The list of recognized parts and
!     unrecognized parts is shown in the table below.
!
!       Recognized       Unrecognized
!       ----------       ------------
!       'MON'            ' '
!       'DD'             ','
!       'YYYY'           '  '
!       'HR'             ':'
!       'MN'             '(TDB)'
!       'SC'
!       '.####'
!       '::TDB'
!
!     The unrecognized parts are called literal markers. They are
!     copied exactly as they appear in PICTUR into the output string.
!     The recognized parts of the picture are replaced by a
!     component of time or, as in the case of '::TDB' are used
!     as instructions about the overall properties of the time
!     string.
!
!     The full list of recognized markers, their classification
!     and meaning are given below.
!
!     MARKER       CLASS     MEANING
!     -----------  --------  -----------------------------------------
!     '.##...'     modifier  represents a numeric component that
!                            immediately precedes this in a decimal
!                            format. Number of decimal places
!                            equals the number of '#' characters
!     '::GCAL'     meta      dates are reported in Gregorian calendar
!     '::JCAL'     meta      dates are reported in Julian calendar
!     '::MCAL'     meta      dates after 15 October, 1582 are reported
!                            in Gregorian calendar; before that
!                            dates are reported in Julian calendar
!
!     '::RND'      meta      round output to places specified by
!                            least significant component
!
!     '::TDB'      meta      all components should be TDB
!
!     '::TDT'      meta      all components should be TT (TDT)
!
!     '::TT'       meta      all components should be TT (TDT)
!
!     '::TRNC'     meta      truncate all output components (default)
!     '::UTC'      meta      all components should be UTC (default)
!     '::UTC+h:m'  meta      all components in UTC offset by +h (hours)
!                            and +m (minutes) so as to allow time zones.
!     '::UTC-h:m'  meta      all components in UTC offset by -h (hours)
!                            and -m (minutes) so as to allow time zones.
!     'AMPM'       string    String (either 'A.M.' or 'P.M.')
!                            indicating whether hours are before
!                            or after noon.
!     'ampm'       string    String (either 'a.m.' or 'p.m.')
!                            indicating whether hours are before
!                            or after noon.
!     'AP'         numeric   AM/PM equivalents of the hour component
!                            of a time.
!     'DD'         numeric   Day of month
!     'DOY'        numeric   Day of year
!     'ERA'        string    String (either 'B.C.' or 'A.D.') giving
!                            era associated with an epoch.
!     '?ERA?'      string    String: either ' B.C. ' or ' A.D. ' if the
!                            year is before 1000 A.D. otherwise a
!                            blank: ' '.
!     'era'        string    String (either 'b.c.' or 'a.d.') giving
!                            era associated with an epoch.
!     '?era?'      string    String: either ' b.c. ' or ' a.d. ' if the
!                            year is before 1000 A.D. otherwise a
!                            blank: ' '.
!     'HR'         numeric   hour component of time
!     'JULIAND'    numeric   Julian date component of time
!     'MM'         numeric   numeric representation of month component
!     'MN'         numeric   minute component of time
!     'MON'        string    upper case three letter abbreviation for
!                            month
!     'Mon'        string    capitalized three letter abbreviation for
!                            month
!     'mon'        string    lower case three letter abbreviation for
!                            month
!     'MONTH'      string    upper case full name of month
!     'Month'      string    capitalized full name of month
!     'month'      string    lower case full name of month
!     'SC'         numeric   seconds component of time
!     'SP1950'     numeric   seconds past 1950 component of time
!     'SP2000'     numeric   seconds past 2000 component of time
!     'YR'         numeric   last two digits of year component of time
!     'YYYY'       numeric   year component of time
!     'WEEKDAY'    string    upper case day of week
!     'Weekday'    string    capitalized day of week
!     'weekday'    string    lower case day of week
!     'WKD'        string    upper case three letter abbreviation for
!                            day of week.
!     'Wkd'        string    capitalized three letter abbreviation for
!                            day of week.
!     'wkd'        string    lower case three letter abbreviation for
!                            day of week.
!
!     String Markers
!
!        String markers are portions of the format picture that will
!        be replaced with a character string that represents the
!        corresponding component of a time.
!
!     Numeric Markers
!
!        Numeric markers are portions of the format picture that will
!        be replaced with a decimal string that represents the
!        corresponding component of a time.
!
!     Meta Markers
!
!        Meta markers (listed under the class ``meta'' in the
!        table above) are used to indicate "global" properties of
!        your time string. You may specify time scale and how
!        rounding should be performed on the components of time
!        in your output string. Meta markers may be placed anywhere
!        in your format picture. They do not contribute to placement
!        of characters in output time strings. Also there are no
!        restrictions on how many meta markers you may place in
!        the format picture. However, if you supply conflicting
!        `meta' markers (for example '::TDT' and '::TDB') in your
!        picture the first marker listed (in left to right order)
!        overrules the conflicting marker that appears later in
!        the picture.
!
!     Default Meta Markers
!
!        If you do not specify a time system, calendar, or time
!        zone through the use of a Meta Marker, TIMOUT uses the
!        values returned by the SPICE routine TIMDEF. The default
!        time system, calendar returned by TIMDEF are UTC and
!        the Gregorian calendar. The default time zone returned
!        by TIMDEF is a blank indicating that no time zone offset
!        should be used.
!
!        See the header for the routine TIMDEF for a more complete
!        discussion of setting and retrieving default values.
!
!     Modifier Markers
!
!        The numeric markers listed in the table above stand
!        for integers unless they are modified through use of a
!        modifier marker. The strings
!
!           .#
!           .##
!           .###
!           .####
!
!        are used to this end. When a numeric marker is followed
!        immediately by one of these modifiers, the corresponding time
!        component will be written with the number of decimal places
!        indicated by the number of successive occurrences of the
!        character '#'. Any numeric token may be modified.
!
!     Rounding vs. Truncation
!
!        The meta markers ::TRNC and ::RND allow you to control
!        how the output time picture is rounded. If you specify
!        ::TRNC all components of time are simply truncated to
!        the precision specified by the marker and any modifier.
!        If you specify ::RND the output time is rounded to the
!        least significant component of the format picture. The
!        default action is truncation.
!
!        Whether an output time string should be rounded or
!        truncated depends upon what you plan to do with the
!        string. For example suppose you simply want to get the
!        calendar date associated with a time and not the time of
!        day. Then you probably do not want to round your output.
!        Rounding 1992 Dec 31, 13:12:00 to the nearest day
!        produces 1993 Jan 1. Thus in this case rounding is probably
!        not appropriate.
!
!        However, if you are producing output for plotting using
!        Julian Date, seconds past 1950 or seconds past 2000, you will
!        probably want your output rounded so as to produce a smoother
!        plot.
!
!     Time Systems
!
!        TIMOUT can produce output strings for epochs relative to
!        any of the systems UTC, TT or TDT, or TDB. If you do not
!        explicitly specify a time system, TIMOUT will produce strings
!        relative to the time system returned by the SPICE routine
!        TIMDEF. Unless you call TIMDEF and change it, the default time
!        system is UTC. However, by using one of the Meta Markers
!        ::UTC, ::TT, ::TDT, or ::TDB you may specify that TIMOUT
!        produce time strings relative to the UTC, TT or TDT, or TDB
!        system respectively.
!
!     Time Zones
!
!        The meta markers ::UTC+h:m  and ::UTC-h:m  allow you to
!        offset UTC times so that you may represent times in a time
!        zone other than GMT. For example you can output times in
!        Pacific Standard time by placing the meta-marker ::UTC-8 in
!        your format picture.
!
!        For instance, if you use the picture
!
!           YYYY Mon DD, HR:MN:SC ::UTC
!
!        you will get output strings such as:
!
!           1995 Jan 03, 12:00:00
!
!        If you use the picture
!
!
!           YYYY Mon DD, HR:MN:SC ::UTC-8
!
!        you will get output strings such as:
!
!           1995 Jan 03, 04:00:00
!
!        Finally, if you use the picture
!
!           YYYY Mon DD, HR:MN:SC ::UTC-8:15
!
!        you will get output string
!
!           1995 Jan 03, 03:45:00
!
!        Note that the minutes are always added or subtracted based on
!        the sign present in the time zone specifier. In the case of
!        ::UTC+h:m, minutes are added. In the case ::UTC-h:m, minutes
!        are subtracted.
!
!        The unsigned part of the hours component can be no more than
!        12. The unsigned part of the minutes component can be no
!        more than 59.
!
!     Calendars
!
!        The calendar currently used by western countries is the
!        Gregorian calendar. This calendar begins on Oct 15, 1582.
!        Prior to Gregorian calendar the Julian calendar was used. The
!        last Julian calendar date prior to the beginning of the
!        Gregorian calendar is Oct 5, 1582.
!
!        The primary difference between the Julian and Gregorian
!        calendars is in the determination of leap years. Nevertheless,
!        both can be formally extended backward and forward in time
!        indefinitely.
!
!        By default TIMOUT uses the default calendar returned by
!        TIMDEF. Under most circumstances this will be the Gregorian
!        calendar (::GCAL). However you may specify that TIMOUT use a
!        specific calendar through use of one of the calendar Meta
!        Markers. You may specify that TIMOUT use the Julian calendar
!        (::JCAL), the Gregorian calendar (::GCAL)  or a mixture of
!        both (::MCAL).
!
!        If you specify ::MCAL, epochs that occur after the beginning
!        of the Gregorian calendar will be represented using the
!        Gregorian calendar, and epochs prior to the beginning of the
!        Gregorian calendar will be represented using the Julian
!        calendar.
!
!     Getting Software to Construct Pictures for You.
!
!        Although it is not difficult to construct time format
!        pictures, you do need to be aware of the various markers that
!        may appear in a format picture.
!
!        There is an alternative means for getting a format picture.
!        The routine TPICTR constructs format pictures from a sample
!        time string. For example, suppose you would like your time
!        strings to look like the basic pattern of the string below.
!
!           'Fri Jul 26 12:22:09 PDT 1996'
!
!        You can call TPICTR with this string, and it will create the
!        appropriate PICTUR for use with TIMOUT.
!
!           CALL TPICTR ( 'Fri Jul 26 12:22:09 PDT 1996',
!          .              PICTUR, OK, ERRMSG             )
!
!        The result will be:
!
!           'Wkd Mon DD HR:MN:SC (PDT) ::UTC-7'
!
!        Note: not every date that you can read is interpretable by
!        TPICTR. For example, you might be able to understand that
!        19960212121116 is Feb 2 1996, 12:11:16. However, TPICTR
!        cannot recognize this string. Thus it is important to check
!        the logical output OK to make sure that TPICTR was able to
!        understand the time picture you provided.
!
!        Even thought TPICTR can not recognize every time pattern that
!        has been used by various people, it does recognize nearly all
!        patterns that you use when you want to communicate outside
!        your particular circle of colleagues.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as input,
!     the compiler and supporting libraries, and the machine specific
!     arithmetic implementation.
!
!     1) Given a sample with the format of the UNIX date string
!        local to California, create a SPICE time picture for use
!        in TIMOUT.
!
!        Using that SPICE time picture, convert a series of ephemeris
!        times to that picture format.
!
!        Use the LSK kernel below to load the leap seconds and time
!        constants required for the conversions.
!
!           naif0012.tls
!
!
!        Example code begins here.
!
!
!              PROGRAM TIMOUT_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               ERRLEN
!              PARAMETER           ( ERRLEN  = 400 )
!
!              INTEGER               TIMLEN
!              PARAMETER           ( TIMLEN  = 64  )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(ERRLEN)    ERR
!              CHARACTER*(TIMLEN)    PICTUR
!              CHARACTER*(TIMLEN)    SAMPLE
!              CHARACTER*(TIMLEN)    TIMSTR
!              CHARACTER*(TIMLEN)    UTCSTR
!
!              DOUBLE PRECISION      ET
!
!              LOGICAL               OK
!
!        C
!        C     Load LSK file.
!        C
!              CALL FURNSH ( 'naif0012.tls' )
!
!        C
!        C     Create the required time picture.
!        C
!              SAMPLE = 'Thu Oct 01 11:11:11 PDT 1111'
!
!              CALL TPICTR ( SAMPLE, PICTUR, OK, ERR )
!
!              IF ( .NOT. OK ) THEN
!
!                 WRITE(*,*) 'Invalid time picture.'
!                 WRITE(*,*) ERR
!
!              ELSE
!
!        C
!        C        Convert the input UTC time to ephemeris time.
!        C
!                 UTCSTR = '26 Nov 2018  23:23:00 UTC'
!                 CALL STR2ET ( UTCSTR, ET )
!
!        C
!        C         Now convert ET to the desired output format.
!        C
!                  CALL TIMOUT ( ET, PICTUR, TIMSTR )
!                  WRITE (*,*) 'Sample format: ', SAMPLE
!                  WRITE (*,*) 'Time picture : ', PICTUR
!                  WRITE (*,*)
!                  WRITE (*,*) 'Input UTC    : ', UTCSTR
!                  WRITE (*,*) 'Output       : ', TIMSTR
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
!         Sample format: Thu Oct 01 11:11:11 PDT 1111
!         Time picture : Wkd Mon DD HR:MN:SC PDT YYYY ::UTC-7
!
!         Input UTC    : 26 Nov 2018  23:23:00 UTC
!         Output       : Mon Nov 26 16:23:00 PDT 2018
!
!
!     2) Convert a UTC time to a string that contains both the
!        calendar representations of the date as well as the Julian
!        date; for example a string of the form:
!
!           "Thu Aug 01 09:47:16 PDT 1996 (2450297.1994 JDUTC)"
!
!        Use the LSK kernel below to load the leap seconds and time
!        constants required for the conversions.
!
!           naif0012.tls
!
!
!        Example code begins here.
!
!
!              PROGRAM TIMOUT_EX2
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               TIMLEN
!              PARAMETER           ( TIMLEN  = 80 )
!
!        C
!        C     Local variables
!        C
!              CHARACTER*(TIMLEN)    PICTUR
!              CHARACTER*(TIMLEN)    TIMSTR
!              CHARACTER*(TIMLEN)    UTCSTR
!
!              DOUBLE PRECISION      ET
!
!        C
!        C     Load LSK file.
!        C
!              CALL FURNSH ( 'naif0012.tls' )
!
!        C
!        C     Convert the input UTC time to ephemeris time.
!        C
!              UTCSTR = '26 Nov 2018  16:23:00 UTC'
!              CALL STR2ET ( UTCSTR, ET )
!
!        C
!        C     Create the required time picture. This could be done
!        C     using TPICTR.
!        C
!              PICTUR = 'Wkd Mon DD HR:MN ::UTC-7 YYYY '
!             .      // '(JULIAND.#### JDUTC)'
!
!        C
!        C      Now convert ET to the desired output format.
!        C
!               CALL TIMOUT ( ET, PICTUR, TIMSTR )
!               WRITE (*,*) 'Input UTC: ', UTCSTR
!               WRITE (*,*) 'Output   : ', TIMSTR
!
!               END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Input UTC: 26 Nov 2018  16:23:00 UTC
!         Output   : Mon Nov 26 09:23  2018 (2458449.1826 JDUTC)
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.4.0, 07-AUG-2021 (EDW) (JDR) (NJB)
!
!        Corrected typo preventing correct calculation of decimal
!        values for HR.###... and MN.###... markers with ::UTC+N:M
!        and ::UTC-N:M meta tags.
!
!        Added "::TT" as a time system meta marker equivalent-to/
!        alias-for "::TDT". No change to functionality.
!
!        Corrected OUTPUT argument name in $Brief_I/O section (it was
!        STRING) and improved its description in $Detailed_Output.
!        Fixed call to TPICTR in $Particulars.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary entries in $Revisions section. Converted the
!        existing code fragments into complete examples and added
!        references to required LSKs.
!
!        Updated $Exceptions section, rewording the existing entry
!        and adding three additional cases.
!
!-    SPICELIB Version 3.3.1, 31-JAN-2017 (NJB)
!
!        Updated header comments to draw attention to the fact that
!        rounding can be commanded.
!
!-    SPICELIB Version 3.3.0, 23-OCT-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in RMAIND call. Replaced header references to LDPOOL with
!        references to FURNSH.
!
!-    SPICELIB Version 3.2.0, 09-APR-2002 (WLT)
!
!        Added code to bracket the fractional part of a time component
!        so that it cannot become negative due to inability to invert
!        arithmetic operations with double precision arithmetic.
!
!-    SPICELIB Version 3.1.0, 21-JUN-2001 (WLT)
!
!        Added the format picture components ?ERA? and ?era? which
!        vanish for years after 999 A.D.
!
!-    SPICELIB Version 3.0.2, 10-APR-2000 (WLT)
!
!        Declared SCAN to be external.
!
!-    SPICELIB Version 3.0.1, 22-JUN-1998 (WLT)
!
!        A number of typographical and grammatical errors
!        were corrected in the header.
!
!-    SPICELIB Version 3.0.0, 30-DEC-1997 (WLT)
!
!        The previous version of this routine did not output
!        fractional components for epochs prior to 1 A.D.
!
!        In addition, the default time system, calendar and time zone
!        are obtained from TIMDEF.
!
!-    SPICELIB Version 2.0.0, 01-APR-1997 (WLT)
!
!        In the event that the format picture requested 'YR' as
!        the first component of a time string, the previous edition
!        of this routine used the year value corresponding to the
!        last call to this routine (or whatever happened to be in
!        memory on the first call). This error has been corrected.
!
!-    SPICELIB Version 1.0.0, 26-JUL-1996 (WLT) (MJS) (NJB)
!
!-&


! $ Index_Entries
!
!     Convert and format d.p. seconds past J2000 as a string
!
!-&


! $ Revisions
!
!-    SPICELIB Version 3.0.0, 30-DEC-1997 (WLT)
!
!        The previous version of this routine did not output
!        fractional components for epochs prior to 1 A.D.
!
!        This error was due to overuse of the original year
!        component returned from TTRANS. The original year
!        component is now saved for use in computing the fractional
!        component. The modified year (used in printing B.C. epochs)
!        is stored in a separate variable.
!
!-    SPICELIB Version 2.0.0, 01-APR-1997 (WLT)
!
!        In the event that the format picture requested 'YR' as
!        the first component of a time string, the previous edition
!        of this routine used the year value corresponding to the
!        last call to this routine (or whatever happened to be in
!        memory on the first call). This error has been corrected.
!
!
!        The error was fixed by recoding the following IF THEN statement
!
!              IF (       TYPE .EQ. YEAR
!    .               .OR. TYPE .EQ. MONTH
!    .               .OR. TYPE .EQ. MON
!    .               .OR. TYPE .EQ. DAY
!    .               .OR. TYPE .EQ. DOY
!    .               .OR. TYPE .EQ. NOON
!    .               .OR. TYPE .EQ. HOUR
!    .               .OR. TYPE .EQ. ERA
!    .               .OR. TYPE .EQ. AMPM
!    .               .OR. TYPE .EQ. MINUTE
!    .               .OR. TYPE .EQ. SEC   ) THEN
!
!        as
!
!              IF (       TYPE .EQ. YEAR
!    .               .OR. TYPE .EQ. YR
!    .               .OR. TYPE .EQ. MONTH
!    .               .OR. TYPE .EQ. MON
!    .               .OR. TYPE .EQ. DAY
!    .               .OR. TYPE .EQ. DOY
!    .               .OR. TYPE .EQ. NOON
!    .               .OR. TYPE .EQ. HOUR
!    .               .OR. TYPE .EQ. ERA
!    .               .OR. TYPE .EQ. AMPM
!    .               .OR. TYPE .EQ. MINUTE
!    .               .OR. TYPE .EQ. SEC   ) THEN
!
!
!-    Beta Version 2.1.0, 17-MAR-1994 (MJS) (NJB)
!
!        Integer argument to BRCKTD changed from 0 to 0.0D0.
!
!-&


!
!     SPICELIB functions
!
      integer               brckti
      integer               bsrchc
      integer               isrchi
      integer               rtrim
      external              scan

      double precision      brcktd
      double precision      unitim
      double precision      spd
      double precision      j2000
      double precision      j1950

      logical               return

!
!     Local parameters
!
!
!     The parameters below act essentially as an enumeration
!     of the various kinds of components we will be looking at in the
!     input time string.
!
      integer               currnt
      parameter           ( currnt = 1 )

      integer               begin
      parameter           ( begin  = currnt )

      integer               noname
      parameter           ( noname = currnt  + 1 )

      integer               year
      parameter           ( year   = noname + 1 )

      integer               yr
      parameter           ( yr     = year   + 1 )

      integer               umon
      parameter           ( umon   = yr     + 1 )

      integer               mmon
      parameter           ( mmon   = umon   + 1 )

      integer               lmon
      parameter           ( lmon   = mmon   + 1 )

      integer               umnth
      parameter           ( umnth  = lmon   + 1 )

      integer               mmnth
      parameter           ( mmnth  = umnth  + 1 )

      integer               lmnth
      parameter           ( lmnth  = mmnth  + 1 )

      integer               month
      parameter           ( month  = lmnth  + 1 )

      integer               doy
      parameter           ( doy    = month  + 1 )

      integer               uwkd
      parameter           ( uwkd   = doy    + 1 )

      integer               mwkd
      parameter           ( mwkd   = uwkd   + 1 )

      integer               lwkd
      parameter           ( lwkd   = mwkd   + 1 )

      integer               uweekd
      parameter           ( uweekd = lwkd   + 1 )

      integer               mweekd
      parameter           ( mweekd = uweekd + 1 )

      integer               lweekd
      parameter           ( lweekd = mweekd + 1 )

      integer               day
      parameter           ( day    = lweekd + 1 )

      integer               minute
      parameter           ( minute = day    + 1 )

      integer               hour
      parameter           ( hour   = minute + 1 )

      integer               sec
      parameter           ( sec    = hour   + 1 )

      integer               point
      parameter           ( point  = sec    + 1 )

      integer               place
      parameter           ( place  = point  + 1 )

      integer               julian
      parameter           ( julian = place  + 1 )

      integer               utc
      parameter           ( utc    = julian + 1 )

      integer               tdb
      parameter           ( tdb    = utc    + 1 )

      integer               tdt
      parameter           ( tdt    = tdb    + 1 )

      integer               tt
      parameter           ( tt     = tdt    + 1 )

      integer               sp2000
      parameter           ( sp2000 = tt     + 1 )

      integer               sp1950
      parameter           ( sp1950 = sp2000 + 1 )

      integer               round
      parameter           ( round  = sp1950 + 1 )

      integer               trunc
      parameter           ( trunc  = round  + 1 )

      integer               uera
      parameter           ( uera   = trunc  + 1 )

      integer               lera
      parameter           ( lera   = uera   + 1 )

      integer               uerax
      parameter           ( uerax  = lera + 1 )

      integer               lerax
      parameter           ( lerax  = uerax + 1 )

      integer               uampm
      parameter           ( uampm  = lerax   + 1 )

      integer               lampm
      parameter           ( lampm  = uampm  + 1 )

      integer               utcp
      parameter           ( utcp   = lampm  + 1 )

      integer               utcm
      parameter           ( utcm   = utcp   + 1 )

      integer               jcal
      parameter           ( jcal   = utcm   + 1 )

      integer               gcal
      parameter           ( gcal   = jcal   + 1 )

      integer               mcal
      parameter           ( mcal   = gcal   + 1 )

      integer               timsys
      parameter           ( timsys = mcal   + 1 )

      integer               calndr
      parameter           ( calndr = timsys + 1 )

      integer               ampm
      parameter           ( ampm   = calndr + 1 )

      integer               mon
      parameter           ( mon    = ampm   + 1 )

      integer               wkday
      parameter           ( wkday  = mon    + 1 )

      integer               era
      parameter           ( era    = wkday  + 1 )

      integer               noon
      parameter           ( noon   = era    + 1 )

      integer               rlyear
      parameter           ( rlyear = noon   + 1 )

      integer               finish
      parameter           ( finish = rlyear      )

!
!     The following parameters serve as an enumeration of the various
!     time formats that are recognized.
!
      integer               ymd
      parameter           ( ymd    = 1 )

      integer               contin
      parameter           ( contin = ymd + 1 )

!
!     The parameters below are used to declare the space needed for
!     scanning the input format string.
!
      integer               maxmrk
      parameter           ( maxmrk = 43  )

      integer               room
      parameter           ( room   = 100 )

!
!     The length of the local string that we will use for copying
!     the format string.
!
      integer               maxlen
      parameter           ( maxlen = 256 )

      integer               mrksiz
      parameter           ( mrksiz = 8 )

      integer               loclen
      parameter           ( loclen = 32 )

!
!     Local variables
!
      logical               first

!
!     The next set of variables holds the marks and auxiliary
!     arrays used for scanning the format string.
!
      character*(mrksiz)    marks  (     maxmrk )
      integer               class  ( 0 : maxmrk )
      integer               nmarks
      integer               mrklen (     maxmrk )
      integer               pntrs  (     100    )

      integer               dump   (     11     )
      integer               ndump
      integer               timfmt
      integer               type
      integer               numtyp

!
!     The variables below are used to hold, base formats, values of
!     time vector components, adjustments to use when rounding,
!     the lengths of the format pictures and whether or not various
!     components have already been computed.
!
      character*(32)        orignl ( begin : finish )
      double precision      values ( begin : finish )
      double precision      pad    ( begin : finish )
      integer               length ( begin : finish )
      integer               id     ( begin : finish )
      logical               have   ( begin : finish )

!
!     The array below contains the indexes of the various values
!     associated with the three different times of time vectors
!     that we will be using YMD, YD, CONTIN.
!
      integer               compnt ( 8, contin )

!
!     We will be making a local copy of the input format string
!     and the input time.
!
      character*(maxlen)    mystr
      character*(maxlen)    substr
      character*(maxlen)    string

      double precision      myet

!
!     The integers below are used to mark substring boundaries.
!
      integer               b
      integer               e
      integer               start
      integer               ntokns
      integer               ident  (  room   )
      integer               beg    (  room   )
      integer               end    (  room   )

      character*(loclen)    fmt
      integer               width
      integer               appnd

!
!     Times come in three flavors: TT or TDT, TDB, UTC.  The one used
!     on this particular invocation of TIMOUT is stored in TIMTYP.
!     The routine TTRANS needs to have input and output time vector
!     types.  The one used based upon the input PICTUR is stored
!     in BASTYP.
!
      integer               timtyp
      character*(8)         ymdfmt
      character*(8)         ywfmt
      character*(16)        bastyp
      character*(16)        intyp
      character*(16)        tsys
      character*(16)        cal
      character*(32)        zon

      double precision      hoff
      double precision      moff
      integer               caltyp
      integer               last

      logical               ok
      logical               doera
      logical               dozone

!
!     Loop counters and delimiters
!
      integer               i
      integer               j
      integer               part
      integer               stopat
      integer               trncat

!
!     Utility double precision numbers
!
      double precision      delta
      double precision      factor
      double precision      frac
      double precision      incr
      double precision      intmed
      double precision      ntvec  ( 8 )
      double precision      ptvec  ( 8 )
      double precision      tempd
      double precision      timpad
      double precision      tvec   ( 8 )
      double precision      value

!
!     The array power is used to assist in the truncation of double
!     precision values.
!
      double precision      power  ( 0 : 14 )
      double precision      x

!
!     calendar variables.
!
      integer               jyear
      integer               jmonth
      integer               jday
      integer               jdoy
      integer               gyear
      integer               gmonth
      integer               gday
      integer               gdoy
      logical               go2jul

!
!     Character string representations for months and week days.
!
      character*(9)         months ( 12 )
      character*(9)         mymon
      integer               mylen
      integer               montyp
      integer               mlen   ( 12 )
      character*(9)         wkdays (  7 )
      character*(9)         mywkd
      integer               wklen  (  7 )
      integer               wktyp

      integer               indx

      logical               unknwn
      logical               making
      logical               pumpup
      logical               vanish

!
!     Save everything.
!
      save


!
!     Initial values
!

      data                 months  / 'January',   'February', &
                                     'March',     'April', &
                                     'May',       'June', &
                                     'July',      'August', &
                                     'September', 'October', &
                                     'November',  'December' /

      data                 mlen    /  7,           8, &
                                      5,           5, &
                                      3,           4, &
                                      4,           6, &
                                      9,           7, &
                                      8,           8 /

      data                 wkdays  / 'Sunday',    'Monday', &
                                     'Tuesday',   'Wednesday', &
                                     'Thursday',  'Friday', &
                                     'Saturday' /

      data                 wklen   /  6,           6, &
                                      7,           9, &
                                      8,           6, &
                                      8 /

      data                 first   / .true. /

      data                 power   / 1.0d0,  1.0d1,  1.0d2,  1.0d3, &
                                     1.0d4,  1.0d5,  1.0d6,  1.0d7, &
                                     1.0d8,  1.0d9,  1.0d10, 1.0d11, &
                                     1.0d12, 1.0d13, 1.0d14         /

      if ( return() ) then
         return
      end if

      call chkin ( 'TIMOUT' )

!
!     Chapter 1. Initializations.
!     =================================================================
!
!     On the first pass, we need to set up the recognized tokens
!     that will be used for scanning, the classes of these tokens
!     and the array of ID's for time systems.
!
      if ( first ) then

         first    = .false.

         marks (  1 ) = 'YYYY'
         marks (  2 ) = 'YR'
         marks (  3 ) = 'MON'
         marks (  4 ) = 'Mon'
         marks (  5 ) = 'mon'
         marks (  6 ) = 'MONTH'
         marks (  7 ) = 'Month'
         marks (  8 ) = 'month'
         marks (  9 ) = 'MM'
         marks ( 10 ) = 'DOY'
         marks ( 11 ) = 'WKD'
         marks ( 12 ) = 'Wkd'
         marks ( 13 ) = 'wkd'
         marks ( 14 ) = 'WEEKDAY'
         marks ( 15 ) = 'Weekday'
         marks ( 16 ) = 'weekday'
         marks ( 17 ) = 'DD'
         marks ( 18 ) = 'MN'
         marks ( 19 ) = 'HR'
         marks ( 20 ) = 'SC'
         marks ( 21 ) = '.#'
         marks ( 22 ) = '#'
         marks ( 23 ) = 'JULIAND'
         marks ( 24 ) = '::UTC'
         marks ( 25 ) = '::TDB'
         marks ( 26 ) = '::TDT'
         marks ( 27 ) = '::TT'
         marks ( 28 ) = 'SP2000'
         marks ( 29 ) = 'SP1950'
         marks ( 30 ) = '::RND'
         marks ( 31 ) = '::TRNC'
         marks ( 32 ) = 'ERA'
         marks ( 33 ) = 'era'
         marks ( 34 ) = 'AMPM'
         marks ( 35 ) = 'ampm'
         marks ( 36 ) = '::UTC+'
         marks ( 37 ) = '::UTC-'
         marks ( 38 ) = '::JCAL'
         marks ( 39 ) = '::GCAL'
         marks ( 40 ) = '::MCAL'
         marks ( 41 ) = 'AP'
         marks ( 42 ) = '?ERA?'
         marks ( 43 ) = '?era?'

         nmarks       =  43

         call scanpr ( nmarks, marks, mrklen, pntrs )

!
!        Now that we've prepared our recognized substrings and
!        auxiliary arrays for scanning, collect the id's of the
!        various marks and classify the various marks.
!        substrings.
!
         id ( noname ) = 0
         id ( year   ) = bsrchc ( 'YYYY',    nmarks, marks )
         id ( yr     ) = bsrchc ( 'YR',      nmarks, marks )
         id ( umon   ) = bsrchc ( 'MON',     nmarks, marks )
         id ( mmon   ) = bsrchc ( 'Mon',     nmarks, marks )
         id ( lmon   ) = bsrchc ( 'mon',     nmarks, marks )
         id ( umnth  ) = bsrchc ( 'MONTH',   nmarks, marks )
         id ( mmnth  ) = bsrchc ( 'Month',   nmarks, marks )
         id ( lmnth  ) = bsrchc ( 'month',   nmarks, marks )
         id ( month  ) = bsrchc ( 'MM',      nmarks, marks )
         id ( doy    ) = bsrchc ( 'DOY',     nmarks, marks )
         id ( uwkd   ) = bsrchc ( 'WKD',     nmarks, marks )
         id ( mwkd   ) = bsrchc ( 'Wkd',     nmarks, marks )
         id ( lwkd   ) = bsrchc ( 'wkd',     nmarks, marks )
         id ( uweekd ) = bsrchc ( 'WEEKDAY', nmarks, marks )
         id ( mweekd ) = bsrchc ( 'Weekday', nmarks, marks )
         id ( lweekd ) = bsrchc ( 'weekday', nmarks, marks )
         id ( day    ) = bsrchc ( 'DD',      nmarks, marks )
         id ( minute ) = bsrchc ( 'MN',      nmarks, marks )
         id ( hour   ) = bsrchc ( 'HR',      nmarks, marks )
         id ( sec    ) = bsrchc ( 'SC',      nmarks, marks )
         id ( point  ) = bsrchc ( '.#',      nmarks, marks )
         id ( place  ) = bsrchc ( '#',       nmarks, marks )
         id ( julian ) = bsrchc ( 'JULIAND', nmarks, marks )
         id ( utc    ) = bsrchc ( '::UTC',   nmarks, marks )
         id ( tdb    ) = bsrchc ( '::TDB',   nmarks, marks )
         id ( tdt    ) = bsrchc ( '::TDT',   nmarks, marks )
         id ( tt     ) = bsrchc ( '::TT',    nmarks, marks )
         id ( sp2000 ) = bsrchc ( 'SP2000',  nmarks, marks )
         id ( sp1950 ) = bsrchc ( 'SP1950',  nmarks, marks )
         id ( round  ) = bsrchc ( '::RND',   nmarks, marks )
         id ( trunc  ) = bsrchc ( '::TRNC',  nmarks, marks )
         id ( uera   ) = bsrchc ( 'ERA',     nmarks, marks )
         id ( lera   ) = bsrchc ( 'era',     nmarks, marks )
         id ( uerax  ) = bsrchc ( '?ERA?',   nmarks, marks )
         id ( lerax  ) = bsrchc ( '?era?',   nmarks, marks )
         id ( uampm  ) = bsrchc ( 'AMPM',    nmarks, marks )
         id ( lampm  ) = bsrchc ( 'ampm',    nmarks, marks )
         id ( utcp   ) = bsrchc ( '::UTC+',  nmarks, marks )
         id ( utcm   ) = bsrchc ( '::UTC-',  nmarks, marks )
         id ( jcal   ) = bsrchc ( '::JCAL',  nmarks, marks )
         id ( gcal   ) = bsrchc ( '::GCAL',  nmarks, marks )
         id ( mcal   ) = bsrchc ( '::MCAL',  nmarks, marks )
         id ( ampm   ) = bsrchc ( 'AP',      nmarks, marks )

         class ( id(noname)  ) =  noname
         class ( id(year)    ) =  year
         class ( id(yr)      ) =  yr
         class ( id(umon)    ) =  mon
         class ( id(mmon)    ) =  mon
         class ( id(lmon)    ) =  mon
         class ( id(umnth)   ) =  mon
         class ( id(mmnth)   ) =  mon
         class ( id(lmnth)   ) =  mon
         class ( id(month)   ) =  month
         class ( id(doy)     ) =  doy
         class ( id(uwkd)    ) =  wkday
         class ( id(mwkd)    ) =  wkday
         class ( id(lwkd)    ) =  wkday
         class ( id(uweekd)  ) =  wkday
         class ( id(mweekd)  ) =  wkday
         class ( id(lweekd)  ) =  wkday
         class ( id(day)     ) =  day
         class ( id(minute)  ) =  minute
         class ( id(hour)    ) =  hour
         class ( id(sec)     ) =  sec
         class ( id(point)   ) =  point
         class ( id(place)   ) =  place
         class ( id(julian)  ) =  julian
         class ( id(utc)     ) =  timsys
         class ( id(tdb)     ) =  timsys
         class ( id(tdt)     ) =  timsys
         class ( id(tt)      ) =  timsys
         class ( id(sp2000)  ) =  sp2000
         class ( id(sp1950)  ) =  sp1950
         class ( id(round)   ) =  round
         class ( id(trunc)   ) =  trunc
         class ( id(uera)    ) =  era
         class ( id(lera)    ) =  era
         class ( id(uerax)   ) =  era
         class ( id(lerax)   ) =  era
         class ( id(uampm)   ) =  noon
         class ( id(lampm)   ) =  noon
         class ( id(utcp)    ) =  timsys
         class ( id(utcm)    ) =  timsys
         class ( id(jcal)    ) =  calndr
         class ( id(gcal)    ) =  calndr
         class ( id(mcal)    ) =  calndr
         class ( id(ampm)    ) =  ampm

         do i = begin, finish
            pad (i) = 0.0d0
         end do

         pad  ( sec    )     =   0.5d0
         pad  ( minute )     =  60.0d0 * pad(sec)
         pad  ( hour   )     =  60.0d0 * pad(minute)
         pad  ( day    )     =  24.0d0 * pad(hour)
         pad  ( month  )     =  30.0d0 * pad(day)
         pad  ( doy    )     =           pad(day)
         pad  ( mon    )     =           pad(month)
         pad  ( year   )     = 365.0d0 * pad(day)
         pad  ( yr     )     = 365.0d0 * pad(day)
         pad  ( julian )     =           pad(day)
         pad  ( sp2000 )     =           pad(sec)
         pad  ( sp1950 )     =           pad(sec)
         pad  ( ampm   )     =           pad(hour)

!
!        After we've made the initial scan for tokens and determined
!        the time system requested, we will want to get rid of the
!        time system tokens.
!
         dump (  1 ) = id(utc)
         dump (  2 ) = id(tdt)
         dump (  3 ) = id(tt)
         dump (  4 ) = id(tdb)
         dump (  5 ) = id(round)
         dump (  6 ) = id(trunc)
         dump (  7 ) = id(utcm)
         dump (  8 ) = id(utcp)
         dump (  9 ) = id(jcal)
         dump ( 10 ) = id(gcal)
         dump ( 11 ) = id(mcal)

         ndump      = 11

!
!        Set up the default formats for the various time components
!
         orignl(year)   = 'YYYY'
         length(year)   =  4

         orignl(yr)     = '0Y'
         length(yr)     =  2

         orignl(doy)    = '0DD'
         length(doy)    =  3

         orignl(day)    = '0D'
         length(day)    =  2

         orignl(month)  = '0M'
         length(month)  =  2

         orignl(hour)   = '0H'
         length(hour)   =  2

         orignl(ampm)   = '0H'
         length(ampm)   =  2

         orignl(minute) = '0M'
         length(minute) =  2

         orignl(sec)    = '0S'
         length(sec)    =  2

         orignl(julian) = 'XXXXXXX'
         length(julian) =  7

         orignl(sp2000) = 'XXXXXXXXXXX'
         length(sp2000) =  11

         orignl(sp1950) = 'XXXXXXXXXXX'
         length(sp1950) =  11

!
!        Finally set up the component pointers...
!
         compnt(1,ymd)    = rlyear
         compnt(2,ymd)    = month
         compnt(3,ymd)    = day
         compnt(4,ymd)    = hour
         compnt(5,ymd)    = minute
         compnt(6,ymd)    = sec

         compnt(1,contin) = currnt

      end if

!
!     Chapter 2.  Parsing the input picture.
!     ==============================================================
!
!     First let's copy the input picture into local storage
!     (left justified) and get just past the end of the
!     significant portion (this way the loop that constructs the
!     output string will terminate with no unfinished business
!     left to resolve).
!
      mystr = ' '
      call    ljust ( pictur, mystr(1:maxlen-1) )
      e     = rtrim (         mystr             ) + 1
      start = 1

!
!     Scan the input string.
!
      call scan ( mystr(1:e), &
                  marks,      mrklen, pntrs,  room,  start, &
                  ntokns,     ident,  beg,    end           )

!
!     Locate the time system that will be used.  This must
!     be one of the following: UTC, TDB, TT, TDT
!
      unknwn = .true.
      go2jul = .false.
      dozone = .false.
      i      = 1
      hoff   = 0.0d0
      moff   = 0.0d0

!
!     Get the default time type from TIMDEF
!
      call timdef ( 'GET', 'SYSTEM', tsys )

      if ( tsys .eq. 'UTC' ) then
         timtyp = id(utc)
      else if ( tsys .eq. 'TDB' ) then
         timtyp = id(tdb)
      else if ( (tsys .eq. 'TDT') .or. (tsys .eq. 'TT') ) then
         timtyp = id(tdt)
      else
         timtyp = id(utcp)

         call timdef ( 'GET', 'ZONE', zon )

         call prefix ( '::', 0, zon )
         call zzutcpm( zon, 1, hoff, moff, last, ok )

         dozone = ok

!
!        The routine TIMDEF uses ZZUTCPM to determine whether
!        or not a time zone is legitimate before it stores it
!        to be "GOTTEN."  As a result the value of OK should
!        always be .TRUE.  However, just in case TIMDEF should
!        someday use something other that ZZUTCPM for checking
!        we put in the unneeded check below.
!
         if ( .not. ok ) then
            timtyp = id(utc)
         end if

      end if


      do while ( unknwn .and. i .le. ntokns )

         if ( class(ident(i)) .eq. timsys ) then
            timtyp = ident(i)
            unknwn = .false.
            dozone = .false.

            if(      ident(i) .eq. id(utcp) &
                .or. ident(i) .eq. id(utcm) ) then

!
!              We've got a time zone specification. Parse it and
!              store the offsets from UTC.
!
               call zzutcpm ( mystr, beg(i), hoff, moff, last, ok )

               if ( ok ) then

                  dozone = .true.
                  timtyp = id(utcp)

!
!                 If we ran all the way up to the end of the next
!                 token, we simply reset the identity of the next
!                 token to be a zone type and increment  I.
!
!                 This way we never see the next token in this loop
!                 and it gets removed later when time systems and
!                 other meta markers from  our copy of the time
!                 format string.
!
                  if ( last .eq. end(i+1) ) then
                     ident(i+1) = ident(i)
                     i          = i + 1
                  else
                     end(i)     = last
                     beg(i+1)   = last + 1
                  end if

               end if

            end if


         end if

         i = i + 1

      end do

!
!     Determine whether we should use the Julian or gregorian (default)
!     calendar
!
      unknwn = .true.
      i      = 1

!
!     Get the default calendar from TIMDEF.
!
      call timdef ( 'GET', 'CALENDAR', cal )

      if      ( cal .eq. 'GREGORIAN' ) then
         caltyp = id(gcal)
      else if ( cal .eq. 'JULIAN'    ) then
         caltyp = id(jcal)
      else
         caltyp = id(mcal)
      end if

      do while ( unknwn .and. i .le. ntokns )

         if ( class(ident(i)) .eq. calndr ) then
            caltyp = ident(i)
            unknwn = .false.
         end if

         i = i + 1
      end do


!
!     Next determine whether or not we shall be performing rounding
!     on output.
!
      pumpup =      isrchi ( id(round), ntokns, ident ) .ne. 0

!
!     Determine if we have an Era specification
!
      doera  =      isrchi ( id(lera),  ntokns, ident ) .ne. 0 &
               .or. isrchi ( id(uera),  ntokns, ident ) .ne. 0 &
               .or. isrchi ( id(uerax), ntokns, ident ) .ne. 0 &
               .or. isrchi ( id(lerax), ntokns, ident ) .ne. 0

!
!     Until we've examined the year, we assume that the era is not
!     supposed to vanish.
!
      vanish = .false.
!
!     Next remove all of the time system dudes from the list of
!     tokens.
!
      call scanrj ( dump, ndump, ntokns, ident, beg, end )


!
!     If the user wants to round the output, we need to pump up ET
!     by the smallest significant part of the input picture.  But
!     in either case we are going to pad the input time.  For now
!     we pad it by zero.
!
      timpad = 0.0d0

      if ( pumpup ) then

!
!        We need to determine the amount to pad ET by.  So we need
!        to look at the string and find the least significant component
!        that has been requested.  Keep in mind that the last token
!        is of type NONAME (its a blank) by construction.
!
         i = 1

         do while ( i .le. ntokns )

            type = class ( ident(i) )

            if   (      ( type .eq. noname ) &
                   .or. ( type .eq. point  ) &
                   .or. ( type .eq. place  ) &
                   .or. ( type .eq. noon   ) &
                   .or. ( type .eq. era    ) &
                   .or. ( type .eq. mon    ) &
                   .or. ( type .eq. wkday  ) ) then

!
!              Don't do anything, just go on to the next token.
!
               i = i + 1

            else

!
!              Look up the amount we should pad our time by.
!
               factor = 1.0d0
               incr   = pad ( type )

!
!              Examine the next token.  If it's not a decimal point
!              and marker, we have the least significant part of
!              this component.
!
               i      = i + 1
               type   = class(ident(i))

               if ( type .eq. point ) then

                  factor = factor * 0.1d0
                  i      = i + 1

!
!                 Now just look for the end of the string of place
!                 holders
!
                  do while ( ident(i) .eq. id(place) )
                     factor = factor * 0.1d0
                     i      = i + 1
                  end do

               end if

!
!              Now compute the time pad for this component of the
!              time string.
!
               incr = incr * factor

               if ( timpad .ne. 0.0d0 ) then
                  timpad = min ( timpad, incr )
               else
                  timpad = incr
               end if

            end if

         end do

      end if

!
!     Right now we don't have any components of the time format
!     and we don't need any of them so far.
!
      do part = begin, finish
         have(part) = .false.
      end do


!
!     Set up the input time format and the output time format that will
!     be used later.
!
!     The input time format is used to convert the basic ET we have now
!     to one of the various time formats that are supported by the
!     routine TTRANS.  If we are going to construct a string in one of
!     the dynamical time systems we will call the input time a formal
!     time in seconds past a formal calendar epoch of J2000.  If on the
!     other hand we are going to construct a UTC based string, we will
!     convert our ET to an earth based epoch (TT, TDT) and use this as
!     our base input system.
!
!
      myet = et

      if      ( timtyp .eq. id(tdb) ) then

!
!        Since we are likely to need SP2000, SP1950 or JD, we
!        compute them now.
!
         myet           = myet + timpad
         values(sp2000) = myet
         values(julian) = unitim( myet,   'TDB',  'JDTDB'  )
         values(sp1950) = values(sp2000) + spd()*( j2000() - j1950() )

         bastyp         = 'FORMAL'
         ymdfmt         = 'YMDF'
         ywfmt          = 'YMWDF'
         have  (sp2000) = .true.
         have  (sp1950) = .true.
         have  (julian) = .true.

      else if ( (timtyp .eq. id(tdt)) .or. &
                (timtyp .eq. id(tt)) ) then

         myet           = unitim( myet,   'TDB',  'TDT'    )  + timpad
         values(sp2000) = myet
         values(julian) = unitim( myet,   'TDT',  'JDTDT'  )
         values(sp1950) = values(sp2000) + spd()*( j2000() - j1950() )

         bastyp         = 'FORMAL'
         ymdfmt         = 'YMDF'
         ywfmt          = 'YMWDF'
         have  (sp2000) = .true.
         have  (sp1950) = .true.
         have  (julian) = .true.

      else

!
!        In this case we convert to an earth based frame for our
!        working epoch.  This rounds properly when it's time to get
!        fractional components.
!
         myet   =  unitim ( myet, 'TDB', 'TDT' ) + timpad
         bastyp = 'TDT'
         ymdfmt = 'YMD'
         ywfmt  = 'YMWD'

      end if

!
!     Chapter 3.  Building the Output String
!     ==================================================================
!

!
!     Now we are ready to go, we need to fetch the tokens
!     and construct the output string.  We will
!     put the next portion of the output at APPND
!
      appnd  =  1
      making = .false.

      do i = 1, ntokns

         type    = class( ident(i) )
         tvec(1) = myet

!
!        If the next marker is not one we use as a place holder
!        in the fractional part of decimal formats AND we
!        are in the process of building a format, then the format
!        building is done.  We can construct the component and
!        append it to the string we are building.
!
         if ( (type .ne. place) .and.  making ) then

!
!           We also need to be sure this isn't a decimal point
!           marker before we add on to the output string.
!
            if ( (type .ne. point) .or. have(point) ) then

!
!              We are going to truncate the number to the number of
!              places requested NOT round.
!
               trncat = brckti( width - length(numtyp) - 1, 0, 14 )
               frac   = value - dint(value )

               if ( frac .lt. 0 ) then
                  value = value - 1.0d0
                  frac  = frac  + 1.0d0
               end if

               intmed = ( dint( frac * power(trncat) ) - 0.125d0 ) &
                                      / power(trncat)

               frac   = brcktd( intmed,      0.0d0,      1.0d0 )

               value  = dint(value) + frac

               call dpfmt ( value, fmt, substr )


               string(appnd:) = substr
               appnd          = appnd + width
               have  (point)  =.false.
               making         =.false.

            end if

         end if

!
!        If the token isn't recognized we can just
!        append it to the string we are constructing and
!        adjust the point at which the next substring is
!        to be appended.
!
         if ( type .eq. noname ) then

            string(appnd:) = mystr ( beg(i) : end(i) )
            appnd          = appnd - beg(i) + end(i) + 1

!
!        If the token is a place holder, we either just append it
!        or tack it on to a format string we are creating..
!
         else if ( type .eq. place ) then


            if ( making ) then
               b           = width + 1
               e           = b     - beg(i) + end(i)
               fmt(b:e)    = mystr ( beg(i) : end(i) )
               width       = width - beg(i) + end(i) + 1
            else
               string(appnd:appnd) = mystr ( beg(i) : end(i) )
               appnd               = appnd - beg(i) + end(i) + 1
            end if

!
!        If the token is the decimal point plus place holder
!        AND we are making a format, we append it to the current
!        format and determine the fractional part of the current
!        quantity.
!
         else if ( type .eq. point ) then

            if ( .not. making ) then

               b           =  appnd
               e           =  appnd - beg(i) + end(i)
               string(b:e) =  mystr ( beg(i) : end(i) )
               appnd       =  e + 1
               have(point) = .false.

            else if ( timfmt .eq. contin ) then

               b           = width + 1
               e           = b     - beg(i) + end(i)
               fmt(b:e)    = mystr ( beg(i) : end(i) )
               width       = e
               have(point) =.true.

            else

               b           = width + 1
               e           = b     - beg(i) + end(i)
               fmt(b:e)    = mystr ( beg(i) : end(i) )
               width       = e
               have(point) =.true.

!
!              Since we obviously are going to be needing
!              the fractional part of this component we fetch it
!              now and add it to whatever the integer part of the
!              current value is.  Here's how we do this.
!              If we truncated the input time to this component
!              we'd have a value on an "integer" portion of the
!              time scale.
!              .
!              .               current
!              .               time
!              .               truncated    .---MYET
!                                   |       |
!                                   v       v
!              time scale: ---------+-------X-----------+-----
!                                                       ^
!                                                       |
!              .                               truncated time
!              .                               plus 1 in this
!              .                               component
!              .
!              Add one to the truncated component to get the
!              next integer component.  Finally we convert these
!              two constructed stings to seconds so that we can
!              get the "fractional part" of the current component.
!              Fortunately, when we computed the integer value
!              for this component we constructed the time
!              vectors we need, so we don't have to go to a lot
!              of trouble now.
!

               call ttrans( intyp, bastyp, ptvec  )
               call ttrans( intyp, bastyp, ntvec  )

               delta = max(1.0d0, ntvec(1) - ptvec(1) )
               frac  = brcktd( 0.0d0, 1.0d0, (myet - ptvec(1)) / delta )

               value = value + frac

            end if

         else

!
!        If we get to this point we have an honest time
!        string component to fetch.  We might already have
!        this guy.  If so we can just collect him from the
!        values buffer (although this collection is performed
!        after the next long IF-THEN block that gets the value
!        if we don't already have it).
!
            making      =.true.
            have(point) =.false.
            fmt         = orignl( type )
            width       = length( type )
            numtyp      =         type

            if ( .not. have(type) )  then

               tvec(1) = myet

!
!              Most components are handled in the next block.
!
               if (       (type .eq. year) &
                     .or. (type .eq. yr) &
                     .or. (type .eq. month) &
                     .or. (type .eq. mon) &
                     .or. (type .eq. day) &
                     .or. (type .eq. doy) &
                     .or. (type .eq. noon) &
                     .or. (type .eq. hour) &
                     .or. (type .eq. era) &
                     .or. (type .eq. ampm) &
                     .or. (type .eq. minute) &
                     .or. (type .eq. sec)   ) then


                  call ttrans ( bastyp, ymdfmt, tvec )

!
!                 The seconds component is finished.  Regardless
!                 of any zone or calendar modifications, we just
!                 don't have to deal with this component any more.
!
                  values(sec)    = tvec(6)

!
!                 If we need to deal with time zones, this is
!                 the time to do it.
!
                  if ( timtyp .eq. id(utcp) ) then

                     tvec(4) = tvec(4) + hoff
                     tvec(5) = tvec(5) + moff
                     tvec(6) = 0.0d0

                     call ttrans ( 'YMDF', 'YMDF', tvec )

                  end if

!
!                 One way or the other the hours and minutes components
!                 are finished.  Record their values.
!
                  values(hour)   = tvec(4)
                  values(minute) = tvec(5)

                  if ( values(hour) .eq. 0.0d0 ) then
                     values(ampm) =  12.0d0
                  else if ( values(hour) .gt. 12.0d0 ) then
                     values(ampm) = values(hour) - 12.0d0
                  else
                     values(ampm) = values(hour)
                  end if

!
!                 Finally, if we need to change the calendar to
!                 Julian this is the place to handle it.
!
                  jyear  = nint( tvec(1) )
                  jmonth = nint( tvec(2) )
                  jday   = nint( tvec(3) )

                  call gr2jul ( jyear, jmonth, jday, jdoy )

                  gyear  = jyear
                  gmonth = jmonth
                  gday   = jday

                  call jul2gr ( gyear, gmonth, gday, gdoy )

                  if      ( caltyp .eq. id(gcal) ) then

                     values(year)  = dble(gyear)
                     values(month) = dble(gmonth)
                     values(day)   = dble(gday)
                     values(doy)   = dble(gdoy)
                     go2jul        = .false.

                  else if ( caltyp .eq. id(jcal) ) then

                     values(year)  = dble(jyear)
                     values(month) = dble(jmonth)
                     values(day)   = dble(jday)
                     values(doy)   = dble(jdoy)
                     go2jul        = .true.

                  else if ( caltyp .eq. id(mcal) ) then


                     if      ( gyear .lt. 1582 ) then
                        go2jul = .true.
                     else if ( gyear .gt. 1582 ) then
                        go2jul = .false.
                     else if ( gmonth .lt. 10 ) then
                        go2jul = .true.
                     else if ( gmonth .gt. 10 ) then
                        go2jul = .false.
                     else if ( gday   .ge. 15 ) then
                        go2jul = .false.
                     else
                        go2jul = .true.
                     end if

                     if ( go2jul ) then
                        values(year)  = dble(jyear)
                        values(month) = dble(jmonth)
                        values(day)   = dble(jday)
                        values(doy)   = dble(jdoy)
                     else
                        values(year)  = dble(gyear)
                        values(month) = dble(gmonth)
                        values(day)   = dble(gday)
                        values(doy)   = dble(gdoy)
                     end if

                  end if

!
!                 Determine the era associated with the epoch.  Also
!                 if the year component is negative, we handle  that
!                 now.
!
!                 We store the actual value of the year so that
!                 it can be used when determining rounding of
!                 other components.
!
                  values(rlyear) = values(year)

                  if ( doera ) then

                     if ( values(year) .lt. 1 ) then
                        values(year) = 1.0d0 - values(year)
                        values(era)  = 1.0d0
                     else
                        values(era)  = 2.0d0
                     end if

                     vanish = values(year) .ge. 1000.0d0

                  end if

!
!                 Fetch the last two digits of the year.
!
                  call rmaind ( values(year), 100.0d0, x, tempd )
                  values(yr) = tempd

                  have  (year)   = .true.
                  have  (yr)     = .true.
                  have  (doy)    = .true.
                  have  (month)  = .true.
                  have  (mon)    = .true.
                  have  (day)    = .true.
                  have  (hour)   = .true.
                  have  (minute) = .true.
                  have  (sec)    = .true.
                  have  (ampm)   = .true.
                  have  (era)    = .true.


               else if ( type .eq. wkday ) then

                  tvec(1) = myet

                  call ttrans ( bastyp, ywfmt, tvec )

!
!                 Weekday. If we need to deal with time zones, this is
!                 the time to do it.
!
                  if ( timtyp .eq. id(utcp) ) then

                     tvec(5) = tvec(5) + hoff
                     tvec(6) = tvec(6) + moff
                     tvec(7) = 0.0d0

                     call ttrans ( 'YMWDF', 'YMWDF', tvec )


                  end if

                  values(wkday)  = tvec(4)
                  have  (wkday)  =.true.

               else if (      ( type .eq. sp1950 ) &
                         .or. ( type .eq. sp2000 ) ) then

!
!                 The only way to get here is if the output time
!                 type is UTC or a time zone (otherwise we'd
!                 already HAVE SP2000 and SP1950).
!
                  tvec(1) = myet

                  call ttrans ( bastyp, 'FORMAL', tvec )

                  values(sp2000) = tvec(1)
                  values(sp1950) = values(sp2000) &
                                 + spd() * ( j2000() - j1950() )

                  have  (sp2000) = .true.
                  have  (sp1950) = .true.

               else if ( type .eq. julian ) then

!
!                 The same tale can be told here as in the last
!                 case.  We can only get here if this is UTC
!                 output.
!
                  tvec(1) = myet

                  call ttrans ( bastyp, 'JDUTC', tvec )

                  values(julian) = tvec(1)
                  have  (julian) =.true.

               end if


            end if

!
!           O.K. whatever thing we are about to construct, we now
!           have it's numeric value.  It's time to construct its
!           string  value.
!
!
!           We need to treat character months, weekdays, eras, a.m.'s
!           and p.m.'s specially.
!
            if ( type .eq. mon ) then

               indx           = nint  ( values(month) )
               mymon          = months( indx )
               montyp         = ident ( i )

!
!              There is no ELSE case in the block below because all of
!              the possible MONTYP values are checked explicitly.
!
               if      ( montyp .eq. id(umon)  ) then
                  call ucase ( mymon, mymon )
                  mymon(4:) = ' '
                  mylen     = 3
               else if ( montyp .eq. id(mmon)   ) then
                  mymon(4:) = ' '
                  mylen     = 3
               else if ( montyp .eq. id(lmon)   ) then
                  call lcase ( mymon, mymon )
                  mymon(4:) = ' '
                  mylen     = 3
               else if ( montyp .eq. id(mmnth) ) then
                  mylen     = mlen(indx)
               else if ( montyp .eq. id(umnth) ) then
                  call ucase ( mymon, mymon )
                  mylen     = mlen(indx)
               else if ( montyp .eq. id(lmnth) ) then
                  call lcase ( mymon, mymon )
                  mylen     = mlen(indx)
               end if

               string(appnd:) = mymon
               appnd          = appnd + mylen
               making         =.false.

            else if ( type .eq. wkday ) then

               indx           = nint  ( values(wkday) )
               mywkd          = wkdays( indx )
               wktyp          = ident ( i )

!
!              There is no ELSE case in the block below because all of
!              the possible WKTYP values are checked explicitly.
!
               if      ( wktyp .eq. id(uwkd)   ) then
                  call ucase ( mywkd, mywkd )
                  mywkd(4:) = ' '
                  mylen     = 3
               else if ( wktyp .eq. id(mwkd)   ) then
                  mywkd(4:) = ' '
                  mylen     = 3
               else if ( wktyp .eq. id(lwkd)   ) then
                  call lcase ( mywkd, mywkd )
                  mywkd(4:) = ' '
                  mylen     = 3
               else if ( wktyp .eq. id(mweekd) ) then
                  mylen     = wklen(indx)
               else if ( wktyp .eq. id(uweekd) ) then
                  call ucase ( mywkd, mywkd )
                  mylen     = wklen(indx)
               else if ( wktyp .eq. id(lweekd) ) then
                  call lcase ( mywkd, mywkd )
                  mylen     = wklen(indx)
               end if

               string(appnd:) = mywkd
               appnd          = appnd + mylen
               making         =.false.


            else if ( type .eq. era ) then

               if      (         values(era)    .eq. 2.0d0 &
                         .and. (      ident(i) .eq. id(uera) &
                                 .or. ident(i) .eq. id(uerax) ) ) then

                  string(appnd:) = ' A.D.'

               else if (         values(era)   .eq. 2.0d0 &
                         .and. (      ident(i) .eq. id(lera) &
                                 .or. ident(i) .eq. id(lerax) ) ) then
                   string(appnd:) = ' a.d.'

               else if  (         values(era)  .eq. 1.0d0 &
                         .and. (      ident(i) .eq. id(uera) &
                                 .or. ident(i) .eq. id(uerax) ) ) then

                  string(appnd:) = ' B.C.'

               else

                  string(appnd:) = ' b.c.'

               end if
!
!              If we have the vanishing kind of era, and we've
!              determined that it needs to vanish, then blank out the
!              portion of the string we just filled in. and don't
!              increment the place holder.
!
               if (     ident(i) .eq. id(uerax) &
                   .or. ident(i) .eq. id(lerax)) then

                  if ( vanish ) then
                     string(appnd:) = ' '
                     appnd = appnd + 1
                  else
                     appnd = appnd + 6
                  end if

               else
                  call ljust ( string(appnd:), string(appnd:) )
                  appnd          = appnd + 4
               end if

               making         =.false.

            else if ( type .eq. noon ) then


               if      (       ident(i)   .eq. id(uampm) &
                         .and. values(hour) .ge. 12.0d0    ) then

                  string(appnd:) = 'P.M.'

               else if (       ident(i)    .eq. id(uampm) &
                         .and. values(hour) .lt. 12.0d0    ) then

                  string(appnd:) = 'A.M.'

               else if (       ident(i)    .eq. id(lampm) &
                         .and. values(hour) .ge. 12.0d0    ) then

                  string(appnd:) = 'p.m.'

               else

                  string(appnd:) = 'a.m.'

               end if

               appnd          = appnd + 4
               making         =.false.

            else

               value = values( type )

            end if

!
!           If we are now creating a format string, we should
!           construct the previous time representation and
!           the next for this component (just in case we need it
!           later).
!
            if ( making ) then

!
!              We store the value of our current type in the
!              CURRNT slot of the values array.  This value
!              is used by the single numeric types, JD, SP2000,
!              and SP1950.
!
               values(currnt) = values(type)

!
!              Here's how this works:  We will copy all of
!              the components of the time representation up to
!              the current one.  This is the truncated representation
!              of our epoch.  We then copy these same components into
!              another time vector, but add an increment to the
!              component corresponding to the one we are dealing with
!              now.  We use an increment of 0 for those components that
!              already contain their fractional part. We use an
!              increment of 1 for the components that typically have
!              integer representations.
!
!
!              Zero out the previous and next time vectors so we won't
!              have to do it when we are filling in the truncated
!              portions.
!
               do j = 1, 7
                  ptvec(j) = 0.0d0
                  ntvec(j) = 0.0d0
               end do


               if      ( type .eq. year  .or. type .eq. yr ) then

                  stopat = 1
                  timfmt = ymd
                  intyp  = ymdfmt
                  incr   = 1.0d0

               else if ( type .eq. month) then

                  stopat = 2
                  timfmt = ymd
                  intyp  = ymdfmt
                  incr   = 1.0d0

               else if ( type .eq. day .or. type .eq. doy ) then

                  stopat = 3
                  timfmt = ymd
                  intyp  = ymdfmt
                  incr   = 1.0d0


               else if ( type .eq. hour .or. type .eq. ampm ) then

!
!                 Note that in this case (and the next 2) that if we
!                 an HOUR component, we had to get it either from
!                 a Day of Year format or from a Year Month Day
!                 format. Thus we have all of the more significant
!                 components for this format.
!
                  stopat = 4
                  timfmt = ymd
                  intyp  = ymdfmt
                  incr   = 1.0d0

               else if ( type .eq. minute  ) then

                  stopat = 5
                  timfmt = ymd
                  intyp  = ymdfmt

                  incr   = 1.0d0

               else if ( type .eq. sec  ) then

                  stopat = 6
                  timfmt = ymd
                  intyp  = ymdfmt
                  incr   = 0.0d0


               else if ( type .eq. julian ) then

                  stopat = 1
                  timfmt = contin
                  incr   = 0.0d0

                  if      ( (timtyp .eq. id(tdt)) .or. &
                            (timtyp .eq. id(tt))) then
                     intyp = 'JDTDT'
                  else if ( timtyp .eq. id(tdb) ) then
                     intyp = 'JDTDB'
                  else if (      timtyp .eq. id(utc) &
                            .or. timtyp .eq. id(utcp) ) then
                     intyp = 'JDUTC'
                  end if

               else

!
!                 The only types left are the continuous (numeric)
!                 types.
!
                  stopat = 1
                  timfmt = contin
                  incr   = 0.0d0
                  intyp  = bastyp


               end if

!
!              Ok.  We are now ready to construct the previous
!              and next time vectors.
!
               do j = 1, stopat
                  ptvec(j) = values(compnt(j,timfmt))
                  ntvec(j) = ptvec (j)
               end do

               ntvec(stopat) = ntvec(stopat) + incr

!
!              If the type is a year or month, then we need to set
!              the month to 1, so that we will be working with
!              beginnings of years not beginning of last months of
!              the previous year.
!
               if ( type .eq. year  .or. type .eq. yr ) then
                  ptvec(2) = 1.0d0
                  ntvec(2) = 1.0d0
                  ptvec(3) = 1.0d0
                  ntvec(3) = 1.0d0
               else if ( type .eq. month ) then
                  ptvec(3) = 1.0d0
                  ntvec(3) = 1.0d0
               end if

               if ( go2jul .and. timfmt .ne. contin ) then

!
!                 Convert both PTVEC and NTVEC to the gregorian
!                 calendar
!
                  jyear  = nint(ptvec(1))
                  jmonth = nint(ptvec(2))
                  jday   = nint(ptvec(3))

                  call jul2gr ( jyear, jmonth, jday, jdoy )

                  ptvec(1) = dble(jyear)
                  ptvec(2) = dble(jmonth)
                  ptvec(3) = dble(jday)

                  jyear  = nint(ntvec(1))
                  jmonth = nint(ntvec(2))
                  jday   = nint(ntvec(3))

                  call jul2gr ( jyear, jmonth, jday, jdoy )

                  ntvec(1) = dble(jyear)
                  ntvec(2) = dble(jmonth)
                  ntvec(3) = dble(jday)

               end if

!
!              Handle the +/- time zone shifts.
!
               if ( dozone .and. (timfmt .ne. contin) ) then

                  ptvec(4) = ptvec(4) - hoff
                  ntvec(4) = ntvec(4) - hoff

                  ptvec(5) = ptvec(5) - moff
                  ntvec(5) = ntvec(5) - moff

                  ptvec(6) = 0.0d0
                  ntvec(6) = 0.0d0

                  call ttrans ( 'YMDF', 'YMDF', ptvec )
                  call ttrans ( 'YMDF', 'YMDF', ntvec )

                  if ( type .eq. sec ) then
                     ptvec(6) = values(sec)
                     ntvec(6) = values(sec)
                  end if

               end if


            end if

         end if

      end do

!
!     All that's left to do is to copy the constructed string
!     to the output string.
!
      output = string


      call chkout ( 'TIMOUT' )
      return
      end
