!$Procedure TIMOUT ( Time Output )

      SUBROUTINE  TIMOUT ( ET, PICTUR, OUTPUT )

! $ Abstract
!
!     Convert an input epoch represented in TDB seconds past the TDB
!     epoch of J2000 to a character string formatted to the
!     specifications of a user's format picture.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      DOUBLE PRECISION      ET
      CHARACTER*(*)         PICTUR
      CHARACTER*(*)         OUTPUT

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
      INTEGER               BRCKTI
      INTEGER               BSRCHC
      INTEGER               ISRCHI
      INTEGER               RTRIM
      EXTERNAL              SCAN

      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      UNITIM
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      J1950

      LOGICAL               RETURN

!
!     Local parameters
!
!
!     The parameters below act essentially as an enumeration
!     of the various kinds of components we will be looking at in the
!     input time string.
!
      INTEGER               CURRNT
      PARAMETER           ( CURRNT = 1 )

      INTEGER               BEGIN
      PARAMETER           ( BEGIN  = CURRNT )

      INTEGER               NONAME
      PARAMETER           ( NONAME = CURRNT  + 1 )

      INTEGER               YEAR
      PARAMETER           ( YEAR   = NONAME + 1 )

      INTEGER               YR
      PARAMETER           ( YR     = YEAR   + 1 )

      INTEGER               UMON
      PARAMETER           ( UMON   = YR     + 1 )

      INTEGER               MMON
      PARAMETER           ( MMON   = UMON   + 1 )

      INTEGER               LMON
      PARAMETER           ( LMON   = MMON   + 1 )

      INTEGER               UMNTH
      PARAMETER           ( UMNTH  = LMON   + 1 )

      INTEGER               MMNTH
      PARAMETER           ( MMNTH  = UMNTH  + 1 )

      INTEGER               LMNTH
      PARAMETER           ( LMNTH  = MMNTH  + 1 )

      INTEGER               MONTH
      PARAMETER           ( MONTH  = LMNTH  + 1 )

      INTEGER               DOY
      PARAMETER           ( DOY    = MONTH  + 1 )

      INTEGER               UWKD
      PARAMETER           ( UWKD   = DOY    + 1 )

      INTEGER               MWKD
      PARAMETER           ( MWKD   = UWKD   + 1 )

      INTEGER               LWKD
      PARAMETER           ( LWKD   = MWKD   + 1 )

      INTEGER               UWEEKD
      PARAMETER           ( UWEEKD = LWKD   + 1 )

      INTEGER               MWEEKD
      PARAMETER           ( MWEEKD = UWEEKD + 1 )

      INTEGER               LWEEKD
      PARAMETER           ( LWEEKD = MWEEKD + 1 )

      INTEGER               DAY
      PARAMETER           ( DAY    = LWEEKD + 1 )

      INTEGER               MINUTE
      PARAMETER           ( MINUTE = DAY    + 1 )

      INTEGER               HOUR
      PARAMETER           ( HOUR   = MINUTE + 1 )

      INTEGER               SEC
      PARAMETER           ( SEC    = HOUR   + 1 )

      INTEGER               POINT
      PARAMETER           ( POINT  = SEC    + 1 )

      INTEGER               PLACE
      PARAMETER           ( PLACE  = POINT  + 1 )

      INTEGER               JULIAN
      PARAMETER           ( JULIAN = PLACE  + 1 )

      INTEGER               UTC
      PARAMETER           ( UTC    = JULIAN + 1 )

      INTEGER               TDB
      PARAMETER           ( TDB    = UTC    + 1 )

      INTEGER               TDT
      PARAMETER           ( TDT    = TDB    + 1 )

      INTEGER               TT
      PARAMETER           ( TT     = TDT    + 1 )

      INTEGER               SP2000
      PARAMETER           ( SP2000 = TT     + 1 )

      INTEGER               SP1950
      PARAMETER           ( SP1950 = SP2000 + 1 )

      INTEGER               ROUND
      PARAMETER           ( ROUND  = SP1950 + 1 )

      INTEGER               TRUNC
      PARAMETER           ( TRUNC  = ROUND  + 1 )

      INTEGER               UERA
      PARAMETER           ( UERA   = TRUNC  + 1 )

      INTEGER               LERA
      PARAMETER           ( LERA   = UERA   + 1 )

      INTEGER               UERAX
      PARAMETER           ( UERAX  = LERA + 1 )

      INTEGER               LERAX
      PARAMETER           ( LERAX  = UERAX + 1 )

      INTEGER               UAMPM
      PARAMETER           ( UAMPM  = LERAX   + 1 )

      INTEGER               LAMPM
      PARAMETER           ( LAMPM  = UAMPM  + 1 )

      INTEGER               UTCP
      PARAMETER           ( UTCP   = LAMPM  + 1 )

      INTEGER               UTCM
      PARAMETER           ( UTCM   = UTCP   + 1 )

      INTEGER               JCAL
      PARAMETER           ( JCAL   = UTCM   + 1 )

      INTEGER               GCAL
      PARAMETER           ( GCAL   = JCAL   + 1 )

      INTEGER               MCAL
      PARAMETER           ( MCAL   = GCAL   + 1 )

      INTEGER               TIMSYS
      PARAMETER           ( TIMSYS = MCAL   + 1 )

      INTEGER               CALNDR
      PARAMETER           ( CALNDR = TIMSYS + 1 )

      INTEGER               AMPM
      PARAMETER           ( AMPM   = CALNDR + 1 )

      INTEGER               MON
      PARAMETER           ( MON    = AMPM   + 1 )

      INTEGER               WKDAY
      PARAMETER           ( WKDAY  = MON    + 1 )

      INTEGER               ERA
      PARAMETER           ( ERA    = WKDAY  + 1 )

      INTEGER               NOON
      PARAMETER           ( NOON   = ERA    + 1 )

      INTEGER               RLYEAR
      PARAMETER           ( RLYEAR = NOON   + 1 )

      INTEGER               FINISH
      PARAMETER           ( FINISH = RLYEAR      )

!
!     The following parameters serve as an enumeration of the various
!     time formats that are recognized.
!
      INTEGER               YMD
      PARAMETER           ( YMD    = 1 )

      INTEGER               CONTIN
      PARAMETER           ( CONTIN = YMD + 1 )

!
!     The parameters below are used to declare the space needed for
!     scanning the input format string.
!
      INTEGER               MAXMRK
      PARAMETER           ( MAXMRK = 43  )

      INTEGER               ROOM
      PARAMETER           ( ROOM   = 100 )

!
!     The length of the local string that we will use for copying
!     the format string.
!
      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 256 )

      INTEGER               MRKSIZ
      PARAMETER           ( MRKSIZ = 8 )

      INTEGER               LOCLEN
      PARAMETER           ( LOCLEN = 32 )

!
!     Local variables
!
      LOGICAL               FIRST

!
!     The next set of variables holds the marks and auxiliary
!     arrays used for scanning the format string.
!
      CHARACTER*(MRKSIZ)    MARKS  (     MAXMRK )
      INTEGER               CLASS  ( 0 : MAXMRK )
      INTEGER               NMARKS
      INTEGER               MRKLEN (     MAXMRK )
      INTEGER               PNTRS  (     100    )

      INTEGER               DUMP   (     11     )
      INTEGER               NDUMP
      INTEGER               TIMFMT
      INTEGER               TYPE
      INTEGER               NUMTYP

!
!     The variables below are used to hold, base formats, values of
!     time vector components, adjustments to use when rounding,
!     the lengths of the format pictures and whether or not various
!     components have already been computed.
!
      CHARACTER*(32)        ORIGNL ( BEGIN : FINISH )
      DOUBLE PRECISION      VALUES ( BEGIN : FINISH )
      DOUBLE PRECISION      PAD    ( BEGIN : FINISH )
      INTEGER               LENGTH ( BEGIN : FINISH )
      INTEGER               ID     ( BEGIN : FINISH )
      LOGICAL               HAVE   ( BEGIN : FINISH )

!
!     The array below contains the indexes of the various values
!     associated with the three different times of time vectors
!     that we will be using YMD, YD, CONTIN.
!
      INTEGER               COMPNT ( 8, CONTIN )

!
!     We will be making a local copy of the input format string
!     and the input time.
!
      CHARACTER*(MAXLEN)    MYSTR
      CHARACTER*(MAXLEN)    SUBSTR
      CHARACTER*(MAXLEN)    STRING

      DOUBLE PRECISION      MYET

!
!     The integers below are used to mark substring boundaries.
!
      INTEGER               B
      INTEGER               E
      INTEGER               START
      INTEGER               NTOKNS
      INTEGER               IDENT  (  ROOM   )
      INTEGER               BEG    (  ROOM   )
      INTEGER               END    (  ROOM   )

      CHARACTER*(LOCLEN)    FMT
      INTEGER               WIDTH
      INTEGER               APPND

!
!     Times come in three flavors: TT or TDT, TDB, UTC.  The one used
!     on this particular invocation of TIMOUT is stored in TIMTYP.
!     The routine TTRANS needs to have input and output time vector
!     types.  The one used based upon the input PICTUR is stored
!     in BASTYP.
!
      INTEGER               TIMTYP
      CHARACTER*(8)         YMDFMT
      CHARACTER*(8)         YWFMT
      CHARACTER*(16)        BASTYP
      CHARACTER*(16)        INTYP
      CHARACTER*(16)        TSYS
      CHARACTER*(16)        CAL
      CHARACTER*(32)        ZON

      DOUBLE PRECISION      HOFF
      DOUBLE PRECISION      MOFF
      INTEGER               CALTYP
      INTEGER               LAST

      LOGICAL               OK
      LOGICAL               DOERA
      LOGICAL               DOZONE

!
!     Loop counters and delimiters
!
      INTEGER               I
      INTEGER               J
      INTEGER               PART
      INTEGER               STOPAT
      INTEGER               TRNCAT

!
!     Utility double precision numbers
!
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      FACTOR
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      INCR
      DOUBLE PRECISION      INTMED
      DOUBLE PRECISION      NTVEC  ( 8 )
      DOUBLE PRECISION      PTVEC  ( 8 )
      DOUBLE PRECISION      TEMPD
      DOUBLE PRECISION      TIMPAD
      DOUBLE PRECISION      TVEC   ( 8 )
      DOUBLE PRECISION      VALUE

!
!     The array power is used to assist in the truncation of double
!     precision values.
!
      DOUBLE PRECISION      POWER  ( 0 : 14 )
      DOUBLE PRECISION      X

!
!     calendar variables.
!
      INTEGER               JYEAR
      INTEGER               JMONTH
      INTEGER               JDAY
      INTEGER               JDOY
      INTEGER               GYEAR
      INTEGER               GMONTH
      INTEGER               GDAY
      INTEGER               GDOY
      LOGICAL               GO2JUL

!
!     Character string representations for months and week days.
!
      CHARACTER*(9)         MONTHS ( 12 )
      CHARACTER*(9)         MYMON
      INTEGER               MYLEN
      INTEGER               MONTYP
      INTEGER               MLEN   ( 12 )
      CHARACTER*(9)         WKDAYS (  7 )
      CHARACTER*(9)         MYWKD
      INTEGER               WKLEN  (  7 )
      INTEGER               WKTYP

      INTEGER               INDX

      LOGICAL               UNKNWN
      LOGICAL               MAKING
      LOGICAL               PUMPUP
      LOGICAL               VANISH

!
!     Save everything.
!
      SAVE


!
!     Initial values
!

      DATA                 MONTHS  / 'January',   'February', &
                                     'March',     'April', &
                                     'May',       'June', &
                                     'July',      'August', &
                                     'September', 'October', &
                                     'November',  'December' /

      DATA                 MLEN    /  7,           8, &
                                      5,           5, &
                                      3,           4, &
                                      4,           6, &
                                      9,           7, &
                                      8,           8 /

      DATA                 WKDAYS  / 'Sunday',    'Monday', &
                                     'Tuesday',   'Wednesday', &
                                     'Thursday',  'Friday', &
                                     'Saturday' /

      DATA                 WKLEN   /  6,           6, &
                                      7,           9, &
                                      8,           6, &
                                      8 /

      DATA                 FIRST   / .TRUE. /

      DATA                 POWER   / 1.0D0,  1.0D1,  1.0D2,  1.0D3, &
                                     1.0D4,  1.0D5,  1.0D6,  1.0D7, &
                                     1.0D8,  1.0D9,  1.0D10, 1.0D11, &
                                     1.0D12, 1.0D13, 1.0D14         /

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'TIMOUT' )

!
!     Chapter 1. Initializations.
!     =================================================================
!
!     On the first pass, we need to set up the recognized tokens
!     that will be used for scanning, the classes of these tokens
!     and the array of ID's for time systems.
!
      IF ( FIRST ) THEN

         FIRST    = .FALSE.

         MARKS (  1 ) = 'YYYY'
         MARKS (  2 ) = 'YR'
         MARKS (  3 ) = 'MON'
         MARKS (  4 ) = 'Mon'
         MARKS (  5 ) = 'mon'
         MARKS (  6 ) = 'MONTH'
         MARKS (  7 ) = 'Month'
         MARKS (  8 ) = 'month'
         MARKS (  9 ) = 'MM'
         MARKS ( 10 ) = 'DOY'
         MARKS ( 11 ) = 'WKD'
         MARKS ( 12 ) = 'Wkd'
         MARKS ( 13 ) = 'wkd'
         MARKS ( 14 ) = 'WEEKDAY'
         MARKS ( 15 ) = 'Weekday'
         MARKS ( 16 ) = 'weekday'
         MARKS ( 17 ) = 'DD'
         MARKS ( 18 ) = 'MN'
         MARKS ( 19 ) = 'HR'
         MARKS ( 20 ) = 'SC'
         MARKS ( 21 ) = '.#'
         MARKS ( 22 ) = '#'
         MARKS ( 23 ) = 'JULIAND'
         MARKS ( 24 ) = '::UTC'
         MARKS ( 25 ) = '::TDB'
         MARKS ( 26 ) = '::TDT'
         MARKS ( 27 ) = '::TT'
         MARKS ( 28 ) = 'SP2000'
         MARKS ( 29 ) = 'SP1950'
         MARKS ( 30 ) = '::RND'
         MARKS ( 31 ) = '::TRNC'
         MARKS ( 32 ) = 'ERA'
         MARKS ( 33 ) = 'era'
         MARKS ( 34 ) = 'AMPM'
         MARKS ( 35 ) = 'ampm'
         MARKS ( 36 ) = '::UTC+'
         MARKS ( 37 ) = '::UTC-'
         MARKS ( 38 ) = '::JCAL'
         MARKS ( 39 ) = '::GCAL'
         MARKS ( 40 ) = '::MCAL'
         MARKS ( 41 ) = 'AP'
         MARKS ( 42 ) = '?ERA?'
         MARKS ( 43 ) = '?era?'

         NMARKS       =  43

         CALL SCANPR ( NMARKS, MARKS, MRKLEN, PNTRS )

!
!        Now that we've prepared our recognized substrings and
!        auxiliary arrays for scanning, collect the id's of the
!        various marks and classify the various marks.
!        substrings.
!
         ID ( NONAME ) = 0
         ID ( YEAR   ) = BSRCHC ( 'YYYY',    NMARKS, MARKS )
         ID ( YR     ) = BSRCHC ( 'YR',      NMARKS, MARKS )
         ID ( UMON   ) = BSRCHC ( 'MON',     NMARKS, MARKS )
         ID ( MMON   ) = BSRCHC ( 'Mon',     NMARKS, MARKS )
         ID ( LMON   ) = BSRCHC ( 'mon',     NMARKS, MARKS )
         ID ( UMNTH  ) = BSRCHC ( 'MONTH',   NMARKS, MARKS )
         ID ( MMNTH  ) = BSRCHC ( 'Month',   NMARKS, MARKS )
         ID ( LMNTH  ) = BSRCHC ( 'month',   NMARKS, MARKS )
         ID ( MONTH  ) = BSRCHC ( 'MM',      NMARKS, MARKS )
         ID ( DOY    ) = BSRCHC ( 'DOY',     NMARKS, MARKS )
         ID ( UWKD   ) = BSRCHC ( 'WKD',     NMARKS, MARKS )
         ID ( MWKD   ) = BSRCHC ( 'Wkd',     NMARKS, MARKS )
         ID ( LWKD   ) = BSRCHC ( 'wkd',     NMARKS, MARKS )
         ID ( UWEEKD ) = BSRCHC ( 'WEEKDAY', NMARKS, MARKS )
         ID ( MWEEKD ) = BSRCHC ( 'Weekday', NMARKS, MARKS )
         ID ( LWEEKD ) = BSRCHC ( 'weekday', NMARKS, MARKS )
         ID ( DAY    ) = BSRCHC ( 'DD',      NMARKS, MARKS )
         ID ( MINUTE ) = BSRCHC ( 'MN',      NMARKS, MARKS )
         ID ( HOUR   ) = BSRCHC ( 'HR',      NMARKS, MARKS )
         ID ( SEC    ) = BSRCHC ( 'SC',      NMARKS, MARKS )
         ID ( POINT  ) = BSRCHC ( '.#',      NMARKS, MARKS )
         ID ( PLACE  ) = BSRCHC ( '#',       NMARKS, MARKS )
         ID ( JULIAN ) = BSRCHC ( 'JULIAND', NMARKS, MARKS )
         ID ( UTC    ) = BSRCHC ( '::UTC',   NMARKS, MARKS )
         ID ( TDB    ) = BSRCHC ( '::TDB',   NMARKS, MARKS )
         ID ( TDT    ) = BSRCHC ( '::TDT',   NMARKS, MARKS )
         ID ( TT     ) = BSRCHC ( '::TT',    NMARKS, MARKS )
         ID ( SP2000 ) = BSRCHC ( 'SP2000',  NMARKS, MARKS )
         ID ( SP1950 ) = BSRCHC ( 'SP1950',  NMARKS, MARKS )
         ID ( ROUND  ) = BSRCHC ( '::RND',   NMARKS, MARKS )
         ID ( TRUNC  ) = BSRCHC ( '::TRNC',  NMARKS, MARKS )
         ID ( UERA   ) = BSRCHC ( 'ERA',     NMARKS, MARKS )
         ID ( LERA   ) = BSRCHC ( 'era',     NMARKS, MARKS )
         ID ( UERAX  ) = BSRCHC ( '?ERA?',   NMARKS, MARKS )
         ID ( LERAX  ) = BSRCHC ( '?era?',   NMARKS, MARKS )
         ID ( UAMPM  ) = BSRCHC ( 'AMPM',    NMARKS, MARKS )
         ID ( LAMPM  ) = BSRCHC ( 'ampm',    NMARKS, MARKS )
         ID ( UTCP   ) = BSRCHC ( '::UTC+',  NMARKS, MARKS )
         ID ( UTCM   ) = BSRCHC ( '::UTC-',  NMARKS, MARKS )
         ID ( JCAL   ) = BSRCHC ( '::JCAL',  NMARKS, MARKS )
         ID ( GCAL   ) = BSRCHC ( '::GCAL',  NMARKS, MARKS )
         ID ( MCAL   ) = BSRCHC ( '::MCAL',  NMARKS, MARKS )
         ID ( AMPM   ) = BSRCHC ( 'AP',      NMARKS, MARKS )

         CLASS ( ID(NONAME)  ) =  NONAME
         CLASS ( ID(YEAR)    ) =  YEAR
         CLASS ( ID(YR)      ) =  YR
         CLASS ( ID(UMON)    ) =  MON
         CLASS ( ID(MMON)    ) =  MON
         CLASS ( ID(LMON)    ) =  MON
         CLASS ( ID(UMNTH)   ) =  MON
         CLASS ( ID(MMNTH)   ) =  MON
         CLASS ( ID(LMNTH)   ) =  MON
         CLASS ( ID(MONTH)   ) =  MONTH
         CLASS ( ID(DOY)     ) =  DOY
         CLASS ( ID(UWKD)    ) =  WKDAY
         CLASS ( ID(MWKD)    ) =  WKDAY
         CLASS ( ID(LWKD)    ) =  WKDAY
         CLASS ( ID(UWEEKD)  ) =  WKDAY
         CLASS ( ID(MWEEKD)  ) =  WKDAY
         CLASS ( ID(LWEEKD)  ) =  WKDAY
         CLASS ( ID(DAY)     ) =  DAY
         CLASS ( ID(MINUTE)  ) =  MINUTE
         CLASS ( ID(HOUR)    ) =  HOUR
         CLASS ( ID(SEC)     ) =  SEC
         CLASS ( ID(POINT)   ) =  POINT
         CLASS ( ID(PLACE)   ) =  PLACE
         CLASS ( ID(JULIAN)  ) =  JULIAN
         CLASS ( ID(UTC)     ) =  TIMSYS
         CLASS ( ID(TDB)     ) =  TIMSYS
         CLASS ( ID(TDT)     ) =  TIMSYS
         CLASS ( ID(TT)      ) =  TIMSYS
         CLASS ( ID(SP2000)  ) =  SP2000
         CLASS ( ID(SP1950)  ) =  SP1950
         CLASS ( ID(ROUND)   ) =  ROUND
         CLASS ( ID(TRUNC)   ) =  TRUNC
         CLASS ( ID(UERA)    ) =  ERA
         CLASS ( ID(LERA)    ) =  ERA
         CLASS ( ID(UERAX)   ) =  ERA
         CLASS ( ID(LERAX)   ) =  ERA
         CLASS ( ID(UAMPM)   ) =  NOON
         CLASS ( ID(LAMPM)   ) =  NOON
         CLASS ( ID(UTCP)    ) =  TIMSYS
         CLASS ( ID(UTCM)    ) =  TIMSYS
         CLASS ( ID(JCAL)    ) =  CALNDR
         CLASS ( ID(GCAL)    ) =  CALNDR
         CLASS ( ID(MCAL)    ) =  CALNDR
         CLASS ( ID(AMPM)    ) =  AMPM

         DO I = BEGIN, FINISH
            PAD (I) = 0.0D0
         END DO

         PAD  ( SEC    )     =   0.5D0
         PAD  ( MINUTE )     =  60.0D0 * PAD(SEC)
         PAD  ( HOUR   )     =  60.0D0 * PAD(MINUTE)
         PAD  ( DAY    )     =  24.0D0 * PAD(HOUR)
         PAD  ( MONTH  )     =  30.0D0 * PAD(DAY)
         PAD  ( DOY    )     =           PAD(DAY)
         PAD  ( MON    )     =           PAD(MONTH)
         PAD  ( YEAR   )     = 365.0D0 * PAD(DAY)
         PAD  ( YR     )     = 365.0D0 * PAD(DAY)
         PAD  ( JULIAN )     =           PAD(DAY)
         PAD  ( SP2000 )     =           PAD(SEC)
         PAD  ( SP1950 )     =           PAD(SEC)
         PAD  ( AMPM   )     =           PAD(HOUR)

!
!        After we've made the initial scan for tokens and determined
!        the time system requested, we will want to get rid of the
!        time system tokens.
!
         DUMP (  1 ) = ID(UTC)
         DUMP (  2 ) = ID(TDT)
         DUMP (  3 ) = ID(TT)
         DUMP (  4 ) = ID(TDB)
         DUMP (  5 ) = ID(ROUND)
         DUMP (  6 ) = ID(TRUNC)
         DUMP (  7 ) = ID(UTCM)
         DUMP (  8 ) = ID(UTCP)
         DUMP (  9 ) = ID(JCAL)
         DUMP ( 10 ) = ID(GCAL)
         DUMP ( 11 ) = ID(MCAL)

         NDUMP      = 11

!
!        Set up the default formats for the various time components
!
         ORIGNL(YEAR)   = 'YYYY'
         LENGTH(YEAR)   =  4

         ORIGNL(YR)     = '0Y'
         LENGTH(YR)     =  2

         ORIGNL(DOY)    = '0DD'
         LENGTH(DOY)    =  3

         ORIGNL(DAY)    = '0D'
         LENGTH(DAY)    =  2

         ORIGNL(MONTH)  = '0M'
         LENGTH(MONTH)  =  2

         ORIGNL(HOUR)   = '0H'
         LENGTH(HOUR)   =  2

         ORIGNL(AMPM)   = '0H'
         LENGTH(AMPM)   =  2

         ORIGNL(MINUTE) = '0M'
         LENGTH(MINUTE) =  2

         ORIGNL(SEC)    = '0S'
         LENGTH(SEC)    =  2

         ORIGNL(JULIAN) = 'XXXXXXX'
         LENGTH(JULIAN) =  7

         ORIGNL(SP2000) = 'XXXXXXXXXXX'
         LENGTH(SP2000) =  11

         ORIGNL(SP1950) = 'XXXXXXXXXXX'
         LENGTH(SP1950) =  11

!
!        Finally set up the component pointers...
!
         COMPNT(1,YMD)    = RLYEAR
         COMPNT(2,YMD)    = MONTH
         COMPNT(3,YMD)    = DAY
         COMPNT(4,YMD)    = HOUR
         COMPNT(5,YMD)    = MINUTE
         COMPNT(6,YMD)    = SEC

         COMPNT(1,CONTIN) = CURRNT

      END IF

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
      MYSTR = ' '
      CALL    LJUST ( PICTUR, MYSTR(1:MAXLEN-1) )
      E     = RTRIM (         MYSTR             ) + 1
      START = 1

!
!     Scan the input string.
!
      CALL SCAN ( MYSTR(1:E), &
                  MARKS,      MRKLEN, PNTRS,  ROOM,  START, &
                  NTOKNS,     IDENT,  BEG,    END           )

!
!     Locate the time system that will be used.  This must
!     be one of the following: UTC, TDB, TT, TDT
!
      UNKNWN = .TRUE.
      GO2JUL = .FALSE.
      DOZONE = .FALSE.
      I      = 1
      HOFF   = 0.0D0
      MOFF   = 0.0D0

!
!     Get the default time type from TIMDEF
!
      CALL TIMDEF ( 'GET', 'SYSTEM', TSYS )

      IF ( TSYS .EQ. 'UTC' ) THEN
         TIMTYP = ID(UTC)
      ELSE IF ( TSYS .EQ. 'TDB' ) THEN
         TIMTYP = ID(TDB)
      ELSE IF ( (TSYS .EQ. 'TDT') .OR. (TSYS .EQ. 'TT') ) THEN
         TIMTYP = ID(TDT)
      ELSE
         TIMTYP = ID(UTCP)

         CALL TIMDEF ( 'GET', 'ZONE', ZON )

         CALL PREFIX ( '::', 0, ZON )
         CALL ZZUTCPM( ZON, 1, HOFF, MOFF, LAST, OK )

         DOZONE = OK

!
!        The routine TIMDEF uses ZZUTCPM to determine whether
!        or not a time zone is legitimate before it stores it
!        to be "GOTTEN."  As a result the value of OK should
!        always be .TRUE.  However, just in case TIMDEF should
!        someday use something other that ZZUTCPM for checking
!        we put in the unneeded check below.
!
         IF ( .NOT. OK ) THEN
            TIMTYP = ID(UTC)
         END IF

      END IF


      DO WHILE ( UNKNWN .AND. I .LE. NTOKNS )

         IF ( CLASS(IDENT(I)) .EQ. TIMSYS ) THEN
            TIMTYP = IDENT(I)
            UNKNWN = .FALSE.
            DOZONE = .FALSE.

            IF(      IDENT(I) .EQ. ID(UTCP) &
                .OR. IDENT(I) .EQ. ID(UTCM) ) THEN

!
!              We've got a time zone specification. Parse it and
!              store the offsets from UTC.
!
               CALL ZZUTCPM ( MYSTR, BEG(I), HOFF, MOFF, LAST, OK )

               IF ( OK ) THEN

                  DOZONE = .TRUE.
                  TIMTYP = ID(UTCP)

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
                  IF ( LAST .EQ. END(I+1) ) THEN
                     IDENT(I+1) = IDENT(I)
                     I          = I + 1
                  ELSE
                     END(I)     = LAST
                     BEG(I+1)   = LAST + 1
                  END IF

               END IF

            END IF


         END IF

         I = I + 1

      END DO

!
!     Determine whether we should use the Julian or gregorian (default)
!     calendar
!
      UNKNWN = .TRUE.
      I      = 1

!
!     Get the default calendar from TIMDEF.
!
      CALL TIMDEF ( 'GET', 'CALENDAR', CAL )

      IF      ( CAL .EQ. 'GREGORIAN' ) THEN
         CALTYP = ID(GCAL)
      ELSE IF ( CAL .EQ. 'JULIAN'    ) THEN
         CALTYP = ID(JCAL)
      ELSE
         CALTYP = ID(MCAL)
      END IF

      DO WHILE ( UNKNWN .AND. I .LE. NTOKNS )

         IF ( CLASS(IDENT(I)) .EQ. CALNDR ) THEN
            CALTYP = IDENT(I)
            UNKNWN = .FALSE.
         END IF

         I = I + 1
      END DO


!
!     Next determine whether or not we shall be performing rounding
!     on output.
!
      PUMPUP =      ISRCHI ( ID(ROUND), NTOKNS, IDENT ) .NE. 0

!
!     Determine if we have an Era specification
!
      DOERA  =      ISRCHI ( ID(LERA),  NTOKNS, IDENT ) .NE. 0 &
               .OR. ISRCHI ( ID(UERA),  NTOKNS, IDENT ) .NE. 0 &
               .OR. ISRCHI ( ID(UERAX), NTOKNS, IDENT ) .NE. 0 &
               .OR. ISRCHI ( ID(LERAX), NTOKNS, IDENT ) .NE. 0

!
!     Until we've examined the year, we assume that the era is not
!     supposed to vanish.
!
      VANISH = .FALSE.
!
!     Next remove all of the time system dudes from the list of
!     tokens.
!
      CALL SCANRJ ( DUMP, NDUMP, NTOKNS, IDENT, BEG, END )


!
!     If the user wants to round the output, we need to pump up ET
!     by the smallest significant part of the input picture.  But
!     in either case we are going to pad the input time.  For now
!     we pad it by zero.
!
      TIMPAD = 0.0D0

      IF ( PUMPUP ) THEN

!
!        We need to determine the amount to pad ET by.  So we need
!        to look at the string and find the least significant component
!        that has been requested.  Keep in mind that the last token
!        is of type NONAME (its a blank) by construction.
!
         I = 1

         DO WHILE ( I .LE. NTOKNS )

            TYPE = CLASS ( IDENT(I) )

            IF   (      ( TYPE .EQ. NONAME ) &
                   .OR. ( TYPE .EQ. POINT  ) &
                   .OR. ( TYPE .EQ. PLACE  ) &
                   .OR. ( TYPE .EQ. NOON   ) &
                   .OR. ( TYPE .EQ. ERA    ) &
                   .OR. ( TYPE .EQ. MON    ) &
                   .OR. ( TYPE .EQ. WKDAY  ) ) THEN

!
!              Don't do anything, just go on to the next token.
!
               I = I + 1

            ELSE

!
!              Look up the amount we should pad our time by.
!
               FACTOR = 1.0D0
               INCR   = PAD ( TYPE )

!
!              Examine the next token.  If it's not a decimal point
!              and marker, we have the least significant part of
!              this component.
!
               I      = I + 1
               TYPE   = CLASS(IDENT(I))

               IF ( TYPE .EQ. POINT ) THEN

                  FACTOR = FACTOR * 0.1D0
                  I      = I + 1

!
!                 Now just look for the end of the string of place
!                 holders
!
                  DO WHILE ( IDENT(I) .EQ. ID(PLACE) )
                     FACTOR = FACTOR * 0.1D0
                     I      = I + 1
                  END DO

               END IF

!
!              Now compute the time pad for this component of the
!              time string.
!
               INCR = INCR * FACTOR

               IF ( TIMPAD .NE. 0.0D0 ) THEN
                  TIMPAD = MIN ( TIMPAD, INCR )
               ELSE
                  TIMPAD = INCR
               END IF

            END IF

         END DO

      END IF

!
!     Right now we don't have any components of the time format
!     and we don't need any of them so far.
!
      DO PART = BEGIN, FINISH
         HAVE(PART) = .FALSE.
      END DO


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
      MYET = ET

      IF      ( TIMTYP .EQ. ID(TDB) ) THEN

!
!        Since we are likely to need SP2000, SP1950 or JD, we
!        compute them now.
!
         MYET           = MYET + TIMPAD
         VALUES(SP2000) = MYET
         VALUES(JULIAN) = UNITIM( MYET,   'TDB',  'JDTDB'  )
         VALUES(SP1950) = VALUES(SP2000) + SPD()*( J2000() - J1950() )

         BASTYP         = 'FORMAL'
         YMDFMT         = 'YMDF'
         YWFMT          = 'YMWDF'
         HAVE  (SP2000) = .TRUE.
         HAVE  (SP1950) = .TRUE.
         HAVE  (JULIAN) = .TRUE.

      ELSE IF ( (TIMTYP .EQ. ID(TDT)) .OR. &
                (TIMTYP .EQ. ID(TT)) ) THEN

         MYET           = UNITIM( MYET,   'TDB',  'TDT'    )  + TIMPAD
         VALUES(SP2000) = MYET
         VALUES(JULIAN) = UNITIM( MYET,   'TDT',  'JDTDT'  )
         VALUES(SP1950) = VALUES(SP2000) + SPD()*( J2000() - J1950() )

         BASTYP         = 'FORMAL'
         YMDFMT         = 'YMDF'
         YWFMT          = 'YMWDF'
         HAVE  (SP2000) = .TRUE.
         HAVE  (SP1950) = .TRUE.
         HAVE  (JULIAN) = .TRUE.

      ELSE

!
!        In this case we convert to an earth based frame for our
!        working epoch.  This rounds properly when it's time to get
!        fractional components.
!
         MYET   =  UNITIM ( MYET, 'TDB', 'TDT' ) + TIMPAD
         BASTYP = 'TDT'
         YMDFMT = 'YMD'
         YWFMT  = 'YMWD'

      END IF

!
!     Chapter 3.  Building the Output String
!     ==================================================================
!

!
!     Now we are ready to go, we need to fetch the tokens
!     and construct the output string.  We will
!     put the next portion of the output at APPND
!
      APPND  =  1
      MAKING = .FALSE.

      DO I = 1, NTOKNS

         TYPE    = CLASS( IDENT(I) )
         TVEC(1) = MYET

!
!        If the next marker is not one we use as a place holder
!        in the fractional part of decimal formats AND we
!        are in the process of building a format, then the format
!        building is done.  We can construct the component and
!        append it to the string we are building.
!
         IF ( (TYPE .NE. PLACE) .AND.  MAKING ) THEN

!
!           We also need to be sure this isn't a decimal point
!           marker before we add on to the output string.
!
            IF ( (TYPE .NE. POINT) .OR. HAVE(POINT) ) THEN

!
!              We are going to truncate the number to the number of
!              places requested NOT round.
!
               TRNCAT = BRCKTI( WIDTH - LENGTH(NUMTYP) - 1, 0, 14 )
               FRAC   = VALUE - DINT(VALUE )

               IF ( FRAC .LT. 0 ) THEN
                  VALUE = VALUE - 1.0D0
                  FRAC  = FRAC  + 1.0D0
               END IF

               INTMED = ( DINT( FRAC * POWER(TRNCAT) ) - 0.125D0 ) &
                                      / POWER(TRNCAT)

               FRAC   = BRCKTD( INTMED,      0.0D0,      1.0D0 )

               VALUE  = DINT(VALUE) + FRAC

               CALL DPFMT ( VALUE, FMT, SUBSTR )


               STRING(APPND:) = SUBSTR
               APPND          = APPND + WIDTH
               HAVE  (POINT)  =.FALSE.
               MAKING         =.FALSE.

            END IF

         END IF

!
!        If the token isn't recognized we can just
!        append it to the string we are constructing and
!        adjust the point at which the next substring is
!        to be appended.
!
         IF ( TYPE .EQ. NONAME ) THEN

            STRING(APPND:) = MYSTR ( BEG(I) : END(I) )
            APPND          = APPND - BEG(I) + END(I) + 1

!
!        If the token is a place holder, we either just append it
!        or tack it on to a format string we are creating..
!
         ELSE IF ( TYPE .EQ. PLACE ) THEN


            IF ( MAKING ) THEN
               B           = WIDTH + 1
               E           = B     - BEG(I) + END(I)
               FMT(B:E)    = MYSTR ( BEG(I) : END(I) )
               WIDTH       = WIDTH - BEG(I) + END(I) + 1
            ELSE
               STRING(APPND:APPND) = MYSTR ( BEG(I) : END(I) )
               APPND               = APPND - BEG(I) + END(I) + 1
            END IF

!
!        If the token is the decimal point plus place holder
!        AND we are making a format, we append it to the current
!        format and determine the fractional part of the current
!        quantity.
!
         ELSE IF ( TYPE .EQ. POINT ) THEN

            IF ( .NOT. MAKING ) THEN

               B           =  APPND
               E           =  APPND - BEG(I) + END(I)
               STRING(B:E) =  MYSTR ( BEG(I) : END(I) )
               APPND       =  E + 1
               HAVE(POINT) = .FALSE.

            ELSE IF ( TIMFMT .EQ. CONTIN ) THEN

               B           = WIDTH + 1
               E           = B     - BEG(I) + END(I)
               FMT(B:E)    = MYSTR ( BEG(I) : END(I) )
               WIDTH       = E
               HAVE(POINT) =.TRUE.

            ELSE

               B           = WIDTH + 1
               E           = B     - BEG(I) + END(I)
               FMT(B:E)    = MYSTR ( BEG(I) : END(I) )
               WIDTH       = E
               HAVE(POINT) =.TRUE.

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

               CALL TTRANS( INTYP, BASTYP, PTVEC  )
               CALL TTRANS( INTYP, BASTYP, NTVEC  )

               DELTA = MAX(1.0D0, NTVEC(1) - PTVEC(1) )
               FRAC  = BRCKTD( 0.0D0, 1.0D0, (MYET - PTVEC(1)) / DELTA )

               VALUE = VALUE + FRAC

            END IF

         ELSE

!
!        If we get to this point we have an honest time
!        string component to fetch.  We might already have
!        this guy.  If so we can just collect him from the
!        values buffer (although this collection is performed
!        after the next long IF-THEN block that gets the value
!        if we don't already have it).
!
            MAKING      =.TRUE.
            HAVE(POINT) =.FALSE.
            FMT         = ORIGNL( TYPE )
            WIDTH       = LENGTH( TYPE )
            NUMTYP      =         TYPE

            IF ( .NOT. HAVE(TYPE) )  THEN

               TVEC(1) = MYET

!
!              Most components are handled in the next block.
!
               IF (       (TYPE .EQ. YEAR) &
                     .OR. (TYPE .EQ. YR) &
                     .OR. (TYPE .EQ. MONTH) &
                     .OR. (TYPE .EQ. MON) &
                     .OR. (TYPE .EQ. DAY) &
                     .OR. (TYPE .EQ. DOY) &
                     .OR. (TYPE .EQ. NOON) &
                     .OR. (TYPE .EQ. HOUR) &
                     .OR. (TYPE .EQ. ERA) &
                     .OR. (TYPE .EQ. AMPM) &
                     .OR. (TYPE .EQ. MINUTE) &
                     .OR. (TYPE .EQ. SEC)   ) THEN


                  CALL TTRANS ( BASTYP, YMDFMT, TVEC )

!
!                 The seconds component is finished.  Regardless
!                 of any zone or calendar modifications, we just
!                 don't have to deal with this component any more.
!
                  VALUES(SEC)    = TVEC(6)

!
!                 If we need to deal with time zones, this is
!                 the time to do it.
!
                  IF ( TIMTYP .EQ. ID(UTCP) ) THEN

                     TVEC(4) = TVEC(4) + HOFF
                     TVEC(5) = TVEC(5) + MOFF
                     TVEC(6) = 0.0D0

                     CALL TTRANS ( 'YMDF', 'YMDF', TVEC )

                  END IF

!
!                 One way or the other the hours and minutes components
!                 are finished.  Record their values.
!
                  VALUES(HOUR)   = TVEC(4)
                  VALUES(MINUTE) = TVEC(5)

                  IF ( VALUES(HOUR) .EQ. 0.0D0 ) THEN
                     VALUES(AMPM) =  12.0D0
                  ELSE IF ( VALUES(HOUR) .GT. 12.0D0 ) THEN
                     VALUES(AMPM) = VALUES(HOUR) - 12.0D0
                  ELSE
                     VALUES(AMPM) = VALUES(HOUR)
                  END IF

!
!                 Finally, if we need to change the calendar to
!                 Julian this is the place to handle it.
!
                  JYEAR  = NINT( TVEC(1) )
                  JMONTH = NINT( TVEC(2) )
                  JDAY   = NINT( TVEC(3) )

                  CALL GR2JUL ( JYEAR, JMONTH, JDAY, JDOY )

                  GYEAR  = JYEAR
                  GMONTH = JMONTH
                  GDAY   = JDAY

                  CALL JUL2GR ( GYEAR, GMONTH, GDAY, GDOY )

                  IF      ( CALTYP .EQ. ID(GCAL) ) THEN

                     VALUES(YEAR)  = DBLE(GYEAR)
                     VALUES(MONTH) = DBLE(GMONTH)
                     VALUES(DAY)   = DBLE(GDAY)
                     VALUES(DOY)   = DBLE(GDOY)
                     GO2JUL        = .FALSE.

                  ELSE IF ( CALTYP .EQ. ID(JCAL) ) THEN

                     VALUES(YEAR)  = DBLE(JYEAR)
                     VALUES(MONTH) = DBLE(JMONTH)
                     VALUES(DAY)   = DBLE(JDAY)
                     VALUES(DOY)   = DBLE(JDOY)
                     GO2JUL        = .TRUE.

                  ELSE IF ( CALTYP .EQ. ID(MCAL) ) THEN


                     IF      ( GYEAR .LT. 1582 ) THEN
                        GO2JUL = .TRUE.
                     ELSE IF ( GYEAR .GT. 1582 ) THEN
                        GO2JUL = .FALSE.
                     ELSE IF ( GMONTH .LT. 10 ) THEN
                        GO2JUL = .TRUE.
                     ELSE IF ( GMONTH .GT. 10 ) THEN
                        GO2JUL = .FALSE.
                     ELSE IF ( GDAY   .GE. 15 ) THEN
                        GO2JUL = .FALSE.
                     ELSE
                        GO2JUL = .TRUE.
                     END IF

                     IF ( GO2JUL ) THEN
                        VALUES(YEAR)  = DBLE(JYEAR)
                        VALUES(MONTH) = DBLE(JMONTH)
                        VALUES(DAY)   = DBLE(JDAY)
                        VALUES(DOY)   = DBLE(JDOY)
                     ELSE
                        VALUES(YEAR)  = DBLE(GYEAR)
                        VALUES(MONTH) = DBLE(GMONTH)
                        VALUES(DAY)   = DBLE(GDAY)
                        VALUES(DOY)   = DBLE(GDOY)
                     END IF

                  END IF

!
!                 Determine the era associated with the epoch.  Also
!                 if the year component is negative, we handle  that
!                 now.
!
!                 We store the actual value of the year so that
!                 it can be used when determining rounding of
!                 other components.
!
                  VALUES(RLYEAR) = VALUES(YEAR)

                  IF ( DOERA ) THEN

                     IF ( VALUES(YEAR) .LT. 1 ) THEN
                        VALUES(YEAR) = 1.0D0 - VALUES(YEAR)
                        VALUES(ERA)  = 1.0D0
                     ELSE
                        VALUES(ERA)  = 2.0D0
                     END IF

                     VANISH = VALUES(YEAR) .GE. 1000.0D0

                  END IF

!
!                 Fetch the last two digits of the year.
!
                  CALL RMAIND ( VALUES(YEAR), 100.0D0, X, TEMPD )
                  VALUES(YR) = TEMPD

                  HAVE  (YEAR)   = .TRUE.
                  HAVE  (YR)     = .TRUE.
                  HAVE  (DOY)    = .TRUE.
                  HAVE  (MONTH)  = .TRUE.
                  HAVE  (MON)    = .TRUE.
                  HAVE  (DAY)    = .TRUE.
                  HAVE  (HOUR)   = .TRUE.
                  HAVE  (MINUTE) = .TRUE.
                  HAVE  (SEC)    = .TRUE.
                  HAVE  (AMPM)   = .TRUE.
                  HAVE  (ERA)    = .TRUE.


               ELSE IF ( TYPE .EQ. WKDAY ) THEN

                  TVEC(1) = MYET

                  CALL TTRANS ( BASTYP, YWFMT, TVEC )

!
!                 Weekday. If we need to deal with time zones, this is
!                 the time to do it.
!
                  IF ( TIMTYP .EQ. ID(UTCP) ) THEN

                     TVEC(5) = TVEC(5) + HOFF
                     TVEC(6) = TVEC(6) + MOFF
                     TVEC(7) = 0.0D0

                     CALL TTRANS ( 'YMWDF', 'YMWDF', TVEC )


                  END IF

                  VALUES(WKDAY)  = TVEC(4)
                  HAVE  (WKDAY)  =.TRUE.

               ELSE IF (      ( TYPE .EQ. SP1950 ) &
                         .OR. ( TYPE .EQ. SP2000 ) ) THEN

!
!                 The only way to get here is if the output time
!                 type is UTC or a time zone (otherwise we'd
!                 already HAVE SP2000 and SP1950).
!
                  TVEC(1) = MYET

                  CALL TTRANS ( BASTYP, 'FORMAL', TVEC )

                  VALUES(SP2000) = TVEC(1)
                  VALUES(SP1950) = VALUES(SP2000) &
                                 + SPD() * ( J2000() - J1950() )

                  HAVE  (SP2000) = .TRUE.
                  HAVE  (SP1950) = .TRUE.

               ELSE IF ( TYPE .EQ. JULIAN ) THEN

!
!                 The same tale can be told here as in the last
!                 case.  We can only get here if this is UTC
!                 output.
!
                  TVEC(1) = MYET

                  CALL TTRANS ( BASTYP, 'JDUTC', TVEC )

                  VALUES(JULIAN) = TVEC(1)
                  HAVE  (JULIAN) =.TRUE.

               END IF


            END IF

!
!           O.K. whatever thing we are about to construct, we now
!           have it's numeric value.  It's time to construct its
!           string  value.
!
!
!           We need to treat character months, weekdays, eras, a.m.'s
!           and p.m.'s specially.
!
            IF ( TYPE .EQ. MON ) THEN

               INDX           = NINT  ( VALUES(MONTH) )
               MYMON          = MONTHS( INDX )
               MONTYP         = IDENT ( I )

!
!              There is no ELSE case in the block below because all of
!              the possible MONTYP values are checked explicitly.
!
               IF      ( MONTYP .EQ. ID(UMON)  ) THEN
                  CALL UCASE ( MYMON, MYMON )
                  MYMON(4:) = ' '
                  MYLEN     = 3
               ELSE IF ( MONTYP .EQ. ID(MMON)   ) THEN
                  MYMON(4:) = ' '
                  MYLEN     = 3
               ELSE IF ( MONTYP .EQ. ID(LMON)   ) THEN
                  CALL LCASE ( MYMON, MYMON )
                  MYMON(4:) = ' '
                  MYLEN     = 3
               ELSE IF ( MONTYP .EQ. ID(MMNTH) ) THEN
                  MYLEN     = MLEN(INDX)
               ELSE IF ( MONTYP .EQ. ID(UMNTH) ) THEN
                  CALL UCASE ( MYMON, MYMON )
                  MYLEN     = MLEN(INDX)
               ELSE IF ( MONTYP .EQ. ID(LMNTH) ) THEN
                  CALL LCASE ( MYMON, MYMON )
                  MYLEN     = MLEN(INDX)
               END IF

               STRING(APPND:) = MYMON
               APPND          = APPND + MYLEN
               MAKING         =.FALSE.

            ELSE IF ( TYPE .EQ. WKDAY ) THEN

               INDX           = NINT  ( VALUES(WKDAY) )
               MYWKD          = WKDAYS( INDX )
               WKTYP          = IDENT ( I )

!
!              There is no ELSE case in the block below because all of
!              the possible WKTYP values are checked explicitly.
!
               IF      ( WKTYP .EQ. ID(UWKD)   ) THEN
                  CALL UCASE ( MYWKD, MYWKD )
                  MYWKD(4:) = ' '
                  MYLEN     = 3
               ELSE IF ( WKTYP .EQ. ID(MWKD)   ) THEN
                  MYWKD(4:) = ' '
                  MYLEN     = 3
               ELSE IF ( WKTYP .EQ. ID(LWKD)   ) THEN
                  CALL LCASE ( MYWKD, MYWKD )
                  MYWKD(4:) = ' '
                  MYLEN     = 3
               ELSE IF ( WKTYP .EQ. ID(MWEEKD) ) THEN
                  MYLEN     = WKLEN(INDX)
               ELSE IF ( WKTYP .EQ. ID(UWEEKD) ) THEN
                  CALL UCASE ( MYWKD, MYWKD )
                  MYLEN     = WKLEN(INDX)
               ELSE IF ( WKTYP .EQ. ID(LWEEKD) ) THEN
                  CALL LCASE ( MYWKD, MYWKD )
                  MYLEN     = WKLEN(INDX)
               END IF

               STRING(APPND:) = MYWKD
               APPND          = APPND + MYLEN
               MAKING         =.FALSE.


            ELSE IF ( TYPE .EQ. ERA ) THEN

               IF      (         VALUES(ERA)    .EQ. 2.0D0 &
                         .AND. (      IDENT(I) .EQ. ID(UERA) &
                                 .OR. IDENT(I) .EQ. ID(UERAX) ) ) THEN

                  STRING(APPND:) = ' A.D.'

               ELSE IF (         VALUES(ERA)   .EQ. 2.0D0 &
                         .AND. (      IDENT(I) .EQ. ID(LERA) &
                                 .OR. IDENT(I) .EQ. ID(LERAX) ) ) THEN
                   STRING(APPND:) = ' a.d.'

               ELSE IF  (         VALUES(ERA)  .EQ. 1.0D0 &
                         .AND. (      IDENT(I) .EQ. ID(UERA) &
                                 .OR. IDENT(I) .EQ. ID(UERAX) ) ) THEN

                  STRING(APPND:) = ' B.C.'

               ELSE

                  STRING(APPND:) = ' b.c.'

               END IF
!
!              If we have the vanishing kind of era, and we've
!              determined that it needs to vanish, then blank out the
!              portion of the string we just filled in. and don't
!              increment the place holder.
!
               IF (     IDENT(I) .EQ. ID(UERAX) &
                   .OR. IDENT(I) .EQ. ID(LERAX)) THEN

                  IF ( VANISH ) THEN
                     STRING(APPND:) = ' '
                     APPND = APPND + 1
                  ELSE
                     APPND = APPND + 6
                  END IF

               ELSE
                  CALL LJUST ( STRING(APPND:), STRING(APPND:) )
                  APPND          = APPND + 4
               END IF

               MAKING         =.FALSE.

            ELSE IF ( TYPE .EQ. NOON ) THEN


               IF      (       IDENT(I)   .EQ. ID(UAMPM) &
                         .AND. VALUES(HOUR) .GE. 12.0D0    ) THEN

                  STRING(APPND:) = 'P.M.'

               ELSE IF (       IDENT(I)    .EQ. ID(UAMPM) &
                         .AND. VALUES(HOUR) .LT. 12.0D0    ) THEN

                  STRING(APPND:) = 'A.M.'

               ELSE IF (       IDENT(I)    .EQ. ID(LAMPM) &
                         .AND. VALUES(HOUR) .GE. 12.0D0    ) THEN

                  STRING(APPND:) = 'p.m.'

               ELSE

                  STRING(APPND:) = 'a.m.'

               END IF

               APPND          = APPND + 4
               MAKING         =.FALSE.

            ELSE

               VALUE = VALUES( TYPE )

            END IF

!
!           If we are now creating a format string, we should
!           construct the previous time representation and
!           the next for this component (just in case we need it
!           later).
!
            IF ( MAKING ) THEN

!
!              We store the value of our current type in the
!              CURRNT slot of the values array.  This value
!              is used by the single numeric types, JD, SP2000,
!              and SP1950.
!
               VALUES(CURRNT) = VALUES(TYPE)

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
               DO J = 1, 7
                  PTVEC(J) = 0.0D0
                  NTVEC(J) = 0.0D0
               END DO


               IF      ( TYPE .EQ. YEAR  .OR. TYPE .EQ. YR ) THEN

                  STOPAT = 1
                  TIMFMT = YMD
                  INTYP  = YMDFMT
                  INCR   = 1.0D0

               ELSE IF ( TYPE .EQ. MONTH) THEN

                  STOPAT = 2
                  TIMFMT = YMD
                  INTYP  = YMDFMT
                  INCR   = 1.0D0

               ELSE IF ( TYPE .EQ. DAY .OR. TYPE .EQ. DOY ) THEN

                  STOPAT = 3
                  TIMFMT = YMD
                  INTYP  = YMDFMT
                  INCR   = 1.0D0


               ELSE IF ( TYPE .EQ. HOUR .OR. TYPE .EQ. AMPM ) THEN

!
!                 Note that in this case (and the next 2) that if we
!                 an HOUR component, we had to get it either from
!                 a Day of Year format or from a Year Month Day
!                 format. Thus we have all of the more significant
!                 components for this format.
!
                  STOPAT = 4
                  TIMFMT = YMD
                  INTYP  = YMDFMT
                  INCR   = 1.0D0

               ELSE IF ( TYPE .EQ. MINUTE  ) THEN

                  STOPAT = 5
                  TIMFMT = YMD
                  INTYP  = YMDFMT

                  INCR   = 1.0D0

               ELSE IF ( TYPE .EQ. SEC  ) THEN

                  STOPAT = 6
                  TIMFMT = YMD
                  INTYP  = YMDFMT
                  INCR   = 0.0D0


               ELSE IF ( TYPE .EQ. JULIAN ) THEN

                  STOPAT = 1
                  TIMFMT = CONTIN
                  INCR   = 0.0D0

                  IF      ( (TIMTYP .EQ. ID(TDT)) .OR. &
                            (TIMTYP .EQ. ID(TT))) THEN
                     INTYP = 'JDTDT'
                  ELSE IF ( TIMTYP .EQ. ID(TDB) ) THEN
                     INTYP = 'JDTDB'
                  ELSE IF (      TIMTYP .EQ. ID(UTC) &
                            .OR. TIMTYP .EQ. ID(UTCP) ) THEN
                     INTYP = 'JDUTC'
                  END IF

               ELSE

!
!                 The only types left are the continuous (numeric)
!                 types.
!
                  STOPAT = 1
                  TIMFMT = CONTIN
                  INCR   = 0.0D0
                  INTYP  = BASTYP


               END IF

!
!              Ok.  We are now ready to construct the previous
!              and next time vectors.
!
               DO J = 1, STOPAT
                  PTVEC(J) = VALUES(COMPNT(J,TIMFMT))
                  NTVEC(J) = PTVEC (J)
               END DO

               NTVEC(STOPAT) = NTVEC(STOPAT) + INCR

!
!              If the type is a year or month, then we need to set
!              the month to 1, so that we will be working with
!              beginnings of years not beginning of last months of
!              the previous year.
!
               IF ( TYPE .EQ. YEAR  .OR. TYPE .EQ. YR ) THEN
                  PTVEC(2) = 1.0D0
                  NTVEC(2) = 1.0D0
                  PTVEC(3) = 1.0D0
                  NTVEC(3) = 1.0D0
               ELSE IF ( TYPE .EQ. MONTH ) THEN
                  PTVEC(3) = 1.0D0
                  NTVEC(3) = 1.0D0
               END IF

               IF ( GO2JUL .AND. TIMFMT .NE. CONTIN ) THEN

!
!                 Convert both PTVEC and NTVEC to the gregorian
!                 calendar
!
                  JYEAR  = NINT(PTVEC(1))
                  JMONTH = NINT(PTVEC(2))
                  JDAY   = NINT(PTVEC(3))

                  CALL JUL2GR ( JYEAR, JMONTH, JDAY, JDOY )

                  PTVEC(1) = DBLE(JYEAR)
                  PTVEC(2) = DBLE(JMONTH)
                  PTVEC(3) = DBLE(JDAY)

                  JYEAR  = NINT(NTVEC(1))
                  JMONTH = NINT(NTVEC(2))
                  JDAY   = NINT(NTVEC(3))

                  CALL JUL2GR ( JYEAR, JMONTH, JDAY, JDOY )

                  NTVEC(1) = DBLE(JYEAR)
                  NTVEC(2) = DBLE(JMONTH)
                  NTVEC(3) = DBLE(JDAY)

               END IF

!
!              Handle the +/- time zone shifts.
!
               IF ( DOZONE .AND. (TIMFMT .NE. CONTIN) ) THEN

                  PTVEC(4) = PTVEC(4) - HOFF
                  NTVEC(4) = NTVEC(4) - HOFF

                  PTVEC(5) = PTVEC(5) - MOFF
                  NTVEC(5) = NTVEC(5) - MOFF

                  PTVEC(6) = 0.0D0
                  NTVEC(6) = 0.0D0

                  CALL TTRANS ( 'YMDF', 'YMDF', PTVEC )
                  CALL TTRANS ( 'YMDF', 'YMDF', NTVEC )

                  IF ( TYPE .EQ. SEC ) THEN
                     PTVEC(6) = VALUES(SEC)
                     NTVEC(6) = VALUES(SEC)
                  END IF

               END IF


            END IF

         END IF

      END DO

!
!     All that's left to do is to copy the constructed string
!     to the output string.
!
      OUTPUT = STRING


      CALL CHKOUT ( 'TIMOUT' )
      RETURN
      END
