!$Procedure      M2CAL ( Parse a UTC time string )

      SUBROUTINE M2CAL ( UTCSTR, MSSG, TCODE )
      IMPLICIT NONE

! $ Abstract
!
!      See is a string is a legitimate time string.
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
! $ Keywords
!
!      PARSING, TIME
!
! $ Declarations

      CHARACTER*(*)         UTCSTR
      CHARACTER*(*)         MSSG
      INTEGER               TCODE

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      UTCSTR     I   Input time string, UTC.
!      MSSG       O   A diagnostic indicating why parsing failed.
!      TCODE      O   A short  parsing error flag.
!
! $ Detailed_Input
!
!      UTCSTR      is an input time string, containing a Calendar or
!                  Julian Date, UTC.
!
!                  Calendar dates consist of up to seven tokens:
!                  one each for System, Year, Month, Day, Hours,
!                  Minutes, and Seconds.
!
!                  Valid token delimiters are:
!
!                        ' '           space
!                        ','           comma
!                        '/'           slash
!                        '-'           dash
!                        ':'           colon
!
!                  The month may be an integer or a name. (At least
!                  three characters are required in a name.) The last
!                  three tokens always represent Hours, Minutes, and
!                  Seconds respectively. The first three tokens always
!                  represent Year, Month, and Day, with the order
!                  determined according to the following rules:
!
!                     1. If a month name is present, then the year is
!                        taken to be an integer greater than 1000 and
!                        less than 3000.  The day of the month is taken
!                        to be the non-negative integer less than 32.
!
!                     2. If no month name is present, the token greater
!                        than 1000 and less than 3000 is taken to be
!                        the year this must be the first token or the
!                        third.  In either case the other two tokens
!                        in order are then taken to be the month and
!                        day of month.
!
!                  Missing tokens are assigned the following defaults:
!
!                      - Month                    January
!                      - Day                      1
!                      - Hours                    0
!                      - Minutes                  0
!                      - Seconds                  0.0000000
!
!                  Note that Day of Year may be substituted for Month
!                  and Day in either of the following ways:
!
!                     1. By setting the month to January and the day to
!                        Day of Year, e.g.,
!
!                           '1986 JAN 247 12:00:01.184'
!
!                     2. By eliminating the month token altogether.
!                        (It defaults to January anyway.) The most
!                        popular form for DOY entry is:
!
!                           '1986//247 12:00:01.184'
!
!                  Julian Dates consist of two tokens.
!                  The first contains the letters 'JD', in any
!                  combinations of upper- or lower-case. The
!                  second token is a Julian Date. For convenience,
!                  the two tokens may be concatenated, as shown
!                  in the examples below. Valid token delimiters
!                  are the same as for Calendar format.
!
!                  If the token 'JD' is entered by itself, the
!                  input string is rejected as ambiguous.
!
!                  The length of UTC should not exceed 80 characters.
!
! $ Detailed_Output
!
!      MSSG        is a descriptive message indicating what went wrong
!                  if the string could not be parsed. It is blank when
!                  the string parses successfully as a time.
!
!      TCODE       is a short string that indicates why the date did not
!                  parse.
! $ Input_Files
!
!      None.
!
! $ Output_Files
!
!      None.
!
! $ Common_Variables
!
!      None.
!
! $ Detailed_Description
!
!      The input string is parsed for six tokens, delimited by any
!      of the valid delimiters (space, comma, slash, hyphen, colon).
!
!      If the first token is (or begins with) 'JD', the input is
!      taken to be a Julian Date. Extra tokens are ignored.
!
!      Otherwise, the last three tokens are assigned to hours,
!      minutes, and seconds respectively. The first three are
!      assigned to year, month, and day, according to magnitude and
!      the presence (or lack) of a month name, according to the rules
!      described under Detailed_Inputs above. The Muller-Wimberly
!      formula is used to compute the number of days past 2000 JAN 1,
!      which is then converted to UTC seconds past J2000.
!
! $ Examples
!
!      The following are examples of valid inputs to M2CAL:
!
!         '29 February 1975 3:00'       (  1 MAR 1975 03:00:00       )
!         'JD 2451545.'                 (  1 JAN 2000 12:00:00       )
!         'JD-2451545.'                 (  1 JAN 2000 12:00:00       )
!         'jd 2451545.'                 (  1 JAN 2000 12:00:00       )
!         'JD2451545.'                  (  1 JAN 2000 12:00:00       )
!
!      The following examples would be rejected as ambiguous.
!
!          '32 jan 32'
!          '85 86 january'
!          '86 3  january'
!          'January 80 81'
!          'JD,,,2451545'
!
! $ Restrictions
!
!      None.
!
! $ Required_Reading
!
!      TIME
!
! $ Literature_References
!
!      None.
!
! $ Author_and_Institution
!
!      W. M. Owen, Jr. (JPL)
!      I. M. Underwood (JPL)
!      W. L. Taber     (JPL)
!
! $ Version
!
!-     META/2 Version 3.0.0, 3-SEP-1998 (WLT)
!
!         Replaced everything with foundation Time routine calls.
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!      Version 1, 22-APR-1987
!
!-&




!
!     NAIFLIB functions
!
      LOGICAL               RETURN

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(16)        MODIFY ( 5 )
      CHARACTER*(8)         TYPE
      CHARACTER*(LNSIZE)    PICTUR

      DOUBLE PRECISION      TVEC ( 8 )

      INTEGER               NTVEC

      LOGICAL               MODS
      LOGICAL               SUCCES
      LOGICAL               YABBRV



      IF ( RETURN() ) THEN

         RETURN

      END IF

      MSSG  = ' '
      TCODE = 0



      CALL TPARTV ( UTCSTR, &
                    TVEC,   NTVEC, TYPE, &
                    MODIFY, MODS,  YABBRV, SUCCES, &
                    PICTUR, MSSG )

      IF ( .NOT. SUCCES ) THEN

         TCODE = 1

      ELSE IF ( TYPE .EQ. 'JD' ) THEN
!
!        Don't do anything.
!

      ELSE

         CALL TCHECK ( TVEC, TYPE, MODS, MODIFY, SUCCES, MSSG )

         IF ( .NOT. SUCCES ) THEN
            TCODE = 2
         END IF

      END IF

      RETURN
      END
