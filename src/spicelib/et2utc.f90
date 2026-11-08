!$Procedure ET2UTC ( Ephemeris Time to UTC )

      SUBROUTINE ET2UTC ( ET, FORMAT, PREC, UTCSTR )

! $ Abstract
!
!     Convert an input time from ephemeris seconds past J2000
!     to Calendar, Day-of-Year, or Julian Date format, UTC.
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
      CHARACTER*(*)         FORMAT
      INTEGER               PREC
      CHARACTER*(*)         UTCSTR

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch, given in ephemeris seconds past J2000.
!     FORMAT     I   Format of output epoch.
!     PREC       I   Digits of precision in fractional seconds or days.
!     UTCSTR     O   Output time string, UTC.
!
! $ Detailed_Input
!
!     ET       is the input epoch, ephemeris seconds past J2000.
!
!     FORMAT   is the format of the output time string. It may be
!              any of the following:
!
!                 'C'      Calendar format, UTC.
!
!                 'D'      Day-of-Year format, UTC.
!
!                 'J'      Julian Date format, UTC.
!
!                 'ISOC'   ISO Calendar format, UTC.
!
!                 'ISOD'   ISO Day-of-Year format, UTC.
!
!     PREC     is the number of digits of precision to which
!              fractional seconds (for Calendar and Day-of-Year
!              formats) or days (for Julian Date format) are to
!              be computed. If PREC is zero or smaller, no decimal
!              point is appended to the output string. If PREC is
!              greater than 14, it is treated as 14.
!
! $ Detailed_Output
!
!     UTCSTR   is the output time string equivalent to the input
!              epoch, in the specified format. Some examples are
!              shown below.
!
!                 'C'      '1986 APR 12 16:31:09.814'
!                 'D'      '1986-102 // 16:31:12.814'
!                 'J'      'JD 2446533.18834276'
!                 'ISOC'   '1987-04-12T16:31:12.814'
!                 'ISOD'   '1987-102T16:31:12.814'
!
!              If an error occurs, UTCSTR is not changed.
!
!              Fractional seconds, or for Julian dates, fractional
!              days, are rounded to the precision level specified
!              by the input argument PREC.
!
!              UTCSTR should be declared to be at least
!              20 + PREC characters in length to ensure
!              sufficient room to hold calendar strings
!              for modern epochs. For epochs prior to
!              1000 A.D. at least 24 + PREC characters in
!              length are required to hold the output
!              calendar string.
!
!              For epochs prior to 1000 A.D. Jan 1 calendar
!              and day of year formats are returned with the
!              era (A.D. or B.C.) attached to the year. For
!              example
!
!                 '877 A.D. MAR 17 13:29:11.829'
!                 '471 B.C. Jan 01 12:00:00.000'
!                 '471 B.C. 001 // 12:00:00.000'
!
!              ISO formats do not support the inclusion of an era.
!              For years prior to 1 A.D. an error will be signaled
!              if ISO format has been requested.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the format for the output string is not recognized, the
!         error SPICE(INVALIDTIMEFORMAT) is signaled.
!
!     2)  If PREC is less than or equal to zero, it is treated as
!         zero. If PREC is greater than 14, it is treated as 14.
!
!     3)  If one of the ISO formats is specified (ISOC or ISOD) but the
!         year corresponding to ET is prior to 1 A.D. on the Gregorian
!         Calendar, the error SPICE(YEAROUTOFRANGE) is signaled.
!
!     4)  Epochs prior to 15 Oct, 1582 on the Gregorian calendar (the
!         calendar commonly used in western societies) are returned in
!         the "extended" Gregorian Calendar. To convert epochs to the
!         Julian calendar see the SPICELIB routine GR2JUL.
!
!     5)  This routine does not attempt to account for variations
!         in the length of the second that were in effect prior
!         to Jan 1, 1972. For days prior to that date, we assume
!         there are exactly 86400 ephemeris seconds. Consequently
!         the UTC Gregorian calendar strings produced for epochs
!         prior to Jan 1, 1972 differ from the corresponding
!         TDB calendar strings by approximately 41.18 seconds.
!         (TDB Gregorian calendar strings are produced by the
!         routine ETCAL).
!
!     6)  If a leapseconds kernel has not been loaded prior to calling
!         this routine, an error is signaled by a routine in the
!         call tree of this routine.
!
! $ Files
!
!     A leapseconds kernel must be loaded via FURNSH prior to calling
!     this routine. The kernel need be loaded only once during a program
!     run.
!
! $ Particulars
!
!     This routine handles the task of converting a double precision
!     representation of an epoch to a character string suitable for
!     human consumption. The more general routine TIMOUT may also be
!     used to convert ET to time strings.
!
! $ Examples
!
!     Let the value of ET be -527644192.5403653 ephemeris seconds
!     past J2000. Assuming that the nominal values in the kernel pool
!     have not been altered, the following calls
!
!        CALL ET2UTC ( ET, 'C', 0, UTCSTR )
!        CALL ET2UTC ( ET, 'C', 3, UTCSTR )
!        CALL ET2UTC ( ET, 'D', 5, UTCSTR )
!        CALL ET2UTC ( ET, 'J', 7, UTCSTR )
!
!     produce the following output strings
!
!        '1983 APR 13 12:09:14'
!        '1983 APR 13 12:09:14.274'
!        '1983-103 // 12:09:14.27400'
!        'JD 2445438.0064152'
!
!     respectively.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  J. Jespersen and J. Fitz-Randolph, "From Sundials to Atomic
!          Clocks, Understanding Time and Frequency," Dover
!          Publications, Inc. New York, 1982.
!
! $ Author_and_Institution
!
!     C.H. Acton         (JPL)
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     J.M. Lynch         (JPL)
!     W.M. Owen          (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.5, 24-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Extended $Files
!        section.
!
!-    SPICELIB Version 3.0.4, 06-APR-2009 (NJB)
!
!        Header was updated to state that fractional
!        seconds or days are rounded in the output
!        string.
!
!-    SPICELIB Version 3.0.3, 28-JAN-2008 (BVS)
!
!        Fixed typo in the ISOC example string in $Detailed_Output.
!
!-    SPICELIB Version 3.0.2, 29-JUL-2003 (NJB) (CHA)
!
!        Various header changes were made to improve clarity and
!        more fully explain the routine's functionality.
!
!-    SPICELIB Version 3.0.1, 14-SEP-2000 (EDW)
!
!        Added FAILED check after TTRANS call during the calendar "C"
!        format processing to catch failure signal from TTRANS.
!        Lack of this check caused CSPICE based programs to core dump
!        if ET2UTC was called without a leapseconds kernel while
!        error action was set to RETURN.
!
!-    SPICELIB Version 3.0.0, 13-MAR-1996 (WLT)
!
!        The construction of the numerical components of the
!        output string are now handled by the SPICELIB routine
!        TTRANS.
!
!        In addition the routine now supports the ISO formats and
!        the era associated with an epoch (B.C. or A.D.) in non
!        ISO formats.
!
!-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
!
!        Removed some potential compile warnings that could be caused
!        by truncation of double precision values to integers through
!        a direct assignment. The direct assignment has been replaced
!        with a call to the intrinsic function IDINT.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 21-MAR-1991 (NJB) (JML)
!
!        Two bugs involving rounding errors were corrected. One of
!        the bugs caused conversion errors of magnitude as large as
!        1 second. See $Revisions for details.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (IMU)
!
!-&


! $ Index_Entries
!
!     ephemeris time to utc
!
!-&


! $ Revisions
!
!-    SPICELIB Version 3.0.0, 13-MAR-1996 (WLT)
!
!        The construction of the numerical components of the
!        output string are now handled by the SPICELIB routine
!        TTRANS.
!
!        In addition the routine now supports the era associated
!        with an epoch (B.C. or A.D.)
!
!-    SPICELIB Version 2.1.0, 11-JUL-1995 (KRG)
!
!        Removed some potential compile warnings that could be
!        caused by truncation of double precision values to
!        integers through a direct assignment. The direct
!        assignment has been replaced with a call to the
!        intrinsic function IDINT.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was
!        added following the header.
!
!-    SPICELIB Version 2.0.0, 21-MAR-1991 (NJB) (JML)
!
!        1) In the previous version of this routine, the algorithm
!        that was used permitted inconsistent conversion of the
!        integer and fractional parts of the UTC value
!        corresponding to the input ET value.
!
!           In the case where rounding the double precision UTC
!           time corresponding to the input ET value to PREC
!           decimal places resulted in a carry (to the integer
!           portion of the UTC value), the integer portion of the
!           UTC value was treated correctly, but the fraction was
!           not always rounded correctly. The specific case where
!           the problem occurred was when the input ET value
!           mapped to a UTC time having a fractional part that
!           rounded up to 1.0 when rounded PREC decimal places,
!           but that did not round up to 1.0 when rounded to the
!           nearest PREC+1 decimal places. The set of such
!           fractions can be represented as
!
!             { 1 - EPSILON :      EPSILON  <  5 * ( 10 **
!             -(PREC+1) ) - and
!
!                                  EPSILON  >  5 * ( 10 **
!                                  -(PREC+2) )
!
!                                                                     }
!
!           For example, if the input ET mapped to the UTC time
!
!              2 JAN 1991 00:34:12.99994,
!
!           then a call to this routine with PREC set to 3 would
!           result in the output
!
!              2 JAN 1991 00:34:13.999
!
!           instead of the correct value
!
!              2 JAN 1991 00:34:13.000
!
!           On the other hand, if the input ET mapped to the UTC
!           time
!
!              2 JAN 1991 00:34:12.99996,
!
!           then a call to this routine with PREC set to 3 would
!           result in the correct output.
!
!           This error was apparently difficult to generate: it
!           has never been reported by any SPICELIB users, and was
!           eventually discovered by NAIF staff.
!
!        2) The second bug is somewhat less severe, as far as the
!        magnitude of the error is concerned. However, it's
!        easier to generate this error. Namely, in some cases,
!        the fractional part of the input ET value is rounded to
!        PREC SIGNIFICANT DIGITS, rather than to PREC decimal
!        places. The effect of this is that the fraction is
!        occasionally truncated rather than rounded. For example,
!        the ET value equivalent to the UTC string
!
!              1991 JAN 2 00:34:12.0009
!
!           would be converted to
!
!              1991 JAN 2 00:34:12.000
!
!           instead of the correct value
!
!              1991 JAN 2 00:34:12.001
!
!           when the input argument PREC was set equal to 3.
!
!        The modifications made to solve these problems are as
!        follows:
!
!           1)  The input ET value, after conversion to `UTC
!               seconds past 2000', is broken up into the sum of a
!               whole number of seconds and a non-negative,
!               fractional number of seconds. The fact that the
!               fractional part is non-negative simplifies the
!               conversion of the fraction.
!
!           2)  The fraction is rounded to PREC decimal places---
!               that is, to the nearest integer multiple of
!               10**(-PREC). If the rounding results in a carry,
!               the whole number portion of the time value is
!               incremented by 1 second. After this step, the
!               whole number of seconds correctly accounts for
!               any necessary rounding of the fraction.
!
!           3)  The whole number portion of the time value is
!           passed through the inverse Muller-Wimberly algorithm
!           to obtain years, months, days, hours, minutes, and
!           whole seconds. A small fraction is added to the whole
!           number to prevent round-off error from occurring when
!           divisions are performed.
!
!           4)  The fraction is converted to a string using the
!               SPICELIB routine DPSTRF. To ensure that DPSTRF
!               produces an output string containing PREC decimal
!               places, an integer is added to the fraction value
!               before supplying it to DPSTRF. This integer
!               `anchors' the first significant digit of the input
!               value in the units place.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
      DOUBLE PRECISION      UNITIM
      LOGICAL               FAILED

!
!     Local Functions (Statement Functions)
!
      INTEGER               NDIGIT

!
!     Local variables
!

      CHARACTER*(4)         FMT
      CHARACTER*(80)        ENDSTR
      CHARACTER*(80)        STR
      CHARACTER*(80)        FRACT

      DOUBLE PRECISION      TAI
      DOUBLE PRECISION      FRCSEC
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      TVEC   ( 8 )
      DOUBLE PRECISION      WHLSEC

      INTEGER               DAY
      INTEGER               HOUR
      INTEGER               MINUTE
      INTEGER               MONTH
      INTEGER               SECOND
      INTEGER               YEAR

      INTEGER               MYPREC

      INTEGER               BDAY
      INTEGER               BHR
      INTEGER               BMN
      INTEGER               BMONTH
      INTEGER               BSC
      INTEGER               EDAY
      INTEGER               EHR
      INTEGER               EMN
      INTEGER               EMONTH
      INTEGER               ESC

      INTEGER               I

      CHARACTER*3           MTHNAM   ( 12 )


!
!     Save everything between calls
!
      SAVE

!
!     Initial values
!
      DATA         MTHNAM       / 'JAN', 'FEB', 'MAR', 'APR', &
                                  'MAY', 'JUN', 'JUL', 'AUG', &
                                  'SEP', 'OCT', 'NOV', 'DEC'  /



!
!     The function NDIGIT gives the number of digits required to
!     display a non-negative integer that is less than 10000
!
      NDIGIT(DAY) = MIN(1,DAY/1000) + MIN(1,DAY/100) + MIN(1,DAY/10) + 1

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ET2UTC' )

!
!     Convert FORMAT to uppercase for ease of comparison. Make sure it's
!     one of the recognized formats.
!
      CALL UCASE ( FORMAT, FMT )

      IF (       FMT .NE. 'J' &
           .AND. FMT .NE. 'C' &
           .AND. FMT .NE. 'D' &
           .AND. FMT .NE. 'ISOD' &
           .AND. FMT .NE. 'ISOC' ) THEN

         CALL SETMSG ( 'ET2UTC: Format specification for output ' &
         //            'time string is not recognized. Valid ' &
         //            'specifications are: ''C'', ' &
         //            '''D'', ''J'', ''ISOC'', or ''ISOD''. ' &
         //            'The supplied format was ''#''. ' )

         CALL ERRCH  ( '#', FORMAT )
         CALL SIGERR ( 'SPICE(INVALIDTIMEFORMAT)' )
         CALL CHKOUT ( 'ET2UTC' )
         RETURN

      END IF
!
!     Force PREC into an acceptable range
!
      MYPREC = MAX ( 0 , MIN ( 14, PREC ))

!
!     If the output is Julian Date, we're ready to go. Remember that
!     the day part of Julian Date already has seven digits built in.
!
      IF ( FMT .EQ. 'J' ) THEN

         TVEC(1) = ET

         CALL TTRANS ( 'TDB',  'JDUTC',        TVEC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ET2UTC' )
            RETURN
         END IF

         CALL DPSTRF ( TVEC(1), MYPREC+7, 'F', STR  )
         CALL PREFIX ( 'JD', 0,                STR  )

         UTCSTR = STR

         CALL CHKOUT ( 'ET2UTC' )
         RETURN

      END IF
!
!     If we've dropped past the IF-THEN block above, we need
!     to construct a calendar format string. First thing to
!     do is convert from ET to TAI.
!

      TAI = UNITIM ( ET, 'TDB', 'TAI' )

!
!     We're going to break up TAI into an integer and a
!     fractional part.  The integer will be the greatest
!     integer less than or equal to TAI, and the fraction
!     will be the difference between TAI and the integer
!     part.  The fraction will always be in the interval
!
!        [0, 1)
!
!     After making this decomposition, we'll adjust the integer
!     and fraction to take rounding into account.  The result
!     of the adjustment is that the fraction will be an integer
!     number of time units of length 10**(-MYPREC) seconds, where
!     the integer is in the range [0, (10**MYPREC)-1].  If the
!     fraction rounds up to 1, the fraction will be set to zero,
!     and the whole number portion of TAI will be incremented.
!
!     Since the integers involved may be too large to represent
!     using the INTEGER data type, we'll represent them with
!     double precision numbers.  We'll use the intrinsic ANINT
!     function to keep round-off from creeping into these d.p.
!     numbers representing integers.
!
!     Find the greatest integer less than or equal to TAI.
!     Recall that INT truncates toward the origin.  If TAI
!     is negative and is not already an integer, the result we
!     want is one less than AINT(TAI).
!
      WHLSEC = AINT ( TAI )

      IF (        ( TAI .LT. 0.0D0  ) &
            .AND. ( TAI .NE. WHLSEC )  )  THEN

         WHLSEC = WHLSEC - 1.0D0

      END IF

!
!     The fractional part of TAI must be rounded to the
!     nearest multiple of 10**(-MYPREC).  Fractions that are
!     equidistant from two multiples of 10**(-MYPREC) are
!     rounded up.
!
!     To accomplish the rounding, we scale the fraction by
!     10**MYPREC.
!
!
      SCALE  = ANINT (  10.0D0 ** MYPREC )
      FRCSEC = ANINT (  SCALE * ( TAI - WHLSEC )  )

!
!     If a carry occurred, the fraction becomes zero, and
!     we must increment WHLSEC.
!
      IF (  FRCSEC  .EQ.  SCALE  ) THEN

         WHLSEC = WHLSEC + 1.0D0
         FRCSEC = 0.0D0

      END IF

      FRCSEC = FRCSEC / SCALE

!
!     Now, we let TTRANS handle the transformation to
!     the desired components for output.
!
!     FRCSEC will be converted to a string containing MYPREC digits.
!     This will be done later on when the output string is
!     assembled.
!
      TVEC(1) = WHLSEC

      IF ( FMT .EQ. 'C' .OR. FMT .EQ. 'ISOC' ) THEN

         CALL TTRANS ( 'TAI', 'YMD', TVEC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ET2UTC' )
            RETURN
         END IF

         YEAR        = NINT( TVEC(1) )
         MONTH       = NINT( TVEC(2) )
         DAY         = NINT( TVEC(3) )
         HOUR        = NINT( TVEC(4) )
         MINUTE      = NINT( TVEC(5) )
         SECOND      = NINT( TVEC(6) )
!
!        The beginning of the string is going to be the year.
!        Depending upon the size of the year, it may or
!        may not have an era label.  However the end of the
!        string has a fixed size.  We set up that portion of the
!        string now.  First fill in the month...
!
         IF ( FMT .EQ. 'C' ) THEN
            ENDSTR      = ' MMM 00 00:00:00'
            ENDSTR(2:4) = MTHNAM(MONTH)
!
!           ... and then fill in the day portion of the string.
!
            EDAY = 7
            BDAY = EDAY - NDIGIT(DAY) + 1


            CALL INTSTR ( DAY, ENDSTR(BDAY:EDAY) )
            EHR  = 10
            EMN  = 13
            ESC  = 16

         ELSE

            ENDSTR = '-0M-00T00:00:00'
            EDAY   =  6
            BDAY   =  EDAY - NDIGIT(DAY) + 1

            EMONTH = 3
            BMONTH = EMONTH - NDIGIT(MONTH) + 1
            CALL INTSTR ( MONTH, ENDSTR(BMONTH:EMONTH) )
            CALL INTSTR ( DAY,   ENDSTR(BDAY:EDAY)     )
            EHR  =  9
            EMN  = 12
            ESC  = 15

         END IF

      ELSE
!
!        We must have day of year format.  Convert TAI to that
!        format.
!

         CALL TTRANS ( 'TAI', 'YD', TVEC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ET2UTC' )
            RETURN
         END IF

         YEAR   = NINT( TVEC(1) )
         MONTH  = 1
         DAY    = NINT( TVEC(2) )
         HOUR   = NINT( TVEC(3) )
         MINUTE = NINT( TVEC(4) )
         SECOND = NINT( TVEC(5) )
!
!        As in the previous case, the end of the output string will
!        have a fixed size.  We fill in the day portion of the string
!        now.  Note that we have set things up so that the hour,
!        minutes and seconds appear in the same location in both
!        day of year and calendar format of strings.
!
         IF ( FMT .EQ. 'D' ) THEN
            ENDSTR = '-000 // 00:00:00'
            EDAY   = 4
            BDAY   = EDAY - NDIGIT(DAY) + 1

            CALL INTSTR ( DAY, ENDSTR(BDAY:EDAY) )
            EHR  = 10
            EMN  = 13
            ESC  = 16
         ELSE
            ENDSTR = '-000T00:00:00'
            EDAY   = 4
            BDAY   = EDAY - NDIGIT(DAY) + 1

            CALL INTSTR ( DAY, ENDSTR(BDAY:EDAY) )
            EHR  = 7
            EMN  = 10
            ESC  = 13
         END IF
      END IF
!
!     Fill out the hours, minutes and integer portion of
!     seconds in the output string.
!
      BHR  = EHR - NDIGIT(HOUR) + 1
      BMN  = EMN - NDIGIT(MINUTE) + 1
      BSC  = ESC - NDIGIT(SECOND) + 1

      CALL INTSTR ( HOUR,   ENDSTR(BHR:EHR) )
      CALL INTSTR ( MINUTE, ENDSTR(BMN:EMN) )
      CALL INTSTR ( SECOND, ENDSTR(BSC:ESC) )
!
!     Append the fractional part of the seconds component.
!
      IF ( MYPREC .GT. 0 ) THEN

!
!        DPSTRF gives MYPREC significant digits in the output,
!        not necessarily MYPREC digits to the right of the
!        decimal point.  We will add a one-digit integer to
!        FRCSEC to `anchor' the first significant digit of
!        FRCSEC in a known place.  That way, we can get DPSTRF
!        to give us a known number of digits after the decimal
!        point.
!
!        The integer part of FRCSEC will not affect the output
!        string.
!
         FRCSEC = FRCSEC + 1.0D0

         CALL DPSTRF ( FRCSEC, MYPREC+1, 'F', FRACT )

         I = INDEX ( FRACT, '.' )
         ENDSTR(ESC+1: ) = FRACT(I:I+MYPREC)

      END IF
!
!     The end of the time string is now complete.  We need to
!     construct the year portion of the string. We are going to
!     append an era if the year is before 1000 A.D. Note that
!     we make sure the first character in the ending string
!     is a blank (' ') if the era is to be attached.  Otherwise
!     we'd get confusing day of year formats like
!     999 A.D.-019 // 12:13:18.
!
      IF ( YEAR .GE. 1000 ) THEN

         CALL   INTSTR( YEAR, STR )

      ELSE IF ( YEAR .GT. 0 ) THEN

         CALL INTSTR ( YEAR,      STR )

         IF ( FMT .EQ. 'C' .OR. FMT .EQ. 'D' ) THEN
            CALL SUFFIX ( 'A.D.', 1, STR )
            ENDSTR(1:1) = ' '
         END IF

      ELSE IF ( YEAR .LE. 0 ) THEN

         IF ( FMT .EQ. 'C' .OR. FMT .EQ. 'D' ) THEN
            YEAR = -YEAR + 1

            CALL INTSTR ( YEAR,      STR )
            CALL SUFFIX ( 'B.C.', 1, STR )

            ENDSTR(1:1) = ' '
         ELSE

            YEAR = -YEAR + 1
            CALL SETMSG ( 'The year of the ET epoch supplied is ' &
            //            '# B.C.  Years in this era are not ' &
            //            'supported in ISO format. ' )
            CALL ERRINT ( '#', YEAR )
            CALL SIGERR ( 'SPICE(YEAROUTOFRANGE)'  )
            CALL CHKOUT ( 'ET2UTC' )
            RETURN

         END IF

      END IF

!
!     Finally append the ENDSTR to STR to get the fully formatted
!     string.
!
      CALL SUFFIX ( ENDSTR, 0, STR )

      UTCSTR = STR

      CALL CHKOUT ( 'ET2UTC' )
      RETURN


      END
