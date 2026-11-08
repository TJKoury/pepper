!$Procedure ETCAL ( Convert ET to Calendar format )

      SUBROUTINE ETCAL ( ET, CALSTR )

! $ Abstract
!
!     Convert from an ephemeris epoch measured in seconds past
!     the epoch of J2000 to a calendar string format using a
!     formal calendar free of leapseconds.
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
      CHARACTER*(*)         CALSTR

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
      DOUBLE PRECISION      SPD
      INTEGER               INTMAX
      INTEGER               INTMIN
      INTEGER               LSTLTI



!
!     We declare the variables that contain the number of days in
!     400 years, 100 years, 4 years and 1 year.
!
      INTEGER               DP400Y
      PARAMETER           ( DP400Y = 365*400 + 97 )

      INTEGER               DP100Y
      PARAMETER           ( DP100Y = 365*100 + 24 )

      INTEGER               DP4Y
      PARAMETER           ( DP4Y   = 365*4   + 1  )

      INTEGER               DP1Y
      PARAMETER           ( DP1Y   = 365 )


!
!     The following integers give the number of days during the
!     associated month of a non-leap year.
!
      INTEGER               JAN
      PARAMETER           ( JAN = 31 )

      INTEGER               FEB
      PARAMETER           ( FEB = 28 )

      INTEGER               MAR
      PARAMETER           ( MAR = 31 )

      INTEGER               APR
      PARAMETER           ( APR = 30 )

      INTEGER               MAY
      PARAMETER           ( MAY = 31 )

      INTEGER               JUN
      PARAMETER           ( JUN = 30 )

      INTEGER               JUL
      PARAMETER           ( JUL = 31 )

      INTEGER               AUG
      PARAMETER           ( AUG = 31 )

      INTEGER               SEP
      PARAMETER           ( SEP = 30 )

      INTEGER               OCT
      PARAMETER           ( OCT = 31 )

      INTEGER               NOV
      PARAMETER           ( NOV = 30 )

      INTEGER               DEC
      PARAMETER           ( DEC = 31 )

!
!     The integers that follow give the number of days in a normal
!     year that precede the first of the month.
!
      INTEGER               JAN0
      PARAMETER           ( JAN0 = 0  )

      INTEGER               FEB0
      PARAMETER           ( FEB0 = JAN + JAN0 )

      INTEGER               MAR0
      PARAMETER           ( MAR0 = FEB + FEB0 )

      INTEGER               APR0
      PARAMETER           ( APR0 = MAR + MAR0 )

      INTEGER               MAY0
      PARAMETER           ( MAY0 = APR + APR0 )

      INTEGER               JUN0
      PARAMETER           ( JUN0 = MAY + MAY0 )

      INTEGER               JUL0
      PARAMETER           ( JUL0 = JUN + JUN0 )

      INTEGER               AUG0
      PARAMETER           ( AUG0 = JUL + JUL0 )

      INTEGER               SEP0
      PARAMETER           ( SEP0 = AUG + AUG0 )

      INTEGER               OCT0
      PARAMETER           ( OCT0 = SEP + SEP0 )

      INTEGER               NOV0
      PARAMETER           ( NOV0 = OCT + OCT0 )

      INTEGER               DEC0
      PARAMETER           ( DEC0 = NOV + NOV0 )

!
!     The integers that follow give the number of days in a leap
!     year that precede the first of the month.
!
      INTEGER               JANL0
      PARAMETER           ( JANL0 = JAN0  )

      INTEGER               FEBL0
      PARAMETER           ( FEBL0 = FEB0 )

      INTEGER               MARL0
      PARAMETER           ( MARL0 = MAR0 + 1 )

      INTEGER               APRL0
      PARAMETER           ( APRL0 = APR0 + 1 )

      INTEGER               MAYL0
      PARAMETER           ( MAYL0 = MAY0 + 1 )

      INTEGER               JUNL0
      PARAMETER           ( JUNL0 = JUN0 + 1 )

      INTEGER               JULL0
      PARAMETER           ( JULL0 = JUL0 + 1 )

      INTEGER               AUGL0
      PARAMETER           ( AUGL0 = AUG0 + 1 )

      INTEGER               SEPL0
      PARAMETER           ( SEPL0 = SEP0 + 1 )

      INTEGER               OCTL0
      PARAMETER           ( OCTL0 = OCT0 + 1 )

      INTEGER               NOVL0
      PARAMETER           ( NOVL0 = NOV0 + 1 )

      INTEGER               DECL0
      PARAMETER           ( DECL0 = DEC0 + 1 )

      INTEGER               STRSIZ
      PARAMETER           ( STRSIZ = 16 )

      INTEGER               LNGSIZ
      PARAMETER           ( LNGSIZ = 180 )


!
!     The variables below hold the components of the output string
!     before they are put together.
!
      CHARACTER*(STRSIZ)    ERA
      CHARACTER*(STRSIZ)    YSTR
      CHARACTER*(STRSIZ)    DSTR
      CHARACTER*(STRSIZ)    HSTR
      CHARACTER*(STRSIZ)    MSTR
      CHARACTER*(STRSIZ)    SSTR
      CHARACTER*(STRSIZ)    MESSGE

!
!     We will construct our string using the local variable DATE
!     and transfer the results to the output CALSTR when we are
!     done.
!
      CHARACTER*(LNGSIZ)    DATE

!
!     MONTHS contains 3-letter abbreviations for the months of the year
!
      CHARACTER*(3)         MONTHS ( 12 )

      DOUBLE PRECISION      DMNINT
      DOUBLE PRECISION      DMXINT
      DOUBLE PRECISION      DP2000
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      HALFD
      DOUBLE PRECISION      MYDNOM
      DOUBLE PRECISION      MYNUM
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      REMD
      DOUBLE PRECISION      SECS
      DOUBLE PRECISION      SECSPD

      INTEGER               BH
      INTEGER               BM
      INTEGER               DAY
      INTEGER               DAYNUM
      INTEGER               DN2000
      INTEGER               DOFYR
      INTEGER               DOY
      INTEGER               DP0001
      INTEGER               HOURS
      INTEGER               IQ
      INTEGER               LDAYS
      INTEGER               MINS
      INTEGER               MONTH
      INTEGER               OFFSET
      INTEGER               REM
      INTEGER               TSECS
      INTEGER               YDAYS
      INTEGER               YEAR
      INTEGER               YR1
      INTEGER               YR100
      INTEGER               YR4
      INTEGER               YR400

      LOGICAL               FIRST
      LOGICAL               ADJUST

!
!     The array EXTRA contains the number of additional days that
!     appear before the first of a month during a leap year (as opposed
!     to a non-leap year).
!

      INTEGER               EXTRA  ( 12 )
      INTEGER               DPBEGL ( 12 )

!
!     DPJAN0(I) gives the number of days that occur before the I'th
!     month of a normal year.
!
      INTEGER               DPJAN0 ( 12 )

      SAVE

      DATA                  FIRST   / .TRUE. /

      DATA                  EXTRA   /  0,     0,     1,     1, &
                                       1,     1,     1,     1, &
                                       1,     1,     1,     1    /

      DATA                  DPJAN0  /  JAN0,  FEB0,  MAR0,  APR0, &
                                       MAY0,  JUN0,  JUL0,  AUG0, &
                                       SEP0,  OCT0,  NOV0,  DEC0 /

      DATA                  DPBEGL  /  JANL0, FEBL0, MARL0, APRL0, &
                                       MAYL0, JUNL0, JULL0, AUGL0, &
                                       SEPL0, OCTL0, NOVL0, DECL0 /

      DATA                  MONTHS  / 'JAN',  'FEB', 'MAR', 'APR', &
                                      'MAY',  'JUN', 'JUL', 'AUG', &
                                      'SEP',  'OCT', 'NOV', 'DEC' /



!
!     Definitions of statement functions.
!
!
!     The number of days elapsed since Jan 1, of year 1 A.D. to
!     Jan 1 of YEAR is given by:
!
      YDAYS(YEAR) = 365*(YEAR-1) &
                    +  ((YEAR-1)/4) &
                    -  ((YEAR-1)/100) &
                    +  ((YEAR-1)/400)

!
!     The number of leap days in a year is given by:
!
      LDAYS(YEAR) =   ( (YEAR/4  ) * 4   ) / YEAR &
                    - ( (YEAR/100) * 100 ) / YEAR &
                    + ( (YEAR/400) * 400 ) / YEAR

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
      DOY(YEAR,MONTH,DAY) =   DPJAN0 ( MONTH ) &
                            + EXTRA  ( MONTH ) * LDAYS(YEAR) &
                            + DAY


!
!     The number of days since 1 Jan 1 A.D. is given by:
!
      DP0001(YEAR,MONTH,DAY) = YDAYS(YEAR) &
                             + DOY  (YEAR, MONTH, DAY) &
                             - 1




      IF ( FIRST ) THEN

         FIRST  = .FALSE.
         HALFD  = SPD()/2.0D0
         SECSPD = SPD()
         DN2000 = DP0001 ( 2000, 1, 1 )
         DMXINT = INTMAX()
         DMNINT = INTMIN()

      END IF
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
      MYDNOM = SECSPD
      MYNUM  = ET + HALFD

      Q    = DINT ( MYNUM / MYDNOM )
      REMD = MYNUM -    Q * MYDNOM

      IF ( REMD .LT. 0.0D0 ) THEN
         Q    = Q   - 1.0D0
         REMD = REMD + MYDNOM
      END IF

      SECS   = REMD
      DP2000 = Q

!
!     Do something about the problem when ET is vastly
!     out of range.  (Day number outside MAX and MIN integer).
!
      IF      ( DP2000 + DN2000 .LT. DMNINT + 1) THEN

         DP2000 = DMNINT - DN2000 + 1
         MESSGE = 'Epoch before '
         SECS   = 0.0D0

      ELSE IF ( DP2000 + DN2000 .GT. DMXINT - 1 ) THEN

         DP2000 = DMXINT - DN2000 - 1
         MESSGE = 'Epoch after '
         SECS   = 0.0D0

      ELSE

         MESSGE = ' '

      END IF

!
!     Compute the number of days since 1 .A.D. Jan 1, 00:00:00.
!     From the tests in the previous IF-ELSE IF-ELSE block this
!     addition is guaranteed not to overflow.
!
      DAYNUM = INT( DP2000 + DBLE(DN2000) )

!
!     If the number of days is negative, we need to do a little
!     work so that we can represent the date in the B.C. era.
!     We add enough multiples of 400 years so that the year will
!     be positive and then we subtract off the appropriate multiple
!     of 400 years later.
!
      IF ( DAYNUM .LT. 0 ) THEN

!
!        Since we can't make the call below and remain
!        error free, we compute it ourselves.
!
!        call rmaini ( daynum, dp400y, offset, daynum )
!
         IQ   = DAYNUM / DP400Y
         REM  = DAYNUM - DP400Y*IQ

         IF ( REM .LT. 0 ) THEN
            IQ   = IQ   - 1
            REM  = REM  + DP400Y
         END IF

         OFFSET = IQ
         DAYNUM = REM

         ADJUST = .TRUE.

      ELSE

         ADJUST = .FALSE.

      END IF

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
      YR400  =  DAYNUM / DP400Y
      REM    =  DAYNUM - DP400Y*YR400

      YR100  =  MIN ( 3, REM   / DP100Y )
      REM    =  REM    - YR100 * DP100Y

      YR4    =  MIN (24, REM / DP4Y )
      REM    =  REM    - YR4 * DP4Y

      YR1    =  MIN (3,  REM / DP1Y )
      REM    =  REM    - YR1 * DP1Y

      DOFYR  =  REM    + 1

      YEAR   = YR400*400 + YR100*100 + YR4*4 + YR1 + 1
!
!     Get the month, and day of month (depending upon whether
!     we have a leap year or not).
!
      IF ( LDAYS(YEAR) .EQ. 0 ) THEN
         MONTH  = LSTLTI ( DOFYR, 12, DPJAN0 )
         DAY    = DOFYR - DPJAN0(MONTH)
      ELSE
         MONTH  = LSTLTI ( DOFYR, 12, DPBEGL )
         DAY    = DOFYR - DPBEGL(MONTH)
      END IF
!
!     If we had to adjust the year to make it positive, we now
!     need to correct it and then convert it to a B.C. year.
!
      IF ( ADJUST ) THEN

         YEAR   =  YEAR + OFFSET*400
         YEAR   = -YEAR + 1
         ERA    = ' B.C. '

      ELSE
!
!        If the year is less than 1000, we can't just write it
!        out.  We need to add the era.  If we don't do this
!        the dates look very confusing.
!
         IF ( YEAR .LT. 1000 ) THEN
            ERA   = ' A.D. '
         ELSE
            ERA   = ' '
         END IF

      END IF
!
!     Convert Seconds to Hours, Minute and Seconds.
!     We work with thousandths of a second in integer arithmetic
!     so that all of the truncation work with seconds will already
!     be done.  (Note that we already know that SECS is greater than
!     or equal to zero so we'll have no problems with HOURS, MINS
!     or SECS becoming negative.)
!
      TSECS  = INT(SECS*1000.0D0)
      FRAC   = SECS - DBLE(TSECS)

      HOURS  = TSECS / 3600000
      TSECS  = TSECS - 3600000 * HOURS

      MINS   = TSECS / 60000
      TSECS  = TSECS - 60000 * MINS

      SECS   = DBLE ( TSECS ) / 1000.0D0
!
!     We round seconds if we can do so without getting seconds to be
!     bigger than 60.
!
      IF ( SECS + 0.0005D0 .LT. 60.0D0 ) THEN
         SECS = SECS + 0.0005D0
      END IF

!
!     Finally, get the components of our date string.
!
      CALL INTSTR ( YEAR,   YSTR )
      IF ( DAY .GE. 10 ) THEN
         CALL INTSTR ( DAY, DSTR )
      ELSE
         DSTR = '0'
         CALL INTSTR( DAY, DSTR(2:) )
      END IF

!
!     We want to zero pad the hours minutes and seconds.
!
      IF ( HOURS .LT. 10 ) THEN
         BH = 2
      ELSE
         BH = 1
      END IF

      IF ( MINS .LT. 10 ) THEN
         BM = 2
      ELSE
         BM = 1
      END IF


      MSTR = '00'
      HSTR = '00'
      SSTR = ' '

!
!     Now construct the string components for hours, minutes and
!     seconds.
!
      SECS = INT(SECS*1000.0D0) / 1000.0D0

      CALL INTSTR ( HOURS,         HSTR(BH:) )
      CALL INTSTR ( MINS,          MSTR(BM:) )
      CALL DPSTRF ( SECS,  6, 'F', SSTR      )
!
!     The form of the output for SSTR has a leading blank followed by
!     the first significant digit.  If a decimal point is in the
!     third slot, then SSTR is of the form ' x.xxxxx'  and we need
!     to insert a leading zero.
!
      IF ( SSTR(3:3) .EQ. '.' ) THEN
         SSTR(1:1) = '0'
      END IF
!
!     We don't want any leading spaces in SSTR, (HSTR and MSTR don't
!     have leading spaces by construction.
!
      CALL LJUST ( SSTR, SSTR )
!
!     Now form the date string, squeeze out extra spaces and
!     left justify the whole thing.
!
      DATE = MESSGE &
      //     YSTR &
      //     ERA &
      //     MONTHS(MONTH) &
      //     ' ' &
      //     DSTR(1:3) &
      //     ' ' &
      //     HSTR(1:2) &
      //     ':' &
      //     MSTR(1:2) &
      //     ':' &
      //     SSTR(1:6)


      CALL CMPRSS ( ' ', 1, DATE, DATE )
      CALL LJUST  (         DATE, DATE )

      CALSTR = DATE

      RETURN
      END
