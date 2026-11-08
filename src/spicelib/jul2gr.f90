!$Procedure JUL2GR (Julian to Gregorian Calendar)

      SUBROUTINE JUL2GR (  YEAR, MONTH, DAY, DOY )

! $ Abstract
!
!     Convert Year Month and Day on the Julian Calendar
!     to the Gregorian Calendar
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
!     None.
!
! $ Keywords
!
!     TIME
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               YEAR
      INTEGER               MONTH
      INTEGER               DAY
      INTEGER               DOY

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     YEAR      I-O  Year  of Julian Calendar/Gregorian Calendar
!     MONTH     I-O  Month of Julian Calendar/Gregorian Calendar
!     DAY       I-O  Day of Month in Julian Calendar/Gregorian Calendar
!     DOY        O   Day of Year in Gregorian Calendar
!
! $ Detailed_Input
!
!     YEAR     is an integer representing the year of an epoch, E, in
!              the Julian proleptic calendar. Note that the year 0
!              and negative values are required to represent
!              years in the pre-Christian era (B.C.) A year, Y B.C.,
!              should be represented as -(Y-1).  For example the year
!              435 B.C. should be input as -434.
!
!     MONTH    is an integer representing the month of some epoch, E,
!              in the Julian proleptic calendar. Months
!              outside the usual range from 1 to 12 are converted
!              to the standard range using modular arithmetic and
!              the input year is adjusted appropriately.
!
!
!     DAY      is the day of the month of some epoch, E, in the Julian
!              proleptic calendar.
!
!              Note to input an epoch as the day of a year, set MONTH
!              to 1 and DAY to the day of the year.
!
! $ Detailed_Output
!
!     YEAR     is an integer representing the year of the epoch, E,
!              above in the Gregorian calendar. Note that the year
!              0 (zero) and negative values are used to represent
!              years in the pre-Christian era (B.C.) A year, Y B.C.,
!              is be represented as -(Y-1).  For example the year
!              435 B.C. will be returned as -434.
!
!     MONTH    is an integer representing the month of the epoch, E,
!              above in the Gregorian Calendar calendar.
!
!     DAY      is the day of the month of the epoch, E, above in the
!              Gregorian Calendar
!
!     DOY      is the day of the year of the epoch, E, above in the
!              Gregorian Calendar.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a mid-level utility routine to assist in the assignment
!     and presentation of ancient epochs.
!
!     The SPICE software deals primarily with epochs represented on
!     in the Gregorian Calendar. However, the Gregorian calendar
!     was not adopted until October 15, 1582. As a result, epochs
!     prior to that time are usually represented in the Julian
!     proleptic calendar.
!
!     Formally, both calendars can be extended indefinitely forward
!     and backward in time due the algorithmic nature of the
!     determination of calendar representation.
!
!     When converting "parsed" calendar epochs in the SPICE system,
!     you need to first convert to the Gregorian Calendar. From that
!     point the SPICE toolkit can easily convert the epoch to Julian
!     date or seconds past the J2000 epoch.
!
!     This routine allows you to take a numeric representation of
!     an epoch represented in the Julian proleptic calendar and
!     convert that to an epoch in the Gregorian calendar.
!
!     To convert from Gregorian Calendar to Julian proleptic
!     calendar, use the entry point GR2JUL.
!
! $ Examples
!
!     Suppose you need to find the epoch (in seconds past the
!     J2000) of some ancient epoch that occurred at
!     3:00 on March 4 of the year 121 B.C. And that this epoch
!     is based on the Julian proleptic calendar. We first need
!     to convert the Julian Calendar date to the Gregorian Calendar.
!
!     Here's the declarations we'll need
!
!        INTEGER               YEAR
!        INTEGER               MONTH
!        INTEGER               DAY
!        INTEGER               DOY
!
!        DOUBLE PRECISION      TVEC ( 6 )
!        DOUBLE PRECISION      TDB
!
!     You first need to convert the calendar date of this epoch
!     integers. (We don't need to worry about the hours for a moment).
!
!        YEAR  = -120
!        MONTH =  3
!        DAY   =  4
!
!     Convert this Year, Month and Day to the Gregorian Calendar.
!
!        CALL JUL2GR ( YEAR, MONTH, DAY, DOY )
!
!     Now construct a time vector for use in the routine TTRANS.
!     Note now we use the hour component of the epoch (the fourth
!     component of the time vector TVEC).
!
!        TVEC(1) = DBLE( YEAR )
!        TVEC(2) = DBLE( MONTH )
!        TVEC(3) = DBLE( DAY )
!        TVEC(4) = 3.0D0
!        TVEC(5) = 0.0D0
!        TVEC(6) = 0.0D0
!
!     Now the routine TTRANS can convert the time vector from
!     the input YMD format to barycentric dynamical time.
!
!        CALL TTRANS ( 'YDM', 'TDB', TVEC )
!
!        TDB = TVEC(1)
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.1, 02-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.0, 26-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in RMAINI calls.
!
!-    SPICELIB Version 1.1.1, 23-SEP-1999 (WLT)
!
!        Removed the unused variable DPMON.
!
!-    SPICELIB Version 1.1.0, 23-FEB-1998 (WLT)
!
!        The routine was upgraded so that it will handle without
!        error months that are outside the range from 1 to 12.
!
!-    SPICELIB Version 1.0.0, 13-MAR-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Convert from Julian proleptic to Gregorian Calendar
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 23-FEB-1998 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in RMAINI calls.
!
!-&


!
!     Spicelib Functions
!
      INTEGER               LSTLTI

!
!     Local (in-line) Functions
!
      INTEGER               DIVBLE
      INTEGER               GDOY
      INTEGER               GDP001
      INTEGER               GLDAYS
      INTEGER               GYDAYS
      INTEGER               JDOY
      INTEGER               JDP001
      INTEGER               JLDAYS
      INTEGER               JYDAYS

!
!     Local parameters
!
!     We declare the variables that contain the number of days in
!     400 years (Gregorian), 100 years (Gregorian), 4 years and 1 year.
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


!
!     Local variables
!
      INTEGER               DAYG
      INTEGER               DAYJ
      INTEGER               DOFYR
      INTEGER               DY
      INTEGER               M
      INTEGER               M100
      INTEGER               M4
      INTEGER               M400
      INTEGER               MON
      INTEGER               OFFSET
      INTEGER               OFFSTG
      INTEGER               OFFSTJ
      INTEGER               RDAYG
      INTEGER               RDAYJ
      INTEGER               TMPDAY
      INTEGER               TMPYR
      INTEGER               YR
      INTEGER               YROFF

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

      LOGICAL               FIRST

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
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
!
!     Definitions of statement functions.
!
!     The number of days elapsed since Gregorian Jan 1, of year 1 A.D.
!     to Jan 1 of YR is given by:
!
      GYDAYS(YR) = 365*( YR-1) &
                   +   ((YR-1)/4) &
                   -   ((YR-1)/100) &
                   +   ((YR-1)/400)

!
!     The number of days elapsed since Julian Jan 1, of year 1 A.D.
!     to Jan 1 of YR is given by:
!
      JYDAYS(YR) = 365*(YR-1) &
                   +  ((YR-1)/4)

!
!     Return 1 if YR is divisible by M, otherwise return 0.
!
      DIVBLE(YR,M) = MAX ( 0, 1 + ((IABS(YR)/M)*M) - IABS(YR) )

!
!     The number of leap days in a Gregorian year is given by:
!
      GLDAYS(YR) = DIVBLE( YR, 4   ) &
                 - DIVBLE( YR, 100 ) &
                 + DIVBLE( YR, 400 )

!
!     The number of leap days in a Julian year is given by:
!
      JLDAYS(YR) = DIVBLE( YR, 4   )

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
      GDOY(YR,MON,DY) = DPJAN0 ( MON ) &
                      + EXTRA  ( MON ) * GLDAYS(YR) &
                      + DY

      JDOY(YR,MON,DY) = DPJAN0 ( MON ) &
                      + EXTRA  ( MON ) * JLDAYS(YR) &
                      + DY

!
!     The number of days since 1 Jan 1 A.D. (Gregorian) is given by:
!
      GDP001(YR,MON,DY) = GYDAYS(YR) &
                        + GDOY  (YR, MON, DY) &
                        - 1

!
!     The number of days since 1 Jan 1 A.D. (Julianis given by:
!
      JDP001(YR,MON,DY) = JYDAYS(YR) &
                        + JDOY  (YR, MON, DY) &
                        - 1

!
!     If this is the first pass through this entry point (or the
!     companion entry point) we need to set up some reference points.
!
!     RDAYG   is the number of days past 1 A.D. Jan 1 of the Gregorian
!             calendar of the date Oct 15, 1582
!
!     RDAYJ   is the number of days past 1 A.D. Jan 1 of the Julian
!             calendar of the date Oct 5, 1582.
!
!     OFFSTJ and OFFSTG are just the offset from one count of days
!     to the other.
!
      IF ( FIRST ) THEN

         FIRST  = .FALSE.

         RDAYG  = GDP001 ( 1582, 10, 15 )
         RDAYJ  = JDP001 ( 1582, 10,  5 )
         OFFSTJ = RDAYJ - RDAYG
         OFFSTG = RDAYG - RDAYJ

      END IF

!
!     Make local copies of the year, month and day.  Then get the
!     YEARs into a positive range.
!
      CALL RMAINI ( MONTH-1, 12, YROFF, MON )

      YR  = YEAR + YROFF
      MON = MON + 1
      DY  = DAY

      IF ( YR .LE. 0 ) THEN

         CALL RMAINI ( YR, 4, M4, TMPYR )
         YR = TMPYR

         IF ( YR .EQ. 0 ) THEN
            YR = YR + 4
            M4   = M4   - 1
         END IF

         OFFSET = M4 * DP4Y

      ELSE

         OFFSET = 0

      END IF

!
!     First get the day number (Julian) for the input
!     year month and day.
!
      DAYJ = JDP001 ( YR, MON, DY  ) + OFFSET

!
!     This day is DAYJ - RDAYJ days after 1582 Oct 5 on the
!     julian calendar.  But this is the same as the number
!     of days past 1582 Oct 15 on the Gregorian Calendar
!     So the Gregorian day number is DAYJ - RDAYJ + RDAYG
!     which is the same as DAYJ + OFFSTG.
!
      DAYG = DAYJ + OFFSTG
!
!     Now that we have the Gregorian day number it's a fairly
!     straight forward task to get the year, month and day
!     on the Gregorian calendar.
!

      CALL RMAINI ( DAYG, DP400Y, M400, TMPDAY )
      DAYG = TMPDAY

      M100 = MIN ( 3, DAYG/DP100Y )
      DAYG = DAYG -   M100*DP100Y

      M4   = MIN ( 24, DAYG/DP4Y  )
      DAYG = DAYG  -     M4*DP4Y

      M    = MIN (  3, DAYG/DP1Y )
      DAYG = DAYG   -     M*DP1Y


      DOFYR  = DAYG + 1
      YR     = 400 * M400 &
             + 100 * M100 &
             +   4 * M4 &
             +       M &
             +       1

!
!     Now look up the month number and compute the day of the month.
!     How we do this depends on whether or not this is a leap year.
!
      IF ( GLDAYS(YR) .EQ. 0 ) THEN
         MON  = LSTLTI ( DOFYR, 12, DPJAN0 )
         DY   = DOFYR  -            DPJAN0(MON)
      ELSE
         MON  = LSTLTI ( DOFYR, 12, DPBEGL )
         DY   = DOFYR  -            DPBEGL(MON)
      END IF

      YEAR  = YR
      MONTH = MON
      DAY   = DY
      DOY   = DOFYR

      RETURN


!$Procedure GR2JUL (Gregorian to Julian Calendar)

      ENTRY GR2JUL (  YEAR, MONTH, DAY, DOY )

! $ Abstract
!
!     Convert Year Month and Day on the  Gregorian Calendar
!     to the Julian Calendar
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
!     None.
!
! $ Keywords
!
!     TIME
!
! $ Declarations
!
!     INTEGER               YEAR
!     INTEGER               MONTH
!     INTEGER               DAY
!     INTEGER               DOY
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     YEAR      I-O  Year  of Gregorian Calendar/Julian Calendar
!     MONTH     I-O  Month of Gregorian Calendar/Julian Calendar
!     DAY       I-O  Day of Month in Gregorian Calendar/Julian Calendar
!     DOY        O   Day of Year in Julian Calendar
!
! $ Detailed_Input
!
!     YEAR     is an integer representing the year of an epoch, E, in
!              the Gregorian calendar. Note that the year 0 (zero)
!              and negative values are required to represent
!              years in the pre-Christian era (B.C.) A year, Y B.C.
!              should be represented as -(Y-1).  For example the year
!              435 B.C. should be input as -434.
!
!     MONTH    is an integer representing the month of some epoch, E,
!              in the Gregorian calendar. Months
!              outside the usual range from 1 to 12 are converted
!              to the standard range using modular arithmetic and
!              the input year is adjusted appropriately.
!
!     DAY      is the day of the month of some epoch, E, in the
!              Gregorian calendar.
!
!              Note to input an epoch as the day of a year, set MONTH
!              to 1 and DAY to the day of the year.
!
! $ Detailed_Output
!
!     YEAR     is an integer representing the year of the epoch, E,
!              above in the Julian calendar. Note that the year 0
!              (zero) and negative values are used to represent
!              years in the pre-Christian era (B.C.) A year, Y B.C.,
!              is be represented as -(Y-1).  For example the year
!              435 B.C. will be returned as -434.
!
!     MONTH    is an integer representing the month of the epoch, E,
!              above in the Julian Calendar calendar.
!
!     DAY      is the day of the month of the epoch, E, above in the
!              Julian Calendar
!
!     DOY      is the day of the year of the epoch, E, above in the
!              Julian Calendar.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This is a mid-level utility routine to assist in the assignment
!     and presentation of Ancient epochs.
!
!     The SPICE software deals primarily with epochs represented on
!     in the Gregorian Calendar. However, the Gregorian calendar
!     was not adopted until October 15, 1582. As a result, epochs
!     prior to that time are usually represented in the Julian
!     proleptic calendar.
!
!     Formally, both calendars can be extended indefinitely forward
!     and backward in time due the algorithmic nature of the
!     determination of calendar representation.
!
!     This routine allows you to take a numeric representation of
!     an epoch represented in the Gregorian calendar and
!     convert that to an epoch in the Julian calendar.
!
!     To convert from Julian Calendar to Gregorian
!     calendar, use the entry point JUL2GR.
!
! $ Examples
!
!     Suppose you need to print an epoch (given in seconds past the
!     J2000 epoch) of some ancient epoch that occurred during
!     pre-Christian era, and that you want to represent this epoch
!     using the Julian proleptic calendar.
!
!     Here's the declarations we'll need
!
!        INTEGER               YEAR
!        INTEGER               MONTH
!        INTEGER               DAY
!        INTEGER               DOY
!
!        DOUBLE PRECISION      TVEC ( 6 )
!        DOUBLE PRECISION      TDB
!
!     You first need to convert TDB (the epoch in Seconds past J2000)
!     to a calendar representation.
!
!        TVEC(1) = TDB.
!
!        CALL TTRANS ( 'TDB', 'YMD', TVEC )
!
!     The output time vector will be relative to the Gregorian
!     Calendar. Collect the year, month and day from the time
!     vector.
!
!        YEAR    = INT ( TVEC(1) )
!        MONTH   = INT ( TVEC(2) )
!        DAY     = INT ( TVEC(3) )
!
!     The hours, minutes and seconds appear in components 4 through 6
!     of the time vector. We can ignore them in the conversion
!     of the calendar from Gregorian to Julian.
!
!        CALL GR2JUL ( YEAR, MONTH, DAY, DOY )
!
!     Now create a string from the YEAR, MONTH, DAY and TVEC(4)
!     through TVEC(6).
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
!-    SPICELIB Version 1.1.1, 02-OCT-2021 (JDR)
!
!        Reordered header sections. Edited the header to comply with
!        NAIF standard.
!
!-    SPICELIB Version 1.1.0, 23-FEB-1998 (WLT)
!
!        The routine was upgraded so that it will handle without
!        error months that are outside the range from 1 to 12.
!
!-    SPICELIB Version 1.0.0, 13-MAR-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Convert from Gregorian to Julian Calendar
!
!-&


!
!     If this is the first pass through this entry point (or the
!     companion entry point) we need to set up some reference points.
!
!     RDAYG   is the number of days past 1 A.D. Jan 1 of the Gregorian
!             calendar of the date Oct 15, 1582
!
!     RDAYJ   is the number of days past 1 A.D. Jan 1 of the Julian
!             calendar of the date Oct 5, 1582.
!
!     OFFSTJ and OFFSTG are just the offset from one count of days
!     to the other.
!
      IF ( FIRST ) THEN

         FIRST  = .FALSE.

         RDAYG  = GDP001 ( 1582, 10, 15 )
         RDAYJ  = JDP001 ( 1582, 10,  5 )
         OFFSTJ = RDAYJ - RDAYG
         OFFSTG = RDAYG - RDAYJ

      END IF
!
!     Make Local Copies of YEAR, MONTH and DAY and get YEAR into
!     a positive range.
!
      CALL RMAINI ( MONTH-1, 12, YROFF, MON )

      YR  = YEAR + YROFF
      MON = MON  + 1
      DY  = DAY

      IF ( YR .LE. 0 ) THEN

         CALL RMAINI ( YR, 400, M400, TMPYR )
         YR = TMPYR

         IF ( YR .EQ. 0 ) THEN
            YR   = YR + 400
            M400 = M400 - 1
         END IF

         OFFSET = M400 * DP400Y

      ELSE

         OFFSET = 0

      END IF

!
!     First get the day number (Gregorian) for the input
!     year month and day.
!
      DAYG = GDP001 ( YR, MON, DY  ) + OFFSET

!
!     This day is DAYG - RDAYG days after 1582 Oct 15 on the
!     Gregorian calendar.  But this is the same as the number
!     of days past 1582 Oct 5 on the Julian Calendar
!     So the Julian day number is DAYG - RDAYG + RDAYJ
!     which is the same as DAYG + OFFSTJ.
!
      DAYJ = DAYG + OFFSTJ
!
!     Now that we have the Julian day number it's a fairly
!     straight forward task to get the year, month and day
!     on the Julian calendar.
!

      CALL RMAINI ( DAYJ, DP4Y,  M4,  TMPDAY )
      DAYJ = TMPDAY

      M    = MIN ( 3, DAYJ/DP1Y )
      DAYJ = DAYJ -      M*DP1Y


      DOFYR  = DAYJ + 1
      YR     = 4 * M4 &
             +     M &
             +     1

!
!     Now look up the month number and compute the day of the month.
!     How we do this depends on whether or not this is a leap year.
!
      IF ( JLDAYS(YR) .EQ. 0 ) THEN
         MON  = LSTLTI ( DOFYR, 12, DPJAN0 )
         DY   = DOFYR  -            DPJAN0(MON)
      ELSE
         MON  = LSTLTI ( DOFYR, 12, DPBEGL )
         DY   = DOFYR  -            DPBEGL(MON)
      END IF

      YEAR    = YR
      MONTH   = MON
      DAY     = DY
      DOY     = DOFYR

      RETURN
      END
