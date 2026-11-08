!$Procedure TTRANS ( Time transformation )

      SUBROUTINE TTRANS ( FROM, TO, TVEC )

! $ Abstract
!
!     Transform a time vector from one representation and system
!     to another.
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
!     PARSING
!     TIME
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      INTEGER               MXCOMP
      PARAMETER           ( MXCOMP = 10 )

      CHARACTER*(*)         FROM
      CHARACTER*(*)         TO
      DOUBLE PRECISION      TVEC ( MXCOMP )

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
      LOGICAL               RETURN

      INTEGER               BSRCHC
      INTEGER               LSTLEI
      INTEGER               LSTLED
      INTEGER               LSTLTI

      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      UNITIM

      LOGICAL               ODD
      LOGICAL               ELEMC


!
!     Local (in-line) functions
!
      INTEGER               DIVBLE
      INTEGER               DOY
      INTEGER               DP0001
      INTEGER               LDAYS
      INTEGER               YDAYS

      DOUBLE PRECISION      HMSSEC

!
!     Local parameters
!
!
!     Parameters
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

!
!     MAXLP is the maximum number of leap seconds that can be
!     stored internally.   The value of 140 should be sufficient
!     to store leap seconds through the year 2100.
!
      INTEGER               MAXLP
      PARAMETER           ( MAXLP = 140 )

!
!     MAXVAR is the number of kernel pool variables required by this
!     routine.
!
      INTEGER               MAXVAR
      PARAMETER           ( MAXVAR = 1 )

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

      INTEGER               DAYSEC
      PARAMETER           ( DAYSEC = 1 )

      INTEGER               DAYP2
      PARAMETER           ( DAYP2 = DAYSEC + 1 )

      INTEGER               ET
      PARAMETER           ( ET     = DAYP2 + 1 )

      INTEGER               FRML
      PARAMETER           ( FRML = ET     + 1 )

      INTEGER               JDTDB
      PARAMETER           ( JDTDB  = FRML + 1 )

      INTEGER               JDTDT
      PARAMETER           ( JDTDT  = JDTDB  + 1 )

      INTEGER               JDUTC
      PARAMETER           ( JDUTC  = JDTDT  + 1 )

      INTEGER               JED
      PARAMETER           ( JED    = JDUTC  + 1 )

      INTEGER               TTAI
      PARAMETER           ( TTAI   = JED    + 1 )

      INTEGER               TDB
      PARAMETER           ( TDB    = TTAI   + 1 )

      INTEGER               TDT
      PARAMETER           ( TDT    = TDB    + 1 )

      INTEGER               YD
      PARAMETER           ( YD     = TDT    + 1 )

      INTEGER               YDD
      PARAMETER           ( YDD    = YD     + 1 )

      INTEGER               YDDF
      PARAMETER           ( YDDF   = YDD    + 1 )

      INTEGER               YDF
      PARAMETER           ( YDF    = YDDF   + 1 )

      INTEGER               YMD
      PARAMETER           ( YMD    = YDF    + 1 )

      INTEGER               YMDF
      PARAMETER           ( YMDF   = YMD    + 1 )

      INTEGER               YMWD
      PARAMETER           ( YMWD   = YMDF   + 1 )

      INTEGER               YMWDF
      PARAMETER           ( YMWDF  = YMWD   + 1 )

      INTEGER               YWD
      PARAMETER           ( YWD    = YMWDF  + 1 )

      INTEGER               YWDF
      PARAMETER           ( YWDF   = YWD    + 1 )

      INTEGER               NREC
      PARAMETER           ( NREC   = YWDF )
!
!     The following parameters just make the code seem a bit
!     more natural.
!
      LOGICAL               NO
      PARAMETER           ( NO  = .FALSE. )

      LOGICAL               YES
      PARAMETER           ( YES = .TRUE. )

!
!     Local variables
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )


      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN =  8 )

      CHARACTER*(TYPLEN)    UNIFRM (LBCELL : NREC )

      CHARACTER*(TYPLEN)    RECOG   ( NREC )
      INTEGER               PARSED  ( NREC )
      LOGICAL               NEEDY   ( NREC )
      LOGICAL               FORML   ( NREC )
      INTEGER               ORDVEC  ( NREC )

      CHARACTER*(32)        REST
      CHARACTER*(32)        MYFROM
      CHARACTER*(32)        MYTO
      CHARACTER*(32)        VARS   ( MAXVAR )

      DOUBLE PRECISION      DAYDP
      DOUBLE PRECISION      DAYLEN
      DOUBLE PRECISION      DP2000
      DOUBLE PRECISION      DT
      DOUBLE PRECISION      EXSECS
      DOUBLE PRECISION      FORMAL
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      HALFD
      DOUBLE PRECISION      HOURS
      DOUBLE PRECISION      JD1101
      DOUBLE PRECISION      JDSECS
      DOUBLE PRECISION      LASTDT
      DOUBLE PRECISION      MINS
      DOUBLE PRECISION      SECS
      DOUBLE PRECISION      SECSPD
      DOUBLE PRECISION      TAI
      DOUBLE PRECISION      TAITAB ( 2*MAXLP )
      DOUBLE PRECISION      TEMPD
      DOUBLE PRECISION      TSECS

      INTEGER               DAY
      INTEGER               DAYNUM
      INTEGER               DAYPTR
      INTEGER               DAYTAB ( 2*MAXLP )
      INTEGER               DN2000
      INTEGER               DOFFST
      INTEGER               DOFYR
      INTEGER               DPSUN
      INTEGER               DYEAR
      INTEGER               FMDAY
      INTEGER               FYRDAY
      INTEGER               I
      INTEGER               MONTH
      INTEGER               NREF
      INTEGER               OFFSET
      INTEGER               PFROM
      INTEGER               PTO
      INTEGER               QINT
      INTEGER               REFPTR
      INTEGER               REM
      INTEGER               SUNDAY
      INTEGER               TAIPTR
      INTEGER               TEMPI
      INTEGER               USRCTR ( CTRSIZ )
      INTEGER               WEEK
      INTEGER               WKDAY
      INTEGER               YEAR
      INTEGER               YR1
      INTEGER               YR100
      INTEGER               YR4
      INTEGER               YR400

      LOGICAL               FIRST
      LOGICAL               FOUND
      LOGICAL               NODATA
      LOGICAL               UPDATE

!
!     The array EXTRA contains the number of many additional days that
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

      DATA                  NODATA  / .TRUE. /


!
!     Definitions of statement functions.
!
!     The number of days elapsed since Jan 1, of year 1 A.D. to
!     Jan 1 of YEAR is given by:
!
      YDAYS(YEAR) = 365*(YEAR-1) &
                  +    ((YEAR-1)/4) &
                  -    ((YEAR-1)/100) &
                  +    ((YEAR-1)/400)

!
!     Return 1 if YEAR is divisible by N, otherwise return 0.
!
      DIVBLE(YEAR,I) = MAX ( 0, 1 + ((IABS(YEAR)/I)*I) - IABS(YEAR) )

!
!     The number of leap days in a year is given by:
!
      LDAYS(YEAR) = DIVBLE( YEAR, 4   ) &
                  - DIVBLE( YEAR, 100 ) &
                  + DIVBLE( YEAR, 400 )

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
      DOY(YEAR,MONTH,DAY) = DPJAN0 ( MONTH ) &
                          + EXTRA  ( MONTH ) * LDAYS(YEAR) &
                          + DAY


!
!     The number of days since 1 Jan 1 A.D. is given by:
!
      DP0001(YEAR,MONTH,DAY) = YDAYS(YEAR) &
                             + DOY  (YEAR, MONTH, DAY) &
                             - 1


!
!     The number of seconds represented by HOURS hours MINS minutes
!     and SECS seconds.
!
      HMSSEC ( HOURS, MINS, SECS ) = HOURS * 3600.0D0 &
                                   + MINS  *   60.0D0 &
                                   + SECS





      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'TTRANS' )
      END IF

!
!     The first time any of the entry points are called we
!     must set up the "watcher" for the kernel pool variables
!     that will be needed by this routine.
!
      IF ( FIRST ) THEN

         FIRST   = .FALSE.

         SECSPD  = SPD()
         HALFD   = SPD() / 2.0D0

         DN2000  = DP0001 ( 2000, 1, 1 )
         SUNDAY  = DP0001 ( 1991, 1, 6 )

         JD1101  = J2000() - DBLE(DN2000) - 0.5D0

!
!        Initialize the list of Uniform time systems.
!
         CALL SSIZEC ( NREC, UNIFRM )

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
         RECOG ( 01 ) = 'DAYSEC '
         PARSED( 01 ) =  DAYSEC
         FORML ( 01 ) =  NO
         NEEDY ( 01 ) =  NO

         RECOG ( 02 ) = 'DP2000 '
         PARSED( 02 ) =  DAYP2
         FORML ( 02 ) =  NO
         NEEDY ( 02 ) =  NO

         RECOG ( 03 ) = 'ET '
         PARSED( 03 ) =  ET
         FORML ( 03 ) =  NO
         NEEDY ( 03 ) =  NO

         CALL INSRTC ( 'ET', UNIFRM )

         RECOG ( 04 ) = 'FORMAL '
         PARSED( 04 ) =  FRML
         FORML ( 04 ) =  YES
         NEEDY ( 04 ) =  NO

         RECOG ( 05 ) = 'JDTDB '
         PARSED( 05 ) =  JDTDB
         FORML ( 05 ) =  NO
         NEEDY ( 05 ) =  NO

         CALL INSRTC ( 'JDTDB', UNIFRM )

         RECOG ( 06 ) = 'JDTDT '
         PARSED( 06 ) =  JDTDT
         FORML ( 06 ) =  NO
         NEEDY ( 06 ) =  NO

         CALL INSRTC ( 'JDTDT', UNIFRM )

         RECOG ( 07 ) = 'JDUTC '
         PARSED( 07 ) =  JDUTC
         FORML ( 07 ) =  YES
         NEEDY ( 07 ) =  NO

         RECOG ( 08 ) = 'JED '
         PARSED( 08 ) =  JED
         FORML ( 08 ) =  NO
         NEEDY ( 08 ) =  NO

         CALL INSRTC ( 'JED', UNIFRM )

         RECOG ( 09 ) = 'TAI '
         PARSED( 09 ) =  TTAI
         FORML ( 09 ) =  NO
         NEEDY ( 09 ) =  NO

         CALL INSRTC ( 'TAI', UNIFRM )

         RECOG ( 10 ) = 'TDB '
         PARSED( 10 ) =  TDB
         FORML ( 10 ) =  NO
         NEEDY ( 10 ) =  NO

         CALL INSRTC ( 'TDB', UNIFRM )

         RECOG ( 11 ) = 'TDT '
         PARSED( 11 ) =  TDT
         FORML ( 11 ) =  NO
         NEEDY ( 11 ) =  NO

         CALL INSRTC ( 'TDT', UNIFRM )

         RECOG ( 12 ) = 'YD '
         PARSED( 12 ) =  YD
         FORML ( 12 ) =  NO
         NEEDY ( 12 ) =  YES

         RECOG ( 13 ) = 'YD.D '
         PARSED( 13 ) =  YDD
         FORML ( 13 ) =  NO
         NEEDY ( 13 ) =  YES

         RECOG ( 14 ) = 'YD.DF '
         PARSED( 14 ) =  YDDF
         FORML ( 14 ) =  YES
         NEEDY ( 14 ) =  YES

         RECOG ( 15 ) = 'YDF '
         PARSED( 15 ) =  YDF
         FORML ( 15 ) =  YES
         NEEDY ( 15 ) =  YES

         RECOG ( 16 ) = 'YMD '
         PARSED( 16 ) =  YMD
         FORML ( 16 ) =  NO
         NEEDY ( 16 ) =  YES

         RECOG ( 17 ) = 'YMDF '
         PARSED( 17 ) =  YMDF
         FORML ( 17 ) =  YES
         NEEDY ( 17 ) =  YES

         RECOG ( 18 ) = 'YMWD '
         PARSED( 18 ) =  YMWD
         FORML ( 18 ) =  NO
         NEEDY ( 18 ) =  YES

         RECOG ( 19 ) = 'YMWDF '
         PARSED( 19 ) =  YMWDF
         FORML ( 19 ) =  YES
         NEEDY ( 19 ) =  YES

         RECOG ( 20 ) = 'YWD '
         PARSED( 20 ) =  YWD
         FORML ( 20 ) =  NO
         NEEDY ( 20 ) =  YES

         RECOG ( 21 ) = 'YWDF '
         PARSED( 21 ) =  YWDF
         FORML ( 21 ) =  YES
         NEEDY ( 21 ) =  YES

         CALL ORDERC ( RECOG,  NREC, ORDVEC )
         CALL REORDC ( ORDVEC, NREC, RECOG  )
         CALL REORDI ( ORDVEC, NREC, PARSED )
         CALL REORDL ( ORDVEC, NREC, FORML  )
         CALL REORDL ( ORDVEC, NREC, NEEDY  )

!
!        Initialize the local POOL counter to user value.
!
         CALL ZZCTRUIN( USRCTR )

!
!        Set up the kernel pool watchers
!
         VARS(1) = 'DELTET/DELTA_AT'

         CALL SWPOOL ( 'TTRANS', 1, VARS )

      END IF

!
!     Check to see if any of the kernel items required by this
!     routine have been updated since the last call to this
!     entry point.
!
      CALL ZZCVPOOL ( 'TTRANS', USRCTR, UPDATE )

      IF ( UPDATE .OR. NODATA ) THEN
!
!        We load the TAI-UTC offsets and formal leapsecond epochs
!        into the TAITAB.  (We will modify this array in a minute).
!
         CALL GDPOOL ( 'DELTET/DELTA_AT',  1,       2*MAXLP, &
                       NREF,               TAITAB,  FOUND    )

!
!        Make sure all of the requested data was there.
!
         IF ( .NOT. FOUND ) THEN

            NODATA = .TRUE.

            CALL SETMSG ( 'The variable that points to the '          // &
                          'leapseconds (DELTET/DELTA_AT) could not '  // &
                          'be located in the kernel pool.  It is '    // &
                          'likely that the leapseconds kernel has '   // &
                          'not been loaded.'                          )
            CALL SIGERR ( 'SPICE(NOLEAPSECONDS)'                      )
            CALL CHKOUT ( 'TTRANS'                                    )
            RETURN

         END IF


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
         LASTDT = TAITAB(1) - 1.0D0

         DO I = 1, NREF, 2

            OFFSET         = I
            REFPTR         = I + 1

            DT             = TAITAB(OFFSET)
            FORMAL         = TAITAB(REFPTR)
            TAITAB(OFFSET) = FORMAL - SECSPD + LASTDT
            TAITAB(REFPTR) = FORMAL          + DT

            DAYNUM         = INT ( ( FORMAL + HALFD ) / SECSPD ) &
                           + DN2000

            DAYTAB(OFFSET) = DAYNUM - 1
            DAYTAB(REFPTR) = DAYNUM

            LASTDT         = DT

         END DO

!
!        Since we don't have to do it very often, make sure the
!        times in the TAI table are in increasing order.
!
         DO I = 2, NREF

            NODATA = .TRUE.

            IF ( TAITAB(I-1) .GE. TAITAB(I) ) THEN
               CALL SETMSG ( 'Either the leapsecond epochs taken '    // &
                             'from the kernel pool are not properly ' // &
                             'ordered or the UTC - TAI offsets '      // &
                             'are completely out of range. '          )
               CALL SIGERR ( 'SPICE(BADLEAPSECONDS)'                  )
               CALL CHKOUT ( 'TTRANS'                                 )
               RETURN
            END IF

         END DO

!
!        At this point, we've completed all checks on kernel data.
!
         NODATA = .FALSE.

      END IF

!
!     Make local normalized copies of FROM and TO.
!
      CALL NEXTWD ( FROM,   MYFROM, REST )
      CALL NEXTWD ( TO,     MYTO,   REST )
      CALL UCASE  ( MYFROM, MYFROM       )
      CALL UCASE  ( MYTO,   MYTO         )

!
!     Make sure that the FROM and TO are recognized time types.
!
      PTO   = BSRCHC ( MYTO,   NREC, RECOG )
      PFROM = BSRCHC ( MYFROM, NREC, RECOG )

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
      IF ( PFROM .EQ. 0 ) THEN

         CALL SETMSG ( 'The FROM time representation ''#'' is ' &
         //            'not recognized. ' )
         CALL ERRCH  ( '#', FROM                   )
         CALL SIGERR ( 'SPICE(UNKNONWNTIMESYSTEM)' )
         CALL CHKOUT ( 'TTRANS'                    )
         RETURN

      ELSE IF ( PTO .EQ. 0 ) THEN

         CALL SETMSG ( 'The TO time representation ''#'' is ' &
         //            'not recognized. ' )
         CALL ERRCH  ( '#', FROM                   )
         CALL SIGERR ( 'SPICE(UNKNONWNTIMESYSTEM)' )
         CALL CHKOUT ( 'TTRANS'                    )
         RETURN

      END IF

!
!     OK.  We have made our last attempt at diagnosing a user error.
!     From this point on we assume that the user input exactly what
!     was intended.
!
!     We convert the time vector to days past 1 jan 01 and seconds
!     past the beginning of the day.  None of the cases below
!     are particularly tricky.  There's just a lot of cases.
!
      IF ( (PFROM .EQ. YMD) .OR. (PFROM .EQ. YMDF) ) THEN

         YEAR   = INT( TVEC(1) )
         MONTH  = INT( TVEC(2) )
         DAY    = INT( TVEC(3) )

         CALL RMAINI ( MONTH-1, 12, DYEAR, MONTH )

         YEAR   = YEAR  + DYEAR
         MONTH  = MONTH + 1
         DOFFST = 0

         IF ( YEAR .LE. 0 ) THEN

            CALL RMAINI ( YEAR, 400, YR400, TEMPI )
            YEAR = TEMPI

            IF ( YEAR .EQ. 0 ) THEN
               YEAR  = YEAR + 400
               YR400 = YR400 - 1
            END IF

            DOFFST = DP400Y * YR400

         END IF

         DAYNUM = DP0001 ( YEAR,          MONTH,         DAY     ) &
                + DOFFST

!
!        Calculate seconds from midnight, 00:00:00.
!
         SECS   = HMSSEC( TVEC(4), TVEC(5), TVEC(6) )

      ELSE IF ( PFROM .EQ. YD  .OR. PFROM .EQ. YDF ) THEN

         YEAR   = INT( TVEC(1) )
         DAY    = INT( TVEC(2) )
         MONTH  = 1

         DOFFST = 0

         IF ( YEAR .LE. 0 ) THEN

            CALL RMAINI ( YEAR, 400, YR400, TEMPI )
            YEAR = TEMPI

            IF ( YEAR .EQ. 0 ) THEN
               YEAR  = YEAR + 400
               YR400 = YR400 - 1
            END IF

            DOFFST = DP400Y * YR400

         END IF

         DAYNUM = DP0001 ( YEAR,          MONTH,         DAY     ) &
                + DOFFST

!
!        Calculate seconds from midnight, 00:00:00.
!
         SECS   = HMSSEC ( TVEC(3), TVEC(4), TVEC(5) )

      ELSE IF ( PFROM .EQ. YDD .OR. PFROM .EQ. YDDF   ) THEN

         YEAR   = INT(TVEC(1))
         DAY    = INT(TVEC(2))
         MONTH  = 1

         DOFFST = 0

         IF ( YEAR .LE. 0 ) THEN

            CALL RMAINI ( YEAR, 400, YR400, TEMPI )
            YEAR = TEMPI

            IF ( YEAR .EQ. 0 ) THEN
               YEAR  = YEAR + 400
               YR400 = YR400 - 1
            END IF

            DOFFST = DP400Y * YR400

         END IF

         FRAC   = TVEC(2) - DBLE(DAY)
         DAYNUM = DP0001  ( YEAR, MONTH, DAY ) &
                + DOFFST

!
!        Normally the length of a day is 86400 seconds, but this day
!        might be a leapsecond day.  We will set DAYLEN to SECSPD and
!        change it if it turns out this is a day with a leapsecond.
!
         IF ( PFROM .EQ. YDDF ) THEN
            SECS = FRAC * SECSPD
         ELSE
            DAYLEN = SECSPD
            DAYPTR = LSTLEI ( DAYNUM, NREF, DAYTAB )

            IF ( ODD(DAYPTR) ) THEN
               DAYLEN = TAITAB(DAYPTR+1) - TAITAB(DAYPTR)
            END IF

            SECS = FRAC * DAYLEN
         END IF


      ELSE IF ( PFROM .EQ. FRML ) THEN

!
!        First lets get the number of days since 1-Jan-2000 00:00:00
!
         CALL RMAIND ( TVEC(1) + HALFD, SECSPD, DP2000, SECS )

         DAYNUM = INT(DP2000) + DN2000



      ELSE IF ( PFROM .EQ. JDUTC  ) THEN

!
!        JD1101 is the julian date UTC of Jan 1, 1 AD.
!
         JDSECS = (TVEC(1) - JD1101)*SECSPD

         CALL RMAIND ( JDSECS, SECSPD, DAYDP, SECS )

         DAYNUM = INT(DAYDP)



      ELSE IF ( PFROM .EQ. DAYSEC ) THEN

         DAYNUM = INT(TVEC(1))
         SECS   = TVEC(2)



      ELSE IF ( PFROM .EQ. DAYP2 ) THEN

         DAYNUM = INT(TVEC(1)) + DN2000
         SECS   = TVEC(2)



      ELSE IF ( (PFROM .EQ. YWD) .OR. (PFROM .EQ. YWDF) ) THEN

         YEAR   = INT( TVEC(1) )
         WEEK   = INT( TVEC(2) ) - 1
         WKDAY  = INT( TVEC(3) )
         MONTH  = 1

!
!        Compute the days past 1 jan 1 of the beginning of this
!        year and month.
!

         DOFFST = 0

         IF ( YEAR .LE. 0 ) THEN

            CALL RMAINI ( YEAR, 400, YR400, TEMPI )
            YEAR = TEMPI

            IF ( YEAR .EQ. 0 ) THEN
               YEAR  = YEAR + 400
               YR400 = YR400 - 1
            END IF

            DOFFST = DP400Y * YR400

         END IF

         DAYNUM = DP0001 ( YEAR, MONTH, 1 ) &
                + DOFFST

         CALL RMAINI ( DAYNUM - SUNDAY, 7, QINT, DPSUN )

         FYRDAY = DPSUN + 1

         CALL RMAINI ( WKDAY - FYRDAY, 7,  QINT, OFFSET )

         DAYNUM = DAYNUM + WEEK*7 + OFFSET

!
!        Calculate seconds from midnight, 00:00:00.
!
         SECS   = HMSSEC ( TVEC(4), TVEC(5), TVEC(6) )

      ELSE IF ( PFROM .EQ. YMWD .OR. PFROM .EQ. YMWDF ) THEN

         YEAR   = INT( TVEC(1) )
         MONTH  = INT( TVEC(2) )
         WEEK   = INT( TVEC(3) ) - 1
         DAY    = INT( TVEC(4) )

         DOFFST = 0

         IF ( YEAR .LE. 0 ) THEN

            CALL RMAINI ( YEAR, 400, YR400, TEMPI )
            YEAR = TEMPI

            IF ( YEAR .EQ. 0 ) THEN
               YEAR  = YEAR + 400
               YR400 = YR400 - 1
            END IF

            DOFFST = DP400Y * YR400

         END IF

         DAYNUM = DP0001 ( YEAR, MONTH, 1 ) &
                + DOFFST

         CALL RMAINI ( DAYNUM - SUNDAY, 7, QINT, DPSUN  )
         FMDAY  = DPSUN + 1

         CALL RMAINI ( DAY - FMDAY, 7, QINT, OFFSET )

         DAYNUM = DAYNUM + WEEK*7 + OFFSET

!
!        Calculate seconds from midnight, 00:00:00.
!
         SECS   = HMSSEC ( TVEC(5), TVEC(6), TVEC(7) )




!
!     If we get to this point the type must be one of the continuous
!     time types: 'TAI', 'TDT', 'TDB', 'JED', 'ET', 'JDTDT', 'JDTDB'.
!
      ELSE
!
!        If the output time is one of the continuous time systems
!        we can take a short cut and just perform the computation
!        directly.
!
         IF ( ELEMC ( MYTO, UNIFRM ) ) THEN

            TVEC(1) = UNITIM ( TVEC(1), MYFROM, MYTO )
            CALL CHKOUT ( 'TTRANS' )
            RETURN

         END IF

!
!        The output time system isn't one of the uniform time systems.
!        Convert what we have to TAI and then to the DAYNUM, SECOND
!        representation.
!
         TAI    = UNITIM ( TVEC(1), MYFROM, 'TAI'   )
         TAIPTR = LSTLED ( TAI,     NREF,    TAITAB )

!
!        If the TAIPTR value is odd, then the TAI time falls during
!        a day with a leap second.  We can just look up the day
!        number and compute the number of seconds into that
!        day directly ...
!
         IF ( ODD(TAIPTR) ) THEN

            DAYNUM = DAYTAB (TAIPTR)
            SECS   = TAI &
                   - TAITAB (TAIPTR)

!
!        ...Otherwise, all days since the reference TAI time have
!        the same number of seconds (SECSPD).  (This statement applies
!        to days that precede the first reference TAI time too.)
!        Thus we can simply compute the number of days and seconds
!        that have elapsed since the reference TAI time.
!
         ELSE

!
!           If TAI is before the first time in the table, we can
!           compute the number of days and seconds before the first
!           entry in the TAI table.
!
            TAIPTR = MAX ( TAIPTR, 1)

            CALL  RMAIND ( TAI     - TAITAB(TAIPTR), SECSPD, &
                           DAYDP,                    SECS    )

            DAYNUM = INT ( DAYDP ) + DAYTAB(TAIPTR)

         END IF


      END IF

      IF ( FORML ( PFROM )  ) THEN

         CALL RMAIND ( SECS, SECSPD, DAYDP, TSECS )
         DAYNUM = DAYNUM + INT(DAYDP)
         SECS   = TSECS

      END IF


!     ==================================================================
!
!     Force the seconds into the range 0 to 86401 or 86400
!     depending upon whether or not the output system is a formal
!     time system or not.
!
      IF ( FORML(PTO) .AND. FORML(PFROM) ) THEN
!
!        We don't have to do anything here.
!
      ELSE

         IF (      (SECS .GT. (SECSPD - 1.0D0)) &
              .OR. (SECS .LT. 0.0D0)          ) THEN

!
!           First convert to TAI...
!
            DAYPTR = MAX( 1, LSTLEI ( DAYNUM, NREF, DAYTAB ) )
            SECS   = SECS &
                   + DBLE ( DAYNUM - DAYTAB(DAYPTR) ) * SECSPD
            TAI    = TAITAB(DAYPTR) + SECS

!
!           ...then back to DAYNUM and SECS
!
            TAIPTR = LSTLED ( TAI, NREF, TAITAB )

            IF ( ODD(TAIPTR) ) THEN

               DAYNUM = DAYTAB ( TAIPTR )
               SECS   = TAI &
                      - TAITAB ( TAIPTR )

            ELSE

               TAIPTR = MAX ( 1, TAIPTR )
               DAYNUM = DAYTAB ( TAIPTR )

               CALL RMAIND ( TAI - TAITAB(TAIPTR), SECSPD, DAYDP, SECS )

               DAYNUM = DAYNUM + INT ( DAYDP )

            END IF
         END IF

      END IF
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

      IF ( FORML(PTO) .AND. SECS .GT. SECSPD ) THEN
         DAYNUM = DAYNUM + 1
         SECS   = 0
      END IF
!
!     OK. Now we have DAYNUM and SECS,  convert this form to the
!     one requested.
!
!     If there is a 'Y' in the form we are to convert to, then we
!     will need some form of year, etc.  Do the work now and sort it
!     it all out at the appropriate time later on.
!
      IF ( NEEDY ( PTO )  ) THEN

         YR400  =  DAYNUM / DP400Y
         REM    =  DAYNUM - DP400Y*YR400
!
!        We want to be able to deal with years prior to  1 Jan 1
!        So we make sure the remainder is positive.
!
         IF ( REM .LT. 0 ) THEN
            YR400 = YR400 - 1
            REM   = REM + DP400Y
         END IF

         YR100  =  MIN ( 3, REM   / DP100Y )
         REM    =  REM    - YR100 * DP100Y

         YR4    =  MIN (24, REM / DP4Y )
         REM    =  REM    - YR4 * DP4Y

         YR1    =  MIN (3,  REM / DP1Y )
         REM    =  REM    - YR1 * DP1Y

         DOFYR  =  REM    + 1

         YEAR   = YR400*400 + YR100*100 + YR4*4 + YR1 + 1

         IF ( LDAYS(YEAR) .EQ. 0 ) THEN
            MONTH  = LSTLTI ( DOFYR, 12, DPJAN0 )
            DAY    = DOFYR - DPJAN0(MONTH)
         ELSE
            MONTH  = LSTLTI ( DOFYR, 12, DPBEGL )
            DAY    = DOFYR - DPBEGL(MONTH)
         END IF


!
!        We only want to convert that portion of seconds less than
!        86399 to hours, minutes and seconds.  Take anything extra
!        and put it in EXSECS.
!
         EXSECS = MAX ( 0.0D0, SECS - SECSPD + 1 )
         TSECS  = SECS - EXSECS

         CALL RMAIND ( TSECS, 3600.0D0, HOURS, TEMPD )
         CALL RMAIND ( TEMPD,   60.0D0, MINS,  TSECS )

         TSECS  = TSECS + EXSECS

      END IF

!=====================================================================
!
!     Finally, we convert to the requested output.
!
      IF      ( PTO .EQ. YMD .OR. PTO .EQ. YMDF ) THEN

         TVEC(1) = DBLE ( YEAR  )
         TVEC(2) = DBLE ( MONTH )
         TVEC(3) = DBLE ( DAY   )
         TVEC(4) = HOURS
         TVEC(5) = MINS
         TVEC(6) = TSECS

      ELSE IF ( PTO .EQ. YD .OR. PTO .EQ. YDF  ) THEN

         TVEC(1) = DBLE ( YEAR  )
         TVEC(2) = DBLE ( DOFYR )
         TVEC(3) = HOURS
         TVEC(4) = MINS
         TVEC(5) = TSECS

      ELSE IF ( PTO .EQ. YDD .OR. PTO .EQ. YDDF   ) THEN

         TVEC(1) = DBLE   ( YEAR  )

         IF ( PTO .EQ. YDD ) THEN
            DAYPTR  = LSTLEI ( DAYNUM, NREF, DAYTAB )
            DAYLEN  = SECSPD

            IF ( ODD(DAYPTR) ) THEN
               DAYLEN = TAITAB(DAYPTR+1) - TAITAB(DAYPTR)
            END IF

            TVEC(2) = DBLE(DOFYR) + SECS/DAYLEN
         ELSE
            TVEC(2) = DBLE(DOFYR) + SECS/SECSPD
         END IF


      ELSE IF ( PTO .EQ. FRML ) THEN

         TVEC(1) = DBLE ( DAYNUM - DN2000 ) * SECSPD &
                 - HALFD &
                 + SECS

      ELSE IF ( PTO .EQ. JDUTC  ) THEN

         TVEC(1) = JD1101 + DBLE( DAYNUM ) + SECS/SECSPD

      ELSE IF ( PTO .EQ. DAYSEC ) THEN

         TVEC(1) = DBLE(DAYNUM)
         TVEC(2) = SECS

      ELSE IF ( PTO .EQ. DAYP2 ) THEN

         TVEC(1) = DBLE (DAYNUM - DN2000)
         TVEC(2) = SECS

      ELSE IF ( PTO  .EQ. YWD .OR. PTO .EQ. YWDF ) THEN
!
!        First compute the day of the week, and the week number
!
         CALL   RMAINI  ( DAYNUM - SUNDAY, 7, QINT, DAY )
         WEEK    = 1 + (( DOFYR  - 1 )   / 7)

!
!        Now just put everything where it belongs.
!
         TVEC(1) = DBLE(YEAR)
         TVEC(2) = DBLE(WEEK)
         TVEC(3) = DBLE(DAY) + 1.0D0
         TVEC(4) = HOURS
         TVEC(5) = MINS
         TVEC(6) = TSECS

      ELSE IF ( PTO .EQ. YMWD .OR. PTO .EQ. YMWDF ) THEN

!
!        First compute how many weeks into the month DAYNUM is,
!        and compute the day of week number.
!
         TVEC(1) = DBLE (YEAR)
         DOFFST  = 0

         IF ( YEAR .LE. 0 ) THEN

            CALL RMAINI ( YEAR, 400, YR400, TEMPI )
            YEAR = TEMPI

            IF ( YEAR .EQ. 0 ) THEN
               YEAR  = YEAR + 400
               YR400 = YR400 - 1
            END IF

            DOFFST = DP400Y * YR400

         END IF

         WEEK    = 1 + ( DAYNUM - DP0001(YEAR, MONTH, 1) - DOFFST ) / 7
         CALL RMAINI   ( DAYNUM - SUNDAY,   7, QINT, DAY )

!
!        Now just move the remaining stuff into TVEC.
!
         TVEC(2) = DBLE (MONTH)
         TVEC(3) = DBLE (WEEK)
         TVEC(4) = DBLE (DAY) + 1.0D0
         TVEC(5) = HOURS
         TVEC(6) = MINS
         TVEC(7) = TSECS

!
!     If we get to this point the type must be one of the continuous
!     time types: 'TAI', 'TDT', 'TDB', 'JED', 'ET', 'JDTDT', 'JDTDB'.
!
!     First convert to TAI and then to the appropriate output type.
!
      ELSE

         DAYPTR  = MAX( 1, LSTLEI ( DAYNUM, NREF, DAYTAB ) )
         SECS    = SECS &
                 + DBLE ( DAYNUM - DAYTAB(DAYPTR) ) * SECSPD
         TAI     = TAITAB(DAYPTR) + SECS

         TVEC(1) = UNITIM ( TAI, 'TAI', MYTO )

      END IF


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

      CALL CHKOUT ( 'TTRANS' )
      RETURN
      END
