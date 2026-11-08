!$Procedure ZZGETELM ( Get the components from two-line elements)

      SUBROUTINE ZZGETELM ( FRSTYR, LINES, EPOCH, ELEMS, OK, ERROR )

! $ Abstract
!
!    Given a the "lines" of a two-line element set, parse the
!    lines and return the elements in units suitable for use
!    in SPICE software.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PARSING
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               FRSTYR
      CHARACTER*(*)         LINES ( 2 )
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      ELEMS ( * )
      LOGICAL               OK
      CHARACTER*(*)         ERROR

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FRSTYR     I   year of earliest representable two-line elements
!     LINES      I   a pair of "lines" containing two-line elements
!     EPOCH      O   The epoch of the elements in seconds past J2000
!     ELEMS      O   The elements converted to SPICE units.
!     OK         O   Boolean indicating error state.
!     ERROR      O   String describing error.
!
! $ Detailed_Input
!
!     FRSTYR    is the first year possible for two line elements.
!               Since two line elements allow only two digits for
!               the year, some conventions must be followed concerning
!               which century the two digits refer to .  FRSTYR
!               is the year of the earliest representable elements.
!               The two-digit year is mapped to the year in
!               the interval from FRSTYR to FRSTYR + 99 that
!               has the same last two digits as the two digit
!               year in the element set.  For example if FRSTYR
!               is set to 1960  then the two digit years are mapped
!               as shown in the table below:
!
!               Two-line         Maps to
!               element year
!                  00            2000
!                  01            2001
!                  02            2002
!                   .              .
!                   .              .
!                   .              .
!                  58            2058
!                  59            2059
!                 --------------------
!                  60            1960
!                  61            1961
!                  62            1962
!                   .              .
!                   .              .
!                   .              .
!                  99            1999
!
!                Note that if Space Command should decide to represent
!                years in 21st century as 100 + the last two digits
!                of the year (for example: 2015 is represented as 115)
!                instead of simply dropping the first two digits of
!                the year, this routine will correctly map the year
!                as long as you set FRSTYR to some value between 1900
!                and 1999.
!
!     LINES      is a pair of lines of text that comprise a Space
!                command ``two-line element'' set.  These text lines
!                should be the same as they are presented in the
!                two-line element files available from Space Command
!                (formerly NORAD). Below is an example of a two-line
!                set for TOPEX.
!
!  TOPEX
!  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594
!  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550
!
!
! $ Detailed_Output
!
!     EPOCH      is the epoch of the two line elements supplied via
!                the input array LINES.  Epoch is returned in TDB
!                seconds past J2000.
!
!     ELEMS      is an array containing the elements from the two line
!                set supplied via the array LINES.  The elements are
!                in units suitable for use by the SPICE routine
!                EV2LIN.
!
!                Also note that the elements XNDD6O and BSTAR
!                incorporate the exponential factor present in the
!                input two line elements in LINES.  (See particulars
!                below.
!
!                    ELEMS (  1 ) = XNDT2O in radians/minute**2
!                    ELEMS (  2 ) = XNDD6O in radians/minute**3
!                    ELEMS (  3 ) = BSTAR
!                    ELEMS (  4 ) = XINCL  in radians
!                    ELEMS (  5 ) = XNODEO in radians
!                    ELEMS (  6 ) = EO
!                    ELEMS (  7 ) = OMEGAO in radians
!                    ELEMS (  8 ) = XMO    in radians
!                    ELEMS (  9 ) = XNO    in radians/minute
!                    ELEMS ( 10 ) = EPOCH of the elements in seconds
!                                   past ephemeris epoch J2000.
!
!     OK         a boolean flag indicating whether an error occured
!                while processing the TLE. Processing errors include
!                incorrect format for TLEs, angular values beyond
!                allowed range.
!
!     ERROR      a string containing a description of any TLE
!                processing error.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     You must have loaded a SPICE leapseconds kernel into the
!     kernel pool prior to caling this routine.
!
! $ Exceptions
!
!     This routine does not signal errors, rather it returns an
!     error flag and description to the calling program. Errors
!     reported:
!
!     1) TLE line has incorrect format.
!
!     2) A TLE '2' line has a different vehicle tag than the
!         corresponding '1' line.
!
!     3) TLE data length more than 69 characters, or less than 68
!        characters.
!
!     4) An NPARS* routine cannot parse a string to a numeric value.
!
!     5) The value of an angular measure, NODE0, OMEGA, MO, or INCL
!        fails to lie within the expected numerical bounds.
!
! $ Particulars
!
!     This routine parses a Space Command Two-line element set and
!     returns the orbital elements properly scaled and in units
!     suitable for use by other SPICE software.  Input elements
!     have the form:
!
!  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594
!  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550
!  ^
!  123456789012345678901234567890123456789012345678901234567890123456789
!           1         2         3         4         5         6
!
!     The ``raw'' elements in the first  and second lines are marked
!     below.  Note that in several instances exponents and decimal
!     points are implied.  Also note that
!     input units are degrees, degrees/day**n and revolutions/day.
!
!
!                      DAY OF YEAR             NDD60    BSTAR
!                      vvvvvvvvvvvv            vvvvvv   vvvvvv
!  ---------------------------------------------------------------------
!  1 22076U 92052A   97173.53461370 -.00000038  00000-0  10000-3 0   594
!  ---------------------------------------------------------------------
!                    ^^             ^^^^^^^^^^       ^^       ^^
!                    YEAR             NDT20          IEXP     IBEXP
!
!
!
!     The ``raw'' elements in the second line are marked below
!                   NODE0            OMEGA             N0
!                   vvvvvvvv         vvvvvvvv          vvvvvvvvvvv
!  ---------------------------------------------------------------------
!  2 22076  66.0378 163.4372 0008359 278.7732  81.2337 12.80930736227550
!  ---------------------------------------------------------------------
!          ^^^^^^^^          ^^^^^^^          ^^^^^^^^
!          Inclination       Eccentricity     M0
!
!     This routine extracts these values ``inserts'' the implied
!     decimal points and exponents and then converts the inputs
!     to units of radians, radians/minute, radians/minute**2, and
!     radians/minute**3
!
! $ Examples
!
!     Suppose you have a set of two-line elements and an array
!     containing the related geophysical constants necessary
!     to evaluate a state.  The example below shows how you
!     can use this routine together with the routine EV2LIN to
!     propagate a state to an epoch of interest.
!
!
!        The parameters below will make it easier to make assignments
!        to the array GEOPHS required by EV2LIN.
!
!        J2  --- location of J2
!        J3  --- location of J3
!        J4  --- location if J4
!        KE  --- location of KE = sqrt(GM) in eart-radii**1.5/MIN
!        QO  --- location of upper bound of atmospheric model in KM
!        SO  --- location of lower bound of atmospheric model in KM
!        ER  --- location of earth equatorial radius in KM.
!        AE  --- location of distance units/earth radius
!
!        PARAMETER           ( J2 = 1 )
!        PARAMETER           ( J3 = 2 )
!        PARAMETER           ( J4 = 3 )
!        PARAMETER           ( KE = 4 )
!        PARAMETER           ( QO = 5 )
!        PARAMETER           ( SO = 6 )
!        PARAMETER           ( ER = 7 )
!        PARAMETER           ( AE = 8 )
!
!
!        We set the lower bound for the years to be the beginning
!        of the space age.
!
!        FRSTYR = 1957
!
!        Read in the next two lines from the text file that contains
!        the two-line elements.  We assume that file has been opened
!        properly and that we have set the ``file pointer'' to the
!        correct location for reading the next set of elements.
!
!        READ  (UNIT,FMT='(A)' ) LINE(1)
!        READ  (UNIT,FMT='(A)' ) LINE(2)
!
!        CALL ZZGETELM ( FRSTYR, LINE, EPOCH, ELEMS, OK, ERROR )
!
!        Set up the geophysical quantities.  At last check these
!        were the values used by Space Command.
!
!        GEOPHS( J2 ) =    1.082616D-3
!        GEOPHS( J3 ) =   -2.53881D-6
!        GEOPHS( J4 ) =   -1.65597D-6
!        GEOPHS( KE ) =    7.43669161D-2
!        GEOPHS( QO ) =  120.0D0
!        GEOPHS( SO ) =   78.0D0
!        GEOPHS( ER ) = 6378.135D0
!        GEOPHS( AE ) =    1.0D0
!
!        Now propagate the state using EV2LIN to the epoch of
!        interest.
!
!        CALL EV2LIN ( ET, GEOPHS, ELEMS, STATE )
!
!
! $ Restrictions
!
!    The format of the two-line elements suffer from a "millenium"
!    problem---only two digits are used for the year of the elements.
!    It is not clear how Space Command will deal with this problem
!    as the year 2000 comes and goes.  We hope that by adjusting
!    the input FRSTYR you should be able to use this
!    routine well into the 21st century.  However, since we can't
!    predict how others will resolve the millenium problem we
!    can't be sure that our approach will be addequate to deal with
!    the problem.
!
!    The approach taken to mapping the two-digit year to the
!    full year is given by the code below. Here, YR is the
!    integer obtained by parsing the two-digit year from the first
!    line of the elements.
!
!        BEGYR = (FRSTYR/100)*100
!        YEAR  = BEGYR + YR
!
!        IF ( YEAR .LT. FRSTYR ) THEN
!           YEAR = YEAR + 100
!        END IF
!
!     This mapping will be changed if future two-line element
!     representations make this method of computing the full year
!     inaccurate.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     E.D. Wright     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!
!-    SPICELIB Version 1.0.0, 26-APR-2004 (EDW)
!
!        Modified routine GETELM to confirm
!        acceptable range for angular measures and exponents.
!        The routine does not signal errors,it returns an error
!        flag and error string to the calling program.
!
!        Routine named ZZGETELM from GETELM.
!
!-&

! $ Index_Entries
!
!     Parse two-line elements
!
!-&

!
!     Spicelib functions
!
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI
      LOGICAL               RETURN
      INTEGER               LASTNB

!
!     An enumeration of the various components of the
!     elements array---ELEMS
!
!        KNDT20
!        KNDD60
!        KBSTAR
!        KINCL
!        KNODE0
!        KECC
!        KOMEGA
!        KMO
!        KNO
!

      INTEGER               START
      PARAMETER           ( START = 0 )

      INTEGER               KNDT20
      PARAMETER           ( KNDT20 = START  + 1 )

      INTEGER               KNDD60
      PARAMETER           ( KNDD60 = KNDT20 + 1 )

      INTEGER               KBSTAR
      PARAMETER           ( KBSTAR = KNDD60 + 1 )

      INTEGER               KINCL
      PARAMETER           ( KINCL  = KBSTAR + 1 )

      INTEGER               KNODE0
      PARAMETER           ( KNODE0 = KINCL  + 1 )

      INTEGER               KECC
      PARAMETER           ( KECC   = KNODE0 + 1 )

      INTEGER               KOMEGA
      PARAMETER           ( KOMEGA = KECC   + 1 )

      INTEGER               KMO
      PARAMETER           ( KMO    = KOMEGA + 1 )

      INTEGER               KNO
      PARAMETER           ( KNO    = KMO    + 1 )

      INTEGER               KEPOCH
      PARAMETER           ( KEPOCH = KNO    + 1 )

      INTEGER               NELEMS
      PARAMETER           ( NELEMS = KEPOCH )

!
!     Character string lengths
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 160 )

!
!     Maximum exponent (base 10)
!
      INTEGER               MAXP
      PARAMETER           ( MAXP =     37     )

      INTEGER               MINP
      PARAMETER           ( MINP =  -MAXP     )

!
!     Double precision constants.
!
      DOUBLE PRECISION      ONE
      PARAMETER           ( ONE  =      1.0D0 )

      DOUBLE PRECISION      TEN
      PARAMETER           ( TEN  =     10.0D0 )

!
!     Minutes/day
!
      DOUBLE PRECISION      MNPDAY
      PARAMETER           ( MNPDAY = 1440.0D0 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ERRPRS ( 13 )
      CHARACTER*(LNSIZE)    TERM  ( 13 )

      CHARACTER*(WDSIZE)    CYEAR
      CHARACTER*(WDSIZE)    CDAY
      CHARACTER*(WDSIZE)    CNDT20
      CHARACTER*(WDSIZE)    CNDD60
      CHARACTER*(WDSIZE)    CIEXP
      CHARACTER*(WDSIZE)    CBSTAR
      CHARACTER*(WDSIZE)    CIBEXP
      CHARACTER*(WDSIZE)    CINCL
      CHARACTER*(WDSIZE)    CNODE0
      CHARACTER*(WDSIZE)    CECC
      CHARACTER*(WDSIZE)    COMEGA
      CHARACTER*(WDSIZE)    CMO
      CHARACTER*(WDSIZE)    CNO

      DOUBLE PRECISION      BSTAR
      DOUBLE PRECISION      DAY
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      INCL
      DOUBLE PRECISION      MO
      DOUBLE PRECISION      NDD60
      DOUBLE PRECISION      NDT20
      DOUBLE PRECISION      NO
      DOUBLE PRECISION      NODE0
      DOUBLE PRECISION      OMEGA
      DOUBLE PRECISION      D2R
      DOUBLE PRECISION      PI2
      DOUBLE PRECISION      POWER  ( MINP:MAXP )
      DOUBLE PRECISION      TVEC   ( 8 )



      INTEGER               BEXP
      INTEGER               BEGYR
      INTEGER               I
      INTEGER               NEXP
      INTEGER               PTR
      INTEGER               YEAR
      INTEGER               YR
      INTEGER               K

      LOGICAL               FIRST

      SAVE

      DATA                  FIRST / .TRUE. /

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGETELM' )

!
!     Initialize the error indicators and the elements to zero.
!
      OK               = .TRUE.
      ERROR            = ' '
      ELEMS ( KNDT20 ) = 0.D0
      ELEMS ( KNDD60 ) = 0.D0
      ELEMS ( KBSTAR ) = 0.D0
      ELEMS ( KINCL  ) = 0.D0
      ELEMS ( KNODE0 ) = 0.D0
      ELEMS ( KECC   ) = 0.D0
      ELEMS ( KOMEGA ) = 0.D0
      ELEMS ( KMO    ) = 0.D0
      ELEMS ( KNO    ) = 0.D0
      ELEMS ( KEPOCH ) = 0.D0
      EPOCH            = 0.D0


!
!     First entry initialization.
!
      IF ( FIRST ) THEN

!
!        Define two constants. This initialization proves the most
!        useful when processing thousands of TLE sets.
!
         D2R = RPD()
         PI2 = TWOPI()

         FIRST    = .FALSE.

         POWER(0) =  ONE

         DO I = 1, MAXP
            POWER(I)  = TEN * POWER(I-1)
            POWER(-I) = ONE / POWER(I  )
         END DO

         TERM( 1) = '"YEAR" (characters 19 to 20 of the first line ' &
         //         'of a two-line element set)'

         TERM( 2) = '"DAY" (characters 21 to 32 of the first line o' &
         //         'f a two-line element set)'

         TERM( 3) = '"NDT20" (characters 34 to 43 of the first line' &
         //         ' of a two-line element set)'

         TERM( 4) = '"NDD60" (characters 45 to 45 of the first line' &
         //         ' of a two-line element set)'

         TERM( 5) = '"IEXP" (characters 51 to 52 of the first line ' &
         //         'of a two-line element set)'

         TERM( 6) = '"BSTAR" (characters 54 to 54 of the first line' &
         //         ' of a two-line element set)'

         TERM( 7) = '"IBEXP" (characters 60 to 61 of the first line' &
         //         ' of a two-line element set)'

         TERM( 8) = '"INCL" (characters 9 to 16 of the second line o' &
         //         'f a two-line element set)'

         TERM( 9) = '"NODE0" (characters 18 to 25 of the second line' &
         //         ' of a two-line element set)'

         TERM(10) = '"ECC" (characters 27 to 33 of the second line o' &
         //         'f a two-line element set)'

         TERM(11) = '"OMEGA" (characters 35 to 42 of the second line' &
         //         ' of a two-line element set)'

         TERM(12) = '"MO" (characters 44 to 51 of the second line of' &
         //         ' a two-line element set)'

         TERM(13) = '"NO" (characters 53 to 63 of the second line of' &
         //         ' a two-line element set)'

      END IF

!
!     Ensure the vehicle IDs match in each line.
!
      IF ( LINES(1)(2:7) .NE. LINES(2)(2:7) ) THEN

!
!        Vehicle IDs do not match. Flag an error.
!
         ERROR = 'Line 1 of the TLE pair tagged with vehicle ID #1,' &
            //   '  line 2 of TLE pair tagged with vehicle ID #2'
         CALL REPMC ( ERROR, '#1', LINES(1)(2:7), ERROR )
         CALL REPMC ( ERROR, '#2', LINES(2)(2:7), ERROR )

         OK = .FALSE.

         CALL CHKOUT ( 'ZZGETELM' )
         RETURN
      END IF

!
!    Check line format and length.
!
      DO K = 1, 2

         IF ( LASTNB( LINES(K) ) .NE. 68 .AND. &
              LASTNB( LINES(K) ) .NE. 69 ) THEN

!
!          The TLE data line was not 68 or 69 characters long (ignoring
!          trailing whitespace). Flag an error.
!
            ERROR = 'Line #1 of the TLE has incorrect data length.' &
               //   ' Expected length 68 or 69 elements, ' &
               //   'actual length: #2. TLE line value: #3'
            CALL REPMI ( ERROR, '#1', K                 , ERROR )
            CALL REPMI ( ERROR, '#2', LASTNB( LINES(K) ), ERROR )
            CALL REPMC ( ERROR, '#3', LINES(K)          , ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN
         END IF

      END DO

!
!     This isn't particularly pretty, but it is straight
!     forward.  According to the documentation on the two line
!     element sets, (as well as what's indicated by the program
!     driver that is documented in SPACETRACK REPORT NO.3
!     we can simply pick out the various components of the
!     elements from the input lines 1 and 2.
!
!     For the record we include the DECODE statement in DRIVER
!     for fetching the data out of lines one and two (after a bit
!     of pretty printing).  Note that some of these formats have
!     and implied decimal point.  In particular f6.5 and f7.7  in
!     all other cases the decimal points seem to be given explicitely.
!
!     decode (abuf,702) epoch,     xndt20,                       ...
!                       xndd60,    iexp,        bstar,    ibexp, ...
!                       xincl,     xnodeo,                       ...
!                       eo,        omegao,     xmo,       xno
!     format(      18x, d14.8, 1x, f10.8,
!                   1x, f6.5,      i2,     1x, f6.5,      i2,    /,
!                   8x, f8.4,  1x, f8.4,
!                   1x, f7.7,  1x, f8.4,   1x, f8.4   1x, f11.8 )
!
!     Note that in the two-line element sets, the epoch is read
!     as a single number.  However the documentation that describes
!     this data (as well as the code in THETAG) show that it's a lot
!     easier to capture the year and day of year separately.
!
      CYEAR  = LINES(1)(19:20)
      CDAY   = LINES(1)(21:32)
      CNDT20 = LINES(1)(34:43)
      CNDD60 = LINES(1)(45:45) // '.' // LINES(1)(46:50)
      CIEXP  = LINES(1)(51:52)
      CBSTAR = LINES(1)(54:54) // '.' // LINES(1)(55:59)
      CIBEXP = LINES(1)(60:61)

      CINCL  = LINES(2)( 9:16)
      CNODE0 = LINES(2)(18:25)
      CECC   = '0.'            // LINES(2)(27:33)
      COMEGA = LINES(2)(35:42)
      CMO    = LINES(2)(44:51)
      CNO    = LINES(2)(53:63)


!
!     Parse the numerical values from the data string.
!
      CALL NPARSI ( CYEAR,  YR,     ERRPRS(1),  PTR )
      CALL NPARSD ( CDAY,   DAY,    ERRPRS(2),  PTR )
      CALL NPARSD ( CNDT20, NDT20,  ERRPRS(3),  PTR )
      CALL NPARSD ( CNDD60, NDD60,  ERRPRS(4),  PTR )
      CALL NPARSI ( CIEXP,  NEXP,   ERRPRS(5),  PTR )
      CALL NPARSD ( CBSTAR, BSTAR,  ERRPRS(6),  PTR )
      CALL NPARSI ( CIBEXP, BEXP,   ERRPRS(7),  PTR )
      CALL NPARSD ( CINCL,  INCL,   ERRPRS(8),  PTR )
      CALL NPARSD ( CNODE0, NODE0,  ERRPRS(9),  PTR )
      CALL NPARSD ( CECC,   ECC,    ERRPRS(10), PTR )
      CALL NPARSD ( COMEGA, OMEGA,  ERRPRS(11), PTR )
      CALL NPARSD ( CMO,    MO,     ERRPRS(12), PTR )
      CALL NPARSD ( CNO,    NO,     ERRPRS(13), PTR )

!
!     Check for parse errors.
!
      DO I = 1, 13

         IF ( ERRPRS(I) .NE. ' ' ) THEN

!
!           Something could not parse. Set the error message then
!           return.
!
            ERROR = 'An error occurred while trying to ' &
            //      'parse the term #. The diagnostic was:  # '
            CALL REPMC ( ERROR, '#', TERM(I)  , ERROR )
            CALL REPMC ( ERROR, '#', ERRPRS(I), ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN
         END IF

      END DO


!
!     Check for reasonable exponets; a single digit. These should
!     probably be LE 0.
!
      IF ( ABS(NEXP) .GT. 9 ) THEN

            ERROR = 'NEXP (exponent) not a single digit. ' &
                    //'Actual value #1'
            CALL REPMI ( ERROR, '#1', NEXP, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN

         END IF

      IF ( ABS(BEXP) .GT. 9 ) THEN

            ERROR = 'BEXP (exponent) not a single digit. ' &
                    //'Actual value #1'
            CALL REPMI ( ERROR, '#1', BEXP, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN

      END IF

!
!     Confirm correct bounds on angular values.
!
!     NODE0 - right ascension of the ascending node, [0,360)
!
      IF ( NODE0 .LT. 0.D0 .OR. NODE0 .GE. 360.D0 ) THEN

            ERROR = 'NODE0 (RA acend node) expected bounds [0,360). ' &
                    //'Actual value #1'
            CALL REPMD ( ERROR, '#1', NODE0, 4, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN

         END IF

!
!     OMEAGA - argument of the periapsis, [0,360)
!
      IF ( OMEGA .LT. 0.D0 .OR. OMEGA .GE. 360.D0 ) THEN

            ERROR = 'OMEGA (arg periap) expected bounds [0,360). ' &
                    //'Actual value #1'
            CALL REPMD ( ERROR, '#1', OMEGA, 4, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN
         END IF

!
!     MO - mean anomoly, [0,360)
!
      IF ( MO .LT. 0.D0 .OR. MO .GE. 360.D0 ) THEN

            ERROR = 'MO (mean anomoly) expected bounds [0,360). ' &
                    //'Actual value #1'
            CALL REPMD ( ERROR, '#1', MO, 4, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN
         END IF

!
!     INCL - inclination, [0,180]
!
      IF ( INCL .LT. 0.D0 .OR. INCL .GT. 180.D0 ) THEN

            ERROR = 'INCL (inclination) expected bounds [0,180). ' &
                    //'Actual value #1'
            CALL REPMD ( ERROR, '#1', INCL, 4, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN
         END IF


!
!     NO - mean motion (0,20) (Earth orbiter).
!
      IF ( NO .GT. 20.D0 .OR. NO .LT. 0.D0 ) THEN

            ERROR = 'NO (mean motion) expected bounds (0,20). ' &
                    //'Actual value #1'
            CALL REPMD ( ERROR, '#1', NO, 4, ERROR )

            OK = .FALSE.

            CALL CHKOUT ( 'ZZGETELM' )
            RETURN
         END IF




!
!     Finish up the computation of NDD60 and BSTAR
!
      NDD60 = NDD60 * POWER(NEXP)
      BSTAR = BSTAR * POWER(BEXP)

!
!     Convert everything from degrees to radians ...
!
      NODE0 = NODE0  * D2R
      OMEGA = OMEGA  * D2R
      MO    = MO     * D2R
      INCL  = INCL   * D2R

!
!     ... and from revolutions/day**n to radians/minutes**n
!
      NO    =  NO    * PI2/MNPDAY
      NDT20 =  NDT20 * PI2/MNPDAY/MNPDAY
      NDD60 =  NDD60 * PI2/MNPDAY/MNPDAY/MNPDAY

!
!     Finally, we need to convert the input epoch to
!     seconds past 2000. First let's adjust the year.
!     Add to YR the largest multiple of 100 that is
!     less than or equal to FRSTYR
!
      BEGYR = (FRSTYR/100)*100
      YEAR  = BEGYR + YR

      IF ( YEAR .LT. FRSTYR ) THEN
         YEAR = YEAR + 100
      END IF

!
!     Compute the epoch of the year and date.
!
      TVEC(1) = DBLE(YEAR)
      TVEC(2) = DAY

      CALL TTRANS ( 'YD.D', 'TDB', TVEC )

      EPOCH = TVEC(1)

!
!     That's it.  Load ELEMS with the elements and ship them
!     back to the calling routine.
!
      ELEMS ( KNDT20 ) = NDT20
      ELEMS ( KNDD60 ) = NDD60
      ELEMS ( KBSTAR ) = BSTAR
      ELEMS ( KINCL  ) = INCL
      ELEMS ( KNODE0 ) = NODE0
      ELEMS ( KECC   ) = ECC
      ELEMS ( KOMEGA ) = OMEGA
      ELEMS ( KMO    ) = MO
      ELEMS ( KNO    ) = NO
      ELEMS ( KEPOCH ) = EPOCH

      CALL CHKOUT ( 'ZZGETELM' )
      RETURN
      END

