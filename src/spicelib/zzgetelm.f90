!$Procedure ZZGETELM ( Get the components from two-line elements)

      subroutine zzgetelm ( frstyr, lines, epoch, elems, ok, error )
      use pepper_kinds, only: ip, wp

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

      implicit none

      integer(ip)           frstyr
      character(*)          lines ( 2 )
      real(wp)              epoch
      real(wp)              elems ( * )
      logical               ok
      character(*)          error

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
      real(wp)              rpd
      real(wp)              twopi
      logical               return
      integer(ip)           lastnb

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

      integer(ip)           start
      parameter           ( start = 0 )

      integer(ip)           kndt20
      parameter           ( kndt20 = start  + 1 )

      integer(ip)           kndd60
      parameter           ( kndd60 = kndt20 + 1 )

      integer(ip)           kbstar
      parameter           ( kbstar = kndd60 + 1 )

      integer(ip)           kincl
      parameter           ( kincl  = kbstar + 1 )

      integer(ip)           knode0
      parameter           ( knode0 = kincl  + 1 )

      integer(ip)           kecc
      parameter           ( kecc   = knode0 + 1 )

      integer(ip)           komega
      parameter           ( komega = kecc   + 1 )

      integer(ip)           kmo
      parameter           ( kmo    = komega + 1 )

      integer(ip)           kno
      parameter           ( kno    = kmo    + 1 )

      integer(ip)           kepoch
      parameter           ( kepoch = kno    + 1 )

      integer(ip)           nelems
      parameter           ( nelems = kepoch )

!
!     Character string lengths
!
      integer(ip)           wdsize
      parameter           ( wdsize = 32 )

      integer(ip)           lnsize
      parameter           ( lnsize = 160 )

!
!     Maximum exponent (base 10)
!
      integer(ip)           maxp
      parameter           ( maxp =     37     )

      integer(ip)           minp
      parameter           ( minp =  -maxp     )

!
!     Double precision constants.
!
      real(wp)              one
      parameter           ( one  =      1.0_wp )

      real(wp)              ten
      parameter           ( ten  =     10.0_wp )

!
!     Minutes/day
!
      real(wp)              mnpday
      parameter           ( mnpday = 1440.0_wp )

!
!     Local variables
!
      character(lnsize)     errprs ( 13 )
      character(lnsize)     term  ( 13 )

      character(wdsize)     cyear
      character(wdsize)     cday
      character(wdsize)     cndt20
      character(wdsize)     cndd60
      character(wdsize)     ciexp
      character(wdsize)     cbstar
      character(wdsize)     cibexp
      character(wdsize)     cincl
      character(wdsize)     cnode0
      character(wdsize)     cecc
      character(wdsize)     comega
      character(wdsize)     cmo
      character(wdsize)     cno

      real(wp)              bstar
      real(wp)              day
      real(wp)              ecc
      real(wp)              incl
      real(wp)              mo
      real(wp)              ndd60
      real(wp)              ndt20
      real(wp)              no
      real(wp)              node0
      real(wp)              omega
      real(wp)              d2r
      real(wp)              pi2
      real(wp)              power  ( minp:maxp )
      real(wp)              tvec   ( 8 )



      integer(ip)           bexp
      integer(ip)           begyr
      integer(ip)           i
      integer(ip)           nexp
      integer(ip)           ptr
      integer(ip)           year
      integer(ip)           yr
      integer(ip)           k

      logical               first

      save

      data                  first / .true. /

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'ZZGETELM' )

!
!     Initialize the error indicators and the elements to zero.
!
      ok               = .true.
      error            = ' '
      elems ( kndt20 ) = 0.0_wp
      elems ( kndd60 ) = 0.0_wp
      elems ( kbstar ) = 0.0_wp
      elems ( kincl  ) = 0.0_wp
      elems ( knode0 ) = 0.0_wp
      elems ( kecc   ) = 0.0_wp
      elems ( komega ) = 0.0_wp
      elems ( kmo    ) = 0.0_wp
      elems ( kno    ) = 0.0_wp
      elems ( kepoch ) = 0.0_wp
      epoch            = 0.0_wp


!
!     First entry initialization.
!
      if ( first ) then

!
!        Define two constants. This initialization proves the most
!        useful when processing thousands of TLE sets.
!
         d2r = rpd()
         pi2 = twopi()

         first    = .false.

         power(0) =  one

         do i = 1, maxp
            power(i)  = ten * power(i-1)
            power(-i) = one / power(i  )
         end do

         term( 1) = '"YEAR" (characters 19 to 20 of the first line ' &
         //         'of a two-line element set)'

         term( 2) = '"DAY" (characters 21 to 32 of the first line o' &
         //         'f a two-line element set)'

         term( 3) = '"NDT20" (characters 34 to 43 of the first line' &
         //         ' of a two-line element set)'

         term( 4) = '"NDD60" (characters 45 to 45 of the first line' &
         //         ' of a two-line element set)'

         term( 5) = '"IEXP" (characters 51 to 52 of the first line ' &
         //         'of a two-line element set)'

         term( 6) = '"BSTAR" (characters 54 to 54 of the first line' &
         //         ' of a two-line element set)'

         term( 7) = '"IBEXP" (characters 60 to 61 of the first line' &
         //         ' of a two-line element set)'

         term( 8) = '"INCL" (characters 9 to 16 of the second line o' &
         //         'f a two-line element set)'

         term( 9) = '"NODE0" (characters 18 to 25 of the second line' &
         //         ' of a two-line element set)'

         term(10) = '"ECC" (characters 27 to 33 of the second line o' &
         //         'f a two-line element set)'

         term(11) = '"OMEGA" (characters 35 to 42 of the second line' &
         //         ' of a two-line element set)'

         term(12) = '"MO" (characters 44 to 51 of the second line of' &
         //         ' a two-line element set)'

         term(13) = '"NO" (characters 53 to 63 of the second line of' &
         //         ' a two-line element set)'

      end if

!
!     Ensure the vehicle IDs match in each line.
!
      if ( lines(1)(2:7) /= lines(2)(2:7) ) then

!
!        Vehicle IDs do not match. Flag an error.
!
         error = 'Line 1 of the TLE pair tagged with vehicle ID #1,' &
            //   '  line 2 of TLE pair tagged with vehicle ID #2'
         call repmc ( error, '#1', lines(1)(2:7), error )
         call repmc ( error, '#2', lines(2)(2:7), error )

         ok = .false.

         call chkout ( 'ZZGETELM' )
         return
      end if

!
!    Check line format and length.
!
      do k = 1, 2

         if ( lastnb( lines(k) ) /= 68 .and. &
              lastnb( lines(k) ) /= 69 ) then

!
!          The TLE data line was not 68 or 69 characters long (ignoring
!          trailing whitespace). Flag an error.
!
            error = 'Line #1 of the TLE has incorrect data length.' &
               //   ' Expected length 68 or 69 elements, ' &
               //   'actual length: #2. TLE line value: #3'
            call repmi ( error, '#1', k                 , error )
            call repmi ( error, '#2', lastnb( lines(k) ), error )
            call repmc ( error, '#3', lines(k)          , error )

            ok = .false.

            call chkout ( 'ZZGETELM' )
            return
         end if

      end do

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
      cyear  = lines(1)(19:20)
      cday   = lines(1)(21:32)
      cndt20 = lines(1)(34:43)
      cndd60 = lines(1)(45:45) // '.' // lines(1)(46:50)
      ciexp  = lines(1)(51:52)
      cbstar = lines(1)(54:54) // '.' // lines(1)(55:59)
      cibexp = lines(1)(60:61)

      cincl  = lines(2)( 9:16)
      cnode0 = lines(2)(18:25)
      cecc   = '0.'            // lines(2)(27:33)
      comega = lines(2)(35:42)
      cmo    = lines(2)(44:51)
      cno    = lines(2)(53:63)


!
!     Parse the numerical values from the data string.
!
      call nparsi ( cyear,  yr,     errprs(1),  ptr )
      call nparsd ( cday,   day,    errprs(2),  ptr )
      call nparsd ( cndt20, ndt20,  errprs(3),  ptr )
      call nparsd ( cndd60, ndd60,  errprs(4),  ptr )
      call nparsi ( ciexp,  nexp,   errprs(5),  ptr )
      call nparsd ( cbstar, bstar,  errprs(6),  ptr )
      call nparsi ( cibexp, bexp,   errprs(7),  ptr )
      call nparsd ( cincl,  incl,   errprs(8),  ptr )
      call nparsd ( cnode0, node0,  errprs(9),  ptr )
      call nparsd ( cecc,   ecc,    errprs(10), ptr )
      call nparsd ( comega, omega,  errprs(11), ptr )
      call nparsd ( cmo,    mo,     errprs(12), ptr )
      call nparsd ( cno,    no,     errprs(13), ptr )

!
!     Check for parse errors.
!
      do i = 1, 13

         if ( errprs(i) /= ' ' ) then

!
!           Something could not parse. Set the error message then
!           return.
!
            error = 'An error occurred while trying to ' &
            //      'parse the term #. The diagnostic was:  # '
            call repmc ( error, '#', term(i)  , error )
            call repmc ( error, '#', errprs(i), error )

            ok = .false.

            call chkout ( 'ZZGETELM' )
            return
         end if

      end do


!
!     Check for reasonable exponets; a single digit. These should
!     probably be LE 0.
!
      if ( abs(nexp) > 9 ) then

            error = 'NEXP (exponent) not a single digit. ' &
                    //'Actual value #1'
            call repmi ( error, '#1', nexp, error )

            ok = .false.

            call chkout ( 'ZZGETELM' )
            return

         end if

      if ( abs(bexp) > 9 ) then

            error = 'BEXP (exponent) not a single digit. ' &
                    //'Actual value #1'
            call repmi ( error, '#1', bexp, error )

            ok = .false.

            call chkout ( 'ZZGETELM' )
            return

      end if

!
!     Confirm correct bounds on angular values.
!
!     NODE0 - right ascension of the ascending node, [0,360)
!
      if ( node0 < 0.0_wp .or. node0 >= 360.0_wp ) then

            error = 'NODE0 (RA acend node) expected bounds [0,360). ' &
                    //'Actual value #1'
            call repmd ( error, '#1', node0, 4, error )

            ok = .false.

            call chkout ( 'ZZGETELM' )
            return

         end if

!
!     OMEAGA - argument of the periapsis, [0,360)
!
      if ( omega < 0.0_wp .or. omega >= 360.0_wp ) then

            error = 'OMEGA (arg periap) expected bounds [0,360). ' &
                    //'Actual value #1'
            call repmd ( error, '#1', omega, 4, error )

            ok = .false.

            call chkout ( 'ZZGETELM' )
            return
         end if

!
!     MO - mean anomoly, [0,360)
!
      if ( mo < 0.0_wp .or. mo >= 360.0_wp ) then

            error = 'MO (mean anomoly) expected bounds [0,360). ' &
                    //'Actual value #1'
            call repmd ( error, '#1', mo, 4, error )

            ok = .false.

            call chkout ( 'ZZGETELM' )
            return
         end if

!
!     INCL - inclination, [0,180]
!
      if ( incl < 0.0_wp .or. incl > 180.0_wp ) then

            error = 'INCL (inclination) expected bounds [0,180). ' &
                    //'Actual value #1'
            call repmd ( error, '#1', incl, 4, error )

            ok = .false.

            call chkout ( 'ZZGETELM' )
            return
         end if


!
!     NO - mean motion (0,20) (Earth orbiter).
!
      if ( no > 20.0_wp .or. no < 0.0_wp ) then

            error = 'NO (mean motion) expected bounds (0,20). ' &
                    //'Actual value #1'
            call repmd ( error, '#1', no, 4, error )

            ok = .false.

            call chkout ( 'ZZGETELM' )
            return
         end if




!
!     Finish up the computation of NDD60 and BSTAR
!
      ndd60 = ndd60 * power(nexp)
      bstar = bstar * power(bexp)

!
!     Convert everything from degrees to radians ...
!
      node0 = node0  * d2r
      omega = omega  * d2r
      mo    = mo     * d2r
      incl  = incl   * d2r

!
!     ... and from revolutions/day**n to radians/minutes**n
!
      no    =  no    * pi2/mnpday
      ndt20 =  ndt20 * pi2/mnpday/mnpday
      ndd60 =  ndd60 * pi2/mnpday/mnpday/mnpday

!
!     Finally, we need to convert the input epoch to
!     seconds past 2000. First let's adjust the year.
!     Add to YR the largest multiple of 100 that is
!     less than or equal to FRSTYR
!
      begyr = (frstyr/100)*100
      year  = begyr + yr

      if ( year < frstyr ) then
         year = year + 100
      end if

!
!     Compute the epoch of the year and date.
!
      tvec(1) = real(year, wp)
      tvec(2) = day

      call ttrans ( 'YD.D', 'TDB', tvec )

      epoch = tvec(1)

!
!     That's it.  Load ELEMS with the elements and ship them
!     back to the calling routine.
!
      elems ( kndt20 ) = ndt20
      elems ( kndd60 ) = ndd60
      elems ( kbstar ) = bstar
      elems ( kincl  ) = incl
      elems ( knode0 ) = node0
      elems ( kecc   ) = ecc
      elems ( komega ) = omega
      elems ( kmo    ) = mo
      elems ( kno    ) = no
      elems ( kepoch ) = epoch

      call chkout ( 'ZZGETELM' )
      return
      end

