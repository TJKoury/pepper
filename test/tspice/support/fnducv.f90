!$Procedure      FNDUCV ( Find unit, class and value. )

      SUBROUTINE FNDUCV ( UNIN, KNOWN, CLASS, VALUE )
      IMPLICIT NONE

! $ Abstract
!
!     Find the class (length, time, angle, mass, charge) and value of
!     1 unit relative to the reference set of units ( radian, km, sec,
!     kg, coulomb).
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
!     CONSTANTS
!     CONVERSION
!     PARSING
!     UNITS
!     UTILITY
!
! $ Declarations


      CHARACTER*(*)         UNIN
      LOGICAL               KNOWN
      INTEGER               CLASS
      DOUBLE PRECISION      VALUE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     UNIN       I   string that may be a primitive unit.
!     KNOWN      O   indicates whether UNIN was recognized.
!     CLASS      O   type of unit (angle, time, length, mass, charge).
!     VALUE      O   the number of these units in 1 reference unit.
!
! $ Detailed_Input
!
!     UNIN       is a string that may be a number or one of the
!                primitive units of angle, time, length, mass or
!                charge.  A list of recognized units are given below.
!                The case of UNIN (upper or lower) is insignificant.
!
! $ Detailed_Output
!
!     KNOWN      is true if UNIN is recognized as a primitive unit,
!                or number.  Otherwise it is .FALSE.
!
!     CLASS      is the type of UNIN if it is recognized.  The class
!                values are:
!
!                   0  for a number
!                   1  for an angle
!                   2  for length
!                   3  for time
!                   4  for mass
!                   5  for charge
!
!                if UNIN is not recognized as belonging to any of these
!                classes, CLASS is assigned the value of -1.
!
!     VALUE      is the value of 1 UNIN in reference units.
!                The reference units are:
!
!                   Number           1
!                   Angle            radians
!                   length           kilometers
!                   time             second
!                   mass             kilogram
!                   charge           coulomb
!
!                if UNIN is not recognized as belonging to any of these
!                classes, VALUE is set to 0.0d0.
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1) This routine is NOT case sensitive.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine examines UNIN and determines if it is a number or
!     recognized unit of angle, length, time, mass or charge.  If
!     it is recognized it sets a logical variable to .TRUE. to
!     indicate the recognition.  In addition, it returns the type of
!     object as an integer code: 0 for number, 1 for angle,
!     2 for length, 3 for time and 5 for charge.  Finally it returns
!     the number of fundamental units 1 UNIN is equal to.  The
!     fundamental units for each class of object are:
!
!        number  ---  1
!        angle   ---  radians
!        length  ---  kilometers
!        time    ---  seconds
!        mass    ---  kilograms
!        charge  ---  coulombs
!
!      The routine does not recognize any compound units such as
!      newtons or joules.
!
! $ Examples
!
!      This routine is intended primarily as a utility routine for
!      a more general units conversion routine.
!
! $ Restrictions
!
!      None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 24-MAY-1991 (WLT)
!
!-&



!
!     SPICELIB Functions
!
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      INTEGER               BSRCHC
      LOGICAL               BENUM

!
!
!     Local parameters
!
      INTEGER               USIZE
      PARAMETER           ( USIZE = 32 )

      INTEGER               NUNITS
      PARAMETER           ( NUNITS = 84 )

!
!     These are the various classes of recognized objects.
!
      INTEGER               NUMBER
      PARAMETER           ( NUMBER = 0 )

      INTEGER               ANGLE
      PARAMETER           ( ANGLE  = 1 )

      INTEGER               LENGTH
      PARAMETER           ( LENGTH = 2 )

      INTEGER               TIME
      PARAMETER           ( TIME   = 3 )

      INTEGER               MASS
      PARAMETER           ( MASS   = 4 )

      INTEGER               CHARGE
      PARAMETER           ( CHARGE = 5 )


!
!     The reference values for length will be kilometers
!                          for time   will be seconds
!                          for angles will be radians
!                          for mass   will be kilograms
!                          for charge will be coulombs
!
      DOUBLE PRECISION      KM
      PARAMETER           ( KM = 1.0D0 )

      DOUBLE PRECISION      METER
      PARAMETER           ( METER = 0.001D0 * KM )

      DOUBLE PRECISION      CM
      PARAMETER           ( CM = 0.01D0 * METER )

      DOUBLE PRECISION      MM
      PARAMETER           ( MM = 0.1D0 * CM     )

      DOUBLE PRECISION      INCH
      PARAMETER           ( INCH = 2.54D0 * CM )

      DOUBLE PRECISION      FOOT
      PARAMETER           ( FOOT = 12.0D0 * INCH )

      DOUBLE PRECISION      YARD
      PARAMETER           ( YARD = 3.0D0 * FOOT )

      DOUBLE PRECISION      MILEST
      PARAMETER           ( MILEST = 5280.0D0 * FOOT )

      DOUBLE PRECISION      MILENT
      PARAMETER           ( MILENT = 1852.0D0 * METER )


      DOUBLE PRECISION      AU
!
!     This value will be computed at run time or default to the
!     value given here.
!

      DOUBLE PRECISION      DE200V
      PARAMETER           ( DE200V = 1.4959787066D8  )
!
!      Some of the units are not "defined" quantities.  In such a case
!      a best estimate is provided as of the date of the current version
!      of this routine.  Those estimated quantities are:
!
!         1 AU    --- the astronomical unit  is taken from the JPL
!                     ephemeris DE200.  It is believed to be accurate to
!                     about 40 meters.
!
!         The tropical year is the time from equinox to equinox.  This
!         varies slightly with time.
!
!         1 PARSEC --- is dependent upon the value of the astronomical
!                      unit.
!
!
!     1.0d0 divided by the sin of 1 arc second
!
      DOUBLE PRECISION      SCALE

      DOUBLE PRECISION      PARSEC

      DOUBLE PRECISION      SECOND
      PARAMETER           ( SECOND = 1.0D0 )

      DOUBLE PRECISION      MINUTE
      PARAMETER           ( MINUTE = 60.0D0 * SECOND )

      DOUBLE PRECISION      HOUR
      PARAMETER           ( HOUR = 60.0D0 * MINUTE )

      DOUBLE PRECISION      DAY
      PARAMETER           ( DAY = 24.0D0 * HOUR )

      DOUBLE PRECISION      WEEK
      PARAMETER           ( WEEK = 7.0D0 * DAY )

      DOUBLE PRECISION      JYEAR
      PARAMETER           ( JYEAR = 365.25D0 * DAY )

      DOUBLE PRECISION      TYEAR
      PARAMETER           ( TYEAR = DAY * 365.2421988D0 )

      DOUBLE PRECISION      JCENT
      PARAMETER           ( JCENT = 100.0D0 * JYEAR )

      DOUBLE PRECISION      RADIAN
      PARAMETER           ( RADIAN = 1.0D0 )

      DOUBLE PRECISION      MILRAD
      PARAMETER           ( MILRAD = 0.001D0 * RADIAN )

      DOUBLE PRECISION      MICRAD
      PARAMETER           ( MICRAD = 0.001D0 * MILRAD )

      DOUBLE PRECISION      NANRAD
      PARAMETER           ( NANRAD = 0.001D0 * MICRAD )

      DOUBLE PRECISION      KG
      PARAMETER           ( KG = 1.0D0 )

      DOUBLE PRECISION      GRAM
      PARAMETER           ( GRAM = 0.001D0*KG )

      DOUBLE PRECISION      POUND
      PARAMETER           ( POUND = 0.45359237D0 * KG )

      DOUBLE PRECISION      OUNCE
      PARAMETER           ( OUNCE = POUND / 16.0D0 )

      DOUBLE PRECISION      COULOM
      PARAMETER           ( COULOM = 1.0D0 )

      DOUBLE PRECISION      ECHARG
      PARAMETER           ( ECHARG = COULOM / 6.24196D18 )

      DOUBLE PRECISION      STATCL
      PARAMETER           ( STATCL = COULOM * 2.997930D9 )

!
!     Local variables
!
      CHARACTER*(USIZE)     CANDS
      CHARACTER*(USIZE+1)   CANDP
      CHARACTER*(USIZE)     ERROR

      DOUBLE PRECISION      DEGREE
      DOUBLE PRECISION      ARCMIN
      DOUBLE PRECISION      ARCSEC
      DOUBLE PRECISION      HRANG
      DOUBLE PRECISION      LIGHT
      DOUBLE PRECISION      LSEC
      DOUBLE PRECISION      LMIN
      DOUBLE PRECISION      LHOUR
      DOUBLE PRECISION      LDAY
      DOUBLE PRECISION      LYEAR
      DOUBLE PRECISION      MINANG
      DOUBLE PRECISION      REV
      DOUBLE PRECISION      SECANG

      INTEGER               COUNT
      INTEGER               I
      INTEGER               J
      INTEGER               PTR
      CHARACTER*(8)         NAMES  ( 1 )
      LOGICAL               UPDATE
      LOGICAL               FOUND
      INTEGER               IAU
      INTEGER               IAUS
      INTEGER               IPARSC
      INTEGER               NNAMES


!
!     Conversion values.
!
      CHARACTER*(USIZE)     UNITS  ( NUNITS )
      INTEGER               UCLASS ( NUNITS )
      DOUBLE PRECISION      UVALUE ( NUNITS )
      INTEGER               ORDVEC ( NUNITS )
      LOGICAL               FIRST
      SAVE

!
!     Initial values
!
      DATA                  FIRST   / .TRUE. /


!
!     This next block of code sets up the constants, names, values
!     and classes for all the recognized strings.  We do this here
!     because FORTRAN just doesn't do this kind of stuff in a
!     convenient manner.
!
      IF ( FIRST ) THEN

         FIRST = .FALSE.

         DEGREE = PI()   / 180.0D0
         ARCMIN = DEGREE / 60.0D0
         ARCSEC = ARCMIN / 60.0D0
         SCALE  = 1.0D0  / DSIN ( ARCSEC )

         SECANG = ARCSEC * 15.0D0
         MINANG = ARCMIN * 15.0D0
         HRANG  = DEGREE * 15.0D0

         REV    = TWOPI()

         LIGHT  = CLIGHT()
         LSEC   = SECOND * LIGHT
         LMIN   = MINUTE * LIGHT
         LHOUR  = HOUR   * LIGHT
         LDAY   = DAY    * LIGHT
         LYEAR  = JYEAR  * LIGHT

         NNAMES   = 1
         NAMES(1) = 'AU'

!
!        If available and the value of the AU is reasonable, we fetch
!        it from the kernel pool.  Otherwise we use the value in
!        DE200.
!
         CALL SWPOOL ( 'FNDUCV', NNAMES, NAMES  )
         CALL CVPOOL ( 'FNDUCV',         UPDATE )
         CALL RTPOOL ( 'AU',     I,  AU, FOUND  )

         IF ( .NOT. FOUND ) THEN

            AU = DE200V

         ELSE IF ( DABS( AU - DE200V ) .GT. 10.0D0 ) THEN

            AU = DE200V

         END IF

         PARSEC = SCALE * AU



         I = 0

         I         = I + 1
         UNITS(I)  = 'METERS'
         UVALUE(I) =  METER
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'CM'
         UVALUE(I) =  CM
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'KM'
         UVALUE(I) =  KM
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'KMS'
         UVALUE(I) =  KM
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'CENTIMETERS'
         UVALUE(I) =  CM
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'KILOMETERS'
         UVALUE(I) =  KM
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'INCH'
         UVALUE(I) =  INCH
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'INCHES'
         UVALUE(I) =  INCH
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'FOOT'
         UVALUE(I) =  FOOT
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'FEET'
         UVALUE(I) =  FOOT
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'YARDS'
         UVALUE(I) =  YARD
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'AU'
         UVALUE(I) =  AU
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'AUS'
         UVALUE(I) =  AU
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'MILES'
         UVALUE(I) =  MILEST
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'STATUTE_MILES'
         UVALUE(I) =  MILEST
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'LIGHTSECONDS'
         UVALUE(I) =  LSEC
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'LIGHTYEAR'
         UVALUE(I) =  LYEAR
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'SECS'
         UVALUE(I) =  SECOND
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'SECONDS'
         UVALUE(I) =  SECOND
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'MINS'
         UVALUE(I) =  MINUTE
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'MINUTES'
         UVALUE(I) =  MINUTE
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'HRS'
         UVALUE(I) =  HOUR
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'HOURS'
         UVALUE(I) =  HOUR
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'DAYS'
         UVALUE(I) =  DAY
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'WEEKS'
         UVALUE(I) =  WEEK
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'JYEARS'
         UVALUE(I) =  JYEAR
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'JULIAN_YEARS'
         UVALUE(I) =  JYEAR
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'CENTURY'
         UVALUE(I) =  JCENT
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'CENTURIES'
         UVALUE(I) =  JCENT
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'JULIAN_CENTURIES'
         UVALUE(I) =  JCENT
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'JULIAN_CENTURY'
         UVALUE(I) =  JCENT
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'LIGHTDAYS'
         UVALUE(I) =  LDAY
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'LIGHTYEARS'
         UVALUE(I) =  LYEAR
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'RADIANS'
         UVALUE(I) =  RADIAN
         UCLASS(I) =  ANGLE

         I         = I + 1
         UNITS(I)  = 'MILLIRADIANS'
         UVALUE(I) = MILRAD
         UCLASS(I) = ANGLE

         I         = I + 1
         UNITS(I)  = 'MICRORADIANS'
         UVALUE(I) = MICRAD
         UCLASS(I) = ANGLE

         I         = I + 1
         UNITS(I)  = 'NANORADIANS'
         UVALUE(I) = NANRAD
         UCLASS(I) = ANGLE

         I         = I + 1
         UNITS(I)  = 'DEGREES'
         UVALUE(I) =  DEGREE
         UCLASS(I) =  ANGLE

         I         = I + 1
         UNITS(I)  = 'DEGS'
         UVALUE(I) =  DEGREE
         UCLASS(I) =  ANGLE

         I         = I + 1
         UNITS(I)  = 'ARCSECONDS'
         UVALUE(I) =  ARCSEC
         UCLASS(I) =  ANGLE

         I         = I + 1
         UNITS(I)  = 'ARCMINUTES'
         UVALUE(I) =  ARCMIN
         UCLASS(I) =  ANGLE

         I         = I + 1
         UNITS(I)  = 'SECONDANGLES'
         UVALUE(I) =  SECANG
         UCLASS(I) =  ANGLE

         I         = I + 1
         UNITS(I)  = 'MINUTEANGLES'
         UVALUE(I) =  MINANG
         UCLASS(I) =  ANGLE

         I         = I + 1
         UNITS(I)  = 'HOURANGLES'
         UVALUE(I) =  HRANG
         UCLASS(I) =  ANGLE

         I         = I + 1
         UNITS(I)  = 'KILOGRAMS'
         UVALUE(I) =  KG
         UCLASS(I) =  MASS

         I         = I + 1
         UNITS(I)  = 'KGS'
         UVALUE(I) =  KG
         UCLASS(I) =  MASS

         I         = I + 1
         UNITS(I)  = 'GRAMS'
         UVALUE(I) =  GRAM
         UCLASS(I) =  MASS

         I         = I + 1
         UNITS(I)  = 'POUNDS'
         UVALUE(I) =  POUND
         UCLASS(I) =  MASS

         I         = I + 1
         UNITS(I)  = 'OUNCES'
         UVALUE(I) =  OUNCE
         UCLASS(I) =  MASS

         I         = I + 1
         UNITS(I)  = 'PARSECS'
         UVALUE(I) =  PARSEC
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'YEARS'
         UVALUE(I) =  JYEAR
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'JULIANYEARS'
         UVALUE(I) =  JYEAR
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'TROPICALYEARS'
         UVALUE(I) =  TYEAR
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'TROPICAL_YEARS'
         UVALUE(I) =  TYEAR
         UCLASS(I) =  TIME

         I         = I + 1
         UNITS(I)  = 'STATUTEMILES'
         UVALUE(I) =  MILEST
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'NAUTICALMILES'
         UVALUE(I) =  MILENT
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'NAUTICAL_MILES'
         UVALUE(I) =  MILENT
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'MMS'
         UVALUE(I) =  MM
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'MILLIMETERS'
         UVALUE(I) =  MM
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'REVOLUTIONS'
         UVALUE(I) =  REV
         UCLASS(I) =  ANGLE

         I         = I + 1
         UNITS(I)  = 'REVS'
         UVALUE(I) =  REV
         UCLASS(I) =  ANGLE

         I         = I + 1
         UNITS(I)  = 'LIGHTHOURS'
         UVALUE(I) =  LHOUR
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'LIGHTMINUTES'
         UVALUE(I) =  LMIN
         UCLASS(I) =  LENGTH

         I         = I + 1
         UNITS(I)  = 'COULOMBS'
         UVALUE(I) =  COULOM
         UCLASS(I) =  CHARGE

         I         = I + 1
         UNITS(I)  = 'ELECTRON_CHARGES'
         UVALUE(I) =  ECHARG
         UCLASS(I) =  CHARGE

         I         = I + 1
         UNITS(I)  = 'STATCOULOMBS'
         UVALUE(I) =  STATCL
         UCLASS(I) =  CHARGE

         I         = I + 1
         UNITS(I)  = 'PI'
         UVALUE(I) =  PI()
         UCLASS(I) = NUMBER

         I         = I + 1
         UNITS(I)  = '-PI'
         UVALUE(I) =  -PI()
         UCLASS(I) = NUMBER
!
!        I         = I + 1
!        UNITS(I)  =
!        UVALUE(I) =
!        UCLASS(I) =
!
         COUNT = I

!
!        Sort everything for quick lookup.
!
         CALL ORDERC ( UNITS,  COUNT, ORDVEC )
         CALL REORDC ( ORDVEC, COUNT, UNITS  )
         CALL REORDD ( ORDVEC, COUNT, UVALUE  )
         CALL REORDI ( ORDVEC, COUNT, UCLASS  )

      END IF


      CALL CVPOOL ( 'FNDUCV', UPDATE )

      IF ( UPDATE ) THEN

         IAU    = BSRCHC ( 'AU',      COUNT, UNITS )
         IAUS   = BSRCHC ( 'AUS',     COUNT, UNITS )
         IPARSC = BSRCHC ( 'PARSECS', COUNT, UNITS )

         CALL RTPOOL ( 'AU', I, AU, FOUND )

         IF ( DABS(AU - DE200V) .LT. 10.0D0 ) THEN

            UVALUE(IAU)    = AU
            UVALUE(IAUS)   = AU
            UVALUE(IPARSC) = SCALE * AU

         END IF

      END IF


!
!     Left justify, convert to upper case and form a "plural" version
!     of UNIN
!
      CALL LJUST  ( UNIN,   CANDS )
      CALL UCASE  ( CANDS,  CANDS )

      CANDP =               CANDS
      CALL SUFFIX ( 'S', 0, CANDP )

!
!     Look for the "singular" version first.
!
      J = BSRCHC ( CANDS, COUNT, UNITS )

!
!     If we didn't have any luck with the singular version,
!     look for the plural form.
!
      IF ( J .EQ. 0 ) THEN
         J = BSRCHC ( CANDP, COUNT, UNITS )
      END IF

!
!     If we got something, just copy the class and value.
!
      IF ( J .GT. 0 ) THEN

         KNOWN = .TRUE.
         CLASS = UCLASS ( J )
         VALUE = UVALUE ( J )

      ELSE

!
!        We don't have a unit.  Get ready to return...
!
         KNOWN = .FALSE.
         CLASS = -1
         VALUE = 0.0D0

!
!        ... but before we do, see if we've got a number.
!
         IF ( BENUM ( CANDS ) ) THEN

            CALL NPARSD ( CANDS, VALUE, ERROR, PTR )

            IF ( ERROR .EQ. ' ' ) THEN
               KNOWN = .TRUE.
               CLASS = NUMBER
            END IF

         END IF

      END IF

!
!     Since the user can potentially enter a bad value for the AU
!     via the kernel pool, we will signal an error.  However we
!     wait until this point so that routines that need to have
!     an AU value in order to continue functioning,
!
      IF ( DABS(AU - DE200V) .GT. 10.0D0 ) THEN

         CALL CHKIN  ( 'FNDUCV' )
         CALL SETMSG ( 'The value of the astronomical unit '       // &
                       'extracted from the kernel pool varies '    // &
                       'from the well trusted value used in DE200 '// &
                       '(149,597,870.660 km) by more than 10 km. ' // &
                       'The value in DE200 is believed to be good '// &
                       'to 60 meters or so.  The value in the '    // &
                       'kernel pool was #. '                      )
         CALL ERRDP  ( '#',    AU                                 )
         CALL SIGERR ( 'SPICE(BADAUVALUE)'                        )
         CALL CHKOUT ( 'FNDUCV'                                   )
!
!        Reset the value of the AU back to the DE200 value so that
!        the next time we hit this without doing a kernel pool read
!        we will not get this error message again.
!
         AU = DE200V
         RETURN

      END IF

      RETURN
      END
