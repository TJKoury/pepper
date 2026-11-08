!$Procedure CONVRT ( Convert Units )

      SUBROUTINE CONVRT ( X, IN, OUT, Y )

! $ Abstract
!
!     Take a measurement X, the units associated with
!     X, and units to which X should be converted; return Y ---
!     the value of the measurement in the output units.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONVERSION
!     UNITS
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION X
      CHARACTER*(*)    IN
      CHARACTER*(*)    OUT
      DOUBLE PRECISION Y

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     X          I   Number representing a measurement in some units.
!     IN         I   The units in which X is measured.
!     OUT        I   Desired units for the measurement.
!     Y          O   The measurement in the desired units.
!
! $ Detailed_Input
!
!     X        is a number representing a measurement in the units
!              specified by IN.
!
!     IN       is the identifier of the units associated with the
!              measurement X. Acceptable units are:
!
!                 Angles:                 'RADIANS'
!                                         'DEGREES'
!                                         'ARCMINUTES'
!                                         'ARCSECONDS'
!                                         'HOURANGLE'
!                                         'MINUTEANGLE'
!                                         'SECONDANGLE'
!
!                 Metric Distances:       'M'
!                                         'METERS'
!                                         'KM'
!                                         'KILOMETERS'
!                                         'CM'
!                                         'CENTIMETERS'
!                                         'MM'
!                                         'MILLIMETERS'
!
!                 English Distances:      'FEET'
!                                         'INCHES'
!                                         'YARDS'
!                                         'STATUTE_MILES'
!                                         'NAUTICAL_MILES'
!
!                 Astrometric Distances:  'AU'
!                                         'PARSECS'
!                                         'LIGHTSECS'
!                                         'LIGHTYEARS' julian lightyears
!
!                 Time:                   'SECONDS'
!                                         'MINUTES'
!                                         'HOURS'
!                                         'DAYS'
!                                         'JULIAN_YEARS'
!                                         'TROPICAL_YEARS'
!                                         'YEARS' (same as julian years)
!
!     OUT      is the identifier of the units desired for the
!              measurement X. See the description of IN.
!
! $ Detailed_Output
!
!     Y        is the input measurement converted to the desired
!              units.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input units, output units, or both input and
!         output units are not recognized, the error
!         SPICE(UNITSNOTREC) is signaled.
!
!     2)  If the units being converted between are incompatible, the
!         error SPICE(INCOMPATIBLEUNITS) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine converts a measurement X given in units specified by
!     IN to the equivalent value Y in units specified by OUT.
!
!     If a unit is not recognized, an error message is produced that
!     indicates which one was not recognized.
!
!     If input and output units are incompatible (for example ANGLE
!     and DISTANCE units) and error message will be produced stating
!     the requested units and associated types.
!
! $ Examples
!
!     To convert 1 meter to statute miles and feet you could
!
!        CALL CONVRT ( 1.0D0, 'METERS',        'STATUTE_MILES', MILES )
!        CALL CONVRT ( MILES, 'STATUTE_MILES', 'FEET',          FEET  )
!
!     or
!
!        CALL CONVRT ( 1.0D0, 'METERS', 'STATUTE_MILES', MILES )
!        CALL CONVRT ( 1.0D0, 'METERS', 'FEET',          FEET  )
!
! $ Restrictions
!
!     1)  This routine does not do any checking for overflow. The caller
!         is required to make sure that the units used for the
!         measurement are such that no floating point overflow will
!         occur when the conversion is performed.
!
!     2)  Some of the units are not "defined" quantities. In such a case
!         a best estimate is provided as of the date of the current
!         version of this routine. Those estimated quantities are:
!
!            AU               The astronomical unit. The value was taken
!                             from the JPL ephemeris DE125. This value
!                             is an approximation and should not be used
!                             for high-accuracy work. It agrees with the
!                             value used in the JPL planetary ephemeris
!                             DE430 (149597870.700 km) at the 100m
!                             level.
!
!            TROPICAL_YEARS   The tropical year is the time from equinox
!                             to equinox. This varies slightly with
!                             time.
!
!            PARSECS          The parsec is the distance to an object
!                             whose parallax angle is one arcsecond. Its
!                             value is dependent upon the value of the
!                             astronomical unit.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.M. Owen          (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!        Corrected a typo in $Restrictions section.
!
!-    SPICELIB Version 2.0.0, 12-MAY-2015 (NJB)
!
!        Added support for full names of metric distance units. Added
!        support for the abbreviation 'M' indicating meters.
!
!-    SPICELIB Version 1.0.2, 01-JUL-2014 (NJB)
!
!        Updated the description of the AU in the $Restrictions
!        section.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WMO) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     convert units
!
!-&


!
!     SPICELIB functions
!
      LOGICAL          RETURN
      INTEGER          ISRCHC
      DOUBLE PRECISION DPR

!
!     Local variables
!
      CHARACTER*16     INU
      CHARACTER*16     OUTU



      INTEGER          I
      INTEGER          J
      DOUBLE PRECISION TEMP

      DOUBLE PRECISION SECPJY
      PARAMETER      ( SECPJY =  86400.0D0 * 365.25D0      )

      DOUBLE PRECISION SECPTY
      PARAMETER      ( SECPTY =  86400.0D0 * 365.2421988D0 )

      DOUBLE PRECISION LTYEAR
      PARAMETER      ( LTYEAR = 86400.0D0 * 365.25D0 * 299792458.0D0)

      DOUBLE PRECISION DPM
      PARAMETER      ( DPM  = 1.0D0  /   60.0D0      )

      DOUBLE PRECISION DPS
      PARAMETER      ( DPS  = 1.0D0  / 3600.0D0      )

      DOUBLE PRECISION DPSA
      PARAMETER      ( DPSA = 15.0D0 / 3600.0D0      )

      DOUBLE PRECISION AU
      PARAMETER      ( AU   = 1.4959787061368887D11  )
!
!     1.0d0 divided by the sin of 1 arc second
!
      DOUBLE PRECISION SCALE
      PARAMETER      ( SCALE = 2.0626480624790438D+05 )


      DOUBLE PRECISION PARSEC
      PARAMETER      ( PARSEC = AU * SCALE )

      INTEGER          ANG
      PARAMETER      ( ANG  = 0         )

      INTEGER          DIST
      PARAMETER      ( DIST = ANG  + 7  )

      INTEGER          TME
      PARAMETER      ( TME  = DIST + 17 )

      INTEGER          TOTAL
      PARAMETER      ( TOTAL = TME + 7  )

      CHARACTER*16     UNITS  (TOTAL)
      SAVE             UNITS

      DOUBLE PRECISION CNVRTN (TOTAL)
      SAVE             CNVRTN

      CHARACTER*8      TYPE   (TOTAL)
      SAVE             TYPE

      LOGICAL          FIRST
      SAVE             FIRST



      DATA             FIRST / .TRUE. /

!
!     Angular Conversions:
!
!                 (1)  Degrees/Radians
!                 (2)  Degrees/Degrees
!                 (3)  Degrees/ARCMINUTES
!                 (4)  Degrees/ARCSECONDS
!
!                 ()   Degrees/HOURANGLE
!                 ()   Degrees/MINUTEANGLE
!                 ()   Degrees/SECONDANGLE
!
      DATA UNITS  (ANG + 1)  / 'RADIANS'     /
      DATA UNITS  (ANG + 2)  / 'DEGREES'     /
      DATA UNITS  (ANG + 3)  / 'ARCMINUTES'  /
      DATA UNITS  (ANG + 4)  / 'ARCSECONDS'  /
      DATA UNITS  (ANG + 5)  / 'HOURANGLE'   /
      DATA UNITS  (ANG + 6)  / 'MINUTEANGLE' /
      DATA UNITS  (ANG + 7)  / 'SECONDANGLE' /
!
!     DATA CNVRTN (ANG + 1)  /      DPR()   /
!
!     This value will be loaded using the SPICELIB function DPR()
!     on the first execution of this routine.
!
      DATA CNVRTN (ANG + 2)  /    1.0D0   /
      DATA CNVRTN (ANG + 3)  /      DPM   /
      DATA CNVRTN (ANG + 4)  /      DPS   /
      DATA CNVRTN (ANG + 5)  / 15.000D0   /
      DATA CNVRTN (ANG + 6)  /  0.250D0   /
      DATA CNVRTN (ANG + 7)  /  DPSA      /

      DATA TYPE   (ANG + 1)  / 'ANGLE'    /
      DATA TYPE   (ANG + 2)  / 'ANGLE'    /
      DATA TYPE   (ANG + 3)  / 'ANGLE'    /
      DATA TYPE   (ANG + 4)  / 'ANGLE'    /
      DATA TYPE   (ANG + 5)  / 'ANGLE'    /
      DATA TYPE   (ANG + 6)  / 'ANGLE'    /
      DATA TYPE   (ANG + 7)  / 'ANGLE'    /
!
!     Distance Conversions ( 8 through 17 )
!
!                 (+  1) Meters/Meter
!                 (+  2) Meters/Km
!                 (+  3) Meters/Cm
!                 (+  4) Meters/mm
!                 (+  5) Meters/Lightsecs
!                 (+  6) Meters/AU
!                 (+  7) Meters/Meter
!                 (+  8) Meters/Km
!                 (+  9) Meters/cm
!                 (+ 10) Meters/mm
!
      DATA UNITS (DIST + 1)  / 'METERS'               /
      DATA UNITS (DIST + 2)  / 'KM'                   /
      DATA UNITS (DIST + 3)  / 'CM'                   /
      DATA UNITS (DIST + 4)  / 'MM'                   /
      DATA UNITS (DIST + 5)  / 'LIGHTSECS'            /
      DATA UNITS (DIST + 6)  / 'AU'                   /
      DATA UNITS (DIST + 7)  / 'M'                    /
      DATA UNITS (DIST + 8)  / 'KILOMETERS'           /
      DATA UNITS (DIST + 9)  / 'CENTIMETERS'          /
      DATA UNITS (DIST + 10) / 'MILLIMETERS'          /

      DATA CNVRTN(DIST + 1)  / 1.00D0                 /
      DATA CNVRTN(DIST + 2)  / 1000.0D0               /
      DATA CNVRTN(DIST + 3)  / 0.01D0                 /
      DATA CNVRTN(DIST + 4)  / 0.001D0                /
      DATA CNVRTN(DIST + 5)  / 299792458.0D0        /
      DATA CNVRTN(DIST + 6)  / AU                     /
      DATA CNVRTN(DIST + 7)  / 1.00D0                 /
      DATA CNVRTN(DIST + 8)  / 1000.0D0               /
      DATA CNVRTN(DIST + 9)  / 0.01D0                 /
      DATA CNVRTN(DIST + 10) / 0.001D0                /

      DATA TYPE  (DIST + 1)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 2)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 3)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 4)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 5)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 6)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 7)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 8)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 9)  / 'DISTANCE'             /
      DATA TYPE  (DIST + 10) / 'DISTANCE'             /

!
!     Distance Conversions
!
!                 (+ 11) Meters/Foot
!                 (+ 12) Meters/inch
!                 (+ 13) Meters/Statute Mile
!                 (+ 14) Meters/Nautical Mile
!                 (+ 15) Meters/Yard
!
      DATA UNITS (DIST + 11) / 'FEET'            /
      DATA UNITS (DIST + 12) / 'INCHES'          /
      DATA UNITS (DIST + 13) / 'STATUTE_MILES'   /
      DATA UNITS (DIST + 14) / 'NAUTICAL_MILES'  /
      DATA UNITS (DIST + 15) / 'YARDS'           /

      DATA CNVRTN(DIST + 11) / 0.3048D0          /
      DATA CNVRTN(DIST + 12) / 0.0254D0          /
      DATA CNVRTN(DIST + 13) / 1609.344D0       /
      DATA CNVRTN(DIST + 14) / 1852.0D0         /
      DATA CNVRTN(DIST + 15) / 0.9144D0          /

      DATA TYPE  (DIST + 11) / 'DISTANCE'        /
      DATA TYPE  (DIST + 12) / 'DISTANCE'        /
      DATA TYPE  (DIST + 13) / 'DISTANCE'        /
      DATA TYPE  (DIST + 14) / 'DISTANCE'        /
      DATA TYPE  (DIST + 15) / 'DISTANCE'        /

!
!     Distance Conversions
!
!                 (+ 16) Meters/LightYear
!                 (+ 17) Meters/Parsec
!
      DATA UNITS (DIST + 16) / 'LIGHTYEARS'            /
      DATA UNITS (DIST + 17) / 'PARSECS'               /

      DATA CNVRTN(DIST + 16) / LTYEAR                  /
      DATA CNVRTN(DIST + 17) / PARSEC                  /

      DATA TYPE  (DIST + 16) / 'DISTANCE'              /
      DATA TYPE  (DIST + 17) / 'DISTANCE'              /

!
!     Time Conversions
!
!                 (+ 1 ) seconds / second
!                 (+ 2 ) seconds / minute
!                 (+ 3 ) seconds / hour
!                 (+ 4 ) seconds / day
!                 (+ 5 ) Seconds / Julian year
!                 (+ 6 ) Seconds / Tropical year
!                 (+ 7 ) Seconds / year          --- same as Julian year
!
      DATA UNITS ( TME + 1 ) / 'SECONDS'        /
      DATA UNITS ( TME + 2 ) / 'MINUTES'        /
      DATA UNITS ( TME + 3 ) / 'HOURS'          /
      DATA UNITS ( TME + 4 ) / 'DAYS'           /
      DATA UNITS ( TME + 5 ) / 'JULIAN_YEARS'   /
      DATA UNITS ( TME + 6 ) / 'TROPICAL_YEARS' /
      DATA UNITS ( TME + 7 ) / 'YEARS'          /

      DATA CNVRTN( TME + 1 ) /     1.0D0        /
      DATA CNVRTN( TME + 2 ) /    60.0D0        /
      DATA CNVRTN( TME + 3 ) /  3600.0D0        /
      DATA CNVRTN( TME + 4 ) / 86400.0D0        /
      DATA CNVRTN( TME + 5 ) / SECPJY           /
      DATA CNVRTN( TME + 6 ) / SECPTY           /
      DATA CNVRTN( TME + 7 ) / SECPJY           /

      DATA TYPE  ( TME + 1 ) / 'TIME'           /
      DATA TYPE  ( TME + 2 ) / 'TIME'           /
      DATA TYPE  ( TME + 3 ) / 'TIME'           /
      DATA TYPE  ( TME + 4 ) / 'TIME'           /
      DATA TYPE  ( TME + 5 ) / 'TIME'           /
      DATA TYPE  ( TME + 6 ) / 'TIME'           /
      DATA TYPE  ( TME + 7 ) / 'TIME'           /


!
!     Set up the error processing.
!
      IF ( RETURN() ) RETURN
      CALL CHKIN ( 'CONVRT' )



      IF ( FIRST ) THEN
         CNVRTN( ANG + 1 ) =  DPR()
         FIRST             = .FALSE.
      END IF


      CALL UCASE ( IN,  INU  )
      CALL UCASE ( OUT, OUTU )

      I    = ISRCHC( INU,  TOTAL, UNITS )
      J    = ISRCHC( OUTU, TOTAL, UNITS )

      IF (      ( I .EQ. 0 ) &
           .OR. ( J.EQ. 0 ) ) THEN

         IF (       ( I .EQ. 0 ) &
              .AND. ( J .EQ. 0 ) ) THEN


            CALL SETMSG ('CONVRT: Neither the input units ' // INU  // &
                         'nor the output units '            // OUTU // &
                         'were recognized.' &
                        )

            CALL SIGERR ('SPICE(UNITSNOTREC)')

            CALL CHKOUT ( 'CONVRT' )
            RETURN

         ELSE IF ( I .EQ. 0 ) THEN

            CALL SETMSG ('CONVRT: Input units '// INU // &
                         ' were not recognized' &
                        )

            CALL SIGERR ('SPICE(UNITSNOTREC)')

            CALL CHKOUT ( 'CONVRT' )
            RETURN

         ELSE IF ( J .EQ. 0 ) THEN

            CALL SETMSG ('CONVRT: Output units '// OUTU // &
                         ' were not recognized' &
                        )

            CALL SIGERR ('SPICE(UNITSNOTREC)')

            CALL CHKOUT ( 'CONVRT' )
            RETURN

         END IF

      END IF

      IF ( TYPE(I) .NE. TYPE(J) ) THEN

         CALL SETMSG ('CONVRT: Incompatible units. '   // &
                      'You are attempting to convert ' // &
                      INU  // 'type: ' // TYPE(I)      // &
                      ' to '                           // &
                      OUTU // 'type: ' // TYPE(J)      // &
                      '.' &
                     )

         CALL SIGERR ('SPICE(INCOMPATIBLEUNITS)')

         CALL CHKOUT ( 'CONVRT' )
         RETURN

      END IF


      TEMP = X    * CNVRTN ( I )
      Y    = TEMP / CNVRTN ( J )


      CALL CHKOUT ( 'CONVRT' )
      RETURN

      END
