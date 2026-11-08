!$Procedure CONVRT ( Convert Units )

      subroutine convrt ( x, in, out, y )
      use pepper_kinds, only: ip, wp

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

      implicit none

      real(wp) x
      character(*)     in
      character(*)     out
      real(wp) y

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
      logical          return
      integer(ip)      isrchc
      real(wp) dpr

!
!     Local variables
!
      character(16)    inu
      character(16)    outu



      integer(ip)      i
      integer(ip)      j
      real(wp) temp

      real(wp) secpjy
      parameter      ( secpjy =  86400.0_wp * 365.25_wp      )

      real(wp) secpty
      parameter      ( secpty =  86400.0_wp * 365.2421988_wp )

      real(wp) ltyear
      parameter      ( ltyear = 86400.0_wp * 365.25_wp * 299792458.0_wp)

      real(wp) dpm
      parameter      ( dpm  = 1.0_wp  /   60.0_wp      )

      real(wp) dps
      parameter      ( dps  = 1.0_wp  / 3600.0_wp      )

      real(wp) dpsa
      parameter      ( dpsa = 15.0_wp / 3600.0_wp      )

      real(wp) au
      parameter      ( au   = 1.4959787061368887e11_wp  )
!
!     1.0d0 divided by the sin of 1 arc second
!
      real(wp) scale
      parameter      ( scale = 2.0626480624790438e05_wp )


      real(wp) parsec
      parameter      ( parsec = au * scale )

      integer(ip)      ang
      parameter      ( ang  = 0         )

      integer(ip)      dist
      parameter      ( dist = ang  + 7  )

      integer(ip)      tme
      parameter      ( tme  = dist + 17 )

      integer(ip)      total
      parameter      ( total = tme + 7  )

      character(16)    units  (total)
      save             units

      real(wp) cnvrtn (total)
      save             cnvrtn

      character(8)     type   (total)
      save             type

      logical          first
      save             first



      data             first / .true. /

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
      data units  (ang + 1)  / 'RADIANS'     /
      data units  (ang + 2)  / 'DEGREES'     /
      data units  (ang + 3)  / 'ARCMINUTES'  /
      data units  (ang + 4)  / 'ARCSECONDS'  /
      data units  (ang + 5)  / 'HOURANGLE'   /
      data units  (ang + 6)  / 'MINUTEANGLE' /
      data units  (ang + 7)  / 'SECONDANGLE' /
!
!     DATA CNVRTN (ANG + 1)  /      DPR()   /
!
!     This value will be loaded using the SPICELIB function DPR()
!     on the first execution of this routine.
!
      data cnvrtn (ang + 2)  /    1.0_wp   /
      data cnvrtn (ang + 3)  /      dpm   /
      data cnvrtn (ang + 4)  /      dps   /
      data cnvrtn (ang + 5)  / 15.000_wp   /
      data cnvrtn (ang + 6)  /  0.250_wp   /
      data cnvrtn (ang + 7)  /  dpsa      /

      data type   (ang + 1)  / 'ANGLE'    /
      data type   (ang + 2)  / 'ANGLE'    /
      data type   (ang + 3)  / 'ANGLE'    /
      data type   (ang + 4)  / 'ANGLE'    /
      data type   (ang + 5)  / 'ANGLE'    /
      data type   (ang + 6)  / 'ANGLE'    /
      data type   (ang + 7)  / 'ANGLE'    /
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
      data units (dist + 1)  / 'METERS'               /
      data units (dist + 2)  / 'KM'                   /
      data units (dist + 3)  / 'CM'                   /
      data units (dist + 4)  / 'MM'                   /
      data units (dist + 5)  / 'LIGHTSECS'            /
      data units (dist + 6)  / 'AU'                   /
      data units (dist + 7)  / 'M'                    /
      data units (dist + 8)  / 'KILOMETERS'           /
      data units (dist + 9)  / 'CENTIMETERS'          /
      data units (dist + 10) / 'MILLIMETERS'          /

      data cnvrtn(dist + 1)  / 1.00_wp                 /
      data cnvrtn(dist + 2)  / 1000.0_wp               /
      data cnvrtn(dist + 3)  / 0.01_wp                 /
      data cnvrtn(dist + 4)  / 0.001_wp                /
      data cnvrtn(dist + 5)  / 299792458.0_wp        /
      data cnvrtn(dist + 6)  / au                     /
      data cnvrtn(dist + 7)  / 1.00_wp                 /
      data cnvrtn(dist + 8)  / 1000.0_wp               /
      data cnvrtn(dist + 9)  / 0.01_wp                 /
      data cnvrtn(dist + 10) / 0.001_wp                /

      data type  (dist + 1)  / 'DISTANCE'             /
      data type  (dist + 2)  / 'DISTANCE'             /
      data type  (dist + 3)  / 'DISTANCE'             /
      data type  (dist + 4)  / 'DISTANCE'             /
      data type  (dist + 5)  / 'DISTANCE'             /
      data type  (dist + 6)  / 'DISTANCE'             /
      data type  (dist + 7)  / 'DISTANCE'             /
      data type  (dist + 8)  / 'DISTANCE'             /
      data type  (dist + 9)  / 'DISTANCE'             /
      data type  (dist + 10) / 'DISTANCE'             /

!
!     Distance Conversions
!
!                 (+ 11) Meters/Foot
!                 (+ 12) Meters/inch
!                 (+ 13) Meters/Statute Mile
!                 (+ 14) Meters/Nautical Mile
!                 (+ 15) Meters/Yard
!
      data units (dist + 11) / 'FEET'            /
      data units (dist + 12) / 'INCHES'          /
      data units (dist + 13) / 'STATUTE_MILES'   /
      data units (dist + 14) / 'NAUTICAL_MILES'  /
      data units (dist + 15) / 'YARDS'           /

      data cnvrtn(dist + 11) / 0.3048_wp          /
      data cnvrtn(dist + 12) / 0.0254_wp          /
      data cnvrtn(dist + 13) / 1609.344_wp       /
      data cnvrtn(dist + 14) / 1852.0_wp         /
      data cnvrtn(dist + 15) / 0.9144_wp          /

      data type  (dist + 11) / 'DISTANCE'        /
      data type  (dist + 12) / 'DISTANCE'        /
      data type  (dist + 13) / 'DISTANCE'        /
      data type  (dist + 14) / 'DISTANCE'        /
      data type  (dist + 15) / 'DISTANCE'        /

!
!     Distance Conversions
!
!                 (+ 16) Meters/LightYear
!                 (+ 17) Meters/Parsec
!
      data units (dist + 16) / 'LIGHTYEARS'            /
      data units (dist + 17) / 'PARSECS'               /

      data cnvrtn(dist + 16) / ltyear                  /
      data cnvrtn(dist + 17) / parsec                  /

      data type  (dist + 16) / 'DISTANCE'              /
      data type  (dist + 17) / 'DISTANCE'              /

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
      data units ( tme + 1 ) / 'SECONDS'        /
      data units ( tme + 2 ) / 'MINUTES'        /
      data units ( tme + 3 ) / 'HOURS'          /
      data units ( tme + 4 ) / 'DAYS'           /
      data units ( tme + 5 ) / 'JULIAN_YEARS'   /
      data units ( tme + 6 ) / 'TROPICAL_YEARS' /
      data units ( tme + 7 ) / 'YEARS'          /

      data cnvrtn( tme + 1 ) /     1.0_wp        /
      data cnvrtn( tme + 2 ) /    60.0_wp        /
      data cnvrtn( tme + 3 ) /  3600.0_wp        /
      data cnvrtn( tme + 4 ) / 86400.0_wp        /
      data cnvrtn( tme + 5 ) / secpjy           /
      data cnvrtn( tme + 6 ) / secpty           /
      data cnvrtn( tme + 7 ) / secpjy           /

      data type  ( tme + 1 ) / 'TIME'           /
      data type  ( tme + 2 ) / 'TIME'           /
      data type  ( tme + 3 ) / 'TIME'           /
      data type  ( tme + 4 ) / 'TIME'           /
      data type  ( tme + 5 ) / 'TIME'           /
      data type  ( tme + 6 ) / 'TIME'           /
      data type  ( tme + 7 ) / 'TIME'           /


!
!     Set up the error processing.
!
      if ( return() ) return



      if ( first ) then
         cnvrtn( ang + 1 ) =  dpr()
         first             = .false.
      end if


      call ucase ( in,  inu  )
      call ucase ( out, outu )

      i    = isrchc( inu,  total, units )
      j    = isrchc( outu, total, units )

      if (      ( i == 0 ) &
           .or. ( j== 0 ) ) then

         if (       ( i == 0 ) &
              .and. ( j == 0 ) ) then


            call setmsg ('CONVRT: Neither the input units ' // inu  // &
                         'nor the output units '            // outu // &
                         'were recognized.' &
                        )

            call sigerr ('SPICE(UNITSNOTREC)')

            return

         else if ( i == 0 ) then

            call setmsg ('CONVRT: Input units '// inu // &
                         ' were not recognized' &
                        )

            call sigerr ('SPICE(UNITSNOTREC)')

            return

         else if ( j == 0 ) then

            call setmsg ('CONVRT: Output units '// outu // &
                         ' were not recognized' &
                        )

            call sigerr ('SPICE(UNITSNOTREC)')

            return

         end if

      end if

      if ( type(i) /= type(j) ) then

         call setmsg ('CONVRT: Incompatible units. '   // &
                      'You are attempting to convert ' // &
                      inu  // 'type: ' // type(i)      // &
                      ' to '                           // &
                      outu // 'type: ' // type(j)      // &
                      '.' &
                     )

         call sigerr ('SPICE(INCOMPATIBLEUNITS)')

         return

      end if


      temp = x    * cnvrtn ( i )
      y    = temp / cnvrtn ( j )


      return

      end
