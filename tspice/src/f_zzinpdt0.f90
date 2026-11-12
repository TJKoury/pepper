!$Procedure F_ZZINPDT0 ( ZZINPDT0 tests )

      SUBROUTINE F_ZZINPDT0 ( OK )
      use pepper_legacy
      use pepper_inc_dskdsc
      use pepper_inc_dsktol

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZINPDT0.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the private SPICELIB routine ZZINPDT0.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 10-OCT-2016 (NJB)
!
!        Removed non-error case with matching minimum and
!        maximum longitudes.
!
!        Previous version 16-AUG-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      INTEGER               NLAT
      PARAMETER           ( NLAT   = 6 )

      INTEGER               NLON
      PARAMETER           ( NLON   = 9 )

      INTEGER               NELL
      PARAMETER           ( NELL   = 3 )

      INTEGER               TITLSZ
      PARAMETER           ( TITLSZ = 300 )

!
!     Local Variables
!
      CHARACTER*(TITLSZ)    STEM
      CHARACTER*(TITLSZ)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      AEPS
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      EPS
      DOUBLE PRECISION      EQHI
      DOUBLE PRECISION      EQLOW
      DOUBLE PRECISION      EQRAD  ( NELL )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      HIALT
      DOUBLE PRECISION      HIGHPT ( 3 )
      DOUBLE PRECISION      HILAT
      DOUBLE PRECISION      HILON
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LOWALT
      DOUBLE PRECISION      LOWLAT
      DOUBLE PRECISION      LOWLON
      DOUBLE PRECISION      LOWPT  ( 3 )
      DOUBLE PRECISION      MAXALT ( NELL )
      DOUBLE PRECISION      MAXLAT ( NLAT )
      DOUBLE PRECISION      MAXLON ( NLON )
      DOUBLE PRECISION      MIDLAT
      DOUBLE PRECISION      MIDLON
      DOUBLE PRECISION      MIDALT
      DOUBLE PRECISION      MINALT ( NELL )
      DOUBLE PRECISION      MINLAT ( NLAT )
      DOUBLE PRECISION      MINLON ( NLON )
      DOUBLE PRECISION      MODBDS ( 2, 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      NRMMAX
      DOUBLE PRECISION      NRMMIN
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      POLHI
      DOUBLE PRECISION      POLLOW
      DOUBLE PRECISION      POLRAD ( NELL )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )

      INTEGER               EXCLUD
      INTEGER               I
      INTEGER               L
      INTEGER               LATIX1
      INTEGER               LATIX2
      INTEGER               LONIX
      INTEGER               M
      INTEGER               N
      INTEGER               SHAPIX

      LOGICAL               FOUND
      LOGICAL               INSIDE
      LOGICAL               LATLB
      LOGICAL               LATUB
      LOGICAL               LONBDS
      LOGICAL               XIN

!
!     Saved values
!
      SAVE

!
!     Initial values
!

!
!     Set the initial angular bounds in degrees, since we can
!     do this without function calls. The bounds will be
!     converted to radians at run time.
!

!
!     For the latitude boundaries, every valid combination of
!     minimum and maximum will be tested.
!
      DATA                  MINLAT / -90.D0, &
                                     -89.999999D0, &
                                     -45.D0, &
                                       0.D0, &
                                      45.D0, &
                                      89.999999D0 /

      DATA                  MAXLAT / -89.999999D0, &
                                     -45.D0, &
                                       0.D0, &
                                      45.D0, &
                                      89.999999D0, &
                                      90.D0       /


!
!     For the longitude boundaries, each pair of bounds
!     consisting of the Ith minimum and Ith maximum will
!     be tested.
!
      DATA                  MINLON /  -10.D0, &
                                      -10.D0, &
                                     -180.D0, &
                                     -360.D0, &
                                       10.D0, &
                                      179.999999D0, &
                                     -179.999999D0, &
                                     -260.D0, &
                                      350.D0       /

      DATA                  MAXLON /   -5.D0, &
                                       20.D0, &
                                      180.D0, &
                                        0.D0, &
                                      -10.D0, &
                                     -179.999999D0, &
                                      179.999999D0, &
                                      200.D0, &
                                     -350.D0       /

!
!     For the altitude boundaries, there is one valid combination of
!     minimum and maximum for each shape.
!
      DATA                  MINALT  /  -2.0D2,  -1.5D2,  2.0D2 /
      DATA                  MAXALT  /  -1.0D2,   0.5D2,  3.0D2 /

      DATA                  EQRAD   /   1.0D4,   0.8D4,  2.0D4 /
      DATA                  POLRAD  /   0.8D4,   1.0D4,  0.5D4 /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZINPDT0' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize coordinate bounds' )

!
!     Convert angular bounds to radians.
!
      DO I = 1, NLAT

         MINLAT(I) = MINLAT(I) * RPD()
         MAXLAT(I) = MAXLAT(I) * RPD()

      END DO


      DO I = 1, NLON

         MINLON(I) = MINLON(I) * RPD()
         MAXLON(I) = MAXLON(I) * RPD()

      END DO

!
!     Initialize CORPAR.
!
      CALL CLEARD ( NSYPAR, CORPAR )

!
! --- Case: ------------------------------------------------------
!

!
!     Loop over the volume element cases.
!
      I = 0

      DO LONIX = 1, NLON

         BOUNDS(1,1) = MINLON(LONIX)
         BOUNDS(2,1) = MAXLON(LONIX)

!
!        Normalize the element's longitude bounds.
!
         TOL = 1.D-13

         CALL ZZNRMLON ( BOUNDS(1,1), BOUNDS(2,1), TOL, &
                         NRMMIN,      NRMMAX            )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Indicate whether the longitude boundaries exist (vs
!        2*pi longitude extent).
!
         LONBDS = ( NRMMAX - NRMMIN ) .LT. (2*PI())- LONALI


         DO LATIX1 = 1, NLAT

            BOUNDS(1,2) = MINLAT(LATIX1)
!
!           Indicate whether the lower latitude boundary is a surface.
!
            LATLB = ( MINLAT(LATIX1) .GT. -HALFPI() )


            DO LATIX2 = LATIX1, NLAT

               BOUNDS(2,2) = MAXLAT(LATIX2)

!
!              Indicate whether the upper latitude boundary is a
!              surface.
!
               LATUB = ( MAXLAT(LATIX2) .LT. HALFPI() )


               DO SHAPIX = 1, NELL

                  A           = EQRAD (SHAPIX)
                  B           = POLRAD(SHAPIX)
                  F           = ( A - B ) / A

                  CORPAR(1)   = A
                  CORPAR(2)   = F

                  BOUNDS(1,3) = MINALT(SHAPIX)
                  BOUNDS(2,3) = MAXALT(SHAPIX)

                  DO EXCLUD = 0, 3


                     I = I + 1

!
! --- Case: ------------------------------------------------------
!

!
!                    Set the input point so that each coordinate
!                    is the midpoint of the element's range for
!                    that coordinate.
!

                     STEM = 'Lon #:#; Lat #:#; Alt #:#; ' &
                     //     'EXCLUD = #;'

                     CALL REPMD ( STEM, '#', BOUNDS(1,1)*DPR(), &
                                  9,     STEM                  )
                     CALL REPMD ( STEM, '#', BOUNDS(2,1)*DPR(), &
                                  9,     STEM                  )
                     CALL REPMD ( STEM, '#', BOUNDS(1,2)*DPR(), &
                                  9,     STEM                  )
                     CALL REPMD ( STEM, '#', BOUNDS(2,2)*DPR(), &
                                  9,     STEM                  )
                     CALL REPMD ( STEM, '#', BOUNDS(1,3), &
                                  9,     STEM                  )
                     CALL REPMD ( STEM, '#', BOUNDS(2,3), &
                                  9,     STEM                  )
                     CALL REPMI ( STEM, '#', EXCLUD, STEM )

                     CALL CHCKXC ( .FALSE., ' ', OK )


                     I = I + 1

!
! --- Case: ------------------------------------------------------
!

                     TITLE = STEM
                     CALL SUFFIX ( 'Midpoint case', 1, TITLE )
                     CALL TCASE(TITLE)

                     MIDLON = ( NRMMIN      + NRMMAX      ) / 2
                     MIDLAT = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
                     MIDALT = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

                     CALL GEOREC ( MIDLON, MIDLAT, MIDALT, A, F, P )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL ZZINPDT0( P, &
                                    MIDLON, BOUNDS, CORPAR, &
                                    EXCLUD, INSIDE         )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     XIN = .TRUE.

                     CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

!
! --- Case: ------------------------------------------------------
!
!
!                    Check interior points near each corner of
!                    the volume element.
!
                     DO L = 1, 2

                        DO M = 1, 2

                           DO N = 1, 2


                              I = I + 1
!
! --- Case: ------------------------------------------------------
!
                              TITLE = STEM

                              CALL SUFFIX ( 'Point near corner ' &
                              //            '# # #; interior', &
                                            1, TITLE            )
                              CALL REPMI ( TITLE, '#', L, TITLE )
                              CALL REPMI ( TITLE, '#', M, TITLE )
                              CALL REPMI ( TITLE, '#', N, TITLE )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL TCASE(TITLE)

!
!                             Set incremental offsets. AEPS is the
!                             "angular epsilon." EPS is scaled to
!                             yield a "distance epsilon."
!
                              AEPS = 1.D-11
                              EPS  = 1.D-13 * A

!
!                             Obtain the ellipsoid radii of the lower
!                             and upper bounding surfaces. It is these,
!                             not the surfaces of minimum and maximum
!                             altitude relative to the body's reference
!                             ellipsoid, that are used for comparison.
!
                              IF ( A .GT. B ) THEN

                                 CALL ZZELLBDS ( A, &
                                                 B, &
                                                 BOUNDS(2,3), &
                                                 BOUNDS(1,3), &
                                                 EQHI, &
                                                 POLHI, &
                                                 EQLOW, &
                                                 POLLOW       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )


                              ELSE
!
!                                This is the prolate case. ZZELLBDS
!                                works only with oblate spheroids,
!                                so swap A and B on input.
!
                                 CALL ZZELLBDS ( B, &
                                                 A, &
                                                 BOUNDS(2,3), &
                                                 BOUNDS(1,3), &
                                                 EQHI, &
                                                 POLHI, &
                                                 EQLOW, &
                                                 POLLOW       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                The EQ* outputs are actually for
!                                the polar radii of the bounding
!                                ellipsoids, and the POL* outputs
!                                are for the equatorial radii of
!                                the bounding ellipsoids.
!
                                 CALL SWAPD ( EQHI,  POLHI  )
                                 CALL SWAPD ( EQLOW, POLLOW )

                              END IF
!
!                             Multiply the small increments by 1 or
!                             -1 as needed, depending on whether they
!                             are used as offsets from upper or lower
!                             bounds.
!
                              LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )

                              IF ( L .EQ. 1 ) THEN

                                 LON = NRMMIN  + AEPS
                              ELSE
                                 LON = NRMMAX  - AEPS
                              END IF

!
!                             Find the point at the given longitude
!                             and latitude on the reference ellipsoid.
!                             We'll use this below.
!
                              CALL GEOREC ( LON,    LAT, 0.D0, A, F, &
                                            VERTEX                  )
                              CALL CHCKXC ( .FALSE., ' ', OK )


                              CALL SURFNM ( A, A, B, VERTEX, NORMAL )


                              IF ( N .EQ. 1 ) THEN
!
!                                Find the surface point on the lower
!                                bounding ellipsoid, at the given
!                                planetodetic longitude and latitude
!                                (relative to the reference ellipsoid).

                                 IF ( BOUNDS(1,3) .LT. 0.D0 ) THEN
!
!                                   The direction to the inner bounding
!                                   ellipsoid is "down."
!
                                    CALL VMINUS ( NORMAL, DIR )
                                 ELSE
                                    CALL VEQU   ( NORMAL, DIR )
                                 END IF

                                 CALL SURFPT ( VERTEX, DIR, &
                                               EQLOW,  EQLOW, POLLOW, &
                                               LOWPT,  FOUND         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                We should never fail to find an
!                                intercept.
!
                                 CALL CHCKSL ( 'FOUND', FOUND, &
                                               .TRUE.,  OK     )

!
!                                Find the geodetic coordinates of LOWPT
!                                with respect to the reference
!                                ellipsoid.
!
                                 CALL RECGEO ( LOWPT,  A,      F, &
                                               LOWLON, LOWLAT, LOWALT )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                Adjust the altitude of LOWPT by a
!                                small increment.
!
                                 ALT = LOWALT +  EPS*( (-1)**(N+1) )


                              ELSE
!
!                                This is the case for the outer
!                                bounding ellipsoid (N = 2).
!
                                 IF ( BOUNDS(2,3) .LT. 0.D0 ) THEN
!
!                                   The direction to the outer bounding
!                                   ellipsoid is "down."
!
                                    CALL VMINUS ( NORMAL, DIR )
                                 ELSE
                                    CALL VEQU   ( NORMAL, DIR )
                                 END IF

                                 CALL SURFPT ( VERTEX, DIR, &
                                               EQHI,   EQHI, POLHI, &
                                               HIGHPT, FOUND       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                We should never fail to find an
!                                intercept.
!
                                 CALL CHCKSL ( 'FOUND', FOUND, &
                                               .TRUE.,  OK     )
!
!                                Find the geodetic coordinates of
!                                HIGHPT with respect to the reference
!                                ellipsoid.
!
                                 CALL RECGEO ( HIGHPT, A,     F, &
                                               HILON,  HILAT, HIALT )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                Adjust the altitude of LOWPT by a
!                                small increment.
!
                                 ALT = HIALT +  EPS*( (-1)**(N+1) )

                              END IF

!
!                             Produce the perturbed input point using
!                             the perturbed geodetic coordinates.
!
                              CALL GEOREC ( LON, LAT, ALT, A, F, P )
                              CALL CHCKXC ( .FALSE., ' ', OK )


                              CALL ZZINPDT0( P, &
                                             LON,    BOUNDS, CORPAR, &
                                             EXCLUD, INSIDE         )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              XIN = .TRUE.

                              CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)


!
! --- Case: ------------------------------------------------------
!
                              TITLE = STEM
                              CALL SUFFIX ( 'Point near corner ' &
                              //            '# # #; interior; ' &
                              //            'excluded coordinate ' &
                              //            'out of range.', &
                                            1, TITLE            )
                              CALL REPMI ( TITLE, '#', L, TITLE )
                              CALL REPMI ( TITLE, '#', M, TITLE )
                              CALL REPMI ( TITLE, '#', N, TITLE )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL TCASE(TITLE)

!
!                             Use the perturbed point produced for
!                             the previous test case. Modify the
!                             excluded coordinate of the point.
!
                              LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )


                              IF ( EXCLUD .EQ. 2 ) THEN
!
!                                Latitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT0.
!
!                                However, we can't move the latitude
!                                too far out of range, or else the
!                                relationship between the altitude
!                                of the point and the bounding ellipsoid
!                                will change too much.
!
                                 IF ( M .EQ. 1 ) THEN

                                    LAT = MAX( -HALFPI(), LAT-2*AEPS )
                                 ELSE
                                    LAT = MIN(  HALFPI(), LAT+2*AEPS )
                                 END IF

                              END IF


!
!                             Set the initial altitude to an innocuous
!                             value.
!
                              ALT = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2


                              IF ( EXCLUD .EQ. 3 ) THEN
!
!                                Altitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT0.
!
                                 IF ( N .EQ. 1 ) THEN

                                    ALT =  -MIN(A,B)/10
                                 ELSE
                                    ALT =   MIN(A,B) * 1.1D0
                                 END IF

                              END IF


                              IF ( EXCLUD .EQ. 1 ) THEN
!
!                                Longitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT0.
!
                                 IF ( N .EQ. 1 ) THEN

                                    LON =  NRMMIN - AEPS
                                 ELSE
                                    LON =  NRMMAX + AEPS
                                 END IF

                              END IF


                              CALL GEOREC ( LON, LAT, ALT, A, F, P )
                              CALL CHCKXC ( .FALSE., ' ', OK )


                              CALL ZZINPDT0( P, &
                                             LON,    BOUNDS, CORPAR, &
                                             EXCLUD, INSIDE         )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              XIN = .TRUE.

                              CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)



!
! --- Case: ------------------------------------------------------
!
                              TITLE = STEM
                              CALL SUFFIX ( 'Point near corner ' &
                              //            '# # #; exterior; ' &
                              //            'successor of excluded ' &
                              //            'coordinate out of range.', &
                                            1, TITLE            )
                              CALL REPMI ( TITLE, '#', L, TITLE )
                              CALL REPMI ( TITLE, '#', M, TITLE )
                              CALL REPMI ( TITLE, '#', N, TITLE )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL TCASE(TITLE)

!
!                             We're going to work with a modified
!                             version of the input bounds. This will
!                             make it easier to create coordinates that
!                             are slightly out of range.
!
                              CALL MOVED ( BOUNDS, 6, MODBDS )

!
!                             Use the perturbed point produced for
!                             the previous test case. Modify the
!                             excluded coordinate of the point.
!

                              LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )


                              IF ( EXCLUD .EQ. 1 ) THEN
!
!                                Longitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT0. We'll move the latitude
!                                out of bounds.
!
!                                However, we can't move the latitude
!                                too far out of range, or else the
!                                relationship between the altitude
!                                of the point and the bounding ellipsoid
!                                will change too much.
!
                                 IF ( M .EQ. 1 ) THEN

                                    LAT = MAX( -HALFPI(), LAT-2*AEPS)
                                 ELSE
                                    LAT = MIN(  HALFPI(), LAT+2*AEPS)
                                 END IF


                                 LATUB = MAXLAT(LATIX2) .LT.  HALFPI()
                                 LATLB = MINLAT(LATIX1) .GT. -HALFPI()

                              END IF


                              IF (      ( EXCLUD .EQ. 2 ) &
                                   .OR. ( EXCLUD .EQ. 0 ) ) THEN
!
!                                Either all coordinates are considered,
!                                or latitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT0. We'll move the altitude
!                                out of bounds.
!
                                 IF ( N .EQ. 1 ) THEN

                                    ALT =  -MIN(A,B)/10
                                 ELSE
                                    ALT =   MIN(A,B) * 1.1D0
                                 END IF


                              END IF


                              IF ( EXCLUD .EQ. 3 ) THEN
!
!                                Altitude is not considered in
!                                the bounds comparison performed
!                                by ZZINPDT0. We'll move the
!                                longitude out of bounds.
!
!                                To make it possible to create
!                                longitude values that are out
!                                of bounds, adjust the longitude
!                                bounds. We'll nudge the lower
!                                longitude bound upwards for this
!                                test.
!
!                                The magnitude of the "nudge" must
!                                be great enough to overcome the
!                                rounding margin used (even) by the
!                                "no margin" routine ZZINPDT0 for
!                                longitude comparisons.
!
                                 MODBDS(1,1) = MODBDS(1,1) + 1.D-9
                                 MODBDS(2,1) = MODBDS(2,1) - 1.D-9

                                 IF ( N .EQ. 1 ) THEN

                                    LON =  NRMMIN - 10*ANGMRG
                                 ELSE
                                    LON =  NRMMAX + 10*ANGMRG
                                 END IF

                              END IF


                              CALL GEOREC ( LON, LAT, ALT, A, F, P )
                              CALL CHCKXC ( .FALSE., ' ', OK )
!
!                             Perform the test using modified bounds.
!                             See the EXCLUD = 3 case above.
!
                              CALL ZZINPDT0( P, &
                                             LON,    MODBDS, CORPAR, &
                                             EXCLUD, INSIDE         )
                              CALL CHCKXC ( .FALSE., ' ', OK )


!
!                             By default, we expect the point to be
!                             outside the segment.
!
                              XIN = .FALSE.
!
!                             Our latitude modifications won't work for
!                             the cases where the latitude is already
!                             at an extreme value.
!
                              IF ( EXCLUD .EQ. 1 ) THEN
!
!                                Longitude is the excluded coordinate;
!                                this is a case for which we try to
!                                modify latitude. However, the
!                                attempted modification may have no
!                                effect if a boundary is already at
!                                the extreme value.
!
                                 IF (      (  M    .EQ.  1 ) &
                                     .AND. ( .NOT. LATLB   ) ) THEN

                                    XIN = .TRUE.

                                 ELSE IF (      (  M    .EQ.  2 ) &
                                          .AND. ( .NOT. LATUB   ) ) THEN

                                    XIN = .TRUE.

                                 END IF

                              ELSE

                                 XIN = .FALSE.

                              END IF


                              CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)


                           END DO
!
!                          End of "N" loop. N selects the altitude
!                          upper/lower bound.
!
                        END DO
!
!                       End of "M" loop. M selects the altitude
!                       upper/lower bound.
!
                     END DO
!
!                    End of "L" loop. L selects the longitude
!                    upper/lower bound.
!

                  END DO
!
!                 End of coordinate exclusion (EXCLUD) loop.
!
               END DO
!
!              End of altitude bound loop.
!
            END DO
!
!           End of upper latitude bound loop.
!
         END DO
!
!        End of lower latitude bound loop.
!
      END DO
!
!     End of longitude loop.
!


!***********************************************************************
!
!     Error cases
!
!***********************************************************************

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


