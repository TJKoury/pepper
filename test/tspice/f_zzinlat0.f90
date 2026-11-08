!$Procedure F_ZZINLAT0 ( ZZINLAT0 tests )

      SUBROUTINE F_ZZINLAT0 ( OK )

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZINLAT0.
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
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE
      INCLUDE 'dsktol.inc'

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
!     This routine tests the private SPICELIB routine ZZINLAT0.
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
!        Previous version 1.0.0, 25-MAY-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD

!
!     Local Parameters
!
      INTEGER               NLAT
      PARAMETER           ( NLAT   = 6 )

      INTEGER               NLON
      PARAMETER           ( NLON   = 9 )

      INTEGER               NR
      PARAMETER           ( NR     = 4 )

      INTEGER               TITLSZ
      PARAMETER           ( TITLSZ = 200 )

!
!     Local Variables
!
      CHARACTER*(TITLSZ)    STEM
      CHARACTER*(TITLSZ)    TITLE

      DOUBLE PRECISION      AEPS
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      EPS
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      MAXLAT ( NLAT )
      DOUBLE PRECISION      MAXLON ( NLON )
      DOUBLE PRECISION      MAXR   ( NR   )
      DOUBLE PRECISION      MIDLAT
      DOUBLE PRECISION      MIDLON
      DOUBLE PRECISION      MIDR
      DOUBLE PRECISION      MINLAT ( NLAT )
      DOUBLE PRECISION      MINLON ( NLON )
      DOUBLE PRECISION      MINR   ( NR   )
      DOUBLE PRECISION      NRMMAX
      DOUBLE PRECISION      NRMMIN
      DOUBLE PRECISION      R
      DOUBLE PRECISION      TOL

      INTEGER               EXCLUD
      INTEGER               I
      INTEGER               L
      INTEGER               LATIX1
      INTEGER               LATIX2
      INTEGER               LONIX
      INTEGER               M
      INTEGER               N
      INTEGER               RIX1
      INTEGER               RIX2

      LOGICAL               INSIDE
      LOGICAL               LATLB
      LOGICAL               LATUB
      LOGICAL               LONBDS
      LOGICAL               RLB
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
!     For the radius boundaries, every valid combination of
!     minimum and maximum will be tested.
!
      DATA                  MINR    /   0.D0, &
                                        1.D-3, &
                                        1.D3, &
                                        9.999999D3 /

      DATA                  MAXR    /   4 * 1.D4   /



!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZINLAT0' )


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
! --- Case: ------------------------------------------------------
!

!
!     Loop over the volume element cases.
!
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


               DO RIX1 = 1, NR

                  BOUNDS(1,3) = MINR(RIX1)
!
!                 Indicate whether the lower radius boundary is a
!                 surface.
!
                  RLB = ( MINR(RIX1) .GT. 0.D0 )


                  DO RIX2 = RIX1, NR

                     BOUNDS(2,3) = MAXR(RIX2)

                     DO EXCLUD = 0, 3

!
!-- Case: ------------------------------------------------------
!

!
!                       Set the input point so that each coordinate
!                       is the midpoint of the element's range for
!                       that coordinate.
!

                        STEM = 'Lon #:#; Lat #:#; Rad #:#; ' &
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


!
!-- Case: ------------------------------------------------------
!

                        TITLE = STEM
                        CALL SUFFIX ( 'Midpoint case', 1, TITLE )
                        CALL TCASE(TITLE)

!
!                        CALL TOSTDO ( TITLE )
!
                        MIDLON = ( NRMMIN      + NRMMAX      ) / 2
                        MIDLAT = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
                        MIDR   = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2

                        CALL ZZINLAT0 ( MIDR,   MIDLON, MIDLAT, &
                                        BOUNDS, EXCLUD, INSIDE )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        XIN = .TRUE.

                        CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

!
!-- Case: ------------------------------------------------------
!
!
!                       Check interior points near each corner of
!                       the volume element.
!
                        DO L = 1, 2

                           DO M = 1, 2

                              DO N = 1, 2
!
!-- Case: ------------------------------------------------------
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
!                                Set incremental offsets.
!
                                 AEPS = 1.D-13
                                 EPS  = 1.D-13 * BOUNDS(2,3)

!
!                                Multiply the small increments by 1 or
!                                -1 as needed, depending on whether they
!                                are used as offsets from upper or lower
!                                bounds.
!
                                 LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )
                                 R   = BOUNDS(N,3)+  EPS*( (-1)**(N+1) )


                                 IF ( L .EQ. 1 ) THEN

                                    LON = NRMMIN  + AEPS
                                 ELSE
                                    LON = NRMMAX  - AEPS
                                 END IF


                                 CALL ZZINLAT0( R,      LON,    LAT, &
                                                BOUNDS, EXCLUD, INSIDE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 XIN = .TRUE.

                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)

!
!-- Case: ------------------------------------------------------
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
!                                Set incremental offsets.
!
                                 AEPS = 1.D-11
                                 EPS  = 1.D-13 * BOUNDS(2,3)

!
!                                Multiply the small increments by 1 or
!                                -1 as needed, depending on whether they
!                                are used as offsets from upper or lower
!                                bounds.
!
                                 LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )

                                 IF ( EXCLUD .EQ. 2 ) THEN
!
!                                   Latitude is not considered in
!                                   the bounds comparison performed
!                                   by ZZINLAT0.
!
                                    IF ( M .EQ. 1 ) THEN

                                       LAT = -HALFPI()
                                    ELSE
                                       LAT =  HALFPI()
                                    END IF

                                 END IF


                                 R   = BOUNDS(N,3)+  EPS*( (-1)**(N+1) )


                                 IF ( EXCLUD .EQ. 3 ) THEN
!
!                                   Radius is not considered in
!                                   the bounds comparison performed
!                                   by ZZINLAT0.
!
                                    IF ( N .EQ. 1 ) THEN

                                       R   =  0.D0
                                    ELSE
                                       R   =  BOUNDS(2,3) * 2
                                    END IF

                                 END IF



                                 IF ( L .EQ. 1 ) THEN

                                    LON = NRMMIN  + AEPS
                                 ELSE
                                    LON = NRMMAX  - AEPS
                                 END IF


                                 IF ( EXCLUD .EQ. 1 ) THEN
!
!                                   Longitude is not considered in
!                                   the bounds comparison performed
!                                   by ZZINLAT0.
!
                                    IF ( N .EQ. 1 ) THEN

                                       LON =  NRMMIN - AEPS
                                    ELSE
                                       LON =  NRMMAX + AEPS
                                    END IF

                                 END IF


                                 CALL ZZINLAT0( R   ,   LON,    LAT, &
                                                BOUNDS, EXCLUD, INSIDE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 XIN = .TRUE.

                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)

!
!-- Case: ------------------------------------------------------
!
                                 TITLE = STEM
                                 CALL SUFFIX ( 'Point near corner ' &
                                 //            '# # #; exterior; ' &
                                 //            'successor of excluded ' &
                                 //            'coordinate ' &
                                 //            'out of range.', &
                                               1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

!
!                                Give XIN its default value. In most
!                                cases, we'll reset it to .FALSE.
!
                                 XIN = .TRUE.

!
!                                Set incremental offsets.
!
                                 AEPS = 1.D-11
                                 EPS  = 1.D-13 * BOUNDS(2,3)

                                 IF ( EXCLUD .EQ. 1 ) THEN
!
!                                   Set the latitude value out of range.

                                    IF ( M .EQ. 1 ) THEN

                                       LAT = -HALFPI()
!
!                                      If we managed to set the
!                                      latitude to a value below the
!                                      lower latitude bound, then we
!                                      have an exterior point.
!
                                       XIN = .NOT. LATLB

                                    ELSE

                                       LAT =  HALFPI()

                                       XIN = .NOT. LATUB

                                    END IF

                                 ELSE

                                    LAT = MIDLAT

                                 END IF



                                 IF ( EXCLUD .EQ. 2 ) THEN
!
!                                   Set the radius value out of range.
!
                                    IF ( N .EQ. 1 ) THEN

                                       R   =  0.D0

                                       XIN = .NOT. RLB

                                    ELSE

                                       R   =  BOUNDS(2,3) * 2

                                       XIN = .FALSE.

                                    END IF

                                 ELSE

                                    R = MIDR

                                 END IF


                                 IF ( EXCLUD .EQ. 3 ) THEN
!
!                                   Set the longitude value out of
!                                   range.
!
                                    IF ( L .EQ. 1 ) THEN

                                       LON = NRMMIN - AEPS

                                       XIN = .NOT. LONBDS

                                    ELSE

                                       LON = NRMMAX + AEPS

                                       XIN = .NOT. LONBDS

                                    END IF

                                 ELSE

                                    LON = MIDLON

                                 END IF


                                 CALL ZZINLAT0( R,      LON,    LAT, &
                                                BOUNDS, EXCLUD, INSIDE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                XIN has been set.
!
                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)


!
!-- Case: ------------------------------------------------------
!
                                 TITLE = STEM
                                 CALL SUFFIX ( 'Point near corner ' &
                                 //            '# # #; exterior; ' &
                                 //            'predecessor of ' &
                                 //            'excluded coordinate ' &
                                 //            'out of range.', &
                                               1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

!
!                                Give XIN its default value. In most
!                                cases, we'll reset it to .FALSE.
!
                                 XIN = .TRUE.

!
!                                Set incremental offsets.
!
                                 AEPS = 1.D-11
                                 EPS  = 1.D-13 * BOUNDS(2,3)

                                 IF ( EXCLUD .EQ. 3 ) THEN
!
!                                   Set the latitude value out of range.

                                    IF ( M .EQ. 1 ) THEN

                                       LAT = -HALFPI()
!
!                                      If we managed to set the
!                                      latitude to a value below the
!                                      lower latitude bound, then we
!                                      have an exterior point.
!
                                       XIN = .NOT. LATLB

                                    ELSE

                                       LAT =  HALFPI()

                                       XIN = .NOT. LATUB

                                    END IF

                                 ELSE

                                    LAT = MIDLAT

                                 END IF



                                 IF ( EXCLUD .EQ. 1 ) THEN
!
!                                   Set the radius value out of range.
!
                                    IF ( N .EQ. 1 ) THEN

                                       R   =  0.D0

                                       XIN = .NOT. RLB

                                    ELSE

                                       R   =  BOUNDS(2,3) * 2

                                       XIN = .FALSE.

                                    END IF

                                 ELSE

                                    R = MIDR

                                 END IF


                                 IF ( EXCLUD .EQ. 2 ) THEN
!
!                                   Set the longitude value out of
!                                   range.
!
                                    IF ( L .EQ. 1 ) THEN

                                       LON = NRMMIN - AEPS

                                       XIN = .NOT. LONBDS

                                    ELSE

                                       LON = NRMMAX + AEPS

                                       XIN = .NOT. LONBDS

                                    END IF

                                 ELSE

                                    LON = MIDLON

                                 END IF


                                 CALL ZZINLAT0( R,      LON,    LAT, &
                                                BOUNDS, EXCLUD, INSIDE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

!
!                                XIN has been set.
!
                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)

                              END DO
!
!                             End of "N" loop. N selects the radius
!                             upper/lower bound.
!
                           END DO
!
!                          End of "M" loop. M selects the radius
!                          upper/lower bound.
!
                        END DO
!
!                       End of "L" loop. L selects the longitude
!                       upper/lower bound.

                     END DO
!
!                    End of coordinate exclusion (EXCLUD) loop.
!
                  END DO
!
!                 End of upper radius bound loop.
!
               END DO
!
!              End of lower radius bound loop.
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
!     ZZINLAT0 is "error free."
!



!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


