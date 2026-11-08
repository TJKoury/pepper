C$Procedure F_ZZINLAT0 ( ZZINLAT0 tests )
 
      SUBROUTINE F_ZZINLAT0 ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB routine ZZINLAT0.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE
      INCLUDE 'dsktol.inc'

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the private SPICELIB routine ZZINLAT0.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 10-OCT-2016 (NJB)
C
C        Removed non-error case with matching minimum and
C        maximum longitudes.
C
C        Previous version 1.0.0, 25-MAY-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C 
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD

C
C     Local Parameters
C
      INTEGER               NLAT
      PARAMETER           ( NLAT   = 6 )

      INTEGER               NLON
      PARAMETER           ( NLON   = 9 )

      INTEGER               NR
      PARAMETER           ( NR     = 4 )

      INTEGER               TITLSZ
      PARAMETER           ( TITLSZ = 200 )

C
C     Local Variables 
C
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
      

      
C
C     Saved values
C
      SAVE

C
C     Initial values
C

C
C     Set the initial angular bounds in degrees, since we can
C     do this without function calls. The bounds will be 
C     converted to radians at run time.
C

C
C     For the latitude boundaries, every valid combination of
C     minimum and maximum will be tested.
C
      DATA                  MINLAT / -90.D0,
     .                               -89.999999D0,
     .                               -45.D0,
     .                                 0.D0,
     .                                45.D0,
     .                                89.999999D0 /

      DATA                  MAXLAT / -89.999999D0,
     .                               -45.D0,
     .                                 0.D0,
     .                                45.D0,
     .                                89.999999D0,
     .                                90.D0       /


C
C     For the longitude boundaries, each pair of bounds
C     consisting of the Ith minimum and Ith maximum will
C     be tested.
C
      DATA                  MINLON /  -10.D0,
     .                                -10.D0,
     .                               -180.D0,
     .                               -360.D0,
     .                                 10.D0,
     .                                179.999999D0,
     .                               -179.999999D0,
     .                               -260.D0,
     .                                350.D0       /

      DATA                  MAXLON /   -5.D0,
     .                                 20.D0,
     .                                180.D0,
     .                                  0.D0,
     .                                -10.D0,
     .                               -179.999999D0,
     .                                179.999999D0,
     .                                200.D0,
     .                               -350.D0       /

C
C     For the radius boundaries, every valid combination of
C     minimum and maximum will be tested.
C
      DATA                  MINR    /   0.D0,
     .                                  1.D-3,
     .                                  1.D3,
     .                                  9.999999D3 / 

      DATA                  MAXR    /   4 * 1.D4   / 



C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZINLAT0' )


C***********************************************************************
C
C     Normal cases
C
C***********************************************************************
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Initialize coordinate bounds' )
 
C
C     Convert angular bounds to radians.
C
      DO I = 1, NLAT

         MINLAT(I) = MINLAT(I) * RPD()
         MAXLAT(I) = MAXLAT(I) * RPD()
     
      END DO


      DO I = 1, NLON

         MINLON(I) = MINLON(I) * RPD()
         MAXLON(I) = MAXLON(I) * RPD()
     
      END DO



C
C --- Case: ------------------------------------------------------
C

C
C     Loop over the volume element cases.
C
      DO LONIX = 1, NLON         

         BOUNDS(1,1) = MINLON(LONIX)
         BOUNDS(2,1) = MAXLON(LONIX)

C
C        Normalize the element's longitude bounds.
C
         TOL = 1.D-13
 
         CALL ZZNRMLON ( BOUNDS(1,1), BOUNDS(2,1), TOL,
     .                   NRMMIN,      NRMMAX            )
         CALL CHCKXC ( .FALSE., ' ', OK )         
C
C        Indicate whether the longitude boundaries exist (vs 
C        2*pi longitude extent).
C
         LONBDS = ( NRMMAX - NRMMIN ) .LT. (2*PI())- LONALI


         DO LATIX1 = 1, NLAT
 
            BOUNDS(1,2) = MINLAT(LATIX1)
C
C           Indicate whether the lower latitude boundary is a surface.
C
            LATLB = ( MINLAT(LATIX1) .GT. -HALFPI() )

 
            DO LATIX2 = LATIX1, NLAT
 
               BOUNDS(2,2) = MAXLAT(LATIX2)
 
C
C              Indicate whether the upper latitude boundary is a
C              surface.
C
               LATUB = ( MAXLAT(LATIX2) .LT. HALFPI() )


               DO RIX1 = 1, NR
 
                  BOUNDS(1,3) = MINR(RIX1)
C
C                 Indicate whether the lower radius boundary is a
C                 surface.
C
                  RLB = ( MINR(RIX1) .GT. 0.D0 )

 
                  DO RIX2 = RIX1, NR
 
                     BOUNDS(2,3) = MAXR(RIX2)
 
                     DO EXCLUD = 0, 3
  
C
C-- Case: ------------------------------------------------------
C
 
C
C                       Set the input point so that each coordinate
C                       is the midpoint of the element's range for
C                       that coordinate.
C

                        STEM = 'Lon #:#; Lat #:#; Rad #:#; '
     .                  //     'EXCLUD = #;'
 
                        CALL REPMD ( STEM, '#', BOUNDS(1,1)*DPR(),
     .                               9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(2,1)*DPR(),
     .                               9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(1,2)*DPR(),
     .                               9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(2,2)*DPR(),
     .                               9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(1,3),
     .                               9,     STEM                  )
                        CALL REPMD ( STEM, '#', BOUNDS(2,3),
     .                               9,     STEM                  )
                        CALL REPMI ( STEM, '#', EXCLUD, STEM )
 
                        CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C-- Case: ------------------------------------------------------
C
 
                        TITLE = STEM
                        CALL SUFFIX ( 'Midpoint case', 1, TITLE )
                        CALL TCASE(TITLE)
 
C
C                        CALL TOSTDO ( TITLE )
C 
                        MIDLON = ( NRMMIN      + NRMMAX      ) / 2
                        MIDLAT = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
                        MIDR   = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2
 
                        CALL ZZINLAT0 ( MIDR,   MIDLON, MIDLAT,
     .                                  BOUNDS, EXCLUD, INSIDE )
                        CALL CHCKXC ( .FALSE., ' ', OK )
 
                        XIN = .TRUE.
 
                        CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )
 
C
C-- Case: ------------------------------------------------------
C
C
C                       Check interior points near each corner of
C                       the volume element.
C
                        DO L = 1, 2

                           DO M = 1, 2

                              DO N = 1, 2
C
C-- Case: ------------------------------------------------------
C
                                 TITLE = STEM

                                 CALL SUFFIX ( 'Point near corner '
     .                           //            '# # #; interior',
     .                                         1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

C
C                                Set incremental offsets.
C                                
                                 AEPS = 1.D-13
                                 EPS  = 1.D-13 * BOUNDS(2,3)
 
C
C                                Multiply the small increments by 1 or 
C                                -1 as needed, depending on whether they
C                                are used as offsets from upper or lower
C                                bounds.
C
                                 LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )
                                 R   = BOUNDS(N,3)+  EPS*( (-1)**(N+1) )


                                 IF ( L .EQ. 1 ) THEN

                                    LON = NRMMIN  + AEPS
                                 ELSE
                                    LON = NRMMAX  - AEPS
                                 END IF

 
                                 CALL ZZINLAT0( R,      LON,    LAT,
     .                                          BOUNDS, EXCLUD, INSIDE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )
 
                                 XIN = .TRUE.
 
                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)

C
C-- Case: ------------------------------------------------------
C
                                 TITLE = STEM
                                 CALL SUFFIX ( 'Point near corner '
     .                           //            '# # #; interior; '
     .                           //            'excluded coordinate '
     .                           //            'out of range.',
     .                                         1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

C
C                                Set incremental offsets.
C                                
                                 AEPS = 1.D-11
                                 EPS  = 1.D-13 * BOUNDS(2,3)
 
C
C                                Multiply the small increments by 1 or 
C                                -1 as needed, depending on whether they
C                                are used as offsets from upper or lower
C                                bounds.
C
                                 LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )

                                 IF ( EXCLUD .EQ. 2 ) THEN
C
C                                   Latitude is not considered in
C                                   the bounds comparison performed
C                                   by ZZINLAT0.
C
                                    IF ( M .EQ. 1 ) THEN

                                       LAT = -HALFPI()
                                    ELSE
                                       LAT =  HALFPI()
                                    END IF
                                    
                                 END IF


                                 R   = BOUNDS(N,3)+  EPS*( (-1)**(N+1) )


                                 IF ( EXCLUD .EQ. 3 ) THEN
C
C                                   Radius is not considered in
C                                   the bounds comparison performed
C                                   by ZZINLAT0.
C
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
C
C                                   Longitude is not considered in
C                                   the bounds comparison performed
C                                   by ZZINLAT0.
C
                                    IF ( N .EQ. 1 ) THEN

                                       LON =  NRMMIN - AEPS
                                    ELSE
                                       LON =  NRMMAX + AEPS
                                    END IF
                                    
                                 END IF


                                 CALL ZZINLAT0( R   ,   LON,    LAT,
     .                                          BOUNDS, EXCLUD, INSIDE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )
 
                                 XIN = .TRUE.
 
                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)

C
C-- Case: ------------------------------------------------------
C
                                 TITLE = STEM
                                 CALL SUFFIX ( 'Point near corner '
     .                           //            '# # #; exterior; '
     .                           //            'successor of excluded '
     .                           //            'coordinate '
     .                           //            'out of range.',
     .                                         1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

C
C                                Give XIN its default value. In most
C                                cases, we'll reset it to .FALSE.
C
                                 XIN = .TRUE.

C
C                                Set incremental offsets.
C                                
                                 AEPS = 1.D-11
                                 EPS  = 1.D-13 * BOUNDS(2,3)
 
                                 IF ( EXCLUD .EQ. 1 ) THEN
C
C                                   Set the latitude value out of range.

                                    IF ( M .EQ. 1 ) THEN

                                       LAT = -HALFPI()
C
C                                      If we managed to set the
C                                      latitude to a value below the
C                                      lower latitude bound, then we
C                                      have an exterior point.
C
                                       XIN = .NOT. LATLB

                                    ELSE

                                       LAT =  HALFPI()

                                       XIN = .NOT. LATUB

                                    END IF

                                 ELSE

                                    LAT = MIDLAT 

                                 END IF



                                 IF ( EXCLUD .EQ. 2 ) THEN
C
C                                   Set the radius value out of range.
C
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
C
C                                   Set the longitude value out of
C                                   range.
C
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
 

                                 CALL ZZINLAT0( R,      LON,    LAT,
     .                                          BOUNDS, EXCLUD, INSIDE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C                                XIN has been set.
C 
                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)

 
C
C-- Case: ------------------------------------------------------
C
                                 TITLE = STEM
                                 CALL SUFFIX ( 'Point near corner '
     .                           //            '# # #; exterior; '
     .                           //            'predecessor of '
     .                           //            'excluded coordinate '
     .                           //            'out of range.',
     .                                         1, TITLE            )
                                 CALL REPMI ( TITLE, '#', L, TITLE )
                                 CALL REPMI ( TITLE, '#', M, TITLE )
                                 CALL REPMI ( TITLE, '#', N, TITLE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL TCASE(TITLE)

C
C                                Give XIN its default value. In most
C                                cases, we'll reset it to .FALSE.
C
                                 XIN = .TRUE.

C
C                                Set incremental offsets.
C                                
                                 AEPS = 1.D-11
                                 EPS  = 1.D-13 * BOUNDS(2,3)
 
                                 IF ( EXCLUD .EQ. 3 ) THEN
C
C                                   Set the latitude value out of range.

                                    IF ( M .EQ. 1 ) THEN

                                       LAT = -HALFPI()
C
C                                      If we managed to set the
C                                      latitude to a value below the
C                                      lower latitude bound, then we
C                                      have an exterior point.
C
                                       XIN = .NOT. LATLB

                                    ELSE

                                       LAT =  HALFPI()

                                       XIN = .NOT. LATUB

                                    END IF

                                 ELSE

                                    LAT = MIDLAT 

                                 END IF



                                 IF ( EXCLUD .EQ. 1 ) THEN
C
C                                   Set the radius value out of range.
C
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
C
C                                   Set the longitude value out of
C                                   range.
C
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
 

                                 CALL ZZINLAT0( R,      LON,    LAT,
     .                                          BOUNDS, EXCLUD, INSIDE )
                                 CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C                                XIN has been set.
C 
                                 CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)
 
                              END DO
C
C                             End of "N" loop. N selects the radius
C                             upper/lower bound.
C                             
                           END DO
C
C                          End of "M" loop. M selects the radius
C                          upper/lower bound.
C
                        END DO
C
C                       End of "L" loop. L selects the longitude
C                       upper/lower bound.

                     END DO
C
C                    End of coordinate exclusion (EXCLUD) loop.
C 
                  END DO
C
C                 End of upper radius bound loop.
C
               END DO
C
C              End of lower radius bound loop.
C 
            END DO
C
C           End of upper latitude bound loop. 
C
         END DO
C
C        End of lower latitude bound loop.
C
      END DO
C
C     End of longitude loop.
C
 

C***********************************************************************
C
C     Error cases
C
C***********************************************************************

C
C     ZZINLAT0 is "error free."
C

 

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


