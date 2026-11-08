C$Procedure F_ZZINPDT0 ( ZZINPDT0 tests )
 
      SUBROUTINE F_ZZINPDT0 ( OK )
 
C$ Abstract
C
C     Exercise the private SPICELIB routine ZZINPDT0.
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
      INCLUDE 'dskdsc.inc'
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
C     This routine tests the private SPICELIB routine ZZINPDT0.
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
C        Previous version 16-AUG-2016 (NJB)
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

      INTEGER               NELL
      PARAMETER           ( NELL   = 3 )

      INTEGER               TITLSZ
      PARAMETER           ( TITLSZ = 300 )

C
C     Local Variables 
C
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
C     For the altitude boundaries, there is one valid combination of
C     minimum and maximum for each shape.
C
      DATA                  MINALT  /  -2.0D2,  -1.5D2,  2.0D2 / 
      DATA                  MAXALT  /  -1.0D2,   0.5D2,  3.0D2 / 

      DATA                  EQRAD   /   1.0D4,   0.8D4,  2.0D4 /
      DATA                  POLRAD  /   0.8D4,   1.0D4,  0.5D4 /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZINPDT0' )


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
C     Initialize CORPAR.
C
      CALL CLEARD ( NSYPAR, CORPAR )

C
C --- Case: ------------------------------------------------------
C

C
C     Loop over the volume element cases.
C
      I = 0

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
 
C
C --- Case: ------------------------------------------------------
C
 
C
C                    Set the input point so that each coordinate
C                    is the midpoint of the element's range for
C                    that coordinate.
C
 
                     STEM = 'Lon #:#; Lat #:#; Alt #:#; '
     .               //     'EXCLUD = #;'
 
                     CALL REPMD ( STEM, '#', BOUNDS(1,1)*DPR(),
     .                            9,     STEM                  )
                     CALL REPMD ( STEM, '#', BOUNDS(2,1)*DPR(),
     .                            9,     STEM                  )
                     CALL REPMD ( STEM, '#', BOUNDS(1,2)*DPR(),
     .                            9,     STEM                  )
                     CALL REPMD ( STEM, '#', BOUNDS(2,2)*DPR(),
     .                            9,     STEM                  )
                     CALL REPMD ( STEM, '#', BOUNDS(1,3),
     .                            9,     STEM                  )
                     CALL REPMD ( STEM, '#', BOUNDS(2,3),
     .                            9,     STEM                  )
                     CALL REPMI ( STEM, '#', EXCLUD, STEM )
 
                     CALL CHCKXC ( .FALSE., ' ', OK )
 
 
                     I = I + 1
 
C
C --- Case: ------------------------------------------------------
C
 
                     TITLE = STEM
                     CALL SUFFIX ( 'Midpoint case', 1, TITLE )
                     CALL TCASE(TITLE)

                     MIDLON = ( NRMMIN      + NRMMAX      ) / 2
                     MIDLAT = ( BOUNDS(1,2) + BOUNDS(2,2) ) / 2
                     MIDALT = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2
 
                     CALL GEOREC ( MIDLON, MIDLAT, MIDALT, A, F, P )
                     CALL CHCKXC ( .FALSE., ' ', OK )
 
                     CALL ZZINPDT0( P,     
     .                              MIDLON, BOUNDS, CORPAR,
     .                              EXCLUD, INSIDE         )
                     CALL CHCKXC ( .FALSE., ' ', OK )
 
                     XIN = .TRUE.
 
                     CALL CHCKSL ( 'INSIDE', INSIDE, XIN, OK )

C
C --- Case: ------------------------------------------------------
C
C
C                    Check interior points near each corner of
C                    the volume element.
C
                     DO L = 1, 2
 
                        DO M = 1, 2
 
                           DO N = 1, 2
 

                              I = I + 1
C
C --- Case: ------------------------------------------------------
C
                              TITLE = STEM
 
                              CALL SUFFIX ( 'Point near corner '
     .                        //            '# # #; interior',
     .                                      1, TITLE            )
                              CALL REPMI ( TITLE, '#', L, TITLE )
                              CALL REPMI ( TITLE, '#', M, TITLE )
                              CALL REPMI ( TITLE, '#', N, TITLE )
                              CALL CHCKXC ( .FALSE., ' ', OK )
 
                              CALL TCASE(TITLE)
 
C
C                             Set incremental offsets. AEPS is the
C                             "angular epsilon." EPS is scaled to
C                             yield a "distance epsilon."
C
                              AEPS = 1.D-11
                              EPS  = 1.D-13 * A

C
C                             Obtain the ellipsoid radii of the lower
C                             and upper bounding surfaces. It is these,
C                             not the surfaces of minimum and maximum
C                             altitude relative to the body's reference
C                             ellipsoid, that are used for comparison.
C
                              IF ( A .GT. B ) THEN

                                 CALL ZZELLBDS ( A,    
     .                                           B, 
     .                                           BOUNDS(2,3), 
     .                                           BOUNDS(1,3),
     .                                           EQHI,
     .                                           POLHI,
     .                                           EQLOW,
     .                                           POLLOW       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )


                              ELSE
C
C                                This is the prolate case. ZZELLBDS
C                                works only with oblate spheroids,
C                                so swap A and B on input. 
C
                                 CALL ZZELLBDS ( B,    
     .                                           A, 
     .                                           BOUNDS(2,3), 
     .                                           BOUNDS(1,3),
     .                                           EQHI,
     .                                           POLHI,
     .                                           EQLOW,
     .                                           POLLOW       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

C
C                                The EQ* outputs are actually for
C                                the polar radii of the bounding
C                                ellipsoids, and the POL* outputs
C                                are for the equatorial radii of
C                                the bounding ellipsoids.
C
                                 CALL SWAPD ( EQHI,  POLHI  )
                                 CALL SWAPD ( EQLOW, POLLOW )

                              END IF
C
C                             Multiply the small increments by 1 or
C                             -1 as needed, depending on whether they
C                             are used as offsets from upper or lower
C                             bounds.
C
                              LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )
 
                              IF ( L .EQ. 1 ) THEN
 
                                 LON = NRMMIN  + AEPS
                              ELSE
                                 LON = NRMMAX  - AEPS
                              END IF

C
C                             Find the point at the given longitude
C                             and latitude on the reference ellipsoid.
C                             We'll use this below.
C
                              CALL GEOREC ( LON,    LAT, 0.D0, A, F,
     .                                      VERTEX                  )
                              CALL CHCKXC ( .FALSE., ' ', OK )


                              CALL SURFNM ( A, A, B, VERTEX, NORMAL )


                              IF ( N .EQ. 1 ) THEN
C
C                                Find the surface point on the lower
C                                bounding ellipsoid, at the given
C                                planetodetic longitude and latitude
C                                (relative to the reference ellipsoid).

                                 IF ( BOUNDS(1,3) .LT. 0.D0 ) THEN
C
C                                   The direction to the inner bounding
C                                   ellipsoid is "down."
C                                   
                                    CALL VMINUS ( NORMAL, DIR )
                                 ELSE
                                    CALL VEQU   ( NORMAL, DIR )
                                 END IF

                                 CALL SURFPT ( VERTEX, DIR,
     .                                         EQLOW,  EQLOW, POLLOW, 
     .                                         LOWPT,  FOUND         )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

C
C                                We should never fail to find an
C                                intercept.
C
                                 CALL CHCKSL ( 'FOUND', FOUND,
     .                                         .TRUE.,  OK     )

C
C                                Find the geodetic coordinates of LOWPT
C                                with respect to the reference
C                                ellipsoid.
C
                                 CALL RECGEO ( LOWPT,  A,      F,
     .                                         LOWLON, LOWLAT, LOWALT )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

C
C                                Adjust the altitude of LOWPT by a 
C                                small increment.
C  
                                 ALT = LOWALT +  EPS*( (-1)**(N+1) )


                              ELSE
C
C                                This is the case for the outer
C                                bounding ellipsoid (N = 2).
C
                                 IF ( BOUNDS(2,3) .LT. 0.D0 ) THEN
C
C                                   The direction to the outer bounding
C                                   ellipsoid is "down."
C                                   
                                    CALL VMINUS ( NORMAL, DIR )
                                 ELSE
                                    CALL VEQU   ( NORMAL, DIR )
                                 END IF

                                 CALL SURFPT ( VERTEX, DIR,
     .                                         EQHI,   EQHI, POLHI, 
     .                                         HIGHPT, FOUND       )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

C
C                                We should never fail to find an
C                                intercept.
C
                                 CALL CHCKSL ( 'FOUND', FOUND,
     .                                         .TRUE.,  OK     )
C
C                                Find the geodetic coordinates of
C                                HIGHPT with respect to the reference
C                                ellipsoid.
C
                                 CALL RECGEO ( HIGHPT, A,     F,
     .                                         HILON,  HILAT, HIALT )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

C
C                                Adjust the altitude of LOWPT by a 
C                                small increment.
C  
                                 ALT = HIALT +  EPS*( (-1)**(N+1) )

                              END IF

C
C                             Produce the perturbed input point using
C                             the perturbed geodetic coordinates.
C  
                              CALL GEOREC ( LON, LAT, ALT, A, F, P ) 
                              CALL CHCKXC ( .FALSE., ' ', OK )
                              

                              CALL ZZINPDT0( P,      
     .                                       LON,    BOUNDS, CORPAR,
     .                                       EXCLUD, INSIDE         )
                              CALL CHCKXC ( .FALSE., ' ', OK )
 
                              XIN = .TRUE.
 
                              CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)
 
  
C
C --- Case: ------------------------------------------------------
C
                              TITLE = STEM
                              CALL SUFFIX ( 'Point near corner '
     .                        //            '# # #; interior; '
     .                        //            'excluded coordinate '
     .                        //            'out of range.',
     .                                      1, TITLE            )
                              CALL REPMI ( TITLE, '#', L, TITLE )
                              CALL REPMI ( TITLE, '#', M, TITLE )
                              CALL REPMI ( TITLE, '#', N, TITLE )
                              CALL CHCKXC ( .FALSE., ' ', OK )
 
                              CALL TCASE(TITLE)
 
C
C                             Use the perturbed point produced for
C                             the previous test case. Modify the
C                             excluded coordinate of the point.
C 
                              LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )


                              IF ( EXCLUD .EQ. 2 ) THEN
C
C                                Latitude is not considered in
C                                the bounds comparison performed
C                                by ZZINPDT0.
C
C                                However, we can't move the latitude
C                                too far out of range, or else the
C                                relationship between the altitude
C                                of the point and the bounding ellipsoid
C                                will change too much.
C
                                 IF ( M .EQ. 1 ) THEN
 
                                    LAT = MAX( -HALFPI(), LAT-2*AEPS )
                                 ELSE
                                    LAT = MIN(  HALFPI(), LAT+2*AEPS )
                                 END IF
 
                              END IF
  

C
C                             Set the initial altitude to an innocuous
C                             value.
C                             
                              ALT = ( BOUNDS(1,3) + BOUNDS(2,3) ) / 2


                              IF ( EXCLUD .EQ. 3 ) THEN
C
C                                Altitude is not considered in
C                                the bounds comparison performed
C                                by ZZINPDT0.
C
                                 IF ( N .EQ. 1 ) THEN
 
                                    ALT =  -MIN(A,B)/10
                                 ELSE
                                    ALT =   MIN(A,B) * 1.1D0
                                 END IF
 
                              END IF
 
 
                              IF ( EXCLUD .EQ. 1 ) THEN
C
C                                Longitude is not considered in
C                                the bounds comparison performed
C                                by ZZINPDT0.
C
                                 IF ( N .EQ. 1 ) THEN
 
                                    LON =  NRMMIN - AEPS
                                 ELSE
                                    LON =  NRMMAX + AEPS
                                 END IF
 
                              END IF

 
                              CALL GEOREC ( LON, LAT, ALT, A, F, P ) 
                              CALL CHCKXC ( .FALSE., ' ', OK )
 

                              CALL ZZINPDT0( P,          
     .                                       LON,    BOUNDS, CORPAR,
     .                                       EXCLUD, INSIDE         )
                              CALL CHCKXC ( .FALSE., ' ', OK )
 
                              XIN = .TRUE.
 
                              CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)


 
C
C --- Case: ------------------------------------------------------
C
                              TITLE = STEM
                              CALL SUFFIX ( 'Point near corner '
     .                        //            '# # #; exterior; '
     .                        //            'successor of excluded '
     .                        //            'coordinate out of range.',
     .                                      1, TITLE            )
                              CALL REPMI ( TITLE, '#', L, TITLE )
                              CALL REPMI ( TITLE, '#', M, TITLE )
                              CALL REPMI ( TITLE, '#', N, TITLE )
                              CALL CHCKXC ( .FALSE., ' ', OK )
 
                              CALL TCASE(TITLE)
 
C
C                             We're going to work with a modified
C                             version of the input bounds. This will
C                             make it easier to create coordinates that
C                             are slightly out of range.
C
                              CALL MOVED ( BOUNDS, 6, MODBDS )

C
C                             Use the perturbed point produced for
C                             the previous test case. Modify the
C                             excluded coordinate of the point.
C 

                              LAT = BOUNDS(M,2)+ AEPS*( (-1)**(M+1) )


                              IF ( EXCLUD .EQ. 1 ) THEN
C
C                                Longitude is not considered in
C                                the bounds comparison performed
C                                by ZZINPDT0. We'll move the latitude
C                                out of bounds.
C
C                                However, we can't move the latitude
C                                too far out of range, or else the
C                                relationship between the altitude
C                                of the point and the bounding ellipsoid
C                                will change too much.
C
                                 IF ( M .EQ. 1 ) THEN
 
                                    LAT = MAX( -HALFPI(), LAT-2*AEPS)
                                 ELSE
                                    LAT = MIN(  HALFPI(), LAT+2*AEPS)
                                 END IF
 

                                 LATUB = MAXLAT(LATIX2) .LT.  HALFPI()
                                 LATLB = MINLAT(LATIX1) .GT. -HALFPI()

                              END IF
  
 
                              IF (      ( EXCLUD .EQ. 2 ) 
     .                             .OR. ( EXCLUD .EQ. 0 ) ) THEN
C
C                                Either all coordinates are considered,
C                                or latitude is not considered in
C                                the bounds comparison performed
C                                by ZZINPDT0. We'll move the altitude
C                                out of bounds.
C
                                 IF ( N .EQ. 1 ) THEN
 
                                    ALT =  -MIN(A,B)/10
                                 ELSE
                                    ALT =   MIN(A,B) * 1.1D0
                                 END IF
 
 
                              END IF
 
 
                              IF ( EXCLUD .EQ. 3 ) THEN
C
C                                Altitude is not considered in
C                                the bounds comparison performed
C                                by ZZINPDT0. We'll move the
C                                longitude out of bounds.
C
C                                To make it possible to create
C                                longitude values that are out
C                                of bounds, adjust the longitude
C                                bounds. We'll nudge the lower
C                                longitude bound upwards for this
C                                test.
C
C                                The magnitude of the "nudge" must 
C                                be great enough to overcome the
C                                rounding margin used (even) by the
C                                "no margin" routine ZZINPDT0 for
C                                longitude comparisons.
C
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
C
C                             Perform the test using modified bounds.
C                             See the EXCLUD = 3 case above.
C
                              CALL ZZINPDT0( P,      
     .                                       LON,    MODBDS, CORPAR,
     .                                       EXCLUD, INSIDE         )
                              CALL CHCKXC ( .FALSE., ' ', OK )


C
C                             By default, we expect the point to be 
C                             outside the segment.
C
                              XIN = .FALSE.
C
C                             Our latitude modifications won't work for
C                             the cases where the latitude is already
C                             at an extreme value. 
C
                              IF ( EXCLUD .EQ. 1 ) THEN
C
C                                Longitude is the excluded coordinate;
C                                this is a case for which we try to
C                                modify latitude. However, the
C                                attempted modification may have no
C                                effect if a boundary is already at
C                                the extreme value.
C                                
                                 IF (      (  M    .EQ.  1 ) 
     .                               .AND. ( .NOT. LATLB   ) ) THEN

                                    XIN = .TRUE.

                                 ELSE IF (      (  M    .EQ.  2 ) 
     .                                    .AND. ( .NOT. LATUB   ) ) THEN

                                    XIN = .TRUE.

                                 END IF

                              ELSE 

                                 XIN = .FALSE.

                              END IF

 
                              CALL CHCKSL( 'INSIDE', INSIDE, XIN, OK)


                           END DO
C
C                          End of "N" loop. N selects the altitude
C                          upper/lower bound.
C
                        END DO
C
C                       End of "M" loop. M selects the altitude
C                       upper/lower bound.
C
                     END DO
C
C                    End of "L" loop. L selects the longitude
C                    upper/lower bound.
C
 
                  END DO
C
C                 End of coordinate exclusion (EXCLUD) loop.
C
               END DO
C
C              End of altitude bound loop.
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
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


