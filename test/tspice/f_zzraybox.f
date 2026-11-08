C$Procedure F_ZZRAYBOX ( ray-box intercept tests )
 
      SUBROUTINE F_ZZRAYBOX ( OK )
 
C$ Abstract
C
C     Exercise the ray-box intercept routine ZZRAYBOX.
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
C     This routine test the DSKLIB ray-box intercept routine
C     ZZRAYBOX.
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
C-    TSPICE Version 1.0.0, 05-JUN-2014 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
C      DOUBLE PRECISION      VDIST

C
C     Local Parameters
C
      DOUBLE PRECISION      VTIGHT  
      PARAMETER           ( VTIGHT = 1.D-14 )
 
C      DOUBLE PRECISION      TIGHT
C      PARAMETER           ( TIGHT  = 1.D-12 )

      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 50 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )


C
C     Local Variables 
C 
      CHARACTER*(LBLSIZ)    LABEL
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      BIGRAD 
      DOUBLE PRECISION      BOXORI ( 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      DELTA  ( 3 )
      DOUBLE PRECISION      EXTENT ( 3 )
      DOUBLE PRECISION      MEDRAD
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SMLRAD
      DOUBLE PRECISION      TARG   ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )

      INTEGER               H1
      INTEGER               H2
      INTEGER               HCIDX1
      INTEGER               HCIDX2
      INTEGER               I
      INTEGER               J
      INTEGER               NEXT   ( 3 )
      INTEGER               NSTEP
      INTEGER               V1
      INTEGER               V2

      LOGICAL               FOUND

      LOGICAL               OUT1
      LOGICAL               OUT2

C
C     Saved values
C
      SAVE 
C
C     Initial values
C
      DATA                 NEXT / 2, 3, 1 /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZRAYBOX' )

C**********************************************************************
C
C     Error cases
C
C**********************************************************************
     
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Ray direction is zero vector.' )

      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )
      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )

      CALL VPACK (  1.D3,  1.D3,  1.D3, VERTEX )

      CALL CLEARD ( 3, RAYDIR )

      CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
      CALL CHCKXC   ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

      
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Non-positive extents' )

      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )
      CALL VPACK (  1.D3,  1.D3,  1.D3, VERTEX )

      CALL VMINUS ( VERTEX, RAYDIR )

      CALL VPACK (  0.D0, 20.D0, 10.D0, EXTENT )

      CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
      CALL CHCKXC   ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL VPACK ( -1.D0, 20.D0, 10.D0, EXTENT )

      CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
      CALL CHCKXC   ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL VPACK (  10.D0,  0.D0, 10.D0, EXTENT )

      CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
      CALL CHCKXC   ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL VPACK (  10.D0, -1.D0, 10.D0, EXTENT )

      CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
      CALL CHCKXC   ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL VPACK (  10.D0, 20.D0,  0.D0, EXTENT )

      CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
      CALL CHCKXC   ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL VPACK (  10.D0, 20.D0, -1.D0, EXTENT )

      CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
      CALL CHCKXC   ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      


C**********************************************************************
C
C     Normal cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Vertex is inside box.' )


      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )
      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )

      CALL VPACK ( 15.D0, 10.D0,  5.D0, VERTEX )
      CALL VEQU  ( VERTEX, RAYDIR )

      CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
      CALL CHCKXC   ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      IF ( FOUND ) THEN
         CALL CHCKAD ( 'XPT', XPT, '=', VERTEX, 3, 0.D0, OK )
      END IF

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Simple non-intersection case. Vertices '
     .//           'are outside the bounding sphere.'       )

C
C     The ray points directly away from the box in each case.
C
      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )
      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )


      CALL VLCOM ( 1.D0, BOXORI, 0.5D0, EXTENT, CENTER )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL VPACK ( 15.D0, 10.D0,  5.D0, VERTEX )

      DO I = 1, 3

         DO J = 1, 2
C
C           S is 1 for J = 1; -1 for J = 2.
C
            S = 2*J - 3


            CALL VEQU ( CENTER, VERTEX )
            VERTEX(I) = CENTER(I) + S*EXTENT(I) * 3

            CALL CLEARD ( 3, RAYDIR )
            RAYDIR(I) = S

            CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
            CALL CHCKXC   ( .FALSE., ' ', OK )
            
            LABEL = 'FOUND # #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )

         END DO

      END DO

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Simple non-intersection case. Vertices '
     .//           'are inside the bounding sphere.'       )

C
C     The ray points directly away from the box in each case.
C
      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )
      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )

      CALL VLCOM ( 1.D0, BOXORI, 0.5D0, EXTENT, CENTER )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL VPACK ( 15.D0, 10.D0,  5.D0, VERTEX )
      CALL VEQU  ( VERTEX, RAYDIR )

      DO I = 1, 3

         DO J = 1, 2
C
C           S is 1 for J = 1; -1 for J = 2.
C
            S = 2*J - 3


            CALL VEQU ( CENTER, VERTEX )
            VERTEX(I) = CENTER(I) +   S * EXTENT(I) * 0.500001D0

            CALL CLEARD ( 3, RAYDIR )
            RAYDIR(I) = S

            CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
            CALL CHCKXC   ( .FALSE., ' ', OK )
            
            LABEL = 'FOUND # #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )

         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Simple intersection case' )

C
C     The ray points directly toward from the box in each case.
C
      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )
      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )


      CALL VLCOM ( 1.D0, BOXORI, 0.5D0, EXTENT, CENTER )
      CALL CHCKXC( .FALSE., ' ', OK )


      DO I = 1, 3

         DO J = 1, 2
C
C           S is -1 for J = 1; 1 for J = 2.
C
            S = 2*J - 3

C
C           Select a vertex outside the bounding sphere.
C
            CALL VEQU ( CENTER, VERTEX )
            VERTEX(I) = CENTER(I) +  S * EXTENT(I) * 3

            CALL CLEARD ( 3, RAYDIR )
            RAYDIR(I) = -S


            CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
            CALL CHCKXC   ( .FALSE., ' ', OK )
            
            LABEL = 'FOUND # #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

            IF ( FOUND ) THEN
            
               CALL VEQU ( CENTER, XXPT )
               XXPT(I) = CENTER(I) + S*EXTENT(I)/2

               LABEL = 'XPT # #'
               CALL REPMI ( LABEL, '#', I, LABEL )
               CALL REPMI ( LABEL, '#', J, LABEL )

               TOL = VTIGHT

               CALL CHCKAD ( LABEL, XPT, '~~/', XXPT, 3, TOL, OK )

            END IF

         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Simple intersection case; vertex is inside ' 
     .//           'the bounding sphere.'                        )

C
C     The ray points directly toward from the box in each case.
C
      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )
      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )


      CALL VLCOM ( 1.D0, BOXORI, 0.5D0, EXTENT, CENTER )
      CALL CHCKXC( .FALSE., ' ', OK )


      DO I = 1, 3

         DO J = 1, 2
C
C           S is -1 for J = 1; 1 for J = 2.
C
            S = 2*J - 3

C
C           Select a vertex outside the bounding sphere.
C
            CALL VEQU ( CENTER, VERTEX )
            VERTEX(I) = CENTER(I) +  S * EXTENT(I) * ( 0.50001D0 )

            CALL CLEARD ( 3, RAYDIR )
            RAYDIR(I) = -S


            CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
            CALL CHCKXC   ( .FALSE., ' ', OK )
            
            LABEL = 'FOUND # #'
            CALL REPMI ( LABEL, '#', I, LABEL )
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

            IF ( FOUND ) THEN
            
               CALL VEQU ( CENTER, XXPT )
               XXPT(I) = CENTER(I) + S*EXTENT(I)/2

               LABEL = 'XPT # #'
               CALL REPMI ( LABEL, '#', I, LABEL )
               CALL REPMI ( LABEL, '#', J, LABEL )

               TOL = VTIGHT

               CALL CHCKAD ( LABEL, XPT, '~~/', XXPT, 3, TOL, OK )

            END IF

         END DO

      END DO

C
C --- Case: ------------------------------------------------------
C 
C
C     A series of detailed intersection tests follows. Vertices
C     will be generated for a set of directions spanning the
C     unit sphere centered at the box center. All rays are 
C     pointed at specific points on the box's surface. The
C     vertices are outside the bounding sphere but not at
C     a very great distance from it.
C
C     MEDRAD is the vertex distance from the center.
C
      MEDRAD = 1.D2
      
      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )

      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )

      CALL VLCOM ( 1.D0, BOXORI, 0.5D0, EXTENT, CENTER )
      CALL CHCKXC( .FALSE., ' ', OK )


C
C     Compute steps separating target points.
C
      NSTEP = 20

      DO I = 1, 3

         DELTA(I) = EXTENT(I) / NSTEP

      END DO

C
C     Loop over all coordinates and both faces for each
C     coordinate.
C
      DO I = 1, 3
C
C        Set the indices of the "horizontal" coordinates;
C        these are the coordinate on the axes orthogonal
C        to the Ith axis.
C
         HCIDX1 = NEXT(I)
         HCIDX2 = NEXT(HCIDX1)

         DO J = 1, 2
C
C           S is -1 for J = 1; 1 for J = 2.
C
            S = 2*J - 3
C
C           Loop over the horizontal coordinates; generate
C           target points.
C
            XXPT(I) = CENTER(I)  +  S * EXTENT(I) / 2

            DO H1 = 0, NSTEP

               XXPT( HCIDX1 ) =    CENTER( HCIDX1 ) 
     .                           - EXTENT( HCIDX1 ) / 2
     .                           + DELTA ( HCIDX1 ) * H1

               DO H2 = 0, NSTEP

                  XXPT( HCIDX2 ) =    CENTER( HCIDX2 ) 
     .                              - EXTENT( HCIDX2 ) / 2
     .                              + DELTA ( HCIDX2 ) * H2

C
C --- Case: ------------------------------------------------------
C 

                  TITLE = 'Medium range intercept: I =  #; '
     .            //      'J = #; H1 =  #; H2 = #'
                  CALL REPMI ( TITLE, '#', I,  TITLE )
                  CALL REPMI ( TITLE, '#', J,  TITLE )
                  CALL REPMI ( TITLE, '#', H1, TITLE )
                  CALL REPMI ( TITLE, '#', H2, TITLE )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  CALL TCASE ( TITLE )
C
C                 Generate the ray's vertex for this case.
C                 The vertex lies along a ray emanating from
C                 the box's center. The distance of the vertex
C                 from the center is MEDRAD.
C
                  CALL VSUB  ( CENTER, XXPT,   RAYDIR )
                  CALL VHAT  ( RAYDIR, UDIR   )
                  CALL VLCOM ( 1.D0,   CENTER, -MEDRAD, UDIR, VERTEX )

                  CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, 
     .                            EXTENT, XPT,    FOUND )
                  CALL CHCKXC   ( .FALSE., ' ', OK )
            
                  LABEL = 'FOUND # # # #'
                  CALL REPMI ( LABEL, '#', I,  LABEL )
                  CALL REPMI ( LABEL, '#', J,  LABEL )
                  CALL REPMI ( LABEL, '#', H1, LABEL )
                  CALL REPMI ( LABEL, '#', H2, LABEL )
                  CALL CHCKXC   ( .FALSE., ' ', OK )
                  CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

                  IF ( FOUND ) THEN
            
                     LABEL = 'XPT # #'
                     CALL REPMI ( LABEL, '#', I, LABEL )
                     CALL REPMI ( LABEL, '#', J, LABEL )
                     CALL REPMI ( LABEL, '#', H1, LABEL )
                     CALL REPMI ( LABEL, '#', H2, LABEL )
                     CALL CHCKXC   ( .FALSE., ' ', OK )

                     TOL = VTIGHT

                     CALL CHCKAD ( LABEL, XPT, '~~/', 
     .                                    XXPT, 3, TOL, OK )

                  END IF

               END DO

            END DO
            
         END DO

      END DO




C
C --- Case: ------------------------------------------------------
C 
C     Repeat the previous tests but use a very large vertex
C     distance. We must reduce the relative error tolerance
C     to a value suitable for the large expected errors in the
C     ray-bounding sphere intercept.
C     
      BIGRAD = 1.D14

      TOL    = VTIGHT * BIGRAD / 10
      
C
C     Compute steps separating target points.
C
      NSTEP = 20

      DO I = 1, 3

         DELTA(I) = EXTENT(I) / NSTEP

      END DO

C
C     Loop over all coordinates and both faces for each
C     coordinate.
C
      DO I = 1, 3
C
C        Set the indices of the "horizontal" coordinates;
C        these are the coordinate on the axes orthogonal
C        to the Ith axis.
C
         HCIDX1 = NEXT(I)
         HCIDX2 = NEXT(HCIDX1)


         DO J = 1, 2
C
C           S is -1 for J = 1; 1 for J = 2.
C
            S = 2*J - 3
C
C           Loop over the horizontal coordinates; generate
C           target points.
C
            XXPT(I) = CENTER(I)  +  S * EXTENT(I) / 2

            DO H1 = 0, NSTEP

               XXPT( HCIDX1 ) =    CENTER( HCIDX1 ) 
     .                           - EXTENT( HCIDX1 ) / 2
     .                           + DELTA ( HCIDX1 ) * H1

               DO H2 = 0, NSTEP

                  XXPT( HCIDX2 ) =    CENTER( HCIDX2 ) 
     .                              - EXTENT( HCIDX2 ) / 2
     .                              + DELTA ( HCIDX2 ) * H2

C
C --- Case: ------------------------------------------------------
C 

                  TITLE = 'Long range intercept: I =  #; '
     .            //      'J = #; H1 =  #; H2 = #'
                  CALL REPMI ( TITLE, '#', I,  TITLE )
                  CALL REPMI ( TITLE, '#', J,  TITLE )
                  CALL REPMI ( TITLE, '#', H1, TITLE )
                  CALL REPMI ( TITLE, '#', H2, TITLE )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  CALL TCASE ( TITLE )
C
C                 Generate the ray's vertex for this case.
C                 The vertex lies along a ray emanating from
C                 the box's center. The distance of the vertex
C                 from the center is BIGRAD.
C
                  CALL VSUB  ( CENTER, XXPT,   RAYDIR )
                  CALL VHAT  ( RAYDIR, UDIR   )
                  CALL VLCOM ( 1.D0,   CENTER, -BIGRAD, UDIR, VERTEX )

                  CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, 
     .                            EXTENT, XPT,    FOUND )
                  CALL CHCKXC   ( .FALSE., ' ', OK )
            
                  LABEL = 'FOUND # # # #'
                  CALL REPMI ( LABEL, '#', I,  LABEL )
                  CALL REPMI ( LABEL, '#', J,  LABEL )
                  CALL REPMI ( LABEL, '#', H1, LABEL )
                  CALL REPMI ( LABEL, '#', H2, LABEL )
                  CALL CHCKXC   ( .FALSE., ' ', OK )
                  CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

                  IF ( FOUND ) THEN
            
                     LABEL = 'XPT # #'
                     CALL REPMI ( LABEL, '#', I, LABEL )
                     CALL REPMI ( LABEL, '#', J, LABEL )
                     CALL REPMI ( LABEL, '#', H1, LABEL )
                     CALL REPMI ( LABEL, '#', H2, LABEL )
                     CALL CHCKXC   ( .FALSE., ' ', OK )

                     CALL CHCKAD ( LABEL, XPT, '~~/', 
     .                                    XXPT, 3, TOL, OK )

                  END IF

               END DO

            END DO
            
         END DO

      END DO


C
C --- Case: ------------------------------------------------------
C 
C     Repeat the previous tests but use vertices slightly outside
C     the box.
C     
      SMLRAD = 1.D-3

      TOL    = VTIGHT 
      
C
C     Compute steps separating target points.
C
      NSTEP = 20

      DO I = 1, 3

         DELTA(I) = EXTENT(I) / NSTEP

      END DO

C
C     Loop over all coordinates and both faces for each
C     coordinate.
C
      DO I = 1, 3
C
C        Set the indices of the "horizontal" coordinates;
C        these are the coordinate on the axes orthogonal
C        to the Ith axis.
C
         HCIDX1 = NEXT(I)
         HCIDX2 = NEXT(HCIDX1)


         DO J = 1, 2
C
C           S is -1 for J = 1; 1 for J = 2.
C
            S = 2*J - 3
C
C           Loop over the horizontal coordinates; generate
C           target points.
C
            XXPT(I) = CENTER(I)  +  S * EXTENT(I) / 2

            DO H1 = 0, NSTEP

               XXPT( HCIDX1 ) =    CENTER( HCIDX1 ) 
     .                           - EXTENT( HCIDX1 ) / 2
     .                           + DELTA ( HCIDX1 ) * H1

               DO H2 = 0, NSTEP

                  XXPT( HCIDX2 ) =    CENTER( HCIDX2 ) 
     .                              - EXTENT( HCIDX2 ) / 2
     .                              + DELTA ( HCIDX2 ) * H2

C
C --- Case: ------------------------------------------------------
C 

                  TITLE = 'short range intercept: I =  #; '
     .            //      'J = #; H1 =  #; H2 = #'
                  CALL REPMI ( TITLE, '#', I,  TITLE )
                  CALL REPMI ( TITLE, '#', J,  TITLE )
                  CALL REPMI ( TITLE, '#', H1, TITLE )
                  CALL REPMI ( TITLE, '#', H2, TITLE )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  CALL TCASE ( TITLE )
C
C                 Generate the ray's vertex for this case.
C                 The vertex lies along a ray emanating from
C                 the box's center. The distance of the vertex
C                 from the expected intercept (NOT the center)
C                 is SMLRAD.
C
                  CALL VSUB  ( CENTER, XXPT,   RAYDIR )
                  CALL VHAT  ( RAYDIR, UDIR   )
                  CALL VLCOM ( 1.D0,   XXPT, -SMLRAD, UDIR, VERTEX )

                  CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, 
     .                            EXTENT, XPT,    FOUND )
                  CALL CHCKXC   ( .FALSE., ' ', OK )
            
                  LABEL = 'FOUND # # # #'
                  CALL REPMI ( LABEL, '#', I,  LABEL )
                  CALL REPMI ( LABEL, '#', J,  LABEL )
                  CALL REPMI ( LABEL, '#', H1, LABEL )
                  CALL REPMI ( LABEL, '#', H2, LABEL )
                  CALL CHCKXC   ( .FALSE., ' ', OK )
                  CALL CHCKSL ( LABEL, FOUND, .TRUE., OK )

                  IF ( FOUND ) THEN
            
                     LABEL = 'XPT # #'
                     CALL REPMI ( LABEL, '#', I, LABEL )
                     CALL REPMI ( LABEL, '#', J, LABEL )
                     CALL REPMI ( LABEL, '#', H1, LABEL )
                     CALL REPMI ( LABEL, '#', H2, LABEL )
                     CALL CHCKXC   ( .FALSE., ' ', OK )

                     CALL CHCKAD ( LABEL, XPT, '~~/', 
     .                                    XXPT, 3, TOL, OK )

                  END IF

               END DO

            END DO            

         END DO

      END DO




C**********************************************************************
C
C     Normal non-intersection cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
C
C     A series of detailed non-intersection tests follows. 
C     

C
C     Compute steps separating target points.
C
      NSTEP = 8

      DO I = 1, 3
C
C        Note that the increments are based on twice the
C        box extents.
C
         DELTA(I) = 2 * EXTENT(I) / NSTEP

      END DO


C
C     Generate target points that the rays will pass through.
C
C     Loop over all coordinates and both faces for each
C     coordinate.
C
      DO I = 1, 3
C
C        Set the indices of the "horizontal" coordinates;
C        these are the coordinate on the axes orthogonal
C        to the Ith axis.
C
         HCIDX1 = NEXT(I)
         HCIDX2 = NEXT(HCIDX1)


         DO J = 1, 2
C
C           S is -1 for J = 1; 1 for J = 2.
C
            S = 2*J - 3
C
C           Loop over the horizontal coordinates; generate
C           target points. The target plane is on the far side
C           of the box, relative to the Ith coordinate of the
C           vertex.
C
            TARG(I) = CENTER(I)  -  S * EXTENT(I) / 2

            DO H1 = 0, NSTEP

               TARG( HCIDX1 ) =    CENTER( HCIDX1 ) 
     .                           - EXTENT( HCIDX1 ) 
     .                           + DELTA ( HCIDX1 ) * H1

               DO H2 = 0, NSTEP

                  TARG( HCIDX2 ) =    CENTER( HCIDX2 ) 
     .                              - EXTENT( HCIDX2 ) 
     .                              + DELTA ( HCIDX2 ) * H2

C
C                 Use only those target points that lie outside
C                 of the horizontal bounds of the box.
C
                  OUT1 =      ABS( TARG  (HCIDX1) - CENTER(HCIDX1) )
     .                   .GT.    ( EXTENT(HCIDX1) / 2              )  

                  OUT2 =      ABS( TARG  (HCIDX2) - CENTER(HCIDX2) )
     .                   .GT.    ( EXTENT(HCIDX2) / 2              )  


                  IF ( OUT1 .OR. OUT2 ) THEN
C
C                    Generate vertices. We'll place the vertices
C                    far enough from the box that no "outside"
C                    target point is obscured by the box.
C
                     CALL CLEARD ( 3, VERTEX )
                     
                     VERTEX(I) = 1000.D0 * S

                     DO V1 = 0, NSTEP

                        VERTEX(HCIDX1) =         CENTER(HCIDX1) 
     .                                    + V1 * DELTA (HCIDX1)

                        DO V2 = 0, NSTEP

                           VERTEX(HCIDX2) =        CENTER(HCIDX2) 
     .                                      + V2 * DELTA (HCIDX2)
 
C
C --- Case: ------------------------------------------------------
C 

                           TITLE = 'non-intercept: I =  #; '
     .                     //      'J = #; H1 =  #; H2 = #; '
     .                     //      'V1 = #; V2 = #'

                           CALL REPMI ( TITLE, '#', I,  TITLE )
                           CALL REPMI ( TITLE, '#', J,  TITLE )
                           CALL REPMI ( TITLE, '#', H1, TITLE )
                           CALL REPMI ( TITLE, '#', H2, TITLE )
                           CALL REPMI ( TITLE, '#', V1, TITLE )
                           CALL REPMI ( TITLE, '#', V2, TITLE )
                           CALL CHCKXC( .FALSE., ' ', OK )

                           CALL TCASE ( TITLE )

C
C                          The ray points from the vertex toward
C                          the target on the far boundary plane.
C
                           CALL VSUB  ( TARG, VERTEX, RAYDIR )

                           CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, 
     .                                     EXTENT, XPT,    FOUND )
                           CALL CHCKXC   ( .FALSE., ' ', OK )
            
                           LABEL = 'FOUND # # # # # #'
                           CALL REPMI ( LABEL, '#', I,  LABEL )
                           CALL REPMI ( LABEL, '#', J,  LABEL )
                           CALL REPMI ( LABEL, '#', H1, LABEL )
                           CALL REPMI ( LABEL, '#', H2, LABEL )
                           CALL REPMI ( LABEL, '#', V1, LABEL )
                           CALL REPMI ( LABEL, '#', V2, LABEL )
                           CALL CHCKXC( .FALSE., ' ', OK )
C
C                          No intercept should be found.
C
                           CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )

                        END DO

                     END DO

                  END IF
                  
               END DO

            END DO

         END DO

      END DO


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


