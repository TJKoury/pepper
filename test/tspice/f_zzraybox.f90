!$Procedure F_ZZRAYBOX ( ray-box intercept tests )

      SUBROUTINE F_ZZRAYBOX ( OK )

! $ Abstract
!
!     Exercise the ray-box intercept routine ZZRAYBOX.
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
!     This routine test the DSKLIB ray-box intercept routine
!     ZZRAYBOX.
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
!-    TSPICE Version 1.0.0, 05-JUN-2014 (NJB)
!
!-&

!
!     SPICELIB functions
!
!      DOUBLE PRECISION      VDIST

!
!     Local Parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

!      DOUBLE PRECISION      TIGHT
!      PARAMETER           ( TIGHT  = 1.D-12 )

      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 50 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )


!
!     Local Variables
!
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

!
!     Saved values
!
      SAVE
!
!     Initial values
!
      DATA                 NEXT / 2, 3, 1 /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZRAYBOX' )

!**********************************************************************
!
!     Error cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Ray direction is zero vector.' )

      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )
      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )

      CALL VPACK (  1.D3,  1.D3,  1.D3, VERTEX )

      CALL CLEARD ( 3, RAYDIR )

      CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, EXTENT, XPT, FOUND )
      CALL CHCKXC   ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
! --- Case: ------------------------------------------------------
!
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




!**********************************************************************
!
!     Normal cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple non-intersection case. Vertices ' &
      //           'are outside the bounding sphere.'       )

!
!     The ray points directly away from the box in each case.
!
      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )
      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )


      CALL VLCOM ( 1.D0, BOXORI, 0.5D0, EXTENT, CENTER )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL VPACK ( 15.D0, 10.D0,  5.D0, VERTEX )

      DO I = 1, 3

         DO J = 1, 2
!
!           S is 1 for J = 1; -1 for J = 2.
!
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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple non-intersection case. Vertices ' &
      //           'are inside the bounding sphere.'       )

!
!     The ray points directly away from the box in each case.
!
      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )
      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )

      CALL VLCOM ( 1.D0, BOXORI, 0.5D0, EXTENT, CENTER )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL VPACK ( 15.D0, 10.D0,  5.D0, VERTEX )
      CALL VEQU  ( VERTEX, RAYDIR )

      DO I = 1, 3

         DO J = 1, 2
!
!           S is 1 for J = 1; -1 for J = 2.
!
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


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple intersection case' )

!
!     The ray points directly toward from the box in each case.
!
      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )
      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )


      CALL VLCOM ( 1.D0, BOXORI, 0.5D0, EXTENT, CENTER )
      CALL CHCKXC( .FALSE., ' ', OK )


      DO I = 1, 3

         DO J = 1, 2
!
!           S is -1 for J = 1; 1 for J = 2.
!
            S = 2*J - 3

!
!           Select a vertex outside the bounding sphere.
!
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


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple intersection case; vertex is inside ' &
      //           'the bounding sphere.'                        )

!
!     The ray points directly toward from the box in each case.
!
      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )
      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )


      CALL VLCOM ( 1.D0, BOXORI, 0.5D0, EXTENT, CENTER )
      CALL CHCKXC( .FALSE., ' ', OK )


      DO I = 1, 3

         DO J = 1, 2
!
!           S is -1 for J = 1; 1 for J = 2.
!
            S = 2*J - 3

!
!           Select a vertex outside the bounding sphere.
!
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

!
! --- Case: ------------------------------------------------------
!
!
!     A series of detailed intersection tests follows. Vertices
!     will be generated for a set of directions spanning the
!     unit sphere centered at the box center. All rays are
!     pointed at specific points on the box's surface. The
!     vertices are outside the bounding sphere but not at
!     a very great distance from it.
!
!     MEDRAD is the vertex distance from the center.
!
      MEDRAD = 1.D2

      CALL VPACK (  1.D0,  2.D0,  3.D0, BOXORI )

      CALL VPACK ( 30.D0, 20.D0, 10.D0, EXTENT )

      CALL VLCOM ( 1.D0, BOXORI, 0.5D0, EXTENT, CENTER )
      CALL CHCKXC( .FALSE., ' ', OK )


!
!     Compute steps separating target points.
!
      NSTEP = 20

      DO I = 1, 3

         DELTA(I) = EXTENT(I) / NSTEP

      END DO

!
!     Loop over all coordinates and both faces for each
!     coordinate.
!
      DO I = 1, 3
!
!        Set the indices of the "horizontal" coordinates;
!        these are the coordinate on the axes orthogonal
!        to the Ith axis.
!
         HCIDX1 = NEXT(I)
         HCIDX2 = NEXT(HCIDX1)

         DO J = 1, 2
!
!           S is -1 for J = 1; 1 for J = 2.
!
            S = 2*J - 3
!
!           Loop over the horizontal coordinates; generate
!           target points.
!
            XXPT(I) = CENTER(I)  +  S * EXTENT(I) / 2

            DO H1 = 0, NSTEP

               XXPT( HCIDX1 ) =    CENTER( HCIDX1 ) &
                                 - EXTENT( HCIDX1 ) / 2 &
                                 + DELTA ( HCIDX1 ) * H1

               DO H2 = 0, NSTEP

                  XXPT( HCIDX2 ) =    CENTER( HCIDX2 ) &
                                    - EXTENT( HCIDX2 ) / 2 &
                                    + DELTA ( HCIDX2 ) * H2

!
! --- Case: ------------------------------------------------------
!

                  TITLE = 'Medium range intercept: I =  #; ' &
                  //      'J = #; H1 =  #; H2 = #'
                  CALL REPMI ( TITLE, '#', I,  TITLE )
                  CALL REPMI ( TITLE, '#', J,  TITLE )
                  CALL REPMI ( TITLE, '#', H1, TITLE )
                  CALL REPMI ( TITLE, '#', H2, TITLE )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  CALL TCASE ( TITLE )
!
!                 Generate the ray's vertex for this case.
!                 The vertex lies along a ray emanating from
!                 the box's center. The distance of the vertex
!                 from the center is MEDRAD.
!
                  CALL VSUB  ( CENTER, XXPT,   RAYDIR )
                  CALL VHAT  ( RAYDIR, UDIR   )
                  CALL VLCOM ( 1.D0,   CENTER, -MEDRAD, UDIR, VERTEX )

                  CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, &
                                  EXTENT, XPT,    FOUND )
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

                     CALL CHCKAD ( LABEL, XPT, '~~/', &
                                          XXPT, 3, TOL, OK )

                  END IF

               END DO

            END DO

         END DO

      END DO




!
! --- Case: ------------------------------------------------------
!
!     Repeat the previous tests but use a very large vertex
!     distance. We must reduce the relative error tolerance
!     to a value suitable for the large expected errors in the
!     ray-bounding sphere intercept.
!
      BIGRAD = 1.D14

      TOL    = VTIGHT * BIGRAD / 10

!
!     Compute steps separating target points.
!
      NSTEP = 20

      DO I = 1, 3

         DELTA(I) = EXTENT(I) / NSTEP

      END DO

!
!     Loop over all coordinates and both faces for each
!     coordinate.
!
      DO I = 1, 3
!
!        Set the indices of the "horizontal" coordinates;
!        these are the coordinate on the axes orthogonal
!        to the Ith axis.
!
         HCIDX1 = NEXT(I)
         HCIDX2 = NEXT(HCIDX1)


         DO J = 1, 2
!
!           S is -1 for J = 1; 1 for J = 2.
!
            S = 2*J - 3
!
!           Loop over the horizontal coordinates; generate
!           target points.
!
            XXPT(I) = CENTER(I)  +  S * EXTENT(I) / 2

            DO H1 = 0, NSTEP

               XXPT( HCIDX1 ) =    CENTER( HCIDX1 ) &
                                 - EXTENT( HCIDX1 ) / 2 &
                                 + DELTA ( HCIDX1 ) * H1

               DO H2 = 0, NSTEP

                  XXPT( HCIDX2 ) =    CENTER( HCIDX2 ) &
                                    - EXTENT( HCIDX2 ) / 2 &
                                    + DELTA ( HCIDX2 ) * H2

!
! --- Case: ------------------------------------------------------
!

                  TITLE = 'Long range intercept: I =  #; ' &
                  //      'J = #; H1 =  #; H2 = #'
                  CALL REPMI ( TITLE, '#', I,  TITLE )
                  CALL REPMI ( TITLE, '#', J,  TITLE )
                  CALL REPMI ( TITLE, '#', H1, TITLE )
                  CALL REPMI ( TITLE, '#', H2, TITLE )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  CALL TCASE ( TITLE )
!
!                 Generate the ray's vertex for this case.
!                 The vertex lies along a ray emanating from
!                 the box's center. The distance of the vertex
!                 from the center is BIGRAD.
!
                  CALL VSUB  ( CENTER, XXPT,   RAYDIR )
                  CALL VHAT  ( RAYDIR, UDIR   )
                  CALL VLCOM ( 1.D0,   CENTER, -BIGRAD, UDIR, VERTEX )

                  CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, &
                                  EXTENT, XPT,    FOUND )
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

                     CALL CHCKAD ( LABEL, XPT, '~~/', &
                                          XXPT, 3, TOL, OK )

                  END IF

               END DO

            END DO

         END DO

      END DO


!
! --- Case: ------------------------------------------------------
!
!     Repeat the previous tests but use vertices slightly outside
!     the box.
!
      SMLRAD = 1.D-3

      TOL    = VTIGHT

!
!     Compute steps separating target points.
!
      NSTEP = 20

      DO I = 1, 3

         DELTA(I) = EXTENT(I) / NSTEP

      END DO

!
!     Loop over all coordinates and both faces for each
!     coordinate.
!
      DO I = 1, 3
!
!        Set the indices of the "horizontal" coordinates;
!        these are the coordinate on the axes orthogonal
!        to the Ith axis.
!
         HCIDX1 = NEXT(I)
         HCIDX2 = NEXT(HCIDX1)


         DO J = 1, 2
!
!           S is -1 for J = 1; 1 for J = 2.
!
            S = 2*J - 3
!
!           Loop over the horizontal coordinates; generate
!           target points.
!
            XXPT(I) = CENTER(I)  +  S * EXTENT(I) / 2

            DO H1 = 0, NSTEP

               XXPT( HCIDX1 ) =    CENTER( HCIDX1 ) &
                                 - EXTENT( HCIDX1 ) / 2 &
                                 + DELTA ( HCIDX1 ) * H1

               DO H2 = 0, NSTEP

                  XXPT( HCIDX2 ) =    CENTER( HCIDX2 ) &
                                    - EXTENT( HCIDX2 ) / 2 &
                                    + DELTA ( HCIDX2 ) * H2

!
! --- Case: ------------------------------------------------------
!

                  TITLE = 'short range intercept: I =  #; ' &
                  //      'J = #; H1 =  #; H2 = #'
                  CALL REPMI ( TITLE, '#', I,  TITLE )
                  CALL REPMI ( TITLE, '#', J,  TITLE )
                  CALL REPMI ( TITLE, '#', H1, TITLE )
                  CALL REPMI ( TITLE, '#', H2, TITLE )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  CALL TCASE ( TITLE )
!
!                 Generate the ray's vertex for this case.
!                 The vertex lies along a ray emanating from
!                 the box's center. The distance of the vertex
!                 from the expected intercept (NOT the center)
!                 is SMLRAD.
!
                  CALL VSUB  ( CENTER, XXPT,   RAYDIR )
                  CALL VHAT  ( RAYDIR, UDIR   )
                  CALL VLCOM ( 1.D0,   XXPT, -SMLRAD, UDIR, VERTEX )

                  CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, &
                                  EXTENT, XPT,    FOUND )
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

                     CALL CHCKAD ( LABEL, XPT, '~~/', &
                                          XXPT, 3, TOL, OK )

                  END IF

               END DO

            END DO

         END DO

      END DO




!**********************************************************************
!
!     Normal non-intersection cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
!
!     A series of detailed non-intersection tests follows.
!

!
!     Compute steps separating target points.
!
      NSTEP = 8

      DO I = 1, 3
!
!        Note that the increments are based on twice the
!        box extents.
!
         DELTA(I) = 2 * EXTENT(I) / NSTEP

      END DO


!
!     Generate target points that the rays will pass through.
!
!     Loop over all coordinates and both faces for each
!     coordinate.
!
      DO I = 1, 3
!
!        Set the indices of the "horizontal" coordinates;
!        these are the coordinate on the axes orthogonal
!        to the Ith axis.
!
         HCIDX1 = NEXT(I)
         HCIDX2 = NEXT(HCIDX1)


         DO J = 1, 2
!
!           S is -1 for J = 1; 1 for J = 2.
!
            S = 2*J - 3
!
!           Loop over the horizontal coordinates; generate
!           target points. The target plane is on the far side
!           of the box, relative to the Ith coordinate of the
!           vertex.
!
            TARG(I) = CENTER(I)  -  S * EXTENT(I) / 2

            DO H1 = 0, NSTEP

               TARG( HCIDX1 ) =    CENTER( HCIDX1 ) &
                                 - EXTENT( HCIDX1 ) &
                                 + DELTA ( HCIDX1 ) * H1

               DO H2 = 0, NSTEP

                  TARG( HCIDX2 ) =    CENTER( HCIDX2 ) &
                                    - EXTENT( HCIDX2 ) &
                                    + DELTA ( HCIDX2 ) * H2

!
!                 Use only those target points that lie outside
!                 of the horizontal bounds of the box.
!
                  OUT1 =      ABS( TARG  (HCIDX1) - CENTER(HCIDX1) ) &
                         .GT.    ( EXTENT(HCIDX1) / 2              )

                  OUT2 =      ABS( TARG  (HCIDX2) - CENTER(HCIDX2) ) &
                         .GT.    ( EXTENT(HCIDX2) / 2              )


                  IF ( OUT1 .OR. OUT2 ) THEN
!
!                    Generate vertices. We'll place the vertices
!                    far enough from the box that no "outside"
!                    target point is obscured by the box.
!
                     CALL CLEARD ( 3, VERTEX )

                     VERTEX(I) = 1000.D0 * S

                     DO V1 = 0, NSTEP

                        VERTEX(HCIDX1) =         CENTER(HCIDX1) &
                                          + V1 * DELTA (HCIDX1)

                        DO V2 = 0, NSTEP

                           VERTEX(HCIDX2) =        CENTER(HCIDX2) &
                                            + V2 * DELTA (HCIDX2)

!
! --- Case: ------------------------------------------------------
!

                           TITLE = 'non-intercept: I =  #; ' &
                           //      'J = #; H1 =  #; H2 = #; ' &
                           //      'V1 = #; V2 = #'

                           CALL REPMI ( TITLE, '#', I,  TITLE )
                           CALL REPMI ( TITLE, '#', J,  TITLE )
                           CALL REPMI ( TITLE, '#', H1, TITLE )
                           CALL REPMI ( TITLE, '#', H2, TITLE )
                           CALL REPMI ( TITLE, '#', V1, TITLE )
                           CALL REPMI ( TITLE, '#', V2, TITLE )
                           CALL CHCKXC( .FALSE., ' ', OK )

                           CALL TCASE ( TITLE )

!
!                          The ray points from the vertex toward
!                          the target on the far boundary plane.
!
                           CALL VSUB  ( TARG, VERTEX, RAYDIR )

                           CALL ZZRAYBOX ( VERTEX, RAYDIR, BOXORI, &
                                           EXTENT, XPT,    FOUND )
                           CALL CHCKXC   ( .FALSE., ' ', OK )

                           LABEL = 'FOUND # # # # # #'
                           CALL REPMI ( LABEL, '#', I,  LABEL )
                           CALL REPMI ( LABEL, '#', J,  LABEL )
                           CALL REPMI ( LABEL, '#', H1, LABEL )
                           CALL REPMI ( LABEL, '#', H2, LABEL )
                           CALL REPMI ( LABEL, '#', V1, LABEL )
                           CALL REPMI ( LABEL, '#', V2, LABEL )
                           CALL CHCKXC( .FALSE., ' ', OK )
!
!                          No intercept should be found.
!
                           CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )

                        END DO

                     END DO

                  END IF

               END DO

            END DO

         END DO

      END DO


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


