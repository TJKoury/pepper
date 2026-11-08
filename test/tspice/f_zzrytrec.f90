!$Procedure F_ZZRYTREC ( ZZRYTREC tests )

      SUBROUTINE F_ZZRYTREC ( OK )

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZRYTREC.
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

      INCLUDE 'dskdsc.inc'
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
!     This routine tests the private SPICELIB routine ZZRYTREC.
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
!-    TSPICE Version 1.0.0, 22-JUL-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

!
!     Local Variables
!
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      D      ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      H
      DOUBLE PRECISION      L
      DOUBLE PRECISION      MARGIN
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      W
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )

      INTEGER               C1
      INTEGER               C2
      INTEGER               F
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               M
      INTEGER               NEXT   ( 3 )
      INTEGER               NXPTS
      INTEGER               P
      INTEGER               PERPCO ( 2 )
      INTEGER               S
      INTEGER               XNXPTS


!
!     Saved values
!
      SAVE

!
!     Initial values
!
      DATA                  NEXT / 2, 3, 1 /


!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZRYTREC' )


!
!     We rely on ZZRAYBOX to work correctly; these tests simply
!     validate the use by ZZRYTREC of ZZRAYBOX.
!

!***********************************************************************
!
!     Normal cases
!
!***********************************************************************


!***********************************************************************
!
!
!     Simple hit/miss cases
!
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Hit case. Zero MARGIN.' )



      MARGIN = 0.D0
!
!
!     Set the coordinate system and bounds.

      L =  40.D0
      W =  20.D0
      H =  10.D0

      BOUNDS(1,1) = -L/2
      BOUNDS(2,1) =  L/2
      BOUNDS(1,2) = -W/2
      BOUNDS(2,2) =  W/2
      BOUNDS(1,3) = -H/2
      BOUNDS(2,3) =  H/2

      D(1) = L
      D(2) = W
      D(3) = H

!
!     Try an intercept using rays emanating from points on
!     each side of the volume element.
!

      DO I = 1, 3

         DO J = 1, 2

            M = 2*J - 3

            CALL CLEARD ( 3, VERTEX )

            VERTEX(I) = 2 * M * D(I)

            CALL VMINUS( VERTEX, RAYDIR )




            CALL ZZRYTREC ( VERTEX, RAYDIR, BOUNDS, &
                            MARGIN, NXPTS,  XPT    )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           We expect an intercept.
!
            XNXPTS = 1

            CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

!
!           Create the expected intercept point.
!
            CALL CLEARD ( 3, XXPT )
            XXPT(I) = M * D(I)/2


            TOL = VTIGHT

            CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

         END DO

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Miss case. Zero MARGIN.' )


      MARGIN = 0.D0
!
!     Try an intercept using rays emanating from points on
!     each side of the volume element. For each face, try
!     rays that miss on all sides.
!
      DELTA = 1.D-12

      DO I = 1, 3

         PERPCO(1) = NEXT(I)
         PERPCO(2) = NEXT(NEXT(I))

         DO J = 1, 2

            F = 2*J - 3

!
!           Loop over the coordinates orthogonal to coordinate I.
!
            DO K = 1, 2

               C1 = PERPCO(K  )
               C2 = PERPCO(3-K)

               DO P = 1, 2
!
!                 S is the sign of the displacement in the direction of
!                 C1.
!
                  S = 2*P - 3


                  CALL CLEARD ( 3, VERTEX )

                  VERTEX(I)  = 2 * F * D(I)
                  VERTEX(C1) = S * (  ( D(C1)/2 ) + DELTA  )
                  VERTEX(C2) = 0.D0

                  CALL CLEARD( 3, RAYDIR )

                  RAYDIR(I) = -F

                  MARGIN = 0.D0

                  CALL ZZRYTREC ( VERTEX, RAYDIR, BOUNDS, &
                                  MARGIN, NXPTS,  XPT    )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 We expect to find no intercept.
!
                  XNXPTS = 0

                  CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

               END DO
!
!              End of perpendicular coordinate face loop.
!
            END DO
!
!           End of perpendicular coordinate loop.
!
         END DO
!
!        End of primary coordinate face loop.
!
      END DO
!
!     End of primary coordinate loop.
!



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Hit case. Positive MARGIN.' )



      MARGIN = 1.D-11
!
!     Try an intercept using rays emanating from points on
!     each side of the volume element. For each face, try
!     rays that hits slightly beyond the edges of the
!     unexpanded face, but within the edges of the expanded
!     face, on all sides.
!

      DO I = 1, 3

         PERPCO(1) = NEXT(I)
         PERPCO(2) = NEXT(NEXT(I))


         DO J = 1, 2

            F = 2*J - 3

!
!           Loop over the coordinates orthogonal to coordinate I.
!
            DO K = 1, 2

               C1    = PERPCO(K  )
               C2    = PERPCO(3-K)

               DELTA = ( MARGIN / 2 ) * D(C1)

               DO P = 1, 2
!
!                 S is the sign of the displacement in the direction of
!                 C1.
!
                  S = 2*P - 3


                  CALL CLEARD ( 3, VERTEX )

                  VERTEX(I)  = 2 * F * D(I)
                  VERTEX(C1) = S * (  ( D(C1)/2 ) + DELTA  )
                  VERTEX(C2) = 0.D0

                  CALL CLEARD( 3, RAYDIR )

                  RAYDIR(I) = -F


                  CALL ZZRYTREC ( VERTEX, RAYDIR, BOUNDS, &
                                  MARGIN, NXPTS,  XPT    )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 We expect to find an intercept.
!
                  XNXPTS = 1

                  CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

!
!                 Create the expected intercept point.
!
                  CALL CLEARD ( 3, XXPT )

                  XXPT(I ) = F * ( (D(I)/2) + (MARGIN*D(I)) )
                  XXPT(C1) = VERTEX(C1)
                  XXPT(C2) = VERTEX(C2)

                  TOL = VTIGHT

                  CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )

               END DO
!
!              End of perpendicular coordinate face loop.
!
            END DO
!
!           End of perpendicular coordinate loop.
!
         END DO
!
!        End of primary coordinate face loop.
!
      END DO
!
!     End of primary coordinate loop.
!



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Miss case. Positive MARGIN.' )



      MARGIN = 1.D-11
!
!     Try an intercept using rays emanating from points on
!     each side of the volume element. For each face, try
!     rays that hits slightly beyond the edges of the
!     expanded face on all sides.
!

      DO I = 1, 3

         PERPCO(1) = NEXT(I)
         PERPCO(2) = NEXT(NEXT(I))


         DO J = 1, 2

            F = 2*J - 3

!
!           Loop over the coordinates orthogonal to coordinate I.
!
            DO K = 1, 2

               C1    = PERPCO(K  )
               C2    = PERPCO(3-K)

               DELTA = ( MARGIN * 2 ) * D(C1)

               DO P = 1, 2
!
!                 S is the sign of the displacement in the direction of
!                 C1.
!
                  S = 2*P - 3


                  CALL CLEARD ( 3, VERTEX )

                  VERTEX(I)  = 2 * F * D(I)
                  VERTEX(C1) = S * (  ( D(C1)/2 ) + DELTA  )
                  VERTEX(C2) = 0.D0

                  CALL CLEARD( 3, RAYDIR )

                  RAYDIR(I) = -F


                  CALL ZZRYTREC ( VERTEX, RAYDIR, BOUNDS, &
                                  MARGIN, NXPTS,  XPT    )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 We expect to find no intercept.
!
                  XNXPTS = 0

                  CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )


               END DO
!
!              End of perpendicular coordinate face loop.
!
            END DO
!
!           End of perpendicular coordinate loop.
!
         END DO
!
!        End of primary coordinate face loop.
!
      END DO
!
!     End of primary coordinate loop.
!


!***********************************************************************
!
!     Non-error exceptional cases
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Vertex inside element. Positive MARGIN.' )



      MARGIN = 1.D-11
!
!     Try an intercept using rays emanating from points on
!     each side of the volume element. For each face, try
!     rays having vertices slightly beyond unexpanded element
!     but within the expanded element.
!

      DO I = 1, 3

         PERPCO(1) = NEXT(I)
         PERPCO(2) = NEXT(NEXT(I))


         DO J = 1, 2

            F = 2*J - 3

!
!           Loop over the coordinates orthogonal to coordinate I.
!
            DO K = 1, 2

               C1    = PERPCO(K  )
               C2    = PERPCO(3-K)

!
!              The margin used by ZZRYTREC for this test is twice the
!              input margin. Use a large delta to verify this.
!
               DELTA = ( MARGIN * 1.999D0 ) * D(C1)

               DO P = 1, 2
!
!                 S is the sign of the displacement in the direction of
!                 C1.
!
                  S = 2*P - 3


                  CALL CLEARD ( 3, VERTEX )

                  VERTEX(I)  = ( D(I)/2 ) * DELTA
                  VERTEX(C1) = S * (  ( D(C1)/2 ) + DELTA  )
                  VERTEX(C2) = 0.D0

                  CALL CLEARD( 3, RAYDIR )

                  RAYDIR(I) = -F


                  CALL ZZRYTREC ( VERTEX, RAYDIR, BOUNDS, &
                                  MARGIN, NXPTS,  XPT    )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 We expect to find an intercept.
!
                  XNXPTS = 1

                  CALL CHCKSI ( 'NXPTS', NXPTS, '=', XNXPTS, 0, OK )

!
!                 The expected intercept point is the vertex itself.
!
                  CALL VEQU ( VERTEX, XXPT )

                  CALL CHCKAD ( 'XPT', XPT, '=', XXPT, 3, 0.D0, OK )

               END DO
!
!              End of perpendicular coordinate face loop.
!
            END DO
!
!           End of perpendicular coordinate loop.
!
         END DO
!
!        End of primary coordinate face loop.
!
      END DO
!
!     End of primary coordinate loop.
!


!***********************************************************************
!
!     Error cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bounds out of order.' )

      MARGIN = 0.D0


      BOUNDS(1,1) = -L/2
      BOUNDS(2,1) =  L/2
      BOUNDS(1,2) = -W/2
      BOUNDS(2,2) =  W/2
      BOUNDS(1,3) = -H/2
      BOUNDS(2,3) =  H/2

      DO I = 1, 3

         CALL SWAPD ( BOUNDS(1,I), BOUNDS(2,I) )

         CALL CLEARD( 3, VERTEX )
         VERTEX(I) = 2 * D(I)

         CALL VMINUS( VERTEX, RAYDIR )


         CALL ZZRYTREC ( VERTEX, RAYDIR, BOUNDS, &
                         MARGIN, NXPTS,  XPT    )
         CALL CHCKXC ( .TRUE., 'SPICE(BADCOORDBOUNDS)', OK )
!
!        Restore bounds.
!
         CALL SWAPD ( BOUNDS(1,I), BOUNDS(2,I) )

      END DO




!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


