!$Procedure F_INSANG ( INSANG tests )

      SUBROUTINE F_INSANG ( OK )

! $ Abstract
!
!     Exercise the ray-voxel grid intersection routine INSANG.
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
!     This routine tests the DSKLIB type 2 ray-voxel grid
!     intersection routine INSANG.
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
!-    TSPICE Version 1.0.0, 20-JUN-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI


!
!     Local Parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-9 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 320 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      INTEGER               NSHAPE
      PARAMETER           ( NSHAPE = 3 )

!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      BETA
      DOUBLE PRECISION      GAMMA
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      E1     ( 3 )
      DOUBLE PRECISION      E2     ( 3 )
      DOUBLE PRECISION      E3     ( 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      PLATES ( 3, 3, NSHAPE )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SCLPLT ( 3, 3, NSHAPE )
      DOUBLE PRECISION      SCLVTX ( 3 )
      DOUBLE PRECISION      SIDES  ( 3, 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )
      DOUBLE PRECISION      ZVEC   ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               M
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NRAD
      INTEGER               NSCALE
      INTEGER               NXPTS

      LOGICAL               FOUND

!
!     Saved values
!
      SAVE


!
!     Initial values
!
      DATA                  ORIGIN / 3 * 0.D0 /
      DATA                  ZVEC   / 0.D0, 0.D0, 1.D0 /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_INSANG' )


!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple intersection case.' )


      CALL VPACK ( 0.D0, 0.D0, 10.D0, E1 )
      CALL VPACK ( 4.D0, 0.D0, 10.D0, E2 )
      CALL VPACK ( 0.D0, 4.D0, 10.D0, E3 )
      CALL VPACK ( 1.D0, 1.D0, 10.D0, V  )


      CALL INSANG ( V, E1, E2, E3, FOUND, SCALE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSCL ( SCALE, V, XPT )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Create a SPICELIB plane containing the plate defined
!     by E1, E2, E3.
!
      CALL NVC2PL ( ZVEC, 10.D0, PLANE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Find the expected intercept.
!
      CALL INRYPL ( ORIGIN, V, PLANE, NXPTS, XXPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple non-intersection case.' )

!
!     Use the edge vectors from the previous case. Update V.
!
      CALL VPACK ( 0.D0, 5.D0, 10.D0, V  )


      CALL INSANG ( V, E1, E2, E3, FOUND, SCALE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )



!
! --- Case: ------------------------------------------------------
!


!
!
!     The following set of cases covers a variety of plate shapes,
!     vertex positions, and scales.
!
!

!
!     Create a rotation to be applied to all inputs to INSANG.
!
      ALPHA =  30.D0 * RPD()
      BETA  = -10.D0 * RPD()
      GAMMA =  70.D0 * RPD()

      CALL EUL2M ( GAMMA, BETA, ALPHA, 3, 2, 3, XFORM )

      CALL IDENT ( XFORM )

!
!     Create the plate shapes to be used in the following tests.
!
!
!        Plate 1 is an acute triangle.
!
      CALL VPACK ( 0.D0, 0.D0, 0.D0, PLATES(1,1,1) )
      CALL VPACK ( 2.D0, 0.D0, 0.D0, PLATES(1,2,1) )
      CALL VPACK ( 1.D0, 4.D0, 0.D0, PLATES(1,3,1) )

!
!        Plate 2 is a right triangle.
!
      CALL VPACK ( 0.D0, 0.D0, 0.D0, PLATES(1,1,2) )
      CALL VPACK ( 4.D0, 0.D0, 0.D0, PLATES(1,2,2) )
      CALL VPACK ( 4.D0, 3.D0, 0.D0, PLATES(1,3,2) )

!
!        Plate 3 is an obtuse triangle.
!
      CALL VPACK ( 0.D0, 0.D0, 0.D0, PLATES(1,1,3) )
      CALL VPACK ( 4.D0, 0.D0, 0.D0, PLATES(1,2,3) )
      CALL VPACK ( 2.D0, 1.D0, 0.D0, PLATES(1,3,3) )

!
!     Set parameters used for vertex placement.
!
      NLON   = 5
      DLON   = TWOPI() / NLON

      NLAT   = 10
      DLAT   = PI() / ( NLAT - 1 )

      NRAD   = 5

!
!     Set parameters used to determine the scale of the inputs.
!
      NSCALE = 7

       DO I = 1, NLON

         LON = (I-1)*DLON

         DO J = 0, NLAT

            LAT = HALFPI() - (J*DLAT)

            DO K = 1, NRAD

               R = 10.D0 ** ( K - 2 )

               DO L = 1, NSCALE

                  SCALE = 10.D0 ** ( 20.D0 * DBLE( L - (NSCALE/2) ) )

!                  SCALE = 1.D0

!
!                 Create the vertex.
!
                  CALL LATREC ( R, LON, LAT, VERTEX )

!
!                 Scale the vertex.
!
                  CALL VSCL ( SCALE, VERTEX, SCLVTX )


!
!                 Loop over the shapes.
!
                  DO M = 1, 3

                     DO N = 1, 3

                        CALL VSCL ( SCALE, PLATES(1,N,M), &
                                           SCLPLT(1,N,M) )
                     END DO

!
!                    Transform vertex and plate using XFORM.
!
                     CALL MXV  ( XFORM, VERTEX, VTEMP  )
                     CALL VEQU ( VTEMP,         VERTEX )


                     DO N = 1, 3
                        CALL MXV ( XFORM, PLATES(1,N,M), VTEMP         )
                        CALL VEQU( VTEMP,                PLATES(1,N,M) )
                     END DO

!
!                    Create the edges of the pyramid for the current
!                    plate.
!
                     CALL VSUB ( SCLPLT(1,1,M), SCLVTX, E1 )
                     CALL VSUB ( SCLPLT(1,2,M), SCLVTX, E2 )
                     CALL VSUB ( SCLPLT(1,3,M), SCLVTX, E3 )

!
! --- Case: ------------------------------------------------------
!
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. ' &
                     //      'Aim at plate''s centroid.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )

                     CALL VLCOM3 ( 1.D0, E1, 1.D0, E2, 1.D0, E3, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     CALL VSCL ( S, V, XPT )

!
!                    Create a SPICELIB plane containing the plate
!                    defined by E1, E2, E3.
!
                     CALL VSUB ( E2, E1, SIDES(1,1) )
                     CALL VSUB ( E3, E1, SIDES(1,3) )

                     CALL PSV2PL ( E1,         SIDES(1,1), &
                                   SIDES(1,3), PLANE      )
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Find the expected intercept.
!
                     CALL INRYPL ( ORIGIN, V, PLANE, NXPTS, XXPT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

                     TOL = TIGHT

                     CALL CHCKAD ( 'XPT', XPT, '~~/', &
                                          XXPT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. ' &
                     //      'Aim at interior point close to ' &
                     //      'the E1-E2 side.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )


                     CALL VLCOM3 ( 0.5D0, E1, 0.5D0, E2, 1.D-3, E3, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     CALL VSCL ( S, V, XPT )


!
!                    Create a SPICELIB plane containing the plate
!                    defined by E1, E2, E3.
!
                     CALL VSUB ( E2, E1, SIDES(1,1) )
                     CALL VSUB ( E3, E1, SIDES(1,3) )

                     CALL PSV2PL ( E1,         SIDES(1,1), &
                                   SIDES(1,3), PLANE      )
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Find the expected intercept.
!
                     CALL INRYPL ( ORIGIN, V, PLANE, NXPTS, XXPT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

                                        IF ( .NOT. OK ) STOP

                     TOL = TIGHT

                     CALL CHCKAD ( 'XPT', XPT, '~~/', &
                                          XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. ' &
                     //      'Aim at exterior point close to ' &
                     //      'the E1-E2 side.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )


                     CALL VLCOM3 ( 0.5D0, E1, 0.5D0, E2, -1.D-3, E3, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )



!
! --- Case: ------------------------------------------------------
!
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. ' &
                     //      'Aim at interior point close to ' &
                     //      'the E1-E3 side.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )


                     CALL VLCOM3 ( 0.5D0, E1, 0.5D0, E3, 1.D-3, E2, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     CALL VSCL ( S, V, XPT )


!
!                    Create a SPICELIB plane containing the plate
!                    defined by E1, E2, E3.
!
                     CALL VSUB ( E2, E1, SIDES(1,1) )
                     CALL VSUB ( E3, E1, SIDES(1,3) )

                     CALL PSV2PL ( E1,         SIDES(1,1), &
                                   SIDES(1,3), PLANE      )
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Find the expected intercept.
!
                     CALL INRYPL ( ORIGIN, V, PLANE, NXPTS, XXPT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

                                        IF ( .NOT. OK ) STOP

                     TOL = TIGHT

                     CALL CHCKAD ( 'XPT', XPT, '~~/', &
                                          XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. ' &
                     //      'Aim at exterior point close to ' &
                     //      'the E1-E3 side.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )


                     CALL VLCOM3 ( 0.5D0, E1, 0.5D0, E3, -1.D-3, E2, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )



!
! --- Case: ------------------------------------------------------
!
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. ' &
                     //      'Aim at interior point close to ' &
                     //      'the E2-E3 side.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )


                     CALL VLCOM3 ( 0.5D0, E2, 0.5D0, E3, 1.D-3, E1, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     CALL VSCL ( S, V, XPT )


!
!                    Create a SPICELIB plane containing the plate
!                    defined by E1, E2, E3.
!
                     CALL VSUB ( E2, E1, SIDES(1,1) )
                     CALL VSUB ( E3, E1, SIDES(1,3) )

                     CALL PSV2PL ( E1,         SIDES(1,1), &
                                   SIDES(1,3), PLANE      )
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Find the expected intercept.
!
                     CALL INRYPL ( ORIGIN, V, PLANE, NXPTS, XXPT )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL CHCKSI ( 'NXPTS', NXPTS, '=', 1, 0, OK )

                                        IF ( .NOT. OK ) STOP


                     TOL = TIGHT

                     CALL CHCKAD ( 'XPT', XPT, '~~/', &
                                          XXPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
                     TITLE = 'I = #; J = #, K = #, L = #, M = #. ' &
                     //      'Aim at exterior point close to ' &
                     //      'the E2-E3 side.'
                     CALL REPMI ( TITLE, '#', I, TITLE )
                     CALL REPMI ( TITLE, '#', J, TITLE )
                     CALL REPMI ( TITLE, '#', K, TITLE )
                     CALL REPMI ( TITLE, '#', L, TITLE )
                     CALL REPMI ( TITLE, '#', M, TITLE )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL TCASE ( TITLE )


                     CALL VLCOM3 ( 0.5D0, E2, 0.5D0, E3, -1.D-3, E1, V )

                     CALL INSANG ( V, E1, E2, E3, FOUND, S )

                     CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

                  END DO

               END DO

            END DO

         END DO

      END DO



!**********************************************************************
!
!     Exceptional cases
!
!**********************************************************************

!
!     INSANG is error-free.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Ray direction is zero vector.' )

      CALL VPACK ( 0.D0, 0.D0, 10.D0, E1 )
      CALL VPACK ( 4.D0, 0.D0, 10.D0, E2 )
      CALL VPACK ( 0.D0, 4.D0, 10.D0, E3 )
      CALL CLEARD ( 3, V )


      CALL INSANG ( V, E1, E2, E3, FOUND, SCALE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Edge vectors are linearly dependent.' )

!
!     Create independent edge vectors, but pass dependent
!     selections of the them to INSANG.
!
      CALL VPACK ( 0.D0, 0.D0, 10.D0, E1 )
      CALL VPACK ( 4.D0, 0.D0, 10.D0, E2 )
      CALL VPACK ( 0.D0, 4.D0, 10.D0, E3 )
      CALL VPACK ( 1.D0, 1.D0, 10.D0, V  )

!
!     E2 = E1:
!
      CALL INSANG ( V, E1, E1, E3, FOUND, SCALE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSD ( 'SCALE', SCALE, '=', 0.D0, 0.D0, OK )

!
!     E3 = E1:
!
      CALL INSANG ( V, E1, E2, E1, FOUND, SCALE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSD ( 'SCALE', SCALE, '=', 0.D0, 0.D0, OK )

!
!     E3 = E2:
!
      CALL INSANG ( V, E1, E2, E2, FOUND, SCALE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSD ( 'SCALE', SCALE, '=', 0.D0, 0.D0, OK )


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END







