!$Procedure F_ZZTANSLV ( ZZTANSLV tests )

      SUBROUTINE F_ZZTANSLV ( OK )

! $ Abstract
!
!     Exercise the routine ZZTANSLV.
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
!     This routine tests the SPICELIB routine ZZTANSLV.
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
!-    TSPICE Version 1.0.0, 30-JUN-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      INTEGER               WNCARD

!
!     EXTERNAL routines
!
      EXTERNAL              GFSTEP
      EXTERNAL              GFREFN
      EXTERNAL              T_TANSTA

!
!     Local Parameters
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXIVL
      PARAMETER           ( MAXIVL = 1000 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 2 * MAXIVL )

!
!     Local Variables
!
      DOUBLE PRECISION      CNVTOL
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      POINTS ( 3, MAXWIN )
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      XRESLT ( MAXWIN )
      DOUBLE PRECISION      XPNTS  ( 3, MAXWIN )

      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               XN

      LOGICAL               CSTEP
      LOGICAL               ENDFLG ( 2 )


!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZTANSLV' )



!**********************************************************************
!
!     ZZTANSLV normal cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find all state transitions on interval [1,10]. ' &
      //           'Use constant step of 0.5.'                      )


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 1.D0
      FINISH = 10.D0
      CNVTOL = 1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intervals found.
!
      XN = 5
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

!
!     Check the interval endpoints.
!
      DO I = 1, 2*XN
         XRESLT(I) = DBLE(I)
      END DO

      TOL = 2 * CNVTOL

      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2*XN, TOL, OK )

!
!     Check the endpoint flags. The first and last endpoints are
!     not considered to be state transitions.
!
      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .FALSE., OK )
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .FALSE., OK )

!
!     Check the returned points. The values are defined only for
!     transitions, so the first and last points in the array are
!     not checked.
!
      DO I = 1, 2*XN, 2

         DO J = 1, 3

            XPNTS(J,I  ) = DBLE (I+J-1)
            XPNTS(J,I+1) = XPNTS(J,I)

         END DO

      END DO

      CALL CHCKAD ( 'POINTS', POINTS(1,2), '~', &
                              XPNTS (1,2), 3*(2*XN-2), TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find all state transitions on interval [1,10]. ' &
      //           'Use a step of 0.5, but use the GF step callback.' )


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .FALSE.

      CALL GFSSTP ( 0.5D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      STEP   = 0.5D0
      START  = 1.D0
      FINISH = 10.D0
      CNVTOL = 1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intervals found.
!
      XN = 5
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

!
!     Check the interval endpoints.
!
      DO I = 1, 2*XN
         XRESLT(I) = DBLE(I)
      END DO

      TOL = 2 * CNVTOL

      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2*XN, TOL, OK )

!
!     Check the endpoint flags. The first and last endpoints are
!     not considered to be state transitions.
!
      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .FALSE., OK )
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .FALSE., OK )

!
!     Check the returned points. The values are defined only for
!     transitions, so the first and last points in the array are
!     not checked.
!
      DO I = 1, 2*XN, 2

         DO J = 1, 3

            XPNTS(J,I  ) = DBLE (I+J-1)
            XPNTS(J,I+1) = XPNTS(J,I)

         END DO

      END DO

      CALL CHCKAD ( 'POINTS', POINTS(1,2), '~', &
                              XPNTS (1,2), 3*(2*XN-2), TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find all state transitions on interval [1,10]. ' &
      //           'Use constant step of 0.99.'                      )


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.99D0
      START  = 1.D0
      FINISH = 10.D0
      CNVTOL = 1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intervals found.
!
      XN = 5
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

!
!     Check the interval endpoints.
!
      DO I = 1, 2*XN
         XRESLT(I) = DBLE(I)
      END DO

      TOL = 2 * CNVTOL

      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2*XN, TOL, OK )

!
!     Check the endpoint flags. The first and last endpoints are
!     not considered to be state transitions.
!
      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .FALSE., OK )
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .FALSE., OK )

!
!     Check the returned points. The values are defined only for
!     transitions, so the first and last points in the array are
!     not checked.
!
      DO I = 1, 2*XN, 2

         DO J = 1, 3

            XPNTS(J,I  ) = DBLE (I+J-1)
            XPNTS(J,I+1) = XPNTS(J,I)

         END DO

      END DO

      CALL CHCKAD ( 'POINTS', POINTS(1,2), '~', &
                              XPNTS (1,2), 3*(2*XN-2), TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find all state transitions on interval [0.5,10.5]. ' &
      //           'Use constant step of 0.5.'                        )


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  =  0.5D0
      FINISH = 10.5D0
      CNVTOL = 1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intervals found.
!
      XN = 5
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

!
!     Check the interval endpoints.
!
      DO I = 1, 2*XN
         XRESLT(I) = DBLE(I)
      END DO

      TOL = 2 * CNVTOL

      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2*XN, TOL, OK )

!
!     Check the endpoint flags. The first and last endpoints ARE
!     not considered to be state transitions.
!
      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .TRUE., OK )
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .TRUE., OK )

!
!     Check the returned points. All of the points should be valid.
!
      DO I = 1, 2*XN, 2

         DO J = 1, 3

            XPNTS(J,I  ) = DBLE (I+J-1)
            XPNTS(J,I+1) = XPNTS(J,I)

         END DO

      END DO

      CALL CHCKAD ( 'POINTS', POINTS, '~', XPNTS, 6*XN, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find all state transitions on interval [0.5,10.5]. ' &
      //           'Use a step of 0.5, but use the GF step callback.' )



      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .FALSE.

      CALL GFSSTP ( 0.5D0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      STEP   = 0.5D0
      START  =  0.5D0
      FINISH = 10.5D0
      CNVTOL = 1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intervals found.
!
      XN = 5
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

!
!     Check the interval endpoints.
!
      DO I = 1, 2*XN
         XRESLT(I) = DBLE(I)
      END DO

      TOL = 2 * CNVTOL

      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2*XN, TOL, OK )

!
!     Check the endpoint flags. The first and last endpoints ARE
!     not considered to be state transitions.
!
      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .TRUE., OK )
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .TRUE., OK )

!
!     Check the returned points. All of the points should be valid.
!
      DO I = 1, 2*XN, 2

         DO J = 1, 3

            XPNTS(J,I  ) = DBLE (I+J-1)
            XPNTS(J,I+1) = XPNTS(J,I)

         END DO

      END DO

      CALL CHCKAD ( 'POINTS', POINTS, '~', XPNTS, 6*XN, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find all state transitions on interval [0.5,10.5]. ' &
      //           'Use constant step of 0.99.'                        )


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.99D0
      START  =  0.5D0
      FINISH = 10.5D0
      CNVTOL = 1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intervals found.
!
      XN = 5
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

!
!     Check the interval endpoints.
!
      DO I = 1, 2*XN
         XRESLT(I) = DBLE(I)
      END DO

      TOL = 2 * CNVTOL

      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2*XN, TOL, OK )

!
!     Check the endpoint flags. The first and last endpoints ARE
!     not considered to be state transitions.
!
      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .TRUE., OK )
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .TRUE., OK )

!
!     Check the returned points. All of the points should be valid.
!
      DO I = 1, 2*XN, 2

         DO J = 1, 3

            XPNTS(J,I  ) = DBLE (I+J-1)
            XPNTS(J,I+1) = XPNTS(J,I)

         END DO

      END DO

      CALL CHCKAD ( 'POINTS', POINTS, '~', XPNTS, 6*XN, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Look for state transition on interval [2.0,2.5]. ' &
      //           'Use constant step of 0.5. A transition should ' &
      //           'be found. The first interval endpoint should ' &
      //           'be considered a non-transition endpoint, while ' &
      //           'the second one should be considered a transition.' )

!
!     In this case the state function is .TRUE. at X = 1.0 and
!     nowhere else.
!

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.0D0
      FINISH = 2.5D0
      CNVTOL = 1.D-12

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intervals found. We should have a transition
!     at the right endpoint of the search interval.
!
      XN = 1
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .FALSE., OK )

!
!     The last endpoint flag should indicate a transition.
!
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .TRUE., OK )

!
!     Check the result window. The second endpoint is determined
!     by the convergence algorithm. We expect the actual result
!     only to be close to the expected value.
!
      XRESLT(1) = START
      XRESLT(2) = START + CNVTOL/2


      TOL = 2 * CNVTOL

      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2, TOL, OK )

!
!     The point value at X = 2 is the same as at X = 1.
!
      CALL VPACK ( 1.D0, 2.D0, 3.D0, XPNTS )

      CALL CHCKAD ( 'POINTS', POINTS(1,2), '~~', XPNTS, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Look for state transition on interval [2.5,3.0]. ' &
      //           'Use constant step of 0.5. A transition should ' &
      //           'be found. The first interval endpoint should ' &
      //           'be considered a transition endpoint, while ' &
      //           'the second one should NOT be considered a ' &
      //           'transition.'                               )

!
!     In this case the state function is .TRUE. at X = 1.0 and
!     nowhere else.
!

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.5D0
      FINISH = 3.0D0
      CNVTOL = 1.D-12

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the number of intervals found. We should have a transition
!     at the right endpoint of the search interval.
!
      XN = 1
      N  = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', XN, 0, OK )

      CALL CHCKSL ( 'First endpoint flag', ENDFLG(1), .TRUE., OK )

!
!     The last endpoint flag should indicate a transition.
!
      CALL CHCKSL ( 'Last endpoint flag',  ENDFLG(2), .FALSE., OK )

!
!     Check the result window. The second endpoint is determined
!     by the convergence algorithm. We expect the actual result
!     only to be close to the expected value.
!
      XRESLT(1) = FINISH - CNVTOL/2
      XRESLT(2) = FINISH


      TOL = 2 * CNVTOL

      CALL CHCKAD ( 'RESULT', RESULT(1), '~', XRESLT, 2, TOL, OK )

!
!     The point value should correspond to the left endpoint.
!
      CALL VPACK ( 3.D0, 4.D0, 5.D0, XPNTS )

      CALL CHCKAD ( 'POINTS', POINTS, '~~', XPNTS, 3, TOL, OK )



!**********************************************************************
!
!     ZZTANSLV Error cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-positive tolerance.' )



      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.5D0
      FINISH = 3.0D0
      CNVTOL = 0.D0

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )

      CNVTOL   = -1.D-12

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Tolerance too small compared to START.' )



      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.5D0
      FINISH = 3.0D0

      CNVTOL = 1.D-20 * START

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Tolerance too small compared to FINISH.' )



      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.5D0
      FINISH = 3.0D0

      CNVTOL = 1.D-20 * FINISH

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-positive constant step.' )


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.D0
      START  = 2.5D0
      FINISH = 3.0D0

      CNVTOL = 1.D-3 * FINISH

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCONSTSTEP)', OK )

      STEP = -1.D0

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCONSTSTEP)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Constant step is positive but too small.' )


      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      START  = 2.5D0
      FINISH = 3.0D0

      STEP   = 1.D-19 * FINISH

      CNVTOL = 1.D-3 * FINISH

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCONSTSTEP)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'START > FINISH.' )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.5D0
      FINISH = START - 1.D0

      CNVTOL = 1.D-12

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'START = FINISH.' )
!
!     This is a non-error exception.
!

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   = 0.5D0
      START  = 2.5D0
      FINISH = START

      CNVTOL = 1.D-12

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Points array too small.' )



      CALL SSIZED ( 8, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   =  0.5D0
      START  =  0.5D0
      FINISH = 10.5D0
      CNVTOL = 1.D-12

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Result window too small.' )



      CALL SSIZED ( 9, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CSTEP  = .TRUE.
      STEP   =  0.5D0
      START  =  0.5D0
      FINISH = 10.5D0
      CNVTOL = 1.D-12

      CALL CLEARD( 3*MAXWIN, POINTS )

      CALL ZZTANSLV ( T_TANSTA, GFSTEP, GFREFN, &
                      CSTEP,    STEP,   START,  FINISH, &
                      CNVTOL,   RESULT, POINTS, ENDFLG  )
      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWEXCESS)', OK )

!
!     End of error cases.
!

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END



!
!     State function for use by F_ZZTANSLV.
!
!     Let LOWER be defined as
!
!        LOWER = floor( X ).
!
!     The function returns .TRUE. if
!
!        LOWER
!
!     is odd and
!
!        0  <  LOWER  <  X  <  LOWER+1
!           -         -     -
!
!     For all other X, the function returns .FALSE.
!
!     When the function returns .TRUE., the returned
!     value of POINT is
!
!        ( LOWER, LOWER+1, LOWER+2 )
!
!     When the function returns .FALSE., the returned
!     value of POINT is
!
!        ( 0, 0, 0 )
!
!
      SUBROUTINE T_TANSTA ( X, STATE, POINT )

      IMPLICIT NONE

      DOUBLE PRECISION      X
      LOGICAL               STATE
      DOUBLE PRECISION      POINT ( 3 )

!
!     SPICELIB functions
!
      LOGICAL               ODD

!
!     Local variables
!
      DOUBLE PRECISION      DVAL
      DOUBLE PRECISION      FLOOR

      INTEGER               IFLOOR


      IF ( X .LT. 1.D0 ) THEN

         STATE = .FALSE.
         CALL CLEARD ( 3, POINT )

      ELSE

         FLOOR  = AINT ( X )
         IFLOOR = INT ( FLOOR )

         IF ( ODD(IFLOOR) ) THEN

            STATE = .TRUE.
            DVAL  = FLOOR

         ELSE IF ( X .EQ. FLOOR ) THEN
!
!           IFLOOR is even.
!
            STATE = .TRUE.
            DVAL  = FLOOR - 1.D0

         ELSE

            STATE = .FALSE.

         END IF

      END IF


      IF ( STATE ) THEN

         CALL VPACK ( DVAL, DVAL+1.D0, DVAL+2.D0, POINT )

      ELSE

         CALL CLEARD ( 3, POINT )

      END IF

      RETURN
      END
