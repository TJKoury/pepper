!$Procedure F_EDNMPT ( EDNMPT tests )

      SUBROUTINE F_EDNMPT ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the SPICELIB routine EDNMPT.
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
!     This routine tests the SPICELIB routine EDNMPT.
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
!-    TSPICE Version 1.0.0, 17-MAY-2016 (NJB)
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


      INTEGER               TITLEN
      PARAMETER           ( TITLEN = 160 )

!
!     Local Variables
!
      CHARACTER*(TITLEN)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      P      ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XNORML ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               NSTEP

!
!     Saved values
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_EDNMPT' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Sphere tests' )

      R     = 7.D0
      A     = R
      B     = R
      C     = R

      NSTEP = 10
      DLON  = 2*PI() / NSTEP
      DLAT  =   PI() / NSTEP

      DO I = 1, NSTEP

         LON = I * DLON

         DO J = 0, NSTEP

            LAT = ( PI()/2 ) - J * DLAT

            CALL LATREC ( 1.D0, LON, LAT, XNORML )

!
! --- Case: ------------------------------------------------------
!
            TITLE = 'Sphere: normal = (#,#,#)'
            CALL REPMD ( TITLE, '#', XNORML(1), 14, TITLE )
            CALL REPMD ( TITLE, '#', XNORML(2), 14, TITLE )
            CALL REPMD ( TITLE, '#', XNORML(3), 14, TITLE )
            CALL CHCKXC( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )


            CALL EDNMPT ( A, B, C, XNORML, P )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Check that we can recover the input normal
!           from P.
!
            CALL SURFNM ( A, B, C, P, NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = VTIGHT

            CALL CHCKAD ( 'NORMAL',  NORMAL, '~~/', &
                          XNORML, 3, TOL,    OK   )

         END DO

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Triaxial ellipsoid tests' )

      A     = 100.D0
      B     =  50.D0
      C     =   3.D0

      NSTEP = 10
      DLON  = 2*PI() / NSTEP
      DLAT  =   PI() / NSTEP

      DO I = 1, NSTEP

         LON = I * DLON

         DO J = 0, NSTEP

            LAT = ( PI()/2 ) - J * DLAT

            CALL LATREC ( 1.D0, LON, LAT, XNORML )

!
! --- Case: ------------------------------------------------------
!
            TITLE = 'Ellipsoid: normal = (#,#,#)'
            CALL REPMD ( TITLE, '#', XNORML(1), 14, TITLE )
            CALL REPMD ( TITLE, '#', XNORML(2), 14, TITLE )
            CALL REPMD ( TITLE, '#', XNORML(3), 14, TITLE )
            CALL CHCKXC( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )


            CALL EDNMPT ( A, B, C, XNORML, P )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Check that we can recover the input normal
!           from P.
!
            CALL SURFNM ( A, B, C, P, NORMAL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = VTIGHT

            CALL CHCKAD ( 'NORMAL',  NORMAL, '~~/', &
                          XNORML, 3, TOL,    OK   )

         END DO

      END DO





!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid semi-axis lengths.' )


      A     = 100.D0
      B     =  50.D0
      C     =   3.D0

      CALL VPACK ( 0.D0, 0.D0, 1.D0, XNORML )

      CALL EDNMPT ( -A, B, C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL EDNMPT (  A, -B, C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL EDNMPT (  A, B, -C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL EDNMPT (  0.D0, B, C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL EDNMPT (  A, 0.D0, C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

      CALL EDNMPT (  A, B, 0.D0, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid semi-axis length after scaling.' )


      A     = 1.D300
      B     = 1.D-300
      C     =   3.D0

      CALL VPACK ( 0.D0, 0.D0, 1.D0, XNORML )

      CALL EDNMPT (  A, B, C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(AXISUNDERFLOW)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Zero normal vector.' )

      A = 1.D0
      B = A
      C = A

      CALL CLEARD ( 3, XNORML )

      CALL EDNMPT (  A, B, C, XNORML, P )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )



!
! --- Case: ------------------------------------------------------
!

!
!     The degenerate case error resulting from a non-positive
!     reciprocal of the square of lambda should never occur.
!     A disposable version of the code should be used to
!     exercise the error handling for this case.
!


!
! ---------------------------------------------------------
!

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


