!$Procedure F_ZZRECBOX ( ZZRECBOX tests )

      SUBROUTINE F_ZZRECBOX ( OK )

! $ Abstract
!
!     Exercise the private SPICELIB routine ZZRECBOX.
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
!     This routine tests the private SPICELIB routine ZZRECBOX.
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
!-    TSPICE Version 1.0.0, 04-JUN-2016 (NJB)
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
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      LX
      DOUBLE PRECISION      LY
      DOUBLE PRECISION      LZ
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XCTR   ( 3 )
      DOUBLE PRECISION      XR
      DOUBLE PRECISION      XLX
      DOUBLE PRECISION      XLY
      DOUBLE PRECISION      XLZ

      INTEGER               I


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
      CALL TOPEN ( 'F_ZZRECBOX' )


!***********************************************************************
!
!     Normal cases
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Box includes origin.' )

      BOUNDS(1,1) =  -1.D0
      BOUNDS(2,1) =  3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) =  4.D0

      BOUNDS(1,3) = -6.D0
      BOUNDS(2,3) =  8.D0


      DO I = 1, 3
         XCTR(I) = ( BOUNDS(1,I) + BOUNDS(2,I) ) / 2
      END DO

      XLX = BOUNDS(2,1) - BOUNDS(1,1)
      XLY = BOUNDS(2,2) - BOUNDS(1,2)
      XLZ = BOUNDS(2,3) - BOUNDS(1,3)

      XR  = SQRT( XLX**2 + XLY**2 + XLZ**2 ) / 2


      CALL ZZRECBOX ( BOUNDS, CENTER, LX, LY, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~~/', XCTR, 3, TOL, OK )

      CALL CHCKSD ( 'LX', LX, '~', XLX, TOL, OK )
      CALL CHCKSD ( 'LY', LY, '~', XLY, TOL, OK )
      CALL CHCKSD ( 'LZ', LZ, '~', XLZ, TOL, OK )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XR, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Box excludes origin.' )

      BOUNDS(1,1) =  3.D0
      BOUNDS(2,1) =  5.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) = -6.D0
      BOUNDS(2,3) = -5.D0


      DO I = 1, 3
         XCTR(I) = ( BOUNDS(1,I) + BOUNDS(2,I) ) / 2
      END DO

      XLX = BOUNDS(2,1) - BOUNDS(1,1)
      XLY = BOUNDS(2,2) - BOUNDS(1,2)
      XLZ = BOUNDS(2,3) - BOUNDS(1,3)

      XR  = SQRT( XLX**2 + XLY**2 + XLZ**2 ) / 2


      CALL ZZRECBOX ( BOUNDS, CENTER, LX, LY, LZ, RADIUS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'CENTER', CENTER, '~~/', XCTR, 3, TOL, OK )

      CALL CHCKSD ( 'LX', LX, '~', XLX, TOL, OK )
      CALL CHCKSD ( 'LY', LY, '~', XLY, TOL, OK )
      CALL CHCKSD ( 'LZ', LZ, '~', XLZ, TOL, OK )

      CALL CHCKSD ( 'RADIUS', RADIUS, '~', XR, TOL, OK )




!***********************************************************************
!
!     Error cases
!
!***********************************************************************

! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: invalid rectangular coordinate bounds.' )


      BOUNDS(1,1) =  1.D0
      BOUNDS(2,1) =  -3.D0

      BOUNDS(1,2) = -3.D0
      BOUNDS(2,2) = -1.D0

      BOUNDS(1,3) =  6.D0
      BOUNDS(2,3) =  8.D0


      CALL ZZRECBOX ( BOUNDS, CENTER, LX, LY, LZ, RADIUS )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


