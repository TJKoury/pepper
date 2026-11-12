!$Procedure F_ZZPDPLTC ( ZZPDPLTC tests )

      SUBROUTINE F_ZZPDPLTC ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise the private SPICELIB geometry routine ZZPDPLTC.
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
!     This routine tests the private SPICELIB
!     geometry routine ZZPDPLTC.
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
!-    TSPICE Version 1.0.0, 18-FEB-2017 (NJB)
!
!-&

!
!     SPICELIB functions
!


!
!     Local Parameters
!

!
!     This value must match that in ZZPDPLTC.
!
      DOUBLE PRECISION      LIMIT
      PARAMETER           ( LIMIT = 1.D-2 )

!
!     Local Variables
!
!
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      P     ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      S
      DOUBLE PRECISION      XXPT
      DOUBLE PRECISION      XYPT

      LOGICAL               ISON
      LOGICAL               XISON

!
!     Saved values
!
!     Initial values
!


!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZPDPLTC' )




!***********************************************************************
!
!     Error cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RE is zero.' )


      RE    = 0.D0
      F     = 0.D0
      LAT   = 0.D0

      P(1)  = 1.D0
      P(2)  = 1.D0
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'RE is negative.' )


      RE    = -1.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4

      P(1)  = 1.D0
      P(2)  = 1.D0
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'F > 1.' )


      RE    = 3.D0
      RP    = 2.D0
      F     = 2.D0
      LAT   = PI()/4

      P(1)  = 1.D0
      P(2)  = 1.D0
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )






!***********************************************************************
!
!     Oblate tests
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = PI/4; P(3) > 0' )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4

      P(1)  = 1.D0
      P(2)  = 1.D0
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = -PI/4; P(3) > 0' )


      LAT   = -PI()/4


      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .FALSE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) > 0' )


      LAT   = 0.D0


      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = PI/4; P(3) < 0' )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4

      P(1)  =  1.D0
      P(2)  =  1.D0
      P(3)  = -1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .FALSE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = -PI/4; P(3) < 0' )


      LAT   = -PI()/4


      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) < 0' )


      LAT   = 0.D0


      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )




!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = PI/4; P(3) = 0' )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4

      P(1)  =  1.D0
      P(2)  =  1.D0
      P(3)  =  0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = -PI/4; P(3) = 0' )


      LAT   = -PI()/4


      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) = 0' )


      LAT   = 0.D0


      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )




!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = LIMIT/2; P(3) > 0, ' &
      //           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = LIMIT / 2

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = LIMIT/2; P(3) > 0, ' &
      //           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = LIMIT / 2

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The point is accepted on the basis of its Z component
!     having the correct sign.
!
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = LIMIT/2; P(3) < 0, ' &
      //           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = LIMIT / 2

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = -1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = LIMIT/2; P(3) < 0, ' &
      //           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = LIMIT / 2

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = -1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .FALSE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = LIMIT/2; P(3) = 0, ' &
      //           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = LIMIT / 2

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = LIMIT/2; P(3) = 0, ' &
      //           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = LIMIT / 2

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The point is accepted because its Z component is zero.
!
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = -LIMIT/2; P(3) = 0, ' &
      //           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -LIMIT / 2

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = -LIMIT/2; P(3) = 0, ' &
      //           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -LIMIT / 2

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The point is accepted because its Z component is zero.
!
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) = 0, ' &
      //           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = 0.D0; P(3) = 0, ' &
      //           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )





!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = -LIMIT/2; P(3) > 0, ' &
      //           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -LIMIT / 2

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = -LIMIT/2; P(3) > 0, ' &
      //           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -LIMIT / 2

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .FALSE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = -LIMIT/2; P(3) < 0, ' &
      //           'radius > XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -LIMIT / 2

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = -1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = -LIMIT/2; P(3) < 0, ' &
      //           'radius < XXPT.'                          )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -LIMIT / 2

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = -1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The point is accepted on the basis of the sign of its Z
!     component.
!
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) > 0, ' &
      //           'radius > XXPT.'                  )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) > 0, ' &
      //           'radius < XXPT.'                   )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) < 0, ' &
      //           'radius > XXPT.'                   )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = -1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) < 0, ' &
      //           'radius < XXPT.'                   )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = -1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) = 0, ' &
      //           'radius > XXPT.'                   )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT * 2
      P(2)  = S * XXPT * 2
      P(3)  = 0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Oblate case; LAT = 0; P(3) = 0, ' &
      //           'radius < XXPT.'                   )


      RE    = 4.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      CALL ZZELNAXX ( RE, F, LAT, XXPT, XYPT )

      S     = SQRT( 2.D0 ) / 2

      P(1)  = S * XXPT / 2
      P(2)  = S * XXPT / 2
      P(3)  = 0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )





!***********************************************************************
!
!     Prolate tests
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Prolate case; LAT = PI/4; P(3) > 0' )


      RE    = 2.D0
      RP    = 4.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4

      P(1)  = 1.D0
      P(2)  = 1.D0
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Prolate case; LAT = -PI/4; P(3) > 0' )

      LAT   = -PI()/4


      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note: the prolate case ALWAYS returns .TRUE.
!
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Prolate case; LAT = 0; P(3) > 0' )

      LAT   = 0.D0


      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Prolate case; LAT = PI/4; P(3) < 0' )


      LAT   = PI()/4

      P(1)  =  1.D0
      P(2)  =  1.D0
      P(3)  = -1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Prolate case; LAT = -PI/4; P(3) < 0' )


      LAT   = -PI()/4


      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note: the prolate case ALWAYS returns .TRUE.
!
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Prolate case; LAT = 0; P(3) < 0' )

      LAT   = 0.D0


      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Prolate case; LAT = PI/4; P(3) = 0' )


      LAT   = PI()/4

      P(1)  =  1.D0
      P(2)  =  1.D0
      P(3)  =  0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Prolate case; LAT = -PI/4; P(3) = 0' )


      LAT   = -PI()/4


      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note: the prolate case ALWAYS returns .TRUE.
!
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Prolate case; LAT = 0; P(3) = 0' )


      LAT   = 0.D0


      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )




!***********************************************************************
!
!     Sphere tests
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Sphere case; LAT = PI/4; P(3) > 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4

      P(1)  = 1.D0
      P(2)  = 1.D0
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Sphere case; LAT = -PI/4; P(3) > 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -PI()/4

      P(1)  = 1.D0
      P(2)  = 1.D0
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note: the prolate case ALWAYS returns .TRUE.
!
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Sphere case; LAT = 0; P(3) > 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      P(1)  = 1.D0
      P(2)  = 1.D0
      P(3)  = 1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Sphere case; LAT = PI/4; P(3) < 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4

      P(1)  =  1.D0
      P(2)  =  1.D0
      P(3)  = -1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Sphere case; LAT = -PI/4; P(3) < 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -PI()/4

      P(1)  =  1.D0
      P(2)  =  1.D0
      P(3)  = -1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note: the prolate case ALWAYS returns .TRUE.
!
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Sphere case; LAT = 0; P(3) < 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      P(1)  =  1.D0
      P(2)  =  1.D0
      P(3)  = -1.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )

!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Sphere case; LAT = PI/4; P(3) = 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = PI()/4

      P(1)  =  1.D0
      P(2)  =  1.D0
      P(3)  =  0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Sphere case; LAT = -PI/4; P(3) = 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = -PI()/4

      P(1)  =  1.D0
      P(2)  =  1.D0
      P(3)  =  0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note: the prolate case ALWAYS returns .TRUE.
!
      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Sphere case; LAT = 0; P(3) = 0' )


      RE    = 2.D0
      RP    = 2.D0
      F     = ( RE - RP ) / RE
      LAT   = 0.D0

      P(1)  =  1.D0
      P(2)  =  1.D0
      P(3)  =  0.D0

      ISON  = ZZPDPLTC ( RE, F, P, LAT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XISON = .TRUE.

      CALL CHCKSL ( 'ISON', ISON, XISON, OK )



!
! --- Case: ------------------------------------------------------
!


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


