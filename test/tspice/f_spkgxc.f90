!$Procedure      F_SPKGXC (Family of tests for SPKGEO frame exceptions)

      SUBROUTINE F_SPKGXC ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the behaviour of the SPICE routine SPKGEO
!     when a bogus input frame is supplied as an argument.
!
!-&

!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               LWSIZE
      PARAMETER           ( LWSIZE = 40 )


      CHARACTER*(WDSIZE)    REF

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      STATE   ( 6 )


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SPKGXC' )

      ET = 0.0D0

      CALL TCASE ( 'Check that we get reasonable unrecognized ' &
      //           'frame diagnostics subcase 1.' )

         REF = 'TRANSGALCTIC'
         CALL SPKGEO ( -10, ET, REF, 399, STATE,   LT )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )

      CALL TCASE ( 'Check that we get reasonable unrecognized ' &
      //           'frame diagnostics subcase 2.' )

         REF = ' '
         CALL SPKGEO ( -10, ET, REF, 399, STATE,   LT )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )

      CALL TCASE ( 'Check that we get reasonable unrecognized ' &
      //           'frame diagnostics subcase 3.' )

         REF = 'MY' // CHAR(9) //'FRAME'
         CALL SPKGEO ( -10, ET, REF, 399, STATE,   LT )
         CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
