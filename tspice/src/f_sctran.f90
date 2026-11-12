!$Procedure      F_SCTRAN ( Test SCLK name/ID translation )

      SUBROUTINE F_SCTRAN ( OK )
      use pepper_legacy
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the routines SCN2ID and SCID2N.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 02-MAR-2000 (NJB)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!

!
!     Local Parameters
!
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 80 )
!
!     Local Variables
!
      CHARACTER*(NAMLEN)    NAME

      INTEGER               ID

      LOGICAL               FOUND

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SCTRAN' )

      CALL TCASE ( 'Map ID -77 to clock string.'  )

      CALL SCID2N ( -77, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'GALILEO ORBITER SCLK', OK )


      CALL TCASE ( 'Map ID -77777 to clock string.  No string '// &
                   'should be found.'                             )

      CALL SCID2N ( -77777, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .FALSE.,  OK )


      CALL TCASE ( 'Map clock string ''GALILEO ORBITER SCLK'' ' // &
                   'to ID -77 .'                                    )

      CALL SCN2ID ( 'GALILEO ORBITER SCLK', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSI ( 'ID', ID, '=', -77, 0, OK )


      CALL TCASE ( 'Map clock string ''galileo orbiter sclk'' ' // &
                   'to ID -77 .'                                    )

      CALL SCN2ID ( 'galileo orbiter sclk', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSI ( 'ID', ID, '=', -77, 0, OK )


      CALL TCASE ( 'Map clock string ''  gAlileo orbIter  sclk'' ' // &
                   'to ID -77 .'                                    )

      CALL SCN2ID ( '  gAlileo orbIter  sclk', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSI ( 'ID', ID, '=', -77, 0, OK )


      CALL TCASE ( 'Map clock string ''  CAS sclk'' ' // &
                   'to ID -82 .'                                    )

      CALL SCN2ID ( '  CAS sclk', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .TRUE.,  OK )
      CALL CHCKSI ( 'ID', ID, '=', -82, 0, OK )

      CALL TCASE ( 'Map ''XYXYX'' to clock ID.  No ID '// &
                   'should be found.'                             )

      CALL SCN2ID ( 'XYXYX', ID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND',   FOUND,  .FALSE.,  OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
