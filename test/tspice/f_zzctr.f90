!$Procedure F_ZZCTR ( Family of tests for ZZCTR )

      SUBROUTINE F_ZZCTR ( OK )

      IMPLICIT NONE

! $ Abstract
!
!     This routine tests state counter arrays.
!
! $ Required_Reading
!
!     None
!
! $ Keywords
!
!     None
!
! $ Declarations

      INCLUDE               'zzctr.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     None
!
! $ Detailed_Input
!
!     None
!
! $ Detailed_Output
!
!     None
!
! $ Parameters
!
!     None
!
! $ Exceptions
!
!     None
!
! $ Files
!
!     None
!
! $ Particulars
!
!     None
!
! $ Examples
!
!     None
!
! $ Restrictions
!
!     None
!
! $ Literature_References
!
!     None
!
! $ Author_and_Institution
!
!     None
!
! $ Version
!
!-    Version 1.0.0  29-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     None
!
!-&

!
!     Test Utility Functions
!

!
!     SPICELIB Functions
!
      INTEGER               INTMIN
      INTEGER               INTMAX

!
!     Local parameters
!

!
!     Local Variables
!
      INTEGER               NEWCTR ( CTRSIZ )
      INTEGER               OLDCTR ( CTRSIZ )
      INTEGER               SAVCTR ( CTRSIZ )

      LOGICAL               UPDATE

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZCTR' )

!
!     Initialize counters to high value.
!
      CALL TCASE ( 'Initialize counter to user values.' )

      CALL ZZCTRUIN ( OLDCTR )

      CALL CHCKSI ( 'OLDCTR(1)', OLDCTR(1), '=', INTMAX(), 0, OK )
      CALL CHCKSI ( 'OLDCTR(2)', OLDCTR(2), '=', INTMAX(), 0, OK )

!
!     Initialize counters to low value.
!
      CALL TCASE ( 'Initialize counter to subsystem values.' )

      CALL ZZCTRSIN ( OLDCTR )

      CALL CHCKSI ( 'OLDCTR(1)', OLDCTR(1), '=', INTMIN(), 0, OK )
      CALL CHCKSI ( 'OLDCTR(2)', OLDCTR(2), '=', INTMIN(), 0, OK )

!
!     Increment counters.
!
      CALL TCASE ( 'Increment counter.' )

      CALL ZZCTRINC ( OLDCTR )

      CALL CHCKSI ( 'OLDCTR(1)', OLDCTR(1), '=', INTMIN()+1, 0, OK )
      CALL CHCKSI ( 'OLDCTR(2)', OLDCTR(2), '=', INTMIN(), 0, OK )

!
!     Increment counters with roll over.
!
      CALL TCASE ( 'Increment counter with roll over.' )

      OLDCTR(1) = INTMAX()

      CALL ZZCTRINC ( OLDCTR )

      CALL CHCKSI ( 'OLDCTR(1)', OLDCTR(1), '=', INTMIN(), 0, OK )
      CALL CHCKSI ( 'OLDCTR(2)', OLDCTR(2), '=', INTMIN()+1, 0, OK )

!
!     Check counters without update.
!
      CALL TCASE ( 'Check counter with no update.' )

      CALL MOVEI ( OLDCTR, CTRSIZ, NEWCTR )
      CALL MOVEI ( OLDCTR, CTRSIZ, SAVCTR )

      CALL ZZCTRCHK ( NEWCTR, OLDCTR, UPDATE )

      CALL CHCKAI ( 'OLDCTR', OLDCTR, '=', SAVCTR, CTRSIZ,  OK )
      CALL CHCKAI ( 'NEWCTR', NEWCTR, '=', SAVCTR, CTRSIZ,  OK )
      CALL CHCKSL ( 'UPDATE', UPDATE, .FALSE.,  OK )

!
!     Check with first element different and update.
!
      CALL TCASE ( 'Check counter with update, first element.' )

      CALL MOVEI ( OLDCTR, CTRSIZ, NEWCTR )
      NEWCTR(1) = NEWCTR(1) + 1
      CALL MOVEI ( NEWCTR, CTRSIZ, SAVCTR )

      CALL ZZCTRCHK ( NEWCTR, OLDCTR, UPDATE )

      CALL CHCKAI ( 'OLDCTR', OLDCTR, '=', SAVCTR, CTRSIZ,  OK )
      CALL CHCKAI ( 'NEWCTR', NEWCTR, '=', SAVCTR, CTRSIZ,  OK )
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE.,  OK )

!
!     Check with second element different and update.
!
      CALL TCASE ( 'Check counter with update, second element.' )

      CALL MOVEI ( OLDCTR, CTRSIZ, NEWCTR )
      NEWCTR(2) = NEWCTR(2) + 1
      CALL MOVEI ( NEWCTR, CTRSIZ, SAVCTR )

      CALL ZZCTRCHK ( NEWCTR, OLDCTR, UPDATE )

      CALL CHCKAI ( 'OLDCTR', OLDCTR, '=', SAVCTR, CTRSIZ,  OK )
      CALL CHCKAI ( 'NEWCTR', NEWCTR, '=', SAVCTR, CTRSIZ,  OK )
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE.,  OK )

!
!     Exception: try to increment counters at maximum value.
!
      CALL TCASE ( 'Exception: try to increment at maximum value.' )

      CALL ZZCTRUIN ( OLDCTR )

      CALL CHCKSI ( 'OLDCTR(1)', OLDCTR(1), '=', INTMAX(), 0, OK )
      CALL CHCKSI ( 'OLDCTR(2)', OLDCTR(2), '=', INTMAX(), 0, OK )

      CALL ZZCTRINC ( OLDCTR )

      CALL CHCKXC ( .TRUE., 'SPICE(SPICEISTIRED)', OK )
      CALL CHCKSI ( 'OLDCTR(1)', OLDCTR(1), '=', INTMAX(), 0, OK )
      CALL CHCKSI ( 'OLDCTR(2)', OLDCTR(2), '=', INTMAX(), 0, OK )

!
!     This is good enough for now.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
