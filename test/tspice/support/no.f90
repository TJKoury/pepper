      LOGICAL FUNCTION NO ( ERROR )
      IMPLICIT NONE
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!

      CHARACTER*(*)         ERROR
      LOGICAL               HAVE

      NO = .NOT. HAVE(ERROR)

      RETURN
      END
