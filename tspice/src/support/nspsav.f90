      SUBROUTINE NSPSAV  ( FILE, ERROR )
      use pepper_legacy
!
!     This routine opens the file specified by FILE for saving all
!     screen output.
!
      IMPLICIT NONE

      CHARACTER*(*)         FILE
      CHARACTER*(*)         ERROR ( 2 )

!
!     Interface to SPICELIB error handling.
!
      LOGICAL               HAVE
!
!     Not much to this, just call the correct routine from NSPIO.
!
      CALL NSPOPN  ( 'SAVE', FILE )

      IF ( HAVE(ERROR)  ) THEN
         CALL PREFIX ( 'NSPSAV: ', 1, ERROR(1) )
         RETURN
      END IF


      RETURN
      END
