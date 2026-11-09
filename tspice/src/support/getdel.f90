

      SUBROUTINE GETDEL ( LETTER )
      IMPLICIT NONE

      CHARACTER*(1)         LETTER
!
!     This is simply a utility routine that sets the special
!     character that is used by the a command loop program to
!     terminate command inputs.  It has a get and set entry
!     points.  The functions should be obvious.  If not you
!     should consider another career.
!
      CHARACTER*(1)         DELIM
      SAVE
      DATA                  DELIM / ';' /

      LETTER = DELIM
      RETURN


      ENTRY SETDEL ( LETTER )

      DELIM = LETTER
      RETURN

      END
