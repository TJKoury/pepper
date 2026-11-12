

      SUBROUTINE GETEQ ( LETTER )
      use pepper_legacy
      IMPLICIT NONE

      CHARACTER*(1)         LETTER
!
!     This is simply a utility routine that sets the special
!     protection character that is used by the symbol and
!     query resolution software so that protected strings are
!     not translated.  There are two entry points GETEQ and SETEQ
!     Their functions should be obvious.  The default value of
!     the special marker is '@'.
!
      CHARACTER*(1)         EQUOTE
      SAVE
      DATA                  EQUOTE / '@' /

      LETTER = EQUOTE
      RETURN


      ENTRY SETEQ ( LETTER )

      EQUOTE = LETTER
      RETURN

      END
