
      CHARACTER*(*) FUNCTION BEGDAT ()
      use pepper_legacy

      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )

      BEGDAT = CHAR(BSLASH) // 'begindata'
      RETURN

      END








