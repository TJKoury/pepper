
      CHARACTER*(*) FUNCTION BEGDAT ()

      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )

      BEGDAT = CHAR(BSLASH) // 'begindata'
      RETURN

      END








