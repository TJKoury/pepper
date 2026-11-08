
      CHARACTER*(*) FUNCTION BEGTXT ()

      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )

      BEGTXT = CHAR(BSLASH) // 'begintext'
      RETURN
      END



