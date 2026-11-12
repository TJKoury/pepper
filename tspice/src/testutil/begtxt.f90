
      CHARACTER*(*) FUNCTION BEGTXT ()
      use pepper_legacy

      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )

      BEGTXT = CHAR(BSLASH) // 'begintext'
      RETURN
      END



