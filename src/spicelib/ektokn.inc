!     Include Section:  EK Token Code Parameters
!
!        ektokn.inc  Version 2    25-JAN-1995 (NJB)
!
!           Updated to distinguish between special characters.
!
!
!        ektokn.inc  Version 1    05-DEC-1994 (NJB)
!
!
!     The EK query language tokens and codes are:
!
!        <keyword>
!        <identifier>
!        <integer>
!        <d.p. number>
!        <quoted string>
!        <left parenthesis>
!        <right parenthesis>
!        <comma>
!        <period>
!        <end of query>
!
!
      INTEGER               TKKEY
      PARAMETER           ( TKKEY  = 1 )

      INTEGER               TKID
      PARAMETER           ( TKID   = TKKEY + 1 )

      INTEGER               TKINT
      PARAMETER           ( TKINT  = TKID + 1 )

      INTEGER               TKDP
      PARAMETER           ( TKDP   = TKINT + 1 )

      INTEGER               TKQSTR
      PARAMETER           ( TKQSTR = TKDP + 1 )

      INTEGER               TKLPAR
      PARAMETER           ( TKLPAR = TKQSTR + 1 )

      INTEGER               TKRPAR
      PARAMETER           ( TKRPAR = TKLPAR + 1 )

      INTEGER               TKCOMA
      PARAMETER           ( TKCOMA = TKRPAR + 1 )

      INTEGER               TKDOT
      PARAMETER           ( TKDOT  = TKCOMA + 1 )

      INTEGER               TKSTAR
      PARAMETER           ( TKSTAR = TKDOT  + 1 )

      INTEGER               TKEOQ
      PARAMETER           ( TKEOQ  = TKSTAR + 1 )

!
!     End Include Section:  EK Token Code Parameters
!
