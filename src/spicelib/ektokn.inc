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
      integer               tkkey
      parameter           ( tkkey  = 1 )

      integer               tkid
      parameter           ( tkid   = tkkey + 1 )

      integer               tkint
      parameter           ( tkint  = tkid + 1 )

      integer               tkdp
      parameter           ( tkdp   = tkint + 1 )

      integer               tkqstr
      parameter           ( tkqstr = tkdp + 1 )

      integer               tklpar
      parameter           ( tklpar = tkqstr + 1 )

      integer               tkrpar
      parameter           ( tkrpar = tklpar + 1 )

      integer               tkcoma
      parameter           ( tkcoma = tkrpar + 1 )

      integer               tkdot
      parameter           ( tkdot  = tkcoma + 1 )

      integer               tkstar
      parameter           ( tkstar = tkdot  + 1 )

      integer               tkeoq
      parameter           ( tkeoq  = tkstar + 1 )

!
!     End Include Section:  EK Token Code Parameters
!
