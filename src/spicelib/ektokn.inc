! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
!
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
