!> EK Token Code Parameters.
!>
!>     The EK query language tokens and codes are:
!>
!>## Version
!>
!>        ektokn.inc  Version 2    25-JAN-1995 (NJB)
!>
!>           Updated to distinguish between special characters.
!>
!>        ektokn.inc  Version 1    05-DEC-1994 (NJB)
module pepper_inc_ektokn

   use pepper_kinds, only: ip

   implicit none

   public

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
   integer(ip), parameter :: tkkey = 1

   integer(ip), parameter :: tkid = tkkey + 1

   integer(ip), parameter :: tkint = tkid + 1

   integer(ip), parameter :: tkdp = tkint + 1

   integer(ip), parameter :: tkqstr = tkdp + 1

   integer(ip), parameter :: tklpar = tkqstr + 1

   integer(ip), parameter :: tkrpar = tklpar + 1

   integer(ip), parameter :: tkcoma = tkrpar + 1

   integer(ip), parameter :: tkdot = tkcoma + 1

   integer(ip), parameter :: tkstar = tkdot + 1

   integer(ip), parameter :: tkeoq = tkstar + 1

end module pepper_inc_ektokn
