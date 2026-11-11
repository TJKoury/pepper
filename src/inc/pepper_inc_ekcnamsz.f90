!> EK Column Name Size.
!>
!>## Version
!>
!>        ekcnamsz.inc Version 1    17-JAN-1995 (NJB)
module pepper_inc_ekcnamsz

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: cnamsz = 32  !! Size of column name, in characters.

end module pepper_inc_ekcnamsz
