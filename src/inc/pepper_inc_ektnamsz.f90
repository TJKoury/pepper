!> EK Table Name Size.
!>
!>## Version
!>
!>        ektnamsz.inc Version 1    17-JAN-1995 (NJB)
module pepper_inc_ektnamsz

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: tnamsz = 64  !! Size of table name, in characters.

end module pepper_inc_ektnamsz
