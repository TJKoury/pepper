!> EK Column Class 9 Parameters.
!>
!>     The following parameters give the offsets of items in the
!>     class 9 integer metadata array.
!>
!>## Version
!>
!>        ekclas08.inc  Version 1    07-NOV-1995 (NJB)
module pepper_inc_ekclas09

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: dbix09 = 1  !! Data array base address:

   integer(ip), parameter :: nfix09 = 2  !! Null flag array base address:

   integer(ip), parameter :: mdsz09 = 2  !! Size of class 9 metadata array:

end module pepper_inc_ekclas09
