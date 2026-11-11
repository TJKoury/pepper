!> EK Column Class 8 Parameters.
!>
!>     The following parameters give the offsets of items in the
!>     class 8 integer metadata array.
!>
!>## Version
!>
!>        ekclas08.inc  Version 1    07-NOV-1995 (NJB)
module pepper_inc_ekclas08

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: dbix08 = 1  !! Data array base address:

   integer(ip), parameter :: nfix08 = 2  !! Null flag array base address:

   integer(ip), parameter :: mdsz08 = 2  !! Size of class 8 metadata array:

end module pepper_inc_ekclas08
