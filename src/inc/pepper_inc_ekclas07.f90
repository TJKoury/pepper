!> EK Column Class 7 Parameters.
!>
!>     The following parameters give the offsets of items in the
!>     class 7 integer metadata array.
!>
!>## Version
!>
!>        ekclas07.inc  Version 1    07-NOV-1995 (NJB)
module pepper_inc_ekclas07

   use pepper_kinds, only: ip

   implicit none

   public

!     Data array base address:
!
   integer(ip), parameter :: dbix07 = 1

!
!     Null flag array base address:
!
   integer(ip), parameter :: nfix07 = 2

!
!     Size of class 7 metadata array:
!
   integer(ip), parameter :: mdsz07 = 2

end module pepper_inc_ekclas07
