!> The parameters of SPICELIB's include file ekclas09.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekclas09

   use pepper_kinds, only: ip

   implicit none

   public

!     Include Section:  EK Column Class 9 Parameters
!
!        ekclas08.inc  Version 1    07-NOV-1995 (NJB)
!
!
!     The following parameters give the offsets of items in the
!     class 9 integer metadata array.
!
!     Data array base address:
!
   integer(ip), parameter :: dbix09 = 1

!
!     Null flag array base address:
!
   integer(ip), parameter :: nfix09 = 2

!
!     Size of class 9 metadata array:
!
   integer(ip), parameter :: mdsz09 = 2

!
!     End Include Section:  EK Column Class 9 Parameters
!

end module pepper_inc_ekclas09
