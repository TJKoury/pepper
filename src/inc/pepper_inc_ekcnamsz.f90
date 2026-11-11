!> The parameters of SPICELIB's include file ekcnamsz.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekcnamsz

   use pepper_kinds, only: ip

   implicit none

   public

!     Include Section:  EK Column Name Size
!
!        ekcnamsz.inc Version 1    17-JAN-1995 (NJB)
!
!
!     Size of column name, in characters.
!
   integer(ip), parameter :: cnamsz = 32

!
!     End Include Section:  EK Column Name Size
!

end module pepper_inc_ekcnamsz
