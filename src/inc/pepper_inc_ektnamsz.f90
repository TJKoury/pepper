!> The parameters of SPICELIB's include file ektnamsz.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ektnamsz

   use pepper_kinds, only: ip

   implicit none

   public

!     Include Section:  EK Table Name Size
!
!        ektnamsz.inc Version 1    17-JAN-1995 (NJB)
!
!
!     Size of table name, in characters.
!
   integer(ip), parameter :: tnamsz = 64

!
!     End Include Section:  EK Table Name Size
!

end module pepper_inc_ektnamsz
