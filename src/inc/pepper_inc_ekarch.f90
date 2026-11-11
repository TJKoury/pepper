!> The parameters of SPICELIB's include file ekarch.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekarch

   use pepper_kinds, only: ip

   implicit none

   public

!     Include Section:  EK Architecture Version Parameters
!
!        ekarch.inc  Version 1    01-NOV-1995 (NJB)
!
!
!     The following parameter indicates the EK file architecture
!     version.  EK files read by the EK system must have the
!     architecture expected by the reader software; the architecture ID
!     below is used to test for compatibility.
!
!     Architecture code:
!
   integer(ip), parameter :: archid = 8

!
!     End Include Section:  EK Architecture Version Parameters
!

end module pepper_inc_ekarch
