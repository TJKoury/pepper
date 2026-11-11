!> EK Architecture Version Parameters.
!>
!>     The following parameter indicates the EK file architecture
!>     version.  EK files read by the EK system must have the
!>     architecture expected by the reader software; the architecture ID
!>     below is used to test for compatibility.
!>
!>## Version
!>
!>        ekarch.inc  Version 1    01-NOV-1995 (NJB)
module pepper_inc_ekarch

   use pepper_kinds, only: ip

   implicit none

   public

!     Architecture code:
!
   integer(ip), parameter :: archid = 8

end module pepper_inc_ekarch
