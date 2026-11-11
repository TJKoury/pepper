!> Parameters of the DSK subsystem.
!>
!>## Version
!>
!>     Version 1.0.0 05-FEB-2016 (NJB)
module pepper_inc_dsk

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: maxsrf = 100  !! Maximum size of surface ID list.

end module pepper_inc_dsk
