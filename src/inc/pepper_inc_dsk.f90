!> The parameters of SPICELIB's include file dsk.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_dsk

   implicit none

   public

!
!     File: dsk.inc
!
!
!     Version 1.0.0 05-FEB-2016 (NJB)

!
!     Maximum size of surface ID list.
!
   integer               maxsrf
   parameter           ( maxsrf = 100 )

!
!     End of include file dsk.inc
!

end module pepper_inc_dsk
