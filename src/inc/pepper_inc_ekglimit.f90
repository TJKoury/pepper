!> The parameters of SPICELIB's include file ekglimit.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekglimit

   use pepper_kinds, only: ip

   implicit none

   public

!     Include Section:  EK General Limit Parameters
!
!        ekglimit.inc  Version 1    21-MAY-1995 (NJB)
!
!
!     This file contains general limits for the EK system.
!
!     MXCLSG is the maximum number of columns allowed in a segment.
!     This limit applies to logical tables as well, since all segments
!     in a logical table must have the same column definitions.
!
   integer(ip), parameter :: mxclsg = 100

!
!     End Include Section:  EK General Limit Parameters
!

end module pepper_inc_ekglimit
