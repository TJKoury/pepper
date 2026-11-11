!> EK General Limit Parameters.
!>
!>     This file contains general limits for the EK system.
!>
!>## Version
!>
!>        ekglimit.inc  Version 1    21-MAY-1995 (NJB)
module pepper_inc_ekglimit

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: mxclsg = 100  !! MXCLSG is the maximum number of columns allowed in a segment.
                                           !! This limit applies to logical tables as well, since all segments
                                           !! in a logical table must have the same column definitions.

end module pepper_inc_ekglimit
