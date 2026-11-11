!> This include file defines the dimension of the counter array used by various SPICE subsystems to uniquely identify changes in their states.
!>
!>## Parameters
!>
!>     CTRSIZ      is the dimension of the counter array used by
!>                 various SPICE subsystems to uniquely identify
!>                 changes in their states.
!>
!>## Author and Institution
!>
!>     B.V. Semenov    (JPL)
!>
!>## Literature References
!>
!>     None.
!>
!>## Version
!>
!>-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
module pepper_inc_zzctr

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: ctrsiz = 2

end module pepper_inc_zzctr
