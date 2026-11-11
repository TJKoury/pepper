!> This file contains the number of inertial reference frames that are currently known by the SPICE toolkit software.
!>
!>## Parameters
!>
!>     NINERT     is the number of recognized inertial reference
!>                frames.  This value is needed by both CHGIRF
!>                ZZFDAT, and FRAMEX.
!>
!>## Author and Institution
!>
!>     W.L. Taber      (JPL)
!>
!>## Literature References
!>
!>     None.
!>
!>## Version
!>
!>-    SPICELIB Version 1.0.0, 10-OCT-1996 (WLT)
module pepper_inc_ninert

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: ninert = 21

end module pepper_inc_ninert
