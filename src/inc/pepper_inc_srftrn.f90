!> Declare public surface name/ID mapping parameters.
!>
!>## Restrictions
!>
!>     None.
!>
!>## Author and Institution
!>
!>     N.J. Bachman      (JPL)
!>
!>## Literature References
!>
!>     None.
!>
!>## Version
!>
!>-    SPICELIB Version 1.0.0, 02-DEC-2015 (NJB)
module pepper_inc_srftrn

   use pepper_kinds, only: ip

   implicit none

   public

!
!     Maximum number of surface name/ID mapping entries:
!
   integer(ip), parameter :: mxnsrf = 2000

!
!     Maximum length of a surface name string:
!
   integer(ip), parameter :: sfnmln = 36

end module pepper_inc_srftrn
