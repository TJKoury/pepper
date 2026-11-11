!> The parameters of SPICELIB's include file srftrn.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_srftrn

   use pepper_kinds, only: ip

   implicit none

   public

! $ Abstract
!
!     Declare public surface name/ID mapping parameters.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 02-DEC-2015 (NJB)
!

!
!     Maximum number of surface name/ID mapping entries:
!
   integer(ip), parameter :: mxnsrf = 2000

!
!     Maximum length of a surface name string:
!
   integer(ip), parameter :: sfnmln = 36

!
!     End of file srftrn.inc.
!

end module pepper_inc_srftrn
