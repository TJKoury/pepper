!> The parameters of SPICELIB's include file zzswtchf.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_zzswtchf

   use pepper_kinds, only: ip

   implicit none

   public

! $ Abstract
!
!     Include file zzswtchf.inc
!
!     SPICE private file intended solely for the support of SPICE
!     routines. Users should not include this file directly due
!     to the volatile nature of this file
!
!     Define SPICE-private parameters related to switch frames.
!
! $ Parameters
!
!     The values of the parameters described below are provided for
!     informational purposes. The values of these parameters may be
!     increased in the future.
!
!
!     MAXFRM      is the maximum number of switch frame specifications
!                 that can be buffered concurrently by the switch frame
!                 subsystem.
!
!     MAXBAS      is the maximum number of base frame specifications
!                 that can be buffered concurrently by the switch frame
!                 subsystem. This limit applies to the total base frame
!                 count for all switch frames.
!
!     LBSNGL      is the lower bound of a singly linked list pool
!                 managed by the integer hash subsystem.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 03-DEC-2021 (NJB) (BVS)
!

   integer(ip), parameter :: maxfrm = 1013

   integer(ip), parameter :: maxbas = 15000

   integer(ip), parameter :: lbsngl = -5

!
!     End of INCLUDE file zzswtchf.inc
!

end module pepper_inc_zzswtchf
