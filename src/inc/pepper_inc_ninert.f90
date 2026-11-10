!> The parameters of SPICELIB's include file ninert.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ninert

   implicit none

   public

! $ Abstract
!
!     This file contains the number of inertial reference
!     frames that are currently known by the SPICE toolkit
!     software.
!
   integer               ninert
   parameter           ( ninert = 21 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NINERT     P   Number of known inertial reference frames.
!
! $ Parameters
!
!     NINERT     is the number of recognized inertial reference
!                frames.  This value is needed by both CHGIRF
!                ZZFDAT, and FRAMEX.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 10-OCT-1996 (WLT)
!

end module pepper_inc_ninert
