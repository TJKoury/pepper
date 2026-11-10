!> The parameters of SPICELIB's include file spk18.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_spk18

   implicit none

   public

! $ Abstract
!
!     Declare parameters specific to SPK type 18.
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
!-    SPICELIB Version 1.0.0, 18-AUG-2002 (NJB)
!

!
!     SPK type 18 subtype codes:
!
!
!     Subtype 0:  Hermite interpolation, 12-element packets, order
!                 reduction at boundaries to preceding number
!                 equivalent to 3 mod 4.
!
   integer               s18tp0
   parameter           ( s18tp0 = 0 )

!
!     Subtype 1:  Lagrange interpolation, 6-element packets, order
!                 reduction at boundaries to preceding odd number.
!
   integer               s18tp1
   parameter           ( s18tp1 = s18tp0 + 1 )

!
!     Packet sizes associated with the various subtypes:
!
   integer               s18ps0
   parameter           ( s18ps0 = 12 )

   integer               s18ps1
   parameter           ( s18ps1 =  6 )

!
!     End of include file spk18.inc.
!

end module pepper_inc_spk18
