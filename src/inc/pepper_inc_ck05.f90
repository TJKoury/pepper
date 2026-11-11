!> Declare parameters specific to CK type 05.
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
!>-    SPICELIB Version 1.0.0, 20-AUG-2002 (NJB)
module pepper_inc_ck05

   use pepper_kinds, only: ip

   implicit none

   public

!
!     CK type 5 subtype codes:
!
!
!     Subtype 0:  Hermite interpolation, 8-element packets. Quaternion
!                 and quaternion derivatives only, no angular velocity
!                 vector provided. Quaternion elements are listed
!                 first, followed by derivatives. Angular velocity is
!                 derived from the quaternions and quaternion
!                 derivatives.
!
   integer(ip), parameter :: c05tp0 = 0

!
!     Subtype 1:  Lagrange interpolation, 4-element packets. Quaternion
!                 only. Angular velocity is derived by differentiating
!                 the interpolating polynomials.
!
   integer(ip), parameter :: c05tp1 = c05tp0 + 1

!
!     Subtype 2:  Hermite interpolation, 14-element packets.
!                 Quaternion and angular angular velocity vector, as
!                 well as derivatives of each, are provided. The
!                 quaternion comes first, then quaternion derivatives,
!                 then angular velocity and its derivatives.
!
   integer(ip), parameter :: c05tp2 = c05tp1 + 1

!
!     Subtype 3:  Lagrange interpolation, 7-element packets. Quaternion
!                 and angular velocity vector provided.  The quaternion
!                 comes first.
!
   integer(ip), parameter :: c05tp3 = c05tp2 + 1

!
!     Packet sizes associated with the various subtypes:
!
   integer(ip), parameter :: c05ps0 = 8

   integer(ip), parameter :: c05ps1 = 4

   integer(ip), parameter :: c05ps2 = 14

   integer(ip), parameter :: c05ps3 = 7

!
!     End of file ck05.inc.
!

end module pepper_inc_ck05
