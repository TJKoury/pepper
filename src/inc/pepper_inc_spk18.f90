!> Declare parameters specific to SPK type 18.
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
!>-    SPICELIB Version 1.0.0, 18-AUG-2002 (NJB)
module pepper_inc_spk18

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: s18tp0 = 0  !! SPK type 18 subtype codes:
                                         !!
                                         !! Subtype 0:  Hermite interpolation, 12-element packets, order
                                         !! reduction at boundaries to preceding number
                                         !! equivalent to 3 mod 4.

   integer(ip), parameter :: s18tp1 = s18tp0 + 1  !! Subtype 1:  Lagrange interpolation, 6-element packets, order
                                                  !! reduction at boundaries to preceding odd number.

   integer(ip), parameter :: s18ps0 = 12  !! Packet sizes associated with the various subtypes:

   integer(ip), parameter :: s18ps1 = 6

end module pepper_inc_spk18
