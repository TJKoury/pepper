!> Declare ZZOCCED return code parameters, comparison strings and other parameters.
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
!>-    SPICELIB Version 1.0.0, 01-SEP-2005 (NJB)
module pepper_inc_zzocced

   use pepper_kinds, only: ip

   implicit none

   public

   ! The function returns an integer code indicating the geometric
   ! relationship of the three bodies.

   integer(ip), parameter :: total1 = -3  !! Total occultation of first target by second.

   integer(ip), parameter :: annlr1 = -2  !! Annular occultation of first target by
                                          !! second.  The second target does not
                                          !! block the limb of the first.

   integer(ip), parameter :: partl1 = -1  !! Partial occultation of first target by
                                          !! second target.

   integer(ip), parameter :: noocc = 0  !! No occultation or transit:  both objects
                                        !! are completely visible to the observer.

   integer(ip), parameter :: partl2 = 1  !! Partial occultation of second target by
                                         !! first target.

   integer(ip), parameter :: annlr2 = 2  !! Annular occultation of second target by
                                         !! first.

   integer(ip), parameter :: total2 = 3  !! Total occultation of second target by
                                         !! first.

end module pepper_inc_zzocced
