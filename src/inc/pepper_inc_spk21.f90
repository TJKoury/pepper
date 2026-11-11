!> Declare parameters specific to SPK type 21.
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
!>-    SPICELIB Version 1.0.0, 25-DEC-2013 (NJB)
module pepper_inc_spk21

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: maxtrm = 25  !! MAXTRM is the maximum number of terms allowed in each
                                          !! component of the difference table contained in a type
                                          !! 21 SPK difference line. MAXTRM replaces the fixed
                                          !! table parameter value of 15 used in SPK type 1
                                          !! segments.
                                          !!
                                          !! Type 21 segments have variable size. Let MAXDIM be
                                          !! the dimension of each component of the difference
                                          !! table within each difference line. Then the size
                                          !! DLSIZE of the difference line is
                                          !!
                                          !!     ( 4 * MAXDIM ) + 11
                                          !!
                                          !! MAXTRM is the largest allowed value of MAXDIM.

end module pepper_inc_spk21
