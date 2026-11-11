!> This file contains public, global parameter declarations for the SPICELIB occultation routines.
!>
!>## Restrictions
!>
!>     None.
!>
!>## Author and Institution
!>
!>     S.C. Krening      (JPL)
!>     N.J. Bachman      (JPL)
!>
!>## Literature References
!>
!>     None.
!>
!>## Version
!>
!>-    SPICELIB Version 1.0.0, 24-JAN-2012 (SCK) (NJB)
module pepper_inc_occult

   use pepper_kinds, only: ip

   implicit none

   public

!
!     The following integer codes indicate the geometric relationship
!     of the three bodies.
!
!     The meaning of the sign of each code is given below.
!
!                    Code sign          Meaning
!                    ---------          ------------------------------
!                       > 0             The second ellipsoid is
!                                       partially or fully occulted
!                                       by the first.
!
!                       < 0             The first ellipsoid is
!                                       partially of fully
!                                       occulted by the second.
!
!                       = 0             No occultation.
!
!     The meanings of the codes are given below. The variable names
!     indicate the type of occultation and which target is in the back.
!     For example, TOTAL1 represents a total occultation in which the
!     first target is in the back (or occulted by) the second target.
!
!                    Name      Code     Meaning
!                    ------    -----    ------------------------------
!                    TOTAL1     -3      Total occultation of first
!                                       target by second.
!
!                    ANNLR1     -2      Annular occultation of first
!                                       target by second.  The second
!                                       target does not block the limb
!                                       of the first.
!
!                    PARTL1     -1      Partial occultation of first
!                                       target by second target.
!
!                    NOOCC       0      No occultation or transit:  both
!                                       objects are completely visible
!                                       to the observer.
!
!                    PARTL2      1      Partial occultation of second
!                                       target by first target.
!
!                    ANNLR2      2      Annular occultation of second
!                                       target by first.
!
!                    TOTAL2      3      Total occultation of second
!                                       target by first.
!

   integer(ip), parameter :: total1 = -3

   integer(ip), parameter :: annlr1 = -2

   integer(ip), parameter :: partl1 = -1

   integer(ip), parameter :: noocc = 0

   integer(ip), parameter :: partl2 = 1

   integer(ip), parameter :: annlr2 = 2

   integer(ip), parameter :: total2 = 3

end module pepper_inc_occult
