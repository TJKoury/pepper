!> Declare private surface name/ID mapping parameters.
!>
!> SPICE private include file intended solely for the support of SPICE routines. User software should not include this file due to the volatile nature of this file.
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
!>-    SPICELIB Version 1.0.0, 04-FEB-2017 (NJB)
!>
!>        Original version 03-DEC-2015 (NJB)
module pepper_inc_zzsrftrn

   use pepper_kinds, only: ip

   implicit none

   public

!
!     Size of the lists and hashes storing the POOL-defined name/ID
!     mappings. To ensure efficient hashing, this size is set to the
!     first prime number greater than MXNSRF defined in the public
!     include file
!
!        srftrn.inc.
!
   integer(ip), parameter :: nroom = 2003

!
!     Singly-linked list pool lower bound:
!
   integer(ip), parameter :: lbsngl = -5

end module pepper_inc_zzsrftrn
