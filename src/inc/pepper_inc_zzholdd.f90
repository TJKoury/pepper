!> This file contains parameter declarations for the ZZHOLDD routine.
!>
!> SPICE private routine intended solely for the support of SPICE routines. Users should not call this routine directly due to the volatile nature of this routine.
!>
!>## Brief I/O
!>
!>     None.
!>
!>## Detailed Input
!>
!>     None.
!>
!>## Detailed Output
!>
!>     None.
!>
!>## Parameters
!>
!>     GEN       general value, primarily for testing.
!>
!>     GF_REF    user defined GF reference value.
!>
!>     GF_TOL    user defined GF convergence tolerance.
!>
!>     GF_DT     user defined GF step for numeric differentiation.
!>
!>## Exceptions
!>
!>     None.
!>
!>## Files
!>
!>     None.
!>
!>## Particulars
!>
!>     None.
!>
!>## Examples
!>
!>     None.
!>
!>## Restrictions
!>
!>     None.
!>
!>## Literature References
!>
!>     None.
!>
!>## Author and Institution
!>
!>     E.D. Wright    (JPL)
!>
!>## Version
!>
!>-    SPICELIB Version 1.0.0  03-DEC-2013 (EDW)
module pepper_inc_zzholdd

   use pepper_kinds, only: ip

   implicit none

   public

!
!     OP codes. The values exist in the integer domain
!     [ -ZZNOP, -1],
!
   integer(ip), parameter :: zzget = -1

   integer(ip), parameter :: zzput = -2

   integer(ip), parameter :: zzreset = -3

!
!     Current number of OP codes.
!
   integer(ip), parameter :: zznop = 3

!
!     ID codes. The values exist in the integer domain
!     [ 1, NID],
!

!
!     General use, primarily testing.
!
   integer(ip), parameter :: gen = 1

!
!     The user defined GF reference value.
!
   integer(ip), parameter :: gf_ref = 2

!
!     The user defined GF convergence tolerance.
!
   integer(ip), parameter :: gf_tol = 3

!
!     The user defined GF step for numeric differentiation.
!
   integer(ip), parameter :: gf_dt = 4

!
!     Current number of ID codes, dimension of array
!     in ZZHOLDD. Bad things can happen if this parameter
!     does not have the proper value.
!
   integer(ip), parameter :: nid = 4

end module pepper_inc_zzholdd
