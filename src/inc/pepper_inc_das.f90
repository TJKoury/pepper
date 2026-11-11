!> This file contains public, global parameter declarations for the SPICELIB Direct Access Segregated (DAS) subsystem.
!>
!>## Restrictions
!>
!>     None.
!>
!>## Author and Institution
!>
!>     N.J. Bachman       (JPL)
!>     J. Diaz del Rio    (ODC Space)
!>
!>## Version
!>
!>-    SPICELIB Version 1.1.0, 07-APR-2020 (JDR)
!>
!>        Added CHARDT, DPDT and INTDT parameters.
!>
!>-    SPICELIB Version 1.0.0, 10-FEB-2017 (NJB)
module pepper_inc_das

   use pepper_kinds, only: ip

   implicit none

   public

!
!     Parameter declarations follow.
!

!
!     DAS file table size:
!
!        The parameter name is FTSIZE. The value of the parameter is
!        defined in the include file
!
!           zzddhman.inc
!
!        That value is duplicated here, since zzddhman.inc contains
!        other declarations that conflict with some of those in DAS
!        routines.
!
   integer(ip), parameter :: ftsize = 5000

!
!     Capacity of DAS data records:
!
!        -- NWD double precision numbers.
!        -- NWI integers.
!        -- NWC characters.
!
!     These parameters are named to enhance ease of maintenance of
!     the code; the values should not be changed.

   integer(ip), parameter :: nwd = 128

   integer(ip), parameter :: nwi = 256

   integer(ip), parameter :: nwc = 1024

!
!     DAS data type specifiers used in all DAS routines that require
!     a data type either as input or to extract data from an output
!     array.
!
!     CHARDT,
!     DPDT,
!     INTDT    are data type specifiers which indicate CHARACTER,
!              DOUBLE PRECISION, and INTEGER respectively. These
!              parameters are used in all DAS routines that require a
!              data type specifier.
!
   integer(ip), parameter :: chardt = 1

   integer(ip), parameter :: dpdt = 2

   integer(ip), parameter :: intdt = 3

end module pepper_inc_das
