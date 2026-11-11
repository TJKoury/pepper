!> Private parameters of the DSK subsystem.
!>
!>## Version
!>
!>     Version 4.0.0 13-NOV-2015 (NJB)
!>
!>        Changed parameter LBTLEN to CVTLEN.
!>        Added parameter LMBCRV.
!>
!>     Version 3.0.0 05-NOV-2015 (NJB)
!>
!>        Added parameters
!>
!>           CTRCOR
!>           ELLCOR
!>           GUIDED
!>           LBTLEN
!>           PNMBRL
!>           TANGNT
!>           TMTLEN
!>           UMBRAL
!>
!>     Version 2.0.0 04-MAR-2015 (NJB)
!>
!>        Removed declaration of parameter SHPLEN.
!>        This name is already in use in the include
!>        file gf.inc.
!>
!>     Version 1.0.0 26-JAN-2015 (NJB)
module pepper_inc_zzdsk

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: dskshp = 2  !! Parameters supporting METHOD string parsing:

   integer(ip), parameter :: ellshp = 1

   integer(ip), parameter :: mthlen = 500  !! Local method length.

   integer(ip), parameter :: sublen = 20  !! Length of sub-point type string.

   integer(ip), parameter :: cvtlen = 20  !! Length of curve type string.

   integer(ip), parameter :: tangnt = 1  !! Limb type parameter codes.

   integer(ip), parameter :: guided = 2

   integer(ip), parameter :: tmtlen = 20  !! Length of terminator type string.

   integer(ip), parameter :: lmbcrv = 0  !! Terminator type and limb parameter codes.

   integer(ip), parameter :: umbral = 1

   integer(ip), parameter :: pnmbrl = 2

   integer(ip), parameter :: acllen = 25  !! Length of aberration correction locus string.

   integer(ip), parameter :: ctrcor = 1  !! Aberration correction locus codes.

   integer(ip), parameter :: ellcor = 2

end module pepper_inc_zzdsk
