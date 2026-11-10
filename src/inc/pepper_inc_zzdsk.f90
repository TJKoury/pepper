!> The parameters of SPICELIB's include file zzdsk.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_zzdsk

   implicit none

   public

!
!     File: zzdsk.inc
!
!
!     Version 4.0.0 13-NOV-2015 (NJB)
!
!        Changed parameter LBTLEN to CVTLEN.
!        Added parameter LMBCRV.
!
!     Version 3.0.0 05-NOV-2015 (NJB)
!
!        Added parameters
!
!           CTRCOR
!           ELLCOR
!           GUIDED
!           LBTLEN
!           PNMBRL
!           TANGNT
!           TMTLEN
!           UMBRAL
!
!     Version 2.0.0 04-MAR-2015 (NJB)
!
!        Removed declaration of parameter SHPLEN.
!        This name is already in use in the include
!        file gf.inc.
!
!     Version 1.0.0 26-JAN-2015 (NJB)
!

!
!     Parameters supporting METHOD string parsing:
!
   integer               dskshp
   parameter           ( dskshp = 2 )

   integer               ellshp
   parameter           ( ellshp = 1 )

!
!     Local method length.
!
   integer               mthlen
   parameter           ( mthlen = 500 )

!
!     Length of sub-point type string.
!
   integer               sublen
   parameter           ( sublen = 20 )

!
!     Length of curve type string.
!
   integer               cvtlen
   parameter           ( cvtlen = 20 )

!
!     Limb type parameter codes.
!
   integer               tangnt
   parameter           ( tangnt = 1 )

   integer               guided
   parameter           ( guided = 2 )

!
!     Length of terminator type string.
!
   integer               tmtlen
   parameter           ( tmtlen = 20 )

!
!     Terminator type and limb parameter codes.
!
   integer               lmbcrv
   parameter           ( lmbcrv = 0 )

   integer               umbral
   parameter           ( umbral = 1 )

   integer               pnmbrl
   parameter           ( pnmbrl = 2 )

!
!     Length of aberration correction locus string.
!
   integer               acllen
   parameter           ( acllen = 25 )

!
!     Aberration correction locus codes.
!
   integer               ctrcor
   parameter           ( ctrcor = 1 )

   integer               ellcor
   parameter           ( ellcor = 2 )

!
!     End of include file zzdsk.inc
!

end module pepper_inc_zzdsk
