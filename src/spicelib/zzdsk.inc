
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
      INTEGER               DSKSHP
      PARAMETER           ( DSKSHP = 2 )

      INTEGER               ELLSHP
      PARAMETER           ( ELLSHP = 1 )

!
!     Local method length.
!
      INTEGER               MTHLEN
      PARAMETER           ( MTHLEN = 500 )

!
!     Length of sub-point type string.
!
      INTEGER               SUBLEN
      PARAMETER           ( SUBLEN = 20 )

!
!     Length of curve type string.
!
      INTEGER               CVTLEN
      PARAMETER           ( CVTLEN = 20 )

!
!     Limb type parameter codes.
!
      INTEGER               TANGNT
      PARAMETER           ( TANGNT = 1 )

      INTEGER               GUIDED
      PARAMETER           ( GUIDED = 2 )

!
!     Length of terminator type string.
!
      INTEGER               TMTLEN
      PARAMETER           ( TMTLEN = 20 )

!
!     Terminator type and limb parameter codes.
!
      INTEGER               LMBCRV
      PARAMETER           ( LMBCRV = 0 )

      INTEGER               UMBRAL
      PARAMETER           ( UMBRAL = 1 )

      INTEGER               PNMBRL
      PARAMETER           ( PNMBRL = 2 )

!
!     Length of aberration correction locus string.
!
      INTEGER               ACLLEN
      PARAMETER           ( ACLLEN = 25 )

!
!     Aberration correction locus codes.
!
      INTEGER               CTRCOR
      PARAMETER           ( CTRCOR = 1 )

      INTEGER               ELLCOR
      PARAMETER           ( ELLCOR = 2 )

!
!     End of include file zzdsk.inc
!
