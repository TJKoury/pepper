!> Parameter assignments for SGP4 algorithm as expressed by Vallado [2].
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
!>     None.
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
!>     J2    = GEOPHS(K_J2)
!>     J3    = GEOPHS(K_J3)
!>     J4    = GEOPHS(K_J4)
!>     ER    = GEOPHS(K_ER)
!>     XKE   = GEOPHS(K_KE)
!>
!>     TUMIN = 1.D0/XKE
!>     J3OJ2 = J3/J2
!>
!>## Restrictions
!>
!>     None.
!>
!>## Literature References
!>
!>   [1] Hoots, F. R., and Roehrich, R. L. 1980. "Models for
!>       Propagation of the NORAD Element Sets." Spacetrack Report #3.
!>       U.S. Air Force: Aerospace Defense Command.
!>
!>   [2] Vallado, David, Crawford, Paul, Hujsak, Richard, and Kelso, T.S.
!>       2006. Revisiting Spacetrack Report #3. Paper AIAA 2006-6753
!>       presented at the AIAA/AAS Astrodynamics Specialist Conference,
!>       August 21-24, 2006. Keystone, CO.
!>
!>## Author and Institution
!>
!>     E. D. Wright    (JPL)
!>
!>## Version
!>
!>-    SPICELIB Version 1.0.0, MAY-27-2020 (EDW)
!>
!>        Updated descriptions of GEOPHS constants to be consistent
!>        with what's used in other routines.
!>
!>-    SPICELIB Version 1.0.0 22-JUL-2014 (EDW)
module pepper_inc_zzsgp4

   use pepper_kinds, only: ip

   implicit none

   public

!
!      WGS gravitational constants IDs.
!

   integer(ip), parameter :: ngravs = 3

   integer(ip), parameter :: ngravc = 8

   integer(ip), parameter :: wgs721 = 1

   integer(ip), parameter :: wgs72 = 2

   integer(ip), parameter :: wgs84 = 3

!
!      Gravitational constant indices.
!

   integer(ip), parameter :: p_rad = 1

   integer(ip), parameter :: p_xke = 2

   integer(ip), parameter :: p_mu = 3

   integer(ip), parameter :: p_tumn = 4

   integer(ip), parameter :: p_j2 = 5

   integer(ip), parameter :: p_j3 = 6

   integer(ip), parameter :: p_j4 = 7

   integer(ip), parameter :: p_j3j2 = 8

!
!     The following parameters give the indices in the GEOPHS
!     array of the various geophysical parameters needed for
!     the two line element sets.
!
!     K_J2  --- index of J2 gravitational harmonic for earth
!     K_J3  --- index of J3 gravitational harmonic for earth
!     K_J4  --- index of J4 gravitational harmonic for earth
!     K_KE  --- index of KE = sqrt(GM) in earth-radii**1.5/MIN
!     K_QO  --- index of high altitude bound for atmospheric
!               model in km
!     K_SO  --- index of low altitude bound for atmospheric
!               model in km
!     K_ER  --- index of earth equatorial radius in km
!     K_AE  --- index of distance units/earth radius
!

   integer(ip), parameter :: k_j2 = 1

   integer(ip), parameter :: k_j3 = 2

   integer(ip), parameter :: k_j4 = 3

   integer(ip), parameter :: k_ke = 4

   integer(ip), parameter :: k_qo = 5

   integer(ip), parameter :: k_so = 6

   integer(ip), parameter :: k_er = 7

   integer(ip), parameter :: k_ae = 8

   integer(ip), parameter :: ngeo = k_ae

!
!     Operation mode values, OPMODE.
!

   integer(ip), parameter :: afspc = 1

   integer(ip), parameter :: imprvd = 2

!
!     An enumeration of the various components of the
!     elements array---ELEMS
!
!     KNDT20  --- location of NDT20
!     KNDD60  --- location of NDD60
!     KBSTAR  --- location of BSTAR
!     KINCL   --- location of INCL
!     KNODE0  --- location of NODE0
!     KECC    --- location of ECC
!     KOMEGA  --- location of OMEGA
!     KMO     --- location of MO
!     KNO     --- location of NO
!

   integer(ip), parameter :: kndt20 = 1

   integer(ip), parameter :: kndd60 = 2

   integer(ip), parameter :: kbstar = 3

   integer(ip), parameter :: kincl = 4

   integer(ip), parameter :: knode0 = 5

   integer(ip), parameter :: kecc = 6

   integer(ip), parameter :: komega = 7

   integer(ip), parameter :: kmo = 8

   integer(ip), parameter :: kno = 9

   integer(ip), parameter :: kepoch = 10

   integer(ip), parameter :: nelems = kepoch

end module pepper_inc_zzsgp4
