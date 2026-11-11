!> This include file lists the parameter collection defining the number of SPICE ID -> NAME mappings.
!>
!>## Parameters
!>
!>     MAXL        is the maximum length of a body name.
!>
!>     MAXP        is the maximum number of additional names that may
!>                 be added via the ZZBODDEF interface.
!>
!>     NPERM       is the count of the mapping assignments built into
!>                 SPICE.
!>
!>     MAXE        is the size of the lists and hashes storing combined
!>                 built-in and ZZBODDEF-defined name/ID mappings. To
!>                 ensure efficient hashing this size is the set to the
!>                 first prime number greater than ( MAXP + NPERM ).
!>
!>     NROOM       is the size of the lists and hashes storing the
!>                 POOL-defined name/ID mappings. To ensure efficient
!>                 hashing and to provide the ability to store nearly as
!>                 many names as can fit in the POOL, this size is
!>                 set to the first prime number less than MAXLIN
!>                 defined in the POOL umbrella routine.
!>
!>## Author and Institution
!>
!>     B.V. Semenov (JPL)
!>     E.D. Wright  (JPL)
!>
!>## Version
!>
!>-    SPICELIB Version 2.0.0, 10-DEC-2021 (BVS)(EDW)
!>
!>        Increased NROOM to 14983. Added a comment note explaining
!>        NROOM and MAXE
!>
!>-    SPICELIB Version 1.0.0, 20-MAY-2010 (EDW)
!>
!>        N0064 version with MAXP = 150, NPERM = 563,
!>        MAXE = MAXP + NPERM, and NROOM = 2000.
module pepper_inc_zzbodtrn

   use pepper_kinds, only: ip

   implicit none

   public

!
!     A script generates this file. Do not edit by hand.
!     Edit the creation script to modify the contents of
!     ZZBODTRN.INC.
!

!
!     Maximum size of a NAME string
!
   integer(ip), parameter :: maxl = 36

!
!     Maximum number of additional names that may be added via the
!     ZZBODDEF interface.
!
   integer(ip), parameter :: maxp = 150

!
!     Count of default SPICE mapping assignments.
!
   integer(ip), parameter :: nperm = 692

!
!     Size of the lists and hashes storing the built-in and
!     ZZBODDEF-defined name/ID mappings. To ensure efficient hashing
!     this size is the set to the first prime number greater than
!     ( MAXP + NPERM ).
!
   integer(ip), parameter :: maxe = 853

!
!     Size of the lists and hashes storing the POOL-defined name/ID
!     mappings. To ensure efficient hashing and to provide the ability
!     to store nearly as many names as can fit in the POOL, this size
!     is set to the first prime number less than MAXLIN defined in
!     the POOL umbrella routine.
!
   integer(ip), parameter :: nroom = 14983

end module pepper_inc_zzbodtrn
