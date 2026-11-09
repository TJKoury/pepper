!> The parameters of SPICELIB's include file ekclas08.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekclas08

   implicit none

   public

!     Include Section:  EK Column Class 8 Parameters
!
!        ekclas08.inc  Version 1    07-NOV-1995 (NJB)
!
!
!     The following parameters give the offsets of items in the
!     class 8 integer metadata array.
!
!     Data array base address:
!
   integer               dbix08
   parameter           ( dbix08 = 1 )

!
!     Null flag array base address:
!
   integer               nfix08
   parameter           ( nfix08 = 2 )

!
!     Size of class 8 metadata array:
!
   integer               mdsz08
   parameter           ( mdsz08 = 2 )

!
!     End Include Section:  EK Column Class 8 Parameters
!

end module pepper_inc_ekclas08
