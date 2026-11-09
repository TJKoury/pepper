!> The parameters of SPICELIB's include file ekclas07.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekclas07

   implicit none

   public

!     Include Section:  EK Column Class 7 Parameters
!
!        ekclas07.inc  Version 1    07-NOV-1995 (NJB)
!
!
!     The following parameters give the offsets of items in the
!     class 7 integer metadata array.
!
!     Data array base address:
!
   integer               dbix07
   parameter           ( dbix07 = 1 )

!
!     Null flag array base address:
!
   integer               nfix07
   parameter           ( nfix07 = 2 )

!
!     Size of class 7 metadata array:
!
   integer               mdsz07
   parameter           ( mdsz07 = 2 )

!
!     End Include Section:  EK Column Class 7 Parameters
!

end module pepper_inc_ekclas07
