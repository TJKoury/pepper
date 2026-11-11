!> The parameters of SPICELIB's include file errhnd.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_errhnd

   use pepper_kinds, only: ip

   implicit none

   public

!     Include File:  SPICELIB Error Handling Parameters
!
!        errhnd.inc  Version 2    18-JUN-1997 (WLT)
!
!           The size of the long error message was
!           reduced from 25*80 to 23*80 so that it
!           will be accepted by the Microsoft Power Station
!           FORTRAN compiler which has an upper bound
!           of 1900 for the length of a character string.
!
!        errhnd.inc  Version 1    29-JUL-1997 (NJB)
!
!
!
!     Maximum length of the long error message:
!
   integer(ip), parameter :: lmsgln = 23 * 80

!
!     Maximum length of the short error message:
!
   integer(ip), parameter :: smsgln = 25

!
!     End Include File:  SPICELIB Error Handling Parameters
!

end module pepper_inc_errhnd
