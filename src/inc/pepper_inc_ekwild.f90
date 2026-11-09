!> The parameters of SPICELIB's include file ekwild.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekwild

   implicit none

   public

!     Include Section:  EK Template Matching Wild Characters
!
!
!        ekwild.inc  Version 1   16-JAN-1995 (NJB)
!
!
!     Within the EK system, templates used for pattern matching
!     are those accepted by the SPICELIB routine MATCHW.  MATCHW
!     accepts two special characters:  one representing wild
!     strings and one representing wild characters.  This include
!     file defines those special characters for use within the EK
!     system.
!
!
!     Wild string symbol:  this character matches any string.
!
   character(1), parameter :: wstr = '*'

!
!     Wild character symbol:  this character matches any character.
!
   character(1), parameter :: wchr = '%'

!
!     End Include Section:  EK Template Matching Wild Characters
!

end module pepper_inc_ekwild
