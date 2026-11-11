!> EK Template Matching Wild Characters.
!>
!>     Within the EK system, templates used for pattern matching
!>     are those accepted by the SPICELIB routine MATCHW.  MATCHW
!>     accepts two special characters:  one representing wild
!>     strings and one representing wild characters.  This include
!>     file defines those special characters for use within the EK
!>     system.
!>
!>## Version
!>
!>        ekwild.inc  Version 1   16-JAN-1995 (NJB)
module pepper_inc_ekwild

   implicit none

   public

!     Wild string symbol:  this character matches any string.
!
   character(1), parameter :: wstr = '*'

!
!     Wild character symbol:  this character matches any character.
!
   character(1), parameter :: wchr = '%'

end module pepper_inc_ekwild
