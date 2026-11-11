!> The parameters of SPICELIB's include file ekbool.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekbool

   use pepper_kinds, only: ip

   implicit none

   public

!     Include Section:  EK Boolean Enumerated Type
!
!
!        ekbool.inc Version 1   21-DEC-1994 (NJB)
!
!
!     Within the EK system, boolean values sometimes must be
!     represented by integer or character codes.  The codes and their
!     meanings are listed below.
!
!     Integer code indicating `true':
!
   integer(ip), parameter :: itrue = 1

!
!     Integer code indicating `false':
!
   integer(ip), parameter :: ifalse = -1

!
!     Character code indicating `true':
!
   character(*), parameter :: ctrue = 'T'

!
!     Character code indicating `false':
!
   character(*), parameter :: cfalse = 'F'

!
!     End Include Section:  EK Boolean Enumerated Type
!

end module pepper_inc_ekbool
