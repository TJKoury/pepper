!> EK Boolean Enumerated Type.
!>
!>     Within the EK system, boolean values sometimes must be
!>     represented by integer or character codes.  The codes and their
!>     meanings are listed below.
!>
!>## Version
!>
!>        ekbool.inc Version 1   21-DEC-1994 (NJB)
module pepper_inc_ekbool

   use pepper_kinds, only: ip

   implicit none

   public

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

end module pepper_inc_ekbool
