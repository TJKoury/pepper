!> EK Data Types.
!>
!>     Within the EK system, data types of EK column contents are
!>     represented by integer codes.  The codes and their meanings
!>     are listed below.
!>
!>     Integer codes are also used within the DAS system to indicate
!>     data types; the EK system makes no assumptions about compatibility
!>     between the codes used here and those used in the DAS system.
!>
!>## Version
!>
!>        ektype.inc Version 1  27-DEC-1994 (NJB)
module pepper_inc_ektype

   use pepper_kinds, only: ip

   implicit none

   public

!     Character type:
!
   integer(ip), parameter :: chr = 1

!
!     Double precision type:
!
   integer(ip), parameter :: dp = 2

!
!     Integer type:
!
   integer(ip), parameter :: int = 3

!
!     `Time' type:
!
!     Within the EK system, time values are represented as ephemeris
!     seconds past J2000 (TDB), and double precision numbers are used
!     to store these values.  However, since time values require special
!     treatment both on input and output, and since the `TIME' column
!     has a special role in the EK specification and code, time values
!     are identified as a type distinct from double precision numbers.
!
   integer(ip), parameter :: time = 4

end module pepper_inc_ektype
