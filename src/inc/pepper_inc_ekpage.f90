!> EK Das Paging Parameters.
!>
!>     The EK DAS paging system makes use of the integer portion
!>     of an EK file's DAS address space to store the few numbers
!>     required to describe the system's state.  The allocation
!>     of DAS integer addresses is shown below.
!>
!>## Version
!>
!>        ekpage.inc  Version 4    25-AUG-1995 (NJB)
!>
!>                       DAS integer array
!>
!>        +--------------------------------------------+
!>        |            EK architecture code            |  Address = 1
!>        +--------------------------------------------+
!>        |      Character page size (in DAS words)    |
!>        +--------------------------------------------+
!>        |        Character page base address         |
!>        +--------------------------------------------+
!>        |      Number of character pages in file     |
!>        +--------------------------------------------+
!>        |   Number of character pages on free list   |
!>        +--------------------------------------------+
!>        |      Character free list head pointer      |  Address = 6
!>        +--------------------------------------------+
!>        |                                            |  Addresses =
!>        |           Metadata for d.p. pages          |    7--11
!>        |                                            |
!>        +--------------------------------------------+
!>        |                                            |  Addresses =
!>        |         Metadata for integer pages         |    12--16
!>        |                                            |
!>        +--------------------------------------------+
!>                              .
!>                              .
!>                              .
!>        +--------------------------------------------+
!>        |                                            |  End Address =
!>        |                Unused space                |  integer page
!>        |                                            |  end
!>        +--------------------------------------------+
!>        |                                            |  Start Address =
!>        |             First integer page             |  integer page
!>        |                                            |  base
!>        +--------------------------------------------+
!>                              .
!>                              .
!>                              .
!>        +--------------------------------------------+
!>        |                                            |
!>        |              Last integer page             |
!>        |                                            |
!>        +--------------------------------------------+
module pepper_inc_ekpage

   use pepper_kinds, only: ip

   implicit none

   public

!     The following parameters indicate positions of elements in the
!     paging system metadata array:
!
!
   integer(ip), parameter :: eparch = 1

!
!     Number of metadata items per data type:
!
   integer(ip), parameter :: epnipt = 5

!
!     Character metadata indices:
!
   integer(ip), parameter :: eppszc = eparch + 1

   integer(ip), parameter :: epbasc = eppszc + 1

   integer(ip), parameter :: epnpc = epbasc + 1

   integer(ip), parameter :: epnfpc = epnpc + 1

   integer(ip), parameter :: epfpc = epnfpc + 1

!
!     Double precision metadata indices:
!
   integer(ip), parameter :: eppszd = eppszc + epnipt

   integer(ip), parameter :: epbasd = eppszd + 1

   integer(ip), parameter :: epnpd = epbasd + 1

   integer(ip), parameter :: epnfpd = epnpd + 1

   integer(ip), parameter :: epfpd = epnfpd + 1

!
!     Integer metadata indices:
!
   integer(ip), parameter :: eppszi = eppszd + epnipt

   integer(ip), parameter :: epbasi = eppszi + 1

   integer(ip), parameter :: epnpi = epbasi + 1

   integer(ip), parameter :: epnfpi = epnpi + 1

   integer(ip), parameter :: epfpi = epnfpi + 1

!
!     Size of metadata area:
!
   integer(ip), parameter :: epmdsz = 1 + 3*epnipt

!
!     Page sizes, in units of DAS words of the appropriate type:
!
   integer(ip), parameter :: pgsizc = 1024

   integer(ip), parameter :: pgsizd = 128

   integer(ip), parameter :: pgsizi = 256

!
!     Default page base addresses:
!
   integer(ip), parameter :: pgbasc = 0

   integer(ip), parameter :: pgbasd = 0

   integer(ip), parameter :: pgbasi = 256

end module pepper_inc_ekpage
