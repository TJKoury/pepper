!     Include Section:  EK Das Paging Parameters
!
!        ekpage.inc  Version 4    25-AUG-1995 (NJB)
!
!
!
!     The EK DAS paging system makes use of the integer portion
!     of an EK file's DAS address space to store the few numbers
!     required to describe the system's state.  The allocation
!     of DAS integer addresses is shown below.
!
!
!                       DAS integer array
!
!        +--------------------------------------------+
!        |            EK architecture code            |  Address = 1
!        +--------------------------------------------+
!        |      Character page size (in DAS words)    |
!        +--------------------------------------------+
!        |        Character page base address         |
!        +--------------------------------------------+
!        |      Number of character pages in file     |
!        +--------------------------------------------+
!        |   Number of character pages on free list   |
!        +--------------------------------------------+
!        |      Character free list head pointer      |  Address = 6
!        +--------------------------------------------+
!        |                                            |  Addresses =
!        |           Metadata for d.p. pages          |    7--11
!        |                                            |
!        +--------------------------------------------+
!        |                                            |  Addresses =
!        |         Metadata for integer pages         |    12--16
!        |                                            |
!        +--------------------------------------------+
!                              .
!                              .
!                              .
!        +--------------------------------------------+
!        |                                            |  End Address =
!        |                Unused space                |  integer page
!        |                                            |  end
!        +--------------------------------------------+
!        |                                            |  Start Address =
!        |             First integer page             |  integer page
!        |                                            |  base
!        +--------------------------------------------+
!                              .
!                              .
!                              .
!        +--------------------------------------------+
!        |                                            |
!        |              Last integer page             |
!        |                                            |
!        +--------------------------------------------+
!
!     The following parameters indicate positions of elements in the
!     paging system metadata array:
!
!
      INTEGER               EPARCH
      PARAMETER           ( EPARCH = 1 )

!
!     Number of metadata items per data type:
!
      INTEGER               EPNIPT
      PARAMETER           ( EPNIPT = 5 )

!
!     Character metadata indices:
!
      INTEGER               EPPSZC
      PARAMETER           ( EPPSZC = EPARCH + 1 )

      INTEGER               EPBASC
      PARAMETER           ( EPBASC = EPPSZC + 1 )

      INTEGER               EPNPC
      PARAMETER           ( EPNPC  = EPBASC + 1 )

      INTEGER               EPNFPC
      PARAMETER           ( EPNFPC = EPNPC  + 1 )

      INTEGER               EPFPC
      PARAMETER           ( EPFPC  = EPNFPC + 1 )

!
!     Double precision metadata indices:
!
      INTEGER               EPPSZD
      PARAMETER           ( EPPSZD = EPPSZC + EPNIPT )

      INTEGER               EPBASD
      PARAMETER           ( EPBASD = EPPSZD + 1 )

      INTEGER               EPNPD
      PARAMETER           ( EPNPD  = EPBASD + 1 )

      INTEGER               EPNFPD
      PARAMETER           ( EPNFPD = EPNPD  + 1 )

      INTEGER               EPFPD
      PARAMETER           ( EPFPD  = EPNFPD + 1 )

!
!     Integer metadata indices:
!
      INTEGER               EPPSZI
      PARAMETER           ( EPPSZI = EPPSZD + EPNIPT )

      INTEGER               EPBASI
      PARAMETER           ( EPBASI = EPPSZI + 1 )

      INTEGER               EPNPI
      PARAMETER           ( EPNPI  = EPBASI + 1 )

      INTEGER               EPNFPI
      PARAMETER           ( EPNFPI = EPNPI  + 1 )

      INTEGER               EPFPI
      PARAMETER           ( EPFPI  = EPNFPI + 1 )

!
!     Size of metadata area:
!
      INTEGER               EPMDSZ
      PARAMETER           ( EPMDSZ = 1 + 3*EPNIPT )

!
!     Page sizes, in units of DAS words of the appropriate type:
!
      INTEGER               PGSIZC
      PARAMETER           ( PGSIZC = 1024 )

      INTEGER               PGSIZD
      PARAMETER           ( PGSIZD = 128 )

      INTEGER               PGSIZI
      PARAMETER           ( PGSIZI = 256 )

!
!     Default page base addresses:
!
      INTEGER               PGBASC
      PARAMETER           ( PGBASC = 0 )

      INTEGER               PGBASD
      PARAMETER           ( PGBASD = 0 )

      INTEGER               PGBASI
      PARAMETER           ( PGBASI = 256 )


!
!     End Include Section:  EK Das Paging Parameters
!
