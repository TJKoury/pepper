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
      integer               eparch
      parameter           ( eparch = 1 )

!
!     Number of metadata items per data type:
!
      integer               epnipt
      parameter           ( epnipt = 5 )

!
!     Character metadata indices:
!
      integer               eppszc
      parameter           ( eppszc = eparch + 1 )

      integer               epbasc
      parameter           ( epbasc = eppszc + 1 )

      integer               epnpc
      parameter           ( epnpc  = epbasc + 1 )

      integer               epnfpc
      parameter           ( epnfpc = epnpc  + 1 )

      integer               epfpc
      parameter           ( epfpc  = epnfpc + 1 )

!
!     Double precision metadata indices:
!
      integer               eppszd
      parameter           ( eppszd = eppszc + epnipt )

      integer               epbasd
      parameter           ( epbasd = eppszd + 1 )

      integer               epnpd
      parameter           ( epnpd  = epbasd + 1 )

      integer               epnfpd
      parameter           ( epnfpd = epnpd  + 1 )

      integer               epfpd
      parameter           ( epfpd  = epnfpd + 1 )

!
!     Integer metadata indices:
!
      integer               eppszi
      parameter           ( eppszi = eppszd + epnipt )

      integer               epbasi
      parameter           ( epbasi = eppszi + 1 )

      integer               epnpi
      parameter           ( epnpi  = epbasi + 1 )

      integer               epnfpi
      parameter           ( epnfpi = epnpi  + 1 )

      integer               epfpi
      parameter           ( epfpi  = epnfpi + 1 )

!
!     Size of metadata area:
!
      integer               epmdsz
      parameter           ( epmdsz = 1 + 3*epnipt )

!
!     Page sizes, in units of DAS words of the appropriate type:
!
      integer               pgsizc
      parameter           ( pgsizc = 1024 )

      integer               pgsizd
      parameter           ( pgsizd = 128 )

      integer               pgsizi
      parameter           ( pgsizi = 256 )

!
!     Default page base addresses:
!
      integer               pgbasc
      parameter           ( pgbasc = 0 )

      integer               pgbasd
      parameter           ( pgbasd = 0 )

      integer               pgbasi
      parameter           ( pgbasi = 256 )


!
!     End Include Section:  EK Das Paging Parameters
!
