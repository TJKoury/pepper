! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
!
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
