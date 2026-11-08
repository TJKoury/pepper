!     Include Section:  EK Data Page Parameters
!
!        ekfilpar.inc  Version 1  03-APR-1995 (NJB)
!
!     These parameters apply to EK files using architecture 4.
!     These files use a paged DAS file as their underlying file
!     structure.
!
!     In paged DAS EK files, data pages are structured:  they contain
!     metadata as well as data.  The metadata is located in the last
!     few addresses of each page, so as to interfere as little as
!     possible with calculation of data addresses.
!
!     Each data page belongs to exactly one segment.  Some bookkeeping
!     information, such as record pointers, is also stored in data
!     pages.
!
!     Each page contains a forward pointer that allows rapid lookup
!     of data items that span multiple pages.  Each page also keeps
!     track of the current number of links from its parent segment
!     to the page.  Link counts enable pages to `know' when they
!     are no longer in use by a segment; unused pages are deallocated
!     and returned to the free list.
!
!     The parameters in this include file depend on the parameters
!     declared in the include file ekpage.inc.  If those parameters
!     change, this file must be updated.  The specified parameter
!     declarations we need from that file are:
!
!        INTEGER               PGSIZC
!        PARAMETER           ( PGSIZC = 1024 )
!
!        INTEGER               PGSIZD
!        PARAMETER           ( PGSIZD = 128 )
!
!        INTEGER               PGSIZI
!        PARAMETER           ( PGSIZI = 256 )
!
!
!
!     Character pages use an encoding mechanism to represent integer
!     metadata.  Each integer is encoded in five consecutive
!     characters.
!
!
!     Character data page parameters:
!
!
!     Size of encoded integer:
!
      INTEGER               ENCSIZ
      PARAMETER           ( ENCSIZ = 5 )

!
!     Usable page size:
!
      INTEGER               CPSIZE
      PARAMETER           ( CPSIZE = 1014 )

!
!     Location of character forward pointer:
!
      INTEGER               CFPIDX
      PARAMETER           ( CFPIDX = CPSIZE + 1)


!
!     Location of character link count:
!
      INTEGER               CLCIDX
      PARAMETER           ( CLCIDX = CFPIDX + ENCSIZ )


!
!     Double precision data page parameters:
!
!     Usable page size:
!
      INTEGER               DPSIZE
      PARAMETER           ( DPSIZE = 126 )

!
!     Location of d.p. forward pointer:
!
      INTEGER               DFPIDX
      PARAMETER           ( DFPIDX = DPSIZE + 1)


!
!     Location of d.p. link count:
!
      INTEGER               DLCIDX
      PARAMETER           ( DLCIDX = DFPIDX + 1 )


!
!     Integer data page parameters:
!
!     Usable page size:
!
      INTEGER               IPSIZE
      PARAMETER           ( IPSIZE = 254 )

!
!     Location of integer forward pointer:
!
      INTEGER               IFPIDX
      PARAMETER           ( IFPIDX = IPSIZE + 1)


!
!     Location of integer link count:
!
      INTEGER               ILCIDX
      PARAMETER           ( ILCIDX = IFPIDX + 1 )


!
!     End Include Section:  EK Data Page Parameters
!
