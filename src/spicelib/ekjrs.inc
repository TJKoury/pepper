!     Include Section:  EK Join Row Set Parameters
!
!        ekjrs.inc  Version 1    07-FEB-1995 (NJB)
!
!
!     Maximum number of join row sets in a join row set union:
!
      INTEGER               MXJRS
      PARAMETER           ( MXJRS  = 200 )


!
!     The layout of a join row set in the EK scratch area is shown
!     below:
!
!        +--------------------------------------------+
!        |              join row set size             |  1 element
!        +--------------------------------------------+
!        |    number of row vectors in join row set   |  1 element
!        +--------------------------------------------+
!        |               table count (TC)             |  1 element
!        +--------------------------------------------+
!        |          segment vector count (SVC)        |  1 element
!        +--------------------------------------------+
!        |               segment vector 1             |  TC elements
!        +--------------------------------------------+
!                              .
!                              .
!                              .
!        +--------------------------------------------+
!        |               segment vector SVC           |  TC elements
!        +--------------------------------------------+
!        |   segment vector 1 row set base address    |  1 element
!        +--------------------------------------------+
!        |      segment vector 1 row count (RC_1)     |  1 element
!        +--------------------------------------------+
!                              .
!                              .
!                              .
!        +--------------------------------------------+
!        |  segment vector SVC row set base address   |  1 element
!        +--------------------------------------------+
!        |   segment vector SVC row count (RC_SVC)    |  1 element
!        +--------------------------------------------+
!        | Augmented row vectors for segment vector 1 |  (TC+1)*RC_1
!        +--------------------------------------------+  elements
!                              .
!                              .
!                              .
!        +--------------------------------------------+
!        |Augmented row vectors for segment vector SVC|  (TC+1)*RC_SVC1
!        +--------------------------------------------+  elements
!
!
!     The following parameters indicate positions of elements in the
!     join row set structure:
!
!
!     Base-relative index of join row set size
!
      INTEGER               JSZIDX
      PARAMETER           ( JSZIDX = 1 )

!
!     Index of row vector count
!
      INTEGER               JRCIDX
      PARAMETER           ( JRCIDX = 2 )

!
!     Index of table count
!
      INTEGER               JTCIDX
      PARAMETER           ( JTCIDX = 3 )

!
!     Index of segment vector count
!
      INTEGER               JSCIDX
      PARAMETER           ( JSCIDX = 4 )

!
!     Base address of first segment vector
!
      INTEGER               JSVBAS
      PARAMETER           ( JSVBAS = 4 )
!
!
!     End Include Section:  EK Join Row Set Parameters
!
