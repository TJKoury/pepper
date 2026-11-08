!     Include Section:  EK General Limit Parameters
!
!        ekglimit.inc  Version 1    21-MAY-1995 (NJB)
!
!
!     This file contains general limits for the EK system.
!
!     MXCLSG is the maximum number of columns allowed in a segment.
!     This limit applies to logical tables as well, since all segments
!     in a logical table must have the same column definitions.
!
      integer               mxclsg
      parameter           ( mxclsg = 100 )

!
!     End Include Section:  EK General Limit Parameters
!
