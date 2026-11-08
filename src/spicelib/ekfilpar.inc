!     Include Section:  EK File Metadata Parameters
!
!        ekfilpar.inc  Version 1  28-MAR-1995 (NJB)
!
!     These parameters apply to EK files using architecture 4.
!     These files use a paged DAS file as their underlying file
!     structure.
!
!     The metadata for an architecture 4 EK file is very simple:  it
!     consists of a single integer, which is a pointer to a tree
!     that in turn points to the segments in the EK.  However, in the
!     interest of upward compatibility, one integer page is reserved
!     for the file's metadata.
!
!
!     Size of file parameter block:
!
      INTEGER               FPARSZ
      PARAMETER           ( FPARSZ = 1 )

!
!     All offsets shown below are relative to the beginning of the
!     first integer page in the EK.
!
!
!     Index of the segment pointer tree---this location contains the
!     root page number of the tree:
!
      INTEGER               SGTIDX
      PARAMETER           ( SGTIDX  =  1 )

!
!     End Include Section:  EK File Metadata Parameters
!
