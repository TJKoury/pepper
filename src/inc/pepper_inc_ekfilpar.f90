!> EK File Metadata Parameters.
!>
!>     These parameters apply to EK files using architecture 4.
!>     These files use a paged DAS file as their underlying file
!>     structure.
!>
!>     The metadata for an architecture 4 EK file is very simple:  it
!>     consists of a single integer, which is a pointer to a tree
!>     that in turn points to the segments in the EK.  However, in the
!>     interest of upward compatibility, one integer page is reserved
!>     for the file's metadata.
!>
!>## Version
!>
!>        ekfilpar.inc  Version 1  28-MAR-1995 (NJB)
module pepper_inc_ekfilpar

   use pepper_kinds, only: ip

   implicit none

   public

   integer(ip), parameter :: fparsz = 1  !! Size of file parameter block:

   integer(ip), parameter :: sgtidx = 1  !! All offsets shown below are relative to the beginning of the
                                         !! first integer page in the EK.
                                         !!
                                         !! Index of the segment pointer tree---this location contains the
                                         !! root page number of the tree:

end module pepper_inc_ekfilpar
