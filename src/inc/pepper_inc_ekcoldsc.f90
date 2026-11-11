!> The parameters of SPICELIB's include file ekcoldsc.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekcoldsc

   use pepper_kinds, only: ip

   implicit none

   public

!     Include Section:  EK Column Descriptor Parameters
!
!        ekcoldsc.inc Version 6    23-AUG-1995 (NJB)
!
!
!     Note:  The column descriptor size parameter CDSCSZ  is
!     declared separately in the include section CDSIZE$INC.FOR.
!
!     Offset of column descriptors, relative to start of segment
!     integer address range.  This number, when added to the last
!     integer address preceding the segment, yields the DAS integer
!     base address of the first column descriptor.  Currently, this
!     offset is exactly the size of a segment descriptor.  The
!     parameter SDSCSZ, which defines the size of a segment descriptor,
!     is declared in the include file eksegdsc.inc.
!
   integer(ip), parameter :: cdoff = 24

!
!     Size of column descriptor
!
   integer(ip), parameter :: cdscsz = 11

!
!     Indices of various pieces of column descriptors:
!
!
!     CLSIDX is the index of the column's class code.  (We use the
!     word `class' to distinguish this item from the column's data
!     type.)
!
   integer(ip), parameter :: clsidx = 1

!
!     TYPIDX is the index of the column's data type code (CHR, INT, DP,
!     or TIME).  The type is actually implied by the class, but it
!     will frequently be convenient to look up the type directly.
!
!
   integer(ip), parameter :: typidx = clsidx + 1

!
!     LENIDX is the index of the column's string length value, if the
!     column has character type.  A value of IFALSE in this element of
!     the descriptor indicates that the strings have variable length.
!
   integer(ip), parameter :: lenidx = typidx + 1

!
!     SIZIDX is the index of the column's element size value.  This
!     descriptor element is meaningful for columns with fixed-size
!     entries.  For variable-sized columns, this value is IFALSE.
!
   integer(ip), parameter :: sizidx = lenidx + 1

!
!     NAMIDX is the index of the base address of the column's name.
!
   integer(ip), parameter :: namidx = sizidx + 1

!
!     IXTIDX is the data type of the column's index.  IXTIDX
!     contains a type value only if the column is indexed. For columns
!     that are not indexed, the location IXTIDX contains the boolean
!     value IFALSE.
!
   integer(ip), parameter :: ixtidx = namidx + 1

!
!     IXPIDX is a pointer to the column's index.  IXTPDX contains a
!     meaningful value only if the column is indexed.  The
!     interpretation of the pointer depends on the data type of the
!     index.
!
   integer(ip), parameter :: ixpidx = ixtidx + 1

!
!     NFLIDX is the index of a flag indicating whether nulls are
!     permitted in the column.  The value at location NFLIDX is
!     ITRUE if nulls are permitted and IFALSE otherwise.
!
   integer(ip), parameter :: nflidx = ixpidx + 1

!
!     ORDIDX is the index of the column's ordinal position in the
!     list of columns belonging to the column's parent segment.
!
   integer(ip), parameter :: ordidx = nflidx + 1

!
!     METIDX is the index of the column's integer metadata pointer.
!     This pointer is a DAS integer address.
!
   integer(ip), parameter :: metidx = ordidx + 1

!
!     The last position in the column descriptor is reserved.  No
!     parameter is defined to point to this location.
!

!
!     End Include Section:  EK Column Descriptor Parameters
!

end module pepper_inc_ekcoldsc
