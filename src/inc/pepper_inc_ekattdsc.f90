!> EK Column Attribute Descriptor Parameters.
!>
!>     This include file declares parameters used in EK column
!>     attribute descriptors.  Column attribute descriptors are
!>     a simplified version of column descriptors:  attribute
!>     descriptors describe attributes of a column but do not contain
!>     addresses or pointers.
!>
!>## Version
!>
!>        ekattdsc.inc Version 1    23-AUG-1995 (NJB)
module pepper_inc_ekattdsc

   use pepper_kinds, only: ip

   implicit none

   public

!     Size of column attribute descriptor
!
   integer(ip), parameter :: adscsz = 6

!
!     Indices of various pieces of attribute descriptors:
!
!
!     ATTSIZ is the index of the column's class code.  (We use the
!     word `class' to distinguish this item from the column's data
!     type.)
!
   integer(ip), parameter :: attcls = 1

!
!     ATTTYP is the index of the column's data type code (CHR, INT, DP,
!     or TIME).  The type is actually implied by the class, but it
!     will frequently be convenient to look up the type directly.
!
!
   integer(ip), parameter :: atttyp = attcls + 1

!
!     ATTLEN is the index of the column's string length value, if the
!     column has character type.  A value of IFALSE in this element of
!     the descriptor indicates that the strings have variable length.
!
   integer(ip), parameter :: attlen = atttyp + 1

!
!     ATTSIZ is the index of the column's element size value.  This
!     descriptor element is meaningful for columns with fixed-size
!     entries.  For variable-sized columns, this value is IFALSE.
!
   integer(ip), parameter :: attsiz = attlen + 1

!
!     ATTIDX is the location of a flag that indicates whether the column
!     is indexed.  The flag takes the value ITRUE if the column is
!     indexed and otherwise takes the value IFALSE.
!
   integer(ip), parameter :: attidx = attsiz + 1

!
!     ATTNFL is the index of a flag indicating whether nulls are
!     permitted in the column.  The value at location NFLIDX is
!     ITRUE if nulls are permitted and IFALSE otherwise.
!
   integer(ip), parameter :: attnfl = attidx + 1

end module pepper_inc_ekattdsc
