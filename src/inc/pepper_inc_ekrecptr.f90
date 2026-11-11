!> The parameters of SPICELIB's include file ekrecptr.inc (an include
!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.
module pepper_inc_ekrecptr

   use pepper_kinds, only: ip

   implicit none

   public

!     Include Section:  EK Record Pointer Parameters
!
!        ekrecptr.inc Version 2  18-JUL-1995 (NJB)
!
!
!     This file declares parameters used in EK record pointers.
!     Each segment references data in a given record via two levels
!     of indirection:  a record number points to a record pointer,
!     which is a structured array of metadata and data pointers.
!
!     Record pointers always occupy contiguous ranges of integer
!     addresses.
!
!     The parameter declarations in this file depend on the assumption
!     that integer pages contain 256 DAS integer words and that the
!     maximum number of columns in a segment is 100.  Record pointers
!     are stored in integer data pages, so they must fit within the
!     usable data area afforded by these pages.  The size of the usable
!     data area is given by the parameter IPSIZE which is declared in
!     ekdatpag.inc.  The assumed value of IPSIZE is 254.
!
!
!     The first element of each record pointer is a status indicator.
!     The meanings of status indicators depend on whether the parent EK
!     is shadowed or not.  For shadowed EKs, allowed status values and
!     their meanings are:
!
!        OLD       The record has not been modified since
!                  the EK containing the record was opened.
!
!        UPDATE    The record is an update of a previously existing
!                  record.  The original record is now on the
!                  modified record list.
!
!        NEW       The record has been added since the EK containing the
!                  record was opened.  The record is not an update
!                  of a previously existing record.
!
!        DELOLD    This status applies only to a backup record.
!                  DELOLD status indicates that the record corresponds
!                  to a deleted OLD record in the source segment.
!
!        DELNEW    This status applies only to a backup record.
!                  DELNEW status indicates that the record corresponds
!                  to a deleted NEW record in the source segment.
!
!        DELUPD    This status applies only to a backup record.
!                  DELUPD status indicates that the record corresponds
!                  to a deleted UPDATEd record in the source segment.
!
!     In EKs that are not shadowed, all records have status OLD.
!
!
   integer(ip), parameter :: old = 1

   integer(ip), parameter :: update = old + 1

   integer(ip), parameter :: new = update + 1

   integer(ip), parameter :: delold = new + 1

   integer(ip), parameter :: delnew = delold + 1

   integer(ip), parameter :: delupd = delnew + 1

!
!     The following parameters refer to indices within the record
!     pointer structure:
!
!     Index of status indicator:
!
   integer(ip), parameter :: staidx = 1

!
!     Each record pointer contains a pointer to its companion:  for a
!     record belonging to a shadowed EK, this is the backup counterpart,
!     or if the parent EK is itself a backup EK, a pointer to the
!     record's source record.  The pointer is UNINIT (see below) if the
!     record is unmodified.
!
!     Record companion pointers contain record numbers, not record
!     base addresses.
!
!     Index of record's companion pointer:
!
   integer(ip), parameter :: rcpidx = staidx + 1

!
!     Each data item is referenced by an integer.  The meaning of
!     this integer depends on the representation of data in the
!     column to which the data item belongs.  Actual lookup of a
!     data item must be done by subroutines appropriate to the class of
!     the column to which the item belongs.  Note that data items don't
!     necessarily occupy contiguous ranges of DAS addresses.
!
!     Base address of data pointers:
!
   integer(ip), parameter :: dptbas = 2

!
!     Maximum record pointer size:
!
   integer(ip), parameter :: mxrpsz = 254

!
!     Data pointers are given the value UNINIT to start with; this
!     indicates that the data item is uninitialized.  UNINIT is
!     distinct from the value NULL.  NOBACK indicates an uninitialized
!     backup column entry.
!
   integer(ip), parameter :: uninit = -1

   integer(ip), parameter :: null = uninit - 1

   integer(ip), parameter :: noback = null - 1

!
!     End Include Section:  EK Record Pointer Parameters
!

end module pepper_inc_ekrecptr
