!$Procedure      ZZEKSRCP ( EK, set record companion pointer )

      subroutine zzeksrcp ( handle, recptr, recno )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Set the companion pointer of a specified EK record.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     EK
!     PRIVATE
!
! $ Declarations

      include 'ekrecptr.inc'

      integer(ip)           handle
      integer(ip)           recptr
      integer(ip)           recno

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     RECPTR     I   Record pointer.
!     RECNO      I   Record number of companion of specified EK record.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     RECPTR         is a pointer to the record whose companion pointer
!                    is to be set.
!
!     RECNO          is the `companion record pointer' of the record
!                    designated by RECPTR.  This name is a little
!                    misleading:  we identify the companion record
!                    by its record number, not by an address.  So RECNO
!                    is a pointer only in an abstract sense.
!
!                    If the input record belongs to a shadowed EK, the
!                    companion record is the corresponding backup
!                    record.  If the input record is a backup record,
!                    the companion record is the corresponding source
!                    record.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the action of this
!     routine.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     2)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine sets the companion record pointer of a specified EK
!     record.  As noted in $Detailed_Input, the `pointer' is simply
!     the number of the companion record.
!
! $ Examples
!
!     See ZZEKRBCK.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 18-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Local variables
!
      integer(ip)           loc

!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      call zzekpgch ( handle, 'WRITE' )

      if ( failed() ) return

!
!     Compute the companion pointer and set the pointer.
!
      loc  =  recptr + rcpidx

      call dasudi ( handle, loc, loc, recno )

      return
      end
