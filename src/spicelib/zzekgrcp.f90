!$Procedure      ZZEKGRCP ( EK, get record companion pointer )

      SUBROUTINE ZZEKGRCP ( HANDLE, RECPTR, PTR )

! $ Abstract
!
!     Get the companion pointer of a specified EK record.
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

      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ekrecptr.inc'

      INTEGER               HANDLE
      INTEGER               RECPTR
      INTEGER               PTR

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     RECPTR     I   Record pointer.
!     PTR        O   Pointer to companion of specified EK record.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     RECPTRC        is a pointer to the record whose companion pointer
!                    is desired.
!
! $ Detailed_Output
!
!     PTR            is a pointer to the companion record of the
!                    specified input record.  If the input record
!                    belongs to a shadowed EK, the companion record
!                    is the corresponding backup record.  If the input
!                    record is a backup record, the companion record
!                    is the corresponding source record.
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
!     2)  If an I/O error occurs while reading the indicated file, the
!         error will be diagnosed by routines called by this routine.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine sets the companion record pointer of a specified EK
!     record.
!
! $ Examples
!
!     See EKROLL.
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
!-    Beta Version 1.0.0, 10-OCT-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Local variables
!
      INTEGER               LOC

!
!     Use discovery check-in.
!
!
!     Is this file handle valid--is the file open for paged write
!     access?  Signal an error if not.
!
      CALL ZZEKPGCH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Compute the companion pointer and set the pointer.
!
      LOC  =  RECPTR + RCPIDX

      CALL DASRDI ( HANDLE, LOC, LOC, PTR )

      RETURN
      END
