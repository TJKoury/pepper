!$Procedure      ZZEKGRS ( EK, get record status )

      SUBROUTINE ZZEKGRS ( HANDLE, RECPTR, STATUS )

! $ Abstract
!
!     Return the status of a specified EK record.
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

      INCLUDE 'ekrecptr.inc'

      INTEGER               HANDLE
      INTEGER               RECPTR
      INTEGER               STATUS

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     RECPTR     I   Record pointer.
!     STATUS     O   Status of specified EK record.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for read or write
!                    access.
!
!     RECPTR         is a pointer to the record whose status is desired.
!
! $ Detailed_Output
!
!     STATUS         is the status word of the specified record.  See
!                    the include file ekrecptr.inc for values and
!                    meanings.
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
!     This routine hides details of status word access.
!
! $ Examples
!
!     See EKCOMM.
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
!     Local variables
!
      INTEGER               LOC

!
!     Use discovery check-in.
!
!
!     Compute the status word location, and read the status.
!
      LOC  =  RECPTR + STAIDX

      CALL DASRDI ( HANDLE, LOC, LOC, STATUS )

      RETURN
      END
