!$Procedure      ZZEKSRS ( EK, set record status )

      SUBROUTINE ZZEKSRS ( HANDLE, RECPTR, STATUS )

! $ Abstract
!
!     Set the status of a specified EK record.
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
!     STATUS     I   Status of specified EK record.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     RECPTR         is a pointer to the record whose status is to be
!                    set.
!
!     STATUS         is the status word of the specified record.  See
!                    the include file ekrecptr.inc for values and
!                    meanings.
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
!     This routine sets the status word of a specified EK record.
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
!-    Beta Version 1.0.0, 17-OCT-1995 (NJB)
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
!     Compute the status word location and set the status.
!
      LOC  =  RECPTR + STAIDX

      CALL DASUDI ( HANDLE, LOC, LOC, STATUS )

      RETURN
      END
