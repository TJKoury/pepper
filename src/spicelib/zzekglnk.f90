!$Procedure      ZZEKGLNK ( EK, get link count for data page )

      SUBROUTINE ZZEKGLNK ( HANDLE, TYPE, P, NLINKS )

! $ Abstract
!
!     Return the link count for a specified EK data page.
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

      INCLUDE 'ekdatpag.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               TYPE
      INTEGER               P
      INTEGER               NLINKS

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TYPE       I   Data type of page.
!     P          I   Page number.
!     NLINKS     O   Number of links to page.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     TYPE           is the data type of the desired page.
!
!     P              is the page number of the allocated page.  This
!                    number is recognized by the EK paged access
!                    routines.
!
! $ Detailed_Output
!
!     NLINKS         is the currently held number of links to the
!                    specified data page.
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
!     2)  If TYPE is invalid, the error will be diagnosed by routines
!         called by this routine.
!
!     3)  If an I/O error occurs while reading or writing the indicated
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
!     This routine centralizes EK data page link count accesses.
!
! $ Examples
!
!     See EKDELR.
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
      DOUBLE PRECISION      DPLNK
      INTEGER               BASE

!
!     Use discovery check-in.
!
!     Look up the base address of the page.
!
      CALL ZZEKPGBS ( TYPE, P, BASE )

      IF ( FAILED() ) THEN
         RETURN
      END IF


      IF ( TYPE .EQ. CHR ) THEN
!
!        Look up the encoded count.
!
         CALL ZZEKGEI ( HANDLE, BASE+CLCIDX, NLINKS )


      ELSE IF ( TYPE .EQ. DP ) THEN
!
!        Convert the encoded count to integer type.
!
         CALL DASRDD ( HANDLE, BASE+DLCIDX, BASE+DLCIDX, DPLNK )

         NLINKS  =  NINT ( DPLNK )


      ELSE
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, ZZEKPGBS would have complained.
!
         CALL DASRDI ( HANDLE, BASE+ILCIDX, BASE+ILCIDX, NLINKS )

      END IF

      RETURN
      END
