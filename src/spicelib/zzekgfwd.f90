!$Procedure      ZZEKGFWD ( EK, get forward pointer for data page )

      SUBROUTINE ZZEKGFWD ( HANDLE, TYPE, P, FWARD )

! $ Abstract
!
!     Return the forward data pointer for a specified EK data page.
!
! $ Required_Reading
!
!     EK
!
! $ Keywords
!
!     PRIVATE
!     UTILITY
!
! $ Declarations

      INCLUDE 'ekdatpag.inc'
      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               TYPE
      INTEGER               P
      INTEGER               FWARD

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     TYPE       I   Data type of page.
!     P          I   Page number.
!     FWARD      O   Forward data pointer.
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
!     FWARD          is a forward data pointer.  This is the number
!                    of a data page on which the last column entry
!                    on page P is continued.
!
!                    FWARD may overwrite P.
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
!     This routine returns the forward data pointer of the specified EK
!     data page.  The value of the pointer is a page number.
!
! $ Examples
!
!     See ZZEKDE03.
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
      DOUBLE PRECISION      DPPTR
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
!        Get the encoded pointer.
!
         CALL ZZEKGEI ( HANDLE, BASE+CFPIDX, FWARD )


      ELSE IF ( TYPE .EQ. DP ) THEN
!
!        Convert the d.p. pointer to integer type.
!
         CALL DASRDD ( HANDLE, BASE+DFPIDX, BASE+DFPIDX, DPPTR )

         FWARD  =  NINT ( DPPTR )

      ELSE
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, ZZEKPGBS would have complained.
!
         CALL DASRDI ( HANDLE, BASE+IFPIDX, BASE+IFPIDX, FWARD )

      END IF

      RETURN
      END
