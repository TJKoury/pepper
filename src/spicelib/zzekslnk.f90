!$Procedure      ZZEKSLNK ( EK, set link count for data page )

      SUBROUTINE ZZEKSLNK ( HANDLE, TYPE, P, NLINKS )

! $ Abstract
!
!     Set the link count for a specified EK data page.
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
!     NLINKS     I   Number of links to page.
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
!     NLINKS         is the new number of links to the specified data
!                    page.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the effect of this
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
!     This routine updates the link count of the specified EK data page.
!
!     Link counts are used to indicate how many `users' of a page
!     there are.  When the link count of a page drops to zero, that
!     page is eligible to be freed.
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
!        Set the encoded count.
!
         CALL ZZEKSEI ( HANDLE, BASE+CLCIDX, NLINKS )


      ELSE IF ( TYPE .EQ. DP ) THEN
!
!        Convert the input count to d.p. type.
!
         DPLNK  =  NLINKS

         CALL DASUDD ( HANDLE, BASE+DLCIDX, BASE+DLCIDX, DPLNK )


      ELSE
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, ZZEKPGBS would have complained.
!
         CALL DASUDI ( HANDLE, BASE+ILCIDX, BASE+ILCIDX, NLINKS )

      END IF

      RETURN
      END
