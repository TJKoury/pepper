!$Procedure      ZZEKAPS ( EK, allocate page for segment )

      SUBROUTINE ZZEKAPS ( HANDLE, SEGDSC, TYPE, NEW, P, BASE  )

! $ Abstract
!
!     Allocate a data page for a specified EK segment.
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

      INCLUDE 'eksegdsc.inc'
      INCLUDE 'ektype.inc'

      INTEGER               HANDLE
      INTEGER               SEGDSC ( SDSCSZ )
      INTEGER               TYPE
      LOGICAL               NEW
      INTEGER               P
      INTEGER               BASE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     TYPE       I   Data type of page.
!     NEW        I   Flag indicating whether page is new.
!     P          O   Page number.
!     BASE       O   DAS base address of page.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     SEGDSC         is the descriptor of the segment for which to
!                    allocate a data page.
!
!     TYPE           is the data type of the desired page.
!
!     NEW            is a logical flag indicating whether a new page
!                    is desired.  A new page is one that has not been
!                    allocated before.  If NEW is .FALSE., a page
!                    on the free list may be returned.
!
! $ Detailed_Output
!
!     P              is the page number of the allocated page.  This
!                    number is recognized by the EK paged access
!                    routines.
!
!     BASE           is the DAS base address of the allocated page.
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is invalid, the error will be diagnosed by routines
!         called by this routine.  The file will not be modified.
!
!     2)  If an I/O error occurs while reading or writing the indicated
!         file, the error will be diagnosed by routines called by this
!         routine.  The file may be corrupted.
!
! $ Files
!
!     See the EK Required Reading for a discussion of the EK file
!     format.
!
! $ Particulars
!
!     This routine operates by side effects:  it allocates an EK data
!     page for a specified segment.  The segment's metadata is updated
!     to reflect acquisition of the page.
!
!     The allocated page is initialized as follows:
!
!        - The page's link count is zeroed out.
!
!        - The page's forward pointer is zeroed out.
!
!        - The metadata for the segment is adjusted to reflect ownership
!          of the allocated page.
!
!     The changes made by this routine to the target EK file become
!     permanent when the file is closed.  Failure to close the file
!     properly will leave it in an indeterminate state.
!
! $ Examples
!
!     See EKIFLD.
!
! $ Restrictions
!
!     1)  This routine cannot be used to allocate series of contiguous
!         pages!  Use ZZEKACPS if contiguous pages are required.
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
!-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    Beta Version 1.0.0, 08-NOV-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Local variables
!
      INTEGER               IDX
      INTEGER               TREE


!
!     Use discovery check-in.
!
      IF ( NEW ) THEN
!
!        We must allocate a new page.
!
         CALL ZZEKPGAN ( HANDLE, TYPE, P, BASE )

      ELSE
!
!        We can allocate a page from the free list if one is available.
!        Otherwise take a new page.
!
         CALL ZZEKPGAL ( HANDLE, TYPE, P, BASE )

      END IF


      IF ( FAILED() ) THEN
         RETURN
      END IF

!
!     Zero out the page's link count and forward pointer.
!
      CALL ZZEKSLNK ( HANDLE, TYPE, P, 0 )
      CALL ZZEKSFWD ( HANDLE, TYPE, P, 0 )

!
!     Update the segment's metadata.  For type 1 segments,
!     the new page into the page tree of the appropriate data type.
!
      IF ( TYPE .EQ. CHR ) THEN

         TREE  =  SEGDSC ( CPTIDX )


      ELSE IF ( TYPE .EQ. DP ) THEN

         TREE  =  SEGDSC ( DPTIDX )


      ELSE IF ( TYPE .EQ. INT ) THEN
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, one of the allocation routines would have
!        complained.
!
         TREE  =  SEGDSC ( IPTIDX )

      END IF


      CALL ZZEKTRAP ( HANDLE, TREE, P, IDX )


      RETURN
      END
