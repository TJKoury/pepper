!$Procedure ZZEKACPS ( EK, allocate contiguous pages for segment )

      subroutine zzekacps ( handle, segdsc, type, n, p, base  )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Allocate a series of contiguous data pages for a specified EK
!     segment.
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

      include 'eksegdsc.inc'
      include 'ektype.inc'

      integer(ip)           handle
      integer(ip)           segdsc ( sdscsz )
      integer(ip)           type
      integer(ip)           n
      integer(ip)           p
      integer(ip)           base

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     SEGDSC     I   Segment descriptor.
!     TYPE       I   Data type of page.
!     N          I   Number of pages to allocate.
!     P          O   Page number.
!     BASE       O   DAS base address of page.
!
! $ Detailed_Input
!
!     HANDLE         is a file handle of an EK open for write access.
!
!     SEGDSC         is the descriptor of the segment for which to
!                    allocate a series of data pages.
!
!     TYPE           is the data type of the desired pages.
!
!     N              is the number of pages desired.  All pages
!                    allocated are new.   A new page is one that has not
!                    been allocated before.
!
! $ Detailed_Output
!
!     P              is the number of the first page of the allocated
!                    series.  The rest of the pages have numbers
!
!                       P+1, P+2, ... , P+N-1
!
!                    These numbers are recognized by the EK paged access
!                    routines.
!
!     BASE           is the DAS base address of the first allocated
!                    page.
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
!     This routine operates by side effects: it allocates a series of
!     new, contiguous EK data pages for a specified segment. The
!     segment's metadata are updated to reflect acquisition of the
!     pages.
!
!     This routine, not ZZEKAPS, should be used when contiguous pages
!     are required.
!
!     Each allocated page is initialized as follows:
!
!        - The page's link count is zeroed out.
!
!        - The page's forward pointer is zeroed out.
!
!     After all pages are allocated, the metadata for the segment are
!     adjusted to reflect ownership of the allocated pages.
!
!     The changes made by this routine to the target EK file become
!     permanent when the file is closed.  Failure to close the file
!     properly will leave it in an indeterminate state.
!
! $ Examples
!
!     See ZZEKWPAI.
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
!-    SPICELIB Version 1.0.1, 01-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    Beta Version 1.0.0, 09-NOV-1995 (NJB)
!
!-&

!
!     SPICELIB functions
!
      logical               failed

!
!     Local variables
!
      integer(ip)           b
      integer(ip)           i
      integer(ip)           idx
      integer(ip)           p2
      integer(ip)           tree

!
!     Use discovery check-in.
!
!
!     Allocate the pages.
!
      call zzekpgan ( handle, type, p, base )

      do i = 2, n
         call zzekpgan ( handle, type, p2, b )
      end do


      if ( failed() ) then
         return
      end if

!
!     Initialize the pages.
!
      do i = 1, n
!
!        Zero out the page's link count and forward pointer.
!
         call zzekslnk ( handle, type, p+i-1, 0 )
         call zzeksfwd ( handle, type, p+i-1, 0 )

      end do

!
!     Update the segment's metadata.  Insert the number of each new
!     page into the page tree of the appropriate data type.
!
      if ( type == chr ) then

         tree  =  segdsc ( cptidx )

      else if ( type == dp ) then

         tree  =  segdsc ( dptidx )

      else
!
!        The remaining possibility is that TYPE is INT.  If we had had
!        an unrecognized type, one of the allocation routines would have
!        complained.
!
         tree  =  segdsc ( iptidx )

      end if


      do i = 1, n
         call zzektrap ( handle, tree, p+i-1, idx )
      end do

      return
      end
