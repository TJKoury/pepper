!$Procedure SPKS01 ( S/P Kernel, subset, type 1 )

      subroutine spks01 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Extract a subset of the data in a SPK segment of type 1
!     into a new segment.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      integer(ip)           handle
      integer(ip)           baddr
      integer(ip)           eaddr
      real(wp)              begin
      real(wp)              end

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of source segment.
!     BADDR      I   Beginning address of source segment.
!     EADDR      I   Ending address of source segment.
!     BEGIN      I   Beginning (initial epoch) of subset.
!     END        I   End (final epoch) of subset.
!
! $ Detailed_Input
!
!     HANDLE,
!     BADDR,
!     EADDR    are the file handle assigned to a SPK file, and the
!              beginning and ending addresses of a segment within
!              the file. Together they determine a complete set of
!              ephemeris data, from which a subset is to be
!              extracted.
!
!     BEGIN,
!     END      are the initial and final epochs (ephemeris time)
!              of the subset.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while reading data from the source SPK
!         file, the error is signaled by a routine in the call tree of
!         this routine.
!
!     2)  If an error occurs while writing data to the output SPK file,
!         the error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     The exact structure of a segment of data type 1 is detailed in
!     the SPK Required Reading file.
!
! $ Examples
!
!     None.
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
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 14-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Completed
!        $Exceptions section. Moved SPK required reading from
!        $Literature_References to $Required_Reading section.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.3, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.2, 23-AUG-1991 (HAN)
!
!        SPK01 was removed from the $Required_Reading section of the
!        header. The information in the SPK01 Required Reading file
!        is now part of the SPK Required Reading file.
!
!-    SPICELIB Version 1.0.1, 22-MAR-1990 (HAN)
!
!        Literature references added to the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     subset type_1 SPK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      real(wp)              data     ( 71 )

      integer(ip)           nrec
      integer(ip)           ndir
      integer(ip)           offe
      integer(ip)           first
      integer(ip)           last
      integer(ip)           offset
      integer(ip)           i


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Get the number of records in the segment. From that, we can
!     compute
!
!        NDIR      The number of directory epochs.
!
!        OFFE      The offset of the first epoch.
!
!
!     the number of directory epochs.
!
      call dafgda ( handle, eaddr, eaddr, data )
      nrec = int ( data(1) )

      ndir  = nrec / 100
      offe  = eaddr - ndir - nrec - 1

!
!     Well, the new segment has already been begun. We just have to
!     decide what to move, and move it (using DAFADA).
!
!     Let's agree right now that speed is not of the greatest
!     importance here. We can probably do this with two passes
!     through the record epochs, and one pass through the records.
!
!        1) Determine the first and last records to be included
!           in the subset.
!
!        2) Move the records.
!
!        3) Write the epochs.
!
!     We can leap through the epochs one last time to get the
!     directory epochs.
!

!
!     First pass: which records are to be moved?
!
      first = 0
      last  = 0

      do i = 1, nrec
         call dafgda ( handle, offe+i, offe+i, data )

         if (        first   == 0 &
              .and.  data(1) >= begin )   first = i

         if (        first   /= 0 &
              .and.  last    == 0 &
              .and.  data(1) >= end )     last = i

      end do

!
!     Second pass. Move the records.
!
      offset = (baddr - 1) + (first - 1) * 71

      do i = first, last
         call dafgda ( handle, offset+1, offset+71, data     )
         call dafada (                              data, 71 )

         offset = offset + 71
      end do

!
!     Third pass. Move the epochs.
!
      do i = first, last
         call dafgda ( handle, offe+i, offe+i, data    )
         call dafada (                         data, 1 )
      end do

!
!     Get every 100'th epoch for the directory.
!
      do i = first + 99, last, 100
         call dafgda ( handle, offe+i, offe+i, data    )
         call dafada (                         data, 1 )
      end do

!
!     Add the number of records, and we're done.
!
      data(1) = real( last - first + 1, wp )
      call dafada ( data, 1 )

      return
      end
