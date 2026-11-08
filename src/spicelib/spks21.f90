!$Procedure SPKS21 ( S/P Kernel, subset, type 21 )

      subroutine spks21 ( handle, baddr, eaddr, begin, end )

! $ Abstract
!
!     Extract a subset of the data in a SPK segment of type 21
!     into a new segment.
!
! $ Required_Reading
!
!     DAF
!     SPK
!     TIME
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      include 'spk21.inc'

      integer               handle
      integer               baddr
      integer               eaddr
      double precision      begin
      double precision      end

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
!              of the subset to be extracted.
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
!     The exact structure of a segment of data type 21 is detailed in
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     F.T. Krogh         (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-OCT-2021 (JDR) (BVS)
!
!        Bug fix: fixed routine name in CHKIN/CHKOUT calls (SPKS01
!        -> SPKS21).
!
!        Edited the header to comply with NAIF standard. Moved SPK
!        required reading from $Literature_References to
!        $Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 16-JAN-2014 (NJB) (FTK) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     subset type_21 SPK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!
      integer               dirsiz
      parameter           ( dirsiz = 100 )

      integer               maxdsz
      parameter           ( maxdsz =  (4 * maxtrm) + 11 )

!
!     Local variables
!
      double precision      data     ( maxdsz )

      integer               dlsize
      integer               maxdim
      integer               nrec
      integer               ndir
      integer               offe
      integer               first
      integer               last
      integer               offset
      integer               i


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'SPKS21' )

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
      call dafgda ( handle, eaddr-1, eaddr, data )

      maxdim = nint( data(1) )
      nrec   = nint( data(2) )

      ndir   = nrec / dirsiz
      offe   = eaddr - ndir - nrec - 2

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
!     First pass: which records are to be moved?
!
      first = 0
      last  = 0

      do i = 1, nrec

         call dafgda ( handle, offe+i, offe+i, data )

         if (       (  first   .eq. 0     ) &
              .and. (  data(1) .ge. begin ) ) then

             first = i

         end if

         if (        ( first   .ne. 0   ) &
              .and.  ( last    .eq. 0   ) &
              .and.  ( data(1) .ge. end )  ) then

            last = i

         end if

      end do

!
!     Second pass. Move the records.
!
      dlsize = ( 4 * maxdim ) + 11

      offset = (baddr - 1) + (first - 1) * dlsize

      do i = first, last

         call dafgda ( handle, offset+1, offset+dlsize, data         )
         call dafada (                                  data, dlsize )

         offset = offset + dlsize

      end do

!
!     Third pass. Move the epochs.
!
      do i = first, last

         call dafgda ( handle, offe+i, offe+i, data    )
         call dafada (                         data, 1 )

      end do

!
!     Get every DIRSIZ'th epoch for the directory.
!
      do i = (first + dirsiz - 1), last, dirsiz

         call dafgda ( handle, offe+i, offe+i, data    )
         call dafada (                         data, 1 )

      end do

!
!     Add the maximum difference line dimension and the
!     number of records, and we're done.
!
      call dafada ( dble( maxdim ), 1 )

      data(1) = dble ( last - first + 1 )

      call dafada ( data, 1 )

      call chkout ( 'SPKS21' )
      return
      end
