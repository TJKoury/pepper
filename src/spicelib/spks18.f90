!$Procedure SPKS18 ( S/P Kernel, subset, type 18 )

      subroutine spks18 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Extract a subset of the data in an SPK segment of type 18
!     into a new segment.
!
! $ Required_Reading
!
!     SPK
!     DAF
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      include 'spk18.inc'

      integer(ip)           handle
      integer(ip)           baddr
      integer(ip)           eaddr
      real(wp)              begin
      real(wp)              end

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of file containing source segment.
!     BADDR      I   Beginning address in file of source segment.
!     EADDR      I   Ending address in file of source segment.
!     BEGIN      I   Beginning (initial epoch) of subset.
!     END        I   End (final epoch) of subset.
!
! $ Detailed_Input
!
!     HANDLE,
!     BADDR,
!     EADDR    are the file handle assigned to an SPK file, and the
!              beginning and ending addresses of a segment within
!              that file. Together they determine a complete set of
!              ephemeris data, from which a subset is to be
!              extracted.
!
!     BEGIN,
!     END      are the initial and final epochs (ephemeris time)
!              of the subset.
!
!              The first epoch for which there will be ephemeris
!              data in the new segment will be the greatest time
!              in the source segment that is less than or equal
!              to BEGIN.
!
!              The last epoch for which there will be ephemeris
!              data in the new segment will be the smallest time
!              in the source segment that is greater than or equal
!              to END.
!
! $ Detailed_Output
!
!     See $Files section.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine relies on the caller to ensure that the
!         interval [BEGIN, END] is contained in the coverage
!         interval of the segment.
!
!     2)  If BEGIN > END, no data are written to the target file.
!
!     3)  If an unexpected SPK type 18 subtype is found in the input
!         segment, the error SPICE(INVALIDVALUE) is signaled.
!
! $ Files
!
!     Data are extracted from the file connected to the input
!     handle, and written to the current DAF open for writing.
!
!     The segment descriptor and summary must already have been written
!     prior to calling this routine. The segment must be ended
!     external to this routine.
!
! $ Particulars
!
!     This routine is intended solely for use as a utility by the
!     routine SPKSUB.
!
!     It transfers a subset of a type 18 SPK data segment to
!     a properly initialized segment of a second SPK file.
!
!     The exact structure of a segment of data type 18 is described
!     in the section on type 18 in the SPK Required Reading.
!
! $ Examples
!
!     This routine is intended only for use as a utility by SPKSUB.
!     To use this routine successfully, you must:
!
!        Open the SPK file from which to extract data.
!        Locate the segment from which data should be extracted.
!
!        Open the SPK file to which this data should be written.
!        Begin a new segment (array).
!        Write the summary information for the array.
!
!        Call this routine to extract the appropriate data from the
!        SPK open for read.
!
!        End the array to which this routine writes data.
!
!     Much of this procedure is carried out by the routine SPKSUB. The
!     examples of that routine illustrate more fully the process
!     described above.
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
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 22-DEC-2012 (NJB)
!
!        Bug fix: code applicable to SPK type 9 for
!        creating padding in the output segment was
!        deleted.
!
!-    SPICELIB Version 1.0.0, 16-AUG-2002 (NJB) (WLT) (IMU)
!
!-&

! $ Index_Entries
!
!     subset type_18 SPK segment
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 22-DEC-2012 (NJB)
!
!        Bug fix: code applicable to SPK type 9 for
!        creating padding in the output segment was
!        deleted.
!
!        The offending code was meant to ensure that
!        the output segment's size is at least the
!        window size corresponding to the segment's
!        interpolation degree. This is correct behavior
!        for SPK types 9 and 13; for these types,
!        segments are not allowed to have sizes less
!        than the nominal window size.
!
!        However, for type 18, segments can have as
!        few as two data packets, regardless of their
!        interpolation degree. The code that creates
!        padding packets in this case reads from
!        invalid locations.
!
!        Also, the variable WINSIZ was introduced, and
!        comments indicating that the stored size
!        parameter in the segment control area is the
!        window size minus one were corrected.
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: msize = 3

!
!     Local variables
!
      real(wp)              data     ( 12 )

      integer(ip)           i
      integer(ip)           ndir
      integer(ip)           nrec
      integer(ip)           offe
      integer(ip)           offset
      integer(ip)           packsz
      integer(ip)           rec      ( 2 )
      integer(ip)           subtyp
      integer(ip)           winsiz
      integer(ip)           wnszm1

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     See whether there's any work to do; return immediately if not.
!
      if (  begin  >  end  ) return

!
!     Read the segment structure metadata.
!
!     Get the type 18 segment subtype.  Next get the quantity "window
!     size." Also get the number of records in the segment.
!
      call dafgda ( handle, eaddr-msize+1, eaddr, data )

      subtyp = nint ( data(1) )
      winsiz = nint ( data(2) )
      nrec   = nint ( data(3) )

!
!     Set the packet size based on the subtype.
!
      if ( subtyp == s18tp0 ) then

         packsz = s18ps0

      else if ( subtyp == s18tp1 ) then

         packsz = s18ps1

      else

         call setmsg ( 'Unexpected SPK type 18 subtype found in ' // &
                       'type 18 record.'                          )
         call errint ( '#',  subtyp                               )
         call sigerr ( 'SPICE(INVALIDVALUE)'                      )
         return

      end if

!
!     From the number of records, we can compute
!
!        NDIR      The number of directory epochs.
!
!        OFFE      The offset of the first epoch.
!
      ndir = ( nrec - 1 ) / 100
      offe = eaddr - ndir - nrec - msize

!
!     Examine the epochs in forward order, looking for the first
!     epoch greater than or equal to END (or the final epoch,
!     whichever comes first). This epoch corresponds to the last
!     state to be transferred.
!
      rec(2) = 1
      call dafgda ( handle, offe+rec(2), offe+rec(2), data )

      do while  ( rec(2) < nrec  .and.  data(1) < end )
         rec(2) = rec(2) + 1
         call dafgda ( handle, offe+rec(2), offe+rec(2), data )
      end do

!
!     Let WNSZM1 be one less than the window size.
!
!     Make sure that there are WNSZM1/2 additional states to the right
!     of the one having index REC(2), if possible.  If not, take as
!     many states as we can.
!
      wnszm1  =   winsiz - 1

      rec(2)  =   min (  nrec,   rec(2) + wnszm1/2   )

!
!     Now examine the epochs in reverse order, looking for the first
!     epoch less than or equal to BEGIN (or the initial epoch,
!     whichever comes first). This epoch corresponds to the first
!     state to be transferred.
!
      rec(1) = nrec
      call dafgda ( handle, offe+rec(1), offe+rec(1), data )

      do while ( rec(1) > 1  .and.  data(1) > begin )
         rec(1) = rec(1) - 1
         call dafgda ( handle, offe+rec(1), offe+rec(1), data )
      end do

!
!     Make sure that there are WNSZM1/2 additional states to the left
!     of the one having index REC(1), if possible.  If not, take as
!     many states as we can.
!
      rec(1)  =   max (  1,   rec(1) - wnszm1/2   )

!
!     Copy states REC(1) through REC(2) to the output file.
!
      do  i = rec(1), rec(2)
         offset = baddr  -  1  +  (i - 1) * packsz

         call dafgda ( handle, offset+1, offset+packsz, data         )
         call dafada (                                  data, packsz )
      end do

!
!     Copy epochs REC(1) through REC(2) to the output file.
!
      do  i = rec(1), rec(2)
         call dafgda ( handle, offe+i, offe+i, data    )
         call dafada (                         data, 1 )
      end do

!
!     Put every 100'th epoch into the directory, except the last
!     epoch, if that epoch's index would be a multiple of 100.
!
      do  i = rec(1)+99, rec(2)-1, 100
         call dafgda ( handle, offe+i, offe+i, data    )
         call dafada (                         data, 1 )
      end do

!
!     Store subtype, the window size, and the number of
!     records to end the segment.
!
      call dafada ( real( subtyp, wp ),                  1 )
      call dafada ( real( winsiz, wp ),                  1 )
      call dafada ( real( rec(2) - rec(1) + 1, wp ),     1 )

      return
      end
