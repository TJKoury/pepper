!$Procedure SPKS09 ( S/P Kernel, subset, type 9 )

      subroutine spks09 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Extract a subset of the data in an SPK segment of type 9
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
!     2)  If BEGIN > END, no data is written to the target file.
!
! $ Files
!
!     Data is extracted from the file connected to the input
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
!     It transfers a subset of a type 09 SPK data segment to
!     a properly initialized segment of a second SPK file.
!
!     The exact structure of a segment of data type 09 is described
!     in the section on type 09 in the SPK Required Reading.
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
!     J.M. Lynch         (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 2.0.0, 27-AUG-1994 (NJB)
!
!        Bug fix: Sufficient bracketing states are now included in the
!        output segment to ensure duplication of states given by source
!        segment.
!
!        Test for null subset simplified.
!
!-    SPICELIB Version 1.0.0, 08-AUG-1993 (NJB) (JML) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     subset type_9 SPK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      real(wp)              data     ( 6 )

      integer(ip)           degree
      integer(ip)           i
      integer(ip)           ndir
      integer(ip)           nrec
      integer(ip)           offe
      integer(ip)           offset
      integer(ip)           rec      ( 2 )


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPKS09' )
      end if

!
!     See whether there's any work to do; return immediately if not.
!
      if (  begin  .gt.  end  ) then
         call chkout ( 'SPKS09' )
         return
      end if

!
!     Get the number of records in the segment.  Get the polynomial
!     degree as well.
!
      call dafgda ( handle, eaddr-1, eaddr, data )

      degree = nint ( data(1) )
      nrec   = nint ( data(2) )

!
!     From the number of records, we can compute
!
!        NDIR      The number of directory epochs.
!
!        OFFE      The offset of the first epoch.
!
      ndir = ( nrec - 1 ) / 100
      offe = eaddr - ndir - nrec - 2

!
!     Examine the epochs in forward order, looking for the first
!     epoch greater than or equal to END (or the final epoch,
!     whichever comes first). This epoch corresponds to the last
!     state to be transferred.
!
      rec(2) = 1
      call dafgda ( handle, offe+rec(2), offe+rec(2), data )

      do while  ( rec(2) .lt. nrec  .and.  data(1) .lt. end )
         rec(2) = rec(2) + 1
         call dafgda ( handle, offe+rec(2), offe+rec(2), data )
      end do

!
!     Make sure that there are DEGREE/2 additional states to the right
!     of the one having index REC(2), if possible.  If not, take as
!     many states as we can.
!
      rec(2)  =   min (  nrec,   rec(2) + degree/2   )

!
!     Make sure that REC(2) is large enough so that there are are at
!     least DEGREE+1 states in the segment.
!
      rec(2)  =   max (  rec(2),  degree + 1 )

!
!     Now examine the epochs in reverse order, looking for the first
!     epoch less than or equal to BEGIN (or the initial epoch,
!     whichever comes first). This epoch corresponds to the first
!     state to be transferred.
!
      rec(1) = nrec
      call dafgda ( handle, offe+rec(1), offe+rec(1), data )

      do while ( rec(1) .gt. 1  .and.  data(1) .gt. begin )
         rec(1) = rec(1) - 1
         call dafgda ( handle, offe+rec(1), offe+rec(1), data )
      end do

!
!     Make sure that there are DEGREE/2 additional states to the left
!     of the one having index REC(1), if possible.  If not, take as
!     many states as we can.
!
      rec(1)  =   max (  1,   rec(1) - degree/2   )

!
!     Make sure that REC(1) is small enough so that there are are at
!     least DEGREE+1 states in the segment.
!
      rec(1)  =   min (  rec(1),  nrec - degree  )

!
!     Copy states REC(1) through REC(2) to the output file.
!
      do  i = rec(1), rec(2)
         offset = baddr  -  1  +  (i - 1) * 6

         call dafgda ( handle, offset+1, offset+6, data    )
         call dafada (                             data, 6 )
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
!     Store the polynomial degree and the number of records
!     to end the segment.
!
      call dafada ( real( degree, wp ),                  1 )
      call dafada ( real( rec(2) - rec(1) + 1, wp ),     1 )

      call chkout ( 'SPKS09' )
      return
      end
