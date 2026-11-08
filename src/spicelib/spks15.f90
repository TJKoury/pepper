!$Procedure SPKS15 ( S/P Kernel, subset, type 15 )

      subroutine spks15 ( handle, baddr, eaddr, begin, end )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Extract a subset of the data in an SPK segment of type 15
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
!     routine SPKSUB. It transfers a subset of a type 15 SPK data
!     segment to a properly initialized segment of a second SPK file.
!
!     The exact structure of a segment of data type 15 is described
!     in the section on type 15 in the SPK Required Reading.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.0, 07-NOV-1994 (WLT)
!
!-&


! $ Index_Entries
!
!     subset type_15 SPK segment
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      integer(ip)           nitems
      parameter           ( nitems = 16 )

      real(wp)              data     ( nitems )



!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     See whether there's any work to do; return immediately if not.
!
      if (  begin  >  end  ) return


!
!     This couldn't be much easier.  First copy the entire
!     type 15 segment out of the file.
!
      call dafgda ( handle, baddr, eaddr, data )

!
!     Now write the data into the output file.
!
      call dafada ( data, nitems )


      return
      end
