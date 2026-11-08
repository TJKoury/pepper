!$Procedure ZZCKCV06 ( Private --- C-kernel segment coverage, type 06 )

      subroutine zzckcv06 ( handle, arrbeg, arrend, sclkid, &
                            dc,     tol,    timsys, schedl )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the "window" of coverage of a type 06 C-kernel segment.
!
! $ Required_Reading
!
!     CK
!     DAF
!
! $ Keywords
!
!     CK
!     UTILITY
!     PRIVATE
!
! $ Declarations

      implicit none

      include              'ck06.inc'

      integer(ip), parameter :: lbcell = -5

      integer(ip)           handle
      integer(ip)           arrbeg
      integer(ip)           arrend
      integer(ip)           sclkid
      real(wp)              dc     ( 2 )
      real(wp)              tol
      character(*)          timsys
      real(wp)              schedl ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a C-kernel open for read access.
!     ARRBEG     I   Beginning DAF address.
!     ARREND     I   Ending DAF address.
!     SCLKID     I   ID of SCLK associated with segment.
!     DC         I   D.p. component of CK segment descriptor.
!     TOL        I   Tolerance in ticks.
!     TIMSYS     I   Time system used to represent coverage.
!     SCHEDL    I/O  An initialized window/schedule of interval
!
! $ Detailed_Input
!
!     HANDLE     is the handle of some DAF that is open for reading.
!
!     ARRBEG     is the beginning address of the type 06 segment
!                to be examined.
!
!     ARREND     is the ending address of the type 06 segment.
!
!     SCLKID     is the ID code of the spacecraft clock associated with
!                the object for which the segment contains pointing.
!                This is the ID code used by the SCLK conversion
!                routines.
!
!     DC         is the double precision component of the descriptor of
!                the CK segment. The components are the segment start
!                and stop times.
!
!                Each mini-segment interval is replaced with its
!                intersection with the segment coverage interval
!
!                   [ DC(1), DC(2) ]
!
!                before being expanded by TOL. Mini-segment intervals
!                that don't intersect the segment coverage interval are
!                discarded, even if after expansion by TOL they would
!                have non-empty intersection with the segment coverage
!                interval.
!
!     TOL        is a tolerance value expressed in ticks of the
!                spacecraft clock associated with the segment. After
!                truncation by the segment coverage interval, and
!                before insertion into the coverage window, each
!                non-empty truncated mini-segment interval is expanded
!                by TOL:  the left endpoint of each interval is reduced
!                by TOL and the right endpoint is increased by TOL.
!                Any intervals that overlap as a result of the
!                expansion are merged.
!
!                The coverage window returned when TOL > 0 indicates
!                the coverage provided by the file to the CK readers
!                CKGPAV and CKGP when that value of TOL is passed to
!                them as an input.
!
!     TIMSYS     is a string indicating the time system used in the
!                output coverage window.TIMSYS may have the values:
!
!                   'SCLK'    Elements of SCHEDL are expressed in
!                             encoded SCLK ("ticks"), where the clock
!                             is associated with the object designated
!                             by IDCODE.
!
!                   'TDB'     Elements of SCHEDL are expressed as
!                             seconds past J2000 TDB.
!
!                TIMSYS must be consistent with the system used for
!                the contents of SCHEDL on input, if any.
!
!     SCHEDL     is a schedule (window) of intervals, to which the
!                intervals of coverage for this segment will be added.
!
! $ Detailed_Output
!
!     SCHEDL     the input schedule updated to include the intervals
!                of coverage for this segment. The schedule has
!                been adjusted to account for the provided tolerance
!                value. Coverage lying outside the interval
!
!                   DC(1) - TOL : DC(2) + TOL
!
!                is excluded.
!
!                The elements of SCHEDL are given in the time system
!                indicated by TIMSYS.
!
! $ Parameters
!
!     Several parameters associated with the type 06 C-kernel
!     are utilized to compute the packet size of each subtype.
!     See the include file 'ck06.inc' for details.
!
! $ Exceptions
!
!     1)  The error SPICE(NOTSUPPORTED) is signaled if the subtype of
!         the CK type 06 segment is not recognized.
!
!     2)  Routines in the call tree of this routine may signal errors
!         if insufficient room in SCHEDL exists or other error
!         conditions relating to file access arise.
!
!     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
!         signaled.
!
!     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
!         is signaled.
!
!     5)  If a time conversion error occurs, the error will be
!         diagnosed by a routine in the call tree of this routine.
!
! $ Files
!
!     This routine reads the contents of the file associated with
!     HANDLE to locate coverage intervals.
!
! $ Particulars
!
!     This is a utility routine that determines the intervals
!     of coverage for a type 06 C-kernel segment.
!
! $ Examples
!
!     See CKCOV.
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
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!     W.L. Taber      (JPL)
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-FEB-2014 (NJB) (BVS) (FST) (WLT)
!
!-&

!
!     SPICELIB Functions
!
      logical               eqstr
      logical               failed
      logical               return

!
!     Number of elements in a type 6 mini-segment
!     interval directory:
!
      integer(ip), parameter :: nvdsiz = 100

!
!     Mini-segment epoch directory size:
!
      integer(ip), parameter :: dirsiz = 100

!
!     Type 6 control area size:
!
      integer(ip), parameter :: nsgpar = 2

!
!     Type 6 mini-segment control area size:
!
      integer(ip), parameter :: mctlsz = 4

!
!     Local Variables
!
      real(wp)              begin
      real(wp)              buffer ( mctlsz )
      real(wp)              et
      real(wp)              finish
      real(wp)              ivlbds ( 2 )
      real(wp)              lstepc

      integer(ip)           epaddr
      integer(ip)           i
      integer(ip)           ivlbas
      integer(ip)           minie
      integer(ip)           ndir
      integer(ip)           nintvl
      integer(ip)           nivdir
      integer(ip)           nrec
      integer(ip)           ptrbas

      logical               istdb

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Check tolerance value.
!
      if ( tol < 0.0_wp ) then

         call setmsg ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         call errdp  ( '#',  tol                                      )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                       )
         return

      end if

!
!     Set a logical flag indicating whether the time system is SCLK.
!
      istdb = eqstr ( timsys, 'TDB' )

!
!     Check time system.
!
      if ( .not. istdb ) then

         if (  .not.  eqstr ( timsys, 'SCLK' )  )then

            call setmsg ( 'Time system spec TIMSYS was #; allowed ' // &
                          'values are SCLK and TDB.'                )
            call errch  ( '#',  timsys                              )
            call sigerr ( 'SPICE(INVALIDOPTION)'                    )
            return

         end if

      end if

!
!     Fetch the mini-segment count from the segment.
!
      call dafgda ( handle, arrend, arrend, buffer )

      nintvl = nint( buffer(1) )

!
!     Each mini-segment contributes a coverage interval to the
!     total coverage of the segment. Since mini-segments can
!     contain gaps, we need to examine not only the mini-segment
!     interval bounds but the final epochs of the mini-segments.
!
!     Let IVLBAS be the base address of the mini-segment interval
!     bounds. Let PTRBAS be the base address of the mini-segment
!     pointers.
!
!     First compute PTRBAS. There are NINTVL+1 pointers.
!
      ptrbas = arrend - nsgpar - ( nintvl + 1 )

!
!     Compute the number of mini-segment interval directories.
!     There are NINTVL + 1 interval boundaries, so the directory
!     count is
!
!        (  ( NINTVL + 1 ) - 1  )  /  NVDSIZ
!
      nivdir = nintvl / nvdsiz

!
!     The interval bounds and their directories precede the
!     mini-segment pointers.
!
      ivlbas = ptrbas - nivdir - ( nintvl + 1 )

!
!     Now loop over the mini-segments and find the contribution
!     from each one.
!
      do i = 1,  nintvl
!
!        Find the interval bounds for this mini-segment.
!
         call dafgda ( handle, ivlbas+i, ivlbas+i+1, ivlbds )

         if ( failed() ) return

!
!        Now find the last epoch of this mini-segment, since
!        there could be a gap at the end.
!
!        Find the begin and end pointers for the current
!        mini-segment. Convert these from relative to
!        absolute DAF addresses.
!
         call dafgda ( handle, ptrbas+i, ptrbas+i+1, buffer )

         if ( failed() ) return

         minie = ( arrbeg - 1 )  +  nint( buffer(2) )  -  1

!
!        Fetch the mini-segment's record count NREC.
!
         call dafgda ( handle, minie, minie, buffer )

         if ( failed() ) return

         nrec = nint( buffer(1) )

!
!        Compute the number of epoch directories for this
!        mini-segment.
!
         ndir = ( nrec - 1 ) / dirsiz

!
!        The last epoch precedes the mini-segment control
!        area and the epoch directories.
!
         epaddr = minie - mctlsz - ndir

         call dafgda ( handle, epaddr, epaddr, lstepc )

         if ( failed() ) return

         begin  = ivlbds(1)
!
!        The smaller of LSTEPC and IVLBDS(2) is the
!        end of the mini-segment's coverage.
!
         finish = min ( lstepc, ivlbds(2) )
!
!        Truncate the interval using the segment bounds.
!
         begin   =  max ( begin,  dc(1) )
         finish  =  min ( finish, dc(2) )

!
!        Adjust the interval using the tolerance. Empty
!        intervals *do not get expanded*; this choice is
!        consistent with the type 6 reading algorithm.
!
         if ( begin <= finish ) then

            if ( tol > 0.0_wp ) then

               begin   =  max( begin  - tol, 0.0_wp )
               finish  =       finish + tol

            end if

         end if

!
!        Convert the time to TDB if necessary.
!
         if ( istdb ) then

            call sct2e ( sclkid, begin,  et )
            begin  = et

            call sct2e ( sclkid, finish, et )
            finish = et

            if ( failed() ) return

         end if

!
!        Insert the interval into the window.
!
         if ( begin <= finish ) then

            call wninsd ( begin, finish, schedl )

            if ( failed() ) return

         end if

      end do

      return
      end
