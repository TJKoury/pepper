!$Procedure ZZCKCV05 ( Private --- C-kernel segment coverage, type 05 )

      subroutine zzckcv05 ( handle,  arrbeg,  arrend,  sclkid, &
                            dc,      tol,     timsys,  schedl  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the "window" of coverage of a type 05 C-kernel segment.
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

      include              'ck05.inc'

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

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
!     HANDLE     I   Handle of a C-kernel open for read access
!     ARRBEG     I   Beginning DAF address
!     ARREND     I   Ending DAF address
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
!     ARRBEG     is the beginning address of a type 05 segment
!
!     ARREND     is the ending address of a type 05 segment.
!
!     SCLKID     is the ID code of the spacecraft clock associated with
!                the object for which the segment contains pointing.
!                This is the ID code used by the SCLK conversion
!                routines.
!
!     DC         is the double precision component of the descriptor of
!                the CK segment.  The components are the segment start
!                and stop times.
!
!                Each interpolation interval is replaced with its
!                intersection with the segment coverage interval
!
!                   [ DC(1), DC(2) ]
!
!                before being expanded by TOL. Interpolation intervals
!                that don't intersect the segment coverage interval are
!                discarded, even if after expansion by TOL they would
!                have non-empty intersection with the segment coverage
!                interval.
!
!     TOL        is a tolerance value expressed in ticks of the
!                spacecraft clock associated with the segment. After
!                truncation by the segment coverage interval, and
!                before insertion into the coverage window, each
!                non-empty truncated interpolation interval is expanded
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
!
!     TIMSYS     is a string indicating the time system used in the
!                output coverage window.  TIMSYS may have the values:
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
!
!     SCHEDL     is a schedule (window) of intervals, to which the
!                intervals of coverage for this segment will be added.
!
! $ Detailed_Output
!
!     SCHEDL     the input schedule updated to include the intervals
!                of coverage for this segment.  The schedule has
!                been adjusted to account for the provided tolerance
!                value.  Coverage lying outside the interval
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
!     Several parameters associated with the type 05 C-kernel
!     are utilized to compute the packet size of each subtype.
!     See the include file 'ck05.inc' for details.
!
! $ Files
!
!     This routine reads the contents of the file associated with
!     HANDLE to locate coverage intervals.
!
! $ Exceptions
!
!     1)  The error SPICE(NOTSUPPORTED) is signaled if the subtype of
!         the CK type 05 segment is not recognized.
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
! $ Particulars
!
!     This is a utility routine that determines the intervals
!     of coverage for a type 05 C-kernel segment.
!
! $ Examples
!
!     See CKCOV.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     W.L. Taber      (JPL)
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 07-JAN-2005 (NJB) (FST) (WLT)
!
!-&

!
!     SPICELIB Functions
!
      logical               eqstr
      logical               return

!
!     Local Variables
!
      real(wp)              begin
      real(wp)              buffer ( 4 )
      real(wp)              et
      real(wp)              finish
      real(wp)              start
      real(wp)              tick

      integer(ip)           intat
      integer(ip)           intbeg
      integer(ip)           invls
      integer(ip)           lstint
      integer(ip)           lsttik
      integer(ip)           ndir
      integer(ip)           nrec
      integer(ip)           rsize
      integer(ip)           subtyp
      integer(ip)           tickat

      logical               bail
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
!     Set a logical flag indicating whether the time systm is SCLK.
!
      istdb  = eqstr ( timsys, 'TDB' )

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
!     Get the meta-data associated with this segment that we
!     require to produce the schedule.
!
!     BUFFER(1) = Subtype Code
!     BUFFER(2) = Window Size
!     BUFFER(3) = Number of Interpolation Intervals
!     BUFFER(4) = Number of Packets
!
      call dafgda ( handle, arrend - 3, arrend, buffer )

      subtyp = nint ( buffer ( 1 ) )
      invls  = nint ( buffer ( 3 ) )
      nrec   = nint ( buffer ( 4 ) )
      ndir   = (nrec-1)/ 100

!
!     Compute the packet size.  This requires parameters listed
!     in the include file 'ck05.inc' and is based on the subtype.
!
      if      ( subtyp == c05tp0 ) then
         rsize = c05ps0
      else if ( subtyp == c05tp1 ) then
         rsize = c05ps1
      else if ( subtyp == c05tp2 ) then
         rsize = c05ps2
      else if ( subtyp == c05tp3 ) then
         rsize = c05ps3
      else
         call setmsg ( 'CK type 5 subtype <#> is not supported.' )
         call errint ( '#', buffer(1)                            )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                     )
         return
      end if

!
!     Recall that the segment is layed out as:
!
!
!       +------------------------------+
!       |                              |
!       |  Pointing                    |
!       |                              |
!       +------------------------------+
!       |                        |
!       |  SCLK times            |
!       |                        |
!       +------------------------+
!       |                        |
!       |  SCLK directory        |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Interval start times  |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Start times directory |
!       |                        |
!       +------------------------+
!       |    Seconds per tick    |
!       +------------------------+
!       |      Subtype code      |
!       +------------------------+
!       |      Window size       |
!       +------------------------+
!       |                        |
!       |  Number of intervals   |
!       |                        |
!       +------------------------+
!       |                        |
!       |  Number of pointing    |
!       |      instances         |
!       |                        |
!       +------------------------+
!

      tickat = arrbeg + rsize*nrec
      lsttik = tickat + nrec - 1
      intbeg = arrbeg + rsize*nrec + nrec + ndir
      intat  = intbeg
      lstint = intbeg + invls - 1

      call dafgda ( handle, intat,  intat,   start )
      call dafgda ( handle, tickat, tickat,  tick  )

      do while ( tick < start .and. tickat < lsttik )
         tickat = tickat + 1
         call dafgda ( handle, tickat, tickat, tick )
      end do

!
!     If we did not find a TICK at least as big as START, we can
!     just return now.
!
      if ( tick < start ) return


      bail = .false.

      do while (       ( intat  <=  lstint ) &
                 .and. ( tickat <=  lsttik ) &
                 .and. (        .not. bail   )  )
!
!        At this point, we have an interval that begins at START
!        and ends at FINISH (unless of course we never found a "good"
!        TICK to start with.)
!
         begin = start

!
!        If the start of the interval was the start of the LAST
!        interval available, we can short cut the remainder of the
!        reads.
!
         if ( intat == lstint ) then

            call dafgda ( handle, lsttik, lsttik, finish )

            bail = .true.
!
!           The routine will return at the end of this loop
!           iteration.  But first, we may have to update BEGIN
!           and FINISH, depending on the values of TOL and TIMSYS,
!           and we have to insert these values into SCHEDL.
!           We'll carry out these tasks at the end of this IF block.


         else
!
!           This is the expected case.  Get the start of the next
!           interval.
!
            intat  = intat  + 1
            call dafgda ( handle, intat, intat, start )

!
!           Read forward from the last tick until we reach the
!           START of the next interval or until we run out of TICKS.
!
            do while ( tick < start .and. tickat < lsttik )
               finish = tick
               tickat = tickat + 1
               call dafgda ( handle, tickat, tickat, tick  )
            end do

!
!           A structurally correct CK-5 segment should never allow the
!           next test to pass, but it's just easier to check than
!           police the writers of C-kernels.  The only way to get into
!           the block below is if TICKAT .EQ. LSTTIK
!
            if ( tick < start ) then
               finish = tick
               tickat = tickat + 1
            end if

         end if

!
!        Truncate the interval using the segment bounds.
!
         begin   =  max ( begin,  dc(1) )
         finish  =  min ( finish, dc(2) )

!
!        Adjust the interval using the tolerance.  Empty
!        intervals *do not get expanded*; this choice is
!        consistent with the type 5 reading algorithm.
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

         end if

!
!        Insert the interval into the window.
!
         if ( begin <= finish ) then
            call wninsd ( begin, finish, schedl )
         end if

      end do


      return
      end
