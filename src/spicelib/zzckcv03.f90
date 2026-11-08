!$Procedure ZZCKCV03 ( Private --- C-kernel segment coverage, type 03 )

      subroutine zzckcv03 ( handle,  arrbeg,  arrend, &
                            sclkid,  tol,     timsys,  schedl  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the "window" of coverage of a type 03 C-kernel segment.
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

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      integer(ip)           handle
      integer(ip)           arrbeg
      integer(ip)           arrend
      integer(ip)           sclkid
      real(wp)              tol
      character*(*)         timsys
      real(wp)              schedl ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a C-kernel open for read access
!     ARRBEG     I   Beginning DAF address
!     ARREND     I   Ending DAF address
!     SCLKID     I   ID of SCLK associated with segment.
!     TOL        I   Tolerance in ticks.
!     TIMSYS     I   Time system used to represent coverage.
!     SCHEDL    I/O  An initialized window/schedule of interval
!
! $ Detailed_Input
!
!     HANDLE     is the handle of some DAF that is open for reading.
!
!     ARRBEG     is the beginning address of a type 03 segment
!
!     ARREND     is the ending address of a type 03 segment.
!
!     SCLKID     is the ID code of the spacecraft clock associated with
!                the object for which the segment contains pointing.
!                This is the ID code used by the SCLK conversion
!                routines.
!
!     TOL        is a tolerance value expressed in ticks of the
!                spacecraft clock associated with the segment. Before
!                each interval is inserted into the coverage window,
!                the intervals are expanded by TOL:  the left endpoint
!                of each interval is reduced by TOL and the right
!                endpoint is increased by TOL.  Any intervals that
!                overlap as a result of the expansion are merged.
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
!                of coverage for this segment.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     This routine reads the contents of the file associated with
!     HANDLE to locate coverage intervals.
!
! $ Exceptions
!
!     1) The error SPICE(BADCK3SEGMENT) is signaled if the derived
!        segment length from ARRBEG and ARREND does not match
!        the possible lengths computed from the segment metadata.
!
!     2) Routines in the call tree of this routine may signal errors
!        if insufficient room in SCHEDL exists or other error
!        conditions relating to file access arise.
!
!     3)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
!         signaled.
!
!     4)  If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
!         is signaled.
!
!     5) If a time conversion error occurs, the error will be
!         diagnosed by a routine in the call tree of this routine.
!
! $ Particulars
!
!     This is a utility routine that determines the intervals
!     of coverage for a type 03 C-kernel segment.
!
! $ Examples
!
!     See CKBRIEF's main driver.
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
!-    SPICELIB Version 1.0.0, 03-JAN-2005 (NJB) (FST) (WLT)
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
      real(wp)              buffer ( 2 )
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
      integer(ip)           tickat

      integer(ip)           navsln
      integer(ip)           avsln
      integer(ip)           seglen

      logical               bail
      logical               istdb

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'ZZCKCV03' )
      end if

!
!     Check tolerance value.
!
      if ( tol .lt. 0.0_wp ) then

         call setmsg ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         call errdp  ( '#',  tol                                      )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                       )
         call chkout ( 'ZZCKCV03'                                     )
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
            call chkout ( 'ZZCKCV03'                                )
            return

         end if

      end if

!
!     Get the number of intervals and pointing instances ( records )
!     in this segment, and from that determine the number of respective
!     directory epochs.
!
      call dafgda ( handle, arrend - 1, arrend, buffer )

      invls  = nint ( buffer ( 1 ) )
      nrec   = nint ( buffer ( 2 ) )
      ndir   = (nrec-1)/ 100

!
!     Determine the size of the pointing packets.  This is dependent
!     on whether angular rate data is present in the segment or not.
!     We can determine this with the following computation:
!
!     Assume a record size of 4, i.e. no angular rate data.
!
      navsln = 5*nrec + ndir + invls + (invls-1)/100 + 2

!
!     Assume a record size of 7, i.e. angular rate data.
!
      avsln = 8*nrec + ndir + invls + (invls-1)/100 + 2

!
!     Compute the actual length of the segment.
!
      seglen = arrend - arrbeg + 1

      if ( seglen .eq. navsln ) then
         rsize = 4
      else if ( seglen .eq. avsln ) then
         rsize = 7
      else
         call setmsg ( 'The requested segment in file # reports a ' &
         //            'length of # d.p. numbers, but the metadata ' &
         //            'in the segment indicates the length must ' &
         //            'either be # (no angular rate data) or # ' &
         //            '(angular rate data). Perhaps the segment ' &
         //            'is not type 3?'                              )
         call errhan ( '#', handle                                   )
         call errint ( '#', seglen                                   )
         call errint ( '#', navsln                                   )
         call errint ( '#', avsln                                    )
         call sigerr ( 'SPICE(BADCK3SEGMENT)'                        )
         call chkout ( 'ZZCKCV03'                                    )
         return
      end if

!
!     Recall that the segment is layed out as:
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

      do while ( tick .lt. start .and. tickat .lt. lsttik )
         tickat = tickat + 1
         call dafgda ( handle, tickat, tickat, tick )
      end do

!
!     If we did not find a TICK at least as big as START, we can
!     just return now.
!
      if ( tick .lt. start ) then
         call chkout ( 'ZZCKCV03' )
         return
      end if


      bail = .false.

      do while (       ( intat  .le.  lstint ) &
                 .and. ( tickat .le.  lsttik ) &
                 .and. (        .not. bail   )  )
!
!        At this point, we have an interval that begins at START
!        and ends at FINISH (unless of course we never found a "good"
!        TICK to start with.)
!
         begin  = start

!
!        If the start of the interval was the start of the LAST
!        interval available, we can short cut the remainder of the
!        reads.
!
         if ( intat .eq. lstint ) then

            call dafgda ( handle, lsttik, lsttik, finish )

            bail = .true.
!
!           The routine will return at the end of this loop
!           iteration.  But first, we may have to update BEGIN
!           and FINISH, depending on the values of TOL and TIMSYS,
!           and we have to insert these values into SCHEDL.
!           We'll carry out these tasks at the end of this IF block.
!

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
            do while ( tick .lt. start .and. tickat .lt. lsttik )
               finish = tick
               tickat = tickat + 1
               call dafgda ( handle, tickat, tickat, tick  )
            end do

!
!           A structurally correct CK-3 segment should never allow the
!           next test to pass, but it's just easier to check than
!           police the writers of C-kernels.  The only way to get into
!           the block below is if TICKAT .EQ. LSTTIK
!
            if ( tick .lt. start ) then
               finish = tick
               tickat = tickat + 1
            end if

         end if

!
!        Adjust the interval using the tolerance.
!
         if ( tol .gt. 0.0_wp ) then

            begin   =  max( begin  - tol, 0.0_wp )
            finish  =       finish + tol

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
         call wninsd ( begin, finish, schedl )

      end do

      call chkout ( 'ZZCKCV03' )
      return
      end
