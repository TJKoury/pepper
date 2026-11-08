!$Procedure ZZCKCV02 ( Private --- C-kernel segment coverage, type 02 )

      subroutine zzckcv02 ( handle,  arrbeg,  arrend, &
                            sclkid,  tol,     timsys,  schedl  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the "window" of coverage of a type 02 C-kernel segment.
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

      integer               lbcell
      parameter           ( lbcell = -5 )

      integer               handle
      integer               arrbeg
      integer               arrend
      integer               sclkid
      double precision      tol
      character*(*)         timsys
      double precision      schedl ( lbcell:* )

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
!     ARRBEG     is the beginning address of a type 02 segment
!
!     ARREND     is the ending address of a type 02 segment.
!
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
!     1) Routines in the call tree of this routine may signal errors
!        if insufficient room in SCHEDL exists or other error
!        conditions relating to file access arise.
!
!     2) If TOL is negative, the error SPICE(VALUEOUTOFRANGE) is
!        signaled.
!
!     3) If TIMSYS is not recognized, the error SPICE(INVALIDOPTION)
!        is signaled.
!
!     4) If a time conversion error occurs, the error will be
!        diagnosed by a routine in the call tree of this routine
!
! $ Particulars
!
!     This is a utility routine that determines the intervals
!     of coverage for a type 02 C-kernel segment.
!
! $ Examples
!
!     None.
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
!     B.V. Semenov    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 03-JAN-2005 (NJB) (FST) (WLT) (BVS)
!
!-&

!
!     SPICELIB Functions
!
      logical               eqstr
      logical               return

!
!     Local Parameters
!
      integer               psize
      parameter           ( psize = 8 )

      integer               bsize
      parameter           ( bsize = 100 )

!
!     Local Variables
!
      double precision      begin
      double precision      et
      double precision      finish
      double precision      first ( bsize )
      double precision      last  ( bsize )

      integer               arrsiz
      integer               begat
      integer               endat
      integer               get
      integer               got
      integer               i
      integer               nrec

      logical               istdb

!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      else
         call chkin ( 'ZZCKCV02' )
      end if

!
!     Check tolerance value.
!
      if ( tol .lt. 0.d0 ) then

         call setmsg ( 'Tolerance must be non-negative; actual value '// &
                       'was #.'                                       )
         call errdp  ( '#',  tol                                      )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                       )
         call chkout ( 'ZZCKCV02'                                     )
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
            call chkout ( 'ZZCKCV02'                                )
            return

         end if

      end if

!
!     Determine the size of the array and the number of records
!     in it.
!
      arrsiz = arrend - arrbeg + 1
      nrec   = nint (  ( 100.d0 * (dble(arrsiz)) + 1.d0 ) / 1001.d0  )

!
!     The variable GOT tells us how many time endpoints we've
!     gotten so far.
!
      got = 0

      do while ( got .lt. nrec )

         get = min ( bsize, nrec - got )

         begat = arrbeg + nrec*psize + got
         endat = arrbeg + nrec*psize + nrec + got

!
!        Retrieve the list next list of windows.
!
         call dafgda ( handle, begat, begat + get - 1, first )
         call dafgda ( handle, endat, endat + get - 1, last  )

!
!        Insert the coverage intervals into the schedule.
!
         do i = 1, get
!
!           Adjust the interval using the tolerance.
!
            begin  = first(i)
            finish = last(i)

            if ( tol .gt. 0.d0 ) then

               begin   =  max( begin  - tol, 0.d0 )
               finish  =       finish + tol

            end if

!
!           Convert the time to TDB if necessary.
!
            if ( istdb ) then

               call sct2e ( sclkid, begin,  et )
               begin  = et

               call sct2e ( sclkid, finish, et )
               finish = et

            end if

            call wninsd ( begin, finish, schedl )

         end do

         got = got + get

      end do

      call chkout ( 'ZZCKCV02' )
      return
      end
