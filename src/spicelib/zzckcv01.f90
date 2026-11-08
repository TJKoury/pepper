!$Procedure ZZCKCV01 ( Private --- C-kernel segment coverage, type 01 )

      subroutine zzckcv01 ( handle,  arrbeg,  arrend, &
                            sclkid,  tol,     timsys,  schedl  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the "window" of coverage of a type 01 C-kernel segment.
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

      integer(ip), parameter :: lbcell = -5

      integer(ip)           handle
      integer(ip)           arrbeg
      integer(ip)           arrend
      integer(ip)           sclkid
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
!     TOL        I   Tolerance in ticks.
!     TIMSYS     I   Time system used to represent coverage.
!     SCHEDL    I/O  An initialized window/schedule of interval
!
! $ Detailed_Input
!
!     HANDLE     is the handle of some DAF that is open for reading.
!
!     ARRBEG     is the beginning address of a type 01 segment
!
!     ARREND     is the ending address of a type 01 segment.
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
!                of coverage for this segment. Since type 01 segments,
!                don't have interpolation intervals, each epoch
!                associated with a pointing instance is treated as a
!                singleton interval.
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
!     1) The error SPICE(BADCK1SEGMENT) is signaled if the derived
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
!C
! $ Particulars
!
!     This is a utility routine that determines the intervals of
!     coverage for a type 01 C-kernel segment. Since type 01 segments,
!     don't have interpolation intervals, each epoch associated with a
!     pointing instance is treated as a singleton interval.
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
!     W.L. Taber      (JPL)
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SUPPORT Version 1.0.0, 03-JAN-2005 (WLT)(NJB)(BVS)
!
!        Initial version.
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
      integer(ip), parameter :: bufsiz = 100

      integer(ip), parameter :: qavsiz = 7

      integer(ip), parameter :: qsiz = 4

!
!     Local Variables
!
      real(wp)              begin
      real(wp)              buffer ( bufsiz )
      real(wp)              et
      real(wp)              finish

      integer(ip)           i
      integer(ip)           n
      integer(ip)           nrec
      integer(ip)           offset
      integer(ip)           psiz
      integer(ip)           remain
      integer(ip)           tbase
      integer(ip)           navsln
      integer(ip)           avsln
      integer(ip)           seglen

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
!     The coverage window is the set of singleton intervals consisting
!     of the epochs of the pointing records. We'll need to find the
!     epochs.
!
!     First, get the number of records in this segment.
!
      call dafgda ( handle, arrend, arrend, buffer )

      nrec = int( buffer(1) )

!
!     Determine the size of the pointing packets. This is dependent
!     on whether angular rate data is present in the segment or not.
!     We can determine this with the following computation:
!
!     Assume a record size of 4, i.e. no angular rate data.
!
      navsln = 5 * nrec + ( nrec - 1 ) / 100 + 1

!
!     Assume a record size of 7, i.e. angular rate data.
!
      avsln  = 8 * nrec + ( nrec - 1 ) / 100 + 1

!
!     Compute the actual length of the segment.
!
      seglen = arrend - arrbeg + 1

      if ( seglen == navsln ) then
         psiz = qsiz
      else if ( seglen == avsln ) then
         psiz = qavsiz
      else
         call setmsg ( 'The requested segment in file # reports a ' &
         //            'length of # d.p. numbers, but the metadata ' &
         //            'in the segment indicates the length must ' &
         //            'either be # (no angular rate data) or # ' &
         //            '(angular rate data). Perhaps the segment ' &
         //            'is not type 1?'                              )
         call errhan ( '#', handle                                   )
         call errint ( '#', seglen                                   )
         call errint ( '#', navsln                                   )
         call errint ( '#', avsln                                    )
         call sigerr ( 'SPICE(BADCK1SEGMENT)'                        )
         return
      end if

!
!     The epochs start right after the pointing data. Let TBASE be the
!     address preceding the first epoch.
!
      tbase  = arrbeg  +  nrec * psiz  -  1

!
!     Grab the epochs. Make a singleton interval out of each one; add
!     the interval to the coverage window.
!
!     For efficiency, we'll read the epochs into a buffer of length
!     BUFSIZ.
!
      remain = nrec
      offset = 0

      do while ( remain > 0 )

!
!        Buffer the next set of epochs.
!
         n  =  min ( bufsiz, remain )

         call dafgda ( handle, &
                       tbase + offset + 1, &
                       tbase + offset + n, &
                       buffer              )

!
!        Insert the current batch of N singleton intervals.
!
         do i = 1, n

            begin   =  buffer(i)
            finish  =  buffer(i)


            if ( tol > 0.0_wp ) then
!
!              Adjust the interval using the tolerance.
!
               begin   =  max( begin  - tol, 0.0_wp )
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

         offset = offset + n
         remain = remain - n

      end do

      return
      end
