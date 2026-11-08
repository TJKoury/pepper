!$Procedure ZZCKCV04 ( Private --- C-kernel segment coverage, type 04 )

      subroutine zzckcv04 ( handle,  arrbeg,  arrend, &
                            sclkid,  tol,     timsys,  schedl  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Determine the "window" of coverage of a type 04 C-kernel segment.
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

      include              'ckparam.inc'

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
!     CK4RSZ     P   C-kernel Type 04 Maximum Record Size
!
! $ Detailed_Input
!
!     HANDLE     is the handle of some DAF that is open for reading.
!
!     ARRBEG     is the beginning address of a type 04 segment
!
!     ARREND     is the ending address of a type 04 segment.
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
!     CK4RSZ     is the maximum length of a CK4 record (with angular
!                velocity). Defined in the include file 'ckparam.inc'.
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
!        diagnosed by a routine in the call tree of this routine.
!
! $ Particulars
!
!     This is a utility routine that determines the intervals
!     of coverage for a type 04 C-kernel segment.
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
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 30-DEC-2004 (NJB) (FST)
!
!-&

!
!     SPICELIB Functions
!
      integer(ip)           intmax
      logical               eqstr
      logical               return

!
!     Local Parameters
!
      integer(ip), parameter :: nd = 2

      integer(ip), parameter :: ni = 6

!
!     Local Variables
!
      real(wp)              dc     ( nd )
      real(wp)              descr  ( 5 )
      real(wp)              et
      real(wp)              left
      real(wp)              right
      real(wp)              values ( ck4rsz )

      integer(ip)           ends   ( 2 )
      integer(ip)           i
      integer(ip)           ic     ( ni )
      integer(ip)           nrec

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
!     Build a descriptor record that satisfies the requirements
!     of CKNR04 and SGFPKT.
!
!     Note: This is a hack dependent on the implementation of
!     the generic segments routines.  But for C-kernels it
!     should always work, as ND and NI aren't changing any
!     time soon.
!
      ic ( 1 ) = intmax()
      ic ( 2 ) = intmax()
      ic ( 3 ) = 4
      ic ( 4 ) = intmax()
      ic ( 5 ) = arrbeg
      ic ( 6 ) = arrend

      dc ( 1 ) = 0.0_wp
      dc ( 2 ) = 0.0_wp

      call dafps ( nd, ni, dc, ic, descr )

!
!     Determine the number of records in the array.
!
      call cknr04 ( handle, descr, nrec )

      do i = 1, nrec

!
!        Extract each packet of pointing coefficients.
!
         call sgfpkt ( handle, descr, i, i, values, ends )

!
!        Compute the left and right end points of the interval
!        of coverage related to this packet.
!
         left  = values ( 1 ) - values ( 2 )
         right = values ( 1 ) + values ( 2 )

!
!        Adjust the interval using the tolerance.
!
         if ( tol > 0.0_wp ) then

            left   =  max( left  - tol, 0.0_wp )
            right  =       right + tol

         end if

!
!        Convert the time to TDB if necessary.
!
         if ( istdb ) then

            call sct2e ( sclkid, left,  et )
            left  = et

            call sct2e ( sclkid, right, et )
            right = et

         end if

!
!        Store the results in the schedule.
!
         call wninsd ( left, right, schedl )

      end do

      return
      end
