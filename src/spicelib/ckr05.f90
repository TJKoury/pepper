!$Procedure CKR05 ( Read CK record from segment, type 05 )

      subroutine ckr05 ( handle, descr, sclkdp, tol, needav, &
                         record, found                       )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single CK data record from a segment of type 05
!     (MEX/Rosetta Attitude file interpolation).
!
! $ Required_Reading
!
!     CK
!
! $ Keywords
!
!     POINTING
!
! $ Declarations

      implicit none

      include 'ck05.inc'
      include 'ckparam.inc'

      integer(ip)           handle
      real(wp)              descr    ( 5 )
      real(wp)              sclkdp
      real(wp)              tol
      logical               needav
      real(wp)              record   ( * )
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     SCLKDP     I   Pointing request time.
!     TOL        I   Lookup tolerance.
!     NEEDAV     I   Angular velocity flag.
!     RECORD     O   Data record.
!     FOUND      O   Flag indicating whether record was found.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle and segment descriptor for
!              a CK segment of type 05.
!
!     SCLKDP   is an encoded spacecraft clock time indicating
!              the epoch for which pointing is desired.
!
!     TOL      is a time tolerance, measured in the same units as
!              encoded spacecraft clock.
!
!              When SCLKDP falls within the bounds of one of the
!              interpolation intervals then the tolerance has no
!              effect because pointing will be returned at the
!              request time.
!
!              However, if the request time is not in one of the
!              intervals, then the tolerance is used to determine
!              if pointing at one of the interval endpoints should
!              be returned.
!
!     NEEDAV   is .TRUE. if angular velocity is requested.
!
! $ Detailed_Output
!
!     RECORD   is a set of data from the specified segment which,
!              when evaluated at epoch SCLKDP, will give the
!              attitude and angular velocity of some body, relative
!              to the reference frame indicated by DESCR.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | evaluation epoch     |
!                 +----------------------+
!                 | subtype code         |
!                 +----------------------+
!                 | number of packets (n)|
!                 +----------------------+
!                 | nominal SCLK rate    |
!                 +----------------------+
!                 | packet 1             |
!                 +----------------------+
!                 | packet 2             |
!                 +----------------------+
!                             .
!                             .
!                             .
!                 +----------------------+
!                 | packet n             |
!                 +----------------------+
!                 | epochs 1--n          |
!                 +----------------------+
!
!              The packet size is a function of the subtype code.
!              All packets in a record have the same size.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine follows the pattern established in the lower-numbered
!     CK data type readers of not explicitly performing error
!     diagnoses. Exceptions are listed below nonetheless.
!
!     1)  If the input HANDLE does not designate a loaded CK file, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the segment specified by DESCR is not of data type 05,
!         the error SPICE(WRONGCKTYPE) is signaled.
!
!     3)  If the input SCLK value is not within the range specified
!         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
!         is signaled.
!
!     4)  If the window size is non-positive or greater than the
!         maximum allowed value, the error SPICE(INVALIDVALUE) is
!         signaled.
!
!     5)  If the window size is not compatible with the segment
!         subtype, the error SPICE(INVALIDVALUE) is signaled.
!
!     6)  If the segment subtype is not recognized, the error
!         SPICE(NOTSUPPORTED) is signaled.
!
!     7)  If the tolerance is negative, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the CK Required Reading file for a description of the
!     structure of a data type 05 segment.
!
! $ Examples
!
!     The data returned by the CKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the CKRxx
!     routines might be used to "dump" and check segment data for a
!     particular epoch.
!
!     The search performed here does not mimic the behavior of the CK
!     reader APIs CKGP and CKGPAV, which continue searching when an
!     applicable segment doesn't satisfy a pointing request. See the CK
!     Required reading for details.
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL CKBSS ( INST,   SCLKDP, TOL,   NEEDAV )
!           CALL CKSNS ( HANDLE, DESCR,  SEGID, SFND   )
!
!           IF ( .NOT. SFND ) THEN
!              [Handle case of pointing not being found]
!           END IF
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 05 ) THEN
!
!              CALL CKR05 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
!          .                RECORD, FOUND                       )
!
!              IF ( .NOT. FOUND ) THEN
!                 [Handle case of pointing not being found]
!              END IF
!
!              [Look at the RECORD data]
!                  .
!                  .
!                  .
!           END IF
!
! $ Restrictions
!
!     1)  Correctness of inputs must be ensured by the caller of
!         this routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 06-JUL-2021 (NJB) (JDR)
!
!        Corrected code example: removed comment character preceding
!        CKBSS call. Added note regarding difference between this
!        search and those performed by the CK reader APIs CKGP and
!        CKGPAV.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 27-JAN-2014 (NJB)
!
!        Increased MAXDEG to 23 for compatibility with CK type 6.
!
!-    SPICELIB Version 1.1.0, 06-SEP-2002 (NJB)
!
!-&


! $ Index_Entries
!
!     read record from type_5 CK segment
!
!-&


!
!     SPICELIB functions
!
      real(wp)              brcktd
      real(wp)              dpmax

      integer(ip)           lstled
      integer(ip)           lstltd

      logical               odd
      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           nd
      parameter           ( nd     = 2 )

      integer(ip)           ni
      parameter           ( ni     = 6 )

      integer(ip)           dirsiz
      parameter           ( dirsiz = 100 )

      integer(ip)           pbufsz
      parameter           ( pbufsz = dirsiz + 1 )

      integer(ip)           ctrlsz
      parameter           ( ctrlsz = 5 )

      integer(ip)           parsiz
      parameter           ( parsiz = 4 )

      integer(ip)           sbufsz
      parameter           ( sbufsz = dirsiz + 3 )

!
!     Maximum polynomial degree:
!
      integer(ip)           maxdeg
      parameter           ( maxdeg = 23 )

!
!     Local variables
!
      real(wp)              contrl ( ctrlsz )
      real(wp)              dc     ( nd )
      real(wp)              hepoch
      real(wp)              lepoch
      real(wp)              nstart
      real(wp)              nnstrt
      real(wp)              pbuffr ( pbufsz )
      real(wp)              prevn
      real(wp)              prevnn
      real(wp)              prevs
      real(wp)              rate
      real(wp)              sbuffr ( sbufsz )
      real(wp)              start
      real(wp)              t

      integer(ip)           pbegix
      integer(ip)           begin
      integer(ip)           bufbas
      integer(ip)           dirbas
      integer(ip)           end
      integer(ip)           pendix
      integer(ip)           first
      integer(ip)           high
      integer(ip)           i
      integer(ip)           ic     ( ni )
      integer(ip)           j
      integer(ip)           last
      integer(ip)           lbeg
      integer(ip)           lend
      integer(ip)           lhand
      integer(ip)           lsize
      integer(ip)           low
      integer(ip)           maxwnd
      integer(ip)           n
      integer(ip)           nidir
      integer(ip)           npdir
      integer(ip)           nints
      integer(ip)           npread
      integer(ip)           nsrch
      integer(ip)           nsread
      integer(ip)           packsz
      integer(ip)           pgroup
      integer(ip)           remain
      integer(ip)           rsize
      integer(ip)           sbegix
      integer(ip)           sendix
      integer(ip)           sgroup
      integer(ip)           subtyp
      integer(ip)           timbas
      integer(ip)           type
      integer(ip)           wndsiz
      integer(ip)           wstart

!
!     Saved variables
!
      save                  prevs
      save                  prevn
      save                  prevnn
      save                  lhand
      save                  lbeg
      save                  lend

!
!     Initial values
!
      data    lbeg    / -1    /
      data    lend    / -1    /
      data    lhand   / 0     /
      data    prevn   / -1.0_wp /
      data    prevnn  / -1.0_wp /
      data    prevs   / -1.0_wp /


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'CKR05' )

!
!     No pointing found so far.
!
      found = .false.

!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      call dafus ( descr, 2, 6, dc, ic )

      type  = ic( 3 )
      begin = ic( 5 )
      end   = ic( 6 )

!
!     Make sure that this really is a type 05 data segment.
!
      if ( type .ne. 5 ) then
         call setmsg( 'You are attempting to locate type * ' // &
                      'data in a type 5 data segment.'       )
         call errint( '*',  type                             )
         call sigerr( 'SPICE(WRONGCKTYPE)'                   )
         call chkout( 'CKR05'                                )
         return
      end if

!
!     Check the tolerance value.
!
      if ( tol .lt. 0.0_wp ) then
         call setmsg( 'Tolerance must be non-negative but '  // &
                      'was actually *.'                      )
         call errdp ( '*',  tol                              )
         call sigerr( 'SPICE(VALUEOUTOFRANGE)'               )
         call chkout( 'CKR05'                                )
         return
      end if

!
!     Check the request time and tolerance against the bounds in
!     the segment descriptor.
!
      if (       ( (sclkdp + tol) .lt. dc(1) ) &
           .or.  ( (sclkdp - tol) .gt. dc(2) )  )  then
!
!        The request time is too far outside the segment's coverage
!        interval for any pointing to satisfy the request.
!
         call chkout ( 'CKR05' )
         return

      end if

!
!     Set the request time to use for searching.
!
      t = brcktd ( sclkdp, dc(1), dc(2) )

!
!     From this point onward, we assume the segment was constructed
!     correctly.  In particular, we assume:
!
!        1)  The segment descriptor's time bounds are in order and are
!            distinct.
!
!        2)  The epochs in the segment are in strictly increasing
!            order.
!
!
!        3)  The interpolation interval start times in the segment are
!            in strictly increasing order.
!
!
!        4)  The degree of the interpolating polynomial specified by
!            the segment is at least 1 and is no larger than MAXDEG.
!
!
      call dafgda ( handle, end-ctrlsz+1, end, contrl )

!
!     Check the FAILED flag just in case HANDLE is not attached to
!     any DAF file and the error action is not set to ABORT.  We
!     do this only after the first call to DAFGDA, as in CKR03.
!
      if ( failed() ) then
         call chkout ( 'CKR05' )
         return
      end if

      rate    =         contrl(1)
      subtyp  =  nint ( contrl(2) )
      wndsiz  =  nint ( contrl(3) )
      nints   =  nint ( contrl(4) )
      n       =  nint ( contrl(5) )

!
!     Set the packet size, which is a function of the subtype.
!
      if ( subtyp .eq. c05tp0 ) then

         packsz = c05ps0

      else if ( subtyp .eq. c05tp1 ) then

         packsz = c05ps1

      else if ( subtyp .eq. c05tp2 ) then

         packsz = c05ps2

      else if ( subtyp .eq. c05tp3 ) then

         packsz = c05ps3

      else

         call setmsg ( 'Unexpected CK type 5 subtype # found in ' // &
                       'type 5 segment.'                          )
         call errint ( '#',  subtyp                               )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                      )
         call chkout ( 'CKR05'                                    )
         return

      end if

!
!     Check the window size.
!
      if ( wndsiz .le. 0 ) then

         call setmsg ( 'Window size in type 05 segment was #; must ' // &
                       'be positive.'                                )
         call errint ( '#',  wndsiz                                  )
         call sigerr ( 'SPICE(INVALIDVALUE)'                         )
         call chkout ( 'CKR05'                                       )
         return

      end if


      if (      ( subtyp .eq. c05tp0 ) &
           .or. ( subtyp .eq. c05tp2 )  ) then
!
!        These are the Hermite subtypes.
!
         maxwnd  =  (maxdeg+1)/2

         if ( wndsiz .gt. maxwnd ) then

            call setmsg ( 'Window size in type 05 segment was #; max '// &
                          'allowed value is # for subtypes 0 and 2 '  // &
                          '(Hermite, 8 or 14-element packets).'       )
            call errint ( '#',  wndsiz                                )
            call errint ( '#',  maxwnd                                )
            call sigerr ( 'SPICE(INVALIDVALUE)'                       )
            call chkout ( 'CKR05'                                     )
            return

         end if

         if ( odd(wndsiz) ) then

            call setmsg ( 'Window size in type 05 segment was #; '    // &
                          'must be even for subtypes 0 and 2 '        // &
                          '(Hermite, 8 or 14-element packets).'       )
            call errint ( '#',  wndsiz                                )
            call sigerr ( 'SPICE(INVALIDVALUE)'                       )
            call chkout ( 'CKR05'                                     )
            return

         end if


      else if (      ( subtyp .eq. c05tp1 ) &
                .or. ( subtyp .eq. c05tp3 )  ) then
!
!        These are the Lagrange subtypes.
!
         maxwnd  =  maxdeg + 1

         if ( wndsiz .gt. maxwnd ) then

            call setmsg ( 'Window size in type 05 segment was #; max '// &
                          'allowed value is # for subtypes 1 and 3 '  // &
                          '(Lagrange, 4 or 7-element packets).'       )
            call errint ( '#',  wndsiz                                )
            call errint ( '#',  maxwnd                                )
            call sigerr ( 'SPICE(INVALIDVALUE)'                       )
            call chkout ( 'CKR05'                                     )
            return

         end if

         if ( odd(wndsiz) ) then

            call setmsg ( 'Window size in type 05 segment was #; '    // &
                          'must be even for subtypes 1 and 3 '        // &
                          '(Lagrange, 4 or 7-element packets).'       )
            call errint ( '#',  wndsiz                                )
            call sigerr ( 'SPICE(INVALIDVALUE)'                       )
            call chkout ( 'CKR05'                                     )
            return

         end if

      else

         call setmsg ( 'This point should not be reached. Getting ' // &
                       'here may indicate that the code needs to '  // &
                       'updated to handle the new subtype #'        )
         call errint ( '#', subtyp                                  )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                        )
         call chkout ( 'CKR05'                                      )
         return

      end if

!
!     We now need to select the pointing values to interpolate
!     in order to satisfy the pointing request.  The first step
!     is to use the pointing directories (if any) to locate a set of
!     epochs bracketing the request time.  Note that the request
!     time might not be bracketed:  it could precede the first
!     epoch or follow the last epoch.
!
!     We'll use the variable PGROUP to refer to the set of epochs
!     to search.  The first group consists of the epochs prior to
!     and including the first pointing directory entry.  The last
!     group consists of the epochs following the last pointing
!     directory entry.  Other groups consist of epochs following
!     one pointing directory entry up to and including the next
!     pointing directory entry.
!

      npdir   =  (n-1) / dirsiz
      dirbas  =  begin +  n*packsz  +  n  -  1

      if ( npdir .eq. 0 ) then
!
!        There's no mystery about which group of epochs to search.
!
         pgroup  = 1

      else
!
!        There's at least one directory.  Find the first directory
!        whose time is greater than or equal to the request time, if
!        there is such a directory.  We'll search linearly through the
!        directory entries, reading up to DIRSIZ of them at a time.
!        Having found the correct set of directory entries, we'll
!        perform a binary search within that set for the desired entry.
!
         bufbas  =  dirbas
         npread  =  min ( npdir, dirsiz )

         call dafgda ( handle, bufbas+1, bufbas+npread, pbuffr )

         remain  =  npdir - npread


         do while (       ( pbuffr(npread) .lt. t ) &
                    .and. ( remain         .gt. 0 )   )

            bufbas  =  bufbas + npread
            npread  =  min ( remain, dirsiz )
!
!           Note:  NPREAD is always > 0 here.
!
            call dafgda ( handle, bufbas+1, bufbas+npread, pbuffr )

            remain  =  remain - npread

         end do

!
!        At this point, BUFBAS - DIRBAS is the number of directory
!        entries preceding the one contained in PBUFFR(1).
!
!        PGROUP is one more than the number of directories we've
!        passed by.
!
         pgroup  =     ( bufbas - dirbas ) &
                    +   lstltd ( t, npread, pbuffr ) &
                    +   1

      end if

!
!     PGROUP now indicates the set of epochs in which to search for the
!     request epoch.  The following cases can occur:
!
!        PGROUP = 1
!        ==========
!
!           NPDIR = 0
!           --------
!           The request time may precede the first time tag
!           of the segment, exceed the last time tag, or lie
!           in the closed interval bounded by these time tags.
!
!           NPDIR >= 1
!           ---------
!           The request time may precede the first time tag
!           of the group but does not exceed the last epoch
!           of the group.
!
!
!        1 < PGROUP <= NPDIR
!        ===================
!
!           The request time follows the last time of the
!           previous group and is less than or equal to
!           the pointing directory entry at index PGROUP.
!
!        1 < PGROUP = NPDIR + 1
!        ======================
!
!           The request time follows the last time of the
!           last pointing directory entry.  The request time
!           may exceed the last time tag.
!
!
!     Now we'll look up the time tags in the group of epochs
!     we've identified.
!
!     We'll use the variable names PBEGIX and PENDIX to refer to
!     the indices, relative to the set of time tags, of the first
!     and last time tags in the set we're going to look up.
!
      if ( pgroup .eq. 1 ) then

         pbegix  =  1
         pendix  =  min ( n, dirsiz )

      else
!
!        If the group index is greater than 1, we'll include the last
!        time tag of the previous group in the set of time tags we look
!        up.  That way, the request time is strictly bracketed on the
!        low side by the time tag set we look up.
!
         pbegix  =      (  pgroup  - 1 ) * dirsiz
         pendix  =  min (  pbegix + dirsiz,  n  )

      end if

      timbas  =  dirbas - n

      call dafgda ( handle, timbas+pbegix, timbas+pendix, pbuffr )

      npread  =  pendix - pbegix + 1

!
!     At this point, we'll deal with the cases where T lies outside
!     of the range of epochs we've buffered.
!
      if ( t .lt. pbuffr(1) ) then
!
!        This can happen only if PGROUP = 1 and T precedes all epochs.
!        If the input request time is too far from PBUFFR(1) on
!        the low side, we're done.
!
         if ( sclkdp + tol .lt. pbuffr(1) ) then

            call chkout ( 'CKR05' )
            return

         end if

!
!        Bracket T to move it within the range of buffered epochs.
!
         t = pbuffr(1)


      else if ( t .gt. pbuffr(npread) ) then
!
!        This can happen only if T follows all epochs.
!
         if ( sclkdp - tol .gt. pbuffr(npread) ) then

            call chkout ( 'CKR05' )
            return

         end if

!
!        Bracket T to move it within the range of buffered epochs.
!
         t = pbuffr(npread)

      end if

!
!     At this point,
!
!        | T - SCLKDP |  <=  TOL
!
!     Also, one of the following is true:
!
!        T is the first time of the segment
!
!        T is the last time of the segment
!
!        T equals SCLKDP
!
!
!
!     Find two adjacent time tags bounding the request epoch.  The
!     request time cannot be greater than all of time tags in the
!     group, and it cannot precede the first element of the group.
!
      i  =  lstltd (  t,  npread,  pbuffr  )

!
!     The variables LOW and HIGH are the indices of a pair of time
!     tags that bracket the request time.  Remember that NPREAD could
!     be equal to 1, in which case we would have LOW = HIGH.
!
      if ( i .eq. 0 ) then
!
!        This can happen only if PGROUP = 1 and T = PBUFFR(1).
!
         low    =  1
         lepoch =  pbuffr(1)

         if ( n .eq. 1 ) then
            high = 1
         else
            high = 2
         end if

         hepoch = pbuffr(high)

      else

         low    =  pbegix + i - 1
         lepoch =  pbuffr(i)

         high   =  low + 1
         hepoch =  pbuffr(i+1)

      end if

!
!     We now need to find the interpolation interval containing
!     T, if any.  We may be able to use the interpolation
!     interval found on the previous call to this routine.  If
!     this is the first call or if the previous interval is not
!     applicable, we'll search for the interval.
!
!     First check if the request time falls in the same interval as
!     it did last time.  We need to make sure that we are dealing
!     with the same segment as well as the same time range.
!
!
!        PREVS      is the start time of the interval that satisfied
!                   the previous request for pointing.
!
!        PREVN      is the start time of the interval that followed
!                   the interval specified above.
!
!        PREVNN     is the start time of the interval that followed
!                   the interval starting at PREVN.
!
!        LHAND      is the handle of the file that PREVS and PREVN
!                   were found in.
!
!        LBEG,      are the beginning and ending addresses of the
!        LEND       segment in the file LHAND that PREVS and PREVN
!                   were found in.
!
      if (  ( handle .eq. lhand ) .and. &
            ( begin  .eq. lbeg  ) .and. &
            ( end    .eq. lend  ) .and. &
            ( t      .ge. prevs ) .and. &
            ( t      .lt. prevn )      ) then


         start  = prevs
         nstart = prevn
         nnstrt = prevnn

      else
!
!        Search for the interpolation interval.
!
         nidir   =  (nints-1) / dirsiz
         dirbas  =   end - ctrlsz - nidir

         if ( nidir .eq. 0 ) then
!
!           There's no mystery about which group of epochs to search.
!
            sgroup  = 1

         else
!
!           There's at least one directory.  Find the first directory
!           whose time is greater than or equal to the request time, if
!           there is such a directory.  We'll search linearly through
!           the directory entries, reading up to DIRSIZ of them at a
!           time. Having found the correct set of directory entries,
!           we'll perform a binary search within that set for the
!           desired entry.
!
            bufbas  =  dirbas
            nsread  =  min ( nidir, dirsiz )
            remain  =  nidir - nsread

            call dafgda ( handle, bufbas+1, bufbas+nsread, sbuffr )

            do while (       ( sbuffr(nsread) .lt. t ) &
                       .and. ( remain         .gt. 0 )   )

               bufbas  =  bufbas + nsread
               nsread  =  min ( remain, dirsiz )
               remain  =  remain - nsread
!
!              Note:  NSREAD is always > 0 here.
!
               call dafgda ( handle, bufbas+1, bufbas+nsread, sbuffr )

            end do

!
!           At this point, BUFBAS - DIRBAS is the number of directory
!           entries preceding the one contained in SBUFFR(1).
!
!           SGROUP is one more than the number of directories we've
!           passed by.
!
            sgroup  =     ( bufbas - dirbas ) &
                       +   lstltd ( t, nsread, sbuffr ) &
                       +   1

         end if

!
!        SGROUP now indicates the set of interval start times in which
!        to search for the request epoch.
!
!        Now we'll look up the time tags in the group of epochs we've
!        identified.
!
!        We'll use the variable names SBEGIX and SENDIX to refer to the
!        indices, relative to the set of start times, of the first and
!        last start times in the set we're going to look up.
!
         if ( sgroup .eq. 1 ) then

            sbegix  =  1
            sendix  =  min ( nints, dirsiz + 2 )

         else
!
!           Look up the start times for the group of interest. Also
!           buffer last start time from the previous group. Also, it
!           turns out to be useful to pick up two extra start
!           times---the first two start times of the next group---if
!           they exist.
!
            sbegix  =      (  sgroup  - 1 ) * dirsiz

            sendix  =  min (  sbegix + dirsiz + 2,  nints  )

         end if

         timbas  =  dirbas - nints

         call dafgda ( handle, timbas+sbegix, timbas+sendix, sbuffr )

         nsread  =  sendix - sbegix + 1

!
!        Find the last interval start time less than or equal to the
!        request time.  We know T is greater than or equal to the
!        first start time, so I will be > 0.
!
         nsrch  =  min ( dirsiz + 1, nsread )

         i      =  lstled (  t,  nsrch, sbuffr  )

         start  =  sbuffr(i)

!
!        Let NSTART ("next start") be the start time that follows
!        START, if START is not the last start time.  If NSTART
!        has a successor, let NNSTRT be that start time.
!
         if ( i .lt. nsread ) then

            nstart = sbuffr(i+1)

            if ( i+1 .lt. nsread ) then

               nnstrt = sbuffr(i+2)
            else
               nnstrt = dpmax()
            end if

         else

            nstart = dpmax()
            nnstrt = dpmax()

         end if

      end if

!
!     If T does not lie within the interpolation interval starting
!     at time START, we'll determine whether T is closer to this
!     interval or the next.  If the distance between T and the
!     closer interval is less than or equal to TOL, we'll map T
!     to the closer endpoint of the closer interval.  Otherwise,
!     we return without finding pointing.
!
      if ( hepoch .eq. nstart ) then
!
!        The first time tag greater than or equal to T is the start
!        time of the next interpolation interval.
!
!        The request time lies between interpolation intervals.
!        LEPOCH is the last time tag of the first interval; HEPOCH
!        is the first time tag of the next interval.
!
         if (  abs( t - lepoch ) .le. abs( hepoch - t )  ) then
!
!           T is closer to the first interval...
!
            if (  abs( t - lepoch )  .gt.  tol  ) then
!
!              ...But T is too far from the interval.
!
               call chkout ( 'CKR05' )
               return

            end if

!
!           Map T to the right endpoint of the preceding interval.
!
            t      = lepoch
            high   = low
            hepoch = lepoch

         else
!
!           T is closer to the second interval...
!
            if (  abs( hepoch - t )  .gt.  tol  ) then
!
!              ...But T is too far from the interval.
!
               call chkout ( 'CKR05' )
               return

            end if

!
!           Map T to the left endpoint of the next interval.
!
            t      = hepoch
            low    = high
            lepoch = hepoch

!
!           Since we're going to be picking time tags from the next
!           interval, we'll need to adjust START and NSTART.
!
            start  = nstart
            nstart = nnstrt

         end if

      end if

!
!     We now have
!
!        LEPOCH < T <  HEPOCH
!                -   -
!
!     where LEPOCH and HEPOCH are the time tags at indices
!     LOW and HIGH, respectively.
!
!     Now select the set of packets used for interpolation.  Note
!     that the window size is known to be even.
!
!     Unlike CK types 8, 9, 12, and 13, for type 05 we adjust
!     the window size to keep the request time within the central
!     interval of the window.
!
!     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
!     and (WNDSIZ/2 + 1)st of the interpolating set.  If the request
!     time is too close to one end of the interpolation interval, we
!     reduce the window size, after which one endpoint of the window
!     will coincide with an endpoint of the interpolation interval.
!
!     We start out by looking up the set of time tags we'd use
!     if there were no gaps in the coverage.  We then trim our
!     time tag set to ensure all tags are in the interpolation
!     interval.  It's possible that the interpolation window will
!     collapse to a single point as a result of this last step.
!
!     Let LSIZE be the size of the "left half" of the window:  the
!     size of the set of window epochs to the left of the request time.
!     We want this size to be WNDSIZ/2, but if not enough states are
!     available, the set ranges from index 1 to index LOW.
!
      lsize =  min ( wndsiz/2, low )

!
!     RSIZE is defined analogously for the right half of the window.
!
      rsize =  min (  wndsiz/2,  ( n - high + 1 )  )

!
!     The window size is simply the sum of LSIZE and RSIZE.
!
      wndsiz =  lsize + rsize

!
!     FIRST and LAST are the endpoints of the range of indices of
!     time tags (and packets) we'll collect in the output record.
!
      first =  low    -  lsize   +  1

      last  =  first  +  wndsiz  -  1

!
!     Buffer the epochs.
!
      wstart  =  begin  +  n * packsz  +  first  -  1

      call dafgda (  handle, wstart, wstart+wndsiz-1, pbuffr )

!
!     Discard any epochs less than START or greater than or equal
!     to NSTART.  The set of epochs we want ranges from indices
!     I+1 to J.  This range is non-empty unless START and NSTART
!     are both DPMAX().
!
      i = lstltd ( start,  wndsiz, pbuffr )
      j = lstltd ( nstart, wndsiz, pbuffr )


      if ( i .eq. j ) then
!
!        Fuggedaboudit.
!
         call chkout ( 'CKR05' )
         return

      end if

!
!     Update FIRST, LAST, and WNDSIZ.
!
      wndsiz = j     - i
      first  = first + i
      last   = first + wndsiz - 1

!
!     Put the subtype into the output record.  The size of the group
!     of packets is derived from the subtype, so we need not include
!     the size.
!
      record(1) =  t
      record(2) =  subtyp
      record(3) =  wndsiz
      record(4) =  rate

!
!     Read the packets.
!
      call dafgda ( handle, &
                    begin + (first-1)*packsz, &
                    begin +      last*packsz  -  1, &
                    record(parsiz+1)                )

!
!     Finally, add the epochs to the output record.
!
      call moved ( pbuffr(i+1), &
                   j-i, &
                   record( parsiz + 1 + wndsiz*packsz )  )


!
!     Save the information about the interval and segment.
!
      lhand  = handle
      lbeg   = begin
      lend   = end
      prevs  = start
      prevn  = nstart
      prevnn = nnstrt

!
!     Indicate pointing was found.
!
      found = .true.

      call chkout ( 'CKR05' )
      return
      end
