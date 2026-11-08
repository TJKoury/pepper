!$Procedure CKR06 ( C-kernel, read record from segment, type 6 )

      subroutine ckr06 ( handle, descr, sclkdp, tol, needav, &
                         record, found                       )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single CK data record from a segment of type 6
!     (ESOC/DDID Piecewise Interpolation).
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

      include 'ckparam.inc'
      include 'ck06.inc'

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
!     DESCR    are the file handle and segment descriptor for a CK
!              segment of type 6.
!
!     SCLKDP   is an encoded spacecraft clock time indicating the
!              epoch for which pointing is desired.
!
!     TOL      is a time tolerance, measured in the same units as
!              encoded spacecraft clock.
!
!              When SCLKDP falls between the start time of one of
!              the mini-segment intervals and the last time tag of
!              that interval, the tolerance has no effect because
!              pointing will be returned at the request time.
!
!              However, if the request time falls within a coverage
!              gap in one of the intervals, or outside of any
!              interval, then the tolerance is used to determine if
!              pointing should be returned at the closest epoch for
!              which pointing is available. This epoch is either an
!              interval's start time or the smaller of an interval's
!              end time and its last time tag.
!
!
!     NEEDAV   is .TRUE. if angular velocity is requested. If the
!              input segment descriptor indicates angular velocity
!              is absent, the error SPICE(NOAVDATA) is signaled.
!
!              Note: although all subtypes of type 6 records either
!              contain or compute angular velocity, a CK creator may
!              choose to indicate that the provided angular velocity
!              data are not valid; this can be done by setting the
!              segment descriptor angular velocity flag to .FALSE.
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
!
!     FOUND    is a logical flag indicating whether data were found.
!              If NEEDAV is .FALSE., data will be found if the
!              request time is within TOL ticks of a time for which
!              the segment provides data. If NEEDAV is .TRUE., the
!              segment's angular velocity flag must also be set in
!              order for data to be found.
!
!              A type 6 segment provides data for times that are
!              between its descriptor time bounds and that are
!              within the coverage region of a mini-segment
!              interval. The coverage region of a mini-segment
!              interval extends from its start time to the lesser of
!              its stop time and its last time tag.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine roughly follows the pattern established in the
!     lower-numbered CK data type readers of not explicitly performing
!     error diagnoses. In particular, the C-kernel from which data are
!     read is assumed to be valid in most respects. The few exceptions
!     that are handled here are listed below.
!
!     1)  If the input HANDLE does not designate a loaded CK file, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the segment specified by DESCR is not of data type 6, the
!         error SPICE(WRONGCKTYPE) is signaled.
!
!     3)  If the input SCLK value is not within TOL ticks of a time
!         for which the segment provides data, FOUND is set to .FALSE.
!         and the output record is undefined.
!
!     4)  If the window size is non-positive or greater than the
!         maximum allowed value, the error SPICE(INVALIDVALUE) is
!         signaled.
!
!     5)  If the window size is not compatible with the segment
!         subtype, the error SPICE(INVALIDVALUE) is signaled.
!
!     6)  If the segment subtype is not recognized, the error
!         SPICE(INVALIDSUBTYPE) is signaled.
!
!     7)  If the tolerance is negative, the error SPICE(NEGATIVETOL) is
!         signaled.
!
!     8)  If an error occurs while trying to read data from a C-kernel,
!         the error is signaled by a routine in the call tree of this
!         routine.
!
!     9)  If the input segment descriptor indicates that angular
!         velocity data are not present, and if the input flag NEEDAV
!         is set to .TRUE., the error SPICE(NOAVDATA) is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the CK Required Reading file for a description of the
!     structure of a data type 6 segment.
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
!           IF ( TYPE .EQ. 6 ) THEN
!
!              CALL CKR06 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
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
!     1)  Correctness of the C-kernel read by this routine is
!         assumed.
!
!     2)  Correctness of inputs must be ensured by the caller of
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
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 06-JUL-2021 (NJB) (JDR)
!
!        Corrected code example: removed comment character preceding
!        CKBSS call. Added note regarding difference between this
!        search and those performed by the CK reader APIs CKGP and
!        CKGPAV.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
!
!-&


! $ Index_Entries
!
!     read record from type_6 CK segment
!
!-&


!
!     SPICELIB functions
!
      real(wp)              brcktd

      integer(ip)           lstled
      integer(ip)           lstltd

      logical               failed
      logical               odd
      logical               return
      logical               touchl

!
!     Local parameters
!
      integer(ip)           nd
      parameter           ( nd     = 2 )

      integer(ip)           ni
      parameter           ( ni     = 6 )

      integer(ip)           dirsiz
      parameter           ( dirsiz = 100 )

      integer(ip)           bufsiz
      parameter           ( bufsiz = dirsiz + 1 )

      integer(ip)           ctrlsz
      parameter           ( ctrlsz = 4 )

      integer(ip)           nsgpar
      parameter           ( nsgpar = 2 )


!
!     Maximum window sizes, based on subtypes:
!
      integer(ip)           c06mw0
      parameter           ( c06mw0 =  ( maxdeg + 1 )/2  )

      integer(ip)           c06mw1
      parameter           ( c06mw1 =  ( maxdeg + 1 )    )

      integer(ip)           c06mw2
      parameter           ( c06mw2 =  ( maxdeg + 1 )/2  )

      integer(ip)           c06mw3
      parameter           ( c06mw3 =  ( maxdeg + 1 )    )

!
!     Local variables
!
      real(wp)              buffer ( bufsiz )
      real(wp)              contrl ( ctrlsz )
      real(wp)              dc     ( nd )
      real(wp)              gap
      real(wp)              lstepc
      real(wp)              mintim ( 2 )
      real(wp)              rate
      real(wp)              svbtim
      real(wp)              svetim
      real(wp)              svrate
      real(wp)              t

      integer(ip)           baddr
      integer(ip)           begidx
      integer(ip)           bufbas
      integer(ip)           dirbas
      integer(ip)           eaddr
      integer(ip)           endidx
      integer(ip)           first
      integer(ip)           group
      integer(ip)           high
      integer(ip)           i
      integer(ip)           ic     ( ni )
      integer(ip)           isel
      integer(ip)           ivbas
      integer(ip)           ivbix
      integer(ip)           iveix
      integer(ip)           last
      integer(ip)           low
      integer(ip)           lsize
      integer(ip)           maxwnd
      integer(ip)           minib
      integer(ip)           minie
      integer(ip)           miniix
      integer(ip)           mxwnsz ( 0 : c06nst-1 )
      integer(ip)           n
      integer(ip)           ndir
      integer(ip)           npkdir
      integer(ip)           npkt
      integer(ip)           nrcpkt
      integer(ip)           nread
      integer(ip)           pkdbas
      integer(ip)           pktsiz
      integer(ip)           pktszs ( 0 : c06nst-1 )
      integer(ip)           remain
      integer(ip)           rsize
      integer(ip)           subtyp
      integer(ip)           svbeg
      integer(ip)           svhan
      integer(ip)           svmiix
      integer(ip)           svminb
      integer(ip)           svn
      integer(ip)           svnpkt
      integer(ip)           svpkdb
      integer(ip)           svpknd
      integer(ip)           svpksz
      integer(ip)           svstyp
      integer(ip)           svwnsz
      integer(ip)           timbas
      integer(ip)           type
      integer(ip)           wndsiz

      logical               avflag
      logical               ivlsel
      logical               lval
      logical               pass1
      logical               prvfnd
      logical               samivl
      logical               samseg
      logical               svfnd
      logical               svlast

!
!     Saved variables
!
      save                  mxwnsz
      save                  pass1
      save                  pktszs
      save                  svbeg
      save                  svbtim
      save                  svetim
      save                  svfnd
      save                  svhan
      save                  svlast
      save                  svmiix
      save                  svminb
      save                  svn
      save                  svnpkt
      save                  svpkdb
      save                  svpknd
      save                  svpksz
      save                  svrate
      save                  svstyp
      save                  svwnsz

!
!     Initial values
!
      data                  mxwnsz  /  c06mw0, c06mw1, c06mw2, c06mw3 /
      data                  pass1   /  .true.  /
      data                  pktszs  /  c06ps0, c06ps1, c06ps2, c06ps3 /
      data                  svbeg   /  -1      /
      data                  svbtim  /   0.0_wp   /
      data                  svetim  /  -1.0_wp   /
      data                  svfnd   /  .false. /
      data                  svhan   /   0      /
      data                  svlast  /  .false. /
      data                  svmiix  /  -1      /
      data                  svminb  /  -1      /
      data                  svn     /  -1      /
      data                  svnpkt  /  -1      /
      data                  svpkdb  /  -1      /
      data                  svpknd  /  -1      /
      data                  svpksz  /  -1      /
      data                  svrate  /  -1.0_wp   /
      data                  svstyp  /  -1      /
      data                  svwnsz  /  -1      /

!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Start with a parameter compatibility check on the first
!     pass.
!
      if ( pass1 ) then

         if ( ckmrsz < maxrsz ) then

            call setmsg ( 'CK type 6 record size may be as ' &
            //            'large as #, but CKPFS record size ' &
            //            '(defined in ckparam.inc) is #.'     )
            call errint ( '#',  maxrsz                         )
            call errint ( '#',  ckmrsz                         )
            call sigerr ( 'SPICE(BUG)'                         )

         end if
!
!        Indicate the first pass was completed.
!
         pass1 = .false.

      end if

!
!     No pointing found so far.
!
      found  = .false.

!
!     Let PRVFND indicate the last value of FOUND we returned. PRVFND
!     allows us to reset SVFND to .FALSE. at the start of this routine,
!     so we don't have to do this prior to every return (of which there
!     are more than 35).
!
      prvfnd = svfnd

!
!     Set the saved value of FOUND so as to reflect the value
!     of FOUND we'll return next.
!
      svfnd  = .false.

!
!     "Touch" the input argument NEEDAV to suppress compiler warnings.
!
      lval = touchl ( needav )
      lval = touchl ( lval   )

!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      call dafus ( descr, 2, 6, dc, ic )

      type   = ic( 3 )
      avflag = ic( 4 ) == 1
      baddr  = ic( 5 )
      eaddr  = ic( 6 )

!
!     Check whether angular velocity data are requested but not
!     available.
!
      if (  needav  .and. ( .not. avflag ) ) then

         call setmsg ( 'Segment descriptor indicates angular velocity ' &
         //            'data are not available, but such data were ' &
         //            'requested.'                                    )
         call sigerr ( 'SPICE(NOAVDATA)'                               )
         return

      end if

!
!     Check the tolerance value.
!
      if ( tol < 0.0_wp ) then

         call setmsg( 'Tolerance must be non-negative but ' &
         //           'was actually *.'                      )
         call errdp ( '*',  tol                              )
         call sigerr( 'SPICE(NEGATIVETOL)'                   )
         return

      end if

!
!     Check the request time and tolerance against the bounds in
!     the segment descriptor.
!
      if (       ( (sclkdp + tol) < dc(1) ) &
           .or.  ( (sclkdp - tol) > dc(2) )  )  then
!
!        The request time is too far outside the segment's coverage
!        interval for any pointing to satisfy the request.
!
         return

      end if

!
!     Set the request time to use for searching.
!
      t = brcktd ( sclkdp, dc(1), dc(2) )

!
!     From this point onward, we assume the segment was constructed
!     correctly.
!
!
!     Terminology: below, the phrase "base address of 'X'" refers to
!     the DAF address immediately preceding X. Base addresses simplify
!     mapping DAF array (here "array" means an array stored in
!     consecutive DAF addresses, not "segment") indices to DAF
!     addresses, since the DAF address of the Ith array element is
!     obtained by adding I to the DAF array's base address.
!
!     Key variables:
!
!        Name      Meaning
!        ----      -------
!        BADDR     Segment begin DAF address.
!
!        DIRBAS    Base address of mini-segment interval directory.
!
!        EADDR     Segment end DAF address.
!
!        FIRST     Index (mini-segment-relative) of first time tag in
!                  sequence transferred to the output record.
!
!        HIGH      Index (mini-segment-relative) of time tag following
!                  the tag at index LOW (see description below).
!
!        IVBIX     Index in the mini-segment interval bounds array of
!                  the start time of the applicable interval.
!
!        IVLBAS    Base address of mini-segment interval time bounds.
!
!        IVLSEL    Interval selection flag: this routine selects the
!                  last applicable interval if true; otherwise it
!                  selects the first applicable interval.
!
!        LAST      Index (mini-segment-relative) of last time tag in
!                  sequence transferred to the output record.
!
!        LOW       Index (mini-segment-relative) of last time tag less
!                  than the request time, or of the first time tag if
!                  this tag equals the request time.
!
!        MINIB,
!        MINIE     Mini-segment begin and end DAF addresses. These
!                  addresses are absolute, not segment-relative.
!
!        MINIIX    Mini-segment/mini-segment interval index.
!
!        N         Count of mini-segments.
!
!        NDIR      Number of mini-segment interval time bounds
!                  directories.
!
!        NPKDIR    Number of packet directory entries for current
!                  mini-segment.
!
!        NPKT      Packet count for current mini-segment.
!
!        NRCPKT    Output record packet count. Note that this count,
!                  due to reduction of order at mini-segment
!                  boundaries, may be smaller than the window size
!                  stored in the current mini-segment.
!
!        PKDBAS    Base address of packet directory for current
!                  mini-segment.
!
!        PKTSIZ    Size of packets of current mini-segment.
!
!        SUBTYP    Subtype of current mini-segment.
!
!        TIMBAS    Base address of time tags of current mini-segment.
!
!        WNDSIZ    Interpolation window size of current mini-segment.
!
!
!     Re-used variables: the variables shown in the list below
!     are used as short-duration variables, much like loop index
!     variables; they are re-used as needed.
!
!        BUFBAS
!        BUFFER
!        GROUP
!        NREAD
!        REMAIN
!
!
!     Decide whether we're looking at the same segment we saw on the
!     previous call, and whether the mini-segment interval used on
!     that call is still applicable.
!
!     Re-use of data from a previous call requires that the saved
!     data were set on a successful call. Note that PRVFND can not
!     be true on the first pass.
!
      samseg  =       ( handle ==  svhan ) &
                .and. ( baddr  ==  svbeg ) &
                .and.   prvfnd

!
!     Give SAMIVL an initial value. If we do have the
!     same interval, update SAMIVL to indicate this.
!
      samivl = .false.

      if ( samseg ) then
!
!        We now assume that all data saved from the last
!        read of this segment are valid.
!
         if ( svlast ) then
!
!           We pick the last interval containing T. For all intervals
!           but the last, T must be less than the interval end time.
!           For the last interval, T may equal the end time.
!
!           Note that we don't bother to test for the special case
!           where the interval is not the last, there's a gap at the
!           end of the interval and T equals the last epoch of the
!           interval. In this rare case, we do not reuse the old
!           interval data, even though it would be possible to
!           add code to do so.
!
            if ( svmiix < svn ) then

               samivl =       ( t >= svbtim ) &
                        .and. ( t < svetim )
            else

               samivl =         t >= svbtim &
                        .and. ( t <= svetim )
            end if

         else
!
!           We pick the first interval containing T. For all intervals
!           but the first, T must be greater than the interval start
!           time. For the first interval, T may equal the start time.
!
            if ( svmiix > 1 ) then

               samivl =       ( t  > svbtim ) &
                        .and. ( t  <= svetim )

            else

               samivl =       ( t  >= svbtim ) &
                        .and. ( t  <= svetim )
            end if

         end if

      end if


      if ( samseg .and. samivl ) then
!
!        We're looking at the same segment as last time, and the
!        mini-segment interval we looked up last time is applicable
!        for the input time T.
!
!        Simply restore the segment and interval parameters we
!        saved from the previous lookup.
!
!        We don't need to restore the segment start DAF address
!        BADDR, since we've already extracted it from DESCR.
!
!        Restore
!
!           - The mini-segment's packet directory count
!           - The mini-segment's packet directory base address
!
         npkdir = svpknd
         pkdbas = svpkdb

!
!        Restore
!
!           - The mini-segment/interval count
!           - The mini-segment/interval index
!           - The mini-segment/interval start pointer
!
         n      = svn
         miniix = svmiix
         minib  = svminb

!
!        Restore
!
!           - The mini-segment subtype
!           - The mini-segment packet size
!           - The mini-segment packet count
!           - The mini-segment interpolation window size
!           - The mini-segment clock rate
!
         subtyp = svstyp
         pktsiz = svpksz
         npkt   = svnpkt
         wndsiz = svwnsz
         rate   = svrate

      else
!
!        The segment and interval information for the current segment
!        must be looked up.
!
!        Perform checks on this segment.
!
!        Make sure that this really is a type 06 data segment.
!
         if ( type /= 6 ) then

            call setmsg( 'You are attempting to locate type * ' &
            //           'data in a type 6 data segment.'       )
            call errint( '*',  type                             )
            call sigerr( 'SPICE(WRONGCKTYPE)'                   )
            return

         end if

!
!        Locate the mini-segment interval that contains the request
!        time. If the request time lies a common boundary of two
!        intervals, the choice of interval is determined by the
!        interval selection flag.
!
!        Before getting started, we need to determine which interval to
!        use if the request time lies on a boundary between two
!        intervals. The segment's interval selection flag tells us how
!        to resolve this.
!
         call dafgda ( handle, eaddr-1, eaddr, contrl )

         if ( failed() ) return

         isel   = nint ( contrl(1) )
         n      = nint ( contrl(2) )

         ivlsel = isel == itrue

!
!        Determine the number of interval directory entries in the
!        segment. Note that for most CK types, this computation is
!        performed by computing
!
!           ( N - 1 ) / DIRSIZ
!
!        where N is the segment's epoch count.
!
!        However the set of items in this case is a sequence
!        of N start times followed by a final stop time, so
!        the epoch count is
!
!           N + 1
!
!        and the numerator in the ratio above is incremented by 1.
!
         ndir = n / dirsiz

!
!        Note that the directory placement scheme always leaves
!        a non-empty group of epochs following the last directory
!        entry.
!
!        Let DIRBAS be the base address of the interval directory.
!        We'll compute DIRBAS whether or not the interval directory
!        is non-empty.
!
!        If the interval directory is non-empty, it spans the address
!        range
!
!           DIRBAS+1 : DIRBAS+NDIR
!
!        We compute DIRBAS by starting at the end of the segment
!        and skipping over the control area, the mini-segment
!        start/stop pointers, and the interval directory itself.
!
         dirbas = eaddr - nsgpar - ( n + 1 ) - ndir

!
!        The way we search the directory depends on the treatment
!        of request times that lie on interval boundaries.
!
         if ( ivlsel ) then
!
!           We must pick the latest interval containing the request
!           time.
!
!           The stop time of the interval we seek is the first interval
!           boundary strictly greater than T, unless T is the stop time
!           of the final interval.
!
!           We want to find the group of interval boundaries containing
!           the stop time of the interval containing T. There are
!           NDIR+1 such groups; all but the last have a directory entry
!           that coincides with the final epoch of the group. We'll use
!           the variable GROUP as the group index.
!
!           If there is an interval directory, search it to determine
!           the group of interval times to search next.
!
            if ( ndir == 0 ) then
!
!              There's no question about which group of epochs to
!              search.
!
               group = 1

            else
!
!              The index of the group we seek is the index of the first
!              directory entry that is greater than T, if such an entry
!              exists. If there's no such entry, the group we seek is
!              the final one.
!
!              Find the last directory entry less than or equal to
!              the request time. The directory entry after that one,
!              if such exists, is the one to pick.
!
               nread  = min ( ndir, bufsiz )
               bufbas = dirbas
!
!              Fetch the current batch of directory entries.
!
               call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

               if ( failed() ) return

               remain = ndir - nread
!
!              The variable NREAD always contains a positive value at
!              this point, so we can use it as an array index.
!
               do while (       ( remain        > 0 ) &
                          .and. ( buffer(nread) <= t )  )

                  bufbas = bufbas + nread
                  nread  = min ( remain, bufsiz )
!
!                 Fetch the current batch of directory entries.
!
                  call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

                  if ( failed() ) return

                  remain = remain - nread

               end do
!
!              Count the directory entries that are less than or equal
!              to T. The number we skipped over before the final loop
!              iteration is BUFBAS-DIRBAS; the number of buffered
!              entries we're skipping is the number of entries that are
!              less than or equal to T. The index of the group of
!              epochs containing T exceeds the skipped directory count
!              by 1.
!
               group =    bufbas - dirbas &
                       +  lstled ( t, nread, buffer ) &
                       +  1
!
!              GROUP is in the range 1 : NDIR+1.
!
            end if

!
!           Let IVBAS be the base address of the sequence of interval
!           time bounds.
!
            ivbas  = dirbas - ( n + 1 )
!
!           Now find the index of the last interval boundary less than
!           or equal to T. We'll need to read the current group of
!           epochs first, so compute the base of the range of addresses
!           containing this group.

            bufbas = ivbas + ( ( group - 1 ) * dirsiz )
!
!           Compute the number of epochs to read. Note that all groups
!           of epochs except the last have DIRSIZ elements.
!
            remain = ( n + 1 ) - ( ( group - 1 ) * dirsiz )
!
!           Note that REMAIN is always non-zero, since there's always
!           at least one epoch that exceeds the last directory entry.
!
            nread  = min ( dirsiz, remain )

            call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

            if ( failed() ) return

!
!           Find the index of the first epoch greater than T; this is
!           the number of epochs that are less than or equal to T, plus
!           1. The case where T matches the final epoch must be handled
!           here, since in this case no epoch exceeds T.
!
            iveix =    bufbas - ivbas &
                    +  lstled ( t, nread, buffer ) &
                    +  1

            iveix = min ( iveix,  n+1 )

!
!           Backstop test:
!
            if ( iveix < 2 ) then

               call setmsg ( 'IVEIX = #.' )
               call errint ( '#', iveix   )
               call sigerr ( 'SPICE(BUG)' )
               return

            end if

!
!           The epoch at index IVEIX is the end time of the
!           mini-segment interval we'll use. The index of
!           the interval itself is IVEIX - 1.
!
            miniix = iveix - 1


         else
!
!           IVLSEL is .FALSE., meaning we must pick the first interval
!           containing the request time.

!           The start time of the interval we seek is the last interval
!           boundary strictly less than T, unless T is the start time
!           of the first interval. The stop time of this interval is
!           the first boundary greater than or equal to T.
!
!           We want to find the group of interval boundaries containing
!           the stop time of the interval containing T. There are
!           NDIR+1 such groups; all but the last have a directory entry
!           that coincides with the final epoch of the group. We'll use
!           the variable GROUP as the group index.
!
!           If there is an interval directory, search it to determine
!           the group of interval times to search next.
!
            if ( ndir == 0 ) then
!
!              There's no question about which group of epochs to
!              search.
!
               group = 1

            else
!
!              Find the last directory entry strictly less than the
!              request time. The directory entry after that one, if
!              such exists, is the one to pick.
!
               nread  = min ( ndir, bufsiz )
               bufbas = dirbas
               remain = ndir - nread
!
!              Fetch the current batch of directory entries.
!
               call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

               if ( failed() ) return
!
!              The variable NREAD always contains a positive value at
!              this point, so we can use it as an array index.
!
               do while (      ( remain        > 0 ) &
                         .and. ( buffer(nread) < t )  )

                  bufbas = bufbas + nread
                  nread  = min ( remain, bufsiz )
!
!                 Fetch the current batch of directory entries.
!
                  call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

                  if ( failed() ) return

                  remain = remain - nread

               end do
!
!              Count the directory entries that are less than T. The
!              number we skipped over before the final loop iteration
!              is BUFBAS-DIRBAS; the number of buffered entries we're
!              skipping is the number of entries that are less than T.
!              The index of the group of epochs containing T exceeds
!              the skipped directory count by 1.
!
               group =    bufbas - dirbas &
                       +  lstltd ( t, nread, buffer ) &
                       +  1
!
!              GROUP is in the range 1 : NDIR+1.
!
            end if

!
!           Let IVBAS be the base address of the sequence of interval
!           time bounds.
!
            ivbas  = dirbas - ( n + 1 )

!
!           Now find the index of the last interval boundary epoch less
!           than T. We'll need to read the current group of epochs
!           first, so compute the base of the range of addresses
!           containing this group.
!
            bufbas = ivbas + ( ( group - 1 ) * dirsiz )

!
!           Compute the number of epochs to read. Note that all groups
!           of epochs except the last have DIRSIZ elements.
!
            remain = ( n + 1 ) - ( ( group - 1 ) * dirsiz )
!
!           Note that REMAIN is always non-zero, since there's always
!           at least one epoch that exceeds the last directory entry.
!
            nread  = min ( dirsiz, remain )

            call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

            if ( failed() ) return

!
!           Find the index of the last interval boundary less than T.
!           The case where T matches the first boundary must be handled
!           here, since in this case no boundary precedes T.
!
            ivbix =    bufbas - ivbas &
                    +  lstltd ( t, nread, buffer )

            ivbix = max ( ivbix,  1 )

!
!           Backstop test:
!
            if ( ivbix > n ) then

               call setmsg ( 'IVBIX = #.' )
               call errint ( '#', ivbix   )
               call sigerr ( 'SPICE(BUG)' )
               return

            end if

!
!           The epoch at index IVBIX is the begin time of the
!           mini-segment interval we'll use.
!
            miniix = ivbix

         end if
!
!        This is the end of the IF block that handles mini-segment
!        selection for the two possible values of IVLSEL.
!
!        If the mini-segment we just found has a gap, and if TOL is
!        positive, it's possible that the mini-segment we want actually
!        is the successor of the one at index MINIIX. We'll check this
!        by finding the last epoch of the mini-segment we just
!        identified.
!
!        Look up the begin and end pointers of the mini-segment at index
!        MINIIX. For the first N-1 mini-segments, the "end pointer"
!        of one mini-segment is the "begin" pointer of the next.
!
         bufbas = eaddr - nsgpar - ( n + 1 ) + ( miniix - 1 )

         call dafgda ( handle, bufbas+1, bufbas+2, buffer )

         if ( failed() ) return

         minib = nint ( buffer(1) ) + baddr - 1
!
!        Note that the end of the current mini-segment
!        precedes the start of the next mini-segment by
!        one address.
!
         minie = nint ( buffer(2) ) + baddr - 2

!
!        Look up the control area of the mini-segment.
!
         call dafgda ( handle, minie-ctrlsz+1, minie, contrl )

         if ( failed() ) return

!
!        Fetch the control area parameters for the mini-segment.
!
         rate    =         contrl(1)
         subtyp  =  nint ( contrl(2) )
         wndsiz  =  nint ( contrl(3) )
         npkt    =  nint ( contrl(4) )

!
!        Compute the directory count for the mini-segment.
!
         npkdir  =  ( npkt - 1 ) / dirsiz

!
!        The last epoch of the mini-segment precedes the epoch
!        directories and the control area. Look up this epoch.
!
         bufbas  =  minie - ctrlsz - npkdir - 1

         call dafgda ( handle, bufbas+1, bufbas+1, lstepc )

         if ( failed() ) return

!
!        Determine whether the request time is in a gap.
!
         if ( t > lstepc ) then
!
!           Yep, T lies in a gap. But we may still be able to
!           find data for this request, if the lookup tolerance
!           is positive.
!
            if ( tol == 0.0_wp ) then
!
!              We're out of luck. We can't find pointing for this
!              request. FOUND is already .FALSE., so just return.
!
               return

            else
!
!              Determine the distance of T from the nearest epochs.
!
!              Look up the time bounds of the mini-segment at index
!              MINIIX.
!
               call dafgda ( handle, ivbas+miniix, &
                                     ivbas+miniix+1, mintim )

               if ( failed() ) return

!
!              See whether T is close enough to a stored epoch for
!              us to find pointing. If not, return now.
!
               if (      (  ( t         - lstepc )  >  tol  ) &
                   .and. (  ( mintim(2) - t      )  >  tol  )  ) then
!
!                 We can't find pointing for T. FOUND is already
!                 .FALSE., so just return.
!
                  return

               end if

!
!              Continue to look for pointing usable for time T.
!
               if ( miniix == n ) then
!
!                 We're looking at the final mini-segment. If
!                 T is close enough to LSTEPC, we can find
!                 pointing.
!
                  if (  ( t - lstepc )  <=  tol  ) then
!
!                    We're going to carry on using the current
!                    mini-segment. We'll update T to be the last epoch
!                    of this mini-segment.
!
                     t = lstepc

                  else
!
!                    T is too far from LSTEPC. We're done. FOUND is
!                    already .FALSE., so just return.
!
                     return

                  end if

               else
!
!                 There's a successor to the current interval. Determine
!                 which interval contains an epoch closest to T.
!
!                 Compute the size of the gap at the right end of the
!                 interior of the current interval.
!
                  gap  =  mintim(2) - lstepc

                  if (  ( t - lstepc )  <=  ( gap/2 )  ) then
!
!                    T is closer to LSTEPC than the start time of the
!                    next interval. We're going to carry on using the
!                    current mini-segment. We'll update T to be the
!                    last epoch of this mini-segment.
!
                     t = lstepc

                  else
!
!                    T is closer to the start time of the next interval
!                    than to LSTEPC. than the start time of the next
!                    interval. We're going to use the next
!                    mini-segment.
!
                     miniix  =  miniix + 1

!
!                    Update the mini-segment parameters we already
!                    found, since these have been superseded.
!
!                    The mini-segment pointers:
!
                     bufbas = eaddr - nsgpar - (n + 1) + ( miniix - 1 )

                     call dafgda ( handle, bufbas+1, bufbas+2, buffer )

                     if ( failed() ) return

                     minib = nint ( buffer(1) ) + baddr - 1
!
!                    Note that the end of the current mini-segment
!                    precedes the start of the next mini-segment by one
!                    address.
!
                     minie = nint ( buffer(2) ) + baddr - 2

!
!                    Look up the control area of the mini-segment.
!
                     call dafgda ( handle, minie-ctrlsz+1, &
                                           minie,          contrl )

                     if ( failed() ) return

!
!                    Fetch the control area parameters for the
!                    mini-segment.
!
                     rate    =  nint ( contrl(1) )
                     subtyp  =  nint ( contrl(2) )
                     wndsiz  =  nint ( contrl(3) )
                     npkt    =  nint ( contrl(4) )
!
!                    Since we have new mini-segment parameters, we need
!                    to check them. We'll defer these checks until
!                    later, so we can perform one set of checks,
!                    regardless of which logic path we followed to
!                    select a mini-segment.
!
!                    Compute the directory count for the mini-segment.
!
                     npkdir  =  ( npkt - 1 ) / dirsiz
!
!                    We're going to set T to the start time of the
!                    current mini-segment interval, which is the stop
!                    time of the previous one.
!
                     t = mintim(2)
!
!                    We still need to look up the last epoch of the
!                    current mini-segment. We'll use this when we save
!                    the time bounds of the mini-segment.
!
                     bufbas = minie - ctrlsz - npkdir - 1

                     call dafgda ( handle, bufbas+1, bufbas+1, lstepc )

                     if ( failed() ) return

                  end if

               end if
!
!              At this point T is set. If we had to update the
!              mini-segment index and its parameters, we did so.
!
            end if
!
!           We've handled the case where T lies in a gap and the
!           tolerance is non-zero.

         end if
!
!        This is the end of the block that handles the case where T lies
!        in a gap. At this point, the following items are set:
!
!           T
!           MINIIX
!           MINIB
!           MINIE
!           SUBTYP
!           WNDSIZ
!           NPKT
!           NPKDIR
!           RATE
!
!        Look up the time bounds of the mini-segment at index MINIIX.
!        These bounds are used quite a bit farther on, when we save
!        them for future use.
!
         call dafgda ( handle, ivbas+miniix, ivbas+miniix+1, mintim )

         if ( failed() ) return

!
!        From this point onward, we'll work with the mini-segment
!        that occupies the address range MINIB : MINIE.
!
!        Set the packet size, which is a function of the subtype.
!        Also set the maximum window size. First check the subtype,
!        which will be used as an array index.
!
         if (  ( subtyp < 0 ) .or. ( subtyp >= c06nst ) ) then

            call setmsg ( 'Unexpected CK type 6 subtype # found in ' &
            //            'type 06 segment within mini-segment #.'     )
            call errint ( '#',  subtyp                                 )
            call errint ( '#',  miniix                                 )
            call sigerr ( 'SPICE(INVALIDSUBTYPE)'                      )
            return

         end if

         pktsiz = pktszs ( subtyp )
         maxwnd = mxwnsz ( subtyp )

!
!        Check the window size.
!
         if ( ( wndsiz < 2 ) .or. ( wndsiz > maxwnd ) ) then

            call setmsg ( 'Window size in type 6 segment was #; must ' &
            //            'be in the range 2:# for subtype #. ' &
            //            'Mini-segment index is #.'                   )
            call errint ( '#',  wndsiz                                 )
            call errint ( '#',  maxwnd                                 )
            call errint ( '#',  subtyp                                 )
            call errint ( '#',  miniix                                 )
            call sigerr ( 'SPICE(INVALIDVALUE)'                        )
            return

         end if

         if ( odd(wndsiz) ) then

            call setmsg ( 'Window size in type 06 segment was #; ' &
            //            'must be even for subtype #. ' &
            //            'Mini-segment index is #.'               )
            call errint ( '#',  wndsiz                             )
            call errint ( '#',  subtyp                             )
            call errint ( '#',  miniix                             )
            call sigerr ( 'SPICE(INVALIDVALUE)'                    )
            return

         end if

!
!        Compute the base address of the sequence of packet
!        directory entries for the current mini-segment/interval.
!
         pkdbas  =  minib - 1 + ( npkt * ( pktsiz + 1 ) )

!
!        The test below is done for safety. No SPICE errors
!        should ever be detected at this point.
!
         if ( failed() ) return

!
!        If we made it this far, we did so without a SPICE error. We
!        have valid segment parameters which can be saved for the next
!        call.
!
!        Save
!
!           - The DAF handle
!           - The segment begin DAF address
!           - The segment's "select last/first interval" flag
!
         svhan  = handle
         svbeg  = baddr
         svlast = ivlsel

!
!        Save the time bounds of the applicable mini-segment/interval.
!        We don't want to indicate data availability within a gap, since
!        the re-use logic assumes data availability.
!
         svbtim =       mintim(1)
         svetim = min ( mintim(2), lstepc )

!
!        Save
!
!           - The mini-segment/interval directory count
!           - The mini-segment/interval directory base address
!
         svpknd = npkdir
         svpkdb = pkdbas

!
!        Save
!
!           - The mini-segment/interval count
!           - The mini-segment/interval index
!           - The mini-segment/interval start pointer
!
         svn    = n
         svmiix = miniix
         svminb = minib

!
!        Save
!
!           - The mini-segment subtype
!           - The mini-segment packet size
!           - The mini-segment packet count
!           - The mini-segment window size
!           - The mini-segment clock rate
!
         svstyp = subtyp
         svpksz = pktsiz
         svnpkt = npkt
         svwnsz = wndsiz
         svrate = rate

      end if

!
!     We're ready to construct the output record. The first step is to
!     identify the indices of the packets and epochs corresponding to
!     the request.
!
!     We'll now select the set of packets that define the interpolating
!     polynomials.   We'll start out by finding the first directory
!     entry that is greater than or equal to the request epoch.  We'll
!     use the variable GROUP to indicate the set of epochs to search
!     within, once we've found the right directory entry.
!
      if ( npkdir == 0 ) then
!
!        There's no mystery about which group of epochs to search.
!
         group  = 1

      else
!
!        There's at least one directory entry. Find the first directory
!        entry whose time is greater than or equal to the request time,
!        if there is such an entry.  We'll search linearly through the
!        directory entries, reading up to DIRSIZ of them at a time.
!        Having found the correct set of directory entries, we'll
!        perform a binary search within that set for the desired entry.
!
         bufbas  =  pkdbas
         nread   =  min ( npkdir, dirsiz )
         remain  =  npkdir - nread

         call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

         if ( failed() ) return

         do while (       ( buffer(nread) < t ) &
                    .and. ( remain        > 0 )  )

            bufbas  =  bufbas + nread
            nread   =  min ( remain, dirsiz )
            remain  =  remain - nread
!
!           Note:  NREAD is always > 0 here.
!
            call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

            if ( failed() ) return

         end do

!
!        At this point, BUFBAS - PKDBAS is the number of directory
!        entries preceding the one contained in BUFFER(1).
!
         group  =     ( bufbas - pkdbas ) &
                    +   lstltd ( t, nread, buffer ) &
                    +   1
      end if

!
!     GROUP now indicates the set of epochs in which to search for the
!     request epoch.  If GROUP is 1, the request time lies within the
!     inclusive time interval bounded by the first and last epochs of
!     the first group.  Otherwise, the request time lies in the time
!     interval bounded by the last element of the preceding group and
!     the last element of the current group.
!
!     We'll use the variable names BEGIDX and ENDIDX to refer to
!     the indices, relative to the set of time tags, of the first
!     and last time tags in the set we're going to look up.
!
      if ( group == 1 ) then

         begidx  =  1
         endidx  =  min ( npkt, dirsiz )

      else
!
!        If the group index is greater than 1, we'll include the last
!        time tag of the previous group in the set of time tags we look
!        up.  That way, the request time is bracketed by the time tag
!        set we look up.
!
         begidx  =      (  group  - 1 ) * dirsiz
         endidx  =  min (  begidx + dirsiz,  npkt  )

      end if


      timbas  =  pkdbas - npkt

      call dafgda ( handle, timbas+begidx, timbas+endidx, buffer )

      if ( failed() ) return

!
!     Find two adjacent epochs bounding the request epoch.  The request
!     time cannot be greater than all of epochs in the group, and it
!     cannot precede the first element of the group.
!
      i  =  lstltd (  t,  endidx-begidx+1,  buffer  )

!
!     The variables LOW and high are the indices of a pair of time
!     tags that bracket the request time.
!
      if ( i == 0 ) then
         low  =  1
      else
         low  =  begidx + i - 1
      end if

      high  =  low  +  1

!
!     Now select the set of packets used for interpolation. Note
!     that the window size is known to be even.
!
!     For CK type 6 we allow the window size to shrink when the window
!     must be truncated due to proximity to an interval boundary.
!
!     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
!     and (WNDSIZ/2 + 1)st of the interpolating set.  If the
!     request time is too close to one end of the coverage interval,
!     we reduce the window size, after which one endpoint of the
!     window will coincide with an endpoint of the coverage interval.
!
!     Let LSIZE be the size of the "left half" of the window: the size
!     set of window epochs to the left of the request time. We want
!     this size to be WNDSIZ/2, but if not enough packets are
!     available, the set ranges from index 1 to index LOW.
!

      lsize =  min ( wndsiz/2, low )

!
!     RSIZE is defined analogously for the right half of the window.
!
      rsize =  min (  wndsiz/2,  ( npkt - high + 1 )  )

!
!     The actual window size is simply the sum of LSIZE and RSIZE.
!
      nrcpkt =  lsize + rsize

!
!     FIRST and LAST are the endpoints of the range of indices of
!     time tags (and packets) we'll collect in the output record.
!
      first =  low    -  lsize   +  1

      last  =  first  +  nrcpkt  -  1

!
!     We're ready to construct the output record.
!
!     Put the subtype and window size into the output record.
!     The fourth element is the nominal SCLK rate.
!
      record(1) =  t
      record(2) =  subtyp
      record(3) =  nrcpkt
      record(4) =  rate

!
!     Read the packets.
!
      call dafgda ( handle, &
                    minib + (first-1)*pktsiz, &
                    minib +      last*pktsiz  -  1, &
                    record( ctrlsz + 1 )           )
!
!     Finally, add the epochs to the output record.
!     Read the sequence of time tags.
!
      bufbas  =  ( minib - 1 )  +  ( npkt * pktsiz )  +  ( first - 1 )

      call dafgda ( handle, &
                    bufbas+1, &
                    bufbas+nrcpkt, &
                    record( ctrlsz + nrcpkt*pktsiz + 1 )  )

      if ( failed() ) return
!
!     Indicate pointing was found.
!
      found = .true.
      svfnd = .true.

      return
      end
