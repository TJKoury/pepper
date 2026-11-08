!$Procedure SPKR19 ( SPK, read record from segment, type 19 )

      subroutine spkr19 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single SPK data record from a segment of type 19
!     (ESOC/DDID Piecewise Interpolation).
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      implicit none

      include 'spk19.inc'
      include 'spkrec.inc'

      integer(ip)           handle
      real(wp)              descr    ( 5 )
      real(wp)              et
      real(wp)              record   ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   File handle.
!     DESCR      I   Segment descriptor.
!     ET         I   Target epoch.
!     RECORD     O   Data record.
!
! $ Detailed_Input
!
!     HANDLE,
!     DESCR    are the file handle and segment descriptor for an SPK
!              segment of type 19. The SPK file designated by HANDLE
!              must be open for read access.
!
!     ET       is an epoch for which a data record from a specific
!              segment is required. ET is expressed as seconds past
!              J2000 TDB.
!
! $ Detailed_Output
!
!     RECORD   is an array of data from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of the target body identified
!              by the input segment descriptor. The descriptor
!              specifies the center of motion and reference frame of
!              the state.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | subtype code         |
!                 +----------------------+
!                 | number of packets (n)|
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
!              The packet size is a function of the type 19 subtype.
!              All packets in a record have the same size.
!
! $ Parameters
!
!     See the Fortran INCLUDE file spk19.inc.
!
! $ Exceptions
!
!     1)  If the input HANDLE does not designate a loaded SPK file, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the segment specified by DESCR is not of data type 19,
!         the error SPICE(WRONGSPKTYPE) is signaled.
!
!     3)  If the input ET value is not within the range specified
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
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     SPICE user applications normally will have no need to call this
!     routine directly. For further information, see the headers of the
!     SPICE SPK APIs
!
!        SPKEZR
!        SPKPOS
!
!     the SPK Required Reading file spk.req, and the SPICE SPK
!     tutorial.
!
!     See the SPK Required Reading file for a description of the
!     structure of a data type 19 segment.
!
! $ Examples
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRxx
!     routines might be used to "dump" and check segment data for a
!     particular epoch.
!
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           CENTER = ICD( 2 )
!           REF    = ICD( 3 )
!           TYPE   = ICD( 4 )
!
!           IF ( TYPE .EQ. 19 ) THEN
!              CALL SPKR19 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
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
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 11-MAY-2015 (NJB)
!
!        Updated to support subtype 2.
!
!-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
!
!-&


! $ Index_Entries
!
!     read record from type_19 SPK segment
!
!-&


!
!     SPICELIB functions
!
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

      integer(ip)           bufsiz
      parameter           ( bufsiz = dirsiz + 1 )

      integer(ip)           ctrlsz
      parameter           ( ctrlsz = 3 )

!
!     Maximum window sizes, based on subtypes:
!
      integer(ip)           s19mw0
      parameter           ( s19mw0 =  ( maxdeg + 1 )/2  )

      integer(ip)           s19mw1
      parameter           ( s19mw1 =  ( maxdeg + 1 )    )

      integer(ip)           s19mw2
      parameter           ( s19mw2 =  ( maxdeg + 1 )    )

!
!     Local variables
!
      real(wp)              buffer ( bufsiz )
      real(wp)              contrl ( ctrlsz )
      real(wp)              dc     ( nd )
      real(wp)              mintim ( nd )
      real(wp)              svbtim
      real(wp)              svetim

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
      integer(ip)           mxwnsz ( 0 : s19nst-1 )
      integer(ip)           n
      integer(ip)           ndir
      integer(ip)           npkdir
      integer(ip)           npkt
      integer(ip)           nrcpkt
      integer(ip)           nread
      integer(ip)           pkdbas
      integer(ip)           pktsiz
      integer(ip)           pktszs ( 0 : s19nst-1 )
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

      logical               ivlsel
      logical               pass1
      logical               prvok
      logical               samivl
      logical               samseg
      logical               svlast
      logical               svok

!
!     Saved variables
!
      save                  mxwnsz
      save                  pass1
      save                  pktszs
      save                  svbeg
      save                  svbtim
      save                  svetim
      save                  svhan
      save                  svlast
      save                  svmiix
      save                  svminb
      save                  svn
      save                  svnpkt
      save                  svok
      save                  svpkdb
      save                  svpknd
      save                  svpksz
      save                  svstyp
      save                  svwnsz


!
!     Initial values
!
      data                  mxwnsz  /  s19mw0, s19mw1, s19mw2 /
      data                  pass1   /  .true.  /
      data                  pktszs  /  s19ps0, s19ps1, s19ps2 /
      data                  svbeg   /  -1      /
      data                  svbtim  /   0.0_wp   /
      data                  svetim  /  -1.0_wp   /
      data                  svhan   /   0      /
      data                  svlast  /  .false. /
      data                  svmiix  /  -1      /
      data                  svminb  /  -1      /
      data                  svn     /  -1      /
      data                  svnpkt  /  -1      /
      data                  svok    /  .false. /
      data                  svpkdb  /  -1      /
      data                  svpknd  /  -1      /
      data                  svpksz  /  -1      /
      data                  svstyp  /  -1      /
      data                  svwnsz  /  -1      /



      if ( return () ) then
         return
      end if

      call chkin ( 'SPKR19' )

!
!     Before any error checks are done, copy the status from
!     the previous call. Set the saved status variable to
!     .FALSE. here so it will be .FALSE. on exit unless this
!     call is successful.
!
      prvok = svok
      svok  = .false.

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
!        DIRBAS    Base address of interpolation interval directory.
!
!        EADDR     Segment end DAF address.
!
!        FIRST     Index (mini-segment-relative) of first time tag in
!                  sequence transferred to to output record.
!
!        HIGH      Index (mini-segment-relative) of time tag following
!                  the tag at index LOW (see description below).
!
!        IVBIX     Index in the interpolation interval bounds array of
!                  the start time of the applicable interval.
!
!        IVLBAS    Base address of interpolation interval time bounds.
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
!        MINIIX    Interpolation interval/mini-segment index.
!
!        N         Count of interpolation intervals/mini-segments.
!
!        NDIR      Number of interpolation interval time bounds
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
!     Start with a parameter compatibility check on the first
!     pass.
!
      if ( pass1 ) then

         if ( maxrec .lt. maxrsz ) then

            call setmsg ( 'SPK type 19 record size may be as ' &
            //            'large as #, but SPKPVN record size ' &
            //            '(defined in spkrec.inc) is #.'      )
            call errint ( '#',  maxrsz                         )
            call errint ( '#',  maxrec                         )
            call sigerr ( 'SPICE(BUG)'                         )

         end if

!
!        Indicate the first pass was completed.
!
         pass1 = .false.

      end if

!
!     Unpack the segment descriptor, and get the start and end
!     addresses of the segment.
!
      call dafus ( descr, 2, 6, dc, ic )

      type  = ic( 4 )
      baddr = ic( 5 )
      eaddr = ic( 6 )


!
!     Check the request time against the bounds in the segment
!     descriptor.
!
      if (  ( et .lt. dc(1) )  .or.  ( et .gt. dc(2) )  )  then

         call setmsg( 'Request time # is outside of descriptor ' &
         //           'bounds # : #.'                           )
         call errdp ( '#',  et                                  )
         call errdp ( '#',  dc(1)                               )
         call errdp ( '#',  dc(2)                               )
         call sigerr( 'SPICE(TIMEOUTOFBOUNDS)'                  )
         call chkout( 'SPKR19'                                  )
         return

      end if

!
!     Decide whether we're looking at the same segment we saw on the
!     previous call, and whether the interpolation interval used on
!     that call is still applicable.
!
!     Re-use of data from a previous call requires that the saved
!     data were set on a successful call.
!
      samseg  =       ( handle .eq.  svhan ) &
                .and. ( baddr  .eq.  svbeg ) &
                .and.                prvok

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
!           We pick the last interval containing ET. For
!           all intervals but the last, ET must be
!           less than the interval end time.
!
            if ( svmiix .lt. svn ) then

               samivl =       ( et .ge. svbtim ) &
                        .and. ( et .lt. svetim )
            else

               samivl =         et .ge. svbtim &
                        .and. ( et .le. svetim )

            end if

         else
!
!           We pick the first interval containing ET. For
!           all intervals but the first, ET must be
!           greater than the interval start time.
!
            if ( svmiix .gt. 1 ) then

               samivl =       ( et  .gt. svbtim ) &
                        .and. ( et  .le. svetim )

            else

               samivl =         et  .ge. svbtim &
                        .and. ( et  .le. svetim )

            end if

         end if

      end if


      if ( samseg .and. samivl ) then
!
!        We're looking at the same segment as last time, and the
!        interpolation interval we looked up last time is applicable
!        for the input time ET.
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
!           - The mini-segment window size
!
         subtyp = svstyp
         pktsiz = svpksz
         npkt   = svnpkt
         wndsiz = svwnsz

      else
!
!        The segment and interval information for the current segment
!        must be looked up.
!
!        Perform checks on this segment.
!
!        Make sure that this really is a type 19 data segment.
!
         if ( type .ne. 19 ) then

            call setmsg( 'You are attempting to locate type * ' &
            //           'data in a type 19 data segment.'      )
            call errint( '*',  type                             )
            call sigerr( 'SPICE(WRONGSPKTYPE)'                  )
            call chkout( 'SPKR19'                               )
            return
         end if

!
!        Locate the interpolation interval that contains the request
!        time.
!
!        Before getting started, we need to determine which interval to
!        use if the request time lies on a boundary between two
!        intervals. The segment's interval selection flag tells us how
!        to resolve this.
!
         call dafgda ( handle, eaddr-1, eaddr, contrl )

         if ( failed() ) then
            call chkout ( 'SPKR19' )
            return
         end if

         isel   = nint ( contrl(1) )
         n      = nint ( contrl(2) )

         ivlsel = isel .eq. itrue

!
!        Determine the number of interval directory entries in the
!        segment. Note that for most SPK types, this computation is
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
!        a non-empty set of epochs following the last directory
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
         dirbas = eaddr - 2 - ( n + 1 ) - ndir

!
!        The way we search the directory depends on the treatment
!        of request times that lie on interval boundaries.
!
         if ( ivlsel ) then
!
!           If there is an interval directory, search it to determine
!           the group of interval times to search next.
!
            if ( ndir .gt. 0 ) then
!
!              Find the last directory entry *less than or equal to*
!              the request time. The directory entry *after* that one,
!              if such exists, is the one to pick.
!
               nread  = min ( ndir, bufsiz )
               bufbas = dirbas

!
!              Fetch the current batch of directory entries.
!
               call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

               if ( failed() ) then
                  call chkout( 'SPKR19' )
                  return
               end if

               remain = ndir - nread

!
!              The variable NREAD always contains a positive value at
!              this point, so we can use it as an array index.
!
               do while (       ( remain        .gt. 0  ) &
                          .and. ( buffer(nread) .le. et )  )

                  bufbas = bufbas + nread
                  nread  = min ( remain, bufsiz )
!
!                 Fetch the current batch of directory entries.
!
                  call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

                  if ( failed() ) then
                     call chkout( 'SPKR19' )
                     return
                  end if

                  remain = remain - nread

               end do

!
!              Count the directory entries that are less than or equal
!              to ET. The number we skipped over before the final loop
!              iteration is BUFBAS-DIRBAS. The index of the group of
!              epochs containing ET exceeds the skipped directory count
!              by 1.
!
               group =    bufbas - dirbas &
                       +  lstled ( et, nread, buffer ) &
                       +  1

            else
!
!              There's no question about which group of epochs to
!              search.
!
               group = 1

            end if

!
!           Let IVBAS be the base address of the sequence of interval
!           time bounds.
!
            ivbas  = dirbas - ( n + 1 )

!
!           Now find the index of the last interval boundary less than
!           or equal to ET. We'll need to read the current group of
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

            if ( failed() ) then
               call chkout( 'SPKR19' )
               return
            end if

!
!           Find the index of the first epoch greater than ET. The case
!           where ET matches the final epoch must be handled here,
!           since in this case no epoch exceeds ET.
!
            iveix =    bufbas - ivbas &
                    +  lstled ( et, nread, buffer ) &
                    +  1

            iveix = min ( iveix,  n+1 )

!
!           Backstop test:
!
            if ( iveix .lt. 2 ) then

               call setmsg ( 'IVEIX = #.' )
               call errint ( '#', iveix   )
               call sigerr ( 'SPICE(BUG)' )
               call chkout ( 'SPKR19'     )
               return

            end if

!
!           The epoch at index IVEIX is the end time of the
!           interpolation interval we'll use. The index of
!           the interval itself is IVEIX - 1.
!
            miniix = iveix - 1


         else
!
!           IVLSEL is .FALSE., meaning we must pick the first interval
!           containing the request time.
!
!           If there is an interval directory, search it to determine
!           the group of interval times to search next.
!
            if ( ndir .gt. 0 ) then
!
!              Find the last directory entry *less than* the request
!              time. The directory entry *after* that one, if such
!              exists, is the one to pick.
!
               nread  = min ( ndir, bufsiz )
               bufbas = dirbas
               remain = ndir - nread

!
!              Fetch the current batch of directory entries.
!
               call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

               if ( failed() ) then
                  call chkout( 'SPKR19' )
                  return
               end if

!
!              The variable NREAD always contains a positive value at
!              this point, so we can use it as an array index.
!
                do while (      ( remain        .gt. 0  ) &
                          .and. ( buffer(nread) .lt. et )  )

                  bufbas = bufbas + nread
                  nread  = min ( remain, bufsiz )
!
!                 Fetch the current batch of directory entries.
!
                  call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

                  if ( failed() ) then
                     call chkout( 'SPKR19' )
                     return
                  end if

                  remain = remain - nread

               end do

!
!              Count the directory entries that are less than ET. The
!              number we skipped over before the final loop iteration
!              is BUFBAS-DIRBAS. The index of the group of epochs
!              containing ET exceeds the skipped directory count by 1.
!
               group =    bufbas - dirbas &
                       +  lstltd ( et, nread, buffer ) &
                       +  1

            else
!
!              There's no question about which group of epochs to
!              search.
!
               group = 1

            end if

!
!           Let IVBAS be the base address of the sequence of interval
!           time bounds.
!
            ivbas  = dirbas - ( n + 1 )

!
!           Now find the index of the last interval epoch less than ET.
!           We'll need to read the current group of epochs first, so
!           compute the base of the range of addresses containing this
!           group.

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

            if ( failed() ) then
               call chkout( 'SPKR19' )
               return
            end if

!
!           Find the index of the last epoch less than ET. The case
!           where ET matches the first epoch must be handled here,
!           since in this case no epoch precedes ET.
!
            ivbix =    bufbas - ivbas &
                    +  lstltd ( et, nread, buffer )

            ivbix = max ( ivbix,  1 )

!
!           Backstop test:
!
            if ( ivbix .gt. n ) then

               call setmsg ( 'IVBIX = #.' )
               call errint ( '#', ivbix   )
               call sigerr ( 'SPICE(BUG)' )
               call chkout ( 'SPKR19'     )
               return

            end if

!
!           The epoch at index IVBIX is the begin time of the
!           interpolation interval we'll use. The index of the interval
!           itself is also IVBIX.
!
            miniix = ivbix

         end if
!
!        This is the end of the IF block that handles mini-segment
!        selection for the two possible values of IVLSEL.
!
!        Look up the begin and end pointers of the mini-segment at index
!        MINIIX. For the first N-1 mini-segments, the "end pointer"
!        of one mini-segment is the "begin" pointer of the next.
!
         bufbas = eaddr - 2 - ( n + 1 ) + ( miniix - 1 )

         call dafgda ( handle, bufbas+1, bufbas+2, buffer )

         if ( failed() ) then
            call chkout ( 'SPKR19' )
            return
         end if

         minib = nint ( buffer(1) ) + baddr - 1
!
!        Note that the end of the current mini-segment
!        precedes the start of the next mini-segment by
!        one address.
!
         minie = nint ( buffer(2) ) + baddr - 2

!
!        Look up the time bounds of the mini-segment at index MINIIX.
!        These bounds are used quite a bit farther on, when we save
!        them for future use.
!
         call dafgda ( handle, ivbas+miniix, ivbas+miniix+1, mintim )

         if ( failed() ) then
            call chkout ( 'SPKR19' )
            return
         end if

!
!        From this point onward, we'll work with the mini-segment
!        that occupies the address range MINIB : MINIE.
!
!        Look up the control area of the mini-segment.
!
         call dafgda ( handle, minie-ctrlsz+1, minie, contrl )

         if ( failed() ) then
            call chkout ( 'SPKR19' )
            return
         end if

!
!        Fetch the control area parameters for the mini-segment.
!
         subtyp  =  nint ( contrl(1) )
         wndsiz  =  nint ( contrl(2) )
         npkt    =  nint ( contrl(3) )

         if (  ( subtyp .lt. 0 ) .or. ( subtyp .ge. s19nst ) ) then

            call setmsg ( 'Unexpected SPK type 19 subtype # found in ' &
            //            'type 19 segment within mini-segment #.'     )
            call errint ( '#',  subtyp                                 )
            call errint ( '#',  miniix                                 )
            call sigerr ( 'SPICE(NOTSUPPORTED)'                        )
            call chkout ( 'SPKR19'                                     )
            return

         end if

         pktsiz = pktszs ( subtyp )
         maxwnd = mxwnsz ( subtyp )

!
!        Check the window size.
!
         if ( ( wndsiz .lt. 2 ) .or. ( wndsiz .gt. maxwnd ) ) then

            call setmsg ( 'Window size in type 19 segment was #; must ' &
            //            'be in the range 2:# for subtype #. ' &
            //            'Mini-segment index is #.'                   )
            call errint ( '#',  wndsiz                                 )
            call errint ( '#',  maxwnd                                 )
            call errint ( '#',  subtyp                                 )
            call errint ( '#',  miniix                                 )
            call sigerr ( 'SPICE(INVALIDVALUE)'                        )
            call chkout ( 'SPKR19'                                     )
            return

         end if

         if ( odd(wndsiz) ) then

            call setmsg ( 'Window size in type 19 segment was #; ' &
            //            'must be even for subtype #. ' &
            //            'Mini-segment index is #.'               )
            call errint ( '#',  wndsiz                             )
            call errint ( '#',  subtyp                             )
            call errint ( '#',  miniix                             )
            call sigerr ( 'SPICE(INVALIDVALUE)'                    )
            call chkout ( 'SPKR19'                                 )
            return

         end if

!
!        Compute the number of packet directory entries for
!        the current mini-segment/interval.
!
         npkdir  =  (npkt-1) / dirsiz
!
!        Compute the base address of the sequence of packet
!        directory entries for the current mini-segment/interval.
!
         pkdbas  =  minib - 1 + ( npkt * ( pktsiz + 1 ) )

!
!        The test below is done for safety. No SPICE error s
!        should ever be detected at this point.
!
         if ( failed() ) then
            call chkout ( 'SPKR19' )
            return
         end if

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
!
         svbtim = mintim(1)
         svetim = mintim(2)

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
!
         svstyp = subtyp
         svpksz = pktsiz
         svnpkt = npkt
         svwnsz = wndsiz

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
      if ( npkdir .eq. 0 ) then
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

         if ( failed() ) then
            call chkout ( 'SPKR19' )
            return
         end if

         do while (       ( buffer(nread) .lt. et ) &
                    .and. ( remain        .gt. 0  )   )

            bufbas  =  bufbas + nread
            nread   =  min ( remain, dirsiz )
            remain  =  remain - nread
!
!           Note:  NREAD is always > 0 here.
!
            call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

            if ( failed() ) then
               call chkout ( 'SPKR19' )
               return
            end if

         end do

!
!        At this point, BUFBAS - PKDBAS is the number of directory
!        entries preceding the one contained in BUFFER(1).
!
         group  =     ( bufbas - pkdbas ) &
                    +   lstltd ( et, nread, buffer ) &
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
      if ( group .eq. 1 ) then

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

      if ( failed() ) then
         call chkout ( 'SPKR19' )
         return
      end if

!
!     Find two adjacent epochs bounding the request epoch.  The request
!     time cannot be greater than all of epochs in the group, and it
!     cannot precede the first element of the group.
!
      i  =  lstltd (  et,  endidx-begidx+1,  buffer  )

!
!     The variables LOW and high are the indices of a pair of time
!     tags that bracket the request time.
!
      if ( i .eq. 0 ) then
         low  =  1
      else
         low  =  begidx + i - 1
      end if

      high  =  low  +  1

!
!     Now select the set of packets used for interpolation.  Note
!     that the window size is known to be even.
!
!     Unlike SPK types 8, 9, 12, and 13, for type 19 we allow the
!     window size to shrink when the window must be truncated due to
!     proximity to an interval boundary.
!
!     The nominal bracketing epochs we've found are the (WNDSIZ/2)nd
!     and (WNDSIZ/2 + 1)st of the interpolating set.  If the
!     request time is too close to one end of the coverage interval,
!     we reduce the window size, after which one endpoint of the
!     window will coincide with an endpoint of the coverage interval.
!
!     Let LSIZE be the size of the "left half" of the window:  the
!     size set of window epochs to the left of the request time.
!     We want this size to be WNDSIZ/2, but if not enough states are
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
!
      record(1) =  subtyp

      record(2) =  nrcpkt

!
!     Read the packets.
!
      call dafgda ( handle, &
                    minib + (first-1)*pktsiz, &
                    minib +      last*pktsiz  -  1, &
                    record(3)                       )

!
!     Finally, add the epochs to the output record.
!     Read the sequence of time tags.
!
      bufbas  =  ( minib - 1 )  +  ( npkt * pktsiz )  +  ( first - 1 )

      call dafgda ( handle, &
                    bufbas+1, &
                    bufbas+nrcpkt, &
                    record( 3 + nrcpkt*pktsiz )  )

      if ( failed() ) then
         call chkout ( 'SPKR19' )
         return
      end if

!
!     The call was successful. Record this fact so that saved
!     interval data are available for re-use.
!
      svok = .true.

      call chkout ( 'SPKR19' )
      return
      end
