!$Procedure SPKR18 ( Read SPK record from segment, type 18 )

      subroutine spkr18 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single SPK data record from a segment of type 18
!     (MEX/Rosetta Orbit file interpolation).
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

      include 'spk18.inc'

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
!     DESCR    are the file handle and segment descriptor for
!              a SPK segment of type 18.
!
!     ET       is a target epoch, for which a data record from
!              a specific segment is required.
!
! $ Detailed_Output
!
!     RECORD   is a set of data from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative
!              to some center, in some reference frame.
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
!              The packet size is a function of the subtype code.
!              All packets in a record have the same size.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input HANDLE does not designate a loaded SPK file, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the segment specified by DESCR is not of data type 18,
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
!     7)  If the input segment contains fewer than 2 packets, the
!         error SPICE(TOOFEWSTATES) is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the SPK Required Reading file for a description of the
!     structure of a data type 18 segment.
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
!           IF ( TYPE .EQ. 18 ) THEN
!              CALL SPKR18 ( HANDLE, DESCR, ET, RECORD )
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
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 21-DEC-2012 (NJB)
!
!        An error check was added for segment packet counts
!        less than 2.
!
!        An in-line comment regarding deducibility of record size from
!        segment subtype was removed. The comment now says the actual
!        count of packets in the output record is inserted into the
!        record.
!
!-    SPICELIB Version 1.0.0, 04-SEP-2002 (NJB)
!
!-&


! $ Index_Entries
!
!     read record from type_18 SPK segment
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 21-DEC-2012 (NJB)
!
!        An error check was added for segment packet counts
!        less than 2.
!
!-&


!
!     SPICELIB functions
!
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
!     Maximum polynomial degree:
!
      integer(ip)           maxdeg
      parameter           ( maxdeg = 15 )

!
!     Local variables
!
      real(wp)              buffer ( bufsiz )
      real(wp)              contrl ( ctrlsz )
      real(wp)              dc     ( nd )

      integer(ip)           begidx
      integer(ip)           begin
      integer(ip)           bufbas
      integer(ip)           dirbas
      integer(ip)           end
      integer(ip)           endidx
      integer(ip)           first
      integer(ip)           group
      integer(ip)           high
      integer(ip)           i
      integer(ip)           ic     ( ni )
      integer(ip)           last
      integer(ip)           lsize
      integer(ip)           low
      integer(ip)           maxwnd
      integer(ip)           n
      integer(ip)           ndir
      integer(ip)           nread
      integer(ip)           packsz
      integer(ip)           remain
      integer(ip)           rsize
      integer(ip)           start
      integer(ip)           subtyp
      integer(ip)           timbas
      integer(ip)           type
      integer(ip)           wndsiz


      if ( return () ) then
         return
      end if

      call chkin ( 'SPKR18' )

!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      call dafus ( descr, 2, 6, dc, ic )

      type  = ic( 4 )
      begin = ic( 5 )
      end   = ic( 6 )

!
!     Make sure that this really is a type 18 data segment.
!
      if ( type .ne. 18 ) then
         call setmsg( 'You are attempting to locate type * ' // &
                      'data in a type 18 data segment.'      )
         call errint( '*',  type                             )
         call sigerr( 'SPICE(WRONGSPKTYPE)'                  )
         call chkout( 'SPKR18'                               )
         return
      end if

!
!     Check the request time against the bounds in the segment
!     descriptor.
!
      if (  ( et .lt. dc(1) )  .or.  ( et .gt. dc(2) )  )  then
         call setmsg( 'Request time # is outside of descriptor '   // &
                      'bounds # : #.'                               )
         call errdp ( '#',  et                                      )
         call errdp ( '#',  dc(1)                                   )
         call errdp ( '#',  dc(2)                                   )
         call sigerr( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         call chkout( 'SPKR18'                                      )
         return

      end if

!     We'll need the last two items before we can determine which
!     packets make up our output record.
!
      call dafgda ( handle, end-ctrlsz+1, end, contrl )

!
!     Check the FAILED flag just in case HANDLE is not attached to
!     any DAF file and the error action is not set to ABORT. You need
!     need to do this only after the first call to DAFGDA.
!
      if ( failed() ) then
         call chkout ( 'SPKR18' )
         return
      end if

      subtyp  =  nint ( contrl(1) )
      wndsiz  =  nint ( contrl(2) )
      n       =  nint ( contrl(3) )

      if ( n .lt. 2 )  then

         call setmsg( 'Packet count # is less than the minimum '   // &
                      'valid value, which is 2.'                   )
         call errint( '#',  n                                      )
         call sigerr( 'SPICE(TOOFEWSTATES)'                        )
         call chkout( 'SPKR18'                                     )
         return

      end if

!
!     From this point onward, we assume the segment was constructed
!     correctly.  In particular, we assume:
!
!        1)  The first and last epochs in the segment define a time
!            interval that contains the interval defined by the segment
!            descriptor's time bounds.
!
!        2)  The segment descriptor's time bounds are in order and are
!            distinct.
!
!        3)  The epochs in the segment are in strictly increasing
!            order.
!
!        4)  The degree of the interpolating polynomial specified by
!            the segment is at least 1 and is no larger than
!
!               MAXDEG
!


!
!     Set the packet size, which is a function of the subtype.
!
      if ( subtyp .eq. s18tp0 ) then

         packsz = s18ps0

      else if ( subtyp .eq. s18tp1 ) then

         packsz = s18ps1

      else

         call setmsg ( 'Unexpected SPK type 18 subtype # found in ' // &
                       'type 18 segment.'                           )
         call errint ( '#',  subtyp                                 )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                        )
         call chkout ( 'SPKR18'                                     )
         return

      end if

!
!     Check the window size.
!
      if ( wndsiz .le. 0 ) then

         call setmsg ( 'Window size in type 18 segment was #; must ' // &
                       'be positive.'                                )
         call errint ( '#',  subtyp                                  )
         call sigerr ( 'SPICE(INVALIDVALUE)'                         )
         call chkout ( 'SPKR18'                                      )
         return

      end if


      if ( subtyp .eq. s18tp0 ) then

         maxwnd  =  (maxdeg+1)/2

         if ( wndsiz .gt. maxwnd ) then

            call setmsg ( 'Window size in type 18 segment was #; max '// &
                          'allowed value is # for subtype 0 (Hermite,'// &
                          ' 12-element packets).'                     )
            call errint ( '#',  wndsiz                                )
            call errint ( '#',  maxwnd                                )
            call sigerr ( 'SPICE(INVALIDVALUE)'                       )
            call chkout ( 'SPKR18'                                    )
            return

         end if

         if ( odd(wndsiz) ) then

            call setmsg ( 'Window size in type 18 segment was #; '    // &
                          'must be even for subtype 0 (Hermite,'      // &
                          ' 12-element packets).'                     )
            call errint ( '#',  wndsiz                                )
            call sigerr ( 'SPICE(INVALIDVALUE)'                       )
            call chkout ( 'SPKR18'                                    )
            return

         end if


      else if ( subtyp .eq. s18tp1 ) then

         maxwnd  =  maxdeg + 1

         if ( wndsiz .gt. maxwnd ) then

            call setmsg ( 'Window size in type 18 segment was #; max '// &
                          'allowed value is # for subtype 1 '         // &
                          '(Lagrange, 6-element packets).'            )
            call errint ( '#',  wndsiz                                )
            call errint ( '#',  maxwnd                                )
            call sigerr ( 'SPICE(INVALIDVALUE)'                       )
            call chkout ( 'SPKR18'                                    )
            return

         end if

         if ( odd(wndsiz) ) then

            call setmsg ( 'Window size in type 18 segment was #; '// &
                          'must be even for subtype 1 '           // &
                          '(Lagrange, 6-element packets).'        )
            call errint ( '#',  wndsiz                            )
            call sigerr ( 'SPICE(INVALIDVALUE)'                   )
            call chkout ( 'SPKR18'                                )
            return

         end if

      else

         call setmsg ( 'This point should not be reached. Getting ' // &
                       'here may indicate that the code needs to '  // &
                       'updated to handle new subtypes.'            )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                        )
         call chkout ( 'SPKR18'                                     )
         return

      end if

!
!     We'll now select the set of packets that define the interpolating
!     polynomials.   We'll start out by finding the first directory
!     entry that is greater than or equal to the request epoch.  We'll
!     use the variable GROUP to indicate the set of epochs to search
!     within, once we've found the right directory entry.
!
      ndir    =  (n-1) / dirsiz
      dirbas  =  end  -  ndir  -  ctrlsz

      if ( ndir .eq. 0 ) then
!
!        There's no mystery about which group of epochs to search.
!
         group  = 1

      else
!
!        There's at least one directory.  Find the first directory
!        whose time is greater than or equal to the request time, if
!        there is such a directory.  We'll search linearly through the
!        directory entries, reading up to BUFSIZ of them at a time.
!        Having found the correct set of directory entries, we'll
!        perform a binary search within that set for the desired entry.
!
         bufbas  =  dirbas
         nread   =  min ( ndir, dirsiz )
         remain  =  ndir - nread

         call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

         do while (       ( buffer(nread) .lt. et ) &
                    .and. ( remain        .gt. 0  )   )

            bufbas  =  bufbas + nread
            nread   =  min ( remain, dirsiz )
            remain  =  remain - nread
!
!           Note:  NREAD is always > 0 here.
!
            call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

         end do

!
!        At this point, BUFBAS - DIRBAS is the number of directory
!        entries preceding the one contained in BUFFER(1).
!
         group  =     ( bufbas - dirbas ) &
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
         endidx  =  min ( n, dirsiz )

      else
!
!        If the group index is greater than 1, we'll include the last
!        time tag of the previous group in the set of time tags we look
!        up.  That way, the request time is bracketed by the time tag
!        set we look up.
!
         begidx  =      (  group  - 1 ) * dirsiz
         endidx  =  min (  begidx + dirsiz,  n  )

      end if


      timbas  =  dirbas - n

      call dafgda ( handle, timbas+begidx, timbas+endidx, buffer )

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
!     Unlike SPK types 8, 9, 12, and 13, for type 18 we adjust
!     the window size to keep the request time within the central
!     interval of the window.
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
!     Put the subtype and actual window size, which is the number of
!     packets in the record, into the output record.
!
      record(1) =  subtyp

      record(2) =  wndsiz

!
!     Read the packets.
!
      call dafgda ( handle, &
                    begin + (first-1)*packsz, &
                    begin +      last*packsz  -  1, &
                    record(3)                       )

!
!     Finally, add the epochs to the output record.
!
      start  =  begin   +  n * packsz  +  first - 2

      call dafgda (  handle, &
                     start + 1, &
                     start + wndsiz, &
                     record( 3 + wndsiz*packsz )   )

      call chkout ( 'SPKR18' )
      return
      end
