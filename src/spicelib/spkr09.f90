!$Procedure SPKR09 ( Read SPK record from segment, type 9 )

      subroutine spkr09 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single SPK data record from a segment of type 9
!     (Unequally spaced discrete states, interpolated by Lagrange
!     polynomials).
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
!              a SPK segment of type 9.
!
!     ET       is a target epoch, for which a data record from
!              a specific segment is required.
!
! $ Detailed_Output
!
!     RECORD   is a set of data from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative
!              to some center, in some inertial reference frame.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | number of states (n) |
!                 +----------------------+
!                 | state 1 (6 elts.)    |
!                 +----------------------+
!                 | state 2 (6 elts.)    |
!                 +----------------------+
!                             .
!                             .
!                             .
!                 +----------------------+
!                 | state n (6 elts.)    |
!                 +----------------------+
!                 | epochs 1--n          |
!                 +----------------------+
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine follows the pattern established in the lower-numbered
!     SPK data type readers of not explicitly performing error
!     diagnoses. Exceptions are listed below nonetheless.
!
!     1)  If the input HANDLE does not designate a loaded SPK file, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the segment specified by DESCR is not of data types 9 or
!         13, the error SPICE(WRONGSPKTYPE) is signaled.
!
!     3)  If the input ET value is not within the range specified
!         in the segment descriptor, the error SPICE(TIMEOUTOFBOUNDS)
!         is signaled.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     See the SPK Required Reading file for a description of the
!     structure of a data type 9 segment.
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
!           IF ( TYPE .EQ. 9 ) THEN
!              CALL SPKR09 ( HANDLE, DESCR, ET, RECORD )
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 2.0.0, 06-NOV-1999 (NJB)
!
!        Data type check was relaxed to enable reading type 13
!        segments.
!
!-    SPICELIB Version 1.0.1, 24-OCT-1994 (NJB)
!
!        In-line comment concerning transpose of state data was
!        removed.
!
!-    SPICELIB Version 1.0.0, 14-AUG-1993 (NJB)
!
!-&


! $ Index_Entries
!
!     read record from type_9 SPK segment
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           lstltd

      logical               odd
      logical               return

!
!     Local parameters
!
      integer(ip)           nd
      parameter           ( nd     = 2 )

      integer(ip)           ni
      parameter           ( ni     = 6 )

      integer(ip)           statsz
      parameter           ( statsz = 6 )

      integer(ip)           dirsiz
      parameter           ( dirsiz = 100 )

      integer(ip)           bufsiz
      parameter           ( bufsiz = dirsiz + 1 )


!
!     Local variables
!
      real(wp)              buffer ( bufsiz )
      real(wp)              contrl ( 2  )
      real(wp)              dc     ( nd )

      integer(ip)           begidx
      integer(ip)           begin
      integer(ip)           bufbas
      integer(ip)           degree
      integer(ip)           dirbas
      integer(ip)           end
      integer(ip)           endidx
      integer(ip)           first
      integer(ip)           group
      integer(ip)           high
      integer(ip)           i
      integer(ip)           ic     ( ni )
      integer(ip)           last
      integer(ip)           low
      integer(ip)           n
      integer(ip)           ndir
      integer(ip)           near
      integer(ip)           nread
      integer(ip)           remain
      integer(ip)           start
      integer(ip)           timbas
      integer(ip)           type
      integer(ip)           wndsiz


!
!     Use discovery check-in.
!
      if ( return () ) then
         return
      end if

!
!     Unpack the segment descriptor, and get the start and end addresses
!     of the segment.
!
      call dafus ( descr, 2, 6, dc, ic )

      type  = ic( 4 )
      begin = ic( 5 )
      end   = ic( 6 )

!
!     Make sure that this really is a type 9 or type 13 data segment.
!
      if (  ( type /= 09 ) .and. ( type /= 13 )  ) then
         call chkin ( 'SPKR09'                                        )
         call setmsg( 'You are attempting to locate type 9 or type ' // &
                      '13 data in a type # data segment.'             )
         call errint( '#',  type                                      )
         call sigerr( 'SPICE(WRONGSPKTYPE)'                           )
         call chkout( 'SPKR09'                                        )
         return
      end if

!
!     Check the request time against the bounds in the segment
!     descriptor.
!
      if (  ( et < dc(1) )  .or.  ( et > dc(2) )  )  then
         call chkin ( 'SPKR09'                                      )
         call setmsg( 'Request time # is outside of descriptor '   // &
                      'bounds # : #.'                               )
         call errdp ( '#',  et                                      )
         call errdp ( '#',  dc(1)                                   )
         call errdp ( '#',  dc(2)                                   )
         call sigerr( 'SPICE(TIMEOUTOFBOUNDS)'                      )
         call chkout( 'SPKR09'                                      )
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
!               ( L - 1 ) / 7         [integer division]
!
!            where L is the declared length of the argument RECORD.
!
!        5)  There are at least as many epochs in the segment as the
!            the number of points required to define an interpolating
!            polynomial of the specified degree.
!
!
!     We'll need the last two items before we can determine which
!     states make up our output record.
!
!
      call dafgda ( handle, end-1, end, contrl )

      degree  =  nint ( contrl(1) )
      n       =  nint ( contrl(2) )

      wndsiz  =  degree + 1

!
!     We'll now select the set of states that define the interpolating
!     polynomials.   We'll start out by finding the first directory
!     entry that is greater than or equal to the request epoch.  We'll
!     use the variable GROUP to indicate the set of epochs to search
!     within, once we've found the right directory entry.
!
      ndir    =  (n-1) / dirsiz
      dirbas  =  end  -  ndir  -  2

      if ( ndir == 0 ) then
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
         nread   =  min ( ndir, bufsiz )
         remain  =  ndir - nread

         call dafgda ( handle, bufbas+1, bufbas+nread, buffer )

         do while (       ( buffer(nread) < et ) &
                    .and. ( remain        > 0  )   )

            bufbas  =  bufbas + nread
            nread   =  min ( remain, bufsiz )
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
      if ( group == 1 ) then

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
      i     =  lstltd (  et,  endidx-begidx+1,  buffer  )

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
!     Now select the set of states used for interpolation.
!
      if ( odd(wndsiz) ) then
!
!        Find the index of the state whose epoch is closest to the
!        input epoch.  The index I is in the range [0, DIRSIZ],
!        since ENDIDX - BEGIDX never exceeds DIRSIZ, and ET is
!        never larger than the (ENDIDX-BEGIDX+1)th element of the
!        buffer.
!
         if ( i == 0 ) then
!
!           This can happen only if the request time matches the
!           first time tag of the segment.
!
            near = low

         else if (       dabs(  et  -  buffer(i)    ) &
                   <  dabs(  et  -  buffer(i+1)  )    )  then

            near = low

         else
            near = high
         end if

!
!        The epochs whose index is NEAR is the (WNDSIZ/2 + 1)th
!        of the interpolating set, unless the request time is too close
!        to the end of the coverage interval, in which case one endpoint
!        of the window will coincide with an endpoint of the coverage
!        interval.
!
         first =  min (  max ( near - degree/2,  1),   n-degree  )

         last  =  first  +  degree

      else
!
!        The group size is even.
!
!        The bracketing epochs we've found are the (WNDSIZ/2)th
!        and (WNDSIZ/2 + 1)th of the interpolating set, unless the
!        request time is too close to the end of the coverage interval,
!        in which case one endpoint of the window will coincide with
!        an endpoint of the coverage interval.
!
         first =  min (  max ( low - degree/2,  1 ),   n-degree  )

         last  =  first  +  degree

      end if

!
!     Put the size of the group of states into the output record.
!
      record(1) =  wndsiz

!
!     Read the states.
!
      call dafgda ( handle, &
                    begin + (first-1)*statsz, &
                    begin +      last*statsz  -  1, &
                    record(2)                       )

!
!     Finally, add the epochs to the output record.
!
      start  =  begin   +  n * statsz  +  first - 2

      call dafgda (  handle, &
                     start + 1, &
                     start + wndsiz, &
                     record( 2 + wndsiz*statsz )   )

      return
      end
