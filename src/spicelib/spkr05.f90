!$Procedure SPKR05 ( Read SPK record from segment, type 5 )

      subroutine spkr05 ( handle, descr, et, record )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Read a single SPK data record from a segment of type 5
!     ( two body propagation between discrete state vectors ).
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
!              the type 05 SPK segment to be read.
!
!     ET       is a target epoch, specified as ephemeris seconds past
!              J2000, for which a data record from the segment is
!              required.
!
! $ Detailed_Output
!
!     RECORD   is a logical record from the specified segment which,
!              when evaluated at epoch ET, will give the state
!              (position and velocity) of some body, relative
!              to some center, in some inertial reference frame.
!
!              The structure of RECORD is:
!
!                  RECORD(1)
!                     .            state of the body at epoch 1.
!                     .
!                     .
!                  RECORD(6)
!
!                  RECORD(7)
!                     .
!                     .            state of the body at epoch 2.
!                     .
!                  RECORD(12)
!                  RECORD(13)      epoch 1 in seconds past 2000.
!                  RECORD(14)      epoch 2 in seconds past 2000.
!                  RECORD(15)      GM for the center of motion.
!
!
!              Epoch 1 and epoch 2 are the times in the segment that
!              bracket ET. If ET is less than the first time in the
!              segment then both epochs 1 and 2 are equal to the
!              first time. And if ET is greater than the last time
!              then, epochs 1 and 2 are set equal to this last time.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the segment specified by DESCR is not of data type 05,
!         the error SPICE(WRONGSPKTYPE) is signaled.
!
!     2)  No error is signaled if ET is outside the time bounds of
!         the segment. The output RECORD will contain epochs and the
!         associated states which satisfy the rules stated above.
!
! $ Files
!
!     See argument HANDLE.
!
! $ Particulars
!
!     This routine reads the segment specified by DESCR from the SPK
!     file attached to HANDLE to locate the two epochs in the segment
!     that bracket the input ET. It then returns a logical record which
!     contains these times and their associated states, and also the
!     mass of the center of motion. The routine makes explicit use of
!     the structure of the type 05 data segment to locate this data.
!
!     See the section of the SPK Required Reading on data type 05 for
!     a description of the structure of a type 05 segment.
!
! $ Examples
!
!     The data returned by the SPKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the SPKRnn
!     routines might be used to "dump" and check segment data for a
!     particular epoch.
!
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!           IF ( FOUND ) THEN
!
!     C
!     C        Look at parts of the descriptor.
!     C
!              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!              CENTER = ICD( 2 )
!              REF    = ICD( 3 )
!              TYPE   = ICD( 4 )
!
!              IF ( TYPE .EQ. 05 ) THEN
!
!                 CALL SPKR05 ( HANDLE, DESCR, ET, RECORD )
!                     .
!                     .  Look at the RECORD data.
!                     .
!              END IF
!
!           END IF
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     J.M. Lynch         (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW)
!
!        Replaced DAFRDA call with DAFGDA.
!        Added IMPLICIT NONE.
!
!-    SPICELIB Version 1.0.0, 01-APR-1992 (JML) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     read record from type_5 SPK segment
!
!-&


!
!     Local parameters
!
      integer(ip)           size
      parameter           ( size    = 100 )

      integer(ip)           dirsiz
      parameter           ( dirsiz  = size )

      integer(ip)           bufsiz
      parameter           ( bufsiz  = size )

      integer(ip)           stasiz
      parameter           ( stasiz =    6 )

      integer(ip)           nd
      parameter           ( nd =        2 )

      integer(ip)           ni
      parameter           ( ni =        6 )

!
!     SPICELIB functions
!
      integer(ip)           lstltd
      logical               return

!
!     Local variables
!
      real(wp)              dc       (   nd )
      real(wp)              data     ( size )

      integer(ip)           ic       (   ni )
      integer(ip)           begin
      integer(ip)           end
      integer(ip)           type
      integer(ip)           nrec
      integer(ip)           ndir
      integer(ip)           group
      integer(ip)           dirloc
      integer(ip)           remain
      integer(ip)           skip
      integer(ip)           grpadd
      integer(ip)           addrss
      integer(ip)           i
      integer(ip)           n

      logical               fnd



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPKR05' )
      end if

!
!     Unpack the segment descriptor.
!
      call dafus ( descr, nd, ni, dc, ic )

      type  = ic(4)
      begin = ic(5)
      end   = ic(6)

!
!     Make sure that this really is a type 5 data segment.
!
      if ( type /= 05 ) then
         call setmsg( 'You are attempting to locate type 5 '      // &
                      'data in a type # data segment.'              )
         call errint( '#',  type                                    )
         call sigerr( 'SPICE(WRONGSPKTYPE)'                         )
         call chkout( 'SPKR05'                                      )
         return
      end if

!
!     Get the number of records in the segment. While we're at it,
!     get the GM of the central body (it's adjacent to NREC)
!     since we'll need it anyway. Put it where it belongs, at the
!     end of the output record.
!
      call dafgda ( handle, end-1, end, data )

      nrec       = nint ( data(2) )
      record(15) =        data(1)

!
!     From the number of records, we can compute the number of
!     directory epochs.
!
      ndir = nrec / dirsiz

!
!     The directory epochs narrow down the search to a group of DIRSIZ
!     or fewer records. Because the Ith directory epoch is the I*100th
!     epoch, the Ith group will contain epochs ((I-1)*100 + 1) through
!     (I*100).  For example:
!                            group   first epoch #   last epoch #
!                            -----   -------------   ------------
!                              1               1          100
!                              2             101          200
!                              .               .            .
!                              .               .            .
!                             10             901         1000
!                              .               .            .
!                              .               .            .
!                              N     (N-1)*100+1        N*100

      if ( ndir == 0 ) then
!
!        There is only one group if there are no directory epochs.
!
         group = 1

      else
!
!        Compute the location of the first directory epoch.  From the
!        beginning of the segment, we need to go through all of the
!        NREC states and epochs.
!
         dirloc = begin + ( stasiz + 1 ) * nrec

!
!        Determine which group of DIRSIZ times to search, by finding
!        the last directory epoch that is less than ET.
!
         fnd    = .false.
         remain =  ndir
         group  =  0

         do while ( .not. fnd )
!
!           Read in as many as BUFSIZ directory epochs at a time
!           for comparison.
!
            n = min( remain, bufsiz )

            call dafgda ( handle, dirloc, dirloc + n - 1, data )

            remain = remain - n

!
!           Determine the last directory element in DATA that's less
!           than ET.
!
!           If we reach the end of the directories, and still haven't
!           found one bigger than the epoch, the group is the last group
!           in the segment.
!
!           Otherwise keep looking.
!
!
            i = lstltd( et, n, data )

            if ( i < n ) then

               group =  group + i + 1
               fnd   = .true.

            else if ( remain == 0 ) then

               group =  ndir + 1
               fnd   = .true.

            else

               dirloc = dirloc + n
               group  = group  + n

            end if

         end do

      end if

!
!     Now we know which group of DIRSIZ (or less) epochs to look at.
!     Out of the NREC epochs, the number that we should skip over
!     to get to the proper group is DIRSIZ * ( GROUP - 1 ).
!
      skip = dirsiz * ( group - 1 )

!
!     From this we can compute the index into the segment of the group
!     of times we want.  From the beginning, we need to pass through
!     STASIZ * NREC state numbers to get to the first epoch. Then we
!     skip over the number just computed above.
!
      grpadd = begin + nrec * ( stasiz ) + skip

!
!     The number of epochs that we have to look at may be less than
!     DIRSIZ.  However many there are, go ahead and read them into the
!     buffer.
!
!     If there are no times in the last group then the time that we
!     are looking for is the same as the last directory epoch.
!     We should not try to read in this instance.
!
      n = min ( dirsiz, nrec - skip )

      if ( n /= 0 ) then

         call dafgda ( handle, grpadd, grpadd + n - 1, data )
!
!        Find the index of the largest time in the group that is less
!        than the input time.
!
         i = lstltd ( et, n, data )

      else
!
!        If we are here it means that ET is greater then the last
!        time in the segment and there are no elements in the last
!        group.  This can occur when the number of epochs is a multiple
!        DIRSIZ.
!
!        By setting N equal to I we can handle this case in the
!        same branch as when there are elements in the last group.
!        This is because the DATA array still contains the directory
!        epochs and I is pointing at the last element which is also the
!        last time in the segment.
!
         n = i

      end if

!
!     At this point N is the number of epochs in this GROUP which is
!     also the size of the array DATA which contains the epochs. I is
!     the index of the largest time in DATA which is less than ET.
!
!     We need to take different actions depending on whether ET is less
!     than the first time or greater than the last one in the GROUP.
!

      if ( i == 0 ) then

         if ( group == 1 ) then

!
!           ET is less than or equal to the first time in the segment.
!           Return the state at the first time twice.
!
            record(13) = data(1)
            record(14) = data(1)

            call dafgda ( handle, begin, begin + 5, data )

            call moved  ( data, 6, record(1) )
            call moved  ( data, 6, record(7) )

            call chkout( 'SPKR05' )
            return

         else
!
!           ET is less than or equal to the first time in this group
!           but not the first time in the segment. Get the last time
!           from the preceding group. The states for this case will
!           be read outside of the IF block.
!
            call dafgda ( handle, grpadd - 1, grpadd, data )

            record(13) = data(1)
            record(14) = data(2)

         end if

      else if ( i == n ) then

         if ( group == (ndir + 1) ) then
!
!           ET is greater than all of the times in the segment. Return
!           the state for the last time twice.
!
            record(13) = data(n)
            record(14) = data(n)

            addrss = begin + ( nrec - 1 ) * stasiz

            call dafgda ( handle, addrss, addrss + 5, data )

            call moved ( data, 6, record(1) )
            call moved ( data, 6, record(7) )

            call chkout( 'SPKR05' )
            return

         else
!
!           ET is greater than the last time in this group but this is
!           not the last time in the segment.  Need the first time from
!           the following group. The states for this case will be read
!           outside of the IF block.
!
            call dafgda ( handle, grpadd + n - 1, grpadd + n, data )

            record(13) = data(1)
            record(14) = data(2)

         end if

      else
!
!        There are two times in the group that bracket ET. The states
!        for this case will be read outside of the IF block.
!
         record(13) = data( i )
         record(14) = data(i+1)

      end if

!
!     Read the consecutive states for the two epochs found above.
!     ET is greater than the (SKIP + I)th time but less than or
!     equal to the time (SKIP + I + 1).
!

      addrss = begin + ( skip + i - 1 ) * stasiz

      call dafgda ( handle, addrss, addrss + 11, data )

      call moved  ( data, 12, record(1) )

      call chkout( 'SPKR05' )
      return
      end
