!$Procedure SPKW21 ( Write SPK segment, type 21 )

      subroutine spkw21 ( handle, body,   center, frame, &
                          first,  last,   segid,  n, &
                          dlsize, dlines, epochs        )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Write a type 21 segment to an SPK file.
!
! $ Required_Reading
!
!     NAIF_IDS
!     SPK
!     TIME
!
! $ Keywords
!
!     EPHEMERIS
!     FILES
!
! $ Declarations

      implicit none

      include 'spk21.inc'

      integer(ip)           handle
      integer(ip)           body
      integer(ip)           center
      character*(*)         frame
      real(wp)              first
      real(wp)              last
      character*(*)         segid
      integer(ip)           n
      integer(ip)           dlsize
      real(wp)              dlines ( dlsize, * )
      real(wp)              epochs (         * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of an SPK file open for writing.
!     BODY       I   NAIF code for an ephemeris object.
!     CENTER     I   NAIF code for center of motion of BODY.
!     FRAME      I   Reference frame name.
!     FIRST      I   Start time of interval covered by segment.
!     LAST       I   End time of interval covered by segment.
!     SEGID      I   Segment identifier.
!     N          I   Number of difference lines in segment.
!     DLSIZE     I   Difference line size.
!     DLINES     I   Array of difference lines.
!     EPOCHS     I   Coverage end times of difference lines.
!     MAXTRM     P   Maximum number of terms per difference table
!                    component.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of an SPK file that has been
!              opened for writing.
!
!     BODY     is the NAIF integer code for an ephemeris object
!              whose state relative to another body is described
!              by the segment to be created.
!
!     CENTER   is the NAIF integer code for the center of motion
!              of the object identified by BODY.
!
!     FRAME    is the NAIF name for a reference frame relative to
!              which the state information for BODY is specified.
!
!     FIRST,
!     LAST     are, respectively, the start and stop times of
!              the time interval over which the segment defines
!              the state of BODY.
!
!     SEGID    is the segment identifier. An SPK segment
!              identifier may contain up to 40 characters.
!
!     N        is the number of difference lines in the input
!              difference line array.
!
!     DLSIZE   is the size of each difference line data structure
!              in the difference line array input DLINES. Let
!              MAXDIM be the dimension of each component of the
!              difference table within each difference line. Then
!              the size DLSIZE of the difference line is
!
!                 ( 4 * MAXDIM ) + 11
!
!     DLINES   contains a time-ordered array of difference lines.
!              The Ith difference line occupies elements (1,I)
!              through (MAXDIM,I) of DLINES, where MAXDIM is
!              as described above in the description of DLSIZE.
!              Each difference line represents the state (x, y,
!              z, dx/dt, dy/dt, dz/dt, in kilometers and
!              kilometers per second) of BODY relative to CENTER,
!              specified relative to FRAME, for an interval of
!              time. The time interval covered by the Ith
!              difference line ends at the Ith element of the
!              array EPOCHS (described below). The interval
!              covered by the first difference line starts at the
!              segment start time.
!
!              The contents of a difference line are as shown
!              below:
!
!                 Dimension  Description
!                 ---------  ----------------------------------
!                 1          Reference epoch of difference line
!                 MAXDIM     Stepsize function vector
!                 1          Reference position vector,  x
!                 1          Reference velocity vector,  x
!                 1          Reference position vector,  y
!                 1          Reference velocity vector,  y
!                 1          Reference position vector,  z
!                 1          Reference velocity vector,  z
!                 MAXDIM,3   Modified divided difference
!                            arrays (MDAs)
!                 1          Maximum integration order plus 1
!                 3          Integration order array
!
!              The reference position and velocity are those of
!              BODY relative to CENTER at the reference epoch.
!              (A difference line is essentially a polynomial
!              expansion of acceleration about the reference
!              epoch.)
!
!     EPOCHS   is an array of epochs corresponding to the members
!              of the difference line array. The epochs are
!              specified as seconds past J2000 TDB.
!
!              The first difference line covers the time interval
!              from the segment start time to EPOCHS(1). For
!              I > 1, the Ith difference line covers the half-open
!              time interval from, but not including, EPOCHS(I-1)
!              through EPOCHS(I).
!
!              The elements of EPOCHS must be strictly increasing.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     MAXTRM   is the maximum number of terms allowed in
!              each component of the difference table
!              contained in the input argument RECORD.
!              See the INCLUDE file spk21.inc for the value
!              of MAXTRM.
!
! $ Exceptions
!
!     If any of the following exceptions occur, this routine will return
!     without creating a new segment.
!
!     1)  If FRAME is not a recognized name, the error
!         SPICE(INVALIDREFFRAME) is signaled.
!
!     2)  If the last non-blank character of SEGID occurs past index 40,
!         the error SPICE(SEGIDTOOLONG) is signaled.
!
!     3)  If SEGID contains any nonprintable characters, the error
!         SPICE(NONPRINTABLECHARS) is signaled.
!
!     4)  If the number of difference lines N is not at least one,
!         the error SPICE(INVALIDCOUNT) is signaled.
!
!     5)  If FIRST is greater than LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     6)  If the elements of the array EPOCHS are not in strictly
!         increasing order, the error SPICE(TIMESOUTOFORDER) is
!         signaled.
!
!     7)  If the last epoch EPOCHS(N) is less than LAST, the error
!         SPICE(COVERAGEGAP) is signaled.
!
!     8)  If DLSIZE is greater than the limit
!
!            ( 4 * MAXTRM ) + 11
!
!         the error SPICE(DIFFLINETOOLARGE) is signaled. If
!         DLSIZE is less than 71, the error SPICE(DIFFLINETOOSMALL)
!         is signaled.
!
!     9)  Let KQMAX1 be the maximum integration order for a given
!         difference line. If any value at index KQMAX1-1 or greater
!         in the step size array of that difference line is zero, the
!         error SPICE(ZEROSTEP) is signaled. The checked portion
!         of the step size vector lies in the index range 2:KQMAX-1
!         of the difference line containing the vector.
!
! $ Files
!
!     A new type 21 SPK segment is written to the SPK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes an SPK type 21 data segment to the open SPK
!     file according to the format described in the type 21 section of
!     the SPK Required Reading. The SPK file must have been opened with
!     write access.
!
! $ Examples
!
!     Suppose that you have difference lines and are prepared to
!     produce a segment of type 21 in an SPK file.
!
!     The following code fragment could be used to add the new segment
!     to a previously opened SPK file attached to HANDLE. The file must
!     have been opened with write access.
!
!        C
!        C     Create a segment identifier.
!        C
!              SEGID = 'MY_SAMPLE_SPK_TYPE_21_SEGMENT'
!
!        C
!        C     Write the segment.
!        C
!              CALL SPKW21 (  HANDLE,  BODY,    CENTER,  FRAME,
!             .               FIRST,   LAST,    SEGID,   N,
!             .               DLSIZE,  DLINES,  EPOCHS         )
!
! $ Restrictions
!
!     1)  The validity of the difference lines is not checked by
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
!-    SPICELIB Version 2.0.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 21-AUG-2015 (NJB)
!
!        Relaxed step size error check to allow zero
!        values in elements indexed greater than KQMAX1-2.
!
!-    SPICELIB Version 1.0.0, 03-FEB-2014 (NJB)
!
!-&


! $ Index_Entries
!
!     write SPK type_21 ephemeris data segment
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           lastnb

      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           sidlen
      parameter           ( sidlen  =  40 )

      integer(ip)           fprint
      parameter           ( fprint  =  32 )

      integer(ip)           lprint
      parameter           ( lprint  = 126 )

!
!     MINDSZ is the minimum MDA size; this is the size
!     of type 1 MDAs.
!
      integer(ip)           mindsz
      parameter           ( mindsz =   71 )

!
!     Local variables
!
      integer(ip)           dscsiz
      parameter           ( dscsiz  =   5 )

      integer(ip)           dtype
      parameter           ( dtype   =  21 )

      integer(ip)           dirsiz
      parameter           ( dirsiz  = 100 )


!
!     Local variables
!
      real(wp)              descr  ( dscsiz )
      real(wp)              prvepc

      integer(ip)           chrcod
      integer(ip)           i
      integer(ip)           j
      integer(ip)           kqmax1
      integer(ip)           kqmloc
      integer(ip)           maxdim
      integer(ip)           maxdsz
      integer(ip)           refcod


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'SPKW21' )

!
!     Make sure the difference line size is within limits.
!
      maxdsz = ( 4 * maxtrm ) + 11

      if ( dlsize .gt. maxdsz ) then

         call setmsg ( 'The input difference line size is #, ' &
         //            'while the maximum supported by ' &
         //            'this routine is #. It is possible ' &
         //            'that this problem is due to your ' &
         //            'SPICE Toolkit being out of date.'      )
         call errint ( '#',  dlsize                            )
         call errint ( '#',  maxdsz                            )
         call sigerr ( 'SPICE(DIFFLINETOOLARGE)'               )
         call chkout ( 'SPKW21'                                )
         return

      end if

      if ( dlsize .lt. mindsz ) then

         call setmsg ( 'The input difference line size is #, ' &
         //            'while the minimum supported by ' &
         //            'this routine is #. It is possible ' &
         //            'that this problem is due to your ' &
         //            'SPICE Toolkit being out of date.'      )
         call errint ( '#',  dlsize                            )
         call errint ( '#',  mindsz                            )
         call sigerr ( 'SPICE(DIFFLINETOOSMALL)'               )
         call chkout ( 'SPKW21'                                )
         return

      end if

!
!     Get the NAIF integer code for the reference frame.
!
      call namfrm ( frame, refcod )

      if ( refcod .eq. 0 ) then

         call setmsg ( 'The reference frame # is not supported.'   )
         call errch  ( '#', frame                                  )
         call sigerr ( 'SPICE(INVALIDREFFRAME)'                    )
         call chkout ( 'SPKW21'                                    )
         return

      end if

!
!     Check to see if the segment identifier is too long.
!
      if ( lastnb(segid) .gt. sidlen ) then

         call setmsg ( 'Segment identifier contains more than ' // &
                       '40 characters.'                          )
         call sigerr ( 'SPICE(SEGIDTOOLONG)'                     )
         call chkout ( 'SPKW21'                                  )
         return

      end if

!
!     Now check that all the characters in the segment identifier
!     can be printed.
!
      do i = 1, lastnb(segid)

         chrcod = ichar( segid(i:i) )

         if ( ( chrcod .lt. fprint ) .or. ( chrcod .gt. lprint ) ) then

            call setmsg ( 'The segment identifier contains '  // &
                          'nonprintable characters'               )
            call sigerr ( 'SPICE(NONPRINTABLECHARS)'              )
            call chkout ( 'SPKW21'                                )
            return

         end if

      end do

!
!     The difference line count must be at least one.
!
      if ( n .lt. 1 ) then

         call setmsg ( 'The difference line count was #; the count ' // &
                       'must be at least one.'                       )
         call errint ( '#', n                                        )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                         )
         call chkout ( 'SPKW21'                                      )
         return

      end if

!
!     The segment stop time should be greater than or equal to
!     the begin time.
!
      if ( first .gt. last ) then

         call setmsg ( 'The segment start time: # is greater than ' // &
                       'the segment end time: #'                   )
         call errdp  ( '#', first                                  )
         call errdp  ( '#', last                                   )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                      )
         call chkout ( 'SPKW21'                                    )
         return

      end if

!
!     Make sure the epochs form a strictly increasing sequence.
!
      prvepc = epochs(1)

      do i = 2, n

         if ( epochs(i) .le. prvepc ) then

            call setmsg ( 'EPOCH # having index # is not greater '    // &
                          'than its predecessor #.'                   )
            call errdp  ( '#',  epochs(i)                             )
            call errint ( '#',  i                                     )
            call errdp  ( '#',  epochs(i-1)                           )
            call sigerr ( 'SPICE(TIMESOUTOFORDER)'                    )
            call chkout ( 'SPKW21'                                    )
            return

         end if

         prvepc = epochs(i)

      end do

!
!     Make sure there's no gap between the last difference line
!     epoch and the end of the time interval defined by the segment
!     descriptor.
!
      if (  epochs(n) .lt. last ) then

         call setmsg ( 'Segment has coverage gap: segment end time # ' &
         //            'follows last epoch #.'                        )
         call errdp  ( '#',  last                                     )
         call errdp  ( '#',  epochs(n)                                )
         call sigerr ( 'SPICE(COVERAGEGAP)'                           )
         call chkout ( 'SPKW21'                                       )
         return

      end if

!
!     Check the step size vectors in the difference lines.
!
      maxdim = ( dlsize - 11 ) / 4
      kqmloc = ( 4 * maxdim  ) + 8


      do i = 1, n
!
!        Check only the first KQMAX1-2 elements of the step size
!        vector. The higher-indexed elements are allowed to be zero.
!
         kqmax1 = nint( dlines(kqmloc,i) )

         do j = 2, kqmax1-1

            if ( dlines(j,i) .eq. 0.0_wp ) then

               call setmsg ( 'Step size was zero at step size vector ' &
               //            'index # within difference line #.'      )
               call errint ( '#',  j-1                                )
               call errint ( '#',  i                                  )
               call sigerr ( 'SPICE(ZEROSTEP)'                        )
               call chkout ( 'SPKW21'                                 )
               return

            end if

         end do

      end do

!
!     If we made it this far, we're ready to start writing the segment.
!
!     Create the segment descriptor.
!
      call spkpds ( body, center, frame, dtype, first, last, descr )

!
!     Begin a new segment.
!
      call dafbna ( handle, descr, segid )

      if ( failed() ) then
         call chkout ( 'SPKW21' )
         return
      end if

!
!     The type 21 segment structure is shown below:
!
!        +-----------------------+
!        | Difference line 1     |
!        +-----------------------+
!        | Difference line 2     |
!        +-----------------------+
!                   ...
!        +-----------------------+
!        | Difference line N     |
!        +-----------------------+
!        | Epoch 1               |
!        +-----------------------+
!        | Epoch 2               |
!        +-----------------------+
!                   ...
!        +-----------------------+
!        | Epoch N               |
!        +-----------------------+
!        | Epoch 100             | (First directory)
!        +-----------------------+
!                   ...
!        +-----------------------+
!        | Epoch (N/100)*100     | (Last directory)
!        +-----------------------+
!        | Max diff table size   |
!        +-----------------------+
!        | Number of diff lines  |
!        +-----------------------+
!

      call dafada ( dlines,  n*dlsize )
      call dafada ( epochs,  n        )

      do i = 1,   n / dirsiz
         call dafada (  epochs( dirsiz*i ),  1  )
      end do

      call dafada ( real( maxdim, wp ),  1 )
      call dafada ( real( n, wp      ),  1 )

!
!     As long as nothing went wrong, end the segment.
!
      if ( .not. failed() ) then
         call dafena
      end if

      call chkout ( 'SPKW21' )
      return
      end
