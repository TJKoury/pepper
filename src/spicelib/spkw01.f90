!$Procedure SPKW01 ( Write SPK segment, type 1 )

      subroutine spkw01 (  handle,  body,    center,  frame,   first, &
                           last,    segid,   n,       dlines,  epochs )

! $ Abstract
!
!     Write a type 1 segment to an SPK file.
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

      integer               dlsize
      parameter           ( dlsize = 71 )

      integer               handle
      integer               body
      integer               center
      character*(*)         frame
      double precision      first
      double precision      last
      character*(*)         segid
      integer               n
      double precision      dlines ( dlsize, * )
      double precision      epochs (         * )

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
!     DLINES     I   Array of difference lines.
!     EPOCHS     I   Coverage end times of difference lines.
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
!     DLINES   contains a time-ordered array of difference lines
!              The Ith difference line occupies elements (1,I)
!              through (71,I) of DLINES. Each difference line
!              represents the state (x, y, z, dx/dt, dy/dt,
!              dz/dt, in kilometers and kilometers per second)
!              of BODY relative to CENTER, specified relative to
!              FRAME, for an interval of time. The time interval
!              covered by the Ith difference line ends at the
!              Ith element of the array EPOCHS (described below).
!              The interval covered by the first difference line
!              starts at the segment start time.
!
!              The contents of a difference line are as shown
!              below:
!
!                 Dimension  Description
!                 ---------  ----------------------------------
!                 1          Reference epoch of difference line
!                 15         Stepsize function vector
!                 1          Reference position vector,  x
!                 1          Reference velocity vector,  x
!                 1          Reference position vector,  y
!                 1          Reference velocity vector,  y
!                 1          Reference position vector,  z
!                 1          Reference velocity vector,  z
!                 15,3       Modified divided difference
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
!
!     EPOCHS   is an array of epochs corresponding to the members
!              of the state array. The epochs are specified as
!              seconds past J2000, TDB.
!
!              The first difference line covers the time interval
!              from the segment start time to EPOCHS(1).  For
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
!     None.
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
!     5)  If FIRST is greater than or equal to LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     6)  If the elements of the array EPOCHS are not in strictly
!         increasing order, the error SPICE(TIMESOUTOFORDER) is
!         signaled.
!
!     7)  If the last epoch EPOCHS(N) is less than LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
! $ Files
!
!     A new type 1 SPK segment is written to the SPK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes an SPK type 1 data segment to the open SPK
!     file according to the format described in the type 1 section of
!     the SPK Required Reading. The SPK file must have been opened with
!     write access.
!
! $ Examples
!
!     Suppose that you have difference lines and are prepared to
!     produce a segment of type 1 in an SPK file.
!
!     The following code fragment could be used to add the new segment
!     to a previously opened SPK file attached to HANDLE. The file must
!     have been opened with write access.
!
!        C
!        C     Create a segment identifier.
!        C
!                  SEGID = 'MY_SAMPLE_SPK_TYPE_1_SEGMENT'
!
!        C
!        C     Write the segment.
!        C
!              CALL SPKW01 (  HANDLE,  BODY,    CENTER,  FRAME,
!             .               FIRST,   LAST,    SEGID,   N,
!             .               DLINES,  EPOCHS                  )
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
!-    SPICELIB Version 1.0.2, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 07-APR-2010 (NJB)
!
!        Updated $Detailed_Input to state that the elements
!        of EPOCHS must be strictly increasing. The $Exceptions
!        section already described this error condition.
!
!-    SPICELIB Version 1.0.0, 30-JAN-2003 (NJB)
!
!-&


! $ Index_Entries
!
!     write SPK type_1 ephemeris data segment
!
!-&


!
!     SPICELIB functions
!
      integer               lastnb

      logical               failed
      logical               return

!
!     Local parameters
!
      integer               sidlen
      parameter           ( sidlen  =  40 )

      integer               fprint
      parameter           ( fprint  =  32 )

      integer               lprint
      parameter           ( lprint  = 126 )

!
!     Local variables
!
      integer               dscsiz
      parameter           ( dscsiz  =   5 )

      integer               typidx
      parameter           ( typidx  =   4 )

      integer               dtype
      parameter           ( dtype   =   1 )

      integer               dirsiz
      parameter           ( dirsiz  = 100 )


!
!     Local variables
!
      double precision      descr  ( dscsiz )
      double precision      maxtim

      integer               chrcod
      integer               i
      integer               refcod


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SPKW01' )
      end if

!
!     Get the NAIF integer code for the reference frame.
!
      call namfrm ( frame, refcod )

      if ( refcod .eq. 0 ) then

         call setmsg ( 'The reference frame # is not supported.'   )
         call errch  ( '#', frame                                  )
         call sigerr ( 'SPICE(INVALIDREFFRAME)'                    )
         call chkout ( 'SPKW01'                                    )
         return

      end if

!
!     Check to see if the segment identifier is too long.
!
      if ( lastnb(segid) .gt. sidlen ) then

         call setmsg ( 'Segment identifier contains more than ' // &
                       '40 characters.'                          )
         call sigerr ( 'SPICE(SEGIDTOOLONG)'                     )
         call chkout ( 'SPKW01'                                  )
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
            call chkout ( 'SPKW01'                                )
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
         call chkout ( 'SPKW01'                                      )
         return

      end if

!
!     The segment stop time should be greater then the begin time.
!
      if ( first .ge. last ) then

         call setmsg ( 'The segment start time: # is greater then ' // &
                       'the segment end time: #'                   )
         call errdp  ( '#', first                                  )
         call errdp  ( '#', last                                   )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                      )
         call chkout ( 'SPKW01'                                    )
         return

      end if

!
!     Make sure the epochs form a strictly increasing sequence.
!
      maxtim = epochs(1)

      do i = 2, n

         if ( epochs(i) .le. maxtim ) then

            call setmsg ( 'EPOCH # having index # is not greater '    // &
                          'than its predecessor #.'                   )
            call errdp  ( '#',  epochs(i)                             )
            call errint ( '#',  i                                     )
            call errdp  ( '#',  epochs(i-1)                           )
            call sigerr ( 'SPICE(TIMESOUTOFORDER)'                    )
            call chkout ( 'SPKW01'                                    )
            return
         else
            maxtim = epochs(i)
         end if

      end do

!
!     Make sure there's no gap between the last difference line
!     epoch and the end of the time interval defined by the segment
!     descriptor.
!
      if (  epochs(n) .lt. last ) then

         call setmsg ( 'Segment end time # follows last epoch #.'     )
         call errdp  ( '#',  last                                     )
         call errdp  ( '#',  epochs(n)                                )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                         )
         call chkout ( 'SPKW01'                                       )
         return

      end if

!
!     If we made it this far, we're ready to start writing the segment.
!
!
!     Create the segment descriptor.
!
      call spkpds ( body, center, frame, dtype, first, last, descr )

!
!     Begin a new segment.
!
      call dafbna ( handle, descr, segid )

      if ( failed() ) then
         call chkout ( 'SPKW01' )
         return
      end if


!
!     The type 1 segment structure is shown below:
!
!        +-----------------------+
!        | Difference line 1     |
!        +-----------------------+
!        | Difference line 2     |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Difference line N     |
!        +-----------------------+
!        | Epoch 1               |
!        +-----------------------+
!        | Epoch 2               |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Epoch N               |
!        +-----------------------+
!        | Epoch 100             | (First directory)
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Epoch (N/100)*100     | (Last directory)
!        +-----------------------+
!        | Number of diff lines  |
!        +-----------------------+
!
!

      call dafada ( dlines,  n*dlsize )
      call dafada ( epochs,  n        )

      do i = 1,   n / dirsiz
         call dafada (  epochs( dirsiz*i ),  1  )
      end do

      call dafada ( dble( n ),  1 )

!
!     As long as nothing went wrong, end the segment.
!
      if ( .not. failed() ) then
         call dafena
      end if


      call chkout ( 'SPKW01' )
      return
      end
