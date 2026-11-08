!$Procedure WNDIFD ( Difference two DP windows )

      subroutine wndifd ( a, b, c )

! $ Abstract
!
!     Place the difference of two double precision windows into
!     a third window.
!
! $ Required_Reading
!
!     WINDOWS
!
! $ Keywords
!
!     WINDOWS
!
! $ Declarations

      implicit none

      integer               lbcell
      parameter           ( lbcell = -5 )

      double precision      a        ( lbcell:* )
      double precision      b        ( lbcell:* )
      double precision      c        ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A,
!     B          I   Input windows.
!     C          O   Difference of A and B.
!
! $ Detailed_Input
!
!     A,
!     B        are SPICE windows, each of which contains zero or more
!              intervals.
!
! $ Detailed_Output
!
!     C        is the output SPICE window, containing the difference
!              of A and B --- every point contained in A, but not
!              contained in B.
!
!              C must be distinct from both A and B.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the difference of the two windows results in an excess of
!         elements, the error SPICE(WINDOWEXCESS) is signaled.
!
!     2)  The cardinality of the input windows must be even. Left
!         endpoints of stored intervals must be strictly greater than
!         preceding right endpoints. Right endpoints must be greater
!         than or equal to corresponding left endpoints. Invalid window
!         data are not diagnosed by this routine and may lead to
!         unpredictable results.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Mathematically, the difference of two windows contains every
!     point contained in the first window but not contained in the
!     second window.
!
!     Fortran offers no satisfactory floating point representation
!     of open intervals. Thus, for floating point windows we must
!     return the closure of the set theoretical difference: that is,
!     the difference plus the endpoints of the first window that are
!     contained in the second window.
!
! $ Examples
!
!     Let A contain the intervals
!
!           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
!
!     and B contain the intervals
!
!           [ 2, 4 ]  [ 8, 10 ]  [ 16, 18 ]
!
!     Then the difference of A and B contains the intervals
!
!           [ 1, 2 ]  [ 7, 8 ]  [ 10, 11 ]  [ 23, 27 ]
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 24-AUG-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Fixed I/O type
!        of argument C in $Brief_I/O table.
!
!        Added entry #2 in $Exceptions section.
!
!-    SPICELIB Version 2.0.0, 16-SEP-1998 (WLT)
!
!        The previous version did not work when removing
!        singletons. This has been corrected.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     difference two d.p. windows
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 27-FEB-1989  (HAN)
!
!        Due to the calling sequence and functionality changes
!        in the routine EXCESS, the method of signaling an
!        excess of elements needed to be changed.
!
!-&


!
!     SPICELIB functions
!
      integer               cardd
      integer               sized
      logical               return

!
!     Local variables
!
      integer               acard
      integer               bcard
      integer               csize

      integer               apb
      integer               ape
      integer               bpb
      integer               bpe
      integer               put
      double precision      f
      double precision      l
      integer               needed

      integer               over

      logical               keep
      logical               unrslv


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'WNDIFD' )

!
!     Find the cardinality of the input windows, and the allowed size
!     of the output window. Also, save the size of the second window.
!
      acard = cardd ( a )
      bcard = cardd ( b )
      csize = sized ( c )
      over  = 0

!
!     Empty out the output window.
!
      call ssized ( csize, c )

!
!     Let's handle the pathological cases first.
!
      if ( bcard .eq. 0 ) then
         call copyd ( a, c )
         call chkout ( 'WNDIFD' )
         return
      else if ( acard .eq. 0 ) then
         call chkout ( 'WNDIFD' )
         return
      end if

!
!     Now get pointers to the first intervals of A and B.
!
      apb = 1
      ape = 2
      bpb = 1
      bpe = 2
      put = 1
!
!     As long as the end pointer for A is less than the cardinality
!     of A we need to examine intervals and decide how much of
!     them to keep in C.
!
      do while ( ape .le. acard )
!
!        We will work with the interval [F,L] which starts out
!        as the next interval of A.  We modify it below as required
!        when subtracting out intervals of B.
!
         f      = a(apb)
         l      = a(ape)
!
!        Right now we have not resolved whether to keep the interval
!        [F,L], but until we know better we assume it is a keeper.
!
         unrslv =  bpe .le. bcard
         keep   = .true.

         do while ( unrslv )

            if ( l .lt. b(bpb) ) then
!
!              The interval [F,L] is before the next interval of B, we
!              have resolved what to do with this one.   It is a
!              keeper.
!
               unrslv = .false.

            else if ( f .gt. b(bpe) ) then
!
!              [F,L] is after the end of the current interval in B,
!              we need to look at the next interval of B
!
               bpb    = bpb + 2
               bpe    = bpe + 2
               unrslv = bpe .le. bcard

            else
!
!              There is some overlap between the current interval
!              of B and the current interval of A. There are
!              several possibilities
!
!              1) The current interval of A is contained in the
!                 current interval of B (This includes singleton
!                 intervals in A). We just mark [F,L] so that it
!                 won't be kept.  We have fully resolved what to
!                 do with [F,L].
!
!              2) The interval from B overlaps at the beginning
!                 of the interval of A
!
!                 B interval [......]
!                 A interval     [............]
!                 result of A-B     [.........]
!
!                 In this case we need to shrink the interval [F,L]
!                 but we have not resolved how much of the result
!                 to keep.
!
!              3) The interval from B falls inside the current
!                 interval [F,L]
!
!                 B interval        [......]
!                 A interval     [............]
!                 result of A-B  [..]      [..]
!
!                 If the interval from B is not a singleton, we store
!                 the first part of [F,L] in C and then set [F,L] to
!                 be the right interval which is still not resolved.
!
!                 If the B interval is a singleton we can ignore ignore
!                 it.  But we have not resolved what to do about
!                 [F,L], we need to look at the next interval of B.
!

!
!              4) The interval from B overlaps at the ending
!                 of the interval of A
!
!                 B interval          [......]
!                 A interval     [......]
!                 result of A-B  [....]
!
!                 We need to shrink [F,L]. In this case we know we can
!                 keep all of what's left because all other intervals
!                 of B are to the right of [F,L]
!
               if (       b(bpb) .le. f &
                    .and. l      .le. b(bpe) ) then
!
!                 Case 1 above
!
                  keep   = .false.
                  unrslv = .false.

               else if ( b(bpb) .le. f ) then
!
!                 Case 2 above
!
                  f      = b(bpe)
                  bpb    = bpb+2
                  bpe    = bpe+2
                  unrslv = bpe .le. bcard

               else if (      f      .le. b(bpb) &
                        .and. l      .ge. b(bpe) &
                        .and. b(bpb) .lt. b(bpe) ) then
!
!                 Case 3 above (non-singleton interval of B).
!

                  if ( put .lt. csize ) then
                     c(put)   = f
                     c(put+1) = b(bpb)

                     call scardd ( put+1, c )

                     put      = put + 2
                  else
                     over = over + 2
                  end if

                  f      = b(bpe)
!
!                 If the interval from B contained L, we will not
!                 want to be keeping the singleton [F,L].
!
                  if ( f .eq. l ) then
                     keep   = .false.
                     unrslv = .false.
                  end if

                  bpb    = bpb + 2
                  bpe    = bpe + 2
                  unrslv = unrslv .and. bpe .le. bcard

               else if (      f      .le. b(bpb) &
                        .and. l      .ge. b(bpe) &
                        .and. b(bpb) .eq. b(bpe) ) then
!
!                 Case 3 above (singleton interval of B).
!
                  bpb    = bpb + 2
                  bpe    = bpe + 2
                  unrslv = bpe .le. bcard

               else
!
!                 Case 4 above
!
                  l      =  b(bpb)
                  unrslv = .false.

               end if

            end if

         end do

!
!        If there is anything to keep in C, put it there.
!
         if ( keep ) then
!
!           Make sure there is sufficient room to do the putting.
!
            if ( put .lt. csize ) then
               c(put)   = f
               c(put+1) = l
               call scardd ( put+1, c )
               put      = put + 2
            else
               over = over + 2
            end if

         end if
!
!        Move the pointers in A to the next interval.
!
         apb = apb + 2
         ape = ape + 2

      end do

!
!     We've examined all of the intervals of A and B, but if we
!     didn't actually store all of the difference, signal an error.
!
      if ( over .gt. 0 ) then

         needed = over + csize

         call setmsg ( 'The output window did not have ' &
         //            'sufficient room to contain the result of ' &
         //            'the window difference.  It has room for ' &
         //            '# endpoints, but # were needed to ' &
         //            'describe the difference. ' )

         call errint ( '#', csize  )
         call errint ( '#', needed )
         call sigerr ( 'SPICE(WINDOWEXCESS)'  )

      end if

      call chkout ( 'WNDIFD' )

      return
      end
