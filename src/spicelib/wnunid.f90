!$Procedure WNUNID ( Union two DP windows )

      subroutine wnunid ( a, b, c )

! $ Abstract
!
!     Place the union of two double precision windows into a third
!     window.
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
!     C          O   Union of A and B.
!
! $ Detailed_Input
!
!     A,
!     B        are SPICE windows, each of which contains zero or more
!              intervals.
!
! $ Detailed_Output
!
!     C        is the output SPICE window, containing the union of A
!              and B --- every point contained in A, or in B, or in
!              both.
!
!              C must be distinct from both A and B.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the union of the two windows results in an excess of
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
!     The union of two windows contains every point contained in the
!     first window, or the second window, or both.
!
! $ Examples
!
!     Let A contain the intervals
!
!           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
!
!     and B contain the intervals
!
!           [ 2, 6 ]  [ 8, 10 ]  [ 16, 18 ]
!
!     Then the union of A and B contains the intervals
!
!           [ 1, 6 ]  [ 7, 11 ]  [ 16, 18 ]  [ 23, 27 ]
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
!-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Fixed I/O type
!        of argument C in $Brief_I/O table.
!
!        Added entry #2 in $Exceptions section. Removed unnecessary
!        entries in $Revisions section.
!
!-    SPICELIB Version 1.1.0, 08-FEB-1999 (WLT)
!
!        The variable END was not initialized in the previous
!        edition. It is now initialized to be the minimum of
!        A(1) and B(1).
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
!     union two d.p. windows
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

      integer               ap
      integer               bp
      integer               cp
      double precision      end
      character*1           use
      integer               over




!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'WNUNID' )
      end if


!
!     Find the cardinality of the input windows, and the allowed size
!     of the output window.
!
      acard = cardd ( a )
      bcard = cardd ( b )
      csize = sized ( c )

!
!     Begin with the input pointers at the first elements of the
!     input windows. The initial cardinality of the output window
!     is zero. And there is no overflow so far.
!
!     (Note that AP and BP point to the LEFT endpoints of intervals
!     in A and B, while CP points to the RIGHT endpoint of the latest
!     interval in C.)
!
      ap   = 1
      bp   = 1
      cp   = 0
      end  = min( a(1), b(1) )
      over = 0

!
!     When the ends of both input windows are reached, we're done.
!
      do while ( ap .lt. acard  .or.  bp .lt. bcard )

!
!        If the end of one window has been reached, copy (or merge)
!        the next interval from the other window.
!
         if ( ap .gt. acard ) then
            use = 'B'

         else if ( bp .gt. bcard ) then
            use = 'A'

!
!        Otherwise, let's see what we can do with the earlier of
!        the next intervals from A and B.
!
         else if ( a(ap) .lt. b(bp) ) then
            use = 'A'

         else if ( b(bp) .le. a(ap) ) then
            use = 'B'
         end if

!
!        If there is still space in the output window, fill it
!        as necessary. Otherwise, stop filling the array, but continue
!        to count the number of elements in excess of the size of the
!        output window.
!
!        The general idea is this: if the next interval overlaps the
!        latest output interval, merge the two (extending the output
!        interval to the right). Otherwise, insert the next interval
!        intact.
!
         if ( use .eq. 'A' ) then

            if ( cp .lt. csize ) then

               if ( a(ap) .le. end .and. cp .gt. 0 ) then
                  c(cp) = max ( c(cp), a(ap+1) )
               else
                  cp      = cp + 2
                  c(cp-1) = a(ap  )
                  c(cp  ) = a(ap+1)
               end if

               end = c(cp)

            else

               if ( a(ap) .le. end ) then
                  end  = max ( end, a(ap+1) )
               else
                  over = over + 2
                  end  = a(ap+1)
               end if

            end if

            ap = ap + 2

!
!        This is the same as the last clause, with B replacing A.
!
         else if ( use .eq. 'B' ) then

            if ( cp .lt. csize ) then

               if ( b(bp) .le. end .and. cp .gt. 0 ) then
                  c(cp)   = max ( c(cp), b(bp+1) )
               else
                  cp      = cp + 2
                  c(cp-1) = b(bp  )
                  c(cp  ) = b(bp+1)
               end if

               end = c(cp)

            else

               if ( b(bp) .le. end ) then
                  end  = max ( end, b(bp+1) )
               else
                  over = over + 2
                  end  = b(bp+1)
               end if
            end if

            bp = bp + 2

         end if

      end do

!
!     Set the cardinality of the output window.
!
      call scardd ( cp, c )

!
!     If there is an excess of elements, signal an error and check out
!     as usual.
!
      if ( over .gt. 0 ) then
         call excess ( over, 'window' )
         call sigerr ( 'SPICE(WINDOWEXCESS)' )
      end if


      call chkout ( 'WNUNID' )

      return
      end
