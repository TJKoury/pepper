!$Procedure WNINTD ( Intersect two DP windows )

      subroutine wnintd ( a, b, c )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Place the intersection of two double precision windows into
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

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      real(wp)              a        ( lbcell:* )
      real(wp)              b        ( lbcell:* )
      real(wp)              c        ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A,
!     B          I   Input windows.
!     C          O   Intersection of A and B.
!
! $ Detailed_Input
!
!     A,
!     B        are SPICE windows, each of which contains zero or more
!              intervals.
!
! $ Detailed_Output
!
!     C        is the output SPICE window, containing the
!              intersection of A and B --- every point contained in both
!              A and B.
!
!              C must be distinct from both A and B.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the intersection of the two windows results in an excess of
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
!     The intersection of two windows contains every point contained
!     both in the first window and in the second window.
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
!     Then the intersection of A and B contains the intervals
!
!           [ 2, 3 ]  [ 8, 10 ]
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
!-    SPICELIB Version 1.1.0, 24-AUG-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Fixed I/O type
!        of argument C in $Brief_I/O table.
!
!        Added entry #2 in $Exceptions section.
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
!     intersect two d.p. windows
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
      integer(ip)           cardd
      integer(ip)           sized
      logical               return

!
!     Local variables
!
      integer(ip)           acard
      integer(ip)           bcard
      integer(ip)           csize

      integer(ip)           ap
      integer(ip)           bp
      integer(ip)           cp
      character(1)          use
      integer(ip)           over



!
!     Standard SPICE error handling.
!
      if ( return () ) return

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
      over = 0

!
!     When the end of either input window is reached, we're done.
!
      do while ( ap < acard  .and.  bp < bcard )

!
!        Let's see what we can do with the earlier of the next
!        intervals from A and B.
!
         if ( a(ap+1) < b(bp+1) ) then
            use = 'A'

         else if ( b(bp+1) <= a(ap+1) ) then
            use = 'B'
         end if

!
!        If there is still space in the output window, fill it
!        as necessary. Otherwise, stop filling the array, but continue
!        to count the number of elements in excess of the size of the
!        output window.
!
!        The general idea is this: if the next interval of A overlaps
!        the next interval of B, save the area of overlap. (Similarly
!        for B.)
!
         if ( use == 'A' ) then

            if ( a(ap+1) >= b(bp) ) then

               if ( cp < csize ) then
                  cp      = cp + 2
                  c(cp-1) = max ( b(bp), a(ap) )
                  c(cp  ) = a(ap+1)
               else
                  over    = over + 2
               end if

            end if

            ap = ap + 2

!
!        This is the same as the last clause, with B replacing A.
!
         else if ( use == 'B' ) then

            if ( b(bp+1) >= a(ap) ) then

               if ( cp < csize ) then
                  cp      = cp + 2
                  c(cp-1) = max ( a(ap), b(bp) )
                  c(cp  ) = b(bp+1)
               else
                  over    = over + 2
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
!     If there are any excess elements, signal an error and check out
!     as usual.
!
      if ( over > 0 ) then
         call excess ( over, 'window' )
         call sigerr ( 'SPICE(WINDOWEXCESS)' )
      end if




      return
      end
