!$Procedure SWAPAC ( Swap array, character )

      subroutine swapac ( n, locn, m, locm, array )

! $ Abstract
!
!     Swap (exchange) two non-intersecting groups of contiguous
!     elements of a character array.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!
! $ Declarations

      implicit none

      integer               n
      integer               locn
      integer               m
      integer               locm
      character*(*)         array  ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          I   Number of elements in the first group.
!     LOCN       I   Location of the first group.
!     M          I   Number of elements in the second group.
!     LOCM       I   Location of the second group.
!     ARRAY     I-O  The array.
!
! $ Detailed_Input
!
!     N,
!     LOCN     define the first group of elements to be exchanged:
!              ARRAY(LOCN) through ARRAY(LOCN+N-1).
!
!     M,
!     LOCM     define the second group of elements to be exchanged:
!              ARRAY(LOCM) through ARRAY(LOCM+M-1). These must be
!              distinct from the first group.
!
!     ARRAY    on input contains both groups of elements in their
!              original locations.
!
! $ Detailed_Output
!
!     ARRAY    on output contains the input array with the indicated
!              groups of elements exchanged.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the elements to be swapped are not distinct, the error
!         SPICE(NOTDISTINCT) is signaled.
!
!     2)  If LOCN or LOCM is less than one, the error
!         SPICE(INVALIDINDEX) is signaled.
!
!     3)  If the number of elements to be swapped is less than zero,
!         the error SPICE(INVALIDARGUMENT) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     If N [M] is zero, the second [first] group is removed from
!     its current location and inserted in front of ARRAY(LOCN)
!     [ARRAY(LOCM)]. Thus, to move the second [first] group to the
!     front of the list, set N [M] and LOCN [LOCM] to zero and one
!     respectively. To move the group to the end of the list, set
!     N [M] and LOCN [LOCM] to zero and one more than the number of
!     elements in the array.
!
!     All of the elements to be swapped must be distinct.
!
! $ Examples
!
!     Let ARRAY contain the following elements.
!
!           Roosevelt
!           Truman
!           Eisenhower
!           Kennedy
!           Johnson
!           Nixon
!           Ford
!           Carter
!           Reagan
!           Cuomo
!
!     Then the following calls
!
!           CALL SWAPAC (  1,  2,  2,  7,  ARRAY )
!           CALL SWAPAC (  3,  1,  3,  8,  ARRAY )
!           CALL SWAPAC (  3,  4,  0,  1,  ARRAY )
!           CALL SWAPAC (  2,  4,  0,  11, ARRAY )
!
!     yield the following arrays respectively.
!
!           [1]          [2]          [3]          [4]
!
!           Roosevelt    Carter       Kennedy      Roosevelt
!           Ford         Reagan       Johnson      Truman
!           Carter       Cuomo        Nixon        Eisenhower
!           Eisenhower   Kennedy      Roosevelt    Nixon
!           Kennedy      Johnson      Truman       Ford
!           Johnson      Nixon        Eisenhower   Carter
!           Nixon        Ford         Ford         Reagan
!           Truman       Roosevelt    Carter       Cuomo
!           Reagan       Truman       Reagan       Kennedy
!           Cuomo        Eisenhower   Cuomo        Johnson
!
!     The following calls
!
!           CALL SWAPAC ( 3, 2, 4, 5, ARRAY )
!           CALL SWAPAC ( 4, 5, 3, 2, ARRAY )
!
!     yield the following arrays. Note that the resulting arrays
!     are equivalent.
!
!           [1]          [2]
!
!           Roosevelt    Roosevelt
!           Johnson      Johnson
!           Nixon        Nixon
!           Ford         Ford
!           Carter       Carter
!           Truman       Truman
!           Eisenhower   Eisenhower
!           Kennedy      Kennedy
!           Reagan       Reagan
!           Cuomo        Cuomo
!
!
!     The calls
!
!           CALL SWAPAC ( 3,  5, 4,  6, ARRAY )
!           CALL SWAPAC ( 3, -3, 3, 10, ARRAY )
!
!     signal the errors
!
!           SPICE(NOTDISTINCT)
!           SPICE(INVALIDINDEX)
!
!     respectively.
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 05-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.1, 18-MAY-2010 (BVS)
!
!        Removed "C$" marker from text in the header.
!
!-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in CYCLAC call.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     swap elements of character array
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 09-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in CYCLAC call.
!
!-    Beta Version 2.0.0, 3-JAN-1989 (HAN)
!
!        The "Particulars" section stated that by setting N [M]
!        to zero, the second [first] group is removed from its current
!        location and inserted in front of ARRAY(LOCM) [ARRAY(LOCN)].
!        That statement was incorrect. Insertion occurs in front of
!        ARRAY(LOCN) [ARRAY(LOCM)]. The section has been corrected.
!
!        New checks for locations were added. LOCN and LOCM must be
!        greater than one, not zero as specified before. If they are
!        not, and error is signaled.
!
!        More examples were added to the "Examples" section, and
!        the long error messages were revised.
!
!-&


!
!     SPICELIB functions
!
      logical               return


!
!     Local variables
!
      integer               nn
      integer               ln
      integer               nm
      integer               lm

      integer               direct
      integer               extra

      character*1           dir
      integer               begsub
      integer               nsub

      integer               i



!
!     We will assume that LOCN and N refer to the earlier group of
!     elements, LOCM and M to the later group. (We can always make
!     this true by exchanging their values.) We also assume that
!     all the elements to be swapped are distinct. (That is, LOCM
!     is greater than or equal to LOCN+N.)
!
!     It's easy enough to swap elements on a one-to-one basis, but
!     what about the ones left over? Without extra storage, they can
!     be moved one at a time; but each such move requires moving every
!     element between the origin and destination as well. For large
!     arrays, this is clearly unacceptable.
!
!     In the figure below, the array on the left contains two groups
!     of elements which are to be swapped. We can begin by swapping the
!     leading elements of each group one-for-one.
!
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        | Adam         |        | Barney       |
!        +--------------+        +--------------+
!        | Alvin        |        | Betty        |
!        +--------------+        +--------------+
!        |              |        |              |  <---+
!        +--------------+        +--------------+      |
!        |              |        |              |      |
!        +--------------+        +--------------+      |
!        | Barney       |        | Adam         |      |
!        +--------------+        +--------------+      |
!        | Betty        |        | Alvin        |      |
!        +--------------+        +--------------+      |
!        | Bill         |        | Bill         |      |
!        +--------------+        +--------------+      |
!        | Bob          |        | Bob          |  <---+
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!
!     Notice that cycling the indicated sub-array forward twice brings
!     the remaining elements to their proper locations. This is most
!     fortunate, because cycling the elements of an array is a linear
!     operation. (See CYCLAx for details.)
!
!     And what if the extra elements are in the first group?
!
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        | Barney       |        | Adam         |
!        +--------------+        +--------------+
!        | Betty        |        | Alvin        |
!        +--------------+        +--------------+
!        | Bill         |        | Bill         |  <---+
!        +--------------+        +--------------+      |
!        | Bob          |        | Bob          |      |
!        +--------------+        +--------------+      |
!        |              |        |              |      |
!        +--------------+        +--------------+      |
!        |              |        |              |      |
!        +--------------+        +--------------+      |
!        | Adam         |        | Barney       |      |
!        +--------------+        +--------------+      |
!        | Alvin        |        | Betty        |  <---+
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!        |              |        |              |
!        +--------------+        +--------------+
!
!     In this case, the indicated sub-array must be cycled backward
!     in order to bring the extra elements to their proper places.
!
!     The algorithm is:
!
!        1) Let DIRECT be the smaller of N and M, and let EXTRA
!           be the absolute value of the difference (N-M).
!
!        2) Exchange DIRECT elements directly.
!
!        3) Determine the direction of the cycle: forward when N < M,
!           backward when N > M.
!
!        4) Determine the sub-array to be cycled. It begins at element
!           (LOCN+DIRECT) and contains (LOCM-LOCN) + (M-DIRECT) elements
!
!        5) Cycle the sub-array EXTRA times in the indicated direction.
!




!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SWAPAC' )
      end if


!
!     Check to see if the inputs are valid.
!
      if ( n .lt. 0 ) then

         call setmsg ( 'Number of elements in the first group is *.' )
         call errint ( '*', n                   )
         call sigerr ( 'SPICE(INVALIDARGUMENT)' )
         call chkout ( 'SWAPAC' )
         return

      else if ( m .lt. 0 ) then

         call setmsg ( 'Number of elements in the second group is *.' )
         call errint ( '*', m                   )
         call sigerr ( 'SPICE(INVALIDARGUMENT)' )
         call chkout ( 'SWAPAC' )
         return

      else if ( locn .lt. 1 ) then

         call setmsg ( 'Location of the first group is *.' )
         call errint ( '*', locn             )
         call sigerr ( 'SPICE(INVALIDINDEX)' )
         call chkout ( 'SWAPAC' )
         return

      else if ( locm .lt. 1 ) then

         call setmsg ( 'Location of the second group is *.' )
         call errint ( '*', locm             )
         call sigerr ( 'SPICE(INVALIDINDEX)' )
         call chkout ( 'SWAPAC' )
         return

      end if

!
!     Make sure we have the groups in the right order.
!
      if ( locn .lt. locm ) then
         ln = locn
         lm = locm
         nn = n
         nm = m
      else
         ln = locm
         lm = locn
         nn = m
         nm = n
      end if

!
!     The elements must be distinct.
!
      if ( lm .lt. ln + nn ) then
         call setmsg ( 'Elements to be swapped are not distinct.' )
         call sigerr ( 'SPICE(NOTDISTINCT)' )
         call chkout ( 'SWAPAC' )
         return
      end if

!
!     Direct exchange.
!
      direct = min ( nn, nm )

      do i = 0, direct-1
         call swapc ( array(ln+i), array(lm+i) )
      end do

!
!     Cycle.
!
      extra = abs ( nn - nm )

      if ( extra .gt. 0 ) then

         if ( nn .lt. nm ) then
            dir = 'F'
         else
            dir = 'B'
         end if

         begsub = ln + direct
         nsub   = (lm - ln) + (nm - direct)

         call cyacip ( nsub, dir, extra, array(begsub) )

      end if


      call chkout ( 'SWAPAC' )
      return
      end
