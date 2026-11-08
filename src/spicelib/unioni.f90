!$Procedure UNIONI ( Union two integer sets )

      subroutine unioni ( a, b, c )

! $ Abstract
!
!     Compute the union of two integer sets to form a third set.
!
! $ Required_Reading
!
!     SETS
!
! $ Keywords
!
!     CELLS
!     SETS
!
! $ Declarations

      implicit none

      integer               lbcell
      parameter           ( lbcell = -5 )

      integer          a      ( lbcell:* )
      integer          b      ( lbcell:* )
      integer          c      ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   First input set.
!     B          I   Second input set.
!     C          O   Union of A and B.
!
! $ Detailed_Input
!
!     A        is a set.
!
!
!     B        is a set, distinct from A.
!
! $ Detailed_Output
!
!     C        is a set, distinct from sets A and B, which
!              contains the union of A and B (that is, all of
!              the elements which are in A or B or both).
!
!              If the size (maximum cardinality) of C is smaller
!              than the cardinality of the union of A and B,
!              then only as many items as will fit in C are
!              included, and an error is signaled.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the union of the two sets causes an excess of elements, the
!         error SPICE(SETEXCESS) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     The UNION of two sets contains every element which is
!     in the first set, or in the second set, or in both sets.
!
!           {a,b}      union  {c,d}     =  {a,b,c,d}
!           {a,b,c}           {b,c,d}      {a,b,c,d}
!           {a,b,c,d}         {}           {a,b,c,d}
!           {}                {a,b,c,d}    {a,b,c,d}
!           {}                {}           {}
!
!     The following call
!
!           CALL UNIONC  ( PLANETS, ASTEROIDS, RESULT )
!
!     places the union of the character sets PLANETS and
!     ASTEROIDS into the character set RESULT.
!
!     The output set must be distinct from both of the input sets.
!     For example, the following calls are invalid.
!
!           CALL UNIONI  ( CURRENT,     NEW, CURRENT )
!           CALL UNIONI  (     NEW, CURRENT, CURRENT )
!
!     In each of the examples above, whether or not the subroutine
!     signals an error, the results will almost certainly be wrong.
!     Nearly the same effect can be achieved, however, by placing the
!     result into a temporary set, which is immediately copied back
!     into one of the input sets, as shown below.
!
!           CALL UNIONI ( CURRENT, NEW,  TEMP )
!           CALL COPYI  ( TEMP,    NEW )
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
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) (NJB)
!
!-&


! $ Index_Entries
!
!     union two integer sets
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 05-JAN-1989 (NJB)
!
!        Calling protocol for EXCESS updated. Call to SETMSG
!        deleted.
!
!-&


!
!     SPICELIB functions
!
      integer             cardi
      integer             sizei
      logical             return

!
!     Local variables
!

      integer             apoint
      integer             bpoint

      integer             csize
      integer             acard
      integer             bcard
      integer             ccard

      integer             over



!
!     Set up the error processing.
!
      if ( return () ) return
      call chkin ( 'UNIONI' )

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardi ( a )
      bcard = cardi ( b )
      csize = sizei ( c )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      apoint = 1
      bpoint = 1

      ccard  = 0
      over   = 0

!
!     When the ends of both input sets are reached, we're done.
!
      do while ( apoint .le. acard .or. bpoint .le. bcard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard .lt. csize ) then

            if ( apoint .gt. acard ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            else if ( bpoint .gt. bcard ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) .eq. b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1
               bpoint   = bpoint + 1

            else if ( a(apoint) .lt. b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)
               apoint   = apoint + 1

            else if ( a(apoint) .gt. b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = b(bpoint)
               bpoint   = bpoint + 1

            end if

!
!        Otherwise, stop filling the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if ( apoint .gt. acard ) then

               over   = over   + 1
               bpoint = bpoint + 1

            else if ( bpoint .gt. bcard ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) .eq. b(bpoint) ) then

               over   = over   + 1
               apoint = apoint + 1
               bpoint = bpoint + 1

            else if ( a(apoint) .lt. b(bpoint) ) then

               over   = over   + 1
               apoint = apoint + 1

            else if ( a(apoint) .gt. b(bpoint) ) then

               over   = over   + 1
               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardi ( ccard, c )

!
!     Report any excess.
!
      if ( over .gt. 0 ) then
         call excess ( over, 'set' )
         call sigerr ( 'SPICE(SETEXCESS)' )
      end if


      call chkout ( 'UNIONI' )

      return
      end
