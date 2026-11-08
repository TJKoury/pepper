!$Procedure INTERD ( Intersect two double precision sets )

      subroutine interd ( a, b, c )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Intersect two double precision sets to form a third set.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CELLS
!     SETS
!
! $ Declarations

      implicit none

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      real(wp) a      ( lbcell:* )
      real(wp) b      ( lbcell:* )
      real(wp) c      ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   First input set.
!     B          I   Second input set.
!     C          O   Intersection of A and B.
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
!              contains the intersection of A and B (that is,
!              all of the elements which are in A, AND in B).
!
!              If the size (maximum cardinality) of C is smaller
!              than the cardinality of the intersection of A and B,
!              then only as many items as will fit in C are
!              included, and an error is signaled.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the intersection of the two sets causes an excess of
!         elements, the error SPICE(SETEXCESS) is signaled.
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
!     The INTERSECTION of two sets contains every element
!     which is in the first set AND in the second set.
!
!           {a,b}      intersect  {c,d}     =  {}
!           {a,b,c}               {b,c,d}      {b,c}
!           {a,b,c,d}             {}           {}
!           {}                    {a,b,c,d}    {}
!           {}                    {}           {}
!
!     The following call
!
!           CALL INTERC  ( PLANETS, ASTEROIDS, RESULT )
!
!     places the intersection of the character sets PLANETS and
!     ASTEROIDS into the character set RESULT.
!
!     The output set must be distinct from both of the input sets.
!     For example, the following calls are invalid.
!
!           CALL INTERI  ( CURRENT,     NEW, CURRENT )
!           CALL INTERI  (     NEW, CURRENT, CURRENT )
!
!     In each of the examples above, whether or not the subroutine
!     signals an error, the results will almost certainly be wrong.
!     Nearly the same effect can be achieved, however, by placing the
!     result into a temporary set, which is immediately copied back
!     into one of the input sets, as shown below.
!
!           CALL INTERI ( CURRENT, NEW,  TEMP )
!           CALL COPYI  ( TEMP,    NEW        )
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
!-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
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
!     intersect two d.p. sets
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
!
!        Calling protocol of EXCESS changed. Call to SETMSG removed.
!
!-&


!
!     SPICELIB functions
!
      integer(ip)         cardd
      integer(ip)         sized
      logical             return

!
!     Local variables
!

      integer(ip)         apoint
      integer(ip)         bpoint

      integer(ip)         csize
      integer(ip)         acard
      integer(ip)         bcard
      integer(ip)         ccard

      integer(ip)         over



!
!     Set up the error processing.
!
      if ( return () ) return

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      acard = cardd ( a )
      bcard = cardd ( b )
      csize = sized ( c )

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
!     When the end of either input set is reached, we're done.
!
      do while ( apoint <= acard .and. bpoint <= bcard )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         if ( ccard < csize ) then

            if ( a(apoint) == b(bpoint) ) then

               ccard    = ccard + 1
               c(ccard) = a(apoint)

               apoint   = apoint + 1
               bpoint   = bpoint + 1

            else if ( a(apoint) < b(bpoint) ) then

               apoint = apoint + 1

            else if ( b(bpoint) < a(apoint) ) then

               bpoint = bpoint + 1

            end if


!
!        Otherwise, stop following the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         else

            if (a(apoint) == b(bpoint)) then

               over   = over   + 1
               apoint = apoint + 1
               bpoint = bpoint + 1

            else if (a(apoint) < b(bpoint)) then

               apoint = apoint + 1

            else if (b(bpoint) < a(apoint)) then

               bpoint = bpoint + 1

            end if

         end if

      end do

!
!     Set the cardinality of the output set.
!
      call scardd ( ccard, c )

!
!     Report any excess.
!
      if ( over > 0 ) then
         call excess ( over, 'set' )
         call sigerr ( 'SPICE(SETEXCESS)' )
      end if


      return
      end
