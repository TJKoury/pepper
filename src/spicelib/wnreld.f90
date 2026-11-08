!$Procedure WNRELD ( Compare two DP windows )

      logical function  wnreld ( a, op, b )

! $ Abstract
!
!     Compare two double precision windows.
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
      character*(*)         op
      double precision      b        ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   First window.
!     OP         I   Comparison operator.
!     B          I   Second window.
!
!     The function returns the result of comparison: A (OP) B.
!
! $ Detailed_Input
!
!     A,
!     B        are SPICE windows, each of which contains zero or more
!              intervals.
!
!     OP       is a comparison operator, indicating the way in which the
!              input sets are to be compared. OP may be any of the
!              following:
!
!                 Operator             Meaning
!                 --------  -------------------------------------------
!                   '='     A = B is .TRUE. if A and B are equal
!                           (contain the same intervals).
!
!                   '<>'    A <> B is .TRUE. if A and B are not equal.
!
!                   '<='    A <= B is .TRUE. if A is a subset of B.
!
!                   '<'     A < B is .TRUE. if A is a proper subset
!                           of B.
!
!                   '>='    A >= B is .TRUE. if B is a subset of A.
!
!                   '>'     A > B is .TRUE. if B is a proper subset
!                           of A.
!
! $ Detailed_Output
!
!     The function returns the result of the comparison.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the relational operator is not recognized, the error
!         SPICE(INVALIDOPERATION) is signaled.
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
!     This function returns .TRUE. whenever the specified relationship
!     between the input windows, A and B, is satisfied. For example,
!     the expression
!
!        WNRELD ( NEEDED, '<=', AVAIL )
!
!     is .TRUE. whenever the window NEEDED is a subset of the window
!     AVAIL. One window is a subset of another window if each of
!     the intervals in the first window is included in one of the
!     intervals in the second window. In addition, the first window
!     is a proper subset of the second if the second window contains
!     at least one point not contained in the first window. (Thus,
!     '<' implies '<=', and '>' implies '>='.)
!
!     The following pairs of expressions are equivalent.
!
!        WNRELD ( A, '>', B )
!        WNRELD ( B, '<', A )
!
!        WNRELD ( A, '>=', B )
!        WNRELD ( B, '<=', A )
!
! $ Examples
!
!     Let A contain the intervals
!
!           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
!
!     Let B and C contain the intervals
!
!           [ 1, 2 ]  [ 9, 9 ]  [ 24, 27 ]
!
!     Let D contain the intervals
!
!           [ 5, 10 ]  [ 15, 25 ]
!
!     Finally, let E and F be empty windows (containing no intervals).
!
!     Because B and C contain the same intervals,
!
!           WNRELD ( B, '=',  C )
!           WNRELD ( B, '<=', C )
!           WNRELD ( B, '>=', C )
!
!     are all true, while
!
!           WNRELD ( B, '<>', C )
!
!     is .FALSE. Because neither B nor C contains any points not also
!     contained by the other, neither is a proper subset of the other.
!     Thus,
!
!           WNRELD ( B, '<', C )
!           WNRELD ( B, '>', C )
!
!     are both false.
!
!     Every point contained in B and C is also contained in A. Thus,
!
!           WNRELD ( B, '<=', A )
!           WNRELD ( A, '>=', C )
!
!     are both true. In addition, A contains points not contained in
!     B and C. (That is, the differences A-B and A-C are not empty.)
!     Thus, B and C are proper subsets of A as well, and
!
!           WNRELD ( B, '<', A )
!           WNRELD ( A, '>', B )
!
!     are both true.
!
!     Although A and D have points in common, neither contains the
!     other. Thus
!
!           WNRELD ( A, '=',  D )
!           WNRELD ( A, '<=', D )
!           WNRELD ( A, '>=', D )
!
!     are all false.
!
!     In addition, any window is equal to itself, a subset of itself,
!     and a superset of itself. Thus,
!
!           WNRELD ( A, '=',  A )
!           WNRELD ( A, '<=', A )
!           WNRELD ( A, '>=', A )
!
!     are always true. However, no window is a proper subset or a
!     proper superset of itself. Thus,
!
!           WNRELD ( A, '<', A )
!           WNRELD ( A, '>', A )
!
!     are always false.
!
!     Finally, an empty window is a a proper subset of any window
!     except another empty window. Thus,
!
!           WNRELD ( E, '<', A )
!
!     is .TRUE., but
!
!           WNRELD ( E, '<', F )
!
!     is .FALSE.
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
!        Edited the header to comply with NAIF standard. Added entry #2
!        in $Exceptions section.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        Set the default function value to either 0, 0.0D0, .FALSE.,
!        or blank depending on the type of the function.
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
!     compare two d.p. windows
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 02-FEB-1989 (HAN)
!
!        If the relational operator is not recognized, an error is
!        signaled. The previous version returned .FALSE. as the
!        function value, and no error was signaled.
!
!        Also, the $Required_Reading section has been changed to
!        include WINDOWS as the required reading for the module.
!
!-&


!
!     SPICELIB functions
!
      logical               return
      integer               cardd
      logical               wnincd

!
!     Local variables
!
      integer               acard
      integer               bcard
      logical               equal
      logical               subset
      integer               i



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         wnreld = .false.
         return
      else
         call chkin ( 'WNRELD' )
         wnreld = .false.
      end if

!
!     Find the cardinality of the input windows.
!
      acard  = cardd ( a )
      bcard  = cardd ( b )

!
!     A and B are equal if they contain exactly the same intervals.
!     We need to know this for nearly every relationship, so find out
!     before going any further.
!
         if ( acard .ne. bcard ) then
            equal = .false.

         else
            equal = .true.

            do i = 1, acard
               equal = equal .and. ( a(i) .eq. b(i) )
            end do
         end if

!
!     Simple equality and inequality are trivial at this point.
!
      if ( op .eq. '=' ) then

         wnreld = equal

      else if ( op .eq. '<>' ) then

         wnreld = .not. equal

!
!     Subsets are a little trickier. A is a subset of B if every
!     interval in A is included in B. In addition, A is a proper
!     subset if A and B are not equal.
!
      else if ( op .eq. '<='  .or.  op .eq. '<' ) then

         subset = .true.

         do i = 1, acard, 2
            subset = subset .and. ( wnincd ( a(i), a(i+1), b ) )
         end do

         if ( op .eq. '<=' ) then
            wnreld = subset

         else
            wnreld = subset .and. ( .not. equal )
         end if

!
!     A and B change places here...
!
      else if ( op .eq. '>='  .or.  op .eq. '>' ) then

         subset = .true.

         do i = 1, bcard, 2
            subset = subset .and. ( wnincd ( b(i), b(i+1), a ) )
         end do

         if ( op .eq. '>=' ) then
            wnreld = subset

         else
            wnreld = subset .and. ( .not. equal )
         end if

!
!     An unrecognized operator always fails.
!
      else

         call setmsg ( 'Relational operator, *, is not '         // &
                       'recognized.'                              )
         call errch  ( '*', op                   )
         call sigerr ( 'SPICE(INVALIDOPERATION)' )
         call chkout ( 'WNRELD' )
         return

      end if


      call chkout ( 'WNRELD' )
      return
      end
