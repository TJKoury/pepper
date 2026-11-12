!$Procedure      T_CHOOSE ( Compute "N choose K" )

      INTEGER FUNCTION T_CHOOSE ( N, K )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Return "N choose K," the number of combinations of N items,
!     taken K at a time.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     SPK
!
! $ Declarations

      INTEGER               N
      INTEGER               K

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     N          I   Number of elements of a set.
!     K          I   Size of subset.
!
!     The function returns "N choose K," the number of combinations
!     of N items, taken K at at time.
!
! $ Detailed_Input
!
!     N              is some non-negative integer representing the
!                    cardinality of a finite, possibly empty, set.
!
!     K              is the cardinality of a subset of a set of
!                    cardinality N.
!
! $ Detailed_Output
!
!     The function returns "N choose K," the number of combinations
!     of N items, taken K at at time.  This number is equal to
!
!            N!
!        ---------
!        (N-K)! K!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1) If N is negative, the value zero is returned.
!
!     2) If K is negative, the value zero is returned.
!
!     3) If K > N, the value zero is returned.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The quantity "N choose K" occurs frequently in mathematical
!     applications, so it's convenient to encapsulate the computation
!     in a routine.  This quantity, among other things, is the Kth
!     coefficient
!
!         C
!          K
!
!     in the binomial expansion
!
!                           N
!                          ___
!                 N        \           N-K  K
!        ( a + b )    =    /      C   a    b
!                          ---     K
!                         K = 0
!
! $ Examples
!
!     1) Print the coefficients of the polynomial
!
!                7
!           (x+1)
!
!
!
!           INTEGER T_CHOOSE
!           INTEGER I
!
!           DO I = 0, 7
!              WRITE(*,*) T_CHOOSE(7,I)
!           END DO
!
!           END
!
! $ Restrictions
!
!     1) No check for arithmetic overflow is done in this routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TESTUTIL Version 2.0.1, 24-NOV-2021 (NJB)
!
!        This routine has been moved to the TESTUTIL library.
!
!-    TSPICE Version 2.0.0, 19-OCT-2012 (NJB)
!
!        Renamed for use in TSPICE. An error was corrected in the
!        comments describing the computation method. Redundant
!        parentheses were added to a computation to clarify the order
!        of operations.
!
!-    SPKNIO Version 1.0.0, 06-APR-2000 (NJB)
!
!-&

!
!     Local variables
!
      INTEGER               I
      INTEGER               R

!
!     Deal with the exceptional cases first.
!
      IF ( N .LT. 0 ) THEN

         T_CHOOSE = 0
         RETURN

      ELSE IF ( K .LT. 0 ) THEN

         T_CHOOSE = 0
         RETURN

      ELSE IF ( K .GT. N ) THEN

         T_CHOOSE = 0
         RETURN

      END IF

!
!     We wish to compute
!
!            n!
!        ---------
!        (n-k)! k!
!
!
!     This is equal to
!
!        [ N (N-1) (N-2)...(N-K+1) ] / [ K (K-1)... 1 ]
!
!     The above number is an integer because it's the number
!     of possible ways to choose a subset of k items out of
!     a set of n items. One can also prove the number is
!     an integer by mathematical induction; the induction
!     step is to prove, for 0 < k < n, that
!
!
!            n!                (n-1)!                 (n-1)!
!        ---------  =  ---------------------   +  -------------
!        (n-k)! k!     ((n-1)-(k-1))! (k-1)!      ((n-1)-k)! k!
!
!     The right hand side terms are integers by induction. The
!     case of k = 0 is evident by inspection.
!
!     Since we know that
!
!        n * (n-1) * ... * (n-k+1)
!        -------------------------
!        1 *   2   * ... * k
!
!     is an integer for any k,  0 < k <= n, we can build up the
!     expression we want by iterating on k.
!
!     We must take care to avoid any division that would have a
!     non-integer result; we can do this by taking as the numerator the
!     product of the first i numerator terms in the above quotient and
!     taking as the denominator the product of the first i terms of the
!     denominator above. The quotient of those two products is
!     guaranteed to be an integer since it's equal to "n choose i."
!

      R         =  N - K + 1
      T_CHOOSE  =  1

      DO I = N, R, -1
         T_CHOOSE  =  ( T_CHOOSE  *  I )  /  ( N + 1 - I )
      END DO

      END
