C$Procedure      T_CHOOSE ( Compute "N choose K" )

      INTEGER FUNCTION T_CHOOSE ( N, K )
      IMPLICIT NONE

C$ Abstract
C
C     Return "N choose K," the number of combinations of N items,
C     taken K at a time.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     SPK
C
C$ Declarations

      INTEGER               N
      INTEGER               K

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Number of elements of a set.
C     K          I   Size of subset.
C
C     The function returns "N choose K," the number of combinations
C     of N items, taken K at at time.
C
C$ Detailed_Input
C
C     N              is some non-negative integer representing the
C                    cardinality of a finite, possibly empty, set.
C
C     K              is the cardinality of a subset of a set of
C                    cardinality N.
C
C$ Detailed_Output
C
C     The function returns "N choose K," the number of combinations
C     of N items, taken K at at time.  This number is equal to
C
C            N!
C        ---------
C        (N-K)! K!
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If N is negative, the value zero is returned.
C
C     2) If K is negative, the value zero is returned.
C
C     3) If K > N, the value zero is returned.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The quantity "N choose K" occurs frequently in mathematical
C     applications, so it's convenient to encapsulate the computation
C     in a routine.  This quantity, among other things, is the Kth
C     coefficient
C
C         C
C          K
C
C     in the binomial expansion
C
C                           N
C                          ___
C                 N        \           N-K  K
C        ( a + b )    =    /      C   a    b
C                          ---     K
C                         K = 0
C
C$ Examples
C
C     1) Print the coefficients of the polynomial
C
C                7
C           (x+1)
C
C
C
C           INTEGER T_CHOOSE
C           INTEGER I
C
C           DO I = 0, 7
C              WRITE(*,*) T_CHOOSE(7,I)
C           END DO
C
C           END
C
C$ Restrictions
C
C     1) No check for arithmetic overflow is done in this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TESTUTIL Version 2.0.1, 24-NOV-2021 (NJB)
C
C        This routine has been moved to the TESTUTIL library.
C
C-    TSPICE Version 2.0.0, 19-OCT-2012 (NJB)
C
C        Renamed for use in TSPICE. An error was corrected in the
C        comments describing the computation method. Redundant
C        parentheses were added to a computation to clarify the order
C        of operations.
C
C-    SPKNIO Version 1.0.0, 06-APR-2000 (NJB)
C
C-&

C
C     Local variables
C
      INTEGER               I
      INTEGER               R

C
C     Deal with the exceptional cases first.
C
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

C
C     We wish to compute
C
C            n!
C        ---------
C        (n-k)! k!
C
C
C     This is equal to
C
C        [ N (N-1) (N-2)...(N-K+1) ] / [ K (K-1)... 1 ]
C
C     The above number is an integer because it's the number
C     of possible ways to choose a subset of k items out of
C     a set of n items. One can also prove the number is
C     an integer by mathematical induction; the induction
C     step is to prove, for 0 < k < n, that
C
C
C            n!                (n-1)!                 (n-1)!
C        ---------  =  ---------------------   +  -------------
C        (n-k)! k!     ((n-1)-(k-1))! (k-1)!      ((n-1)-k)! k!
C
C     The right hand side terms are integers by induction. The
C     case of k = 0 is evident by inspection.
C
C     Since we know that
C
C        n * (n-1) * ... * (n-k+1)
C        -------------------------
C        1 *   2   * ... * k
C
C     is an integer for any k,  0 < k <= n, we can build up the
C     expression we want by iterating on k.
C
C     We must take care to avoid any division that would have a
C     non-integer result; we can do this by taking as the numerator the
C     product of the first i numerator terms in the above quotient and
C     taking as the denominator the product of the first i terms of the
C     denominator above. The quotient of those two products is
C     guaranteed to be an integer since it's equal to "n choose i."
C

      R         =  N - K + 1
      T_CHOOSE  =  1

      DO I = N, R, -1
         T_CHOOSE  =  ( T_CHOOSE  *  I )  /  ( N + 1 - I )
      END DO

      END
