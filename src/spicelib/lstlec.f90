!$Procedure LSTLEC ( Last character element less than or equal to. )

      INTEGER FUNCTION  LSTLEC ( STRING, N, ARRAY )

! $ Abstract
!
!     Find the index of the largest array element less than or equal to
!     a given character string in an ordered array of character strings.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    STRING
      INTEGER          N
      CHARACTER*(*)    ARRAY ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Upper bound value to search against.
!     N          I   Number of elements in ARRAY.
!     ARRAY      I   Array of possible lower bounds.
!
!     The function returns the index of the last element of ARRAY that
!     is lexically less than or equal to STRING.
!
! $ Detailed_Input
!
!     STRING   is a string acting as an upper bound: the element of
!              ARRAY that is lexically the greatest element less than or
!              equal to STRING is to be found. Trailing blanks in this
!              bound value are not significant.
!
!     N        is the total number of elements in ARRAY.
!
!     ARRAY    is an array of character strings to be searched. Trailing
!              blanks in the strings in this array are not significant.
!              The strings in ARRAY must be sorted in non-decreasing
!              order. The elements of ARRAY need not be distinct.
!
! $ Detailed_Output
!
!     The function returns the index of the highest-indexed element in
!     the input array that is lexically less than or equal to STRING.
!     The routine assumes the array elements are sorted in
!     non-decreasing order.
!
!     Indices range from 1 to N.
!
!     If all elements of ARRAY are lexically greater than STRING, the
!     routine returns the value 0. If N is less than or equal to zero,
!     the routine returns the value 0.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If N is less than or equal to zero, the function returns 0.
!         This case is not treated as an error.
!
!     2)  If the input array is not sorted in non-decreasing order, the
!         output of this routine is undefined. No error is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine uses a binary search algorithm and so requires
!     at most on the order of
!
!        log (N)
!           2
!
!     steps to compute the value of LSTLEC.
!
!     Note: If you need to find the first element of the array that is
!     lexically greater than STRING, simply add 1 to the result returned
!     by this function and check to see if the result is within the
!     array bounds given by N.
!
! $ Examples
!
!     Suppose that you have a long list of words, sorted alphabetically
!     and entirely in upper case. Furthermore suppose you wished to
!     find all words that begin the sequence of letters PLA,  then
!     you could execute the following code.
!
!           START = 0
!           I     = 1
!
!           DO I = 1, NWORDS
!
!              IF ( WORD(I)(1:3) .EQ. 'PLA' ) THEN
!
!                 IF ( START .EQ. 0 ) THEN
!                    START = I
!                 END IF
!
!                 END = I
!              END IF
!
!           END DO
!
!     This can of course be improved by stopping the loop once START
!     is non-zero and END remains unchanged after a pass through the
!     loop. However, this is a linear search  and on average can be
!     expected to take NWORDS/2 comparisons. The above algorithm
!     fails to take advantage of the structure of the list of words
!     (they are sorted).
!
!     The code below is much simpler to code, simpler to check, and
!     much faster than the code above.
!
!           START = LSTLEC( 'PL ', NWORDS, WORDS ) + 1
!           END   = LSTLEC( 'PLA', NWORDS, WORDS )
!
!           do something in case there are no such words.
!
!           IF ( START .GT. END ) THEN
!              START = 0
!              END   = 0
!           END IF
!
!     This code will never exceed 2 * LOG_2 ( NWORDS ) comparisons.
!     For a large list of words (say 4096) the second method will
!     take 24 comparisons  the first method requires on average
!     2048 comparisons. About 200 times as much time. Its clear
!     that if searches such as this must be performed often, that
!     the second approach could make the difference between being
!     able to perform the task in a few minutes as opposed to
!     several hours.
!
!     For more ideas regarding the use of this routine see LSTLEI
!     and LSTLTI.
!
! $ Restrictions
!
!     1)  If the sequence of character strings in the input array ARRAY
!         is not non-decreasing, the program will run to completion but
!         the index found will not mean anything.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section. Improved $Detailed_Input,
!        $Detailed_Output, $Particulars, $Exceptions and $Restrictions
!        sections.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN)
!
!-&


! $ Index_Entries
!
!     last character element less_than_or_equal_to
!
!-&


!
!     Local variables
!
      INTEGER          J

      INTEGER          BEGIN
      INTEGER          END
      INTEGER          MIDDLE
      INTEGER          ITEMS



      ITEMS = N

      BEGIN = 1
      END   = N



      IF      ( N .LE. 0            ) THEN

!
!        There's nobody home---that is there is nothing in the array
!        to compare against.  Zero is the only sensible thing to return.
!
         LSTLEC = 0

      ELSE IF ( LLT (STRING, ARRAY(BEGIN)) ) THEN

!
!        None of the array elements are less than or equal to STRING
!
         LSTLEC = 0

      ELSE IF ( LGE (STRING, ARRAY(END))   ) THEN

!
!        STRING is greater than or equal to all elements of the array.
!        Thus the last element of the array is the last item less than
!        or equal to STRING.
!
         LSTLEC = END

      ELSE

!
!        STRING lies between some pair of elements of the array
!

         DO WHILE ( ITEMS .GT. 2 )

            J        = ITEMS/2
            MIDDLE   = BEGIN + J

            IF ( LLE (ARRAY(MIDDLE), STRING) ) THEN
               BEGIN = MIDDLE
            ELSE
               END   = MIDDLE
            END IF

            ITEMS    = 1 + (END - BEGIN)
         END DO

         LSTLEC = BEGIN

      END IF

      RETURN
      END
