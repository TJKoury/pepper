!$Procedure LSTCLD ( Last closest double precision array element )

      INTEGER FUNCTION  LSTCLD ( X, N, ARRAY )

! $ Abstract
!
!     Find the index of the array element closest to a given number X
!     in an array of non-decreasing numbers.
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

      DOUBLE PRECISION   X
      INTEGER            N
      DOUBLE PRECISION   ARRAY ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   Search value.
!     N          I   Number of elements in ARRAY.
!     ARRAY      I   Array to be searched.
!
!     The function returns the index of the element of ARRAY
!     whose value is closest to X.
!
! $ Detailed_Input
!
!     X        is the value to be compared with the elements of ARRAY.
!
!     N        is the number of elements in ARRAY.
!
!     ARRAY    is an array of double precision numbers such that
!
!                         ARRAY( I ) <= ARRAY( J )
!
!              for all I < J.
!
! $ Detailed_Output
!
!     LSTCLD   is the index of the element of the non-decreasing
!              sequence: {ARRAY(I) : 1 <= I <= N} that is closest
!              to X. In other words, ARRAY( LSTCLD( X, N, ARRAY ) )
!              is the element of ARRAY whose value is closest to X.
!
!              If X falls precisely on the midpoint of consecutive array
!              elements, the index of the larger of the two values is
!              returned.
!
!              If X is closest to a value which appears more than
!              once in the array (since the array is ordered, these
!              elements would have to be consecutive), the highest index
!              for that value will be returned.
!
!              LSTCLD = I for some I in the range 1 to N, unless N is
!              less than or equal to zero, in which case LSTCLD is zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the value of N is non-positive, LSTCLD returns the value
!         zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     LSTCLD uses a binary search algorithm to locate the value closest
!     to X in the non-decreasing sequence of double precision numbers
!     represented by the elements of ARRAY.
!
! $ Examples
!
!     Suppose ARRAY contains the following double precision elements:
!
!     ARRAY: -1    0    1    1.5   1.5    2    3    9    9.5   100
!
!     index:  1    2    3     4     5     6    7    8     9     10
!
!     The following table shows the values of LSTCLD that would be
!     returned for various values of X, and the corresponding closest
!     array element values.
!
!            X      LSTCLD( X,10,ARRAY )   ARRAY( LSTCLD( X,10,ARRAY ))
!          -----    --------------------   ---------------------------
!           0.12             2                          0
!          -0.12             2                          0
!          -2.0              1                         -1
!           2.5              7                          3
!           1.3              5                        1.5
!         100.0             10                        100
!         100.1             10                        100
!
! $ Restrictions
!
!     1)  If the sequence is not non-decreasing, the routine will run
!         to completion but the index found will not mean anything.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     R.E. Thurman       (JPL)
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
!-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET)
!
!-&


! $ Index_Entries
!
!     last closest d.p. array element
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 30-AUG-1990 (MJS)
!
!        The following changes were made as a result of the
!        NAIF CK Code and Documentation Review:
!
!        1) The name of this routine was changed from CLOSTD to
!           LSTCLD because it was a more descriptive name.
!        2) All references (comments and code) were changed to reflect
!           the name change.
!
!-    Beta Version 1.0.0, 15-MAY-1990 (RET)
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

!
!     Save the size of the array and point to the beginning and ending
!     positions. The pointers delimit the current search interval.
!
      ITEMS = N
      BEGIN = 1
      END   = N

      IF      ( N .LE. 0 ) THEN

!
!        There is nothing in the array to compare against. Zero is the
!        only sensible thing to return.
!
         LSTCLD = 0
         RETURN

      ELSE IF ( X .LE. ARRAY( BEGIN ) ) THEN

!
!        All elements of the array are at least as big as X. So the
!        first element is the closest to X.
!
         LSTCLD = 1

      ELSE IF ( ARRAY( END ) .LE. X ) THEN

!
!        X is at least as big as all elements of the array.  So the last
!        element is the closest to X.
!
         LSTCLD = END

      ELSE

!
!        X lies between some pair of elements of the array.
!
         DO WHILE ( ITEMS .GT. 2 )

            J        = ITEMS/2
            MIDDLE   = BEGIN + J

            IF ( ARRAY( MIDDLE ) .LT. X ) THEN
               BEGIN  = MIDDLE
            ELSE
               END    = MIDDLE
            END IF

            ITEMS     = 1 + ( END - BEGIN )

         END DO

!
!        Which of the two is closest?
!
         IF ( X - ARRAY( BEGIN ) .LT. ARRAY( END ) - X ) THEN
            LSTCLD = BEGIN
         ELSE
            LSTCLD = END
         END IF

      END IF

!
!     March down the array to find the last element equal to the
!     closet value.
!
      DO WHILE (       LSTCLD   .LT. N                   .AND. &
                ARRAY( LSTCLD ) .EQ. ARRAY( LSTCLD + 1 ))

         LSTCLD = LSTCLD + 1

      END DO

      RETURN
      END
