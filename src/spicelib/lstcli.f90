!$Procedure LSTCLI ( Closest integer array element )

      INTEGER FUNCTION  LSTCLI ( X, N, ARRAY )

! $ Abstract
!
!     Find the index of the array element closest to a given integer X
!     in an array of non-decreasing integers.
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

      INTEGER               X
      INTEGER               N
      INTEGER               ARRAY ( * )

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
!     ARRAY    is an array of integers such that
!
!                         ARRAY( I ) <= ARRAY( J )
!
!              for all I < J.
!
! $ Detailed_Output
!
!     LSTCLI   is the index of the element of the non-decreasing
!              sequence: {ARRAY(I) : 1 <= I <= N} that is closest to
!              X. In other words, ARRAY( LSTCLI( X, N, ARRAY ) ) is the
!              closest element of ARRAY to X.
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
!              LSTCLI = I for some I in the range 1 to N, unless N is
!              less than or equal to zero, in which case LSTCLI is zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the value of N is non-positive, LSTCLI returns the value
!         zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     LSTCLI uses a binary search algorithm to locate the value closest
!     to X in the non-decreasing sequence of integers represented by
!     the elements of ARRAY.
!
! $ Examples
!
!     Suppose ARRAY contains the following integer elements:
!
!     ARRAY: -1    0    10  15    15    20    30   39  40  10
!
!     index:  1    2    3    4     5     6     7    8   9   10
!
!     The following table shows the values of LSTCLI that would be
!     returned for various values of X, and the corresponding closest
!     array element values.
!
!            X      LSTCLI( X,10,ARRAY )   ARRAY( LSTCLI( X,10,ARRAY ))
!          -----    --------------------   ---------------------------
!           -2               1                         -1
!           -1               1                         -1
!            1               2                          0
!           14               5                         15
!           17               5                         15
!           18               6                         20
!           60               9                         40
!          110              10                        100
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
!     closest integer array element
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
         LSTCLI = 0
         RETURN

      ELSE IF ( X .LE. ARRAY( BEGIN ) ) THEN

!
!        All elements of the array are at least as big as X. So the
!        first element is the closest to X.
!
         LSTCLI = 1

      ELSE IF ( ARRAY( END ) .LE. X ) THEN

!
!        X is at least as big as all elements of the array.  So the last
!        element is the closest to X.
!
         LSTCLI = END

      ELSE

!
!        X lies between some pair of elements of the array.
!
         DO WHILE ( ITEMS .GT. 2 )

            J        = ITEMS/2
            MIDDLE   = BEGIN + J

            IF ( ARRAY( MIDDLE ) .LE. X ) THEN
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
            LSTCLI = BEGIN
         ELSE
            LSTCLI = END
         END IF

      END IF

!
!     March down the array to find the last element equal to the
!     closet value.
!
      DO WHILE (         LSTCLI .LT. N                     .AND. &
                ARRAY( LSTCLI ) .EQ. ARRAY( LSTCLI + 1 ))

         LSTCLI = LSTCLI + 1

      END DO


      RETURN
      END
