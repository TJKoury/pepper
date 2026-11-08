!$Procedure UNIONC ( Union two character sets )

      SUBROUTINE UNIONC ( A, B, C )

! $ Abstract
!
!     Compute the union of two character sets to form a third set.
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

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      CHARACTER*(*)      A      ( LBCELL:* )
      CHARACTER*(*)      B      ( LBCELL:* )
      CHARACTER*(*)      C      ( LBCELL:* )

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
!     2)  If length of the elements of the output set is < the
!         maximum of the lengths of the elements of the input
!         sets, the error SPICE(ELEMENTSTOOSHORT) is signaled.
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
!-    SPICELIB Version 1.2.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Made CHKOUT calls consistent with CHKIN.
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
!     union two character sets
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 05-JAN-1989 (NJB)
!
!        Error signaled if output set elements are not long enough.
!        Length must be at least max of lengths of input elements.
!        Also, calling protocol for EXCESS has been changed.
!
!-&


!
!     SPICELIB functions
!
      INTEGER             CARDC
      INTEGER             SIZEC
      LOGICAL             RETURN

!
!     Local variables
!

      INTEGER             APOINT
      INTEGER             BPOINT

      INTEGER             CSIZE
      INTEGER             ACARD
      INTEGER             BCARD
      INTEGER             CCARD

      INTEGER             OVER




!
!     Set up the error processing.
!
      IF ( RETURN () ) RETURN
      CALL CHKIN ( 'UNIONC' )

!
!     Make sure output set elements are long enough.
!
      IF (               LEN( C(LBCELL) ) &
            .LT.  MAX (  LEN( A(LBCELL) ), LEN( B(LBCELL) )  )   ) THEN

         CALL SETMSG ( 'Length of output cell is #.  Length required' // &
                       ' to contain result is #.'  )

         CALL ERRINT ( '#', LEN( C(LBCELL) )  )
         CALL ERRINT ( '#', MAX (  LEN(A(LBCELL)), LEN(B(LBCELL)) )  )

         CALL SIGERR ( 'SPICE(ELEMENTSTOOSHORT)' )

         CALL CHKOUT ( 'UNIONC' )
         RETURN

      END IF

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      ACARD = CARDC ( A )
      BCARD = CARDC ( B )
      CSIZE = SIZEC ( C )

!
!     Begin with the input pointers at the first elements of the
!     input sets. The cardinality of the output set is zero.
!     And there is no overflow so far.
!
      APOINT = 1
      BPOINT = 1

      CCARD  = 0
      OVER   = 0

!
!     When the ends of both input sets are reached, we're done.
!
      DO WHILE ( APOINT .LE. ACARD .OR. BPOINT .LE. BCARD )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         IF ( CCARD .LT. CSIZE ) THEN

            IF ( APOINT .GT. ACARD ) THEN

               CCARD    = CCARD + 1
               C(CCARD) = B(BPOINT)
               BPOINT   = BPOINT + 1

            ELSE IF ( BPOINT .GT. BCARD ) THEN

               CCARD    = CCARD + 1
               C(CCARD) = A(APOINT)
               APOINT   = APOINT + 1

            ELSE IF ( A(APOINT) .EQ. B(BPOINT) ) THEN

               CCARD    = CCARD + 1
               C(CCARD) = A(APOINT)
               APOINT   = APOINT + 1
               BPOINT   = BPOINT + 1

            ELSE IF ( LLT ( A(APOINT), B(BPOINT) ) ) THEN

               CCARD    = CCARD + 1
               C(CCARD) = A(APOINT)
               APOINT   = APOINT + 1

            ELSE IF ( LGT ( A(APOINT), B(BPOINT) ) ) THEN

               CCARD    = CCARD + 1
               C(CCARD) = B(BPOINT)
               BPOINT   = BPOINT + 1

            END IF


!
!        Otherwise, stop filling the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         ELSE

            IF ( APOINT .GT. ACARD ) THEN

               OVER   = OVER   + 1
               BPOINT = BPOINT + 1

            ELSE IF ( BPOINT .GT. BCARD ) THEN

               OVER   = OVER   + 1
               APOINT = APOINT + 1

            ELSE IF ( A(APOINT) .EQ. B(BPOINT) ) THEN

               OVER   = OVER   + 1
               APOINT = APOINT + 1
               BPOINT = BPOINT + 1

            ELSE IF ( LLT ( A(APOINT), B(BPOINT) ) ) THEN

               OVER   = OVER   + 1
               APOINT = APOINT + 1

            ELSE IF ( LGT ( A(APOINT), B(BPOINT) ) ) THEN

               OVER   = OVER   + 1
               BPOINT = BPOINT + 1

            END IF

         END IF

      END DO

!
!     Set the cardinality of the output set.
!
      CALL SCARDC ( CCARD, C )

!
!     Report any excess.
!
      IF ( OVER .GT. 0 ) THEN
         CALL EXCESS ( OVER, 'set' )
         CALL SIGERR ( 'SPICE(SETEXCESS)' )
      END IF

      CALL CHKOUT ( 'UNIONC' )

      RETURN
      END
