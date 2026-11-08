!$Procedure DIFFI ( Difference of two integer sets )

      SUBROUTINE DIFFI ( A, B, C )

! $ Abstract
!
!     Take the difference of two integer sets to form a third set.
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

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER            A      ( LBCELL:* )
      INTEGER            B      ( LBCELL:* )
      INTEGER            C      ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A          I   First input set.
!     B          I   Second input set.
!     C          O   Difference of A and B.
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
!              contains the difference of A and B (that is,
!              all of the elements which are in A, but NOT
!              in B).
!
!              If the size (maximum cardinality) of C is smaller
!              than the cardinality of the difference of A and B,
!              then only as many items as will fit in C are
!              included, and an error is returned.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the difference of the two sets causes an excess of
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
!     The DIFFERENCE of two sets contains every element which is
!     in the first set, but NOT in the second.
!
!           {a,b}      difference  {c,d}     =  {a,b}
!           {a,b,c}                {b,c,d}      {a}
!           {a,b,c,d}              {}           {a,b,c,d}
!           {}                     {a,b,c,d}    {}
!           {}                     {}           {}
!
!     The following call
!
!           CALL DIFFC  ( PLANETS, ASTEROIDS, RESULT )
!
!     places the difference of the character sets PLANETS and
!     ASTEROIDS into the character set RESULT.
!
!     The output set must be distinct from both of the input sets.
!     For example, the following calls are invalid.
!
!           CALL DIFFI  ( CURRENT,     NEW, CURRENT )
!           CALL DIFFI  (     NEW, CURRENT, CURRENT )
!
!     In each of the examples above, whether or not the subroutine
!     signals an error, the results will almost certainly be wrong.
!     Nearly the same effect can be achieved, however, by placing the
!     result into a temporary set, which is immediately copied back
!     into one of the input sets, as shown below.
!
!           CALL DIFFI  ( CURRENT, NEW,  TEMP )
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
!     difference of two integer sets
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 06-JAN-1989 (NJB)
!
!        Calling protocol of EXCESS changed. Call to SETMSG removed.
!
!        Also, in the overflow case, the number of excess elements was
!        computed incorrectly; this has been fixed. The problem was
!        that OVER was incremented in all cases of the overflow IF
!        block, rather than only in the cases where the cardinality of
!        the output cell would have been incremented if there were
!        enough room.
!
!-&


!
!     SPICELIB functions
!
      INTEGER             CARDI
      INTEGER             SIZEI
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
      CALL CHKIN ( 'DIFFI' )

!
!     Find the cardinality of the input sets, and the allowed size
!     of the output set.
!
      ACARD = CARDI ( A )
      BCARD = CARDI ( B )
      CSIZE = SIZEI ( C )

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
!     When the end of the first input set is reached, we're done.
!
      DO WHILE ( APOINT .LE. ACARD )

!
!        If there is still space in the output set, fill it
!        as necessary.
!
         IF ( CCARD .LT. CSIZE ) THEN

            IF ( BPOINT .GT. BCARD) THEN

               CCARD    = CCARD + 1
               C(CCARD) = A(APOINT)
               APOINT   = APOINT + 1

            ELSE IF ( A(APOINT) .EQ. B(BPOINT)) THEN

               APOINT = APOINT + 1
               BPOINT = BPOINT + 1

            ELSE IF ( A(APOINT) .LT. B(BPOINT) ) THEN

               CCARD    = CCARD + 1
               C(CCARD) = A(APOINT)
               APOINT   = APOINT + 1

            ELSE IF ( B(BPOINT) .LT. A(APOINT) ) THEN

               BPOINT = BPOINT + 1

            END IF


!
!        Otherwise, stop following the array, but continue to count the
!        number of elements in excess of the size of the output set.
!
         ELSE

            IF ( BPOINT .GT. BCARD ) THEN

               OVER   = OVER   + 1
               APOINT = APOINT + 1

            ELSE IF ( A(APOINT) .EQ. B(BPOINT) ) THEN

               APOINT = APOINT + 1
               BPOINT = BPOINT + 1

            ELSE IF ( A(APOINT) .LT. B(BPOINT) ) THEN

               OVER   = OVER   + 1
               APOINT   = APOINT + 1

            ELSE IF ( B(BPOINT) .LT. A(APOINT) ) THEN

               BPOINT = BPOINT + 1

            END IF

         END IF

      END DO

!
!     Set the cardinality of the output set.
!
      CALL SCARDI ( CCARD, C )

!
!     Report any excess.
!

      IF ( OVER .GT. 0 ) THEN
         CALL EXCESS ( OVER, 'set' )
         CALL SIGERR ( 'SPICE(SETEXCESS)' )
      END IF

      CALL CHKOUT ( 'DIFFI' )

      RETURN
      END
