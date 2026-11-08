!$Procedure SDIFFC ( Symmetric difference of two character sets )

      SUBROUTINE SDIFFC ( A, B, C )

! $ Abstract
!
!     Take the symmetric difference of two character sets to form
!     a third set.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
!     C          O   Symmetric difference of A and B.
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
!              contains the symmetric difference of A and B
!              (that is, all of the elements which are in A
!              OR in B, but NOT in both).
!
!              If the size (maximum cardinality) of C is smaller
!              than the cardinality of the symmetric difference of
!              A and B, then only as many items as will fit in C
!              are included, and an error is signaled.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the symmetric difference of the two sets causes an excess
!         of elements, the error SPICE(SETEXCESS) is signaled.
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
!     The SYMMETRIC DIFFERENCE of two sets contains every
!     element which is in the first set OR in the second set,
!     but NOT in both sets.
!
!           {a,b}      sym. difference {c,d}     =  {a,b,c,d}
!           {a,b,c}                    {b,c,d}      {a,d}
!           {a,b,c,d}                  {}           {a,b,c,d}
!           {}                         {a,b,c,d}    {a,b,c,d}
!           {}                         {}           {}
!
!     The following call
!
!           CALL SDIFFC  ( PLANETS, ASTEROIDS, RESULT )
!
!     places the symmetric difference of the character sets PLANETS and
!     ASTEROIDS into the character set RESULT.
!
!     The output set must be distinct from both of the input sets.
!     For example, the following calls are invalid.
!
!           CALL SDIFFI ( CURRENT,     NEW, CURRENT )
!           CALL SDIFFI (     NEW, CURRENT, CURRENT )
!
!     In each of the examples above, whether or not the subroutine
!     signals an error, the results will almost certainly be wrong.
!     Nearly the same effect can be achieved, however, by placing the
!     result into a temporary set, which is immediately copied back
!     into one of the input sets, as shown below.
!
!           CALL SDIFFI ( CURRENT, NEW,  TEMP )
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
!-    SPICELIB Version 1.2.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Modified call to CHKOUT to be consistent with CHKIN.
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
!     symmetric difference of two character sets
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
      INTEGER              CARDC
      INTEGER              SIZEC
      LOGICAL              RETURN
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
      CALL CHKIN ( 'SDIFFC' )

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

         CALL CHKOUT ( 'SDIFFC' )
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
!     When the end of both input sets are reached, we're done.
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

               APOINT = APOINT + 1
               BPOINT = BPOINT + 1

            ELSE IF ( LLT ( A(APOINT), B(BPOINT) ) ) THEN

               CCARD    = CCARD + 1
               C(CCARD) = A(APOINT)
               APOINT   = APOINT + 1

            ELSE IF ( LLT ( B(BPOINT), A(APOINT) ) ) THEN

               CCARD    = CCARD + 1
               C(CCARD) = B(BPOINT)
               BPOINT   = BPOINT + 1

            END IF



!
!        Otherwise, stop following the array, but continue to count the
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

               APOINT = APOINT + 1
               BPOINT = BPOINT + 1

            ELSE IF ( LLT ( A(APOINT), B(BPOINT) ) ) THEN

               OVER   = OVER   + 1
               APOINT = APOINT + 1

            ELSE IF ( LLT ( B(BPOINT), A(APOINT) ) ) THEN

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

      CALL CHKOUT ( 'SDIFFC' )

      RETURN
      END
