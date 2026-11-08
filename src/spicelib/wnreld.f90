!$Procedure WNRELD ( Compare two DP windows )

      LOGICAL FUNCTION  WNRELD ( A, OP, B )

! $ Abstract
!
!     Compare two double precision windows.
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
!     WINDOWS
!
! $ Keywords
!
!     WINDOWS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      DOUBLE PRECISION      A        ( LBCELL:* )
      CHARACTER*(*)         OP
      DOUBLE PRECISION      B        ( LBCELL:* )

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
      LOGICAL               RETURN
      INTEGER               CARDD
      LOGICAL               WNINCD

!
!     Local variables
!
      INTEGER               ACARD
      INTEGER               BCARD
      LOGICAL               EQUAL
      LOGICAL               SUBSET
      INTEGER               I



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         WNRELD = .FALSE.
         RETURN
      ELSE
         CALL CHKIN ( 'WNRELD' )
         WNRELD = .FALSE.
      END IF

!
!     Find the cardinality of the input windows.
!
      ACARD  = CARDD ( A )
      BCARD  = CARDD ( B )

!
!     A and B are equal if they contain exactly the same intervals.
!     We need to know this for nearly every relationship, so find out
!     before going any further.
!
         IF ( ACARD .NE. BCARD ) THEN
            EQUAL = .FALSE.

         ELSE
            EQUAL = .TRUE.

            DO I = 1, ACARD
               EQUAL = EQUAL .AND. ( A(I) .EQ. B(I) )
            END DO
         END IF

!
!     Simple equality and inequality are trivial at this point.
!
      IF ( OP .EQ. '=' ) THEN

         WNRELD = EQUAL

      ELSE IF ( OP .EQ. '<>' ) THEN

         WNRELD = .NOT. EQUAL

!
!     Subsets are a little trickier. A is a subset of B if every
!     interval in A is included in B. In addition, A is a proper
!     subset if A and B are not equal.
!
      ELSE IF ( OP .EQ. '<='  .OR.  OP .EQ. '<' ) THEN

         SUBSET = .TRUE.

         DO I = 1, ACARD, 2
            SUBSET = SUBSET .AND. ( WNINCD ( A(I), A(I+1), B ) )
         END DO

         IF ( OP .EQ. '<=' ) THEN
            WNRELD = SUBSET

         ELSE
            WNRELD = SUBSET .AND. ( .NOT. EQUAL )
         END IF

!
!     A and B change places here...
!
      ELSE IF ( OP .EQ. '>='  .OR.  OP .EQ. '>' ) THEN

         SUBSET = .TRUE.

         DO I = 1, BCARD, 2
            SUBSET = SUBSET .AND. ( WNINCD ( B(I), B(I+1), A ) )
         END DO

         IF ( OP .EQ. '>=' ) THEN
            WNRELD = SUBSET

         ELSE
            WNRELD = SUBSET .AND. ( .NOT. EQUAL )
         END IF

!
!     An unrecognized operator always fails.
!
      ELSE

         CALL SETMSG ( 'Relational operator, *, is not '         // &
                       'recognized.'                              )
         CALL ERRCH  ( '*', OP                   )
         CALL SIGERR ( 'SPICE(INVALIDOPERATION)' )
         CALL CHKOUT ( 'WNRELD' )
         RETURN

      END IF


      CALL CHKOUT ( 'WNRELD' )
      RETURN
      END
