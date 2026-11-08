!$Procedure WNUNID ( Union two DP windows )

      SUBROUTINE WNUNID ( A, B, C )

! $ Abstract
!
!     Place the union of two double precision windows into a third
!     window.
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
      DOUBLE PRECISION      B        ( LBCELL:* )
      DOUBLE PRECISION      C        ( LBCELL:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     A,
!     B          I   Input windows.
!     C          O   Union of A and B.
!
! $ Detailed_Input
!
!     A,
!     B        are SPICE windows, each of which contains zero or more
!              intervals.
!
! $ Detailed_Output
!
!     C        is the output SPICE window, containing the union of A
!              and B --- every point contained in A, or in B, or in
!              both.
!
!              C must be distinct from both A and B.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the union of the two windows results in an excess of
!         elements, the error SPICE(WINDOWEXCESS) is signaled.
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
!     The union of two windows contains every point contained in the
!     first window, or the second window, or both.
!
! $ Examples
!
!     Let A contain the intervals
!
!           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
!
!     and B contain the intervals
!
!           [ 2, 6 ]  [ 8, 10 ]  [ 16, 18 ]
!
!     Then the union of A and B contains the intervals
!
!           [ 1, 6 ]  [ 7, 11 ]  [ 16, 18 ]  [ 23, 27 ]
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
!        Edited the header to comply with NAIF standard. Fixed I/O type
!        of argument C in $Brief_I/O table.
!
!        Added entry #2 in $Exceptions section. Removed unnecessary
!        entries in $Revisions section.
!
!-    SPICELIB Version 1.1.0, 08-FEB-1999 (WLT)
!
!        The variable END was not initialized in the previous
!        edition. It is now initialized to be the minimum of
!        A(1) and B(1).
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
!     union two d.p. windows
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 27-FEB-1989  (HAN)
!
!        Due to the calling sequence and functionality changes
!        in the routine EXCESS, the method of signaling an
!        excess of elements needed to be changed.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDD
      INTEGER               SIZED
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               ACARD
      INTEGER               BCARD
      INTEGER               CSIZE

      INTEGER               AP
      INTEGER               BP
      INTEGER               CP
      DOUBLE PRECISION      END
      CHARACTER*1           USE
      INTEGER               OVER




!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'WNUNID' )
      END IF


!
!     Find the cardinality of the input windows, and the allowed size
!     of the output window.
!
      ACARD = CARDD ( A )
      BCARD = CARDD ( B )
      CSIZE = SIZED ( C )

!
!     Begin with the input pointers at the first elements of the
!     input windows. The initial cardinality of the output window
!     is zero. And there is no overflow so far.
!
!     (Note that AP and BP point to the LEFT endpoints of intervals
!     in A and B, while CP points to the RIGHT endpoint of the latest
!     interval in C.)
!
      AP   = 1
      BP   = 1
      CP   = 0
      END  = MIN( A(1), B(1) )
      OVER = 0

!
!     When the ends of both input windows are reached, we're done.
!
      DO WHILE ( AP .LT. ACARD  .OR.  BP .LT. BCARD )

!
!        If the end of one window has been reached, copy (or merge)
!        the next interval from the other window.
!
         IF ( AP .GT. ACARD ) THEN
            USE = 'B'

         ELSE IF ( BP .GT. BCARD ) THEN
            USE = 'A'

!
!        Otherwise, let's see what we can do with the earlier of
!        the next intervals from A and B.
!
         ELSE IF ( A(AP) .LT. B(BP) ) THEN
            USE = 'A'

         ELSE IF ( B(BP) .LE. A(AP) ) THEN
            USE = 'B'
         END IF

!
!        If there is still space in the output window, fill it
!        as necessary. Otherwise, stop filling the array, but continue
!        to count the number of elements in excess of the size of the
!        output window.
!
!        The general idea is this: if the next interval overlaps the
!        latest output interval, merge the two (extending the output
!        interval to the right). Otherwise, insert the next interval
!        intact.
!
         IF ( USE .EQ. 'A' ) THEN

            IF ( CP .LT. CSIZE ) THEN

               IF ( A(AP) .LE. END .AND. CP .GT. 0 ) THEN
                  C(CP) = MAX ( C(CP), A(AP+1) )
               ELSE
                  CP      = CP + 2
                  C(CP-1) = A(AP  )
                  C(CP  ) = A(AP+1)
               END IF

               END = C(CP)

            ELSE

               IF ( A(AP) .LE. END ) THEN
                  END  = MAX ( END, A(AP+1) )
               ELSE
                  OVER = OVER + 2
                  END  = A(AP+1)
               END IF

            END IF

            AP = AP + 2

!
!        This is the same as the last clause, with B replacing A.
!
         ELSE IF ( USE .EQ. 'B' ) THEN

            IF ( CP .LT. CSIZE ) THEN

               IF ( B(BP) .LE. END .AND. CP .GT. 0 ) THEN
                  C(CP)   = MAX ( C(CP), B(BP+1) )
               ELSE
                  CP      = CP + 2
                  C(CP-1) = B(BP  )
                  C(CP  ) = B(BP+1)
               END IF

               END = C(CP)

            ELSE

               IF ( B(BP) .LE. END ) THEN
                  END  = MAX ( END, B(BP+1) )
               ELSE
                  OVER = OVER + 2
                  END  = B(BP+1)
               END IF
            END IF

            BP = BP + 2

         END IF

      END DO

!
!     Set the cardinality of the output window.
!
      CALL SCARDD ( CP, C )

!
!     If there is an excess of elements, signal an error and check out
!     as usual.
!
      IF ( OVER .GT. 0 ) THEN
         CALL EXCESS ( OVER, 'window' )
         CALL SIGERR ( 'SPICE(WINDOWEXCESS)' )
      END IF


      CALL CHKOUT ( 'WNUNID' )

      RETURN
      END
