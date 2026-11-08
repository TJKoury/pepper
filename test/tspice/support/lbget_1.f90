
!$Procedure LBGET ( Line buffer, get )

      SUBROUTINE LBGET_1 ( POS, PTRS, BUFFER, LINE, FOUND )
      IMPLICIT NONE

! $ Abstract
!
!     Get (return) the line at a particular position within a
!     line buffer.
!
! $ Required_Reading
!
!     CB, LB
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     STRING
!     TEXT
!
! $ Declarations

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF =  0 )

      INTEGER               POS
      INTEGER               PTRS        ( LBCELL:* )
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )
      CHARACTER*(*)         LINE
      LOGICAL               FOUND

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     POS        I   Position of line.
!     PTRS,
!     BUFFER     I   Line buffer.
!     LINE       O   Line.
!     FOUND      O   True if the line was found.
!
! $ Detailed_Input
!
!     POS         is the position of an existing line within a line
!                 buffer.
!
!     PTRS,
!     BUFFER      are the pointer and character components of a line
!                 buffer.
!
! $ Detailed_Output
!
!     LINE        is a copy of the specified line. If LINE is shorter
!                 than the stored line, it is truncated. If longer, it
!                 is padded with spaces.
!
!     FOUND       is true whenever the specified line exists, and is
!                 false otherwise.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If FOUND is false, LINE is not changed.
!
! $ Particulars
!
!     LBGET is the only way to retrieve lines from a line buffer.
!
! $ Examples
!
!     Let the line buffer (P,B) contain the following lines.
!
!       If you can keep your head when all about you
!       Are losing theirs and blaming it on you;
!       If you can trust yourself when all men doubt you,
!       But make allowance for their doubting too:
!       If you can wait and not be tired by waiting,
!       Or, being lied about, don't deal in lies,
!       Or being hated don't give way to hating,
!       And yet don't look too good, nor talk too wise;
!
!     The code fragment
!
!       N = 1
!       CALL LBGET ( N, P, B, LINE, FOUND )
!
!       DO WHILE ( FOUND )
!          WRITE (*,*) '(', N, ') ', LINE
!
!          N = N + 1
!          CALL LBGET ( N, P, B, LINE, FOUND )
!       END DO
!
!     produces the following output:
!
!       (  1) If you can keep your head when all about you
!       (  2) Are losing theirs and blaming it on you;
!       (  3) If you can trust yourself when all men doubt you,
!       (  4) But make allowance for their doubting too:
!       (  5) If you can wait and not be tired by waiting,
!       (  6) Or, being lied about, don't deal in lies,
!       (  7) Or being hated don't give way to hating,
!       (  8) And yet don't look too good, nor talk too wise;
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
!     Dagny Taggart, (JPL)
!
! $ Version
!
!-    Beta Version 1.0.0, 19-JAN-1989 (DT)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               MAXLN
      INTEGER               NLINE
      INTEGER               NCOM
      INTEGER               PCARD
      INTEGER               POSPTR

!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LBGET_1' )
      END IF

!
!     Recover all the essential control information.
!
      CALL LBDES_1 ( PTRS, MAXLN, NLINE, NCOM, PCARD )

!
!     What are the endpoints of the stored line? Once we have
!     them, we can return the line directly.
!
      FOUND = ( POS .GE. 1  .AND.  POS .LE. NLINE )

      IF ( FOUND ) THEN
         POSPTR = 2 * POS - 1

         CALL CBGET_1 ( PTRS(POSPTR), PTRS(POSPTR+1), BUFFER, LINE )
      END IF

      CALL CHKOUT ( 'LBGET_1' )
      RETURN
      END
