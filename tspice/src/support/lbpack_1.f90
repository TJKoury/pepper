
!$Procedure LBPACK ( Line buffer, pack )

      SUBROUTINE LBPACK_1 ( PTRS, BUFFER )
      IMPLICIT NONE

! $ Abstract
!
!     Pack the contents of a line buffer.
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

      INTEGER               PTRS        ( LBCELL:* )
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     PTRS,
!     BUFFER    I,O  Line buffer.
!
! $ Detailed_Input
!
!     PTRS,
!     BUFFER      are the pointer and character components of a line
!                 buffer.
!
! $ Detailed_Output
!
!     PTRS,
!     BUFFER      are the pointer and character components of the
!                 same line buffer after packing.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Particulars
!
!     As lines are added to and removed from a line buffer, the
!     buffer becomes fragmented, with free space spread throughout.
!     Occasionally, the LB routines will pull all the current lines
!     toward the front of the buffer, accumulating all the free
!     space in one contiguous chunk.
!
!     LBPACK is provided mainly for internal use by the LB routines,
!     but you may pack a line buffer any time you want. Packing a
!     buffer will typically speed up operations that change the contents
!     of a buffer, but will have no effect on retrieval operations.
!
! $ Examples
!
!     LBPACK is used by LBINS.
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
      INTEGER               SIZECB_1

!
!     Local variables
!
      INTEGER               MAXLN
      INTEGER               NLINE
      INTEGER               NCOM
      INTEGER               PCARD

      INTEGER               OFFSET
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               INTLEN
      INTEGER               I
      INTEGER               J



!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LBPACK_1' )
      END IF

!
!     Recover the essential control information.
!
      CALL LBDES_1 ( PTRS, MAXLN, NLINE, NCOM, PCARD )

!
!     For each interval in the complement...
!
      OFFSET = NLINE * 2

      DO I = OFFSET+1, PCARD, 2

!
!        Remove the contents of the interval from the CB, pulling
!        the remaining contents forward.
!
         BEGIN  = PTRS(I  )
         END    = PTRS(I+1)
         INTLEN = END - BEGIN + 1

         IF ( BEGIN .LE. END ) THEN
            CALL CBREM_1 ( BEGIN, END, BUFFER )

!
!           Adjust the pointers for both the lines and the complement
!           intervals that followed the purged interval.
!
            DO J = 1, PCARD
               IF ( PTRS(J) .GT. END ) THEN
                  PTRS(J) = PTRS(J) - INTLEN
               END IF
            END DO
         END IF

      END DO

!
!     There is only one interval in the complement now. It begins
!     just after the last line, and runs to the end of the buffer.
!
      CALL MAXAI ( PTRS(1), OFFSET, END, J )

      PTRS(OFFSET+1) = END + 1
      PTRS(OFFSET+2) = SIZECB_1 ( BUFFER )

      CALL LBUPD_1 ( NLINE, 1, PTRS )

      CALL CHKOUT ( 'LBPACK_1' )
      RETURN
      END
