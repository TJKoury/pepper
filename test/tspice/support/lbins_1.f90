
!$Procedure LBINS ( Line buffer, insert )

      SUBROUTINE LBINS_1 ( POS, LINE, PTRS, BUFFER )
      IMPLICIT NONE

! $ Abstract
!
!     Insert a line into a line buffer.
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
      CHARACTER*(*)         LINE
      INTEGER               PTRS      ( LBCELL:* )
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     POS        I   Position of new line.
!     LINE       I   Line to be inserted.
!     PTRS,
!     BUFFER    I,O  Line buffer.
!
! $ Detailed_Input
!
!     POS         is the position (line number) at which the new line is
!                 to be inserted.
!
!     LINE        is the line to be inserted.
!
!     PTRS,
!     BUFFER      are the pointer and character components of a line
!                 buffer.
!
! $ Detailed_Output
!
!     PTRS,
!     BUFFER      are the pointer and character components of the
!                 same line buffer, after the new line has been
!                 inserted.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If POS exceeds by exactly one the number of lines currently
!        stored in the buffer, then LINE is appended to the end of
!        the buffer, and no other lines are affected.
!
!     2) If POS is less than one, or if POS exceeds by more than one
!        the number of lines currently stored in the buffer, the error
!        'SPICE(LBNOSUCHLINE)' is signalled.
!
!     3) If the maximum number of lines is currently stored, the
!        error 'SPICE(LBTOOMANYLINES)' is signalled.
!
!     4) If the line buffer contains insufficient free space to store
!        the new line, the error 'SPICE(LBLINETOOLONG)' is signalled.
!
! $ Particulars
!
!     New lines may be inserted at any position within a line buffer.
!     The line currently at the specified position and all subsequent
!     lines are moved back to make room for the new line.
!
! $ Examples
!
!     Let the line buffer (P,B) contain the following lines
!
!       If neither foes nor loving friends can hurt you,
!       If all men count with you, but none too much:
!
!     Following the calls
!
!       CALL LBINS ( 1,
!      .            'If you can talk with crowds and keep your virtue,',
!      .             P, B )
!
!       CALL LBINS ( 2,
!      .            'Or walk with Kings---nor lose the common touch,',
!      .             P, B )
!
!     it contains the lines
!
!       If you can talk with crowds and keep your virtue,
!       Or walk with Kings---nor lose the common touch,
!       If neither foes nor loving friends can hurt you,
!       If all men count with you, but none too much:
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
      INTEGER               LASTNB
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               MAXLN
      INTEGER               NLINE
      INTEGER               NCOM
      INTEGER               PCARD

      INTEGER               POSPTR
      INTEGER               F
      INTEGER               L
      INTEGER               LNLEN
      INTEGER               AVAIL
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               PTR         ( 2 )

!
!     Equivalences
!
      EQUIVALENCE         ( BEGIN, PTR(1) )
      EQUIVALENCE         ( END,   PTR(2) )

!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LBINS_1' )
      END IF

!
!     Recover all the essential control information.
!
      CALL LBDES_1 ( PTRS, MAXLN, NLINE, NCOM, PCARD )

!
!     Where should this line be inserted, if at all?
!
      IF ( NLINE .EQ. MAXLN ) THEN
         CALL SETMSG ( 'Current line limit is #.' )
         CALL ERRINT ( '#', MAXLN                 )
         CALL SIGERR ( 'SPICE(LBTOOMANYLINES)'    )

      ELSE IF ( POS .LT. 1  .OR.  POS - NLINE .GT. 1 ) THEN
         CALL SETMSG ( 'Tried to access line # of #.' )
         CALL ERRINT ( '#', POS                       )
         CALL ERRINT ( '#', NLINE                     )
         CALL SIGERR ( 'SPICE(LBNOSUCHLINE)'          )

      ELSE
         POSPTR = 2 * POS - 1

!
!        Leading blanks are significant; trailing blanks are history.
!        (Store a blank string as a single blank character.)
!
         F      =       1
         L      = MAX ( 1, LASTNB ( LINE ) )
         LNLEN  = L - F + 1

!
!        Store each new string at the end of the end of the CB.
!        If the final interval in the complement isn't large enough
!        to hold the new string, pack the CB and try again.
!
         AVAIL = PTRS(PCARD) - PTRS(PCARD-1) + 1

         IF ( AVAIL .LT. LNLEN ) THEN
            CALL LBPACK_1 ( PTRS, BUFFER )

            CALL LBDES_1 ( PTRS, MAXLN, NLINE, NCOM, PCARD )
            AVAIL  = PTRS(PCARD) - PTRS(PCARD-1) + 1
         END IF

!
!        If there still isn't enough room? Well, those are the breaks.
!
         IF ( AVAIL .LT. LNLEN ) THEN
            CALL SIGERR ( 'SPICE(LBLINETOOLONG)' )

!
!        If there is room, allocate just enough of the final interval
!        in the complement to contain the new string; store the string;
!        and insert the name and pointers at their proper locations.
!
         ELSE
            BEGIN         = PTRS(PCARD-1)
            END           = BEGIN + LNLEN - 1
            PTRS(PCARD-1) = END   + 1

            CALL CBPUT_1  ( BEGIN, END, LINE(F:L), BUFFER )

            CALL INSLAI ( PTR, 2, POSPTR, PTRS(1), PCARD )

            CALL LBUPD_1  ( NLINE+1, NCOM, PTRS )
         END IF

      END IF

      CALL CHKOUT ( 'LBINS_1' )
      RETURN
      END
