
!$Procedure SBGET ( String buffer, get )

      SUBROUTINE SBGET_1 ( NAME, NAMES, &
                               PTRS, &
                               BUFFER, STR, POS )
      use pepper_legacy, pepper_legacy_pos => pos
      IMPLICIT NONE


! $ Abstract
!
!     Get (return) a string from a string buffer.
!
! $ Required_Reading
!
!     CB, LB, SB
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

      CHARACTER*(*)         NAME
      CHARACTER*(*)         NAMES       ( LBCELL:* )
      INTEGER               PTRS        ( LBCELL:* )
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )
      CHARACTER*(*)         STR
      INTEGER               POS

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the string to be returned.
!     NAMES,
!     PTRS,
!     BUFFER     I   String buffer.
!     STR        O   The string.
!     POS        O   Position of the string within the buffer.
!
! $ Detailed_Input
!
!     NAME        is the name of a string contained within a string
!                 buffer.
!
!     NAMES,
!     PTRS,
!     BUFFER      are the name, pointer, and character components
!                 of the string buffer.
!
! $ Detailed_Output
!
!     STR         is the string associated with the specified name.
!                 If STRING is shorter than the stored string, it is
!                 truncated. If longer, STRING is padded with spaces.
!
!     POS         is the position of the specified string within the
!                 string buffer, as determined by the ASCII collating
!                 sequence.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If the specified string is not in the list, POS is zero
!        and STR is not changed.
!
! $ Particulars
!
!     There are two routines that you can use to retrieve a string
!     from a string buffer:
!
!        SBGET      which takes the name of the string, and returns
!                   the string and its position within the buffer.
!
!        SBGETP     which takes the position of the string within
!                   the buffer, and returns the string and its address
!                   within the name table.
!
! $ Examples
!
!     The following code fragment stores three strings, associated
!     with the names WHO, WHAT, and WHERE.
!
!        CALL SBSET  ( 'WHO',   'Feynman',                 N, P, B )
!        CALL SBSET  ( 'WHAT',  'Quantum electrodynamics', N, P, B )
!        CALL SBSET  ( 'WHERE', 'Caltech',                 N, P, B )
!
!     The strings can be retrieved using either SBGET,
!
!        CALL SBGET  ( 'WHO',   S(1), N, P, B, POS )
!        CALL SBGET  ( 'WHAT',  S(2), N, P, B, POS )
!        CALL SBGET  ( 'WHERE', S(3), N, P, B, POS )
!
!      or SBGETP,
!
!        CALL SBGETP ( 3, S(1), N, P, B, ADDR )
!        CALL SBGETP ( 1, S(2), N, P, B, ADDR )
!        CALL SBGETP ( 2, S(3), N, P, B, ADDR )
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

!
!     Local variables
!
      LOGICAL               FOUND

!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SBGET_1' )
      END IF

!
!     Is this string even in the list?
!
      POS = BSRCHC ( NAME, CARDC ( NAMES ), NAMES(1) )

!
!     If so, get it.
!
      IF ( POS .GT. 0 ) THEN
         CALL LBGET_1 ( POS, PTRS, BUFFER, STR, FOUND )
      END IF

      CALL CHKOUT ( 'SBGET_1' )
      RETURN
      END
