
!$Procedure SBREM ( String buffer, remove )

      SUBROUTINE SBREM_1 ( NAME, NAMES, &
                               PTRS, &
                               BUFFER  )
      IMPLICIT NONE


! $ Abstract
!
!     Remove a string from a string buffer.
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

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the string to be removed.
!     NAMES,
!     PTRS,
!     BUFFER    I,O  String buffer.
!
! $ Detailed_Input
!
!     NAME        is the name of a string currently stored within a
!                 string buffer.
!
!     NAMES,
!     PTRS,
!     BUFFER      are the name, pointer, and character components
!                 of a string buffer.
!
! $ Detailed_Output
!
!     NAMES,
!     PTRS,
!     BUFFER      are the name, pointer, and character components
!                 of the same string buffer, from which the specified
!                 string has been removed.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If NAME is longer than the maximum length of the names in
!        the buffer, it is truncated. Thus, long names may conflict,
!        removing each other's associated strings.
!
!     2) If a string with the specified name is not already contained
!        in the string buffer, nothing happens.
!
! $ Particulars
!
!     SBREM is the only way to get a string out of a string buffer.
!
! $ Examples
!
!     The code fragment
!
!        CALL SBSET ( 'EINSTEIN', 'Brownian motion',  N, P, B      )
!        CALL SBSET ( 'BOHR',     'Atomic structure', N, P, B      )
!        CALL SBGET ( 'EINSTEIN',                     N, P, B, POS )
!
!        WRITE (*,*) 'Found at position ', POS
!
!        CALL SBREM ( 'EINSTEIN', N, P, B      )
!        CALL SBGET ( 'EINSTEIN', N, P, B, POS )
!
!        WRITE (*,*) 'Found at position ', POS
!
!     Produces the following output.
!
!        Found at position 2
!        Found at position 0
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
      INTEGER               BSRCHC
      INTEGER               CARDC
      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               NSTR
      INTEGER               POS

!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SBREM_1' )
      END IF

!
!     Recover the essential control information.
!
      NSTR = CARDC ( NAMES )

!
!     Which string is to be removed?
!
      POS = BSRCHC ( NAME, NSTR, NAMES(1) )

!
!     If the string is not in the buffer, do nothing.
!
      IF ( POS .GT. 0 ) THEN

!
!        Remove the name from the name list, and the string from the
!        line buffer.
!
         CALL REMLAC ( 1,    POS, NAMES(1), NSTR  )
         CALL SCARDC ( NSTR,      NAMES           )

         CALL LBREM_1  ( POS, PTRS, BUFFER )

      END IF

      CALL CHKOUT ( 'SBREM_1' )
      RETURN
      END
