
!$Procedure CBGET ( Character buffer, get )

      SUBROUTINE CBGET_1 ( BEGIN, END, BUFFER, STRING  )
      IMPLICIT NONE


! $ Abstract
!
!     Get (return) a substring of a character buffer.
!
! $ Required_Reading
!
!     CB
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     STRING
!     TEXT
!
! $ Declarations

      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF = 0 )

      INTEGER               BEGIN
      INTEGER               END
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )
      CHARACTER*(*)         STRING

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BEGIN,
!     END        I   Initial, final buffer locations.
!     BUFFER     I   Character buffer.
!     STRING     O   String.
!
! $ Detailed_Input
!
!     BEGIN,
!     END         are the initial and final buffer locations of
!                 the string to be returned.
!
!     BUFFER      is a character buffer.
!
! $ Detailed_Output
!
!     STRING      is the string contained between locations BEGIN and
!                 END of BUFFER.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) The error 'SPICE(CBNOSUCHSTR)' is signalled whenever any of
!        the following conditions is detected:
!
!           -- BEGIN is less than one.
!
!           -- END is greater than the size of BUFFER.
!
!           -- BEGIN is greater than END.
!
! $ Particulars
!
!     If you think of the character buffer as a single character string,
!     this is exactly equivalent to the operation
!
!        STRING = BUFFER(BEGIN:END)
!
!     If shorter than the substring, STRING is truncated. If longer,
!     it is padded with blanks.
!
! $ Examples
!
!     The code fragment
!
!        STR = '..........................'
!
!        CALL CBPUT (  1, 13, 'ABCDEFGHIJKLM', BUFFER             )
!        CALL CBPUT ( 14, 26, 'NOPQRSTUVWXYZ', BUFFER             )
!        CALL CBGET (  1,  3,                  BUFFER, STR( 1:10) )
!        CALL CBGET (  1, 26,                  BUFFER, STR(11:13) )
!
!        WRITE (*,*) '+--------------------------+'
!        WRITE (*,*) '|'    // STR(1:26) //     '|'
!        WRITE (*,*) '+--------------------------+'
!
!     produces the following output.
!
!        +--------------------------+
!        |ABC       ABC.............|
!        +--------------------------+
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
      INTEGER               BUFLEN
      INTEGER               L
      INTEGER               B
      INTEGER               NEXT
      INTEGER               LAST
      INTEGER               I

!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CBGET_1' )

         IF (      BEGIN  .LT.  1 &
              .OR. END    .GT.  SIZECB_1 ( BUFFER ) &
              .OR. BEGIN  .GT.  END                 ) THEN

            CALL SETMSG ( 'Tried to access locations #:#.' )
            CALL ERRINT ( '#', BEGIN                       )
            CALL ERRINT ( '#', END                         )

            CALL SIGERR ( 'SPICE(CBNOSUCHSTR)' )
            CALL CHKOUT ( 'CBGET_1'              )
            RETURN
         END IF
      END IF

!
!     Storage begins at location B in line L.
!
      BUFLEN = LEN ( BUFFER(1) )

      L      =     ( BEGIN - 1 ) / BUFLEN   + 1
      B      = MOD ( BEGIN - 1,    BUFLEN ) + 1

!
!     Assign one character at a time, changing input lines when
!     necessary. Do not assign any characters beyond the end of
!     the output string.
!
      NEXT = 1
      LAST = LEN ( STRING )

      DO I = BEGIN, END

         IF ( NEXT .LE. LAST ) THEN
            STRING(NEXT:NEXT) = BUFFER(L)(B:B)
            NEXT              = NEXT + 1
         END IF

         IF ( B .LT. BUFLEN ) THEN
            B = B + 1
         ELSE
            L = L + 1
            B = 1
         END IF

      END DO

!
!     Pad the output string with blanks, if necessary.
!
      IF ( NEXT .LE. LAST ) THEN
         STRING(NEXT: ) = ' '
      END IF

      CALL CHKOUT ( 'CBGET_1' )
      RETURN
      END
