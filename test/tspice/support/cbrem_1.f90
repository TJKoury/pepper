
!$Procedure CBREM ( Character buffer, remove )

      SUBROUTINE CBREM_1 ( BEGIN, END, BUFFER )
      IMPLICIT NONE

! $ Abstract
!
!     Remove a string from a character buffer.
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

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BEGIN,
!     END        I   Initial, final buffer locations.
!     BUFFER    I,O  Character buffer.
!
! $ Detailed_Input
!
!     BEGIN,
!     END         are the initial and final locations within the
!                 character buffer bounding the part of the buffer
!                 to be removed.
!
!     BUFFER      is a character buffer.
!
! $ Detailed_Output
!
!     BUFFER      is the same character buffer, with the original
!                 contents of locations BEGIN through END removed.
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
!     this is exactly equivalent to the sequence
!
!        TEMP            = BUFFER(END+1: )
!        BUFFER(BEGIN: ) = TEMP
!
!     where TEMP is a string of infinite length.
!
! $ Examples
!
!     The code fragment
!
!       CALL CBPUT (  1, 26, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', BUFFER      )
!       CALL CBPUT ( 27, 52, '..........................', BUFFER      )
!       CALL CBREM (  2, 25,                               BUFFER      )
!       CALL CBGET (  1, 26,                               BUFFER, STR )
!
!       WRITE (*,*) '+--------------------------+'
!       WRITE (*,*) '|'    // STR(1:26) //     '|'
!       WRITE (*,*) '+--------------------------+'
!
!     produces the following output.
!
!       +--------------------------+
!       |AZ........................|
!       +--------------------------+
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
      INTEGER               ENDBUF
      INTEGER               L
      INTEGER               B
      INTEGER               NL
      INTEGER               NB
      INTEGER               I



!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CBREM_1' )

         IF (      BEGIN  .LT.  1 &
              .OR. END    .GT.  SIZECB_1 ( BUFFER ) &
              .OR. BEGIN  .GT.  END                 ) THEN

            CALL SETMSG ( 'Tried to access locations #:#.' )
            CALL ERRINT ( '#', BEGIN                       )
            CALL ERRINT ( '#', END                         )

            CALL SIGERR ( 'SPICE(CBNOSUCHSTR)' )
            CALL CHKOUT ( 'CBREM_1'              )
            RETURN
         END IF
      END IF

!
!     Essential limits.
!
      BUFLEN = LEN    ( BUFFER(1) )
      ENDBUF = SIZECB_1 ( BUFFER    )

!
!     Each guy gets moved from location B in line L to location NB
!     in line NL. (N stands for New.)
!
      L      =     ( END ) / BUFLEN   + 1
      B      = MOD ( END,    BUFLEN ) + 1

      NL     =     ( BEGIN - 1 ) / BUFLEN   + 1
      NB     = MOD ( BEGIN - 1,    BUFLEN ) + 1

      DO I = END+1, ENDBUF

         BUFFER(NL)(NB:NB) = BUFFER(L)(B:B)

         IF ( B .LT. BUFLEN ) THEN
            B = B + 1
         ELSE
            L = L + 1
            B = 1
         END IF

         IF ( NB .LT. BUFLEN ) THEN
            NB = NB + 1
         ELSE
            NL = NL + 1
            NB = 1
         END IF

      END DO

!
!     Now we can just overwrite the vacated space at the end.
!
      CALL CBPUT_1 ( ENDBUF-(END-BEGIN), ENDBUF, ' ', BUFFER )

      CALL CHKOUT ( 'CBREM_1' )
      RETURN
      END
