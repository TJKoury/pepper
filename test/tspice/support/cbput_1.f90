
!$Procedure CBPUT ( Character buffer, put )

      SUBROUTINE CBPUT_1 ( BEGIN, END, STRING, BUFFER )
      IMPLICIT NONE


! $ Abstract
!
!     Put (overwrite) a substring of a character buffer.
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
      CHARACTER*(*)         STRING
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BEGIN,
!     END        I   Initial, final buffer locations.
!     STRING     I   String.
!     BUFFER    I,O  Character buffer.
!
! $ Detailed_Input
!
!     BEGIN,
!     END         are the initial and final buffer locations of the
!                 part of the buffer to be overwritten.
!
!     STRING      is a character string.
!
!     BUFFER      is a character buffer.
!
! $ Detailed_Output
!
!     BUFFER      is the same character buffer, in which locations
!                 BEGIN through END have been replaced with the
!                 contents of STRING.
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
!        BUFFER(BEGIN:END) = STRING
!
!     If shorter than STRING, the substring is truncated. If longer,
!     it is padded with blanks.
!
! $ Examples
!
!     The code fragment
!
!        CALL CBPUT (  1, 26, ' ',                BUFFER      )
!        CALL CBPUT (  1, 10, 'ABC',              BUFFER      )
!        CALL CBPUT ( 11, 20, 'KLMNOPQRSTUVWXYZ', BUFFER      )
!        CALL CBGET (  1, 26,                     BUFFER, STR )
!
!        WRITE (*,*) '+--------------------------+'
!        WRITE (*,*) '|'    // STR(1:26) //     '|'
!        WRITE (*,*) '+--------------------------+'
!
!     produces the following output.
!
!        +--------------------------+
!        |ABC       KLMNOPQRST      |
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
         CALL CHKIN ( 'CBPUT_1' )

         IF (      BEGIN  .LT.  1 &
              .OR. END    .GT.  SIZECB_1 ( BUFFER ) &
              .OR. BEGIN  .GT.  END                 ) THEN

            CALL SETMSG ( 'Tried to access locations #:#.' )
            CALL ERRINT ( '#', BEGIN                       )
            CALL ERRINT ( '#', END                         )

            CALL SIGERR ( 'SPICE(CBNOSUCHSTR)' )
            CALL CHKOUT ( 'CBPUT_1'              )
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
!     Assign one character at a time, changing output lines when
!     necessary, and assigning blanks if the input string should
!     come to an early end.
!
      NEXT = 1
      LAST = LEN ( STRING )

      DO I = BEGIN, END

         IF ( NEXT .LE. LAST ) THEN
            BUFFER(L)(B:B) = STRING(NEXT:NEXT)
            NEXT           = NEXT + 1
         ELSE
            BUFFER(L)(B:B) = ' '
         END IF

         IF ( B .LT. BUFLEN ) THEN
            B = B + 1
         ELSE
            L = L + 1
            B = 1
         END IF

      END DO

      CALL CHKOUT ( 'CBPUT_1' )
      RETURN
      END
