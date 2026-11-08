
!$Procedure LBREM ( Line buffer, remove )

      SUBROUTINE LBREM_1 ( POS, PTRS, BUFFER )
      IMPLICIT NONE

! $ Abstract
!
!     Remove a line from a line buffer.
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

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     POS        I   Position of line to be removed.
!     PTRS,
!     BUFFER    I,O  Line buffer.
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
!     PTRS,
!     BUFFER      are the pointer and character components of the
!                 same line buffer, after the specified line has been
!                 removed.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     1) If POS is less than one, or if POS is greater than the
!        number of lines currently stored in the buffer, the error
!        'SPICE(LBNOSUCHLINE)' is signalled.
!
! $ Particulars
!
!     Existing lines may be removed from at any position within a line
!     buffer. All subsequent lines are moved forward to take up the
!     slack.
!
! $ Examples
!
!     Let the line buffer (P,B) contain the following lines
!
!       If you can make one heap of all your winnings
!       And risk it on one turn of pitch-and-toss,
!       And lose, and start again at your beginnings,
!       And never breathe a word about your loss:
!       If you can force your heart and nerve and sinew
!       To serve your turn long after they are gone,
!
!     Following the calls
!
!       CALL LBREM ( 3, P, B )
!       CALL LBREP ( 3, P, B )
!
!     it contains the lines
!
!       If you can make one heap of all your winnings
!       And risk it on one turn of pitch-and-toss,
!       If you can force your heart and nerve and sinew
!       To serve your turn long after they are gone,
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
!-    Inspekt Version 3.0.0 9-May-1994 (WLT)
!
!        Added a "TOUCHC" to the input buffer so that compilers
!        won't complain about input arguments not being used.
!
!        And fixed the addition of "TOUCHC" to refere to LBCBUF
!        instead of LBCELL
!
!-    Beta Version 1.0.0, 19-JAN-1989 (DT)
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               RETURN
!
!     Other Functions
!
      CHARACTER*(1)         TOUCHC

!
!     Local variables
!
      INTEGER               MAXLN
      INTEGER               NLINE
      INTEGER               NCOM
      INTEGER               PCARD

      INTEGER               POSPTR
      INTEGER               POSCOM
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               PTR            ( 2 )
      INTEGER               OFFSET
      INTEGER               I

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
         CALL CHKIN ( 'LBREM_1' )
      END IF
!
!     We touch the input buffer so that compilers will not complain
!     that BUFFER is an unused argument.  It really is unused, but
!     it's in the calling sequence for the sake of uniformity of
!     the calling sequences for the line buffer routines.
!
      BUFFER(LBCBUF)(1:1) = TOUCHC ( BUFFER(LBCBUF) )


!
!     Recover the essential control information.
!
      CALL LBDES_1 ( PTRS, MAXLN, NLINE, NCOM, PCARD )

!
!     No way to remove a line that's not in the table.
!
      IF ( POS .LT. 1  .OR.  POS .GT. NLINE ) THEN
         CALL SETMSG ( 'Tried to access line # of #.' )
         CALL ERRINT ( '#', POS                       )
         CALL ERRINT ( '#', NLINE                     )
         CALL SIGERR ( 'SPICE(LBNOSUCHLINE)'          )
         CALL CHKOUT ( 'LBREM_1'                        )
         RETURN
      END IF

!
!     Save the bounds of the stored line before removing the name
!     and pointers from their respective tables.
!
      POSPTR = 2 * POS - 1
      BEGIN  = PTRS(POSPTR  )
      END    = PTRS(POSPTR+1)
      NLINE  = NLINE - 1

      CALL REMLAI ( 2, POSPTR, PTRS(1), PCARD )

!
!     Add the interval to the complement. Insert it directly, then
!     do any merges required.
!
      OFFSET = NLINE  * 2
      POSCOM = OFFSET + 1

      DO I = OFFSET+2, PCARD, 2
         IF ( BEGIN .GT. PTRS(I) ) THEN
            POSCOM = I + 1
         END IF
      END DO

      CALL INSLAI ( PTR, 2, POSCOM, PTRS(1), PCARD )

      DO I = PCARD-2, OFFSET+2, -2
         IF ( PTRS(I+1) .EQ. PTRS(I) + 1 ) THEN
            CALL REMLAI ( 2, I, PTRS(1), PCARD )
         END IF
      END DO

      NCOM = ( PCARD / 2 ) - NLINE

      CALL LBUPD_1 ( NLINE, NCOM, PTRS )

      CALL CHKOUT ( 'LBREM_1' )
      RETURN
      END
