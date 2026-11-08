!$Procedure ZZWNINSD ( Insert an interval into a DP window )

      SUBROUTINE ZZWNINSD ( LEFT, RIGHT, CONTEXT, WINDOW )

! $ Abstract
!
!      Insert an interval into a double precision window.
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
!      WINDOWS
!
! $ Keywords
!
!      WINDOWS
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE                'errhnd.inc'

      INTEGER                 LBCELL
      PARAMETER             ( LBCELL = -5 )

      DOUBLE PRECISION        LEFT
      DOUBLE PRECISION        RIGHT
      CHARACTER*(*)           CONTEXT
      DOUBLE PRECISION        WINDOW    ( LBCELL:* )

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      LEFT,
!      RIGHT      I   Left, right endpoints of new interval.
!      CONTEXT    I   A call explanation string.
!      WINDOW    I,O  Input, output window.
!
! $ Detailed_Input
!
!      LEFT,
!      RIGHT       are the left and right endpoints of the interval
!                  to be inserted.
!
!      CONTEXT     a context/explanation string to append to the
!                  long error message if an error signals. The caller
!                  need not include a message. A single blank, ' ',
!                  represents no message.
!
!      WINDOW      on input, is a window containing zero or more
!                  intervals.
!
! $ Detailed_Output
!
!      WINDOW      on output, is the original window following the
!                  insertion of the interval from LEFT to RIGHT.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If LEFT is greater than RIGHT, the error SPICE(BADENDPOINTS) is
!        signaled.
!
!     2) If the insertion of the interval causes an excess of elements,
!        the error SPICE(WINDOWEXCESS) is signaled.
!
! $ Files
!
!      None.
!
! $ Particulars
!
!      This routine inserts the interval from LEFT to RIGHT into the
!      input window. If the new interval overlaps any of the intervals
!      in the window, the intervals are merged. Thus, the cardinality
!      of the input window can actually decrease as the result of an
!      insertion. However, because inserting an interval that is
!      disjoint from the other intervals in the window can increase the
!      cardinality of the window, the routine signals an error.
!
!      This is the only unary routine to signal an error. No
!      other unary routine can increase the number of intervals in
!      the input window.
!
!      If a non-blank CONTEXT string passes from the caller, any error
!      signal will return the long error message with the CONTEXT
!      string appended to that message.
!
! $ Examples
!
!      Let WINDOW contain the intervals
!
!            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
!
!      Then the following series of calls
!
!            CALL ZZWNINSD ( 5,  5, CONTEXT, WINDOW)       (1)
!            CALL ZZWNINSD ( 4,  8, CONTEXT, WINDOW)       (2)
!            CALL ZZWNINSD ( 0, 30, CONTEXT, WINDOW)       (3)
!
!      produces the following series of windows
!
!            [ 1,  3 ]  [ 5,  5 ]  [  7, 11 ]  [ 23, 27 ]   (1)
!            [ 1,  3 ]  [ 4, 11 ]  [ 23, 27 ]               (2)
!            [ 0, 30 ]                                      (3)
!
! $ Restrictions
!
!      None.
!
! $ Literature_References
!
!      None.
!
! $ Author_and_Institution
!
!      K.R. Gehringer  (JPL)
!      N.J. Bachman    (JPL)
!      H.A. Neilan     (JPL)
!      W.L. Taber      (JPL)
!      I.M. Underwood  (JPL)
!
! $ Version
!
!-     SPICELIB Version 1.1.1, 05-OCT-2021 (NJB)
!
!         Corrected typos in long error messages and in comments.
!
!-     SPICELIB Version 1.0.0, 03-MAR-2009 (EDW)
!
!         This routine is a copy of the SPICELIB WNINSD routine
!         changed only by the addition of the CONTEXT string.
!
!-&

! $ Index_Entries
!
!     insert interval into d.p. window, with context string
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CARDD
      INTEGER               SIZED
      INTEGER               LASTNB
      LOGICAL               RETURN

!
!     Local Variables
!
      INTEGER               SIZE
      INTEGER               CARD

      INTEGER               I
      INTEGER               J

!
!     Local parameters
!
      CHARACTER*(LMSGLN)    MSG

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZWNINSD' )
      END IF

!
!     Get the size and cardinality of the window.
!
      SIZE = SIZED ( WINDOW )
      CARD = CARDD ( WINDOW )

!
!     Let's try the easy cases first. No input interval? No change.
!     Signal that an error has occurred and set the error message.
!
      IF ( LEFT .GT. RIGHT ) THEN

         MSG = 'Left endpoint greater-than right. Left endpoint ' &
            // 'was #1. Right endpoint was #2.'

         MSG = MSG(1: LASTNB(MSG) ) //' '// CONTEXT(1: LASTNB(CONTEXT) )

         CALL SETMSG ( MSG)
         CALL ERRDP  ( '#1', LEFT  )
         CALL ERRDP  ( '#2', RIGHT )
         CALL SIGERR ( 'SPICE(BADENDPOINTS)' )

         CALL CHKOUT ( 'ZZWNINSD' )
         RETURN

      ELSE IF ( CARD .EQ. 0  .OR.  LEFT .GT. WINDOW(CARD) ) THEN

!
!        Empty window? Input interval later than the end of the window?
!        Just insert the interval, if there's room.
!
         IF ( SIZE .GE. CARD+2 ) THEN

            CALL SCARDD ( CARD+2, WINDOW )
            WINDOW(CARD+1) = LEFT
            WINDOW(CARD+2) = RIGHT

         ELSE

            MSG = 'Window has size, #1, cardinality #2. Cannot ' &
               // 'insert an additional interval into the ' &
               // 'window.'

            MSG = MSG(1: LASTNB(MSG) ) //' '// &
                  CONTEXT(1: LASTNB(CONTEXT) )

            CALL SETMSG ( MSG)
            CALL ERRINT ( '#1', SIZE  )
            CALL ERRINT ( '#2', CARD )
            CALL SIGERR ( 'SPICE(WINDOWEXCESS)' )

         END IF

         CALL CHKOUT ( 'ZZWNINSD' )
         RETURN

      END IF


!
!     Now on to the tougher cases.
!
!     Skip intervals which lie completely to the left of the input
!     interval. (The index I will always point to the right endpoint
!     of an interval).
!
      I = 2

      DO WHILE ( I .LE. CARD  .AND.  WINDOW(I) .LT. LEFT )
         I = I + 2
      END DO

!
!     There are three ways this can go. The new interval can:
!
!        1) lie entirely between the previous interval and the next.
!
!        2) overlap the next interval, but no others.
!
!        3) overlap more than one interval.
!
!     Only the first case can possibly cause an overflow, since the
!     other two cases require existing intervals to be merged.
!

!
!     Case (1). If there's room, move succeeding intervals back and
!     insert the new one. If there isn't room, signal an error.
!
      IF ( RIGHT .LT. WINDOW(I-1) ) THEN

         IF ( SIZE .GE. CARD+2 ) THEN

            DO J = CARD, I-1, -1
               WINDOW(J+2) = WINDOW(J)
            END DO

            CALL SCARDD ( CARD+2, WINDOW )
            WINDOW(I-1) = LEFT
            WINDOW(I  ) = RIGHT

         ELSE

            MSG = 'Window has size, #1, cardinality #2. Cannot ' &
               // 'insert an additional interval into the ' &
               // 'window. The new interval lies entirely ' &
               // 'between the previous interval and the' &
               // 'next.'

            MSG = MSG(1: LASTNB(MSG) ) //' '// &
                  CONTEXT(1: LASTNB(CONTEXT) )

            CALL SETMSG ( MSG)
            CALL ERRINT ( '#1', SIZE  )
            CALL ERRINT ( '#2', CARD )
            CALL SIGERR ( 'SPICE(WINDOWEXCESS)' )

            CALL CHKOUT ( 'ZZWNINSD' )
            RETURN

         END IF

!
!     Cases (2) and (3).
!
      ELSE

!
!        The left and right endpoints of the new interval may or
!        may not replace the left and right endpoints of the existing
!        interval.
!
         WINDOW(I-1) = MIN ( LEFT,  WINDOW(I-1) )
         WINDOW(I  ) = MAX ( RIGHT, WINDOW(I  ) )

!
!        Skip any intervals contained in the one we modified.
!        (Like I, J always points to the right endpoint of an
!        interval.)
!
         J = I + 2

         DO WHILE ( J .LE. CARD  .AND.  WINDOW(J) .LE. WINDOW(I) )
            J = J + 2
         END DO

!
!        If the modified interval extends into the next interval,
!        merge the two. (The modified interval grows to the right.)
!
         IF ( J .LE. CARD  .AND.  WINDOW(I) .GE. WINDOW(J-1) ) THEN
            WINDOW(I) = WINDOW(J)
            J         = J + 2
         END IF

!
!        Move the rest of the intervals forward to take up the
!        spaces left by the absorbed intervals.
!
         DO WHILE ( J .LE. CARD )
            I           = I + 2
            WINDOW(I-1) = WINDOW(J-1)
            WINDOW(I  ) = WINDOW(J  )
            J           = J + 2
         END DO

         CALL SCARDD ( I, WINDOW )

      END IF

      CALL CHKOUT ( 'ZZWNINSD' )

      RETURN
      END
