!$Procedure SHIFTC ( Shift a character string )

      SUBROUTINE SHIFTC ( IN, DIR, NSHIFT, FILLC, OUT )

! $ Abstract
!
!     Shift the contents of a character string to the left or right.
!     Characters moved past the beginning or end of the string are
!     lost. Vacant spaces are filled with a specified character.
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
!     None.
!
! $ Keywords
!
!     CHARACTER
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)      IN
      CHARACTER*1        DIR
      INTEGER            NSHIFT
      CHARACTER*1        FILLC
      CHARACTER*(*)      OUT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     DIR        I   Direction to shift.
!     NSHIFT     I   Number of times to shift.
!     FILLC      I   Character to fill spaces left vacant.
!     OUT        O   Shifted string.
!
! $ Detailed_Input
!
!     IN       is the input character string.
!
!     DIR      is the direction in which the characters in the
!              string are to be shifted.
!
!                    'L' or 'l'  to shift left.
!                    'R' or 'r'  to shift right.
!
!     NSHIFT   is the number of times the string is to be
!              shifted.
!
!     FILLC    is the character with which spaces left vacant by
!              the shift are to be filled.
!
! $ Detailed_Output
!
!     OUT      is the output string. This is the input string,
!              shifted N times, filled with FILLC.
!
!              OUT may overwrite IN.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  A negative shift in one direction is equal to a positive
!         shift in the other.
!
!     2)  If a legal direction ('L', 'l', 'R', 'r') is not supplied,
!         the error SPICE(ILLEGSHIFTDIR) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The first NSHIFT characters of the output string are filled
!     with the fill character, and the input string is appended.
!
! $ Examples
!
!     If FILLC = ' '
!
!            'abcde'   shifted left twice becomes     'cde  '
!            'abcde'   shifted right once becomes     ' abcd'
!
!     If FILLC = '.'
!
!            '12345 '  shifted right once becomes     '.12345'
!            'Apple '  shifted left ten times becomes '......'
!
! $ Restrictions
!
!     1)  SHIFTC is being maintained for historical reasons only.
!         To avoid the overhead imposed by the error handling in this
!         routine, use the equivalent routines SHIFTL and SHIFTR.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     shift a character_string
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 17-OCT-1988 (IMU)
!
!        Dick Simpson reported that the statement
!
!           OUT(N+1: ) = IN
!
!        which began the right-shift section failed on his Data
!        General, presumably because it requires temporary buffering
!        of characters. The new version seems to work for all cases.
!        It has been tested on the VAX and on the Sun (f77 compiler).
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SHIFTC' )
      END IF


!
!     Hand off to one of the other routines.
!
      IF ( DIR .EQ. 'L' .OR. DIR .EQ. 'l' ) THEN

         IF ( NSHIFT .GE. 0 ) THEN
            CALL SHIFTL ( IN, NSHIFT, FILLC, OUT )

         ELSE
            CALL SHIFTR ( IN, -NSHIFT, FILLC, OUT )
         END IF

      ELSE IF ( DIR .EQ. 'R' .OR. DIR .EQ. 'r' ) THEN

         IF ( NSHIFT .GE. 0 ) THEN
            CALL SHIFTR ( IN, NSHIFT, FILLC, OUT )

         ELSE
            CALL SHIFTL ( IN, -NSHIFT, FILLC, OUT )
         END IF

      ELSE
         CALL SETMSG ( 'Shift direction (#) must be L, l, R, or r.' )
         CALL ERRCH  ( '#', DIR )

         CALL SIGERR ( 'SPICE(ILLEGSHIFTDIR)' )
      END IF

      CALL CHKOUT ( 'SHIFTC' )
      RETURN
      END
