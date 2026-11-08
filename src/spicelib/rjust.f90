!$Procedure RJUST ( Right justify a character string )

      SUBROUTINE RJUST ( INPUT, OUTPUT )

! $ Abstract
!
!     Right justify a character string.
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
!     ASCII
!     CHARACTER
!     STRING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    INPUT
      CHARACTER*(*)    OUTPUT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INPUT      I   Input character string.
!     OUTPUT     O   Output character string, right justified.
!
! $ Detailed_Input
!
!     INPUT    is the input character string.
!
! $ Detailed_Output
!
!     OUTPUT   is the output character string, right justified.
!              If INPUT is too large to fit into OUTPUT, it is
!              truncated on the left.
!
!              OUTPUT may overwrite INPUT.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Any trailing blanks in the input string are removed, and
!     the remaining string is copied to the output string.
!
! $ Examples
!
!     The following examples should illustrate the use of RJUST.
!
!        'ABCDE          '   becomes  '          ABCDE'
!        'AN EXAMPLE     '            '     AN EXAMPLE'
!        '   AN EXAMPLE  '            '     AN EXAMPLE'
!        '               '            '               '
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
!     right justify a character_string
!
!-&


! $ Revisions
!
!-     Beta Version 1.1.0, 11-DEC-1989 (IMU)
!
!         Did not work on Sun when INPUT and OUTPUT were
!         the same string, and where the initial and final
!         locations of the non-blank part of the string
!         overlapped.
!
!         The solution is to move the characters one by one,
!         starting from the right side of the input string.
!         That way, nothing gets clobbered.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               FRSTNB
      INTEGER               LASTNB

!
!     Local variables
!
      INTEGER               FIRST
      INTEGER               I
      INTEGER               LAST
      INTEGER               LOC
      INTEGER               START



!
!     Blank string? It's all the same.
!
      IF ( INPUT .EQ. ' ' ) THEN
         OUTPUT = INPUT

!
!     Get the first non-blank character. Start OUTPUT at that point.
!
      ELSE
         FIRST  = FRSTNB ( INPUT )
         LAST   = LASTNB ( INPUT )
         START  = LEN ( OUTPUT ) - (LAST-FIRST)

!
!        If the input string is too long (START < 1), move FIRST
!        up a little to truncate on the left.
!
         IF ( START .LT. 1 ) THEN
            FIRST = FIRST + (1-START)
            START = 1
         END IF

!
!        Move the characters in reverse order, to keep from stomping
!        anything if the operation is being done in place.
!
         LOC = LEN ( OUTPUT )

         DO I = LAST, FIRST, -1
            OUTPUT(LOC:LOC) = INPUT(I:I)
            LOC = LOC - 1
         END DO

!
!        Clear the first part of OUTPUT, if necessary.
!
         IF ( START .GT. 1 ) THEN
            OUTPUT(1:START-1) = ' '
         END IF

      END IF

      RETURN
      END
