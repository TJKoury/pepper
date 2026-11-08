!$Procedure UCASE ( Convert to uppercase )

      SUBROUTINE UCASE ( IN, OUT )

! $ Abstract
!
!     Convert the characters in a string to uppercase.
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
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    IN
      CHARACTER*(*)    OUT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     OUT        O   Output string, all uppercase.
!
! $ Detailed_Input
!
!     IN       is the input string.
!
! $ Detailed_Output
!
!     OUT      is the output string. This is the input string
!              with all lowercase letters converted to uppercase.
!              Non-letters are not affected.
!
!              OUT may overwrite IN.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the output string length is less than the input string
!         length, the result will be truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Convert each lowercase character in IN to uppercase.
!
! $ Examples
!
!     'This is an example'   becomes   'THIS IS AN EXAMPLE'
!     '12345 +-=? > * $ &'             '12345 +-=? > * $ &'
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
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 05-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Added entry #1 in $Exceptions section.
!
!-    SPICELIB Version 1.1.0, 13-MAR-1996 (KRG)
!
!        Removed the calls to the string lexicographic functions.
!
!        Modified the algorithm to use the ICHAR() intrinsic function
!        and some local integer storage for the bases of the lower and
!        upper case letters.
!
!        Added a "FIRST" clause to the code so that the lower and
!        upper case bases and the separation between them are only
!        initialized the first time the subroutine is called rather
!        than every time.
!
!        These changes were made to improve the execution speed of
!        the subroutine
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
!     convert to uppercase
!
!-&


!
!     Local variables
!
      INTEGER          I
      INTEGER          ICH
!
!     Saved variables
!
      INTEGER          SHIFT
      INTEGER          LOWA
      INTEGER          LOWZ

      LOGICAL          FIRST

      SAVE             FIRST
      SAVE             SHIFT
      SAVE             LOWA
      SAVE             LOWZ
!
!     Initial Data
!
      DATA             FIRST / .TRUE. /
!
!     Do some set up stuff the first time through so that we do not
!     need to reinitialize the boundary values used for comparisons
!     and the shift on each call.
!
      IF ( FIRST ) THEN
         FIRST = .FALSE.
         LOWA  = ICHAR ('a')
         LOWZ  = ICHAR ('z')
         SHIFT = ICHAR ('A') - LOWA
      END IF
!
!     Move the string from IN to OUT. Step through OUT one character
!     at a time, translating letters between 'a' and 'z' to uppercase.
!
      OUT   = IN

      DO I = 1, LEN (OUT)

         ICH = ICHAR( OUT(I:I) )

         IF ( ( ICH .GE. LOWA ) .AND. ( ICH .LE. LOWZ ) ) THEN
            OUT(I:I) = CHAR ( ICH + SHIFT )
         END IF

      END DO

      RETURN
      END
