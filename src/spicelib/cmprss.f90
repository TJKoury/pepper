!$Procedure CMPRSS ( Compress a character string )

      SUBROUTINE CMPRSS ( DELIM, N, INPUT, OUTPUT )

! $ Abstract
!
!     Compress a character string by removing occurrences of
!     more than N consecutive occurrences of a specified
!     character.
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

      CHARACTER*1         DELIM
      INTEGER             N
      CHARACTER*(*)       INPUT
      CHARACTER*(*)       OUTPUT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DELIM      I   Delimiter to be compressed.
!     N          I   Maximum consecutive occurrences of DELIM.
!     INPUT      I   Input string.
!     OUTPUT     O   Compressed string.
!
! $ Detailed_Input
!
!     DELIM    is the delimiter to be compressed out of the string.
!              This may be any ASCII character.
!
!     N        is the maximum number of consecutive occurrences
!              of DELIM that will be allowed to remain in the
!              output string.
!
!     INPUT    is the input string.
!
! $ Detailed_Output
!
!     OUTPUT   is the output string. This is the input string
!              with all occurrences of more than N consecutive
!              delimiters removed.
!
!              If OUTPUT is not large enough to hold the
!              compressed string, it is truncated on the right.
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
!     1)  If the output string length is too short to contain the result
!         of compressing the input string, the result is truncated on
!         the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Occurrences of more than N consecutive delimiters are removed
!     from the input string as it is copied to the output string.
!     If the output string is not large enough to hold the compressed
!     string, it is truncated on the right.
!
! $ Examples
!
!     Let DELIM = '.', and N = 2. Then
!
!        'ABC...DE.F...',           becomes   'ABC..DE.F..'
!        ' ...........'                       ' ..'
!        '.. ..AB....CD'                      '.. ..AB..CD'
!
!     Let DELIM = ' ', and N = 0. Then
!
!        ' DISK:[USER.  SUB  ]'     becomes   'DISK:[USER.SUB]'
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
!-    SPICELIB Version 1.1.0, 09-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Added $Exceptions entry #1.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     compress a character_string
!
!-&


!
!
!     Local Variables
!
      INTEGER          I
      INTEGER          J
      INTEGER          COUNT

      INTEGER          INLEN
      INTEGER          OUTLEN


!
!     Find out how much space there is in the INPUT and OUTPUT strings
!     and initialize the delimiter counter and output place holder.
!
      INLEN  = LEN(INPUT)
      OUTLEN = LEN(OUTPUT)

      COUNT  = 0
      J      = 0

      DO I = 1, INLEN

!
!        Check each character to see if it is a delimiter or not.
!
         IF ( INPUT(I:I) .EQ. DELIM ) THEN

            COUNT = COUNT + 1

!
!           Copy delimiters until enough consecutive delimiters
!           have been accumulated.  When enough consecutive delimiters
!           have accumulated, we no longer copy them.
!
            IF ( COUNT .LE. N ) THEN
               J           = J + 1
               OUTPUT(J:J) = INPUT(I:I)
            END IF

         ELSE

!
!           We don't have a delimiter here so we just copy the
!           character and set the delimiter counter to zero.
!
            COUNT       = 0
            J           = J + 1
            OUTPUT(J:J) = INPUT(I:I)

         END IF

         IF ( J .EQ. OUTLEN ) THEN
            RETURN
         END IF

      END DO

!
!     Pad any left over space in the output string with blanks.
!
      IF ( J .LT. OUTLEN ) THEN
         OUTPUT(J+1:) = ' '
      END IF

      RETURN
      END
