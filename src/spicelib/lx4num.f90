!$Procedure LX4NUM (Scan for a number)

      SUBROUTINE LX4NUM ( STRING, FIRST, LAST, NCHAR )

! $ Abstract
!
!     Scan a string from a specified starting position for the
!     end of a number.
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
!     PARSING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         STRING
      INTEGER               FIRST
      INTEGER               LAST
      INTEGER               NCHAR

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   any character string
!     FIRST      I   first character to scan from in STRING
!     LAST       O   last character that is part of a number
!     NCHAR      O   number of characters in the number.
!
! $ Detailed_Input
!
!     STRING   is any character string.
!
!     FIRST    is the location in the string to beginning scanning
!              for a  number. It is assumed that the number begins
!              at FIRST.
!
! $ Detailed_Output
!
!     LAST     is the last character at or after FIRST such that
!              the substring STRING(FIRST:LAST) is a number.
!              If there is no such substring, LAST will be returned
!              with the value FIRST-1.
!
!     NCHAR    is the number of characters in the number
!              that begins at FIRST and ends at last. If there
!              is no such string NCHAR will be given the value 0.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If FIRST is beyond either end of the string, then LAST will be
!         returned with the value FIRST-1 and NCHAR will be returned
!         with the value 0.
!
!     2)  If STRING(FIRST:FIRST) is not part of a number
!         then LAST will be returned with the value FIRST-1 and NCHAR
!         will be returned with the value 0.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows you to scan forward in a string to locate
!     a number that begins on the input character FIRST. Note
!     that all decimal numbers are included in the list of numbers.
!     The main difference between decimal numbers and numbers is that
!     numbers may have an exponential expression attached (i.e. the
!     exponent character 'e','E','d' or 'D' followed by an signed
!     integer).
!
! $ Examples
!
!     Suppose you believe that a string has the form
!
!        X%Y%Z
!
!     where X, Y, and Z are decimal numbers of some unknown
!     length and % stands for some non-numeric character. You could
!     use this routine to locate the numbers in the
!     string as shown below. We'll keep track of the beginning and
!     ending of the numbers in the integer arrays B and E.
!
!     FIRST = 1
!     I     = 0
!
!     DO WHILE ( FIRST .LT. LEN(STRING) )
!
!        CALL LX4NUM ( STRING, FIRST, LAST, NCHAR )
!
!        IF ( NCHAR .GT. 0 ) THEN
!
!           I     = I    + 1
!           B(I)  = FIRST
!           E(I)  = LAST
!           FIRST = LAST + 2
!
!        ELSE
!
!           FIRST = FIRST + 1
!
!        END IF
!
!     END DO
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
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 04-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Fixed return value for LAST in $Exceptions section entry #1.
!
!-    SPICELIB Version 1.0.0, 12-JUL-1994 (WLT)
!
!-&


! $ Index_Entries
!
!     Scan a string for a number.
!
!-&


      INTEGER               F
      INTEGER               I
      INTEGER               L
      INTEGER               N
      INTEGER               TEMP

      LAST  = FIRST - 1
      L     = LEN ( STRING )
!
!     If start is beyond the ends of the string, we  can quit now.
!
      IF ( FIRST .LT. 1 .OR. FIRST .GT. L ) THEN
         NCHAR = 0
         RETURN
      END IF
!
!     If this is a number, it must begin with a decimal number
!     substring.
!
      CALL LX4DEC ( STRING, FIRST, LAST, NCHAR )

      IF ( NCHAR .GT. 0 .AND. LAST .LT. L ) THEN

         F = LAST + 1
         I = ICHAR ( STRING(F:F) )
!
!        See if we have an exponent.
!
         IF (      I .EQ. ICHAR('e') &
              .OR. I .EQ. ICHAR('E') &
              .OR. I .EQ. ICHAR('D') &
              .OR. I .EQ. ICHAR('d') ) THEN
!
!           Starting after the exponent character see
!           if we have a signed integer.
!
            F = F + 1

            CALL LX4SGN ( STRING, F, TEMP, N )
!
!           If there was a signed integer, N will be bigger than
!           zero and TEMP will point to the last character of
!           the number.  Otherwise we just fall through and leave
!           LAST and NCHAR alone.
!
            IF ( N .GT. 0 ) THEN

               LAST  = TEMP
               NCHAR = LAST + 1 - FIRST

            END IF

         END IF

      END IF


      RETURN
      END
