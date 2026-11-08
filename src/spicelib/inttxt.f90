!$Procedure INTTXT ( Convert an integer to text )

      SUBROUTINE INTTXT ( N, STRING )

! $ Abstract
!
!     Convert an integer to an equivalent written phrase.
!     For example, convert 121 to 'ONE HUNDRED TWENTY-ONE'.
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
!     CONVERSION
!     PARSING
!     STRING
!     UNITS
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               N
      CHARACTER*(*)         STRING

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     N          I   An integer (less than 10**12 in absolute value).
!     STRING     O   An English string representing the cardinal of N.
!
! $ Detailed_Input
!
!     N        is any integer (less than 10**12 in absolute value).
!              If N is less than 0, -N must be a legitimate number.
!
! $ Detailed_Output
!
!     STRING   is the English cardinal equivalent of N. STRING will
!              contain only upper case letters.
!
!              The longest possible output string contains 145
!              characters. One such string is:
!
!                 'NEGATIVE '                                  //
!                 'SEVEN HUNDRED SEVENTY-SEVEN BILLION '       //
!                 'SEVEN HUNDRED SEVENTY-SEVEN MILLION '       //
!                 'SEVEN HUNDRED SEVENTY-SEVEN THOUSAND '      //
!                 'SEVEN HUNDRED SEVENTY-SEVEN'
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the resulting text is longer than the output string,
!         it will be truncated on the right, leaving only the most
!         significant portion of the number.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is used primarily for constructing error messages.
!     For example, an overflow message might look like the following:
!
!        'An excess of seventy-four parameters was detected.'
!
!     A second use might be to write dollar amounts: it's much harder
!     to tamper with a string like
!
!        'Two thousand four hundred seventy-one dollars'
!
!     than with the equivalent string
!
!        '$ 2471.00'
!
! $ Examples
!
!     N           STRING
!     ------      ------------------------------------------
!     -43         NEGATIVE FORTY-THREE
!      1          ONE
!      2          TWO
!      3          THREE
!      4          FOUR
!      20         TWENTY
!      21         TWENTY-ONE
!      99         NINETY-NINE
!      82131      EIGHTY-TWO THOUSAND ONE HUNDRED THIRTY-ONE
!
! $ Restrictions
!
!     1)  This routine assumes that N will always be less than
!         a trillion (10**12) in absolute value.
!
!     2)  In the event that N is less than zero, this routine assumes
!         that -N is a legitimate integer on the host machine.
!
!     3)  This routine assumes that an integer as large as 10**9
!         (one billion) is representable on the host machine.
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
!-    SPICELIB Version 1.0.0, 15-AUG-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     convert an integer to text
!
!-&


!
!     Local variables
!
      INTEGER               LNGEST
      PARAMETER           ( LNGEST = 9 )

      CHARACTER*(LNGEST)    NUMBER (19)
      CHARACTER*(LNGEST)    SUFF
      CHARACTER*(LNGEST)    TENS   ( 9)

      INTEGER               NUM
      INTEGER               PAD
      INTEGER               SPACE
      INTEGER               X
      INTEGER               Y

!
!     Saved variables
!

      SAVE                  NUMBER
      SAVE                  TENS

!
!     Initial values
!
      DATA       TENS    /  'TEN',     'TWENTY',   'THIRTY', &
                            'FORTY',   'FIFTY',    'SIXTY', &
                            'SEVENTY', 'EIGHTY',   'NINETY' /

      DATA       NUMBER  / 'ONE',         'TWO', &
                           'THREE',       'FOUR', &
                           'FIVE',        'SIX', &
                           'SEVEN',       'EIGHT', &
                           'NINE',        'TEN', &
                           'ELEVEN',      'TWELVE', &
                           'THIRTEEN',    'FOURTEEN', &
                           'FIFTEEN',     'SIXTEEN', &
                           'SEVENTEEN',   'EIGHTEEN', &
                           'NINETEEN'                 /


!
!     Zero is easy.
!
      IF ( N .EQ. 0 ) THEN

         STRING = 'ZERO'
         RETURN

      END IF

!
!     If the number is negative, the string begins with the word
!     `NEGATIVE', and otherwise the number can be treated as though
!     it were positive.
!
      IF ( N .LT. 0 ) THEN

         NUM    = - N
         STRING = 'NEGATIVE'

      ELSE

         NUM    =   N
         STRING = ' '

      END IF

!
!     Construct the number portion, from left to right: billions,
!     then millions, and so on. In case of overflow, SUFFIX simply
!     leaves the output string unchanged, so there is no need to
!     check explicitly for truncation.
!
      DO WHILE ( NUM .GT. 0 )

!
!        Find the right unit (billion, million, or whatever),
!        and the number (X) of those units. X should always
!        be between zero and 999, regardless of the units.
!
         IF ( NUM .GE. 1000000000 ) THEN

            X    = NUM / 1000000000
            SUFF = 'BILLION'
            NUM  = NUM - X * 1000000000

         ELSE IF  ( NUM .GE. 1000000 ) THEN

            X    = NUM / 1000000
            SUFF = 'MILLION'
            NUM  = NUM - X * 1000000

         ELSE IF  ( NUM .GE. 1000 )    THEN

            X    = NUM / 1000
            SUFF = 'THOUSAND'
            NUM  = NUM - X * 1000

         ELSE

            X    = NUM
            SUFF = ' '
            NUM  = 0

         END IF

!
!        Convert X to text, ...
!
         SPACE = 1

         DO WHILE ( X .GT. 0 )

            IF ( STRING .EQ. ' ' ) THEN
               PAD = 0
            ELSE
               PAD = 1
            END IF

            IF ( X .GE. 100 ) THEN

               Y = X / 100
               X = X - Y*100

               CALL SUFFIX ( NUMBER(Y), PAD, STRING )
               CALL SUFFIX ( 'HUNDRED', 1,   STRING )

            ELSE IF ( X .GE. 20 ) THEN

               Y = X / 10
               X = X - Y*10

               CALL SUFFIX ( TENS(Y), PAD, STRING )

               IF ( X .NE. 0 ) THEN
                  CALL SUFFIX ( '-', 0, STRING )
                  SPACE = 0
               END IF

            ELSE

               Y = X
               X = 0

               IF ( STRING .EQ. ' ' ) THEN
                  SPACE = 0
               END IF

               CALL SUFFIX ( NUMBER(Y), SPACE, STRING )

            END IF

         END DO

!
!        ... then add the units. Repeat as necessary.
!
         CALL SUFFIX ( SUFF, 1, STRING )

      END DO

      RETURN
      END
