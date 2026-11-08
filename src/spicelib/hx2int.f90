!$Procedure HX2INT  ( Signed hexadecimal string to integer )

      SUBROUTINE HX2INT ( STRING, NUMBER, ERROR, ERRMSG )

! $ Abstract
!
!     Convert a signed hexadecimal string representation of an integer
!     to its equivalent integer.
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
!     ALPHANUMERIC
!     CONVERSION
!
! $ Declarations
!
      CHARACTER*(*)         STRING
      INTEGER               NUMBER
      LOGICAL               ERROR
      CHARACTER*(*)         ERRMSG
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Hexadecimal string to be converted to an integer.
!     NUMBER     O   Integer value to be returned.
!     ERROR      O   A logical flag which is .TRUE. on error.
!     ERRMSG     O   A descriptive error message.
!
! $ Detailed_Input
!
!     STRING   is the hexadecimal string to be converted to an integer.
!
!              The following table describes the character set used
!              to represent the hexadecimal digits and their
!              corresponding values.
!
!              Character    Value           Character    Value
!              ---------    -----           ---------    -----
!                '0'          0                '8'          8
!                '1'          1                '9'          9
!                '2'          2              'A','a'       10
!                '3'          3              'B','b'       11
!                '4'          4              'C','c'       12
!                '5'          5              'D','d'       13
!                '6'          6              'E','e'       14
!                '7'          7              'F','f'       15
!
!             The plus sign, '+', and the minus sign, '-', are used as
!             well, and they have their usual meanings.
!
!             A hexadecimal character string parsed by this routine
!             should consist of a sign, '+' or '-' (the plus sign is
!             optional for nonnegative numbers), followed immediately
!             by a contiguous sequence of hexadecimal digits, e.g.:
!
!                (1)   +h h ... h
!                        1 2     n
!
!                (2)   -h h ... h
!                        1 2     n
!
!                (3)   h h ... h
!                       1 2     n
!
!             where h  represents an hexadecimal digit.
!                    i
!
!             STRING may have leading and trailing blanks, but blanks
!             embedded within the significant portion of the character
!             string are not allowed. This includes any blanks which
!             appear between the sign character and the first
!             hexadecimal digit.
!
! $ Detailed_Output
!
!     NUMBER   is the integer value to be returned. The value of this
!              variable is not changed if an error occurs while parsing
!              the hexadecimal character string.
!
!     ERROR    is a logical flag which indicates whether an error
!              occurred while attempting to parse NUMBER from the
!              hexadecimal character string STRING. ERROR will have the
!              value .TRUE. if an error occurs. It will have the value
!              .FALSE. otherwise.
!
!     ERRMSG   contains a descriptive error message if an error
!              occurs while attempting to parse NUMBER from the
!              hexadecimal character string STRING, blank otherwise.
!              The error message will be left justified.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If an unexpected character is encountered while parsing the
!         hexadecimal character string, an appropriate error message
!         will be set, and the routine will exit. The value of NUMBER
!         will be unchanged.
!
!     2)  If the string represents a number that is larger than
!         the maximum representable integer an appropriate error
!         message will be set, and the routine will exit. The value
!         of NUMBER will be unchanged.
!
!     3)  If the string represents a number that is smaller than
!         the minimum representable integer, an appropriate error
!         message will be set, and the routine will exit. The value
!         of NUMBER will be unchanged.
!
!     4)  If the input string is blank, an appropriate error message
!         will be set, and the routine will exit. The value of NUMBER
!         will be unchanged.
!
!     5)  If the error message string is not long enough to contain
!         the entire error message, the error message will be
!         truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine will convert a signed hexadecimal character string
!     representation of an integer into its equivalent integer. This
!     provides a machine independent mechanism for storing or porting
!     integer values. This routine is used by the routine HX2DP which
!     converts a character string representation of a double precision
!     into its equivalent double precision value.
!
!     This routine is one of a pair of routines which are used to
!     perform conversions between integers and equivalent signed
!     hexadecimal character strings:
!
!           INT2HX -- Convert an integer into a signed hexadecimal
!                     character string.
!
!           HX2INT -- Convert a signed hexadecimal character string
!                     into an integer.
!
! $ Examples
!
!     All of the values shown are for a two's complement 32 bit
!     representation for signed integers.
!
!     The following argument values illustrate the action of HX2INT for
!     various input values.
!
!         STRING                 NUMBER        ERROR   ERRMSG
!         ---------------------  ------------  ------  ------
!          '1'                    1            .FALSE.   ' '
!          '-1'                  -1            .FALSE.   ' '
!          'DF'                   223          .FALSE.   ' '
!          'Df'                   223          .FALSE.   ' '
!          '+3ABC'                15036        .FALSE.   ' '
!          'ff'                   255          .FALSE.   ' '
!          '-20'                 -32           .FALSE.   ' '
!          '0'                    0            .FALSE.   ' '
!
!          '7FFFFFFF'             2147483647   .FALSE.   ' '
!          (Maximum 32 bit integer)
!
!          '-7FFFFFFF'           -2147483647   .FALSE.   ' '
!          (Minimum 32 bit integer + 1)
!
!          '-80000000'           -2147483648   .FALSE.   ' '
!          (Minimum 32 bit integer)
!
!          STRING = ' '
!          NUMBER = ( Not defined )
!          ERROR  = .TRUE.
!          ERRMSG = 'ERROR: A blank input string is not allowed.'
!
!          STRING = '-AB238Q'
!          NUMBER = ( Not defined )
!          ERROR  = .TRUE.
!          ERRMSG = 'ERROR: Illegal character ''Q'' encountered.'
!
!          STRING = '- AAA'
!          NUMBER = ( Not defined )
!          ERROR  = .TRUE.
!          ERRMSG = 'ERROR: Illegal character '' '' encountered.'
!
!          STRING = '80000000'
!          NUMBER = ( Not defined )
!          ERROR  = .TRUE.
!          ERRMSG = 'ERROR: Integer too large to be represented.'
!
!          STRING = '-800F0000'
!          NUMBER = ( Not defined )
!          ERROR  = .TRUE.
!          ERRMSG = 'ERROR: Integer too small to be represented.'
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
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 20-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 10-MAR-1994 (KRG)
!
!        Changed an IF test operand from .LE. to .LT. so that
!        the ELSE IF clause could be reached. This change has
!        NO effect on the execution of the routine because it
!        makes use of a base that is a power of 2 (16), so the
!        ELSE IF clause never needs to be reached. The algorithm
!        was meant to be as general as possible, however, so that
!        only the base and digits would need to be changed in order to
!        implement a different number base.
!
!-    SPICELIB Version 1.0.0, 22-OCT-1992 (KRG)
!
!-&


! $ Index_Entries
!
!     convert signed hexadecimal string to integer
!
!-&


! $ Revisions
!
!-     SPICELIB Version 1.1.0, 10-MAR-1994 (KRG)
!
!         Changed an IF test operand from .LE. to .LT. so that
!         the ELSE IF clause could be reached. This change has
!         NO effect on the execution of the routine because it
!         makes use of a base that is a power of 2 (16), so the
!         ELSE IF clause never needs to be reached. The algorithm
!         was meant to be as general as possible, however, so that
!         only the base and digits would need to be changed in order to
!         implement a different number base.
!
!         Old code was:
!
!            IF ( TMPNUM .LE. MAXI ) THEN
!
!               TMPNUM = TMPNUM * BASE + IDIGIT
!               POS    = POS + 1
!
!            ELSE IF ( ( TMPNUM .EQ. MAXI   ) .AND.
!     .                ( IDIGIT .LE. MAXMOD ) ) THEN
!
!               TMPNUM = TMPNUM * BASE + IDIGIT
!               POS    = POS + 1
!
!            ELSE ...
!
!         New code:
!
!            IF ( TMPNUM .LT. MAXI ) THEN
!
!               TMPNUM = TMPNUM * BASE + IDIGIT
!               POS    = POS + 1
!
!            ELSE IF ( ( TMPNUM .EQ. MAXI   ) .AND.
!     .                ( IDIGIT .LE. MAXMOD ) ) THEN
!
!               TMPNUM = TMPNUM * BASE + IDIGIT
!               POS    = POS + 1
!
!            ELSE ...
!
!-&


!
!     SPICELIB functions
!
      INTEGER               INTMAX
      INTEGER               INTMIN
!
!     Local Parameters
!
      INTEGER               BASE
      PARAMETER           ( BASE = 16  )
!
!     Local variables
!
      INTEGER               DIGBEG
      INTEGER               DIGEND
      INTEGER               IDIGIT
      INTEGER               IMINUS
      INTEGER               IPLUS
      INTEGER               ISPACE
      INTEGER               LCCBEG
      INTEGER               LCCEND
      INTEGER               LETTER
      INTEGER               MAXI
      INTEGER               MAXMOD
      INTEGER               MINI
      INTEGER               MINMOD
      INTEGER               POS
      INTEGER               STRBEG
      INTEGER               STREND
      INTEGER               TMPNUM
      INTEGER               UCCBEG
      INTEGER               UCCEND

      LOGICAL               FIRST
      LOGICAL               MORE
      LOGICAL               NEGTIV
!
!     Saved variables
!
      SAVE                  DIGBEG
      SAVE                  DIGEND
      SAVE                  LCCBEG
      SAVE                  LCCEND
      SAVE                  IMINUS
      SAVE                  IPLUS
      SAVE                  ISPACE
      SAVE                  MAXI
      SAVE                  MAXMOD
      SAVE                  MINI
      SAVE                  MINMOD
      SAVE                  UCCBEG
      SAVE                  UCCEND

      SAVE                  FIRST
!
!     Initial values
!
      DATA                  FIRST   / .TRUE. /


!
!     The input hexadecimal string is scanned from left to right, and
!     the integer is generated by repeated multiplications and additions
!     or subtractions.
!
!     If this is the first time that this routine has been called,
!     we need to do some setup stuff.
!
      IF ( FIRST ) THEN

         FIRST = .FALSE.
!
!        Initialize the upper and lower bounds for the decimal digits,
!        the upper and lower bounds for the uppercase hexadecimal
!        digits, the upper and lower bounds for the lowercase
!        hexadecimal digits, the space, the plus sign, and the minus
!        sign in the character sequence.
!
         DIGBEG = ICHAR( '0' )
         DIGEND = ICHAR( '9' )
         UCCBEG = ICHAR( 'A' )
         UCCEND = ICHAR( 'F' )
         LCCBEG = ICHAR( 'a' )
         LCCEND = ICHAR( 'f' )
         IMINUS = ICHAR( '-' )
         IPLUS  = ICHAR( '+' )
         ISPACE = ICHAR( ' ' )
!
!        Initialize some boundary values for error checking while
!        constructing the desired integer. These are used to help
!        determine integer overflow or integer underflow errors.
!
         MINI   = INTMIN() / BASE
         MINMOD = BASE * MINI - INTMIN()
         MAXI   = INTMAX() / BASE
         MAXMOD = INTMAX() - BASE * MAXI

      END IF
!
!     There are no errors initially, so set the error flag to
!     .FALSE.
!
      ERROR = .FALSE.
!
!     If the string is blank, set the error flag and return immediately.
!
      IF ( STRING .EQ. ' ' ) THEN

         ERROR  = .TRUE.
         ERRMSG = 'ERROR: A blank input string is not allowed.'
         RETURN

      END IF
!
!     Initialize a few other things.
!
      ERRMSG = ' '
      TMPNUM = 0
!
!     Assume that the number is nonnegative.
!
      NEGTIV = .FALSE.
!
!     Skip any leading white space. We know that there is at least
!     one nonblank character at this point, so we will not loop
!     off the end of the string.
!
      STRBEG = 1

      DO WHILE ( ICHAR( STRING(STRBEG:STRBEG) ) .EQ. ISPACE )

         STRBEG = STRBEG + 1

      END DO
!
!     Now, we want to find the end of the significant portion of
!     the input string.
!
      STREND = STRBEG + 1
      MORE   = .TRUE.

      DO WHILE ( MORE )

         IF ( STREND .LE. LEN( STRING ) ) THEN

            IF ( STRING(STREND:) .NE. ' ' ) THEN

               STREND = STREND + 1

            ELSE

               MORE = .FALSE.

            END IF

         ELSE

            MORE = .FALSE.

         END IF

      END DO
!
!     At this point, STREND is one larger than the length of the
!     significant portion of the string because we incremented
!     its value after the test. We will subtract one from the
!     value of STREND so that it exactly represents the position
!     of the last significant character in the string.
!
      STREND = STREND - 1
!
!     Set the position pointer to the beginning of the significant
!     part, i.e., the nonblank part, of the string, because we are
!     now ready to try and parse the number.
!
      POS = STRBEG
!
!     The first character should be a plus sign, '+', a minus sign,
!     '-', or a digit, '0' - '9', 'A' - 'F', or 'a' - 'f'. Anything
!     else is bogus, and we will catch it in the main loop below.
!
!     If the character is a minus sign, we want to set the value of
!     NEGTIV to .TRUE. and increment the position.
!
!     If the character is a plus sign, we want to increment the
!     position.
!
      IF ( ICHAR( STRING(POS:POS) ) .EQ. IMINUS ) THEN

         NEGTIV = .TRUE.
         POS    = POS + 1

      ELSE IF ( ICHAR( STRING(POS:POS) ) .EQ. IPLUS ) THEN

         POS = POS + 1

      END IF
!
!     When we build up the number from the hexadecimal string we
!     need to treat nonnegative numbers differently from negative
!     numbers. This is because on many computers the minimum
!     integer is one less than the negation of the maximum integer.
!     Negative numbers are the ones which truly might cause
!     problems, because ABS(minimum integer) may equal ABS(maximum
!     integer) + 1, on some machines. For example, on many machines
!     with 32 bit numbers, INTMIN = -2147483648 and INTMAX =
!     2147483647.
!
!     Build up the number from the hexadecimal character string.
!
      IF ( NEGTIV ) THEN

         DO WHILE ( POS .LE. STREND )

            LETTER = ICHAR( STRING(POS:POS) )

            IF ( ( LETTER .GE. DIGBEG ) .AND. &
                 ( LETTER .LE. DIGEND ) ) THEN

               IDIGIT = LETTER - DIGBEG

            ELSE IF ( ( LETTER .GE. UCCBEG ) .AND. &
                      ( LETTER .LE. UCCEND ) ) THEN

               IDIGIT = 10 + LETTER - UCCBEG

            ELSE IF ( ( LETTER .GE. LCCBEG ) .AND. &
                      ( LETTER .LE. LCCEND ) ) THEN

               IDIGIT = 10 + LETTER - LCCBEG

            ELSE

               ERROR  = .TRUE.
               ERRMSG = 'ERROR: Illegal character ''#'''  // &
                        ' encountered.'
               CALL REPMC( ERRMSG, '#', CHAR( LETTER ), ERRMSG )
               RETURN

            END IF

            IF ( TMPNUM .GT. MINI ) THEN

               TMPNUM = TMPNUM * BASE - IDIGIT
               POS    = POS + 1

            ELSE IF ( ( TMPNUM .EQ. MINI   ) .AND. &
                      ( IDIGIT .LE. MINMOD ) ) THEN

               TMPNUM = TMPNUM * BASE - IDIGIT
               POS    = POS + 1

            ELSE

               ERROR  = .TRUE.
               ERRMSG = 'ERROR: Integer too small'  // &
                        ' to be represented.'
               RETURN

            END IF

         END DO

      ELSE

         DO WHILE ( POS .LE. STREND )

            LETTER = ICHAR( STRING(POS:POS) )

            IF ( ( LETTER .GE. DIGBEG ) .AND. &
                 ( LETTER .LE. DIGEND ) ) THEN

               IDIGIT = LETTER - DIGBEG

            ELSE IF ( ( LETTER .GE. UCCBEG ) .AND. &
                      ( LETTER .LE. UCCEND ) ) THEN

               IDIGIT = 10 + LETTER - UCCBEG

            ELSE IF ( ( LETTER .GE. LCCBEG) .AND. &
                      ( LETTER .LE. LCCEND) ) THEN

               IDIGIT = 10 + LETTER - LCCBEG

            ELSE

               ERROR  = .TRUE.
               ERRMSG = 'ERROR: Illegal character ''#'''  // &
                        ' encountered.'
               CALL REPMC( ERRMSG, '#', CHAR( LETTER ), ERRMSG )
               RETURN

            END IF

            IF ( TMPNUM .LT. MAXI ) THEN

               TMPNUM = TMPNUM * BASE + IDIGIT
               POS    = POS + 1

            ELSE IF ( ( TMPNUM .EQ. MAXI   ) .AND. &
                      ( IDIGIT .LE. MAXMOD ) ) THEN

               TMPNUM = TMPNUM * BASE + IDIGIT
               POS    = POS + 1

            ELSE

               ERROR  = .TRUE.
               ERRMSG = 'ERROR: Integer too large'  // &
                        ' to be represented.'
               RETURN

            END IF

         END DO

      END IF
!
!     If we got to here, we have successfully parsed the hexadecimal
!     string into an integer. Set the value and return.
!
      NUMBER = TMPNUM

      RETURN
      END
