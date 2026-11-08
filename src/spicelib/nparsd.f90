!$Procedure NPARSD ( Double Precision parsing of a string )

      SUBROUTINE NPARSD ( STRING, X, ERROR, PTR )

! $ Abstract
!
!     Parse a character string that represents a number and return
!     a double precision value.
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
!     PARSING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    STRING
      DOUBLE PRECISION X
      CHARACTER*(*)    ERROR
      INTEGER          PTR

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  ---------------------------------------------------
!     STRING     I   Character string representing a numeric value.
!     X          O   Double precision value parsed from STRING.
!     ERROR      O   Message indicating whether errors have occurred.
!     PTR        O   Position in string where an error occurred.
!
! $ Detailed_Input
!
!     STRING   is a character string that represents a numeric value.
!              Commas and spaces may be used in this string for
!              ease of reading and writing the number. They
!              are treated as insignificant but non-error-producing
!              characters.
!
!              For exponential representation the characters
!              'E','D','e','d' may be used.
!
!              The following are legitimate numeric expressions
!
!               +12.2 e-1
!               -3. 1415 9276
!               1e12
!               E10
!
!              The program also recognizes the following  mnemonics
!              'PI', 'pi', 'Pi', 'pI'
!              '+PI', '+pi', '+Pi', '+pI'
!              '-PI', '-pi', '-Pi', '-pI'
!              and returns the value
!              ( + OR - ) 3.1415 9265 3589 7932 3846 2600 D0 as
!              appropriate.
!
! $ Detailed_Output
!
!     X        double precision parsed value of input string. If an
!              error is encountered, X is not changed.
!
!     ERROR    is a message indicating that the string could
!              not be parsed due to use of an unexpected or misplaced
!              character or due to a string representing a number
!              too large for double precision. If the number was
!              successfully parsed, ERROR will be returned as a blank.
!
!              In particular, blank strings, or strings that do not
!              contain either a digit or exponent character will
!              be regarded as errors.
!
!     PTR      this indicates which character was being used when
!              the error occurred. If no error occurs, PTR is
!              returned as 0.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the string is non-numeric, PTR indicates the location in
!         the string where the error occurred, and ERROR contains a
!         descriptive error message.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine parses an input character string that represents a
!     number, checks for overflow, unexpected or misplaced
!     characters. It returns the double precision number or an error
!     message.
!
! $ Examples
!
!     Let   LINE = 'DELTA_T_A       =   32.184'
!
!     The following code fragment parses the line and obtains the
!     double precision value.
!
!
!        CALL NEXTWD ( LINE,  FIRST,  REST )
!        CALL NEXTWD ( REST, SECOND,  REST )
!        CALL NEXTWD ( REST,  THIRD,  REST )
!
!        CALL NPARSD (  THIRD,  VALUE, ERROR, PTR    )
!
! $ Restrictions
!
!     1)  Due to rounding errors this routine may not be able to parse
!         the decimal character string representation of the largest
!         and smallest double precision numbers.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.6.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 3.5.0, 15-AUG-2002 (WLT)
!
!        Replaced the call to INSSUB with a call to ZZINSSUB so
!        that this routine can legitimately call itself Error Free
!
!-    SPICELIB Version 3.4.0, 03-DEC-2001 (NJB)
!
!        Added an extra check to make sure that ICHAR of any character
!        of the input string is positive.
!
!-    SPICELIB Version 3.3.0, 29-FEB-1996 (KRG)
!
!        The declaration for the SPICELIB function PI is now
!        preceded by an EXTERNAL statement declaring PI to be an
!        external function. This removes a conflict with any
!        compilers that have a PI intrinsic function.
!
!        Removed the error message and storage for the unexpected
!        comma error message. This variable was set but never used,
!        and according to the spec for this routine a comma is a valid
!        delimiter, treated like a space, within numbers.
!
!-    SPICELIB Version 3.2.0, 10-JAN-1995 (WLT)
!
!        Changed error strings from parameters to assignments to
!        compensate for shortcomings of the Absoft FORTRAN compiler
!        on the NeXT.
!
!-    SPICELIB Version 3.1.0, 12-JUL-1994 (WLT)
!
!        The previous version of the routine assumed that the range
!        of values of ICHAR was 0 to 128. That turns out not to be
!        true on some machines. If a character whose ICHAR value is
!        outside this range is detected, it is now handled properly
!        as an unexpected character.
!
!-    SPICELIB Version 3.0.0, 24-FEB-1993 (WLT)
!
!        The previous version of the algorithm interpreted P or p as 1.
!        This was not the intent of the routine and was corrected.
!
!-    SPICELIB Version 2.0.0, 28-AUG-1992 (WLT) (KRG)
!
!        The basic algorithm was completely re-written. As a result
!        the routine now runs an order of magnitude faster than
!        it did before. In addition, strings that do not contain
!        enough information to assign a value to the string are now
!        regarded as errors. These include blank strings or strings
!        that contain only a sign characters, blanks and commas.
!
!        In addition the error diagnosis and checking for overflow
!        was greatly enhanced.
!
!        Note: strings may now parse with slightly different values
!        from the previous version of NPARSD. The current
!        implementation is more accurate in converting strings to
!        double precision numbers.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 17-APR-1990 (WLT)
!
!        Bug fix. The subscript used to reference individual characters
!        of the input string could sometimes step out of bounds. This
!        went unnoticed until NAIF began compiling with the CHECK=BOUNDS
!        option of the DEC Fortran compiler.
!
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (HAN) (NJB)
!
!-&


! $ Index_Entries
!
!     parse a character_string to a d.p. number
!
!-&


!
!     SPICELIB functions.
!

      DOUBLE PRECISION      DPMAX
      EXTERNAL              PI
      DOUBLE PRECISION      PI

!
!     Local Parameters.
!

      DOUBLE PRECISION      BASE
      PARAMETER           ( BASE   = 10.0D0 )

      INTEGER               DIGIT
      PARAMETER           ( DIGIT  = 1   )

      INTEGER               DPOINT
      PARAMETER           ( DPOINT = 2   )

      INTEGER               EXPONT
      PARAMETER           ( EXPONT = 3   )

      INTEGER               IGNORE
      PARAMETER           ( IGNORE = 4   )

      INTEGER               BPISTR
      PARAMETER           ( BPISTR = 5   )

      INTEGER               EPISTR
      PARAMETER           ( EPISTR = 6   )

      INTEGER               SIGN
      PARAMETER           ( SIGN   = 7   )

      INTEGER               IBASE
      PARAMETER           ( IBASE  = 10  )

      INTEGER               NKNOWN
      PARAMETER           ( NKNOWN = 128 )

      INTEGER               TEST
      PARAMETER           ( TEST = 16 )

      INTEGER               ERRSIZ
      PARAMETER           ( ERRSIZ = 160 )

      CHARACTER*(ERRSIZ)    BLNKST
      CHARACTER*(ERRSIZ)    TOOBIG
      CHARACTER*(ERRSIZ)    UNRCST
      CHARACTER*(ERRSIZ)    UNXPCH
      CHARACTER*(ERRSIZ)    UNXPPT
      CHARACTER*(ERRSIZ)    UNXPSN

      DOUBLE PRECISION      DECVAL
      DOUBLE PRECISION      DIVISR
      DOUBLE PRECISION      DPSIGN (     2     )
      DOUBLE PRECISION      ECOUNT
      DOUBLE PRECISION      EXPVAL
      DOUBLE PRECISION      FACTOR
      DOUBLE PRECISION      INTBND
      DOUBLE PRECISION      INTVAL
      DOUBLE PRECISION      LOOKUP ( 0 : IBASE )
      DOUBLE PRECISION      MAXEXP
      DOUBLE PRECISION      MINEXP
      DOUBLE PRECISION      NEXT
      DOUBLE PRECISION      SMLBND
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      VALUES (     NKNOWN )

      INTEGER               CLASS  ( 0 : NKNOWN )
      INTEGER               EXP
      INTEGER               I
      INTEGER               ID
      INTEGER               SIGNDX

      INTEGER               B
      INTEGER               BLANK
      INTEGER               L
      INTEGER               M
      INTEGER               NL
      INTEGER               NEXTI
      INTEGER               THISI


      LOGICAL               DODEC
      LOGICAL               EXPOK
      LOGICAL               DOEXP
      LOGICAL               FIRST
      LOGICAL               DOINT
      LOGICAL               MANTSA
      LOGICAL               BPIOK
      LOGICAL               EPIOK
      LOGICAL               PNTOK
      LOGICAL               ROUNDI
      LOGICAL               ROUNDD
      LOGICAL               SIGCHR
      LOGICAL               SIGNOK
      LOGICAL               ZEROI
!
!     Save everything.  It's easier than tracking down every
!     little variable that might need to be saved.
!
      SAVE

      DATA                  LOOKUP  /               1.0D0, &
                                                   10.0D0, &
                                                  100.0D0, &
                                                 1000.0D0, &
                                                10000.0D0, &
                                               100000.0D0, &
                                              1000000.0D0, &
                                             10000000.0D0, &
                                            100000000.0D0, &
                                           1000000000.0D0, &
                                          10000000000.0D0  /

      DATA                  FIRST  / .TRUE. /

      DATA                  VALUES  / NKNOWN * 0.0D0 /

      DATA                  CLASS   / 0, NKNOWN * 0 /


      IF ( FIRST ) THEN

         FIRST = .FALSE.
!
!        Set up the error messages
!
         TOOBIG = 'The number represented by the input string is ' &
         //       'too large to be stored as a double precision ' &
         //       'number. '


         UNXPCH = 'An unexpected character was found while ' &
         //       'attempting to parse the input string. '


         UNXPPT = 'An unexpected decimal point was found in the ' &
         //       'input string. '


         UNXPSN = 'An unexpected sign character was found in the ' &
         //       'input string. '


         BLNKST = 'The input string is blank. Blank strings are ' &
         //       'not considered to be numbers. '


         UNRCST = 'The input string could not be recognized as a ' &
         //       'number. '

         BLANK = ICHAR ( ' ' )

         VALUES( ICHAR ( '0' ) ) =  0.0D0
         VALUES( ICHAR ( '1' ) ) =  1.0D0
         VALUES( ICHAR ( '2' ) ) =  2.0D0
         VALUES( ICHAR ( '3' ) ) =  3.0D0
         VALUES( ICHAR ( '4' ) ) =  4.0D0
         VALUES( ICHAR ( '5' ) ) =  5.0D0
         VALUES( ICHAR ( '6' ) ) =  6.0D0
         VALUES( ICHAR ( '7' ) ) =  7.0D0
         VALUES( ICHAR ( '8' ) ) =  8.0D0
         VALUES( ICHAR ( '9' ) ) =  9.0D0
         VALUES( ICHAR ( '-' ) ) = -1.0D0
         VALUES( ICHAR ( '+' ) ) =  1.0D0

         CLASS( ICHAR(' ') ) = IGNORE
         CLASS( ICHAR(',') ) = IGNORE

         CLASS( ICHAR('.') ) = DPOINT

         CLASS( ICHAR('E') ) = EXPONT
         CLASS( ICHAR('D') ) = EXPONT
         CLASS( ICHAR('e') ) = EXPONT
         CLASS( ICHAR('d') ) = EXPONT

         CLASS( ICHAR('+') ) = SIGN
         CLASS( ICHAR('-') ) = SIGN

         CLASS( ICHAR('1') ) = DIGIT
         CLASS( ICHAR('2') ) = DIGIT
         CLASS( ICHAR('3') ) = DIGIT
         CLASS( ICHAR('4') ) = DIGIT
         CLASS( ICHAR('5') ) = DIGIT
         CLASS( ICHAR('6') ) = DIGIT
         CLASS( ICHAR('7') ) = DIGIT
         CLASS( ICHAR('8') ) = DIGIT
         CLASS( ICHAR('9') ) = DIGIT
         CLASS( ICHAR('0') ) = DIGIT

         CLASS( ICHAR('p') ) = BPISTR
         CLASS( ICHAR('P') ) = BPISTR
         CLASS( ICHAR('i') ) = EPISTR
         CLASS( ICHAR('I') ) = EPISTR
!
!        Finally create the numbers that will be used for checking
!        for floating point overflow.
!
!        NOTE: The value for MINEXP may be too small by one, but it
!              really doesn't make any difference, as you're going to
!              underflow anyway, and dividing zero by a number (BASE)
!              still gives you zero.
!
         MAXEXP = DINT ( DLOG10( DPMAX() ) )
         MINEXP =-( MAXEXP + 1 )
         SMLBND = DPMAX()/LOOKUP(IBASE)

         INTBND = BASE
         NEXT   = INTBND + 1.0D0

         DO WHILE ( INTBND .NE. NEXT )

            INTBND = INTBND *  BASE
            NEXT   = INTBND +  1.0D0

         END DO

         INTBND = INTBND / BASE
!
!        That takes care of the first pass initializations.
!
      END IF
!
!     Here's what's true right now.
!
!     There are no errors.
!     The error pointer doesn't need to point anywhere.
!     It's ok for the next token to be a decimal point.
!     It's ok for the next token to be a sign character.
!     It's ok for the next token to be an exponent marker.
!     It's ok for the next character to be the start of pi.
!
!     We expect to be constructing the integer part of the
!     numeric string.
!
      ERROR     = ' '
      PTR       = 0

      PNTOK     = .TRUE.
      SIGNOK    = .TRUE.
      EXPOK     = .TRUE.
      BPIOK     = .TRUE.
      DOINT     = .TRUE.
      ROUNDD    = .TRUE.
      ROUNDI    = .TRUE.
!
!     Here's some other facts.
!
!     We are not parsing the decimal part of the string.
!     We are not parsing the exponent part of the string.
!     We have not encountered any digits in the mantissa.
!     We have not encountered any significant characters.
!     It's not ok for the next character to be the end of pi (i).
!
      DODEC     = .FALSE.
      DOEXP     = .FALSE.
      MANTSA    = .FALSE.
      SIGCHR    = .FALSE.
      EPIOK     = .FALSE.
!
!     So far there is no integer, decimal or exponent part to this
!     string.
!
      INTVAL    = 0.0D0
      DECVAL    = 0.0D0
      EXPVAL    = 0.0D0
      DIVISR    = 1.0D0
      FACTOR    = 1.0D0
      ECOUNT    = 0.0D0
!
!     Right now if we encounter a sign, it's part of the mantissa.
!     And until we know better the sign of both the mantissa and
!     exponent are +1 (as opposed to -1).
!
      SIGNDX    = 1
      DPSIGN(1) = 1.0D0
      DPSIGN(2) = 1.0D0
!
!     Before doing anything else we determine whether or not
!     the input string is empty.
!
      IF ( STRING .EQ. ' ' ) THEN

         ERROR = BLNKST
         PTR   = 1
         RETURN

      END IF

!
!     We need to find the last non-blank character of the input
!     string.  We shall use the idea of binary searching to locate
!     this character.  At first this may appear to be a bit convoluted
!     when compared to the obvious thing to do (start at the end of
!     the string and step backward until a non-blank character is
!     located).  However, on every machine we've looked at this method
!     locates the last non-blank character much more quickly on average
!     than the obvious method.
!
!     L and B denote the last and beginning characters
!     of the substring we are searching.  NL is the next to last
!     character that we are concerned with and M is the middle of
!     the current search interval ( from B to NL ).
!
      L     =  LEN(STRING)
      B     =  1
      NL    =  L - 1

!
!     We want M to be ( B + NL ) / 2   but right now that's L/2
!
      M     =  L / 2

      DO WHILE ( L-B .GT. TEST )
!
!        What is true right now?  The string from L+1 on out
!        is blank.  L > B; L-1 = NL >= B;  M = (B + NL) / 2;
!        and M >= B,  B is at least one and if greater than 1
!        there must be a non-blank character between B and the
!        end of the string.
!
         IF ( ICHAR(STRING(L:L)) .NE. BLANK ) THEN

            B = L

         ELSE IF ( STRING(M:NL) .EQ. ' ' ) THEN

!
!           If you got here, the STRING(L:L) is a blank.
!           The string from L+1 on out is blank.
!           The string from M to NL (=L-1) is blank.  Thus the
!           string from M out is blank.
!
!           M is greater than or equal to B.
!           If M  is less than B + 2, then L will become
!           B or less and there will not be a
!           next pass through the loop.  That means that
!           we will never get to this point again and don't
!           have to worry about the reference STRING(M:NL)
!           giving us an access violation.
!
            L  = M - 1
!
!           With the new value of L, we now know that STRING(L+1:)
!           is blank.
!
         ELSE
!
!           If you get to this point all of the string from
!           L out is blank and L is greater than M.
!           There is a non-blank character between M and NL.
!           If L should get within 16 of B, then the loop
!           will not be executed again.  That means again that
!           we don't have to worry about STRING(M:NL) being
!           an ill formed string.
!
            L  =  NL
            B  =  M
!
!           With the new value of L, we now know that STRING(L+1:)
!           is blank.
!
         END IF


!
!        Finally compute NL,the index of the character that precedes
!        L and the new midpoint of the stuff from B to NL.
!
         NL =  L - 1
         M  = (B + NL) / 2

!
!        What's true now?  The string from L+1 on out is blank.
!
      END DO

!
!     L is now within 16 characters of the last non-blank character
!     of the input string.  We simply search backward from L to
!     locate this last non-blank.
!
      DO WHILE ( ICHAR(STRING(L:L)) .EQ. BLANK )
         L = L - 1
      END DO


!
!     Begin to collect the number in its various parts: an integer
!     portion, a fractional portion, and an exponent.
!
      DO I = 1, L



         ID    = ICHAR(STRING(I:I))

         IF ( ID .GT. NKNOWN .OR. ID .LT. 0 ) THEN

!
!           This is definitely not expected.  Set the error message
!           and return.
!
            NEXTI = I + 1
            THISI = I

            CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
            CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
            CALL PREFIX ( UNXPCH,  1,         ERROR )

            PTR   = I
            RETURN

!
!        The action taken depends upon the class of the token.
!
         ELSE IF ( CLASS(ID) .EQ. DIGIT ) THEN
!
!           Once a digit has been encountered, we can no longer
!           allow the string 'PI' or a sign until an exponent
!           character is hit and resets the SIGNOK flag.
!
            BPIOK  = .FALSE.
            EPIOK  = .FALSE.
            SIGNOK = .FALSE.
            SIGCHR = .TRUE.
!
!           If we are constructing the integer part ...
!
            IF ( DOINT  ) THEN

               MANTSA = .TRUE.
!
!              Check the current value of the integer part to
!              make sure we don't overflow.
!
               IF ( INTVAL .LT. INTBND ) THEN

                  INTVAL = INTVAL * BASE + VALUES(ID)

               ELSE
!
!                 Once the integer exceeds a given bound,
!                 we add the rest on as fractional part and
!                 keep track of the factor we will need to
!                 multiply the decimal part by to scale things
!                 appropriately.  We also keep track of the number
!                 we will need to add to the exponent part.
!
                  ECOUNT = ECOUNT + 1
                  FACTOR = FACTOR / BASE

                  IF ( ROUNDI ) THEN

                     ROUNDI = .FALSE.

                     IF ( VALUES(ID) .GT. 0.5D0*BASE ) THEN
                        INTVAL = INTVAL + 1.0D0
                     END IF

                  END IF

               END IF
!
!           ... or the decimal part ...
!
            ELSE IF ( DODEC ) THEN

               MANTSA = .TRUE.
!
!              There are two cases to consider.  The case in which
!              the integer portion of the string has value 0...
!
               IF ( ZEROI ) THEN
!
!                 We can just keep accumulating the decimal part
!                 as an integer.  But we keep track of how many
!                 places past the decimal point the first non-zero
!                 digit occurs.  Note that once the decimal part
!                 exceeds the integer bound, we don't need to do
!                 anything.  The remaining digits cannot contribute
!                 to the value of the decimal part.
!
                  IF  ( DECVAL .LT. INTBND ) THEN

                     DECVAL = DECVAL * BASE + VALUES(ID)
                     ECOUNT = ECOUNT - 1

                  ELSE IF ( ROUNDD ) THEN

                     ROUNDD = .FALSE.

                     IF ( VALUES(ID) .GE. 0.5D0*BASE ) THEN
                        DECVAL =  DECVAL + 1.0D0
                     END IF

                  END IF
!
!              ...and the case in which the integer portion is not
!              zero.
!
               ELSE
!
!                 In this case, we know there is at least _something_
!                 to the integer part of this string.  We can
!                 stop accumulating the decimal part when the divisor
!                 portion exceeds the integer barrier.  After that
!                 the extra digits can't make any contribution to
!                 the double precision value given to the string.
!
                  IF ( DIVISR .LT. INTBND ) THEN

                     DECVAL = DECVAL * BASE + VALUES(ID)
                     DIVISR = DIVISR * BASE

                  END IF

               END IF
!
!           ...or the exponent part of the string.
!
            ELSE IF ( DOEXP  ) THEN

               IF ( EXPVAL + ECOUNT .GT. MAXEXP ) THEN
!
!                 This number is too big to put into a double
!                 precision number. The marginal case where
!                 EXPVAL + ECOUNT .EQ. MAXEXP will be dealt
!                 with when the integer and fractional parts
!                 of the double precision number are built
!                 at the end of this routine.
!
                  ERROR = TOOBIG
                  PTR   = I
                  RETURN

               ELSE IF ( EXPVAL + ECOUNT .LT. MINEXP ) THEN
!
!                 This number is going to underflow, we can
!                 just stop accumulating exponent. But we don't
!                 stop parsing the string yet. There might be
!                 a bad character lurking somewhere later in the
!                 string.
!
!                 NOTE: It is also possible to underflow when the
!                       value of EXPVAL + ECOUNT is equal to MINEXP,
!                       since an entire 'BASE' scale is not supported
!                       for this particular exponent.
!
               ELSE
!
!                 This is the case we expect.  Just add on the
!                 next part of the exponent.
!
                  EXPVAL = EXPVAL*BASE + DPSIGN(2)*VALUES(ID)

               END IF
!
!           Even though this character is a digit, its not expected
!           for some reason.  Set the error flag and return.
!
            ELSE

               NEXTI = I + 1
               THISI = I

               CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
               CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
               CALL PREFIX ( UNXPCH,  1,         ERROR )

               PTR   = I
               RETURN

            END IF

         ELSE IF ( CLASS(ID) .EQ. DPOINT ) THEN

            IF ( PNTOK ) THEN

               BPIOK  = .FALSE.
               EPIOK  = .FALSE.
               PNTOK  = .FALSE.
               SIGNOK = .FALSE.
               DODEC  = .TRUE.
               DOINT  = .FALSE.
               DOEXP  = .FALSE.
               ZEROI  = INTVAL .EQ. 0.0D0

            ELSE

               NEXTI = I + 1
               THISI = I

               CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
               CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
               CALL PREFIX ( UNXPPT,  1,         ERROR )

               PTR    = I
               RETURN

            END IF

         ELSE IF ( CLASS(ID) .EQ. EXPONT ) THEN

            SIGCHR = .TRUE.

            IF ( EXPOK ) THEN

               BPIOK  = .FALSE.
               EPIOK  = .FALSE.
               EXPOK  = .FALSE.
               PNTOK  = .FALSE.
               DODEC  = .FALSE.
               DOINT  = .FALSE.
               DOEXP  = .TRUE.
               SIGNOK = .TRUE.
               SIGNDX =  2

            ELSE

               NEXTI = I + 1
               THISI = I

               CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
               CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
               CALL PREFIX ( UNXPCH,  1,         ERROR )

               PTR   =  I
               RETURN

            END IF

         ELSE IF ( CLASS(ID) .EQ. SIGN ) THEN

            IF ( SIGNOK ) THEN

               DPSIGN(SIGNDX) = VALUES(ID)
               SIGNOK         = .FALSE.

            ELSE

               NEXTI = I + 1
               THISI = I

               CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
               CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
               CALL PREFIX ( UNXPSN,  1,         ERROR )

               PTR   = I
               RETURN

            END IF

         ELSE IF ( CLASS(ID) .EQ. BPISTR ) THEN

            SIGCHR = .TRUE.

            IF ( BPIOK ) THEN

               DOINT  = .FALSE.
               DODEC  = .FALSE.
               DOEXP  = .FALSE.
               EXPOK  = .FALSE.
               PNTOK  = .FALSE.
               BPIOK  = .FALSE.
               SIGNOK = .FALSE.
               EPIOK  = .TRUE.

            ELSE

               NEXTI = I + 1
               THISI = I

               CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
               CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
               CALL PREFIX ( UNXPCH,  1,         ERROR )

               PTR   = I
               RETURN

            END IF

         ELSE IF ( CLASS(ID) .EQ. EPISTR ) THEN

            IF ( EPIOK ) THEN

               DOINT  = .FALSE.
               DODEC  = .FALSE.
               DOEXP  = .FALSE.
               EXPOK  = .FALSE.
               PNTOK  = .FALSE.
               BPIOK  = .FALSE.
               SIGNOK = .FALSE.
               EPIOK  = .FALSE.
               MANTSA = .TRUE.

               INTVAL = PI()

            ELSE

               NEXTI = I + 1
               THISI = I

               CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
               CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
               CALL PREFIX ( UNXPCH,  1,         ERROR )

               PTR   = I
               RETURN

            END IF

         ELSE IF ( CLASS(ID) .EQ. IGNORE ) THEN

!
!           We don't do anything.
!

         ELSE

!
!           This is definitely not expected.  Set the error message
!           and return.
!
            NEXTI = I + 1
            THISI = I

            CALL ZZINSSUB ( STRING, ']', NEXTI, ERROR )
            CALL ZZINSSUB ( ERROR,  '[', THISI, ERROR )
            CALL PREFIX ( UNXPCH,  1,         ERROR )

            PTR   = I
            RETURN

         END IF

      END DO


!
!     If we got through the loop and it's OK to end PI, then we started
!     it but never finished.  This is an error.
!
      IF ( EPIOK ) THEN
         ERROR = UNRCST
         PTR   = L
         RETURN
      END IF

!
!     Put together the portion that does not involve an exponent.
!
!     If
!        (1) MANTSA = .TRUE., then we had some explicit part of a
!            number, an  integer part, a fractional part, or both.
!
!        (2) SIGCHR = .TRUE, then we had either:
!
!            (a) MANTSA = .TRUE.
!
!         or
!
!            (b) there was an implicit value associated with the input
!                string. For example, an exponent character followed
!                by an optional exponent would produce a valid number:
!                E+10 --> 1.0d+10. This is due to the fact that this
!                routine emulates an RPN calculator of popular repute,
!                not because it is inherently a good idea.
!
      IF ( MANTSA ) THEN
!
!        We had an integer part of the number, a fractional part, or
!        both, so we need to put them together in an appropriate
!        fashion.
!
         VALUE = INTVAL + ( DECVAL / DIVISR ) * FACTOR

      ELSE IF ( SIGCHR ) THEN
!
!        We do not have a mantissa, so we had an  implicit mantissa,
!        see above, so we need to set the value to one.
!
         VALUE = 1.0D0

      ELSE
!
!        We have an error. There were no significant characters in the
!        input character string, and hence we could not parse it into
!        a number. An example of such a string would be: '+  ,,.,,'.
!        So, we will set an appropriate error message and return.
!
         ERROR = UNRCST
         PTR   = LEN(STRING) + 1
         RETURN

      END IF
!
!     Adjust the entered part of the exponent by the amount
!     we "shifted" the decimal point when we were computing
!     the integer and decimal values.
!
      EXPVAL = EXPVAL + ECOUNT
!
!     Now take care of the exponent contribution to the answer.
!
!     If the exponent is negative ...
!
      IF ( EXPVAL .LT. 0 ) THEN

         DO WHILE ( EXPVAL .LT. -BASE )

            VALUE  = VALUE  / LOOKUP(IBASE)
            EXPVAL = EXPVAL + BASE

         END DO

         VALUE  = VALUE / LOOKUP( -INT(EXPVAL) )
!
!     If the exponent is positive ...
!
      ELSE IF ( EXPVAL .GT. 0 ) THEN

         DO WHILE ( EXPVAL .GT. BASE )
!
!           Make sure that a multiply isn't going to create
!           a number that overflows.
!
            IF ( VALUE .GE. SMLBND ) THEN

               ERROR = TOOBIG
               PTR   = LEN(STRING)+1
               RETURN

            ELSE

               VALUE  = VALUE  * LOOKUP(IBASE)
               EXPVAL = EXPVAL - BASE

            END IF

         END DO



         EXP = NINT(EXPVAL)
!
!        Again, make sure that a floating point overflow isn't
!        going to happen.
!
         IF ( VALUE .LT. DPMAX()/LOOKUP(EXP) ) THEN

            VALUE = VALUE * LOOKUP(EXP)

         ELSE

               ERROR = TOOBIG
               PTR   = LEN(STRING)+1
               RETURN

         END IF

      END IF

      X = DPSIGN(1)*VALUE

      RETURN
      END
