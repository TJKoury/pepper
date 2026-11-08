!$Procedure       ZZVSTRNG ( Virtual String )

      SUBROUTINE ZZVSTRNG( X, FILL, FROM, TO, RND, EXPONT, SUBSTR, DID )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Maintain a virtual decimal string associated with a d.p. number X.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!      ALPHANUMERIC, PRIVATE
!
! $ Declarations

      IMPLICIT NONE
      DOUBLE PRECISION      X
      CHARACTER*(1)         FILL
      INTEGER               FROM
      INTEGER               TO
      LOGICAL               RND
      INTEGER               EXPONT
      CHARACTER*(*)         SUBSTR
      LOGICAL               DID

! $ Brief_I/O
!
!     VARIABLE  I/O  Entry
!     --------  ---  --------------------------------------------------
!     X          I   ZZVSTSTR
!     FILL       I   ZZVSTSTR
!     FROM       I   ZZVSBSTR
!     TO         I   ZZVSBSTR
!     EXPONT     O   ZZVSTSTR
!     SUBSTR     O   ZZVSBSTR
!
! $ Detailed_Input
!
!     X          is a double precision number for which we want to
!                create a virtual decimal string.  This is supplied
!                to the routine ZZVSTSTR which sets up the internal
!                representation of the virtual decimal string.
!
!                X is assumed to be positive.
!
!     FILL       is the character to use for digits that precede the
!                first significant digit in the virtual decimal string.
!                Usually this will be a blank or zero ('0')
!
!     FROM       is the index in the virtual decimal string of the
!                first character that will be returned by ZZVSBSTR.
!
!     TO         is the index in the virtual decimal string of the
!                last character that will be returned by ZZVSBSTR.
!
!     RND        is a logical flag used to indicate that the output
!                string should represent the virtual decimal string
!                that results from rounding to the TO'th decimal
!                location.
!
! $ Detailed_Output
!
!     EXPONT     is the exponent associated with X when represented
!                in scientific notation.  It is returned by ZZVSTSTR.
!
!     SUBSTR     is the substring of the virtual decimal string from
!                index FROM to TO returned by ZZVSBSTR
!
!     DID        is a logical flag that is used to indicate that
!                the left most character returned by ZZVSBSTR became
!                a zero as a result of rounding up from 9. (i.e. there
!                are significant digits to the left of the first
!                character returned in SUBSTR.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     Given a character representation of a number such as
!     '1.234567890123E+3' there is a corresponding "infinite"
!     representation.  In this case
!
!           ...0000001234.56789012300000....
!
!     If we let the "index" of the decimal point be zero and number
!     the other characters from left to right in sequence we can
!     speak of the J'th character in the infinite representation.
!
!     We call the combination of the infinite representation and
!     indexing scheme  the virtual decimal string associated with the
!     input string.
!
!     The internal representation of the virtual decimal string is
!     set using the entry point ZZVSTSTR.  This entry point returns
!     the exponent associated with the string when it is written
!     in scientific notation.
!
!     For any J the entry point ZZVSBSTR returns the J'th character
!     of the virtual decimal string.
!
!     You may request that ZZVSBSTR return a string that is rounded
!     to the right most digit returned.  If return to the example
!     above
!
!           ...0000001234.56789012300000....
!
!     and the substring from -5 to 3 is requested with rounding,
!     the virtual decimal string will be treated as virtual string
!     rounded to the 3rd decimal point.
!
!           ...0000001234.56800000000000....
!
!     As a special convenience, you may specify any character to
!     be used in place of the extra leading zeros in the representation.
!     This leading character is specified via the input FILL in
!     ZZVSTSTR.
!
! $ Examples
!
!     Suppose you would like to create an output string associated
!     with X and you would like to present it in decimal format.
!
!     Moreover, suppose you know that X is positive and less than
!     100000.  The following would create the string and set the
!     leading character to be a blank.
!
!        CALL ZZVSTSTR ( X, ' ', EXP )
!
!     Check the exponent returned.  If it's greater than 5, our basic
!     assumptions were violated.
!
!        IF ( EXP .GT. 5 ) THEN
!           WRITE (*,*) 'The exponent is too big. It is: ', EXP
!        END IF
!
!     Now fill in the string.
!
!        CALL ZZVSBSTR ( -6, 5, RND, SUBSTR, DID )
!
!        WRITE (*,*) 'The value of X was: ', SUBSTR
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 12-SEP-1996 (WLT)
!
!-&


!
!     Local Variables
!


      INTEGER               MAXSIG
      PARAMETER           ( MAXSIG = 14 )

      INTEGER               ESGN
      PARAMETER           ( ESGN   = MAXSIG + 4 )

      INTEGER               EFST
      PARAMETER           ( EFST   = ESGN + 1 )

      INTEGER               ESCD
      PARAMETER           ( ESCD   = EFST + 1 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = MAXSIG + 16 )

      LOGICAL               YES
      PARAMETER           ( YES = .TRUE. )

      LOGICAL               NO
      PARAMETER           ( NO  = .FALSE. )



      CHARACTER*(WDSIZE)    STRING
      CHARACTER*(1)         MYFILL
      CHARACTER*(1)         LETTER

      INTEGER               BLANK
      INTEGER               CODE
      INTEGER               CODE0
      INTEGER               EXP
      INTEGER               I
      INTEGER               J
      INTEGER               LSUB
      INTEGER               SLOT
      INTEGER               VALUE

      LOGICAL               MINUS
      LOGICAL               INCR

      SAVE
!
!     Although we don't anticipate ever needing these values
!     we set some initial values for EXP and STRING.
!
      DATA                  STRING / ' 0.0000000000000E+00 '/
      DATA                  EXP    /   0   /
      DATA                  MYFILL / ' '   /
!
!     This routine doesn't do anything.
!
      RETURN


!$Procedure      ZZVSTSTR ( Set Virtual String)

      ENTRY ZZVSTSTR ( X, FILL, EXPONT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Set up the virtual string associated with X and return the
!     exponent associated with X when represented in scientific
!     notation.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ALPHANUMERIC, PRIVATE
!
! $ Declarations
!
!     IMPLICIT NONE
!     DOUBLE PRECISION      X
!     CHARACTER*(1)         FILL
!     INTEGER               EXPONT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   double precision number to needing a virtual string
!     FILL       I   leading character for virtual string.
!     EXPONT     O   The exponent associated with X.
!
!     The function returns the exponent associated with X.
!
! $ Detailed_Input
!
!     X          is a double precision number that from which
!                a virtual decimal string should be created.
!
!     FILL       is the character to use for the leading character
!                in the virtual decimal string.
!
! $ Detailed_Output
!
!     EXPONT     is the value of the scientific notation
!                exponent associated with X.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This entry point is used to establish a virtual decimal string.
!     The companion entry point ZZVSBSTR is used to retrieve the
!     characters in the virtual string.
!
! $ Examples
!
!     See the main entry point or the routine DPFMT.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 12-SEP-1996 (WLT)
!
!
!-&

      MYFILL = FILL

      CALL DPSTR ( X, MAXSIG, STRING )

!
!     Parse the exponent, string looks like the pattern presented
!     below:
!
!                    MAXSIG + 2
!                    |
!                    v
!     by.xxxxxxxxxxxxxEsxxx
!     1234567890123456789
!                      ^^
!                      ||
!                      |EFST = ESGN + 1
!                      |
!                      ESGN = MAXSIG + 4
!

      CODE0 = ICHAR ( '0' )
      BLANK = ICHAR ( ' ' )


      MINUS = ICHAR( STRING(ESGN:ESGN) ).EQ. ICHAR('-')
      CODE  = ICHAR( STRING(EFST:EFST) )
      EXP   = CODE - CODE0

      I     = ESCD
      CODE  = ICHAR( STRING(I:I) )

      DO WHILE ( CODE .NE. BLANK )

         EXP  = EXP * 10 + ( CODE - CODE0 )
         I    = I + 1
         CODE = ICHAR( STRING(I:I) )

      END DO

      IF ( MINUS ) THEN
         EXP = -EXP
      END IF

      EXPONT = EXP

      RETURN


!$Procedure      ZZVSBSTR ( Virtual String Character )

      ENTRY  ZZVSBSTR ( FROM, TO, RND, SUBSTR, DID )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return the character from the specified SLOT of a virtual
!     decimal string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ALPHANUMERIC, PRIVATE
!
! $ Declarations
!
!     IMPLICIT NONE
!     INTEGER               FROM
!     INTEGER               TO
!     LOGICAL               RND
!     CHARACTER*(*)         SUBSTR
!     LOGICAL               DID
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     FROM       I   the index of the first character to retrieve
!     TO         I   the index of the last character to retrieve
!     RND        I   treat the virtual string as rounded string.
!     SUBSTR     O   Contents of virtual string from FROM to TO.
!     DID        O   is a leading zero a result of rounding.
!
! $ Detailed_Input
!
!     FROM       is the index in the virtual decimal string of the
!                first character that will be returned in SUBSTR.
!
!     TO         is the index in the virtual decimal string of the
!                last character that will be returned in SUBSTR.
!
!     RND        is a logical flag used to indicate that the output
!                string should represent the virtual decimal string
!                that results from rounding to the TO'th decimal
!                location.
!
!
! $ Detailed_Output
!
!     SUBSTR     is we regard the virtual string as VIRTUL.  Then
!                in FORTRAN notation SUBSTR = VIRTUL(FROM:TO)
!
!     DID        is a logical flag that is used to indicate that
!                the left most character returned by ZZVSBSTR became
!                a zero as a result of rounding up from 9.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This entry point retrieves a specified character from the
!     virtual decimal string that was established by the last
!     call to the entry point ZZVSTSTR.
!
! $ Examples
!
!     See the main entry point or the routine DPFMT.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 12-SEP-1996 (WLT)
!
!
!-&


!
!     The buffered numeric string has the form:
!
!       by.xxxxxxxxxxxxxEseee...
!       123456789012345678901234
!                1         2
!
!     Ignoring the exponent we can regard this as being the
!     decimal equivalent of the number with the decimal point
!     in the wrong position.  We'll need to remedy this.
!
!       by.xxxxxxxxxxxxx
!       1234567890123456
!                1
!
!      We can think of this decimal representation as being a
!      simplification of the "infinite string" representation
!      below.
!
!                     b    y   .   x   x       x
!        d-4 d-3 d-2 d-1  d00  p  d01 d02 ... d13  0   0   0   0
!        -2  -1   0   1    2   3   4   5       16
!
!
!     From this its clear that i'th digit can be easily computed
!     via following decision block.
!
!
!        if ( i .lt. 0 ) then
!           digit  = '0'
!        else if ( i .eq. 0 ) then
!           digit  = string(2:2)
!        else if ( i .lt. maxsig ) then
!           digit = string(i+3:i+3)
!        else
!           digit = '0'
!        end if
!
!     To have an accurate representation of the number (one that
!     accounts for the exponent) we shift the decimal point ('p')
!     "right" by EXP slots. (If EXP is negative we shift right a
!     negative number of slots).  In the sequence of characters the
!     decimal point will follow d_EXP.
!
!     IF we renumber the slots so that the decimal point is in
!     slot 0 then for S < 0 slot S contains digit d_EXP+1+S
!
!     For S > 0 slot S contains digit d_EXP+S
!
!     Combining these observations we can compute the SLOT'th character
!     of the virtual string as follows.
!
!
!     If the character requested is character zero of the virtual
!     string, we just get the decimal point.
!
!     If the character requested is in a slot whose index is
!     greater than zero it is to the
!     right of the decimal point so it must be D_exp+slot.
!
!     If the character requested is in a slot whose index is negative
!     it is to the left of the decimal point.  Since the slot
!     just to the left of the decimal point contains D_exp it follows
!     by induction that for any negative slot, the decimal is
!     D_exp+slot+1
!
!
!     Since we may need to round the output, we will work from right
!     to left.  First thing we do is get the index of the right most
!     significant portion of SUBSTR that we will manipulate.
!
      J    = TO - FROM + 1
      LSUB = LEN(SUBSTR)
!
!     Blank pad to the right of J (if there's anything to pad).
!
      IF ( J .LT. LSUB ) THEN
         SUBSTR(J+1:) = ' '
      END IF
!
!     If we need to round the output string, locate the first numeric
!     slot after TO.
!
      IF ( RND ) THEN

         SLOT = TO + 1
!
!        If this points to the decimal point, move one more to the
!        right.
!
         IF ( SLOT .EQ. 0 ) THEN
            SLOT = SLOT + 1
         END IF
!
!        Determine which digit D_i corresponds to SLOT.
!
         IF ( SLOT .LT. 0 ) THEN
            I = EXP + SLOT + 1
         ELSE
            I = EXP + SLOT
         END IF
!
!        We will need to round in D_i is 5 or more.
!
         IF ( I .LT. 0 ) THEN
            LETTER = '0'
         ELSE IF ( I .EQ. 0 ) THEN
            LETTER = STRING(2:2)
         ELSE IF ( I .LT. MAXSIG ) THEN
            LETTER = STRING(3+I:3+I)
         ELSE
            LETTER = '0'
         END IF

         INCR = LGE ( LETTER, '5' )

      ELSE
         INCR = NO
      END IF
!
!     Starting at the right most slot, we work left incrementing
!     digits as required.  Note that once we don't round up
!     some value, we are done incrementing.
!
      DO SLOT = TO, FROM, -1

         IF ( SLOT .EQ. 0 ) THEN

            LETTER = '.'

         ELSE

!
!           Otherwise we need to first see which digit, d_I, is being
!           requested.
!
            IF ( SLOT .LT. 0 ) THEN
               I = EXP + SLOT + 1
            ELSE
               I = EXP + SLOT
            END IF

!
!           Now just look up d_I according to the rule we established
!           earlier.
!
            IF ( I .LT. 0 ) THEN

!
!              If the SLOT is prior to the first significant character
!              or the virtual string, we use the fill character.
!              Otherwise we use a zero.
!

               IF ( INCR ) THEN
                  LETTER = '1'
                  INCR   = NO
               ELSE

                  IF ( SLOT .LT. -1 ) THEN
                     LETTER = MYFILL
                  ELSE
                     LETTER = '0'
                  END IF

               END IF

            ELSE IF ( I .EQ. 0 ) THEN


               LETTER = STRING(2:2)
!
!              If necessary, increment LETTER.
!
               IF ( INCR ) THEN

                  VALUE = ICHAR(LETTER) - CODE0 + 1
!
!                 If value is 10 or more we will need to
!                 increment the next character too.  If VALUE
!                 is less than 10, we are done incrementing set
!                 INCR to NO.
!
                  IF ( VALUE .EQ. 10 ) THEN
                     LETTER = '0'
                  ELSE
                     LETTER = CHAR ( VALUE + CODE0 )
                     INCR   = NO
                  END IF

               END IF


            ELSE IF ( I .LT. MAXSIG ) THEN
!
!              This case is virtually identical to the previous
!              case, except that we need to pick off a different
!              letter from STRING.
!
               LETTER = STRING(I+3:I+3)

               IF ( INCR ) THEN

                  VALUE = ICHAR(LETTER) - CODE0 + 1

                  IF ( VALUE .EQ. 10 ) THEN
                     LETTER = '0'
                  ELSE
                     LETTER = CHAR ( VALUE + CODE0 )
                     INCR   = NO
                  END IF
               END IF

            ELSE

               LETTER = '0'
               INCR   = NO

            END IF

         END IF

         IF ( J .LE. LSUB ) THEN
            SUBSTR(J:J) = LETTER
         END IF

         J = J - 1

      END DO

      DID = INCR

      RETURN
      END
