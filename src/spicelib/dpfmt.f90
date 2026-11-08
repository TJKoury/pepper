!$Procedure DPFMT ( Format a double precision number )

      SUBROUTINE DPFMT ( X, PICTUR, STR )

! $ Abstract
!
!     Create a formatted string that represents a double precision
!     number, using a format picture.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ALPHANUMERIC
!     CONVERSION
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      X
      CHARACTER*(*)         PICTUR
      CHARACTER*(*)         STR

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   a double precision number.
!     PICTUR     I   a string describing the appearance of the output
!     STR        O   a string representing X as prescribed by PICTUR
!
! $ Detailed_Input
!
!     X        is any double precision number.
!
!     PICTUR   is a string used to describe the format of the
!              output string. There are four special characters
!              recognized by DPFMT --- a leading + or -, a leading
!              zero ( '0' ) or a zero that follows a leading + or -,
!              and the first decimal point of the string.
!
!              All other non-blank characters are regarded as
!              equivalent. The picture ends at the first blank
!              character. The effects associated with the various
!              characters in a picture are spelled out in the
!              description of the output STRING.
!
!              The following pictures are treated as errors.
!
!              ' ', '+', '-', '.', '+.', '-.'
!
! $ Detailed_Output
!
!     STRING   is a string representing X that matches the input
!              picture. The format of STRING is governed by PICTUR.
!              It will represent X rounded to the level of precision
!              specified by PICTUR.
!
!              If the first character of the picture is a minus sign,
!              the first character in the output string will be
!              a blank if the number is non-negative, a minus sign
!              if the number is negative.
!
!              If the first character of the picture is a plus sign,
!              the first character of the output string will be a
!              plus if the number is positive, a blank if the number
!              is zero, and a minus sign if the number is negative.
!
!              If the first character of the string is NOT a sign
!              (plus or minus) the first character of the output
!              string will be a minus sign if the number is negative
!              and will be the first character of the integer part
!              of the number otherwise.
!
!              The integer portion of STRING will contain the same
!              number of characters as appear before the decimal
!              point (or last character if there is no decimal
!              point) but after a leading + or -.
!
!              If the picture begins with any of the following
!
!                 '+0', '-0', or '0'
!
!              it is said to have a leading zero. If a picture has
!              a leading zero and the integer portion is not large
!              enough to fill up the integer space specified by
!              PICTUR, STRING will be zero padded from the sign (if
!              one is required) up to the first character of the
!              integer part of the number.
!
!              If picture does NOT have a leading zero and the
!              integer portion is not large enough to fill up the
!              space specified by PICTUR, STRING will be blank
!              padded on the left between the sign (if one is
!              required) and the first character of the integer part
!              of the number.
!
!              If a decimal point ( '.' ) is present in PICTUR it
!              will be present following the integer portion of
!              STRING. Moreover, the decimal portion of STRING will
!              contain the same number of digits as there are
!              non-blank characters following the decimal point in
!              PICTUR. However, only the first 14 digits starting
!              with the first non-zero digit are meaningful.
!
!              If the format specified by PICTUR does not provide
!              enough room for the integer portion of X, the routine
!              determines whether or not the number of characters
!              present in the picture is sufficient to create a
!              representation for X using scientific notation. If
!              so, the output is displayed using scientific notation
!              (leading signs, if they are present in PICTUR, will
!              also appear in STRING).   If the format specified by
!              PICTUR is too short to accommodate scientific
!              notation, the output string is filled with '*' to the
!              same length as the length of PICTUR. Leading signs
!              are not preserved in this overflow case.
!
!              STRING may overwrite PICTUR.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If PICTUR begins with a blank, the error SPICE(NOPICTURE) is
!         signaled.
!
!     2)  If PICTUR consists only of '+', '-', '.', '+.' or '-.' are
!         regarded as invalid (there's no significant component to the
!         picture.) therefore, the error SPICE(BADPICTURE) is signaled.
!
!     3)  If the length of STR is less than the length of the first
!         non-blank portion of PICTUR, the error SPICE(OUTPUTTOOSHORT)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides a mechanism for producing numeric strings
!     formatted according to a user supplied picture. We expect that
!     the string produced by this routine will be used to assist in
!     the construction of a string that can be read by people.
!
!     Note that the process of converting a double precision number
!     to a string, in not precisely invertible even if the string
!     contains all of the significant figures allowed by this
!     routine. You should not anticipate that the string produced
!     by this routine can be "read" into a double precision number
!     to reproduce the double precision number X. To the level of
!     accuracy implied by the string representation, they will be
!     the same. But, they are unlikely to have the same internal
!     binary representation.
!
! $ Examples
!
!     Suppose that X has the binary representation of PI. Then the
!     table below illustrates the strings that would be produced
!     by a variety of different pictures.
!
!     PICTUR         |    STRING
!     -------------------------------
!     '0x.xxx'       |  '03.142'
!     'xx.xxx'       |  ' 3.142'
!     '+xxx.yyyy'    |  '+  3.1416'
!     '-.yyyy'       |  '******'
!     'xxxxxxxx'     |  '       3'
!     '00xx'         |  '0003'
!     '-00.0000000'  |  ' 03.1415927'
!     '00'           |  '03'
!     'x.'           |  '3.'
!     '.mynumber'    |  '3.142E+00'
!     'my dog spot'  |  ' 3'
!     'my.dog spot'  |  ' 3.142'
!     '+my.dog,spot' |  '+ 3.14159265'
!
!
!
!     Suppose that X has the binary representation of 2/3. Then the
!     table below illustrates the strings that would be produced
!     by a variety of different pictures.
!
!     PICTUR         |    STRING
!     -------------------------------
!     '+x.xxx'       |  '+0.667'
!     '+xx.xxx'      |  '+ 0.667'
!     'xxx.yyyy'     |  '  0.6667'
!     '.yyyy'        |  '.6667'
!     'xxxxxxxx'     |  '       1'
!     '00xx'         |  '0001'
!     '-0.0000000'   |  ' 0.6666667'
!     '00'           |  '01'
!     'x.'           |  '1.'
!     'mynumber'     |  '       1'
!     'my dog spot'  |  ' 1'
!     'my.dog spot'  |  ' 0.667'
!     'my.dog,spot'  |  ' 0.66666667'
!
!     Suppose that X has the binary representation of -8/9. Then the
!     table below illustrates the strings that would be produced
!     by a variety of different pictures.
!
!
!     PICTUR         |    STRING
!     -------------------------------
!     '+x.xxx'       |  '-0.889'
!     '-00.xxxx'     |  '-00.8889'
!     'xxx.xxx'      |  ' -0.889'
!     '000.000'      |  '-00.889'
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.3, 27-OCT-2021 (JDR) (EDW) (NJB)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 31-JAN-2008 (BVS)
!
!        Removed non-standard end-of-declarations marker
!        'C%&END_DECLARATIONS' from comments.
!
!-    SPICELIB Version 1.0.1, 22-JUN-1998 (WLT)
!
!        A number of typographical and grammatical errors
!        were corrected in the header.
!
!-    SPICELIB Version 1.0.0, 17-SEP-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     format a string representing a d.p. number
!     string from a d.p. number and format picture
!
!-&


!
!     SPICELIB functions
!
      INTEGER               POS
      INTEGER               NCPOS


      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )


      CHARACTER*(1)         FILL
      CHARACTER*(1)         SIGN
      CHARACTER*(WDSIZE)    MYSTR

      DOUBLE PRECISION      Y

      INTEGER               DECLEN
      INTEGER               DPAT
      INTEGER               EXP
      INTEGER               EXPSIZ
      INTEGER               FRSTCH
      INTEGER               I
      INTEGER               INTLEN
      INTEGER               LASTCH
      INTEGER               FIRSTB
      INTEGER               SGNLEN
      INTEGER               SIGDIG
      INTEGER               SPRSIZ
      INTEGER               START


      LOGICAL               NEEDSN
      LOGICAL               OVFLOW
      LOGICAL               SHIFT


!
!     Initial values
!

!
!     Determine where the picture ends.
!
      FIRSTB  = POS( PICTUR, ' ',  1 )

      IF ( FIRSTB .EQ. 0 ) THEN
         LASTCH = LEN ( PICTUR )
      ELSE
         LASTCH = FIRSTB - 1
      END IF

!
!     Make sure there is a picture to worry about.
!
      IF ( LASTCH .EQ. 0 ) THEN

         CALL CHKIN  ( 'DPFMT'                                      )
         CALL SETMSG ( 'The format picture must begin with a '       // &
                       'non-blank character. The picture supplied '  // &
                       'was begun with a blank.'                    )
         CALL SIGERR ( 'SPICE(NOPICTURE)'                           )
         CALL CHKOUT ( 'DPFMT'                                      )
         RETURN

      ELSE IF ( LASTCH .EQ. 1 ) THEN

         IF (     PICTUR .EQ. '+' &
             .OR. PICTUR .EQ. '-' &
             .OR. PICTUR .EQ. '.' ) THEN

            CALL CHKIN ( 'DPFMT' )
            CALL SETMSG ( 'Format pictures must have at least ' &
            //            'one significant character. The ' &
            //            'picture provided ''#'' does not. ' )
            CALL ERRCH  ( '#', PICTUR(1:1)                    )
            CALL SIGERR ( 'SPICE(BADPICTURE)'                 )
            CALL CHKOUT ( 'DPFMT'                             )
            RETURN

         END IF

      ELSE IF ( LASTCH .EQ. 2 ) THEN

         IF (     PICTUR .EQ. '+.' &
             .OR. PICTUR .EQ. '-.' ) THEN

            CALL CHKIN ( 'DPFMT' )
            CALL SETMSG ( 'Format pictures must have at least ' &
            //            'one significant character. The ' &
            //            'picture provided ''#'' does not. ' )
            CALL ERRCH  ( '#', PICTUR(1:2)                    )
            CALL SIGERR ( 'SPICE(BADPICTURE)'                 )
            CALL CHKOUT ( 'DPFMT'                             )
            RETURN
         END IF

      ELSE IF ( LASTCH .GT. LEN(STR) ) THEN

         CALL CHKIN  ( 'DPFMT'                                      )
         CALL SETMSG ( 'The output string is not long enough to ' &
         //            'accommodate a number formatted according ' &
         //            'to the supplied format picture. The ' &
         //            'output string has length #. The output ' &
         //            'picture ''#'' requires # characters. ' )

         CALL ERRINT ( '#', LEN(STR) )
         CALL ERRCH  ( '#', PICTUR(1:LASTCH) )
         CALL ERRINT ( '#', LASTCH )
         CALL SIGERR ( 'SPICE(OUTPUTTOOSHORT)' )
         CALL CHKOUT ( 'DPFMT'                                      )
         RETURN

      END IF

!
!     If we get this far, the routine can go ahead and do its business.
!     Determine the sign of X.  Also, determine how many characters
!     are needed to represent the sign if leading sign is suppressed for
!     positive numbers.
!
      IF ( X .GT. 0 ) THEN
         SIGN   = '+'
         SPRSIZ =  0
      ELSE IF ( X .LT. 0 ) THEN
         SIGN   = '-'
         SPRSIZ = 1
      ELSE
         SIGN   = ' '
         SPRSIZ =  0
      END IF
!
!     Look at the picture and see if a leading sign is required and
!     if so whether the sign just determined should use a different
!     character and how many characters are needed for the sign.
!
      IF      ( PICTUR(1:1) .EQ. '+' ) THEN

         NEEDSN = .TRUE.
         SGNLEN =  1

      ELSE IF ( PICTUR(1:1) .EQ. '-' ) THEN

         NEEDSN = .TRUE.
         SGNLEN = 1

         IF ( X .GT. 0 ) THEN
            SIGN = ' '
         END IF

      ELSE

         IF ( X .GT. 0 ) THEN
            SIGN = ' '
         END IF

         NEEDSN = .FALSE.
         SGNLEN =  SPRSIZ

      END IF

!
!     If we need a leading sign. The numeric part of the string
!     will start at character 2.  Otherwise it starts at character 1.
!
      IF ( NEEDSN ) THEN
         START    = 2
      ELSE
         START = 1
      END IF

!
!     We can set the sign portion of the string now.
!
      STR = SIGN

!
!     Determine what character should be use for leading characters
!     before the first significant character of the output string.
!
      IF ( PICTUR(START:START) .EQ. '0' ) THEN
         FILL   = '0'
      ELSE
         FILL  = ' '
      END IF

!
!     See if there is a decimal point.
!
      DPAT   = POS( PICTUR, '.',  1 )
!
!     The integer part is the stuff to the left of the first
!     decimal point and that follows the sign (if there is one
!     that is explicitly required.  The length of the decimal
!     portion is the stuff to the right of the decimal point.
!
      IF ( DPAT .GT. 0 ) THEN

         INTLEN = DPAT - START
         DECLEN = LASTCH - DPAT

      ELSE

         INTLEN =  LASTCH - START + 1
         DECLEN = -1

      END IF
!
!     If a sign was not explicitly requested by placing it in
!     the first digit of the picture START will be 1.  If in
!     addition X is less than zero ( SGNLEN will be 1 in this
!     case) we have one fewer digits available for the integer
!     portion of the string than is currently set in INTLEN.
!     Adjust INTLEN to reflect the actual number of digits
!     available.
!
!     Also set the SHIFT flag to .TRUE. so that we know to swap
!     the sign and any blanks that might lie between the sign
!     and the first significant character of the output string.
!
      IF ( START .EQ. 1 .AND. SGNLEN .EQ. 1 ) THEN
         INTLEN = INTLEN - 1
         SHIFT  = .TRUE.
!
!        If INTLEN has become negative (i.e. -1) the picture
!        must be of the form .xxxxx and the input number must
!        be negative. Add 1 back onto the INTLEN but take one
!        away from the decimal length DECLEN.
!
         IF ( INTLEN .EQ. -1 ) THEN
            INTLEN = 0
            DECLEN = DECLEN - 1

            IF (       DECLEN .EQ. 0 &
                 .AND. INTLEN .EQ. 0 ) THEN
!
!              There is no room for anything other than a
!              decimal point.  We simply fill the output
!              string with the '*' character.
!
               DO I = 1, LASTCH
                  STR(I:I) = '*'
               END DO
               RETURN

            END IF

         END IF

      ELSE
         SHIFT  = .FALSE.
      END IF

!
!     Create the "virtual decimal string" associated with the
!     unsigned part of X.
!
      Y   = DABS(X)

      CALL ZZVSTSTR ( Y, FILL, EXP )

!
!     The actual number of digits required to print the unsigned integer
!     portion X is EXP + 1 (provided EXP is at least 0.) We have
!     INTLEN slots available.  So if EXP + 1 is more than INTLEN
!     ( which is equivalent to EXP being at least INTLEN) we don't
!     have enough room to print the unsigned integer portion of the
!     number.
!


      IF ( EXP .GE. INTLEN .AND. Y .NE. 0.0D0 ) THEN
!
!        See if we have room to print an exponential form.
!        First we need the number of characters for the
!        exponent which is always of the form 'E+dd...'
!
         EXPSIZ = 4 + MIN(1, EXP/1000 ) + MIN(1,EXP/100)
!
!        The number of significant digits that can be printed is the
!        size of the picture minus:   the size of the sign
!                                     the size of the exponent
!                                     the size of the decimal point.
!
         SIGDIG = LASTCH - SGNLEN - EXPSIZ - 1
!
!        If we don't have room for at least one significant digit,
!        there's not much we can do.  Fill the string with '*'.
!

         IF ( SIGDIG .LT. 1 ) THEN
            DO I = 1, LASTCH
               STR(I:I) = '*'
            END DO
         ELSE

            CALL DPSTR ( X, SIGDIG, MYSTR )
            MYSTR(1:1) = SIGN
            CALL LJUST ( MYSTR, STR )
            CALL RJUST ( STR(1:LASTCH), STR(1:LASTCH) )

         END IF


         RETURN

      END IF
!
!     One more check.  If -INTLEN is greater than DECLEN, or if
!     both are zero, we don't have room to create an output string.
!
      IF (  (       INTLEN .EQ. 0 &
              .AND. DECLEN .EQ. 0 ) &
              .OR. -INTLEN .GT. DECLEN ) THEN

         DO I = 1, LASTCH
            STR(I:I) = '*'
         END DO

         RETURN
      END IF
!
!     We have a reasonable chance of successfully constructing
!     the string without overflow.
!
      START = SGNLEN + 1
      CALL ZZVSBSTR ( -INTLEN, DECLEN, .TRUE., STR(START:), OVFLOW )

!
!     We might be done at this point.  The IF-THEN block below
!     handles the one snag that could arise.
!
!     If the first digit is a zero as a result of rounding it up
!     OVFLOW will be true.  This means we don't have enough room
!     in the picture for the integer portion of the string.  We try
!     to make an exponential picture.
!
      IF ( OVFLOW ) THEN
!
!        See if we have room to print an exponential form.
!
         EXPSIZ = 4 + MIN(1, EXP/1000 ) + MIN(1,EXP/100)
!
!        The number of significant digits that can be printed is the
!        size of the picture minus:   the size of the sign
!                                     the size of the exponent
!                                     the size of the decimal point.
!
         SIGDIG = LASTCH - SGNLEN - EXPSIZ - 1


         IF ( SIGDIG .LT. 1 ) THEN
            DO I = 1, LASTCH
               STR(I:I) = '*'
            END DO
         ELSE

            CALL DPSTR ( X, SIGDIG, MYSTR )
            MYSTR(1:1) = SIGN
            CALL LJUST ( MYSTR, STR )
            CALL RJUST ( STR(1:LASTCH), STR(1:LASTCH) )
            RETURN

         END IF

      ELSE IF ( SHIFT ) THEN
!
!        We need to move the sign right until, there are no
!        blanks between it and the next character.
!
         FRSTCH = NCPOS ( STR, ' -', 1 )

         IF ( FRSTCH .GT. 2 ) THEN
            STR(FRSTCH-1:FRSTCH-1) = STR(1:1)
            STR(1:1)               = ' '
         END IF

      END IF



      RETURN
      END
