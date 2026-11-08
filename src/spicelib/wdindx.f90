!$Procedure WDINDX ( Index of a Word Within a String )

      INTEGER FUNCTION WDINDX ( STRING, WORD )

! $ Abstract
!
!     Find the index of a word within a string. If the word does not
!     exist as a word within the string, the value zero is returned.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PARSING
!     SEARCH
!     WORD
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)    STRING
      CHARACTER*(*)    WORD

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   String of characters, potentially containing words
!     WORD       I   A string of consecutive printable letters.
!
!     The function returns the location of WORD within STRING.
!
! $ Detailed_Input
!
!     STRING   is a string of characters, potentially containing the
!              word.
!
!              Leading and trailing blanks are not significant in
!              STRING.
!
!     WORD     is a string of consecutive printable characters.
!
!              Leading and trailing blanks are not significant in
!              WORD.
!
! $ Detailed_Output
!
!     The function returns the location of WORD within STRING, providing
!     the index of the first letter of WORD within the input STRING. If
!     WORD does not exist or WORD is blank, the function returns zero.
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
!     A word within a string is a substring beginning and ending with a
!     non-blank characters that is delimited by blanks on each end. (A
!     blank is assumed to precede and follow the first and last
!     characters of a string.)
!
!     Given a word, this routine returns the index of the first letter
!     of the first word of STRING that matches the word.
!
! $ Examples
!
!     STRING:
!                      1         2         3         4
!     WORD    1234567890123456789012345678901234567890123456    WDINDX
!     ------  ----------------------------------------------    ------
!     'POT'   'PUT THE POTATOES IN THE POT'                     25
!     'TOES'                                                    0
!     'PUT'                                                     1
!     'THE'                                                     5
!     'IN THE'                                                  18
!     'THE PO'                                                  0
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
!-    SPICELIB Version 1.1.0, 03-OCT-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     index of a word within a string
!
!-&


!
!     SPICELIB functions
!
      INTEGER          FRSTNB
      INTEGER          LASTNB

!
!     Local variables
!
      INTEGER          I
      INTEGER          J

      INTEGER          BEGWD
      INTEGER          ENDWD

      INTEGER          BEGSTR
      INTEGER          ENDSTR

      INTEGER          STRLEN
      INTEGER          WDLEN

      INTEGER          BGTOND

!
!     Find the ends of the word and input string.
!
      ENDSTR = LASTNB ( STRING )
      BEGSTR = FRSTNB ( STRING )

      ENDWD  = LASTNB ( WORD   )
      BEGWD  = FRSTNB ( WORD   )

!
!     Get the offset from the beginning of the word to the end of the
!     word, the word length and the string length.
!
      BGTOND =     ENDWD  - BEGWD
      WDLEN  = 1 + BGTOND
      STRLEN = 1 + ENDSTR - BEGSTR

!
!     We deal with all of the pathologies first...
!
      IF (      ( ENDWD  .LT. 1      ) &
           .OR. ( STRLEN .LT. WDLEN  ) ) THEN

!
!        ... If we got a blank word or a string that is too short, then
!        the index of the word is zero.
!
         WDINDX =  0
         RETURN

      ELSE IF ( STRLEN .EQ. WDLEN ) THEN

!
!        ... the word and string have the same non-blank length.
!        Either they match up or they don't.  Find out and return.
!
         IF ( STRING(BEGSTR:ENDSTR) .EQ. WORD(BEGWD:ENDWD) ) THEN
            WDINDX = BEGSTR
         ELSE
            WDINDX = 0
         END IF

         RETURN
      END IF

!
!     Ok.  Now we've got a realistic case to deal with.  The string
!     length is longer than the word length.  Check to see if we have a
!     match at the beginning of the string.
!
      I      = BEGSTR
      J      = I + BGTOND

      IF (       ( STRING(I:J)     .EQ. WORD(BEGWD:ENDWD) ) &
           .AND. ( STRING(J+1:J+1) .EQ. ' '               )  ) THEN

         WDINDX = I
         RETURN

      END IF

!
!     No luck yet?  Search the string until we find a word match or
!     we run out of string to check.
!
      I      = BEGSTR + 1
      J      = I      + BGTOND

      DO WHILE (       ( J                    .LT. ENDSTR            ) &
                 .AND. &
                 .NOT. (     (STRING(I  :J  ) .EQ. WORD(BEGWD:ENDWD) ) &
                        .AND.(STRING(I-1:I-1) .EQ. ' '               ) &
                        .AND.(STRING(J+1:J+1) .EQ. ' '               ) &
                       ) &
               )
         I   = I + 1
         J   = J + 1
      END DO

!
!     If J equals ENDSTR then no match was found in the interior of the
!     string.  We make a last check at the end.
!
      IF ( J .EQ. ENDSTR ) THEN

         IF (       ( STRING(I-1:I-1) .EQ. ' '               ) &
              .AND. ( STRING(I  :J  ) .EQ. WORD(BEGWD:ENDWD) ) ) THEN
            WDINDX = I
         ELSE
            WDINDX = 0
         END IF

      ELSE
!
!        The only way to get here is if we exited the above loop before
!        running out of room --- that is we had a word match.  Set
!        the index to the value of "I" that got us out of the loop.
!
         WDINDX    = I

      END IF

      RETURN

      END
