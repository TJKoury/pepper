
!$Procedure      CUTSTR ( Cut a long string into substrings )

      SUBROUTINE CUTSTR ( STRING, START, WIDTH, BREAKS, BEG, END )
      IMPLICIT NONE

! $ Abstract
!
!     Cut a long string into substrings, breaking at "good" points
!     whenever possible.
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
!     STRING
!     UTILITY
!
! $ Declarations

      CHARACTER*(*)         STRING
      INTEGER               START
      INTEGER               WIDTH
      CHARACTER*(*)         BREAKS
      INTEGER               BEG
      INTEGER               END

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   Long string.
!     START      I   Nominal beginning of substring.
!     WIDTH      I   Maximum width of substrings.
!     BREAKS     I   Characters indicating good break points.
!     BEG        I   Beginning of substring.
!     END        O   End of substring.
!
! $ Detailed_Input
!
!     STRING      is an arbitrary character string. Typically, this
!                 is too wide to fit into an area of limited width:
!                 an element of a character array, for instance, or
!                 an area on a terminal screen.
!
!     START       is the nominal beginning of the next substring.
!                 (STRING(START:START) is the first character that
!                 can appear in the substring.) It is used to skip
!                 past substrings returned by previous calls.
!
!     WIDTH       is the width (in characters) of the limited area.
!                 Thus, it is the maximum width of the substrings
!                 to be returned.
!
!     BREAKS      is a collection of characters indicating preferred
!                 places to break the string into substrings: commas,
!                 colons, and periods, for instance. BREAKS is always
!                 treated as though it contains a space, whether it
!                 does or not. (That is, '+-=' is treated as ' +-='.)
!
! $ Detailed_Output
!
!     BEG,
!     END         are the endpoints of a substring no wider than
!                 WIDTH. Substrings always begin and end with non-blank
!                 characters. BEG is zero if no non-blank substring
!                 was found.
!
! $ Exceptions.
!
!     1) If STRING(START:) is blank or BEG is greater than the declared
!        length of STRING, both BEG and END are zero.
!
!     2) If START is less than one, the error 'SPICE(BEFOREBEGSTR)'
!        is signalled.
!
!     4) If WIDTH is less than one, the error 'SPICE(WIDTHTOOSMALL)'
!        is signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!      This routine is useful primarily for displaying messages on
!      terminal screens or within log files. Since messages can run
!      to hundreds of characters, while most output devices cannot
!      handle more than 80 or 132 characters at a time, it is necessary
!      to break the strings. The friendliness of a message is enhanced
!      if these breaks occur at "natural" places within the message,
!      rather than at rigid intervals.
!
!      The most natural breaks occur before spaces. Slightly less
!      natural breaks occur at the characters
!
!         Comma             ,
!         Period            .
!         Semicolon         ;
!         Colon             :
!         Question          ?
!         Exclamation       !
!         End parenthesis   )
!         End bracket       ]
!         End brace         }
!         End angle         >
!
!      or before the characters
!
!         Begin parenthesis   (
!         Begin bracket       [
!         Begin brace         {
!         Begin angle         <
!
!      At any rate, breaks should occur between adjacent letters or
!      numeric characters only as a last resort.
!
!      In the absence of other instructions, CUTSTR tries to break:
!
!         1) before      space
!
!         2) at           , . ; : - ) ] } >
!            or before              ( [ { <
!
!         3) at           ' "                   (even occurrence)
!            or before    ' "                   (odd  occurrence)
!
!         4) at           ? ! = _ %
!            or before    \ ~ $ @ ^ * / | & +
!
!      before forcing a break at an aribitrary location.
!
!      You may override these rules by supplying a set of preferred
!      characters in BREAKS. Before applying the rules shown above,
!      CUTSTR will try to break AT these characters. (However, breaks
!      always occur BEFORE spaces.)
!
! $ Examples
!
!      CUTSTR might typically be used to display a long diagnostic
!      messages on a terminal screen. For example, suppose that the
!      following message has been returned by a subroutine.
!
!         'I believe you have made an significant error by requesting
!          that I send to the printer a file containing 250 megabytes
!          of text information. The system manager is likely to be
!          very unhappy with such a request. I suggest you reconsider
!          your intended action.'
!
!      and that this needs to be displayed on a 40-character monitor.
!      The following code fragment
!
!         WIDTH  = 40
!         BREAKS = ' ,.'
!
!         CALL CUTSTR ( MSSG, 1, WIDTH, BREAKS, BEG, END )
!
!         DO WHILE ( BEG .NE. 0 )
!            WRITE (6,*) MSSG (BEG:END)
!
!            START = END + 1
!            CALL CUTSTR ( MSSG, START, WIDTH, BREAKS, BEG, END )
!         END DO
!
!      would display something like
!
!         I believe you have made an significant
!         error by requesting that I send to the
!         printer a file containing 250 megabytes
!         of text information. The system manager
!         is likely to be very unhappy with such a
!         request.  I suggest you reconsider your
!         intended action.
!
!      On a more whimsical note, you could indent each successive lines
!      by three characters: the code fragment
!
!         WIDTH  = 40
!         BREAKS = ' ,.'
!         INDENT = 1
!
!         CALL CUTSTR ( MSSG, 1, WIDTH, BREAKS, BEG, END )
!
!         DO WHILE ( BEG .NE. 0 )
!            TEMP           = ' '
!            TEMP(INDENT: ) = MSSG(BEG:END)
!            WRITE (6,*) TEMP
!
!            INDENT = INDENT + 3
!            WIDTH  = MAX ( WIDTH-3, 9 )
!
!            START = END + 1
!            CALL CUTSTR ( MSSG, START, WIDTH, BREAKS, BEG, END )
!         END DO
!
!      would display something like
!
!         I believe you have made an significant
!            error by requesting that I send to
!               the printer a file containing 250
!                  megabytes of text information.
!                     The system manager is likely
!                        to be very unhappy with
!                           such a request. I
!                              suggest you
!                                 reconsider your
!                                    intended
!                                       action.
!
!     Note that both loops terminate when BEG is zero. This indicates
!     that no substring was found (and that none will be returned by
!     subsequent calls). If the string is full, the loop will terminate
!     normally when START becomes greater than the length of the string.
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
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!     Version B1.0.0, 29-APR-1988 (WLT) (IMU)
!
!-&



!
!     SPICELIB functions
!
      LOGICAL               EVEN
      INTEGER               OCCURS
      LOGICAL               RETURN

!
!     Local variables
!

      INTEGER               A
      INTEGER               B
      INTEGER               BLANK
      INTEGER               P

      INTEGER               LENGTH
      INTEGER               LONG

      CHARACTER*15          PUNCT
      CHARACTER*15          QUOTE
      CHARACTER*15          OTHER
      CHARACTER*15          DEF
      CHARACTER*15          QTYPE
      CHARACTER*15          PTYPE
      CHARACTER*15          OTYPE
      CHARACTER*15          DTYPE

      INTEGER               PASS

      CHARACTER*1           THIS
      CHARACTER*1           NEXT
      INTEGER               HERE
      INTEGER               THERE
!
!     Parameters used to simulate an enumerated type for
!     the various passes required to break the string at
!     good places.  Note that the order is important.
!     This forces the routine to try spaces first, user
!     supplied preferences next, etc.    It is also
!     critical that these be defined to be a sequence
!     of consecutive integers.
!
      INTEGER               SPACE
      PARAMETER           ( SPACE  = 1 )

      INTEGER               PREF
      PARAMETER           ( PREF   = SPACE  + 1 )

      INTEGER               PUNC
      PARAMETER           ( PUNC   = PREF   + 1 )

      INTEGER               QUOT
      PARAMETER           ( QUOT   = PUNC   + 1 )

      INTEGER               OTHR
      PARAMETER           ( OTHR   = QUOT   + 1 )

      INTEGER               FAIL
      PARAMETER           ( FAIL   = OTHR   + 1 )

      INTEGER               FINSH
      PARAMETER           ( FINSH  = FAIL   + 1 )
!
!     The ASCII character value for the backslash is needed for
!     uniformity of porting this routine (Some platforms treat the
!     backslah as a special character and so we can't just use
!     the character in strings.)
!
      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )

!
!     Standard error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF


!
!     Exceptions first. Is START outside the bounds of the string?
!
      LENGTH = LEN ( STRING )

      IF ( START .GT. LENGTH ) THEN
         BEG = 0
         END = 0
         RETURN

      ELSE IF ( START .LT. 1 ) THEN
         CALL CHKIN  ( 'CUTSTR' )
         CALL SIGERR ( 'SPICE(BEFOREBEGSTR)' )
         CALL CHKOUT ( 'CUTSTR' )
         RETURN
      END IF

!
!     Is the width reasonable?
!
      IF ( WIDTH .LT. 1 ) THEN
         CALL CHKIN  ( 'CUTSTR' )
         CALL SIGERR ( 'SPICE(WIDTHTOOSMALL)' )
         CALL CHKOUT ( 'CUTSTR' )
         RETURN
      END IF

!
!     Does the remainder of the string contain anything besides blanks?
!
      IF ( STRING(START: ) .EQ. ' ' ) THEN
         BEG   = 0
         END   = 0
         RETURN
      END IF

!
!     Obviously, we should try to get the longest possible substring.
!
!
      BEG   = START
      BLANK = ICHAR( ' ' )

      DO WHILE ( ICHAR(STRING(BEG:BEG)) .EQ. BLANK )
         BEG = BEG + 1
      END DO

      LONG = BEG + WIDTH - 1

!
!     The remainder of the substring may fit without a trim.
!     But drop trailing blanks anyway.
!
      IF ( LENGTH .LE. LONG ) THEN

         END = LENGTH

         DO WHILE ( ICHAR(STRING(END:END)) .EQ. BLANK )
            END = END - 1
         END DO

         RETURN
      END IF


!
!     Assign the default break characters. Each character in PUNCT,
!     QUOTE, or OTHER indicates a good place to break. The associated
!     type indicates whether the break should occur at or before the
!     the character:
!
!        Type         Break occurs
!        ----         ------------------------------------------------
!         A           At the character.
!         B           Before the character.
!         P           At an EVEN occurrence, or
!                     Before an ODD occurrence.
!
!
      PUNCT = ',.;:-)]}>([{<'
      PTYPE = 'AAAAAAAAABBBB'

      QUOTE = '"'''
      QTYPE = 'PP'

      OTHER = '?!~$@^=_%*/|&+' // CHAR(BSLASH)
      OTYPE = 'AABBBBAAABBBBB' // 'B'

!
!     We will do this in five passes. During the first pass, we will
!     try to break before a space. During the second pass, we will try
!     to break at one of the preferred characters. During the third,
!     fourth, and fifth passes, we will try to break at or before one
!     of the quotation, punctuation, or other default characters.
!
      PASS      = SPACE
      A         = ICHAR( 'A' )
      P         = ICHAR( 'P' )
      B         = ICHAR( 'B' )


      DO WHILE ( PASS .NE. FAIL )

         END = LONG

         DO WHILE ( END .GE. BEG )

            THIS = STRING(END   : END  )
            NEXT = STRING(END+1 : END+1)

!
!           Always break BEFORE a space.
!
            IF ( PASS .EQ. SPACE ) THEN

              IF ( ICHAR(NEXT) .EQ. BLANK ) THEN
                PASS = FINSH
              END IF

!
!           Always break AT a preferred character.
!
            ELSE IF ( PASS .EQ. PREF ) THEN

              IF ( INDEX ( BREAKS, THIS ) .GT. 0 ) THEN
                PASS = FINSH
              END IF

!
!           But with default characters, some break at, some
!           before, and some depend on the parity of strangers.
!
            ELSE

               HERE  = INDEX ( DEF, THIS )
               THERE = INDEX ( DEF, NEXT )

               IF ( HERE .GT. 0 ) THEN

                 IF ( ICHAR(DTYPE(HERE:HERE)) .EQ. A ) THEN
                   PASS = FINSH

                 ELSE IF ( ICHAR(DTYPE(HERE:HERE)) .EQ. P ) THEN
                   IF ( EVEN ( OCCURS ( STRING( :END), THIS ) ) ) THEN
                     PASS = FINSH
                   END IF
                 END IF
               END IF

               IF (      THERE .GT. 0 &
                   .AND. PASS  .NE. FINSH ) THEN

                 IF ( ICHAR(DTYPE(THERE:THERE)) .EQ. B ) THEN
                   PASS = FINSH

                 ELSE IF ( ICHAR(DTYPE(THERE:THERE)) .EQ. P ) THEN
                   IF ( EVEN ( OCCURS ( STRING( :END), NEXT ) ) ) THEN
                     PASS = FINSH
                   END IF
                 END IF

               END IF

            END IF

!
!           If we've found a break point, remove any trailing blanks
!           before returning.
!
            IF ( PASS .EQ. FINSH ) THEN
               DO WHILE ( ICHAR(STRING(END:END)) .EQ. BLANK )
                  END = END - 1
               END DO

               RETURN
            ELSE
               END = END - 1
            END IF

         END DO

!
!        We may have to try another pass.
!
         PASS  = PASS + 1

!
!        In the final passes, only the character set changes.
!
         IF ( PASS .EQ. PUNC ) THEN
            DEF   = PUNCT
            DTYPE = PTYPE

         ELSE IF ( PASS .EQ. QUOT ) THEN
            DEF   = QUOTE
            DTYPE = QTYPE

         ELSE IF ( PASS .EQ. OTHR ) THEN
            DEF   = OTHER
            DTYPE = OTYPE
         END IF

      END DO

!
!     Looks like we'll have to do this the hard way.
!
      END = LONG

      RETURN

      END
