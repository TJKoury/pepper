!$Procedure      PARCML ( Parse command line )

      SUBROUTINE PARCML ( LINE, NKEYS, CLKEYS, CLFLAG, CLVALS, FOUND, &
                          UNPRSD )

! $ Abstract
!
!     Parse a command-line like string in the "key value key value ..."
!     format with keys provided in any order and any letter case
!     (lower, upper, mixed) and return values of requested keys.
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

      CHARACTER*(*)         LINE
      INTEGER               NKEYS
      CHARACTER*(*)         CLKEYS ( * )
      LOGICAL               CLFLAG ( * )
      CHARACTER*(*)         CLVALS ( * )
      LOGICAL               FOUND
      CHARACTER*(*)         UNPRSD

      INTEGER               LLNSIZ
      PARAMETER           ( LLNSIZ = 2048 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LINE      I/O  Input command-line like string.
!     NKEYS      I   Number of keys to look for.
!     CLKEYS     I   Keys to look for.
!     CLFLAG     O   "A particular key found" flags.
!     CLVALS     O   Key values.
!     FOUND      O   "At least one key found" flag.
!     UNPRSD     O   Beginning part of the LINE that was not parsed
!     LLNSIZ     P   Size of longest sub-string that can be processed.
!
! $ Detailed_Input
!
!     LINE        is the input command-line like string in the "key
!                 value key value ..." format. The line should start
!                 with one of the keys provided in CLKEYS as the
!                 routine ignores any words before the first recognized
!                 key.
!
!                 To avoid limiting the size of the input string that
!                 can be processed, this routine uses LINE as the work
!                 buffer; it modifies LINE in the process of execution,
!                 and sets it to blank before return.
!
!     NKEYS       is the number of keys to look for provided in the
!                 CLKEYS array.
!
!     CLKEYS      is an array of keys to look for. Individual keys
!                 must be left-justified string consisting of any
!                 printable the characters except lower-case letters
!                 and blanks.
!
! $ Detailed_Output
!
!     LINE        is set to blank on the output.
!
!     CLFLAG      are the "key found" flags; set to TRUE if
!                 corresponding key was found.
!
!     CLVALS      are the key values; if a key wasn't found, its value
!                 set to a blank string.
!
!     FOUND       is set to .TRUE. if at least one key was found.
!                 Otherwise it is set to .FALSE.
!
!     UNPRSD      is the beginning part of the LINE, preceeding the
!                 first recognized key, that was ignored by this
!                 routine.
!
! $ Parameters
!
!     LLNSIZ      is the size of the internal buffer that holds a
!                 portion of the input string that is being examined.
!                 It limits the maximum total length of a front and
!                 back blank-padded, blank-separated sub-string
!                 containing a key, the value that follows it, and the
!                 next key (e.g. ' key value key ') that this routine
!                 can correctly process.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine modifies the input string. It returns it set to
!     blank.
!
!     The case of the keys in the input string is not significant.
!
!     The order of keys in the input string is not significant.
!
!     If any key appears in the string more than once, only the
!     last value of that key is returned.
!
!     The part of the line from the start up to the first recognized
!     key is returned in the UNPRSD argument.
!
! $ Examples
!
!     If CLKEYS are
!
!        CLKEYS(1) = '-SETUP'
!        CLKEYS(2) = '-TO'
!        CLKEYS(3) = '-FROM'
!        CLKEYS(4) = '-HELP'
!
!     then:
!
!     line '-setup my.file -FROM utc -TO sclk'
!     will be parsed as
!
!        CLFLAG(1) = .TRUE.       CLVALS(1) = 'my.file'
!        CLFLAG(2) = .TRUE.       CLVALS(2) = 'utc'
!        CLFLAG(3) = .TRUE.       CLVALS(3) = 'sclk'
!        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
!        UNPRSD    = ' '
!        FOUND = .TRUE.
!
!     line '-bogus -setup my.file -FROM utc -TO sclk'
!     will be parsed as
!
!        CLFLAG(1) = .TRUE.       CLVALS(1) = 'my.file'
!        CLFLAG(2) = .TRUE.       CLVALS(2) = 'utc'
!        CLFLAG(3) = .TRUE.       CLVALS(3) = 'sclk'
!        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
!        UNPRSD    = '-bogus'
!        FOUND = .TRUE.
!
!     line 'why not -setup my.file -FROM utc -TO sclk'
!     will be parsed as
!
!        CLFLAG(1) = .TRUE.       CLVALS(1) = 'my.file'
!        CLFLAG(2) = .TRUE.       CLVALS(2) = 'utc'
!        CLFLAG(3) = .TRUE.       CLVALS(3) = 'sclk'
!        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
!        UNPRSD    = 'why not'
!        FOUND = .TRUE.
!
!     line '-SETUP my.file -setup your.file'
!     will be parsed as
!
!        CLFLAG(1) = .TRUE.       CLVALS(1) = 'your.file'
!        CLFLAG(2) = .FALSE.      CLVALS(2) = ' '
!        CLFLAG(3) = .FALSE.      CLVALS(3) = ' '
!        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
!        UNPRSD    = ' '
!        FOUND = .TRUE.
!
!     line '-setup my.file -SeTuP your.file'
!     will be parsed as
!
!        CLFLAG(1) = .TRUE.       CLVALS(1) = 'your.file'
!        CLFLAG(2) = .FALSE.      CLVALS(2) = ' '
!        CLFLAG(3) = .FALSE.      CLVALS(3) = ' '
!        CLFLAG(4) = .FALSE.      CLVALS(4) = ' '
!        UNPRSD    = ' '
!        FOUND = .TRUE.
!
!     line '-help'
!     will be parsed as
!
!        CLFLAG(1) = .FALSE.      CLVALS(1) = ' '
!        CLFLAG(2) = .FALSE.      CLVALS(2) = ' '
!        CLFLAG(3) = .FALSE.      CLVALS(3) = ' '
!        CLFLAG(4) = .TRUE.       CLVALS(4) = ' '
!        UNPRSD    = ' '
!        FOUND = .TRUE.
!
!     and so on.
!
! $ Restrictions
!
!     This routine cannot process input lines with any ' -key value
!     -key ' sub-string that is longer than LLNSIZ.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SUPPORT Version 1.0.0, 15-FEB-2012 (BVS)
!
!-&

!
!     Local variables.
!
      CHARACTER*(LLNSIZ)    HKEY
      CHARACTER*(LLNSIZ)    HLINE
      CHARACTER*(LLNSIZ)    HLNGWD
      CHARACTER*(LLNSIZ)    LNGWD
      CHARACTER*(LLNSIZ)    ULINE

      INTEGER               BEGPOS
      INTEGER               CLIDX
      INTEGER               ENDPOS
      INTEGER               I
      INTEGER               PCLIDX

!
!     Save everything to prevent potential memory problems in f2c'ed
!     version.
!
      SAVE

!
!     SPICELIB functions.
!
      INTEGER               ISRCHC
      INTEGER               POS
      INTEGER               RTRIM

      LOGICAL               RETURN

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PARCML' )
      END IF

!
!     Set initial values of keys to blanks and flags to .FALSE.
!
      DO I = 1, NKEYS
         CLFLAG( I ) = .FALSE.
         CLVALS( I ) = ' '
      END DO

      FOUND = .FALSE.

!
!     Parsing loop. We will set the sub-string buffer HLINE to as many
!     characters from the input line as it will fit, starting with the
!     initial part of the line on the first iteration and resetting to
!     sub-strings starting at the first character of each value after
!     the previous key-value pair was processed, and will pick at HLINE
!     word by word looking for recognized keys. The loop will
!     continue until we reach the end of the string -- all key-value
!     pairs were processed and the sub-string buffer HLINE was set to
!     blank.
!
      HLINE  = LINE

      PCLIDX = 0
      CLIDX  = 0

      UNPRSD = LINE

      DO WHILE ( HLINE .NE. ' ' )

!
!        Get next word; uppercase it; look for it in the input keys
!        array.
!
         CALL NEXTWD ( HLINE, LNGWD, HLINE )
         CALL UCASE  ( LNGWD, HLNGWD )
         CLIDX = ISRCHC( HLNGWD, NKEYS, CLKEYS )

!
!        Is the token that we found a recognized key?
!
         IF ( CLIDX .NE. 0 ) THEN

!
!           Yes, it is. Is it the first key that we have found?
!
            IF ( PCLIDX .NE. 0 ) THEN

!
!              No it is not. We need to save the value of the previous
!              key.
!
!              Compute the begin and end positions of the sub-string
!              that contains the previous value by looking for the
!              previous and current keys in the upper-cased remainder of
!              the input line.
!
!              The begin position is the position of the previous key
!              plus its length. The end position is the position of the
!              front-n-back blank-padded current key.
!
               CALL UCASE( LINE, ULINE )
               BEGPOS = &
                  POS(ULINE, CLKEYS(PCLIDX)(:RTRIM(CLKEYS(PCLIDX))), 1) &
                  + RTRIM( CLKEYS(PCLIDX) )

               HKEY = ' ' // CLKEYS(CLIDX)(:RTRIM(CLKEYS(CLIDX)))
               ENDPOS = POS(ULINE//' ', HKEY(:RTRIM(HKEY)+1), BEGPOS )

!
!              Extract the value, left-justify it, and RTRIM it. Set
!              "value found" flag to .TRUE.
!
               CLVALS(PCLIDX) = LINE(BEGPOS:ENDPOS)
               CALL LJUST( CLVALS(PCLIDX), CLVALS(PCLIDX) )
               CLVALS(PCLIDX) = CLVALS(PCLIDX)(:RTRIM(CLVALS(PCLIDX)))

               CLFLAG(PCLIDX) = .TRUE.

!
!              Check whether we already parsed the whole line. It will
!              be so if the remainder of the buffer holding the
!              sub-string that we examine word-by-word is a blank
!              string.
!
               IF ( HLINE .NE. ' ' ) THEN

!
!                 No, we did not parse the whole line yet. There is
!                 more stuff to parse and we reset the temporary
!                 sub-string buffer to hold the part of the input string
!                 starting with the first character after the current
!                 key -- the end position plus the length of the
!                 current key.
!
!
                  HLINE = LINE(ENDPOS+1+RTRIM(CLKEYS(CLIDX)):)

               END IF

!
!              Now reset the line to its portion starting with the
!              first character of the current key.
!
               LINE  = LINE(ENDPOS+1:)

            ELSE

!
!              This is the first key that we have found. Set UNPRSD
!              to the part of the line from the start to this key.
!
               CALL UCASE( LINE, ULINE )

               HKEY = ' ' // CLKEYS(CLIDX)(:RTRIM(CLKEYS(CLIDX)))
               BEGPOS = POS( ' ' // ULINE, HKEY(:RTRIM(HKEY)+1), 1 )
               IF ( BEGPOS .LE. 1 ) THEN
                  UNPRSD = ' '
               ELSE
                  UNPRSD = LINE(1:BEGPOS-1)
               END IF

            END IF

!
!           Save the current key index in as previous.
!
            PCLIDX = CLIDX

         END IF

      END DO

!
!     If we found at least one recognized key, we need to save the last
!     value.
!
      IF ( PCLIDX .NE. 0 ) THEN

!
!        Set "found any" output flag and "found previous key" flags to
!        .TRUE.
!
         FOUND = .TRUE.
         CLFLAG(PCLIDX) = .TRUE.

!
!        Check if there was any value following the last key (there was
!        if the non-blank length of what's left in the line starting
!        with the last key if greater than the non-blank length of the
!        last key).
!
         IF ( RTRIM(LINE) .GT. RTRIM(CLKEYS(PCLIDX)) ) THEN

!
!           Compute begin position of, extract, left justify and
!           RTRIM the last value.
!
            CALL UCASE( LINE, ULINE )
            BEGPOS = &
               POS(ULINE,CLKEYS(PCLIDX)(:RTRIM(CLKEYS(PCLIDX))), 1) &
                + RTRIM( CLKEYS(PCLIDX) )

            CLVALS(PCLIDX) = LINE(BEGPOS:)
            CALL LJUST( CLVALS(PCLIDX), CLVALS(PCLIDX) )
            CLVALS(PCLIDX) = CLVALS(PCLIDX)(:RTRIM(CLVALS(PCLIDX)))

         ELSE

!
!           The key was the last thing on the line. So, it's value is
!           blank.
!
            CLVALS(PCLIDX) = ' '

         END IF

      END IF

      CALL CHKOUT ( 'PARCML' )
      RETURN
      END

