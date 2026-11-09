!$Procedure GETOPT ( Get an option from a menu )

      SUBROUTINE GETOPT ( TITLE, NOPT, OPTNAM, OPTTXT, OPTION )

! $ Abstract
!
!     Display a list of options in a standard menu format and get
!     an option from a user returning the corresponding index of
!     the option selected.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations

      EXTERNAL              PROMPT

      CHARACTER*(*)         TITLE
      INTEGER               NOPT
      CHARACTER*(*)         OPTNAM(*)
      CHARACTER*(*)         OPTTXT(*)
      INTEGER               OPTION

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TITLE      I   Title for the menu.
!     NOPT       I   Number of options available.
!     OPTNAM     I   Names for the options.
!     OPTTXT     I   Brief text describing an option.
!     OPTVAL     I   The value returned when its option is selected.
!     OPTION     O   The number of the option selected.
!
! $ Detailed_Input
!
!     TITLE    Title for the option menu.
!
!     NOPT     The number of menu options to be displayed.
!
!     OPTNAM   A list of single character names for the menu options.
!              These are the names used to select an option. The names
!              must each be a single alphanumeric character. All names
!              must be upper case if they are characters.
!
!              If the option names is a period, '.', then a blank line
!              is to be displayed at that position in the menu list.
!
!     OPTTXT   A list of character strings which contain brief
!              descriptions for each of the menu options. These
!              character strings should be kept relatively short.
!
!     Please note that the lengths of the option names, OPTNAM, and
!     the descriptive text for each option, OPTTXT, should be kept
!     reasonable, they both need to fit on the same output line with
!     a width of 80 characters. 13 characters out of the 80 available
!     are used for spacing and menu presentation, so there are 67
!     characters available for the option name and the descriptive text
!     combined.
!
! $ Detailed_Output
!
!     OPTION   The index of the option selected from the menu.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)   If the number of options, NOPT, is not > 0, the error
!          SPICE(INVALIDARGUMENT) will be signalled.
!
!     2)   If the option names are not all upper case alphanumeric
!          characters, the error SPICE(BADOPTIONNAME) will be signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine will display a menu of options in a standardized
!     format, promting for the selection of one of the listed options.
!     This routine will not return to the caller until one of the
!     supplied options has been selected or an error occurs.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     This routine makes explicit use fo the ASCII character sequence.
!
! $ Author_and_Institution
!
!     K.R. Gehringer  (JPL)
!
! $ Version
!
!-    Beta Version 4.2.0, 18-DEC-2010 (EDW)
!
!        Declared PROMPT as EXTERNAL. Eliminated unneeded Revisions
!        section.
!
!-    Beta Version 4.1.0, 05-JUL-1995  (KRG)
!
!        Removed the initial blank line that was printed before the
!        title of the menu. The calling program should determine the
!        whitespace requirements for the appearance of the menu
!        displayed by this routine.
!
!-    Beta Version 4.0.0, 25-APR-1994  (KRG)
!
!        Modified the routine to output the index into the list of menu
!        options rather than a character string representing the option
!        selected. Also removed several calling arguments that were not
!        needed anymore.
!
!        Added the capability of inserting a blank line into the menu.
!        This is done by placing a period, '.', into the option name
!        location where the blank line lshould occur.
!
!        Added the missing $ Index_Entries section to the header.
!
!        Clarified a few of the comments in the header.
!
!-    Beta Version 3.0.0, 03-SEP-1992  (KRG)
!
!-&

! $ Index_Entries
!
!      display a menu and get a user's selection
!
!-&


!
!     SPICELIB functions
!
      INTEGER               RTRIM
      INTEGER               ISRCHC

      LOGICAL               FAILED
      LOGICAL               RETURN
!
!     Local Parameters
!
      INTEGER               TB4POS
      PARAMETER           ( TB4POS = 4       )

      INTEGER               TTLPOS
      PARAMETER           ( TTLPOS = 10      )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80      )

!
!     Mnemonic for the standard output.
!
      INTEGER               STDOUT
      PARAMETER           ( STDOUT = 6 )
!
!     Local variables
!
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    PRMPT
      CHARACTER*(LINLEN)    MSG

      INTEGER               I
      INTEGER               IOPT

      LOGICAL               DONE
      LOGICAL               OK
      LOGICAL               OKALPH
      LOGICAL               OKDIGI
      LOGICAL               OKEQU

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETOPT' )
      END IF
!
!     Check to make sure that the number of menu options is positive.
!     if it is not, then signal an error with an appropriate error
!     message.
!
      IF ( NOPT .LT. 1 ) THEN

         CALL SETMSG ( 'The number of options was not positive: #.' )
         CALL ERRINT ( '#', NOPT                                    )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                     )
         CALL CHKOUT ( 'GETOPT'                                     )
         RETURN

      END IF
!
!     Initialize the option prompt.
!
      PRMPT = ' '
      PRMPT(TB4POS:) = 'Option: '
!
!     Check to make sure that all of the option names are alphanumeric
!     and uppercase. The only exception is the period, which signals a
!     blank line.
!
      OK = .TRUE.
      DO I = 1, NOPT

         OKDIGI = ( ICHAR ( OPTNAM(I) ) .GE. ICHAR ( '0' ) ) .AND. &
                  ( ICHAR ( OPTNAM(I) ) .LE. ICHAR ( '9' ) )

         OKALPH = ( ICHAR ( OPTNAM(I) ) .GE. ICHAR ( 'A' ) ) .AND. &
                  ( ICHAR ( OPTNAM(I) ) .LE. ICHAR ( 'Z' ) )

         OKEQU  = ( ICHAR ( OPTNAM(I) ) .EQ. ICHAR ( '.' ) )

         OK = OK .AND. ( OKDIGI .OR. OKALPH .OR. OKEQU )

         IF ( .NOT. OK ) THEN

            CALL SETMSG ( 'An illegal option name was found:'  // &
                          ' option #, name ''#''. '             )
            CALL ERRINT ( '#', I                                )
            CALL SIGERR ( 'SPICE(ILLEGALOPTIONNAME)'            )
            CALL CHKOUT ( 'GETOPT'                              )
            RETURN

         END IF

      END DO
!
!     Do until we get a valid option.
!
      DONE = .FALSE.
      DO WHILE ( .NOT. DONE )
!
!        Display the menu title if it is non blank
!
         IF ( TITLE .NE. ' ' ) THEN

            LINE = ' '
            LINE (TTLPOS:) = '#'
            CALL REPMC ( LINE, '#', TITLE, LINE )
            CALL WRITLN ( LINE, STDOUT )

         END IF
!
!        Display the menu and read in an option.
!
         CALL WRITLN ( ' ', STDOUT )

         DO I = 1, NOPT

            LINE = ' '
            IF ( OPTNAM(I) .NE. '.' ) THEN

               LINE(TB4POS:) = '( # ) #'
               CALL REPMC ( LINE, '#', OPTNAM(I), LINE )
               CALL REPMC ( LINE, '#', OPTTXT(I), LINE )

            END IF

            CALL WRITLN ( LINE, STDOUT )

         END DO

         CALL WRITLN ( ' ', STDOUT )

         I = RTRIM ( PRMPT ) + 1
         CALL PROMPT ( PRMPT(:I), LINE )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'GETOPT' )
            RETURN
         END IF
!
!        Initialize the option value to zero, invalid option.
!
         IOPT = 0
         IF ( LINE .EQ. ' ' ) THEN

            CALL WRITLN ( ' ', STDOUT )

         ELSE

            CALL LJUST ( LINE, LINE )
            CALL UCASE ( LINE, LINE )
!
!           Check to make sure that the option we got is a valid
!           candidate: It must be alpha numeric.
!
            OKDIGI = ( ICHAR ( LINE(1:1) ) .GE. ICHAR ( '0' ) ) .AND. &
                     ( ICHAR ( LINE(1:1) ) .LE. ICHAR ( '9' ) )

            OKALPH = ( ICHAR ( LINE(1:1) ) .GE. ICHAR ( 'A' ) ) .AND. &
                     ( ICHAR ( LINE(1:1) ) .LE. ICHAR ( 'Z' ) )

            OK = OKDIGI .OR. OKALPH
!
!           If we got a valid candidate for an option, see if it is one
!           of the options that we are supplying.
!
            IF ( OK ) THEN

               IOPT = ISRCHC ( LINE(1:1), NOPT, OPTNAM )
               OK   = IOPT .NE. 0

            END IF

            IF ( .NOT. OK ) THEN

               MSG = '''#'' was not a valid option.' // &
                     ' Please try again.'
               CALL REPMC ( MSG, '#', LINE(1:1), MSG )
               CALL WRITLN ( ' ', STDOUT )
               LINE          = ' '
               LINE(TB4POS:) = '***'
               CALL WRITLN ( LINE, STDOUT )
               LINE(TB4POS:) = '*** #'
               CALL REPMC ( LINE, '#', MSG, LINE )
               CALL WRITLN ( LINE, STDOUT )
               LINE(TB4POS:) = '***'
               CALL WRITLN ( LINE, STDOUT )
               CALL WRITLN ( ' ', STDOUT )

            ELSE

               OPTION = IOPT
               DONE   = .TRUE.

            END IF

         END IF

      END DO

      CALL CHKOUT ( 'GETOPT' )
      RETURN
      END
