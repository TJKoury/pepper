!
! $ Procedure     GETOPT_2 ( Get option string from a specified list )
!
      SUBROUTINE GETOPT_2 ( TITLE,  TINDNT, NOPT, OPTNAM, OPTTXT, &
                            OINDNT, OPTION )
      use pepper_legacy
      IMPLICIT NONE

!
! $ Abstract
!
!     Display a list of options in a standard menu format, and get
!     an option from a user returning the corresponding value from
!     a specified list of option values.
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

      CHARACTER*(*)         TITLE
      INTEGER               TINDNT
      INTEGER               NOPT
      CHARACTER*(*)         OPTNAM(*)
      CHARACTER*(*)         OPTTXT(*)
      INTEGER               OINDNT
      INTEGER               OPTION

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TITLE      I   Title for the option menu.
!     NOPT       I   Number of options available.
!     OPTNAM     I   Names for the options (the selection names).
!     NAMLEN     I   Length of all of the option names.
!     OPTTXT     I   Brief text describing an option.
!     TXTLEN     I   Length of the descriptive text for all options.
!     OPTVAL     I   The value returned when its option is selected.
!     OPTION     O   The value of the option selected.
!
! $ Detailed_Input
!
!     TITLE    Title for the option menu.
!
!     NOPT     The number of menu options to be displayed.
!
!     OPTNAM   A list of short (mnemonic) names for the menu options.
!              These are the names used to selectan option.
!
!     NAMLEN   The maximum length of the short names for the menu
!              options. This number should probably be kept small,
!              say 6 characters or less.
!
!     OPTTXT   A list of character strings which contain brief
!              descriptions for each of the menu options. These
!              character strings should be kept relatively short.
!
!     TXTLEN   The maximum length of the brief descriptions of the
!              menu options. This number should probably be relatively
!              small small, say 50 characters or less.
!
!     OPTVAL   A list of textual values one of which will be returned
!              when a menu option is selected.
!
! $ Detailed_Output
!
!     OPTION   The value of the option selected from the menu, as
!              specified by the appropriate value of OPTVAL.
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
!     2)   If the length of the option names, NAMLEN, is  not > 0,
!          the error SPICE(INVALIDARGUMENT) will be signalled.
!
!     3)   If the length of the option text, TXTLEN, is  not > 0,
!          the error SPICE(INVALIDARGUMENT) will be signalled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine will display a menu of options in a standardized
!     format, promting for an option to be selected. This routine
!     will not return to the caller until one of the supplied options
!     has been selected.
!
!     Please note that the lengths of the option names, OPTNAM, and
!     the descriptive text for each option, OPTTXT, should be kept
!     reasonable, they both need to fit on the same output line with
!     a width of 80 characters. 13 characters out of the 80 available
!     are used for spacing and menu presentation, so there are 67
!     characters available for the option name and the descriptive text
!     combined.
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
!     None.
!
! $ Author_and_Institution
!
!     K.R. Gehringer  (JPL)
!
! $ Version
!
!-    Beta Version 3.0.0, 03-SEP-1992  (KRG)
!
!-&

! $ Revisions
!
!     None.
!
!-&


!
!     SPICELIB functions
!

!
!     Local variables
!
      CHARACTER*(*)         STARS
      PARAMETER           ( STARS = '*****' )


      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )
!
!     Local variables
!
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    MSG
      CHARACTER*(LINLEN)    MYOPT
      CHARACTER*(LINLEN)    SPACE

      INTEGER               ITASK
      INTEGER               NAMLEN

      LOGICAL               DONE
!
!     Saved variables
!
!     None.
!
!
!     Initial values
!
!     None.
!


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETOPT_2' )
      END IF
!
!     Check to make sure that the number of menu options is positive.
!     if it is not, then signal an error with an appropriate error
!     message.
!
      IF ( NOPT .LT. 1 ) THEN

         CALL SETMSG ( 'The number of options was not positive: #.' )
         CALL ERRINT ( '#', NOPT )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)' )
         CALL CHKOUT ( 'GETOPT_2' )
         RETURN

      END IF
!
!     Do until we get an option
!

      NAMLEN = NBWID ( OPTNAM, NOPT )

      DONE  = .FALSE.
      SPACE = ' '
      DO WHILE ( .NOT. DONE )
!
!        Display the menu title if it is non blank
!
         IF ( TITLE .NE. ' ' ) THEN

            IF ( TINDNT .GT. 0 ) THEN
               LINE  = SPACE(1:TINDNT) // TITLE
            ELSE
               LINE  = TITLE
            END IF

            WRITE (*,*)
            WRITE (*,*) LINE

         END IF
         LINE = ' '
         WRITE (*,*) LINE(1:1)

         DO ITASK = 1, NOPT

            IF ( OPTNAM(ITASK) .NE. ' ' ) THEN
               MYOPT = '( ' &
               //      OPTNAM(ITASK)(1:NAMLEN) &
               //      ' ) ' &
               //      OPTTXT(ITASK)
            ELSE
               MYOPT = SPACE(1:NAMLEN + 5) &
               //      OPTTXT(ITASK)
            END IF


            IF ( OINDNT .GT. 0 ) THEN
               LINE = SPACE(1:OINDNT) &
               //     MYOPT
            ELSE
               LINE = MYOPT
            END IF

            WRITE (*,*) LINE(1:RTRIM(LINE))

         END DO

!
!        Initialize the task indicator to zero, invalid task.
!
         ITASK  = 0

         WRITE (*,*)
         CALL PROMPT ( SPACE(1:OINDNT) // 'Option: ', LINE )

         IF ( LINE .NE. ' ' ) THEN

            CALL LJUST( LINE, LINE )
            CALL UCASE( LINE, LINE )

            ITASK = ISRCHC( LINE, NOPT, OPTNAM)

            IF ( ITASK .EQ. 0 ) THEN

               MSG =  STARS &
               //    ' ''#'' was not a valid option.' &
               //    ' Please try again.'
               CALL REPMC ( MSG, '#', LINE, MSG )
               WRITE (*,*)
               WRITE (*,*) STARS
               WRITE (*,*) MSG(:RTRIM(MSG))
               WRITE (*,*) STARS

            ELSE

               OPTION = ITASK
               DONE = .TRUE.

            END IF

         END IF

      END DO

      CALL CHKOUT ( 'GETOPT_2' )
      RETURN
      END
