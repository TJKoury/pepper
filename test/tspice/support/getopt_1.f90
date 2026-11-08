! $ Procedure      GETOPT_1 ( Get option string from a specified list )
!
      SUBROUTINE GETOPT_1( TITLE, NOPT, OPTNAM, NAMLEN, OPTTXT, TXTLEN, &
                           OPTVAL, OPTION )
      IMPLICIT NONE
!
! $ Abstract
!
!     Display a list of options in a standard menu format, and get
!     an option from a user returning the corresponding value from
!     a specified list of option values.
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
!     None.
!
! $ Declarations

      CHARACTER*(*)         TITLE
      INTEGER               NOPT
      CHARACTER*(*)         OPTNAM(*)
      INTEGER               NAMLEN
      CHARACTER*(*)         OPTTXT(*)
      INTEGER               TXTLEN
      CHARACTER*(*)         OPTVAL(*)
      CHARACTER*(*)         OPTION

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
      INTEGER               RTRIM
      INTEGER               ISRCHC
      LOGICAL               RETURN
!
!     Local variables
!
      CHARACTER*(*)         STARS
      PARAMETER           ( STARS = '*****' )

      CHARACTER*(*)         TOVR
      PARAMETER           ( TOVR = '    ' )

      CHARACTER*(*)         TTOVR
      PARAMETER           ( TTOVR = '        ' )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )
!
!     Local variables
!
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(LINLEN)    MSG

      INTEGER               ITASK

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
         CALL CHKIN ( 'GETOPT_1' )
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
         CALL CHKOUT ( 'GETOPT_1' )
         RETURN

      END IF
!
!     Check to make sure that the length of the option names is at
!     least 1. If not, then signal an error with an appropriate error
!     message.
!
      IF ( NAMLEN .LT. 1 ) THEN

         CALL SETMSG ( 'The length of the option names was not' // &
                       ' positive: #.' )
         CALL ERRINT ( '#', NAMLEN )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)' )
         CALL CHKOUT ( 'GETOPT_1' )
         RETURN

      END IF
!
!     Check to make sure that the length of the descriptive text for
!     each option is at least 1. If not, then signal an error with an
!     appropriate error message.
!
      IF ( TXTLEN .LT. 1 ) THEN

         CALL SETMSG ( 'The length of the option descriptions was' // &
                       ' not positive: #.' )
         CALL ERRINT ( '#', TXTLEN )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)' )
         CALL CHKOUT ( 'GETOPT_1' )
         RETURN

      END IF
!
!     Do until we get an option
!
      DONE = .FALSE.
      DO WHILE ( .NOT. DONE )
!
!        Display the menu title if it is non blank
!
         IF ( TITLE .NE. ' ' ) THEN

            LINE = TTOVR // TTOVR // TITLE
            WRITE (*,*)
            WRITE (*,*) LINE(:RTRIM(LINE))

         END IF
!
!        Display the menu and read in an option.
!
         WRITE (*,*)

         DO ITASK = 1, NOPT

            LINE = TTOVR                      // &
                   '( '                       // &
                   OPTNAM(ITASK)(1:NAMLEN)    // &
                   ' ) '                      // &
                   OPTTXT(ITASK)(1:TXTLEN)
            WRITE (*,*) LINE(:RTRIM(LINE))

         END DO

!
!        Initialize the task indicator to zero, invalid task.
!
         ITASK  = 0

         WRITE (*,*)
         CALL PROMPT ( TOVR // 'Option: ', LINE )

         IF ( LINE .NE. ' ' ) THEN

            CALL LJUST ( LINE, LINE )
            CALL UCASE ( LINE, LINE )

            ITASK = ISRCHC ( LINE(1:NAMLEN), NOPT, OPTNAM)

            IF ( ITASK .EQ. 0 ) THEN

               MSG = '''#'' was not a valid option.' // &
                     ' Please try again.'
               CALL REPMC ( MSG, '#', LINE, MSG )
               WRITE (*,*)
               LINE = TOVR // STARS
               WRITE (*,*) LINE(:RTRIM(LINE))
               LINE = TOVR // STARS // ' ' // MSG(:RTRIM(MSG))
               WRITE (*,*) LINE(:RTRIM(LINE))
               LINE = TOVR // STARS
               WRITE (*,*) LINE(:RTRIM(LINE))

            ELSE

               OPTION = OPTVAL(ITASK)
               DONE   = .TRUE.

            END IF

         END IF

      END DO

      CALL CHKOUT ( 'GETOPT_1' )
      RETURN
      END
