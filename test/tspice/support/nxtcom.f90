!$Procedure      NXTCOM ( Next command )

      SUBROUTINE NXTCOM ( PROMPT, DELIM, COMMND, SOURCE )
      IMPLICIT NONE

! $ Abstract
!
!     Get the next command from the keyboard or a file.
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
!     FILES
!     PARSING
!
! $ Declarations

      CHARACTER*(*)         PROMPT
      CHARACTER*(*)         DELIM
      CHARACTER*(*)         COMMND
      INTEGER               SOURCE

! $ Brief_I/O
!
!     Variable  I/O  Entry
!     --------  ---  --------------------------------------------------
!     PROMPT     I   SETDAP
!     DELIM      I   SETDAP
!     COMMND     O   GETCOM
!     SOURCE     O   GETCOM
!
! $ Detailed_Input
!
!     See the ENTRY points for a discussion of their arguments.
!
! $ Detailed_Output
!
!     See the ENTRY points for a discussion of their arguments.
!
! $ Files
!
!     If the commands are contained in a file, they will be read from
!     that file. (The 'START' keyword indicates that commands are to
!     be read from a specified file.) If they are not contained in a
!     file, they are read from the keyboard.
!
! $ Exceptions
!
!     1) If NXTCOM is called directly, the error SPICE(BOGUSENTRY) is
!        signalled.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     The delimeter has a fixed length of one character. The prompt has
!     a fixed length of eighty characters.
!
!     The file name length has been parameterized internally to the
!     maximum file name length length on the VAX, 128 characters.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     H.A. Neilan    (JPL)
!
! $ Version
!
!-     Commad Loop Version 2.0.0              19-NOV-1995 (WLT)
!
!         Added the batch mode capability.  If the BATCH function
!         returns TRUE then all keyboard routines return EXIT.
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!-    Beta Version 1.0.0, 02-DEC-1988 (HAN)
!
!-&

      INCLUDE           'commdpar.inc'

!
!     SPICELIB functions
!
      INTEGER               BRCKTI
      LOGICAL               BATCH
      LOGICAL               RETURN

!
!     Below are the various sources from which
!     commands might come.
!
!     NONE
!     COMBUF
!     KEYBRD
!     INPFIL
!
      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )

      INTEGER               COMBUF
      PARAMETER           ( COMBUF = NONE   + 1 )

      INTEGER               KEYBRD
      PARAMETER           ( KEYBRD = COMBUF + 1 )

      INTEGER               INPFIL
      PARAMETER           ( INPFIL = KEYBRD + 1 )


!
!     Local variables
!


      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )


      CHARACTER*(     1)    SAVDLM
      CHARACTER*(    80)    SAVPMT
      CHARACTER*(    80)    WORD
      CHARACTER*(   300)    ERROR
      CHARACTER*(   300)    LNGMSG
      CHARACTER*(COMSIZ)    BUFFER (  MAXCOM )
      CHARACTER*(FILEN )    FILE
      CHARACTER*(FILEN )    REST
      CHARACTER*(WDSIZE)    EXIT
      CHARACTER*(WDSIZE)    SHTMSG
      CHARACTER*(WDSIZE)    START
      CHARACTER*(WDSIZE)    STOP

      INTEGER               BUFFED
      INTEGER               BUFSRC (  MAXCOM )

      LOGICAL               FIRST
      LOGICAL               NOCOM
      LOGICAL               READNG

      SAVE
!
!     Initial values
!
      DATA                  BUFFED        /  0        /
      DATA                  FIRST         / .TRUE.    /
      DATA                  READNG        / .FALSE.   /
      DATA                  SAVDLM        / ';'       /
      DATA                  SAVPMT        / ' '       /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NXTCOM' )
      END IF


!
!     This routine should never be called. If this routine is called,
!     an error is signalled.
!
      CALL SETMSG ( 'NXTCOM: You have called an entry which performs '// &
                    'no run-time function. This may indicate a bug. ' // &
                    'Please check the documentation for the '         // &
                    'subroutine NXTCOM.'  )

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )

      CALL CHKOUT ( 'NXTCOM' )
      RETURN




!$Procedure GETCOM ( Get a command )

      ENTRY GETCOM ( COMMND, SOURCE )

! $ Abstract
!
!     Get a command from a file or the keyboard.
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
!     FILES
!     PARSING
!
! $ Declarations
!
!     CHARACTER*(*)         COMMND
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     COMMND     O   A command read from a file or from the screen.
!     SOURCE     O   The source of the command, file, terminal etc.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     COMMND     is the command which is parsed from a line read from
!                either the screen or a file.
!
!     SOURCE     Is an integer that indicates the source of the
!                command.  The table below shows the various
!                values and their meanings.
!
!                   0  ---   None, an error occurred
!                   1  ---   Command buffer
!                   2  ---   From standard input
!                   3  ---   From a STARTED File.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     If an error occurs in PRSTRT, the error SPICE(FILEREADFAILED)
!     is signalled. ( PRSTRT has not been modified to participate in the
!     new error handling. )
!
! $ Particulars
!
!     None.
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
!     H.A. Neilan    (JPL)
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!-    Beta Version 1.0.0, 29-NOV-1988 (HAN)
!
!-&



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'GETCOM' )
      END IF

      IF ( FIRST ) THEN
         CALL TRNLAT ( 'STOP',      STOP   )
         CALL TRNLAT ( 'EXIT',      EXIT   )
         CALL TRNLAT ( 'START',     START  )
         CALL TRNLAT ( 'DEFPROMPT', SAVPMT )

         FIRST = .FALSE.
      END IF

!
!     While we don't have a command, try to get one.  We look
!     in the command buffer first.
!

!
!     We don't have a command yet.
!
      NOCOM = .TRUE.


      DO WHILE ( NOCOM )

         IF ( BUFFED .GT. 0 ) THEN

            COMMND = BUFFER (  BUFFED   )
            SOURCE = BUFSRC (  BUFFED  )
            BUFFED = BUFFED -  1

         ELSE

!
!           If we're already reading from a file then just let PRREAD
!           take care of obtaining the command. If PRREAD reaches the
!           end of the current file, the previous file is popped off
!           the stack, and the next command from this file is read
!           instead. (If no files remain to be read, DELIM is returned.)
!           In that case we are no longer reading from files.
!
            IF ( READNG ) THEN

               CALL PRREAD ( SAVDLM, COMMND )

               SOURCE = INPFIL

               IF ( COMMND .EQ. SAVDLM ) THEN
               READNG = .FALSE.
               END IF

            END IF


!
!           If we're not reading from a file, get the command from the
!           keyboard. ( If the command was terminated by a blank line,
!           the command is returned as a blank. )
!
            IF ( .NOT. READNG ) THEN

               IF ( BATCH() ) THEN
                  COMMND = EXIT
               ELSE
                  CALL RDSTMT ( SAVPMT, SAVDLM, COMMND )
               END IF

               SOURCE = KEYBRD

            END IF

         END IF

!
!        We must have a command at this point.
!
         NOCOM = .FALSE.

!
!        We need to check to see if what we have is a control word.
!
         CALL NEXTWD ( COMMND, WORD, REST )
         CALL UCASE  ( WORD,   WORD       )

!
!        If the control word is 'START', we know that we will be
!        reading from a file. Let PRSTRT take care of keeping track of
!        the files being read from. If there's a problem in PRSTRT we
!        need to signal an error here due to PRSTRT's error handling.
!        Bail out if there's a problem. If all goes well in PRSTR,
!        we will read the first command in the file the next pass
!        through the DO LOOP.
!

         IF ( WORD .EQ. START ) THEN
!
!           We need to log this command commented out so that anyone
!           using the resulting log file, will not have to worry
!           about starting a file twice.
!
            CALL NSPLOG ( COMMND, .TRUE. )

            FILE = ' '
            CALL NEXTWD ( REST,  FILE, REST )


            IF ( FILE .EQ. ' ' ) THEN

               SOURCE = NONE

               CALL TRNLAT ( 'MISSINGFILELONG',  LNGMSG )
               CALL TRNLAT ( 'MISSINGFILESHORT', SHTMSG )
               CALL SETMSG ( LNGMSG    )
               CALL SIGERR ( SHTMSG    )
               CALL CHKOUT ( 'GETCOM'  )
               RETURN

            END IF

            CALL PRSTRT ( FILE , ERROR      )

!
!           If an error occurs in PRSTRT we're in trouble. Signal an
!           error and bail. If there's no problem, we're now reading
!           from a file.
!
            IF ( ERROR .NE. ' ' ) THEN

               SOURCE = NONE

               CALL TRNLAT (  'MISSINGFILESHORT', SHTMSG )
               CALL SETMSG ( ERROR    )
               CALL SIGERR ( SHTMSG   )
               CALL CHKOUT ( 'GETCOM' )
               RETURN
            ELSE
               READNG = .TRUE.
               NOCOM  = .TRUE.
            END IF

!
!        If the control word is 'STOP', clear the stack of files.
!        If we were reading commands from files, we won't be anymore.
!        If we were reading commands from the keyboard, the command to
!        return is 'STOP'.
!

         ELSE IF ( WORD .EQ. STOP ) THEN


            IF ( READNG ) THEN
               CALL PRCLR
               CALL NSPLOG ( COMMND, .TRUE. )
               READNG = .FALSE.
               NOCOM  = .TRUE.
            ELSE
               COMMND = WORD
            END IF

!
!        If the control word is 'EXIT', and we're reading from a file,
!        we need to remove that file from the stack. If we're reading
!        commands from the keyboard, we'll return the command 'EXIT'.
!
          ELSE IF ( WORD .EQ. EXIT ) THEN

             IF ( READNG ) THEN
                CALL PREXIT
                CALL NSPLOG ( COMMND, .TRUE. )
                NOCOM  = .TRUE.
             ELSE
                COMMND = WORD
             END IF

          END IF


      END DO


      CALL CHKOUT ( 'GETCOM' )
      RETURN



!$Procedure SETDAP ( Set the delimeter and prompt values )

      ENTRY SETDAP ( DELIM, PROMPT )

! $ Abstract
!
!     Set the delimeter and prompt values that are used for parsing
!     commands.
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
!     CHARACTER
!     PARSING
!
! $ Declarations
!
!     CHARACTER*1           DELIM
!     CHARACTER*80          PROMPT
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     DELIM     I/O  Character delimiting the end of a command.
!     PROMPT    I/O  Character string indicating the beginning of a
!                    command.
!
! $ Detailed_Input
!
!     DELIM      is a single character delimiting the end of a command.
!                The default value of DELIM is ';'.
!
!     PROMPT     is a character string indicating the beginning of a
!                command. PROMPT has a maximum length of eighty
!                characters. The default value of PROMPT is 'Next? >'.
!
! $ Detailed_Output
!
!     DELIM      is the new character delimiting the end of a command.
!
!     PROMPT     is the new character string indicating the beginning
!                of a command. PROMPT has a maximum length of eighty
!                characters.
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
!     DELIM and PROMPT have the default values of ';' and 'Next? >'
!     respectively. This module is called in order to change their
!     values.
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
!     H.A. Neilan    (JPL)
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!-    Beta Version 1.0.0, 02-DEC-1988 (HAN)
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SETDAP' )
      END IF


!
!     Set the values of the delimeter and prompt.
!
      SAVDLM = DELIM
      SAVPMT = PROMPT

      CALL TRNLAT ( 'STOP',  STOP  )
      CALL TRNLAT ( 'EXIT',  EXIT  )
      CALL TRNLAT ( 'START', START )

      IF ( SAVPMT .EQ. ' ' ) THEN
         CALL TRNLAT ( 'DEFPROMPT', SAVPMT )
      END IF

      FIRST = .FALSE.

      CALL CHKOUT ( 'SETDAP' )
      RETURN

!
! $ Procedure
!
      ENTRY PUTCOM ( COMMND, SOURCE )
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!

      IF ( BUFFED .LT. MAXCOM ) THEN

         BUFFED            = BUFFED + 1
         BUFFER ( BUFFED ) = COMMND
         BUFSRC ( BUFFED ) = BRCKTI ( SOURCE, NONE, INPFIL )
         RETURN

      END IF
!
!     If you get to this point there's a problem.  No room
!     is left in the command buffer.
!
      CALL CHKIN  ( 'PUTCOM'                )
      CALL TRNLAT ( 'COMBUFFULLLNG', LNGMSG )
      CALL TRNLAT ( 'COMBUFFULLSHT', SHTMSG )
      CALL SETMSG (  LNGMSG                 )
      CALL SIGERR (  SHTMSG                 )
      CALL CHKOUT ( 'PUTCOM'                )
      RETURN


      END
