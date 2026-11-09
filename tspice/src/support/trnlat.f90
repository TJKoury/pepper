      SUBROUTINE TRNLAT ( PHRASE,  MESSGE )
      IMPLICIT NONE
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!


!
!     This is a language dependent routine.
!

      CHARACTER*(*)         PHRASE
      CHARACTER*(*)         MESSGE
!
!     Spicelib functions
!
      INTEGER               BSRCHC

!
!     Local parameters and variables
!

      INTEGER               NTITLE
      PARAMETER           ( NTITLE = 28 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               MSGSIZ
      PARAMETER           ( MSGSIZ = 400 )

      CHARACTER*(MSGSIZ)    ENGLSH ( NTITLE )
      CHARACTER*(MSGSIZ)    FRENCH ( NTITLE )
      CHARACTER*(MSGSIZ)    GERMAN ( NTITLE )
      CHARACTER*(MSGSIZ)    RUSSAN ( NTITLE )
      CHARACTER*(WDSIZE)    LANG
      CHARACTER*(WDSIZE)    TITLE  ( NTITLE )

      INTEGER               IORDER ( NTITLE )
      INTEGER               ITEM

      LOGICAL               FIRST
      SAVE

      DATA                  FIRST /.TRUE./


      IF ( FIRST ) THEN

         FIRST      = .FALSE.

         TITLE (1) = 'ERRFLAG'

         ENGLSH(1) = '-Oops!-'
         FRENCH(1) = '--%-Sacre^Bleu!!-%^^:'
         GERMAN(1) = '--%-Achtung!!-%^^:'
         RUSSAN(1) = '--%-ERROR-%^^:'

         TITLE  (2) = 'EXIT'

         ENGLSH (2) = 'EXIT'
         FRENCH (2) = 'SORTIE'
         GERMAN (2) = 'EXIT'
         RUSSAN (2) = 'EXIT'

         TITLE  (3) = 'START'

         ENGLSH (3) = 'START'
         FRENCH (3) = 'COMMENCER'
         GERMAN (3) = 'START'
         RUSSAN (3) = 'START'

         TITLE  (4) = 'STOP'

         ENGLSH (4) = 'STOP'
         FRENCH (4) = 'ARETE'
         GERMAN (4) = 'STOP'
         RUSSAN (4) = 'STOP'

         TITLE  (5) = 'DEMO'

         ENGLSH (5) = 'DEMO'
         FRENCH (5) = 'MONTRER'
         GERMAN (5) = 'DEMO'
         RUSSAN (5) = 'DEMO'

         TITLE  (6) = 'PAUSE'

         ENGLSH (6) = 'PAUSE'
         FRENCH (6) = 'PAUSE'
         GERMAN (6) = 'PAUSE'
         RUSSAN (6) = 'PAUSE'

         TITLE  (7) = 'WAIT'

         ENGLSH (7) = 'WAIT'
         FRENCH (7) = 'ATTENDRE'
         GERMAN (7) = 'WAIT'
         RUSSAN (7) = 'WAIT'

         TITLE  (8) = 'QUIT'

         ENGLSH (8) = 'QUIT'
         FRENCH (8) = 'ARETE'
         GERMAN (8) = 'QUIT'
         RUSSAN (8) = 'QUIT'

         TITLE  (9) = 'DEFPROMPT'

         ENGLSH(9) = 'Yes? >'
         FRENCH(9) = 'Oui? >'
         GERMAN(9) = 'Ja? >'
         RUSSAN(9) = 'Dah? >'

         TITLE (10) = 'MISSINGFILELONG'

         ENGLSH(10) = 'No command sequence file was specified in ' &
         //           'the START command. '
         FRENCH(10) = 'Le fichier command sequence n''est pas ' &
         //           'present dans le command "COMMENCER". '

         GERMAN(10) = 'No command sequence file was specified in ' &
         //           'the START command. '
         RUSSAN(10) = 'No command sequence file was specified in ' &
         //           'the START command. '


         TITLE (11) = 'MISSINGFILESHORT'

         ENGLSH(11) = 'Missing_File_Name'
         FRENCH(11) = 'Nom_de_fichier_abscent'
         GERMAN(11) = 'Missing_File_Name'
         RUSSAN(11) = 'Missing_File_Name'

         TITLE (12) = 'UNABLETOSTART'

         ENGLSH(12) = 'Unable_To_Start_File'
         FRENCH(12) = 'Unable_To_Start_File'
         GERMAN(12) = 'Unable_To_Start_File'
         RUSSAN(12) = 'Unable_To_Start_File'

         TITLE (13) = 'COMBUFFULLLNG'

         ENGLSH(13) = 'The designer of the program has ' &
         //           'inadvertantly exceeded the internal ' &
         //           'command buffer.  Please keep your session ' &
         //           'log and report this problem to NAIF. '
         FRENCH(13) = 'The designer of the program has ' &
         //           'inadvertantly exceeded the internal ' &
         //           'command buffer.  Please keep your session ' &
         //           'log and report this problem to NAIF. '
         GERMAN(13) = 'The designer of the program has ' &
         //           'inadvertantly exceeded the internal ' &
         //           'command buffer.  Please keep your session ' &
         //           'log and report this problem to NAIF. '
         RUSSAN(13) = 'The designer of the program has ' &
         //           'inadvertantly exceeded the internal ' &
         //           'command buffer.  Please keep your session ' &
         //           'log and report this problem to NAIF. '

         TITLE (14) = 'COMBUFFULLSHT'

         ENGLSH(14) = 'Command_Buffer_Full'
         FRENCH(14) = 'Command_Buffer_Full'
         GERMAN(14) = 'Command_Buffer_Full'
         RUSSAN(14) = 'Command_Buffer_Full'

         TITLE (15) = 'NESTINGTOODEEP'

         ENGLSH(15) = 'The command sequence contained in # could ' &
         //           'not be started. There are already # ' &
         //           'command sequences files that have been ' &
         //           'started without resolution. This is the ' &
         //           'limit on the number of active command ' &
         //           'sequence files that can be active at any ' &
         //           'time. '
         FRENCH(15) = 'The command sequence contained in # could ' &
         //           'not be started. There are already # ' &
         //           'command sequences files that have been ' &
         //           'started without resolution. This is the ' &
         //           'limit on the number of active command ' &
         //           'sequence files that can be active at any ' &
         //           'time. '
         GERMAN(15) = 'The command sequence contained in # could ' &
         //           'not be started. There are already # ' &
         //           'command sequences files that have been ' &
         //           'started without resolution. This is the ' &
         //           'limit on the number of active command ' &
         //           'sequence files that can be active at any ' &
         //           'time. '
         RUSSAN(15) = 'The command sequence contained in # could ' &
         //           'not be started. There are already # ' &
         //           'command sequences files that have been ' &
         //           'started without resolution. This is the ' &
         //           'limit on the number of active command ' &
         //           'sequence files that can be active at any ' &
         //           'time. '


         TITLE (16) = 'NOLOGUNITSFREE'

         ENGLSH(16) = 'The command sequence contained in # could ' &
         //           'not be started. There are no FORTRAN ' &
         //           'logical units available that can be ' &
         //           'attached to the file. A possible cause ' &
         //           'for this problem is that there are too ' &
         //           'many files already in use by the program. '
         FRENCH(16) = 'The command sequence contained in # could ' &
         //           'not be started. There are no FORTRAN ' &
         //           'logical units available that can be ' &
         //           'attached to the file. A possible cause ' &
         //           'for this problem is that there are too ' &
         //           'many files already in use by the program. '
         GERMAN(16) = 'The command sequence contained in # could ' &
         //           'not be started. There are no FORTRAN ' &
         //           'logical units available that can be ' &
         //           'attached to the file. A possible cause ' &
         //           'for this problem is that there are too ' &
         //           'many files already in use by the program. '
         RUSSAN(16) = 'The command sequence contained in # could ' &
         //           'not be started. There are no FORTRAN ' &
         //           'logical units available that can be ' &
         //           'attached to the file. A possible cause ' &
         //           'for this problem is that there are too ' &
         //           'many files already in use by the program. '


         TITLE (17) = 'FILENOTEXIST'

         ENGLSH(17) = 'The file "#" could not be started. It ' &
         //           'doesn''t exist. '
         FRENCH(17) = 'The file "#" could not be started. It ' &
         //           'doesn''t exist. '
         GERMAN(17) = 'The file "#" could not be started. It ' &
         //           'doesn''t exist. '
         RUSSAN(17) = 'The file "#" could not be started. It ' &
         //           'doesn''t exist. '


         TITLE (18) = 'COMFILEOPENERROR'

         ENGLSH(18) = 'The command sequence contained in # could ' &
         //           'not be started. An error occurred while ' &
         //           'attempting to open the file. '
         FRENCH(18) = 'The command sequence contained in # could ' &
         //           'not be started. An error occurred while ' &
         //           'attempting to open the file. '
         GERMAN(18) = 'The command sequence contained in # could ' &
         //           'not be started. An error occurred while ' &
         //           'attempting to open the file. '
         RUSSAN(18) = 'The command sequence contained in # could ' &
         //           'not be started. An error occurred while ' &
         //           'attempting to open the file. '


         TITLE (19) = 'LOGFILWRITTENTO'

         ENGLSH(19) = 'The log file has been written to: '
         FRENCH(19) = 'Le fichier de log s''est ecrivee : '
         GERMAN(19) = 'Das logenfile hass bin written to: '
         RUSSAN(19) = 'The log file has been written to: '

         TITLE (20) = 'SAVFILWRITTENTO'

         ENGLSH(20) = 'The save file has been written to: '
         FRENCH(20) = 'Le fichier de garde s''est ecrivee : '
         GERMAN(20) = 'Das savenfile hass bin written to: '
         RUSSAN(20) = 'The save file has been written to: '

         TITLE (21) = 'UNABLETOWRITETOFILE'

         ENGLSH(21) = 'I was unable to write to the file: ' &
         //           '/cr/cr(3:3) # /cr/cr(-3;-3) The value of ' &
         //           'IOSTAT that was returned as a diagnosis ' &
         //           'of the problem was: /cr/cr(3:3) # ' &
         //           '/cr/cr(-3;-3) This file is now closed. No ' &
         //           'further attempts will be made to write to ' &
         //           'it. '
         FRENCH(21) = 'I was unable to write to the file: ' &
         //           '/cr/cr(3:3) # /cr/cr(-3;-3) The value of ' &
         //           'IOSTAT that was returned as a diagnosis ' &
         //           'of the problem was: /cr/cr(3:3) # ' &
         //           '/cr/cr(-3;-3) This file is now closed. No ' &
         //           'further attempts will be made to write to ' &
         //           'it. '
         GERMAN(21) = 'I was unable to write to the file: ' &
         //           '/cr/cr(3:3) # /cr/cr(-3;-3) The value of ' &
         //           'IOSTAT that was returned as a diagnosis ' &
         //           'of the problem was: /cr/cr(3:3) # ' &
         //           '/cr/cr(-3;-3) This file is now closed. No ' &
         //           'further attempts will be made to write to ' &
         //           'it. '
         RUSSAN(21) = 'I was unable to write to the file: ' &
         //           '/cr/cr(3:3) # /cr/cr(-3;-3) The value of ' &
         //           'IOSTAT that was returned as a diagnosis ' &
         //           'of the problem was: /cr/cr(3:3) # ' &
         //           '/cr/cr(-3;-3) This file is now closed. No ' &
         //           'further attempts will be made to write to ' &
         //           'it. '


         TITLE (22) = 'WARNING'

         ENGLSH(22) = 'Warning:'
         FRENCH(22) = 'Attention: '
         GERMAN(22) = 'Achtung: '
         RUSSAN(22) = 'Hey!! '

         TITLE (23) = 'CANNOTOPENLOG'

         ENGLSH(23) = 'An error occurred while attempting to ' &
         //           'open the log file. It will not be ' &
         //           'possible to keep a log of this session. ' &
         //           'No further attempts to log commands will ' &
         //           'be attempted. /cr/cr The cause of the ' &
         //           'failure to open the log file was ' &
         //           'diagnosed to be: /cr/cr(3:3) '
         FRENCH(23) = 'An error occurred while attempting to ' &
         //           'open zee log file. It will not be ' &
         //           'possible to keep a log of this session. ' &
         //           'No further attempts to log commands will ' &
         //           'be attempted. /cr/cr Zee cause of zee ' &
         //           'failure to open zee log file was ' &
         //           'diagnosed to be: /cr/cr(3:3) '
         GERMAN(23) = 'An error occurred while attempting to ' &
         //           'open the log file. It will not be ' &
         //           'possible to keep a log of this session. ' &
         //           'No further attempts to log commands will ' &
         //           'be attempted. /cr/cr The cause of the ' &
         //           'failure to open the log file was ' &
         //           'diagnosed to be: /cr/cr(3:3) '
         GERMAN(23) = 'An error occurred while attempting to ' &
         //           'open the log file. It will not be ' &
         //           'possible to keep a log of this session. ' &
         //           'No further attempts to log commands will ' &
         //           'be attempted. /cr/cr The cause of the ' &
         //           'failure to open the log file was ' &
         //           'diagnosed to be: /cr/cr(3:3) '


         TITLE (24) = 'NOMOREDIAGNOSTICS'

         ENGLSH(24) = 'Sorry, no further diagnostics are available.'
         FRENCH(24) = 'Mon ami, I am so sorry. I can say no more' &
         //           ' about zee error I reported earlier.'
         GERMAN(24) = 'No further diagnostics are available.'
         RUSSAN(24) = 'Sorry, no further diagnostics are available.'

         TITLE (25) = 'DONT'

         ENGLSH(25) = 'NO'
         FRENCH(25) = 'NO'
         GERMAN(25) = 'NEIN'
         RUSSAN(25) = 'NYET'

         TITLE (26) = 'ECHO'

         ENGLSH(26) = 'ECHO'
         FRENCH(26) = 'ECHO'
         GERMAN(26) = 'ECHO'
         RUSSAN(26) = 'ECHO'

         TITLE (27) = 'ERRFILWRITTENTO'

         ENGLSH(27) = 'The error file has been written to: '
         FRENCH(27) = 'The error file has been written to: '
         GERMAN(27) = 'The error file has been written to: '
         RUSSAN(27) = 'The error file has been written to: '

         TITLE (28) = 'ERRFILWRITEFAIL'

         ENGLSH(28) = 'WARNING--Unable to create the errorfile: '
         FRENCH(28) = 'WARNING--Unable to create the errorfile: '
         GERMAN(28) = 'WARNING--Unable to create the errorfile: '
         RUSSAN(28) = 'WARNING--Unable to create the errorfile: '

         CALL ORDERC ( TITLE,  NTITLE, IORDER )
         CALL REORDC ( IORDER, NTITLE, TITLE  )
         CALL REORDC ( IORDER, NTITLE, ENGLSH )
         CALL REORDC ( IORDER, NTITLE, FRENCH )
         CALL REORDC ( IORDER, NTITLE, GERMAN )
         CALL REORDC ( IORDER, NTITLE, RUSSAN )

      END IF

      ITEM   =  BSRCHC ( PHRASE, NTITLE, TITLE )
!
!     Look up the current language to be used.
!
      CALL GETLAN ( LANG )

      IF  ( ITEM .EQ. 0 ) THEN
         MESSGE = PHRASE

      ELSE IF ( LANG .EQ. 'FRENCH' ) THEN
         MESSGE = FRENCH(ITEM)
      ELSE IF ( LANG .EQ. 'GERMAN' )  THEN
         MESSGE = GERMAN(ITEM)
      ELSE IF ( LANG .EQ. 'RUSSIAN' ) THEN
         MESSGE = RUSSAN(ITEM)
      ELSE
         MESSGE = ENGLSH(ITEM)
      END IF

      RETURN


      END
