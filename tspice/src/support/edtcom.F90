!$Procedure      EDTCOM (Edit a command)

      SUBROUTINE EDTCOM ( DELIM, PROMPT, COMMND, SOURCE )

! $ Abstract
!
!     This entry point allows the user of a program to fetch
!     previously entered commands, review them, re-execute the commands
!     or edit and re-execute the command.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      CHARACTER*(1)         DELIM
      CHARACTER*(*)         PROMPT
      CHARACTER*(*)         COMMND
      INTEGER               SOURCE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DELIM      I   is the character used to delimit the command ends
!     COMMND    I/O  is a command to process
!     SOURCE    I/O  indicates the source of the command
!
! $ Detailed_Input
!
!     DELIM        is the character used to delimit input commands.
!                  A command begins at the first non-blank character
!                  of COMMND and continues until the last non-blank
!                  character or the first occurrence of DELIM which
!                  ever is first.
!
!
!     COMMND       is a string that indicates some action the program
!                  should take.  The only commands that have meaning
!                  to this routine are those of the form:
!
!                    RECALL @int(1:20)
!
!                    RECALL ALL
!
!                    DO     @int(1:20)
!
!                    EDIT   @int(1:20)
!
!                  all other commands are ignored by this routine.
!                  (See the META/2 language specification language
!                  for a more detailed description of the meaning
!                  of the syntax specifications given above.)
!
!     SOURCE       is an integer indicating where the input command
!                  came from.  Unless SOURCE has a value of 2 meaning
!                  the command was typed interactively, no action
!                  is taken by this routine.
!
! $ Detailed_Output
!
!     COMMND       if the input command is recognized by this routine
!                  COMMND will be set to all blank characters.
!                  Otherwise, COMMND will remain unchanged.
!
!     SOURCE       if the input command is recognized by this routine
!                  SOURCE will be set to zero indicating that there
!                  is no longer a potential command in the string
!                  COMMND.  SOURCE will remain unchanged.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     In the case of a command of the form EDIT @int(1:20) this routine
!     must be able to create a file that the editor can edit.
!
!     If this cannot be done one of the following errors will be
!     signaled.
!
!     1)   If the program cannot create a new file name that
!          could hold the command to be edited, the error
!          COMLOOP(NOFILECREATION) will be signaled.
!
!     2)   If a new file name could be created but the file could
!          not be opened, the error COMLOOP(COMMANDEDITFAILED)
!          will be signaled.
!
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is provided so that command line programs
!     may obtain a history of commands that have been entered
!     into the program and possible re-execute or edit and execute
!     the previous commands.  This is meant to be integrated
!     with the basic command loop software available for
!     constructing command driven programs.  See the routine
!     CMLOOP to see how this fits into the general sequence of
!     command processing.
!
! $ Examples
!
!     See CMLOOP for the intended use of this routine.
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
!     H.A. Neilan     (JPL)
!     W.L. Taber      (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.30.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 1.29.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 1.28.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 1.27.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 1.26.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 1.24.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 1.23.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 1.22.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 1.21.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.14.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 1.13.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 1.12.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 1.11.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 1.10.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 1.9.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 1.8.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 1.7.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 1.6.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 1.5.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 1.4.5, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 1.4.4, 08-OCT-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  New
!        environments are WIN-NT
!
!-    SPICELIB Version 1.4.3, 20-SEP-1999 (NJB)
!
!        CSPICE and PC-LINUX environment lines were added.  Some
!        typos were corrected.
!
!-    SPICELIB Version 1.4.2, 28-JUL-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  New
!        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
!
!-    SPICELIB Version 1.4.1, 18-MAR-1999 (WLT)
!
!        The environment lines were expanded so that the supported
!        environments are now explicitly given.  Previously,
!        environments such as SUN-SUNOS and SUN-SOLARIS were implied
!        by the environment label SUN.
!
!-    SPICELIB Version 1.4.0, 9-JAN-1997 (WLT)
!
!        Added minimal support for the MAC version. When the user
!        requests EDIT, the routine displays the matching item
!        that should be edited.  This is the only option available
!        at the moment on the MAC.  When something better comes along
!        we'll do something more creative.
!
!-    SPICELIB Version 1.3.0, 5-Dec-1995 (WLT)
!
!        Fixed the bug that occurred if you typed RECALL x and
!        there was no matching command (probably should have done
!        thins in version 1.2.0) EDTCOM now pronounces that this
!        is an error.
!
!-    SPICELIB Version 1.2.0, 11-SEP-1995 ( WLT )
!
!        Fixed the bug that occurred if you type EDIT x or
!        DO x and there was no matching command in the history
!        list.  EDTCOM no pronounces that this is an error.
!
!-    SPICELIB Version 1.1.0, 1-JUN-1995 (HAN)
!
!        Created the master source file for VAX/VMS, Alpha/OpenVMS,
!        Sun (Sun OS 4.1.x and Solaris), PC(Microsoft Fortran), HP,
!        and NeXT.
!
!-    Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!        This is the configured version of the Command Loop
!        software as of May 4, 1994
!
!-    SPICELIB Version 1.0.0, 18-APR-1994 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Recall Re-execute or edit and re-execute a command
!
!-&

! $ Revisions
!
!-    SPICELIB Version 1.1.0, 1-JUN-1995 (HAN)
!
!        Created the master source file for VAX/VMS, Alpha/OpenVMS,
!        Sun (Sun OS 4.1.x and Solaris), PC(Microsoft Fortran), HP,
!        and NeXT.
!
!-&

!
!     Spicelib Functions
!
      INTEGER               RTRIM
      INTEGER               CARDC
      LOGICAL               MATCH
      LOGICAL               FAILED
      LOGICAL               HAVE

!
!     Meta/2 functions
!
      LOGICAL               M2WMCH


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
!     Local parameters used for allocating space and controlling loop
!     execution.
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 132 )

      INTEGER               BSIZE
      PARAMETER           ( BSIZE  = 80  )

      INTEGER               INTLEN
      PARAMETER           ( INTLEN = 3   )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5  )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32  )

!
!     Local Variables.
!
      CHARACTER*(1)         SPACE
      CHARACTER*(1)         TAB
      CHARACTER*(1760)      REST
      CHARACTER*(INTLEN)    DSTRNG
      CHARACTER*(LNSIZE)    BUFFER ( LBCELL:BSIZE)
      CHARACTER*(LNSIZE)    EDITOR
      CHARACTER*(LNSIZE)    ERROR  ( 2 )
      CHARACTER*(LNSIZE)    ERRSTR
      CHARACTER*(LNSIZE)    NAME
      CHARACTER*(LNSIZE)    LINE
      CHARACTER*(LNSIZE)    PATTRN
      CHARACTER*(WDSIZE)    FRSTWD
      CHARACTER*(WDSIZE)    SCNDWD

      INTEGER               B1
      INTEGER               B2
      INTEGER               COMNUM
      INTEGER               DEPTH
      INTEGER               E1
      INTEGER               E2
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               PTR
      INTEGER               R
      INTEGER               UNIT

      LOGICAL               FIRST
      LOGICAL               LSTAT  ( 3 )
      LOGICAL               SSTAT  ( 3 )
      LOGICAL               STATUS ( 3 )
      LOGICAL               SVSTAT ( 3 )
      LOGICAL               GOTONE


      SAVE

#if defined(SPICE_COMPILER_INTEL)
      DATA     EDITOR    / 'edit'  /
#else
      DATA     EDITOR    / 'emacs' /
#endif
      DATA     FIRST     / .TRUE.  /

!
!     The only time an EDIT/RECALL/DO command can have any meaning
!     is when it comes from the keyboard.
!
      IF ( SOURCE .NE. KEYBRD ) THEN
         RETURN
      END IF

!
!     Initialize the syntax for the preprocessing commands
!
      IF ( FIRST ) THEN
         FIRST = .FALSE.

         TAB   = CHAR(9)
         SPACE = ' '

      END IF

!
!     Next we take apart the command and see if it is one of the
!     preprocessing commands.
!
      CALL NEXTWD ( COMMND,  FRSTWD,  REST )
      CALL NEXTWD ( REST,    SCNDWD,  REST )

!
!     We probably don't have any of the pathologies below, but they
!     are easy to check so we handle them here.
!
      IF ( REST .NE. ' ' ) THEN
         RETURN
      END IF

      IF ( FRSTWD .EQ. ' ' ) THEN
         RETURN
      END IF

      B1 = 1
      B2 = 1
      E1 = RTRIM  ( FRSTWD )
      E2 = RTRIM  ( SCNDWD )

      IF (       SCNDWD .EQ. ' ' &
           .AND. &
           .NOT. M2WMCH ( FRSTWD, B1, E1, 'RECALL' ) ) THEN
         RETURN
      END IF


!
!     We need the beginning and endings of the words we've extracted.
!
      B1 = 1
      B2 = 1
      E1 = RTRIM  ( FRSTWD )
      E2 = RTRIM  ( SCNDWD )


      IF    (         M2WMCH ( FRSTWD, B1, E1, 'RECALL' ) &
               .AND. (SCNDWD .EQ. ' ' )      ) THEN


!
!        We don't want the RECALL command to show up in the
!        output.
!
         CALL DMPBUF ()

!
!        We don't write the output of a RECALL command to the
!        log file.
!
         CALL NSPGST ( 'LOG', STATUS )
         CALL NSPIOH ( 'LOG'         )
!
!        Determine the depth of the command line buffer.
!
         CALL GETBSZ ( DEPTH )
!
!        Fetch each paragraph and display it.
!
         DO WHILE ( DEPTH .GT. 0 )

            CALL SSIZEC ( BSIZE,  BUFFER )
            CALL INTSTR ( DEPTH,  DSTRNG )
            CALL LJUST  ( DSTRNG, DSTRNG )
            CALL GETBUF ( DEPTH,  BUFFER )

            LINE = DSTRNG(1:3) // BUFFER(1)

            CALL NSPWLN ( LINE )

            DO I = 2, CARDC(BUFFER)
               LINE = '   ' // BUFFER(I)
               CALL NSPWLN ( LINE )
            END DO

            DEPTH = DEPTH - 1

         END DO
!
!        Reset the status of the LOG file back to whatever it
!        was before we started dumping old commands.  Finally
!        set the command to a blank.
!
         CALL NSPPST ( 'LOG', STATUS )
         COMMND = ' '
         SOURCE  = NONE

         RETURN

      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'RECALL'     ) &
               .AND. M2WMCH ( SCNDWD, B2, E2, '@int(1:20)' ) ) THEN
!
!        We don't write the output of a RECALL command to the
!        log file.
!
         CALL NSPGST ( 'LOG', STATUS )
         CALL NSPIOH ( 'LOG'         )
!
!        Find out the depth of the command to fetch.
!
         CALL NPARSI (SCNDWD, DEPTH, ERRSTR, PTR )
!
!        Get rid of the top command (it's the RECALL command).
!
         CALL DMPBUF ()
         CALL SSIZEC ( BSIZE,  BUFFER )
         CALL GETBUF ( DEPTH,  BUFFER )


         DO I = 1, CARDC(BUFFER)
            LINE = '   ' // BUFFER(I)
            CALL NSPWLN ( LINE )
         END DO
!
!        Reset the status of the LOG file back to whatever it
!        was before we started dumping old commands.  Finally
!        set the command to a blank.
!
         CALL NSPPST ( 'LOG', STATUS )
         COMMND = ' '
         SOURCE  = NONE
         RETURN

      ELSE IF ( M2WMCH ( FRSTWD, B1, E1, 'RECALL' ) ) THEN


!
!        Find out the depth of the command to fetch.
!
         CALL GETBSZ (         DEPTH  )
         CALL SUFFIX ( '*', 0, SCNDWD )

         COMNUM = 2
         GOTONE = .FALSE.

         DO WHILE ( COMNUM .LE. DEPTH )

            CALL SSIZEC( BSIZE,  BUFFER )
            CALL GETBUF( COMNUM, BUFFER )

            IF  (       CARDC ( BUFFER  ) .GT.  0 &
                  .AND. MATCH ( BUFFER(1), SCNDWD ) ) THEN

!
!              We don't write the output of a RECALL command to the
!              log file.
!
               CALL NSPGST ( 'LOG', STATUS )
               CALL NSPIOH ( 'LOG'         )
!
!              Dump the top command as it is just the recall command.
!
               CALL DMPBUF ()
               DO I = 1, CARDC(BUFFER)
                  LINE = '   ' // BUFFER(I)
                  CALL NSPWLN ( LINE )
               END DO

               COMNUM = DEPTH
               GOTONE = .TRUE.
               COMMND = ' '
               SOURCE = NONE

               CALL NSPPST ( 'LOG', STATUS )

            END IF

            COMNUM = COMNUM + 1

         END DO
!
!        Reset the status of the LOG file back to whatever it
!        was before we started dumping old commands.
!
         IF ( .NOT. GOTONE ) THEN

            ERROR(1) = 'There is no command in the command ' &
            //         'history list that matches ''#''. '

            CALL REPMC ( ERROR(1), '#', SCNDWD, ERROR(1) )
            COMMND = ' '
            SOURCE = NONE
            CALL SETMSG (  ERROR              )
            CALL SIGERR ( 'EDTCOM(NOMATCH)'   )
            RETURN

         END IF


         RETURN





      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'EDIT'       ) &
               .AND. M2WMCH ( SCNDWD, B2, E2, '@int(1:20)' ) ) THEN

         CALL NPARSI (SCNDWD, DEPTH, ERRSTR, PTR )
         CALL DMPBUF ()
         CALL SSIZEC ( BSIZE, BUFFER )
         CALL GETBUF ( DEPTH, BUFFER )
!
!        Open the utility port to receive the contents of BUFFER.
!
         PATTRN = 'edt{0-z}{0-z}{0-z}{0-z}{0-z}.tmp'

         CALL NEWFIL ( PATTRN, 'UTILITY', NAME )

         IF ( FAILED() ) THEN
            CALL RESET
            CALL CHKIN  ( 'EDTCOM' )
            CALL SETMSG ( 'The program was unable to open a file ' &
            //            'that could be used with the ' &
            //            'editor. ' &
            //            'Command editing cannot be ' &
            //            'performed at this time. ' )
            CALL SIGERR ( 'CMLOOP(COMMANDEDITFAILED)' )
            CALL CHKOUT ( 'EDTCOM' )
            RETURN
         END IF
!
!        We have at this point succeeded in opening a file
!        into which we can write the last command.  But we
!        don't want to write to the screen, log file or save
!        file if there is one. Inhibit writing to any port
!        but the utility port.
!
         CALL NSPGST ( 'LOG',     LSTAT  )
         CALL NSPGST ( 'SCREEN',  SSTAT  )
         CALL NSPGST ( 'SAVE',    SVSTAT )

         CALL NSPIOH ( 'LOG'     )
         CALL NSPIOH ( 'SCREEN'  )
         CALL NSPIOH ( 'SAVE'    )

         DO I = 1, CARDC ( BUFFER )
            CALL NSPWLN ( BUFFER(I) )
         END DO

         CALL NSPIOC ( 'UTILITY' )
!
!        Activate the editor
!
         CALL EDTCMD ( EDITOR(1:RTRIM(EDITOR)), &
                       NAME(1:RTRIM(NAME)) )

         ERROR(1) = ' '
         ERROR(2) = ' '

         IF ( HAVE(ERROR) ) THEN
            CALL NSPPST ( 'LOG',     LSTAT  )
            CALL NSPPST ( 'SCREEN',  SSTAT  )
            CALL NSPPST ( 'SAVE',    SVSTAT )
            COMMND = ' '
            SOURCE = NONE
            CALL SETMSG (  ERROR                   )
            CALL SIGERR ( 'SPICE(FILEREADERROR)'   )
            RETURN
         END IF
!
!        Read the first command from the edited file.
!

         CALL PRSTRT ( NAME, ERROR )

         IF ( HAVE(ERROR) ) THEN
            CALL NSPPST ( 'LOG',     LSTAT  )
            CALL NSPPST ( 'SCREEN',  SSTAT  )
            CALL NSPPST ( 'SAVE',    SVSTAT )
            COMMND = ' '
            SOURCE = NONE
            CALL PREXIT
            CALL SETMSG (  ERROR                   )
            CALL SIGERR ( 'SPICE(FILEREADERROR)'   )
            RETURN
         END IF

         CALL PRREAD ( DELIM,  COMMND )
         CALL PUTCOM ( COMMND, KEYBRD )
         CALL PREXIT ( )

!
!        Finally, delete the file we used with the editor.
!
         CALL TXTOPR ( NAME, UNIT )
         CLOSE  ( UNIT   =  UNIT, &
                  STATUS = 'DELETE', &
                  IOSTAT = IOSTAT )

         CALL SSIZEC ( BSIZE, BUFFER )
         CALL GETBUF ( 1,     BUFFER )

         CALL NSPIOA ( 'SCREEN' )

         R = RTRIM ( PROMPT ) + 2

         DO I = 1, CARDC(BUFFER)
            IF ( I .EQ. 1 ) THEN

               LINE = PROMPT
               CALL SUFFIX ( BUFFER(I), 1, LINE )
            ELSE
               LINE     = ' '
               LINE(R:) = BUFFER(I)
            END IF

            CALL NSPWLN ( LINE(1:RTRIM(LINE)) )
         END DO

!
!        Reset the writing to all other ports.
!
         CALL NSPPST ( 'LOG',     LSTAT  )
         CALL NSPPST ( 'SCREEN',  SSTAT  )
         CALL NSPPST ( 'SAVE',    SVSTAT )

         COMMND = ' '
         SOURCE = NONE


      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'EDIT' ) &
               .AND. SCNDWD .NE. ' '                     ) THEN

         GOTONE = .FALSE.
         COMNUM = 2
         CALL GETBSZ ( DEPTH          )
         CALL SUFFIX ( '*', 0, SCNDWD )

         DO WHILE ( COMNUM .LE. DEPTH )

            CALL SSIZEC( BSIZE,  BUFFER )
            CALL GETBUF( COMNUM, BUFFER )

            IF  (       CARDC ( BUFFER  ) .GT.  0 &
                  .AND. MATCH ( BUFFER(1), SCNDWD ) ) THEN

               GOTONE = .TRUE.
               CALL DMPBUF ()

!
!              Open the utility port to receive the contents of BUFFER.
!
               PATTRN = 'edt{0-z}{0-z}{0-z}{0-z}{0-z}.tmp'

               CALL NEWFIL ( PATTRN, 'UTILITY', NAME )

               IF ( FAILED() ) THEN
                  CALL RESET
                  CALL CHKIN  ( 'EDTCOM' )
                  CALL SETMSG  ( 'The program was unable to ' &
                  //             'open a file that could be used ' &
                  //             'with the editor. Command ' &
                  //             'editing cannot be performed at ' &
                  //             'this time. ')

                  CALL SIGERR ( 'CMLOOP(COMMANDEDITFAILED)' )
                  CALL CHKOUT ( 'EDTCOM' )
                  RETURN
               END IF

!
!              We have at this point succeeded in opening a file
!              into which we can write the last command.  But we
!              don't want to write to the screen, log file or save
!              file if there is one. Inhibit writing to any port
!              but the utility port.
!
               CALL NSPGST ( 'LOG',     LSTAT  )
               CALL NSPGST ( 'SCREEN',  SSTAT  )
               CALL NSPGST ( 'SAVE',    SVSTAT )

               CALL NSPIOH ( 'LOG'     )
               CALL NSPIOH ( 'SCREEN'  )
               CALL NSPIOH ( 'SAVE'    )

               DO I = 1, CARDC ( BUFFER )
                  CALL NSPWLN ( BUFFER(I) )
               END DO

               CALL NSPIOC ( 'UTILITY' )

!
!              Activate the editor
!
               CALL EDTCMD ( EDITOR(1:RTRIM(EDITOR)), &
                              NAME(1:RTRIM(NAME)) )

               ERROR(1) = ' '
               ERROR(2) = ' '

               IF ( HAVE(ERROR) ) THEN
                  CALL NSPPST ( 'LOG',     LSTAT  )
                  CALL NSPPST ( 'SCREEN',  SSTAT  )
                  CALL NSPPST ( 'SAVE',    SVSTAT )
                  COMMND = ' '
                  SOURCE = NONE
                  CALL SETMSG (  ERROR                   )
                  CALL SIGERR ( 'SPICE(FILEREADERROR)'   )
                  RETURN
               END IF

!
!              Read the first command from the edited file.
!

               CALL PRSTRT ( NAME, ERROR )

               IF ( HAVE(ERROR) ) THEN
                  CALL NSPPST ( 'LOG',     LSTAT  )
                  CALL NSPPST ( 'SCREEN',  SSTAT  )
                  CALL NSPPST ( 'SAVE',    SVSTAT )
                  COMMND = ' '
                  SOURCE = NONE
                  CALL PREXIT
                  CALL SETMSG (  ERROR                   )
                  CALL SIGERR ( 'SPICE(FILEREADERROR)'   )
                  RETURN
               END IF

               CALL PRREAD ( DELIM,  COMMND )
               CALL PUTCOM ( COMMND, KEYBRD )
               CALL PREXIT ( )

!
!              Finally, delete the file we used with the editor.
!
               CALL TXTOPR ( NAME, UNIT )
               CLOSE  ( UNIT   =  UNIT, &
                        STATUS = 'DELETE', &
                        IOSTAT = IOSTAT )

               CALL SSIZEC ( BSIZE, BUFFER )
               CALL GETBUF ( 1,     BUFFER )

               CALL NSPIOA ( 'SCREEN' )

               R = RTRIM ( PROMPT ) + 2

               DO I = 1, CARDC(BUFFER)
                  IF ( I .EQ. 1 ) THEN

                     LINE = PROMPT
                     CALL SUFFIX ( BUFFER(I), 1, LINE )
                  ELSE
                     LINE     = ' '
                     LINE(R:) = BUFFER(I)
                  END IF

                  CALL NSPWLN ( LINE(1:RTRIM(LINE)) )
               END DO

!
!              Reset the writing to all other ports.
!
               CALL NSPPST ( 'LOG',     LSTAT  )
               CALL NSPPST ( 'SCREEN',  SSTAT  )
               CALL NSPPST ( 'SAVE',    SVSTAT )

               COMMND = ' '
               SOURCE = NONE

               COMNUM = DEPTH

            END IF

            COMNUM = COMNUM + 1

         END DO


         IF ( .NOT. GOTONE ) THEN

            ERROR(1) = 'There is no command in the command ' &
            //         'history list that matches ''#''. '

            CALL REPMC ( ERROR(1), '#', SCNDWD, ERROR(1) )
            COMMND = ' '
            SOURCE = NONE
            CALL SETMSG (  ERROR              )
            CALL SIGERR ( 'EDTCOM(NOMATCH)'   )
            RETURN

         END IF

      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'DO'         ) &
               .AND. M2WMCH ( SCNDWD, B2, E2, '@int(1:20)' ) ) THEN

         CALL NPARSI ( SCNDWD, DEPTH, ERRSTR, PTR )
         CALL DMPBUF ()
         CALL SSIZEC ( BSIZE, BUFFER )
         CALL GETBUF ( DEPTH, BUFFER )

         CALL NSPGST ( 'SCREEN', SSTAT )
         CALL NSPIOA ( 'SCREEN'        )

         R = RTRIM ( PROMPT ) + 2
!
!        Reset the paragraph buffer so it can receive another
!        paragraph. (This is where we buffer commands and we
!        need to buffer this one.)
!
         CALL RSTBUF ()

         DO I = 1, CARDC(BUFFER)

            CALL PUTBUF(BUFFER(I))

            IF ( I .EQ. 1 ) THEN
               LINE = PROMPT
               CALL SUFFIX ( BUFFER(I), 1, LINE )
            ELSE
               LINE     = ' '
               LINE(R:) = BUFFER(I)
            END IF

            CALL NSPWLN ( LINE(1:RTRIM(LINE)) )
         END DO

         CALL NSPPST ( 'SCREEN',  SSTAT  )

         COMMND = ' '

         COMMND = BUFFER(1)

         DO I = 2, CARDC ( BUFFER )
            CALL SUFFIX ( BUFFER(I), 1, COMMND )
         END DO

         I = INDEX ( COMMND, DELIM )

         IF ( I .GT. 0 ) THEN
            CALL PUTCOM ( COMMND(1:I-1), KEYBRD )
         ELSE
            CALL PUTCOM ( COMMND,        COMBUF )
         END IF

         COMMND = ' '
         SOURCE = NONE


      ELSE IF (      M2WMCH ( FRSTWD, B1, E1, 'DO' ) &
               .AND.  SCNDWD .NE. ' '                ) THEN
!
!        This is basically the same as the last case, but
!        we look for a pattern match before doing anything.
!
         GOTONE = .FALSE.
         CALL GETBSZ ( DEPTH )
         CALL SUFFIX ( '*', 0, SCNDWD )
         COMNUM = 2

         DO WHILE ( COMNUM .LE. DEPTH )

            CALL SSIZEC ( BSIZE,  BUFFER )
            CALL GETBUF ( COMNUM, BUFFER )

            IF  (       CARDC ( BUFFER  ) .GT.  0 &
                  .AND. MATCH ( BUFFER(1), SCNDWD ) ) THEN

               GOTONE = .TRUE.
               CALL DMPBUF ()

               CALL NSPGST ( 'SCREEN', SSTAT )
               CALL NSPIOA ( 'SCREEN'        )

               R = RTRIM ( PROMPT ) + 2
!
!              Reset the paragraph buffer so it can receive another
!              paragraph. (This is where we buffer commands and we
!              need to buffer this one.)
!
               CALL RSTBUF ()

               DO I = 1, CARDC(BUFFER)
                  CALL PUTBUF( BUFFER(I) )
                  IF ( I .EQ. 1 ) THEN
                     LINE = PROMPT
                     CALL SUFFIX ( BUFFER(I), 1, LINE )
                  ELSE
                     LINE     = ' '
                     LINE(R:) = BUFFER(I)
                  END IF

                  CALL NSPWLN ( LINE(1:RTRIM(LINE)) )
               END DO

               CALL NSPPST ( 'SCREEN',  SSTAT  )

               COMMND = ' '

               COMMND = BUFFER(1)

               DO I = 2, CARDC ( BUFFER )
                  CALL SUFFIX ( BUFFER(I), 1, COMMND )
               END DO

               I = INDEX ( COMMND, DELIM )

               IF ( I .GT. 0 ) THEN
                  CALL PUTCOM ( COMMND(1:I-1), KEYBRD )
               ELSE
                  CALL PUTCOM ( COMMND,        COMBUF )
               END IF

               COMMND = ' '
               SOURCE = NONE

               COMNUM = DEPTH

            END IF

            COMNUM = COMNUM + 1

         END DO

         IF ( .NOT. GOTONE ) THEN

            ERROR(1) = 'There is no command in the command ' &
            //         'history list that matches ''#''. '

            CALL REPMC ( ERROR(1), '#', SCNDWD, ERROR(1) )
            COMMND = ' '
            SOURCE = NONE
            CALL SETMSG (  ERROR              )
            CALL SIGERR ( 'EDTCOM(NOMATCH)'   )
            RETURN

         END IF

      END IF



      RETURN

      ENTRY SETEDT ( COMMND )

      EDITOR = COMMND
      RETURN

      ENTRY GETEDT ( COMMND )
      COMMND = EDITOR
      RETURN

      END
