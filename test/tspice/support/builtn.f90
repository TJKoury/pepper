!$Procedure      BUILTN ( Built in Commands )

      SUBROUTINE BUILTN ( COMMND, HIT, ERROR )

! $ Abstract
!
!     This routine handles the normal commands that every
!     command line based program will support if you
!     use the command loop software.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       Command Loop
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         COMMND
      LOGICAL               HIT
      CHARACTER*(*)         ERROR ( 2 )

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      COMMND     I   A command
!      HIT        O   Indicates if the command was a built in command
!      ERROR      O   Indicates any problems that occurred.
!
! $ Detailed_Input
!
!     COMMND      is a command that is to be processed to see if it
!                 is one of the command loop built in commands.
!
! $ Detailed_Output
!
!     HIT         is a logical variable.  If the input command is
!                 recognized and acted on, HIT is returned as .TRUE.
!                 Otherwise it is returned as .FALSE.
!
!     ERROR       is blank unless a built in command is recognized
!                 and causes an error to be triggered.  In the later
!                 case ERROR will contain the diagnostics associated
!                 with the error.
!
! $ Parameters
!
!      None.
!
! $ Files
!
!      None.
!
! $ Exceptions
!
!     Error free.
!
!     1) If a problem is detected, it is diagnosed and returned
!        in the variable ERROR
!
! $ Particulars
!
!     This routine handles the "built in" commands that are
!     automatically available with every command loop routine these
!     are:
!
!        SET  EDITOR (1:)@word
!        SHOW SYMBOL @word
!        SHOW ENVIRONMENT
!        SAVE TO @word
!        DISCARD
!
!     These built in functions can be overridden (turned off) through
!     the companion entry point BUILTO
!
! $ Examples
!
!     See the routine CMLOOP
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 22-APR-1997 (WLT)
!
!        Declares NSPWLN as external
!
!-    SPICELIB Version 1.0.0, 5-DEC-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Command Loop Built in Commands.
!
!-&

!
!     Spicelib functions
!
      INTEGER               RTRIM
      INTEGER               LTRIM
      INTEGER               ISRCHC
      INTEGER               TOUCHI
!
!     Error handling interface routines.
!
      LOGICAL               HAVE
!
!     META/2 Functions
!
      LOGICAL               M2XIST

!
!     Inspekt External Routines
!
      EXTERNAL              NSPWLN

!
!     Variables needed for syntax declarations.
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               NUPPER
      PARAMETER           ( NUPPER =  5 )

      INTEGER               NSYN
      PARAMETER           ( NSYN   = NUPPER )

      INTEGER               SYNLEN
      PARAMETER           ( SYNLEN = 80 )

      CHARACTER*(WDSIZE)    SYNKEY ( LBCELL : NSYN )
      INTEGER               SYNPTR ( LBCELL : NSYN )
      CHARACTER*(SYNLEN)    SYNVAL ( LBCELL : NSYN )

!
!     The following are for special commands that will not be
!     processed by BUILTN.
!
      INTEGER               NSP
      PARAMETER           ( NSP = 2 )

      CHARACTER*(8)         SPCIAL ( NSP )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 512 )


      INTEGER               MAXLNS
      PARAMETER           ( MAXLNS =  3 )

      CHARACTER*(LNSIZE)    MYERR  ( 2      )
      CHARACTER*(80)        TEMPLT
      CHARACTER*(LNSIZE)    VALUES ( MAXLNS )
      CHARACTER*(WDSIZE)    NAMES  ( MAXLNS )

!
!     Other Local Variables
!
      INTEGER               E
      INTEGER               I
      INTEGER               L
      INTEGER               NITEMS
      INTEGER               REST

      LOGICAL               FOUND
      LOGICAL               FIRST
      LOGICAL               STATUS ( 3 )

      LOGICAL               DOSAV
      LOGICAL               DODISC
      LOGICAL               DOEDIT
      LOGICAL               DOSYM
      LOGICAL               DOENV


!
!     Save everything
!
      SAVE

      DATA     FIRST     /.TRUE./

      DATA     DOSAV      /.TRUE./
      DATA     DODISC     /.TRUE./
      DATA     DOEDIT     /.TRUE./
      DATA     DOSYM      /.TRUE./
      DATA     DOENV      /.TRUE./

      DATA (   SYNVAL(I), I = 1, NUPPER ) &
           /   'SET[set]   EDITOR[editor] (1:)@word[rest]', &
               'SHOW[show] SYMBOL[symbol] @word[def]', &
               'SHOW[show] ENVIRONMENT[env]', &
               'SAVE[save] TO  @word[rest]', &
               'DISCARD[discard]' /

      DATA  SPCIAL / ' ', '?' /

      CALL CHKIN  ( 'BUILTN' )

      IF ( FIRST ) THEN

         FIRST = .FALSE.
         I     =  0
         I     = TOUCHI ( I )
         CALL M2INTS ( NSYN, SYNKEY, SYNPTR, SYNVAL )

      END IF

      L    = LTRIM ( COMMND)
      REST = RTRIM ( COMMND ) + 1

      IF ( ISRCHC( COMMND(L:REST), NSP, SPCIAL ) .GT. 0 ) THEN
         CALL CHKOUT ( 'BUILTN' )
         RETURN
      END IF
!
!     There are no errors yet.
!
      ERROR(1) = ' '
      ERROR(2) = ' '
      HIT      = .FALSE.
!
!     Check the input command to see if it is recognizable
!
      CALL M2CHCK ( COMMND, SYNKEY, SYNPTR, SYNVAL, MYERR )

      IF ( MYERR(1) .NE. ' ' ) THEN
         CALL CHKOUT ( 'BUILTN' )
         RETURN
      END IF


      IF      ( M2XIST('set') .AND. DOEDIT ) THEN

         CALL M2VGET ( 'rest', 1, FOUND, REST, E )
         CALL SETEDT ( COMMND(REST:) )
         HIT = .TRUE.

      ELSE IF ( M2XIST('symbol') .AND. DOSYM ) THEN

         CALL M2GETC ( 'def', COMMND, FOUND, TEMPLT  )
         CALL SHOSYM ( TEMPLT  )
         HIT = .TRUE.

      ELSE IF ( M2XIST('env') .AND. DOENV ) THEN

         NITEMS   = 3
         NAMES(1) = 'Editor'
         NAMES(2) = 'Echoing Commands'
         NAMES(3) = 'Screen Output File'


         CALL GETEDT ( VALUES(1) )
         CALL GTECHO ( VALUES(2) )

         CALL NSPGST ( 'SAVE', STATUS )

         IF ( STATUS(1) .AND. STATUS(2) .AND. .NOT. STATUS(3) ) THEN

            CALL NSPPFL ( 'SAVE', VALUES(3) )

         ELSE

            VALUES(3) = 'No Current Screen Save File'

         END IF

         CALL NSPWLN ( ' ' )
         CALL NSPWLN ( 'Current Environment' )
         CALL NSPWLN ( ' ' )
         CALL FLGRPT ( NITEMS, NAMES, VALUES, NSPWLN )
         CALL NSPWLN ( ' ' )
         HIT = .TRUE.

      ELSE IF ( M2XIST('save') .AND. DOSAV     ) THEN

         CALL M2VGET ( 'rest', 1, FOUND, REST, E )
         CALL NSPSAV ( COMMND(REST:), ERROR )
         HIT = .TRUE.

      ELSE IF ( M2XIST('discard') .AND. DODISC ) THEN

         CALL NSPIOC ( 'SAVE' )
         HIT = .TRUE.

      END IF

      FOUND = HAVE(ERROR)
      CALL CHKOUT ( 'BUILTN' )
      RETURN



!$Procedure      BUILTO ( Built in commands off )

      ENTRY BUILTO ( COMMND )

! $ Abstract
!
!    Turn off built-in command loop commands.
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
!     COMMAND LOOP
!
!
! $ Declarations
!
!     CHARACTER*(*)         COMMND
!
! $ Brief_I/O
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     COMMND     I   A list of commands to turn off.
!
! $ Detailed_Input
!
!     COMMND     is a list of words that describes which built-in
!                commands to disable.  The words and commands
!                they turn off are:
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1) No errors are detected.
!
! $ Particulars
!
!     This routine allows you to turn off selected built in commands
!     commands available through command loop programs.
!
! $ Examples
!
!     Suppose you want to turn off the SHOW ENVIRONMENT and
!     SET EDITOR commands.
!
!     Do this:
!
!     COMMAND = 'EDITOR ENVIRONMENT'
!
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!       W.L. Taber      (JPL)
!
! $ Literature_References
!
!       None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 5-DEC-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Disable built in command loop commmands
!
!
!-&
!
!     We just look at command to see which of the built in
!     command should be disabled.
!
      DOSAV  = INDEX ( COMMND, 'SAVE'        ) .EQ. 0
      DOENV  = INDEX ( COMMND, 'ENVIRONMENT' ) .EQ. 0
      DOEDIT = INDEX ( COMMND, 'EDITOR'      ) .EQ. 0
      DOSYM  = INDEX ( COMMND, 'SYMBOL'      ) .EQ. 0
      DODISC = INDEX ( COMMND, 'DISCARD'     ) .EQ. 0

      RETURN

      END
