!$Procedure      ECHO ( Echo the translation of a string )

      SUBROUTINE ECHO ( STRING, TRANSL )

! $ Abstract
!
!    Echo a string if echoing is enabled and a string has been
!    translated from its original value.
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
!     Command Loop
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         STRING
      CHARACTER*(*)         TRANSL

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   is a string
!     TRANSL     I   is string after some kind of processing
!
! $ Detailed_Input
!
!     STRING     is a string.  The intent is that this is some string
!                that the user has specified as a command to a program
!                and that may be subject to some kind of preprocessing
!                such as symbol resolution.
!
!     TRANSL     is the string that results from some user's action on
!                the input STRING.
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
! $ Particulars
!
!     This is a utility routine for the command loop system.
!
!     If as the result of preprocessing a command, some modificactions
!     are created it is sometimes helpful to see the result
!     of these translations.
!
!     If the echoing is enabled (via the entry point DOECHO) and
!     TRANSL is not the same as STRING.  The translation will
!     be echoed to the user's output device and to the user's log
!     file.
!
!     This routine has 3 companion entry points.
!
!     DOECHO  ---  enables echoing of commands.
!     NOECHO  ---  disables echoing of commands.
!     GTECHO  ---  returns 'YES' if echoing is enabled 'NO'
!                  otherwise.
!
!     By default echoing is disabled.
!
! $ Examples
!
!     Suppose that as a result of symbol resolution the
!     command
!
!        "DOIT"
!
!     becomes
!
!          SELECT A, B, C, FROM TABLE WHERE A < B AND B < C
!          AND C < A ORDER BY A B C
!
!
!     If echoing has been enabled the text below will be sent
!     to the user's screen and log file:
!
!     ;;; SELECT A, B, C, FROM TABLE WHERE A < B AND B < C AND
!     ;   C < A ORDER BY A B C '
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
!-    SPICELIB Version 1.0.0, 28-JUL-1995 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Echo translated commands.
!
!-&
      EXTERNAL              NSPWLN

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(LNSIZE)    STYLE
      CHARACTER*(LNSIZE)    HIDE
      CHARACTER*(LNSIZE)    SEEN
      CHARACTER*(LNSIZE)    HSTYLE



      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(WDSIZE)    DONT
      CHARACTER*(WDSIZE)    REPEAT
      CHARACTER*(WDSIZE)    FRSTWD
      CHARACTER*(WDSIZE)    SCNDWD
      CHARACTER*(WDSIZE)    THRDWD

      CHARACTER*(1)         DELIM
      CHARACTER*(1)         CDELIM
      CHARACTER*(3)         FLAG
      CHARACTER*(3)         LEAD

      INTEGER               LOC

      LOGICAL               STAT  ( 3 )
      LOGICAL               DOIT
      LOGICAL               FIRST
      LOGICAL               WIPE

      SAVE

      DATA                  DOIT  / .FALSE. /
      DATA                  FIRST / .TRUE. /

      IF ( FIRST ) THEN
!
!        Find out what the words for NO and ECHO are
!        in the current language.
!
         FIRST = .FALSE.

         CALL TRNLAT ( 'DONT', DONT   )
         CALL TRNLAT ( 'ECHO', REPEAT )

      END IF

      CALL NTHWD ( TRANSL, 1, FRSTWD, LOC )
      CALL NTHWD ( TRANSL, 2, SCNDWD, LOC )
      CALL NTHWD ( TRANSL, 3, THRDWD, LOC )


      CALL UCASE ( FRSTWD, FRSTWD )
      CALL UCASE ( SCNDWD, SCNDWD )
      CALL UCASE ( THRDWD, THRDWD )

      IF (      FRSTWD .EQ. REPEAT &
          .AND. SCNDWD .EQ. ' '    ) THEN

         WIPE = .TRUE.
         DOIT = .TRUE.

      ELSE IF (       FRSTWD .EQ. DONT &
                .AND. SCNDWD .EQ. REPEAT &
                .AND. THRDWD .EQ. ' '    ) THEN

         WIPE = .TRUE.
         DOIT = .FALSE.

      ELSE

         WIPE = .FALSE.

      END IF


      IF ( DOIT ) THEN

         IF ( STRING .NE. TRANSL ) THEN
!
!           Get the current margins and the delimiter.
!
            CALL NSPMRG ( STYLE )
            CALL GETDEL ( DELIM )
!
!           Create the NICEIO style string it will be of the form
!
!              LEFT 1 RIGHT margin FLAG ;;; LEADER  ;
!
!           (provided of course that ';' is the command
!
            FLAG = DELIM // DELIM // DELIM
            LEAD = DELIM // '++'

            CALL PREFIX (  LEAD,     1, STYLE )
            CALL PREFIX ( 'LEADER ', 1, STYLE )
            CALL PREFIX (  FLAG,     1, STYLE )
            CALL PREFIX ( 'FLAG',    1, STYLE )
!
!           Get the current status of the "log" port and
!           for the moment inhibit writing to that port.
!
            CALL NSPGST ( 'LOG', STAT  )
            CALL NSPIOH ( 'LOG'        )
!
!           Display the translated string.
!
            CALL NICEPR_1 ( TRANSL, STYLE, NSPWLN )
!
!           Now re-establish the status of the log port.
!
            CALL NSPPST ( 'LOG', STAT )
!
!           Send the translated string to the log file and
!           do it so that it is a comment in the log file.
!           Note that we use a special logging style for
!           echoing the symbol translation.
!
            HSTYLE = 'LEFT 1 RIGHT 78 '

            CALL PREFIX (  LEAD,     1, HSTYLE )
            CALL PREFIX ( 'LEADER ', 1, HSTYLE )
            CALL PREFIX (  FLAG,     1, HSTYLE )
            CALL PREFIX ( 'FLAG',    1, HSTYLE )

            CALL NSPGLS ( SEEN,    HIDE,   CDELIM )
            CALL NSPLGS ( SEEN,    HSTYLE, CDELIM )
            CALL NSPLOG ( TRANSL, .TRUE.          )
            CALL NSPLGS ( SEEN,    HIDE,   CDELIM )

         END IF

      END IF

      IF ( WIPE ) THEN
         TRANSL = ' '
      END IF

      RETURN

!
!     The following entry points allow you to
!
!        1) Enable echoing of translations
!        2) Disable echoing of translations
!        3) Find out the current status of echoing.
!
!     Since the code in each case is trivial, we aren't
!     going to set up those big old nasty NAIF headers.
!     (What a rebel!)
!
      ENTRY DOECHO ()

         DOIT = .TRUE.
         RETURN

      ENTRY NOECHO ()

         DOIT = .FALSE.
         RETURN

      ENTRY GTECHO ( STRING )

         IF ( DOIT ) THEN
            STRING = 'ENABLED'
         ELSE
            STRING = 'DISABLED'
         END IF

         RETURN

      END
