!$Procedure TSTLGR (Test utilities log files manager)

      SUBROUTINE TSTLGR( MESSGE, ERRLOG, GSTYLE, FSTYLE, &
                         MARKER, INT,    DP )

! $ Abstract
!
!
!     Save the contents of the command to a log file and any save
!     file that might be open and active.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         MESSGE
      LOGICAL               ERRLOG
      CHARACTER*(*)         GSTYLE
      CHARACTER*(*)         FSTYLE
      CHARACTER*(1)         MARKER
      INTEGER               INT
      DOUBLE PRECISION      DP

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TDB
!
! $ Detailed_Input
!
!     TDB
!
! $ Detailed_Output
!
!     TDB
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     TDB
!
! $ Examples
!
!     TDB
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
!     W.L. Taber (JPL)
!
! $ Version
!
!-     Test Utilities Version 2.0.0, 29-MAY-2010 (NJB)
!
!         Updated parameters to increase max length of test family
!         names from 12 characters to MAXFNM and increase max length of
!         test case names from 80 to MAXCNM.
!
!         Added bogus header sections to satisfy checkit.
!
!-     Test Utilities Version 1.3.0, 30-DEC-2002 (EDW)
!
!         Implemented the ZZTSTRE routine to reset the OPNERR
!         flag to TRUE.
!
!-     Test Utilities Version 1.2.1, 30-DEC-2002 (EDW)
!
!         Corrected several spelling errors.
!
!-     Test Utilities Version 1.2.0, 19-JUN-2001 (WLT)
!
!         Added an entry point that turns off the test message.  This
!         should be called only by TCASE.
!
!-     Test Utilities Version 1.1.0, 18-JUN-1999 (WLT)
!
!         Added a return before the first entry point.
!
!-     Test Utilities Version 1.0.0, 3-NOV-1994 (WLT)
!
!-&

! $ Index_Entries
!
!      N/A
!
!-&

      EXTERNAL              TSTWLN
!
!     Test Utility Functions.
!
      LOGICAL               VERBOS
!
!     SPICELIB Functions
!
      INTEGER               RTRIM
!
!     Local Variables
!
      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 128 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXTNM
      PARAMETER           ( MAXTNM = 80 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               MAXCNM
      PARAMETER           ( MAXCNM = 1000 )


      CHARACTER*(1)         MYMARK

      CHARACTER*(STYSIZ)    SEEN
      CHARACTER*(STYSIZ)    HIDE

      CHARACTER*(MAXTNM)    NAME
      CHARACTER*(MAXCNM)    CNAME
      CHARACTER*(MAXCNM)    EXTRA
      CHARACTER*(LNSIZE)    ENV
      CHARACTER*(LNSIZE)    TIME
      CHARACTER*(LNSIZE)    VERSN
      CHARACTER*(FILSIZ)    ERRFIL

      INTEGER               PNUM
      INTEGER               FNUM
      INTEGER               R


      LOGICAL               OPNERR
      LOGICAL               SHOW

      SAVE

      DATA   OPNERR / .TRUE. /
      DATA   SHOW   / .FALSE. /
      DATA   EXTRA  / ' '     /
      DATA   SEEN   / 'LEFT 1 RIGHT 78' /
      DATA   HIDE   / 'LEFT 1 RIGHT 78' /
      DATA   MYMARK / '#'               /

      RETURN




!
!     This entry point handles the logging of commands.
!
      ENTRY TSTLOG ( MESSGE, ERRLOG )
!
!     Version
!
!-     Test Utilities Version 1.0.0, 3-NOV-1994 (WLT)
!


      IF ( ERRLOG ) THEN

!
!        We need to open a log file and write some
!        special additional information if this is the first
!        message for this test case.
!

         CALL T_NAME  ( NAME   )
         CALL T_CNAME ( CNAME  )
         CALL T_FCOUNT( PNUM   )
         CALL T_CFAIL ( )
         CALL T_FCOUNT( FNUM   )

         IF ( OPNERR ) THEN

            OPNERR = .FALSE.

            CALL TSTFIL ( 'ERR{0-9}{0-9}{0-9}{0-9}.LOG', 'SAVE', ERRFIL)

            CALL TSTGET (  ENV, VERSN, TIME )
            CALL TSTIOA ( 'SAVE'   )
            CALL TSTIOH ( 'SCREEN' )
            CALL TSTIOH ( 'LOG'    )

            CALL TSTWLN ( ENV   )
            CALL TSTWLN ( VERSN )
            CALL TSTWLN ( TIME  )

         END IF
!
!        Activate the port for reporting errors, inhibit writing to the
!        screen or log file.
!
         CALL TSTIOA ( 'SAVE' )
         CALL TSTIOA ( 'SCREEN' )
         CALL TSTIOA ( 'LOG'    )
!
!        If there had been no previous errors reported for this
!        family of errors, we create a special message concerning
!        this family and case.
!
         IF ( PNUM .EQ. 0 ) THEN

            CALL TSTWLN( ' ' )
            CALL TSTWLN('A test failure occurred in the test family:')
            CALL TSTWLN( ' ' )
            CALL TSTWLN( 'Family : ' // NAME  )
            CALL NICEPR_1( CNAME, &
                        'LEFT 1 RIGHT 78 HARDSPACE ^ FLAG CASE^^^:', &
                         TSTWLN )
            CALL TSTWLN( ' ' )
!
!        If this is the first error message to be reported for this
!        test case we note the failure for this test case.
!
         ELSE IF ( PNUM .NE. FNUM ) THEN

            CALL TSTWLN( ' '                   )
            CALL TSTWLN( 'Test Case FAILURE. ' )
            CALL NICEPR_1( CNAME, &
                        'LEFT 1 RIGHT 78 HARDSPACE ^ FLAG CASE^^^:', &
                         TSTWLN )
            CALL TSTWLN( ' '                   )

         END IF
!
!        Reactivate the screen and log file.
!
         CALL TSTIOA  ( 'SCREEN' )
         CALL TSTIOA  ( 'LOG'    )


      END IF


      IF ( ERRLOG ) THEN

         IF ( MESSGE .EQ. ' ' ) THEN
            CALL TSTWLN ( ' ' )
         ELSE

            IF ( SHOW ) THEN
               CALL NICEPR_1 ( EXTRA, HIDE, TSTWLN )
               SHOW = .FALSE.
               CALL TSTWLN   ( ' ' )
            END IF

            CALL NICEPR_1 ( MESSGE,  HIDE, TSTWLN )

         END IF

      ELSE

         IF ( MESSGE .EQ. ' ' ) THEN
            CALL TSTWLN ( ' ' )
         ELSE
            CALL NICEPR_1 ( MESSGE,  SEEN, TSTWLN )
         END IF

      END IF
!
!     Inhibit writing to the error log.
!

      CALL TSTIOH ( 'SAVE' )

      RETURN



!
!     This entry point allows users to set the style used for
!     logging failure and non-failure and visible commands.
!
      ENTRY TSTLGS ( GSTYLE, FSTYLE )

!  Version
!
!-     Test Utilities Version 1.0.0, 3-NOV-1994 (WLT)
!
!

      SEEN  = GSTYLE
      HIDE  = FSTYLE

      RETURN



!
!     This entry point allows users to get the style used for
!     logging failure and non-failure and visible commands.
!
      ENTRY TSTSTY ( GSTYLE, FSTYLE )

!  Version
!
!-     Test Utilities Version 1.0.0, 3-NOV-1994 (WLT)
!
!
      GSTYLE = SEEN
      FSTYLE = HIDE

      RETURN


!
!     This entry point handles the logging of commands when we want
!     only verbose logging.
!
      ENTRY VRBLOG ( MESSGE, ERRLOG )
!
!  Version
!
!-     Test Utilities Version 1.0.0, 3-NOV-1994 (WLT)
!


      IF ( ERRLOG ) THEN
!
!        We need to possibly open a log file and write some
!        special additional information if this is the first
!        message for this test case.
!

         CALL T_NAME  ( NAME   )
         CALL T_CNAME ( CNAME  )
         CALL T_FCOUNT( PNUM   )
         CALL T_CFAIL ( )
         CALL T_FCOUNT( FNUM   )

         IF ( OPNERR ) THEN

            OPNERR = .FALSE.

            CALL TSTFIL ( 'ERR{0-9}{0-9}{0-9}{0-9}.LOG', 'SAVE', ERRFIL)

            CALL TSTGET (  ENV, VERSN, TIME )
            CALL TSTIOA ( 'SAVE'   )
            CALL TSTIOH ( 'SCREEN' )
            CALL TSTIOH ( 'LOG'    )

            CALL TSTWLN ( ENV   )
            CALL TSTWLN ( VERSN )
            CALL TSTWLN ( TIME  )

         END IF
!
!        Activate the port for reporting errors, inhibit writing to the
!        screen or log file.
!
         CALL TSTIOA ( 'SAVE' )
         CALL TSTIOA ( 'SCREEN' )
         CALL TSTIOA ( 'LOG'    )
!
!        If there had been no previous errors reported for this
!        family of errors, we create a special message concerning
!        this family and case.
!
         IF ( PNUM .EQ. 0 ) THEN

            CALL TSTWLN( ' ' )
            CALL TSTWLN('A test failure occurred in the test family:')
            CALL TSTWLN( ' ' )
            CALL TSTWLN( 'Family : ' // NAME  )
            CALL NICEPR_1( CNAME, &
                        'LEFT 1 RIGHT 78 HARDSPACE ^ FLAG CASE^^^:', &
                         TSTWLN )
            CALL TSTWLN( ' ' )
!
!        If this is the first error message to be reported for this
!        test case we note the failure for this test case.
!
         ELSE IF ( PNUM .NE. FNUM ) THEN

            CALL TSTWLN( ' '                   )
            CALL TSTWLN( 'Test Case FAILURE. ' )
            CALL NICEPR_1( CNAME, &
                        'LEFT 1 RIGHT 78 HARDSPACE ^ FLAG CASE^^^:', &
                         TSTWLN )
            CALL TSTWLN( ' '                   )

         END IF
!
!        Reactivate the screen and log file.
!
         CALL TSTIOA  ( 'SCREEN' )
         CALL TSTIOA  ( 'LOG'    )

      END IF
!
!     If there is an error, write to every place.
!
      IF ( ERRLOG ) THEN

         IF ( MESSGE .EQ. ' ' ) THEN
            CALL TSTWLN ( ' ' )
         ELSE

            IF ( SHOW ) THEN
               CALL NICEPR_1 ( EXTRA, HIDE, TSTWLN )
               CALL TSTWLN ( ' ' )
               SHOW = .FALSE.
            END IF

            CALL NICEPR_1 ( MESSGE,  HIDE, TSTWLN )
         END IF
!
!     Otherwise we write out the message only if in verbose mode.
!
      ELSE IF ( VERBOS() ) THEN

         IF ( MESSGE .EQ. ' ' ) THEN
            CALL TSTWLN ( ' ' )
         ELSE
            CALL NICEPR_1 ( MESSGE,  SEEN, TSTWLN )
         END IF

      END IF
!
!     Inhibit writing to the error log.
!

      CALL TSTIOH ( 'SAVE' )

      RETURN


!
!     Establish a special message to be printed if an error is
!     detected.
!
      ENTRY TSTMSG ( MARKER, MESSGE )

         MYMARK = MARKER
         EXTRA =  MESSGE
         SHOW  = .TRUE.
         RETURN


!
!     Fill in the next marker with the string representing INT.
!
      ENTRY TSTMSI ( INT )

         CALL REPMI ( EXTRA, MYMARK, INT, EXTRA )
         RETURN


!
!     Fill in the next marker with the string representing DP
!
      ENTRY TSTMSD ( DP  )

         CALL REPMD ( EXTRA, MYMARK, DP, 14, EXTRA )
         RETURN



      ENTRY TSTMSF ( DP  )

         CALL REPMF ( EXTRA, MYMARK, DP, 14, 'F', EXTRA )
         RETURN
!
!     Fill in the next marker with the character string MESSGE.
!
      ENTRY TSTMSC ( MESSGE )

         R = RTRIM(MESSGE)

         CALL REPMC ( EXTRA, MYMARK, MESSGE(1:R), EXTRA )
         RETURN


!$Procedure TSTMSO ( Replace marker with ordinal )

      ENTRY TSTMSO ( INT, MARKER )

! $ Abstract
!
!     Fill in the next marker with an ordinal
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     TDB
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!
!     TDB
!
! $ Detailed_Input
!
!   None
!
! $ Detailed_Output
!
!   None
!
! $ Parameters
!
!     None.
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
!     TDB
!
! $ Examples
!
!     TDB
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
!     W.L. Taber (JPL)
!
! $ Version
!
!     TDB
!-&

! $ Index_Entries
!
!     TDB
!
!-&

         IF  (       ( MARKER .NE. 'C' ) &
               .AND. ( MARKER .NE. 'c' ) &
               .AND. ( MARKER .NE. 'l' ) &
               .AND. ( MARKER .NE. 'u' ) &
               .AND. ( MARKER .NE. 'L' ) &
               .AND. ( MARKER .NE. 'U' ) ) THEN

            CALL REPMOT ( EXTRA, MYMARK, INT, 'l',    EXTRA )

         ELSE

            CALL REPMOT ( EXTRA, MYMARK, INT, MARKER, EXTRA )

         END IF
         RETURN


      ENTRY TSTMST ( INT, MARKER )


         IF  (       ( MARKER .NE. 'C' ) &
               .AND. ( MARKER .NE. 'c' ) &
               .AND. ( MARKER .NE. 'l' ) &
               .AND. ( MARKER .NE. 'u' ) &
               .AND. ( MARKER .NE. 'L' ) &
               .AND. ( MARKER .NE. 'U' ) ) THEN

            CALL REPMCT ( EXTRA, MYMARK, INT, 'l',    EXTRA )

         ELSE

            CALL REPMCT ( EXTRA, MYMARK, INT, MARKER, EXTRA )

         END IF
         RETURN

!
!     Turn off the test message so that it will not be shown.
!
      ENTRY TSTMOF

         SHOW = .FALSE.

         RETURN




!$Procedure ZZTSTRE (reset the error log file flag)

      ENTRY ZZTSTRE

! $ Abstract
!
!   Reset OPNERR to TRUE. FINISH calls this routine immediately prior to
!   return. This action is required for those situations where the test
!   system runs in a persistent environment (e.g. IDL, MATLAB).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
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
!     None of the testutil routines reset the OPNERR flag after a
!     a test run - indicated by a call to tclose. The original testutil
!     design expected test program use in the conventioanl single run
!     environment such as a command line shell so this failure to reset
!     caused no effect. The failure to reset the flag can cause system
!     errors when repeatedly invoking a test program using the testutil
!     library in a persistent environment such as IDL or MATLAB.
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
!     E.D. Wright (JPL)
!
! $ Version
!
!-  Test Utilities Version 1.0.0, 18-JAN-2007 (EDW)
!
!-&

         OPNERR = .TRUE.

      END


