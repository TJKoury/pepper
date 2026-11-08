C$Procedure TSTLGR (Test utilities log files manager)

      SUBROUTINE TSTLGR( MESSGE, ERRLOG, GSTYLE, FSTYLE,
     .                   MARKER, INT,    DP )

C$ Abstract
C
C
C     Save the contents of the command to a log file and any save
C     file that might be open and active.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)         MESSGE
      LOGICAL               ERRLOG
      CHARACTER*(*)         GSTYLE
      CHARACTER*(*)         FSTYLE
      CHARACTER*(1)         MARKER
      INTEGER               INT
      DOUBLE PRECISION      DP

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TDB     
C
C$ Detailed_Input
C
C     TDB
C
C$ Detailed_Output
C
C     TDB
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     TDB
C
C$ Examples
C
C     TDB
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber (JPL)
C
C$ Version
C
C-     Test Utilities Version 2.0.0, 29-MAY-2010 (NJB)
C
C         Updated parameters to increase max length of test family
C         names from 12 characters to MAXFNM and increase max length of
C         test case names from 80 to MAXCNM.
C
C         Added bogus header sections to satisfy checkit.
C
C-     Test Utilities Version 1.3.0, 30-DEC-2002 (EDW)
C
C         Implemented the ZZTSTRE routine to reset the OPNERR
C         flag to TRUE.
C
C-     Test Utilities Version 1.2.1, 30-DEC-2002 (EDW)
C
C         Corrected several spelling errors.
C
C-     Test Utilities Version 1.2.0, 19-JUN-2001 (WLT)
C
C         Added an entry point that turns off the test message.  This
C         should be called only by TCASE.
C
C-     Test Utilities Version 1.1.0, 18-JUN-1999 (WLT)
C
C         Added a return before the first entry point.
C
C-     Test Utilities Version 1.0.0, 3-NOV-1994 (WLT)
C
C-&
 
C$ Index_Entries
C
C      N/A
C
C-&
 
      EXTERNAL              TSTWLN
C
C     Test Utility Functions.
C
      LOGICAL               VERBOS
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
C
C     Local Variables
C
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
 



C
C     This entry point handles the logging of commands.
C
      ENTRY TSTLOG ( MESSGE, ERRLOG )
C
C     Version
C
C-     Test Utilities Version 1.0.0, 3-NOV-1994 (WLT)
C
 
 
      IF ( ERRLOG ) THEN

C
C        We need to open a log file and write some
C        special additional information if this is the first
C        message for this test case.
C
 
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
C
C        Activate the port for reporting errors, inhibit writing to the
C        screen or log file.
C
         CALL TSTIOA ( 'SAVE' )
         CALL TSTIOA ( 'SCREEN' )
         CALL TSTIOA ( 'LOG'    )
C
C        If there had been no previous errors reported for this
C        family of errors, we create a special message concerning
C        this family and case.
C
         IF ( PNUM .EQ. 0 ) THEN
 
            CALL TSTWLN( ' ' )
            CALL TSTWLN('A test failure occurred in the test family:')
            CALL TSTWLN( ' ' )
            CALL TSTWLN( 'Family : ' // NAME  )
            CALL NICEPR_1( CNAME,
     .                  'LEFT 1 RIGHT 78 HARDSPACE ^ FLAG CASE^^^:',
     .                   TSTWLN )
            CALL TSTWLN( ' ' )
C
C        If this is the first error message to be reported for this
C        test case we note the failure for this test case.
C
         ELSE IF ( PNUM .NE. FNUM ) THEN
 
            CALL TSTWLN( ' '                   )
            CALL TSTWLN( 'Test Case FAILURE. ' )
            CALL NICEPR_1( CNAME,
     .                  'LEFT 1 RIGHT 78 HARDSPACE ^ FLAG CASE^^^:',
     .                   TSTWLN )
            CALL TSTWLN( ' '                   )
 
         END IF
C
C        Reactivate the screen and log file.
C
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
C
C     Inhibit writing to the error log.
C
 
      CALL TSTIOH ( 'SAVE' )
 
      RETURN



C
C     This entry point allows users to set the style used for
C     logging failure and non-failure and visible commands.
C
      ENTRY TSTLGS ( GSTYLE, FSTYLE )

C  Version
C
C-     Test Utilities Version 1.0.0, 3-NOV-1994 (WLT)
C
C 
 
      SEEN  = GSTYLE
      HIDE  = FSTYLE
 
      RETURN


 
C
C     This entry point allows users to get the style used for
C     logging failure and non-failure and visible commands.
C
      ENTRY TSTSTY ( GSTYLE, FSTYLE )

C  Version
C
C-     Test Utilities Version 1.0.0, 3-NOV-1994 (WLT)
C
C
      GSTYLE = SEEN
      FSTYLE = HIDE
 
      RETURN
 
 
C
C     This entry point handles the logging of commands when we want
C     only verbose logging.
C
      ENTRY VRBLOG ( MESSGE, ERRLOG )
C
C  Version
C
C-     Test Utilities Version 1.0.0, 3-NOV-1994 (WLT)
C
 
 
      IF ( ERRLOG ) THEN
C
C        We need to possibly open a log file and write some
C        special additional information if this is the first
C        message for this test case.
C
 
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
C
C        Activate the port for reporting errors, inhibit writing to the
C        screen or log file.
C
         CALL TSTIOA ( 'SAVE' )
         CALL TSTIOA ( 'SCREEN' )
         CALL TSTIOA ( 'LOG'    )
C
C        If there had been no previous errors reported for this
C        family of errors, we create a special message concerning
C        this family and case.
C
         IF ( PNUM .EQ. 0 ) THEN
 
            CALL TSTWLN( ' ' )
            CALL TSTWLN('A test failure occurred in the test family:')
            CALL TSTWLN( ' ' )
            CALL TSTWLN( 'Family : ' // NAME  )
            CALL NICEPR_1( CNAME,
     .                  'LEFT 1 RIGHT 78 HARDSPACE ^ FLAG CASE^^^:',
     .                   TSTWLN )
            CALL TSTWLN( ' ' )
C
C        If this is the first error message to be reported for this
C        test case we note the failure for this test case.
C
         ELSE IF ( PNUM .NE. FNUM ) THEN
 
            CALL TSTWLN( ' '                   )
            CALL TSTWLN( 'Test Case FAILURE. ' )
            CALL NICEPR_1( CNAME,
     .                  'LEFT 1 RIGHT 78 HARDSPACE ^ FLAG CASE^^^:',
     .                   TSTWLN )
            CALL TSTWLN( ' '                   )
 
         END IF
C
C        Reactivate the screen and log file.
C
         CALL TSTIOA  ( 'SCREEN' )
         CALL TSTIOA  ( 'LOG'    )
 
      END IF
C
C     If there is an error, write to every place.
C
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
C
C     Otherwise we write out the message only if in verbose mode.
C
      ELSE IF ( VERBOS() ) THEN
 
         IF ( MESSGE .EQ. ' ' ) THEN
            CALL TSTWLN ( ' ' )
         ELSE
            CALL NICEPR_1 ( MESSGE,  SEEN, TSTWLN )
         END IF
 
      END IF
C
C     Inhibit writing to the error log.
C
 
      CALL TSTIOH ( 'SAVE' )
 
      RETURN
 

C
C     Establish a special message to be printed if an error is
C     detected.
C
      ENTRY TSTMSG ( MARKER, MESSGE )
 
         MYMARK = MARKER
         EXTRA =  MESSGE
         SHOW  = .TRUE.
         RETURN
 

C
C     Fill in the next marker with the string representing INT.
C
      ENTRY TSTMSI ( INT )
 
         CALL REPMI ( EXTRA, MYMARK, INT, EXTRA )
         RETURN


C
C     Fill in the next marker with the string representing DP
C
      ENTRY TSTMSD ( DP  )
 
         CALL REPMD ( EXTRA, MYMARK, DP, 14, EXTRA )
         RETURN
 


      ENTRY TSTMSF ( DP  )
 
         CALL REPMF ( EXTRA, MYMARK, DP, 14, 'F', EXTRA )
         RETURN
C
C     Fill in the next marker with the character string MESSGE.
C
      ENTRY TSTMSC ( MESSGE )
 
         R = RTRIM(MESSGE)
 
         CALL REPMC ( EXTRA, MYMARK, MESSGE(1:R), EXTRA )
         RETURN
 

C$Procedure TSTMSO ( Replace marker with ordinal )

      ENTRY TSTMSO ( INT, MARKER )
 
C$ Abstract
C
C     Fill in the next marker with an ordinal
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     TDB
C 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C
C     TDB
C
C$ Detailed_Input
C
C   None
C
C$ Detailed_Output
C
C   None
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     TDB
C
C$ Examples
C
C     TDB
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber (JPL)
C
C$ Version
C
C     TDB
C-&
 
C$ Index_Entries
C
C     TDB
C
C-&
 
         IF  (       ( MARKER .NE. 'C' )
     .         .AND. ( MARKER .NE. 'c' )
     .         .AND. ( MARKER .NE. 'l' )
     .         .AND. ( MARKER .NE. 'u' )
     .         .AND. ( MARKER .NE. 'L' )
     .         .AND. ( MARKER .NE. 'U' ) ) THEN
 
            CALL REPMOT ( EXTRA, MYMARK, INT, 'l',    EXTRA )
 
         ELSE
 
            CALL REPMOT ( EXTRA, MYMARK, INT, MARKER, EXTRA )
 
         END IF
         RETURN
 
 
      ENTRY TSTMST ( INT, MARKER )
 
 
         IF  (       ( MARKER .NE. 'C' )
     .         .AND. ( MARKER .NE. 'c' )
     .         .AND. ( MARKER .NE. 'l' )
     .         .AND. ( MARKER .NE. 'u' )
     .         .AND. ( MARKER .NE. 'L' )
     .         .AND. ( MARKER .NE. 'U' ) ) THEN
 
            CALL REPMCT ( EXTRA, MYMARK, INT, 'l',    EXTRA )
 
         ELSE
 
            CALL REPMCT ( EXTRA, MYMARK, INT, MARKER, EXTRA )
 
         END IF
         RETURN
 
C
C     Turn off the test message so that it will not be shown.
C
      ENTRY TSTMOF
 
         SHOW = .FALSE.
 
         RETURN




C$Procedure ZZTSTRE (reset the error log file flag)

      ENTRY ZZTSTRE

C$ Abstract
C
C   Reset OPNERR to TRUE. FINISH calls this routine immediately prior to
C   return. This action is required for those situations where the test 
C   system runs in a persistent environment (e.g. IDL, MATLAB).
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations
C
C     None.
C
C$ Brief_I/O
C
C     None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None of the testutil routines reset the OPNERR flag after a
C     a test run - indicated by a call to tclose. The original testutil 
C     design expected test program use in the conventioanl single run 
C     environment such as a command line shell so this failure to reset
C     caused no effect. The failure to reset the flag can cause system
C     errors when repeatedly invoking a test program using the testutil
C     library in a persistent environment such as IDL or MATLAB.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     E.D. Wright (JPL)     
C
C$ Version
C
C-  Test Utilities Version 1.0.0, 18-JAN-2007 (EDW)
C
C-&

         OPNERR = .TRUE.

      END


