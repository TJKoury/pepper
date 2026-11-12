!$Procedure TSTIO ( Test utilities IO Manager )

      SUBROUTINE TSTIO ( LINE, NAME, PORT, OK, STATUS )
      use pepper_legacy

! $ Abstract
!
!     Test utilities IO Manager
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         LINE
      CHARACTER*(*)         NAME
      CHARACTER*(*)         PORT
      LOGICAL               OK
      LOGICAL               STATUS ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     LINE       I   TSTWLN
!     NAME       I   TSTOPN
!     PORT       I   TSTOPN, TSTIOH, TSTIOA, TSTGST, TSTPST, TSTIOC,
!                    TSTIOS, TSTIOR, LCKOUT
!     OK         O   TSTIOR
!     STATUS    I-O  TSTGST, TSTPST
!
! $ Detailed_Input
!
!     See the entry points.
!
! $ Detailed_Output
!
!     See the entry points.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.2.2, 06-MAY-2020 (JDR)
!
!        Updated headers of umbrella routine and entry points.
!
!-    Testing Utilities Version 1.2.1, 18-JAN-2007 (EDW)
!
!        Added addition lockout blocks to TSTPST and TSTIOC.
!
!        FINISH now calls ZZTSTRE prior to return to ensure
!        OPNERR has value TRUE after TCLOSE call.
!
!-    Testing Utilities Version 1.2.0, 09-MAY-2002 (EDW)
!
!        Added the LCKOUT entry mode. Routine inhibits
!        all output to the specified port. Primary use
!        prevents tspice from printing to the screen -
!        a quiet mode.
!
!-    Testing Utilities Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Added a RETURN before the first entry point.
!
!-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Test utilities IO Manager
!
!-&

!
!     Below are the various types of output files that
!     might be open.
!
      INTEGER               NPORTS
      PARAMETER           ( NPORTS = 4 )

      INTEGER               SCREEN
      PARAMETER           ( SCREEN = 6 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 127 )

!
!     Define tags for the PORTS array (defined in tstlgr.f)
!     indices corresponding to the log file, LOG ('LOG'),
!     the screen or standard out, STDOUT ('SCREEN'),
!     and the error log file SPOOL ('SAVE'). Note the 'SAVE'
!     port as defined in PORTS use the SPOOL tag because SAVE
!     is a FORTRAN intrinsic function.
!
      INTEGER               LOG
      PARAMETER           ( LOG    = 1 )

      INTEGER               STDOUT
      PARAMETER           ( STDOUT = 2 )

      INTEGER               SPOOL
      PARAMETER           ( SPOOL  = 3 )

!
!     SPICELIB functions
!

      INTEGER               ID
      INTEGER               IOSTAT
      INTEGER               R
      INTEGER               TO

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE  = 32 )

      INTEGER               MSGSIZ
      PARAMETER           ( MSGSIZ = 400 )

      CHARACTER*(MSGSIZ)    MESSGE
      CHARACTER*(WDSIZE)    PORTS  ( NPORTS )
      CHARACTER*(FILSIZ)    FILES  ( NPORTS )
      INTEGER               UNITS  ( NPORTS )
      LOGICAL               ACTIVE ( NPORTS )
      LOGICAL               OPEN   ( NPORTS )
      LOGICAL               SUSPND ( NPORTS )
      LOGICAL               LKPORT ( NPORTS )

      SAVE

      DATA PORTS   / 'LOG',   'SCREEN', 'SAVE',  'UTILITY'     /
      DATA FILES   / ' ',     ' ',      ' ',     ' '           /
      DATA UNITS   /  0,       SCREEN,   0,       0            /
      DATA ACTIVE  / .FALSE., .TRUE.,   .FALSE., .FALSE.       /
      DATA OPEN    / .FALSE., .TRUE.,   .FALSE., .FALSE.       /
      DATA SUSPND  / .FALSE., .FALSE.,  .FALSE., .FALSE.       /
      DATA LKPORT  / .FALSE., .FALSE.,  .FALSE., .FALSE.       /
      RETURN



!$Procedure TSTOPN ( Open a new port )

      ENTRY TSTOPN ( PORT, NAME )

! $ Abstract
!
!     Open a new port.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         PORT
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   Port that will be opened.
!     NAME       I   Name of the file attached to the port.
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
!     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
!        PORT values. PORTS defines acceptable ports.
!
! $ Files
!
!     None.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Open a new port
!
!-&

         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTOPN' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTOPN' )
            RETURN
         END IF


!
!        Check for an ID lockout.
!
         IF( LKPORT(ID) ) THEN
            RETURN
         END IF

         R = RTRIM(NAME)

         CALL TXTOPN ( NAME(1:R), UNITS(ID) )

         IF ( FAILED() ) THEN
            OPEN  (ID) = .FALSE.
            ACTIVE(ID) = .FALSE.
            SUSPND(ID) = .FALSE.
            FILES (ID) =  ' '
            RETURN
         END IF

         OPEN  (ID) = .TRUE.
         ACTIVE(ID) = .TRUE.
         SUSPND(ID) = .FALSE.
         FILES (ID) =  NAME


         RETURN



!$Procedure TSTIOH ( Inhibit output to a port )

      ENTRY TSTIOH ( PORT )

! $ Abstract
!
!     Inhibit output to a port.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         PORT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   Port to which the output will be inhibited.
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
!     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
!        PORT values. PORTS defines acceptable ports.
!
! $ Files
!
!     None.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Inhibit output to a port
!
!-&

         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTIOH' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTIOH' )
            RETURN
         END IF

         ACTIVE(ID) = .FALSE.
         RETURN



!$Procedure TSTIOA ( Activate a port )

      ENTRY TSTIOA ( PORT )

! $ Abstract
!
!     Activate a port.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         PORT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   Port to be activated.
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
!     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
!        PORT values. PORTS defines acceptable ports.
!
! $ Files
!
!     None.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Activate a port
!
!-&


         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTIOA' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTIOA' )
            RETURN
         END IF

!
!        Check for an ID lockout.
!
         IF( LKPORT(ID) ) THEN
            RETURN
         END IF

         ACTIVE(ID) = .TRUE.

         RETURN



!$Procedure TSTGST ( Get the current status of a port )

      ENTRY TSTGST ( PORT, STATUS )

! $ Abstract
!
!     Get the current status of a port.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         PORT
!     LOGICAL               STATUS ( 3 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I
!     STATUS     O   Current status of the PORT.
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
!     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
!        PORT values. PORTS defines acceptable ports.
!
! $ Files
!
!     None.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Get the current status of a port.
!
!-&

         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTGST' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTGST' )
            RETURN
         END IF



         STATUS(1) = ACTIVE(ID)
         STATUS(2) = OPEN  (ID)
         STATUS(3) = SUSPND(ID)

         RETURN




!$Procedure TSTPST ( Put the status of a port )

      ENTRY TSTPST ( PORT, STATUS )

! $ Abstract
!
!     Put the status of a port.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         PORT
!     LOGICAL               STATUS ( 3 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   Port whose STATUS will be set.
!     STATUS     I   Status of the PORT.
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
!     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
!        PORT values. PORTS defines acceptable ports.
!
! $ Files
!
!     None.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Put the status of a port.
!
!-&

         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTPST' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTPST' )
            RETURN
         END IF


!
!        Check for an ID lockout.
!
         IF( LKPORT(ID) ) THEN
            RETURN
         END IF

         ACTIVE(ID) = STATUS(1)
         OPEN  (ID) = STATUS(2)
         SUSPND(ID) = STATUS(3)

         RETURN



!$Procedure TSTIOC ( Close a port )

      ENTRY TSTIOC ( PORT )

! $ Abstract
!
!     Close a port.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         PORT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   Port to be closed.
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
!     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
!        PORT values. PORTS defines acceptable ports.
!
! $ Files
!
!     None.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Close a port.
!
!-&

         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTIOC' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTIOC' )
            RETURN
         END IF


!
!        Check for an ID lockout.
!
         IF( LKPORT(ID) ) THEN
            RETURN
         END IF

         CLOSE ( UNITS(ID) )

         ACTIVE(ID) = .FALSE.
         OPEN  (ID) = .FALSE.
         FILES (ID) = ' '
         SUSPND(ID) = .FALSE.
         RETURN





!$Procedure TSTIOS ( Suspend a port )

      ENTRY TSTIOS ( PORT )

! $ Abstract
!
!     Suspend a port with the possibly to be reopened later.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         PORT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   Port to be suspended.
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
!     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
!        PORT values. PORTS defines acceptable ports.
!
! $ Files
!
!     None.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Suspend a port with possibly to be reopened later.
!
!-&

         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTIOS' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTIOS' )
            RETURN
         END IF


!
!        Check for an ID lockout.
!
         IF( LKPORT(ID) ) THEN
            RETURN
         END IF


!
!        close ( units(id) )
!
         OPEN  ( ID ) = .FALSE.
         SUSPND( ID ) = .TRUE.

         RETURN



!$Procedure TSTIOR ( Reopen a suspended port )

      ENTRY TSTIOR ( PORT, OK )

! $ Abstract
!
!     Reopen a suspended port.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         PORT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   Port to be suspended.
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
!     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
!        PORT values. PORTS defines acceptable ports.
!
! $ Files
!
!     None.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Reopen a suspended port.
!
!-&

         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTIOR' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTIOR' )
            RETURN
         END IF

!
!        Check for an ID lockout.
!
         IF( LKPORT(ID) ) THEN
            RETURN
         END IF

         IF ( .NOT. SUSPND(ID) ) THEN
            OK = .FALSE.
            RETURN
         END IF

         OPEN  (ID) = .TRUE.
         SUSPND(ID) = .FALSE.
         OK         = .TRUE.

         RETURN



!$Procedure TSTWLN ( Write a line to all open and active ports )

      ENTRY TSTWLN ( LINE )

! $ Abstract
!
!     Write a line to all open and active ports.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         LINE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LINE       I   Line to be written to all open ports.
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Write a line to all open and active ports.
!
!-&

         DO ID = 1, NPORTS

            IF ( ACTIVE(ID) .AND. OPEN(ID) ) THEN

               TO  =   UNITS(ID)
               CALL WRITLN ( LINE(1:RTRIM(LINE)), TO )

               IF (      ID .NE. STDOUT &
                   .AND. FAILED()     ) THEN

                  R =  RTRIM( FILES(ID) )

                  MESSGE = 'I was unable to write to the file ' &
                   //       '#.  The value of IOSTAT returned ' &
                   //       'was #. '

                  CALL REPMC( MESSGE, '#', FILES(ID)(1:R), MESSGE )
                  CALL REPMI( MESSGE, '#', IOSTAT,         MESSGE )

                  CALL TOSTDO( MESSGE )

                  CLOSE ( UNITS(ID) )

                  ACTIVE( ID )  = .FALSE.
                  OPEN  ( ID )  = .FALSE.
                  SUSPND( ID )  = .FALSE.
                  FILES ( ID )  = ' '

               END IF

            END IF

         END DO

         RETURN


!$Procedure FINISH ( Close files and inform user of their location )

      ENTRY FINISH

! $ Abstract
!
!     Write a line to all open and active ports.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.1.1, 06-MAY-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.1.0, 18-JAN-2007 (WLT)
!
!        Added a call to ZZTSTRE to force a reset of the OPNERR flag.
!
!-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
!
!        This is the configured version of the Test Utility
!        software as of Nov 3, 1994
!
!-&

! $ Index_Entries
!
!     Write a line to all open and active ports.
!
!-&


!
!        Obtain the port ID for the SCREEN.
!
         ID = ISRCHC ( 'SCREEN', NPORTS, PORTS )

         IF ( OPEN(LOG) ) THEN

!           If a log file exists and a screen lockout is
!           not active, output the log file information message
!           to standard-out.
!
            IF ( ACTIVE(ID) .AND. OPEN(ID) ) THEN
               MESSGE = 'The log file was written to: '
               CALL TOSTDO ( MESSGE(1:RTRIM(MESSGE))          )
               CALL TOSTDO ( FILES(LOG)(1:RTRIM(FILES(LOG)))  )
               CALL TOSTDO ( ' ' )
            END IF

            CLOSE ( UNITS(LOG)   )
         END IF


         IF ( OPEN(SPOOL) ) THEN

!
!           If a log file exists and a screen lockout is
!           not active, output the error log file information message
!           to standard-out.
!
!           Recall SPOOL indicates the error log file identified
!           in the tstlgr.f PORTS array as 'SAVE'.
!
            IF ( ACTIVE(ID) .AND. OPEN(ID) ) THEN
               MESSGE = 'The list of test failures was written to: '
               CALL TOSTDO (  MESSGE(1:RTRIM(MESSGE))             )
               CALL TOSTDO (  FILES(SPOOL)(1:RTRIM(FILES(SPOOL))) )
               CALL TOSTDO ( ' ' )
            END IF

            CLOSE ( UNITS(SPOOL) )
         END IF

!
!        Reset the OPNERR boolean flag in tstlgr.f to TRUE. This action
!        is required for those situations where the test system does
!        not run in a single run environment (e.g. IDL, MATLAB).
!
         CALL ZZTSTRE

         RETURN



!$Procedure LCKOUT ( Lock out a port from use )

      ENTRY LCKOUT( PORT )

! $ Abstract
!
!     Lockout a specific port for all use (ports defined by PORTS
!     assignment in tstlgr.f).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations
!
!     IMPLICIT NONE
!
!     CHARACTER*(*)         PORT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   Port to be locked out from use.
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
!     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
!        PORT values. The PORTS defines acceptable ports.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Use of LCKOUT occurs only for situations when output to
!     standard out (STDOUT) must be prevented.
!
!     Not a subtle implementation, but effective.
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
!     J. Diaz del Rio    (ODC Space)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    Testing Utilities Version 1.1.1, 06-MAY-2020 (JDR)
!
!        Added standard SPICELIB NAIF header.
!
!-    Testing Utilities Version 1.0.0, 09-MAY-2002 (EDW)
!
!-&


! $ Index_Entries
!
!     Lock out a port from use.
!
!-&


!
!        Get the port ID.
!
         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'LCKOUT' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#', PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'LCKOUT' )
            RETURN
         END IF

!
!        Close everything.
!
         ACTIVE (ID) = .FALSE.
         OPEN   (ID) = .FALSE.
         SUSPND (ID) = .FALSE.
         LKPORT (ID) = .TRUE.

      END
