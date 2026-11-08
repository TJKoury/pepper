C$Procedure TSTIO ( Test utilities IO Manager )

      SUBROUTINE TSTIO ( LINE, NAME, PORT, OK, STATUS )

C$ Abstract
C
C     Test utilities IO Manager
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
C     TESTING
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         LINE
      CHARACTER*(*)         NAME
      CHARACTER*(*)         PORT
      LOGICAL               OK
      LOGICAL               STATUS ( 3 )

C$ Brief_I/O
C
C     VARIABLE  I/O  ENTRY POINTS
C     --------  ---  --------------------------------------------------
C     LINE       I   TSTWLN
C     NAME       I   TSTOPN
C     PORT       I   TSTOPN, TSTIOH, TSTIOA, TSTGST, TSTPST, TSTIOC,
C                    TSTIOS, TSTIOR, LCKOUT
C     OK         O   TSTIOR
C     STATUS    I-O  TSTGST, TSTPST
C
C$ Detailed_Input
C
C     See the entry points.
C
C$ Detailed_Output
C
C     See the entry points.
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
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.2.2, 06-MAY-2020 (JDR)
C
C        Updated headers of umbrella routine and entry points.
C
C-    Testing Utilities Version 1.2.1, 18-JAN-2007 (EDW)
C
C        Added addition lockout blocks to TSTPST and TSTIOC.
C
C        FINISH now calls ZZTSTRE prior to return to ensure
C        OPNERR has value TRUE after TCLOSE call.
C
C-    Testing Utilities Version 1.2.0, 09-MAY-2002 (EDW)
C
C        Added the LCKOUT entry mode. Routine inhibits
C        all output to the specified port. Primary use
C        prevents tspice from printing to the screen -
C        a quiet mode.
C
C-    Testing Utilities Version 1.1.0, 18-JUN-1999 (WLT)
C
C        Added a RETURN before the first entry point.
C
C-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Test utilities IO Manager
C
C-&

C
C     Below are the various types of output files that
C     might be open.
C
      INTEGER               NPORTS
      PARAMETER           ( NPORTS = 4 )

      INTEGER               SCREEN
      PARAMETER           ( SCREEN = 6 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 127 )

C
C     Define tags for the PORTS array (defined in tstlgr.f)
C     indices corresponding to the log file, LOG ('LOG'),
C     the screen or standard out, STDOUT ('SCREEN'),
C     and the error log file SPOOL ('SAVE'). Note the 'SAVE'
C     port as defined in PORTS use the SPOOL tag because SAVE
C     is a FORTRAN intrinsic function.
C
      INTEGER               LOG
      PARAMETER           ( LOG    = 1 )

      INTEGER               STDOUT
      PARAMETER           ( STDOUT = 2 )

      INTEGER               SPOOL
      PARAMETER           ( SPOOL  = 3 )

C
C     SPICELIB functions
C
      LOGICAL               FAILED
      INTEGER               ISRCHC
      INTEGER               RTRIM

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



C$Procedure TSTOPN ( Open a new port )

      ENTRY TSTOPN ( PORT, NAME )

C$ Abstract
C
C     Open a new port.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         PORT
C     CHARACTER*(*)         NAME
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   Port that will be opened.
C     NAME       I   Name of the file attached to the port.
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
C     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
C        PORT values. PORTS defines acceptable ports.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Open a new port
C
C-&

         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTOPN' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTOPN' )
            RETURN
         END IF


C
C        Check for an ID lockout.
C
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



C$Procedure TSTIOH ( Inhibit output to a port )

      ENTRY TSTIOH ( PORT )

C$ Abstract
C
C     Inhibit output to a port.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         PORT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   Port to which the output will be inhibited.
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
C     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
C        PORT values. PORTS defines acceptable ports.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Inhibit output to a port
C
C-&

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



C$Procedure TSTIOA ( Activate a port )

      ENTRY TSTIOA ( PORT )

C$ Abstract
C
C     Activate a port.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         PORT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   Port to be activated.
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
C     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
C        PORT values. PORTS defines acceptable ports.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Activate a port
C
C-&


         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTIOA' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTIOA' )
            RETURN
         END IF

C
C        Check for an ID lockout.
C
         IF( LKPORT(ID) ) THEN
            RETURN
         END IF

         ACTIVE(ID) = .TRUE.

         RETURN



C$Procedure TSTGST ( Get the current status of a port )

      ENTRY TSTGST ( PORT, STATUS )

C$ Abstract
C
C     Get the current status of a port.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         PORT
C     LOGICAL               STATUS ( 3 )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I
C     STATUS     O   Current status of the PORT.
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
C     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
C        PORT values. PORTS defines acceptable ports.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Get the current status of a port.
C
C-&

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




C$Procedure TSTPST ( Put the status of a port )

      ENTRY TSTPST ( PORT, STATUS )

C$ Abstract
C
C     Put the status of a port.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         PORT
C     LOGICAL               STATUS ( 3 )
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   Port whose STATUS will be set.
C     STATUS     I   Status of the PORT.
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
C     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
C        PORT values. PORTS defines acceptable ports.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Put the status of a port.
C
C-&

         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTPST' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTPST' )
            RETURN
         END IF


C
C        Check for an ID lockout.
C
         IF( LKPORT(ID) ) THEN
            RETURN
         END IF

         ACTIVE(ID) = STATUS(1)
         OPEN  (ID) = STATUS(2)
         SUSPND(ID) = STATUS(3)

         RETURN



C$Procedure TSTIOC ( Close a port )

      ENTRY TSTIOC ( PORT )

C$ Abstract
C
C     Close a port.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         PORT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   Port to be closed.
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
C     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
C        PORT values. PORTS defines acceptable ports.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Close a port.
C
C-&

         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTIOC' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTIOC' )
            RETURN
         END IF


C
C        Check for an ID lockout.
C
         IF( LKPORT(ID) ) THEN
            RETURN
         END IF

         CLOSE ( UNITS(ID) )

         ACTIVE(ID) = .FALSE.
         OPEN  (ID) = .FALSE.
         FILES (ID) = ' '
         SUSPND(ID) = .FALSE.
         RETURN





C$Procedure TSTIOS ( Suspend a port )

      ENTRY TSTIOS ( PORT )

C$ Abstract
C
C     Suspend a port with the possibly to be reopened later.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         PORT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   Port to be suspended.
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
C     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
C        PORT values. PORTS defines acceptable ports.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Suspend a port with possibly to be reopened later.
C
C-&

         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTIOS' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTIOS' )
            RETURN
         END IF


C
C        Check for an ID lockout.
C
         IF( LKPORT(ID) ) THEN
            RETURN
         END IF


C
C        close ( units(id) )
C
         OPEN  ( ID ) = .FALSE.
         SUSPND( ID ) = .TRUE.

         RETURN



C$Procedure TSTIOR ( Reopen a suspended port )

      ENTRY TSTIOR ( PORT, OK )

C$ Abstract
C
C     Reopen a suspended port.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         PORT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   Port to be suspended.
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
C     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
C        PORT values. PORTS defines acceptable ports.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Reopen a suspended port.
C
C-&

         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'TSTIOR' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#',  PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'TSTIOR' )
            RETURN
         END IF

C
C        Check for an ID lockout.
C
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



C$Procedure TSTWLN ( Write a line to all open and active ports )

      ENTRY TSTWLN ( LINE )

C$ Abstract
C
C     Write a line to all open and active ports.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         LINE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LINE       I   Line to be written to all open ports.
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
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.0.1, 06-MAY-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Write a line to all open and active ports.
C
C-&

         DO ID = 1, NPORTS

            IF ( ACTIVE(ID) .AND. OPEN(ID) ) THEN

               TO  =   UNITS(ID)
               CALL WRITLN ( LINE(1:RTRIM(LINE)), TO )

               IF (      ID .NE. STDOUT
     .             .AND. FAILED()     ) THEN

                  R =  RTRIM( FILES(ID) )

                  MESSGE = 'I was unable to write to the file '
     .             //       '#.  The value of IOSTAT returned '
     .             //       'was #. '

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


C$Procedure FINISH ( Close files and inform user of their location )

      ENTRY FINISH

C$ Abstract
C
C     Write a line to all open and active ports.
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
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
C     None.
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
C     J. Diaz del Rio    (ODC Space)
C     W.L. Taber         (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.1.1, 06-MAY-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.1.0, 18-JAN-2007 (WLT)
C
C        Added a call to ZZTSTRE to force a reset of the OPNERR flag.
C
C-    Testing Utilities Version 1.0.0, 03-NOV-1994 (WLT)
C
C        This is the configured version of the Test Utility
C        software as of Nov 3, 1994
C
C-&

C$ Index_Entries
C
C     Write a line to all open and active ports.
C
C-&


C
C        Obtain the port ID for the SCREEN.
C
         ID = ISRCHC ( 'SCREEN', NPORTS, PORTS )

         IF ( OPEN(LOG) ) THEN

C           If a log file exists and a screen lockout is
C           not active, output the log file information message
C           to standard-out.
C
            IF ( ACTIVE(ID) .AND. OPEN(ID) ) THEN
               MESSGE = 'The log file was written to: '
               CALL TOSTDO ( MESSGE(1:RTRIM(MESSGE))          )
               CALL TOSTDO ( FILES(LOG)(1:RTRIM(FILES(LOG)))  )
               CALL TOSTDO ( ' ' )
            END IF

            CLOSE ( UNITS(LOG)   )
         END IF


         IF ( OPEN(SPOOL) ) THEN

C
C           If a log file exists and a screen lockout is
C           not active, output the error log file information message
C           to standard-out.
C
C           Recall SPOOL indicates the error log file identified
C           in the tstlgr.f PORTS array as 'SAVE'.
C
            IF ( ACTIVE(ID) .AND. OPEN(ID) ) THEN
               MESSGE = 'The list of test failures was written to: '
               CALL TOSTDO (  MESSGE(1:RTRIM(MESSGE))             )
               CALL TOSTDO (  FILES(SPOOL)(1:RTRIM(FILES(SPOOL))) )
               CALL TOSTDO ( ' ' )
            END IF

            CLOSE ( UNITS(SPOOL) )
         END IF

C
C        Reset the OPNERR boolean flag in tstlgr.f to TRUE. This action
C        is required for those situations where the test system does
C        not run in a single run environment (e.g. IDL, MATLAB).
C
         CALL ZZTSTRE

         RETURN



C$Procedure LCKOUT ( Lock out a port from use )

      ENTRY LCKOUT( PORT )

C$ Abstract
C
C     Lockout a specific port for all use (ports defined by PORTS
C     assignment in tstlgr.f).
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
C     TESTING
C
C$ Declarations
C
C     IMPLICIT NONE
C
C     CHARACTER*(*)         PORT
C
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     PORT       I   Port to be locked out from use.
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
C     1) SPICE signals a TESTUTIL(BUG) for all unrecognized
C        PORT values. The PORTS defines acceptable ports.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Use of LCKOUT occurs only for situations when output to
C     standard out (STDOUT) must be prevented.
C
C     Not a subtle implementation, but effective.
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
C     J. Diaz del Rio    (ODC Space)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    Testing Utilities Version 1.1.1, 06-MAY-2020 (JDR)
C
C        Added standard SPICELIB NAIF header.
C
C-    Testing Utilities Version 1.0.0, 09-MAY-2002 (EDW)
C
C-&


C$ Index_Entries
C
C     Lock out a port from use.
C
C-&


C
C        Get the port ID.
C
         ID = ISRCHC ( PORT, NPORTS, PORTS )

         IF ( ID .EQ. 0 ) THEN
            CALL CHKIN  ( 'LCKOUT' )
            CALL SETMSG ( 'Unrecognized port: #'  )
            CALL ERRCH  ( '#', PORT)
            CALL SIGERR ( 'TESTUTIL(BUG)' )
            CALL CHKOUT ( 'LCKOUT' )
            RETURN
         END IF

C
C        Close everything.
C
         ACTIVE (ID) = .FALSE.
         OPEN   (ID) = .FALSE.
         SUSPND (ID) = .FALSE.
         LKPORT (ID) = .TRUE.

      END
