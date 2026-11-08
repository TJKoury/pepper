!$Procedure NSPIO (Inspekt I/O Manager)

      SUBROUTINE NSPIO ( LINE, PORT, NAME, STATUS, OK )

! $ Abstract
!
!    Manage file and screen logging information for Inspekt.
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
!     TEXT
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         LINE
      CHARACTER*(*)         PORT
      CHARACTER*(*)         NAME
      LOGICAL               STATUS ( 3 )
      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LINE       I   NSPWLN
!     PORT       I   NSPOPN, NSPIOH, NSPIOA, NSPGST, NSPPST, NSPIOC
!                    NSPIOS, NSPIOR, NSPPFL
!     NAME      I/O  NSPOPN, NSPPFL
!     STATUS    I/O  NSPGST, NSPPST
!     OK         O   NSPIOR
!
! $ Detailed_Input
!
!     LINE       is a string of text that is to be written to all the
!                open, active, non-suspended ports.
!
!     PORT       is a string that indicates the name of a port on which
!                to perform an operation.  Acceptable values are:
!
!                 Standard Output Port:
!
!                   'SCREEN'
!
!                 File Based Ports:
!
!                   'LOG'
!                   'SAVE'
!                   'UTILITY'
!                   'ERROR'
!                   'AUX1'
!                   'AUX2'
!                   'AUX3'
!
!     NAME       The name of a file to create and attach to a file
!                based port.
!
!     STATUS     An array of logicals that configures the status of
!                a port.  Acceptable values are as follows:
!
!                   STATUS(1) - Activity Status:
!                               .TRUE.  - the port is active
!                               .FALSE. - the port is inactive
!
!                   STATUS(2) - Open Status:
!                               .TRUE.  - the port is open
!                               .FALSE. - the port is closed
!
!                   STATUS(3) - Suspend Status:
!                               .TRUE.  - I/O on this port is suspended
!                               .FALSE. - I/O can proceed on this port
!
! $ Detailed_Output
!
!     NAME       The name of a file attached to a file based port.
!
!     STATUS     An array of logicals that describes the status of
!                a port.  A description of the values follows:
!
!                   STATUS(1) - Activity Status:
!                               .TRUE.  - the port is active
!                               .FALSE. - the port is inactive
!
!                   STATUS(2) - Open Status:
!                               .TRUE.  - the port is open
!                               .FALSE. - the port is closed
!
!                   STATUS(3) - Suspend Status:
!                               .TRUE.  - I/O on this port is suspended
!                               .FALSE. - I/O can proceed on this port
!
!     OK         is a logical that indicates whether the attempt to
!                reopen a suspended port succeeded.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the bogus entry point NSPIO is called directly, then the
!        error NSPIO(BOGUSENTRY) is signaled.
!
!     2) See entry points for exceptions specific to them.
!
! $ Files
!
!     1) This umbrella may be configured to simultaneously access
!        NPORTS number of files.  They are all opened using the
!        SPICELIB routine TXTOPN.
!
! $ Particulars
!
!     NSPIO is an umbrella that functions as an I/O manager.  It
!     is capable of interfacing with STDOUT as well as several
!     files at once.  To accomplish these management tasks, the
!     following entry points are provided:
!
!        NSPOPN - Open a port.
!
!        NSPIOH - Inhibit access to a port.
!        NSPIOA - Activate an inhibited port.
!
!        NSPGST - Get the status of a port.
!        NSPPST - Put the status of a port.
!
!        NSPIOS - Suspend access to a port.
!        NSPIOR - Reopen a suspended port.
!
!        NSPWLN - Write a line of text to all accessible ports.
!
!        NSPEND - Close all ports and reset the state of the I/O
!                 manager to the default.
!
!        NSPPFL - Retrieve the name of the file associated with a port.
!
!        NSPIOC - Close a port.
!
!     The following ports are provided for usage:
!
!        Standard Output Port:
!
!          'SCREEN'
!
!        File Based Ports:
!
!          'LOG'
!          'SAVE'
!          'UTILITY'
!          'ERROR'
!          'AUX1'
!          'AUX2'
!          'AUX3'
!
!     By default the SCREEN port is open and ready to receive lines
!     of text.  All of the file based ports are closed until opened
!     with NSPOPN.
!
!     NSPEND is provided to close all open ports and reset the I/O
!     manager back to its default state.  If the SCREEN port is
!     accessible for writing, then when the LOG, SAVE, and ERROR ports
!     are closed a message indicating where they may be found is
!     written to the screen port.  The ERROR port is a special case,
!     since if it was unsuccessfully opened, when NSPEND attempts to
!     close this port it writes a brief diagnostic indicating the
!     open failure.
!
!     The suspend and reopen entry points are provided for backwards
!     compatibility and should not be used in developing new code.
!
! $ Examples
!
!     See INSPEKT for examples.
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
!     W.L. Taber      (JPL)
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    NSPIO Version 2.0.1, 06-DEC-2021 (BVS)
!
!        Cleaned up non-printing characters.
!
!        Re-ordered header sections.
!
!-    NSPIO Version 2.0.0, 01-FEB-2000 (FST)
!
!        Added headers and ports ERROR, AUX1, AUX2, and AUX3.
!
!-    NSPIO Version 1.0.0, 15-ARP-1996 (WLT)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               RTRIM
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Other Functions
!
      INTEGER               ZZNSPPOK

!
!     Local Parameters
!
!
!     Error File Port Integer Code.
!
      INTEGER               EFILE
      PARAMETER           ( EFILE  = 5 )
!
!     Log File Port Integer Code.
!
      INTEGER               LOG
      PARAMETER           ( LOG    = 2 )

!
!     The number of total ports supported by this version of NSPIO.
!
      INTEGER               NPORTS
      PARAMETER           ( NPORTS = 8 )

!
!     The logical unit that is associated with STDOUT.
!
      INTEGER               SCREEN
      PARAMETER           ( SCREEN = 6 )

!
!     The maximum filename string length.
!
      INTEGER               SIZFIL
      PARAMETER           ( SIZFIL = 255 )

!
!     The maximum length of a message.
!
      INTEGER               SIZMSG
      PARAMETER           ( SIZMSG = 400 )

!
!     The maximum length of a word.
!
      INTEGER               SIZWRD
      PARAMETER           ( SIZWRD = 32 )

!
!     Spool Port Integer Code.
!
      INTEGER               SPOOL
      PARAMETER           ( SPOOL  = 4 )

!
!     Screen Port Integer Code.
!
      INTEGER               STDOUT
      PARAMETER           ( STDOUT = 1 )

!
!     Local Variables
!
      CHARACTER*(SIZFIL)    FILES  ( NPORTS )
      CHARACTER*(SIZMSG)    MESSGE
      CHARACTER*(SIZWRD)    PORTS  ( NPORTS )

      INTEGER               ID
      INTEGER               R
      INTEGER               TO
      INTEGER               UNITS  ( NPORTS )

      LOGICAL               ACTIVE ( NPORTS )
      LOGICAL               ERROPF
      LOGICAL               OPEN   ( NPORTS )
      LOGICAL               OPENOK
      LOGICAL               SUSPND ( NPORTS )

!
!     Save all local variables.
!
      SAVE

!
!     Initialize the PORT configuration arrays.
!
      DATA   PORTS   / 'SCREEN', 'LOG',   'SAVE',  'UTILITY', &
                       'ERROR',  'AUX1',  'AUX2',  'AUX3'     /

      DATA   FILES   / ' ',      ' ',     ' ',     ' ', &
                       ' ',      ' ',     ' ',     ' '        /

      DATA   UNITS   / SCREEN,   0,       0,       0, &
                       0,        0,       0,       0          /

      DATA   ACTIVE  / .TRUE.,   .FALSE., .FALSE., .FALSE., &
                       .FALSE.,  .FALSE., .FALSE., .FALSE.    /

      DATA   OPEN    / .TRUE.,   .FALSE., .FALSE., .FALSE., &
                       .FALSE.,  .FALSE., .FALSE., .FALSE.    /

      DATA   SUSPND  / .FALSE.,  .FALSE., .FALSE., .FALSE., &
                       .FALSE.,  .FALSE., .FALSE., .FALSE.    /

      DATA   ERROPF  / .FALSE. /

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPIO' )
         CALL SIGERR( 'NSPIO(BOGUSENTRY)' )
         CALL CHKOUT( 'NSPIO' )
      END IF

      RETURN


!$Procedure NSPOPN ( Inspekt I/O Manager -- Open Port )

      ENTRY NSPOPN ( PORT, NAME )

! $ Abstract
!
!     Open a new port.
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
!     TEXT
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         PORT
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   String specifying which port to open.
!     NAME       I   The name of the file to open and attach to PORT.
!
! $ Detailed_Input
!
!     PORT       is a string that indicates the name of a port on which
!                to perform an operation.  Acceptable values are:
!
!                 Standard Output Port:
!
!                   'SCREEN'
!
!                 File Based Ports:
!
!                   'LOG'
!                   'SAVE'
!                   'UTILITY'
!                   'ERROR'
!                   'AUX1'
!                   'AUX2'
!                   'AUX3'
!
!     NAME       The name of a file to create and attach to a file
!                based port.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See NSPIO.
!
! $ Exceptions
!
!     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
!        is signaled by ZZNSPPOK.  Note, in this case, the status of
!        all ports remains the same.
!
!     2) If PORT is file based and already open, then NSPOPN closes
!        the file attached to PORT and opens the requested new one.
!
!     3) If PORT is 'SCREEN', then this entry point does nothing.
!
!     4) If PORT is 'ERROR', then if an error occurs opening the
!        file, this routine simply leaves the port un-open and
!        returns.
!
!     4) Any errors that occur in opening the files not associated with
!        the 'SCREEN' and 'ERROR' ports are processed by TXTOPN.
!
! $ Files
!
!     1) If PORT is a file-based port then this routine will open
!        a file with the SPICE routine TXTOPN.
!
!     2) If PORT is already attached to a file, then this file
!        is closed before PORT is attached to a new file.
!
! $ Particulars
!
!     See NSPIO.
!
! $ Examples
!
!     See NSPIO.
!
! $ Restrictions
!
!     1) NAME should point to a non-existent file that can be opened
!        for write access.
!
!     2) NAME should be a string of less than SIZFIL characters in
!        length.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    NSPIO Version 2.0.0, 01-FEB-2000 (FST)
!
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPOPN' )
      END IF

!
!     Find the integer associated with PORT.
!
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )

!
!     See if an error has been signaled. If so, do nothing
!     further and return.
!
      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'NSPOPN' )
         RETURN

      END IF

!
!     First check to see whether we are dealing with the SCREEN
!     port.  If we are return, do nothing and return.
!
      IF ( ID .EQ. STDOUT ) THEN

         CALL CHKOUT ( 'NSPOPN' )
         RETURN

      END IF

!
!     Now at this point we have a request to open a file based
!     port.  Check first to see if it is already open.
!
      IF ( OPEN(ID) ) THEN

!
!        If the file attached to PORT is already open, close it
!        before attaching this new file to it.
!
         CLOSE( UNIT = UNITS(ID) )

!
!        Now reset PORT's status.
!
         ACTIVE(ID) = .FALSE.
         OPEN  (ID) = .FALSE.
         SUSPND(ID) = .FALSE.
         FILES (ID) = ' '

      END IF

!
!     Check to see if we are opening the ERROR port.  We treat
!     this port differently from the other file based ports, since
!     if an error occurs opening the file, no error is signaled.
!     The port is simply not opened.
!
      IF ( ID .EQ. EFILE ) THEN

!
!        Assume there is will be no error opening the file.
!
         ERROPF = .FALSE.

!
!        Attempt to open the file.
!
         R = RTRIM(NAME)
         CALL ZZTXTOPN ( NAME(1:R), UNITS(ID), OPENOK )

!
!        If the OPEN process failed, then clear the status of the
!        port and return.
!
         IF ( .NOT. OPENOK ) THEN

            ACTIVE(ID) = .FALSE.
            OPEN  (ID) = .FALSE.
            SUSPND(ID) = .FALSE.

!
!           Leave FILES(ID) set, so that the name of the file can
!           be reported.
!

            FILES (ID) = NAME

!
!           Before returning, set ERROPF to .TRUE., since
!           this will facilitate the creation of the warning
!           message when NSPEND is invoked.
!
            ERROPF = .TRUE.

            CALL CHKOUT ( 'NSPOPN' )
            RETURN

         END IF

!
!     Consider all other file based ports.  For these ports we will
!     signal errors if TXTOPN is incapable of opening the file.
!
      ELSE
!
!        Open the new file.
!
         R = RTRIM(NAME)
         CALL TXTOPN ( NAME(1:R), UNITS(ID) )

!
!        Check FAILED(). If an error has occurred, clear PORT status,
!        check out and return.
!
         IF ( FAILED() ) THEN

            ACTIVE(ID) = .FALSE.
            OPEN  (ID) = .FALSE.
            SUSPND(ID) = .FALSE.
            FILES (ID) = ' '
            CALL CHKOUT ( 'NSPOPN' )
            RETURN

         END IF

      END IF

!
!     If we made it this far, then the file was opened successfully.
!     Set PORT status to reflect successful open.
!
      ACTIVE(ID) = .TRUE.
      OPEN  (ID) = .TRUE.
      SUSPND(ID) = .FALSE.
      FILES (ID) = NAME

      CALL CHKOUT( 'NSPOPN' )
      RETURN




!$Procedure NSPIOH ( Inspekt I/O Manager -- Inhibit Port )

      ENTRY NSPIOH ( PORT )

! $ Abstract
!
!     Inhibit output to a port.
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
!     TEXT
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         PORT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   String specifying which port to inhibit.
!
! $ Detailed_Input
!
!     PORT       is a string that indicates the name of a port on which
!                to perform an operation.  Acceptable values are:
!
!                 Standard Output Port:
!
!                   'SCREEN'
!
!                 File Based Ports:
!
!                   'LOG'
!                   'SAVE'
!                   'UTILITY'
!                   'ERROR'
!                   'AUX1'
!                   'AUX2'
!                   'AUX3'
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See NSPIO.
!
! $ Exceptions
!
!     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
!        is signaled by ZZNSPPOK.
!
!     2) If PORT is already inhibited, then it remains inhibited.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See NSPIO.
!
! $ Examples
!
!     See NSPIO.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    NSPIO Version 2.0.0, 01-FEB-2000 (FST)
!
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPIOH' )
      END IF

!
!     Find the integer associated with PORT.
!
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )

!
!     Inhibit I/O to the port, if no error was signaled. Note - if
!     the port is already inhibited, then this does not change it's
!     state.
!
      IF ( .NOT. FAILED() ) THEN

         ACTIVE(ID) = .FALSE.

      END IF

      CALL CHKOUT ( 'NSPIOH' )
      RETURN




!$Procedure NSPIOA ( Inspekt I/O Manager -- Activate Port )

      ENTRY NSPIOA ( PORT )

! $ Abstract
!
!     Activate a port.
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
!     TEXT
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         PORT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   String specifying which port to activate.
!
! $ Detailed_Input
!
!     PORT       is a string that indicates the name of a port on which
!                to perform an operation.  Acceptable values are:
!
!                 Standard Output Port:
!
!                   'SCREEN'
!
!                 File Based Ports:
!
!                   'LOG'
!                   'SAVE'
!                   'UTILITY'
!                   'ERROR'
!                   'AUX1'
!                   'AUX2'
!                   'AUX3'
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See NSPIO.
!
! $ Exceptions
!
!     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
!        is signaled by ZZNSPPOK.
!
!     2) If PORT is already active, then PORT remains active.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See NSPIO.
!
! $ Examples
!
!     See NSPIO.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    NSPIO Version 2.0.0, 02-FEB-2000 (FST)
!
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPIOA' )
      END IF

!
!     Find the integer associated with PORT.
!
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )

!
!     Activate the port, if no error was signaled. Note - if PORT was
!     already activated, then it will remain activated.
!
      IF ( .NOT. FAILED() ) THEN

         ACTIVE(ID) = .TRUE.

      END IF

      CALL CHKOUT ( 'NSPIOA' )
      RETURN



!$Procedure NSPGST ( Inspekt I/O Manager -- Get Port Status )

      ENTRY NSPGST ( PORT, STATUS )

! $ Abstract
!
!     Get the current status of a port.
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
!     TEXT
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         PORT
!     LOGICAL               STATUS ( 3 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   String specifying which port to fetch status.
!     STATUS     O   An array of logicals that indicates port status.
!
! $ Detailed_Input
!
!     PORT       is a string that indicates the name of a port on which
!                to perform an operation.  Acceptable values are:
!
!                 Standard Output Port:
!
!                   'SCREEN'
!
!                 File Based Ports:
!
!                   'LOG'
!                   'SAVE'
!                   'UTILITY'
!                   'ERROR'
!                   'AUX1'
!                   'AUX2'
!                   'AUX3'
!
! $ Detailed_Output
!
!     STATUS     An array of logicals that describes the status of
!                a port.  A description of the values follows:
!
!                   STATUS(1) - Activity Status:
!                               .TRUE.  - the port is active
!                               .FALSE. - the port is inactive
!
!                   STATUS(2) - Open Status:
!                               .TRUE.  - the port is open
!                               .FALSE. - the port is closed
!
!                   STATUS(3) - Suspend Status:
!                               .TRUE.  - I/O on this port is suspended
!                               .FALSE. - I/O can proceed on this port
!
!
! $ Parameters
!
!     See NSPIO.
!
! $ Exceptions
!
!     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
!        is signaled by ZZNSPPOK.  In the event this happens
!        the routine does not alter the contents of STATUS.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See NSPIO.
!
! $ Examples
!
!     See NSPIO.
!
! $ Restrictions
!
!     1) STATUS must be an array with space for 3 logicals.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    NSPIO Version 2.0.0, 03-FEB-2000 (FST)
!
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPGST' )
      END IF

!
!     Find the integer associated with PORT.
!
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )

!
!     Return the status of the port if no error was signaled.
!
      IF ( .NOT. FAILED() ) THEN

         STATUS(1) = ACTIVE(ID)
         STATUS(2) = OPEN  (ID)
         STATUS(3) = SUSPND(ID)

      END IF

      CALL CHKOUT ( 'NSPGST' )
      RETURN




!$Procedure NSPPST ( Inspekt I/O Manager -- Put Port Status )

      ENTRY NSPPST ( PORT, STATUS )

! $ Abstract
!
!     Put the status of a port.
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
!     TEXT
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         PORT
!     LOGICAL               STATUS ( 3 )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   String specifying which port to receive status.
!     STATUS     O   An array of logicals that indicates port status.
!
! $ Detailed_Input
!
!     PORT       is a string that indicates the name of a port on which
!                to perform an operation.  Acceptable values are:
!
!                 Standard Output Port:
!
!                   'SCREEN'
!
!                 File Based Ports:
!
!                   'LOG'
!                   'SAVE'
!                   'UTILITY'
!                   'ERROR'
!                   'AUX1'
!                   'AUX2'
!                   'AUX3'
!
!     STATUS     An array of logicals that describes the status of
!                a port.  A description of the values follows:
!
!                   STATUS(1) - Activity Status:
!                               .TRUE.  - the port is active
!                               .FALSE. - the port is inactive
!
!                   STATUS(2) - Open Status:
!                               .TRUE.  - the port is open
!                               .FALSE. - the port is closed
!
!                   STATUS(3) - Suspend Status:
!                               .TRUE.  - I/O on this port is suspended
!                               .FALSE. - I/O can proceed on this port
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See NSPIO.
!
! $ Exceptions
!
!     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
!        is signaled by ZZNSPPOK.  In the event this happens
!        the routine does not alter the status of any PORT.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See NSPIO.
!
! $ Examples
!
!     See NSPIO.
!
! $ Restrictions
!
!     1) The STATUS array must provide at least 3 logicals.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    NSPIO Version 2.0.0, 03-FEB-2000 (FST)
!
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPPST' )
      END IF

!
!     Find the integer associated with PORT.
!
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )

!
!     Set the status of the port if no error was signaled.
!
      IF ( .NOT. FAILED() ) THEN

         ACTIVE(ID) = STATUS(1)
         OPEN  (ID) = STATUS(2)
         SUSPND(ID) = STATUS(3)

      END IF

      CALL CHKOUT ( 'NSPPST' )
      RETURN



!$Procedure NSPIOC ( Inspekt I/O Manager -- Close Port )

      ENTRY NSPIOC ( PORT )

! $ Abstract
!
!    Close a port.
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
!     TEXT
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         PORT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   String specifying which port to close.
!
! $ Detailed_Input
!
!     PORT       is a string that indicates the name of a port on which
!                to perform an operation.  Acceptable values are:
!
!                 Standard Output Port:
!
!                   'SCREEN'
!
!                 File Based Ports:
!
!                   'LOG'
!                   'SAVE'
!                   'UTILITY'
!                   'ERROR'
!                   'AUX1'
!                   'AUX2'
!                   'AUX3'
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See NSPIO.
!
! $ Exceptions
!
!     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
!        is signaled by ZZNSPPOK.  In the event this happens
!        the routine does not alter the contents of STATUS.
!
!     2) If PORT is already closed, then this routine does nothing,
!        and simply returns.
!
!     3) Attempting to "close" the screen port will have no effect.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See NSPIO.
!
! $ Examples
!
!     See NSPIO.
!
! $ Restrictions
!
!     1) PORT must refer to a file based port.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    NSPIO Version 2.0.0, 03-FEB-2000 (FST)
!
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPIOC' )
      END IF

!
!     Find the integer associated with PORT.
!
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )

!
!     Check FAILED() to see if an error was signaled, or if
!     ID refers to the SCREEN port.  In either case, return without
!     doing anything.
!
      IF ( FAILED() .OR. ( ID .EQ. STDOUT ) ) THEN

         CALL CHKOUT ( 'NSPIOC' )
         RETURN

      END IF

!
!     Now check to see if the port is currently closed or if the
!     requested port to close is the SCREEN port.
!
      IF ( ( .NOT. OPEN(ID) ) .OR. ( ID .EQ. STDOUT ) ) THEN

         CALL CHKOUT ( 'NSPIOC' )
         RETURN

      END IF

!
!     If we make it this far, then we were given an open file
!     based port.  Close the port and reset its status.
!
      CLOSE ( UNITS(ID) )
      ACTIVE(ID) = .FALSE.
      OPEN  (ID) = .FALSE.
      SUSPND(ID) = .FALSE.
      FILES (ID) = ' '

!
!     If we have closed the error file, then clear ERROPF.
!
      IF ( ID .EQ. EFILE ) THEN

         ERROPF = .FALSE.

      END IF

      CALL CHKOUT ( 'NSPIOC' )
      RETURN




!$Procedure NSPIOS ( Inspekt I/O Manager -- Suspend Port )

      ENTRY NSPIOS ( PORT )

! $ Abstract
!
!     Suspend a port.
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
!     TEXT
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         PORT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   String specifying which port to suspend.
!
! $ Detailed_Input
!
!     PORT       is a string that indicates the name of a port on which
!                to perform an operation.  Acceptable values are:
!
!                 Standard Output Port:
!
!                   'SCREEN'
!
!                 File Based Ports:
!
!                   'LOG'
!                   'SAVE'
!                   'UTILITY'
!                   'ERROR'
!                   'AUX1'
!                   'AUX2'
!                   'AUX3'
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See NSPIO.
!
! $ Exceptions
!
!     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
!        is signaled by ZZNSPPOK.  In the event this happens
!        the routine does not alter the contents of STATUS.
!
!     2) If PORT is already has it's I/O suspended, then it will
!        remain suspended.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See NSPIO.
!
! $ Examples
!
!     See NSPIO.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    NSPIO Version 2.0.0, 08-FEB-2000 (FST)
!
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPIOS' )
      END IF

!
!     Find the integer associated with PORT.
!
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )

!
!     Suspend I/O on the port, if no error was signaled. Note - if
!     PORT was already suspended, then it will remain suspended.
!
      IF ( .NOT. FAILED() ) THEN

!
!        Suspend I/O on PORT.
!
         SUSPND (ID) = .TRUE.

      END IF

      CALL CHKOUT ( 'NSPIOS' )
      RETURN




!$Procedure NSPIOR ( Inspekt I/O Manager -- Reopen Port )

      ENTRY NSPIOR ( PORT, OK )

! $ Abstract
!
!     Reopen a suspended port.
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
!     TEXT
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         PORT
!     LOGICAL               OK
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   String specifying which port to re-open.
!     OK         O   logical that indicates a successful re-open.
!
! $ Detailed_Input
!
!     PORT       is a string that indicates the name of a port on which
!                to perform an operation.  Acceptable values are:
!
!                 Standard Output Port:
!
!                   'SCREEN'
!
!                 File Based Ports:
!
!                   'LOG'
!                   'SAVE'
!                   'UTILITY'
!                   'ERROR'
!                   'AUX1'
!                   'AUX2'
!                   'AUX3'
!
! $ Detailed_Output
!
!     OK         is a logical that indicates whether the attempt to
!                reopen a suspended port succeeded.
!
! $ Parameters
!
!     See NSPIO.
!
! $ Exceptions
!
!     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
!        is signaled by ZZNSPPOK.
!
!     2) If PORT is already not suspended, then PORT remains so and
!        OK is returned as .FALSE.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See NSPIO.
!
! $ Examples
!
!     See NSPIO.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    NSPIO Version 2.0.0, 08-FEB-2000 (FST)
!
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPIOR' )
      END IF

!
!     Find the integer associated with PORT.
!
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )

!
!     See if an error has been signaled. If so, do nothing
!     further and return.
!
      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'NSPIOR' )
         RETURN

      END IF

!
!     Check to see if PORT is currently suspended.
!
      IF ( .NOT. SUSPND(ID) ) THEN

!
!        If it's not, then set OK to .FALSE. and return
!
         OK = .FALSE.
         CALL CHKOUT( 'NSPIOR' )
         RETURN

      END IF

!
!     Suspend I/O to this port.
!
      SUSPND(ID) = .FALSE.

      CALL CHKOUT( 'NSPIOR' )
      RETURN




!$Procedure NSPWLN ( Inspekt I/O Manager -- Write Line )

      ENTRY NSPWLN ( LINE )

! $ Abstract
!
!     Write a line to all open and active ports.
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
!     TEXT
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         LINE
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LINE       I   is a line of text to be written to available ports.
!
! $ Detailed_Input
!
!     LINE       is a string of text that is to be written to all the
!                open, active, non-suspended ports.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See NSPIO.
!
! $ Exceptions
!
!     1) If an error occurs writing the line to a particular port,
!        then this routine closes that port, resets its status, and
!        continues writing LINE to the other ports.
!
!     2) Any errors are signaled by routines in the call tree of
!        NSPWLN.
!
! $ Files
!
!     1) This routine will write to any files associated with ports
!        that are open, active, and not suspended when NSPWLN is
!        called.
!
! $ Particulars
!
!     See NSPIO.
!
! $ Examples
!
!     See NSPIO.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    NSPIO Version 2.0.0, 08-FEB-2000 (FST)
!
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPWLN' )
      END IF

!
!     Write to all the open, active, and non-suspended ports.
!
      DO ID = 1, NPORTS

         IF (  ( .NOT. SUSPND(ID) )  .AND. &
                      ACTIVE(ID)     .AND. &
                      OPEN  (ID)           ) THEN

!
!           Write the line to this port.
!
            TO = UNITS(ID)
            CALL WRITLN ( LINE, TO )

!
!           Check for and process any errors.
!
            IF ( ( ID .NE. STDOUT ) .AND. &
                 ( FAILED()       )       ) THEN

!
!              If we have encountered an error then close the
!              file and reset the port status.  Note we do not
!              need to reset error status to continue, since
!              WRITLN does not check RETURN().
!
               CLOSE ( UNIT = UNITS(ID) )
               ACTIVE(ID) = .FALSE.
               OPEN  (ID) = .FALSE.
               SUSPND(ID) = .FALSE.
               FILES (ID) = ' '

            END IF

         END IF

      END DO

      CALL CHKOUT ( 'NSPWLN' )
      RETURN




!$Procedure NSPEND ( Inspekt I/O Manager -- Finished with I/O )

      ENTRY NSPEND

! $ Abstract
!
!     The final entry point handles closing files and informing
!     the user of the location of these files.
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
!     TEXT
!     UTILITY
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
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
!     See NSPIO.
!
! $ Exceptions
!
!     1) If the SCREEN port is not open, it simply closes the port
!        and does not write any notifications.
!
! $ Files
!
!     1) This routine closes the files attached to all open ports.
!
! $ Particulars
!
!     See NSPIO.
!
! $ Examples
!
!     See NSPIO.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    NSPIO Version 2.0.0, 09-FEB-2000 (FST)
!
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPEND' )
      END IF

!
!     If the LOG port is open, then notify the user about it's
!     location, and close it.
!
      IF ( OPEN(LOG) ) THEN

         CALL TRNLAT ( 'LOGFILWRITTENTO', MESSGE )

         IF (  ( .NOT. SUSPND(STDOUT) )  .AND. &
                       ACTIVE(STDOUT)    .AND. &
                       OPEN  (STDOUT)           ) THEN

!
!           Write the message.
!
            CALL WRITLN ( ' ',             SCREEN )

            R = RTRIM(MESSGE)
            CALL WRITLN ( MESSGE(1:R),     SCREEN )

            R = RTRIM(FILES(LOG))
            CALL WRITLN ( FILES(LOG)(1:R), SCREEN )

         END IF

      END IF

!
!     If the SAVE port is open, then notify the user about it's
!     location, and close it.
!
      IF ( OPEN(SPOOL) ) THEN

         CALL TRNLAT ( 'SAVFILWRITTENTO', MESSGE )

         IF (  ( .NOT. SUSPND(STDOUT) )  .AND. &
                       ACTIVE(STDOUT)    .AND. &
                       OPEN  (STDOUT)           ) THEN

!
!           Write the message.
!
            CALL WRITLN ( ' ',               SCREEN )

            R = RTRIM(MESSGE)
            CALL WRITLN ( MESSGE(1:R),       SCREEN )

            R = RTRIM(FILES(SPOOL))
            CALL WRITLN ( FILES(SPOOL)(1:R), SCREEN )

         END IF

      END IF

!
!     If the ERROR port is open, then notify the user about it's
!     location, and close it.
!
      IF ( OPEN(EFILE) ) THEN

         CALL TRNLAT ( 'ERRFILWRITTENTO', MESSGE )

         IF (  ( .NOT. SUSPND(STDOUT) )  .AND. &
                       ACTIVE(STDOUT)    .AND. &
                       OPEN  (STDOUT)           ) THEN

!
!           Write the message.
!
            CALL WRITLN ( ' ',               SCREEN )

            R = RTRIM(MESSGE)
            CALL WRITLN ( MESSGE(1:R),       SCREEN )

            R = RTRIM(FILES(EFILE))
            CALL WRITLN ( FILES(EFILE)(1:R), SCREEN )

         END IF

      ELSE IF ( ERROPF ) THEN

         CALL TRNLAT ( 'ERRFILWRITEFAIL', MESSGE )

         IF (  ( .NOT. SUSPND(STDOUT) )  .AND. &
                       ACTIVE(STDOUT)    .AND. &
                       OPEN  (STDOUT)           ) THEN

!
!           Write the message.
!
            CALL WRITLN ( ' ',               SCREEN )

            R = RTRIM(MESSGE)
            CALL WRITLN ( MESSGE(1:R),       SCREEN )

            R = RTRIM(FILES(EFILE))
            CALL WRITLN ( FILES(EFILE)(1:R), SCREEN )

         END IF

      END IF

!
!     Close all ports and restore NSPIO status to it's uninitialized
!     state.  First handle the screen port, since it's an exception.
!
      ACTIVE(STDOUT) = .TRUE.
      OPEN  (STDOUT) = .TRUE.
      SUSPND(STDOUT) = .FALSE.

!
!     Now reset the file based ports.
!
      DO ID = 2, NPORTS

!
!        Close the file associated with the port if it's open.
!
         IF ( OPEN(ID) ) THEN

            CLOSE ( UNIT = UNITS(ID) )

         END IF
!
!        Restore original port status.
!
         UNITS (ID) = 0
         FILES (ID) = ' '
         ACTIVE(ID) = .FALSE.
         OPEN  (ID) = .FALSE.
         SUSPND(ID) = .FALSE.


      END DO

      CALL CHKOUT ( 'NSPEND' )
      RETURN




!$Procedure NSPPFL ( Inspekt I/O Manager -- Fetch file name )

      ENTRY NSPPFL ( PORT, NAME )

! $ Abstract
!
!     Get the name of the file associated with a port.
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
!     TEXT
!     UTILITY
!
! $ Declarations
!
!
!     CHARACTER*(*)         PORT
!     CHARACTER*(*)         NAME
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     PORT       I   String that indicates the name of the port.
!     NAME       O   String holding the filename associated with PORT.
!
! $ Detailed_Input
!
!     PORT       is a string that indicates the name of a port on which
!                to perform an operation.  Acceptable values are:
!
!                 Standard Output Port:
!
!                   'SCREEN'
!
!                 File Based Ports:
!
!                   'LOG'
!                   'SAVE'
!                   'UTILITY'
!                   'ERROR'
!                   'AUX1'
!                   'AUX2'
!                   'AUX3'
!
! $ Detailed_Output
!
!     NAME       The name of a file attached to a file based port.
!
! $ Parameters
!
!     See NSPIO.
!
! $ Exceptions
!
!     1) If PORT is improperly specified, the error NSPIO(UNKNOWNPORT)
!        is signaled by ZZNSPPOK and NAME is set to ' '.
!
!     2) If PORT is 'SCREEN' then NSPPFL sets NAME to ' '.
!
!     3) If PORT is INACTIVE, SUSPENDED, or CLOSED, then NAME is
!        returned as ' '.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     See NSPIO.
!
! $ Examples
!
!     See NSPIO.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    NSPIO Version 2.0.0, 09-FEB-2000 (FST)
!
!
!-&

!
!     Standard SPICELIB error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'NSPPFL' )
      END IF

!
!     Find the integer associated with PORT.
!
      ID = ZZNSPPOK ( PORT, NPORTS, PORTS )

!
!     See if an error has been signaled. If so, clear NAME
!     and return.
!
      IF ( FAILED() ) THEN

         NAME = ' '
         CALL CHKOUT ( 'NSPPFL' )
         RETURN

!
!     If the ID refers to an active, open, non-suspended port, then
!     set NAME to the name of the file.  Note: in the case when PORT
!     is 'SCREEN', the corresponding entry in the FILES array is ' '.
!
      ELSE IF (  ( .NOT. SUSPND(ID) )  .AND. &
                         ACTIVE(ID)    .AND. &
                         OPEN  (ID)           ) THEN

         NAME = FILES(ID)

!
!     If PORT is inactive, suspended or closed, set NAME to ' '.
!
      ELSE

         NAME = ' '

      END IF

      CALL CHKOUT ( 'NSPPFL' )
      RETURN


      END
