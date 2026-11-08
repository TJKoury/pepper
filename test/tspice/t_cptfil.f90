!$Procedure T_CPTFIL ( Create Partial Test Files )

      SUBROUTINE T_CPTFIL ( NAME,   ARCH,  FDREC,  BFFID,  CNI, &
                            CFDREC, CNSUM, ADDFTP, BRKFTP, NEWIDW )

! $ Abstract
!
!     Create partial test files to support the low-level handle
!     manager interface testing.
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
!     TEST ROUTINE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'
      INCLUDE              'zzftprms.inc'

      CHARACTER*(*)         NAME
      INTEGER               ARCH
      INTEGER               FDREC
      CHARACTER*(*)         BFFID
      CHARACTER*(*)         CNI
      CHARACTER*(*)         CFDREC
      CHARACTER*(*)         CNSUM
      LOGICAL               ADDFTP
      LOGICAL               BRKFTP
      CHARACTER*(*)         NEWIDW

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the file to create.
!     ARCH       I   Integer code indicating the file architecture.
!     FDREC      I   Record number of the first descriptor record.
!     BFFID      I   BFFID to insert into the file record.
!     CNI        I   Character encoding of the value of NI to store.
!     CFDREC     I   Character encoding of the value of FDREC to store.
!     CNSUM      I   Character encoding of the value of NSUM to store.
!     ADDFTP     I   Logical indicating whether to store the FTP string.
!     BRKFTP     I   Logical indicating whether to "break" the FTP test.
!     NEWIDW     I   File ID word to insert into the file record.
!
! $ Detailed_Input
!
!     NAME       is the name of the DAF file to create.
!
!     ARCH       is the integer code indicating the file architecture
!                to use when creating the file.  Acceptable values are:
!
!                   DAF
!                   DAS
!
!                as defined in 'zzddhman.inc'.
!
!     BFFID      is the 8 character string that will be written to the
!                file record to indicate the binary file format of the
!                file created.
!
!     FDREC      is the record number of the first descriptor record
!                in the file.
!
!     CNI        is a 4 byte character string that contains the value
!                of NI byte by byte as it should be written to the file.
!
!     CFDREC     is a 4 byte character string that contains the value
!                of FDREC byte by byte as it should be written to the
!                file.
!
!     CNSUM      is a 4 byte character string that contains the value
!                of NSUM byte by byte as it should be written to the
!                file.
!
!     ADDFTP     is a logical that indicates whether or not to add
!                the FTP string to the file.
!
!     BRKFTP     is a logical that indicates whether or not to "break"
!                the FTP string before placing it in the file.
!
!     NEWIDW     is a character string, if blank indicates to insert
!                the appropriate generic ID word for the requested
!                architecture.  If non-blank, the first 8 characters
!                are placed at the head of the file record.  This is
!                just a hook to create screwed up files for testing
!                purposes.
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
!     This routine creates a file named by NAME that has the properties
!     specified by the input arguments.
!
! $ Exceptions
!
!     1) SPICE(FILEOPENFAILED)
!
!     2) SPICE(WRITEFAILURE)
!
! $ Particulars
!
!     This routine creates small test files for F_DDHPPF.
!
! $ Examples
!
!     See F_DDHIFF for usage.
!
! $ Restrictions
!
!     1) Characters are byte sized.
!
!     2) The file named by NAME does not exist.
!
!     3) NEWIDW if non-blank must contain at least 8 characters.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               RTRIM
      LOGICAL               RETURN

!
!     Local Parameters
!
      INTEGER               RECLEN
      PARAMETER           ( RECLEN = 1024 )

      INTEGER               DASBIL
      PARAMETER           ( DASBIL = 85 )

      INTEGER               DASFTP
      PARAMETER           ( DASFTP = 700 )

      INTEGER               DAFFTP
      PARAMETER           ( DAFFTP = 700 )

!
!     Local Variables
!
      CHARACTER*(RECLEN)    RECORD
      CHARACTER*(SIZDLM)    DELIM
      CHARACTER*(SIZFTP)    FTPSTR
      CHARACTER*(SIZEND)    LFTBKT
      CHARACTER*(SIZEND)    RGTBKT
      CHARACTER*(SIZSTR)    TSTSTR

      INTEGER               LUN
      INTEGER               IOSTAT


!
!     Standard SPICE error handling
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_CPTFIL' )
      END IF

!
!     Fetch the logical unit we're going to use to create the
!     file.
!
      CALL GETLUN ( LUN )

!
!     Open the file we are about to construct.
!
      OPEN ( UNIT   = LUN, &
             FILE   = NAME, &
             ACCESS = 'DIRECT', &
             RECL   = RECL, &
             STATUS = 'NEW', &
             IOSTAT = IOSTAT    )

!
!     Check IOSTAT for trouble.
!
!
      IF ( IOSTAT .NE. 0 ) THEN

         CLOSE ( UNIT = LUN )

         CALL SETMSG ( 'Attempt to open file, ''#'', failed. IOSTAT ' &
         //            'was $.'                                       )
         CALL ERRCH  ( '#', NAME                                      )
         CALL ERRINT ( '$', IOSTAT                                    )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                        )
         CALL CHKOUT ( 'T_CPTFIL'                                     )
         RETURN

      END IF

!
!     Initialize RECORD to be a blank string.
!
      RECORD = ' '

!
!     Do the absolute minimum to produce the necessary test file.
!     In this regard we will be placing only the patterns necessary
!     to exercise the logic of ZZDDHIFF and not focusing on the creation
!     of legitmate DAF and DAS files.
!
      IF ( ARCH .EQ. DAF ) THEN

!
!        Assemble the file record.
!
         RECORD(1:8)   = 'DAF/TEST'
         RECORD(13:16) = CNI
         RECORD(77:80) = CFDREC
         RECORD(89:96) = BFFID

!
!        Before going any further, check to see if we are to replace
!        the ID word with NEWIDW.
!
         IF ( NEWIDW .NE. ' ' ) THEN
            RECORD(1:8) = NEWIDW(1:8)
         END IF

         IF ( ADDFTP ) THEN

!
!           Get the FTP string.
!
            CALL ZZFTPSTR ( TSTSTR, LFTBKT, RGTBKT, DELIM )

            FTPSTR = LFTBKT( 1:RTRIM(LFTBKT) ) &
            //       TSTSTR( 1:RTRIM(TSTSTR) ) &
            //       RGTBKT( 1:RTRIM(RGTBKT) )

!
!           Check to see if we are to "break" the FTP string.
!
            IF ( BRKFTP ) THEN

!
!              Replace the "<13>" in the first cluster with
!              "<10>" to generate the error.
!
               FTPSTR( (RTRIM(LFTBKT)+2):(RTRIM(LFTBKT)+2) ) = CHAR(10)

            END IF

!
!           Add the FTP string at position DASFTP.
!
            RECORD( DAFFTP: ) = FTPSTR

         END IF

!
!        Write the record.
!
         WRITE ( UNIT = LUN, REC = 1, IOSTAT=IOSTAT ) RECORD

!
!        Now create the first descriptor record.
!
         RECORD(1:16) = ' '
         RECORD(17:24) = CNSUM
         RECORD(25:42) = ' '

         WRITE ( UNIT = LUN, REC = FDREC, IOSTAT=IOSTAT ) RECORD

!
!        Check IOSTAT for trouble.
!
         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Attempt to write file ''#'' failed. ' &
            //            'Value of IOSTAT was #. The file has ' &
            //            'been deleted.'                        )
            CALL ERRFNM ( '#', LUN                               )
            CALL ERRINT ( '#', IOSTAT                            )

            CLOSE ( UNIT = LUN, STATUS = 'DELETE' )

            CALL SIGERR ( 'SPICE(WRITEFAILURE)' )
            CALL CHKOUT ( 'T_CPTFIL'            )
            RETURN

         END IF

!
!        We've created the test file, close the UNIT since we will be
!        re-opening it from the test code driver.
!
         CLOSE ( UNIT = LUN )

      ELSE IF ( ARCH .EQ. DAS ) THEN

!
!        We need only to place the BFF ID string in the appropriate
!        place in the file record along with the DAS indentifier.
!
         RECORD(1:8) = 'DAS/TEST'
         RECORD(DASBIL:(DASBIL+7)) = BFFID

!
!        Before going any further, check to see if we are to replace
!        the ID word with NEWIDW.
!
         IF ( NEWIDW .NE. ' ' ) THEN
            RECORD(1:8) = NEWIDW(1:8)
         END IF

         IF ( ADDFTP ) THEN

!
!           Get the FTP string.
!
            CALL ZZFTPSTR ( TSTSTR, LFTBKT, RGTBKT, DELIM )

            FTPSTR = LFTBKT( 1:RTRIM(LFTBKT) ) &
            //       TSTSTR( 1:RTRIM(TSTSTR) ) &
            //       RGTBKT( 1:RTRIM(RGTBKT) )

!
!           Check to see if we are to "break" the FTP string.
!
            IF ( BRKFTP ) THEN

!
!              Replace the "<13>" in the first cluster with
!              "<10>" to generate the error.
!
               FTPSTR( (RTRIM(LFTBKT)+2):(RTRIM(LFTBKT)+2) ) = CHAR(10)

            END IF

!
!           Add the FTP string at position DASFTP.
!
            RECORD( DASFTP: ) = FTPSTR

         END IF

!
!        Write the record.
!
         WRITE ( UNIT = LUN, REC = 1, IOSTAT=IOSTAT ) RECORD

!
!        Check IOSTAT for trouble.
!
         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Attempt to write file ''#'' failed. ' &
            //            'Value of IOSTAT was #. The file has ' &
            //            'been deleted.'                        )
            CALL ERRFNM ( '#', LUN                               )
            CALL ERRINT ( '#', IOSTAT                            )

            CLOSE ( UNIT = LUN, STATUS = 'DELETE' )

            CALL SIGERR ( 'SPICE(WRITEFAILURE)' )
            CALL CHKOUT ( 'T_CPTFIL'            )
            RETURN

         END IF

!
!        We've created the test file, close the UNIT since we will be
!        re-opening it from the test code driver.
!
         CLOSE ( UNIT = LUN )

      ELSE

         CALL SETMSG ( 'Unknown architecture code.' )
         CALL SIGERR ( 'SPICE(BUG)'                 )
         CALL CHKOUT ( 'T_CPTFIL'                   )
         RETURN

      END IF

      CALL CHKOUT ( 'T_CPTFIL' )
      RETURN

      END
