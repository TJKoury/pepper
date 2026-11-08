C$Procedure T_CPTFIL ( Create Partial Test Files )
 
      SUBROUTINE T_CPTFIL ( NAME,   ARCH,  FDREC,  BFFID,  CNI,
     .                      CFDREC, CNSUM, ADDFTP, BRKFTP, NEWIDW )
 
C$ Abstract
C
C     Create partial test files to support the low-level handle
C     manager interface testing.
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
C     TEST ROUTINE
C
C$ Declarations
 
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
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   Name of the file to create.
C     ARCH       I   Integer code indicating the file architecture.
C     FDREC      I   Record number of the first descriptor record.
C     BFFID      I   BFFID to insert into the file record.
C     CNI        I   Character encoding of the value of NI to store.
C     CFDREC     I   Character encoding of the value of FDREC to store.
C     CNSUM      I   Character encoding of the value of NSUM to store.
C     ADDFTP     I   Logical indicating whether to store the FTP string.
C     BRKFTP     I   Logical indicating whether to "break" the FTP test.
C     NEWIDW     I   File ID word to insert into the file record.
C
C$ Detailed_Input
C
C     NAME       is the name of the DAF file to create.
C
C     ARCH       is the integer code indicating the file architecture
C                to use when creating the file.  Acceptable values are:
C
C                   DAF
C                   DAS
C
C                as defined in 'zzddhman.inc'.
C
C     BFFID      is the 8 character string that will be written to the
C                file record to indicate the binary file format of the
C                file created.
C
C     FDREC      is the record number of the first descriptor record
C                in the file.
C
C     CNI        is a 4 byte character string that contains the value
C                of NI byte by byte as it should be written to the file.
C
C     CFDREC     is a 4 byte character string that contains the value
C                of FDREC byte by byte as it should be written to the
C                file.
C
C     CNSUM      is a 4 byte character string that contains the value
C                of NSUM byte by byte as it should be written to the
C                file.
C
C     ADDFTP     is a logical that indicates whether or not to add
C                the FTP string to the file.
C
C     BRKFTP     is a logical that indicates whether or not to "break"
C                the FTP string before placing it in the file.
C
C     NEWIDW     is a character string, if blank indicates to insert
C                the appropriate generic ID word for the requested
C                architecture.  If non-blank, the first 8 characters
C                are placed at the head of the file record.  This is
C                just a hook to create screwed up files for testing
C                purposes.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     This routine creates a file named by NAME that has the properties
C     specified by the input arguments.
C
C$ Exceptions
C
C     1) SPICE(FILEOPENFAILED)
C
C     2) SPICE(WRITEFAILURE)
C
C$ Particulars
C
C     This routine creates small test files for F_DDHPPF.
C
C$ Examples
C
C     See F_DDHIFF for usage.
C
C$ Restrictions
C
C     1) Characters are byte sized.
C
C     2) The file named by NAME does not exist.
C
C     3) NEWIDW if non-blank must contain at least 8 characters.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 06-NOV-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               RECLEN
      PARAMETER           ( RECLEN = 1024 )
 
      INTEGER               DASBIL
      PARAMETER           ( DASBIL = 85 )
 
      INTEGER               DASFTP
      PARAMETER           ( DASFTP = 700 )
 
      INTEGER               DAFFTP
      PARAMETER           ( DAFFTP = 700 )
 
C
C     Local Variables
C
      CHARACTER*(RECLEN)    RECORD
      CHARACTER*(SIZDLM)    DELIM
      CHARACTER*(SIZFTP)    FTPSTR
      CHARACTER*(SIZEND)    LFTBKT
      CHARACTER*(SIZEND)    RGTBKT
      CHARACTER*(SIZSTR)    TSTSTR
 
      INTEGER               LUN
      INTEGER               IOSTAT
 
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_CPTFIL' )
      END IF
 
C
C     Fetch the logical unit we're going to use to create the
C     file.
C
      CALL GETLUN ( LUN )
 
C
C     Open the file we are about to construct.
C
      OPEN ( UNIT   = LUN,
     .       FILE   = NAME,
     .       ACCESS = 'DIRECT',
     .       RECL   = RECL,
     .       STATUS = 'NEW',
     .       IOSTAT = IOSTAT    )
 
C
C     Check IOSTAT for trouble.
C
C
      IF ( IOSTAT .NE. 0 ) THEN
 
         CLOSE ( UNIT = LUN )
 
         CALL SETMSG ( 'Attempt to open file, ''#'', failed. IOSTAT '
     .   //            'was $.'                                       )
         CALL ERRCH  ( '#', NAME                                      )
         CALL ERRINT ( '$', IOSTAT                                    )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                        )
         CALL CHKOUT ( 'T_CPTFIL'                                     )
         RETURN
 
      END IF
 
C
C     Initialize RECORD to be a blank string.
C
      RECORD = ' '
 
C
C     Do the absolute minimum to produce the necessary test file.
C     In this regard we will be placing only the patterns necessary
C     to exercise the logic of ZZDDHIFF and not focusing on the creation
C     of legitmate DAF and DAS files.
C
      IF ( ARCH .EQ. DAF ) THEN
 
C
C        Assemble the file record.
C
         RECORD(1:8)   = 'DAF/TEST'
         RECORD(13:16) = CNI
         RECORD(77:80) = CFDREC
         RECORD(89:96) = BFFID
 
C
C        Before going any further, check to see if we are to replace
C        the ID word with NEWIDW.
C
         IF ( NEWIDW .NE. ' ' ) THEN
            RECORD(1:8) = NEWIDW(1:8)
         END IF
 
         IF ( ADDFTP ) THEN
 
C
C           Get the FTP string.
C
            CALL ZZFTPSTR ( TSTSTR, LFTBKT, RGTBKT, DELIM )
 
            FTPSTR = LFTBKT( 1:RTRIM(LFTBKT) )
     .      //       TSTSTR( 1:RTRIM(TSTSTR) )
     .      //       RGTBKT( 1:RTRIM(RGTBKT) )
 
C
C           Check to see if we are to "break" the FTP string.
C
            IF ( BRKFTP ) THEN
 
C
C              Replace the "<13>" in the first cluster with
C              "<10>" to generate the error.
C
               FTPSTR( (RTRIM(LFTBKT)+2):(RTRIM(LFTBKT)+2) ) = CHAR(10)
 
            END IF
 
C
C           Add the FTP string at position DASFTP.
C
            RECORD( DAFFTP: ) = FTPSTR
 
         END IF
 
C
C        Write the record.
C
         WRITE ( UNIT = LUN, REC = 1, IOSTAT=IOSTAT ) RECORD
 
C
C        Now create the first descriptor record.
C
         RECORD(1:16) = ' '
         RECORD(17:24) = CNSUM
         RECORD(25:42) = ' '
 
         WRITE ( UNIT = LUN, REC = FDREC, IOSTAT=IOSTAT ) RECORD
 
C
C        Check IOSTAT for trouble.
C
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL SETMSG ( 'Attempt to write file ''#'' failed. '
     .      //            'Value of IOSTAT was #. The file has '
     .      //            'been deleted.'                        )
            CALL ERRFNM ( '#', LUN                               )
            CALL ERRINT ( '#', IOSTAT                            )
 
            CLOSE ( UNIT = LUN, STATUS = 'DELETE' )
 
            CALL SIGERR ( 'SPICE(WRITEFAILURE)' )
            CALL CHKOUT ( 'T_CPTFIL'            )
            RETURN
 
         END IF
 
C
C        We've created the test file, close the UNIT since we will be
C        re-opening it from the test code driver.
C
         CLOSE ( UNIT = LUN )
 
      ELSE IF ( ARCH .EQ. DAS ) THEN
 
C
C        We need only to place the BFF ID string in the appropriate
C        place in the file record along with the DAS indentifier.
C
         RECORD(1:8) = 'DAS/TEST'
         RECORD(DASBIL:(DASBIL+7)) = BFFID
 
C
C        Before going any further, check to see if we are to replace
C        the ID word with NEWIDW.
C
         IF ( NEWIDW .NE. ' ' ) THEN
            RECORD(1:8) = NEWIDW(1:8)
         END IF
 
         IF ( ADDFTP ) THEN
 
C
C           Get the FTP string.
C
            CALL ZZFTPSTR ( TSTSTR, LFTBKT, RGTBKT, DELIM )
 
            FTPSTR = LFTBKT( 1:RTRIM(LFTBKT) )
     .      //       TSTSTR( 1:RTRIM(TSTSTR) )
     .      //       RGTBKT( 1:RTRIM(RGTBKT) )
 
C
C           Check to see if we are to "break" the FTP string.
C
            IF ( BRKFTP ) THEN
 
C
C              Replace the "<13>" in the first cluster with
C              "<10>" to generate the error.
C
               FTPSTR( (RTRIM(LFTBKT)+2):(RTRIM(LFTBKT)+2) ) = CHAR(10)
 
            END IF
 
C
C           Add the FTP string at position DASFTP.
C
            RECORD( DASFTP: ) = FTPSTR
 
         END IF
 
C
C        Write the record.
C
         WRITE ( UNIT = LUN, REC = 1, IOSTAT=IOSTAT ) RECORD
 
C
C        Check IOSTAT for trouble.
C
         IF ( IOSTAT .NE. 0 ) THEN
 
            CALL SETMSG ( 'Attempt to write file ''#'' failed. '
     .      //            'Value of IOSTAT was #. The file has '
     .      //            'been deleted.'                        )
            CALL ERRFNM ( '#', LUN                               )
            CALL ERRINT ( '#', IOSTAT                            )
 
            CLOSE ( UNIT = LUN, STATUS = 'DELETE' )
 
            CALL SIGERR ( 'SPICE(WRITEFAILURE)' )
            CALL CHKOUT ( 'T_CPTFIL'            )
            RETURN
 
         END IF
 
C
C        We've created the test file, close the UNIT since we will be
C        re-opening it from the test code driver.
C
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
