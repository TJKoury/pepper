C$Procedure T_DASWFR ( Write a DAS file record to a test DAS )
 
      SUBROUTINE T_DASWFR ( UNIT,   OUTBFF, IDWORD, IFNAME, 
     .                      NRESVR, NRESVC, NCOMR,  NCOMC,  ADDFTP )
 
C$ Abstract
C
C     Write file record to a test DAS having a specified binary file
C     format.
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
 
      INTEGER               UNIT
      INTEGER               OUTBFF
      CHARACTER*(*)         IDWORD
      CHARACTER*(*)         IFNAME
      INTEGER               NRESVR
      INTEGER               NRESVC
      INTEGER               NCOMR
      INTEGER               NCOMC
      LOGICAL               ADDFTP
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     UNIT       I   Unit attached to DAS to receive new file record.
C     OUTBFF     I   Binary file format code for d.p. values in OUTPUT.
C     IDWORD     I   DAS ID Word.
C     IFNAME     I   Internal filename.
C     NRESVR     I   Number of reserved records in file.
C     NRESVC     I   Number of characters in use in reserved rec. area.
C     NCOMR      I   Number of comment records in file.
C     NCOMC      I   Number of characters in use in comment area.
C     ADDFTP     I   Logical that indicates whether to add FTP string.
C
C$ Detailed_Input
C
C     UNIT       is the logical unit attached to the DAS to receive
C                the new file record.
C
C     OUTBFF     is an integer code that indicates the binary file
C                format targeted for OUTPUT.  Acceptable values
C                are the parameters:
C
C                   BIGI3E
C                   LTLI3E
C                   VAXGFL
C                   VAXDFL
C
C                as defined in the include file 'zzddhman.inc'.
C
C     IDWORD     a string containing the DAS ID word.
C
C
C     IFNAME     is the internal file name to be stored in the first
C                (or file) record of the specified file.
C
C     NRESVR     is the number of reserved records in the DAS file
C                specified by UNIT.
C
C     NRESVC     is the number of characters in use in the reserved
C                record area of the DAS file specified by UNIT.
C
C     NCOMR      is the number of comment records in the DAS file
C                specified by UNIT.
C
C     NCOMC      is the number of characters in use in the comment area
C                of the DAS file specified by UNIT.
C
C     ADDFTP     is a string indicating whether to add the FTP
C                diagnostic string and the binary file format ID
C                string to the file record.
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
C     1)  If the binary file format specified by OUTBFF is 
C         unrecognized, the error SPICE(BADFILEFORMAT) is signaled.
C
C     2)  If an error occurs while trying to write the output file,
C         the error SPICE(FILEWRITEFAILED) is signaled.
C
C$ Files
C
C     This routine updates the first record of the file attached
C     to UNIT.
C
C$ Particulars
C
C     Note: This code is not a model of perfection.  It is hacked
C     together from pieces of code written elsewhere to support
C     creation of non-native binaries for testing purposes only.
C
C$ Examples
C
C     See T_DASBGO for usage.
C
C$ Restrictions
C
C     1) The output DAS file is assumed to have been opened
C        by DASONW.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 11-FEB-2015 (NJB) (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               ISRCHC
      INTEGER               RTRIM

      LOGICAL               FAILED
      LOGICAL               RETURN
 
C
C     Local Parameters
C
      INTEGER               RECLEN
      PARAMETER           ( RECLEN = 1024 )
 
      INTEGER               PREFTP
      PARAMETER           ( PREFTP = 599 )
 
      INTEGER               FMTLEN
      PARAMETER           ( FMTLEN = 8 )
 
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )
 
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
 
      INTEGER               INTLEN
      PARAMETER           ( INTLEN = 4 )
 
C
C     Local Variables
C
      CHARACTER*(IDWLEN)    LOCIDW
      CHARACTER*(IFNLEN)    LOCIFN
      CHARACTER*(FMTLEN)    LOCFMT
      CHARACTER*(INTLEN)    HOLDER
      CHARACTER*(PREFTP)    PRESPC
      CHARACTER*(RECLEN)    RECORD
      CHARACTER*(SIZDLM)    DELIM
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(STRSIZ)    TMPSTR
      CHARACTER*(SIZFTP)    FTPSTR
      CHARACTER*(SIZEND)    LFTBKT
      CHARACTER*(SIZEND)    RGTBKT
      CHARACTER*(SIZSTR)    TSTSTR
 
      INTEGER               FTPLEN
      INTEGER               I
      INTEGER               NATBFF
      INTEGER               IOSTAT
 
      LOGICAL               FIRST
 
C
C     Saved Variables
C
      SAVE                  FIRST
      SAVE                  FTPLEN
      SAVE                  FTPSTR
      SAVE                  NATBFF
      SAVE                  STRBFF
 
C
C     Data Statements
C
      DATA                  FIRST  / .TRUE. /
      DATA                  NATBFF / 0      /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF
     
      CALL CHKIN ( 'T_DASWFR' )  
 
C
C     Perform some initialization tasks.
C
      IF ( FIRST ) THEN
 
C
C        Populate STRBFF with the appropriate binary file
C        format labels.
C
         DO I = 1, NUMBFF
            CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
         END DO
 
C
C        Fetch the native binary file format.
C
         CALL ZZPLATFM ( 'FILE_FORMAT', TMPSTR )
         CALL UCASE    ( TMPSTR,        TMPSTR )
 
         NATBFF = ISRCHC ( TMPSTR, NUMBFF, STRBFF )
 
         IF ( NATBFF .EQ. 0 ) THEN
 
            CALL SETMSG ( 'The binary file format, ''#'', is not '
     .      //            'supported by this version of the toolkit. '
     .      //            'This is a serious problem, contact NAIF.'   )
            CALL ERRCH  ( '#', TMPSTR                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'T_DASWFR'                                   )
            RETURN
 
         END IF
 
C
C        Fetch the FTP string.
C
         CALL ZZFTPSTR ( TSTSTR, LFTBKT, RGTBKT, DELIM )
 
         FTPSTR = LFTBKT( 1:RTRIM(LFTBKT) )
     .   //       TSTSTR( 1:RTRIM(TSTSTR) )
     .   //       RGTBKT( 1:RTRIM(RGTBKT) )
 

         FTPLEN = RTRIM( FTPSTR )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_DASWFR' )
            RETURN
         END IF
C
C        Do not perform initialization tasks again.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Check to see if OUTBFF is valid.  This should never occur if this
C     routine is called properly.
C
      IF ( ( OUTBFF .LT. 1 ) .OR. ( OUTBFF .GT. NUMBFF ) ) THEN
 
         CALL SETMSG ( 'The integer code used to indicate the '
     .   //            'binary file format of the input integers, '
     .   //            '#, is out of range.  This error should '
     .   //            'never occur.'                               )
         CALL ERRINT ( '#', OUTBFF                                  )
         CALL SIGERR ( 'SPICE(BADFILEFORMAT)'                       )
         CALL CHKOUT ( 'T_DASWFR'                                   )
         RETURN
 
      END IF
 
C
C     Prepare the local string buffers to hold the possible
C     string arguments.
C
      LOCIDW = IDWORD
      LOCIFN = IFNAME
      LOCFMT = STRBFF( OUTBFF )
      PRESPC = ' '
 
C
C     First, determine if we are to write to the native file format.
C
      IF ( OUTBFF .EQ. NATBFF ) THEN
 
C
C        Initialize RECORD.
C
         DO I = 1, RECLEN
            RECORD(I:I) = CHAR(0)
         END DO
 
         IF ( ADDFTP ) THEN
 
            WRITE ( UNIT   = UNIT,
     .              REC    = 1,
     .              IOSTAT = IOSTAT ) LOCIDW,
     .                                LOCIFN,
     .                                NRESVR,
     .                                NRESVC,
     .                                NCOMR,
     .                                NCOMC,
     .                                LOCFMT,
     .                                PRESPC,
     .                                FTPSTR,
     .                                RECORD (1:297)
 
         ELSE
 
            WRITE ( UNIT   = UNIT,
     .              REC    = 1,
     .              IOSTAT = IOSTAT ) LOCIDW,
     .                                LOCIFN,
     .                                NRESVR,
     .                                NRESVC,
     .                                NCOMR,
     .                                NCOMC,
     .                                RECORD (1:940)
 
         END IF
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to write to #. IOSTAT was #' )
            CALL ERRFNM ( '#', UNIT                            )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
            CALL CHKOUT ( 'T_DASWFR'                           )
            RETURN
         END IF
 
C
C     Handle the non-native case.
C
      ELSE
 
C
C        Initialize RECORD.
C
         DO I = 1, RECLEN
            RECORD(I:I) = CHAR(0)
         END DO
 
C
C        Populate RECORD.
C
         RECORD(1:8)  = LOCIDW

         RECORD(9:68) = LOCIFN 
C
C        Convert and place reserved record and comment
C        parameters.
C
         CALL T_XLTFWI ( NRESVR, 1, OUTBFF, HOLDER )
         RECORD(69:72) = HOLDER
 
         CALL T_XLTFWI ( NRESVC, 1, OUTBFF, HOLDER )
         RECORD(73:76) = HOLDER

         CALL T_XLTFWI ( NCOMR,  1, OUTBFF, HOLDER )
         RECORD(77:80) = HOLDER

         CALL T_XLTFWI ( NCOMC,  1, OUTBFF, HOLDER )
         RECORD(81:84) = HOLDER
  
C
C        Add the FTP string if appropriate.
C
         IF ( ADDFTP ) THEN
            RECORD( 85:92        ) = LOCFMT
            RECORD(700:699+FTPLEN) = FTPSTR
         END IF
 
C
C        Dump the record to the file.
C
         WRITE ( UNIT = UNIT, REC = 1, IOSTAT = IOSTAT ) RECORD
 
         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to write to #. IOSTAT was #' )
            CALL ERRFNM ( '#', UNIT                            )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
            CALL CHKOUT ( 'T_DASWFR'                           )
            RETURN
         END IF
 
      END IF
 
      CALL CHKOUT ( 'T_DASWFR' )
      RETURN
 
      END
