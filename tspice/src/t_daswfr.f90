!$Procedure T_DASWFR ( Write a DAS file record to a test DAS )

      SUBROUTINE T_DASWFR ( UNIT,   OUTBFF, IDWORD, IFNAME, &
                            NRESVR, NRESVC, NCOMR,  NCOMC,  ADDFTP )
      use pepper_legacy
      use pepper_inc_zzddhman
      use pepper_inc_zzftprms

! $ Abstract
!
!     Write file record to a test DAS having a specified binary file
!     format.
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


      INTEGER               UNIT
      INTEGER               OUTBFF
      CHARACTER*(*)         IDWORD
      CHARACTER*(*)         IFNAME
      INTEGER               NRESVR
      INTEGER               NRESVC
      INTEGER               NCOMR
      INTEGER               NCOMC
      LOGICAL               ADDFTP

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Unit attached to DAS to receive new file record.
!     OUTBFF     I   Binary file format code for d.p. values in OUTPUT.
!     IDWORD     I   DAS ID Word.
!     IFNAME     I   Internal filename.
!     NRESVR     I   Number of reserved records in file.
!     NRESVC     I   Number of characters in use in reserved rec. area.
!     NCOMR      I   Number of comment records in file.
!     NCOMC      I   Number of characters in use in comment area.
!     ADDFTP     I   Logical that indicates whether to add FTP string.
!
! $ Detailed_Input
!
!     UNIT       is the logical unit attached to the DAS to receive
!                the new file record.
!
!     OUTBFF     is an integer code that indicates the binary file
!                format targeted for OUTPUT.  Acceptable values
!                are the parameters:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!                as defined in the include file 'zzddhman.inc'.
!
!     IDWORD     a string containing the DAS ID word.
!
!
!     IFNAME     is the internal file name to be stored in the first
!                (or file) record of the specified file.
!
!     NRESVR     is the number of reserved records in the DAS file
!                specified by UNIT.
!
!     NRESVC     is the number of characters in use in the reserved
!                record area of the DAS file specified by UNIT.
!
!     NCOMR      is the number of comment records in the DAS file
!                specified by UNIT.
!
!     NCOMC      is the number of characters in use in the comment area
!                of the DAS file specified by UNIT.
!
!     ADDFTP     is a string indicating whether to add the FTP
!                diagnostic string and the binary file format ID
!                string to the file record.
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
!     1)  If the binary file format specified by OUTBFF is
!         unrecognized, the error SPICE(BADFILEFORMAT) is signaled.
!
!     2)  If an error occurs while trying to write the output file,
!         the error SPICE(FILEWRITEFAILED) is signaled.
!
! $ Files
!
!     This routine updates the first record of the file attached
!     to UNIT.
!
! $ Particulars
!
!     Note: This code is not a model of perfection.  It is hacked
!     together from pieces of code written elsewhere to support
!     creation of non-native binaries for testing purposes only.
!
! $ Examples
!
!     See T_DASBGO for usage.
!
! $ Restrictions
!
!     1) The output DAS file is assumed to have been opened
!        by DASONW.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 11-FEB-2015 (NJB) (FST)
!
!
!-&

!
!     SPICELIB Functions
!


!
!     Local Parameters
!
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

!
!     Local Variables
!
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

!
!     Saved Variables
!
      SAVE                  FIRST
      SAVE                  FTPLEN
      SAVE                  FTPSTR
      SAVE                  NATBFF
      SAVE                  STRBFF

!
!     Data Statements
!
      DATA                  FIRST  / .TRUE. /
      DATA                  NATBFF / 0      /

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_DASWFR' )

!
!     Perform some initialization tasks.
!
      IF ( FIRST ) THEN

!
!        Populate STRBFF with the appropriate binary file
!        format labels.
!
         DO I = 1, NUMBFF
            CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
         END DO

!
!        Fetch the native binary file format.
!
         CALL ZZPLATFM ( 'FILE_FORMAT', TMPSTR )
         CALL UCASE    ( TMPSTR,        TMPSTR )

         NATBFF = ISRCHC ( TMPSTR, NUMBFF, STRBFF )

         IF ( NATBFF .EQ. 0 ) THEN

            CALL SETMSG ( 'The binary file format, ''#'', is not ' &
            //            'supported by this version of the toolkit. ' &
            //            'This is a serious problem, contact NAIF.'   )
            CALL ERRCH  ( '#', TMPSTR                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'T_DASWFR'                                   )
            RETURN

         END IF

!
!        Fetch the FTP string.
!
         CALL ZZFTPSTR ( TSTSTR, LFTBKT, RGTBKT, DELIM )

         FTPSTR = LFTBKT( 1:RTRIM(LFTBKT) ) &
         //       TSTSTR( 1:RTRIM(TSTSTR) ) &
         //       RGTBKT( 1:RTRIM(RGTBKT) )


         FTPLEN = RTRIM( FTPSTR )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_DASWFR' )
            RETURN
         END IF
!
!        Do not perform initialization tasks again.
!
         FIRST = .FALSE.

      END IF

!
!     Check to see if OUTBFF is valid.  This should never occur if this
!     routine is called properly.
!
      IF ( ( OUTBFF .LT. 1 ) .OR. ( OUTBFF .GT. NUMBFF ) ) THEN

         CALL SETMSG ( 'The integer code used to indicate the ' &
         //            'binary file format of the input integers, ' &
         //            '#, is out of range.  This error should ' &
         //            'never occur.'                               )
         CALL ERRINT ( '#', OUTBFF                                  )
         CALL SIGERR ( 'SPICE(BADFILEFORMAT)'                       )
         CALL CHKOUT ( 'T_DASWFR'                                   )
         RETURN

      END IF

!
!     Prepare the local string buffers to hold the possible
!     string arguments.
!
      LOCIDW = IDWORD
      LOCIFN = IFNAME
      LOCFMT = STRBFF( OUTBFF )
      PRESPC = ' '

!
!     First, determine if we are to write to the native file format.
!
      IF ( OUTBFF .EQ. NATBFF ) THEN

!
!        Initialize RECORD.
!
         DO I = 1, RECLEN
            RECORD(I:I) = CHAR(0)
         END DO

         IF ( ADDFTP ) THEN

            WRITE ( UNIT   = UNIT, &
                    REC    = 1, &
                    IOSTAT = IOSTAT ) LOCIDW, &
                                      LOCIFN, &
                                      NRESVR, &
                                      NRESVC, &
                                      NCOMR, &
                                      NCOMC, &
                                      LOCFMT, &
                                      PRESPC, &
                                      FTPSTR, &
                                      RECORD (1:297)

         ELSE

            WRITE ( UNIT   = UNIT, &
                    REC    = 1, &
                    IOSTAT = IOSTAT ) LOCIDW, &
                                      LOCIFN, &
                                      NRESVR, &
                                      NRESVC, &
                                      NCOMR, &
                                      NCOMC, &
                                      RECORD (1:940)

         END IF

         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to write to #. IOSTAT was #' )
            CALL ERRFNM ( '#', UNIT                            )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
            CALL CHKOUT ( 'T_DASWFR'                           )
            RETURN
         END IF

!
!     Handle the non-native case.
!
      ELSE

!
!        Initialize RECORD.
!
         DO I = 1, RECLEN
            RECORD(I:I) = CHAR(0)
         END DO

!
!        Populate RECORD.
!
         RECORD(1:8)  = LOCIDW

         RECORD(9:68) = LOCIFN
!
!        Convert and place reserved record and comment
!        parameters.
!
         CALL T_XLTFWI ( NRESVR, 1, OUTBFF, HOLDER )
         RECORD(69:72) = HOLDER

         CALL T_XLTFWI ( NRESVC, 1, OUTBFF, HOLDER )
         RECORD(73:76) = HOLDER

         CALL T_XLTFWI ( NCOMR,  1, OUTBFF, HOLDER )
         RECORD(77:80) = HOLDER

         CALL T_XLTFWI ( NCOMC,  1, OUTBFF, HOLDER )
         RECORD(81:84) = HOLDER

!
!        Add the FTP string if appropriate.
!
         IF ( ADDFTP ) THEN
            RECORD( 85:92        ) = LOCFMT
            RECORD(700:699+FTPLEN) = FTPSTR
         END IF

!
!        Dump the record to the file.
!
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
