!$Procedure T_DAFWFR ( Write a DAF file record to a test DAF )

      SUBROUTINE T_DAFWFR ( UNIT,   OUTBFF, IDWORD, ND,     NI, &
                            IFNAME, FWARD,  BWARD,  FREE,   ADDFTP )
      use pepper_legacy
      use pepper_inc_zzddhman
      use pepper_inc_zzftprms

! $ Abstract
!
!     Write file record to a test DAF.
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
      INTEGER               ND
      INTEGER               NI
      CHARACTER*(*)         IFNAME
      INTEGER               FWARD
      INTEGER               BWARD
      INTEGER               FREE
      LOGICAL               ADDFTP

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Unit attached to DAF to receive new file record.
!     OUTBFF     I   Binary file format code for d.p. values in OUTPUT.
!     IDWORD     I   DAF ID Word.
!     ND         I   Number of double precision components in summaries.
!     NI         I   Number of integer components in summaries.
!     IFNAME     I   Internal filename.
!     FWARD      I   Forward list pointer.
!     BWARD      I   Backward list pointer.
!     FREE       I   Free address pointer.
!     ADDFTP     I   Logical that indicates whether to add FTP string.
!
! $ Detailed_Input
!
!     UNIT       is the logical unit attached to the DAF to receive
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
!     IDWORD     a string containing the DAF ID word.
!
!     ND,
!     NI         are the numbers of double precision and integer
!                components, respectively, in each array summary
!                in the specified file.
!
!     IFNAME     is the internal file name to be stored in the first
!                (or file) record of the specified file.
!
!     FWARD      is the forward list pointer. This points to the
!                first summary record in the file.
!
!     BWARD      is the backward list pointer. This points to the
!                final summary record in the file.
!
!     FREE       is the free address pointer. This contains the
!                first free address in the file.
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
! $ Files
!
!     This routine updates the first record of the file attached
!     to UNIT.
!
! $ Exceptions
!
!
! $ Particulars
!
!     Note: This code is not a model of perfection.  It is hacked
!     together from pieces of code written elsewhere to support
!     creation of non-native binaries for testing purposes only.
!
! $ Examples
!
!     See F_ZZDGDR for usage.
!
! $ Restrictions
!
!     1) The file was created with T_DAFOPN.
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
!-    SPICELIB Version 1.0.0, 28-OCT-2001 (FST)
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
      PARAMETER           ( PREFTP = 603 )

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

      INTEGER               I
      INTEGER               NATBFF
      INTEGER               IOSTAT

      LOGICAL               FIRST

!
!     Saved Variables
!
      SAVE                  FIRST
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
      ELSE
         CALL CHKIN ( 'T_DAFWFR' )
      END IF

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
            CALL CHKOUT ( 'T_DAFWFR'                                   )
            RETURN

         END IF

!
!        Fetch the FTP string.
!
         CALL ZZFTPSTR ( TSTSTR, LFTBKT, RGTBKT, DELIM )

         FTPSTR = LFTBKT( 1:RTRIM(LFTBKT) ) &
         //       TSTSTR( 1:RTRIM(TSTSTR) ) &
         //       RGTBKT( 1:RTRIM(RGTBKT) )

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
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'T_DAFWFR'                                   )
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

         RECORD = ' '

         IF ( ADDFTP ) THEN

            WRITE ( UNIT   = UNIT, &
                    REC    = 1, &
                    IOSTAT = IOSTAT ) LOCIDW, &
                                      ND, &
                                      NI, &
                                      LOCIFN, &
                                      FWARD, &
                                      BWARD, &
                                      FREE, &
                                      LOCFMT, &
                                      PRESPC, &
                                      FTPSTR, &
                                      RECORD (1:297)

         ELSE

            WRITE ( UNIT   = UNIT, &
                    REC    = 1, &
                    IOSTAT = IOSTAT ) LOCIDW, &
                                      ND, &
                                      NI, &
                                      LOCIFN, &
                                      FWARD, &
                                      BWARD, &
                                      FREE, &
                                      RECORD (1:936)

         END IF

         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Unable to write to #. IOSTAT was #' )
            CALL ERRFNM ( '#', UNIT                            )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
            CALL CHKOUT ( 'T_DAFWFR'                           )
            RETURN
         END IF

!
!     Handle the non-native case.
!
      ELSE

!
!        Clear RECORD.
!
         RECORD = ' '

!
!        Populate RECORD.
!
         RECORD(1:8) = LOCIDW

!
!        Convert and place ND, NI.
!
         CALL T_XLTFWI ( ND, 1, OUTBFF, HOLDER )
         RECORD(9:12)  = HOLDER

         CALL T_XLTFWI ( NI, 1, OUTBFF, HOLDER )
         RECORD(13:16) = HOLDER

         RECORD(17:76) = LOCIFN

!
!        Convert and place FWARD, BWARD, and FREE.
!
         CALL T_XLTFWI ( FWARD, 1, OUTBFF, HOLDER )
         RECORD(77:80) = HOLDER

         CALL T_XLTFWI ( BWARD, 1, OUTBFF, HOLDER )
         RECORD(81:84) = HOLDER

         CALL T_XLTFWI ( FREE, 1, OUTBFF, HOLDER )
         RECORD(85:88) = HOLDER

!
!        Add the FTP string if appropriate.
!
         IF ( ADDFTP ) THEN
            RECORD(89:96) = LOCFMT
            RECORD(700:)  = FTPSTR
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
            CALL CHKOUT ( 'T_DAFWFR'                           )
            RETURN
         END IF

      END IF

      CALL CHKOUT ( 'T_DAFWFR' )
      RETURN

      END
