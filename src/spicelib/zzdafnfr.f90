!$Procedure ZZDAFNFR ( Private --- DAF write New File Record )

      SUBROUTINE ZZDAFNFR ( LUN, &
                            IDWORD, &
                            ND, &
                            NI, &
                            IFNAME, &
                            FWARD, &
                            BWARD, &
                            FREE, &
                            FORMAT  )

! $ Abstract
!
!    Write the file record to a new DAF file.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DAF
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzftprms.inc'

      INTEGER               LUN
      CHARACTER*(*)         IDWORD
      INTEGER               ND
      INTEGER               NI
      CHARACTER*(*)         IFNAME
      INTEGER               FWARD
      INTEGER               BWARD
      INTEGER               FREE
      CHARACTER*(*)         FORMAT


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LUN        I   Logical unit number of an open DAF file.
!     IDWORD     I   DAF ID word.
!     ND         I   Number of double precision components in a summary.
!     NI         I   Number of integer components in a summary.
!     IFNAME     I   Internal filename.
!     FWARD      I   First descriptor record.
!     BWARD      I   Last descriptor record.
!     FREE       I   First free address.
!     FORMAT     I   File binary format identifier string.
!
! $ Detailed_Input
!
!     LUN        is a logical unit number of a DAF whose first record is
!                to be created with a DAF file record bearing the
!                attributes specified by the other arguments.
!
!     IDWORD     is the 'ID word' contained in the first eight
!                characters of the file record.
!
!     ND,        are the number of double precision and integer
!     NI         components, respectively, in each array summary
!                in the specified file.
!
!     IFNAME     is the internal filename to be stored in the file
!                record for identification purposes.
!
!     FWARD,     are the record numbers of the first and last
!     BWARD      descriptor records in the DAF file, respectively.
!                FWARD is greater than 2 whenever reserved records
!                are present.
!
!     FREE       is the first free address pointer.  This integer
!                stores the first free DAF address for writing the
!                next array to be appended to the file.
!
!     FORMAT     is a character string that indicates what the numeric
!                binary format the DAF is utilizing.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     See include file zzftprms.inc.
!
! $ Exceptions
!
!     1) If any errors occur from the WRITE to the logical unit LUN,
!        the error SPICE(DAFWRITEFAIL) is signaled. Before returning
!        to the caller, the file attached to LUN is closed and deleted.
!
! $ Files
!
!     This routine writes to the first record of the DAF whose
!     logical unit is LUN.
!
! $ Particulars
!
!     This routine assembles the file record and writes it to the
!     first record in a DAF.  Its purpose is to write new file
!     records only.  For updates, use DAFWFR.
!
!     Make certain the caller checks FAILED() after this returns, since
!     on error it closes and deletes the file.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) An individual character must occupy 1 byte of space and
!        conform to the ASCII standard.
!
!     2) The word size for the machine should be at least 32 bits,
!        else the computations to null pad the gaps in the file
!        record may overstep record boundaries.
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
!-    SPICELIB Version 1.0.2, 02-OCT-2021 (NJB)
!
!        Corrected typos in comments. Reordered header sections.
!
!-    SPICELIB Version 1.0.1, 11-DEC-2001 (FST)
!
!        Corrected the omission of IDWORD from the Brief_I/O and
!        Detailed_Input sections of the module header.
!
!-    SPICELIB Version 1.0.0, 02-MAR-1999 (FST)
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
!     Amount of space measured in characters necessary to
!     null pad between the last character of FORMAT and the
!     first character of FTPSTR to keep FTPSTR at character
!     700 in a 1024 byte record.
!
      INTEGER               PREFTP
      PARAMETER           ( PREFTP = 603 )

!
!     Amount of space measured in characters necessary to
!     null pad from the last character of FTPSTR to the
!     end of the file record. Note: This value assumes the
!     length of the file record is 1024 bytes.  The DAF
!     specification only requires the presence of 1000
!     characters, so this may require modification for
!     non-standard platforms.
!
      INTEGER               PSTFTP
      PARAMETER           ( PSTFTP = 297 )

!
!     Lengths of internal file name, ID word, and format word.
!
      INTEGER               FMTLEN
      PARAMETER           ( FMTLEN = 8 )

      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )

      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )

!
!     Local Variables
!
      CHARACTER*(1)         NULLCH
      CHARACTER*(FMTLEN)    LOCFMT
      CHARACTER*(IDWLEN)    LOCIDW
      CHARACTER*(IFNLEN)    LOCIFN
      CHARACTER*(PREFTP)    PRENUL
      CHARACTER*(PSTFTP)    PSTNUL
      CHARACTER*(SIZDLM)    DELIM
      CHARACTER*(SIZEND)    LFTBKT
      CHARACTER*(SIZEND)    RGTBKT
      CHARACTER*(SIZFTP)    FTPSTR
      CHARACTER*(SIZSTR)    TSTSTR

      INTEGER               I
      INTEGER               IOSTAT

      LOGICAL               FIRST

!
!     Saved Variables
!
      SAVE                FIRST
      SAVE                FTPSTR
      SAVE                PRENUL
      SAVE                PSTNUL

!
!     Data Statements
!
      DATA   FIRST        / .TRUE. /

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZDAFNFR')
      END IF

!
!     On the first pass, format the PRENUL and PSTNUL strings,
!     and build FTPSTR from its components.
!
      IF ( FIRST ) THEN

!
!        Store NULL into NULLCH.
!
         NULLCH = CHAR(0)

!
!        Set all of the characters of PRENUL to nulls.
!
         DO I = 1, PREFTP

            PRENUL (I:I) = NULLCH

         END DO

!
!        Set all of the characters of PSTNUL to nulls.
!
         DO I = 1, PSTFTP

            PSTNUL (I:I) = NULLCH

         END DO

!
!        Build FTPSTR from its components that come back from
!        ZZFTPSTR.  This private SPICE routine returns the
!        following components:
!7
!           TSTSTR - The test component of the FTP string
!           LFTBKT - The left bracketing, printable, component of
!                    the FTP string.
!           RGTBKT - The right bracketing, printable, component of
!                    the FTP string.
!           DELIM  - The printable delimiter that separates the
!                    individual test character blocks in TSTSTR.
!
!        which are assembled into the FTP string as it appears in
!        the DAF file record.
!
         CALL ZZFTPSTR ( TSTSTR, LFTBKT, RGTBKT, DELIM )

         FTPSTR = LFTBKT( 1:RTRIM(LFTBKT) )                          // &
                  TSTSTR( 1:RTRIM(TSTSTR) )                          // &
                  RGTBKT( 1:RTRIM(RGTBKT) )

!
!        Stop this block from executing except on the first pass.
!
         FIRST = .FALSE.

      END IF

!
!     Make local copies of each of the string arguments.  This way we
!     maintain the proper sizes for each of the string objects, in
!     the event larger or smaller strings are passed in.
!
      LOCIDW = IDWORD
      LOCIFN = IFNAME
      LOCFMT = FORMAT

!
!     Write the file record components out to the first record of the
!     file.
!
      WRITE ( UNIT = LUN, REC=1, IOSTAT=IOSTAT ) LOCIDW, &
                                                 ND, &
                                                 NI, &
                                                 LOCIFN, &
                                                 FWARD, &
                                                 BWARD, &
                                                 FREE, &
                                                 LOCFMT, &
                                                 PRENUL, &
                                                 FTPSTR, &
                                                 PSTNUL
!
!     Check IOSTAT for errors.
!
      IF ( IOSTAT .NE. 0 ) THEN

!
!        Since we are unable to write to the file record, make
!        certain the output file is destroyed.
!
         CALL SETMSG ( 'Attempt to write file ''#'' failed. '   // &
                       'Value of IOSTAT was #. The file has '   // &
                       'been deleted.'                           )
         CALL ERRFNM ( '#', LUN                                  )
         CALL ERRINT ( '#', IOSTAT                               )

         CLOSE ( UNIT = LUN, STATUS = 'DELETE' )

         CALL SIGERR ( 'SPICE(DAFWRITEFAIL)' )
         CALL CHKOUT ( 'ZZDAFNFR'            )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZDAFNFR' )

      RETURN
      END
