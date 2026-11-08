!$Procedure ZZDASNFR ( Private --- DAS write New File Record )

      SUBROUTINE ZZDASNFR ( LUN, &
                            IDWORD, &
                            IFNAME, &
                            NRESVR, &
                            NRESVC, &
                            NCOMR, &
                            NCOMC, &
                            FORMAT  )

! $ Abstract
!
!     Write the file record to a new DAS file.
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
!     DAS
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzftprms.inc'

      INTEGER               LUN
      CHARACTER*(*)         IDWORD
      CHARACTER*(*)         IFNAME
      INTEGER               NRESVR
      INTEGER               NRESVC
      INTEGER               NCOMR
      INTEGER               NCOMC
      CHARACTER*(*)         FORMAT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LUN        I   Logical unit number of an open DAS file.
!     IDWORD     I   DAS File ID word.
!     IFNAME     I   DAS internal file name.
!     NRESVR     I   Number of reserved records in file.
!     NRESVC     I   Number of characters in use in reserved rec. area.
!     NCOMR      I   Number of comment records in file.
!     NCOMC      I   Number of characters in use in comment area.
!     FORMAT     I   File binary format identifier string.
!
! $ Detailed_Input
!
!     LUN        is a logical unit number of a DAS whose first record is
!                to be created with a DAS file record bearing the
!                attributes specified by the other arguments.
!
!     IDWORD     is the 'ID word' contained in the first eight
!                characters of the file record.
!
!     IFNAME     is the internal file name of the DAS file.  The
!                maximum length of the internal file name is 60
!                characters.
!
!     NRESVR     is the number of reserved records in the DAS file
!                specified by HANDLE.
!
!     NRESVC     is the number of characters in use in the reserved
!                record area of the DAS file specified by HANDLE.
!
!     NCOMR      is the number of comment records in the DAS file
!                specified by HANDLE.
!
!     NCOMC      is the number of characters in use in the comment area
!                of the DAS file specified by HANDLE.
!
!     FORMAT     is a character string that indicates what the numeric
!                binary format the DAS is utilizing.
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
!        the error SPICE(DASWRITEFAIL) is signaled.  Before returning
!        to the caller, the file attached to LUN is closed and deleted.
!
! $ Files
!
!     This routine writes to the first record of the DAS whose
!     logical unit is LUN.
!
! $ Particulars
!
!     This routine assembles the file record and writes it to the
!     first record in a DAS.  Its purpose is to write new file
!     records only.  For updates, use DASWFR.
!
!     Make certain the caller checks FAILED() after this returns,
!     since on error it closes and deletes the file.
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
!-    SPICELIB Version 1.0.1, 03-OCT-2021 (NJB)
!
!        Reordered header sections. Corrected typo in comments.
!
!-    SPICELIB Version 1.0.0, 11-DEC-2001 (FST)
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
!     Amount of space measured in characters necessary to null pad
!     between the last character of FORMAT and the first character
!     of FTPSTR to keep FTPSTR at character 700 in a 1024 byte
!     record.
!
      INTEGER               PREFTP
      PARAMETER           ( PREFTP = 607 )

!
!     Amount of space measured in characters necessary to
!     null pad from the last character of FTPSTR to the
!     end of the file record. Note: This value assumes the
!     length of the file record is 1024 bytes.
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
         CALL CHKIN ( 'ZZDASNFR')
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
!
!           TSTSTR - The test component of the FTP string
!           LFTBKT - The left bracketing, printable, component of
!                    the FTP string.
!           RGTBKT - The right bracketing, printable, component of
!                    the FTP string.
!           DELIM  - The printable delimiter that separates the
!                    individual test character blocks in TSTSTR.
!
!        which are assembled into the FTP string as it appears in
!        the DAS file record.
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
                                                 LOCIFN, &
                                                 NRESVR, &
                                                 NRESVC, &
                                                 NCOMR, &
                                                 NCOMC, &
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

         CALL SIGERR ( 'SPICE(DASWRITEFAIL)' )
         CALL CHKOUT ( 'ZZDASNFR'            )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZDASNFR' )

      RETURN
      END
