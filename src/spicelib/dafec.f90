!$Procedure DAFEC ( DAF extract comments )

      SUBROUTINE DAFEC ( HANDLE, BUFSIZ, N, BUFFER, DONE )

! $ Abstract
!
!     Extract comments from the comment area of a binary DAF.
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
!     DAF
!
! $ Keywords
!
!     FILES
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               BUFSIZ
      INTEGER               N
      CHARACTER*(*)         BUFFER(*)
      LOGICAL               DONE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of binary DAF opened with read access.
!     BUFSIZ     I   Maximum size, in lines, of BUFFER.
!     N          O   Number of extracted comment lines.
!     BUFFER     O   Buffer where extracted comment lines are placed.
!     DONE       O   Indicates whether all comments have been extracted.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of a binary DAF which has been opened
!              with read access.
!
!     BUFSIZ   is the maximum number of comments that may be placed into
!              BUFFER. This would typically be the declared array size
!              for the Fortran character string array passed into this
!              routine.
!
! $ Detailed_Output
!
!     N        is the number of comment lines extracted from the comment
!              area of the binary DAF attached to HANDLE. This number
!              will be <= BUFSIZ on output. If N = BUFSIZ and DONE <>
!              .TRUE., then there are more comments left to to extract.
!              If N = 0, then DONE = .TRUE., i.e., there were no
!              comments in the comment area or we have extracted all
!              of the comments. If there are comments in the comment
!              area, or comments remaining after the extraction process
!              has begun, N > 0, always.
!
!     BUFFER   is an array of at most BUFSIZ comments which have been
!              extracted from the comment area of the binary DAF
!              attached to HANDLE.
!
!     DONE     is a logical flag indicating whether or not all of the
!              comment lines from the comment area of the DAF have
!              been read. This variable has the value .TRUE. after the
!              last comment line has been read. It will have the value
!              .FALSE. otherwise.
!
!              If there are no comments in the comment area, this
!              variable will have the value .TRUE., and N = 0.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the size of the output line buffer is is not positive,
!         the error SPICE(INVALIDARGUMENT) is signaled.
!
!     2)  If a comment line in a DAF is longer than the length
!         of a character string array element of BUFFER, the error
!         SPICE(COMMENTTOOLONG) is signaled.
!
!     3)  If the end of the comments cannot be found, i.e., the end of
!         comments marker is missing on the last comment record, the
!         error SPICE(BADCOMMENTAREA) is signaled.
!
!     4)  If the number of comment characters scanned exceeds the
!         number of comment characters computed, the error
!         SPICE(BADCOMMENTAREA) is signaled.
!
!     5)  If the binary DAF attached to HANDLE is not open for reading,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     See argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     A binary DAF contains an area which is reserved for storing
!     annotations or descriptive textual information describing the data
!     contained in a file. This area is referred to as the ``comment
!     area'' of the file. The comment area of a DAF is a line
!     oriented medium for storing textual information. The comment
!     area preserves any leading or embedded white space in the line(s)
!     of text which are stored, so that the appearance of the of
!     information will be unchanged when it is retrieved (extracted) at
!     some other time. Trailing blanks, however, are NOT preserved,
!     due to the way that character strings are represented in
!     standard Fortran 77.
!
!     This routine will read the comments from the comment area of
!     a binary DAF, placing them into a line buffer. If the line
!     buffer is not large enough to hold the entire comment area,
!     the portion read will be returned to the caller, and the DONE
!     flag will be set to .FALSE. This allows the comment area to be
!     read in ``chunks,'' a buffer at a time. After all of the comment
!     lines have been read, the DONE flag will be set to .TRUE.
!
!     This routine can be used to ``simultaneously'' extract comments
!     from the comment areas of multiple binary DAFs. See Example
!     2 in the $Examples section.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following example will extract a maximum of 30 lines
!        from the comment area of a binary DAF, displaying the
!        comments on the terminal screen.
!
!        Although it would be possible to read the 30 lines in one
!        go, for this example, use only a buffer size of 10,
!        demonstrating the use of the DONE logical flag.
!
!
!        Use the SPK kernel below as input DAF file for the program.
!
!           earthstns_itrf93_201023.bsp
!
!
!        Example code begins here.
!
!
!              PROGRAM DAFEC_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               RTRIM
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         KERNEL
!              PARAMETER           ( KERNEL =
!             .                         'earthstns_itrf93_201023.bsp' )
!
!              INTEGER               BUFSIZ
!              PARAMETER           ( BUFSIZ = 10 )
!
!              INTEGER               LINLEN
!              PARAMETER           ( LINLEN = 1000 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(LINLEN)    BUFFER ( BUFSIZ )
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               J
!              INTEGER               N
!
!              LOGICAL               DONE
!
!        C
!        C     Open a DAF for read. Return a HANDLE referring to the
!        C     file.
!        C
!              CALL DAFOPR ( KERNEL, HANDLE )
!
!        C
!        C     Read a maximum of 30 lines of comments.
!        C
!              WRITE(*,'(A)') 'Comment area of input DAF file '
!             .            // '(max. 30 lines): '
!              WRITE(*,'(A)') '---------------------------------------'
!             .            // '-----------------------'
!              DO I = 0, 2
!
!                 CALL DAFEC  ( HANDLE, BUFSIZ, N, BUFFER, DONE )
!
!        C
!        C        Write the N lines to the terminal screen.
!        C
!                 DO J = 1, N
!
!                    WRITE (*,*) BUFFER(J)(:RTRIM(BUFFER(J)))
!
!                 END DO
!
!              END DO
!
!              WRITE(*,'(A)') '---------------------------------------'
!             .            // '-----------------------'
!
!        C
!        C     Have all the comments been read?
!        C
!              IF ( .NOT. DONE ) THEN
!
!                 WRITE(*,*) ' '
!                 WRITE(*,*) 'Warning: Not all comments have been read!'
!
!              END IF
!
!        C
!        C     Safely close the DAF.
!        C
!              CALL DAFCLS ( HANDLE )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Comment area of input DAF file (max. 30 lines):
!        --------------------------------------------------------------
!
!            SPK for DSN Station Locations
!            ========================================================***
!
!            Original file name:                   earthstns_itrf93_2***
!            Creation date:                        2020 October 28 12:30
!            Created by:                           Nat Bachman  (NAIF***
!
!
!            Introduction
!            ========================================================***
!
!            This file provides geocentric states---locations and vel***
!            set of DSN stations cited in the list below under "Posit***
!            position vectors point from the earth's barycenter to th***
!            velocities are estimates of the derivatives with respect***
!            vectors; in this file, velocities are constant. Station ***
!            magnitudes on the order of a few cm/year.
!
!            The states in this file are given relative to the terres***
!            frame ITRF93.
!
!            This SPK file has a companion file
!
!               earthstns_fx_201023.bsp
!
!            which differs from this one only in that it uses the ref***
!            frame alias 'EARTH_FIXED'. See the comment area of that ***
!            and the Frames Required Reading for details.
!
!        --------------------------------------------------------------
!
!         Warning: Not all comments have been read!
!
!
!        Warning: incomplete output. 12 lines extended past the right
!        margin of the header and have been truncated. These lines are
!        marked by "***" at the end of each line.
!
!
!     2) The following example demonstrates the use of this routine to
!        simultaneously read the comment areas of multiple DAFs. For
!        each file, the comments will be displayed on the screen as
!        they are extracted.
!
!        Use the SPK kernel below as the first DAF file for the
!        program.
!
!           earthstns_itrf93_201023.bsp
!
!
!        Use the CK kernel below as the second DAF file for the
!        program.
!
!           vo2_swu_ck2.bc
!
!
!        Example code begins here.
!
!
!              PROGRAM DAFEC_EX2
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              INTEGER               RTRIM
!
!              LOGICAL               ALLTRU
!
!        C
!        C     Local parameters
!        C
!              INTEGER               BUFSIZ
!              PARAMETER           ( BUFSIZ = 10   )
!
!              INTEGER               FNAMLN
!              PARAMETER           ( FNAMLN = 255  )
!
!              INTEGER               LINLEN
!              PARAMETER           ( LINLEN = 1000 )
!
!              INTEGER               NUMFIL
!              PARAMETER           ( NUMFIL = 2    )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(FNAMLN)    DAFNAM ( NUMFIL )
!              CHARACTER*(LINLEN)    BUFFER ( BUFSIZ )
!
!              INTEGER               HANDLE ( NUMFIL )
!              INTEGER               I
!              INTEGER               J
!              INTEGER               N
!
!              LOGICAL               DONE   ( NUMFIL )
!
!        C
!        C     Set the DAF file names.
!        C
!              DATA                  DAFNAM /
!             .                          'earthstns_itrf93_201023.bsp',
!             .                          'vo2_swu_ck2.bc'          /
!
!        C
!        C     Set the initial values for DONE and HANDLE.
!        C
!              DO I = 1, NUMFIL
!
!                 DONE(I)   = .FALSE.
!                 HANDLE(I) = 0
!
!              END DO
!
!        C
!        C     Open the DAFs.
!        C
!              DO I = 1, NUMFIL
!
!                 CALL DAFOPR ( DAFNAM(I), HANDLE(I) )
!
!              END DO
!
!        C
!        C     While there are still some comments left to read in at
!        C     least one of the files, read them and display them.
!        C
!              DO WHILE ( .NOT. ALLTRU( DONE, NUMFIL ) )
!
!                 DO I = 1, NUMFIL
!
!                    IF ( .NOT. DONE(I) ) THEN
!
!                       WRITE (*,*)
!                       WRITE (*,*) 'File: ',
!             .                     DAFNAM(I)(:RTRIM(DAFNAM(I)))
!                       WRITE (*,*) '---------------------------------'
!             .                 //  '-----------------------------'
!                       N = 0
!
!                       CALL DAFEC ( HANDLE(I), BUFSIZ, N,
!             .                      BUFFER,    DONE(I)   )
!
!                       DO J = 1, N
!
!                          WRITE (*,*) BUFFER(J)(:RTRIM(BUFFER(J)))
!
!                       END DO
!
!                    END IF
!
!                 END DO
!
!              END DO
!
!        C
!        C     Safely close the DAF files.
!        C
!              DO I = 1, NUMFIL
!
!                 CALL DAFCLS ( HANDLE(I) )
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         File: earthstns_itrf93_201023.bsp
!         --------------------------------------------------------------
!
!            SPK for DSN Station Locations
!            ========================================================***
!
!            Original file name:                   earthstns_itrf93_2***
!            Creation date:                        2020 October 28 12:30
!            Created by:                           Nat Bachman  (NAIF***
!
!
!            Introduction
!
!         File: vo2_swu_ck2.bc
!         --------------------------------------------------------------
!         \beginlabel
!         PDS_VERSION_ID               = PDS3
!         RECORD_TYPE                  = FIXED_LENGTH
!         RECORD_BYTES                 = 1024
!         ^SPICE_KERNEL                = "vo2_swu_ck2.bc"
!         MISSION_NAME                 = VIKING
!         SPACECRAFT_NAME              = "VIKING ORBITER 2"
!         DATA_SET_ID                  = "VO1/VO2-M-SPICE-6-V1.0"
!         KERNEL_TYPE_ID               = CK
!         PRODUCT_ID                   = "vo2_swu_ck2.bc"
!
!         File: earthstns_itrf93_201023.bsp
!         --------------------------------------------------------------
!            ========================================================***
!
!            This file provides geocentric states---locations and vel***
!            set of DSN stations cited in the list below under "Posit***
!            position vectors point from the earth's barycenter to th***
!            velocities are estimates of the derivatives with respect***
!            vectors; in this file, velocities are constant. Station ***
!            magnitudes on the order of a few cm/year.
!
!            The states in this file are given relative to the terres***
!
!         File: vo2_swu_ck2.bc
!         --------------------------------------------------------------
!         PRODUCT_CREATION_TIME        = 2008-12-03T14:03:35
!         PRODUCER_ID                  = "NAIF/JPL"
!         MISSION_PHASE_NAME           = {
!                                        PRIMARY_MISSION,
!                                        EXTENDED_MISSION,
!                                        CONTINUATION_MISSION
!                                        }
!         PRODUCT_VERSION_TYPE         = ACTUAL
!         PLATFORM_OR_MOUNTING_NAME    = "SPACECRAFT BUS"
!         START_TIME                   = 1977-01-09T18:33:12.707
!
!         File: earthstns_itrf93_201023.bsp
!         --------------------------------------------------------------
!            frame ITRF93.
!
!            This SPK file has a companion file
!
!               earthstns_fx_201023.bsp
!
!            which differs from this one only in that it uses the ref***
!            frame alias 'EARTH_FIXED'. See the comment area of that ***
!            and the Frames Required Reading for details.
!
!
!         File: vo2_swu_ck2.bc
!         --------------------------------------------------------------
!         STOP_TIME                    = 1977-11-27T05:55:16.772
!         SPACECRAFT_CLOCK_START_COUNT = "1/0032380393.707"
!         SPACECRAFT_CLOCK_STOP_COUNT  = "1/0060155717.772"
!         TARGET_NAME                  = MARS
!         INSTRUMENT_NAME              = "SCAN PLATFORM"
!         NAIF_INSTRUMENT_ID           = -30000
!         SOURCE_PRODUCT_ID            = "N/A"
!         NOTE                         = "See comments in the file fo***
!         OBJECT                       = SPICE_KERNEL
!           INTERCHANGE_FORMAT         = BINARY
!
!         File: earthstns_itrf93_201023.bsp
!         --------------------------------------------------------------
!
!            Revision description
!            --------------------
!
!            This kernel contains data from a single, current source:***
!
!            This kernel supersedes the kernels
!
!               earthstns_itrf93_050714.bsp
!               dss_53_prelim_itrf93_201018.bsp (data in this kernel ***
!
!         File: vo2_swu_ck2.bc
!         --------------------------------------------------------------
!           KERNEL_TYPE                = POINTING
!           DESCRIPTION                = "VO2 instrument platform ori***
!         reconstructed during cartographic image registration by S. ***
!         Type 2 CK file supersedes the less usable Type 1 CK vo2_swu***
!         END_OBJECT                   = SPICE_KERNEL
!         \endlabel
!
!        [...]
!
!
!        Warning: incomplete output. Only 100 out of 1049 lines have
!        been provided. 18 lines extended past the right margin of the
!        header and have been truncated. These lines are marked by "***"
!        at the end of each line.
!
!
! $ Restrictions
!
!     1)  The comment area may consist only of printing ASCII
!         characters, decimal values 32 - 126.
!
!     2)  There is NO maximum length imposed on the significant portion
!         of a text line that may be placed into the comment area of a
!         DAF. The maximum length of a line stored in the comment area
!         should be kept reasonable, so that they may be easily
!         extracted. A good value for this would be 1000 characters, as
!         this can easily accommodate ``screen width'' lines as well as
!         long lines which may contain some other form of information.
!
!     3)  This routine is only used to read records on environments
!         whose characters are a single byte in size. Updates
!         to this routine and routines in its call tree may be
!         required to properly handle other cases.
!
!     4)  This routine is intended to be used on DAF files whose comment
!         area does not change while this routine is called to extract
!         comments, between the start and end of the extraction process.
!         If the comment area does change (gets updated, reduced,
!         extended, or deleted) between calls to this routine on the
!         same DAF file, the routine's outputs are undefined and
!         subsequent calls to it are likely to trigger an exception.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     B.V. Semenov       (JPL)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 25-NOV-2021 (JDR) (BVS)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!        Added complete code examples from existing code fragments.
!
!-    SPICELIB Version 1.1.0, 12-APR-2012 (BVS)
!
!        Increased FTSIZE (from 1000 to 5000).
!
!-    SPICELIB Version 1.0.0, 08-NOV-2006 (NJB) (KRG) (FST)
!
!        Based on Support Version 2.0.0, 16-NOV-2001 (FST)
!
!-&


! $ Index_Entries
!
!     extract comments from a DAF
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CPOS
      INTEGER               ISRCHI
      INTEGER               NCPOS

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
!     Length of a DAF internal filename.
!
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )

!
!     Decimal value for the DAF comment area end-of-comment (EOC)
!     marker.
!
      INTEGER               INTEOC
      PARAMETER           ( INTEOC = 4 )

!
!     Decimal value for the DAF comment area end-of-line (EOL) marker.
!
      INTEGER               INTEOL
      PARAMETER           ( INTEOL = 0 )

!
!     The maximum number of DAFs that may be open simultaneously.
!
      INTEGER               FTSIZE
      PARAMETER           ( FTSIZE = 5000 )

!
!     Length of a DAF character record, in characters.
!
      INTEGER               MXCREC
      PARAMETER           ( MXCREC = 1000 )
!
!     Local variables
!
      CHARACTER*(1)         CH
      CHARACTER*(MXCREC)    CRECRD
      CHARACTER*(1)         EOCMRK
      CHARACTER*(1)         EOLMRK
      CHARACTER*(IFNLEN)    IFNAME

      INTEGER               CURPOS
      INTEGER               DAFLUN
      INTEGER               EOCPOS
      INTEGER               I
      INTEGER               INDEX
      INTEGER               J
      INTEGER               K
      INTEGER               LINLEN
      INTEGER               NCHARS
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NELPOS
      INTEGER               NUMCOM
      INTEGER               RECNO

      INTEGER               IOSTAT
      INTEGER               BWARD
      INTEGER               FWARD
      INTEGER               FREE
      INTEGER               ND
      INTEGER               NI
!
!     The file table declarations for keeping track of which files
!     are currently in the process of having comments extracted.
!
      INTEGER               FILCHR(FTSIZE)
      INTEGER               FILCNT(FTSIZE)
      INTEGER               FILHAN(FTSIZE)
      INTEGER               LSTHAN
      INTEGER               LSTPOS(FTSIZE)
      INTEGER               LSTREC(FTSIZE)
      INTEGER               NFILES

      LOGICAL               EMPTY
      LOGICAL               EOL
      LOGICAL               FIRST
      LOGICAL               FOUND

!
!     Saved variables
!
      SAVE CRECRD
      SAVE EOCMRK
      SAVE EOLMRK
!
!     Save all of the file table information.
!
      SAVE FILCHR
      SAVE FILCNT
      SAVE FILHAN
      SAVE LSTHAN
      SAVE LSTPOS
      SAVE LSTREC
      SAVE NFILES

      SAVE FIRST

!
!     Initial values
!
      DATA                  FIRST   / .TRUE. /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFEC' )
      END IF
!
!     If this is the first time that this routine has been called,
!     we need to initialize the character value of the end-of-line
!     marker, and the file table variables.
!
      IF ( FIRST ) THEN

         FIRST  = .FALSE.
         NFILES = 0
         LSTHAN = 0
         EOCMRK = CHAR ( INTEOC )
         EOLMRK = CHAR ( INTEOL )

         DO I = 1, FTSIZE

            FILCHR(I) = 0
            FILCNT(I) = 0
            FILHAN(I) = 0
            LSTPOS(I) = 0
            LSTREC(I) = 0

         END DO

      END IF
!
!     Verify that the DAF attached to HANDLE is opened for reading
!     by calling the routine to signal an invalid access mode on a
!     handle.
!
      CALL DAFSIH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFEC' )
         RETURN

      END IF
!
!     Check for a nonpositive BUFFER size.
!
      IF ( BUFSIZ .LE. 0 ) THEN

         CALL SETMSG ( 'The output buffer size was not positive: #.' )
         CALL ERRINT ( '#', BUFSIZ                                   )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                      )
         CALL CHKOUT ( 'DAFEC'                                       )
         RETURN

      END IF
!
!     Convert the DAF handle to its corresponding Fortran logical
!     unit number for reading the comment records.
!
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., DAFLUN )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFEC' )
         RETURN
      END IF
!
!     Get the length of a single character string in the buffer.
!
      LINLEN = LEN ( BUFFER(1) )
!
!     If we have extracted comments from at least one file and we
!     didn't finish, check to see if HANDLE is in the file table.
!
      IF ( NFILES .GT. 0 ) THEN

         INDEX = ISRCHI ( HANDLE, NFILES, FILHAN )

      ELSE

         INDEX = 0

      END IF
!
!     Check to see if we found HANDLE in the file handle table. If
!     we did, INDEX will be > 0.
!
      IF ( INDEX .GT. 0 ) THEN
!
!        Set the record number and the starting position accordingly,
!        i.e., where we left off when we last read from that file.
!
         RECNO  = LSTREC(INDEX)
         CURPOS = LSTPOS(INDEX)
         NCHARS = FILCHR(INDEX)
         NCOMC  = FILCNT(INDEX)

      ELSE
!
!        We have not yet read any comments from this file, so start at
!        the start. To get to the first comment record, we need to skip
!        the file record. We also need to count the number of comment
!        characters.
!
!        Read the file record from the DAF attached to HANDLE. We will
!        get back some stuff that we do not use.
!
         CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )

         IF ( FAILED() ) THEN

            CALL CHKOUT ( 'DAFEC' )
            RETURN

         END IF
!
!        Compute the number of comment records and the number of
!        comment characters. In order to perform these calculations,
!        we assume that we have a valid comment area in the DAF
!        attached to HANDLE.
!
         NCOMR = FWARD - 2

         IF ( NCOMR .GT. 0 ) THEN
!
!           The starting record number is the number of comment records
!           + 1 where the 1 skips the file record.
!
            EMPTY  = .TRUE.
            FOUND  = .FALSE.

            DO WHILE (    ( NCOMR .GT. 0 ) .AND. ( .NOT. FOUND ) &
                      .AND. EMPTY                                )

               RECNO = NCOMR + 1

               READ ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD

               IF ( IOSTAT .NE. 0 ) THEN

                  CALL SETMSG ( 'Error reading comment area of' // &
                                ' binary file named ''#''.'     // &
                                ' IOSTAT = #.'                   )
                  CALL ERRFNM ( '#', DAFLUN                      )
                  CALL ERRINT ( '#', IOSTAT                      )
                  CALL SIGERR ( 'SPICE(FILEREADFAILED)'          )
                  CALL CHKOUT ( 'DAFEC'                          )
                  RETURN

               END IF
!
!              Scan the comment record looking for the end of comments
!              marker.
!
               EOCPOS = CPOS ( CRECRD(1:MXCREC), EOCMRK, 1 )

               IF ( EOCPOS .GT. 0 ) THEN

                  FOUND = .TRUE.

               ELSE

                  NELPOS = NCPOS ( CRECRD(1:MXCREC), EOLMRK, 1 )

                  IF ( NELPOS .NE. 0 ) THEN

                     EMPTY = .FALSE.

                  ELSE

                     NCOMR  = NCOMR  - 1

                  END IF

               END IF

            END DO
!
!           If we do not find the end of comments marker and the
!           comment area is not empty, then it is an error.
!
            IF ( ( .NOT. FOUND ) .AND. ( .NOT. EMPTY ) ) THEN

               CALL SETMSG ( 'The comment area in the DAF file'   // &
                             ' ''#'' may be damaged. The end of'  // &
                             ' the comments could not be found.'   )
               CALL ERRFNM ( '#', DAFLUN                           )
               CALL SIGERR ( 'SPICE(BADCOMMENTAREA)'               )
               CALL CHKOUT ( 'DAFEC'                               )
               RETURN

            ELSE IF ( FOUND ) THEN

               NCOMC = MXCREC * ( NCOMR - 1 ) + EOCPOS - 1

            ELSE IF ( EMPTY ) THEN

               NCOMC = 0

            END IF

         ELSE

            NCOMC = 0

         END IF
!
!        If the number of comment characters, NCOMC, is equal to zero,
!        then we have no comments to read, so set the number of comments
!        to zero, set DONE to .TRUE., check out,  and return.
!
         IF ( NCOMC .EQ. 0 ) THEN

            N    = 0
            DONE = .TRUE.
            CALL CHKOUT ( 'DAFEC' )
            RETURN

         END IF
!
!        Otherwise, set the initial position  in the comment area.
!
         RECNO  = 2
         CURPOS = 1
         NCHARS = 0

      END IF
!
!     Begin reading the comment area into the buffer.
!
      IF ( HANDLE .NE. LSTHAN ) THEN
!
!        If the current DAF handle is not the same as the handle on
!        the last call, then we need to read in the appropriate record
!        from the DAF comment area. Otherwise the record was saved and
!        so we don't need to read it in.
!
         READ ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Error reading comment area of binary' // &
                          ' file named FILE.  IOSTAT = *.'        )
            CALL ERRINT ( '*', IOSTAT                             )
            CALL ERRFNM ( 'FILE', DAFLUN                          )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                 )
            CALL CHKOUT ( 'DAFEC'                                 )
            RETURN

         END IF

      END IF
!
!     Initialize the BUFFER line counter, I, and the line position
!     counter, J.
!
      I = 1
      J = 1
!
!     Start filling up the BUFFER.
!
      NUMCOM = 0
      DONE   = .FALSE.

      DO WHILE ( ( I .LE. BUFSIZ ) .AND. ( .NOT. DONE ) )

         EOL = .FALSE.
         DO WHILE ( .NOT. EOL )

            NCHARS = NCHARS + 1
            CH     = CRECRD(CURPOS:CURPOS)

            IF ( ICHAR( CH ) .EQ. INTEOL ) THEN

               EOL           = .TRUE.
               IF ( J .LE. LINLEN ) THEN

                  BUFFER(I)(J:) = ' '

               END IF

            ELSE

               IF ( J .LE. LINLEN ) THEN

                  BUFFER(I)(J:J) = CH
                  J              = J + 1

               ELSE

                  CALL SETMSG ( 'The output buffer line length (#)'  // &
                                ' was not long enough to contain'    // &
                                ' comment line #.'                    )
                  CALL ERRINT ( '#', LINLEN                           )
                  CALL ERRINT ( '#', I                                )
                  CALL SIGERR ( 'SPICE(COMMENTTOOLONG)'               )
                  CALL CHKOUT ( 'DAFEC'                               )
                  RETURN

               END IF

            END IF
!
!           If we have reached the end of the current comment record,
!           read in the next one and reset the current position.
!           Otherwise, just increment the current position.
!
            IF ( CURPOS .EQ. MXCREC ) THEN

               RECNO = RECNO + 1
               READ ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD

               IF ( IOSTAT .NE. 0 ) THEN

                  CALL SETMSG ( 'Error reading comment area of'  // &
                                ' binary file named #.'          // &
                                '  IOSTAT = #.'                   )
                  CALL ERRFNM ( '#', DAFLUN                       )
                  CALL ERRINT ( '#', IOSTAT                       )
                  CALL SIGERR ( 'SPICE(FILEREADFAILED)'           )
                  CALL CHKOUT ( 'DAFEC'                           )
                  RETURN

               END IF

               CURPOS = 1

            ELSE

               CURPOS = CURPOS + 1

            END IF
!
!           Check to make sure that it is safe to continue, i.e.,
!           that the number of comment characters we have processed
!           has not exceeded the number of comment characters in the
!           comment area of the DAF file. This should never happen.
!
            IF ( NCHARS .GT. NCOMC ) THEN

               CALL SETMSG ( 'Count of comment characters (#)'     // &
                             ' exceeds the number of comment'      // &
                             ' characters (#) in the DAF file #.'   )
               CALL ERRINT ( '#',NCHARS                             )
               CALL ERRINT ( '#',NCOMC                              )
               CALL ERRFNM ( '#',DAFLUN                             )
               CALL SIGERR ( 'SPICE(BADCOMMENTAREA)'                )
               CALL CHKOUT ( 'DAFEC'                                )
               RETURN

            END IF

         END DO
!
!        We have just completed a comment line, so we save the comment
!        number, increment the buffer line counter, I, and reset the
!        buffer line position counter, J.
!
         NUMCOM = I
         I      = I + 1
         J      = 1
!
!        Check for the end of the comments.
!
         IF ( NCHARS .EQ. NCOMC ) THEN
!
!           If we have reached the end of the comments, signaled
!           by having processed all of the comment characters, NCOMC,
!           then we are done. So, set DONE to .TRUE. and remove the
!           entry for this file from the file table.
!
            DONE = .TRUE.
            LSTHAN = 0
!
!           0 <= INDEX <= NFILES, and we only want to remove things
!           from the file table if:
!
!              The file we are currently reading from is in the
!              file table, INDEX > 0, which implies NFILES > 0.
!
!           So, if INDEX > 0, we know that there are files in the file
!           table, and that we are currently reading from one of them.
!
            IF ( INDEX .GT. 0 ) THEN

               DO K = INDEX, NFILES - 1

                  FILCHR(K) = FILCHR(K+1)
                  FILCNT(K) = FILCNT(K+1)
                  FILHAN(K) = FILHAN(K+1)
                  LSTREC(K) = LSTREC(K+1)
                  LSTPOS(K) = LSTPOS(K+1)

               END DO

               NFILES = NFILES - 1

            END IF

         END IF

      END DO
!
!     Set the number of comment lines in the buffer
!
      N = NUMCOM
!
!     At this point, we have either filled the buffer or we have
!     finished reading in the comment area. Find out what has
!     happened and act accordingly.
!
      IF ( .NOT. DONE ) THEN
!
!        If we are not done, then we have filled the buffer, so save
!        everything that needs to be saved in the file table before
!        exiting.
!
         IF ( INDEX .EQ. 0 ) THEN
!
!           This was the first time that the comment area of this file
!           has been read, so add it to the file table and save all of
!           its information if there is room in the file table.
!
            IF ( NFILES .GE. FTSIZE ) THEN

               CALL SETMSG ( 'The file table is full with # files,'  // &
                             ' and another file could not be added.'  )
               CALL ERRINT ( '#',FTSIZE                               )
               CALL SIGERR ( 'SPICE(FILETABLEFULL)'                   )
               CALL CHKOUT ( 'DAFEC'                                  )
               RETURN

            END IF

            NFILES         = NFILES + 1
            FILCHR(NFILES) = NCHARS
            FILCNT(NFILES) = NCOMC
            FILHAN(NFILES) = HANDLE
            LSTREC(NFILES) = RECNO
            LSTPOS(NFILES) = CURPOS
            LSTHAN         = HANDLE

         ELSE
!
!           The comment area of this file is already in the file table,
!           so just update its information.
!
            FILCHR(INDEX) = NCHARS
            LSTREC(INDEX) = RECNO
            LSTPOS(INDEX) = CURPOS
            LSTHAN        = HANDLE

         END IF

      END IF

      CALL CHKOUT ( 'DAFEC' )
      RETURN
      END
