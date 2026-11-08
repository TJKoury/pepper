!$Procedure SPCAC ( SPK and CK, add comments )

      SUBROUTINE SPCAC ( HANDLE, UNIT, BMARK, EMARK )

! $ Abstract
!
!     Store text from a text file in the comment area of a binary SPK
!     or CK file, appending it to whatever text may already have
!     been stored there.
!
! $ Required_Reading
!
!     SPC
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               HANDLE
      INTEGER               UNIT
      CHARACTER*(*)         BMARK
      CHARACTER*(*)         EMARK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle assigned to binary SPK or CK file.
!     UNIT       I   Logical unit connected to comment file.
!     BMARK      I   Beginning marker.
!     EMARK      I   Ending marker.
!
! $ Detailed_Input
!
!     HANDLE   is the handle assigned to the binary SPK or CK file
!              which has been opened for write access.
!
!              Use the SPICELIB routine DAFOPW to open the file for
!              write access and get HANDLE. Upon exit, this binary file
!              will contain the specified text from the comment file in
!              its comment area, appended to whatever text may already
!              have been stored there. SPCAC will include an extra blank
!              line between the original text and the appended text.
!
!     UNIT     is the logical unit connected to the comment file.
!              This file must contain only text (printable
!              ASCII characters, namely ASCII 32-126).  Open this
!              file with read access and get its UNIT using TXTOPR.
!
!     BMARK,
!     EMARK    are markers that delimit a group of consecutive
!              lines in the text file (UNIT), that get stored in the
!              comment area of the binary file (HANDLE).
!
!              The group of lines begins with the line that
!              immediately follows the first line of the file
!              equivalent to BMARK. It ends with line that
!              precedes the next line of the file equivalent to
!              EMARK, including blank lines. Leading and
!              trailing blanks are ignored when testing for
!              equivalence.
!
!              By convention, if BMARK is blank, the first line of
!              the group is the first line of the file; if EMARK is
!              blank, the last line of the group is the last line
!              of the file.
!
!              If a marker is non-blank and is not found, or if
!              non-blank markers are on successive lines in the text
!              file, nothing gets stored in the comment area of
!              the binary file.
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
!     1)  If the specified DAF file is not open for write access, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If there is a problem reading from the comment area of the
!         binary file, the error SPICE(FILEREADFAILED) is signaled.
!
!     3)  If there is a problem writing to the comment area of the
!         binary file, the error SPICE(FILEWRITEFAILED) is signaled.
!
!     4)  If there is a problem reading from the text file, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     5)  If a non-printing ASCII character is encountered in the
!         comments, an error is signaled by a routine in the call tree
!         of this routine.
!
! $ Files
!
!     See arguments HANDLE and UNIT.
!
! $ Particulars
!
!     The structure of SPK and CK files accommodates comments in
!     addition to data. The following three routines are available
!     for accessing the comment area of a binary SPK or CK file:
!
!           SPCAC           add comments
!
!           SPCEC           extract comments
!
!           SPCDC           delete comments
!
!     Note that comments must consist of only text, that is, printable
!     ASCII characters, specifically ASCII 32-126. This excludes
!     tabs (ASCII 9) and control characters.
!
!     The SPC conversion routines---SPCB2A, SPCA2B, SPCB2T, and
!     SPCT2B---include these comments when converting SPK and CK
!     files between binary and text formats.
!
! $ Examples
!
!     Suppose we have a binary SPK file called A.BSP and we have
!     a text file called COMMENTS.TXT that contains comments
!     about the data in the SPK file.
!
!     The following code fragment stores the entire contents of
!     COMMENTS.TXT in the comment area of A.BSP.
!
!            CALL DAFOPW ( 'A.BSP', HANDLE )
!
!            CALL TXTOPR ( 'COMMENTS.TXT', UNIT )
!
!            BMARK = ' '
!            EMARK = ' '
!
!            CALL SPCAC  ( HANDLE, UNIT, BMARK, EMARK )
!
!            CLOSE ( UNIT )
!
!     Now suppose MORE.TXT is a text file that contains additional
!     information about the data in A.BSP, as well as information
!     about several other SPK files.  The contents of MORE.TXT are
!
!               \begin A info
!
!                 DATAFILE = A
!                 SOURCE   = JPL, 1990 September 12
!                 MISSION  = Galileo
!
!               \end A info
!
!               \begin B info
!
!                 DATAFILE = B
!                 SOURCE   = JPL, 1988 August 1
!                 MISSION  = Voyager 2
!
!               \end B info
!
!               \begin C info
!
!                 DATAFILE = C
!                 SOURCE   = JPL, 1994 January 31
!                 MISSION  = Mars Observer
!
!               \end C info
!
!     This code fragment stores only the information that pertains
!     to A.BSP, and appends it to the text from COMMENTS.TXT that
!     has already been stored in the comment area of A.BSP
!
!            CALL TXTOPR ( 'MORE.TXT', UNIT )
!
!            BMARK = '\begin A info'
!            EMARK = '\end A info'
!
!            CALL SPCAC  ( HANDLE, UNIT, BMARK, EMARK )
!
!            CLOSE ( UNIT )
!
!            CALL DAFCLS ( HANDLE )
!
!     Note that, ignoring leading and trailing blanks, BMARK and
!     EMARK are exactly equivalent to lines in the text file.
!     If the assignment had been instead BMARK = '\ begin A info',
!     with an extra space between the slash and the word begin,
!     SPCAC would not have found the marker and no comments from
!     the text file would be written to the binary file.
!
! $ Restrictions
!
!     1)  The lines in the comment file should not exceed 1000
!         characters in length. SPCAC truncates lines longer than
!         this on the right.
!
!     2)  Use TXTOPR to open text files for read access and get
!         the logical unit. System dependencies regarding
!         opening text files have been isolated in the routines
!         TXTOPN and TXTOPR.
!
!     3)  This routine assumes that the comment area of the binary SPK
!         or CK file contains only text stored by SPCAC. Comments
!         written any other way may not be handled properly.
!
!     4)  The comment area of the binary SPK or CK file must contain
!         only one EOT character. This routine seeks back from the
!         last reserved record searching for the first EOT it
!         encounters. Thus the multiple EOT's will cause the appended
!         comments to be invisible to any reader that starts at the
!         first reserved record and reads until the first EOT present.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     J.E. McLean        (JPL)
!     W.L. Taber         (JPL)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!        Moved the contents of the $Files section to the description of
!        HANDLE and UNIT in $Detailed_Input section, and referred to
!        them from $Files. Removed unnecessary entries from $Revisions
!        section.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        Updated this routine to utilize new handle manager
!        interfaces.
!
!-    SPICELIB Version 1.3.0, 12-FEB-1999 (FST)
!
!        Modified the EOT search code to seek back through any
!        reserved records, as opposed to just the last one. This
!        provides the flexibility to use DAFOPN to reserve records
!        that may ultimately be used for storing comments. As a direct
!        result of these changes the SPICE(MISSINGEOT) error is no
!        longer signaled, since if no EOT is found in the reserved
!        records, they are considered available for writes.
!
!-    SPICELIB Version 1.2.0, 12-MAY-1994 (KRG)
!
!        Added an IF statement so that DAFARR is called only if new
!        reserved records need to be added to the comment area.
!
!-    SPICELIB Version 1.1.0, 09-APR-1993 (KRG)
!
!        Added code to initialize the variable LASTRR to zero. This
!        variable is used in a function call, MAX ( LASTRR-1, 1 ),
!        regardless of whether or not any reserved records are in
!        the file. Thus the need to initialize it.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 05-APR-1991 (JEM)
!
!-&


! $ Index_Entries
!
!     add comments to SPK or CK file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        This routine now utilizes DAFSIH to determine if
!        HANDLE is open for WRITE access. The call to DAFHLU
!        has been replaced with a call to ZZDDHHLU, the handle
!        manager interface for retrieving a logical unit.
!        DAFHLU is no longer used, since it locks the unit
!        returned to its HANDLE, tying up resources in the
!        handle manager.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               COUNTC
      INTEGER               LASTNB

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
!     IFNLEN      is the length of a DAF internal file name.
!
!     MAXCPR      is the maximum number of characters per DAF record and
!                 hence the maximum comment line length.
!
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60     )

      INTEGER               MAXCPR
      PARAMETER           ( MAXCPR = 1000   )

!
!     Local variables
!
      CHARACTER*(MAXCPR+2)  DATA
      CHARACTER*(1)         EOT
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(MAXCPR)    LINE
      CHARACTER*(1)         NULL
      CHARACTER*(MAXCPR)    RECORD

      INTEGER               BLINE
      INTEGER               BWARD
      INTEGER               C
      INTEGER               CHARS
      INTEGER               DAFU
      INTEGER               EOL
      INTEGER               ELINE
      INTEGER               FREE
      INTEGER               FWARD
      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               L
      INTEGER               LASTRR
      INTEGER               LINES
      INTEGER               ND
      INTEGER               NI
      INTEGER               NR
      INTEGER               NRR
      INTEGER               POS
      INTEGER               POSEOT
      INTEGER               REC
      INTEGER               SPACE
      INTEGER               START
      INTEGER               TOTAL

      LOGICAL               FOUND

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'SPCAC' )
      END IF

!
!     Before doing anything, determine if the file associated with
!     HANDLE is available for WRITE access.
!
      CALL DAFSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPCAC' )
         RETURN
      END IF

!
!     Rewind the comment file - we'll start the search for BMARK
!     and EMARK at the beginning.  Once we have located the markers,
!     count the number of lines between them and the number of
!     characters in those lines, ignoring trailing blanks.
!
!     We rewind the file so that we know where the file pointer is.
!     LOCLN will compute BLINE and ELINE taking the current position
!     of the file pointer as line 1.
!
      REWIND ( UNIT )

      CALL LOCLN ( UNIT, BMARK, EMARK, LINE, BLINE, ELINE, FOUND )

!
!     If the markers are not found, or if BMARK and EMARK are on
!     successive lines, there is nothing to put in the comment area.
!
      IF ( .NOT. FOUND ) THEN
         CALL CHKOUT ( 'SPCAC' )
         RETURN
      END IF

!
!     Adjust BLINE and ELINE so we are pointing to the group of lines
!     BETWEEN the markers.  Check and make sure there is at least one
!     line in the group.
!
      IF ( BMARK .NE. ' ' ) THEN
         BLINE = BLINE + 1
      END IF

      IF ( EMARK .NE. ' ' ) THEN
         ELINE = ELINE - 1
      END IF

      IF ( BLINE .GT. ELINE ) THEN
         CALL CHKOUT ( 'SPCAC' )
         RETURN
      END IF

!
!     Calculate the number of lines and the total number of characters
!     in those lines.  The characters must all be printable, or
!     else COUNTC will signal an error.
!
      LINES = ELINE - BLINE + 1
      CHARS = COUNTC ( UNIT, BLINE, ELINE, LINE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPCAC' )
         RETURN
      END IF

!
!     Read the file record to find out if the DAF contains any
!     reserved records.  The reserved records in an array file
!     are stored between the first record (the file record) and
!     the first summary record.  FWARD is the record number of
!     that first summary record, and NRR is the number of reserved
!     records in the file.
!
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )
      NRR    = FWARD - 2

!
!     Get the logical unit for reading from and writing to the DAF.
!
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., DAFU )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SPCAC' )
         RETURN
      END IF

!
!     Assign the value of NULL and EOT.  NULL gets appended to the
!     end of each line of text.  EOT gets appended to the end of
!     all the comments.  Assign initial values for SPACE, RECORD,
!     and START.
!
      NULL = CHAR( 0)
      EOT  = CHAR( 4)
      SPACE  = 0
      RECORD = ' '
      START  = 0
      LASTRR = 0

      IF ( NRR .NE. 0 ) THEN
!
!        At this point, we know there exist reserved records in the
!        DAF. We need to search from the last record to the first,
!        seeking for the EOT (end of transmission) character, as it
!        marks the end of the comment region.
!
         LASTRR = FWARD - 1
         I      = LASTRR + 1
         POSEOT = 0

         DO WHILE ( ( I .GT. 1 ) .AND. ( POSEOT .EQ. 0 ) )
!
!           Decrement the counter now.  This keeps it in
!           sync with the exit conditions.
!
            I = I - 1
            READ ( DAFU, REC=I, IOSTAT=IOSTAT ) RECORD

            IF ( IOSTAT .NE. 0 ) THEN
               CALL SETMSG ( 'Error reading comment area of binary '// &
                             'file named FILE.  IOSTAT = *.'          )
               CALL ERRINT ( '*', IOSTAT                              )
               CALL ERRFNM ( 'FILE', DAFU                             )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'                  )
               CALL CHKOUT ( 'SPCAC'                                  )
               RETURN
            END IF
!
!           Call INDEX. If POSEOT is 0, then RECORD doesn't contain
!           the EOT character.
!
            POSEOT = INDEX ( RECORD, EOT )

         END DO

!
!        The amount of free space in the reserved records of the
!        files is determined by the number of empty reserved
!        records ( LASTRR - I ), and the number of characters used
!        in last record with data (MAXCPR - POSEOT).
!
         SPACE = MAXCPR*(LASTRR - I + 1) - POSEOT
!
!        Adjust the value of LASTRR to indicate the record where
!        the EOT lies.  From here on out, the purpose of this
!        variable is to indicate where to start dumping comments.
!
         LASTRR = I
!
!        If POSEOT is 0, then there are no comments in the file, but
!        there are reserved records.  Branch on this:
!
         IF ( POSEOT .EQ. 0 ) THEN
!
!           Leaving this string index at zero may be causing all sorts
!           of warning bells to go off in your head. However, before
!           this index value is used to address the contents of a
!           string it's incremented by 1.
!
            START = POSEOT
!
!        Handle the case when POSEOT is non-zero.
!
         ELSE
!
!           Replace the end-of-transmission character with a new line
!           character (we use null), so a blank line will come between
!           the old text and new text in the comment area.  START is the
!           position after which the first character of the new text
!           goes.
!
            RECORD ( POSEOT:POSEOT ) = NULL

            START = POSEOT

         END IF

      END IF

!
!     Compute the number of records (NR) needed to store all of these
!     characters.
!
!     Each line should end with a null (ASCII 0) character.  The final
!     line should also be followed by an end-of-transmission (ASCII 4)
!     character.  So the total is the number of characters, plus the
!     number of lines, plus one for the EOT.
!
!     If the TOTAL fits in the SPACE available in the last reserved
!     record, we don't need to reserve any more.  Otherwise compute
!     the number we need.
!
      TOTAL = CHARS + LINES + 1

      IF (  ( TOTAL - SPACE ) .GT. 0  ) THEN

         NR = (  ( TOTAL - SPACE ) - 1  ) / MAXCPR    +   1

      ELSE

         NR = 0

      END IF

!
!     Reserve the records to create a comment area large enough
!     to hold it all, if we need to.  If we can't do it, there's no
!     point in going on.
!
      IF ( NR .GT. 0 ) THEN

         CALL DAFARR ( HANDLE, NR )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SPCAC' )
            RETURN
         END IF

      END IF

!
!     Load the group of lines in the comment file into the reserved
!     records. Keep adding lines to the current record until it has
!     been filled, then write it to the DAF, and begin a new record.
!
      REC    = MAX ( LASTRR-1, 1 )
      POS    = START

!
!     Rewind the text file then skip past the lines that we don't want
!     to position the file pointer at the correct record.
!
      REWIND ( UNIT )

      DO L = 1, BLINE - 1

         READ ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE

         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Error reading line # in text file named '  // &
                          'FILE.  IOSTAT = *.'                        )
            CALL ERRINT ( '#', L                                      )
            CALL ERRINT ( '*', IOSTAT                                 )
            CALL ERRFNM ( 'FILE', UNIT                                )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
            CALL CHKOUT ( 'SPCAC'                                     )
            RETURN
         END IF

      END DO

!
!     Start reading the lines that we do want.  LINE is MAXCPR long
!     so that's the maximum number of characters that are read.
!
      DO L = 1, LINES

         READ ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE

         IF ( IOSTAT .NE. 0 ) THEN
            CALL SETMSG ( 'Error reading line # in text file named '  // &
                          'FILE.  IOSTAT = *.'                        )
            CALL ERRINT ( '#', L+BLINE-1                              )
            CALL ERRINT ( '*', IOSTAT                                 )
            CALL ERRFNM ( 'FILE', UNIT                                )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
            CALL CHKOUT ( 'SPCAC'                                     )
            RETURN
         END IF

!
!        Each line is followed by a null character.
!
         DATA          = LINE
         EOL           = LASTNB ( DATA ) + 1
         DATA(EOL:EOL) = NULL

!
!        The final line is followed by an additional
!        end-of-transmission character.
!
         IF ( L .EQ. LINES ) THEN
            EOL           = EOL + 1
            DATA(EOL:EOL) = EOT
         END IF

!
!        Moving characters one at a time is slower, but simpler,
!        than trying to move them in blocks.
!
         DO C = 1, EOL

!
!           If the current record is full, write it to the DAF.
!
            IF ( POS .EQ. MAXCPR ) THEN

               REC = REC + 1
               WRITE ( DAFU, REC=REC, IOSTAT=IOSTAT ) RECORD

               IF ( IOSTAT .EQ. 0 ) THEN
                  RECORD = ' '
                  POS    = 0

               ELSE
                  CALL SETMSG ( 'Error writing to record # of the '  // &
                                'binary file named FILE. IOSTAT = *.' )
                  CALL ERRINT ( '#', REC                              )
                  CALL ERRINT ( '*', IOSTAT                           )
                  CALL ERRFNM ( 'FILE', DAFU                          )
                  CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'              )
                  CALL CHKOUT ( 'SPCAC'                               )
                  RETURN
               END IF

            END IF

!
!           Add the next character to the current record.
!
            POS             = POS + 1
            RECORD(POS:POS) = DATA(C:C)

         END DO

      END DO

!
!     Write the final record to the DAF.
!
      REC = REC + 1
      WRITE ( DAFU, REC=REC, IOSTAT=IOSTAT ) RECORD

      IF ( IOSTAT .NE. 0 ) THEN
         CALL SETMSG ( 'Error writing the final record, record #, '   // &
                       'of the binary file named FILE.  IOSTAT = *.'  )
         CALL ERRINT ( '#', REC                                       )
         CALL ERRINT ( '*', IOSTAT                                    )
         CALL ERRFNM ( 'FILE', DAFU                                   )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'                       )
         CALL CHKOUT ( 'SPCAC'                                        )
         RETURN
      END IF

      CALL CHKOUT ( 'SPCAC' )
      RETURN
      END
