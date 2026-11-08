!$Procedure DAFAC ( DAF add comments )

      SUBROUTINE DAFAC ( HANDLE, N, BUFFER )

! $ Abstract
!
!     Add comments from a buffer of character strings to the comment
!     area of a binary DAF file, appending them to any comments which
!     are already present in the file's comment area.
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
      INTEGER               N
      CHARACTER*(*)         BUFFER(*)

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a DAF opened with write access.
!     N          I   Number of comments to put into the comment area.
!     BUFFER     I   Buffer of comments to put into the comment area.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of a binary DAF which has been opened
!              with write access.
!
!     N        is the number of comments in BUFFER that are to be added
!              to the comment area of the binary DAF attached to HANDLE.
!
!     BUFFER   is a buffer containing comments which are to be added
!              to the comment area of the binary DAF attached to HANDLE.
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
!     1)  If the number of comments to be added is not positive, the
!         error SPICE(INVALIDARGUMENT) is signaled.
!
!     2)  If a non printing ASCII character is encountered in the
!         comments, the error SPICE(ILLEGALCHARACTER) is signaled.
!
!     3)  If the binary DAF file attached to HANDLE is not open with
!         write access, an error is signaled by a routine in the call
!         tree of this routine.
!
!     4)  If the end of the comments cannot be found, i.e., the end of
!         comments marker is missing on the last comment record, the
!         error SPICE(BADCOMMENTAREA) is signaled.
!
! $ Files
!
!     See argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     A binary DAF contains a data area which is reserved for storing
!     annotations or descriptive textual information about the data
!     contained in a file. This area is referred to as the ``comment
!     area'' of the file. The comment area of a DAF is a line oriented
!     medium for storing textual information. The comment area
!     preserves leading or embedded white space in the line(s) of text
!     which are stored so that the appearance of the information will
!     be unchanged when it is retrieved (extracted) at some other time.
!     Trailing blanks, however, are NOT preserved, due to the way that
!     character strings are represented in standard Fortran 77.
!
!     This routine will take a buffer of text lines and add (append)
!     them to the comment area of a binary DAF. If there are no
!     comments in the comment area of the file, then space will be
!     allocated and the text lines in BUFFER will be placed into the
!     comment area. The text lines may contain only printable ASCII
!     characters (decimal values 32 - 126).
!
!     There is NO maximum length imposed on the significant portion
!     of a text line that may be placed into the comment area of a
!     DAF. The maximum length of a line stored in the comment area
!     should be reasonable, however, so that they may be easily
!     extracted. A good maximum value for this would be 255 characters,
!     as this can easily accommodate ``screen width'' lines as well as
!     long lines which may contain some other form of information.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) This example demonstrates how to append new comments to the
!        comment area of a DAF file.
!
!        Use the SPK kernel below as input DAF file for the program.
!
!           earthstns_itrf93_201023.bsp
!
!
!        Example code begins here.
!
!
!              PROGRAM DAFAC_EX1
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
!              PARAMETER           ( BUFSIZ = 25 )
!
!              INTEGER               CMTSIZ
!              PARAMETER           ( CMTSIZ = 7  )
!
!              INTEGER               LINLEN
!              PARAMETER           ( LINLEN = 1000 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(LINLEN)    BUFFER ( BUFSIZ )
!              CHARACTER*(LINLEN)    NEWCMT ( CMTSIZ )
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               N
!
!              LOGICAL               DONE
!
!        C
!        C     Set the new comments to be added to the DAF file.
!        C
!              DATA                  NEWCMT /
!             .  '================== NEW COMMENTS ==================',
!             .  '',
!             .  '   New comments can be appended to the end of the',
!             .  '   comment area of a DAF file, with a single',
!             .  '   operation.',
!             .  '',
!             .  '================ END NEW COMMENTS ================' /
!
!
!        C
!        C     Open a DAF for write. Return a HANDLE referring to the
!        C     file.
!        C
!              CALL DAFOPW ( KERNEL, HANDLE )
!
!        C
!        C     Print the end of comment area from the DAF file.
!        C     (Maximum 15 lines.)
!        C
!              DONE = .FALSE.
!
!              DO WHILE ( .NOT. DONE )
!
!                 CALL DAFEC  ( HANDLE, 15, N, BUFFER, DONE )
!
!                 IF ( DONE ) THEN
!
!                    WRITE(*,'(A)') 'End of comment area of input '
!             .                  // 'DAF file (max. 15 lines): '
!                    WRITE(*,'(A)') '-------------------------------'
!             .                  // '-------------------------------'
!
!                    DO I = 1, N
!
!                       WRITE (*,*) BUFFER(I)(:RTRIM(BUFFER(I)))
!
!                    END DO
!
!                    WRITE(*,'(A)') '-------------------------------'
!             .                  // '-------------------------------'
!
!                 END IF
!
!              END DO
!
!        C
!        C     Append the new comments to the DAF file.
!        C
!              CALL DAFAC ( HANDLE, CMTSIZ, NEWCMT )
!
!        C
!        C     Safely close the DAF.
!        C
!              CALL DAFCLS ( HANDLE )
!
!        C
!        C     Check if the comments have indeed appended.
!        C
!        C     Open a DAF for read.
!        C
!              CALL DAFOPR ( KERNEL, HANDLE )
!              DONE = .FALSE.
!
!              DO WHILE ( .NOT. DONE )
!
!                 CALL DAFEC  ( HANDLE, BUFSIZ, N, BUFFER, DONE )
!
!                 IF ( DONE ) THEN
!
!                    WRITE(*,'(A)') 'End of comment area of input '
!             .                  // 'DAF file (max. 25 lines): '
!                    WRITE(*,'(A)') '-------------------------------'
!             .                  // '-------------------------------'
!
!                    DO I = 1, N
!
!                       WRITE (*,*) BUFFER(I)(:RTRIM(BUFFER(I)))
!
!                    END DO
!
!                    WRITE(*,'(A)') '-------------------------------'
!             .                  // '-------------------------------'
!
!                 END IF
!
!              END DO
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
!        End of comment area of input DAF file (max. 15 lines):
!        --------------------------------------------------------------
!            DSS-65_DXYZ       =    (    -0.0100          0.0242     ***
!            DSS-65_TOPO_EPOCH =       @2020-OCT-23/00:00
!            DSS-65_UP         =       'Z'
!            DSS-65_NORTH      =       'X'
!
!         \begintext
!        --------------------------------------------------------------
!        End of comment area of input DAF file (max. 25 lines):
!        --------------------------------------------------------------
!            DSS-65_DXYZ       =    (    -0.0100          0.0242     ***
!            DSS-65_TOPO_EPOCH =       @2020-OCT-23/00:00
!            DSS-65_UP         =       'Z'
!            DSS-65_NORTH      =       'X'
!
!         \begintext
!         ================== NEW COMMENTS ==================
!
!            New comments can be appended to the end of the
!            comment area of a DAF file, with a single
!            operation.
!
!         ================ END NEW COMMENTS ================
!        --------------------------------------------------------------
!
!
!        Warning: incomplete output. 2 lines extended past the right
!        margin of the header and have been truncated. These lines are
!        marked by "***" at the end of each line.
!
!
! $ Restrictions
!
!     1)  This routine uses constants that are specific to the ASCII
!         character sequence. The results of using this routine with
!         a different character sequence are unpredictable.
!
!     2)  This routine is only used to extract records on environments
!         whose characters are a single byte in size. Updates to this
!         routine and routines in its call tree may be required to
!         properly handle other cases.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 25-NOV-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code examples from existing code fragments.
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        Updated this routine to utilize the new handle manager
!        interfaces.
!
!-    SPICELIB Version 1.0.0, 26-JUL-1994 (KRG)
!
!-&


! $ Index_Entries
!
!     add comments to a binary DAF file
!     append comments to a DAF file comment area
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 16-NOV-2001 (FST)
!
!        The call to DAFHLU has been replaced with a call to ZZDDHHLU,
!        the handle manager interface for retrieving a logical unit.
!        DAFHLU is no longer used, since it locks the unit returned to
!        its HANDLE, tying up resources in the handle manager.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               CPOS
      INTEGER               LASTNB
      INTEGER               NCPOS

      LOGICAL               FAILED
      LOGICAL               RETURN
!
!     Local parameters
!
!     Length of a DAF file internal filename.
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
!     Length of a DAF character record, in characters.
!
      INTEGER               MXCREC
      PARAMETER           ( MXCREC = 1000 )
!
!     Maximum and minimum decimal values for the printable ASCII
!     characters.
!
      INTEGER               MAXPCH
      PARAMETER           ( MAXPCH = 126 )

      INTEGER               MINPCH
      PARAMETER           ( MINPCH = 32 )
!
!     Local variables
!
      CHARACTER*(MXCREC)    CRECRD
      CHARACTER*(1)         EOCMRK
      CHARACTER*(1)         EOLMRK
      CHARACTER*(IFNLEN)    IFNAME

      INTEGER               CURPOS
      INTEGER               DAFLUN
      INTEGER               EOCPOS
      INTEGER               I
      INTEGER               J
      INTEGER               LENGTH
      INTEGER               NCHARS
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NELPOS
      INTEGER               NEWREC
      INTEGER               NOTUSD
      INTEGER               RECNO
      INTEGER               RINUSE
      INTEGER               SPACE

      INTEGER               IOSTAT
      INTEGER               BWARD
      INTEGER               FWARD
      INTEGER               FREE
      INTEGER               ND
      INTEGER               NI

      LOGICAL               EMPTY
      LOGICAL               FIRST
      LOGICAL               FOUND
!
!     Saved variables
!
      SAVE EOLMRK
      SAVE EOCMRK
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
         CALL CHKIN ( 'DAFAC' )
      END IF
!
!     The lines of text in BUFFER will be ``packed'' into DAF comment
!     records: the significant portion of each comment line from BUFFER
!     will be terminated using the special character EOLMRK to indicate
!     the end of the line. When a comment record is full or all of the
!     comments have been added, the comment record will be written to
!     the comment area of the binary DAF file.
!
!     If this is the first time that this routine has been called,
!     we need to initialize the character value for the end-of-line
!     marker and the character value for the end of comments marker.
!
      IF ( FIRST ) THEN

         FIRST  = .FALSE.
         EOCMRK = CHAR ( INTEOC )
         EOLMRK = CHAR ( INTEOL )

      END IF
!
!     Verify that the DAF file attached to HANDLE is opened with write
!     access.
!
      CALL DAFSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFAC' )
         RETURN

      END IF
!
!     Convert the DAF file handle to its corresponding Fortran logical
!     unit number for reading and writing comment records.
!
      CALL ZZDDHHLU ( HANDLE, 'DAF', .FALSE., DAFLUN )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'DAFAC' )
         RETURN
      END IF
!
!     Check for a nonpositive number of lines in the buffer.
!
      IF ( N .LE. 0 ) THEN

         CALL SETMSG ( 'The number of comment lines to be added'  // &
                       ' to the binary DAF file ''#'' was not'    // &
                       ' positive: #.'                             )
         CALL ERRFNM ( '#', DAFLUN                                 )
         CALL ERRINT ( '#', N                                      )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                    )
         CALL CHKOUT ( 'DAFAC'                                     )
         RETURN

      END IF
!
!     Count the number of characters in the buffer ignoring trailing
!     blanks on nonblank lines and blank lines. The count will be
!     modified to include the contribution of blank lines later. This
!     count is used to determine the number of character records to be
!     added to the binary DAF file attached to HANDLE.
!
      NCHARS = 0

      DO I = 1, N
!
!        Get the length of the significant portion of a comment line.
!
         LENGTH = LASTNB ( BUFFER(I) )
!
!        Scan the comment line for non printing characters.
!
         DO J = 1, LENGTH
!
!           Check to see that the characters in the buffer are all
!           printing ASCII characters. The bounds for printing ASCII
!           characters are given by MINPCH and MAXPCH, which are
!           defined in the $ Local Parameters section of the header.
!
            IF ( ( ICHAR ( BUFFER(I)(J:J) ) .GT. MAXPCH ) .OR. &
                 ( ICHAR ( BUFFER(I)(J:J) ) .LT. MINPCH )      ) THEN

                  CALL SETMSG ( 'A nonprinting character was'      // &
                                ' encountered in the comment'      // &
                                ' buffer. Value: #'                 )
                  CALL ERRINT ( '#', ICHAR( BUFFER(I)(J:J) )        )
                  CALL SIGERR ( 'SPICE(ILLEGALCHARACTER)'           )
                  CALL CHKOUT ( 'DAFAC'                             )
                  RETURN

            END IF

         END DO
!
!        Increment the number of characters by the length of the
!        significant portion of the current line in the buffer.
!
         NCHARS = NCHARS + LENGTH

      END DO
!
!     We need to include the number of end of line markers in the
!     number of characters, so add the number of comment lines to
!     be added, N, to the number of characters, NCHARS. This is where
!     the contribution of any blank lines gets added to the character
!     count. We also need to have space for the end of comments marker.
!
      NCHARS = NCHARS + N + 1
!
!     Get the current number of comment records and comment characters
!     from the DAF file attached to HANDLE. We will also get back some
!     extra stuff that we do not use.
!
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFAC' )
         RETURN

      END IF
!
!     Compute the number of comment records and the number of comment
!     characters. In order to perform these calculations, we assume
!     that we have a valid comment area in the DAF file attached to
!     HANDLE.
!
      NCOMR = FWARD - 2

      IF ( NCOMR .GT. 0 ) THEN
!
!        The starting record number is the number of comment records + 1
!        where the 1 skips the file record.
!
         EMPTY  = .TRUE.
         FOUND  = .FALSE.
         NOTUSD = 0

         DO WHILE ( ( NCOMR .GT. 0 ) .AND. ( .NOT. FOUND ) .AND. EMPTY )

            RECNO = NCOMR + 1

            READ ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD

            IF ( IOSTAT .NE. 0 ) THEN

               CALL SETMSG ( 'Error reading comment area of binary' // &
                             ' file named ''#''.  IOSTAT = #.'       )
               CALL ERRFNM ( '#', DAFLUN                             )
               CALL ERRINT ( '#', IOSTAT                             )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'                 )
               CALL CHKOUT ( 'DAFAC'                                 )
               RETURN

            END IF
!
!           Scan the comment record looking for the end of comments
!           marker.
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
                  NOTUSD = NOTUSD + 1

               END IF

            END IF

         END DO
!
!        If we do not find the end of comments marker and the comment
!        area is not empty, then it is an error.
!
         IF ( ( .NOT. FOUND ) .AND. ( .NOT. EMPTY ) ) THEN

            CALL SETMSG ( 'The comment area in the DAF file'   // &
                          ' ''#'' may be damaged. The end of'  // &
                          ' the comments could not be found.'   )
            CALL ERRFNM ( '#', DAFLUN                           )
            CALL SIGERR ( 'SPICE(BADCOMMENTAREA)'               )
            CALL CHKOUT ( 'DAFAC'                               )
            RETURN

         ELSE IF ( FOUND ) THEN

            NCOMC = MXCREC * ( NCOMR - 1 ) + EOCPOS - 1

         ELSE IF ( EMPTY ) THEN

            NCOMC = 0

         END IF

      ELSE

         NCOMC  = 0
         NOTUSD = 0

      END IF
!
!     Determine the amount of free space in the comment area. If
!     there are some comment records allocated, the space available
!     is the number of comment records allocated times the length of
!     a comment record, minus the number of comment characters already
!     used. Otherwise, the space available is zero.
!
      IF ( NCOMR + NOTUSD .GT. 0 ) THEN

         SPACE = NOTUSD * MXCREC + NCOMR * MXCREC - NCOMC

      ELSE

         SPACE = 0

      END IF
!
!     Determine the number of new comment records which are necessary
!     to store all of the comments from the buffer.
!
      IF ( NCHARS .GT. SPACE ) THEN
!
!        If there are more characters to store than available space
!        we need at least one new record.
!
         NEWREC = 1  +  (  ( NCHARS - SPACE )  - 1  ) / MXCREC

      ELSE
!
!        Otherwise, we do not need any new records.
!
         NEWREC = 0

      END IF
!
!     Now add the necessary number of comment records to the file,
!     if we need to add any.
!
      IF ( NEWREC .GT. 0 ) THEN

         CALL DAFARR ( HANDLE, NEWREC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'DAFAC' )
            RETURN
         END IF

      END IF
!
!     At this point, we know that we have enough space to write all of
!     the comments in BUFFER to the comment area. Either there was
!     enough space already there, or we calculated how many new comment
!     records were needed, and we added them to the file. So, now we
!     begin ``packing'' the comments into DAF comment records and
!     writing them to the file.
!
!     We begin initializing the appropriate variables.
!
      IF ( NCOMC .EQ. 0 ) THEN
!
!        If there are no comments in the comment area, then we need
!        to skip the file record. The first available comment record
!        is the record immediately after the file record, so we set
!        RECNO accordingly. We also initialize the current position in
!        the comment record, and the comment record itself.
!
         RECNO  = 2
         CURPOS = 1
         CRECRD = ' '

      ELSE
!
!        If there are comments in the comment area, then we need to
!        skip the file record and any comment records which have been
!        filled. The first comment record with space available is the
!        record immediately following the last completely filled
!        comment record. So calculate the number of comment records
!        in use, and set RECNO appropriately. Finally calculate the
!        initial position.
!
         RINUSE = 1 + NCOMC / MXCREC
         RECNO  = 1 + RINUSE
         CURPOS = NCOMC - MXCREC * ( RINUSE - 1 ) + 1

      END IF
!
!     Begin ``packing'' the comments from the input buffer into the
!     comment records, writing the comment records to the file as they
!     become filled.
!
      DO I = 1, N
!
!        Get the length of the significant portion of comment line I.
!
         LENGTH = LASTNB ( BUFFER(I) )
!
!        Process the comment line.
!
         DO J = 1, LENGTH
!
!           If we have filled the comment record while processing
!           comment line BUFFER(I), write out the comment record,
!           increment the record number, RECNO, and reset the values
!           of the current position and the comment record.
!
            IF ( CURPOS .GT. MXCREC ) THEN

               WRITE ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD

               IF ( IOSTAT .NE. 0 ) THEN

                  CALL SETMSG ( 'Error writing to record # of the'  // &
                                ' binary file named ''#''. IOSTAT'  // &
                                ' = #.'                              )
                  CALL ERRINT ( '#', RECNO                           )
                  CALL ERRFNM ( '#', DAFLUN                          )
                  CALL ERRINT ( '#', IOSTAT                          )
                  CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
                  CALL CHKOUT ( 'DAFAC'                              )
                  RETURN

               END IF

               RECNO  = RECNO + 1
               CURPOS = 1
               CRECRD = ' '

            END IF

            CRECRD(CURPOS:CURPOS) = BUFFER(I)(J:J)
            CURPOS                = CURPOS + 1

         END DO
!
!        Check to see if we happened to exactly fill the comment record
!        when we finished processing comment line BUFFER(I). If we
!        did, CURPOS will be 1 greater than MXCREC, and we will need
!        to write the comment record to the file, increment the record
!        number, RECNO, and reset the values of the current position
!        and the comment record.
!
         IF ( CURPOS .GT. MXCREC ) THEN

               WRITE ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD

               IF ( IOSTAT .NE. 0 ) THEN

                  CALL SETMSG ( 'Error writing to record # of the'  // &
                                ' binary file named ''#''. IOSTAT'  // &
                                ' = #.'                              )
                  CALL ERRINT ( '#', RECNO                           )
                  CALL ERRFNM ( '#', DAFLUN                          )
                  CALL ERRINT ( '#', IOSTAT                          )
                  CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'             )
                  CALL CHKOUT ( 'DAFAC'                              )
                  RETURN

               END IF

            RECNO  = RECNO + 1
            CURPOS = 1
            CRECRD = ' '

         END IF
!
!        Append the end-of-line marker to the comment line that we just
!        placed into the comment record.
!
         CRECRD(CURPOS:CURPOS) = EOLMRK
         CURPOS                = CURPOS + 1

      END DO
!
!     We have now finished processing all of the comment lines in
!     BUFFER, so we need write the current record to the file. This
!     record will always contain something, so we always need to write
!     it.
!
      IF ( CURPOS .GT. MXCREC ) THEN
!
!        If we have completely filled the comment record, the last
!        character of the last line n the buffer coincides with the
!        last character in the comment record, then we need to write
!        the record and get set up to add the end of comments mark on
!        the next record.
!
         WRITE ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD

         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Error writing to record # of the'    // &
                          ' binary file named ''#''. IOSTAT'    // &
                          ' = #.'                                )
            CALL ERRINT ( '#', RECNO                             )
            CALL ERRFNM ( '#', DAFLUN                            )
            CALL ERRINT ( '#', IOSTAT                            )
            CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
            CALL CHKOUT ( 'DAFAC'                                )
            RETURN

         END IF

         RECNO  = RECNO + 1
         CURPOS = 1
         CRECRD = ' '

      END IF
!
!     Add the end of comments mark to the final comment record and
!     write it to the file.
!
      CRECRD(CURPOS:CURPOS) = EOCMRK

      WRITE ( DAFLUN, REC=RECNO, IOSTAT=IOSTAT ) CRECRD

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'Error writing to record # of the'    // &
                       ' binary file named ''#''. IOSTAT'    // &
                       ' = #.'                                )
         CALL ERRINT ( '#', RECNO                             )
         CALL ERRFNM ( '#', DAFLUN                            )
         CALL ERRINT ( '#', IOSTAT                            )
         CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
         CALL CHKOUT ( 'DAFAC'                                )
         RETURN

      END IF
!
!     Check out and leave DAFAC.
!
      CALL CHKOUT ( 'DAFAC' )
      RETURN
      END
