!$Procedure DASEC  ( DAS extract comments )

      SUBROUTINE DASEC ( HANDLE, BUFSIZ, N, BUFFER, DONE )

! $ Abstract
!
!     Extract comments from the comment area of a binary DAS file.
!
! $ Required_Reading
!
!     DAS
!
! $ Keywords
!
!     FILES
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'das.inc'

      INTEGER               HANDLE
      INTEGER               BUFSIZ
      INTEGER               N
      CHARACTER*(*)         BUFFER(*)
      LOGICAL               DONE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of binary DAS file open with read access.
!     BUFSIZ     I   Maximum size, in lines, of BUFFER.
!     N          O   Number of comments extracted from the DAS file.
!     BUFFER     O   Buffer in which extracted comments are placed.
!     DONE       O   Indicates whether all comments have been extracted.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of a binary DAS file which has been
!              opened with read access.
!
!     BUFSIZ   is the maximum number of comments that may be placed into
!              BUFFER. This would typically be the declared array size
!              for the Fortran character string array passed into this
!              routine.
!
! $ Detailed_Output
!
!     N        is the number of comment lines extracted from the comment
!              area of the binary DAS file attached to HANDLE. This
!              number will be <= BUFSIZ on output. If N = BUFSIZ and
!              DONE <> .TRUE. then there are more comments left to to
!              extract. If N = 0, then DONE = .TRUE., i.e., there were
!              no comments in the comment area. If there are comments
!              in the comment area, or comments remaining after the
!              extraction process has begun, N > 0, always.
!
!     BUFFER   is a list of at most BUFSIZ comments which have been
!              extracted from the comment area of the binary DAS
!              file attached to HANDLE.
!
!     DONE     is a logical flag indicating whether or not all of the
!              comment lines from the comment area of the DAS file have
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
!     2)  If a comment line in a DAS file is longer than the length
!         of a character string array element of BUFFER, the error
!         SPICE(COMMENTTOOLONG) is signaled.
!
!     3)  If there is a mismatch between the number of comment
!         characters found and the number of comment characters
!         expected, the error SPICE(BADDASCOMMENTAREA) is signaled.
!
!     4)  If the binary DAS file attached to HANDLE is not open for
!         reading, an error is signaled by a routine in the call tree of
!         this routine.
!
! $ Files
!
!     See argument HANDLE in $Detailed_Input.
!
! $ Particulars
!
!     Binary DAS files contain an area which is reserved for storing
!     annotations or descriptive textual information describing the data
!     contained in a file. This area is referred to as the ``comment
!     area'' of the file. The comment area of a DAS file is a line
!     oriented medium for storing textual information. The comment
!     area preserves any leading or embedded white space in the line(s)
!     of text which are stored, so that the appearance of the of
!     information will be unchanged when it is retrieved (extracted) at
!     some other time. Trailing blanks, however, are NOT preserved,
!     due to the way that character strings are represented in
!     standard Fortran 77.
!
!     This routine will read the comments from the comment area of
!     a binary DAS file, placing them into a line buffer. If the line
!     buffer is not large enough to hold the entire comment area,
!     the portion read will be returned to the caller, and the DONE
!     flag will be set to .FALSE. This allows the comment area to be
!     read in ``chunks,'' a buffer at a time. After all of the comment
!     lines have been read, the DONE flag will be set to .TRUE.
!
!     This routine can be used to ``simultaneously'' extract comments
!     from the comment areas of multiple binary DAS files. See Example
!     2 in the $Examples section.
!
! $ Examples
!
!     Example 1
!     ---------
!
!     The following example will extract the entire comment area of a
!     binary DAS file attached to HANDLE, displaying the comments on
!     the terminal screen.
!
!     Let
!
!        BUFFER  have the following declaration:
!
!           CHARACTER*(80)  BUFFER(25)
!
!        HANDLE  be the handle of an open binary DAS file.
!
!     then
!
!        BUFSIZ = 25
!        DONE   = .FALSE.
!
!        DO WHILE ( .NOT. DONE )
!
!           CALL DASEC( HANDLE, BUFSIZ, N, BUFFER, DONE )
!
!           DO I = 1, N
!
!              WRITE (*,*) BUFFER(I)
!
!           END DO
!
!        END DO
!
!     Example 2
!     ---------
!
!     The following example demonstrates the use of this routine to
!     simultaneously read the comment areas of multiple DAS files.
!     For each file, the comments will be displayed on the screen as
!     they are extracted.
!
!     Let
!
!        BUFFER  have the following declaration:
!
!           CHARACTER*(80)  BUFFER(25)
!
!        NUMFIL     be the number of binary DAS files that are to have
!                   their comment areas displayed.
!
!        DASNAM(I)  Be a list of filenames for the DAS files which are
!                   to have their comment areas displayed.
!
!        HANDLE(I)  be a list of handles for the DAS files which are
!                   to have their comment areas displayed.
!
!        DONE(I)    be a list of logical flags indicating whether
!                   we are done extracting the comment area from the
!                   DAS file attached to HANDLE(I)
!
!     then
!
!            BUFSIZ = 25
!
!            DO I = 1, NUMFIL
!
!               DONE(I)   = .FALSE.
!               HANDLE(I) = 0
!
!            END DO
!     C
!     C      Open the DAS files.
!     C
!            DO I = 1, NUMFIL
!
!               CALL DASOPR ( DASNAM(I), HANDLE(I) )
!
!            END DO
!     C
!     C      While there are still some comments left to read in at
!     C      least one of the files, read them and display them.
!     C
!            DO WHILE ( .NOT. ALLTRU( DONE, NUMFIL ) )
!
!               DO I = 1, NUMFIL
!
!                  IF ( .NOT. DONE(I) ) THEN
!
!                     WRITE (*,*)
!                     WRITE (*,*) 'File: ', DASNAM(I)(:RTRIM(DASNAM(I)))
!                     WRITE (*,*)
!                     N = 0
!
!                     CALL DASEC ( HANDLE(I),
!           .                      BUFSIZ,
!           .                      N,
!           .                      BUFFER,
!           .                      DONE(I) )
!
!                     DO J = 1, N
!
!                        WRITE (*,*) BUFFER(J)(:RTRIM(BUFFER(J)))
!
!                     END DO
!
!                  END IF
!
!               END DO
!
!            END DO
!
! $ Restrictions
!
!     1)  The comment area may consist only of printing ASCII
!         characters, decimal values 32 - 126.
!
!     2)  There is NO maximum length imposed on the significant portion
!         of a text line that may be placed into the comment area of a
!         DAS file. The maximum length of a line stored in the comment
!         area should be kept reasonable, so that they may be easily
!         extracted. A good value for this would be 255 characters, as
!         this can easily accommodate "screen width" lines as well as
!         long lines which may contain some other form of information.
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.4.1, 20-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Removed
!        reference to non-described parameters from entry #1 in
!        $Restrictions section.
!
!-    SPICELIB Version 1.4.0, 10-FEB-2017 (NJB)
!
!        Updated to use ZZDDHHLU.
!
!        Now imports parameter FTSIZE from das.inc.
!
!-    SPICELIB Version 1.3.0, 18-JUN-1999 (WLT)
!
!        Changed name used in CHKOUT to be consistent with the CHKIN
!        value.
!
!-    SPICELIB Version 1.2.0, 04-AUG-1994 (KRG)
!
!        Rearranged some of the code to avoid always reading the file
!        record. Now we look for the input HANDLE in the file table
!        first, and only read the file record if we do not find it. Also
!        added a new array to be saved: FILCNT. This is the number of
!        comment characters in a file; we save it now rather than
!        reading it every time.
!
!        Fixed a bug. If the Fortran character string array elements
!        have exactly the same length as a comment in the comment area,
!        this routine would halt rather unexpectedly from a memory over
!        run.
!
!-    SPICELIB Version 1.1.0, 22-NOV-1993 (KRG)
!
!        Changed the value of the parameter FTSIZE from 20 to 21. This
!        change makes the value of FTSIZE in DASEC compatible with the
!        value in DASFM. See DASFM for a discussion of the reasons for
!        the increase in the value.
!
!-    SPICELIB Version 1.0.0, 23-NOV-1992 (KRG)
!
!-&


! $ Index_Entries
!
!     extract comments from a DAS file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.2.0, 04-AUG-1994 (KRG)
!
!        Rearranged some of the code to avoid always reading the file
!        record. Now we look for the input HANDLE in the file table
!        first, and only read the file record if we do not find it. Also
!        added a new array to be saved: FILCNT. This is the number of
!        comment characters in a file; we save it now rather than
!        reading it every time.
!
!        Fixed a bug. If the Fortran character string array elements
!        have exactly the same length as a comment in the comment area,
!        this routine would halt rather unexpectedly from a memory over
!        run. This occurred when attempting to clear, i.e., blank pad,
!        the portion of a character string element that extended beyond
!        the text in a comment line. A test has been added to verify
!        that blank padding can be performed.
!
!-    SPICELIB Version 1.1.0, 22-NOV-1993 (KRG)
!
!        Changed the value of the parameter FTSIZE from 20 to 21. This
!        change makes the value of FTSIZE in DASEC compatible with the
!        value in DASFM. See DASFM for a discussion of the reasons for
!        the increase in the value.
!
!-    SPICELIB Version 1.0.0, 23-NOV-1992 (KRG)
!
!-&


!
!     SPICELIB functions
!
      INTEGER               ISRCHI

      LOGICAL               FAILED
      LOGICAL               RETURN
!
!     Local parameters
!

!
!     Length of a DAS character record, in characters.
!
      INTEGER               MXCREC
      PARAMETER           ( MXCREC = 1024 )
!
!     Maximum and minimum decimal values for the printable ASCII
!     characters.
!
      INTEGER               MAXPCH
      PARAMETER           ( MAXPCH = 126 )

      INTEGER               MINPCH
      PARAMETER           ( MINPCH = 32 )
!
!     Decimal value for the DAS comment area end-of-line (EOL) marker.
!
      INTEGER               INTEOL
      PARAMETER           ( INTEOL = 0 )
!
!     Maximum length of a filename.
!
      INTEGER               FNMLEN
      PARAMETER           ( FNMLEN = 128 )
!
!     Length of a DAS file ID word.
!
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )
!
!     Length of a DAS file internal filename.
!
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
!
!     Local variables
!
      CHARACTER*(1)         CH
      CHARACTER*(MXCREC)    CRECRD
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME

      INTEGER               CURPOS
      INTEGER               DASLUN
      INTEGER               I
      INTEGER               INDEX
      INTEGER               J
      INTEGER               K
      INTEGER               LINLEN
      INTEGER               NCHARS
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NUMCOM
      INTEGER               RECNO
!
!     The file table declarations for keeping track of which files
!     are currently in the process of having comments extracted.
!
      INTEGER               FILCNT(FTSIZE)
      INTEGER               FILCHR(FTSIZE)
      INTEGER               FILHAN(FTSIZE)
      INTEGER               LSTHAN
      INTEGER               LSTPOS(FTSIZE)
      INTEGER               LSTREC(FTSIZE)
      INTEGER               NFILES

      LOGICAL               EOL
      LOGICAL               FIRST
!
!     Saved variables
!
      SAVE CRECRD
!
!     Save all of the file table information.
!
      SAVE FILCNT
      SAVE FILCHR
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
         CALL CHKIN ( 'DASEC' )
      END IF
!
!     If this is the first time that this routine has been called,
!     we need to initialize the character value of the end-of-line
!     marker, and the file table variables.
!
      IF ( FIRST ) THEN

         FIRST  = .FALSE.
         NFILES = 0
         LSTHAN = -1

         DO I = 1, FTSIZE

            FILCNT(I) = 0
            FILCHR(I) = 0
            FILHAN(I) = 0
            LSTREC(I) = 0
            LSTPOS(I) = 0

         END DO

      END IF
!
!     Verify that the DAS file attached to HANDLE is opened for reading
!     by calling the routine to signal an invalid access mode on a
!     handle.
!
      CALL DASSIH ( HANDLE, 'READ' )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DASEC' )
         RETURN

      END IF
!
!     Check for a nonpositive BUFFER size.
!
      IF ( BUFSIZ .LE. 0 ) THEN

         CALL SETMSG ( 'The output buffer size was not positive: #.' )
         CALL ERRINT ( '#', BUFSIZ                                   )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                      )
         CALL CHKOUT ( 'DASEC'                                       )
         RETURN

      END IF
!
!     Convert the DAS file handle to its corresponding Fortran logical
!     unit number for reading the comment records.
!
      CALL ZZDDHHLU ( HANDLE, 'DAS', .FALSE., DASLUN )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DASEC' )
         RETURN

      END IF
!
!     Get the length of a single character string in the buffer.
!
      LINLEN = LEN ( BUFFER(1) )
!
!     If we have extracted comments from at least one file and we
!     didn't finish, get the index for that file in the file table.
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
!        the file record and any reserved records that are in the file.
!        The first comment record immediately follows the last reserved
!        record.
!
!        Get the current number of comment records and comment
!        characters from the DAS file attached to HANDLE. We will also
!        get back some extra stuff that we do not use.
!
         CALL DASRFR ( HANDLE, &
                       IDWORD, IFNAME, &
                       NRESVR, NRESVC, &
                       NCOMR,  NCOMC   )

         IF ( FAILED() ) THEN

            CALL CHKOUT ( 'DASEC' )
            RETURN

         END IF
!
!        If the number of comment characters, NCOMC, is equal to zero,
!        then we have no comments to read, so set the number of comments
!        to zero, set DONE to .TRUE., check out,  and return.
!
         IF ( NCOMC .EQ. 0 ) THEN

            N    = 0
            DONE = .TRUE.
            CALL CHKOUT ( 'DASEC' )
            RETURN

         END IF

         RECNO  = 1 + NRESVR + 1
         CURPOS = 1
         NCHARS = 0

      END IF
!
!     Begin reading the comment area into the buffer.
!
      IF ( HANDLE .NE. LSTHAN ) THEN
!
!        If the current DAS handle is not the same as the handle on
!        the last call, then we need to read in the appropriate record
!        from the DAS file comment area. Otherwise the record was saved,
!        so we don't need to read it in.
!
         CALL DASIOC ( 'READ', DASLUN, RECNO, CRECRD )

      END IF
!
!     Initialize the BUFFER line counter, I, and the line position
!     counter, J.
!
      I = 1
      J = 1

      DONE = .FALSE.
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
                                ' a comment line with length #.'      )
                  CALL ERRINT ( '#', LINLEN                           )
                  CALL ERRINT ( '#', I                                )
                  CALL SIGERR ( 'SPICE(COMMENTTOOLONG)'               )
                  CALL CHKOUT ( 'DASEC'                               )
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
               CALL DASIOC ( 'READ', DASLUN, RECNO, CRECRD )
               CURPOS = 1

            ELSE

               CURPOS = CURPOS + 1

            END IF
!
!           Check to make sure that it is safe to continue, i.e.,
!           that the number of comment characters we have processed
!           has not exceeded the number of comment characters in the
!           comment area of the DAS file.
!
            IF ( NCHARS .GT. NCOMC ) THEN

               CALL SETMSG ( 'Count of comment characters (#)'     // &
                             ' exceeds the number of comment'      // &
                             ' characters (#) in the DAS file #.'   )
               CALL ERRINT ( '#',NCHARS                             )
               CALL ERRINT ( '#',NCOMC                              )
               CALL ERRFNM ( '#',DASLUN                             )
               CALL SIGERR ( 'SPICE(BADDASCOMMENTAREA)'             )
               CALL CHKOUT ( 'DASEC'                                )
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
            DONE   = .TRUE.
            LSTHAN = -1
!
!           0 <= INDEX <= NFILES, and we only want to remove things
!           from the file table if:
!
!              1) There are files in the file table, NFILES > 0
!              2) The file we are currently reading from is in the
!                 file table, INDEX > 0.
!
!           So, if INDEX > 0, we know that there are files in the file
!           table, and that we are currently reading from one of them.
!
            IF ( INDEX .GT. 0 ) THEN

               DO K = INDEX, NFILES - 1

                  FILCNT(K) = FILCNT(K+1)
                  FILCHR(K) = FILCHR(K+1)
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
               CALL CHKOUT ( 'DASEC'                                  )
               RETURN

            END IF

            NFILES         = NFILES + 1
            FILCNT(NFILES) = NCOMC
            FILCHR(NFILES) = NCHARS
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

      CALL CHKOUT ( 'DASEC' )
      RETURN
      END
