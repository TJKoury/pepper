
! $ Procedure CONVTB ( Convert kernel file from text to binary )

      SUBROUTINE CONVTB ( TXTFIL, BINFIL )
      use pepper_legacy

! $ Abstract
!
!     Convert a SPICE text file into its equivalent binary format.
!
!     NOTE: This routine is currently for use ONLY with the SPACIT
!           utility program. Use it at your own risk.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      CHARACTER*(*)         TXTFIL
      CHARACTER*(*)         BINFIL

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TXTFIL     I   Name of text file to be converted.
!     BINARY     I   Name of a binary file to be created.
!
! $ Detailed_Input
!
!     None.
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
!     1)  This routine uses a Fortran scratch file to temporarily
!         store the lines of comments if there are any.
!
! $ Exceptions
!
!     1) If there is a problem opening or writing to the binary
!        file, a routine that CONVTB calls diagnoses and signals
!        an error.
!
!     2) If there is a problem reading from the text file, the
!        error SPICE(FILEREADFAILED) is signalled.
!
!     3) If there is a problem opening the scratch file, the error
!        SPICE(FILEOPENERROR) is signalled.
!
!     4) If there is a problem writing to the scratch file, the
!        error SPICE(FILEWRITEFAILED) is signalled.
!
!     5) If the binary file archictecture is not recognized, the error
!        SPICE(UNSUPPBINARYARCH) will be signalled.
!
!     7) If the transfer file format is not recognized, the error
!        SPICE(NOTATRANSFERFILE) will be signalled.
!
!     8) If the input file format cannot be identified, the error
!        SPICE(UNRECOGNIZABLEFILE) will be signalled..
!
! $ Particulars
!
!     This routine is currently only for use with the SPACIT program.
!
! $ Examples
!
!
!
! $ Restrictions
!
!     1)  This routine assumes that the data and comments in the
!         text format SPK, PCK or CK file come from a binary file
!         and were written by one of the SPICELIB binary to text
!         conversion routines. Data and/or comments written any
!         other way may not be in the correct format and, therefore,
!         may not be handled properly.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     K.R. Gehringer  (JPL)
!
! $ Version
!
!-    Beta Version 3.2.0, 12-AUG-1994 (KRG)
!
!        Added a new exception SPICE(UNRECOGNIZABLEFILE). This occurs
!        when the file that is to be converted cannot be adequately
!        identified to give a meaningful error message or proceed with
!        the conversion.
!
!        Fixed (I hope) a bug when dealing with LARGE binary files that
!        were accidentally passed to this reoutine as the text/transfer
!        file to be converted. The routine would hang in a formatted
!        READ while searching for the next "record" inthe binary file.
!        Not Good.
!
!-    Beta Version 3.1.0, 12-AUG-1994 (KRG)
!
!        Added a few more tests for incorrect files on input.
!
!        Fixed a minor bug that would occur when formatting a long error
!        message. ERRFNM was called with a logical unit that had already
!        been closed.
!
!        ERRFNM was also called with the logical unit of a SCRATCH file.
!        Fortran 77 does not allow names to be associated with SCRATCH
!        files, so this was removed.
!
!        Replaced the exception SPICE(UNSUPPTEXTFORMAT) with the
!        exception: SPICE(NOTATRANSFERFILE).
!
!-    Beta Version 3.0.0, 22-APR-1994 (KRG)
!
!        Made updates to the routine to make use of the new SPICE
!        capability of determining binary kernel file types at run time.
!
!        Removed the arguments for the file architecture and file type
!        from the calling list. This information was no longer
!        necessary.
!
!        Rearranged some of the code to make it easier to understand.
!
!        When checking for comments in a DAF text file the program now
!        backspaces if the forst record read after the data portion of
!        the file has been converted does not indicate the existence
!        of comments in the text file. This repositions the input
!        pointer so that the next read will return the record that we
!        read to test for the existence of comments. THIS ACTION IS
!        DIFFERENT FROM THE BEHAVIOR OF PREVIOUS VERSIONS OF THIS
!        ROUTINE WHICH DID NOT PERFORM THE BACKSPACE. See the comments
!        in the code near the end of this file for the details.
!
!-    Beta Version 2.0.0, 28-JAN-1994 (KRG)
!
!-&

! $ Index_Entries
!
!     convert text SPICE files to binary
!
!-&

!
!     SPICELIB functions
!

!
!     Local parameters
!
!     Begin and end markers in the file for the comment area.
!
      CHARACTER*(*)         BCMARK
      PARAMETER           ( BCMARK ='~NAIF/SPC BEGIN COMMENTS~')

      CHARACTER*(*)         ECMARK
      PARAMETER           ( ECMARK ='~NAIF/SPC END COMMENTS~'  )
!
!     Maximum length of an input text line.
!
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255 )
!
!     Maximum length of a file architecture.
!
      INTEGER               ARCHLN
      PARAMETER           ( ARCHLN = 3 )
!
!     Maximum length of a file type.
!
      INTEGER               TYPELN
      PARAMETER           ( TYPELN = 4 )
!
!     Number of reserved records to use when creating a binar DAF file.
!
      INTEGER               RESREC
      PARAMETER           ( RESREC = 0 )
!
!     Local variables
!
      CHARACTER*(ARCHLN)    ARCH
      CHARACTER*(LINLEN)    LINE
      CHARACTER*(TYPELN)    TYPE

      INTEGER               HANDLE
      INTEGER               IOSTAT
      INTEGER               SCRLUN
      INTEGER               TXTLUN

      LOGICAL               HAVCOM
      LOGICAL               EOC
!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CONVTB' )
      END IF
!
!     Get the architecture and type of the file to be converted.
!
      CALL GETFAT ( TXTFIL, ARCH, TYPE )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'CONVTB' )
         RETURN

      END IF
!
!     Verify the architecture and type of the file, and perform any
!     processing necessary..
!
      IF ( ( ARCH .EQ. 'XFR' ) .OR. ( ARCH .EQ. 'DEC' ) )THEN
!
!        Open the text file that is to be converted to binary.
!
         CALL TXTOPR( TXTFIL, TXTLUN )

         IF ( FAILED() ) THEN

            CALL CHKOUT ( 'CONVTB' )
            RETURN

         END IF
!
!        Read the information line to skip it. We already know the
!        architecture and type of the file.
!
         READ (TXTLUN,FMT='(A)',IOSTAT=IOSTAT) LINE

         IF ( IOSTAT .NE. 0 ) THEN
!
!           If there was an error then we need to close the text
!           file, and then check out and return to the caller.
!
            CLOSE       ( TXTLUN                                 )
            CALL SETMSG ( 'Error reading the text file: #.'     // &
                          ' IOSTAT =  #.'                        )
            CALL ERRCH  ( '#', TXTFIL                            )
            CALL ERRINT ( '#', IOSTAT                            )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                )
            CALL CHKOUT ( 'CONVTB'                               )
            RETURN

         END IF

      END IF
!
!     Process the file based on the derived architecture and type.
!
       IF ( ( ARCH .EQ. 'XFR' ) .AND. ( TYPE .EQ. 'DAF' ) ) THEN
!
!        We got a DAF file.
!
!        Convert the data portion of the text file to binary. At this
!        point, we know that we have a current DAF text file format.
!
!        We expect to have comments.
!
         HAVCOM = .TRUE.
!
!        Convert it.
!
         CALL DAFTB ( TXTLUN, BINFIL )

         IF ( FAILED() ) THEN
!
!           If there was an error then we need to close the
!           text file, and then check out and return to the
!           caller.
!
            CLOSE       ( TXTLUN   )
            CALL CHKOUT ( 'CONVTB' )
            RETURN

         END IF

      ELSE IF ( ( ARCH .EQ. 'XFR' ) .AND. ( TYPE .EQ. 'DAS' ) ) THEN
!
!        We got a DAS file. So we should begin converting it to binary.
!        DAS files are easier: all we do is call one routine.
!
!        We do not have comments. Actually, we might but they are
!        included as part of the DAS file conversion process.
!
         HAVCOM = .FALSE.
!
!        Convert it.
!
         CALL DASTB ( TXTLUN, BINFIL )

         IF ( FAILED() ) THEN
!
!           If there was an error then we need to close the
!           text file, and then check out and return to the
!           caller.
!
            CLOSE       ( TXTLUN   )
            CALL CHKOUT ( 'CONVTB' )
            RETURN

         END IF

      ELSE IF ( ARCH .EQ. 'DAS' ) THEN
!
!        This is an error case, most likely caused by reading a binary
!        DAS file by accident. So signal an appropriate error.
!
         CALL SETMSG ( 'The file ''#'' appears to be a binary' // &
                       ' DAS file and not a transfer file.'     )
         CALL ERRCH  ( '#', TXTFIL                              )
         CALL SIGERR ( 'SPICE(NOTATRANSFERFILE)'                )
         CALL CHKOUT ( 'CONVTB'                                 )
         RETURN

      ELSE IF ( ( ARCH .EQ. 'DAS' ) .AND. ( TYPE .EQ. 'PRE' ) ) THEN
!
!        This is an error case, most likely caused by reading a binary
!        DAS file by accident. So signal an appropriate error.
!
         CLOSE       ( TXTLUN                                  )
         CALL SETMSG ( 'The file ''#'' appears to be a'       // &
                       ' pre-release binary DAS file and not' // &
                       ' a transfer file.'                     )
         CALL ERRCH  ( '#', TXTFIL                             )
         CALL SIGERR ( 'SPICE(NOTATRANSFERFILE)'               )
         CALL CHKOUT ( 'CONVTB'                                )
         RETURN

      ELSE IF ( ARCH .EQ. 'DAF' ) THEN
!
!        This is an error case, most likely caused by reading a binary
!        DAF file by accident. So signal an appropriate error.
!
         CALL SETMSG ( 'The file ''#'' appears to be a binary' // &
                       ' DAF file and not a transfer file.'     )
         CALL ERRCH  ( '#', TXTFIL                              )
         CALL SIGERR ( 'SPICE(NOTATRANSFERFILE)'                )
         CALL CHKOUT ( 'CONVTB'                                 )
         RETURN

      ELSE IF ( ( ARCH .EQ. 'DEC') .AND. ( TYPE .EQ. 'DAF' ) ) THEN
!
!        This is the case for the old text file format. It has no
!        identifying marks whatsoever, so we simply have to try and
!        convert it.
!
!        We expect to have comments.
!
         HAVCOM = .TRUE.
!
!        Back up one record so that we are positioned in the file where
!        we were when this routine was entered.
!
         BACKSPACE   ( TXTLUN )
!
!        Convert it.
!
         CALL DAFT2B ( TXTLUN, BINFIL, RESREC )

         IF ( FAILED() ) THEN
!
!           If there was an error then we need to close the text
!           file, and then check out and return to the caller.
!
            CLOSE       ( TXTLUN   )
            CALL CHKOUT ( 'CONVTB' )
            RETURN

         END IF

      ELSE
!
!        This is the catch all error case. At this point, we didn't
!        match any of the files whose architecture and types are
!        recognized. So, we toss our hands in the air and signal an
!        error.
!
         CALL SETMSG ( 'The architecture and type of the file'  // &
                       ' ''#''could not be determined.'          )
         CALL ERRCH  ( '#', TXTFIL                               )
         CALL SIGERR ( 'SPICE(UNRECOGNIZABLEFILE)'               )
         CALL CHKOUT ( 'CONVTB'                                  )
         RETURN

      END IF
!
!     If we have comments to process, then process them.
!
      IF ( HAVCOM ) THEN
!
!        There are three situations that we need to consider here:
!
!           1) We have a SPICE text file with comments. This implies
!              that we have a bunch of comments to be put into the
!              comment area that are surrounded by the begin comments
!              marker, BCMARK, and the end comemnts marker, ECMARK.
!
!           2) We are at the end of the file. This means that we have
!              an old SPICE kernel file, from the good old days before
!              the comment area was implemented, or we ahve a plain old
!              ordinary DAF file.
!
!           3) We are not at the end of the file, but there are no
!              comments. This means that a text DAF file may be embedded
!              in a larger text file or something. PDS does things like
!              this: SFDUs and such.
!
!        So, we need to look out for and deal with each of these
!        possibilities.
!
         READ ( TXTLUN, FMT='(A)', IOSTAT=IOSTAT ) LINE

         IF ( IOSTAT .GT. 0 ) THEN
!
!           If there was an error then we need to close the text
!           file, and then check out and return to the caller.
!
            CLOSE       ( TXTLUN                             )
            CALL SETMSG ( 'Error reading the text file: #.' // &
                          ' IOSTAT = #.'                     )
            CALL ERRCH  ( '#', TXTFIL                        )
            CALL ERRINT ( '#', IOSTAT                        )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'            )
            CALL CHKOUT ( 'CONVTB'                           )
            RETURN

         END IF
!
!        If we encountered the end of the file, just check out and
!        return. This is not an error.
!
         IF ( IOSTAT .LT.  0 ) THEN

            CALL CHKOUT ( 'CONVTB' )
            RETURN

         END IF
!
!        We got a line, so left justify it and see if it matches the
!        begin comments marker. If not, then use the Fortran BACKSPACE
!        command to reposition the file pointer to be ready to read the
!        line we just read.
!
         IF ( LINE ( LTRIM(LINE): ) .NE. BCMARK ) THEN

            BACKSPACE   ( TXTLUN   )
            CALL CHKOUT ( 'CONVTB' )
            RETURN

         END IF
!
!        We're not at the end of the file, and the line we read
!        is BCMARK, so we write the comments to a scratch file.
!        We do this because we have to use SPCAC to add the comments
!        to the comment area of the binary file, and SPCAC rewinds
!        the file. It's okay for SPCAC to rewind a scratch file, because
!        it will probably not be very big, but it's not okay to rewind
!        the file connected to TXTLUN -- we don't know the initial
!        location of the file pointer or how big the file is.
!
         CALL GETLUN ( SCRLUN )
         OPEN ( UNIT            =  SCRLUN, &
                FORM            = 'FORMATTED', &
                ACCESS          = 'SEQUENTIAL', &
                STATUS          = 'SCRATCH', &
                IOSTAT          =  IOSTAT      )

         IF ( IOSTAT .NE. 0 ) THEN
!
!           If there was an error then we need to close the text
!           file, and then check out and return to the caller.
!
            CLOSE       ( SCRLUN                               )
            CLOSE       ( TXTLUN                               )
            CALL SETMSG ( 'Error opening temporary file.'     // &
                          ' IOSTAT = #.'                       )
            CALL ERRINT ( '#', IOSTAT                          )
            CALL SIGERR ( 'SPICE(FILEOPENERROR)'               )
            CALL CHKOUT ( 'CONVTB'                             )
            RETURN

         END IF
!
!        Continue reading lines from the text file and storing them
!        in the scratch file until we get to the end marker. We do not
!        write the begin and end markers to the scratch file. We do not
!        need them.
!
         EOC = .FALSE.

         DO WHILE ( .NOT. EOC )

            READ ( TXTLUN, FMT='(A)', IOSTAT=IOSTAT ) LINE

            IF ( IOSTAT .NE. 0 ) THEN
!
!              If there was an error then we need to close the
!              scratch file, the text file, and then check out
!              and return to the caller.
!
               CLOSE       ( SCRLUN                              )
               CLOSE       ( TXTLUN                              )
               CALL SETMSG ( 'Error reading the text file: #.'  // &
                             ' IOSTAT = #.'                      )
               CALL ERRCH  ( '#', TXTFIL                         )
               CALL ERRINT ( '#', IOSTAT                         )
               CALL SIGERR ( 'SPICE(FILEREADFAILED)'             )
               CALL CHKOUT ( 'CONVTB'                            )
               RETURN

            END IF
!
!           If we are not at the end of the comments, then write the
!           line ot the scratch file. Otherwise set the end of comments
!           flag to .TRUE..
!
            IF ( LINE ( LTRIM(LINE): ) .NE. ECMARK ) THEN

               WRITE ( SCRLUN, FMT='(A)', IOSTAT=IOSTAT ) &
                                                 LINE(1:RTRIM(LINE))

               IF ( IOSTAT .NE. 0 ) THEN
!
!                 If there was an error then we need to close the
!                 scratch file, the text file, and then check out
!                 and return to the caller.
!
                  CLOSE       ( SCRLUN                                 )
                  CLOSE       ( TXTLUN                                 )
                  CALL SETMSG ( 'Error writing to temporary file.'    // &
                                ' IOSTAT = #.'                         )
                  CALL ERRINT ( '#', IOSTAT                            )
                  CALL SIGERR ( 'SPICE(FILEWRITEFAILED)'               )
                  CALL CHKOUT ( 'CONVTB'                               )
                  RETURN

               END IF

            ELSE

               EOC = .TRUE.

            END IF

         END DO
!
!        Open the new binary file and add the comments that have been
!        stored temporarily in a scratch file.
!
         CALL DAFOPW ( BINFIL, HANDLE )

         IF ( FAILED() ) THEN
!
!           If there was an error then we need to close the scratch
!           file and the text file, and then check out and return to
!           the caller.
!
            CLOSE       ( SCRLUN   )
            CLOSE       ( TXTLUN   )
            CALL CHKOUT ( 'CONVTB' )
            RETURN

         END IF

         CALL SPCAC  ( HANDLE, SCRLUN, ' ', ' ' )

         IF ( FAILED() ) THEN
!
!           If there was an error then we need to close the scratch
!           file and the text file, and then check out and return to
!           the caller.
!
            CLOSE       ( SCRLUN   )
            CLOSE       ( TXTLUN   )
            CALL DAFCLS ( HANDLE   )
            CALL CHKOUT ( 'CONVTB' )
            RETURN

         END IF
!
!        We succeeded, so close the files we opened to deal with the
!        comments. The scratch file is automatically deleted.
!
         CLOSE       ( SCRLUN )
         CALL DAFCLS ( HANDLE )

      END IF
!
!     Close the transfer file. We know it is open, because we got here.
!
      CLOSE       ( TXTLUN )

      CALL CHKOUT ( 'CONVTB' )
      RETURN
      END
