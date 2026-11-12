!$Procedure DAFACU ( DAF add comments from a logical unit )

      SUBROUTINE DAFACU ( COMLUN, BEGMRK, ENDMRK, INSBLN, HANDLE )
      use pepper_legacy

! $ Abstract
!
!     Add comments to an open binary DAF from an opened text file
!     attached to a Fortran logical unit.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations

      INTEGER               COMLUN
      CHARACTER*(*)         BEGMRK
      CHARACTER*(*)         ENDMRK
      LOGICAL               INSBLN
      INTEGER               HANDLE

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!      COMLUN    I   Logical unit of the open comment text file.
!      BEGMRK    I   The begin comments marker to be used.
!      ENDMRK    I   The end comments marker to be used.
!      INSBLN    I   A flag indicating whether to insert a blank line.
!      HANDLE    I   Handle of a DAF opened with write access.
!
! $ Detailed_Input
!
!     COMLUN   The Fortran logical unit of a previously opened text
!              file which contains comments that are to be added to
!              the comment area of a binary DAF.
!
!     BEGMRK   A marker which identifies the beginning of the comments
!              in the comment text file. This marker must appear on a
!              line by itself and leading and trailing blanks are not
!              significant. The marker is case sensitive.
!
!              The line immediately following this marker is the first
!              comment line to be placed into the comment area of the
!              binary DAF.
!
!              If the begin marker is blank, BEGMRK .EQ. ' ', then the
!              comments are assumed to start at the current location
!              in the comment text file.
!
!     ENDMRK   A marker which identifies the end of the comments in the
!              comment text file. This marker must appear on a line by
!              itself and leading and trailing blanks are not
!              significant. The marker is case sensitive.
!
!              The line immediately preceeding this marker is the last
!              comment line to be placed into the comment area of the
!              binary DAF file.
!
!              If the end marker is blank, ENDMRK .EQ. ' ', then the
!              comments are assumed to stop at the end of the comment
!              text file.
!
!     INSBLN   A logical flag which indicates whether a blank line is
!              to be inserted into the comment area of the binary DAF
!              attached to HANDLE before any comments are added to the
!              comment area of the file. This is to provide a simple
!              mechanism for separating any comments already contained
!              in the comment area of a DAF from those comments that
!              are being added.
!
!              If the comment area of a binary DAF is empty, the value
!              of this flag is not significant, the comments are simply
!              be placed into the comment area.
!
!     HANDLE   The file handle for a binary DAF file that has been
!              opened with write access. The comments from the text
!              file are placed into the comment area of this file.
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
!     1)   If the input logical unit COMLUN is not positive or there
!          is not an opened file attached to it, the error
!          SPICE(INVALIDARGUMENT) will be signalled.
!
!     2)   If the INQUIRE on the logical unit to see if there is a
!          file attached fails, the error SPICE(INQUIREFAILED) will
!          be signalled.
!
!     3)   If the scratch file for temporarily holding the comments
!          culled from the text file cannot be opened, then the
!          error SPICE(FILEOPENFAILED) will be signalled.
!
!     4)   If a nonprinting ASCII character is encountered in the
!          comments, the error SPICE(ILLEGALCHARACTER) will be
!          signalled.
!
!     5)   If the begin marker cannot be found in the text file, the
!          error SPICE(MARKERNOTFOUND) will be signalled.
!
!     6)   If the end marker cannot be found in the text file, the
!          error SPICE(MARKERNOTFOUND) will be signalled.
!
! $ Files
!
!     1)   See parameters COMLUN and HANDLE in the $ Detailed_Inputs
!          section.
!
!     2)   A scratch file is used to temporarily hold the comments
!          culled from the comment text file. This is so we do not
!          have to find the place where we started searching for
!          comments in the text file.
!
! $ Particulars
!
!     This routine will place all lines between two specified markers,
!     a `begin comments marker' and an `end comments marker,' in a
!     text file into the comment area of a binary DAF attached to
!     HANDLE. If the `begin comments marker' is blank, then the
!     comments are asumed to start at the current location in the
!     comment text file attached to COMLUN. If the `end comments
!     marker' is blank, then the comments are assumed to stop at the
!     end of the comment text file attached to COMLUN.
!
! $ Examples
!
!     We will be using the files `jabber.txt', 'batty.txt', and
!     `wndrland.daf' in the examples which follow.
!
!     `wndrland.daf' is a binary DAF file with an empty comment area
!                    into which we are going to place the entire file
!                    `jabber.txt' and a selected portion of the file
!                    `batty.txt'.
!
!     `jabber.txt'   is a text file that is to be placed into the
!                    comment area of the binary DAF file `wndrland.daf'.
!
!     `batty.txt'    is a text file from which will have a selected
!                    portion of its text placed into the comment area
!                    of the binary DAF file `wndrland.daf'.
!
!     Let -BOF- and -EOF- denote the beginning and end of a file,
!     respectively.
!
!     The file `jabber.txt' contains:
!
!        -BOF-
!                  The Jabberwock
!
!        'Twas brillig, and the slithy toves
!              Did gyre and gimble in the wabe;
!        All mimsy were the borogoves,
!              And the mome raths outgrabe.
!
!        ``Beware the Jabberwock, my son!
!              The jaws that bite, the claws that catch!''
!
!        And as in uffish thought he stood,
!              The Jabberwock, with eyes of flame,
!        Came whiffling through the tulgey wood,
!              And burbled as it came!
!
!        One, two! One, two! And through and through
!              The vorpal blade went snicker-snack!
!        He left it dead, and with its head
!              He went galumphing back.
!
!        ``And hast thou slain the Jabberwock?
!              Come to my arms, my beamish boy!
!        O frabjous day! Callooh! Callay!''
!              He chortled in his joy.
!
!               Through the Looking-Glass
!               Lewis Carroll
!        -EOF-
!
!     The file `batty.txt' contains:
!
!        -BOF-
!        This file contains a brief poem about bats.
!
!        BEGIN bat poem
!        Twinkle, twinkle, little bat!
!        How I wonder what you're at!
!        Up above the world you fly!
!        Like a teatray in the sky.
!
!               Alice's Adventures in Wonderland
!               Lewis Carroll
!        END bat poem
!
!        And that's that for bats.
!        -EOF-
!
!     Let
!
!           JABLUN   be the logical unit for the file `jabber.txt'
!           BATLUN   be the logical unit for the file `batty.txt'
!     and
!           HANDLE   be the DAF handle for the file `wndrland.daf'
!
!     The code fragment
!
!     C
!     C      Open the files.
!     C
!            CALL DAFOPW ( `wndrland.daf', HANDLE )
!            CALL TXTOPR ( `jabber.txt'  , JABLUN )
!            CALL TXTOPR ( `batty.txt'   , BATLUN )
!     C
!     C      Initialize the markers for the file `jabber.txt'. We want
!     C      to include the entire file, so both markers are blank.
!     C
!            BEGMRK = ' '
!            ENDMRK = ' '
!            INSBLN = .TRUE.
!     C
!     C      Add the comments from the file 'jabber.txt'
!     C
!            CALL DAFACU ( JABLUN, BEGMRK, ENDMRK, INSBLN, HANDLE )
!     C
!     C      Initialize the markers for the file `batty.txt'. We want
!     C      to include the bat poem only, so we define the begin and
!     C      end markere accordingly.
!     C
!            BEGMRK = 'BEGIN bat poem'
!            ENDMRK = 'END bat poem'
!            INSBLN = .TRUE.
!     C
!     C      Add the comments from the file 'batty.txt'
!     C
!            CALL DAFACU ( BATLUN, BEGMRK, ENDMRK, INSBLN, HANDLE )
!     C
!     C      Close the files.
!
!            CLOSE       ( JABLUN )
!            CLOSE       ( BATLUN )
!            CALL DAFCLS ( HANDLE )
!
!     will create a comment area in `wndrland.daf' which contains:
!
!        -BOC-
!                  The Jabberwock
!
!        'Twas brillig, and the slithy toves
!              Did gyre and gimble in the wabe;
!        All mimsy were the borogoves,
!              And the mome raths outgrabe.
!
!        ``Beware the Jabberwock, my son!
!              The jaws that bite, the claws that catch!''
!
!        And as in uffish thought he stood,
!              The Jabberwock, with eyes of flame,
!        Came whiffling through the tulgey wood,
!              And burbled as it came!
!
!        One, two! One, two! And through and through
!              The vorpal blade went snicker-snack!
!        He left it dead, and with its head
!              He went galumphing back.
!
!        ``And hast thou slain the Jabberwock?
!              Come to my arms, my beamish boy!
!        O frabjous day! Callooh! Callay!''
!              He chortled in his joy.
!
!               Through the Looking-Glass
!               Lewis Carroll
!
!        Twinkle, twinkle, little bat!
!        How I wonder what you're at!
!        Up above the world you fly!
!        Like a teatray in the sky.
!
!               Alice's Adventures in Wonderland
!               Lewis Carroll
!        -EOC-
!
!     where -BOC- and -EOC- represent the beginning and end of the
!     comments, respectively.
!
! $ Restrictions
!
!     1) The begin comments marker, BEGMRK, and the end comments marker,
!        ENDMRK, must each appear alone on a line in the comment text
!        file, if they are not blank.
!
!     2) The maximum length of a text line in a comment file is
!        specified by the LINLEN parameter defined below. Currently
!        this values is 1000 characters.
!
!     3) The maximum length of a single line comment in the comment
!        area is specified by the parameter LINLEN defined below.
!        Currently this value is 1000 characters.
!
!     4) This routine uses constants that are specific to the ASCII
!        character sequence. The results of using this routine with
!        a different character sequence are unpredictable.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     K.R. Gehringer (JPL)
!
! $ Version
!
!-    Support Version 1.3.0, 01-NOV-2006 (NJB) (EDW)
!
!        Changed storage duration of array COMBUF to "saved" to
!        prevent memory problems on the PC/Windows/Visual C platform.
!
!-    Support Version 1.2.0, 16-NOV-2001 (BVS) (FST)
!
!        Buffer line size (LINLEN) was increased from 255 to 1000
!        characters to make it consistent the line size in SPC
!        routines.
!
!        Removed an unnecesary call to DAFHLU, as this routine
!        does not interact with the DAF attached to HANDLE at
!        the unit level.
!
!-    Beta Version 1.1.1, 23-JAN-1999 (BVS)
!
!        Buffer size (BUFSIZ) was increases from 22 to 2000 lines.
!
!-    Beta Version 1.1.0, 18-JAN-1996 (KRG)
!
!        Added a test and errors for checking to see whether COMLUN
!        was actually attached to an ASCII text file when this routine
!        was called.
!
!-    Beta Version 1.0.0, 4-JAN-1993 (KRG)
!
!-&

! $ Index_Entries
!
!      add comments from a logical unit to a daf file
!
!-&

!
!     SPICELIB functions
!

!
!     Local parameters
!
!     Set the value for the maximum length of a text line.
!
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 1000 )
!
!     Set the length of a DAF file internal filename.
!
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
!
!     Set the size of the comment buffer.
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 2000 )
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
      CHARACTER*(LINLEN)    COMBUF(BUFSIZ)
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(LINLEN)    LINE

      INTEGER               I
      INTEGER               INTCHR
      INTEGER               IOSTAT
      INTEGER               J
      INTEGER               LENGTH
      INTEGER               NCOMR
      INTEGER               NUMCOM
      INTEGER               SCRLUN

      INTEGER               BWARD
      INTEGER               FWARD
      INTEGER               FREE
      INTEGER               ND
      INTEGER               NI

      LOGICAL               EOF
      LOGICAL               MORE
      LOGICAL               OPENED

!
!     Saved variables
!
      SAVE                  COMBUF

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DAFACU' )
      END IF
!
!     Verify that the DAF file attached to HANDLE is opened with write
!     access.
!
      CALL DAFSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFACU' )
         RETURN

      END IF
!
!     Logical units must be positive. If it is not, signal an error.
!
      IF ( COMLUN .LE. 0 ) THEN

         CALL SETMSG ( '# is not a valid logical unit. Logical' &
         //            ' units must be positive.'               )
         CALL ERRINT ( '#', COMLUN                              )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                 )
         CALL CHKOUT ( 'DAFACU'                                 )
         RETURN

      END IF
!
!     Verify that there is an open ASCII text file attached to COMLUN.
!
      INQUIRE ( UNIT=COMLUN, OPENED=OPENED, IOSTAT=IOSTAT )

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'The INQUIRE on logical unit # failed. The' &
         //            ' value of IOSTAT was #.'                   )
         CALL ERRINT ( '#', COMLUN                                 )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(INQUIREFAILED)'                      )
         CALL CHKOUT ( 'DAFACU'                                    )
         RETURN

      END IF

      IF ( .NOT. OPENED ) THEN

         CALL SETMSG ( 'There is no open file attached to logical' &
         //            ' unit #, so no comments could be read.'    )
         CALL ERRINT ( '#', COMLUN                                 )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                    )
         CALL CHKOUT ( 'DAFACU'                                    )
         RETURN

      END  IF

!
!     Read the file record of the DAF attached to HANDLE. We get back
!     some stuff that we do not use.
!
      CALL DAFRFR ( HANDLE, ND, NI, IFNAME, FWARD, BWARD, FREE )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFACU' )
         RETURN

      END IF
!
!     Compute the number of comment records.
!
      NCOMR = FWARD - 2
!
!     Get an available logical unit for the comment scratch file.
!
      CALL GETLUN ( SCRLUN )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DAFACU' )
         RETURN

      END IF
!
!     Attempt to open the comment scratch file.
!
      OPEN ( UNIT=SCRLUN, STATUS='SCRATCH', IOSTAT=IOSTAT )

      IF ( IOSTAT .NE. 0 ) THEN

         CALL SETMSG ( 'Attempt to open a temporary file failed.' // &
                       ' IOSTAT = #.'                              )
         CALL ERRINT ( '#', IOSTAT                                 )
         CALL SIGERR ( 'SPICE(FILEOPENFAILED)'                     )
         CALL CHKOUT ( 'DAFACU'                                    )
         RETURN

      END IF
!
!     Start looking for the begin comment marker. If the begin marker
!     is a blank line, then the comments begin on the first line of the
!     comment file. Otherwise, the comments begin on the line
!     immediately following the line which contains the begin comments
!     marker.
!
      LINE = ' '
      EOF  = .FALSE.
      DO WHILE ( LINE .NE. BEGMRK )

         CALL READLN ( COMLUN, LINE, EOF )
         CALL LJUST  ( LINE, LINE        )

         IF ( FAILED() ) THEN

            CLOSE       ( SCRLUN   )
            CALL CHKOUT ( 'DAFACU' )
            RETURN

         END IF
!
!        If we have encountered the end of file  here, we have a
!        problem: We did not find the begin comments marker in the
!        text file. So, set an appropriate error message and signal
!        the error. don't forget to close the scratch file.
!
         IF ( EOF ) THEN

            CLOSE       ( SCRLUN                                      )
            CALL SETMSG ( 'The begin comments marker ''#'' was not'  // &
                          ' found in the comment file ''#''.'         )
            CALL ERRCH  ( '#', BEGMRK                                 )
            CALL ERRFNM ( '#', COMLUN                                 )
            CALL SIGERR ( 'SPICE(MARKERNOTFOUND)'                     )
            CALL CHKOUT ( 'DAFACU'                                    )
            RETURN

         END IF

      END DO
!
!     Begin reading in the comment lines from the comment file,
!     placing them a buffer at a time into the temporary file.
!     We also scan each line for non printing characters.
!
      LINE = ' '
      IF ( ENDMRK .EQ. ' ' ) THEN
!
!        If the end mark is blank, then we want to go until we hit the
!        end of the comment file.
!
         DO WHILE ( .NOT. EOF )

            NUMCOM = 0
            CALL READLA ( COMLUN, BUFSIZ, NUMCOM, COMBUF, EOF )

            IF ( FAILED() ) THEN

               CLOSE       ( SCRLUN   )
               CALL CHKOUT ( 'DAFACU' )
               RETURN

            END IF
!
!           If we got some comments, we need to scan them for non-
!           printing characters.
!
            IF ( NUMCOM .GT. 0 ) THEN

               DO I = 1, NUMCOM

                  LENGTH = LASTNB ( COMBUF(I) )
!
!                 Scan the comment line for non printinig characters.
!
                  DO J = 1, LENGTH
!
!                    Check to see that the characters in the buffer
!                    are all printing ASCII characters. The bounds
!                    for printing ASCII characters are given by
!                    MAXPCH and MINPCH, which are defined in the
!                    $ Local Parameters section of the header.
!
                     INTCHR = ICHAR ( COMBUF(I)(J:J) )
                     IF ( ( INTCHR .GT. MAXPCH ) .OR. &
                          ( INTCHR .LT. MINPCH )      ) THEN

                           CLOSE       ( SCRLUN                      )
                           CALL SETMSG ( 'A nonprinting character'  // &
                                         ' was encountered in the'  // &
                                         ' comments. Value: #'       )
                           CALL ERRINT ( '#', INTCHR                 )
                           CALL SIGERR ( 'SPICE(ILLEGALCHARACTER)'   )
                           CALL CHKOUT ( 'DAFACU'                    )
                           RETURN

                     END IF

                  END DO

               END DO
!
!              Write the comments to the temporary file.
!
               CALL WRITLA( NUMCOM, COMBUF, SCRLUN )

            END IF

            IF ( FAILED() ) THEN

               CLOSE       ( SCRLUN   )
               CALL CHKOUT ( 'DAFACU' )
               RETURN

            END IF

         END DO

      ELSE
!
!        The endmark is non blank, then  we want to go until we find a
!        line in the comment file that matches the end mark that was
!        entered.
!
         MORE = .TRUE.
         DO WHILE ( MORE )

            NUMCOM = 0
            CALL READLA ( COMLUN, BUFSIZ, NUMCOM, COMBUF, EOF )

            IF ( FAILED() ) THEN

               CLOSE       ( SCRLUN   )
               CALL CHKOUT ( 'DAFACU' )
               RETURN

            END IF
!
!           Look for ENDMRK in the current buffer, if we got some
!           comments.
!
            IF ( NUMCOM .GT. 0 ) THEN

               I = 1
               DO WHILE ( MORE .AND. ( I .LE. NUMCOM ) )

                  LINE = COMBUF(I)
                  CALL LJUST ( LINE, LINE )

                  IF ( LINE .EQ. ENDMRK ) THEN

                     MORE   = .FALSE.
                     NUMCOM = I - 1

                  ELSE

                     I = I + 1

                  END IF

               END DO

            END IF
!
!           If we still have some comments, we need to scan them for
!           non printing characters.
!
            IF ( NUMCOM .GT. 0 ) THEN

               DO I = 1, NUMCOM

                  LENGTH = LASTNB ( COMBUF(I) )
!
!                 Scan the comment line for non printinig characters.
!
                  DO J = 1, LENGTH
!
!                    Check to see that the characters in the buffer
!                    are all printing ASCII characters. The bounds
!                    for printing ASCII characters are given by
!                    MAXPCH and MINPCH, which are defined in the
!                    $ Local Parameters section of the header.
!
                     INTCHR = ICHAR ( COMBUF(I)(J:J) )
                     IF ( ( INTCHR .GT. MAXPCH ) .OR. &
                          ( INTCHR .LT. MINPCH )      ) THEN

                           CLOSE       ( SCRLUN                      )
                           CALL SETMSG ( 'A nonprinting character'  // &
                                         ' was encountered in the'  // &
                                         ' comment buffer. Value:'  // &
                                         ' #'                        )
                           CALL ERRINT ( '#', INTCHR                 )
                           CALL SIGERR ( 'SPICE(ILLEGALCHARACTER)'   )
                           CALL CHKOUT ( 'DAFACU'                    )
                           RETURN

                     END IF

                  END DO

               END DO
!
!              Write the comments to the temporary file.
!
               CALL WRITLA( NUMCOM, COMBUF, SCRLUN )

            END IF

            IF ( FAILED() ) THEN

               CLOSE       ( SCRLUN   )
               CALL CHKOUT ( 'DAFACU' )
               RETURN

            END IF
!
!           If we have encountered the end of file here, we have a
!           problem: We did not find the end comments marker in the
!           text file. So, set an appropriate error message and
!           signal the error.
!
            IF ( MORE .AND. EOF ) THEN

               CLOSE       ( SCRLUN                                )
               CALL SETMSG ( 'The end comments marker ''#'' was'  // &
                             ' not found in the comment file'     // &
                             ' ''#''.'                             )
               CALL ERRCH  ( '#', ENDMRK                           )
               CALL ERRFNM ( '#', COMLUN                           )
               CALL SIGERR ( 'SPICE(MARKERNOTFOUND)'               )
               CALL CHKOUT ( 'DAFACU'                              )
               RETURN

            END IF

         END DO

      END IF
!
!     If we made it to here, we have culled all of the comments out of
!     the text file and they were all OK. So we need to add all of the
!     comments to the DAF comment area now.
!
!     If we are supposed to insert a blank line to separate the current
!     addition from any previously stored comments, and there are
!     comments already in the comment area, indicated by NCOMR > 0, then
!     we insert the blank line. Otherwise, just add the comments.
!
      IF ( INSBLN .AND. ( NCOMR .GT. 0 ) ) THEN

         CALL DAFAC ( HANDLE, 1, ' ' )

         IF ( FAILED() ) THEN

            CLOSE       ( SCRLUN   )
            CALL CHKOUT ( 'DAFACU' )
            RETURN

         END IF

      END IF
!
!     Rewind the scratch file to get ready to put the comments into the
!     comment area.
!
      REWIND ( SCRLUN )
!
!     Begin reading through the scratch file, placing the comment lines
!     into the comment area of the DAF file a buffer at a time
!
      EOF = .FALSE.
      DO WHILE ( .NOT. EOF )

         NUMCOM = 0
!
!        Read in a buffer of comment lines.
!
         CALL READLA ( SCRLUN, BUFSIZ, NUMCOM, COMBUF, EOF )
!
!        If we got some, add them to the comment area of the DAF file.
!
         IF ( NUMCOM .GT. 0 ) THEN

            CALL DAFAC  ( HANDLE, NUMCOM, COMBUF )

         END IF

         IF ( FAILED() ) THEN

            CLOSE       ( SCRLUN   )
            CALL CHKOUT ( 'DAFACU' )
            RETURN

         END IF

      END DO
!
!     Close the scratch file before exiting, it's the only one we
!     opened.
!
      CLOSE ( SCRLUN )

      CALL CHKOUT ( 'DAFACU' )
      RETURN
      END
