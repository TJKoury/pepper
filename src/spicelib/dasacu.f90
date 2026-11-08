!$Procedure DASACU ( DAS add comments from a logical unit )

      SUBROUTINE DASACU ( COMLUN, BEGMRK, ENDMRK, INSBLN, HANDLE )

! $ Abstract
!
!     Add comments to a previously opened binary DAS file from a
!     previously opened text file attached to a Fortran logical unit.
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
!     None.
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               COMLUN
      CHARACTER*(*)         BEGMRK
      CHARACTER*(*)         ENDMRK
      LOGICAL               INSBLN
      INTEGER               HANDLE

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     COMLUN    I   Logical unit of the open comment text file.
!     BEGMRK    I   The begin comments marker in the comment text file.
!     ENDMRK    I   The end comments marker in the comment text file.
!     INSBLN    I   A flag indicating whether to insert a blank line.
!     HANDLE    I   Handle of a DAS file opened with write access.
!     LNSIZE    P   Maximum length of comment line.
!
! $ Detailed_Input
!
!     COMLUN   is the Fortran logical unit of a previously opened text
!              file which contains comments that are to be added to
!              the comment area of a binary E-Kernel file.
!
!     BEGMRK   is a marker which identifies the beginning of the
!              comments in the comment text file. This marker must
!              appear on a line by itself, and leading and trailing
!              blanks are not significant.
!
!              The line immediately following this marker is the first
!              comment line to be placed into the comment area of the
!              binary DAS file.
!
!              If the begin marker is blank, BEGMRK .EQ. ' ', then the
!              comments are assumed to start at the current location
!              in the comment text file.
!
!     ENDMRK   is a marker which identifies the end of the comments in
!              the comment text file. This marker must appear on a line
!              by itself, and leading and trailing blanks are not
!              significant.
!
!              The line immediately preceding this marker is the last
!              comment line to be placed into the comment area of the
!              binary DAS file.
!
!              If the end marker is blank, ENDMRK .EQ. ' ', then the
!              comments are assumed to stop at the end of the comment
!              text file.
!
!     INSBLN   is a logical flag which indicates whether a blank line is
!              to be inserted into the comment area of the binary DAS
!              file attached to HANDLE before any comments are added
!              to the comment area of the DAS file. This is to provide
!              a simple mechanism for separating any comments already
!              contained in the comment area of a DAS file from those
!              comments that are being added.
!
!              If the comment area of a binary DAS file is empty, the
!              value of this flag is not significant, the comments will
!              simply be placed into the comment area.
!
!     HANDLE   is the file handle for a binary DAS file that has been
!              opened with write access.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     LNSIZE   is both the maximum length of a comment line that can
!              be read from the input file, and the maximum length
!              of a comment line that this routine can write to the
!              output DAS file.
!
!              LNSIZE is set to 255 characters.
!
!              The DAS file format itself does not impose a limit
!              on the length of lines in comment area, other than
!              that the character count must be expressible in a
!              32-bit signed integer.
!
! $ Exceptions
!
!     1)  If the scratch file for temporarily holding the comments
!         culled from the text file cannot be opened, the
!         error SPICE(FILEOPENFAILED) is signaled.
!
!     2)  If a non printing ASCII character is encountered in the
!         comments, the error SPICE(ILLEGALCHARACTER) is signaled.
!
!     3)  If the begin marker cannot be found in the text file, the
!         error SPICE(MARKERNOTFOUND) is signaled.
!
!     4)  If the end marker cannot be found in the text file, the
!         error SPICE(MARKERNOTFOUND) is signaled.
!
! $ Files
!
!     See parameters COMLUN and HANDLE in the $Detailed_Inputs
!     section.
!
!     A scratch file is used to temporarily hold the comments culled
!     from the comment text file. This is so we do not have to find the
!     place where we started searching for comments in the original
!     file.
!
! $ Particulars
!
!     This routine will place all lines between two specified markers,
!     a "begin comments marker" and an "end comments marker," in a
!     text file into the comment area of a binary DAS file attached to
!     HANDLE. If the "begin comments marker" is blank, then the
!     comments are assumed to start at the current location of the
!     comment text file attached to COMLUN. If the "end comments
!     marker" is blank, then the comments are assumed to stop at the
!     end of the comment text file attached to COMLUN.
!
! $ Examples
!
!     We will be using the files 'jabber.txt', 'batty.txt', and
!     'wndrland.DAS' in the example which follows.
!
!     'wndrland.dat' is a binary DAS file with an empty comment area
!                    into which we are going to place the entire file
!                    'jabber.txt' and a selected portion of the file
!                    'batty.txt'.
!
!     'jabber.txt'   is a text file that is to be placed into the
!                    comment area of the binary DAS file 'wndrland.DAS'.
!
!     'batty.txt'    is a text file from which will have a selected
!                    portion of its text placed into the comment area
!                    of the binary DAS file 'wndrland.DAS'.
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
!           JABLUN   be the logical unit for the file 'jabber.txt'
!           BATLUN   be the logical unit for the file 'batty.txt'
!     and
!           HANDLE   be the DAS handle for the file 'wndrland.DAS'
!
!     The code fragment
!
!     C
!     C      Open the files.
!     C
!            CALL DASOPW ( 'wndrland.DAS', HANDLE )
!            CALL TXTOPN ( 'jabber.txt'  , JABLUN )
!            CALL TXTOPN ( 'batty.txt'   , BATLUN )
!     C
!     C      Initialize the markers for the file 'jabber.txt'. We want
!     C      to include the entire file, so both markers are blank.
!     C
!            BEGMRK = ' '
!            ENDMRK = ' '
!            INSBLN = .TRUE.
!     C
!     C      Add the comments from the file 'jabber.txt'
!     C
!            CALL DASACU ( JABLUN, BEGMRK, ENDMRK, INSBLN, HANDLE )
!     C
!     C      Initialize the markers for the file `batty.txt'. We want
!     C      to include the bat poem only, so we define the begin and
!     C      end marker accordingly.
!     C
!            BEGMRK = 'BEGIN bat poem'
!            ENDMRK = 'END bat poem'
!            INSBLN = .TRUE.
!     C
!     C      Add the comments from the file 'batty.txt'
!     C
!            CALL DASACU ( BATLUN, BEGMRK, ENDMRK, INSBLN, HANDLE )
!     C
!     C      Close the files.
!
!            CLOSE       ( JABLUN )
!            CLOSE       ( BATLUN )
!            CALL DASCLS ( HANDLE )
!
!     will create a comment area in 'wndrland.DAS' which contains:
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
!     1)  The begin comments marker, BEGMRK, and the end comments
!         marker, ENDMRK, must each appear alone on a line in the
!         comment text file if they are not blank.
!
!     2)  The maximum length of a text line in the input comment file
!         is specified by the LINLEN parameter defined below. Currently
!         this value is 255 characters.
!
!     3)  The maximum length of a single comment line that can be
!         written by this routine to the output DAS file's comment area
!         is specified by the parameter LINLEN defined below. Currently
!         this value is 255 characters.
!
!     4)  This routine uses constants that are specific to the ASCII
!         character sequence. The results of using this routine with a
!         different character sequence are unpredictable.
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
!
! $ Version
!
!-    SPICELIB Version 1.2.2, 02-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.1, 15-MAR-2017 (NJB)
!
!        Added description of parameter LNSIZE. Fixed typos
!        throughout the comments.
!
!-    SPICELIB Version 1.2.0, 07-JUL-1996 (NJB) (KRG)
!
!        Removed declaration, DATA and SAVE statements for unused
!        variable FIRST.
!
!     Beta Version 1.1.0, 20-SEP-1995 (KRG)
!
!        Added a check of FAILED after the call to GETLUN to trap
!        an error, if one is signaled by GETLUN, before attempting to
!        open the SCRATCH file.
!
!     Beta Version 1.0.0, 04-JAN-1993 (KRG)
!
!-&


! $ Index_Entries
!
!     add comments from a logical unit to a DAS file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.2.0, 07-JUL-1996 (NJB)
!
!        Removed declaration, DATA and SAVE statements for unused
!        variable FIRST.
!
!-    Beta Version 1.1.0, 20-SEP-1995 (KRG)
!
!        Added a check of FAILED after the call to GETLUN to trap
!        an error, if one is signaled by GETLUN, before attempting to
!        open the SCRATCH file.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               LASTNB

      LOGICAL               FAILED
      LOGICAL               RETURN
!
!     Local parameters
!
!     Set the value for the maximum length of a text line.
!
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 255 )
!
!     Set the length of a DAS file ID word.
!
      INTEGER               IDWLEN
      PARAMETER           ( IDWLEN = 8 )
!
!     Set the length of a DAS file internal filename.
!
      INTEGER               IFNLEN
      PARAMETER           ( IFNLEN = 60 )
!
!     Set the size of the comment buffer.
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 22 )
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
      CHARACTER*(IDWLEN)    IDWORD
      CHARACTER*(IFNLEN)    IFNAME
      CHARACTER*(LINLEN)    LINE

      INTEGER               I
      INTEGER               INTCHR
      INTEGER               IOSTAT
      INTEGER               J
      INTEGER               LENGTH
      INTEGER               NCOMC
      INTEGER               NCOMR
      INTEGER               NRESVC
      INTEGER               NRESVR
      INTEGER               NUMCOM
      INTEGER               SCRLUN

      LOGICAL               EOF
      LOGICAL               MORE


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'DASACU' )
      END IF
!
!     Verify that the DAS file attached to HANDLE is opened with write
!     access.
!
      CALL DASSIH ( HANDLE, 'WRITE' )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DASACU' )
         RETURN

      END IF
!
!     Get the number of comment characters, and some other stuff that
!     we will not be using.
!
      CALL DASRFR ( HANDLE, IDWORD, IFNAME, &
                            NRESVR, NRESVC, &
                            NCOMR,  NCOMC   )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DASACU' )
         RETURN

      END IF
!
!     Get an available logical unit for the comment scratch file.
!
      CALL GETLUN ( SCRLUN )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'DASACU' )
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
         CALL CHKOUT ( 'DASACU'                                    )
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
            CALL CHKOUT ( 'DASACU' )
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
            CALL CHKOUT ( 'DASACU'                                    )
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
               CALL CHKOUT ( 'DASACU' )
               RETURN

            END IF
!
!           If we got some comments, we need to scan them for non
!           printing characters.
!
            IF ( NUMCOM .GT. 0 ) THEN

               DO I = 1, NUMCOM

                  LENGTH = LASTNB ( COMBUF(I) )
!
!                 Scan the comment line for non printing characters.
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
                           CALL CHKOUT ( 'DASACU'                    )
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
               CALL CHKOUT ( 'DASACU' )
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
               CALL CHKOUT ( 'DASACU' )
               RETURN

            END IF
!
!           Look for ENDMRK in the current buffer if we got some
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
                           CALL CHKOUT ( 'DASACU'                    )
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
               CALL CHKOUT ( 'DASACU' )
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
               CALL CHKOUT ( 'DASACU'                              )
               RETURN

            END IF

         END DO

      END IF
!
!     If we made it to here, we have culled all of the comments out of
!     the text file and they were all OK. So we need to add all of the
!     comments to the DAS comment area now.
!
!     If we are supposed to insert a blank line to separate the current
!     addition from any previously stored comments, and there are
!     comments already in the comment area, indicated by NCOMC > 0, then
!     we insert the blank line. Otherwise, just add the comments.
!
      IF ( INSBLN .AND. ( NCOMC .GT. 0 ) ) THEN

         CALL DASAC ( HANDLE, 1, ' ' )

         IF ( FAILED() ) THEN

            CLOSE       ( SCRLUN   )
            CALL CHKOUT ( 'DASACU' )
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
!     into the comment area of the DAS file a buffer at a time
!
      EOF = .FALSE.
      DO WHILE ( .NOT. EOF )

         NUMCOM = 0
!
!        Read in a buffer of comment lines.
!
         CALL READLA ( SCRLUN, BUFSIZ, NUMCOM, COMBUF, EOF )
!
!        If we got some, add them to the comment area of the DAS file.
!
         IF ( NUMCOM .GT. 0 ) THEN

            CALL DASAC  ( HANDLE, NUMCOM, COMBUF )

         END IF

         IF ( FAILED() ) THEN

            CLOSE       ( SCRLUN   )
            CALL CHKOUT ( 'DASACU' )
            RETURN

         END IF

      END DO
!
!     Close the scratch file before exiting; it's the only one we
!     opened.
!
      CLOSE ( SCRLUN )

      CALL CHKOUT ( 'DASACU' )
      RETURN
      END
