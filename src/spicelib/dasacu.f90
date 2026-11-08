!$Procedure DASACU ( DAS add comments from a logical unit )

      subroutine dasacu ( comlun, begmrk, endmrk, insbln, handle )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Add comments to a previously opened binary DAS file from a
!     previously opened text file attached to a Fortran logical unit.
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

      implicit none

      integer(ip)           comlun
      character(*)          begmrk
      character(*)          endmrk
      logical               insbln
      integer(ip)           handle

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
      integer(ip)           lastnb

      logical               failed
      logical               return
!
!     Local parameters
!
!     Set the value for the maximum length of a text line.
!
      integer(ip), parameter :: linlen = 255
!
!     Set the length of a DAS file ID word.
!
      integer(ip), parameter :: idwlen = 8
!
!     Set the length of a DAS file internal filename.
!
      integer(ip), parameter :: ifnlen = 60
!
!     Set the size of the comment buffer.
!
      integer(ip), parameter :: bufsiz = 22
!
!     Maximum and minimum decimal values for the printable ASCII
!     characters.
!
      integer(ip), parameter :: maxpch = 126

      integer(ip), parameter :: minpch = 32
!
!     Local variables
!
      character(linlen)     combuf(bufsiz)
      character(idwlen)     idword
      character(ifnlen)     ifname
      character(linlen)     line

      integer(ip)           i
      integer(ip)           intchr
      integer(ip)           iostat
      integer(ip)           j
      integer(ip)           length
      integer(ip)           ncomc
      integer(ip)           ncomr
      integer(ip)           nresvc
      integer(ip)           nresvr
      integer(ip)           numcom
      integer(ip)           scrlun

      logical               eof
      logical               more


!
!     Standard SPICE error handling.
!
      if ( return () ) return
!
!     Verify that the DAS file attached to HANDLE is opened with write
!     access.
!
      call dassih ( handle, 'WRITE' )

      if ( failed() ) return
!
!     Get the number of comment characters, and some other stuff that
!     we will not be using.
!
      call dasrfr ( handle, idword, ifname, &
                            nresvr, nresvc, &
                            ncomr,  ncomc   )

      if ( failed() ) return
!
!     Get an available logical unit for the comment scratch file.
!

      if ( failed() ) return
!
!     Attempt to open the comment scratch file.
!
      open ( newunit=scrlun, status='SCRATCH', iostat=iostat )

      if ( iostat /= 0 ) then

         call setmsg ( 'Attempt to open a temporary file failed.' // &
                       ' IOSTAT = #.'                              )
         call errint ( '#', iostat                                 )
         call sigerr ( 'SPICE(FILEOPENFAILED)'                     )
         return

      end if
!
!     Start looking for the begin comment marker. If the begin marker
!     is a blank line, then the comments begin on the first line of the
!     comment file. Otherwise, the comments begin on the line
!     immediately following the line which contains the begin comments
!     marker.
!
      line = ' '
      eof  = .false.
      do while ( line /= begmrk )

         call readln ( comlun, line, eof )
         call ljust  ( line, line        )

         if ( failed() ) then

            close       ( scrlun   )
            return

         end if
!
!        If we have encountered the end of file  here, we have a
!        problem: We did not find the begin comments marker in the
!        text file. So, set an appropriate error message and signal
!        the error. don't forget to close the scratch file.
!
         if ( eof ) then

            close       ( scrlun                                      )
            call setmsg ( 'The begin comments marker ''#'' was not'  // &
                          ' found in the comment file ''#''.'         )
            call errch  ( '#', begmrk                                 )
            call errfnm ( '#', comlun                                 )
            call sigerr ( 'SPICE(MARKERNOTFOUND)'                     )
            return

         end if

      end do
!
!     Begin reading in the comment lines from the comment file,
!     placing them a buffer at a time into the temporary file.
!     We also scan each line for non printing characters.
!
      line = ' '
      if ( endmrk == ' ' ) then
!
!        If the end mark is blank, then we want to go until we hit the
!        end of the comment file.
!
         do while ( .not. eof )

            numcom = 0
            call readla ( comlun, bufsiz, numcom, combuf, eof )

            if ( failed() ) then

               close       ( scrlun   )
               return

            end if
!
!           If we got some comments, we need to scan them for non
!           printing characters.
!
            if ( numcom > 0 ) then

               do i = 1, numcom

                  length = lastnb ( combuf(i) )
!
!                 Scan the comment line for non printing characters.
!
                  do j = 1, length
!
!                    Check to see that the characters in the buffer
!                    are all printing ASCII characters. The bounds
!                    for printing ASCII characters are given by
!                    MAXPCH and MINPCH, which are defined in the
!                    $ Local Parameters section of the header.
!
                     intchr = ichar ( combuf(i)(j:j) )
                     if ( ( intchr > maxpch ) .or. &
                          ( intchr < minpch )      ) then

                           close       ( scrlun                      )
                           call setmsg ( 'A nonprinting character'  // &
                                         ' was encountered in the'  // &
                                         ' comments. Value: #'       )
                           call errint ( '#', intchr                 )
                           call sigerr ( 'SPICE(ILLEGALCHARACTER)'   )
                           return

                     end if

                  end do

               end do
!
!              Write the comments to the temporary file.
!
               call writla( numcom, combuf, scrlun )

            end if

            if ( failed() ) then

               close       ( scrlun   )
               return

            end if

         end do

      else
!
!        The endmark is non blank, then  we want to go until we find a
!        line in the comment file that matches the end mark that was
!        entered.
!
         more = .true.
         do while ( more )

            numcom = 0
            call readla ( comlun, bufsiz, numcom, combuf, eof )

            if ( failed() ) then

               close       ( scrlun   )
               return

            end if
!
!           Look for ENDMRK in the current buffer if we got some
!           comments.
!
            if ( numcom > 0 ) then

               i = 1
               do while ( more .and. ( i <= numcom ) )

                  line = combuf(i)
                  call ljust ( line, line )

                  if ( line == endmrk ) then

                     more   = .false.
                     numcom = i - 1

                  else

                     i = i + 1

                  end if

               end do

            end if
!
!           If we still have some comments, we need to scan them for
!           non printing characters.
!
            if ( numcom > 0 ) then

               do i = 1, numcom

                  length = lastnb ( combuf(i) )
!
!                 Scan the comment line for non printinig characters.
!
                  do j = 1, length
!
!                    Check to see that the characters in the buffer
!                    are all printing ASCII characters. The bounds
!                    for printing ASCII characters are given by
!                    MAXPCH and MINPCH, which are defined in the
!                    $ Local Parameters section of the header.
!
                     intchr = ichar ( combuf(i)(j:j) )
                     if ( ( intchr > maxpch ) .or. &
                          ( intchr < minpch )      ) then

                           close       ( scrlun                      )
                           call setmsg ( 'A nonprinting character'  // &
                                         ' was encountered in the'  // &
                                         ' comment buffer. Value:'  // &
                                         ' #'                        )
                           call errint ( '#', intchr                 )
                           call sigerr ( 'SPICE(ILLEGALCHARACTER)'   )
                           return

                     end if

                  end do

               end do
!
!              Write the comments to the temporary file.
!
               call writla( numcom, combuf, scrlun )

            end if

            if ( failed() ) then

               close       ( scrlun   )
               return

            end if
!
!           If we have encountered the end of file here, we have a
!           problem: We did not find the end comments marker in the
!           text file. So, set an appropriate error message and
!           signal the error.
!
            if ( more .and. eof ) then

               close       ( scrlun                                )
               call setmsg ( 'The end comments marker ''#'' was'  // &
                             ' not found in the comment file'     // &
                             ' ''#''.'                             )
               call errch  ( '#', endmrk                           )
               call errfnm ( '#', comlun                           )
               call sigerr ( 'SPICE(MARKERNOTFOUND)'               )
               return

            end if

         end do

      end if
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
      if ( insbln .and. ( ncomc > 0 ) ) then

         call dasac ( handle, 1, ' ' )

         if ( failed() ) then

            close       ( scrlun   )
            return

         end if

      end if
!
!     Rewind the scratch file to get ready to put the comments into the
!     comment area.
!
      rewind ( scrlun )
!
!     Begin reading through the scratch file, placing the comment lines
!     into the comment area of the DAS file a buffer at a time
!
      eof = .false.
      do while ( .not. eof )

         numcom = 0
!
!        Read in a buffer of comment lines.
!
         call readla ( scrlun, bufsiz, numcom, combuf, eof )
!
!        If we got some, add them to the comment area of the DAS file.
!
         if ( numcom > 0 ) then

            call dasac  ( handle, numcom, combuf )

         end if

         if ( failed() ) then

            close       ( scrlun   )
            return

         end if

      end do
!
!     Close the scratch file before exiting; it's the only one we
!     opened.
!
      close ( scrlun )

      return
      end
