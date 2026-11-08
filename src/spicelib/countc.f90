!$Procedure COUNTC ( Count characters in a text file )

      integer function countc ( unit, bline, eline, line )

! $ Abstract
!
!     Count the characters in a group of lines in a text file.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTERS
!     FILES
!
! $ Declarations

      implicit none

      integer               unit
      integer               bline
      integer               eline
      character*(*)         line

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Logical unit connected to text file.
!     BLINE      I   Beginning line number.
!     ELINE      I   Ending line number.
!     LINE      I-O  Workspace.
!
!     The function returns the number of characters in a group of lines
!     in a text file.
!
! $ Detailed_Input
!
!     UNIT     is a logical unit that has been connected to a
!              text file by the calling program. Use the routine
!              TXTOPR to open the file for read access and get its
!              logical unit. A text file is a formatted,
!              sequential file that contains only printable
!              characters:  ASCII 32-126.
!
!     BLINE,
!     ELINE    are line numbers in the text file. BLINE is
!              the line where the count will begin, and ELINE
!              is the line where the count will end. The
!              number of characters in the beginning and ending
!              lines are included in the total count.
!
!              By convention, line 1 is the first line of the file.
!
!     LINE     on input, is an arbitrary character string whose
!              contents are ignored. LINE is used to read lines
!              from the file connected to UNIT; its function
!              is to determine the maximum length of the lines
!              that can be read from the file. Lines longer
!              than the declared length of LINE are truncated
!              as they are read.
!
! $ Detailed_Output
!
!     LINE     on output, is undefined.
!
!     The function returns the number of characters in the group of
!     lines in the text file beginning with BLINE and ending with ELINE.
!     Trailing blanks on a line are not included in the count.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If an error occurs while reading from the input file,
!         the error SPICE(FILEREADFAILED) is signaled.
!
!     2)  If a non-printing ASCII character is encountered during
!         the count, the error SPICE(INVALIDTEXT) is signaled.
!
!     3)  If BLINE is greater than ELINE or if the file does not
!         contain both of this lines, the error SPICE(CANNOTFINDGRP)
!         is signaled.
!
! $ Files
!
!     See argument UNIT. COUNTC rewinds the text file connected to
!     UNIT and then steps through the file. The next read statement
!     after calling COUNTC would return the line after ELINE.
!
! $ Particulars
!
!     This routine counts characters in a group of lines in a text
!     file. Using COUNTC, you can determine in advance how much space
!     is required to store those characters.
!
! $ Examples
!
!     The following code fragment opens an existing text file for
!     read access and counts the characters that it contains in
!     the first five lines. We'll assume that the longest line
!     in the file is 80 characters.
!
!        INTEGER               COUNTC
!        INTEGER               UNIT
!        INTEGER               N
!        CHARACTER*(80)        LINE
!
!        CALL TXTOPR ( 'DATA.TXT', UNIT )
!
!        N = COUNTC ( UNIT, 1, 5, LINE )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     J.E. McLean        (JPL)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 18-MAR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
!
!        Set the default function value to either 0, 0.0D0, .FALSE.,
!        or blank depending on the type of the function.
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
!     count characters in a text file
!
!-&


!
!     SPICELIB functions
!
      integer               lastnb

      logical               return

!
!     Local variables
!
      integer               chars
      integer               iostat
      integer               linect

      logical               done

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         countc = 0
         return
      else
         call chkin ( 'COUNTC' )
         countc = 0
      end if

!
!     First, see if the line numbers make sense.
!
      if ( ( bline .gt. eline ) .or. ( bline .le. 0 ) ) then

         call setmsg ( 'The line numbers do not make sense:  '        // &
                       'BLINE = # and  ELINE = #.'                    )
         call errint ( '#', bline                                     )
         call errint ( '#', eline                                     )
         call sigerr ( 'SPICE(CANNOTFINDGRP)'                         )
         call chkout ( 'COUNTC'                                       )
         return

      end if

!
!     Read through the file, line by line, beginning with the first
!     line in the file, checking for I/O errors, and counting
!     characters in the lines between and including BLINE and ELINE.
!
      rewind ( unit )

      linect = 0
      chars  = 0
      done   = .false.

      do while ( .not. done )

         read ( unit, fmt='(A)', iostat=iostat ) line
!
!        An end-of-file condition is indicated by a negative value
!        for IOSTAT. Any other non-zero value indicates some other
!        error.  If IOSTAT is zero, the read was successful.
!
         if ( iostat .gt. 0 ) then

            call setmsg ( 'Error reading text file named FILENAME.'   // &
                          'The value of IOSTAT is #.'                 )
            call errint ( '#', iostat                                 )
            call errfnm ( 'FILENAME', unit                            )
            call sigerr ( 'SPICE(FILEREADFAILED)'                     )
            call chkout ( 'COUNTC'                                    )
            return

         else if ( iostat .lt. 0 ) then

            call setmsg ( 'Reached end of file unexpectedly at line ' // &
                          '# in file FILE.  BLINE = # and ELINE = #.' )
            call errint ( '#', linect                                 )
            call errint ( '#', bline                                  )
            call errint ( '#', eline                                  )
            call errfnm ( 'FILE', unit                                )
            call sigerr ( 'SPICE(CANNOTFINDGRP)'                      )
            call chkout ( 'COUNTC'                                    )
            return

         else
!
!           We've read a line successfully, so add it to the line count.
!           If this line is in the group delimited by BLINE and ELINE,
!           count the characters in it, and if this line is ELINE, we're
!           done.
!
            linect = linect + 1

            if (( linect .ge. bline ) .and. ( linect .le. eline )) then

!
!              Add the number of characters in this line to the count.
!              If LINE is blank, LASTNB will return 0 which is just
!              what we want.
!
               chars = chars + lastnb ( line )

!
!              Remove the printable characters from the line.  If
!              any characters remain, signal an error.
!
               call astrip ( line, char( 32), char(126), line )

               if ( line .ne. ' ' ) then

                  call setmsg ( 'Non-printing ASCII characters were ' // &
                                'found when counting characters on '  // &
                                'line number # in file FILENAME.'     )
                  call errint ( '#', linect                           )
                  call errfnm ( 'FILENAME', unit                      )
                  call sigerr ( 'SPICE(INVALIDTEXT)'                  )
                  call chkout ( 'COUNTC'                              )
                  return

               end if

            end if

            if ( linect .eq. eline ) done = .true.

         end if

      end do

!
!     Assign the final character count.
!
      countc = chars


      call chkout ( 'COUNTC' )
      return
      end
