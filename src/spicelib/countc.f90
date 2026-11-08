!$Procedure COUNTC ( Count characters in a text file )

      INTEGER FUNCTION COUNTC ( UNIT, BLINE, ELINE, LINE )

! $ Abstract
!
!     Count the characters in a group of lines in a text file.
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
!     CHARACTERS
!     FILES
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               UNIT
      INTEGER               BLINE
      INTEGER               ELINE
      CHARACTER*(*)         LINE

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
      INTEGER               LASTNB

      LOGICAL               RETURN

!
!     Local variables
!
      INTEGER               CHARS
      INTEGER               IOSTAT
      INTEGER               LINECT

      LOGICAL               DONE

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         COUNTC = 0
         RETURN
      ELSE
         CALL CHKIN ( 'COUNTC' )
         COUNTC = 0
      END IF

!
!     First, see if the line numbers make sense.
!
      IF ( ( BLINE .GT. ELINE ) .OR. ( BLINE .LE. 0 ) ) THEN

         CALL SETMSG ( 'The line numbers do not make sense:  '        // &
                       'BLINE = # and  ELINE = #.'                    )
         CALL ERRINT ( '#', BLINE                                     )
         CALL ERRINT ( '#', ELINE                                     )
         CALL SIGERR ( 'SPICE(CANNOTFINDGRP)'                         )
         CALL CHKOUT ( 'COUNTC'                                       )
         RETURN

      END IF

!
!     Read through the file, line by line, beginning with the first
!     line in the file, checking for I/O errors, and counting
!     characters in the lines between and including BLINE and ELINE.
!
      REWIND ( UNIT )

      LINECT = 0
      CHARS  = 0
      DONE   = .FALSE.

      DO WHILE ( .NOT. DONE )

         READ ( UNIT, FMT='(A)', IOSTAT=IOSTAT ) LINE
!
!        An end-of-file condition is indicated by a negative value
!        for IOSTAT. Any other non-zero value indicates some other
!        error.  If IOSTAT is zero, the read was successful.
!
         IF ( IOSTAT .GT. 0 ) THEN

            CALL SETMSG ( 'Error reading text file named FILENAME.'   // &
                          'The value of IOSTAT is #.'                 )
            CALL ERRINT ( '#', IOSTAT                                 )
            CALL ERRFNM ( 'FILENAME', UNIT                            )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'                     )
            CALL CHKOUT ( 'COUNTC'                                    )
            RETURN

         ELSE IF ( IOSTAT .LT. 0 ) THEN

            CALL SETMSG ( 'Reached end of file unexpectedly at line ' // &
                          '# in file FILE.  BLINE = # and ELINE = #.' )
            CALL ERRINT ( '#', LINECT                                 )
            CALL ERRINT ( '#', BLINE                                  )
            CALL ERRINT ( '#', ELINE                                  )
            CALL ERRFNM ( 'FILE', UNIT                                )
            CALL SIGERR ( 'SPICE(CANNOTFINDGRP)'                      )
            CALL CHKOUT ( 'COUNTC'                                    )
            RETURN

         ELSE
!
!           We've read a line successfully, so add it to the line count.
!           If this line is in the group delimited by BLINE and ELINE,
!           count the characters in it, and if this line is ELINE, we're
!           done.
!
            LINECT = LINECT + 1

            IF (( LINECT .GE. BLINE ) .AND. ( LINECT .LE. ELINE )) THEN

!
!              Add the number of characters in this line to the count.
!              If LINE is blank, LASTNB will return 0 which is just
!              what we want.
!
               CHARS = CHARS + LASTNB ( LINE )

!
!              Remove the printable characters from the line.  If
!              any characters remain, signal an error.
!
               CALL ASTRIP ( LINE, CHAR( 32), CHAR(126), LINE )

               IF ( LINE .NE. ' ' ) THEN

                  CALL SETMSG ( 'Non-printing ASCII characters were ' // &
                                'found when counting characters on '  // &
                                'line number # in file FILENAME.'     )
                  CALL ERRINT ( '#', LINECT                           )
                  CALL ERRFNM ( 'FILENAME', UNIT                      )
                  CALL SIGERR ( 'SPICE(INVALIDTEXT)'                  )
                  CALL CHKOUT ( 'COUNTC'                              )
                  RETURN

               END IF

            END IF

            IF ( LINECT .EQ. ELINE ) DONE = .TRUE.

         END IF

      END DO

!
!     Assign the final character count.
!
      COUNTC = CHARS


      CALL CHKOUT ( 'COUNTC' )
      RETURN
      END
