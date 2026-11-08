!$Procedure READLA   ( Read array of lines from a logical unit )

      subroutine readla ( unit, maxlin, numlin, array, eof )

! $ Abstract
!
!     Read lines from a Fortran logical unit and place them in a
!     character string array.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      implicit none

      integer               unit
      integer               maxlin
      integer               numlin
      character*(*)         array(*)
      logical               eof

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Fortran unit number to use for input.
!     MAXLIN     I   Maximum number of lines ARRAY can hold.
!     NUMLIN     O   Number of lines read from the file.
!     ARRAY      O   Array containing the lines read from the file.
!     EOF        O   Logical flag indicating the end of file.
!
! $ Detailed_Input
!
!     UNIT     is the Fortran unit number for the input. This may
!              be either the unit number for the terminal, or the
!              unit number of a previously opened text file.
!
!     MAXLIN   is the maximum number of text lines that can be placed
!              into the ARRAY.
!
! $ Detailed_Output
!
!     NUMLIN   is the number of text lines read from the file attached
!              to UNIT and placed into ARRAY. 0 <= NUMLIN <= MAXLIN.
!
!              In the event of an error while attempting to read a line
!              from the text file attached to UNIT, NUMLIN will contain
!              the number of lines successfully read before the error
!              occurred.
!
!     ARRAY    is the array which is to contain the lines of text read
!              from the text file attached to UNIT.
!
!              If an error or the end of file occurs while reading
!              from the text file attached to UNIT, this array will
!              contain the NUMLIN successfully read lines ARRAY(1)
!              through ARRAY(NUMLIN).
!
!     EOF      on output, this variable will be set to .TRUE. if the
!              end of file ( IOSTAT < 0 ) is encountered during an
!              attempt to read from UNIT. Otherwise, this variable
!              will be set to .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the maximum number of lines, MAXLIN, is not positive, the
!         error SPICE(INVALIDARGUMENT) is signaled.
!
!     2)  If an error occurs while attempting to read from the text file
!         attached to unit, the error is signaled by a routine in the
!         call tree of this routine.
!
! $ Files
!
!     See the description of UNIT above.
!
! $ Particulars
!
!     This routine reads lines of text from a file, placing each line
!     into an element of a character string array.
!
!     An end of file flag will have the value .TRUE. if the end of file
!     is reached while reading. If the file contains more lines than the
!     character string array ARRAY can hold, as specified by the
!     argument MAXLIN, the routine will return and the end of file flag
!     will have the value .FALSE., indicating that there are more lines
!     of text that may be read from the file.
!
!     Upon successful completion, the variable NUMLIN will contain the
!     number of lines of text placed into the character string array.
!     This value may be zero.
!
! $ Examples
!
!     For the examples which follow, assume that we have a file named
!     'mary.txt' which contains the following lines of text:
!
!        <BOF>
!        Mary had a little lamb
!        Whose fleece was white as snow
!        And every where that Mary went
!        The lamb was sure to go
!        <EOF>
!
!     where
!
!        <BOF> marks the beginning of the file
!        <EOF> marks the end of the file
!
!     For each example, assume that we have opened the file 'mary.txt',
!     obtaining the Fortran logical unit TXTLUN, and that we are
!     positioned to begin reading at the beginning of the file, '<BOF>'.
!
!     For brevity, none of the examples perform any error handling
!     functions: they simply assume that everything will work.
!
!     Example 1: ARRAY is large enough to contain the entire contents of
!                the file.
!
!        CHARACTER*(80)        ARRAY(10)
!
!        INTEGER               NUMLIN
!
!        LOGICAL               EOF
!
!        CALL READLA ( TXTLUN, 10, NUMLIN, ARRAY, EOF )
!
!     At this point the output variables NUMLIN, ARRAY, and EOF have
!     the following values:
!
!        NUMLIN   = 4
!
!        ARRAY(1) = 'Mary had a little lamb'
!        ARRAY(2) = 'Whose fleece was white as snow'
!        ARRAY(3) = 'And every where that Mary went'
!        ARRAY(4) = 'The lamb was sure to go'
!
!        EOF      = .TRUE.
!
!     Example 2: ARRAY is not large enough to contain the entire
!                contents of the file -- perform multiple reads.
!
!        CHARACTER*(80)        ARRAY(3)
!
!        INTEGER               NUMLIN
!
!        LOGICAL               EOF
!
!        EOF = .FALSE.
!        DO WHILE ( .NOT. EOF )
!
!           CALL READLA ( TXTLUN, 3, NUMLIN, ARRAY, EOF )
!
!        END DO
!
!     Because the line buffer ARRAY may contain at most 3 lines and the
!     file contains 4 lines, the loop calling READLA will be executed
!     twice, terminating after the second call because EOF will be
!     true.
!
!     After the first call to READLA the output variables NUMLIN, ARRAY,
!     and EOF have the following values:
!
!        NUMLIN   = 3
!
!        ARRAY(1) = 'Mary had a little lamb'
!        ARRAY(2) = 'Whose fleece was white as snow'
!        ARRAY(3) = 'And every where that Mary went'
!
!        EOF      = .FALSE.
!
!     After the second call to READLA the output variables NUMLIN,
!     ARRAY, and EOF have the following values:
!
!        NUMLIN   = 1
!
!        ARRAY(1) = 'The lamb was sure to go'
!
!        EOF      = .TRUE.
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
!     K.R. Gehringer     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved $Version
!        history entries for relevant Beta versions to $Revisions
!        section.
!
!-    SPICELIB Version 1.0.0, 20-DEC-1995 (KRG)
!
!-&


! $ Index_Entries
!
!     read an array of text lines from a logical unit
!
!-&


! $ Revisions
!
!-    Beta Version 2.0.0, 05-JAN-1995 (KRG)
!
!        This routine now participates fully with the SPICELIB error
!        handler, checking in on entry and checking out on exit. The
!        overhead associated with the error handler should not be
!        significant relative to the operation of this routine.
!
!        Moved the test for the end of file outside of the loop. There
!        is no need to test for it every time in the loop, because we
!        only do it to decrement the number of lines read by one to
!        account for the pre-increment before the READ that set the end
!        of file.
!
!        Added a local variable MYEOF so that a value of the variable
!        EOF does not affect the termination of the read loop.
!
!-&


!
!     SPICELIB Functions
!
      logical               failed
      logical               return
!
!     Local variables
!
      integer               i

      logical               myeof
!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'READLA' )
      end if
!
!     Check to see if the maximum number of lines is positive.
!
      if ( maxlin .le. 0 ) then

         call setmsg ( 'The maximum number of lines for the output' // &
                       ' line array was not positive. It was: #.'    )
         call errint ( '#', maxlin                                   )
         call sigerr ( 'SPICE(INVALIDARGUMENT)'                      )
         call chkout ( 'READLA'                                      )
         return

      end if
!
!     Begin reading in the lines from the text file attached to UNIT.
!     Stop when the array of lines is full, I = MAXLIN, or we hit the
!     end of file.
!
      myeof  = .false.
      numlin = 0
      i      = 1

      do while ( ( i .le. maxlin ) .and. ( .not. myeof ) )

         call readln( unit, array(i), myeof )

         if ( failed() ) then
!
!           If the read failed, an appropriate error message has already
!           been set, so we need to set the number of lines that have
!           been correctly read from the file and return.
!
            call chkout ( 'READLA' )
            return

         end if

         numlin = i
         i      = i + 1

      end do
!
!     If we got to here, then we have either filled up the line buffer
!     or we reached the end of the file. If we reached the end of the
!     file we need to adjust the value of NUMLIN to remove the last read
!     attempt.
!
      if ( myeof ) then

         numlin = numlin - 1

      end if

      eof = myeof

      call chkout ( 'READLA' )
      return

      end
