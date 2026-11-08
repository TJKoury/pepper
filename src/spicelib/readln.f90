!$Procedure READLN ( Read a text line from a logical unit )

      subroutine readln ( unit, line, eof )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Read a single line of text from the Fortran logical unit UNIT,
!     reporting the end of file if it occurs.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASCII
!     FILES
!     TEXT
!
! $ Declarations
!
      integer(ip)        unit
      character*(*)      line
      logical            eof
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   The Fortran unit number to use for input.
!     LINE       O   The line read from the file.
!     EOF        O   A logical flag indicating the end of file.
!
! $ Detailed_Input
!
!     UNIT     is the Fortran unit number for the input. This may
!              be either the unit number for the terminal, or the
!              unit number of a previously opened text file.
!
! $ Detailed_Output
!
!     LINE     is the next text line encountered when reading from UNIT.
!
!              If the length of the character string LINE is shorter
!              than the length of the current line in the text file, the
!              line is truncated on the right by the Fortran READ
!              statement, filling LINE with the first LEN(LINE)
!              characters from the current line in the file.
!
!              If an error or the end of file occurs during the
!              attempt to read from UNIT, the value of this variable
!              is not guaranteed.
!
!     EOF      is .TRUE. if the end of file ( IOSTAT < 0 ) is
!              encountered during the attempt to read from unit UNIT.
!              Otherwise, this variable will be set to .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine only checks in with the error handler in the event
!     that an error occurred. (Discovery check in)
!
!     1)  If an error occurs while attempting to read from the text file
!         attached to UNIT, the error SPICE(FILEREADFAILED) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine will read a single line, a text record, from the
!     logical unit UNIT. UNIT may be the terminal, or it may be a
!     logical unit number obtained from a Fortran OPEN or INQUIRE
!     statement. This routine will set a logical flag, EOF, on output
!     if the end of the file is encountered during the read attempt.
!
! $ Examples
!
!     CALL READLN ( UNIT, LINE, EOF )
!
!     IF ( EOF ) THEN
!        < The end of file, deal with it appropriately >
!     END IF
!
!     You now have a line of text from unit UNIT.
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
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Version history entries for Beta versions.
!
!-    SPICELIB Version 1.0.0, 20-DEC-1995 (KRG)
!
!-&


! $ Index_Entries
!
!     read a text line from a logical unit
!
!-&


!
!     Local variables
!
      integer(ip)           iostat
!
!     Standard SPICE error handling.
!
!
!     Read in the next line from the text file attached to UNIT.
!
      read ( unit, fmt='(A)', iostat=iostat ) line
!
!     Check to see if we got a read error, and signal it if we did.
!
      if ( iostat .gt. 0  ) then

         call chkin  ( 'READLN'                                  )
         call setmsg ( 'Error reading from file: #. IOSTAT = #.' )
         call errfnm ( '#', unit                                 )
         call errint ( '#', iostat                               )
         call sigerr ( 'SPICE(FILEREADFAILED)'                   )
         call chkout ( 'READLN'                                  )
         return

      end if
!
!     Check to see if we got the end of file, and set the logical
!     flag EOF if we did.
!
      if ( iostat .lt. 0 ) then

         eof = .true.

      else

         eof = .false.

      end if

      return
      end
