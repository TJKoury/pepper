!$Procedure SPCT2B ( SPK and CK, text to binary )

      subroutine spct2b ( unit, binary )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Reconstruct a binary SPK or CK file including comments
!     from a text file opened by the calling program.
!
! $ Required_Reading
!
!     SPC
!
! $ Keywords
!
!     FILES
!
! $ Declarations

      implicit none

      integer(ip)           unit
      character(*)          binary

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Logical unit connected to the text format file.
!     BINARY     I   Name of a binary SPK or CK file to be created.
!
! $ Detailed_Input
!
!     UNIT     is the logical unit connected to an existing text
!              format SPK or CK file that may contain comments in
!              the appropriate SPC format, as written by SPCB2A or
!              SPCB2T. This file must be opened for read access
!              using the routine TXTOPR.
!
!              This file may contain text that precedes and
!              follows the SPK or CK data and comments, however,
!              when calling this routine, the file pointer must be
!              in a position in the file such that the next line
!              returned by a READ statement is
!
!                   ''NAIF/DAF''
!
!              which marks the beginning of the data.
!
!     BINARY   is the name of a binary SPK or CK file to be created.
!              The binary file contains the same data and comments
!              as the text file, but in the binary format required
!              for use with the SPICELIB reader subroutines.
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
!     1)  If there is a problem opening or writing to the binary file,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If there is a problem reading from the text file, the
!         error SPICE(FILEREADFAILED) is signaled.
!
!     3)  If there is a problem opening a scratch file, the error
!         SPICE(FILEOPENERROR) is signaled.
!
!     4)  If there is a problem writing to the scratch file, the
!         error SPICE(FILEWRITEFAILED) is signaled.
!
! $ Files
!
!     See arguments UNIT and BINARY above.
!
!     This routine uses a Fortran scratch file to temporarily store the
!     lines of comments if there are any.
!
! $ Particulars
!
!     The SPICELIB SPK and CK reader subroutines read binary files.
!     However, because different computing environments have different
!     binary representations of numbers, you must convert SPK and CK
!     files to text format when porting from one system to another.
!     After converting the file to text, you can transfer it using
!     a transfer protocol program like Kermit or FTP. Then, convert
!     the text file back to binary format.
!
!     The following is a list of the SPICELIB routines that convert
!     SPK and CK files between binary and text format:
!
!        SPCA2B    converts text to binary. It opens the text file,
!                  creates a new binary file, and closes both files.
!
!        SPCB2A    converts binary to text. It opens the binary file,
!                  creates a new text file, and closes both files.
!
!        SPCT2B    converts text to binary. It creates a new binary
!                  file and closes it. The text file is open on
!                  entrance and exit.
!
!        SPCB2T    converts binary to text. It opens the binary
!                  file and closes it. The text file is open on
!                  entrance and exit
!
!     See the SPC required reading for more information
!     about SPC routines and the SPK and CK file formats.
!
! $ Examples
!
!     1)  The following code fragment creates a text file containing
!         text format SPK data and comments preceded and followed
!         by a standard label.
!
!         The SPICELIB routine TXTOPN opens a new text file and TXTOPR
!         opens an existing text file for read access. TEXT and
!         BINARY are character strings that contain the names of the
!         text and binary files.
!
!            CALL TXTOPN ( TEXT, UNIT )
!
!            (Write header label to UNIT)
!
!            CALL SPCB2T ( BINARY, UNIT )
!
!            (Write trailing label to UNIT)
!
!            CLOSE ( UNIT )
!
!
!         The following code fragment reconverts the text format
!         SPK data and comments back into binary format.
!
!            CALL TXTOPR ( TEXT, UNIT )
!
!            (Read, or just read past, header label from UNIT)
!
!            CALL SPCT2B ( UNIT, BINARY )
!
!            (Read trailing label from UNIT, if desired )
!
!            CLOSE ( UNIT )
!
!
!     2)  Suppose three text format SPK files have been appended
!         together into one text file called THREE.TSP. The following
!         code fragment converts each set of data and comments into
!         its own binary file.
!
!            CALL TXTOPR ( 'THREE.TSP', UNIT  )
!
!            CALL SPCT2B ( UNIT, 'FIRST.BSP'  )
!            CALL SPCT2B ( UNIT, 'SECOND.BSP' )
!            CALL SPCT2B ( UNIT, 'THIRD.BSP'  )
!
!            CLOSE ( UNIT )
!
! $ Restrictions
!
!     1)  This routine assumes that the data and comments in the
!         text format SPK or CK file come from a binary file
!         and were written by one of the routines SPCB2A or SPCB2T.
!         Data and/or comments written any other way may not be
!         in the correct format and, therefore, may not be handled
!         properly.
!
!     2)  Older versions of SPK and CK files did not have a comment
!         area. These files, in text format, may still be converted
!         to binary using SPCT2B. However, upon exit, the file pointer
!         will not be in position ready to read the first line of text
!         after the data. Instead, the next READ statement after
!         calling SPCT2B will return the second line of text after
!         the data. Therefore, example 1 may not work as desired
!         if the trailing label begins on the first line after the
!         data. To solve this problem, use DAFT2B instead of SPCT2B.
!
!     3)  UNIT must be obtained via TXTOPR. Use TXTOPR to open text
!         files for read access and get the logical unit. System
!         dependencies regarding opening text files have been isolated
!         in the routines TXTOPN and TXTOPR.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     J.E. McLean        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
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
!     text SPK or CK to binary
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           ltrim
      integer(ip)           rtrim
      logical               return

!
!     Local parameters
!
      character(*), parameter :: bmark = '~NAIF/SPC BEGIN COMMENTS~'

      character(*), parameter :: emark = '~NAIF/SPC END COMMENTS~'

      integer(ip), parameter :: maxcpr = 1000
!
!     Local variables
!
      character(maxcpr)     line

      integer(ip)           handle
      integer(ip)           iostat
      integer(ip)           scrtch

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     DAFT2B creates the new binary file and writes the data to
!     it.  If the 'NAIF/DAF' keyword is not the first line that
!     it reads from the text file, it will signal an error.
!     Initially, no records are reserved.
!
      call daft2b ( unit, binary, 0 )

!
!     The comments follow the data and are surrounded by markers.
!     BMARK should be the next line that we read.  If it isn't,
!     then this is an old file, created before the comment area
!     existed.  In this case, we've read one line too far, but
!     we can't backspace because the file was written using list-
!     directed formatting (See the ANSI standard).  All we can do
!     is check out, leaving the file pointer where it is, but
!     that's better than signaling an error.
!
      read ( unit, fmt='(A)', iostat=iostat ) line

      if ( iostat > 0 ) then
         call setmsg ( 'Error reading the text file named FNM.  '     // &
                       'Value of IOSTAT is #.'                        )
         call errint ( '#', iostat                                    )
         call errfnm ( 'FNM', unit                                    )
         call sigerr ( 'SPICE(FILEREADFAILED)'                        )
         return
      end if


      if (   (  line ( ltrim(line): )  /=  bmark  )   .or. (  iostat                 <  0      )   ) return

!
!     We're not at the end of the file, and the line we read
!     is BMARK, so we write the comments to a scratch file.
!     We do this because we have to use SPCAC to add the comments
!     to the comment area of the binary file, and SPCAC rewinds
!     the file.  It's okay for SPCAC to rewind a scratch file,
!     but it's not okay to rewind the file connected to UNIT --
!     we don't know the initial location of the file pointer.
!
!     PEPPER: the unit comes from NEWUNIT= in the OPEN below.

      open ( newunit            =  scrtch, &
             form            = 'FORMATTED', &
             access          = 'SEQUENTIAL', &
             status          = 'SCRATCH', &
             iostat          =  iostat      )

      if ( iostat /= 0 ) then
         call setmsg ( 'Error opening a scratch file.  File name '    // &
                       'was FNM.  Value of IOSTAT is #.'              )
         call errint ( '#', iostat                                    )
         call errfnm ( 'FNM', scrtch                                  )
         call sigerr ( 'SPICE(FILEOPENERROR)'                         )
         return
      end if

      write ( scrtch, fmt='(A)', iostat=iostat ) line(1:rtrim(line))

      if ( iostat /= 0 ) then
         call setmsg ( 'Error writing to scratch file. File name '    // &
                       'is FNM.  Value of IOSTAT is #.'               )
         call errint ( '#', iostat                                    )
         call errfnm ( 'FNM', scrtch                                  )
         call sigerr ( 'SPICE(FILEWRITEFAILED)'                       )
         return
      end if

!
!     Continue reading lines from the text file and storing them
!     in the scratch file until we get to the end marker.
!
      do while (  line ( ltrim(line): ) /= emark  )

         read ( unit, fmt='(A)', iostat=iostat ) line

         if ( iostat /= 0 ) then
            call setmsg ( 'Error reading the text file named FNM.  ' // &
                          'Value of IOSTAT is #.'                    )
            call errint ( '#', iostat                                )
            call errfnm ( 'FNM', unit                                )
            call sigerr ( 'SPICE(FILEREADFAILED)'                    )
            return
         end if

         write ( scrtch, fmt='(A)', iostat=iostat ) line(1:rtrim(line))

         if ( iostat /= 0 ) then
            call setmsg ( 'Error writing to scratch file.  File name '// &
                          'is FNM.  Value of IOSTAT is #.'            )
            call errint ( '#', iostat                                 )
            call errfnm ( 'FNM', scrtch                               )
            call sigerr ( 'SPICE(FILEWRITEFAILED)'                    )
            return
         end if

      end do

!
!     Open the new binary file and add the comments that have been
!     stored temporarily in a scratch file.
!
      call dafopw ( binary, handle )

      call spcac  ( handle, scrtch, bmark, emark )

!
!     Close the files.  The scratch file is automatically deleted.
!
      call dafcls ( handle )

      close ( scrtch )

      return
      end
