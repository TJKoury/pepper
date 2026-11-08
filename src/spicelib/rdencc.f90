!$Procedure RDENCC  ( Read encoded characters from a text file )

      subroutine rdencc ( unit, n, data )

! $ Abstract
!
!     Read and decode encoded characters from a text file.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTERS
!     CONVERSION
!     UTILITY
!
! $ Declarations

      implicit none

      integer               unit
      integer               n
      character*(*)         data(*)

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT       I   Fortran unit number of input text file.
!     N          I   Number of characters to be read and decoded.
!     DATA       O   List of decoded characters to be returned.
!
! $ Detailed_Input
!
!     UNIT     is the Fortran unit number for a previously opened text
!              file. All reading will begin at the CURRENT POSITION
!              in the text file.
!
!     N        is the number of characters to be read from the text file
!              attached to UNIT.
!
! $ Detailed_Output
!
!     DATA     is the list of characters which were read from the text
!              file attached to UNIT and decoded.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If N, the number of data items, is not positive, the error
!         SPICE(INVALIDARGUMENT) is signaled.
!
!     2)  If an error occurs while reading from the text file
!         attached to UNIT, the error SPICE(FILEREADFAILED) is signaled.
!
!     3)  If an error occurs while decoding a character, the error
!         SPICE(DECODINGERROR) is signaled.
!
! $ Files
!
!     See the description of UNIT in $Detailed_Input.
!
! $ Particulars
!
!     This routine will read quoted character strings of length
!     MAXENC containing encoded characters produced by the routine
!     WRENCC, or some equivalent procedure. The reading begins at
!     the current position in a previously opened text file attached
!     to logical UNIT and continues until N contiguous characters
!     have been successfully decoded and placed in the data buffer
!     DATA or an error occurs. The current position in a file is
!     defined to be the text line immediately following the last text
!     line that was written or read.
!
!     The character strings are quoted so that a Fortran list directed
!     read may be used to read them, rather than a formatted read with
!     the format specifier FMT = '(A)'.
!
!     As the characters are decoded they are placed into the first N
!     contiguous positions in the data buffer DATA, where the first N
!     contiguous positions are determined by moving from the lowest
!     array indices to highest array indices, i.e., moving from ``left''
!     to ``right'' and ``top'' to ``bottom'' in the character array
!     DATA, beginning at the first character position, DATA(1)(1:1). So,
!     logically all of the quoted strings containing encoded data can
!     be thought of as being concatenated together into one long
!     character string.
!
!     This routine is one of a pair of routines which are used to
!     encode and decode ASCII characters:
!
!           WRENCC -- Encode and write ASCII characters to a file.
!           RDENCC -- Read and decode ASCII characters from a file.
!
!     The encoding/decoding of characters is performed to provide
!     a portable means for transferring character data values.
!
!     This routine is for use with the ASCII character set and
!     extensions to it. The supported characters must have decimal
!     values in the range from 0 to 255.
!
! $ Examples
!
!     The following examples demonstrate the use of this routine. In
!     each of the examples, the variable UNIT is the Fortran logical
!     unit of a previously opened text file, and the variable N is
!     an integer which will represent the number of characters to be
!     read and decoded.
!
!     The first example demonstrates a typical correct usage of this
!     routine. The second example demonstrates what would probably be
!     the most common incorrect usage of this routine. These examples
!     are meant to be illustrative, so for the sake of brevity and
!     clarity, the length of the quoted strings expected in the input
!     text file has been shortened.
!
!     The examples use as data correctly and incorrectly encoded
!     versions of the following character string which has a length
!     of exactly 64 characters:
!
!        'Here is some data. What follows is more '//
!        'data. This is more data.                '
!
!     Example 1
!     ---------
!
!        This example demonstrates a typical usage of this routine.
!
!        Let the symbol '-->' denote the file pointer.
!
!        Let the current file pointer position and succeeding data be
!        the following:
!
!           --> 'Here is some data. W'
!               'hat follows is more '
!               'data. This is more d'
!               'ata.                '
!
!        There are exactly N = 64 characters of encoded data.
!
!        Let the character data buffer have the following
!        declaration in the calling program:
!
!           CHARACTER*(40)         DATA(2)
!
!        Then, the subroutine call
!
!           CALL RDENCC( UNIT, N, DATA )
!
!        with N = 64 would produce the following results:
!
!           DATA(1) = 'Here is some data. What follows is more '
!           DATA(2) = 'data. This is more data.'
!
!     Example 2
!     ---------
!
!        This example is meant to demonstrate what would probably be
!        a common misuse of this routine.
!
!        Let the symbol '-->' denote the file pointer.
!
!        Let the current file pointer position and succeeding data be
!        the following:
!
!           --> 'Here is some data.  '
!               'What follows is more'
!               'data. This is more  '
!               'data.               '
!
!        As in example 1, there are exactly N = 64 characters of
!        encoded data, but to make the data more ``readable'' two extra
!        spaces have been added: one at the end of the first line and
!        one at the end of the third line.
!
!        Let the character data buffer have the following
!        declaration in the calling program:
!
!           CHARACTER*(40)         DATA(2)
!
!        Then, the subroutine call
!
!           CALL RDENCC( UNIT, N, DATA )
!
!        with N = 64 would produce the following results:
!
!           DATA(1) = 'Here is some data.  What follows is more'
!           DATA(2) = ' data. This is  more dat'
!
!        This is probably not what was desired. The problem is that
!        the ``significant'' characters in the encoded string do not
!        appear contiguously; an ``extra'' blank appears at the end
!        of the first and third encoded quoted strings.
!
!     Example 3
!     ---------
!
!        This example demonstrates the use of WRENCC and RDENCC for
!        writing and subsequent reading of character data using data
!        buffers that are ``shaped'' differently, i.e., that have
!        different dimensions.
!
!        Let the input and output character data buffers have the
!        following declarations:
!
!           CHARACTER*(25)  OUTBUF(3)
!           CHARACTER*(10)  INPBUF(7)
!
!        Further, let the output buffer contain the following data:
!
!           OUTBUF(1) = 'Today is the first day of'
!           OUTBUF(2) = ' the rest of my life, so '
!           OUTBUF(3) = 'I will enjoy it.'
!
!        There are exactly N = 66 significant characters in the output
!        buffer. The code fragment
!
!           N = 66
!           CALL WRENCC ( UNIT, N, OUTBUF )
!           REWIND ( UNIT )
!           CALL RDENCC ( UNIT, N, INPBUF )
!
!        has the effect of placing the original data into the
!        differently ``shaped'' input buffer with the following
!        results:
!
!           INPBUF(1) = 'Today is t'
!           INPBUF(2) = 'he first d'
!           INPBUF(3) = 'ay of the '
!           INPBUF(4) = 'rest of my'
!           INPBUF(5) = ' life, so '
!           INPBUF(6) = 'I will enj'
!           INPBUF(7) = 'oy it.    '
!
!       No information has been lost, it is simply arranged differently.
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
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 20-OCT-1992 (KRG)
!
!-&


! $ Index_Entries
!
!     read encoded characters from a text file
!
!-&


!
!     SPICELIB functions
!
      logical               return
!
!     Local parameters
!
      character*(1)         escchr
      parameter           ( escchr = '@')

      integer               mxescd
      parameter           ( mxescd = 2 )

      integer               maxenc
      parameter           ( maxenc = 64 )

      integer               errlen
      parameter           ( errlen = 80 )
!
!     Local variables
!
      character*(1)         ch
      character*(maxenc)    encchr
      character*(errlen)    errmsg
      character*(mxescd)    hexnum


      integer               dtalen
      integer               dtalin
      integer               dtapos
      integer               encpos
      integer               intch
      integer               iostat
      integer               nchars
      integer               nescd

      logical               error
      logical               escape


!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'RDENCC' )
      end if
!
!     Check to see if the number of data items is less than or equal
!     to zero. If it is, signal an error.
!
      if ( n .lt. 1 ) then

         call setmsg ( 'The number of data items to be read was' // &
                       ' not positive: #.'                        )
         call errint ( '#', n                                     )
         call sigerr ( 'SPICE(INVALIDARGUMENT)'                   )
         call chkout ( 'RDENCC'                                   )
         return

      end if
!
!     Initialize some stuff here
!
!     Make sure that the encoding character string is empty when we
!     start.
!
      encchr = ' '
!
!     We have not encountered any errors yet, so set the error indicator
!     to .FALSE..
!
      error = .false.
!
!     Get the length of a data ``line'' in the data buffer DATA.
!
      dtalen = len( data(1) )
!
!     We are not currently parsing an escaped character, so set the
!     escape indicator to .FALSE. and set the number of escape digits
!     to zero.
!
      escape = .false.
      nescd  = 0
!
!     Set the initial line and position for the output data buffer.
!
      dtapos = 1
      dtalin = 1
!
!     Set the initial position in the encoding buffer to be 1 too
!     big so that we read an encoded character string from the file
!     attached to UNIT on the first pass through the loop.
!
      encpos = maxenc + 1
!
!     Set the number of characters decoded to zero and begin the
!     decoding loop.
!
      nchars = 0
      do while ( nchars .lt. n )
!
!        If the last character we processed was the last one in the
!        encoded character string, then we need to read in the next
!        encoded character string from the file. This also accomplishes
!        the task of reading in the first encoded character string.
!
         if ( encpos .gt. maxenc ) then

            read(unit,*,iostat=iostat) encchr
            if ( iostat .ne. 0 ) then

               call setmsg ( 'Error reading from logical unit #,' // &
                             ' IOSTAT = #.'                        )
               call errint ( '#', unit                             )
               call errint ( '#', iostat                           )
               call sigerr ( 'SPICE(FILEREADFAILED)'               )
               call chkout ( 'RDENCC'                              )
               return

            end if
!
!           Set the pointer for the encoded character buffer to the
!           beginning of the buffer.
!
            encpos = 1

         end if

         ch = encchr(encpos:encpos)
!
!        If we are processing a character which was escaped when it was
!        encoded, we need to do some special stuff.
!
         if ( escape ) then

            nescd = nescd + 1

            if ( nescd .eq. mxescd ) then
!
!              If we have all of the digits in the encoded character,
!              then decode it.
!
               hexnum(nescd:nescd) = ch
               call hx2int( hexnum, intch, error, errmsg )

               if ( error ) then

                  call setmsg ( 'Decoding error occurred while'       // &
                                ' attempting to decode item #: @#. #'  )
                  call errint ( '#', nchars+1                          )
                  call errch  ( '#', hexnum                            )
                  call errch  ( '#', errmsg                            )
                  call sigerr ( 'SPICE(DECODINGERROR)'                 )
                  call chkout ( 'RDENCC'                               )
                  return

               end if

               ch = char( intch )
!
!              We now have the decoded character. We are no longer
!              processing an escaped character, so set the escape
!              indicator to .FALSE. and continue. The character we
!              just decoded will be placed into the data buffer DATA
!              below.
!
               escape = .false.
               nescd  = 0

            else if ( ( nescd .lt. mxescd ) .and. &
                      ( nescd .gt. 0      ) ) then
!
!              Otherwise we are still collecting the digits of the
!              encoded character, so store the current character and
!              move on to the next one.
!
               hexnum(nescd:nescd) = ch

            end if

         else
!
!           Check to see if the current character is the escape
!           character. If it is, we need to set the escape indicator
!           to .TRUE. so that we correctly process the encoded
!           digits.
!
            if ( ch .eq. escchr ) then

               escape = .true.

            end if

         end if
!
!        At this point one of the following is true:
!
!           (1) CH contains a character to be placed into the data
!               buffer DATA.
!
!           (2) We are currently building an escaped character from
!               its escape sequence, ESCAPE = .TRUE., and CH contains
!               some part of the escape sequence.
!
!        If we are not currently decoding an escaped character, then
!        we need to store the character value that we have in the data
!        buffer, and move on to the next character.
!
         if ( .not. escape ) then

            nchars = nchars + 1
!
!           If the position in the data buffer is greater than the
!           length  of a data line (DTALEN) then we need to increment
!           the current data line (DTALIN) and reset the current data
!           line buffer position (DTAPOS).
!
            if ( dtapos .gt. dtalen ) then

               dtalin = dtalin + 1
               dtapos = 1

            end if
!
!           Store the current character in the data buffer and
!           increment the buffer position.
!
            data(dtalin)(dtapos:dtapos) = ch
            dtapos                      = dtapos + 1

         end if
!
!        Increment the encoded character buffer position
!
         encpos = encpos + 1
!
!        At this point, we know the following:
!
!        (1)  1 <= ENCPOS <= MAXENC
!        (2)  1 <= NCHARS <= N
!        (3)  1 <= DTAPOS <= DTALEN
!        (4)  1 <= DTALIN
!        (5)  0 <= NESCD <= MXESCD
!        (6)  ESCAPE is .TRUE. if we are currently decoding an escaped
!             character, otherwise it is .FALSE..
!
      end do

      call chkout ( 'RDENCC' )
      return
      end
