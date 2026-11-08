!$Procedure WRENCI ( Write encoded integers to text file )

      subroutine wrenci ( unit, n, data )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Encode and write integers to a text file.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONVERSION
!     NUMBERS
!     UTILITY
!
! $ Declarations

      implicit none

      integer(ip)           unit
      integer(ip)           n
      integer(ip)           data(*)

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT      I    Fortran unit number of output text file.
!     N         I    Number of integers to encode and write.
!     DATA      I    List of integers to be encoded and written.
!
! $ Detailed_Input
!
!     UNIT     is the Fortran unit number for a previously opened text
!              file. All writing will begin at the CURRENT POSITION
!              in the text file.
!
!     N        is the number of integers to be encoded and written to
!              the text file attached to UNIT.
!
!     DATA     is the list of integers to be encoded and written to the
!              text file attached to UNIT.
!
! $ Detailed_Output
!
!     See the $Particulars section for a description of the effect of
!     this routine.
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
!     2)  If an error occurs while writing to the text file attached
!         to UNIT, the error SPICE(FILEWRITEFAILED) is signaled.
!
!     3)  If the Fortran logical unit UNIT is not defined, the results
!         of this routine are unpredictable.
!
! $ Files
!
!     See the description of UNIT in the $Detailed_Input section.
!
! $ Particulars
!
!     This routine will accept a list of one or more integers which
!     it will encode into equivalent text strings and write to the
!     current position in a text file. The current position in a file
!     is defined to be the text line immediately following the last
!     text line that was written or read. The encoded integers are
!     written to the output text file as quoted character strings so
!     that a Fortran list directed read may be used to read the
!     encoded values, rather than a formatted read with the format
!     specifier FMT = '(A)'.
!
!     This routine is one of a pair of routines which are used to
!     encode and decode integers:
!
!           WRENCI -- Encode and write integers to a file.
!           RDENCI -- Read and decode integers from a file.
!
!     The encoding/decoding of integers is performed to provide a
!     portable means for transferring data values.
!
!     Currently the text string produced will be a signed hexadecimal
!     number See INT2HX.FOR and HX2INT.FOR for details.
!
! $ Examples
!
!     Please note that the output format in the examples is not
!     intended to be exactly identical with the output format of this
!     routine in actual use. The output format used in the examples is
!     intended to aid in the understanding of how this routine works.
!     It is NOT intended to be a specification of the output format for
!     this routine.
!
!     Let
!
!        UNIT     be the Fortran logical unit of a previously opened
!                 text file.
!
!        N        = 100
!
!        DATA(I)  = I, I = 1, N
!
!     Then, the subroutine call
!
!           CALL WRENCI( UNIT, N, DATA )
!
!     will write the first 100 integers, encoded, to the output text
!     file attached to UNIT, beginning at the current position in the
!     output file, which is marked by an arrow, '-->'. The resulting
!     output will look something like the following:
!
!        -->'1' '2' '3' '4' '5' '6' '7' '8' '9' 'A' 'B' 'C' 'D' 'E'
!           'F' '10' '11' '12' '13' '14' '15' '16' '17' '18' '19'
!           '1A' '1B' '1C' '1D' '1E' '1F' '20' '21' '22' '23' '24'
!           '25' '26' '27' '28' '29' '2A' '2B' '2C' '2D' '2E' '2F'
!           '30' '31' '32' '33' '34' '35' '36' '37' '38' '39' '3A'
!           '3B' '3C' '3D' '3E' '3F' '40'
!           '41' '42' '43' '44' '45' '46' '47' '48' '49' '4A' '4B'
!           '4C' '4D' '4E' '4F' '50' '51' '52' '53' '54' '55' '56'
!           '57' '58' '59' '5A' '5B' '5C' '5D' '5E' '5F' '60' '61'
!           '62' '63' '64'
!        -->
!
!     At this point, the arrow marks the position of the file pointer
!     immediately after the call to WRENCI.
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
!-    SPICELIB Version 1.3.0, 13-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.2.0, 09-SEP-1993 (KRG)
!
!        The list directed write was changed to a formatted write using
!        the  specifier FMT='(A)'. This was done in order to prevent a
!        space from appearing as the first character on each line of the
!        file for certain computer platforms.
!
!-    SPICELIB Version 1.1.0, 21-JUN-1993 (KRG)
!
!        This routine was modified to avoid the creation of long output
!        lines on some of the supported systems, such as the NeXT with
!        Absoft Fortran 3.2.
!
!        A disclaimer was added to the $Examples section concerning
!        the output format used. The disclaimer simply states that the
!        output format used in the example is not necessarily the
!        output format actually used by the routine.
!
!-    SPICELIB Version 1.0.0, 19-OCT-1992 (KRG)
!
!-&


! $ Index_Entries
!
!     encode and write integers to a text file
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.2.0, 09-SEP-1993 (KRG)
!
!        The list directed write was changed to a formatted write using
!        the  specifier FMT='(A)'. This was done in order to prevent a
!        space from appearing as the first character on each line of the
!        file for certain computer platforms.
!
!-    SPICELIB Version 1.1.0, 21-JUN-1993 (KRG)
!
!        This routine was modified to avoid the creation of long output
!        lines on some of the supported systems, such as the NeXT with
!        Absoft Fortran 3.2.
!
!        On some of the supported computers this routine would produce
!        very long (greater than 1000 characters) output lines due to
!        the implicit DO loop used in the WRITE statement:
!
!            WRITE (UNIT,IOSTAT=IOSTAT,FMT=*)
!           .   ( QUOTE//WORK(I)(1:LENGTH(I))//QUOTE//' ', I=1,NITMS )
!
!        This problem was fixed by removing the implicit DO loop from
!        the WRITE statement and placing an equivalent DO loop around
!        the WRITE statement:
!
!            DO I = 1, NITMS
!               WRITE (UNIT,IOSTAT=IOSTAT,FMT=*)
!           .       QUOTE//WORK(I)(1:LENGTH(I))//QUOTE
!            END DO
!
!        The net effect of this will be that only a single datum will
!        be written on each line of output.
!
!        A disclaimer was added to the $Examples section concerning
!        the output format used. The disclaimer simply states that the
!        output format used in the example is not necessarily the
!        output format actually used by the routine.
!
!-    SPICELIB Version 1.0.0, 19-OCT-1992 (KRG)
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!
      character(1)          quote
      parameter           ( quote = '''' )

      integer(ip)           maxenc
      parameter           ( maxenc = 64 )

      integer(ip)           wrksiz
      parameter           ( wrksiz = 64 )
!
!     Local variables
!
      character(maxenc)     work(wrksiz)

      integer(ip)           i
      integer(ip)           iostat
      integer(ip)           itmbeg
      integer(ip)           length(wrksiz)
      integer(ip)           nitms


!
!     Standard SPICE error handling.
!
      if ( return () ) return
!
!     Check to see if the number of data items is less than or equal
!     to zero. If it is, signal an error.
!
      if ( n < 1 ) then

         call setmsg ( 'The number of data items to be written was' // &
                       ' not positive: #.'                           )
         call errint ( '#', n                                        )
         call sigerr ( 'SPICE(INVALIDARGUMENT)'                      )
         return

      end if
!
!     Initialize the beginning location for the data items to be
!     encoded.
!
      itmbeg = 1
!
!     Begin encoding the input data items in blocks of size NITMS.
!     Each time the number of data items NITMS is reached, write
!     out the encoded items in the workspace.
!
      do while ( itmbeg <= n )
!
!        The number of items is either the size of the workspace, or
!        the number of data items which remain to be processed, which
!        should always be less than or equal to the size of the
!        workspace.
!
         nitms = min ( wrksiz, n - itmbeg + 1 )
!
!        Encode each of the numbers into an equivalent character string.
!
         do i = 1, nitms

            call int2hx ( data(itmbeg + i - 1), work(i), length(i) )

         end do
!
!        Write out the current workspace, placing single quotes around
!        each of the character strings so that they may be read using
!        Fortran list directed read statements rather than the format
!        specifier FMT = '(A)'.
!
         do i = 1, nitms

            write ( unit,fmt='(A)',iostat=iostat ) &
               quote//work(i)(1:length(i))//quote
!
!           Check to see if we got a write error, IOSTAT .NE. 0.
!
            if ( iostat /= 0 ) then

               call setmsg ( 'Error writing to logical unit #,'  // &
                             ' IOSTAT = #.'                       )
               call errint ( '#', unit                            )
               call errint ( '#', iostat                          )
               call sigerr ( 'SPICE(FILEWRITEFAILED)'             )
               return

            end if

         end do
!
!        Position the data item pointer at the next location to begin
!        encoding the items in the array DATA, and continue processing
!        the data items until done.
!
         itmbeg = itmbeg + nitms

      end do

      return
      end
