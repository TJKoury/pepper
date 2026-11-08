!$Procedure RDENCI  ( Read encoded integers from text file )

      SUBROUTINE RDENCI ( UNIT, N, DATA )

! $ Abstract
!
!     Read N encoded integers from a text file, decoding them into
!     their equivalent integers.
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

      IMPLICIT NONE

      INTEGER               UNIT
      INTEGER               N
      INTEGER               DATA(*)

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UNIT      I    Fortran unit number of input text file.
!     N         I    Number of integers to read and decode.
!     DATA      O    List of decoded integers.
!
! $ Detailed_Input
!
!     UNIT     is the Fortran unit number for a previously opened text
!              file. All reading will begin at the CURRENT POSITION
!              in the text file.
!
!     N        is the number of encoded integers to be read from the
!              text file attached to UNIT.
!
! $ Detailed_Output
!
!     DATA     is the list of decoded integers read from the text file
!              attached to UNIT.
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
!     2)  If an error occurs while reading from the text file attached
!         to UNIT, the error SPICE(FILEREADFAILED) is signaled.
!
!     3)  If an error occurs while decoding a number, the error
!         SPICE(DECODINGERROR) is signaled.
!
! $ Files
!
!     See the description of UNIT in $Detailed_Input.
!
! $ Particulars
!
!     This routine will read N encoded integers beginning at the
!     current position in a previously opened text file. The current
!     position in a file is defined to be the text line immediately
!     following the last text line that was written or read. The
!     integers will be decoded and placed into a list of integers
!     which will be passed back to the caller. The encoded integers
!     are represented as quoted character strings so that a Fortran
!     list directed read may be used to read the encoded values,
!     rather than a formatted read with the format specifier
!     FMT = '(A)'.
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
!     Currently the encoded integers are represented as signed
!     hexadecimal numbers See INT2HX.FOR and HX2INT.FOR for details.
!
! $ Examples
!
!     Suppose we have the following input file which contains the values
!     1 - 100 encoded, and that the input file has already been opened
!     for reading. The arrow, '-->', indicates the current position in
!     the file.
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
!
!     Then, the following code fragment would read and decode these
!     100 values.
!
!           N = 100
!           CALL RDENCI( UNIT, N, DATA )
!
!      Upon returning, the array data would contain the values 1 - 100.
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
!-    SPICELIB Version 1.1.0, 03-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Fixed I/O type
!        of argument DATA in $Brief_I/O table.
!
!-    SPICELIB Version 1.0.0, 19-OCT-1992 (KRG)
!
!-&


! $ Index_Entries
!
!     read and decode encoded integers from a text file
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
!
!     Local parameters
!
      INTEGER               ERRLEN
      PARAMETER           ( ERRLEN = 80 )

      INTEGER               MAXENC
      PARAMETER           ( MAXENC = 64 )

      INTEGER               WRKSIZ
      PARAMETER           ( WRKSIZ = 64 )
!
!     Local variables
!
      CHARACTER*(ERRLEN)    ERRMSG
      CHARACTER*(MAXENC)    WORK(WRKSIZ)

      INTEGER               I
      INTEGER               IOSTAT
      INTEGER               ITMBEG
      INTEGER               NITMS

      LOGICAL               ERROR


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RDENCI' )
      END IF
!
!     Check to see if the number of data items is less than or equal
!     to zero. If it is, signal an error.
!
      IF ( N .LT. 1 ) THEN

         CALL SETMSG ( 'The number of data items to be read was' // &
                       ' not positive: #.'                        )
         CALL ERRINT ( '#', N                                     )
         CALL SIGERR ( 'SPICE(INVALIDARGUMENT)'                   )
         CALL CHKOUT ( 'RDENCI'                                   )
         RETURN

      END IF
!
!     Initialize the beginning location to place the decoded data
!     items.
!
      ITMBEG = 1
!
!     We read in the encoded numbers in blocks of size WRKSIZ, and if
!     there was not a read error we will attempt to decode the numbers.
!     We signal an error if either:
!
!                (1) there is a read error
!                (2) there is an error decoding a number.
!
!     NOTE: EOF is interpreted as a read error because we know a priori
!           exactly how many data items we need to read: N.
!
!     Begin decoding the encoded data items read from the input file
!     in blocks of size NITMS. Each time the number of data items
!     NITMS is reached, decode the encoded numbers into the data array.
!
      DO WHILE ( ITMBEG .LE. N )
!
!        The number of items is either the size of the workspace, or
!        the number of data items which remain to be processed, which
!        should always be less than or equal to the size of the
!        workspace.
!
         NITMS = MIN ( WRKSIZ, N - ITMBEG + 1 )
!
!        Read in a block of data items to be decoded.
!
         READ (UNIT,*,IOSTAT=IOSTAT) ( WORK(I), I = 1, NITMS )
!
!        Check to see if we got a read error: IOSTAT .NE. 0. If we did,
!        then signal an error. EOF is considered to be a read error,
!        since we know exactly how many data items we expect to read.
!
         IF ( IOSTAT .NE. 0 ) THEN

            CALL SETMSG ( 'Error reading from logical unit #,' // &
                          ' IOSTAT = #.'                        )
            CALL ERRINT ( '#', UNIT                             )
            CALL ERRINT ( '#', IOSTAT                           )
            CALL SIGERR ( 'SPICE(FILEREADFAILED)'               )
            CALL CHKOUT ( 'RDENCI'                              )
            RETURN

         END IF
!
!        Begin to decode the data items into the data array. Signal an
!        error if we cannot decode a data item.
!
         DO I = 1, NITMS

            CALL HX2INT ( WORK(I), DATA(ITMBEG + I - 1), ERROR, ERRMSG )
            IF ( ERROR ) THEN

               CALL SETMSG ( 'Decoding error occurred while'       // &
                             ' attempting to decode item #: #. #'   )
               CALL ERRINT ( '#', I                                 )
               CALL ERRCH  ( '#', WORK(I)                           )
               CALL ERRCH  ( '#', ERRMSG                            )
               CALL SIGERR ( 'SPICE(DECODINGERROR)'                 )
               CALL CHKOUT ( 'RDENCI'                               )
               RETURN

            END IF

         END DO
!
!        Position the data item pointer at the next location to begin
!        placing the decoded items in the array DATA, and continue
!        processing the until done.
!
         ITMBEG = ITMBEG + NITMS

      END DO

      CALL CHKOUT ( 'RDENCI' )
      RETURN
      END
