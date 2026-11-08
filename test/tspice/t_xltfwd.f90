!$Procedure T_XLTFWD ( Translate For Write, Double Precision Values )

      SUBROUTINE T_XLTFWD ( INPUT, NUMDP, OUTBFF, OUTPUT )

! $ Abstract
!
!     Translate double precision values from the native binary file
!     format to a non-native format for write.
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
!     TEST ROUTINE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      DOUBLE PRECISION      INPUT  ( * )
      INTEGER               NUMDP
      INTEGER               OUTBFF
      CHARACTER*(*)         OUTPUT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INPUT      I   List of d.p. values to be translated.
!     NUMDP      I   Number of integers in INPUT to convert.
!     OUTBFF     I   Binary file format code for d.p. values in OUTPUT.
!     OUTPUT     O   Character string containing the translated values.
!
! $ Detailed_Input
!
!     INPUT      is an array of double precision numbers containing the
!                values to be translated into a non-native binary format
!                and stored in OUTPUT.
!
!     NUMDP      is the number of double precision values to convert to
!                non-native binary file format and store in OUTPUT.
!
!     OUTBFF     is an integer code that indicates the binary file
!                format targeted for OUTPUT.  Acceptable values
!                are the parameters:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!                as defined in the include file 'zzddhman.inc'.
!
! $ Detailed_Output
!
!     OUTPUT     is a string containing a group of double precision
!                values converted from the native binary file format
!                to OUTBFF and encoded byte by byte as characters.
!                This string should be at least the number of bytes
!                used to store an integer multiplied by NUMDP in
!                length.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     This routine may signal SPICE(BUG) if the inputs are not
!     proper.
!
! $ Particulars
!
!     This routine converts double precision values from the native
!     binary file format to a non-native one.  The results are
!     encoded in a character string that can be written directly
!     to a file.
!
!     Note: This code is not a model of perfection.  It is hacked
!     together from pieces of code written elsewhere to support
!     creation of non-native binaries for testing purposes only.
!
! $ Examples
!
!     See F_XLATED for usage.
!
! $ Restrictions
!
!     1) Numeric data when written as characters to a file preserve
!        the bit patterns present in memory in the file.
!
!     2) A byte is 8 bits, and a character is some multiple of
!        bytes.
!
!     3) The intrinsic ICHAR preserves the bit pattern of the character
!        byte read from a file.  Namely if one examines the integer
!        found in the character.
!
!     4) The size of integers on the target environment are a multiple
!        of some number of bytes.
!
!     5) The length of the OUTPUT string is a multiple of the number
!        of bytes for an integer in the OUTBFF format.
!
!     6) OUTBFF is supported for reading on this platform, and not
!        equivalent to NATBFF on this platform.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 19-OCT-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               ISRCHC
      LOGICAL               RETURN

!
!     Local Variables
!
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(STRSIZ)    TMPSTR

      DOUBLE PRECISION      DEQUIV

      INTEGER               I
      INTEGER               J
      INTEGER               LENOPT
      INTEGER               NATBFF
      INTEGER               SPACE
      INTEGER               VALUE
      INTEGER               IEQUIV ( 2 )

      LOGICAL               FIRST

!
!     Equivalence DEQUIV to IEQUIV
!
      EQUIVALENCE         ( DEQUIV, IEQUIV )

!
!     Saved Variables
!
      SAVE                  FIRST
      SAVE                  NATBFF
      SAVE                  STRBFF

!
!     Data Statements
!
      DATA                  FIRST  / .TRUE. /
      DATA                  NATBFF / 0      /

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_XLTFWD' )
      END IF

!
!     Perform some initialization tasks.
!
      IF ( FIRST ) THEN

!
!        Populate STRBFF with the appropriate binary file
!        format labels.
!
         DO I = 1, NUMBFF
            CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
         END DO

!
!        Fetch the native binary file format.
!
         CALL ZZPLATFM ( 'FILE_FORMAT', TMPSTR )
         CALL UCASE    ( TMPSTR,        TMPSTR )

         NATBFF = ISRCHC ( TMPSTR, NUMBFF, STRBFF )

         IF ( NATBFF .EQ. 0 ) THEN

            CALL SETMSG ( 'The binary file format, ''#'', is not ' &
            //            'supported by this version of the toolkit. ' &
            //            'This is a serious problem, contact NAIF.'   )
            CALL ERRCH  ( '#', TMPSTR                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'T_XLTFWD'                                   )
            RETURN

         END IF

!
!        Do not perform initialization tasks again.
!
         FIRST = .FALSE.

      END IF

!
!     Check to see if OUTBFF is valid.  This should never occur if this
!     routine is called properly.
!
      IF ( ( OUTBFF .LT. 1 ) .OR. ( OUTBFF .GT. NUMBFF ) ) THEN

         CALL SETMSG ( 'The integer code used to indicate the ' &
         //            'binary file format of the input integers, ' &
         //            '#, is out of range.  This error should ' &
         //            'never occur.'                               )
         CALL ERRINT ( '#', OUTBFF                                  )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'T_XLTFWD'                                   )
         RETURN

      END IF

!
!     Get the length of the input string.
!
      LENOPT = LEN(OUTPUT)

!
!     Now branch depending on the value of NATBFF.
!
      IF ( NATBFF .EQ. BIGI3E ) THEN

         IF ( OUTBFF .EQ. LTLI3E ) THEN

!
!           Before we go any further check to see that the length
!           of the output string is appropriate and we have enough
!           room to store the results.  Since this string is to contain
!           LTL-IEEE double precision numbers and this is a BIG-IEEE
!           machine, characters are 1-byte and double precision numbers
!           are 8-bytes.  So the length of OUTPUT must be a multiple
!           of 8.
!
            SPACE = LENOPT / 8

            IF ( LENOPT - ( SPACE*8 ) .NE. 0 ) THEN

               CALL SETMSG ( 'The output string that is to be ' &
               //            'translated from the binary format ' &
               //            '# to format # has a length that is ' &
               //            'not a multiple of 8 bytes.  This ' &
               //            'error should never occur.'           )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                   )
               CALL ERRCH  ( '#', STRBFF(OUTBFF)                   )
               CALL SIGERR ( 'SPICE(BUG)'                          )
               CALL CHKOUT ( 'T_XLTFWD'                            )
               RETURN

            END IF

!
!           Now check to see that there is enough room to store
!           the number of integers we are to convert.
!
            IF ( NUMDP .GT. SPACE ) THEN

               CALL SETMSG ( 'The caller specified that # double ' &
               //            'precision numbers are to be translated ' &
               //            'from binary format # to #.  However ' &
               //            'there is only room to hold # numbers ' &
               //            'in the output array.  This error should ' &
               //            'never occur.' )
               CALL ERRINT ( '#', NUMDP                               )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                      )
               CALL ERRCH  ( '#', STRBFF(OUTBFF)                      )
               CALL ERRINT ( '#', SPACE                               )
               CALL SIGERR ( 'SPICE(BUG)'                             )
               CALL CHKOUT ( 'T_XLTFWD'                               )
               RETURN

            END IF

!
!           Start looping over each double precision number and
!           converting them to the 8-byte character packages to
!           be stored in OUTPUT.
!
            DO I = 1, NUMDP

!
!              Compute the substring index of the first character
!              in OUTPUT for this double precision number.
!
               J = 8*(I-1) + 1

!
!              Now extract and arrange the bytes properly.
!              Since these characters are to be stored in a file
!              utilizing LTL-IEEE, we know that J is the
!              least significant byte and that (J+7) is the
!              most significant.
!
!              INPUT:
!
!                      -------------------------------------
!                 . . .|     |  J  | J+1 | J+2 | J+3 |     |. . .
!                      -------------------------------------
!
!                   OUTPUT(J:J) = CHAR( INPUT(I)'s LSB )
!                      .
!                      .
!                   OUTPUT(J+7:J+7) = CHAR( INPUT(I)'s MSB )
!
!              Perform the necessary computations.
!
               CALL MOVED ( INPUT(I), 1, DEQUIV )

               VALUE = ISHFT( IEQUIV(2),  24 )
               VALUE = ISHFT( VALUE,     -24 )
               OUTPUT(J:J) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(2),  16 )
               VALUE = ISHFT( VALUE,     -24 )
               OUTPUT((J+1):(J+1)) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(2),   8 )
               VALUE = ISHFT( VALUE,     -24 )
               OUTPUT((J+2):(J+2)) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(2), -24 )
               OUTPUT((J+3):(J+3)) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(1),  24 )
               VALUE = ISHFT( VALUE,     -24 )
               OUTPUT((J+4):(J+4)) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(1),  16 )
               VALUE = ISHFT( VALUE,     -24 )
               OUTPUT((J+5):(J+5)) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(1),   8 )
               VALUE = ISHFT( VALUE,     -24 )
               OUTPUT((J+6):(J+6)) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(1), -24 )
               OUTPUT((J+7):(J+7)) = CHAR(VALUE)

            END DO

         ELSE

            CALL SETMSG ( 'Unable to translate double precision ' &
            //            'numbers from binary file format # to #. ' &
            //            'This error should never occur and is ' &
            //            'indicative of a bug.  Contact NAIF.'      )
            CALL ERRCH  ( '#', STRBFF(NATBFF)                        )
            CALL ERRCH  ( '#', STRBFF(OUTBFF)                        )
            CALL SIGERR ( 'SPICE(BUG)'                               )
            CALL CHKOUT ( 'T_XLTFWD'                                 )
            RETURN

         END IF

      ELSE IF ( NATBFF .EQ. LTLI3E ) THEN

         IF ( OUTBFF .EQ. BIGI3E ) THEN

!
!           Before we go any further check to see that the length
!           of the output string is appropriate and we have enough
!           room to store the results.  Since this string is to
!           contain BIG-IEEE double precision values and this is a
!           LTL-IEEE machine, characters are 1-byte and double
!           precision values are 8-bytes.  So the length of OUTPUT
!           must be a multiple of 8.
!
            SPACE = LENOPT / 8

            IF ( LENOPT - ( SPACE*8 ) .NE. 0 ) THEN

               CALL SETMSG ( 'The output string that is to be ' &
               //            'translated from the binary format ' &
               //            '# to format # has a length that is ' &
               //            'not a multiple of 8 bytes.  This ' &
               //            'error should never occur.'           )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                   )
               CALL ERRCH  ( '#', STRBFF(OUTBFF)                   )
               CALL SIGERR ( 'SPICE(BUG)'                          )
               CALL CHKOUT ( 'T_XLTFWD'                            )
               RETURN

            END IF

!
!           Now check to see that there is enough room to store
!           the number of integers we are to convert.
!
            IF ( NUMDP .GT. SPACE ) THEN

               CALL SETMSG ( 'The caller specified that # double ' &
               //            'precision numbers are to be ' &
               //            'translated from binary format # to #. ' &
               //            'However there is only room to hold # ' &
               //            'numbers in the output array.  This ' &
               //            'error should never occur.'              )
               CALL ERRINT ( '#', NUMDP                               )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                      )
               CALL ERRCH  ( '#', STRBFF(OUTBFF)                      )
               CALL ERRINT ( '#', SPACE                               )
               CALL SIGERR ( 'SPICE(BUG)'                             )
               CALL CHKOUT ( 'T_XLTFWD'                               )
               RETURN

            END IF

!
!           Start looping over each double precision number and
!           converting them to the 8-byte character packages to
!           be stored in OUTPUT.
!
            DO I = 1, NUMDP

!
!              Compute the substring index of the first character
!              in OUTPUT for this double precision number.
!
               J = 8*(I-1) + 1

!
!              Now extract and arrange the bytes properly.
!              Since these characters are to be stored in a file
!              utilizing BIG-IEEE, we know that J is the most
!              significant byte and that (J+7) is the least
!              significant.
!
!              INPUT:
!
!                      -------------------------------------
!                 . . .|     |  J  | J+1 | J+2 | J+3 |     |. . .
!                      -------------------------------------
!
!                   OUTPUT(J:J) = CHAR( INPUT(I)'s MSB )
!                      .
!                      .
!                   OUTPUT(J+7:J+7) = CHAR( INPUT(I)'s LSB )
!
!              Perform the necessary computations.
!
               CALL MOVED ( INPUT(I), 1, DEQUIV )

               VALUE = ISHFT( IEQUIV(2), -24 )
               OUTPUT(J:J) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(2),   8 )
               VALUE = ISHFT( VALUE,     -24 )
               OUTPUT((J+1):(J+1)) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(2),  16 )
               VALUE = ISHFT( VALUE,     -24 )
               OUTPUT((J+2):(J+2)) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(2),  24 )
               VALUE = ISHFT( VALUE,     -24 )
               OUTPUT((J+3):(J+3)) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(1), -24 )
               OUTPUT((J+4):(J+4)) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(1),   8 )
               VALUE = ISHFT( VALUE,     -24 )
               OUTPUT((J+5):(J+5)) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(1),  16 )
               VALUE = ISHFT( VALUE,     -24 )
               OUTPUT((J+6):(J+6)) = CHAR(VALUE)

               VALUE = ISHFT( IEQUIV(1),  24 )
               VALUE = ISHFT( VALUE,     -24 )
               OUTPUT((J+7):(J+7)) = CHAR(VALUE)

            END DO

         ELSE

            CALL SETMSG ( 'Unable to translate double precision ' &
            //            'numbers from binary file format # to #. ' &
            //            'This error should never occur and is ' &
            //            'indicative of a bug.  Contact NAIF.'     )
            CALL ERRCH  ( '#', STRBFF(NATBFF)                       )
            CALL ERRCH  ( '#', STRBFF(OUTBFF)                       )
            CALL SIGERR ( 'SPICE(BUG)'                              )
            CALL CHKOUT ( 'T_XLTFWD'                                )
            RETURN

         END IF

!
!     The native binary file format on this platform is not supported
!     for the conversion of integers.  This is a bug, as this branch
!     of code should never be reached in normal operation.
!
      ELSE

         CALL SETMSG ( 'The native binary file format of this ' &
         //            'toolkit build, #, is not currently supported ' &
         //            'for translation of double precision numbers ' &
         //            'from non-native formats.'                      )
         CALL ERRCH  ( '#', STRBFF(NATBFF)                             )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'T_XLTFWD'                                      )
         RETURN

      END IF

      CALL CHKOUT ( 'T_XLTFWD' )
      RETURN

      END
