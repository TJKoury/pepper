C$Procedure T_XLTFWD ( Translate For Write, Double Precision Values )
 
      SUBROUTINE T_XLTFWD ( INPUT, NUMDP, OUTBFF, OUTPUT )
 
C$ Abstract
C
C     Translate double precision values from the native binary file
C     format to a non-native format for write.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST ROUTINE
C
C$ Declarations
 
      IMPLICIT NONE
 
      INCLUDE              'zzddhman.inc'
 
      DOUBLE PRECISION      INPUT  ( * )
      INTEGER               NUMDP
      INTEGER               OUTBFF
      CHARACTER*(*)         OUTPUT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     INPUT      I   List of d.p. values to be translated.
C     NUMDP      I   Number of integers in INPUT to convert.
C     OUTBFF     I   Binary file format code for d.p. values in OUTPUT.
C     OUTPUT     O   Character string containing the translated values.
C
C$ Detailed_Input
C
C     INPUT      is an array of double precision numbers containing the
C                values to be translated into a non-native binary format
C                and stored in OUTPUT.
C
C     NUMDP      is the number of double precision values to convert to
C                non-native binary file format and store in OUTPUT.
C
C     OUTBFF     is an integer code that indicates the binary file
C                format targeted for OUTPUT.  Acceptable values
C                are the parameters:
C
C                   BIGI3E
C                   LTLI3E
C                   VAXGFL
C                   VAXDFL
C
C                as defined in the include file 'zzddhman.inc'.
C
C$ Detailed_Output
C
C     OUTPUT     is a string containing a group of double precision
C                values converted from the native binary file format
C                to OUTBFF and encoded byte by byte as characters.
C                This string should be at least the number of bytes
C                used to store an integer multiplied by NUMDP in
C                length.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     This routine may signal SPICE(BUG) if the inputs are not
C     proper.
C
C$ Particulars
C
C     This routine converts double precision values from the native
C     binary file format to a non-native one.  The results are
C     encoded in a character string that can be written directly
C     to a file.
C
C     Note: This code is not a model of perfection.  It is hacked
C     together from pieces of code written elsewhere to support
C     creation of non-native binaries for testing purposes only.
C
C$ Examples
C
C     See F_XLATED for usage.
C
C$ Restrictions
C
C     1) Numeric data when written as characters to a file preserve
C        the bit patterns present in memory in the file.
C
C     2) A byte is 8 bits, and a character is some multiple of
C        bytes.
C
C     3) The intrinsic ICHAR preserves the bit pattern of the character
C        byte read from a file.  Namely if one examines the integer
C        found in the character.
C
C     4) The size of integers on the target environment are a multiple
C        of some number of bytes.
C
C     5) The length of the OUTPUT string is a multiple of the number
C        of bytes for an integer in the OUTBFF format.
C
C     6) OUTBFF is supported for reading on this platform, and not
C        equivalent to NATBFF on this platform.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 19-OCT-2001 (FST)
C
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               ISRCHC
      LOGICAL               RETURN
 
C
C     Local Variables
C
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
 
C
C     Equivalence DEQUIV to IEQUIV
C
      EQUIVALENCE         ( DEQUIV, IEQUIV )
 
C
C     Saved Variables
C
      SAVE                  FIRST
      SAVE                  NATBFF
      SAVE                  STRBFF
 
C
C     Data Statements
C
      DATA                  FIRST  / .TRUE. /
      DATA                  NATBFF / 0      /
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'T_XLTFWD' )
      END IF
 
C
C     Perform some initialization tasks.
C
      IF ( FIRST ) THEN
 
C
C        Populate STRBFF with the appropriate binary file
C        format labels.
C
         DO I = 1, NUMBFF
            CALL ZZDDHGSD ( 'BFF', I, STRBFF(I) )
         END DO
 
C
C        Fetch the native binary file format.
C
         CALL ZZPLATFM ( 'FILE_FORMAT', TMPSTR )
         CALL UCASE    ( TMPSTR,        TMPSTR )
 
         NATBFF = ISRCHC ( TMPSTR, NUMBFF, STRBFF )
 
         IF ( NATBFF .EQ. 0 ) THEN
 
            CALL SETMSG ( 'The binary file format, ''#'', is not '
     .      //            'supported by this version of the toolkit. '
     .      //            'This is a serious problem, contact NAIF.'   )
            CALL ERRCH  ( '#', TMPSTR                                  )
            CALL SIGERR ( 'SPICE(BUG)'                                 )
            CALL CHKOUT ( 'T_XLTFWD'                                   )
            RETURN
 
         END IF
 
C
C        Do not perform initialization tasks again.
C
         FIRST = .FALSE.
 
      END IF
 
C
C     Check to see if OUTBFF is valid.  This should never occur if this
C     routine is called properly.
C
      IF ( ( OUTBFF .LT. 1 ) .OR. ( OUTBFF .GT. NUMBFF ) ) THEN
 
         CALL SETMSG ( 'The integer code used to indicate the '
     .   //            'binary file format of the input integers, '
     .   //            '#, is out of range.  This error should '
     .   //            'never occur.'                               )
         CALL ERRINT ( '#', OUTBFF                                  )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'T_XLTFWD'                                   )
         RETURN
 
      END IF
 
C
C     Get the length of the input string.
C
      LENOPT = LEN(OUTPUT)
 
C
C     Now branch depending on the value of NATBFF.
C
      IF ( NATBFF .EQ. BIGI3E ) THEN
 
         IF ( OUTBFF .EQ. LTLI3E ) THEN
 
C
C           Before we go any further check to see that the length
C           of the output string is appropriate and we have enough
C           room to store the results.  Since this string is to contain
C           LTL-IEEE double precision numbers and this is a BIG-IEEE
C           machine, characters are 1-byte and double precision numbers
C           are 8-bytes.  So the length of OUTPUT must be a multiple
C           of 8.
C
            SPACE = LENOPT / 8
 
            IF ( LENOPT - ( SPACE*8 ) .NE. 0 ) THEN
 
               CALL SETMSG ( 'The output string that is to be '
     .         //            'translated from the binary format '
     .         //            '# to format # has a length that is '
     .         //            'not a multiple of 8 bytes.  This '
     .         //            'error should never occur.'           )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                   )
               CALL ERRCH  ( '#', STRBFF(OUTBFF)                   )
               CALL SIGERR ( 'SPICE(BUG)'                          )
               CALL CHKOUT ( 'T_XLTFWD'                            )
               RETURN
 
            END IF
 
C
C           Now check to see that there is enough room to store
C           the number of integers we are to convert.
C
            IF ( NUMDP .GT. SPACE ) THEN
 
               CALL SETMSG ( 'The caller specified that # double '
     .         //            'precision numbers are to be translated '
     .         //            'from binary format # to #.  However '
     .         //            'there is only room to hold # numbers '
     .         //            'in the output array.  This error should '
     .         //            'never occur.' )
               CALL ERRINT ( '#', NUMDP                               )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                      )
               CALL ERRCH  ( '#', STRBFF(OUTBFF)                      )
               CALL ERRINT ( '#', SPACE                               )
               CALL SIGERR ( 'SPICE(BUG)'                             )
               CALL CHKOUT ( 'T_XLTFWD'                               )
               RETURN
 
            END IF
 
C
C           Start looping over each double precision number and
C           converting them to the 8-byte character packages to
C           be stored in OUTPUT.
C
            DO I = 1, NUMDP
 
C
C              Compute the substring index of the first character
C              in OUTPUT for this double precision number.
C
               J = 8*(I-1) + 1
 
C
C              Now extract and arrange the bytes properly.
C              Since these characters are to be stored in a file
C              utilizing LTL-IEEE, we know that J is the
C              least significant byte and that (J+7) is the
C              most significant.
C
C              INPUT:
C
C                      -------------------------------------
C                 . . .|     |  J  | J+1 | J+2 | J+3 |     |. . .
C                      -------------------------------------
C
C                   OUTPUT(J:J) = CHAR( INPUT(I)'s LSB )
C                      .
C                      .
C                   OUTPUT(J+7:J+7) = CHAR( INPUT(I)'s MSB )
C
C              Perform the necessary computations.
C
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
 
            CALL SETMSG ( 'Unable to translate double precision '
     .      //            'numbers from binary file format # to #. '
     .      //            'This error should never occur and is '
     .      //            'indicative of a bug.  Contact NAIF.'      )
            CALL ERRCH  ( '#', STRBFF(NATBFF)                        )
            CALL ERRCH  ( '#', STRBFF(OUTBFF)                        )
            CALL SIGERR ( 'SPICE(BUG)'                               )
            CALL CHKOUT ( 'T_XLTFWD'                                 )
            RETURN
 
         END IF
 
      ELSE IF ( NATBFF .EQ. LTLI3E ) THEN
 
         IF ( OUTBFF .EQ. BIGI3E ) THEN
 
C
C           Before we go any further check to see that the length
C           of the output string is appropriate and we have enough
C           room to store the results.  Since this string is to
C           contain BIG-IEEE double precision values and this is a
C           LTL-IEEE machine, characters are 1-byte and double
C           precision values are 8-bytes.  So the length of OUTPUT
C           must be a multiple of 8.
C
            SPACE = LENOPT / 8
 
            IF ( LENOPT - ( SPACE*8 ) .NE. 0 ) THEN
 
               CALL SETMSG ( 'The output string that is to be '
     .         //            'translated from the binary format '
     .         //            '# to format # has a length that is '
     .         //            'not a multiple of 8 bytes.  This '
     .         //            'error should never occur.'           )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                   )
               CALL ERRCH  ( '#', STRBFF(OUTBFF)                   )
               CALL SIGERR ( 'SPICE(BUG)'                          )
               CALL CHKOUT ( 'T_XLTFWD'                            )
               RETURN
 
            END IF
 
C
C           Now check to see that there is enough room to store
C           the number of integers we are to convert.
C
            IF ( NUMDP .GT. SPACE ) THEN
 
               CALL SETMSG ( 'The caller specified that # double '
     .         //            'precision numbers are to be '
     .         //            'translated from binary format # to #. '
     .         //            'However there is only room to hold # '
     .         //            'numbers in the output array.  This '
     .         //            'error should never occur.'              )
               CALL ERRINT ( '#', NUMDP                               )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                      )
               CALL ERRCH  ( '#', STRBFF(OUTBFF)                      )
               CALL ERRINT ( '#', SPACE                               )
               CALL SIGERR ( 'SPICE(BUG)'                             )
               CALL CHKOUT ( 'T_XLTFWD'                               )
               RETURN
 
            END IF
 
C
C           Start looping over each double precision number and
C           converting them to the 8-byte character packages to
C           be stored in OUTPUT.
C
            DO I = 1, NUMDP
 
C
C              Compute the substring index of the first character
C              in OUTPUT for this double precision number.
C
               J = 8*(I-1) + 1
 
C
C              Now extract and arrange the bytes properly.
C              Since these characters are to be stored in a file
C              utilizing BIG-IEEE, we know that J is the most
C              significant byte and that (J+7) is the least
C              significant.
C
C              INPUT:
C
C                      -------------------------------------
C                 . . .|     |  J  | J+1 | J+2 | J+3 |     |. . .
C                      -------------------------------------
C
C                   OUTPUT(J:J) = CHAR( INPUT(I)'s MSB )
C                      .
C                      .
C                   OUTPUT(J+7:J+7) = CHAR( INPUT(I)'s LSB )
C
C              Perform the necessary computations.
C
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
 
            CALL SETMSG ( 'Unable to translate double precision '
     .      //            'numbers from binary file format # to #. '
     .      //            'This error should never occur and is '
     .      //            'indicative of a bug.  Contact NAIF.'     )
            CALL ERRCH  ( '#', STRBFF(NATBFF)                       )
            CALL ERRCH  ( '#', STRBFF(OUTBFF)                       )
            CALL SIGERR ( 'SPICE(BUG)'                              )
            CALL CHKOUT ( 'T_XLTFWD'                                )
            RETURN
 
         END IF
 
C
C     The native binary file format on this platform is not supported
C     for the conversion of integers.  This is a bug, as this branch
C     of code should never be reached in normal operation.
C
      ELSE
 
         CALL SETMSG ( 'The native binary file format of this '
     .   //            'toolkit build, #, is not currently supported '
     .   //            'for translation of double precision numbers '
     .   //            'from non-native formats.'                      )
         CALL ERRCH  ( '#', STRBFF(NATBFF)                             )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'T_XLTFWD'                                      )
         RETURN
 
      END IF
 
      CALL CHKOUT ( 'T_XLTFWD' )
      RETURN
 
      END
