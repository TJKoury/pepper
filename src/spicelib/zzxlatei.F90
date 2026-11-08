!$Procedure ZZXLATEI ( Private --- Translate Integers )

      SUBROUTINE ZZXLATEI ( INBFF, INPUT, SPACE, OUTPUT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Convert integers from one binary file format to another.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE              'zzddhman.inc'

      INTEGER               INBFF
      CHARACTER*(*)         INPUT
      INTEGER               SPACE
      INTEGER               OUTPUT ( * )


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INBFF      I   Binary file format code for integers in INPUT.
!     INPUT      I   String containing integers read as characters.
!     SPACE      I   Number of integers that can be placed in OUTPUT.
!     OUTPUT     O   Translated integer values.
!
! $ Detailed_Input
!
!     INBFF      is an integer code that indicates the binary file
!                format of INPUT.  Acceptable values are the
!                parameters:
!
!                   BIGI3E
!                   LTLI3E
!                   VAXGFL
!                   VAXDFL
!
!                as defined in the include file 'zzddhman.inc'.
!
!     INPUT      is a string containing a group of integers read
!                from a file as a character string.  The length of
!                this string must be a multiple of the number of
!                bytes used to store an integer in a file utilizing
!                INBFF.
!
!     SPACE      is the number of integers that OUTPUT has room to
!                store.
!
! $ Detailed_Output
!
!     OUTPUT     is an array of integers containing the translated
!                values from INPUT into the native binary format.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     This routine signals several SPICE(BUG) exceptions.  They are
!     signaled when improperly specified inputs are passed into the
!     routine or if the module or modules in its calling tree are
!     improperly configured to run on this platform.  Callers that
!     prevent invalid inputs from being passed into this routine
!     need not check in.  See the $Restrictions section for a
!     discussion of input argument restrictions.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine translates integers from a non-native integer format
!     read from a file as a sequence of characters to the native format.
!
! $ Examples
!
!     See ZZDAFGFR, ZZDAFGSR.
!
! $ Restrictions
!
!     1) Numeric data when read as characters from a file preserve
!        the bit patterns present in the file in memory.
!
!     2) A byte is 8 bits, and a character is some multiple of
!        bytes.
!
!     3) The intrinsic ICHAR preserves the bit pattern of the character
!        byte read from a file.  Namely if one examines the integer
!        created the 8 least significant bits will be precisely those
!        found in the character.
!
!     4) The size of integers on the target environment are a multiple
!        of some number of bytes.
!
!     5) The length of the INPUT string is a multiple of the number
!        of bytes for an integer in the INBFF format.
!
!     6) INBFF is supported for reading on this platform, and not
!        equivalent to NATBFF on this platform.
!
!     7) This routine must support all of the non-native translations
!        required by the 'READS_BFF' key in ZZPLATFM.
!
!     8) The character label corresponding to INBFF must be one of the
!        non-native entries in the value of 'READS_BFF' returned by
!        ZZPLATFM for this environment.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.27.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 1.26.1, 01-OCT-2021 (NJB)
!
!        Fixed typo in comments.
!
!-    SPICELIB Version 1.26.0, 15-NOV-2015 (EDW)
!
!        Recast J = 4*(-1+I) + 1 as J = -3+4*I to accommodate
!        f2c processing.
!
!-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 1.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 1.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.21.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 1.11.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 1.9.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 1.8.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 1.4.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 1.3.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 1.1.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 1.0.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 1.0.0, 12-NOV-2001 (FST)
!
!
!-&

!
!     SPICELIB Functions
!
      INTEGER               INTMIN
      INTEGER               ISRCHC

      LOGICAL               RETURN

!
!     Local Parameters
!
!
!     These parameters are used for arithmetic shifting.
!
      INTEGER               SHFT8
      PARAMETER           ( SHFT8  = 2**8  )

      INTEGER               SHFT16
      PARAMETER           ( SHFT16 = 2**16 )

      INTEGER               SHFT24
      PARAMETER           ( SHFT24 = 2**24 )

!
!     Local Variables
!
      CHARACTER*(1)         CARG
      CHARACTER*(STRSIZ)    STRBFF ( NUMBFF )
      CHARACTER*(STRSIZ)    TMPSTR

      INTEGER               BIGINT
      INTEGER               I
      INTEGER               J
      INTEGER               LENIPT
      INTEGER               NATBFF
      INTEGER               NUMINT
      INTEGER               OSIGN
      INTEGER               SMLINT
      INTEGER               VALUE

      LOGICAL               FIRST

!
!     Statement Functions
!
      INTEGER               ZZICHR

!
!     Saved Variables
!
      SAVE                  FIRST
      SAVE                  NATBFF
      SAVE                  STRBFF
      SAVE                  SMLINT
      SAVE                  BIGINT

!
!     Data Statements
!
      DATA                  FIRST  / .TRUE. /
      DATA                  NATBFF / 0      /

!
!     Statement Function Definitions
!
!     This function controls the conversion of characters to integers.
#if defined(SPICE_COMPILER_INTEL)
!     On some supported environments, ICHAR is not sufficient to
!     produce the desired results.  This however, is not the case
!     with this particular environment.
#else
!     Some versions of the g77 implement ICHAR with a signed integer.
!     This function computes the value of ICHAR that this code requires
!     on any version of g77 for x86 Linux.
#endif
!
#if defined(SPICE_COMPILER_INTEL)
      ZZICHR(CARG) = ICHAR(CARG)
#else
      ZZICHR(CARG) = ICHAR(CARG) - MAX( -1, MIN(0,ICHAR(CARG)) )*256
#endif

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZXLATEI' )
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
            CALL CHKOUT ( 'ZZXLATEI'                                   )
            RETURN

         END IF


!
!        Store the largest value a 32-bit integer can actually
!        hold.
!
         BIGINT = 2147483647

!
!        Prepare the smallest value a 32-bit integer can actually
!        store, regardless of what INTMIN returns.
!
         SMLINT = INTMIN()

!
!        Set SMLINT to the appropriate value if INTMIN is too large.
!
         IF ( SMLINT .EQ. -2147483647 ) THEN
            SMLINT = SMLINT - 1
         END IF

!
!        Do not perform initialization tasks again.
!
         FIRST = .FALSE.

      END IF

!
!     Check to see if INBFF is valid.  This should never occur if this
!     routine is called properly.
!
      IF ( ( INBFF .LT. 1 ) .OR. ( INBFF .GT. NUMBFF ) ) THEN

         CALL SETMSG ( 'The integer code used to indicate the ' &
         //            'binary file format of the input integers, ' &
         //            '#, is out of range.  This error should ' &
         //            'never occur.'                               )
         CALL ERRINT ( '#', INBFF                                   )
         CALL SIGERR ( 'SPICE(BUG)'                                 )
         CALL CHKOUT ( 'ZZXLATEI'                                   )
         RETURN

      END IF

!
!     Retrieve the length of the input string.
!
      LENIPT = LEN(INPUT)

!
!     Now branch based on the value of NATBFF.
!
      IF ( NATBFF .EQ. BIGI3E ) THEN

         IF ( INBFF .EQ. LTLI3E ) THEN

!
!           Check to see that the length of the input string is
!           appropriate.  Since this is a string containing LTL-IEEE
!           integers and this is a BIG-IEEE machine, characters are
!           1-byte and integers are 4-bytes.  So the length of INPUT
!           must be a multiple of 4.
!
            NUMINT = LENIPT / 4

            IF ( LENIPT - ( NUMINT*4 ) .NE. 0 ) THEN

               CALL SETMSG ( 'The input string that is to be ' &
               //            'translated from the binary format ' &
               //            '# to format # has a length that is ' &
               //            'not a multiple of 4 bytes.  This ' &
               //            'error should never occur.'           )
               CALL ERRCH  ( '#', STRBFF(INBFF)                    )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                   )
               CALL SIGERR ( 'SPICE(BUG)'                          )
               CALL CHKOUT ( 'ZZXLATEI'                            )
               RETURN

            END IF

!
!           Verify there is enough room to store the results of
!           the translation.
!
            IF ( NUMINT .GT. SPACE ) THEN

               CALL SETMSG ( 'The caller specified that # integers ' &
               //            'are to be translated from binary ' &
               //            'format # to #.  However there is only ' &
               //            'room to hold # integers in the output ' &
               //            'array.  This error should never occur.' )
               CALL ERRINT ( '#', NUMINT                              )
               CALL ERRCH  ( '#', STRBFF(INBFF)                       )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                      )
               CALL ERRINT ( '#', SPACE                               )
               CALL SIGERR ( 'SPICE(BUG)'                             )
               CALL CHKOUT ( 'ZZXLATEI'                               )
               RETURN

            END IF

!
!           Start looping over each 4 character package in INPUT and
!           converting them to integers.
!
            DO I = 1, NUMINT

!
!              Compute the substring index of the first character
!              in INPUT for this integer.
!
               J = -3 + 4*I

!
!              Now arrange the bytes properly.  Since these characters
!              were read from a file utilizing LTL-IEEE, we know that
!              J is the least significant byte and that (J+3) is the
!              most significant.
!
!              INPUT:
!
!                      -------------------------------------
!                 . . .|     |  J  | J+1 | J+2 | J+3 |     |. . .
!                      -------------------------------------
!
!              From this we construct OUTPUT(I) using the following
!              relation:
!
!                      INPUT(J:J)
!                      INPUT(J+1:J+1) shifted 8 bits to the MSb
!                      INPUT(J+2:J+2) shifted 16 bits to the MSb
!                   +  INPUT(J+3:J+3) shifted 24 bits to the MSb
!                   -------------------------
!                      OUTPUT(I)
!
!
!              Perform the necessary computations.  What is outlined
!              above is implemented below using arithmetic operations.
!              The last "shifted 24 bits to the MSb" is handled
!              in a special way, since the sign bit can not be shifted
!              into place through simple multiplication.
!
               VALUE     = ZZICHR( INPUT(J:J) )
               OUTPUT(I) = VALUE

               VALUE     = ZZICHR( INPUT((J+1):(J+1)) )
               VALUE     = VALUE*SHFT8
               OUTPUT(I) = OUTPUT(I)+VALUE

               VALUE     = ZZICHR( INPUT((J+2):(J+2)) )
               VALUE     = VALUE*SHFT16
               OUTPUT(I) = OUTPUT(I)+VALUE

               VALUE     = ZZICHR( INPUT((J+3):(J+3)) )

!
!              In order to properly install the last byte,
!              the sign bit needs to be managed separately.
               OSIGN = VALUE / 128

!
!              Strip the sign bit if necessary.
!
               IF ( OSIGN .EQ. 1 ) THEN
                  VALUE = VALUE - 128
               END IF

!
!              Shift the non-sign bits out to their appropriate
!              positions and combine them with OUTPUT(I).
!
               VALUE     = VALUE*SHFT24
               OUTPUT(I) = OUTPUT(I)+VALUE

!
!              Install the sign bit.  At the moment in OUTPUT(I)
!              we have the bits precisely as they need to be
!              arranged.  Perform the following computations:
!
!                 OUTPUT(I) = (2**31-1) - OUTPUT(I) + 1
!
!              Break this up into steps since 2**31 is not
!              representable with 32 bit integers that utilize
!              2's complement.
!
!              First negate the result:
!
!                 OUTPUT(I) = -OUTPUT(I)
!
!              But this negation is effectively:
!
!                 OUTPUT(I) = 2**32 - OUTPUT(I)
!
!              Which yields:
!
!                 2**32 - (2**31) + OUTPUT(I)
!
!              or
!
!                 2**31 + OUTPUT(I)
!
!              which is the desired quantity.  Note, 0 must be
!              treated as a special case.
!
               IF ( OSIGN .EQ. 1 ) THEN
                  IF ( OUTPUT(I) .EQ. 0 ) THEN
                     OUTPUT(I) = SMLINT
                  ELSE
                     OUTPUT(I) = BIGINT-OUTPUT(I)
                     OUTPUT(I) = OUTPUT(I) + 1
                     OUTPUT(I) = -OUTPUT(I)
                  END IF
               END IF

            END DO

         ELSE

            CALL SETMSG ( 'Unable to translate integers from ' &
            //            'binary file format # to #.  This error ' &
            //            'should never occur and is indicative ' &
            //            'of a bug.  Contact NAIF.'                )
            CALL ERRCH  ( '#', STRBFF(INBFF)                        )
            CALL ERRCH  ( '#', STRBFF(NATBFF)                       )
            CALL SIGERR ( 'SPICE(BUG)'                              )
            CALL CHKOUT ( 'ZZXLATEI'                                )
            RETURN

         END IF

      ELSE IF ( NATBFF .EQ. LTLI3E ) THEN

         IF ( INBFF .EQ. BIGI3E ) THEN

!
!           Check to see that the length of the input string is
!           appropriate.  Since this is a string containing BIG-IEEE
!           integers and this is a LTL-IEEE machine, characters are
!           1-byte and integers are 4-bytes.  So the length of INPUT
!           must be a multiple of 4.
!
            NUMINT = LENIPT / 4

            IF ( LENIPT - ( NUMINT*4 ) .NE. 0 ) THEN

               CALL SETMSG ( 'The input string that is to be ' &
               //            'translated from the binary format ' &
               //            '# to format # has a length that is ' &
               //            'not a multiple of 4 bytes.  This ' &
               //            'error should never occur.'           )
               CALL ERRCH  ( '#', STRBFF(INBFF)                    )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                   )
               CALL SIGERR ( 'SPICE(BUG)'                          )
               CALL CHKOUT ( 'ZZXLATEI'                            )
               RETURN

            END IF

!
!           Verify there is enough room to store the results of
!           the translation.
!
            IF ( NUMINT .GT. SPACE ) THEN

               CALL SETMSG ( 'The caller specified that # integers ' &
               //            'are to be translated from binary ' &
               //            'format # to #.  However there is only ' &
               //            'room to hold # integers in the output ' &
               //            'array.  This error should never occur.' )
               CALL ERRINT ( '#', NUMINT                              )
               CALL ERRCH  ( '#', STRBFF(INBFF)                       )
               CALL ERRCH  ( '#', STRBFF(NATBFF)                      )
               CALL ERRINT ( '#', SPACE                               )
               CALL SIGERR ( 'SPICE(BUG)'                             )
               CALL CHKOUT ( 'ZZXLATEI'                               )
               RETURN

            END IF

!
!           Start looping over each 4 character package in INPUT and
!           converting them to integers.
!
            DO I = 1, NUMINT

!
!              Compute the substring index of the first character
!              in INPUT for this integer.
!
               J = -3 + 4*I

!
!              Now arrange the bytes properly.  Since these characters
!              were read from a file utilizing BIG-IEEE, we know that
!              J is the most significant byte and that (J+3) is the
!              least significant.
!
!              INPUT:
!
!                      -------------------------------------
!                 . . .|     |  J  | J+1 | J+2 | J+3 |     |. . .
!                      -------------------------------------
!
!              From this we construct OUTPUT(I) using the following
!              relation:
!
!                      INPUT(J+3:J+3)
!                      INPUT(J+2:J+2)*SHFT8
!                      INPUT(J+1:J+1)*SHFT16
!                   +  INPUT(J:J)*SHFT24
!                   -------------------------
!                      OUTPUT(I)
!
!
!              Perform the necessary computations.  What is outlined
!              above is implemented below using arithmetic operations.
!              The last "shifted 24 bits to the MSb" is handled
!              in a special way, since the sign bit can not be shifted
!              into place through simple multiplication.
!
               VALUE     = ZZICHR( INPUT((J+3):(J+3)) )
               OUTPUT(I) = VALUE

               VALUE     = ZZICHR( INPUT((J+2):(J+2)) )
               VALUE     = VALUE*SHFT8
               OUTPUT(I) = OUTPUT(I)+VALUE

               VALUE     = ZZICHR( INPUT((J+1):(J+1)) )
               VALUE     = VALUE*SHFT16
               OUTPUT(I) = OUTPUT(I)+VALUE

               VALUE     = ZZICHR( INPUT(J:J) )

!
!              In order to properly install the last byte,
!              the sign bit needs to be managed separately.
!
               OSIGN = VALUE / 128

!
!              Strip the sign bit if necessary.
!
               IF ( OSIGN .EQ. 1 ) THEN
                  VALUE = VALUE - 128
               END IF

!
!              Shift the non-sign bits out to their appropriate
!              positions and combine them with OUTPUT(I).
!
               VALUE     = VALUE*SHFT24
               OUTPUT(I) = OUTPUT(I)+VALUE

!
!              Install the sign bit.  At the moment in OUTPUT(I)
!              we have the bits precisely as they need to be
!              arranged.  Perform the following computations:
!
!                 OUTPUT(I) = (2**31-1) - OUTPUT(I) + 1
!
!              Break this up into steps since 2**31 is not
!              representable with 32 bit integers that utilize
!              2's complement.
!
!              First, negate the result:
!
!                 OUTPUT(I) = -OUTPUT(I)
!
!              But this negation is effectively:
!
!                 OUTPUT(I) = 2**32 - OUTPUT(I)
!
!              Which yields:
!
!                 2**32 - (2**31) + OUTPUT(I)
!
!              or
!
!                 2**31 + OUTPUT(I)
!
!              which is the desired quantity.  Note, 0 must be
!              treated as a special case.
!
               IF ( OSIGN .EQ. 1 ) THEN
                  IF ( OUTPUT(I) .EQ. 0 ) THEN
                     OUTPUT(I) = SMLINT
                  ELSE
                     OUTPUT(I) = BIGINT-OUTPUT(I)
                     OUTPUT(I) = OUTPUT(I) + 1
                     OUTPUT(I) = -OUTPUT(I)
                  END IF
               END IF

            END DO

         ELSE

            CALL SETMSG ( 'Unable to translate integers from ' &
            //            'binary file format # to #.  This error ' &
            //            'should never occur and is indicative ' &
            //            'of a bug.  Contact NAIF.'                )
            CALL ERRCH  ( '#', STRBFF(INBFF)                        )
            CALL ERRCH  ( '#', STRBFF(NATBFF)                       )
            CALL SIGERR ( 'SPICE(BUG)'                              )
            CALL CHKOUT ( 'ZZXLATEI'                                )
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
         //            'for translation of integers from non-native ' &
         //            'formats.'                                      )
         CALL ERRCH  ( '#', STRBFF(NATBFF)                             )
         CALL SIGERR ( 'SPICE(BUG)'                                    )
         CALL CHKOUT ( 'ZZXLATEI'                                      )
         RETURN

      END IF

      CALL CHKOUT ( 'ZZXLATEI' )
      RETURN

      END
