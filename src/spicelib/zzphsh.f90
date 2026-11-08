!$Procedure ZZPHSH ( Private---kernel POOL hash function umbrella )

      INTEGER FUNCTION ZZPHSH ( WORD, M, M2 )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is an umbrella routine for the kernel POOL hash function.
!     It should never be called directly.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE UTILITY
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         WORD
      INTEGER               M
      INTEGER               M2

! $ Brief_I/O
!
!     VARIABLE  I/O  Entry point
!     --------  ---  --------------------------------------------------
!     WORD       I   ZZHASH, ZZHASH2
!     M          I   ZZSHSH
!     M2         I   ZZHASH2
!
!     The function returns zero.
!
! $ Detailed_Input
!
!     See individual entry points.
!
! $ Detailed_Output
!
!     The function ZZPHSH should never be called. However, it returns
!     the value zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is an umbrella for the kernel POOL hash function
!     ZZHASH and its set up routine ZZSHSH, and for an arbitrary
!     divisor hash function ZZHASH2 that uses the same algorithm as
!     ZZHASH.
!
!     ZZSHSH and ZZHASH are intended to be used ONLY by the POOL
!     subsystem. ZZSHSH must be called once to save the POOL-specific
!     hash divisor and to initialize character-code map prior to the
!     first call to ZZHASH.
!
!     ZZHASH2 can be used with with an arbitrary divisor (recommended
!     to be a prime number) that is passed in as an input argument.
!     If ZZHASH2 is called prior to ZZSHSH, it does the same
!     character-code map initialization.
!
!     The algorithm implemented in ZZHASH and ZZHASH2 is
!     case-insensitive and uses only the first word of the input
!     string. In order to make effective use of this hash algorithm,
!     input strings should be left-justified and space-less.
!
! $ Examples
!
!     To make use of the ZZHASH hash function the POOL subsystem first
!     calls ZZSHSH. The value returned by ZZSHSH has no meaning and can
!     bed assigned to any temporary variable. Then ZZHASH can be used
!     as needed by the POOL routine to compute hash value for the POOL
!     keywords.
!
!        I = ZZSHSH ( M )
!
!           ...any other set up code...
!
!        LOOKAT = ZZHASH ( WORD )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     1)  Knuth, Donald E. "The Art of Computer Programming, Volume
!         3/Sorting and Searching 2nd Edition" 1997, pp 513-521.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     E.D. Wright     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.2, 31-JUL-2013 (BVS)
!
!        Added more details to the header. Added initialization of POOL
!        divisor to -1 to allow for initialization error checking on
!        the first call to ZZHASH.
!
!-    SPICELIB Version 1.1.1, 21-NOV-2006 (EDW)(BVS)
!
!        Replaced ICHAR('\\') expression with parameter
!        BSLASH, the parameter set to the ASCII value
!        of the backslash character, 92.
!
!-    SPICELIB Version 1.1.0, 14-SEP-2005 (EDW)
!
!        Added function ZZHASH2. Operation matches
!        that of ZZHASH with the exception that ZZHASH2
!        accepts the divisor value, M, as an input.
!
!-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
!
!-&

!
!     Entry Points
!
      INTEGER               ZZSHSH
      INTEGER               ZZHASH
      INTEGER               ZZHASH2

!
!     SPICELIB functions
!
      INTEGER               INTMAX

!
!     Local Variables.
!
      INTEGER               I
      INTEGER               DIVISR
      INTEGER               VAL ( 0:128 )
      INTEGER               F
      INTEGER               BASE
      INTEGER               BLANK
      INTEGER               LENGTH
      INTEGER               MAXDIV

      INTEGER               BSLASH
      PARAMETER           ( BSLASH = 92 )


      LOGICAL               FIRST
      SAVE

      DATA                  FIRST / .TRUE. /

      DATA                  DIVISR / -1 /

!
!     We do not diagnose a bogus call since this is a private routine.
!
      ZZPHSH = 0

      RETURN


!$Procedure ZZSHSH ( Private---Set up POOL hash function )

      ENTRY ZZSHSH ( M )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine sets up the kernel POOL hash function. Call it once
!     per program execution prior to the first call to ZZHASH.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE UTILITY
!
! $ Declarations
!
!     INTEGER               M
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     M          I   Divisor used for the POOL hash function
!
!     The function returns 0.
!
! $ Detailed_Input
!
!     M           is the divisor of the hashing function. It is
!                 recommended that this be a prime number nominally
!                 equal to the maximum number of POOL variables.
!                 The value of M must be in the range from 1 to
!                 INTMAX/68 - 1.
!
! $ Detailed_Output
!
!     The function returns the value zero (0).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the input divisor value is not in the allowed range, the
!        error 'SPICE(INVALIDDIVISOR)' will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This entry point saves the divisor used for hashing input
!     strings. It must be called once by an initialization branch of
!     the kernel POOL.
!
!     The character-code map initialized by this function is
!     case-insensitive.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     This entry must NOT be called by any subsystem except the
!     kernel POOL.
!
! $ Literature_References
!
!     1)  Knuth, Donald E. "The Art of Computer Programming, Volume
!         3/Sorting and Searching 2nd Edition" 1997, pp 513-521.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     E.D. Wright     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.2, 31-JUL-2013 (BVS)
!
!        Added more details to the header. Added computation of
!        the maximum allowed divisor value to the initialization
!        block. Added a check for the input divisor to be in the
!        allowed range.
!
!-    SPICELIB Version 1.1.1, 21-NOV-2006 (EDW)(BVS)
!
!        Replaced ICHAR('\\') expression with parameter
!        BSLASH, the parameter set to the ASCII value
!        of the backslash character, 92.
!
!-    SPICELIB Version 1.1.0, 06-JUL-2005 (EDW)
!
!        Added punctuation marks to array of allowed
!        characters. The function can process any
!        character with ASCII decimal value 33 to 122.
!
!-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
!
!-&

!
!     Return zero.
!
      ZZSHSH = 0

!
!     The initialization block below is identical to the initialization
!     block in the entry ZZHASH2. If this block is changed in any way,
!     the block in ZZHASH2 must be changed in the same way.
!
      IF ( FIRST ) THEN

         FIRST = .FALSE.

         BASE  = 68
         BLANK = ICHAR( ' ' )

         MAXDIV = INTMAX() / BASE - 1

         DO I = 0, 128
            VAL(I) = 0
         END DO

         VAL(ICHAR('0')) = 1
         VAL(ICHAR('1')) = 2
         VAL(ICHAR('2')) = 3
         VAL(ICHAR('3')) = 4
         VAL(ICHAR('4')) = 5
         VAL(ICHAR('5')) = 6
         VAL(ICHAR('6')) = 7
         VAL(ICHAR('7')) = 8
         VAL(ICHAR('8')) = 9
         VAL(ICHAR('9')) = 10
         VAL(ICHAR('A')) = 11
         VAL(ICHAR('B')) = 12
         VAL(ICHAR('C')) = 13
         VAL(ICHAR('D')) = 14
         VAL(ICHAR('E')) = 15
         VAL(ICHAR('F')) = 16
         VAL(ICHAR('G')) = 17
         VAL(ICHAR('H')) = 18
         VAL(ICHAR('I')) = 19
         VAL(ICHAR('J')) = 20
         VAL(ICHAR('K')) = 21
         VAL(ICHAR('L')) = 22
         VAL(ICHAR('M')) = 23
         VAL(ICHAR('N')) = 24
         VAL(ICHAR('O')) = 25
         VAL(ICHAR('P')) = 26
         VAL(ICHAR('Q')) = 27
         VAL(ICHAR('R')) = 28
         VAL(ICHAR('S')) = 29
         VAL(ICHAR('T')) = 30
         VAL(ICHAR('U')) = 31
         VAL(ICHAR('V')) = 32
         VAL(ICHAR('W')) = 33
         VAL(ICHAR('X')) = 34
         VAL(ICHAR('Y')) = 35
         VAL(ICHAR('Z')) = 36
         VAL(ICHAR('-')) = 37
         VAL(ICHAR('_')) = 38
         VAL(ICHAR('.')) = 39
         VAL(ICHAR('/')) = 40
         VAL(ICHAR('!')) = 41
         VAL(ICHAR('@')) = 42
         VAL(ICHAR('#')) = 43
         VAL(ICHAR('$')) = 44
         VAL(ICHAR('%')) = 45
         VAL(ICHAR('^')) = 46
         VAL(ICHAR('&')) = 47
         VAL(ICHAR('*')) = 48
         VAL(ICHAR('(')) = 49
         VAL(ICHAR(')')) = 50
         VAL(ICHAR('+')) = 51
         VAL(ICHAR('=')) = 52
         VAL(ICHAR('[')) = 53
         VAL(ICHAR('{')) = 54
         VAL(ICHAR(']')) = 55
         VAL(ICHAR('}')) = 56
         VAL(ICHAR('|')) = 57
         VAL(BSLASH    ) = 58
         VAL(ICHAR(':')) = 59
         VAL(ICHAR(';')) = 60
         VAL(ICHAR('<')) = 61
         VAL(ICHAR(',')) = 62
         VAL(ICHAR('>')) = 63
         VAL(ICHAR('?')) = 64

!
!        Note, ICHAR('''') returns the ASCII value for the single
!        quote -> '.
!
         VAL(ICHAR(''''))= 65
         VAL(ICHAR('"')) = 66
         VAL(ICHAR('`')) = 67
         VAL(ICHAR('~')) = 68

         VAL(ICHAR('a')) = VAL(ICHAR('A'))
         VAL(ICHAR('b')) = VAL(ICHAR('B'))
         VAL(ICHAR('c')) = VAL(ICHAR('C'))
         VAL(ICHAR('d')) = VAL(ICHAR('D'))
         VAL(ICHAR('e')) = VAL(ICHAR('E'))
         VAL(ICHAR('f')) = VAL(ICHAR('F'))
         VAL(ICHAR('g')) = VAL(ICHAR('G'))
         VAL(ICHAR('h')) = VAL(ICHAR('H'))
         VAL(ICHAR('i')) = VAL(ICHAR('I'))
         VAL(ICHAR('j')) = VAL(ICHAR('J'))
         VAL(ICHAR('k')) = VAL(ICHAR('K'))
         VAL(ICHAR('l')) = VAL(ICHAR('L'))
         VAL(ICHAR('m')) = VAL(ICHAR('M'))
         VAL(ICHAR('n')) = VAL(ICHAR('N'))
         VAL(ICHAR('o')) = VAL(ICHAR('O'))
         VAL(ICHAR('p')) = VAL(ICHAR('P'))
         VAL(ICHAR('q')) = VAL(ICHAR('Q'))
         VAL(ICHAR('r')) = VAL(ICHAR('R'))
         VAL(ICHAR('s')) = VAL(ICHAR('S'))
         VAL(ICHAR('t')) = VAL(ICHAR('T'))
         VAL(ICHAR('u')) = VAL(ICHAR('U'))
         VAL(ICHAR('v')) = VAL(ICHAR('V'))
         VAL(ICHAR('w')) = VAL(ICHAR('W'))
         VAL(ICHAR('x')) = VAL(ICHAR('X'))
         VAL(ICHAR('y')) = VAL(ICHAR('Y'))
         VAL(ICHAR('z')) = VAL(ICHAR('Z'))

      END IF

!
!     Check and save divisor.
!
      IF ( M .LE. 0 .OR. M .GT. MAXDIV ) THEN

         CALL CHKIN  ( 'ZZSHSH'                                      )
         CALL SETMSG ( 'The input hash function divisor was not '    // &
                       'in the allowed range from 1 to #. It was #.' )
         CALL ERRINT ( '#', MAXDIV                                   )
         CALL ERRINT ( '#', M                                        )
         CALL SIGERR ( 'SPICE(INVALIDDIVISOR)'                       )
         CALL CHKOUT ( 'ZZSHSH'                                      )
         RETURN

      END IF

      DIVISR = M

      RETURN


!$Procedure ZZHASH ( Private---POOL Hash function )

      ENTRY ZZHASH ( WORD )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine computes the hash value associated with a kernel
!     POOL variable name.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         WORD
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     WORD       I   A left justified string of characters.
!
!     The function returns the hash value associated with WORD.
!
! $ Detailed_Input
!
!     WORD        is a left justified string of characters. Nominally
!                 this is the name of some kernel POOL variable.
!
! $ Detailed_Output
!
!     The function returns the hash value of WORD.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If this routine is called prior to a call to ZZSPSH, the
!        error 'SPICE(CALLEDOUTOFORDER)' will be signaled. This should
!        never occur. In this case the function returns 0.
!
!     2) If this routine calculates a negative value, the error
!        SPICE(NEGATIVEHASHVALUE1) or SPICE(NEGATIVEHASHVALUE2) will be
!        signaled. This should never occur.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine computes the hash value of a string of characters.
!     The algorithm implemented by this function is case-insensitive and
!     uses only the first word of the input string. In order to make
!     effective use of this hash algorithm, input strings should be
!     left-justified and space-less. All non-left justified strings map
!     to the same value 0.
!
! $ Examples
!
!     See POOL.
!
! $ Restrictions
!
!     ZZSPSH must be called prior to calling this routine.
!
! $ Literature_References
!
!     1)  Knuth, Donald E. "The Art of Computer Programming, Volume
!         3/Sorting and Searching 2nd Edition" 1997, pp 513-521.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     E.D. Wright     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 31-JUL-2013 (BVS)
!
!        Added more details to the header. Add an exception for
!        un-initialized divisor. Added one more exception for
!        a negative output value.
!
!-    SPICELIB Version 1.1.0, 06-JUL-2005 (EDW)
!
!        Added error test to catch non-positive hash values.
!
!-    SPICELIB Version 1.0.0, 20-SEP-1995 (WLT)
!
!-&

!
!     Check if divisor was initialized by a prior call to ZZSHSH.
!
      IF ( DIVISR .EQ. -1 ) THEN

         ZZHASH = 0

         CALL CHKIN  ( 'ZZHASH'                                     )
         CALL SETMSG ( 'The ZZHASH function was called before the ' // &
                       'POOL hash parameters were initialized by '  // &
                       'a call to ZZSHSH.'                          )
         CALL SIGERR ( 'SPICE(CALLEDOUTOFORDER)'                    )
         CALL CHKOUT ( 'ZZHASH'                                     )
         RETURN

      END IF

!
!     Compute hash value for the input string.
!
      F      = 0
      LENGTH = LEN( WORD )

      DO I = 1, LENGTH

         IF ( ICHAR(WORD(I:I)) .EQ. BLANK ) THEN

            ZZHASH = MOD ( F*BASE, DIVISR ) + 1

!
!           A negative value for ZZHASH indicates a serious problem.
!
            IF( ZZHASH .LT. 0 ) THEN

               CALL CHKIN  ( 'ZZHASH'                             )
               CALL SETMSG ( 'The ZZHASH function calculated a '  // &
                             'negative value for string $1. '     // &
                             'Contact NAIF.'                      )
               CALL ERRCH  ( '$1', WORD                           )
               CALL SIGERR ( 'SPICE(NEGATIVEHASHVALUE1)'          )
               CALL CHKOUT ( 'ZZHASH'                             )

            END IF

            RETURN

         END IF

         F = VAL(  MIN( 128, ICHAR(WORD(I:I)) )  ) + F * BASE
         F = MOD( F, DIVISR )

      END DO

      ZZHASH = MOD( F*BASE, DIVISR ) + 1

!
!     A negative value for ZZHASH indicates a serious problem.
!
      IF( ZZHASH .LT. 0 ) THEN

         CALL CHKIN  ( 'ZZHASH'                              )
         CALL SETMSG ( 'The ZZHASH function calculated a '  // &
                       'negative value for string $1. '     // &
                       'Contact NAIF.'                       )
         CALL ERRCH  ( '$1', WORD                            )
         CALL SIGERR ( 'SPICE(NEGATIVEHASHVALUE2)'           )
         CALL CHKOUT ( 'ZZHASH'                              )

      END IF

      RETURN


!$Procedure ZZHASH2 ( Private---Arbitrary divisor hash function )

      ENTRY ZZHASH2 ( WORD, M2 )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine computes the hash value corresponding to an string
!     given a particular divisor value (M2).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PRIVATE UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         WORD
!     INTEGER               M2
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     WORD       I   A left justified string of characters.
!     M2         I   Divisor used for the hash function
!
!     The function returns the hash value associated with WORD.
!
! $ Detailed_Input
!
!     WORD        is a left justified string of characters.
!
!     M2          the divisor of the hashing function. This value
!                 defines the spread of the hash values, that spread
!                 covering the interval [1, M2]. The larger the M2
!                 value, the less the chance of a hash key collision.
!                 The user should always chose a prime for M2. The
!                 value of M2 must be in the range from 1 to INTMAX/68
!                 - 1. If it is not, the function signals an error and
!                 returns 0.
!
! $ Detailed_Output
!
!     The function returns the hash value of WORD as computed using M2
!     as the hash function divisor.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the input divisor value is not in the allowed range, the
!        error 'SPICE(INVALIDDIVISOR)' will be signaled.
!
!     2) If this routine calculates a negative value, the error
!        SPICE(NEGATIVEHASHVALUE1) or SPICE(NEGATIVEHASHVALUE2) will be
!        signaled. This should never occur.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine computes the hash value of a string of characters
!     using the user specified hash divisor value. The algorithm
!     implemented by this function is case-insensitive and uses only
!     the first word of the input string. In order to make effective
!     use of this hash algorithm, input strings should be
!     left-justified and space-less. All non-left justified strings map
!     to the same value 0.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     1)  Knuth, Donald E. "The Art of Computer Programming, Volume
!         3/Sorting and Searching 2nd Edition" 1997, pp 513-521.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     E.D. Wright     (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 31-JUL-2013 (BVS)
!
!        Added more details to the header. Added computation of the
!        maximum allowed divisor value to the initialization block.
!        Added a check for the input divisor to be in the allowed
!        range. Added one more exception for a negative output value.
!
!-    SPICELIB Version 1.0.0, 14-SEP-2005 (EDW)
!
!-&

!
!     The initialization block below is identical to the initialization
!     block in the entry ZZSHSH. If this block is changed in any way,
!     the block in ZZSHSH must be changed in the same way.
!
      IF ( FIRST ) THEN

         FIRST = .FALSE.

         BASE  = 68
         BLANK = ICHAR( ' ' )

         MAXDIV = INTMAX() / BASE - 1

         DO I = 0, 128
            VAL(I) = 0
         END DO

         VAL(ICHAR('0')) = 1
         VAL(ICHAR('1')) = 2
         VAL(ICHAR('2')) = 3
         VAL(ICHAR('3')) = 4
         VAL(ICHAR('4')) = 5
         VAL(ICHAR('5')) = 6
         VAL(ICHAR('6')) = 7
         VAL(ICHAR('7')) = 8
         VAL(ICHAR('8')) = 9
         VAL(ICHAR('9')) = 10
         VAL(ICHAR('A')) = 11
         VAL(ICHAR('B')) = 12
         VAL(ICHAR('C')) = 13
         VAL(ICHAR('D')) = 14
         VAL(ICHAR('E')) = 15
         VAL(ICHAR('F')) = 16
         VAL(ICHAR('G')) = 17
         VAL(ICHAR('H')) = 18
         VAL(ICHAR('I')) = 19
         VAL(ICHAR('J')) = 20
         VAL(ICHAR('K')) = 21
         VAL(ICHAR('L')) = 22
         VAL(ICHAR('M')) = 23
         VAL(ICHAR('N')) = 24
         VAL(ICHAR('O')) = 25
         VAL(ICHAR('P')) = 26
         VAL(ICHAR('Q')) = 27
         VAL(ICHAR('R')) = 28
         VAL(ICHAR('S')) = 29
         VAL(ICHAR('T')) = 30
         VAL(ICHAR('U')) = 31
         VAL(ICHAR('V')) = 32
         VAL(ICHAR('W')) = 33
         VAL(ICHAR('X')) = 34
         VAL(ICHAR('Y')) = 35
         VAL(ICHAR('Z')) = 36
         VAL(ICHAR('-')) = 37
         VAL(ICHAR('_')) = 38
         VAL(ICHAR('.')) = 39
         VAL(ICHAR('/')) = 40
         VAL(ICHAR('!')) = 41
         VAL(ICHAR('@')) = 42
         VAL(ICHAR('#')) = 43
         VAL(ICHAR('$')) = 44
         VAL(ICHAR('%')) = 45
         VAL(ICHAR('^')) = 46
         VAL(ICHAR('&')) = 47
         VAL(ICHAR('*')) = 48
         VAL(ICHAR('(')) = 49
         VAL(ICHAR(')')) = 50
         VAL(ICHAR('+')) = 51
         VAL(ICHAR('=')) = 52
         VAL(ICHAR('[')) = 53
         VAL(ICHAR('{')) = 54
         VAL(ICHAR(']')) = 55
         VAL(ICHAR('}')) = 56
         VAL(ICHAR('|')) = 57
         VAL(BSLASH    ) = 58
         VAL(ICHAR(':')) = 59
         VAL(ICHAR(';')) = 60
         VAL(ICHAR('<')) = 61
         VAL(ICHAR(',')) = 62
         VAL(ICHAR('>')) = 63
         VAL(ICHAR('?')) = 64

!
!        Note, ICHAR('''') returns the ASCII value for the single
!        quote -> '.
!
         VAL(ICHAR(''''))= 65
         VAL(ICHAR('"')) = 66
         VAL(ICHAR('`')) = 67
         VAL(ICHAR('~')) = 68

         VAL(ICHAR('a')) = VAL(ICHAR('A'))
         VAL(ICHAR('b')) = VAL(ICHAR('B'))
         VAL(ICHAR('c')) = VAL(ICHAR('C'))
         VAL(ICHAR('d')) = VAL(ICHAR('D'))
         VAL(ICHAR('e')) = VAL(ICHAR('E'))
         VAL(ICHAR('f')) = VAL(ICHAR('F'))
         VAL(ICHAR('g')) = VAL(ICHAR('G'))
         VAL(ICHAR('h')) = VAL(ICHAR('H'))
         VAL(ICHAR('i')) = VAL(ICHAR('I'))
         VAL(ICHAR('j')) = VAL(ICHAR('J'))
         VAL(ICHAR('k')) = VAL(ICHAR('K'))
         VAL(ICHAR('l')) = VAL(ICHAR('L'))
         VAL(ICHAR('m')) = VAL(ICHAR('M'))
         VAL(ICHAR('n')) = VAL(ICHAR('N'))
         VAL(ICHAR('o')) = VAL(ICHAR('O'))
         VAL(ICHAR('p')) = VAL(ICHAR('P'))
         VAL(ICHAR('q')) = VAL(ICHAR('Q'))
         VAL(ICHAR('r')) = VAL(ICHAR('R'))
         VAL(ICHAR('s')) = VAL(ICHAR('S'))
         VAL(ICHAR('t')) = VAL(ICHAR('T'))
         VAL(ICHAR('u')) = VAL(ICHAR('U'))
         VAL(ICHAR('v')) = VAL(ICHAR('V'))
         VAL(ICHAR('w')) = VAL(ICHAR('W'))
         VAL(ICHAR('x')) = VAL(ICHAR('X'))
         VAL(ICHAR('y')) = VAL(ICHAR('Y'))
         VAL(ICHAR('z')) = VAL(ICHAR('Z'))

      END IF

!
!     Check divisor.
!
      IF ( M2 .LE. 0 .OR. M2 .GT. MAXDIV ) THEN

         ZZHASH2 = 0

         CALL CHKIN  ( 'ZZHASH2'                                     )
         CALL SETMSG ( 'The input hash function divisor was not '    // &
                       'in the allowed range from 1 to #. It was #.' )
         CALL ERRINT ( '#', MAXDIV                                   )
         CALL ERRINT ( '#', M2                                       )
         CALL SIGERR ( 'SPICE(INVALIDDIVISOR)'                       )
         CALL CHKOUT ( 'ZZHASH2'                                     )
         RETURN

      END IF

!
!     Compute hash value for the input string.
!
      F      = 0
      LENGTH = LEN( WORD )

      DO I = 1, LENGTH

         IF ( ICHAR(WORD(I:I)) .EQ. BLANK ) THEN

            ZZHASH2 = MOD ( F*BASE, M2 ) + 1

!
!           A negative value for ZZHASH2 indicates a serious problem.
!
            IF( ZZHASH2 .LT. 0 ) THEN

               CALL CHKIN  ( 'ZZHASH2'                            )
               CALL SETMSG ( 'The ZZHASH2 function calculated a ' // &
                             'negative value for string $1. '     // &
                             'Contact NAIF.'                      )
               CALL ERRCH  ( '$1', WORD                           )
               CALL SIGERR ( 'SPICE(NEGATIVEHASHVALUE1)'          )
               CALL CHKOUT ( 'ZZHASH2'                            )

            END IF

            RETURN

         END IF

         F = VAL(  MIN( 128, ICHAR(WORD(I:I)) )  ) + F * BASE
         F = MOD( F, M2 )

      END DO

      ZZHASH2 = MOD( F*BASE, M2 ) + 1

!
!     A negative value for ZZHASH2 indicates a serious problem.
!
      IF( ZZHASH2 .LT. 0 ) THEN

         CALL CHKIN  ( 'ZZHASH2'                             )
         CALL SETMSG ( 'The ZZHASH2 function calculated a ' // &
                       'negative value for string $1. '     // &
                       'Contact NAIF.'                       )
         CALL ERRCH  ( '$1', WORD                            )
         CALL SIGERR ( 'SPICE(NEGATIVEHASHVALUE2)'           )
         CALL CHKOUT ( 'ZZHASH2'                             )

      END IF

      RETURN
      END


