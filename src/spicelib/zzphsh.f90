!$Procedure ZZPHSH ( Private---kernel POOL hash function umbrella )

      integer(ip) function zzphsh ( word, m, m2 )
      use pepper_kinds, only: ip

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

      implicit none
      character(*)          word
      integer(ip)           m
      integer(ip)           m2

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
      integer(ip)           zzshsh
      integer(ip)           zzhash
      integer(ip)           zzhash2

!
!     SPICELIB functions
!
      integer(ip)           intmax

!
!     Local Variables.
!
      integer(ip)           i
      integer(ip)           divisr
      integer(ip)           val ( 0:128 )
      integer(ip)           f
      integer(ip)           base
      integer(ip)           blank
      integer(ip)           length
      integer(ip)           maxdiv

      integer(ip), parameter :: bslash = 92


      logical               first
      save

      data                  first / .true. /

      data                  divisr / -1 /

!
!     We do not diagnose a bogus call since this is a private routine.
!
      zzphsh = 0

      return


!$Procedure ZZSHSH ( Private---Set up POOL hash function )

      entry zzshsh ( m )

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
      zzshsh = 0

!
!     The initialization block below is identical to the initialization
!     block in the entry ZZHASH2. If this block is changed in any way,
!     the block in ZZHASH2 must be changed in the same way.
!
      if ( first ) then

         first = .false.

         base  = 68
         blank = ichar( ' ' )

         maxdiv = intmax() / base - 1

         do i = 0, 128
            val(i) = 0
         end do

         val(ichar('0')) = 1
         val(ichar('1')) = 2
         val(ichar('2')) = 3
         val(ichar('3')) = 4
         val(ichar('4')) = 5
         val(ichar('5')) = 6
         val(ichar('6')) = 7
         val(ichar('7')) = 8
         val(ichar('8')) = 9
         val(ichar('9')) = 10
         val(ichar('A')) = 11
         val(ichar('B')) = 12
         val(ichar('C')) = 13
         val(ichar('D')) = 14
         val(ichar('E')) = 15
         val(ichar('F')) = 16
         val(ichar('G')) = 17
         val(ichar('H')) = 18
         val(ichar('I')) = 19
         val(ichar('J')) = 20
         val(ichar('K')) = 21
         val(ichar('L')) = 22
         val(ichar('M')) = 23
         val(ichar('N')) = 24
         val(ichar('O')) = 25
         val(ichar('P')) = 26
         val(ichar('Q')) = 27
         val(ichar('R')) = 28
         val(ichar('S')) = 29
         val(ichar('T')) = 30
         val(ichar('U')) = 31
         val(ichar('V')) = 32
         val(ichar('W')) = 33
         val(ichar('X')) = 34
         val(ichar('Y')) = 35
         val(ichar('Z')) = 36
         val(ichar('-')) = 37
         val(ichar('_')) = 38
         val(ichar('.')) = 39
         val(ichar('/')) = 40
         val(ichar('!')) = 41
         val(ichar('@')) = 42
         val(ichar('#')) = 43
         val(ichar('$')) = 44
         val(ichar('%')) = 45
         val(ichar('^')) = 46
         val(ichar('&')) = 47
         val(ichar('*')) = 48
         val(ichar('(')) = 49
         val(ichar(')')) = 50
         val(ichar('+')) = 51
         val(ichar('=')) = 52
         val(ichar('[')) = 53
         val(ichar('{')) = 54
         val(ichar(']')) = 55
         val(ichar('}')) = 56
         val(ichar('|')) = 57
         val(bslash    ) = 58
         val(ichar(':')) = 59
         val(ichar(';')) = 60
         val(ichar('<')) = 61
         val(ichar(',')) = 62
         val(ichar('>')) = 63
         val(ichar('?')) = 64

!
!        Note, ICHAR('''') returns the ASCII value for the single
!        quote -> '.
!
         val(ichar(''''))= 65
         val(ichar('"')) = 66
         val(ichar('`')) = 67
         val(ichar('~')) = 68

         val(ichar('a')) = val(ichar('A'))
         val(ichar('b')) = val(ichar('B'))
         val(ichar('c')) = val(ichar('C'))
         val(ichar('d')) = val(ichar('D'))
         val(ichar('e')) = val(ichar('E'))
         val(ichar('f')) = val(ichar('F'))
         val(ichar('g')) = val(ichar('G'))
         val(ichar('h')) = val(ichar('H'))
         val(ichar('i')) = val(ichar('I'))
         val(ichar('j')) = val(ichar('J'))
         val(ichar('k')) = val(ichar('K'))
         val(ichar('l')) = val(ichar('L'))
         val(ichar('m')) = val(ichar('M'))
         val(ichar('n')) = val(ichar('N'))
         val(ichar('o')) = val(ichar('O'))
         val(ichar('p')) = val(ichar('P'))
         val(ichar('q')) = val(ichar('Q'))
         val(ichar('r')) = val(ichar('R'))
         val(ichar('s')) = val(ichar('S'))
         val(ichar('t')) = val(ichar('T'))
         val(ichar('u')) = val(ichar('U'))
         val(ichar('v')) = val(ichar('V'))
         val(ichar('w')) = val(ichar('W'))
         val(ichar('x')) = val(ichar('X'))
         val(ichar('y')) = val(ichar('Y'))
         val(ichar('z')) = val(ichar('Z'))

      end if

!
!     Check and save divisor.
!
      if ( m <= 0 .or. m > maxdiv ) then

         call setmsg ( 'The input hash function divisor was not '    // &
                       'in the allowed range from 1 to #. It was #.' )
         call errint ( '#', maxdiv                                   )
         call errint ( '#', m                                        )
         call sigerr ( 'SPICE(INVALIDDIVISOR)'                       )
         return

      end if

      divisr = m

      return


!$Procedure ZZHASH ( Private---POOL Hash function )

      entry zzhash ( word )

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
      if ( divisr == -1 ) then

         zzhash = 0

         call setmsg ( 'The ZZHASH function was called before the ' // &
                       'POOL hash parameters were initialized by '  // &
                       'a call to ZZSHSH.'                          )
         call sigerr ( 'SPICE(CALLEDOUTOFORDER)'                    )
         return

      end if

!
!     Compute hash value for the input string.
!
      f      = 0
      length = len( word )

      do i = 1, length

         if ( ichar(word(i:i)) == blank ) then

            zzhash = mod ( f*base, divisr ) + 1

!
!           A negative value for ZZHASH indicates a serious problem.
!
            if( zzhash < 0 ) then

               call setmsg ( 'The ZZHASH function calculated a '  // &
                             'negative value for string $1. '     // &
                             'Contact NAIF.'                      )
               call errch  ( '$1', word                           )
               call sigerr ( 'SPICE(NEGATIVEHASHVALUE1)'          )

            end if

            return

         end if

         f = val(  min( 128, ichar(word(i:i)) )  ) + f * base
         f = mod( f, divisr )

      end do

      zzhash = mod( f*base, divisr ) + 1

!
!     A negative value for ZZHASH indicates a serious problem.
!
      if( zzhash < 0 ) then

         call setmsg ( 'The ZZHASH function calculated a '  // &
                       'negative value for string $1. '     // &
                       'Contact NAIF.'                       )
         call errch  ( '$1', word                            )
         call sigerr ( 'SPICE(NEGATIVEHASHVALUE2)'           )

      end if

      return


!$Procedure ZZHASH2 ( Private---Arbitrary divisor hash function )

      entry zzhash2 ( word, m2 )

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
      if ( first ) then

         first = .false.

         base  = 68
         blank = ichar( ' ' )

         maxdiv = intmax() / base - 1

         do i = 0, 128
            val(i) = 0
         end do

         val(ichar('0')) = 1
         val(ichar('1')) = 2
         val(ichar('2')) = 3
         val(ichar('3')) = 4
         val(ichar('4')) = 5
         val(ichar('5')) = 6
         val(ichar('6')) = 7
         val(ichar('7')) = 8
         val(ichar('8')) = 9
         val(ichar('9')) = 10
         val(ichar('A')) = 11
         val(ichar('B')) = 12
         val(ichar('C')) = 13
         val(ichar('D')) = 14
         val(ichar('E')) = 15
         val(ichar('F')) = 16
         val(ichar('G')) = 17
         val(ichar('H')) = 18
         val(ichar('I')) = 19
         val(ichar('J')) = 20
         val(ichar('K')) = 21
         val(ichar('L')) = 22
         val(ichar('M')) = 23
         val(ichar('N')) = 24
         val(ichar('O')) = 25
         val(ichar('P')) = 26
         val(ichar('Q')) = 27
         val(ichar('R')) = 28
         val(ichar('S')) = 29
         val(ichar('T')) = 30
         val(ichar('U')) = 31
         val(ichar('V')) = 32
         val(ichar('W')) = 33
         val(ichar('X')) = 34
         val(ichar('Y')) = 35
         val(ichar('Z')) = 36
         val(ichar('-')) = 37
         val(ichar('_')) = 38
         val(ichar('.')) = 39
         val(ichar('/')) = 40
         val(ichar('!')) = 41
         val(ichar('@')) = 42
         val(ichar('#')) = 43
         val(ichar('$')) = 44
         val(ichar('%')) = 45
         val(ichar('^')) = 46
         val(ichar('&')) = 47
         val(ichar('*')) = 48
         val(ichar('(')) = 49
         val(ichar(')')) = 50
         val(ichar('+')) = 51
         val(ichar('=')) = 52
         val(ichar('[')) = 53
         val(ichar('{')) = 54
         val(ichar(']')) = 55
         val(ichar('}')) = 56
         val(ichar('|')) = 57
         val(bslash    ) = 58
         val(ichar(':')) = 59
         val(ichar(';')) = 60
         val(ichar('<')) = 61
         val(ichar(',')) = 62
         val(ichar('>')) = 63
         val(ichar('?')) = 64

!
!        Note, ICHAR('''') returns the ASCII value for the single
!        quote -> '.
!
         val(ichar(''''))= 65
         val(ichar('"')) = 66
         val(ichar('`')) = 67
         val(ichar('~')) = 68

         val(ichar('a')) = val(ichar('A'))
         val(ichar('b')) = val(ichar('B'))
         val(ichar('c')) = val(ichar('C'))
         val(ichar('d')) = val(ichar('D'))
         val(ichar('e')) = val(ichar('E'))
         val(ichar('f')) = val(ichar('F'))
         val(ichar('g')) = val(ichar('G'))
         val(ichar('h')) = val(ichar('H'))
         val(ichar('i')) = val(ichar('I'))
         val(ichar('j')) = val(ichar('J'))
         val(ichar('k')) = val(ichar('K'))
         val(ichar('l')) = val(ichar('L'))
         val(ichar('m')) = val(ichar('M'))
         val(ichar('n')) = val(ichar('N'))
         val(ichar('o')) = val(ichar('O'))
         val(ichar('p')) = val(ichar('P'))
         val(ichar('q')) = val(ichar('Q'))
         val(ichar('r')) = val(ichar('R'))
         val(ichar('s')) = val(ichar('S'))
         val(ichar('t')) = val(ichar('T'))
         val(ichar('u')) = val(ichar('U'))
         val(ichar('v')) = val(ichar('V'))
         val(ichar('w')) = val(ichar('W'))
         val(ichar('x')) = val(ichar('X'))
         val(ichar('y')) = val(ichar('Y'))
         val(ichar('z')) = val(ichar('Z'))

      end if

!
!     Check divisor.
!
      if ( m2 <= 0 .or. m2 > maxdiv ) then

         zzhash2 = 0

         call setmsg ( 'The input hash function divisor was not '    // &
                       'in the allowed range from 1 to #. It was #.' )
         call errint ( '#', maxdiv                                   )
         call errint ( '#', m2                                       )
         call sigerr ( 'SPICE(INVALIDDIVISOR)'                       )
         return

      end if

!
!     Compute hash value for the input string.
!
      f      = 0
      length = len( word )

      do i = 1, length

         if ( ichar(word(i:i)) == blank ) then

            zzhash2 = mod ( f*base, m2 ) + 1

!
!           A negative value for ZZHASH2 indicates a serious problem.
!
            if( zzhash2 < 0 ) then

               call setmsg ( 'The ZZHASH2 function calculated a ' // &
                             'negative value for string $1. '     // &
                             'Contact NAIF.'                      )
               call errch  ( '$1', word                           )
               call sigerr ( 'SPICE(NEGATIVEHASHVALUE1)'          )

            end if

            return

         end if

         f = val(  min( 128, ichar(word(i:i)) )  ) + f * base
         f = mod( f, m2 )

      end do

      zzhash2 = mod( f*base, m2 ) + 1

!
!     A negative value for ZZHASH2 indicates a serious problem.
!
      if( zzhash2 < 0 ) then

         call setmsg ( 'The ZZHASH2 function calculated a ' // &
                       'negative value for string $1. '     // &
                       'Contact NAIF.'                       )
         call errch  ( '$1', word                            )
         call sigerr ( 'SPICE(NEGATIVEHASHVALUE2)'           )

      end if

      return
      end


