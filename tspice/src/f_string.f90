!$Procedure F_STRING ( Family of tests for string routines )

      SUBROUTINE F_STRING ( OK )

      IMPLICIT NONE

! $ Abstract
!
!     This routine tests string manipulation routines.
!
! $ Required_Reading
!
!     None
!
! $ Keywords
!
!     None
!
! $ Declarations

      LOGICAL               OK

! $ Brief_I/O
!
!     None
!
! $ Detailed_Input
!
!     None
!
! $ Detailed_Output
!
!     None
!
! $ Parameters
!
!     None
!
! $ Exceptions
!
!     None
!
! $ Files
!
!     None
!
! $ Particulars
!
!     None
!
! $ Examples
!
!     None
!
! $ Restrictions
!
!     None
!
! $ Literature_References
!
!     None
!
! $ Author_and_Institution
!
!     None
!
! $ Version
!
!-    Version 1.0.0  29-JUL-2013 (BVS)
!
!-&

! $ Index_Entries
!
!     None
!
!-&

!
!     Test Utility Functions
!
      INTEGER               LASTNB

!
!     SPICELIB Functions
!

!
!     Local parameters
!
      INTEGER               SMLSIZ
      PARAMETER           ( SMLSIZ = 8 )

      INTEGER               LRGSIZ
      PARAMETER           ( LRGSIZ = 16 )


      INTEGER               NSTR
      PARAMETER           ( NSTR = 13 )

!
!     Local Variables
!

      CHARACTER*(SMLSIZ)    SSTR
      CHARACTER*(SMLSIZ)    SSTR1
      CHARACTER*(SMLSIZ)    SARRAY ( NSTR )
      CHARACTER*(SMLSIZ)    SLJUST ( NSTR )
      CHARACTER*(SMLSIZ)    SUCASE ( NSTR )
      CHARACTER*(SMLSIZ)    SLCASE ( NSTR )
      CHARACTER*(SMLSIZ)    SLJSTU ( NSTR )
      CHARACTER*(SMLSIZ)    SCMPR0 ( NSTR )
      CHARACTER*(SMLSIZ)    SCMPR1 ( NSTR )
      CHARACTER*(SMLSIZ)    SCMPR2 ( NSTR )
      CHARACTER*(SMLSIZ)    SLUCR0 ( NSTR )
      CHARACTER*(SMLSIZ)    SLUCR1 ( NSTR )
      CHARACTER*(SMLSIZ)    SLUCR2 ( NSTR )

      CHARACTER*(LRGSIZ)    LSTR
      CHARACTER*(LRGSIZ)    LSTR1
      CHARACTER*(LRGSIZ)    LARRAY ( NSTR )
      CHARACTER*(LRGSIZ)    LLJUST ( NSTR )
      CHARACTER*(LRGSIZ)    LUCASE ( NSTR )
      CHARACTER*(LRGSIZ)    LLCASE ( NSTR )
      CHARACTER*(LRGSIZ)    LLJSTU ( NSTR )
      CHARACTER*(LRGSIZ)    LCMPR0 ( NSTR )
      CHARACTER*(LRGSIZ)    LCMPR1 ( NSTR )
      CHARACTER*(LRGSIZ)    LCMPR2 ( NSTR )
      CHARACTER*(LRGSIZ)    LLUCR0 ( NSTR )
      CHARACTER*(LRGSIZ)    LLUCR1 ( NSTR )
      CHARACTER*(LRGSIZ)    LLUCR2 ( NSTR )

      INTEGER               I
      INTEGER               R
      INTEGER               E

!
!     Save all
!
      SAVE

!
!     Initial values
!

!
!     Short string array.
!
      DATA                     SARRAY / &
         'a A a A ', ' b   B  ', '  c C c ', '   d   D', '    e E ', &
         '     f  ', '      g ', '       h', 'i-I-i-I ', ' j---J  ', &
         '  k-K-k ', '   l---L', '    m-M ' &
                                      /

!
!     Short string array, left-justified.
!
      DATA                     SLJUST / &
         'a A a A ', 'b   B   ', 'c C c   ', 'd   D   ', 'e E     ', &
         'f       ', 'g       ', 'h       ', 'i-I-i-I ', 'j---J   ', &
         'k-K-k   ', 'l---L   ', 'm-M     ' &
                                      /

!
!     Short string array, upper-cased.
!
      DATA                     SUCASE / &
         'A A A A ', ' B   B  ', '  C C C ', '   D   D', '    E E ', &
         '     F  ', '      G ', '       H', 'I-I-I-I ', ' J---J  ', &
         '  K-K-K ', '   L---L', '    M-M ' &
                                      /

!
!     Short string array, lower-cased.
!
      DATA                     SLCASE / &
         'a a a a ', ' b   b  ', '  c c c ', '   d   d', '    e e ', &
         '     f  ', '      g ', '       h', 'i-i-i-i ', ' j---j  ', &
         '  k-k-k ', '   l---l', '    m-m ' &
                                      /

!
!     Short string array, left-justified and upper-cased.
!
      DATA                     SLJSTU / &
         'A A A A ', 'B   B   ', 'C C C   ', 'D   D   ', 'E E     ', &
         'F       ', 'G       ', 'H       ', 'I-I-I-I ', 'J---J   ', &
         'K-K-K   ', 'L---L   ', 'M-M     ' &
                                      /

!
!     Short string array, compressed, 0 spaces.
!
      DATA                     SCMPR0 / &
         'aAaA    ', 'bB      ', 'cCc     ', 'dD      ', 'eE      ', &
         'f       ', 'g       ', 'h       ', 'i-I-i-I ', 'j---J   ', &
         'k-K-k   ', 'l---L   ', 'm-M     ' &
                                      /

!
!     Short string array, compressed, 1 space.
!
      DATA                     SCMPR1 / &
         'a A a A ', ' b B    ', ' c C c  ', ' d D    ', ' e E    ', &
         ' f      ', ' g      ', ' h      ', 'i-I-i-I ', ' j---J  ', &
         ' k-K-k  ', ' l---L  ', ' m-M    ' &
                                      /

!
!     Short string array, compressed, 2 spaces.
!
      DATA                     SCMPR2 / &
         'a A a A ', ' b  B   ', '  c C c ', '  d  D  ', '  e E   ', &
         '  f     ', '  g     ', '  h     ', 'i-I-i-I ', ' j---J  ', &
         '  k-K-k ', '  l---L ', '  m-M   ' &
                                      /

!
!     Short string array, left-justified, upper-cased, compressed, 0
!     spaces.
!
      DATA                     SLUCR0 / &
         'AAAA    ', 'BB      ', 'CCC     ', 'DD      ', 'EE      ', &
         'F       ', 'G       ', 'H       ', 'I-I-I-I ', 'J---J   ', &
         'K-K-K   ', 'L---L   ', 'M-M     ' &
                                      /

!
!     Short string array, left-justified, upper-cased, compressed, 1
!     space.
!
      DATA                     SLUCR1 / &
         'A A A A ', 'B B     ', 'C C C   ', 'D D     ', 'E E     ', &
         'F       ', 'G       ', 'H       ', 'I-I-I-I ', 'J---J   ', &
         'K-K-K   ', 'L---L   ', 'M-M     ' &
                                      /

!
!     Short string array, left-justified, upper-cased, compressed, 2
!     spaces.
!
      DATA                     SLUCR2 / &
         'A A A A ', 'B  B    ', 'C C C   ', 'D  D    ', 'E E     ', &
         'F       ', 'G       ', 'H       ', 'I-I-I-I ', 'J---J   ', &
         'K-K-K   ', 'L---L   ', 'M-M     ' &
                                      /

!
!     Long string array.
!
      DATA                     LARRAY / &
         'N n N n N n N n ', ' O+++o+++O+++o  ', '  P p P p P p P ', &
         '   Q   q   Q   q', '    R r R r R r ', '     S   s   S  ', &
         '      T t T t T ', '       U   u   U', '        V/v/V/v ', &
         '         W   w  ', '          X x X ', '           Y   y', &
         '            Z z ' &
                                      /

!
!     Long string array, left-justified.
!
      DATA                     LLJUST / &
         'N n N n N n N n ', 'O+++o+++O+++o   ', 'P p P p P p P   ', &
         'Q   q   Q   q   ', 'R r R r R r     ', 'S   s   S       ', &
         'T t T t T       ', 'U   u   U       ', 'V/v/V/v         ', &
         'W   w           ', 'X x X           ', 'Y   y           ', &
         'Z z             ' &
                                      /

!
!     Long string array, upper-cased,
!
      DATA                     LUCASE / &
         'N N N N N N N N ', ' O+++O+++O+++O  ', '  P P P P P P P ', &
         '   Q   Q   Q   Q', '    R R R R R R ', '     S   S   S  ', &
         '      T T T T T ', '       U   U   U', '        V/V/V/V ', &
         '         W   W  ', '          X X X ', '           Y   Y', &
         '            Z Z ' &
                                      /

!
!     Long string array, lower-cased.
!
      DATA                     LLCASE / &
         'n n n n n n n n ', ' o+++o+++o+++o  ', '  p p p p p p p ', &
         '   q   q   q   q', '    r r r r r r ', '     s   s   s  ', &
         '      t t t t t ', '       u   u   u', '        v/v/v/v ', &
         '         w   w  ', '          x x x ', '           y   y', &
         '            z z ' &
                                      /

!
!     Long string array, left-justified, upper-cased.
!
      DATA                     LLJSTU / &
         'N N N N N N N N ', 'O+++O+++O+++O   ', 'P P P P P P P   ', &
         'Q   Q   Q   Q   ', 'R R R R R R     ', 'S   S   S       ', &
         'T T T T T       ', 'U   U   U       ', 'V/V/V/V         ', &
         'W   W           ', 'X X X           ', 'Y   Y           ', &
         'Z Z             ' &
                                      /

!
!     Long string array, compressed, 0 spaces.
!
      DATA                     LCMPR0 / &
         'NnNnNnNn        ', 'O+++o+++O+++o   ', 'PpPpPpP         ', &
         'QqQq            ', 'RrRrRr          ', 'SsS             ', &
         'TtTtT           ', 'UuU             ', 'V/v/V/v         ', &
         'Ww              ', 'XxX             ', 'Yy              ', &
         'Zz              ' &
                                      /

!
!     Long string array, compressed, 1 space.
!
      DATA                     LCMPR1 / &
         'N n N n N n N n ', ' O+++o+++O+++o  ', ' P p P p P p P  ', &
         ' Q q Q q        ', ' R r R r R r    ', ' S s S          ', &
         ' T t T t T      ', ' U u U          ', ' V/v/V/v        ', &
         ' W w            ', ' X x X          ', ' Y y            ', &
         ' Z z            ' &
                                      /

!
!     Long string array, compressed, 2 spaces.
!
      DATA                     LCMPR2 / &
         'N n N n N n N n ', ' O+++o+++O+++o  ', '  P p P p P p P ', &
         '  Q  q  Q  q    ', '  R r R r R r   ', '  S  s  S       ', &
         '  T t T t T     ', '  U  u  U       ', '  V/v/V/v       ', &
         '  W  w          ', '  X x X         ', '  Y  y          ', &
         '  Z z           ' &
                                      /

!
!     Long string array, left-justified, upper-cased, compressed, 0
!     spaces.
!
      DATA                     LLUCR0 / &
         'NNNNNNNN        ', 'O+++O+++O+++O   ', 'PPPPPPP         ', &
         'QQQQ            ', 'RRRRRR          ', 'SSS             ', &
         'TTTTT           ', 'UUU             ', 'V/V/V/V         ', &
         'WW              ', 'XXX             ', 'YY              ', &
         'ZZ              ' &
                                      /

!
!     Long string array, left-justified, upper-cased, compressed, 1
!     space.
!
      DATA                     LLUCR1 / &
         'N N N N N N N N ', 'O+++O+++O+++O   ', 'P P P P P P P   ', &
         'Q Q Q Q         ', 'R R R R R R     ', 'S S S           ', &
         'T T T T T       ', 'U U U           ', 'V/V/V/V         ', &
         'W W             ', 'X X X           ', 'Y Y             ', &
         'Z Z             ' &
                                      /

!
!     Long string array, left-justified, upper-cased, compressed, 2
!     spaces.
!
      DATA                     LLUCR2 / &
         'N N N N N N N N ', 'O+++O+++O+++O   ', 'P P P P P P P   ', &
         'Q  Q  Q  Q      ', 'R R R R R R     ', 'S  S  S         ', &
         'T T T T T       ', 'U  U  U         ', 'V/V/V/V         ', &
         'W  W            ', 'X X X           ', 'Y  Y            ', &
         'Z Z             ' &
                                      /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_string' )

!
!     LJUST tests.
!
      CALL TCASE ( 'LJUST: short to short' )
      DO I = 1, NSTR
         CALL LJUST  ( SARRAY(I), SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SLJUST(I),  OK )
      END DO

      CALL TCASE ( 'LJUST: short to short, same I/O' )
      DO I = 1, NSTR
         SSTR = SARRAY(I)
         CALL LJUST  ( SSTR, SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SLJUST(I),  OK )
      END DO

      CALL TCASE ( 'LJUST: long to long' )
      DO I = 1, NSTR
         CALL LJUST  ( LARRAY(I), LSTR )
         CALL CHCKSC ( 'LARRAY(I)', LSTR, '=', LLJUST(I),  OK )
      END DO

      CALL TCASE ( 'LJUST: short to long' )
      DO I = 1, NSTR
         LSTR1 = ' '
         R = LASTNB( SLJUST(I) )
         E = MIN(R,LEN(LSTR1))
         LSTR1(1:E) = SLJUST(I)(1:E)
         CALL LJUST  ( SARRAY(I), LSTR )
         CALL CHCKSC ( 'SARRAY(I)', LSTR, '=', LSTR1,  OK )
      END DO

      CALL TCASE ( 'LJUST: long to short' )
      DO I = 1, NSTR
         SSTR1 = ' '
         R = LASTNB( LLJUST(I) )
         E = MIN(R,LEN(SSTR1))
         SSTR1(1:E) = LLJUST(I)(1:E)
         CALL LJUST  ( LARRAY(I), SSTR )
         CALL CHCKSC ( 'LARRAY(I)', SSTR, '=', SSTR1,  OK )
      END DO

      CALL TCASE ( 'LJUST: blank' )
      SSTR = ' '
      SSTR1 = 'boo'

      CALL LJUST  ( SSTR, SSTR1 )
      CALL CHCKSC ( 'SLMST1', SSTR1, '=', ' ',  OK )


!
!     UCASE tests.
!
      CALL TCASE ( 'UCASE: short to short' )
      DO I = 1, NSTR
         CALL UCASE  ( SARRAY(I), SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SUCASE(I),  OK )
      END DO

      CALL TCASE ( 'UCASE: short to short, same I/O' )
      DO I = 1, NSTR
         SSTR = SARRAY(I)
         CALL UCASE  ( SSTR, SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SUCASE(I),  OK )
      END DO

      CALL TCASE ( 'UCASE: long to long' )
      DO I = 1, NSTR
         CALL UCASE  ( LARRAY(I), LSTR )
         CALL CHCKSC ( 'LARRAY(I)', LSTR, '=', LUCASE(I),  OK )
      END DO

      CALL TCASE ( 'UCASE: short to long' )
      DO I = 1, NSTR
         LSTR1 = ' '
         R = LASTNB( SUCASE(I) )
         E = MIN(R,LEN(LSTR1))
         LSTR1(1:E) = SUCASE(I)(1:E)
         CALL UCASE  ( SARRAY(I), LSTR )
         CALL CHCKSC ( 'SARRAY(I)', LSTR, '=', LSTR1,  OK )
      END DO

      CALL TCASE ( 'UCASE: long to short' )
      DO I = 1, NSTR
         SSTR1 = ' '
         R = LASTNB( LUCASE(I) )
         E = MIN(R,LEN(SSTR1))
         SSTR1(1:E) = LUCASE(I)(1:E)
         CALL UCASE  ( LARRAY(I), SSTR )
         CALL CHCKSC ( 'LARRAY(I)', SSTR, '=', SSTR1,  OK )
      END DO

      CALL TCASE ( 'UCASE: blank' )
      SSTR = ' '
      SSTR1 = 'boo'

      CALL UCASE  ( SSTR, SSTR1 )
      CALL CHCKSC ( 'SLMST1', SSTR1, '=', ' ',  OK )

!
!     LCASE tests.
!
      CALL TCASE ( 'LCASE: short to short' )
      DO I = 1, NSTR
         CALL LCASE  ( SARRAY(I), SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SLCASE(I),  OK )
      END DO

      CALL TCASE ( 'LCASE: short to short, same I/O' )
      DO I = 1, NSTR
         SSTR = SARRAY(I)
         CALL LCASE  ( SSTR, SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SLCASE(I),  OK )
      END DO

      CALL TCASE ( 'LCASE: long to long' )
      DO I = 1, NSTR
         CALL LCASE  ( LARRAY(I), LSTR )
         CALL CHCKSC ( 'LARRAY(I)', LSTR, '=', LLCASE(I),  OK )
      END DO

      CALL TCASE ( 'LCASE: short to long' )
      DO I = 1, NSTR
         LSTR1 = ' '
         R = LASTNB( SLCASE(I) )
         E = MIN(R,LEN(LSTR1))
         LSTR1(1:E) = SLCASE(I)(1:E)
         CALL LCASE  ( SARRAY(I), LSTR )
         CALL CHCKSC ( 'SARRAY(I)', LSTR, '=', LSTR1,  OK )
      END DO

      CALL TCASE ( 'LCASE: long to short' )
      DO I = 1, NSTR
         SSTR1 = ' '
         R = LASTNB( LLCASE(I) )
         E = MIN(R,LEN(SSTR1))
         SSTR1(1:E) = LLCASE(I)(1:E)
         CALL LCASE  ( LARRAY(I), SSTR )
         CALL CHCKSC ( 'LARRAY(I)', SSTR, '=', SSTR1,  OK )
      END DO

      CALL TCASE ( 'LCASE: blank' )
      SSTR = ' '
      SSTR1 = 'boo'

      CALL LCASE  ( SSTR, SSTR1 )
      CALL CHCKSC ( 'SLMST1', SSTR1, '=', ' ',  OK )


!
!     CMPRSS tests.
!
      CALL TCASE ( 'CMPRSS: short to short, 0' )
      DO I = 1, NSTR
         CALL CMPRSS ( ' ', 0, SARRAY(I), SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SCMPR0(I),  OK )
      END DO

      CALL TCASE ( 'CMPRSS: short to short, 1' )
      DO I = 1, NSTR
         CALL CMPRSS ( ' ', 1, SARRAY(I), SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SCMPR1(I),  OK )
      END DO

      CALL TCASE ( 'CMPRSS: short to short, 2' )
      DO I = 1, NSTR
         CALL CMPRSS ( ' ', 2, SARRAY(I), SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SCMPR2(I),  OK )
      END DO

      CALL TCASE ( 'CMPRSS: short to short, too many' )
      DO I = 1, NSTR
         CALL CMPRSS ( ' ', SMLSIZ+1, SARRAY(I), SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SARRAY(I),  OK )
      END DO

      CALL TCASE ( 'CMPRSS: short to short, same I/O, 0' )
      DO I = 1, NSTR
         SSTR = SARRAY(I)
         CALL CMPRSS ( ' ', 0, SSTR, SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SCMPR0(I),  OK )
      END DO

      CALL TCASE ( 'CMPRSS: short to short, same I/O, 1' )
      DO I = 1, NSTR
         SSTR = SARRAY(I)
         CALL CMPRSS ( ' ', 1, SSTR, SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SCMPR1(I),  OK )
      END DO

      CALL TCASE ( 'CMPRSS: short to short, same I/O, 2' )
      DO I = 1, NSTR
         SSTR = SARRAY(I)
         CALL CMPRSS ( ' ', 2, SSTR, SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SCMPR2(I),  OK )
      END DO

      CALL TCASE ( 'CMPRSS: short to short, same I/O, too many' )
      DO I = 1, NSTR
         SSTR = SARRAY(I)
         CALL CMPRSS ( ' ', SMLSIZ+1, SSTR, SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SARRAY(I),  OK )
      END DO

      CALL TCASE ( 'CMPRSS: long to long, 0' )
      DO I = 1, NSTR
         CALL CMPRSS ( ' ', 0, LARRAY(I), LSTR )
         CALL CHCKSC ( 'LARRAY(I)', LSTR, '=', LCMPR0(I),  OK )
      END DO

      CALL TCASE ( 'CMPRSS: long to long, 1' )
      DO I = 1, NSTR
         CALL CMPRSS ( ' ', 1, LARRAY(I), LSTR )
         CALL CHCKSC ( 'LARRAY(I)', LSTR, '=', LCMPR1(I),  OK )
      END DO

      CALL TCASE ( 'CMPRSS: long to long, 2' )
      DO I = 1, NSTR
         CALL CMPRSS ( ' ', 2, LARRAY(I), LSTR )
         CALL CHCKSC ( 'LARRAY(I)', LSTR, '=', LCMPR2(I),  OK )
      END DO

      CALL TCASE ( 'CMPRSS: long to long, too many' )
      DO I = 1, NSTR
         CALL CMPRSS ( ' ', LRGSIZ+1, LARRAY(I), LSTR )
         CALL CHCKSC ( 'LARRAY(I)', LSTR, '=', LARRAY(I),  OK )
      END DO

      CALL TCASE ( 'CMPRSS: short to long, 0' )
      DO I = 1, NSTR
         LSTR1 = ' '
         R = LASTNB( SCMPR0(I) )
         E = MIN(R,LEN(LSTR1))
         LSTR1(1:E) = SCMPR0(I)(1:E)
         CALL CMPRSS ( ' ', 0, SARRAY(I), LSTR )
         CALL CHCKSC ( 'SARRAY(I)', LSTR, '=', LSTR1,  OK )
      END DO

      CALL TCASE ( 'CMPRSS: short to long, 1' )
      DO I = 1, NSTR
         LSTR1 = ' '
         R = LASTNB( SCMPR1(I) )
         E = MIN(R,LEN(LSTR1))
         LSTR1(1:E) = SCMPR1(I)(1:E)
         CALL CMPRSS ( ' ', 1, SARRAY(I), LSTR )
         CALL CHCKSC ( 'SARRAY(I)', LSTR, '=', LSTR1,  OK )
      END DO

      CALL TCASE ( 'CMPRSS: short to long, 2' )
      DO I = 1, NSTR
         LSTR1 = ' '
         R = LASTNB( SCMPR2(I) )
         E = MIN(R,LEN(LSTR1))
         LSTR1(1:E) = SCMPR2(I)(1:E)
         CALL CMPRSS ( ' ', 2, SARRAY(I), LSTR )
         CALL CHCKSC ( 'SARRAY(I)', LSTR, '=', LSTR1,  OK )
      END DO

      CALL TCASE ( 'CMPRSS: short to long, too many' )
      DO I = 1, NSTR
         LSTR1 = ' '
         R = LASTNB( SARRAY(I) )
         E = MIN(R,LEN(LSTR1))
         LSTR1(1:E) = SARRAY(I)(1:E)
         CALL CMPRSS ( ' ', SMLSIZ+1, SARRAY(I), LSTR )
         CALL CHCKSC ( 'SARRAY(I)', LSTR, '=', LSTR1,  OK )
      END DO

      CALL TCASE ( 'CMPRSS: long to short, 0' )
      DO I = 1, NSTR
         SSTR1 = ' '
         R = LASTNB( LCMPR0(I) )
         E = MIN(R,LEN(SSTR1))
         SSTR1(1:E) = LCMPR0(I)(1:E)
         CALL CMPRSS ( ' ', 0, LARRAY(I), SSTR )
         CALL CHCKSC ( 'LARRAY(I)', SSTR, '=', SSTR1,  OK )
      END DO

      CALL TCASE ( 'CMPRSS: long to short, 1' )
      DO I = 1, NSTR
         SSTR1 = ' '
         R = LASTNB( LCMPR1(I) )
         E = MIN(R,LEN(SSTR1))
         SSTR1(1:E) = LCMPR1(I)(1:E)
         CALL CMPRSS ( ' ', 1, LARRAY(I), SSTR )
         CALL CHCKSC ( 'LARRAY(I)', SSTR, '=', SSTR1,  OK )
      END DO

      CALL TCASE ( 'CMPRSS: long to short, 2' )
      DO I = 1, NSTR
         SSTR1 = ' '
         R = LASTNB( LCMPR2(I) )
         E = MIN(R,LEN(SSTR1))
         SSTR1(1:E) = LCMPR2(I)(1:E)
         CALL CMPRSS ( ' ', 2, LARRAY(I), SSTR )
         CALL CHCKSC ( 'LARRAY(I)', SSTR, '=', SSTR1,  OK )
      END DO

      CALL TCASE ( 'CMPRSS: long to short, too many' )
      DO I = 1, NSTR
         SSTR1 = ' '
         R = LASTNB( LARRAY(I) )
         E = MIN(R,LEN(SSTR1))
         SSTR1(1:E) = LARRAY(I)(1:E)
         CALL CMPRSS ( ' ', LRGSIZ+1, LARRAY(I), SSTR )
         CALL CHCKSC ( 'LARRAY(I)', SSTR, '=', SSTR1,  OK )
      END DO

      CALL TCASE ( 'CMPRSS: blank, 0' )
      SSTR = ' '
      SSTR1 = 'boo'

      CALL CMPRSS ( ' ', 0, SSTR, SSTR1 )
      CALL CHCKSC ( 'SLMST1', SSTR1, '=', ' ',  OK )

      CALL TCASE ( 'CMPRSS: blank, 1' )
      SSTR = ' '
      SSTR1 = 'boo'

      CALL CMPRSS ( ' ', 1, SSTR, SSTR1 )
      CALL CHCKSC ( 'SLMST1', SSTR1, '=', ' ',  OK )

      CALL TCASE ( 'CMPRSS: blank, too many' )
      SSTR = ' '
      SSTR1 = 'boo'

      CALL CMPRSS ( ' ', SMLSIZ+1, SSTR, SSTR1 )
      CALL CHCKSC ( 'SLMST1', SSTR1, '=', ' ',  OK )


!
!     LJUCRS tests.
!
      CALL TCASE ( 'LJUCRS: short to short, 0' )
      DO I = 1, NSTR
         CALL LJUCRS ( 0, SARRAY(I), SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SLUCR0(I),  OK )
      END DO

      CALL TCASE ( 'LJUCRS: short to short, 1' )
      DO I = 1, NSTR
         CALL LJUCRS ( 1, SARRAY(I), SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SLUCR1(I),  OK )
      END DO

      CALL TCASE ( 'LJUCRS: short to short, 2' )
      DO I = 1, NSTR
         CALL LJUCRS ( 2, SARRAY(I), SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SLUCR2(I),  OK )
      END DO

      CALL TCASE ( 'LJUCRS: short to short, too many' )
      DO I = 1, NSTR
         CALL LJUCRS ( SMLSIZ+1, SARRAY(I), SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SLJSTU(I),  OK )
      END DO

      CALL TCASE ( 'LJUCRS: short to short, same I/O, 0' )
      DO I = 1, NSTR
         SSTR = SARRAY(I)
         CALL LJUCRS ( 0, SSTR, SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SLUCR0(I),  OK )
      END DO

      CALL TCASE ( 'LJUCRS: short to short, same I/O, 1' )
      DO I = 1, NSTR
         SSTR = SARRAY(I)
         CALL LJUCRS ( 1, SSTR, SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SLUCR1(I),  OK )
      END DO

      CALL TCASE ( 'LJUCRS: short to short, same I/O, 2' )
      DO I = 1, NSTR
         SSTR = SARRAY(I)
         CALL LJUCRS ( 2, SSTR, SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SLUCR2(I),  OK )
      END DO

      CALL TCASE ( 'LJUCRS: short to short, same I/O, too many' )
      DO I = 1, NSTR
         SSTR = SARRAY(I)
         CALL LJUCRS ( SMLSIZ+1, SSTR, SSTR )
         CALL CHCKSC ( 'SARRAY(I)', SSTR, '=', SLJSTU(I),  OK )
      END DO

      CALL TCASE ( 'LJUCRS: long to long, 0' )
      DO I = 1, NSTR
         CALL LJUCRS ( 0, LARRAY(I), LSTR )
         CALL CHCKSC ( 'LARRAY(I)', LSTR, '=', LLUCR0(I),  OK )
      END DO

      CALL TCASE ( 'LJUCRS: long to long, 1' )
      DO I = 1, NSTR
         CALL LJUCRS ( 1, LARRAY(I), LSTR )
         CALL CHCKSC ( 'LARRAY(I)', LSTR, '=', LLUCR1(I),  OK )
      END DO

      CALL TCASE ( 'LJUCRS: long to long, 2' )
      DO I = 1, NSTR
         CALL LJUCRS ( 2, LARRAY(I), LSTR )
         CALL CHCKSC ( 'LARRAY(I)', LSTR, '=', LLUCR2(I),  OK )
      END DO

      CALL TCASE ( 'LJUCRS: long to long, too many' )
      DO I = 1, NSTR
         CALL LJUCRS ( LRGSIZ+1, LARRAY(I), LSTR )
         CALL CHCKSC ( 'LARRAY(I)', LSTR, '=', LLJSTU(I),  OK )
      END DO

      CALL TCASE ( 'LJUCRS: short to long, 0' )
      DO I = 1, NSTR
         LSTR1 = ' '
         R = LASTNB( SLUCR0(I) )
         E = MIN(R,LEN(LSTR1))
         LSTR1(1:E) = SLUCR0(I)(1:E)
         CALL LJUCRS ( 0, SARRAY(I), LSTR )
         CALL CHCKSC ( 'SARRAY(I)', LSTR, '=', LSTR1,  OK )
      END DO

      CALL TCASE ( 'LJUCRS: short to long, 1' )
      DO I = 1, NSTR
         LSTR1 = ' '
         R = LASTNB( SLUCR1(I) )
         E = MIN(R,LEN(LSTR1))
         LSTR1(1:E) = SLUCR1(I)(1:E)
         CALL LJUCRS ( 1, SARRAY(I), LSTR )
         CALL CHCKSC ( 'SARRAY(I)', LSTR, '=', LSTR1,  OK )
      END DO

      CALL TCASE ( 'LJUCRS: short to long, 2' )
      DO I = 1, NSTR
         LSTR1 = ' '
         R = LASTNB( SLUCR2(I) )
         E = MIN(R,LEN(LSTR1))
         LSTR1(1:E) = SLUCR2(I)(1:E)
         CALL LJUCRS ( 2, SARRAY(I), LSTR )
         CALL CHCKSC ( 'SARRAY(I)', LSTR, '=', LSTR1,  OK )
      END DO

      CALL TCASE ( 'LJUCRS: short to long, too many' )
      DO I = 1, NSTR
         LSTR1 = ' '
         R = LASTNB( SLJSTU(I) )
         E = MIN(R,LEN(LSTR1))
         LSTR1(1:E) = SLJSTU(I)(1:E)
         CALL LJUCRS ( SMLSIZ+1, SARRAY(I), LSTR )
         CALL CHCKSC ( 'SARRAY(I)', LSTR, '=', LSTR1,  OK )
      END DO

      CALL TCASE ( 'LJUCRS: long to short, 0' )
      DO I = 1, NSTR
         SSTR1 = ' '
         R = LASTNB( LLUCR0(I) )
         E = MIN(R,LEN(SSTR1))
         SSTR1(1:E) = LLUCR0(I)(1:E)
         CALL LJUCRS ( 0, LARRAY(I), SSTR )
         CALL CHCKSC ( 'LARRAY(I)', SSTR, '=', SSTR1,  OK )
      END DO

      CALL TCASE ( 'LJUCRS: long to short, 1' )
      DO I = 1, NSTR
         SSTR1 = ' '
         R = LASTNB( LLUCR1(I) )
         E = MIN(R,LEN(SSTR1))
         SSTR1(1:E) = LLUCR1(I)(1:E)
         CALL LJUCRS ( 1, LARRAY(I), SSTR )
         CALL CHCKSC ( 'LARRAY(I)', SSTR, '=', SSTR1,  OK )
      END DO

      CALL TCASE ( 'LJUCRS: long to short, 2' )
      DO I = 1, NSTR
         SSTR1 = ' '
         R = LASTNB( LLUCR2(I) )
         E = MIN(R,LEN(SSTR1))
         SSTR1(1:E) = LLUCR2(I)(1:E)
         CALL LJUCRS ( 2, LARRAY(I), SSTR )
         CALL CHCKSC ( 'LARRAY(I)', SSTR, '=', SSTR1,  OK )
      END DO

      CALL TCASE ( 'LJUCRS: long to short, too many' )
      DO I = 1, NSTR
         SSTR1 = ' '
         R = LASTNB( LLJSTU(I) )
         E = MIN(R,LEN(SSTR1))
         SSTR1(1:E) = LLJSTU(I)(1:E)
         CALL LJUCRS ( LRGSIZ+1, LARRAY(I), SSTR )
         CALL CHCKSC ( 'LARRAY(I)', SSTR, '=', SSTR1,  OK )
      END DO

      CALL TCASE ( 'LJUCRS: blank, 0' )
      SSTR = ' '
      SSTR1 = 'boo'

      CALL LJUCRS ( 0, SSTR, SSTR1 )
      CALL CHCKSC ( 'SLMST1', SSTR1, '=', ' ',  OK )

      CALL TCASE ( 'LJUCRS: blank, 1' )
      SSTR = ' '
      SSTR1 = 'boo'

      CALL LJUCRS ( 1, SSTR, SSTR1 )
      CALL CHCKSC ( 'SLMST1', SSTR1, '=', ' ',  OK )

      CALL TCASE ( 'LJUCRS: blank, too many' )
      SSTR = ' '
      SSTR1 = 'boo'

      CALL LJUCRS ( SMLSIZ+1, SSTR, SSTR1 )
      CALL CHCKSC ( 'SLMST1', SSTR1, '=', ' ',  OK )

!
!     This is good enough for now.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
