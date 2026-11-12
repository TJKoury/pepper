!$Procedure      F_VSTRNG ( Tests for the virtual string routine )

      SUBROUTINE F_VSTRNG ( OK )
      use pepper_legacy
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This exercises the virtual string routine.
!
! $ Required_Reading
!
!     TBD.
!
! $ Keywords
!
!     TBD.
!
! $ Declarations
!
!     TBD.
!
! $ Brief_I/O
!
!     TBD.
!
! $ Detailed_Input
!
!     TBD.
!
! $ Detailed_Output
!
!     TBD.
!
! $ Parameters
!
!     TBD.
!
! $ Exceptions
!
!     TBD.
!
! $ Files
!
!     TBD.
!
! $ Particulars
!
!     TBD.
!
! $ Examples
!
!     TBD.
!
! $ Restrictions
!
!     TBD.
!
! $ Literature_References
!
!     TBD.
!
! $ Author_and_Institution
!
!     TBD.
!
! $ Version
!
!-    SPICELIB Version 1.27.0, 28-NOV-2021 (BVS)
!
!        Updated for MAC-OSX-M1-64BIT-CLANG_C.
!
!-    SPICELIB Version 1.26.0, 10-MAR-2014 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-INTEL.
!
!-    SPICELIB Version 1.25.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-LINUX-64BIT-IFORT.
!
!-    SPICELIB Version 1.24.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-GFORTRAN.
!
!-    SPICELIB Version 1.23.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.22.0, 10-MAR-2014 (BVS)
!
!        Updated for PC-CYGWIN-64BIT-GCC_C.
!
!-    SPICELIB Version 1.21.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL.
!
!-    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-CC_C.
!
!-    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C.
!
!-    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-NATIVE_C.
!
!-    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-WINDOWS-64BIT-IFORT.
!
!-    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-LINUX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS)
!
!        Updated for PC-64BIT-MS_C.
!
!-    SPICELIB Version 1.14.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-INTEL_C.
!
!-    SPICELIB Version 1.13.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-IFORT.
!
!-    SPICELIB Version 1.12.0, 13-MAY-2010 (BVS)
!
!        Updated for MAC-OSX-64BIT-GFORTRAN.
!
!-    SPICELIB Version 1.11.0, 18-MAR-2009 (BVS)
!
!        Updated for PC-LINUX-GFORTRAN.
!
!-    SPICELIB Version 1.10.0, 18-MAR-2009 (BVS)
!
!        Updated for MAC-OSX-GFORTRAN.
!
!-    SPICELIB Version 1.9.0, 19-FEB-2008 (BVS)
!
!        Updated for PC-LINUX-IFORT.
!
!-    SPICELIB Version 1.8.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-LINUX-64BIT-GCC_C.
!
!-    SPICELIB Version 1.7.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-INTEL_C.
!
!-    SPICELIB Version 1.6.0, 14-NOV-2006 (BVS)
!
!        Updated for MAC-OSX-IFORT.
!
!-    SPICELIB Version 1.5.0, 14-NOV-2006 (BVS)
!
!        Updated for PC-WINDOWS-IFORT.
!
!-    SPICELIB Version 1.4.0, 26-OCT-2005 (BVS)
!
!        Updated for SUN-SOLARIS-64BIT-GCC_C.
!
!-    SPICELIB Version 1.3.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN_C.
!
!-    SPICELIB Version 1.2.0, 03-JAN-2005 (BVS)
!
!        Updated for PC-CYGWIN.
!
!-    SPICELIB Version 1.1.1, 17-JUL-2002 (BVS)
!
!        Added MAC-OSX environments.
!
!-    SPICELIB Version 1.1.0, 20-OCT-1999 (WLT)
!
!        Declared PI to be an EXTERNAL Function.
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!
!
!     Local Variables
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )


      DOUBLE PRECISION      X
      INTEGER               EXP
      INTEGER               I
      INTEGER               EXPECT


      CHARACTER*(1)         LETTER
      CHARACTER*(WDSIZE)    EWORD
      CHARACTER*(WDSIZE)    WORD

      LOGICAL               RND
      LOGICAL               DID



!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_VSTRNG' )

      RND = .FALSE.

      CALL TCASE ( 'Make sure that exponents are returned with ' &
      //           'the correct value. ' )

         X   = 1.282792D+02
         CALL ZZVSTSTR ( X, ' ', EXP )

         CALL CHCKSI ( 'EXP', EXP, '=', 2, 0, OK )

         X   = 1.282792D-02
         CALL ZZVSTSTR ( X, ' ', EXP )

         CALL CHCKSI ( 'EXP', EXP, '=', -2, 0, OK )

         X   = 1.282792D+27
         CALL ZZVSTSTR ( X, ' ', EXP )

         CALL CHCKSI ( 'EXP', EXP, '=', 27, 0, OK )

         X   = 1.282792D-34
         CALL ZZVSTSTR ( X, ' ', EXP )

         CALL CHCKSI ( 'EXP', EXP, '=', -34, 0, OK )


         X   = 1.282792D+257
         CALL ZZVSTSTR ( X, ' ', EXP )

         CALL CHCKSI ( 'EXP', EXP, '=', 257, 0, OK )

         X   = 1.282792D-103
         CALL ZZVSTSTR ( X, ' ', EXP )

         CALL CHCKSI ( 'EXP', EXP, '=', -103, 0, OK )


      CALL TCASE ( 'Perform an exhaustive check on the exponent ' &
      //           'properties of ZZVSTSTR. ' )

         X      = 1.2D-257
         EXPECT = -257

         DO I = 1, 500

            CALL ZZVSTSTR ( X, ' ', EXP )
            CALL CHCKSI   ( 'EXP',  EXP, '=', EXPECT, 0, OK )

            X      = X * 10.0D0
            EXPECT = EXPECT + 1

         END DO




      CALL TCASE ( 'Make sure we get the correct virtual ' &
      //           'characters over a wide range of characters. ' )


         X = 123.281928291D+00

         CALL ZZVSTSTR ( X, '*', EXP )

         CALL CHCKSI ( 'EXP', EXP, '=', 2, 0, OK )


         DO I = -10, -4

            CALL ZZVSBSTR ( I, I, RND, LETTER, DID )
            CALL CHCKSC ( 'LETTER', LETTER, '=', '*',  OK )

         END DO

         CALL ZZVSBSTR ( -3, -3, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_-3', LETTER, '=', '1',  OK )

         CALL ZZVSBSTR ( -2, -2, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_-2', LETTER, '=', '2',  OK )

         CALL ZZVSBSTR ( -1, -1, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_-1', LETTER, '=', '3',  OK )

         CALL ZZVSBSTR ( 0,  0, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_0', LETTER, '=', '.',  OK )

         CALL ZZVSBSTR ( 1,  1, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_1', LETTER, '=', '2',  OK )

         CALL ZZVSBSTR ( 2,  2, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_2', LETTER, '=', '8',  OK )

         CALL ZZVSBSTR ( 3,  3, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_3', LETTER, '=', '1',  OK )

         CALL ZZVSBSTR ( 4,  4, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_4', LETTER, '=', '9',  OK )

         CALL ZZVSBSTR ( 5,  5, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_5', LETTER, '=', '2',  OK )

         CALL ZZVSBSTR ( 6,  6, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_6', LETTER, '=', '8',  OK )

         CALL ZZVSBSTR ( 7,  7, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_7', LETTER, '=', '2',  OK )

         CALL ZZVSBSTR ( 8,  8, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_8', LETTER, '=', '9',  OK )

         CALL ZZVSBSTR ( 9,  9, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_9', LETTER, '=', '1',  OK )

         CALL ZZVSBSTR ( 10,  10, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_10', LETTER, '=', '0',  OK )

         CALL ZZVSBSTR ( 15,  15, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_15', LETTER, '=', '0',  OK )

      CALL TCASE ( 'Make sure we get the correct virtual ' &
      //           'characters over a wide range of characters. ' )


         X = 0.000123281928291D+00

         CALL ZZVSTSTR ( X, '&', EXP )

         CALL CHCKSI ( 'EXP', EXP, '=', -4, 0, OK )


         DO I = -10, -2

            CALL ZZVSBSTR ( I,  I, RND, LETTER, DID )
            CALL CHCKSC ( 'LETTER', LETTER, '=', '&',  OK )

         END DO

         CALL ZZVSBSTR ( -1,  -1, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_-1', LETTER, '=', '0',  OK )

         CALL ZZVSBSTR ( 0,  0, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_0', LETTER, '=', '.',  OK )

         CALL ZZVSBSTR ( 1,  1, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_1', LETTER, '=', '0',  OK )

         CALL ZZVSBSTR ( 2,  2, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_2', LETTER, '=', '0',  OK )

         CALL ZZVSBSTR ( 3,  3, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_3', LETTER, '=', '0',  OK )

         CALL ZZVSBSTR ( 4,  4, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_4', LETTER, '=', '1',  OK )

         CALL ZZVSBSTR ( 5,  5, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_5', LETTER, '=', '2',  OK )

         CALL ZZVSBSTR ( 6,  6, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_6', LETTER, '=', '3',  OK )

         CALL ZZVSBSTR ( 7,  7, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_7', LETTER, '=', '2',  OK )

         CALL ZZVSBSTR ( 8,  8, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_8', LETTER, '=', '8',  OK )

         CALL ZZVSBSTR ( 9,  9, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_9', LETTER, '=', '1',  OK )

         CALL ZZVSBSTR ( 10,  10, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_10', LETTER, '=', '9',  OK )

         CALL ZZVSBSTR ( 25,  25, RND, LETTER, DID )
         CALL CHCKSC ( 'LETTER_25', LETTER, '=', '0',  OK )


      CALL TCASE ( 'Retrieve a substring of the virtual string ' )

         X = PI() * 100.0D0

         CALL ZZVSTSTR ( X, '&', EXP )

         CALL CHCKSI ( 'EXP', EXP, '=', 2, 0, OK )

         CALL ZZVSBSTR ( -8, 3, RND, WORD, DID )

         EWORD = '&&&&&314.159'

         CALL CHCKSC ( 'WORD', WORD, '=', EWORD,  OK )

      CALL TCASE ( 'Retrieve a substring of the virtual string ' )

         X = PI() * 0.01D0

         CALL ZZVSTSTR ( X, '&', EXP )

         CALL CHCKSI ( 'EXP', EXP, '=', -2, 0, OK )

         CALL ZZVSBSTR ( -8, 12, RND, WORD, DID )

         EWORD = '&&&&&&&0.031415926535'

         CALL CHCKSC ( 'WORD', WORD, '=', EWORD,  OK )


      CALL TCASE ( 'Check to see if virtual string round works.' )

         X = 4.999995D0

         CALL ZZVSTSTR ( X, ' ', EXP )
         CALL CHCKSI ( 'EXP', EXP, '=', 0, 0, OK )

         CALL ZZVSBSTR ( -1, 5, .TRUE., WORD, DID )

         EWORD = '5.00000'

         CALL CHCKSL ( 'DID',  DID, .FALSE.,      OK )
         CALL CHCKSC ( 'WORD', WORD, '=', EWORD,  OK )

         X = 9.999995D0


         CALL ZZVSTSTR ( X, ' ', EXP )
         CALL CHCKSI ( 'EXP', EXP, '=', 0, 0, OK )

         CALL ZZVSBSTR ( -1, 5, .TRUE., WORD, DID )

         EWORD = '0.00000'

         CALL CHCKSL ( 'DID',  DID, .TRUE.,      OK )
         CALL CHCKSC ( 'WORD', WORD, '=', EWORD,  OK )

         X = 9.5D0


         CALL ZZVSTSTR ( X, ' ', EXP )
         CALL CHCKSI ( 'EXP', EXP, '=', 0, 0, OK )

         CALL ZZVSBSTR ( -1, 0, .TRUE., WORD, DID )

         EWORD = '0.'

         CALL CHCKSL ( 'DID',  DID, .TRUE.,      OK )
         CALL CHCKSC ( 'WORD', WORD, '=', EWORD,  OK )

         CALL ZZVSBSTR ( -3, 0, .TRUE., WORD, DID )

         EWORD = ' 10.'

         CALL CHCKSL ( 'DID',  DID, .FALSE.,      OK )
         CALL CHCKSC ( 'WORD', WORD, '=', EWORD,  OK )




      CALL T_SUCCESS ( OK )
      RETURN
      END
