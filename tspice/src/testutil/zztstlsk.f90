!$Procedure TSTLSK (Create a test LSK)

      SUBROUTINE ZZTSTLSK( NAMELS )
      use pepper_legacy

! $ Abstract
!
!    This test utility routine creates a leapsecond kernel
!    (valid as of July 1, 1997).
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!       TESTING
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         NAMELS

! $ Brief_I/O
!
!     None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None.
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
!     A leapseconds kernel is created.
!
! $ Examples
!
!     Suppose that you are testing some portion of the toolkit
!     that requires the use of a leapseconds kernel.  This routine
!     allows you to create a leapseconds kernel without having to
!     know where a current leapsecond kernel is located on the
!     file system.
!
!        CALL ZZTSTLSK( NAMELS )
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
!     W.L. Taber      (JPL)
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    Test Utilities 1.0.0, 18-AUG-2008 (EDW)
!
!        Replaced WRITE with WRITLN.
!
!-    Test Utilities 1.0.0, 05-JAN-2006 (EDW)
!
!        Reimplementation of TSTLSK removing all code
!        except that needed to write the test LSK.
!
!-&

! $ Index_Entries
!
!     Generate a leapseconds kernel for testing.
!
!-&

!
!     Spicelib Functions
!

!
!     Test Utility Functions
!
      CHARACTER*(16)        BEGDAT

!
!     Local Variables.
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 29 )


      CHARACTER*(LNSIZE)    LSK ( NLINES )
      CHARACTER*(LNSIZE)    LSKLN

      INTEGER               I
      INTEGER               R
      INTEGER               UNIT

      CALL KILFIL ( NAMELS )

      LSK(  1 ) = BEGDAT()
      LSK(  2 ) = ' '
      LSK(  3 ) = 'DELTET/DELTA_T_A       =   32.184'
      LSK(  4 ) = 'DELTET/K               =    1.657D-3'
      LSK(  5 ) = 'DELTET/EB              =    1.671D-2'
      LSK(  6 ) = 'DELTET/M               = (  6.239996D0   ' &
      //           '1.99096871D-7 )'
      LSK(  7 ) = ' '
      LSK(  8 ) = 'DELTET/DELTA_AT        = ( 10,   @1972-JA' &
      //           'N-1'
      LSK(  9 ) = '                           11,   @1972-JU' &
      //           'L-1'
      LSK( 10 ) = '                           12,   @1973-JA' &
      //           'N-1'
      LSK( 11 ) = '                           13,   @1974-JA' &
      //           'N-1'
      LSK( 12 ) = '                           14,   @1975-JA' &
      //           'N-1'
      LSK( 13 ) = '                           15,   @1976-JA' &
      //           'N-1'
      LSK( 14 ) = '                           16,   @1977-JA' &
      //           'N-1'
      LSK( 15 ) = '                           17,   @1978-JA' &
      //           'N-1'
      LSK( 16 ) = '                           18,   @1979-JA' &
      //           'N-1'
      LSK( 17 ) = '                           19,   @1980-JA' &
      //           'N-1'
      LSK( 18 ) = '                           20,   @1981-JU' &
      //           'L-1'
      LSK( 19 ) = '                           21,   @1982-JU' &
      //           'L-1'
      LSK( 20 ) = '                           22,   @1983-JU' &
      //           'L-1'
      LSK( 21 ) = '                           23,   @1985-JU' &
      //           'L-1'
      LSK( 22 ) = '                           24,   @1988-JA' &
      //           'N-1'
      LSK( 23 ) = '                           25,   @1990-JA' &
      //           'N-1'
      LSK( 24 ) = '                           26,   @1991-JA' &
      //           'N-1'
      LSK( 25 ) = '                           27,   @1992-JU' &
      //           'L-1'
      LSK( 26 ) = '                           28,   @1993-JU' &
      //           'L-1'
      LSK( 27 ) = '                           29,   @1994-JU' &
      //           'L-1'
      LSK( 28 ) = '                           30,   @1996-JA' &
      //           'N-1'
      LSK( 29 ) = '                           31,   @1997-JU' &
      //           'L-1 )'


!
!     Create the LSK kernel.
!
      CALL TXTOPN ( NAMELS, UNIT )

      DO I = 1, NLINES
         LSKLN = LSK(I)
         R = RTRIM( LSKLN )
         CALL WRITLN ( LSKLN(1:R), UNIT )
      END DO

      CLOSE ( UNIT )

      RETURN
      END
