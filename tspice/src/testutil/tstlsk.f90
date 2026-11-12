!$Procedure      TSTLSK ( Test Leapseconds Kernel )

      SUBROUTINE TSTLSK
      use pepper_legacy

! $ Abstract
!
!    This test utility routine creates a leapsecond kernel
!    (valid as of July 1, 1997) loads the file into the
!    kernel pool, and then deletes the resulting file.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST UTILITIES
!
! $ Declarations

      IMPLICIT NONE

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
! $ Files
!
!     A leapseconds kernel is created, loaded into the kernel pool
!     and then deleted.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This routine creates a temporary file based on the file
!     naming pattern (see NEWFIL_1 in support for details)
!
!         lsk{0-9}{0-9}{0-9}{0-9}.tmp
!
!     The resulting file is loaded into the kernel pool and
!     then deleted.
!
!     The fact that this file is created is logged in the
!     test log file.
!
! $ Examples
!
!     Suppose that you are testing some portion of the toolkit
!     that requires the use of a leapseconds kernel.  This routine
!     allows you to load a leapseconds kernel without having to
!     know where a current leapsecond kernel is located on the
!     file system.
!
!        CALL TSTLSK
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 1-JUL-1997 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Generate and load a leapseconds kernel for testing.
!
!-&


      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NLINES
      PARAMETER           ( NLINES = 29 )


      CHARACTER*(LNSIZE)    FILE
      CHARACTER*(LNSIZE)    TEXT ( NLINES )
      CHARACTER*(WDSIZE)    BEGDAT

      TEXT(  1 ) = BEGDAT()
      TEXT(  2 ) = ' '
      TEXT(  3 ) = 'DELTET/DELTA_T_A       =   32.184'
      TEXT(  4 ) = 'DELTET/K               =    1.657D-3'
      TEXT(  5 ) = 'DELTET/EB              =    1.671D-2'
      TEXT(  6 ) = 'DELTET/M               = (  6.239996D0   ' &
      //           '1.99096871D-7 )'
      TEXT(  7 ) = ' '
      TEXT(  8 ) = 'DELTET/DELTA_AT        = ( 10,   @1972-JA' &
      //           'N-1'
      TEXT(  9 ) = '                           11,   @1972-JU' &
      //           'L-1'
      TEXT( 10 ) = '                           12,   @1973-JA' &
      //           'N-1'
      TEXT( 11 ) = '                           13,   @1974-JA' &
      //           'N-1'
      TEXT( 12 ) = '                           14,   @1975-JA' &
      //           'N-1'
      TEXT( 13 ) = '                           15,   @1976-JA' &
      //           'N-1'
      TEXT( 14 ) = '                           16,   @1977-JA' &
      //           'N-1'
      TEXT( 15 ) = '                           17,   @1978-JA' &
      //           'N-1'
      TEXT( 16 ) = '                           18,   @1979-JA' &
      //           'N-1'
      TEXT( 17 ) = '                           19,   @1980-JA' &
      //           'N-1'
      TEXT( 18 ) = '                           20,   @1981-JU' &
      //           'L-1'
      TEXT( 19 ) = '                           21,   @1982-JU' &
      //           'L-1'
      TEXT( 20 ) = '                           22,   @1983-JU' &
      //           'L-1'
      TEXT( 21 ) = '                           23,   @1985-JU' &
      //           'L-1'
      TEXT( 22 ) = '                           24,   @1988-JA' &
      //           'N-1'
      TEXT( 23 ) = '                           25,   @1990-JA' &
      //           'N-1'
      TEXT( 24 ) = '                           26,   @1991-JA' &
      //           'N-1'
      TEXT( 25 ) = '                           27,   @1992-JU' &
      //           'L-1'
      TEXT( 26 ) = '                           28,   @1993-JU' &
      //           'L-1'
      TEXT( 27 ) = '                           29,   @1994-JU' &
      //           'L-1'
      TEXT( 28 ) = '                           30,   @1996-JA' &
      //           'N-1'
      TEXT( 29 ) = '                           31,   @1997-JU' &
      //           'L-1 )'


      CALL NEWFIL_1 ( 'lsk{0-9}{0-9}{0-9}{0-9}.tmp', FILE )
      CALL TSTTXT   ( FILE, TEXT, NLINES, .TRUE., .FALSE. )

      RETURN
      END
