!$Procedure F_ZZNAMFRM ( Family of tests for ZZNAMFRM and its callers )

      SUBROUTINE F_ZZNAMFRM ( OK )

      IMPLICIT NONE

! $ Abstract
!
!     This routine tests ZZNAMFRM and the internal save functionality
!     of all routines that call it.
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
!     None
!
! $ Keywords
!
!     None
!
! $ Declarations

      INCLUDE               'zzctr.inc'

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
!-    Version 1.0.0  31-MAR-2014 (BVS)
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

!
!     SPICELIB Functions
!

!
!     Local parameters
!
      DOUBLE PRECISION      VTOL
      PARAMETER           ( VTOL = 1.D-14 )

      DOUBLE PRECISION      TTOL
      PARAMETER           ( TTOL = 1.D-12 )


      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )

      INTEGER               BUFLEN
      PARAMETER           ( BUFLEN = 10 )

      INTEGER               NCASES
      PARAMETER           ( NCASES = 4 )

      INTEGER               NPTS
      PARAMETER           ( NPTS   = 8 )

!
!     Local Variables
!
      INTEGER               USRCTR ( CTRSIZ )
      INTEGER               SAVCTR ( CTRSIZ )

      CHARACTER*(FRNMLN)    FRNAME
      INTEGER               FRCODE
      LOGICAL               FOUND

      CHARACTER*(FRNMLN)    SAVNAM
      INTEGER               SAVCDE

      CHARACTER*(LINLEN)    BUFFER ( BUFLEN )

      INTEGER               HANDLE
      INTEGER               HANDL1

      DOUBLE PRECISION      ET

      CHARACTER*(FRNMLN)    FRAME1
      CHARACTER*(FRNMLN)    FRAME2
      CHARACTER*(FRNMLN)    BODY2

      INTEGER               I
      INTEGER               CI

      CHARACTER*(LINLEN)    TITLE

      INTEGER               N      (          NCASES )
      DOUBLE PRECISION      RADII  ( 3,       NCASES )

      DOUBLE PRECISION      SCLKDP

      DOUBLE PRECISION      CMAT1  ( 3, 3,    NCASES )
      DOUBLE PRECISION      CLKOU1 (          NCASES )
      LOGICAL               FOUND1 (          NCASES )

      DOUBLE PRECISION      CMAT2  ( 3, 3,    NCASES )
      DOUBLE PRECISION      AV2    ( 3,       NCASES )
      DOUBLE PRECISION      CLKOU2 (          NCASES )
      LOGICAL               FOUND2 (          NCASES )

      DOUBLE PRECISION      TRGEP1 (          NCASES )
      DOUBLE PRECISION      OBSPO1 ( 3,       NCASES )
      DOUBLE PRECISION      TRMPT1 ( 3, NPTS, NCASES )

      DOUBLE PRECISION      TRGEP2 (          NCASES )
      DOUBLE PRECISION      SRFVE2 ( 3,       NCASES )
      DOUBLE PRECISION      PHASE2 (          NCASES )
      DOUBLE PRECISION      INCDN2 (          NCASES )
      DOUBLE PRECISION      EMISS2 (          NCASES )

      DOUBLE PRECISION      MAT1   ( 3, 3,    NCASES )

      DOUBLE PRECISION      MAT2   ( 3, 3,    NCASES )

      DOUBLE PRECISION      SPOIN3 ( 3,       NCASES )
      DOUBLE PRECISION      TRGEP3 (          NCASES )
      DOUBLE PRECISION      SRFVE3 ( 3,       NCASES )
      LOGICAL               FOUND3 (          NCASES )

      DOUBLE PRECISION      STATE1 ( 6,       NCASES )
      DOUBLE PRECISION      LT1    (          NCASES )

      DOUBLE PRECISION      STATE2 ( 6,       NCASES )
      DOUBLE PRECISION      LT2    (          NCASES )

      DOUBLE PRECISION      STATE3 ( 6,       NCASES )
      DOUBLE PRECISION      LT3    (          NCASES )

      DOUBLE PRECISION      POS4   ( 3,       NCASES )
      DOUBLE PRECISION      LT4    (          NCASES )

      DOUBLE PRECISION      STATE5 ( 6,       NCASES )
      DOUBLE PRECISION      LT5    (          NCASES )

      DOUBLE PRECISION      POS6   ( 3,       NCASES )
      DOUBLE PRECISION      LT6    (          NCASES )

      DOUBLE PRECISION      DESCR    ( 5 )
      DOUBLE PRECISION      STATE7 ( 6,       NCASES )
      DOUBLE PRECISION      LT7    (          NCASES )

      DOUBLE PRECISION      POS8   ( 3,       NCASES )
      DOUBLE PRECISION      LT8    (          NCASES )

      DOUBLE PRECISION      STATE9 ( 6,       NCASES )
      INTEGER               CENTE9 (          NCASES )

      DOUBLE PRECISION      SPOIN4 ( 3,       NCASES )
      DOUBLE PRECISION      DIST4  (          NCASES )
      DOUBLE PRECISION      TRGEP4 (          NCASES )
      DOUBLE PRECISION      OBSPO4 ( 3,       NCASES )
      LOGICAL               FOUND4 (          NCASES )

      DOUBLE PRECISION      SPOIN5 ( 3,       NCASES )
      DOUBLE PRECISION      TRGEP5 (          NCASES )
      DOUBLE PRECISION      SRFVE5 ( 3,       NCASES )

      DOUBLE PRECISION      SPOIN7 ( 3,       NCASES )
      DOUBLE PRECISION      TRGEP7 (          NCASES )
      DOUBLE PRECISION      SRFVE7 ( 3,       NCASES )

      DOUBLE PRECISION      MAT3   ( 6, 6,    NCASES )

      DOUBLE PRECISION      SPOINT ( 3 )

      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      LT

      DOUBLE PRECISION      OBSSTA ( 6 )

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZNAMFRM' )

!
!     Check ZZNAMFRM update after counter set to user value.
!
!     Call ZZNAMFRM twice, first to make sure that there is an
!     update, second that there is no update.
!
      CALL TCASE ( 'ZZNAMFRM update after initial counter setting.' )

      CALL ZZCTRUIN ( USRCTR )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      FRNAME = 'J2000'
      FRCODE = -1
      SAVNAM = 'ECLIPJ2000'
      SAVCDE = 17

      CALL ZZNAMFRM ( USRCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'USRCTR', USRCTR(1), '!=', SAVCTR(1), 0, OK )
      CALL CHCKSC ( 'SAVNAM', SAVNAM, '=', 'J2000',          OK )
      CALL CHCKSI ( 'SAVCDE', SAVCDE, '=', 1,     0,         OK )
      CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'J2000',          OK )
      CALL CHCKSI ( 'FRCODE', FRCODE, '=', 1,     0,         OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      CALL ZZNAMFRM ( USRCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SAVNAM', SAVNAM, '=', 'J2000',          OK )
      CALL CHCKSI ( 'SAVCDE', SAVCDE, '=', 1,     0,         OK )
      CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'J2000',          OK )
      CALL CHCKSI ( 'FRCODE', FRCODE, '=', 1,     0,         OK )

!
!     Check ZZNAMFRM update after POOL frame addition.
!
!     Call ZZNAMFRM twice, first to make sure that there is an
!     update, second that there is no update.
!
      CALL TCASE ( 'ZZNAMFRM update after POOL frame insertion.' )

      BUFFER(  1 ) = 'FRAME_MYFRAME        = -99'
      BUFFER(  2 ) = 'FRAME_-99_NAME       = ''MYFRAME'''
      BUFFER(  3 ) = 'FRAME_-99_CLASS      = 4'
      BUFFER(  4 ) = 'FRAME_-99_CLASS_ID   = -99'
      BUFFER(  5 ) = 'FRAME_-99_CENTER     = 0'

      CALL LMPOOL ( BUFFER, 5 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      FRNAME = 'MYFRAME'
      FRCODE = -1
      SAVNAM = 'ECLIPJ2000'
      SAVCDE = 17

      CALL ZZNAMFRM ( USRCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'USRCTR', USRCTR(1), '!=', SAVCTR(1), 0, OK )
      CALL CHCKSC ( 'SAVNAM', SAVNAM, '=', 'MYFRAME',        OK )
      CALL CHCKSI ( 'SAVCDE', SAVCDE, '=', -99,   0,         OK )
      CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'MYFRAME',        OK )
      CALL CHCKSI ( 'FRCODE', FRCODE, '=', -99,   0,         OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      CALL ZZNAMFRM ( USRCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SAVNAM', SAVNAM, '=', 'MYFRAME',        OK )
      CALL CHCKSI ( 'SAVCDE', SAVCDE, '=', -99,   0,         OK )
      CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'MYFRAME',        OK )
      CALL CHCKSI ( 'FRCODE', FRCODE, '=', -99,   0,         OK )


!
!     Check ZZNAMFRM update after POOL frame update.
!
!     Call ZZNAMFRM twice, first to make sure that there is an
!     update, second that there is no update.
!
      CALL TCASE ( 'ZZNAMFRM update after POOL frame update.' )

      BUFFER(  1 ) = 'FRAME_MYFRAME        = -88'
      BUFFER(  2 ) = 'FRAME_-88_NAME       = ''MYFRAME'''
      BUFFER(  3 ) = 'FRAME_-88_CLASS      = 4'
      BUFFER(  4 ) = 'FRAME_-88_CLASS_ID   = -88'
      BUFFER(  5 ) = 'FRAME_-88_CENTER     = 0'

      CALL LMPOOL ( BUFFER, 5 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      FRNAME = 'MYFRAME'
      FRCODE = -1
      SAVNAM = 'ECLIPJ2000'
      SAVCDE = 17

      CALL ZZNAMFRM ( USRCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'USRCTR', USRCTR(1), '!=', SAVCTR(1), 0, OK )
      CALL CHCKSC ( 'SAVNAM', SAVNAM, '=', 'MYFRAME',        OK )
      CALL CHCKSI ( 'SAVCDE', SAVCDE, '=', -88,   0,         OK )
      CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'MYFRAME',        OK )
      CALL CHCKSI ( 'FRCODE', FRCODE, '=', -88,   0,         OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      CALL ZZNAMFRM ( USRCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SAVNAM', SAVNAM, '=', 'MYFRAME',        OK )
      CALL CHCKSI ( 'SAVCDE', SAVCDE, '=', -88,   0,         OK )
      CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'MYFRAME',        OK )
      CALL CHCKSI ( 'FRCODE', FRCODE, '=', -88,   0,         OK )


!
!     Check ZZNAMFRM update for input saved code set to 0.
!
!     Call ZZNAMFRM twice, first to make sure that there is an
!     update, second that there is no update.
!
      CALL TCASE ( 'ZZNAMFRM update after 0 saved frame ID.' )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      FRNAME = 'MYFRAME'
      FRCODE = -1
      SAVNAM = 'ECLIPJ2000'
      SAVCDE = 0

      CALL ZZNAMFRM ( USRCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SAVNAM', SAVNAM, '=', 'MYFRAME',        OK )
      CALL CHCKSI ( 'SAVCDE', SAVCDE, '=', -88,   0,         OK )
      CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'MYFRAME',        OK )
      CALL CHCKSI ( 'FRCODE', FRCODE, '=', -88,   0,         OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      CALL ZZNAMFRM ( USRCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SAVNAM', SAVNAM, '=', 'MYFRAME',        OK )
      CALL CHCKSI ( 'SAVCDE', SAVCDE, '=', -88,   0,         OK )
      CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'MYFRAME',        OK )
      CALL CHCKSI ( 'FRCODE', FRCODE, '=', -88,   0,         OK )

!
!     Check ZZNAMFRM update for different input name.
!
!     Call ZZNAMFRM twice, first to make sure that there is an
!     update, second that there is no update.
!
      CALL TCASE ( 'ZZNAMFRM update for different input name.' )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      FRNAME = 'MyFrame'
      FRCODE = -1
      SAVNAM = 'ECLIPJ2000'
      SAVCDE = 17

      CALL ZZNAMFRM ( USRCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SAVNAM', SAVNAM, '=', 'MyFrame',        OK )
      CALL CHCKSI ( 'SAVCDE', SAVCDE, '=', -88,   0,         OK )
      CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'MyFrame',        OK )
      CALL CHCKSI ( 'FRCODE', FRCODE, '=', -88,   0,         OK )

      CALL MOVEI ( USRCTR, CTRSIZ, SAVCTR )

      CALL ZZNAMFRM ( USRCTR, SAVNAM, SAVCDE, FRNAME, FRCODE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAI ( 'USRCTR', USRCTR, '=', SAVCTR, CTRSIZ,   OK )
      CALL CHCKSC ( 'SAVNAM', SAVNAM, '=', 'MyFrame',        OK )
      CALL CHCKSI ( 'SAVCDE', SAVCDE, '=', -88,   0,         OK )
      CALL CHCKSC ( 'FRNAME', FRNAME, '=', 'MyFrame',        OK )
      CALL CHCKSI ( 'FRCODE', FRCODE, '=', -88,   0,         OK )

!
!     Clear POOL and make and load the some test data.
!
      CALL CLPOOL ( )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KILFIL ( 'fun.bsp' )
      CALL KILFIL ( 'fun.ker' )
      CALL KILFIL ( 'fun.bc' )
      CALL KILFIL ( 'fun.tsc' )

      CALL TSTSPK ( 'fun.bsp', .TRUE.,  HANDLE )
      CALL TSTPCK ( 'fun.ker', .TRUE., .TRUE. )
      CALL TSTCK3 ( 'fun.bc', &
                    'fun.tsc', .TRUE., .TRUE., .TRUE., HANDL1 )

!
!     Test all routines that call ZZNAMFRM. Do it by computing two
!     bench mark sets of data (at indexes 1 and 2), each for a unique,
!     non-overlapping set of frames, then setting two unique sets of
!     alias frames and computing the same sets of data for them (at
!     indexes 3 and 4), and checking each "alias" set against its
!     benchmark. If saved mapping was updated correctly set 1 should
!     match set 3 and set 2 should match set 4.
!
!     In addition to setting up all inputs we will call T_CTRBEQF to
!     adjust ZZBODS2C and ZZNAMFRM counters to the same value before
!     calling each of the routine that make use of saved values and
!     counters. This will help exposing any cases when the same counter
!     is erroneously used in more than one place.
!
      DO I = 1, 4

!
!        Clear POOL and reload text kernels.
!
         CALL CLPOOL ( )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL LDPOOL ( 'fun.ker' )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL LDPOOL ( 'fun.tsc' )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Set up frames for each iteration.
!
         IF      ( I .EQ. 1 ) THEN

            FRAME1 = 'J2000'
            FRAME2 = 'IAU_EARTH'
            BODY2  = 'EARTH'

            CALL IRFDEF ( 3 )

            CI = 1

            TITLE = 'Check 1st bench data set against itself'

         ELSE IF ( I .EQ. 2 ) THEN

            FRAME1 = 'ECLIPJ2000'
            FRAME2 = 'IAU_MARS'
            BODY2  = 'MARS'

            CALL IRFDEF ( 13 )

            CI = 2

            TITLE = 'Check 2nd bench data set against itself'

         ELSE IF ( I .EQ. 3 ) THEN

            FRAME1 = 'FRAME1'
            FRAME2 = 'FRAME2'
            BODY2  = 'EARTH'

            CALL IRFDEF ( 3 )

            CI = 1

            BUFFER(  1 ) = 'FRAME_FRAME1         = -99'
            BUFFER(  2 ) = 'FRAME_-99_NAME       = ''FRAME1'''
            BUFFER(  3 ) = 'FRAME_-99_CLASS      = 4'
            BUFFER(  4 ) = 'FRAME_-99_CLASS_ID   = -99'
            BUFFER(  5 ) = 'FRAME_-99_CENTER     = 0'
            BUFFER(  6 ) = 'TKFRAME_-99_SPEC     = ''MATRIX'''
            BUFFER(  7 ) = 'TKFRAME_-99_RELATIVE = ''J2000'''
            BUFFER(  8 ) = 'TKFRAME_-99_MATRIX = ( 1,0,0,0,1,0,0,0,1 )'
            BUFFER(  9 ) = ' '
            BUFFER( 10 ) = ' '

            CALL LMPOOL ( BUFFER, 10 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            BUFFER(  1 ) = 'FRAME_FRAME2         = -88'
            BUFFER(  2 ) = 'FRAME_-88_NAME       = ''FRAME2'''
            BUFFER(  3 ) = 'FRAME_-88_CLASS      = 4'
            BUFFER(  4 ) = 'FRAME_-88_CLASS_ID   = -88'
            BUFFER(  5 ) = 'FRAME_-88_CENTER     = 399'
            BUFFER(  6 ) = 'TKFRAME_-88_SPEC     = ''MATRIX'''
            BUFFER(  7 ) = 'TKFRAME_-88_RELATIVE = ''IAU_EARTH'''
            BUFFER(  8 ) = 'TKFRAME_-88_MATRIX = ( 1,0,0,0,1,0,0,0,1 )'
            BUFFER(  9 ) = ' '
            BUFFER( 10 ) = ' '

            CALL LMPOOL ( BUFFER, 10 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TITLE = 'Check 1st mapped data set against 1st benchmark'

         ELSE IF ( I .EQ. 4 ) THEN

            FRAME1 = 'FRAME1'
            FRAME2 = 'FRAME2'
            BODY2  = 'MARS'

            CALL IRFDEF ( 13 )

            CI = 2

            BUFFER(  1 ) = 'FRAME_FRAME1         = -77'
            BUFFER(  2 ) = 'FRAME_-77_NAME       = ''FRAME1'''
            BUFFER(  3 ) = 'FRAME_-77_CLASS      = 4'
            BUFFER(  4 ) = 'FRAME_-77_CLASS_ID   = -77'
            BUFFER(  5 ) = 'FRAME_-77_CENTER     = 0'
            BUFFER(  6 ) = 'TKFRAME_-77_SPEC     = ''MATRIX'''
            BUFFER(  7 ) = 'TKFRAME_-77_RELATIVE = ''ECLIPJ2000'''
            BUFFER(  8 ) = 'TKFRAME_-77_MATRIX = ( 1,0,0,0,1,0,0,0,1 )'
            BUFFER(  9 ) = ' '
            BUFFER( 10 ) = ' '

            CALL LMPOOL ( BUFFER, 10 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            BUFFER(  1 ) = 'FRAME_FRAME2         = -66'
            BUFFER(  2 ) = 'FRAME_-66_NAME       = ''FRAME2'''
            BUFFER(  3 ) = 'FRAME_-66_CLASS      = 4'
            BUFFER(  4 ) = 'FRAME_-66_CLASS_ID   = -66'
            BUFFER(  5 ) = 'FRAME_-66_CENTER     = 499'
            BUFFER(  6 ) = 'TKFRAME_-66_SPEC     = ''MATRIX'''
            BUFFER(  7 ) = 'TKFRAME_-66_RELATIVE = ''IAU_MARS'''
            BUFFER(  8 ) = 'TKFRAME_-66_MATRIX = ( 1,0,0,0,1,0,0,0,1 )'
            BUFFER(  9 ) = ' '
            BUFFER( 10 ) = ' '

            CALL LMPOOL ( BUFFER, 10 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TITLE = 'Check 2nd mapped data set against 2nd benchmark'

         END IF

!
!        Declare test case.
!
         CALL TCASE ( TITLE )


!
!        Call all routines that make use of ZZNAMFRM. Set arbitrary
!        input time.
!
         ET = 1.0D7

!
!        -----------------------------------------------------------
!
         CALL SCE2T  ( -9, ET, SCLKDP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CTRBEQF ()

         CALL CKGP   ( -9999, SCLKDP, 0.D0, FRAME1, CMAT1(1,1,I), &
                       CLKOU1(I), FOUND1(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'CKGP CMAT', &
                       CMAT1(1,1,I), '=', CMAT1(1,1,CI), 9, 0.D0, OK )
         CALL CHCKSD ( 'CKGP CKLOUT', &
                       CLKOU1(I), '=', CLKOU1(CI), 0.D0, OK )
         CALL CHCKSL ( 'CKGP FOUND', FOUND1(I), FOUND1(CI), OK )

!
!        -----------------------------------------------------------
!
         CALL SCE2T  ( -9, ET, SCLKDP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CTRBEQF ()

         CALL CKGPAV ( -9999, SCLKDP, 0.D0, FRAME1, CMAT2(1,1,I), &
                       AV2(1,I), CLKOU2(I), FOUND2(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'CKGPAV CMAT', &
                       CMAT2(1,1,I), '=', CMAT2(1,1,CI), 9, 0.D0, OK )
         CALL CHCKAD ( 'CKGPAV AV', &
                       AV2(1,I), '=', AV2(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'CKGP CKLOUT', &
                       CLKOU2(I), '=', CLKOU2(CI), 0.D0, OK )
         CALL CHCKSL ( 'CKGPAV FOUND', FOUND2(I), FOUND1(CI),  OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL EDTERM ( 'UMBRAL', 'SUN', BODY2, ET, &
                       FRAME2, 'NONE', 'MERCURY', NPTS, &
                       TRGEP1(I), OBSPO1(1,I), TRMPT1(1,1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'EDTERM TRGEPC', &
                       TRGEP1(I), '=', TRGEP1(CI), 0.D0, OK )
         CALL CHCKAD ( 'EDTERM OBSPOS', &
                       OBSPO1(1,I), '=', OBSPO1(1,CI), 3, 0.D0, OK )
         CALL CHCKAD ( 'EDTERM TRMPTS', &
                TRMPT1(1,1,I), '=', TRMPT1(1,1,CI), 3*NPTS, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL BODVRD ( BODY2, 'RADII', 3,  N(I), RADII(1,I) )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL VPACK  ( RADII(1,I), 0.D0, 0.D0, SPOINT )

         CALL T_CTRBEQF ()

         CALL ILLUMG ( 'Ellipsoid', BODY2, 'SUN',  ET, &
                       FRAME2, 'NONE', 'MERCURY', SPOINT, &
                       TRGEP2(I), SRFVE2(1,I), PHASE2(I), &
                       INCDN2(I), EMISS2(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSD ( 'ILLUMG TRGEPC', &
                       TRGEP2(I), '=', TRGEP2(CI), 0.D0, OK )
         CALL CHCKAD ( 'ILLUMG SRFVEC', &
                       SRFVE2(1,I), '=', SRFVE2(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'ILLUMG PHASE', &
                       PHASE2(I), '=', PHASE2(CI), 0.D0, OK )
         CALL CHCKSD ( 'ILLUMG INCDNC', &
                       INCDN2(I), '=', INCDN2(CI), 0.D0, OK )
         CALL CHCKSD ( 'ILLUMG EMISSN', &
                       EMISS2(I), '=', EMISS2(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL PXFORM ( FRAME1, FRAME2, ET, MAT1(1,1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'PXFORM ROTATE', &
                       MAT1(1,1,I), '=', MAT1(1,1,CI), 9, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL PXFRM2 ( FRAME1, FRAME2, ET, ET+1.D6, MAT2(1,1,I)  )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'PXFRM2 ROTATE', &
                       MAT2(1,1,I), '=', MAT2(1,1,CI), 9, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL SPKPOS ( BODY2, ET, 'J2000', 'NONE', 'SUN', DVEC, LT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CTRBEQF ()

         CALL SINCPT ( 'Ellipsoid', BODY2, ET, FRAME2, &
                       'NONE', 'SUN', 'J2000', DVEC, &
                       SPOIN3(1,I), TRGEP3(I), SRFVE3(1,I), FOUND3(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SINCPT SPOINT', &
                       SPOIN3(1,I), '=', SPOIN3(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SINCPT TRGEPC', &
                       TRGEP3(I), '=', TRGEP3(CI), 0.D0, OK )
         CALL CHCKAD ( 'SINCPT SRFVEC', &
                       SRFVE3(1,I), '=', SRFVE3(1,CI), 3, 0.D0, OK )
         CALL CHCKSL ( 'SINCPT FOUND', &
                       FOUND3(I), FOUND3(CI), OK )

!
!        -----------------------------------------------------------
!
         CALL CLEARD ( 6, OBSSTA )
         OBSSTA(1) = 1.D3

         CALL T_CTRBEQF ()

         CALL SPKCVO ( 'SUN', ET, FRAME1, 'OBSERVER', 'LT+S', &
                       OBSSTA, ET, BODY2, FRAME2, &
                       STATE1(1,I), LT1(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKCVO STATE', &
                       STATE1(1,I), '~/', STATE1(1,CI), 6, VTOL, OK )
         CALL CHCKSD ( 'SPKCVO LT', &
                       LT1(I), '~/', LT1(CI), TTOL, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SPKCVT ( OBSSTA, ET, BODY2, FRAME2, ET, &
                       FRAME1, 'OBSERVER', 'LT+S', 'SUN', &
                       STATE2(1,I), LT2(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKCVT STATE', &
                       STATE2(1,I), '~/', STATE2(1,CI), 6, VTOL, OK )
         CALL CHCKSD ( 'SPKCVT LT', &
                       LT2(I), '~/', LT2(CI), TTOL, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SPKEZ  ( 1, ET, FRAME2, 'NONE', 399, STATE3(1,I), LT3(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKEZR STATE', &
                       STATE3(1,I), '=', STATE3(1,CI), 6, 0.D0, OK )
         CALL CHCKSD ( 'SPKEZR LT', &
                       LT3(I), '=', LT3(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SPKEZP ( 1, ET, FRAME2, 'NONE', 399, POS4(1,I), LT4(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKPOS POS', &
                       POS4(1,I), '=', POS4(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SPKPOS LT', &
                       LT4(I), '=', LT4(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SPKGEO ( 499, ET, FRAME1, 10, STATE5(1,I), LT5(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKGEO STATE', &
                       STATE5(1,I), '~/', STATE5(1,CI), 6, VTOL, OK )
         CALL CHCKSD ( 'SPKGEO LT', &
                       LT5(I), '~/', LT5(CI), TTOL, OK )

         CALL T_CTRBEQF ()

         CALL SPKGEO ( 1, ET, 'DEFAULT', 399, STATE7(1,I), LT7(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKGEO STATE', &
                       STATE7(1,I), '=', STATE7(1,CI), 6, 0.D0, OK )
         CALL CHCKSD ( 'SPKGEO LT', &
                       LT7(I), '=', LT7(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SPKGPS ( 499, ET, FRAME1, 10, POS6(1,I), LT6(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKGPS POS', &
                       POS6(1,I), '=', POS6(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SPKGPS LT', &
                       LT6(I), '=', LT6(CI), 0.D0, OK )

         CALL T_CTRBEQF ()

         CALL SPKGPS ( 1, ET, 'DEFAULT', 399, POS8(1,I), LT8(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKGPS POS', &
                       POS8(1,I), '=', POS8(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SPKGPS LT', &
                       LT8(I), '=', LT8(CI), 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL DAFBFS ( HANDLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DAFFNA ( FOUND  )
         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKSL ( 'DAFFNA FOUND', FOUND, .TRUE., OK )

         CALL DAFGS  ( DESCR )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CTRBEQF ()

         CALL SPKPV( HANDLE, DESCR, ET, FRAME2, STATE9(1,I), CENTE9(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SPKPV STATE', &
                       STATE9(1,I), '=', STATE9(1,CI), 6, 0.D0, OK )
         CALL CHCKSI ( 'SPKPV CENTER', &
                       CENTE9(I),   '=', CENTE9(CI),      0,    OK )

!
!        -----------------------------------------------------------
!
         CALL SPKPOS ( BODY2, ET, FRAME1, 'NONE', 'SUN', DVEC, LT )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL T_CTRBEQF ()

         CALL SRFXPT ( 'Ellipsoid',  BODY2, ET, 'NONE', &
                       'SUN', FRAME1, DVEC, SPOIN4(1,I), &
                       DIST4(I), TRGEP4(I), OBSPO4(1,I), FOUND4(I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SRFXPT SPOINT', &
                       SPOIN4(1,I), '=', SPOIN4(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SRFXPT DIST', &
                       DIST4(I), '=', DIST4(CI), 0.D0, OK )
         CALL CHCKSD ( 'SRFXPT TRGEPC', &
                       TRGEP4(I), '=', TRGEP4(CI), 0.D0, OK )
         CALL CHCKAD ( 'SRFXPT OBSPOS', &
                       OBSPO4(1,I), '=', OBSPO4(1,CI), 3, 0.D0, OK )
         CALL CHCKSL ( 'SRFXPT FOUND', &
                       FOUND4(I), FOUND4(CI), OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SUBPNT ( 'Near point: ellipsoid', BODY2, ET, FRAME2, &
                       'NONE', 'SUN', &
                       SPOIN5(1,I), TRGEP5(I), SRFVE5(1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SUBPNT SPOINT', &
                       SPOIN5(1,I), '=', SPOIN5(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SUBPNT TRGEPC', &
                       TRGEP5(I), '=', TRGEP5(CI), 0.D0, OK )
         CALL CHCKAD ( 'SUBPNT SRFVEC', &
                       SRFVE5(1,I), '=', SRFVE5(1,CI), 3, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SUBSLR ( 'Near point: ellipsoid', BODY2, ET, FRAME2, &
                       'NONE', 'MERCURY', &
                       SPOIN7(1,I), TRGEP7(I), SRFVE7(1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SUBSLR SPOINT', &
                       SPOIN7(1,I), '=', SPOIN7(1,CI), 3, 0.D0, OK )
         CALL CHCKSD ( 'SUBSLR TRGEPC', &
                       TRGEP7(I), '=', TRGEP7(CI), 0.D0, OK )
         CALL CHCKAD ( 'SUBSLR SRFVEC', &
                       SRFVE7(1,I), '=', SRFVE7(1,CI), 3, 0.D0, OK )

!
!        -----------------------------------------------------------
!
         CALL T_CTRBEQF ()

         CALL SXFORM ( FRAME1, FRAME2, ET, MAT3(1,1,I) )

         CALL CHCKXC ( .FALSE., ' ', OK )
         CALL CHCKAD ( 'SXFORM XFORM', &
                       MAT3(1,1,I), '=', MAT3(1,1,CI), 36, 0.D0, OK )


      END DO

!
!     Clean up.
!
      CALL KILFIL ( 'fun.bsp' )
      CALL KILFIL ( 'fun.ker' )
      CALL KILFIL ( 'fun.bc' )
      CALL KILFIL ( 'fun.tsc' )

!
!     This is good enough for now.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
