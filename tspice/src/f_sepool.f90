!$Procedure      F_SEPOOL ( Family of tests for SEPOOL )

      SUBROUTINE F_SEPOOL ( OK )
      use pepper_legacy

! $ Abstract
!
!     This routine performs rudimentary tests on the routine SEPOOL.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine performs rudimentary tests on the routine SEPOOL.
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
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 13-JUN-2013 (BVS)
!
!-&


!
!     Test Utility Functions
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(WDSIZE)    BEGDAT

!
!     SPICELIB Functions
!

!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 11 )

      CHARACTER*(LNSIZE)    TEXT ( TXTSIZ )

      INTEGER               LONGSZ
      PARAMETER           ( LONGSZ = 3000 )

      CHARACTER*(LONGSZ)    EXPECT ( 5 )
      CHARACTER*(LONGSZ)    MYSTR
      CHARACTER*(LONGSZ)    ESTRNG

      CHARACTER*(1)         TYPE


      INTEGER               ESIZE
      INTEGER               I
      INTEGER               N
      INTEGER               SIZE
      INTEGER               GOTBCK
      INTEGER               FIDX
      INTEGER               LIDX

      LOGICAL               FOUND
      LOGICAL               GCFND
      LOGICAL               STFND


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SEPOOL' )


      TEXT(1) = BEGDAT()

      TEXT(  2 ) = ' MYSTRING = ( ''This is a long string tha' &
      //           't will be spread -'','
      TEXT(  3 ) = '              ''across several lines of a' &
      //           ' spice text kernel -'','
      TEXT(  4 ) = '              ''for the sake of testing S' &
      //           'EPOOL.'','
      TEXT(  5 ) = '              ''This is a second string.''' &
      //           ','
      TEXT(  6 ) = '              ''This is a third long stri' &
      //           'ng that will span -'','
      TEXT(  7 ) = '              ''more than one line in a t' &
      //           'ext kernel.'','
      TEXT(  8 ) = '              ''Finally this is a last lo' &
      //           'ng string that will -'''
      TEXT(  9 ) = '              ''be more than one line in ' &
      //           'the text kernel so -'''
      TEXT( 10 ) = '              ''that we can test the fetc' &
      //           'hing of long strings -'','
      TEXT( 11 ) = '              ''from the kernel pool with' &
      //           ' SEPOOL.'' )'


      EXPECT ( 1 ) = 'This is a long string that will be spread ' &
      //             'across several lines of a spice text ' &
      //             'kernel for the sake of testing SEPOOL. '

      EXPECT ( 2 ) = 'This is a second string. '

      EXPECT ( 3 ) = 'This is a third long string that will span ' &
      //             'more than one line in a text kernel. '

      EXPECT ( 4 ) = 'Finally this is a last long string that ' &
      //             'will be more than one line in the text ' &
      //             'kernel so that we can test the fetching of ' &
      //             'long strings from the kernel pool with ' &
      //             'SEPOOL. '

      EXPECT ( 5 ) = 'that we can test the fetching of ' &
      //             'long strings from the kernel pool with ' &
      //             'SEPOOL. '


      CALL KILFIL ( 'sample.txt' )
      CALL TSTTXT ( 'sample.txt', TEXT, TXTSIZ, .TRUE., .FALSE. )

      CALL TCASE ( 'Try to look up a string that isn''t there.' )

         MYSTR = 'xxx'
         CALL SEPOOL ( 'MYITEM', 1, '-', MYSTR, SIZE, LIDX, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE,   '=', 0, 0, OK )
         CALL CHCKSI ( 'LIDX', LIDX,   '=', 0, 0, OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )

      CALL TCASE ( 'See if we can get the loaded strings.' )

         MYSTR = 'xxx'

!
!        Fetch complete strings.
!
         ESIZE = RTRIM(EXPECT(1))
         FIDX  = 1

         CALL SEPOOL ( 'MYSTRING', FIDX, '-', MYSTR, SIZE, LIDX, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', ESIZE, 0, OK )
         CALL CHCKSI ( 'LIDX', LIDX, '=', 3, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', EXPECT(1), OK )

         ESIZE = RTRIM(EXPECT(2))
         FIDX  = LIDX + 1

         CALL SEPOOL ( 'MYSTRING', FIDX, '-', MYSTR, SIZE, LIDX, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', ESIZE, 0, OK )
         CALL CHCKSI ( 'LIDX', LIDX, '=', 4, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', EXPECT(2), OK )

         ESIZE = RTRIM(EXPECT(3))
         FIDX  = LIDX + 1

         CALL SEPOOL ( 'MYSTRING', FIDX, '-', MYSTR, SIZE, LIDX, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', ESIZE, 0, OK )
         CALL CHCKSI ( 'LIDX', LIDX, '=', 6, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', EXPECT(3), OK )

         ESIZE = RTRIM(EXPECT(4))
         FIDX  = LIDX + 1

         CALL SEPOOL ( 'MYSTRING', FIDX, '-', MYSTR, SIZE, LIDX, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', ESIZE, 0, OK )
         CALL CHCKSI ( 'LIDX', LIDX, '=', 10, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', EXPECT(4), OK )

!
!        Fetch partial string for FIDX in the middle.
!
         ESIZE = RTRIM(EXPECT(5))
         FIDX  = LIDX - 1

         CALL SEPOOL ( 'MYSTRING', FIDX, '-', MYSTR, SIZE, LIDX, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', ESIZE, 0, OK )
         CALL CHCKSI ( 'LIDX', LIDX, '=', 10, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', EXPECT(5), OK )

!
!        Get .NOT. FOUNDs and blanks for too small and too big FIDXs.
!
         FIDX  = LIDX + 1

         CALL SEPOOL ( 'MYSTRING', FIDX, '-', MYSTR, SIZE, LIDX, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', 0, 0, OK )
         CALL CHCKSI ( 'LIDX', LIDX, '=', 0, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', ' ', OK )

         FIDX  = 0

         CALL SEPOOL ( 'MYSTRING', FIDX, '-', MYSTR, SIZE, LIDX, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', 0, 0, OK )
         CALL CHCKSI ( 'LIDX', LIDX, '=', 0, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', ' ', OK )


      CALL TCASE ( 'Check for compatibility with GCPOOL when ' &
      //           'there continuation symbol used is not the ' &
      //           'one specified in SEPOOL. ' )


         CALL DTPOOL ( 'MYSTRING', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

         DO I = 1, N

            CALL SEPOOL ( 'MYSTRING', I, '*', MYSTR, SIZE, LIDX, STFND )
            CALL GCPOOL ( 'MYSTRING', I, 1,   GOTBCK, ESTRNG, GCFND )

            CALL CHCKSL ( 'GCFND', GCFND, .TRUE., OK )
            CALL CHCKSL ( 'STFND', STFND,  GCFND, OK )
            CALL CHCKSC ( 'MYSTR', MYSTR, '=', ESTRNG, OK )

            ESIZE = RTRIM(ESTRNG)
            CALL CHCKSI ( 'SIZE', SIZE, '=', ESIZE, 0, OK )

            CALL CHCKSI ( 'LIDX', LIDX, '=', I, 0, OK )

         END DO

         CALL SEPOOL ( 'MYSTRING', N+1, '*', MYSTR,  SIZE, LIDX, STFND )
         CALL CHCKSL ( 'STFND', STFND,  .FALSE.,   OK )
         CALL CHCKSC ( 'MYSTR', MYSTR,  '=',  ' ', OK )
         CALL CHCKSI ( 'SIZE',  SIZE,    '=', 0, 0, OK )
         CALL CHCKSI ( 'LIDX',  LIDX,    '=', 0, 0, OK )

!
!     Cleanup any debris we may have left around.
!
      CALL CLPOOL

      CALL T_SUCCESS ( OK )
      RETURN
      END
