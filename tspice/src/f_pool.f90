!$Procedure F_POOL ( Family of tests for POOL.)

      SUBROUTINE F_POOL ( OK )
      use pepper_legacy
      use pepper_inc_zzctr
      IMPLICIT NONE

! $ Abstract
!
!     This routine exercises a number of features of the
!     kernel pool.
!
! $ Required_Reading
!
!   None
!
! $ Keywords
!
!   None
!
! $ Declarations


      LOGICAL               OK

! $ Brief_I/O
!
!   None
!
! $ Detailed_Input
!
!   None
!
! $ Detailed_Output
!
!   None
!
! $ Parameters
!
!   None
!
! $ Exceptions
!
!   None
!
! $ Files
!
!   None
!
! $ Particulars
!
!   None
!
! $ Examples
!
!   None
!
! $ Restrictions
!
!   None
!
! $ Literature_References
!
!   None
!
! $ Author_and_Institution
!
!   None
!
! $ Version
!
!-    Version 3.2.0  06-SEP-2013 (BVS)
!
!        Added POOL state counter checks. Added ZZCVPOOL tests.
!
!-    Version 3.1.0  09-FEB-2010 (EDW)
!
!        Addition of test family to confirm error signal for kernel
!        pool variable names exceeding MAXLEN, defined in pool.f.
!
!-    Version 3.0.0  06-AUG-2002 (BVS)
!
!        Made environment-independent because PC-LINUX FORTRAN
!        was fixed. Added long/truncated string test cases.
!
!-    Version 2.0.0  19-OCT-1999 (WLT)
!
!        Made environment specific to handle bug in PC-LINUX
!        implementation of FORTRAN.
!
!-&

! $ Index_Entries
!
!   None
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
      PARAMETER           ( LNSIZE = 140 )


      CHARACTER*(1)         TYPE
      CHARACTER*(64)        NAME
      CHARACTER*(LNSIZE)    ERROR
      CHARACTER*(LNSIZE)    TEXT  ( 100 )
      CHARACTER*(LNSIZE)    CVALS (  20 )
      CHARACTER*(LNSIZE)    CVALUE(  20 )
      CHARACTER*(WDSIZE)    ITEM  (   3 )
      CHARACTER*(WDSIZE)    NAMES (  10 )
      CHARACTER*(WDSIZE)    VARNAM
      CHARACTER*(WDSIZE)    MYSTRS ( 20 )

      DOUBLE PRECISION      DVALS  ( 1  )
      DOUBLE PRECISION      EVALS  ( 20 )
      DOUBLE PRECISION      VALUES ( 20 )
      DOUBLE PRECISION      MYDATA ( 20 )

      INTEGER               ESIZE  ( 20 )
      INTEGER               I
      INTEGER               INTVAL ( 20 )
      INTEGER               IVALS  ( 20 )
      INTEGER               MYINTS ( 20 )
      INTEGER               N
      INTEGER               PTR
      INTEGER               UNIT

      LOGICAL               FOUND
      LOGICAL               UPDATE

      CHARACTER*(WDSIZE)    BNAMES(2)
      CHARACTER*(WDSIZE)    NNAMES(2)
      CHARACTER*(WDSIZE)    INAMES(2)

      INTEGER               NBILL
      INTEGER               NNAT
      INTEGER               NIAN

      LOGICAL               BUPDAT
      LOGICAL               NUPDAT
      LOGICAL               IUPDAT

!
!     POOL state counter.
!
      INTEGER               USRCTR ( CTRSIZ )

      INTEGER               USRCT1 ( CTRSIZ )
      INTEGER               USRCT2 ( CTRSIZ )
      INTEGER               USRCT3 ( CTRSIZ )
      INTEGER               USRCT4 ( CTRSIZ )


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_POOL' )

!
!     Case 1
!
      CALL TCASE ( 'Load and check types for numeric values.' )

         TEXT(  1 ) = BEGDAT()
         TEXT(  2 ) = '         VALUE1  = 1'
         TEXT(  3 ) = '         VALUE2  = 2'
         TEXT(  4 ) = '         VALUE3  = PI'
         TEXT(  5 ) = '         VALUE4  = 3'
         TEXT(  6 ) = '         VALUE5  = 4'
         TEXT(  7 ) = '         VALUE6  = 5'
         TEXT(  8 ) = '         VALUE7  = 1.276828E+11'
         TEXT(  9 ) = '         VALUE8  = -28.19729871E+12'
         TEXT( 10 ) = '         VALUE9  = @1-JAN-1994'
         TEXT( 11 ) = '         VALUE10 = ('
         TEXT( 12 ) = '                     1,'
         TEXT( 13 ) = '                     2,'
         TEXT( 14 ) = '                     3,'
         TEXT( 15 ) = '                     4,'
         TEXT( 16 ) = '                     5,'
         TEXT( 17 ) = '                     6,'
         TEXT( 18 ) = '                     7,'
         TEXT( 19 ) = '                     8 )'
         TEXT( 20 ) = '         VALUE11  = ( 5, 4, 3, 2 )'
         TEXT( 21 ) = '         VALUE10 +=  9'
         TEXT( 22 ) = ' '
         TEXT( 23 ) = '         VALUE11 += ( 1, 0 )'
         TEXT( 24 ) = '         VALUE10 += 10'
         TEXT( 25 ) = ' '

         CALL KILFIL ( 'testdata.ker' )
         CALL TSTTXT ( 'testdata.ker', TEXT, 25, .FALSE., .TRUE.)
         CALL LDPOOL ( 'testdata.ker' )

         CALL CHCKXC ( .FALSE., ' ',  OK )


         ESIZE(  1 ) = 1
         ESIZE(  2 ) = 1
         ESIZE(  3 ) = 1
         ESIZE(  4 ) = 1
         ESIZE(  5 ) = 1
         ESIZE(  6 ) = 1
         ESIZE(  7 ) = 1
         ESIZE(  8 ) = 1
         ESIZE(  9 ) = 1
         ESIZE( 10 ) = 10
         ESIZE( 11 ) = 6


         DO I = 1, 11
            VARNAM = 'VALUE#'
            CALL REPMI  ( VARNAM, '#', I, VARNAM )

            CALL DTPOOL ( VARNAM, FOUND, N, TYPE )

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,            OK )
            CALL CHCKSI ( 'N',     N,    '=', ESIZE(I), 0,   OK )
            CALL CHCKSC ( 'TYPE',  TYPE, '=', 'N',           OK )
         END DO


!
!     Case 2
!
      CALL TCASE ( 'Check that loaded values are as expected.' )

         CALL TSTMSG ( '#',  'Checking Value 1' )
         CALL GDPOOL ( 'VALUE1', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL NPARSD ( '1',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE1', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )


         CALL TSTMSG ( '#',  'Checking Value 2' )
         CALL GDPOOL ( 'VALUE2', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( '2',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE2', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 3' )
         CALL GDPOOL ( 'VALUE3', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( 'PI',     EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE3', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 4' )
         CALL GDPOOL ( 'VALUE4', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( '3',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE4', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 5' )
         CALL GDPOOL ( 'VALUE5', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( '4',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE5', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 6' )
         CALL GDPOOL ( 'VALUE6', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( '5',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE6', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 7' )
         CALL NPARSD ( '1.276828E+11',  EVALS(1), ERROR, PTR   )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL GDPOOL ( 'VALUE7', 1, 1, N, VALUES, FOUND )
         CALL CHCKAD ( 'VALUE7', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 8' )
         CALL NPARSD ( '-28.19729871E+12', EVALS(1), ERROR, PTR   )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL GDPOOL ( 'VALUE8', 1, 1, N, VALUES, FOUND )
         CALL CHCKAD ( 'VALUE8', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 9' )
         CALL TPARSE ( '1-JAN-1994', EVALS(1), ERROR )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL GDPOOL ( 'VALUE9', 1, 1, N, VALUES, FOUND )
         CALL CHCKAD ( 'VALUE9', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         EVALS(  1 ) =  1
         EVALS(  2 ) =  2
         EVALS(  3 ) =  3
         EVALS(  4 ) =  4
         EVALS(  5 ) =  5
         EVALS(  6 ) =  6
         EVALS(  7 ) =  7
         EVALS(  8 ) =  8
         EVALS(  9 ) =  9
         EVALS( 10 ) = 10

         CALL TSTMSG ( '#',  'Checking Value 10' )
         CALL GDPOOL ( 'VALUE10', 3, 5, N,     VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'VALUE10', VALUES, '=', EVALS(3), 5, 0.0D0, OK )
         CALL CHCKSI ( 'N',       N,      '=',           5, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 10 part 2.' )
         CALL GDPOOL ( 'VALUE10', 1, 10, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'VALUE10', VALUES, '=', EVALS(1), 10,0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',       N,      '=',           10, 0,     OK )

         CALL TSTMSG ( '#',  'Checking Value 10 part 3' )
         CALL GDPOOL ( 'VALUE10', 11, 5, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',       N,      '=',            0, 0,     OK )


         EVALS(  1 ) = 5
         EVALS(  2 ) = 4
         EVALS(  3 ) = 3
         EVALS(  4 ) = 2
         EVALS(  5 ) = 1
         EVALS(  6 ) = 0

         CALL TSTMSG ( '#',  'Checking Value 11' )
         CALL GDPOOL ( 'VALUE11', 1, 10, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',       N,     '=', 6, 0, OK    )
         CALL CHCKAD ( 'VALUE11', VALUES, '=', EVALS(1), 6,0.0D0, OK )
         CALL TSTMSG ( '#',  ' ' )


!
!     Case 3
!
      CALL TCASE ( 'Write the kernel pool out and clear the ' &
      //           'kernel pool.  Then read in the output from ' &
      //           'the preceding write and make sure everything ' &
      //           'matches. ' )

         CALL KILFIL ( 'writdata.ker'       )
         CALL TXTOPN ( 'writdata.ker', UNIT )
         CALL WRPOOL (  UNIT )
         CLOSE ( UNIT )
         CALL CLPOOL
         CALL LDPOOL ( 'writdata.ker' )

         CALL CHCKXC ( .FALSE., ' ',  OK )

         ESIZE(  1 ) = 1
         ESIZE(  2 ) = 1
         ESIZE(  3 ) = 1
         ESIZE(  4 ) = 1
         ESIZE(  5 ) = 1
         ESIZE(  6 ) = 1
         ESIZE(  7 ) = 1
         ESIZE(  8 ) = 1
         ESIZE(  9 ) = 1
         ESIZE( 10 ) = 10
         ESIZE( 11 ) = 6

         CALL TSTMSG ( '#',  'Checking type and size of values.' )
         DO I = 1, 11
            CALL TSTMSG ( '#', 'I is #' )
            CALL TSTMSI (  I )

            VARNAM = 'VALUE#'
            CALL REPMI  ( VARNAM, '#', I, VARNAM )

            CALL DTPOOL ( VARNAM, FOUND, N, TYPE )

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,            OK )
            CALL CHCKSI ( 'N',     N,    '=', ESIZE(I), 0,   OK )
            CALL CHCKSC ( 'TYPE',  TYPE, '=', 'N',           OK )
         END DO

         CALL TSTMSG ( '#',  'Checking Value 1' )
         CALL GDPOOL ( 'VALUE1', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL NPARSD ( '1',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE1', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )


         CALL TSTMSG ( '#',  'Checking Value 2' )
         CALL GDPOOL ( 'VALUE2', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( '2',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE2', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 3' )
         CALL GDPOOL ( 'VALUE3', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( 'PI',     EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE3', VALUES, '~/', EVALS, 1, 1.0D-14, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 4' )
         CALL GDPOOL ( 'VALUE4', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( '3',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE4', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 5' )
         CALL GDPOOL ( 'VALUE5', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( '4',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE5', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 6' )
         CALL GDPOOL ( 'VALUE6', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( '5',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE6', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 7' )
         CALL NPARSD ( '1.276828E+11',  EVALS(1), ERROR, PTR   )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL GDPOOL ( 'VALUE7', 1, 1, N, VALUES, FOUND )
         CALL CHCKAD ( 'VALUE7', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 8' )
         CALL NPARSD ( '-28.19729871E+12', EVALS(1), ERROR, PTR   )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL GDPOOL ( 'VALUE8', 1, 1, N, VALUES, FOUND )
         CALL CHCKAD ( 'VALUE8', VALUES, '~/', EVALS, 1, 1.0D-7, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 9' )
         CALL TPARSE ( '1-JAN-1994', EVALS(1), ERROR )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL GDPOOL ( 'VALUE9', 1, 1, N, VALUES, FOUND )
         CALL CHCKAD ( 'VALUE9', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         EVALS(  1 ) =  1
         EVALS(  2 ) =  2
         EVALS(  3 ) =  3
         EVALS(  4 ) =  4
         EVALS(  5 ) =  5
         EVALS(  6 ) =  6
         EVALS(  7 ) =  7
         EVALS(  8 ) =  8
         EVALS(  9 ) =  9
         EVALS( 10 ) = 10

         CALL TSTMSG ( '#',  'Checking Value 10' )
         CALL GDPOOL ( 'VALUE10', 3, 5, N,     VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'VALUE10', VALUES, '=', EVALS(3), 5, 0.0D0, OK )
         CALL CHCKSI ( 'N',       N,      '=',           5, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 10 part 2' )
         CALL GDPOOL ( 'VALUE10', 1, 10, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'VALUE10', VALUES, '=', EVALS(1), 10,0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',       N,      '=',           10, 0,     OK )

         CALL TSTMSG ( '#',  'Checking Value 10 part 3' )
         CALL GDPOOL ( 'VALUE10', 11, 5, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',       N,      '=',            0, 0,     OK )


         EVALS(  1 ) = 5
         EVALS(  2 ) = 4
         EVALS(  3 ) = 3
         EVALS(  4 ) = 2
         EVALS(  5 ) = 1
         EVALS(  6 ) = 0

         CALL TSTMSG ( '#',  'Checking Value 11 ' )
         CALL GDPOOL ( 'VALUE11', 1, 10, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',       N,     '=', 6, 0, OK    )
         CALL CHCKAD ( 'VALUE11', VALUES, '=', EVALS(1), 6,0.0D0, OK )


!
!     Case 4
!
      CALL TCASE ( 'Make sure that CLPOOL really empties the ' &
      //           'kernel pool. ' )

         CALL CLPOOL

         DO I = 1, 11
            VARNAM = 'VALUE#'
            CALL REPMI  ( VARNAM, '#', I, VARNAM )

            CALL DTPOOL ( VARNAM, FOUND, N, TYPE )

            CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,     OK )
            CALL CHCKSI ( 'N',     N,    '=',  0, 0,   OK )
            CALL CHCKSC ( 'TYPE',  TYPE, '=', 'X',     OK )
         END DO



!
!     Case 5
!
      CALL TCASE ( 'Check out the routine RTPOOL to make sure it ' &
      //           'still works as before. ' )


         TEXT(  1 ) = BEGDAT()
         TEXT(  2 ) = '         VALUE1  = 1'
         TEXT(  3 ) = '         VALUE2  = 2'
         TEXT(  4 ) = '         VALUE3  = PI'
         TEXT(  5 ) = '         VALUE4  = 3'
         TEXT(  6 ) = '         VALUE5  = 4'
         TEXT(  7 ) = '         VALUE6  = 5'
         TEXT(  8 ) = '         VALUE7  = 1.276828E+11'
         TEXT(  9 ) = '         VALUE8  = -28.19729871E+12'
         TEXT( 10 ) = '         VALUE9  = @1-JAN-1994'
         TEXT( 11 ) = '         VALUE10 = ('
         TEXT( 12 ) = '                     1,'
         TEXT( 13 ) = '                     2,'
         TEXT( 14 ) = '                     3,'
         TEXT( 15 ) = '                     4,'
         TEXT( 16 ) = '                     5,'
         TEXT( 17 ) = '                     6,'
         TEXT( 18 ) = '                     7,'
         TEXT( 19 ) = '                     8 )'
         TEXT( 20 ) = '         VALUE11  = ( 5, 4, 3, 2 )'
         TEXT( 21 ) = '         VALUE10 +=  9'
         TEXT( 22 ) = ' '
         TEXT( 23 ) = '         VALUE11 += ( 1, 0 )'
         TEXT( 24 ) = '         VALUE10 += 10'
         TEXT( 25 ) = ' '



         CALL KILFIL ( 'testdata.ker' )
         CALL TSTTXT ( 'testdata.ker', TEXT, 25, .FALSE., .TRUE.)
         CALL LDPOOL ( 'testdata.ker' )


         CALL RTPOOL ( 'VALUE4',  N, VALUES, FOUND )

         CALL CHCKXC ( .FALSE.,                     ' ',        OK )
         CALL CHCKSI ( 'N      for VALUE4', N,      '=', 1, 0,  OK )
         CALL CHCKSD ( 'Values for VALUE4', VALUES, '=', 3.0D0, &
                                                         0.0D0, OK )
         CALL CHCKSL ( 'FOUND  for VALUE4', FOUND,      .TRUE., OK )


         CALL RTPOOL ( 'VALUE11', N, VALUES, FOUND )

         EVALS ( 1 ) = 5.0D0
         EVALS ( 2 ) = 4.0D0
         EVALS ( 3 ) = 3.0D0
         EVALS ( 4 ) = 2.0D0
         EVALS ( 5 ) = 1.0D0
         EVALS ( 6 ) = 0.0D0

         CALL CHCKXC ( .FALSE.,                      ' ',        OK )
         CALL CHCKSI ( 'N      for VALUE11', N,      '=', 6, 0,  OK )
         CALL CHCKAD ( 'Values for VALUE11', VALUES, '=', EVALS, 6, &
                                                          0.0D0, OK )
         CALL CHCKSL ( 'FOUND  for VALUE4', FOUND,       .TRUE., OK )


!
!     Case 6
!
      CALL TCASE ( 'Make sure we can get integer values out of ' &
      //           'the kernel pool. ' )


         CALL GIPOOL ( 'VALUE11', 1, 20, N, INTVAL, FOUND )


         IVALS(1) = 5
         IVALS(2) = 4
         IVALS(3) = 3
         IVALS(4) = 2
         IVALS(5) = 1
         IVALS(6) = 0

         CALL CHCKXC ( .FALSE.,             ' ',           OK )
         CALL CHCKSI ( 'N for VALUE11',  N, '=', 6,     0, OK )
         CALL CHCKAI ( 'VALUE11',   INTVAL, '=', IVALS, N, OK )


!
!     Case 7
!
      CALL TCASE ( 'Make sure we can load and retrieve text ' &
      //           'values. ' )

         TEXT(  1 ) = BEGDAT()
         TEXT(  2 ) = ' '
         TEXT(  3 ) = '   SVALUE1  = ''String 1'''
         TEXT(  4 ) = '   SVALUE2  = ''String 2'''
         TEXT(  5 ) = '   SVALUE3  = ''String 3'''
         TEXT(  6 ) = '   SVALUE4  = ''String 4'''
         TEXT(  7 ) = '   SVALUE5  = ''String 5'''
         TEXT(  8 ) = '   SVALUE6  = ''String 6.1'', ''String 6.' &
         //           '2'', ''String 6.3'''
         TEXT(  9 ) = '   SVALUE6  = ''String 6'''
         TEXT( 10 ) = ' '
         TEXT( 11 ) = '   SVALUE7 += ''String 7.0'''
         TEXT( 12 ) = '   SVALUE7 += ''String 7.1'''
         TEXT( 13 ) = '   SVALUE7 += ''String 7.2'''
         TEXT( 14 ) = '   SVALUE7 += ''String 7.3'''
         TEXT( 15 ) = '   SVALUE7 += ''String 7.4'''
         TEXT( 16 ) = ' '
         TEXT( 17 ) = '   SVALUE8 = ( ''String 8.0'', ''String 8' &
         //           '.1'','
         TEXT( 18 ) = '               ''String 8.2'', ''String 8' &
         //           '.3'' )'
         TEXT( 19 ) = ' '

         CALL KILFIL ( 'testdata.ker' )
         CALL TSTTXT ( 'testdata.ker', TEXT, 19, .FALSE., .TRUE.)
         CALL LDPOOL ( 'testdata.ker' )

         CALL CHCKXC ( .FALSE., ' ',  OK )

         ESIZE(1) = 1
         ESIZE(2) = 1
         ESIZE(3) = 1
         ESIZE(4) = 1
         ESIZE(5) = 1
         ESIZE(6) = 1
         ESIZE(7) = 5
         ESIZE(8) = 4

         DO I = 1, 8
            VARNAM = 'SVALUE#'
            CALL REPMI  ( VARNAM, '#', I, VARNAM )
            CALL DTPOOL ( VARNAM, FOUND, N, TYPE )

            ITEM(1) = 'FOUND for ' // VARNAM
            ITEM(2) = 'N     for ' // VARNAM
            ITEM(3) = 'TYPE  for ' // VARNAM

            CALL CHCKXC ( .FALSE., ' ',                     OK )
            CALL CHCKSL ( ITEM(1), FOUND,     .TRUE.,       OK )
            CALL CHCKSI ( ITEM(2),     N, '=', ESIZE(I), 0, OK )
            CALL CHCKSC ( ITEM(3),  TYPE, '=', 'C',         OK )
         END DO


!
!     Case 8
!
      CALL TCASE ( 'Check that the text items loaded in the ' &
      //           'previous test have the expected values. ' )



         CVALS(1) = 'String 7.0'
         CVALS(2) = 'String 7.1'
         CVALS(3) = 'String 7.2'
         CVALS(4) = 'String 7.3'
         CVALS(5) = 'String 7.4'

         CALL GCPOOL ( 'SVALUE7', 1, 20, N, CVALUE, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND for SVALUE7', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N     for SVALUE7', N, '=', 5, 0,   OK )


         CALL CHCKSC ( 'Value 1 of SVALUE7', CVALUE(1), '=', &
                                             CVALS (1),     OK )
         CALL CHCKSC ( 'Value 2 of SVALUE7', CVALUE(2), '=', &
                                             CVALS (2),     OK )
         CALL CHCKSC ( 'Value 3 of SVALUE7', CVALUE(3), '=', &
                                             CVALS (3),     OK )
         CALL CHCKSC ( 'Value 4 of SVALUE7', CVALUE(4), '=', &
                                             CVALS (4),     OK )
         CALL CHCKSC ( 'Value 5 of SVALUE7', CVALUE(5), '=', &
                                             CVALS (5),     OK )

         CVALS(1) = 'String 8.0'
         CVALS(2) = 'String 8.1'
         CVALS(3) = 'String 8.2'
         CVALS(4) = 'String 8.3'

         CALL GCPOOL ( 'SVALUE8', 3, 20, N, CVALUE, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND for SVALUE7', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N     for SVALUE7', N, '=', 2, 0,   OK )


         CALL CHCKSC ( 'Value 3 of SVALUE8', CVALUE(1), '=', &
                                             CVALS (3),     OK )
         CALL CHCKSC ( 'Value 4 of SVALUE8', CVALUE(2), '=', &
                                             CVALS (4),     OK )



!
!     Case 9
!
      CALL TCASE ( 'Make sure we don''t get string and numeric ' &
      //           'values confused. ' )


         CALL GDPOOL ( 'SVALUE8', 1, 20, N, VALUES, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'N for d.p.', N, '=', 0, 0, OK )
         CALL CHCKSL ( 'D.P. FOUND for SVALUE8', &
                        FOUND, .FALSE.,  OK )

         CALL GIPOOL ( 'SVALUE8', 1, 20, N, IVALS, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'N for Int', N, '=', 0, 0, OK )
         CALL CHCKSL ( 'Integer FOUND for SVALUE8', &
                        FOUND, .FALSE.,  OK )

         CALL GCPOOL ( 'VALUE1', 1, 20, N, CVALUE, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'N for Char', N, '=', 0, 0, OK )
         CALL CHCKSL ( 'Char FOUND for VALUE1', &
                        FOUND, .FALSE.,  OK )


!
!     Case 10
!
      CALL TCASE ( 'Make sure that we can write out string data ' &
      //           'via WRPOOL ' )


         CALL KILFIL ( 'write.dat' )
         CALL TXTOPN ( 'write.dat', UNIT )
         CALL WRPOOL (  UNIT )
         CLOSE ( UNIT )

         CALL CLPOOL


         CALL DTPOOL ( 'SVALUE1', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'X',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  0, 0, OK )

         CALL DTPOOL ( 'SVALUE2', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'X',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  0, 0, OK )

         CALL DTPOOL ( 'SVALUE3', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'X',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  0, 0, OK )

         CALL DTPOOL ( 'VALUE4', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'X',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  0, 0, OK )

         CALL DTPOOL ( 'VALUE5', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'X',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  0, 0, OK )

         CALL DTPOOL ( 'VALUE6', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'X',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  0, 0, OK )

         CALL DTPOOL ( 'VALUE7', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'X',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  0, 0, OK )

         CALL DTPOOL ( 'VALUE8', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'X',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  0, 0, OK )

         CALL LDPOOL ( 'write.dat' )

         CALL DTPOOL ( 'SVALUE1', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,     OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'C',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  1, 0, OK )

         CALL DTPOOL ( 'SVALUE2', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,     OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'C',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  1, 0, OK )

         CALL DTPOOL ( 'SVALUE7', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,     OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'C',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  5, 0, OK )

         CALL DTPOOL ( 'VALUE4', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,     OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'N',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  1, 0, OK )

         CALL DTPOOL ( 'VALUE5', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,     OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'N',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  1, 0, OK )

         CALL DTPOOL ( 'VALUE6', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,     OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'N',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  1, 0, OK )

         CALL DTPOOL ( 'VALUE10', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,     OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'N',   OK )
         CALL CHCKSI ( 'N',     N,     '=', 10, 0, OK )

         CALL DTPOOL ( 'VALUE11', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,     OK )
         CALL CHCKSC ( 'TYPE',  TYPE,  '=', 'N',   OK )
         CALL CHCKSI ( 'N',     N,     '=',  6, 0, OK )

         CVALS(1) = 'String 7.0'
         CVALS(2) = 'String 7.1'
         CVALS(3) = 'String 7.2'
         CVALS(4) = 'String 7.3'
         CVALS(5) = 'String 7.4'

         CALL GCPOOL ( 'SVALUE7', 1, 20, N, CVALUE, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND for SVALUE7', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N     for SVALUE7', N, '=', 5, 0,   OK )

         CALL RTPOOL ( 'VALUE11', N, VALUES, FOUND )

         EVALS ( 1 ) = 5.0D0
         EVALS ( 2 ) = 4.0D0
         EVALS ( 3 ) = 3.0D0
         EVALS ( 4 ) = 2.0D0
         EVALS ( 5 ) = 1.0D0
         EVALS ( 6 ) = 0.0D0

         CALL CHCKXC ( .FALSE.,                      ' ',        OK )
         CALL CHCKSI ( 'N      for VALUE11', N,      '=', 6, 0,  OK )
         CALL CHCKAD ( 'Values for VALUE11', VALUES, '=', EVALS, 6, &
                                                          0.0D0, OK )


!
!     Case 11
!
      CALL TCASE ( 'Make sure we can set watchers and that they ' &
      //           'get triggered at the appropriate times. ' )

!
!        Create a list of variables to be watched.
!
         NAMES(1) = 'SVALUE8'
         NAMES(2) = 'VALUE10'
         NAMES(3) = 'SPUD'

!
!        Establish a watcher on the string variable and on the
!        numeric variable.
!
         CALL SWPOOL ( 'S_POOL', 1, NAMES    )
         CALL SWPOOL ( 'D_POOL', 1, NAMES(2) )
         CALL SWPOOL ( 'DUMMY',  1, NAMES(3) )
!
!        Check to see that S_POOL, D_POOL and DUMMY have notifications
!        pending.
!
         CALL CVPOOL ( 'S_POOL',    UPDATE   )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .TRUE.,  OK )

         CALL CVPOOL ( 'D_POOL',    UPDATE   )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for D_POOL', UPDATE, .TRUE.,  OK )

         CALL CVPOOL ( 'DUMMY',     UPDATE   )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for DUMMY',  UPDATE, .TRUE.,  OK )
!
!        Now check that S_POOL, D_POOL and DUMMY do not have
!        notifications pending.
!
         CALL CVPOOL ( 'S_POOL', UPDATE )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .FALSE., OK )

         CALL CVPOOL ( 'D_POOL', UPDATE )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .FALSE., OK )

         CALL CVPOOL ( 'DUMMY',     UPDATE   )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for DUMMY',  UPDATE, .FALSE., OK )

         CALL CVPOOL ( 'UNDECLAGENT', UPDATE )
         CALL CHCKXC ( .FALSE.,        ' ',            OK )
         CALL CHCKSL ( 'UNDECLAGENT', UPDATE, .FALSE., OK )

!
!        Clear the kernel pool and see if everything behaves as
!        expected.
!
         CALL CLPOOL

         CALL TSTMSG ( '#', 'Same tests after clearing the pool.' )
!
!        Check to see that S_POOL, D_POOL and DUMMY have notifications
!        pending.
!
         CALL CVPOOL ( 'S_POOL',    UPDATE   )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .TRUE.,  OK )

         CALL CVPOOL ( 'D_POOL',    UPDATE   )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for D_POOL', UPDATE, .TRUE.,  OK )

         CALL CVPOOL ( 'DUMMY',     UPDATE   )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for DUMMY',  UPDATE, .TRUE.,  OK )
!
!        Now check that S_POOL, D_POOL and DUMMY do not have
!        notifications pending.
!
         CALL CVPOOL ( 'S_POOL', UPDATE )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .FALSE., OK )

         CALL CVPOOL ( 'D_POOL', UPDATE )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .FALSE., OK )

         CALL CVPOOL ( 'DUMMY',     UPDATE   )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for DUMMY',  UPDATE, .FALSE., OK )

         CALL CVPOOL ( 'UNDECLAGENT', UPDATE )
         CALL CHCKXC ( .FALSE.,        ' ',            OK )
         CALL CHCKSL ( 'UNDECLAGENT', UPDATE, .FALSE., OK )


!
!        Now load a kernel and see if these items get updated
!        as expected
!
         CALL LDPOOL ( 'write.dat' )
         CALL TSTMSG ( '#', 'Same watcher tests after loading a file.' )

         CALL CVPOOL ( 'S_POOL',    UPDATE   )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .TRUE.,  OK )

         CALL CVPOOL ( 'D_POOL',    UPDATE   )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for D_POOL', UPDATE, .TRUE.,  OK )

         CALL CVPOOL ( 'DUMMY',     UPDATE   )

         CALL CHCKXC ( .FALSE.,        ' ',                   OK )
         CALL CHCKSL ( 'UPDATE for DUMMY',  UPDATE, .FALSE.,  OK )

!
!        Now check that S_POOL, D_POOL and DUMMY do not have
!        notifications pending.
!
         CALL CVPOOL ( 'S_POOL', UPDATE )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .FALSE., OK )

         CALL CVPOOL ( 'D_POOL', UPDATE )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .FALSE., OK )

         CALL CVPOOL ( 'DUMMY',     UPDATE   )

         CALL CHCKXC ( .FALSE.,        ' ',                  OK )
         CALL CHCKSL ( 'UPDATE for DUMMY',  UPDATE, .FALSE., OK )

         CALL CVPOOL ( 'UNDECLAGENT', UPDATE )
         CALL CHCKXC ( .FALSE.,        ' ',            OK )
         CALL CHCKSL ( 'UNDECLAGENT', UPDATE, .FALSE., OK )
         CALL TSTMSG ( '#', ' ' )

!
!        Delete watchers used in this test.
!
         CALL DWPOOL ( 'S_POOL' )
         CALL DWPOOL ( 'D_POOL' )
         CALL DWPOOL ( 'DUMMY'  )


!
!     Case 12
!
      CALL TCASE ( 'Make sure that RTPOOL does not work on ' &
      //           'string valued variables. ' )

         N = 123
         CALL RTPOOL ( 'SVALUE4',  N, VALUES, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
         CALL CHCKSI ( 'N    ', N, '=', 123, 0,   OK )


!
!     Case 13
!
      CALL TCASE ( 'Make sure that the entry point EXPOOL works ' &
      //           'only on numeric data types. ' )

!
!        EXPOOL should not find any of the string variables.
!
         DO I = 1, 8

            VARNAM = 'SVALUE#'
            CALL REPMI ( VARNAM, '#', I, VARNAM )

            CALL EXPOOL ( VARNAM, FOUND )

            CALL PREFIX ( 'FOUND for ',   1,   VARNAM )
            CALL CHCKXC ( .FALSE., ' ',            OK )
            CALL CHCKSL ( VARNAM, FOUND, .FALSE.,  OK )

         END DO
!
!        EXPOOL should find values for all of the numeric variables
!
         DO I = 1, 11

            VARNAM = 'VALUE#'
            CALL REPMI ( VARNAM, '#', I, VARNAM )

            CALL EXPOOL ( VARNAM, FOUND )

            CALL PREFIX ( 'FOUND for ',   1,   VARNAM )
            CALL CHCKXC ( .FALSE., ' ',            OK )
            CALL CHCKSL (  VARNAM, FOUND, .TRUE.,  OK )

         END DO



!
!     Case 14
!
      CALL TCASE ( 'Make sure that GIPOOL doesn''t try to ' &
      //           'swallow integers that are too big. ' )


         TEXT(  1 ) = BEGDAT()
         TEXT(  2 ) = ' '
         TEXT(  3 ) = '      VALUE12 = 1.0D15'
         TEXT(  4 ) = ' '

         CALL KILFIL ( 'testdata.ker' )
         CALL TSTTXT ( 'testdata.ker', TEXT, 4, .TRUE., .FALSE. )

         CALL GIPOOL ( 'VALUE12', 1, 10, N, IVALS, FOUND   )
         CALL CHCKXC ( .TRUE., 'SPICE(INTOUTOFRANGE)',  OK )



!
!     Case 15
!
      CALL TCASE ( 'Make sure that an error is signaled if we ' &
      //           'give a non-positive room size to GCPOOL ' )

         CALL GCPOOL ( 'SVALUE1', 1, 0, N, CVALUE, FOUND )
         CALL CHCKXC ( .TRUE., 'SPICE(BADARRAYSIZE)',  OK )


!
!     Case 16
!
      CALL TCASE ( 'Make sure that an error is signaled if we ' &
      //           'give a non-positive room size to GIPOOL ' )

         CALL GIPOOL ( 'VALUE1', 1, 0, N, IVALS, FOUND )
         CALL CHCKXC ( .TRUE., 'SPICE(BADARRAYSIZE)',  OK )

!
!     Case 17
!
       CALL TCASE ( 'Make sure that an error is signaled if we ' &
      //           'give a non-positive room size to GDPOOL ' )

         CALL GDPOOL ( 'VALUE10', 1, 0, N, VALUES, FOUND )
         CALL CHCKXC ( .TRUE., 'SPICE(BADARRAYSIZE)',  OK )


!
!     Case 18
!
      CALL TCASE ( 'Make sure an error is triggered if we try to ' &
      //           'load a string and numeric value into a ' &
      //           'variable. ' )

         TEXT(  1 ) = BEGDAT()
         TEXT(  2 ) = '      VALUE13 = 123, ''123'''
         TEXT(  3 ) = ' '

         CALL KILFIL ( 'testdata.ker' )
         CALL TSTTXT ( 'testdata.ker', TEXT, 4, .FALSE., .TRUE. )

         CALL LDPOOL ( 'testdata.ker' )

!        CALL GETMSG ( 'LONG',  LONG )
         CALL CHCKXC ( .TRUE., 'SPICE(TYPEMISMATCH)',  OK )
!        CALL TSTLOG (  LONG, .FALSE. )


!
!     Case 19
!
      CALL TCASE ( 'Make sure an error is triggered if we try to ' &
      //           'load a numeric value and a string value (in ' &
      //           'that order) into a variable. ' )

         TEXT(  1 ) = BEGDAT()
         TEXT(  2 ) = '   SVALUE9 = ''123'', 123'
         TEXT(  3 ) = ' '

         CALL KILFIL ( 'testdata.ker' )
         CALL TSTTXT ( 'testdata.ker', TEXT, 4, .FALSE., .TRUE. )

         CALL LDPOOL ( 'testdata.ker' )

!        CALL GETMSG ( 'LONG',  LONG )
         CALL CHCKXC ( .TRUE., 'SPICE(TYPEMISMATCH)',  OK )
!        CALL TSTLOG (  LONG, .FALSE. )


!
!     Case 20
!
      CALL TCASE ( 'Add d.p. data to the kernel pool through the ' &
      //           'PDPOOL interface. ' )



         MYDATA(1) = 1.0D0
         MYDATA(2) = 2.0D0
         MYDATA(3) = 3.0D0
         MYDATA(4) = 4.0D0
         MYDATA(5) = 5.0D0
         MYDATA(6) = 6.0D0

         CALL PDPOOL ( 'MYDPS', 6,   MYDATA )
         CALL CHCKXC ( .FALSE., ' ',  OK     )

         CALL DTPOOL ( 'MYDPS', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N',     N, '=', 6, 0,   OK )
         CALL CHCKSC ( 'TYPE', TYPE, '=', 'N',  OK )

         CALL GDPOOL ( 'MYDPS', 1, 7, N, VALUES, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N',     N, '=', 6, 0,   OK )

         CALL CHCKAD ( 'VALUES', VALUES, '=', MYDATA, 6, 0.0D0, OK )



!
!     Case 21
!
      CALL TCASE ( 'Add string data through the PCPOOL interface.' )


         MYSTRS ( 1 ) = 'String 1'
         MYSTRS ( 2 ) = 'String 2'
         MYSTRS ( 3 ) = 'String 3'
         MYSTRS ( 4 ) = 'String 4'
         MYSTRS ( 5 ) = 'String 5'

         CALL PCPOOL ( 'MYSTRS', 5, MYSTRS )
         CALL CHCKXC ( .FALSE., ' ',  OK     )

         CALL DTPOOL ( 'MYSTRS', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N',     N, '=', 5, 0,   OK )
         CALL CHCKSC ( 'TYPE', TYPE, '=', 'C',  OK )

         CALL GCPOOL ( 'MYSTRS', 1, 7, N, CVALUE, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N',     N, '=', 5, 0,   OK )


         CALL CHCKSC ( 'CVALUE(1)', CVALUE(1), '=', MYSTRS(1), OK )
         CALL CHCKSC ( 'CVALUE(2)', CVALUE(2), '=', MYSTRS(2), OK )
         CALL CHCKSC ( 'CVALUE(3)', CVALUE(3), '=', MYSTRS(3), OK )
         CALL CHCKSC ( 'CVALUE(4)', CVALUE(4), '=', MYSTRS(4), OK )
         CALL CHCKSC ( 'CVALUE(5)', CVALUE(5), '=', MYSTRS(5), OK )


!
!     Case 22
!
      CALL TCASE ( 'Add integer data through the PIPOOL interface' )


         MYINTS(1) = 5
         MYINTS(2) = 6
         MYINTS(3) = 7
         MYINTS(4) = 8
         MYINTS(5) = 9
         MYINTS(6) = 10

         CALL PIPOOL ( 'MYINTS', 4,   MYINTS )
         CALL CHCKXC ( .FALSE., ' ',  OK     )

         CALL DTPOOL ( 'MYINTS', FOUND, N, TYPE )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N',     N, '=', 4, 0,   OK )
         CALL CHCKSC ( 'TYPE', TYPE, '=', 'N',  OK )

         CALL GIPOOL ( 'MYINTS', 1, 7, N, IVALS, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N',     N, '=', 4, 0,   OK )

         CALL CHCKAI ( 'IVALS', IVALS, '=', MYINTS, 4, OK )


!
!     Case 23
!
      CALL TCASE ( 'See if we can still clear the kernel pool. ')


         CALL CLPOOL ()
         CALL DTPOOL ( 'MYDPS',  FOUND, N, TYPE )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
         CALL CHCKSI ( 'N',     N, '=', 0, 0,   OK )
         CALL CHCKSC ( 'TYPE', TYPE, '=', 'X',  OK )

         CALL DTPOOL ( 'MYSTRS', FOUND, N, TYPE )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
         CALL CHCKSI ( 'N',     N, '=', 0, 0,   OK )
         CALL CHCKSC ( 'TYPE', TYPE, '=', 'X',  OK )


!
!     Case 24
!
      CALL TCASE ( 'Check that loading from a text buffer works.' &
      //           'Note that these tests are the same as test ' &
      //           'case 1 and 2 but for variables loaded from ' &
      //           'an internal text buffer. ' )


         TEXT(  2 ) = '         VALUE1  = 1'
         TEXT(  3 ) = '         VALUE2  = 2'
         TEXT(  4 ) = '         VALUE3  = PI'
         TEXT(  5 ) = '         VALUE4  = 3'
         TEXT(  6 ) = '         VALUE5  = 4'
         TEXT(  7 ) = '         VALUE6  = 5'
         TEXT(  8 ) = '         VALUE7  = 1.276828E+11'
         TEXT(  9 ) = '         VALUE8  = -28.19729871E+12'
         TEXT( 10 ) = '         VALUE9  = @1-JAN-1994'
         TEXT( 11 ) = '         VALUE10 = ('
         TEXT( 12 ) = '                     1,'
         TEXT( 13 ) = '                     2,'
         TEXT( 14 ) = '                     3,'
         TEXT( 15 ) = '                     4,'
         TEXT( 16 ) = '                     5,'
         TEXT( 17 ) = '                     6,'
         TEXT( 18 ) = '                     7,'
         TEXT( 19 ) = '                     8 )'
         TEXT( 20 ) = '         VALUE11  = ( 5, 4, 3, 2 )'
         TEXT( 21 ) = ' '
         TEXT( 22 ) = '         VALUE10 +=  9'
         TEXT( 23 ) = '         VALUE11 += ( 1, 0 )'
         TEXT( 24 ) = '         VALUE10 += 10'

         CALL LMPOOL ( TEXT(2), 23 )

         CALL CHCKXC ( .FALSE., ' ',  OK )

         ESIZE(  1 ) = 1
         ESIZE(  2 ) = 1
         ESIZE(  3 ) = 1
         ESIZE(  4 ) = 1
         ESIZE(  5 ) = 1
         ESIZE(  6 ) = 1
         ESIZE(  7 ) = 1
         ESIZE(  8 ) = 1
         ESIZE(  9 ) = 1
         ESIZE( 10 ) = 10
         ESIZE( 11 ) = 6


         DO I = 1, 11
            VARNAM = 'VALUE#'
            CALL REPMI  ( VARNAM, '#', I, VARNAM )

            CALL DTPOOL ( VARNAM, FOUND, N, TYPE )
            CALL TSTMSG ( '#', VARNAM )
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,            OK )
            CALL CHCKSI ( 'N',     N,    '=', ESIZE(I), 0,   OK )
            CALL CHCKSC ( 'TYPE',  TYPE, '=', 'N',           OK )
         END DO

         CALL TSTMSG ( '#', ' ' )

         CALL TSTMSG ( '#',  'Checking Value 1' )
         CALL GDPOOL ( 'VALUE1', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL NPARSD ( '1',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE1', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )


         CALL TSTMSG ( '#',  'Checking Value 2' )
         CALL GDPOOL ( 'VALUE2', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( '2',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE2', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 3' )
         CALL GDPOOL ( 'VALUE3', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( 'PI',     EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE3', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 4' )
         CALL GDPOOL ( 'VALUE4', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( '3',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE4', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 5' )
         CALL GDPOOL ( 'VALUE5', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( '4',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE5', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 6' )
         CALL GDPOOL ( 'VALUE6', 1, 1, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL NPARSD ( '5',      EVALS(1), ERROR, PTR   )
         CALL CHCKAD ( 'VALUE6', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 7' )
         CALL NPARSD ( '1.276828E+11',  EVALS(1), ERROR, PTR   )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL GDPOOL ( 'VALUE7', 1, 1, N, VALUES, FOUND )
         CALL CHCKAD ( 'VALUE7', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 8' )
         CALL NPARSD ( '-28.19729871E+12', EVALS(1), ERROR, PTR   )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL GDPOOL ( 'VALUE8', 1, 1, N, VALUES, FOUND )
         CALL CHCKAD ( 'VALUE8', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',  FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 9' )
         CALL TPARSE ( '1-JAN-1994', EVALS(1), ERROR )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL GDPOOL ( 'VALUE9', 1, 1, N, VALUES, FOUND )
         CALL CHCKAD ( 'VALUE9', VALUES, '=', EVALS, 1, 0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',      N,      '=',        1, 0,     OK )


         EVALS(  1 ) =  1
         EVALS(  2 ) =  2
         EVALS(  3 ) =  3
         EVALS(  4 ) =  4
         EVALS(  5 ) =  5
         EVALS(  6 ) =  6
         EVALS(  7 ) =  7
         EVALS(  8 ) =  8
         EVALS(  9 ) =  9
         EVALS( 10 ) = 10

         CALL TSTMSG ( '#',  'Checking Value 10' )
         CALL GDPOOL ( 'VALUE10', 3, 5, N,     VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'VALUE10', VALUES, '=', EVALS(3), 5, 0.0D0, OK )
         CALL CHCKSI ( 'N',       N,      '=',           5, 0,     OK )


         CALL TSTMSG ( '#',  'Checking Value 10 part 2.' )
         CALL GDPOOL ( 'VALUE10', 1, 10, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKAD ( 'VALUE10', VALUES, '=', EVALS(1), 10,0.0D0, OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',       N,      '=',           10, 0,     OK )

         CALL TSTMSG ( '#',  'Checking Value 10 part 3' )
         CALL GDPOOL ( 'VALUE10', 11, 5, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',       N,      '=',            0, 0,     OK )


         EVALS(  1 ) = 5
         EVALS(  2 ) = 4
         EVALS(  3 ) = 3
         EVALS(  4 ) = 2
         EVALS(  5 ) = 1
         EVALS(  6 ) = 0

         CALL TSTMSG ( '#',  'Checking Value 11' )
         CALL GDPOOL ( 'VALUE11', 1, 10, N, VALUES, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND',   FOUND, .TRUE.,    OK    )
         CALL CHCKSI ( 'N',       N,     '=', 6, 0, OK    )
         CALL CHCKAD ( 'VALUE11', VALUES, '=', EVALS(1), 6,0.0D0, OK )
         CALL TSTMSG ( '#',  ' ' )


!
!     Case 25
!
      CALL TCASE ( 'Make sure we can read strings in from a text ' &
      //           'buffer. This is the same set of checks as ' &
      //           'used when we loaded string valued variables ' &
      //           'from a text file.  This time we load ' &
      //           'directly from a text buffer. ' )


         TEXT(  2 ) = ' '
         TEXT(  3 ) = '   SVALUE1  = ''String 1'''
         TEXT(  4 ) = '   SVALUE2  = ''String 2'''
         TEXT(  5 ) = '   SVALUE3  = ''String 3'''
         TEXT(  6 ) = '   SVALUE4  = ''String 4'''
         TEXT(  7 ) = '   SVALUE5  = ''String 5'''
         TEXT(  8 ) = '   SVALUE6  = ''String 6.1'', ''String 6.' &
         //           '2'', ''String 6.3'''
         TEXT(  9 ) = '   SVALUE6  = ''String 6'''
         TEXT( 10 ) = ' '
         TEXT( 11 ) = '   SVALUE7 += ''String 7.0'''
         TEXT( 12 ) = '   SVALUE7 += ''String 7.1'''
         TEXT( 13 ) = '   SVALUE7 += ''String 7.2'''
         TEXT( 14 ) = '   SVALUE7 += ''String 7.3'''
         TEXT( 15 ) = '   SVALUE7 += ''String 7.4'''
         TEXT( 16 ) = ' '
         TEXT( 17 ) = '   SVALUE8 = ( ''String 8.0'', ''String 8' &
         //           '.1'','
         TEXT( 18 ) = '               ''String 8.2'', ''String 8' &
         //           '.3'' )'


         CALL LMPOOL ( TEXT(2), 17 )


         CALL CHCKXC ( .FALSE., ' ',  OK )

         ESIZE(1) = 1
         ESIZE(2) = 1
         ESIZE(3) = 1
         ESIZE(4) = 1
         ESIZE(5) = 1
         ESIZE(6) = 1
         ESIZE(7) = 5
         ESIZE(8) = 4

         DO I = 1, 8
            VARNAM = 'SVALUE#'
            CALL REPMI  ( VARNAM, '#', I, VARNAM )
            CALL DTPOOL ( VARNAM, FOUND, N, TYPE )

            ITEM(1) = 'FOUND for ' // VARNAM
            ITEM(2) = 'N     for ' // VARNAM
            ITEM(3) = 'TYPE  for ' // VARNAM

            CALL CHCKXC ( .FALSE., ' ',                     OK )
            CALL CHCKSL ( ITEM(1), FOUND,     .TRUE.,       OK )
            CALL CHCKSI ( ITEM(2),     N, '=', ESIZE(I), 0, OK )
            CALL CHCKSC ( ITEM(3),  TYPE, '=', 'C',         OK )
         END DO


         CVALS(1) = 'String 7.0'
         CVALS(2) = 'String 7.1'
         CVALS(3) = 'String 7.2'
         CVALS(4) = 'String 7.3'
         CVALS(5) = 'String 7.4'

         CALL GCPOOL ( 'SVALUE7', 1, 20, N, CVALUE, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND for SVALUE7', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N     for SVALUE7', N, '=', 5, 0,   OK )


         CALL CHCKSC ( 'Value 1 of SVALUE7', CVALUE(1), '=', &
                                             CVALS (1),     OK )
         CALL CHCKSC ( 'Value 2 of SVALUE7', CVALUE(2), '=', &
                                             CVALS (2),     OK )
         CALL CHCKSC ( 'Value 3 of SVALUE7', CVALUE(3), '=', &
                                             CVALS (3),     OK )
         CALL CHCKSC ( 'Value 4 of SVALUE7', CVALUE(4), '=', &
                                             CVALS (4),     OK )
         CALL CHCKSC ( 'Value 5 of SVALUE7', CVALUE(5), '=', &
                                             CVALS (5),     OK )

         CVALS(1) = 'String 8.0'
         CVALS(2) = 'String 8.1'
         CVALS(3) = 'String 8.2'
         CVALS(4) = 'String 8.3'

         CALL GCPOOL ( 'SVALUE8', 3, 20, N, CVALUE, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND for SVALUE7', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N     for SVALUE7', N, '=', 2, 0,   OK )


         CALL CHCKSC ( 'Value 3 of SVALUE8', CVALUE(1), '=', &
                                             CVALS (3),     OK )
         CALL CHCKSC ( 'Value 4 of SVALUE8', CVALUE(2), '=', &
                                             CVALS (4),     OK )


!
!     Case 26
!
      CALL TCASE ( 'Make sure that watchers work when a direct ' &
      //           'write to the kernel pool takes place. ' )

         BNAMES(1) = 'SVALUE1'
         BNAMES(2) = 'MYDPS'

         NNAMES(1) = 'MYSTRS'
         NNAMES(2) = 'MYDPS'

         INAMES(1) = 'SVALUE1'
         INAMES(2) = 'MYINTS'

         NBILL = 2
         NNAT  = 2
         NIAN  = 2

         CALL SWPOOL ( 'BILL', NBILL, BNAMES )
         CALL SWPOOL ( 'NAT',  NNAT,  NNAMES )
         CALL SWPOOL ( 'IAN',  NIAN,  INAMES )

         CALL TSTMSG ( '#', 'Initial check of watchers.' )

         CALL CVPOOL ( 'BILL', BUPDAT )
         CALL CVPOOL ( 'NAT',  NUPDAT )
         CALL CVPOOL ( 'IAN',  IUPDAT )

         CALL CHCKSL ( 'BUPDAT', BUPDAT, .TRUE.,  OK )
         CALL CHCKSL ( 'NUPDAT', NUPDAT, .TRUE.,  OK )
         CALL CHCKSL ( 'IUPDAT', IUPDAT, .TRUE.,  OK )

         CALL CVPOOL ( 'BILL', BUPDAT )
         CALL CVPOOL ( 'NAT',  NUPDAT )
         CALL CVPOOL ( 'IAN',  IUPDAT )

         CALL CHCKSL ( 'BUPDAT', BUPDAT, .FALSE.,  OK )
         CALL CHCKSL ( 'NUPDAT', NUPDAT, .FALSE.,  OK )
         CALL CHCKSL ( 'IUPDAT', IUPDAT, .FALSE.,  OK )


         MYDATA(1) = 1.0D0
         MYDATA(2) = 2.0D0
         MYDATA(3) = 3.0D0
         MYDATA(4) = 4.0D0
         MYDATA(5) = 5.0D0
         MYDATA(6) = 6.0D0

         CALL PDPOOL ( 'MYDPS', 6,   MYDATA )
         CALL CHCKXC ( .FALSE., ' ',  OK     )

         CALL TSTMSG ( '#', 'Check of watchers after call to PDPOOL.' )

         CALL CVPOOL ( 'BILL', BUPDAT )
         CALL CVPOOL ( 'NAT',  NUPDAT )
         CALL CVPOOL ( 'IAN',  IUPDAT )

         CALL CHCKSL ( 'BUPDAT', BUPDAT, .TRUE.,  OK )
         CALL CHCKSL ( 'NUPDAT', NUPDAT, .TRUE.,  OK )
         CALL CHCKSL ( 'IUPDAT', IUPDAT, .FALSE.,  OK )


         TEXT(  2 ) = ' '
         TEXT(  3 ) = '   SVALUE1  = ''String 1'''
         TEXT(  4 ) = '   SVALUE2  = ''String 2'''
         TEXT(  5 ) = '   SVALUE3  = ''String 3'''
         TEXT(  6 ) = '   SVALUE4  = ''String 4'''
         TEXT(  7 ) = '   SVALUE5  = ''String 5'''
         TEXT(  8 ) = '   SVALUE6  = ''String 6.1'', ''String 6.' &
         //           '2'', ''String 6.3'''
         TEXT(  9 ) = '   SVALUE6  = ''String 6'''
         TEXT( 10 ) = ' '
         TEXT( 11 ) = '   SVALUE7 += ''String 7.0'''
         TEXT( 12 ) = '   SVALUE7 += ''String 7.1'''
         TEXT( 13 ) = '   SVALUE7 += ''String 7.2'''
         TEXT( 14 ) = '   SVALUE7 += ''String 7.3'''
         TEXT( 15 ) = '   SVALUE7 += ''String 7.4'''
         TEXT( 16 ) = ' '
         TEXT( 17 ) = '   SVALUE8 = ( ''String 8.0'', ''String 8' &
         //           '.1'','
         TEXT( 18 ) = '               ''String 8.2'', ''String 8' &
         //           '.3'' )'

         CALL LMPOOL ( TEXT(2), 17 )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL TSTMSG ( '#', 'Check of watchers after call to LMPOOL.' )

         CALL CVPOOL ( 'BILL', BUPDAT )
         CALL CVPOOL ( 'NAT',  NUPDAT )
         CALL CVPOOL ( 'IAN',  IUPDAT )

         CALL CHCKSL ( 'BUPDAT', BUPDAT, .TRUE.,  OK )
         CALL CHCKSL ( 'NUPDAT', NUPDAT, .FALSE.,  OK )
         CALL CHCKSL ( 'IUPDAT', IUPDAT, .TRUE.,  OK )

         CALL CVPOOL ( 'BILL', BUPDAT )
         CALL CVPOOL ( 'NAT',  NUPDAT )
         CALL CVPOOL ( 'IAN',  IUPDAT )

         CALL CHCKSL ( 'BUPDAT', BUPDAT, .FALSE.,  OK )
         CALL CHCKSL ( 'NUPDAT', NUPDAT, .FALSE.,  OK )
         CALL CHCKSL ( 'IUPDAT', IUPDAT, .FALSE.,  OK )


         MYINTS(1) = 5
         MYINTS(2) = 6
         MYINTS(3) = 7
         MYINTS(4) = 8
         MYINTS(5) = 9
         MYINTS(6) = 10

         CALL TSTMSG ( '#', 'Check of watchers after call to PIPOOL.' )

         CALL PIPOOL ( 'MYINTS', 4,   MYINTS )
         CALL CHCKXC ( .FALSE., ' ',  OK     )


         CALL CVPOOL ( 'BILL', BUPDAT )
         CALL CVPOOL ( 'NAT',  NUPDAT )
         CALL CVPOOL ( 'IAN',  IUPDAT )

         CALL CHCKSL ( 'BUPDAT', BUPDAT, .FALSE.,  OK )
         CALL CHCKSL ( 'NUPDAT', NUPDAT, .FALSE.,   OK )
         CALL CHCKSL ( 'IUPDAT', IUPDAT, .TRUE.,  OK )

         CALL TSTMSG ( '#', 'Check of watchers after call to PCPOOL.' )

         MYSTRS ( 1 ) = 'String 1'
         MYSTRS ( 2 ) = 'String 2'
         MYSTRS ( 3 ) = 'String 3'
         MYSTRS ( 4 ) = 'String 4'
         MYSTRS ( 5 ) = 'String 5'

         CALL PCPOOL ( 'MYSTRS', 5, MYSTRS )
         CALL CHCKXC ( .FALSE., ' ',  OK     )

         CALL CVPOOL ( 'BILL', BUPDAT )
         CALL CVPOOL ( 'NAT',  NUPDAT )
         CALL CVPOOL ( 'IAN',  IUPDAT )

         CALL CHCKSL ( 'BUPDAT', BUPDAT, .FALSE.,  OK )
         CALL CHCKSL ( 'NUPDAT', NUPDAT, .TRUE.,   OK )
         CALL CHCKSL ( 'IUPDAT', IUPDAT, .FALSE.,  OK )



!
!     Case 27
!
      CALL TCASE ( 'Make a cursory examination of SZPOOL' )

         CALL SZPOOL ( 'MAXVAR', N, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '>', 0,  0, OK )

         CALL SZPOOL ( 'maxval', N, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '>', 0,  0, OK )

         CALL SZPOOL ( 'Maxlin', N, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '>', 0,  0, OK )

         CALL SZPOOL ( 'MAXCHR', N, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '>', 0,  0, OK )

         CALL SZPOOL ( 'MXNOTE', N, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '>', 0,  0, OK )

         CALL SZPOOL ( 'maxlen', N, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '>', 0,  0, OK )

         CALL SZPOOL ( 'MAXAGT', N, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '>', 0,  0, OK )

         CALL SZPOOL ( 'STELLA', N, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 0,  0, OK )

         CALL SZPOOL ( 'stella', N, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 0,  0, OK )



!
!     Case 28
!
      CALL TCASE ( 'Check 1. for the empty vector assignment ' &
      //           'exception. ' )



         TEXT(  1 ) = BEGDAT()
         TEXT(  2 ) = '         VALUE1  = 1'
         TEXT(  3 ) = '         VALUE2  = 2'
         TEXT(  4 ) = '         VALUE3  = PI'
         TEXT(  5 ) = '         VALUE4  = 3'
         TEXT(  6 ) = '         VALUE5  = 4'
         TEXT(  7 ) = '         VALUE6  = 5'
         TEXT(  8 ) = '         VALUE7  = 1.276828E+11'
         TEXT(  9 ) = '         VALUE8  = -28.19729871E+12'
         TEXT( 10 ) = '         VALUE9  = @1-JAN-1994'
         TEXT( 11 ) = '         VALUE10 = ('
         TEXT( 12 ) = '                     1,'
         TEXT( 13 ) = '                     2,'
         TEXT( 14 ) = '                     3,'
         TEXT( 15 ) = '                     4,'
         TEXT( 16 ) = '                     5,'
         TEXT( 17 ) = '                     6,'
         TEXT( 18 ) = '                     7,'
         TEXT( 19 ) = '                     8 )'
         TEXT( 20 ) = '         VALUE11  = ( )'
         TEXT( 21 ) = '         VALUE10 +=  9'
         TEXT( 22 ) = ' '
         TEXT( 23 ) = '         VALUE11 += ( 1, 0 )'
         TEXT( 24 ) = '         VALUE10 += 10'
         TEXT( 25 ) = ' '

         CALL KILFIL ( 'testdata.ker' )
         CALL TSTTXT ( 'testdata.ker', TEXT, 25, .FALSE., .TRUE.)
         CALL LDPOOL ( 'testdata.ker' )

         CALL CHCKXC ( .TRUE., 'SPICE(BADVARASSIGN)',  OK )


!
!     Case 29
!
      CALL TCASE ( 'Check 2. for the empty vector assignment ' &
      //           'exception. ' )



         TEXT(  1 ) = BEGDAT()
         TEXT(  2 ) = '         VALUE1  = 1'
         TEXT(  3 ) = '         VALUE2  = 2'
         TEXT(  4 ) = '         VALUE3  = ''PI'' '
         TEXT(  5 ) = '         VALUE4  = 3'
         TEXT(  6 ) = '         VALUE5  = 4'
         TEXT(  7 ) = '         VALUE6  = 5'
         TEXT(  8 ) = '         VALUE7  = 1.276828E+11'
         TEXT(  9 ) = '         VALUE8  = -28.19729871E+12'
         TEXT( 10 ) = '         VALUE9  = @1-JAN-1994'
         TEXT( 11 ) = '         VALUE10 = ('
         TEXT( 12 ) = '                     1,'
         TEXT( 13 ) = '                     2,'
         TEXT( 14 ) = '                     3,'
         TEXT( 15 ) = '                     4,'
         TEXT( 16 ) = '                     5,'
         TEXT( 17 ) = '                     6,'
         TEXT( 18 ) = '                     7,'
         TEXT( 19 ) = '                     8 )'
         TEXT( 20 ) = '         VALUE11  = ( 1 2 )'
         TEXT( 21 ) = '         VALUE10 +=  9'
         TEXT( 22 ) = ' '
         TEXT( 23 ) = '         VALUE11 += ( 1, 0 )'
         TEXT( 24 ) = '         VALUE10 += 10'
         TEXT( 25 ) = ' '

         CALL KILFIL ( 'testdata.ker' )
         CALL TSTTXT ( 'testdata.ker', TEXT, 25, .FALSE., .TRUE.)
         CALL LDPOOL ( 'testdata.ker' )

         CALL CHCKXC ( .FALSE., ' ',  OK )

         TEXT( 20 ) = '         VALUE11  += ( )'

         CALL KILFIL ( 'testdata.ker' )
         CALL TSTTXT ( 'testdata.ker', TEXT, 25, .FALSE., .TRUE.)
         CALL LDPOOL ( 'testdata.ker' )
         CALL CHCKXC ( .TRUE., 'SPICE(BADVARASSIGN)',  OK )


!
!     Case 30
!
      CALL TCASE ( 'Check 3 for the empty vector assignment ' &
      //           'exception. ' )

         TEXT(  2 ) = '         VALUE1  = 1'
         TEXT(  3 ) = '         VALUE2  = 2'
         TEXT(  4 ) = '         VALUE3  = PI'
         TEXT(  5 ) = '         VALUE4  = 3'
         TEXT(  6 ) = '         VALUE5  = 4'
         TEXT(  7 ) = '         VALUE6  = 5'
         TEXT(  8 ) = '         VALUE7  = 1.276828E+11'
         TEXT(  9 ) = '         VALUE8  = -28.19729871E+12'
         TEXT( 10 ) = '         VALUE9  = @1-JAN-1994'
         TEXT( 11 ) = '         VALUE10 = ('
         TEXT( 12 ) = '                     1,'
         TEXT( 13 ) = '                     2,'
         TEXT( 14 ) = '                     3,'
         TEXT( 15 ) = '                     4,'
         TEXT( 16 ) = '                     5,'
         TEXT( 17 ) = '                     6,'
         TEXT( 18 ) = '                     7,'
         TEXT( 19 ) = '                     8 )'
         TEXT( 20 ) = '         VALUE11  = ( )'
         TEXT( 21 ) = '         VALUE10 +=  9'
         TEXT( 22 ) = ' '
         TEXT( 23 ) = '         VALUE11 += ( 1, 0 )'
         TEXT( 24 ) = '         VALUE10 += 10'

         CALL LMPOOL ( TEXT(2), 22 )
         CALL CHCKXC ( .TRUE., 'SPICE(BADVARASSIGN)',  OK )


!
!     Case 31
!
      CALL TCASE ( 'Check 4 for the empty vector assignment ' &
      //           'exception. ' )


         TEXT(  2 ) = '         VALUE1  = 1'
         TEXT(  3 ) = '         VALUE2  = 2'
         TEXT(  4 ) = '         VALUE3  = PI'
         TEXT(  5 ) = '         VALUE4  = 3'
         TEXT(  6 ) = '         VALUE5  = 4'
         TEXT(  7 ) = '         VALUE6  = 5'
         TEXT(  8 ) = '         VALUE7  = 1.276828E+11'
         TEXT(  9 ) = '         VALUE8  = -28.19729871E+12'
         TEXT( 10 ) = '         VALUE9  = @1-JAN-1994'
         TEXT( 11 ) = '         VALUE10 = ('
         TEXT( 12 ) = '                     1,'
         TEXT( 13 ) = '                     2,'
         TEXT( 14 ) = '                     3,'
         TEXT( 15 ) = '                     4,'
         TEXT( 16 ) = '                     5,'
         TEXT( 17 ) = '                     6,'
         TEXT( 18 ) = '                     7,'
         TEXT( 19 ) = '                     8 )'
         TEXT( 20 ) = '         VALUE11 += ( )'
         TEXT( 21 ) = '         VALUE10 +=  9'
         TEXT( 22 ) = ' '
         TEXT( 23 ) = '         VALUE11 += ( 1, 0 )'
         TEXT( 24 ) = '         VALUE10 += 10'

         CALL LMPOOL ( TEXT(2), 22 )
         CALL CHCKXC ( .TRUE., 'SPICE(BADVARASSIGN)',  OK )



!
!     Case 32
!
      CALL TCASE ( 'See if we can fetch variable names from the ' &
      //           'kernel pool via the interface GNPOOL ' )

         TEXT( 20 ) = '         VALUE11 += ( 10 )'


         CALL CLPOOL

         CVALS( 1) = 'VALUE1'
         CVALS( 2) = 'VALUE2'
         CVALS( 3) = 'VALUE3'
         CVALS( 4) = 'VALUE4'
         CVALS( 5) = 'VALUE5'
         CVALS( 6) = 'VALUE6'
         CVALS( 7) = 'VALUE7'
         CVALS( 8) = 'VALUE8'
         CVALS( 9) = 'VALUE9'
         CVALS(10) = 'VALUE10'
         CVALS(11) = 'VALUE11'

         CALL LMPOOL ( TEXT(2), 22 )
         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL GNPOOL ( '*', 1, 22, N, TEXT, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 11, 0,      OK )

         CALL SHELLC ( 11, CVALS )
         CALL SHELLC ( 11, TEXT  )

         DO I = 1, 11
            CALL CHCKSC ( 'TEXT', TEXT(I), '=', CVALS(I), OK )
         END DO

         CALL GNPOOL ( '*', 1, 5, N, TEXT, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 5,  0,      OK )


         CALL GNPOOL ( '*X*', 1, 22, N, TEXT, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 0, 0,      OK )

         CALL GNPOOL ( '%%%%%%%', 1, 22, N, TEXT, FOUND )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 2,   0,      OK )

         CVALS(1) = 'VALUE10'
         CVALS(2) = 'VALUE11'

         CALL SHELLC ( 2, CVALS )
         CALL SHELLC ( 2, TEXT  )

         DO I = 1, 2
            CALL CHCKSC ( 'TEXT', TEXT(I), '=', CVALS(I), OK )
         END DO


         CALL GNPOOL ( '%%%%%%', -1, 22, N, TEXT, FOUND )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 9,   0,      OK )

         CVALS( 1) = 'VALUE1'
         CVALS( 2) = 'VALUE2'
         CVALS( 3) = 'VALUE3'
         CVALS( 4) = 'VALUE4'
         CVALS( 5) = 'VALUE5'
         CVALS( 6) = 'VALUE6'
         CVALS( 7) = 'VALUE7'
         CVALS( 8) = 'VALUE8'
         CVALS( 9) = 'VALUE9'

         CALL SHELLC ( 9, CVALS )
         CALL SHELLC ( 9, TEXT  )

         DO I = 1, 9
            CALL CHCKSC ( 'TEXT', TEXT(I), '=', CVALS(I), OK )
         END DO


         CALL GNPOOL ( '%%%%%%', 3, 22, N, TEXT, FOUND )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 7,   0,      OK )



         CALL GNPOOL ( 'VALUE', 1, -1, N, TEXT, FOUND )
         CALL CHCKXC ( .TRUE., 'SPICE(BADARRAYSIZE)',  OK )


!
!     Case 33
!
      CALL TCASE( 'Test to see if we can remove kernel pool ' &
      //          'variables using DVPOOL. ' )


         TEXT(  2 ) = '         VALUE1  = 1'
         TEXT(  3 ) = '         VALUE2  = 2'
         TEXT(  4 ) = '         VALUE3  = PI'
         TEXT(  5 ) = '         VALUE4  = 3'
         TEXT(  6 ) = '         VALUE5  = 4'
         TEXT(  7 ) = '         VALUE6  = 5'
         TEXT(  8 ) = '         VALUE7  = 1.276828E+11'
         TEXT(  9 ) = '         VALUE8  = -28.19729871E+12'
         TEXT( 10 ) = '         VALUE9  = @1-JAN-1994'
         TEXT( 11 ) = '         VALUE10 = ('
         TEXT( 12 ) = '                     1,'
         TEXT( 13 ) = '                     2,'
         TEXT( 14 ) = '                     3,'
         TEXT( 15 ) = '                     4,'
         TEXT( 16 ) = '                     5,'
         TEXT( 17 ) = '                     6,'
         TEXT( 18 ) = '                     7,'
         TEXT( 19 ) = '                     8 )'
         TEXT( 20 ) = '         VALUE11 += ( 5 )'
         TEXT( 21 ) = '         VALUE10 +=  9'
         TEXT( 22 ) = ' '
         TEXT( 23 ) = '         VALUE11 += ( 1, 0 )'
         TEXT( 24 ) = '         VALUE10 += 10'

         CALL CLPOOL
         CALL LMPOOL ( TEXT(2), 22 )

         NBILL     = 1
         BNAMES(1) = 'VALUE1'

         NNAT      = 1
         NNAMES(1) = 'VALUE2'
         NNAMES(2) = 'VALUE3'

         CALL SWPOOL ( 'BILL', NBILL, BNAMES )
         CALL SWPOOL ( 'NAT',  NNAT,  NNAMES )

         CALL CVPOOL ( 'BILL', BUPDAT )
         CALL CVPOOL ( 'NAT',  NUPDAT )

         CALL CHCKSL ( 'BUPDAT', BUPDAT, .TRUE.,  OK )
         CALL CHCKSL ( 'NUPDAT', NUPDAT, .TRUE.,  OK )

         CALL GNPOOL ( '*', 1, 22, N, TEXT, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 11, 0,      OK )

         CALL DTPOOL ( 'VALUE1', FOUND, N, TYPE )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )



         CALL DVPOOL ( 'VALUE1' )

         CALL DTPOOL ( 'VALUE1', FOUND, N, TYPE )
         CALL CHCKSL ( 'FOUND',  FOUND, .FALSE.,  OK )


         CALL GNPOOL ( '*', 1, 22, N, TEXT, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 10, 0,      OK )

         CALL CVPOOL ( 'BILL', BUPDAT )
         CALL CVPOOL ( 'NAT',  NUPDAT )

         CALL CHCKSL ( 'BUPDAT', BUPDAT, .TRUE.,   OK )
         CALL CHCKSL ( 'NUPDAT', NUPDAT, .FALSE.,  OK )


         CVALS( 1) = 'VALUE2'
         CVALS( 2) = 'VALUE3'
         CVALS( 3) = 'VALUE4'
         CVALS( 4) = 'VALUE5'
         CVALS( 5) = 'VALUE6'
         CVALS( 6) = 'VALUE7'
         CVALS( 7) = 'VALUE8'
         CVALS( 8) = 'VALUE9'
         CVALS( 9) = 'VALUE10'
         CVALS(10) = 'VALUE11'

         CALL SHELLC ( 10, CVALS )
         CALL SHELLC ( 10, TEXT  )

         DO I = 1, 10
            CALL CHCKSC ( 'TEXT(I)', TEXT(I), '=', CVALS(I), OK )
         END DO


         CALL DTPOOL ( 'VALUE2', FOUND, N, TYPE )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )



         CALL DVPOOL ( 'VALUE2' )

         CALL DTPOOL ( 'VALUE2', FOUND, N, TYPE )
         CALL CHCKSL ( 'FOUND',  FOUND, .FALSE.,  OK )

         CALL GNPOOL ( '*', 1, 22, N, TEXT, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSI ( 'N', N, '=', 9, 0,      OK )

         CALL CVPOOL ( 'BILL', BUPDAT )
         CALL CVPOOL ( 'NAT',  NUPDAT )

         CALL CHCKSL ( 'BUPDAT', BUPDAT, .FALSE.,   OK )
         CALL CHCKSL ( 'NUPDAT', NUPDAT, .TRUE.,  OK )


         CVALS( 1) = 'VALUE3'
         CVALS( 2) = 'VALUE4'
         CVALS( 3) = 'VALUE5'
         CVALS( 4) = 'VALUE6'
         CVALS( 5) = 'VALUE7'
         CVALS( 6) = 'VALUE8'
         CVALS( 7) = 'VALUE9'
         CVALS( 8) = 'VALUE10'
         CVALS( 9) = 'VALUE11'

         CALL SHELLC ( 9, CVALS )
         CALL SHELLC ( 9, TEXT  )

         DO I = 1, 9
            CALL CHCKSC ( 'TEXT(I)', TEXT(I), '=', CVALS(I), OK )
         END DO


!
!     Case 34
!
      CALL TCASE( 'Test to see if long string values are processed ' &
      //          'correctly. ' )

         TEXT(  1 ) = BEGDAT()
         TEXT(  2 ) = 'SVALUE1  = ''Seventy-nine character lo' &
         //           'ng string ----------------------------' &
         //           '--------------89'''
         TEXT(  3 ) = 'SVALUE2  = ''Eighty character long str' &
         //           'ing ----------------------------------' &
         //           '--------------890'''
         TEXT(  4 ) = 'SVALUE3  = ''Eighty-one character long' &
         //           ' string ------------------------------' &
         //           '--------------8901'''
         TEXT(  5 ) = 'SVALUE4  = ''Seventy-nine character lo' &
         //           'ng string with middle quote ''''------' &
         //           '-----------------89'''
         TEXT(  6 ) = 'SVALUE5  = ''Seventy-nine character lo' &
         //           'ng string with closing quote ---------' &
         //           '--------------8'''''''
         TEXT(  7 ) = 'SVALUE6  = ''Eighty character long str' &
         //           'ing with middle quote ''''------------' &
         //           '-----------------890'''
         TEXT(  8 ) = 'SVALUE7  = ''Eighty character long str' &
         //           'ing with closing quote ---------------' &
         //           '--------------89'''''''
         TEXT(  9 ) = 'SVALUE8  = ''Eighty-one character long' &
         //           ' string with middle quote ''''--------' &
         //           '-----------------8901'''
         TEXT( 10 ) = 'SVALUE9  = ''Eighty-one character long' &
         //           ' string with closing quote -----------' &
         //           '--------------890'''''''
         TEXT( 11 ) = 'SVALUE10 = ''Seventy-nine character lo' &
         //           'ng string with two closing quotes ----' &
         //           '-------------7'''''''''''
         TEXT( 12 ) = 'SVALUE11 = ''Eighty character long str' &
         //           'ing with two closing quotes ----------' &
         //           '-------------78'''''''''''
         TEXT( 13 ) = 'SVALUE12 = ''Eighty-one character long' &
         //           ' string with two closing quotes ------' &
         //           '-------------789'''''''''''
         TEXT( 14 ) = 'SVALUE13 = ''Ten space-separated quote' &
         //           's string '''' '''' '''' '''' '''' ''''' &
         //           ' '''' '''' '''' '''''''

         CALL KILFIL ( 'testdata.ker' )
         CALL TSTTXT ( 'testdata.ker', TEXT, 14, .FALSE., .TRUE. )
         CALL CLPOOL
         CALL LDPOOL ( 'testdata.ker' )

         CVALS(  1 ) = 'Seventy-nine character long string --' &
         //            '-------------------------------------' &
         //            '---89'
         CVALS(  2 ) = 'Eighty character long string --------' &
         //            '-------------------------------------' &
         //            '---890'
!
!        PEPPER: string values have no length limit (SPICELIB kept 80
!        characters): the 81-character values 3, 8, 9 and 12 are whole.
!
         CVALS(  3 ) = 'Eighty-one character long string ----' &
         //            '-------------------------------------' &
         //            '---8901'
         CVALS(  4 ) = 'Seventy-nine character long string wi' &
         //            'th middle quote ''-------------------' &
         //            '----89'
         CVALS(  5 ) = 'Seventy-nine character long string wi' &
         //            'th closing quote --------------------' &
         //            '---8'''
         CVALS(  6 ) = 'Eighty character long string with mid' &
         //            'dle quote ''-------------------------' &
         //            '----890'
         CVALS(  7 ) = 'Eighty character long string with clo' &
         //            'sing quote --------------------------' &
         //            '---89'''
         CVALS(  8 ) = 'Eighty-one character long string with' &
         //            ' middle quote ''---------------------' &
         //            '----8901'
         CVALS(  9 ) = 'Eighty-one character long string with' &
         //            ' closing quote ----------------------' &
         //            '---890'''
         CVALS( 10 ) = 'Seventy-nine character long string wi' &
         //            'th two closing quotes ---------------' &
         //            '--7'''''
         CVALS( 11 ) = 'Eighty character long string with two' &
         //            ' closing quotes ---------------------' &
         //            '--78'''''
         CVALS( 12 ) = 'Eighty-one character long string with' &
         //            ' two closing quotes -----------------' &
         //            '--789'''''
         CVALS( 13 ) = 'Ten space-separated quotes string '' ' &
         //            ''' '' '' '' '' '' '' '' '''

         DO I = 1, 13

            VARNAM = 'SVALUE#'

            CALL REPMI  ( VARNAM, '#', I, VARNAM )
            CALL DTPOOL ( VARNAM, FOUND, N, TYPE )

            ITEM(1) = 'FOUND for ' // VARNAM
            ITEM(2) = 'N     for ' // VARNAM
            ITEM(3) = 'TYPE  for ' // VARNAM

            CALL CHCKXC ( .FALSE., ' ',                       OK )
            CALL CHCKSL ( ITEM(1), FOUND,     .TRUE.,         OK )
            CALL CHCKSI ( ITEM(2),     N, '=', 1, 0,          OK )
            CALL CHCKSC ( ITEM(3),  TYPE, '=', 'C',           OK )

            CALL GCPOOL ( VARNAM, 1, 1, N, CVALUE(1), FOUND )

            ITEM(1) = 'VALUE for ' // VARNAM

            CALL CHCKSC ( ITEM(1), CVALUE(1), '=', CVALS (I), OK )

         END DO


!
!     Case 35
!
      CALL TCASE( 'Test to see if long/truncated kernel file lines ' &
      //          'containing string array assignments are ' &
      //          'processed correctly. ' )

         TEXT(  2 ) = 'SVALUE1 = (                           '
         TEXT(  3 ) = '           ''short value''            '
         TEXT(  4 ) = '                                      ' &
         //           '           ''Eighty character long str' &
         //           'ing under 132-character line limit ---' &
         //           '--------------890'''
         TEXT(  5 ) = '          )                           '
         TEXT(  6 ) = 'SVALUE2 = (                           '
         TEXT(  7 ) = '           ''short value''            '
         TEXT(  8 ) = '                                      ' &
         //           '            ''Eighty character long st' &
         //           'ring at 132-character line limit (a)--' &
         //           '---------------890'''
         TEXT(  9 ) = '          )                           '
         TEXT( 10 ) = 'SVALUE3 = (                           '
         TEXT( 11 ) = '           ''short value''            '
         TEXT( 12 ) = '                                      ' &
         //           '             ''Eighty character long s' &
         //           'tring at 132-character line limit (b)-' &
         //           '----------------890'''
         TEXT( 13 ) = '          )                           '
         TEXT( 14 ) = 'SVALUE4 = (                           '
         TEXT( 15 ) = '           ''short value''            '
         TEXT( 16 ) = '                                      ' &
         //           '              ''Eighty character long ' &
         //           'string over 132-character line limit (' &
         //           'a)---------------890'''
         TEXT( 17 ) = '          )                           '
         TEXT( 18 ) = 'SVALUE5 = (                           '
         TEXT( 19 ) = '           ''short value''            '
         TEXT( 20 ) = '                                      ' &
         //           '               ''Eighty character long' &
         //           ' string over 132-character line limit ' &
         //           '(b)---------------890'''
         TEXT( 21 ) = '          )                           '
         TEXT( 22 ) = 'SVALUE6 = (                           '
         TEXT( 23 ) = '           ''short value''            '
         TEXT( 24 ) = '                                      ' &
         //           '            ''Eighty character long st' &
         //           'ring at 132-character line limit (a)--' &
         //           '---------------8'''''''
         TEXT( 25 ) = '          )                           '
         TEXT( 26 ) = 'SVALUE7 = (                           '
         TEXT( 27 ) = '           ''short value''            '
         TEXT( 28 ) = '                                      ' &
         //           '            ''Eighty character long st' &
         //           'ring at 132-character line limit (a)--' &
         //           '---------------89'''''''
         TEXT( 29 ) = '          )                           '

         CALL KILFIL ( 'testdata.ker' )
         CALL TSTTXT ( 'testdata.ker', TEXT, 29, .FALSE., .TRUE. )
         CALL CLPOOL
         CALL LDPOOL ( 'testdata.ker' )

         CVALS(  1 ) = 'Eighty character long string under 13' &
         //            '2-character line limit --------------' &
         //            '---890'
         CVALS(  2 ) = 'Eighty character long string at 132-c' &
         //            'haracter line limit (a)--------------' &
         //            '---890'
         CVALS(  3 ) = 'Eighty character long string at 132-c' &
         //            'haracter line limit (b)--------------' &
         //            '---890'
         CVALS(  4 ) = 'Eighty character long string over 132' &
         //            '-character line limit (a)------------' &
         //            '---89'
         CVALS(  5 ) = 'Eighty character long string over 132' &
         //            '-character line limit (b)------------' &
         //            '---8'
         CVALS(  6 ) = 'Eighty character long string at 132-c' &
         //            'haracter line limit (a)--------------' &
         //            '---8'''
         CVALS(  7 ) = 'Eighty character long string at 132-c' &
         //            'haracter line limit (a)--------------' &
         //            '---89'''

         DO I = 1, 7

            VARNAM = 'SVALUE#'

            CALL REPMI  ( VARNAM, '#', I, VARNAM )
            CALL DTPOOL ( VARNAM, FOUND, N, TYPE )

            ITEM(1) = 'FOUND for ' // VARNAM
            ITEM(2) = 'N     for ' // VARNAM
            ITEM(3) = 'TYPE  for ' // VARNAM

            CALL CHCKXC ( .FALSE., ' ',                       OK )
            CALL CHCKSL ( ITEM(1), FOUND,     .TRUE.,         OK )
            CALL CHCKSI ( ITEM(2),     N, '=', 2, 0,          OK )
            CALL CHCKSC ( ITEM(3),  TYPE, '=', 'C',           OK )

            CALL GCPOOL ( VARNAM, 2, 1, N, CVALUE(1), FOUND )

            ITEM(1) = 'VALUE for ' // VARNAM

            CALL CHCKSC ( ITEM(1), CVALUE(1), '=', CVALS (I), OK )

         END DO


!
!     Case 36
!
      CALL TCASE( 'Test to see if long/truncated kernel file lines ' &
      //          'containing string value assignments are ' &
      //          'processed correctly. ' )

         TEXT(  2 ) = 'SVALUE1 =                             ' &
         //           '           ''Eighty character long str' &
         //           'ing under 132-character line limit ---' &
         //           '--------------890'''
         TEXT(  3 ) = 'SVALUE2 =                             ' &
         //           '            ''Eighty character long st' &
         //           'ring at 132-character line limit (a)--' &
         //           '---------------890'''
         TEXT(  4 ) = 'SVALUE3 =                             ' &
         //           '             ''Eighty character long s' &
         //           'tring at 132-character line limit (b)-' &
         //           '----------------890'''
         TEXT(  5 ) = 'SVALUE4 =                             ' &
         //           '              ''Eighty character long ' &
         //           'string over 132-character line limit (' &
         //           'a)---------------890'''
         TEXT(  6 ) = 'SVALUE5 =                             ' &
         //           '               ''Eighty character long' &
         //           ' string over 132-character line limit ' &
         //           '(b)---------------890'''
         TEXT(  7 ) = 'SVALUE6 =                             ' &
         //           '            ''Eighty character long st' &
         //           'ring at 132-character line limit (a)--' &
         //           '---------------8'''''''
         TEXT(  8 ) = 'SVALUE7 =                             ' &
         //           '            ''Eighty character long st' &
         //           'ring at 132-character line limit (a)--' &
         //           '---------------89'''''''

         CALL KILFIL ( 'testdata.ker' )
         CALL TSTTXT ( 'testdata.ker', TEXT, 8, .FALSE., .TRUE. )
         CALL CLPOOL
         CALL LDPOOL ( 'testdata.ker' )

         CVALS(  1 ) = 'Eighty character long string under 13' &
         //            '2-character line limit --------------' &
         //            '---890'
         CVALS(  2 ) = 'Eighty character long string at 132-c' &
         //            'haracter line limit (a)--------------' &
         //            '---890'
         CVALS(  3 ) = 'Eighty character long string at 132-c' &
         //            'haracter line limit (b)--------------' &
         //            '---890'
         CVALS(  4 ) = 'Eighty character long string over 132' &
         //            '-character line limit (a)------------' &
         //            '---89'
         CVALS(  5 ) = 'Eighty character long string over 132' &
         //            '-character line limit (b)------------' &
         //            '---8'
         CVALS(  6 ) = 'Eighty character long string at 132-c' &
         //            'haracter line limit (a)--------------' &
         //            '---8'''
         CVALS(  7 ) = 'Eighty character long string at 132-c' &
         //            'haracter line limit (a)--------------' &
         //            '---89'''

         DO I = 1, 7

            VARNAM = 'SVALUE#'

            CALL REPMI  ( VARNAM, '#', I, VARNAM )
            CALL DTPOOL ( VARNAM, FOUND, N, TYPE )

            ITEM(1) = 'FOUND for ' // VARNAM
            ITEM(2) = 'N     for ' // VARNAM
            ITEM(3) = 'TYPE  for ' // VARNAM

            CALL CHCKXC ( .FALSE., ' ',                       OK )
            CALL CHCKSL ( ITEM(1), FOUND,     .TRUE.,         OK )
            CALL CHCKSI ( ITEM(2),     N, '=', 1, 0,          OK )
            CALL CHCKSC ( ITEM(3),  TYPE, '=', 'C',           OK )

            CALL GCPOOL ( VARNAM, 1, 1, N, CVALUE(1), FOUND )

            ITEM(1) = 'VALUE for ' // VARNAM

            CALL CHCKSC ( ITEM(1), CVALUE(1), '=', CVALS (I), OK )

         END DO


!
!     Case 37
!
!
!     PEPPER: names have no length limit (SPICELIB signaled
!     SPICE(BADVARNAME) for more than MAXLEN, 32, characters): cases 37
!     to 39 check that a 35-character name is stored and found.
!
      CALL TCASE( 'Kernel pool variable names longer than SPICELIB''s ' &
         //       'MAXLEN (PEPPER: no error):  PCPOOL, PIPOOL, PDPOOL ...')

         NAME     = 'ABDEF123456789012345678901234567890'
         N        = 1
         IVALS(1) = 1
         DVALS(1) = 1.D0
         CVALS(1) = 'A'
         TEXT (1) = 'ABDEF123456789012345678901234567890 = 1'


!
!        Call the PXPOOL routines using NAME as the variable name.
!
         CALL KCLEAR()

         CALL PCPOOL ( NAME, N, CVALS )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL DTPOOL ( NAME, FOUND, N, TYPE )
         CALL CHCKSL ( 'PCPOOL FOUND', FOUND, .TRUE., OK )
         CALL CHCKSC ( 'PCPOOL TYPE', TYPE, '=', 'C', OK )
         N = 1

         CALL KCLEAR()

         CALL PDPOOL ( NAME, N, DVALS )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL DTPOOL ( NAME, FOUND, N, TYPE )
         CALL CHCKSL ( 'PDPOOL FOUND', FOUND, .TRUE., OK )
         CALL CHCKSC ( 'PDPOOL TYPE', TYPE, '=', 'N', OK )
         N = 1

         CALL KCLEAR()

         CALL PIPOOL ( NAME, N, IVALS )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL DTPOOL ( NAME, FOUND, N, TYPE )
         CALL CHCKSL ( 'PIPOOL FOUND', FOUND, .TRUE., OK )


!
!     Case 38: Call LMPOOL with a string of the form NAME = value.
!

      CALL TCASE( '... LMPOOL ...' )

         CALL KCLEAR()

         CALL LMPOOL( TEXT(1), 1 )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL DTPOOL ( NAME, FOUND, N, TYPE )
         CALL CHCKSL ( 'LMPOOL FOUND', FOUND, .TRUE., OK )


!
!     Case 39: same using FURNSH.
!
      CALL TCASE( '... and FURNSH.' )

         CALL KCLEAR()

         CALL KILFIL ( 'pool_t.ker'       )
         CALL TXTOPN ( 'pool_t.ker', UNIT )

         TEXT (1) = 'KPL/FK'
         TEXT (2) = BEGDAT()
         TEXT (3) = '      ABCD123456789012345678901234567890 = 1'
         TEXT (4) = ' '

         DO I=1, 4
            CALL WRITLN ( TEXT(I), UNIT )
         END DO

         CLOSE ( UNIT )

         CALL FURNSH( 'pool_t.ker' )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL DTPOOL ( 'ABCD123456789012345678901234567890', FOUND, N, TYPE )
         CALL CHCKSL ( 'FURNSH FOUND', FOUND, .TRUE., OK )
         CALL KCLEAR()

!
!     Case 40: test POOL state counter.
!
      CALL TCASE( 'Test POOL state counter update' )

!
!     Initialize the local POOL state counter.
!
      CALL ZZCTRUIN( USRCTR )

!
!     Test initial counter update and no update on the second
!     immediate call.
!
      UPDATE = .FALSE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'initial counter UPDATE', UPDATE, .TRUE., OK )

      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no initial counter UPDATE', UPDATE, .FALSE., OK )

!
!     Test counter after CLPOOL.
!
      CALL CLPOOL

      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .FALSE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'CLPOOL counter UPDATE', UPDATE, .TRUE., OK )

!
!     Test counter after LDPOOL.
!
      TEXT(  1 ) = BEGDAT()
      TEXT(  2 ) = '         VALUE1  = 1'
      TEXT(  3 ) = '         VALUE2  = 2.E0'
      TEXT(  4 ) = '         VALUE3  = ''BOO'''
      TEXT(  5 ) = ' '

      CALL KILFIL ( 'testdata.ker' )
      CALL TSTTXT ( 'testdata.ker', TEXT, 5, .FALSE., .TRUE.)

      CALL LDPOOL ( 'testdata.ker' )

      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .FALSE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'LDPOOL counter UPDATE', UPDATE, .TRUE., OK )

!
!     Test counter after RTPOOL.
!
      CALL RTPOOL ( 'VALUE2', N, VALUES, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'N      for VALUE2', N, '=', 1, 0, OK )
      CALL CHCKSD ( 'Values for VALUE2', VALUES, '=', 2.D0, 0.D0, OK )
      CALL CHCKSL ( 'FOUND  for VALUE2', FOUND, .TRUE., OK )

      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no RTPOOL counter UPDATE', UPDATE, .FALSE., OK )

!
!     Test counter after EXPOOL.
!
      CALL EXPOOL ( 'VALUE1', FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'VALUE1', FOUND, .TRUE., OK )

      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no EXPOOL counter UPDATE', UPDATE, .FALSE., OK )

!
!     Test counter after WRPOOL.
!
      CALL KILFIL ( 'writdata.ker' )
      CALL TXTOPN ( 'writdata.ker', UNIT )
      CALL WRPOOL (  UNIT )
      CLOSE ( UNIT )

      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no WRPOOL counter UPDATE', UPDATE, .FALSE., OK )

!
!     Test counter after SWPOOL.
!
      NAMES(1) = 'VALUE1'
      CALL SWPOOL ( 'COUNTER', 1, NAMES )

      CALL CHCKXC ( .FALSE., ' ', OK )

!     PEPPER: watches are kept by each context, not in the pool, so
!     SWPOOL does not change the pool's state counter.
!
      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no SWPOOL counter UPDATE', UPDATE, .FALSE., OK )

!
!     Test counter after CVPOOL.
!
      CALL CVPOOL ( 'COUNTER', FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'UPDATE for COUNTER watcher', FOUND, .TRUE., OK )

      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no CVPOOL counter UPDATE', UPDATE, .FALSE., OK )

!
!     Test counter after GCPOOL.
!
      CALL GCPOOL ( 'VALUE3', 1, 1, N, CVALUE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND for VALUE3', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'N     for VALUE3', N, '=', 1, 0, OK )
      CALL CHCKSC ( 'Value for VALUE3', CVALUE(1), '=', 'BOO', OK )

      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no GCPOOL counter UPDATE', UPDATE, .FALSE., OK )

!
!     Test counter after GDPOOL.
!
      CALL GDPOOL ( 'VALUE2', 1, 1, N, VALUES, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND for VALUE2', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'N     for VALUE2', N, '=', 1, 0, OK )
      CALL CHCKSD ( 'Value for VALUE2', VALUES, '=', 2.D0, 0.D0, OK )

      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no GDPOOL counter UPDATE', UPDATE, .FALSE., OK )

!
!     Test counter after GIPOOL.
!
      CALL GIPOOL ( 'VALUE1', 1, 1, N, INTVAL, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND for VALUE1', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'N     for VALUE1', N, '=', 1, 0, OK )
      CALL CHCKSI ( 'Value for VALUE1', INTVAL, '=', 1, 0, OK )

      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no GIPOOL counter UPDATE', UPDATE, .FALSE., OK )

!
!     Test counter after DTPOOL.
!
      CALL DTPOOL ( 'VALUE1', FOUND, N, TYPE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND for VALUE1', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'N     for VALUE1', N, '=', 1, 0, OK )
      CALL CHCKSC ( 'TYPE  for VALUE1', TYPE, '=', 'N', OK )

      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no DTPOOL counter UPDATE', UPDATE, .FALSE., OK )

!
!     Test counter after PCPOOL.
!
      MYSTRS ( 1 ) = 'String 1'

      CALL PCPOOL ( 'MYSTRS', 1, MYSTRS )

      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .FALSE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'PCPOOL counter UPDATE', UPDATE, .TRUE., OK )

!
!     Test counter after PDPOOL.
!
      MYDATA(1) = 1.0D0

      CALL PDPOOL ( 'MYDPS', 1, MYDATA )

      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .FALSE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'PDPOOL counter UPDATE', UPDATE, .TRUE., OK )

!
!     Test counter after PIPOOL.
!
      MYINTS(1) = 5

      CALL PIPOOL ( 'MYINTS', 1, MYINTS )

      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .FALSE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'PIPOOL counter UPDATE', UPDATE, .TRUE., OK )

!
!     Test counter after LMPOOL.
!
      TEXT(  1 ) = '         VALUE4  = 11'
      TEXT(  2 ) = '         VALUE5  = 22.E0'
      TEXT(  3 ) = '         VALUE6  = ''BOOBOO'''

      CALL LMPOOL ( TEXT, 3 )

      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .FALSE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'LMPOOL counter UPDATE', UPDATE, .TRUE., OK )

!
!     Test counter after SZPOOL
!
      CALL SZPOOL ( 'MAXVAR', N, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'N', N, '>', 0, 0, OK )

      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no SZPOOL counter UPDATE', UPDATE, .FALSE., OK )

!
!     Test counter after DVPOOL
!
      CALL DVPOOL ( 'VALUE6' )

      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .FALSE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'DVPOOL counter UPDATE', UPDATE, .TRUE., OK )

!
!     Test counter after GNPOOL
!
      CALL GNPOOL ( '*', 1, 5, N, TEXT, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'N', N, '=', 5, 0, OK )

      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no GNPOOL counter UPDATE', UPDATE, .FALSE., OK )

!
!     Test counter after DWPOOL
!
      CALL DWPOOL ( 'COUNTER' )

      CALL CHCKXC ( .FALSE., ' ', OK )

!     PEPPER: watches are kept by each context, not in the pool, so
!     DWPOOL does not change the pool's state counter.
!
      UPDATE = .TRUE.
      CALL ZZPCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no DWPOOL counter UPDATE', UPDATE, .FALSE., OK )


!
!     Case 41: test POOL state counter.
!
      CALL TCASE ( 'Make sure we can set watchers and check them ' &
      //           'using ZZCVPOOL instead of CVPOOL. ' )

!
!     Initialize the local POOL state counter.
!
      CALL ZZCTRUIN( USRCT1 )
      CALL ZZCTRUIN( USRCT2 )
      CALL ZZCTRUIN( USRCT3 )
      CALL ZZCTRUIN( USRCT4 )

!
!     Create a list of variables to be watched.
!
      NAMES(1) = 'SVALUE8'
      NAMES(2) = 'VALUE10'
      NAMES(3) = 'SPUD'

!
!     Establish a watcher on the string variable and on the
!     numeric variable.
!
      CALL SWPOOL ( 'S_POOL', 1, NAMES    )
      CALL SWPOOL ( 'D_POOL', 1, NAMES(2) )
      CALL SWPOOL ( 'DUMMY',  1, NAMES(3) )

!
!     Check to see that S_POOL, D_POOL and DUMMY have notifications
!     pending.
!
      CALL ZZCVPOOL ( 'S_POOL', USRCT1,    UPDATE   )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .TRUE.,  OK )

      CALL ZZCVPOOL ( 'D_POOL', USRCT2,    UPDATE   )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for D_POOL', UPDATE, .TRUE.,  OK )

      CALL ZZCVPOOL ( 'DUMMY', USRCT3,     UPDATE   )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for DUMMY',  UPDATE, .TRUE.,  OK )

!
!     Now check that S_POOL, D_POOL and DUMMY do not have
!     notifications pending.
!
      CALL ZZCVPOOL ( 'S_POOL', USRCT1, UPDATE )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .FALSE., OK )

      CALL ZZCVPOOL ( 'D_POOL', USRCT2, UPDATE )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .FALSE., OK )

      CALL ZZCVPOOL ( 'DUMMY', USRCT3,     UPDATE   )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for DUMMY',  UPDATE, .FALSE., OK )

      CALL ZZCVPOOL ( 'UNDECLAGENT', USRCT4, UPDATE )
      CALL CHCKXC ( .FALSE.,        ' ',            OK )
      CALL CHCKSL ( 'UNDECLAGENT', UPDATE, .FALSE., OK )

!
!     Clear the kernel pool and see if everything behaves as
!     expected.
!
      CALL CLPOOL

      CALL TSTMSG ( '#', 'Same tests after clearing the pool.' )

!
!     Check to see that S_POOL, D_POOL and DUMMY have notifications
!     pending.
!
      CALL ZZCVPOOL ( 'S_POOL', USRCT1,    UPDATE   )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .TRUE.,  OK )

      CALL ZZCVPOOL ( 'D_POOL', USRCT2,    UPDATE   )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for D_POOL', UPDATE, .TRUE.,  OK )

      CALL ZZCVPOOL ( 'DUMMY', USRCT3,     UPDATE   )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for DUMMY',  UPDATE, .TRUE.,  OK )

!
!     Now check that S_POOL, D_POOL and DUMMY do not have
!     notifications pending.
!
      CALL ZZCVPOOL ( 'S_POOL', USRCT1, UPDATE )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .FALSE., OK )

      CALL ZZCVPOOL ( 'D_POOL', USRCT2, UPDATE )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .FALSE., OK )

      CALL ZZCVPOOL ( 'DUMMY', USRCT3,     UPDATE   )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for DUMMY',  UPDATE, .FALSE., OK )

      CALL ZZCVPOOL ( 'UNDECLAGENT', USRCT4, UPDATE )
      CALL CHCKXC ( .FALSE.,        ' ',            OK )
      CALL CHCKSL ( 'UNDECLAGENT', UPDATE, .FALSE., OK )


!
!     Now load a kernel and see if these items get updated
!     as expected
!
      CALL LDPOOL ( 'write.dat' )
      CALL TSTMSG ( '#', 'Same watcher tests after loading a file.' )

      CALL ZZCVPOOL ( 'S_POOL', USRCT1,    UPDATE   )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .TRUE.,  OK )

      CALL ZZCVPOOL ( 'D_POOL', USRCT2,    UPDATE   )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for D_POOL', UPDATE, .TRUE.,  OK )

      CALL ZZCVPOOL ( 'DUMMY', USRCT3,     UPDATE   )

      CALL CHCKXC ( .FALSE.,        ' ',                   OK )
      CALL CHCKSL ( 'UPDATE for DUMMY',  UPDATE, .FALSE.,  OK )

!
!     Now check that S_POOL, D_POOL and DUMMY do not have
!     notifications pending.
!
      CALL ZZCVPOOL ( 'S_POOL', USRCT1, UPDATE )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .FALSE., OK )

      CALL ZZCVPOOL ( 'D_POOL', USRCT2, UPDATE )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for S_POOL', UPDATE, .FALSE., OK )

      CALL ZZCVPOOL ( 'DUMMY', USRCT3,     UPDATE   )

      CALL CHCKXC ( .FALSE.,        ' ',                  OK )
      CALL CHCKSL ( 'UPDATE for DUMMY',  UPDATE, .FALSE., OK )

      CALL ZZCVPOOL ( 'UNDECLAGENT', USRCT4, UPDATE )
      CALL CHCKXC ( .FALSE.,        ' ',            OK )
      CALL CHCKSL ( 'UNDECLAGENT', UPDATE, .FALSE., OK )
      CALL TSTMSG ( '#', ' ' )

!
!     Delete watchers used in this test.
!
      CALL DWPOOL ( 'S_POOL' )
      CALL DWPOOL ( 'D_POOL' )
      CALL DWPOOL ( 'DUMMY'  )

!
!     Unload everything, clean the kernel pool.
!
      CALL KCLEAR()

      CALL KILFIL ( 'pool_t.ker' )
      CALL KILFIL ( 'testdata.ker' )
      CALL KILFIL ( 'writdata.ker' )
      CALL KILFIL ( 'write.dat' )

      CALL T_SUCCESS ( OK )
      RETURN

      END
