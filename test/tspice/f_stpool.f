C$Procedure      F_STPOOL ( Family of tests for STPOOL )
 
      SUBROUTINE F_STPOOL ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine performs rudimentary tests on the routine STPOOL.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C-&
 
 
C
C     Test Utility Functions
C
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(WDSIZE)    BEGDAT
 
C
C     SPICELIB Functions
C
      INTEGER               RTRIM
 
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 11 )
 
      CHARACTER*(LNSIZE)    TEXT ( TXTSIZ )
 
      INTEGER               LONGSZ
      PARAMETER           ( LONGSZ = 3000 )
 
      CHARACTER*(LONGSZ)    EXPECT ( 4 )
      CHARACTER*(LONGSZ)    MYSTR
      CHARACTER*(LONGSZ)    ESTRNG
 
      CHARACTER*(1)         TYPE
 
 
      INTEGER               ESIZE
      INTEGER               I
      INTEGER               N
      INTEGER               SIZE
      INTEGER               GOTBCK
 
      LOGICAL               FOUND
      LOGICAL               GCFND
      LOGICAL               STFND
 
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_STPOOL' )
 
 
 
      TEXT(1) = BEGDAT()
 
      TEXT(  2 ) = ' MYSTRING = ( ''This is a long string tha'
     .//           't will be spread -'','
      TEXT(  3 ) = '              ''across several lines of a'
     .//           ' spice text kernel -'','
      TEXT(  4 ) = '              ''for the sake of testing S'
     .//           'TPOOL.'','
      TEXT(  5 ) = '              ''This is a second string.'''
     .//           ','
      TEXT(  6 ) = '              ''This is a third long stri'
     .//           'ng that will span -'','
      TEXT(  7 ) = '              ''more than one line in a t'
     .//           'ext kernel.'','
      TEXT(  8 ) = '              ''Finally this is a last lo'
     .//           'ng string that will -'''
      TEXT(  9 ) = '              ''be more than one line in '
     .//           'the text kernel so -'''
      TEXT( 10 ) = '              ''that we can test the fetc'
     .//           'hing of long strings -'','
      TEXT( 11 ) = '              ''from the kernel pool with'
     .//           ' STPOOL.'' )'
 
 
      EXPECT ( 1 ) = 'This is a long string that will be spread '
     .//             'across several lines of a spice text '
     .//             'kernel for the sake of testing STPOOL. '
 
      EXPECT ( 2 ) = 'This is a second string. '
 
      EXPECT ( 3 ) = 'This is a third long string that will span '
     .//             'more than one line in a text kernel. '
 
      EXPECT ( 4 ) = 'Finally this is a last long string that '
     .//             'will be more than one line in the text '
     .//             'kernel so that we can test the fetching of '
     .//             'long strings from the kernel pool with '
     .//             'STPOOL. '
 
 
      CALL KILFIL ( 'sample.txt' )
      CALL TSTTXT ( 'sample.txt', TEXT, TXTSIZ, .TRUE., .FALSE. )
 
      CALL TCASE ( 'Try to look up a string that isn''t there.' )
 
         MYSTR = 'xxx'
         CALL STPOOL ( 'MYITEM', 1, '-', MYSTR, SIZE, FOUND )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE,   '=', 0, 0, OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )
 
      CALL TCASE ( 'See if we can get the loaded strings.' )
 
         MYSTR = 'xxx'
 
         ESIZE = RTRIM(EXPECT(1))
 
         CALL STPOOL ( 'MYSTRING', 1, '-', MYSTR, SIZE, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', ESIZE, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', EXPECT(1), OK )
 
         ESIZE = RTRIM(EXPECT(2))
 
         CALL STPOOL ( 'MYSTRING', 2, '-', MYSTR, SIZE, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', ESIZE, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', EXPECT(2), OK )
 
         ESIZE = RTRIM(EXPECT(3))
 
         CALL STPOOL ( 'MYSTRING', 3, '-', MYSTR, SIZE, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', ESIZE, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', EXPECT(3), OK )
 
         ESIZE = RTRIM(EXPECT(4))
 
         CALL STPOOL ( 'MYSTRING', 4, '-', MYSTR, SIZE, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', ESIZE, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', EXPECT(4), OK )
 
         CALL STPOOL ( 'MYSTRING', 5, '-', MYSTR, SIZE, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', 0, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', ' ', OK )
 
         CALL STPOOL ( 'MYSTRING', 0, '-', MYSTR, SIZE, FOUND )
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'SIZE', SIZE, '=', 0, 0, OK )
         CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
         CALL CHCKSC ( 'MYSTR', MYSTR, '=', ' ', OK )
 
 
      CALL TCASE ( 'Check for compatibility with GCPOOL when '
     .//           'there continuation symbol used is not the '
     .//           'one specified in STPOOL. ' )
 
 
         CALL DTPOOL ( 'MYSTRING', FOUND, N, TYPE )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
 
         DO I = 1, N
 
            CALL STPOOL ( 'MYSTRING', I, '*', MYSTR, SIZE,     STFND )
            CALL GCPOOL ( 'MYSTRING',  I, 1,   GOTBCK, ESTRNG, GCFND )
 
            CALL CHCKSL ( 'GCFND', GCFND, .TRUE., OK )
            CALL CHCKSL ( 'STFND', STFND,  GCFND, OK )
            CALL CHCKSC ( 'MYSTR', MYSTR, '=', ESTRNG, OK )
 
            ESIZE = RTRIM(ESTRNG)
            CALL CHCKSI ( 'SIZE', SIZE, '=', ESIZE, 0, OK )
 
         END DO
 
         CALL STPOOL ( 'MYSTRING', N+1, '*', MYSTR,  SIZE,   STFND )
         CALL CHCKSL ( 'STFND', STFND,  .FALSE.,   OK )
         CALL CHCKSC ( 'MYSTR', MYSTR,  '=',  ' ', OK )
         CALL CHCKSI ( 'SIZE', SIZE,    '=', 0, 0, OK )
 
 
 
 
 
 
C
C     Cleanup any debris we may have left around.
C
      CALL CLPOOL
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
