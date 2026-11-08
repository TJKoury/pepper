C$Procedure      F_UTC2ET ( A family of tests for UTC2ET )
 
      SUBROUTINE F_UTC2ET ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine uses a fixed leapseconds kernel (hard coded
C     in this routine and tests the routine UTC2ET against
C     fixed values
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
 
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               I
      INTEGER               NLINES
 
      CHARACTER*(LNSIZE)    TSTRNG ( 22  )
      CHARACTER*(LNSIZE)    KERNEL ( 23  )
 
      DOUBLE PRECISION      EXPDET ( 22  )
      DOUBLE PRECISION      ET
 
 
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_UTC2ET' )
 
      KERNEL(  1 ) = BEGDAT()
      KERNEL(  2 ) = ' '
      KERNEL(  3 ) = 'DELTET/DELTA_T_A       =   32.184'
      KERNEL(  4 ) = 'DELTET/K               =    1.657D-3'
      KERNEL(  5 ) = 'DELTET/EB              =    1.671D-2'
      KERNEL(  6 ) = 'DELTET/M               = (  6.239996D0 '
     .//             '  1.99096871D-7 )'
      KERNEL(  7 ) = ' '
      KERNEL(  8 ) = 'DELTET/DELTA_AT        = ( 10,   @1972-'
     .//             'JAN-1'
      KERNEL(  9 ) = '                           11,   @1972-'
     .//             'JUL-1'
      KERNEL( 10 ) = '                           12,   @1973-'
     .//             'JAN-1'
      KERNEL( 11 ) = '                           13,   @1974-'
     .//             'JAN-1'
      KERNEL( 12 ) = '                           14,   @1975-'
     .//             'JAN-1'
      KERNEL( 13 ) = '                           15,   @1976-'
     .//             'JAN-1'
      KERNEL( 14 ) = '                           16,   @1977-'
     .//             'JAN-1'
      KERNEL( 15 ) = '                           17,   @1978-'
     .//             'JAN-1'
      KERNEL( 16 ) = '                           18,   @1979-'
     .//             'JAN-1'
      KERNEL( 17 ) = '                           19,   @1980-'
     .//             'JAN-1'
      KERNEL( 18 ) = '                           20,   @1981-'
     .//             'JUL-1'
      KERNEL( 19 ) = '                           21,   @1982-'
     .//             'JUL-1'
      KERNEL( 20 ) = '                           22,   @1983-'
     .//             'JUL-1'
      KERNEL( 21 ) = '                           23,   @1985-'
     .//             'JUL-1'
      KERNEL( 22 ) = '                           24,   @1988-'
     .//             'JAN-1'
      KERNEL( 23 ) = '                           25,   @1990-'
     .//             'JAN-1 )'
 
      NLINES = 23
 
      CALL CLPOOL
      CALL TSTTXT ( 'testleap.ker', KERNEL, NLINES, .TRUE., .FALSE. )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL TCASE ( 'Check that an exception is generated if an '
     .//           'A.M. or P.M. is specified in the time '
     .//           'string. ' )
 
         TSTRNG(1) = '1994 JAN 12 8:43 A.M.'
 
         CALL UTC2ET ( TSTRNG(1), ET )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTIMESTRING)', OK )
 
 
      CALL TCASE ( 'Check that an exception is generated if a '
     .//           'time zone is included as part of the string. ')
 
         TSTRNG(1) = '1994 JAN 12 8:43 PDT'
 
         CALL UTC2ET ( TSTRNG(1), ET )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTIMESTRING)', OK )
 
      CALL TCASE ( 'Check that an exception is generated if a '
     .//           'time system is included in the time string. ')
 
         TSTRNG(1) = '1994 JAN 12 8:43 TDT'
 
         CALL UTC2ET ( TSTRNG(1), ET )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTIMESTRING)', OK )
 
      CALL TCASE ( 'Make sure that when checking is enabled that '
     .//           'an exception is generated when a component '
     .//           'of the time string is out of range. ' )
 
 
         TSTRNG(1) = '1994 JAN 32 08:43:12'
 
         CALL TPARCH ( 'YES' )
         CALL UTC2ET ( TSTRNG(1), ET )
         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTIMESTRING)', OK )
 
      CALL TCASE ( 'Check UTC2ET against a collection of times '
     .//           'for which the outcome has been computed and '
     .//           'hardcoded in advance. ' )
 
 
 
         TSTRNG (  1  ) = ' 1/9/1986 3:12:59.2'
         EXPDET (  1  ) =  -441103565.6158334016800D+00
 
         TSTRNG (  2  ) = '9 JAN 1986 03:12:59.2'
         EXPDET (  2  ) =  -441103565.6158334016800D+00
 
         TSTRNG (  3  ) = '1 9 1986 3:12:59.2'
         EXPDET (  3  ) =  -441103565.6158334016800D+00
 
         TSTRNG (  4  ) = '9 JAN 1986 03:12:59.2'
         EXPDET (  4  ) =  -441103565.6158334016800D+00
 
         TSTRNG (  5  ) = '2 jan 1991 3:00:12.2'
         EXPDET (  5  ) =  -283942730.6160448789597D+00
 
         TSTRNG (  6  ) = '2 JAN 1991 03:00:12.2'
         EXPDET (  6  ) =  -283942730.6160448789597D+00
 
         TSTRNG (  7  ) = '1991 MAR 10 12:00:00'
         EXPDET (  7  ) =  -278121542.8144892454147D+00
 
         TSTRNG (  8  ) = '10 MAR 1991 12:00:00'
         EXPDET (  8  ) =  -278121542.8144892454147D+00
 
         TSTRNG (  9  ) = '1 March 1975 3:00'
         EXPDET (  9  ) =  -783853153.8146169185638D+00
 
         TSTRNG ( 10  ) = '1 MAR 1975 03:00:00'
         EXPDET ( 10  ) =  -783853153.8146169185638D+00
 
         TSTRNG ( 11  ) = '2010 October 29 3:58'
         EXPDET ( 11  ) =   341596737.1824791431427D+00
 
         TSTRNG ( 12  ) = '29 OCT 2010 03:58:00'
         EXPDET ( 12  ) =   341596737.1824791431427D+00
 
         TSTRNG ( 13  ) = 'dec 31 86 12'
         EXPDET ( 13  ) =  -410313544.8160908222198D+00
 
         TSTRNG ( 14  ) = '31 DEC 1986 12:00:00'
         EXPDET ( 14  ) =  -410313544.8160908222198D+00
 
         TSTRNG ( 15  ) = '86-365 // 12:00'
         EXPDET ( 15  ) =  -410313544.8160908222198D+00
 
         TSTRNG ( 16  ) = '31 DEC 1986 12:00:00'
         EXPDET ( 16  ) =  -410313544.8160908222198D+00
 
         TSTRNG ( 17  ) = 'JD 2451545.'
         EXPDET ( 17  ) =  57.1839272823238D+00
 
         TSTRNG ( 18  ) = '1 JAN 2000 12:00:00'
         EXPDET ( 18  ) =  57.1839272823238D+00
 
         TSTRNG ( 19  ) = 'jd 2451545.'
         EXPDET ( 19  ) =  57.1839272823238D+00
 
         TSTRNG ( 20  ) = '1 JAN 2000 12:00:00'
         EXPDET ( 20  ) =  57.1839272823238D+00
 
         TSTRNG ( 21  ) = 'JD2451545.'
         EXPDET ( 21  ) =  57.1839272823238D+00
 
         TSTRNG ( 22  ) = '1 JAN 2000 12:00:00'
         EXPDET ( 22  ) =  57.1839272823238D+00
 
 
 
         DO I = 1, 22
 
            CALL TSTMSG ( '#', 'Test subcase #.' )
            CALL TSTMSI (  I )
 
            CALL UTC2ET ( TSTRNG(I), ET )
            CALL CHCKXC ( .FALSE., ' ', OK )
            IF ( I .LT. 17 ) THEN
               CALL CHCKSD ( 'ET', ET, '~/', EXPDET(I), 1.0D-13, OK )
            ELSE
               CALL CHCKSD ( 'ET', ET, '~/', EXPDET(I), 1.0D-11, OK )
            END IF
 
         END DO
 
      CALL TCASE ( 'Step accross a leapsecond making sure that '
     .//           'ET has the expected behavior. ' )
 
 
         TSTRNG (  1  ) = '31 DEC 1989 23:59:55.01'
         EXPDET (  1  ) =  -315575948.8060699105263D+00
 
         TSTRNG (  2  ) = '31 DEC 1989 23:59:56.02'
         EXPDET (  2  ) =  -315575947.7960699200630D+00
 
         TSTRNG (  3  ) = '31 DEC 1989 23:59:57.03'
         EXPDET (  3  ) =  -315575946.7860699295998D+00
 
         TSTRNG (  4  ) = '31 DEC 1989 23:59:58.04'
         EXPDET (  4  ) =  -315575945.7760698795319D+00
 
         TSTRNG (  5  ) = '31 DEC 1989 23:59:59.05'
         EXPDET (  5  ) =  -315575944.7660698890686D+00
 
         TSTRNG (  6  ) = '31 DEC 1989 23:59:60.06'
         EXPDET (  6  ) =  -315575943.7560698986053D+00
 
         TSTRNG (  7  ) = '1  JAN 1990 00:00:00.07'
         EXPDET (  7  ) =  -315575942.7460699081421D+00
 
         TSTRNG (  8  ) = '1  JAN 1990 00:00:01.08'
         EXPDET (  8  ) =  -315575941.7360699176788D+00
 
 
         DO I = 1, 8
 
            CALL TSTMSG ( '#', 'Test subcase #.' )
            CALL TSTMSI (  I )
 
            CALL UTC2ET ( TSTRNG(I), ET )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL CHCKSD ( 'ET', ET, '~/', EXPDET(I), 1.0D-13, OK )
 
         END DO
 
 
      CALL TPARCH    ( 'NO' )
      CALL T_SUCCESS (  OK  )
      RETURN
      END
