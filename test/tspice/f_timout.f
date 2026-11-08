C$Procedure      F_TIMOUT ( Family of tests for TIMOUT )
 
      SUBROUTINE F_TIMOUT ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine runs a number of test formats through
C     the routine TIMOUT to compare expected output against
C     actual outputs
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
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      J1950

C
C     Local parameters
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               LONGSZ
      PARAMETER           ( LONGSZ = 240 )
 
C
C     Local Variables
C 
      CHARACTER*(LNSIZE)    CASE
      CHARACTER*(LONGSZ)    ERROR
      CHARACTER*(LNSIZE)    GOODKR ( 23 )
      CHARACTER*(WDSIZE)    MODIFY ( 5 )
      CHARACTER*(LNSIZE)    PICTUR
      CHARACTER*(LNSIZE)    STRING
      CHARACTER*(WDSIZE)    TYPE

      DOUBLE PRECISION      HOFF
      DOUBLE PRECISION      MOFF
      DOUBLE PRECISION      TVEC   ( 10 )

      INTEGER               LAST
      INTEGER               NLINES
      INTEGER               NTVEC
      INTEGER               YEAR
      LOGICAL               MODS
      LOGICAL               SUCCES
      LOGICAL               YABBRV
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_TIMOUT' )
C
C     The next sequence of test cases use TPARTV to parse a
C     string and then call TIMOUT to see if we can duplicate
C     the the input to TPARTV.
C
C     We will need a leapsecond kernel to do this.
C
         GOODKR(  1 ) = BEGDAT()
         GOODKR(  2 ) = ' '
         GOODKR(  3 ) = 'DELTET/DELTA_T_A       =   32.184'
         GOODKR(  4 ) = 'DELTET/K               =    1.657D-3'
         GOODKR(  5 ) = 'DELTET/EB              =    1.671D-2'
         GOODKR(  6 ) = 'DELTET/M               = (  6.239996D0 '
     .   //             '  1.99096871D-7 )'
         GOODKR(  7 ) = ' '
         GOODKR(  8 ) = 'DELTET/DELTA_AT        = ( 10,   @1972-'
     .   //             'JAN-1'
         GOODKR(  9 ) = '                           11,   @1972-'
     .   //             'JUL-1'
         GOODKR( 10 ) = '                           12,   @1973-'
     .   //             'JAN-1'
         GOODKR( 11 ) = '                           13,   @1974-'
     .   //             'JAN-1'
         GOODKR( 12 ) = '                           14,   @1975-'
     .   //             'JAN-1'
         GOODKR( 13 ) = '                           15,   @1976-'
     .   //             'JAN-1'
         GOODKR( 14 ) = '                           16,   @1977-'
     .   //             'JAN-1'
         GOODKR( 15 ) = '                           17,   @1978-'
     .   //             'JAN-1'
         GOODKR( 16 ) = '                           18,   @1979-'
     .   //             'JAN-1'
         GOODKR( 17 ) = '                           19,   @1980-'
     .   //             'JAN-1'
         GOODKR( 18 ) = '                           20,   @1981-'
     .   //             'JUL-1'
         GOODKR( 19 ) = '                           21,   @1982-'
     .   //             'JUL-1'
         GOODKR( 20 ) = '                           22,   @1983-'
     .   //             'JUL-1'
         GOODKR( 21 ) = '                           23,   @1985-'
     .   //             'JUL-1'
         GOODKR( 22 ) = '                           24,   @1988-'
     .   //             'JAN-1'
         GOODKR( 23 ) = '                           25,   @1990-'
     .   //             'JAN-1 )'
 
 
         NLINES = 23
 
         CALL CLPOOL
         CALL TSTTXT ( 'testleap.ker', GOODKR, NLINES, .TRUE., .FALSE. )
 
      CASE = '1996 JAN 12 13:00:15.0 '
      CALL TCASE ( CASE )
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         CALL TIMOUT ( TVEC(1), PICTUR,          STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
 
 
      CASE = '1996 JAN 12 02:00:15.0 A.M.'
      CALL TCASE ( CASE )
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         CALL TTRANS ( TYPE, 'ET',               TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         CALL TIMOUT ( TVEC(1), PICTUR,          STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
 
      CASE = '13 APR 1996 11:12:11.0'
      CALL TCASE ( CASE )
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         CALL TIMOUT ( TVEC(1), PICTUR,          STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
      CASE = '1996-122/ 13:00:15.0 '
      CALL TCASE ( CASE )
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         CALL TIMOUT ( TVEC(1), PICTUR,          STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
      CASE = '1996-03-17T15:12:18.9 '
      CALL TCASE ( CASE )
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         CALL TIMOUT ( TVEC(1), PICTUR,          STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
      CASE = 'Mon Jun 17 10:45:26 PDT 1996'
      CALL TCASE ( CASE )
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
         CALL PREFIX ( '::',    0,       MODIFY(3)      )
 
         CALL ZZUTCPM ( MODIFY(3), 1, HOFF, MOFF, LAST, SUCCES )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         TVEC(4) = TVEC(4) - HOFF
         TVEC(5) = TVEC(5) - MOFF
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         CALL TIMOUT ( TVEC(1), PICTUR,          STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
      CASE = 'Wednesday June 19, 01:12:29.19 P.M. 1996'
      CALL TCASE ( CASE )
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         TVEC(4) = TVEC(4) + 12.0D0
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         PICTUR = 'Weekday Month DD, AP:MN:SC.## AMPM YYYY ::RND'
 
         CALL TIMOUT ( TVEC(1), PICTUR,          STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
 
      CASE = 'Wednesday June 19, 01:12:29.19 1996 '
     .//     '(From Gregorian to Julian)'
      CALL TCASE ( CASE )
 
         CASE = 'Wednesday June 19, 01:12:29.19 1996'
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
         CASE = 'Wednesday June 06, 01:12:29.19 1996 (Julian)'
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         PICTUR = 'Weekday Month DD, HR:MN:SC.## YYYY '
     .   //       '(Julian)::RND::JCAL'
 
         CALL TIMOUT ( TVEC(1), PICTUR,          STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
      CASE = 'Mon Jun 17 10:45:26 PDT ''96'
      CALL TCASE ( CASE )
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
         YEAR = TVEC(1)
         CALL TEXPYR ( YEAR )
         TVEC(1) = YEAR
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
         CALL PREFIX ( '::',    0,       MODIFY(3)      )
 
         CALL ZZUTCPM ( MODIFY(3), 1, HOFF, MOFF, LAST, SUCCES )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         TVEC(4) = TVEC(4) - HOFF
         TVEC(5) = TVEC(5) - MOFF
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         CALL TIMOUT ( TVEC(1), PICTUR,          STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
 
      CALL TCASE ( 'Check to make sure time zones have their '
     .//           'leapseconds at the right time. ' )
 
         CASE = '1989 DEC 31 23:59:60.5 '
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         PICTUR = 'YYYY MON DD HR:MN:SC.# PDT ::RND::UTC-7'
         CASE   = '1989 DEC 31 16:59:60.5 PDT'
 
         CALL TIMOUT ( TVEC(1), PICTUR, STRING )
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
 
      CASE = 'Oct 12, 1492 A.D.'
      CALL TCASE ( CASE )
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         PICTUR = 'Mon DD, YYYY ERA ::JCAL '
 
         CASE   = 'Oct 03, 1492 A.D.'
         CALL TIMOUT ( TVEC(1), PICTUR, STRING )
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
      CASE = '1995 MAY 13 12:28:29.281 TDB '
      CALL TCASE  ( CASE )
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         PICTUR = 'YYYY MON DD HR:MN:SC.### ::TDB::TRNC'
 
         CALL ETCAL  ( TVEC(1), CASE )
         CALL TIMOUT ( TVEC(1), PICTUR, STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
      CALL TCASE ( '-10000000.123 seconds past J2000.' )
 
         TVEC(1) = -10000000.123D0
 
         CASE    = '  -10000000.123'
 
         PICTUR  = 'SP2000.### ::RND::TDB'
 
         CALL TIMOUT ( TVEC(1), PICTUR, STRING )
 
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
 
      CASE    = '  123456789.123 '
      CALL TCASE ( '123456789.123 seconds past 1950' )
 
         TVEC(1) = SPD()*(J1950() - J2000() ) + 123456789.123D0
 
 
         PICTUR  = 'SP1950.### ::RND::TDB'
 
         CALL TIMOUT ( TVEC(1), PICTUR, STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
      CASE = ' 321 B.C. MAR 15 12:00:00'
      CALL TCASE ( 'The ephemeris epoch of 321 B.C. MAR 15 12:00:00' )
 
         TVEC(1) =  -86400.0D0 * ( 847362.0D0  - 74.0D0 )
         PICTUR  = 'YYYY ERA MON DD HR:MN:SC ::TDB'
 
         CALL TIMOUT ( TVEC(1), PICTUR, STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
      CASE = ' 321 B.C. MAR 15 12:00:00'
      CALL TCASE ( 'The ephemeris epoch of 321 B.C. MAR 15 12:00:00' )
 
         TVEC(1) =  -86400.0D0 * ( 847362.0D0  - 74.0D0 )
         PICTUR  = 'YYYY ERA MON DD HR:MN:SC ::TDB'
 
         CALL TIMOUT ( TVEC(1), PICTUR, STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
      CASE = ' 321 B.C. MAR 15 12:00:00'
      CALL TCASE ( 'The ephemeris epoch of 321 B.C. MAR 15 12:00:00' )
 
         TVEC(1) =  -86400.0D0 * ( 847362.0D0  - 74.0D0 )
         PICTUR  = 'YYYY?ERA?MON DD HR:MN:SC ::TDB'
 
         CALL TIMOUT ( TVEC(1), PICTUR, STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
      CASE = '1998 MAY 13 12:28:29.281 TDB '
      CALL TCASE  ( CASE )
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         PICTUR = 'YYYY?ERA?MON DD HR:MN:SC.### ::TDB::TRNC'
 
         CALL ETCAL  ( TVEC(1), CASE )
         CALL TIMOUT ( TVEC(1), PICTUR, STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
      CASE = '398 A.D. MAY 13 12:28:29.281 TDB '
      CALL TCASE  ( CASE )
 
         CALL TPARTV ( CASE, TVEC, NTVEC, TYPE,
     .                 MODIFY, MODS,  YABBRV, SUCCES,
     .                 PICTUR, ERROR )
 
         CALL CHCKXC ( .FALSE., ' ',             OK     )
         CALL CHCKSL ( 'SUCCES', SUCCES, .TRUE., OK     )
 
         CALL TTRANS (  TYPE,   'ET',            TVEC   )
         CALL CHCKXC ( .FALSE., ' ',             OK     )
 
         PICTUR = 'YYYY?ERA?MON DD HR:MN:SC.### ::TDB::TRNC'
 
         CALL ETCAL  ( TVEC(1), CASE )
         CALL TIMOUT ( TVEC(1), PICTUR, STRING )
         CALL LJUST  ( STRING, STRING )
 
         CALL CHCKXC ( .FALSE., ' ',                 OK )
         CALL CHCKSC ( 'STRING', STRING, '=', CASE,  OK )
 
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
