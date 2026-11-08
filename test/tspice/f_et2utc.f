C$Procedure      F_ET2UTC ( Family of tests for ET2UTC)
 
      SUBROUTINE F_ET2UTC ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine performs a number of tests of the SPICELIB
C     routine ET2UTC
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
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               TMSIZE
      PARAMETER           ( TMSIZE = 60 )
 
      INTEGER               I
      INTEGER               NLINES
 
      CHARACTER*(LNSIZE)    KERNEL ( 23  )
      DOUBLE PRECISION      ET
 
      CHARACTER*(TMSIZE)    ESTR   ( 6 )
      CHARACTER*(TMSIZE)    UTCSTR ( 6 )
      CHARACTER*(TMSIZE)    PICTUR
 
      CHARACTER*(LNSIZE)    ERROR
      LOGICAL               PASS
 
      CHARACTER*(4)         FMT  ( 6 )
      INTEGER               PREC ( 6 )
 
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ET2UTC' )
C
C     Set up the leapseconds kernel we'll be needing.
C
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
 
 
 
      CALL TCASE ( 'Test that the advertised conversion in '
     .//           'the header of ET2UTC behaves as predicted. ' )
 
         ET      = -527644192.5403653D0
 
         ESTR(1) = '1983 APR 13 12:09:14'
         ESTR(2) = '1983 APR 13 12:09:14.274'
         ESTR(3) = '1983-103 // 12:09:14.27400'
         ESTR(4) = 'JD 2445438.0064152'
         ESTR(5) = '1983-103T12:09:14.274'
         ESTR(6) = '1983-04-13T12:09:14.274'
 
         CALL ET2UTC ( ET, 'C',    0, UTCSTR(1) )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
         CALL ET2UTC ( ET, 'C',    3, UTCSTR(2) )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
         CALL ET2UTC ( ET, 'D',    5, UTCSTR(3) )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
         CALL ET2UTC ( ET, 'J',    7, UTCSTR(4) )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
         CALL ET2UTC ( ET, 'ISOD', 3, UTCSTR(5) )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
         CALL ET2UTC ( ET, 'ISOC', 3, UTCSTR(6) )
         CALL CHCKXC ( .FALSE., ' ', OK )
 
         CALL CHCKSC ( 'UTCSTR(1)', UTCSTR(1), '=', ESTR(1),  OK )
         CALL CHCKSC ( 'UTCSTR(2)', UTCSTR(2), '=', ESTR(2),  OK )
         CALL CHCKSC ( 'UTCSTR(3)', UTCSTR(3), '=', ESTR(3),  OK )
         CALL CHCKSC ( 'UTCSTR(4)', UTCSTR(4), '=', ESTR(4),  OK )
         CALL CHCKSC ( 'UTCSTR(5)', UTCSTR(5), '=', ESTR(5),  OK )
         CALL CHCKSC ( 'UTCSTR(6)', UTCSTR(6), '=', ESTR(6),  OK )
 
      CALL TCASE ( 'Use the time formatting routine TIMOUT '
     .//           'together with the utility TPICTR to see that '
     .//           'we get consistent strings. ' )
 
         DO I = 1, 6
 
            CALL TSTMSG ( '#', 'Test Case 2, Subcase #.' )
            CALL TSTMSI (  I )
 
            CALL TPICTR (  ESTR(I), PICTUR, PASS, ERROR )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL CHCKSL ( 'PASS', PASS, .TRUE.,  OK )
 
            CALL TIMOUT ( ET, PICTUR, UTCSTR(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
         END DO
 
         CALL CHCKSC ( 'UTCSTR(1)', UTCSTR(1), '=', ESTR(1),  OK )
         CALL CHCKSC ( 'UTCSTR(2)', UTCSTR(2), '=', ESTR(2),  OK )
         CALL CHCKSC ( 'UTCSTR(3)', UTCSTR(3), '=', ESTR(3),  OK )
         CALL CHCKSC ( 'UTCSTR(4)', UTCSTR(4), '=', ESTR(4),  OK )
         CALL CHCKSC ( 'UTCSTR(5)', UTCSTR(5), '=', ESTR(5),  OK )
         CALL CHCKSC ( 'UTCSTR(6)', UTCSTR(6), '=', ESTR(6),  OK )
 
 
 
      CALL TCASE ( 'Test the invertability of the call UTC2ET.' )
 
 
 
         ESTR(1) = 'JD 2451712.2829282      '
         ESTR(2) = '1987 MAR 12 19:28:28.28729  '
         ESTR(3) = '1989 DEC 31 23:59:60.18291  '
         ESTR(4) = '1990-001 // 00:00:00.1728    '
         ESTR(5) = '1987-03-18T17:28:28.182        '
         ESTR(6) = '1986-239T12:29:28.287        '
 
 
         FMT (1) = 'J'
         FMT (2) = 'C'
         FMT (3) = 'C'
         FMT (4) = 'D'
         FMT (5) = 'ISOC'
         FMT (6) = 'ISOD'
 
         PREC(1) = 7
         PREC(2) = 5
         PREC(3) = 5
         PREC(4) = 4
         PREC(5) = 3
         PREC(6) = 3
 
 
 
 
         DO I = 1, 6
            CALL TSTMSG ( '#', 'Test Case 3, Subcase #.' )
            CALL TSTMSI (  I )
 
            CALL UTC2ET ( ESTR(I), ET )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
            CALL ET2UTC ( ET, FMT(I), PREC(I), UTCSTR(I) )
            CALL CHCKXC ( .FALSE., ' ', OK )
 
         END DO
 
         CALL CHCKSC ( 'UTCSTR(1)', UTCSTR(1), '=', ESTR(1),  OK )
         CALL CHCKSC ( 'UTCSTR(2)', UTCSTR(2), '=', ESTR(2),  OK )
         CALL CHCKSC ( 'UTCSTR(3)', UTCSTR(3), '=', ESTR(3),  OK )
         CALL CHCKSC ( 'UTCSTR(4)', UTCSTR(4), '=', ESTR(4),  OK )
         CALL CHCKSC ( 'UTCSTR(5)', UTCSTR(5), '=', ESTR(5),  OK )
         CALL CHCKSC ( 'UTCSTR(6)', UTCSTR(6), '=', ESTR(6),  OK )
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
