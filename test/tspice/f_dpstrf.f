C$Procedure      F_DPSTRF (Family of tests for dpstrf )
 
      SUBROUTINE F_DPSTRF ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine performs a sequence of tests on the SPICELIB
C     routine dpstrf.
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
 
 
C
C     SPICELIB Functions
C
 
 
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(LNSIZE)    ESTRNG
      CHARACTER*(LNSIZE)    STRING
 
      INTEGER               SIGDIG
      DOUBLE PRECISION      X
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_DPSTRF' )
 
 
 
      CALL TCASE  ( 'Test Rounding X =  1.28372D+8, SIGDIG = 2' )
 
         X      = 1.28372D+8
         SIGDIG = 2
         ESTRNG = ' 130000000.'
 
         CALL DPSTRF ( X, SIGDIG, 'F', STRING )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )
 
 
      CALL TCASE  ( 'Test Rounding X = -1.28372D+8, SIGDIG = 2' )
 
         X      = -1.28372D+8
         SIGDIG = 2
         ESTRNG = '-130000000.'
 
         CALL DPSTRF ( X, SIGDIG, 'F', STRING )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )
 
 
 
      CALL TCASE  ( 'Test Rounding X =  9.9995 , SIGDIG = 4' )
 
         X      = 9.9995D+0
         SIGDIG = 4
         ESTRNG = ' 10.00'
 
         CALL DPSTRF ( X, SIGDIG, 'F', STRING )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )
 
 
 
      CALL TCASE  ( 'Test Rounding X =  9.9995 , SIGDIG = 5' )
 
         X      = 9.9995D+0
         SIGDIG = 5
         ESTRNG = ' 9.9995'
 
         CALL DPSTRF ( X, SIGDIG, 'F', STRING )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )
 
 
      CALL TCASE ( 'Test small values X = 1.2829D-21, SIGDIG = 5' )
 
         X      = 1.2829D-21
         SIGDIG = 5
         ESTRNG = ' 0.0000000000000000000012829'
 
 
         CALL DPSTRF ( X, SIGDIG, 'F', STRING )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )
 
      CALL TCASE ( 'Test large values X = 9.92829D+35 SIGDIG = 3' )
 
         X      = 9.92829D+35
         SIGDIG = 3
         ESTRNG =
     .   ' 993000000000000000000000000000000000.'
 
         CALL DPSTRF ( X, SIGDIG, 'F', STRING )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )
 
      CALL TCASE ( 'Test large values X = 9.96829D+35 SIGDIG = 2' )
 
         X      = 9.96829D+35
         SIGDIG = 2
         ESTRNG =
     .   ' 1000000000000000000000000000000000000.'
 
         CALL DPSTRF ( X, SIGDIG, 'F', STRING )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )
 
 
      CALL TCASE ( 'Intermediate value of X with non-zero '
     .//           'integer and fractional parts.  X = 123.18272 '
     .//           'SIGDIG = 14 ' )
 
         X      = 123.18272D+0
         SIGDIG = 14
 
         ESTRNG = ' 123.18272000000'
 
         CALL DPSTRF ( X, SIGDIG, 'F', STRING )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )
 
      CALL TCASE ( 'Intermediate value of X with non-zero '
     .//           'integer and fractional parts.  X = -123.18272 '
     .//           'SIGDIG = 14 ' )
 
         X      = -123.18272D+0
         SIGDIG = 14
 
         ESTRNG = '-123.18272000000'
 
         CALL DPSTRF ( X, SIGDIG, 'F', STRING )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )
 
      CALL TCASE ( 'Zero case X = 0, SIGDIG = 7 ' )
 
         X      = 0.0D0
         SIGDIG = 7
         ESTRNG = ' 0.0000000'
 
         CALL DPSTRF ( X, SIGDIG, 'F', STRING )
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSC ( 'STRING', STRING, '=', ESTRNG, OK )
 
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
