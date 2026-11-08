C$Procedure      F_LTIME ( Family of light time tests )
 
      SUBROUTINE F_LTIME ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine tests the routine LTIME to make sure that it
C     gives results compatible with SPKEZ.
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
C     None.
C
C
C     SPICELIB Functions
C
      EXTERNAL              LTIME
C
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(LNSIZE)    ERROR
 
      DOUBLE PRECISION      ELAPS
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET2
      DOUBLE PRECISION      EXPET
      DOUBLE PRECISION      EXPET2
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      STATE ( 6 )
 
      INTEGER               HANDLE
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_LTIME' )
 
      CALL TSTSPK ( 'test.bsp', .TRUE., HANDLE )
 
      CALL TCASE ( 'Test the advertised exception' )
 
         CALL TPARSE ( '1 JAN 1995',        ET,  ERROR )
         CALL LTIME  (  ET, 399, 'TO', 499, ET2, ELAPS )
 
         CALL CHCKXC ( .TRUE., 'SPICE(BADDIRECTION)', OK )
 
      CALL TCASE ( 'Check to make sure downlink light time '
     .//           'matches the light time returned by SPKEZ '
     .//           'when the option ''CN'' is used in SPKEZ. ' )
 
         CALL TPARSE ( '1 JAN 1995',            ET,  ERROR        )
         CALL LTIME  (  ET, 399, '<-',          599, ET2,   ELAPS )
         CALL SPKEZ  (  599, ET, 'J2000', 'CN', 399, STATE, LT    )
 
         EXPET2 = ET - LT
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ELAPS', ELAPS, '~', LT,    1.0D-12, OK )
         CALL CHCKSD ( 'ET2',   ET2, '~/', EXPET2, 1.0D-13, OK )
 
      CALL TCASE ( 'Check to make sure that the uplink time is '
     .//           'compatible with the down link time. ' )
 
         CALL TPARSE ( '1 JAN 1995', ET, ERROR )
         CALL LTIME  (  ET,  399, '->', 599, ET2,   LT )
         CALL LTIME  (  ET2, 599, '<-', 399, EXPET, ELAPS )
 
         EXPET2 = ET + LT
 
         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'ET2',   ET2, '~/', EXPET2, 1.0D-13, OK )
         CALL CHCKSD ( 'ELAPS', ELAPS, '~/', LT,    1.0D-13, OK )
         CALL CHCKSD ( 'ET',    ET,    '~/', EXPET, 1.0D-13, OK )
 
 
C
C     That's all folks.
C
      CALL SPKUEF (  HANDLE    )
      CALL KILFIL ( 'test.bsp' )
 
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
