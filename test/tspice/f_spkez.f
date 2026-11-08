C$Procedure      F_SPKEZ ( Family of tests for SPKEZ)
 
      SUBROUTINE F_SPKEZ ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine performs a series of tests on the routine
C     SPKEZ to make sure that it behaves as expected.
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
C$ Version
C
C-    TSPICE Version 3.0.0, 08-JAN-2008 (NJB)
C
C        Updated for compatibility with new SPK 
C        aberration-corrected velocity computations.
C
C     TSPICE Version 2.0.0 06-DEC-2001 (NJB)
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
 
      CHARACTER*(LNSIZE)    ERROR
 
 
      DOUBLE PRECISION      CLT
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      ELT
      DOUBLE PRECISION      ESTATE ( 6 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ISTATE ( 6 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STOBS  ( 6 )
      DOUBLE PRECISION      TSTATE ( 6 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )
 
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_SPKEZ' )
 
      CALL KILFIL ( 'phoenix2.bsp' )
      CALL KILFIL ( 'phoenix2.ker' )
 
      CALL TSTSPK ( 'phoenix2.bsp', .TRUE.,  HANDLE )
 
      CALL TSTPCK ( 'phoenix2.ker', .TRUE., .FALSE. )
      CALL TPARSE ( '1 JAN 1995',   ET,     ERROR )
 
      CALL TCASE ( 'Make sure that SPKEZ and SPKGEO return the '
     .//           'same information when the correction '
     .//           'requested is NONE. ' )
 
      CALL SPKEZ ( 401001, ET, 'IAU_EARTH', 'NONE', 301001,
     .             STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SPKGEO( 401001, ET, 'IAU_EARTH', 301001, ESTATE, ELT )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 6, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )
 
 


      CALL TCASE ( 'Make sure that SPKEZ returns the same thing '
     .//           'as SPKLTC when '
     .//           'an inertial frame is the requested output '
     .//           'frame. Light time only correction.' )
 
      CALL SPKEZ ( 301001, ET, 'J2000', 'LT', 399001,
     .             STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SPKSSB( 399001, ET, 'J2000',       STOBS )
      CALL SPKLTC( 301001, ET, 'J2000', 'LT', STOBS,
     .             ESTATE, ELT, DLT  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 6, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )



      CALL TCASE ( 'Repeat the previous test for the transmission ' //
     .             'case.'                                          )

      CALL SPKEZ ( 301001, ET, 'J2000', 'XLT', 399001,
     .             STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SPKSSB( 399001, ET, 'J2000',        STOBS )
      CALL SPKLTC( 301001, ET, 'J2000', 'XLT', STOBS,
     .             ESTATE, ELT, DLT      )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 6, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )


 
      CALL TCASE ( 'Make sure that SPKEZ returns the same thing '
     .//           'as SPKACS when '
     .//           'an inertial frame is the requested output '
     .//           'frame. Converged Newtonian plus stellar aberation' )
 
      CALL SPKEZ ( 301001, ET, 'J2000', 'CN+S', 399001,
     .             STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SPKACS ( 301001, ET, 'J2000', 'CN+S', 399001,
     .              ESTATE, ELT, DLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 6, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )



      CALL TCASE ( 'Repeat the previous test for the transmission ' //
     .             'case.'                                          )

      CALL SPKEZ ( 301001, ET, 'J2000', 'XCN+S', 399001,
     .             STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SPKACS ( 301001, ET, 'J2000', 'XCN+S', 399001,
     .              ESTATE, ELT, DLT )

      CALL CHCKAD ( 'STATE', STATE, '=', ESTATE, 6, 0.0D0, OK )
      CALL CHCKSD ( 'LT',    LT,    '=', ELT,       0.0D0, OK )


 
      CALL TCASE ( 'Perform an independent test to see if '
     .//           '"apparent" positions in non-inertial frames '
     .//           'are properly computed. Frame center is not '
     .//           'target or observer.'                         )
 
 
      CALL SPKEZ ( 401001, ET, 'IAU_EARTH', 'CN+S', 301001,
     .             STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

      CALL SPKACS ( 401001, ET, 'J2000', 'CN+S', 301001,
     .              ISTATE, ELT, DLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKSSB( 301001, ET, 'J2000',         STOBS )
      CALL SPKLTC( 399,    ET, 'J2000', 'CN+S', STOBS, 
     .             TSTATE, CLT, DLT     )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM( 'J2000', 'IAU_EARTH', ET-CLT,       XFORM       )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scale the derivative block to account for the rate of change
C     of light time.
C
      DO J = 4, 6

         DO I = 1, 3
            XFORM(J,I) = (1.D0 - DLT)*XFORM(J,I)
         END DO

      END DO

      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, 1.0D-14, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-14, OK )




      CALL TCASE ( 'Repeat the previous test for the transmission ' //
     .             'case.'                                          )
 
      CALL SPKEZ ( 401001, ET, 'IAU_EARTH', 'XCN+S', 301001,
     .             STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

      CALL SPKACS ( 401001, ET, 'J2000', 'XCN+S', 301001,
     .              ISTATE, ELT, DLT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKSSB( 301001, ET, 'J2000',          STOBS )
      CALL SPKLTC( 399,    ET, 'J2000', 'XCN+S', STOBS, 
     .             TSTATE, CLT, DLT                    )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SXFORM( 'J2000', 'IAU_EARTH', ET+CLT,        XFORM       )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scale the derivative block to account for the rate of change
C     of light time.
C
      DO J = 4, 6

         DO I = 1, 3
            XFORM(J,I) = (1.D0 + DLT)*XFORM(J,I)
         END DO

      END DO


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, 1.0D-14, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-14, OK )




      CALL TCASE ( 'Perform an independent test to see if '
     .//           '"apparent" positions in non-inertial frames '
     .//           'are properly computed. Frame center is '
     .//           'target.'                                    )
 
 
      CALL SPKEZ ( 399, ET, 'IAU_EARTH', 'CN+S', 301001, STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

      CALL SPKACS ( 399, ET, 'J2000', 'CN+S', 301001,
     .              ISTATE, ELT, DLT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SXFORM( 'J2000', 'IAU_EARTH', ET-ELT,       XFORM       )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scale the derivative block to account for the rate of change
C     of light time.
C
      DO J = 4, 6

         DO I = 1, 3
            XFORM(J,I) = (1.D0 - DLT)*XFORM(J,I)
         END DO

      END DO


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, 1.0D-14, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-14, OK )




      CALL TCASE ( 'Repeat the previous test for the transmission ' //
     .             'case.'                                          )
 
      CALL SPKEZ ( 399, ET, 'IAU_EARTH', 'XCN+S', 301001, STATE, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 

      CALL SPKACS ( 399, ET, 'J2000', 'XCN+S', 301001,
     .              ISTATE, ELT, DLT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SXFORM( 'J2000', 'IAU_EARTH', ET+ELT,        XFORM       )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Scale the derivative block to account for the rate of change
C     of light time.
C
      DO J = 4, 6

         DO I = 1, 3
            XFORM(J,I) = (1.D0 + DLT)*XFORM(J,I)
         END DO

      END DO


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, 1.0D-14, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-14, OK )




      CALL TCASE ( 'Perform an independent test to see if '
     .//           '"apparent" positions in non-inertial frames '
     .//           'are properly computed. Frame center is '
     .//           'observer.'                                    )
 
 
      CALL SPKEZ ( 301001, ET, 'IAU_EARTH', 'CN+S', 399, STATE,  LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SPKACS( 301001, ET, 'J2000', 'CN+S', 399, 
     .             ISTATE, ELT, DLT       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM( 'J2000', 'IAU_EARTH', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, 1.0D-14, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-14, OK )


      CALL TCASE ( 'Repeat the previous test for the transmission ' //
     .             'case.'                                          )
 
      CALL SPKEZ ( 301001, ET, 'IAU_EARTH', 'XCN+S', 399, STATE, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SPKACS( 301001, ET, 'J2000', 'XCN+S', 399, 
     .             ISTATE, ELT, DLT       )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SXFORM( 'J2000', 'IAU_EARTH', ET, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL MXVG  ( XFORM, ISTATE, 6, 6, ESTATE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKAD ( 'STATE', STATE, '~/', ESTATE, 6, 1.0D-14, OK )
      CALL CHCKSD ( 'LT',    LT,    '~/', ELT,       1.0D-14, OK )

      CALL SPKUEF (  HANDLE        )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KILFIL ( 'phoenix2.bsp' )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
  
      CALL T_SUCCESS ( OK )
      RETURN
      END
