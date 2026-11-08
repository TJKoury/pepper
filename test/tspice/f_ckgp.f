C$Procedure      F_CKGP (Family of tests for CKGP)
 
      SUBROUTINE F_CKGP ( OK )
      IMPLICIT NONE
 
      LOGICAL               OK
 
C$ Abstract
C
C     This routine performs checks on the routine CKGP to ensure
C     that the upgrade to use non-inertial frames has not broken
C     anything.
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
 
 
 
 
      CHARACTER*(LNSIZE)    CKNM
      CHARACTER*(LNSIZE)    ERROR
      CHARACTER*(LNSIZE)    PCKNM
      CHARACTER*(LNSIZE)    SCLKNM
 
      DOUBLE PRECISION      ANGVEL ( 3 )
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      ECMAT  ( 3, 3 )
      DOUBLE PRECISION      EOUT
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      TICKS
      DOUBLE PRECISION      TMAT   ( 3, 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TOUT
      DOUBLE PRECISION      XFORM  ( 6, 6 )
 
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               ID
      INTEGER               J
      INTEGER               SCLK
 
      LOGICAL               EFOUND
      LOGICAL               FOUND
      LOGICAL               KEEPPC
      LOGICAL               KEEPSC
      LOGICAL               LOADCK
      LOGICAL               LOADPC
      LOGICAL               LOADSC
 
 
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_CKGP' )
 
      CKNM   = 'phoenix.bc'
      PCKNM  = 'phoenix.pck'
      SCLKNM = 'phoenix.sclk'
      LOADCK = .TRUE.
      LOADSC = .TRUE.
      KEEPSC = .TRUE.
      LOADPC = .TRUE.
      KEEPPC = .FALSE.
 
      CALL KILFIL ( CKNM   )
      CALL KILFIL ( PCKNM  )
      CALL KILFIL ( SCLKNM )
 
      CALL TSTCKN ( CKNM,  SCLKNM, LOADCK, LOADSC, KEEPSC, HANDLE )
      CALL TSTPCK ( PCKNM, LOADPC, KEEPPC                         )
      CALL CKMETA ( -10000, 'SCLK',SCLK )
 
      CALL TPARSE ( '1 JAN 1995', ET, ERROR )
      CALL SCE2T  (  SCLK,        ET, TICKS )
 
 
 
      CALL TCASE ( 'Check that the old version of CKGP and the '
     .//           'new version yield the same result when the '
     .//           'frame requested is an inertial frame. ' )
 
         ID = -10000
 
         CALL CKGP   ( ID, TICKS, 0.0D0, 'J2000',  CMAT, TOUT, FOUND  )
         CALL CKGP_O ( ID, TICKS, 0.0D0, 'J2000', ECMAT, EOUT, EFOUND )
 
         CALL CHCKXC ( .FALSE., ' ',                       OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,             OK )
         CALL CHCKAD ( 'CMAT', CMAT, '=', ECMAT, 9, 0.0D0, OK )
         CALL CHCKSD ( 'TOUT', TOUT, '=', TICKS,    0.0D0, OK )
 
      CALL TCASE ( 'Perform an independent computation of the '
     .//           'attitude of object -9999 and compare this '
     .//           'with the value returned by CKGP. ' )
 
         ID  = -9999
         TOL =  0.0D0
 
         CALL CKMETA (  ID, 'SCLK', SCLK )
 
         CALL TPARSE ( '1 JAN 1995', ET, ERROR )
         CALL SCE2T  (  SCLK,        ET, TICKS )
C
C        Now convert TICKS back to ET so that ET and TIKS point to
C        the same epoch (remember TICKS get pushed to an integer value
C        by SCE2T).
C
         CALL SCT2E  (  SCLK,        TICKS, ET )
 
 
         CALL CKGP   ( ID, TICKS, TOL, 'IAU_EARTH', CMAT, TOUT, FOUND  )
C
C        CMAT now contains the transformation from IAU_EARTH to the
C        spacecraft frame (or it's supposed to anyway).
C
 
         CALL TSTATD ( ET,  TMAT,  ANGVEL )
C
C        TMAT gives the orientation of object -9999 relative to the
C        GALACTIC reference frame.  (TMAT is the transformation from
C        galactic to S.C. frame.)
C
         CALL SXFORM ( 'IAU_EARTH', 'GALACTIC', ET, XFORM )
 
         DO I = 1, 3
            DO J = 1,3
               ROT(I,J) = XFORM(I,J)
            END DO
         END DO
 
C
C        ROT      : IAU_EARTH ---> Galactic.
C        TMAT     : Galactic  ---> S.C. frame
C        TMAT*ROT : IAU_EARTH ---> S.C. frame
C
         CALL MXM    ( TMAT, ROT, ECMAT )
 
         CALL CHCKXC ( .FALSE., ' ',                           OK )
         CALL CHCKSL ( 'FOUND', FOUND, EFOUND,                 OK )
         CALL CHCKAD ( 'CMAT',  CMAT, '~/', ECMAT, 9, 1.0D-13, OK )
         CALL CHCKSD ( 'TOUT',  TOUT, '=',  TICKS,    0.0D0,   OK )
 
 
      CALL CKUPF  (  HANDLE      )
      CALL KILFIL ( CKNM   )
      CALL KILFIL ( PCKNM  )
      CALL KILFIL ( SCLKNM )
 
      CALL T_SUCCESS ( OK )
      RETURN
      END
