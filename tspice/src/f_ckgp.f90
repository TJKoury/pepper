!$Procedure      F_CKGP (Family of tests for CKGP)

      SUBROUTINE F_CKGP ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine performs checks on the routine CKGP to ensure
!     that the upgrade to use non-inertial frames has not broken
!     anything.
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!


!
!     Local Variables
!
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


!
!     Begin every test family with an open call.
!
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



      CALL TCASE ( 'Check that the old version of CKGP and the ' &
      //           'new version yield the same result when the ' &
      //           'frame requested is an inertial frame. ' )

         ID = -10000

         CALL CKGP   ( ID, TICKS, 0.0D0, 'J2000',  CMAT, TOUT, FOUND  )
         CALL CKGP_O ( ID, TICKS, 0.0D0, 'J2000', ECMAT, EOUT, EFOUND )

         CALL CHCKXC ( .FALSE., ' ',                       OK )
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,             OK )
         CALL CHCKAD ( 'CMAT', CMAT, '=', ECMAT, 9, 0.0D0, OK )
         CALL CHCKSD ( 'TOUT', TOUT, '=', TICKS,    0.0D0, OK )

      CALL TCASE ( 'Perform an independent computation of the ' &
      //           'attitude of object -9999 and compare this ' &
      //           'with the value returned by CKGP. ' )

         ID  = -9999
         TOL =  0.0D0

         CALL CKMETA (  ID, 'SCLK', SCLK )

         CALL TPARSE ( '1 JAN 1995', ET, ERROR )
         CALL SCE2T  (  SCLK,        ET, TICKS )
!
!        Now convert TICKS back to ET so that ET and TIKS point to
!        the same epoch (remember TICKS get pushed to an integer value
!        by SCE2T).
!
         CALL SCT2E  (  SCLK,        TICKS, ET )


         CALL CKGP   ( ID, TICKS, TOL, 'IAU_EARTH', CMAT, TOUT, FOUND  )
!
!        CMAT now contains the transformation from IAU_EARTH to the
!        spacecraft frame (or it's supposed to anyway).
!

         CALL TSTATD ( ET,  TMAT,  ANGVEL )
!
!        TMAT gives the orientation of object -9999 relative to the
!        GALACTIC reference frame.  (TMAT is the transformation from
!        galactic to S.C. frame.)
!
         CALL SXFORM ( 'IAU_EARTH', 'GALACTIC', ET, XFORM )

         DO I = 1, 3
            DO J = 1,3
               ROT(I,J) = XFORM(I,J)
            END DO
         END DO

!
!        ROT      : IAU_EARTH ---> Galactic.
!        TMAT     : Galactic  ---> S.C. frame
!        TMAT*ROT : IAU_EARTH ---> S.C. frame
!
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
