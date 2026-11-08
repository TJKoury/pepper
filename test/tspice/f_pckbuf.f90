!$Procedure      F_PCKBUF ( Text PCK reader buffering tests )

      SUBROUTINE F_PCKBUF ( OK )

! $ Abstract
!
!     This family tests buffering in the PCK natural body rotation
!     routines.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     PCK
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     OK         O   Test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK             is a boolean flag indicating test status. OK is
!                    returned .TRUE. only if all tests conducted by
!                    this family have passed.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Not applicable.
!
! $ Files
!
!     Not applicable.
!
! $ Particulars
!
!     This routine tests buffering in the PCK natural body rotation
!     routines introduced in N0066.
!
!     Orientation data are checked by means of calls to the SPICELIB
!     routines
!
!        BODMAT
!        TIPBOD
!        TISBOD
!
!     that were updated to do buffering against
!
!        BODEUL
!
!     that was not.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     B.V. Semenov      (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.0, 19-NOV-2020 (NJB)
!
!        Updated to look for SPICE(INSUFFICIENTANGLES) short error
!        message from BODEUL. Now performs tests using the optional
!        BODYnnn_MAX_PHASE_DEGREE kernel variable.
!
!-    TSPICE Version 1.0.0, 02-MAR-2016 (BVS)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      HALFPI

      INTEGER               ZZBODBRY

!
!     Local parameters.
!
!     MXPOLY and MAXANG should be the same as TISBOD values.
!
!     MAXBOD should be greater than TISBOD's MAXBOD to exercise reset
!     on fill up.
!
      INTEGER               MXPOLY
      PARAMETER           ( MXPOLY = 3 )

      INTEGER               MAXANG
      PARAMETER           ( MAXANG = 100 )

      INTEGER               FIRST
      PARAMETER           ( FIRST  = 1000 )

      INTEGER               MAXBOD
      PARAMETER           ( MAXBOD = 300 )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

!
!     Local Variables
!
      CHARACTER*(LINLEN)    LINE

      INTEGER               BODYID
      INTEGER               REFID
      INTEGER               I
      INTEGER               J
      INTEGER               FRMID
      INTEGER               NANGLS

      DOUBLE PRECISION      DVALS  ( MAXANG * 2 )
      DOUBLE PRECISION      ET

      DOUBLE PRECISION      RA
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      W
      DOUBLE PRECISION      LAMBDA

      DOUBLE PRECISION      MEUL     ( 3, 3 )
      DOUBLE PRECISION      MBOD     ( 3, 3 )
      DOUBLE PRECISION      MTIP     ( 3, 3 )
      DOUBLE PRECISION      MTIS     ( 3, 3 )
      DOUBLE PRECISION      M6X6     ( 6, 6 )

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_PCKBUF' )

!
!     Clear pool and fill it with bogus PCK data.
!
      CALL CLPOOL ( )

      DO BODYID = FIRST, FIRST + MAXBOD

         REFID  = ZZBODBRY ( BODYID )

         FRMID  = MOD( BODYID, 15 ) + 1

         NANGLS = MOD( BODYID, MAXANG ) + 1

         CALL FILLD ( DBLE( BODYID )/DBLE( FIRST ), MAXANG * 2, DVALS )

         LINE = 'BODY#_POLE_RA'
         CALL REPMI  ( LINE, '#', BODYID, LINE )
         CALL PDPOOL ( LINE, MXPOLY, DVALS )

         LINE = 'BODY#_POLE_DEC'
         CALL REPMI  ( LINE, '#', BODYID, LINE )
         CALL PDPOOL ( LINE, MXPOLY, DVALS )

         LINE = 'BODY#_PM'
         CALL REPMI  ( LINE, '#', BODYID, LINE )
         CALL PDPOOL ( LINE, MXPOLY, DVALS )

         LINE = 'BODY#_NUT_PREC_RA'
         CALL REPMI  ( LINE, '#', BODYID, LINE )
         CALL PDPOOL ( LINE, NANGLS, DVALS )

         LINE = 'BODY#_NUT_PREC_DEC'
         CALL REPMI  ( LINE, '#', BODYID, LINE )
         CALL PDPOOL ( LINE, NANGLS, DVALS )

         LINE = 'BODY#_NUT_PREC_PM'
         CALL REPMI  ( LINE, '#', BODYID, LINE )
         CALL PDPOOL ( LINE, NANGLS, DVALS )

         LINE = 'BODY#_CONSTANTS_JED_EPOCH'
         CALL REPMI ( LINE, '#', REFID, LINE )
         CALL PDPOOL ( LINE, 1, DVALS(1) + J2000() )

         LINE = 'BODY#_CONSTANTS_REF_FRAME'
         CALL REPMI ( LINE, '#', REFID, LINE )
         CALL PIPOOL ( LINE, 1, FRMID )

         LINE = 'BODY#_NUT_PREC_ANGLES'
         CALL REPMI ( LINE, '#', REFID, LINE )
         CALL PDPOOL ( LINE, NANGLS*2, DVALS )

         LINE = 'BODY#_MAX_PHASE_DEGREE'
         CALL REPMI  ( LINE, '#', BODYID, LINE )
         CALL PIPOOL ( LINE, 1, 1 )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check PCK rotations with un-disturbed POOL' )

      ET = SPD() * MAXBOD

      DO BODYID = FIRST, FIRST + MAXBOD
!
!        Get rotations using all four PCK routines.
!
         CALL BODEUL ( BODYID, ET, RA, DEC, W, LAMBDA )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL EUL2M  ( W, HALFPI() - DEC, HALFPI() + RA, 3, 1, 3, MEUL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL BODMAT ( BODYID, ET, MBOD )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TIPBOD ( 'J2000', BODYID, ET, MTIP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TISBOD ( 'J2000', BODYID, ET, M6X6 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO I = 1, 3
            DO J = 1, 3
               MTIS(I,J) = M6X6 (I,J)
            END DO
         END DO

!
!        Check all matrices against BODEUL matrix.
!
         LINE = 'BODMAT M vs BODEUL M for body #'
         CALL REPMI ( LINE, '#', BODYID, LINE )
         CALL CHCKAD ( LINE, MBOD, '~', MEUL, 9, TIGHT, OK )

         LINE = 'TIPBOD M vs BODEUL M for body #'
         CALL REPMI ( LINE, '#', BODYID, LINE )
         CALL CHCKAD ( LINE, MTIP, '~', MEUL, 9, TIGHT, OK )

         LINE = 'TISBOD M vs BODEUL M for body #'
         CALL REPMI ( LINE, '#', BODYID, LINE )
         CALL CHCKAD ( LINE, MTIS, '~', MEUL, 9, TIGHT, OK )

      END DO

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check PCK rotations with disturbed POOL' )

      ET = SPD() * MAXBOD

      DO BODYID = FIRST, FIRST + MAXBOD

!
!        Get rotations using all four PCK routines with "touching"
!        before all of them but BODEUL.
!
         CALL BODEUL ( BODYID, ET, RA, DEC, W, LAMBDA )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL EUL2M  ( W, HALFPI() - DEC, HALFPI() + RA, 3, 1, 3, MEUL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PIPOOL ( 'I', 1, 1 )

         CALL BODMAT ( BODYID, ET, MBOD )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PIPOOL ( 'I', 1, 1 )

         CALL TIPBOD ( 'J2000', BODYID, ET, MTIP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PIPOOL ( 'I', 1, 1 )

         CALL TISBOD ( 'J2000', BODYID, ET, M6X6 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO I = 1, 3
            DO J = 1, 3
               MTIS(I,J) = M6X6 (I,J)
            END DO
         END DO

!
!        Check all matrices against BODEUL matrix.
!
         LINE = 'BODMAT M vs BODEUL M for body #'
         CALL REPMI ( LINE, '#', BODYID, LINE )
         CALL CHCKAD ( LINE, MBOD, '~', MEUL, 9, TIGHT, OK )

         LINE = 'TIPBOD M vs BODEUL M for body #'
         CALL REPMI ( LINE, '#', BODYID, LINE )
         CALL CHCKAD ( LINE, MTIP, '~', MEUL, 9, TIGHT, OK )

         LINE = 'TISBOD M vs BODEUL M for body #'
         CALL REPMI ( LINE, '#', BODYID, LINE )
         CALL CHCKAD ( LINE, MTIS, '~', MEUL, 9, TIGHT, OK )

      END DO


!
! --- Some success cases with partial data ------------------------
!
      CALL TCASE ( 'Check PCK rotations with partial data' )

      ET = SPD() * MAXBOD

      DO BODYID = FIRST + MAXBOD - 6, FIRST + MAXBOD

!
!        Remove some optional values from the POOL.
!
         REFID  = ZZBODBRY ( BODYID )

         IF      ( BODYID .EQ. FIRST + MAXBOD - 6 ) THEN

            LINE = 'BODY#_MAX_PHASE_DEGREE'
            CALL REPMI  ( LINE, '#', BODYID, LINE )
            CALL DVPOOL ( LINE )

         ELSE IF ( BODYID .EQ. FIRST + MAXBOD - 5 ) THEN

            LINE = 'BODY#_NUT_PREC_RA'
            CALL REPMI  ( LINE, '#', BODYID, LINE )
            CALL DVPOOL ( LINE )

         ELSE IF ( BODYID .EQ. FIRST + MAXBOD - 4 ) THEN

            LINE = 'BODY#_NUT_PREC_DEC'
            CALL REPMI  ( LINE, '#', BODYID, LINE )
            CALL DVPOOL ( LINE )

         ELSE IF ( BODYID .EQ. FIRST + MAXBOD - 3 ) THEN

            LINE = 'BODY#_NUT_PREC_PM'
            CALL REPMI  ( LINE, '#', BODYID, LINE )
            CALL DVPOOL ( LINE )

         ELSE IF ( BODYID .EQ. FIRST + MAXBOD - 2 ) THEN

            LINE = 'BODY#_CONSTANTS_JED_EPOCH'
            CALL REPMI ( LINE, '#', REFID, LINE )
            CALL DVPOOL ( LINE )

         ELSE IF ( BODYID .EQ. FIRST + MAXBOD - 1 ) THEN

            LINE = 'BODY#_CONSTANTS_REF_FRAME'
            CALL REPMI ( LINE, '#', REFID, LINE )
            CALL DVPOOL ( LINE )

         ELSE IF ( BODYID .EQ. FIRST + MAXBOD     ) THEN

            LINE = 'BODY#_NUT_PREC_RA'
            CALL REPMI  ( LINE, '#', BODYID, LINE )
            CALL DVPOOL ( LINE )

            LINE = 'BODY#_NUT_PREC_DEC'
            CALL REPMI  ( LINE, '#', BODYID, LINE )
            CALL DVPOOL ( LINE )

            LINE = 'BODY#_NUT_PREC_PM'
            CALL REPMI  ( LINE, '#', BODYID, LINE )
            CALL DVPOOL ( LINE )

            LINE = 'BODY#_NUT_PREC_ANGLES'
            CALL REPMI ( LINE, '#', REFID, LINE )
            CALL DVPOOL ( LINE )

         END IF

!
!        Get rotations using all four PCK routines.
!
         CALL BODEUL ( BODYID, ET, RA, DEC, W, LAMBDA )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL EUL2M  ( W, HALFPI() - DEC, HALFPI() + RA, 3, 1, 3, MEUL )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PIPOOL ( 'I', 1, 1 )

         CALL BODMAT ( BODYID, ET, MBOD )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PIPOOL ( 'I', 1, 1 )

         CALL TIPBOD ( 'J2000', BODYID, ET, MTIP )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PIPOOL ( 'I', 1, 1 )

         CALL TISBOD ( 'J2000', BODYID, ET, M6X6 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         DO I = 1, 3
            DO J = 1, 3
               MTIS(I,J) = M6X6 (I,J)
            END DO
         END DO

!
!        Check all matrices against BODEUL matrix.
!
         LINE = 'BODMAT M vs BODEUL M for body #'
         CALL REPMI ( LINE, '#', BODYID, LINE )
         CALL CHCKAD ( LINE, MBOD, '~', MEUL, 9, TIGHT, OK )

         LINE = 'TIPBOD M vs BODEUL M for body #'
         CALL REPMI ( LINE, '#', BODYID, LINE )
         CALL CHCKAD ( LINE, MTIP, '~', MEUL, 9, TIGHT, OK )

         LINE = 'TISBOD M vs BODEUL M for body #'
         CALL REPMI ( LINE, '#', BODYID, LINE )
         CALL CHCKAD ( LINE, MTIS, '~', MEUL, 9, TIGHT, OK )

      END DO

!
! --- Some failure cases ------------------------------------------
!

!
!     Since we have the POOL conveniently filled with whole bunch of
!     PCK data, let's do some error checks.
!

!
!     Try without _PM keyword.
!
      BODYID = FIRST
      LINE = 'BODY#_PM'
      CALL REPMI  ( LINE, '#', BODYID, LINE )
      CALL DVPOOL ( LINE )

      CALL TCASE ( 'Check for missing _PM, BODEUL' )

      CALL BODEUL ( BODYID, ET, RA, DEC, W, LAMBDA )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL TCASE ( 'Check for missing _PM, BODMAT' )

      CALL BODMAT ( BODYID, ET, MBOD )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

      CALL TCASE ( 'Check for missing _PM, TIPBOD' )

      CALL TIPBOD ( 'J2000', BODYID, ET, MTIP )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

      CALL TCASE ( 'Check for missing _PM, TISBOD' )

      CALL TISBOD ( 'J2000', BODYID, ET, M6X6 )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

!
!     Try without _POLE_RA keyword.
!
      BODYID = FIRST + 1
      LINE = 'BODY#_POLE_RA'
      CALL REPMI  ( LINE, '#', BODYID, LINE )
      CALL DVPOOL ( LINE )

      CALL TCASE ( 'Check for missing _POLE_RA, BODEUL' )

      CALL BODEUL ( BODYID, ET, RA, DEC, W, LAMBDA )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL TCASE ( 'Check for missing _POLE_RA, BODMAT' )

      CALL BODMAT ( BODYID, ET, MBOD )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL TCASE ( 'Check for missing _POLE_RA, TIPBOD' )

      CALL TIPBOD ( 'J2000', BODYID, ET, MTIP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL TCASE ( 'Check for missing _POLE_RA, TISBOD' )

      CALL TISBOD ( 'J2000', BODYID, ET, M6X6 )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

!
!     Try without _POLE_DEC keyword.
!
      BODYID = FIRST + 2
      LINE = 'BODY#_POLE_DEC'
      CALL REPMI  ( LINE, '#', BODYID, LINE )
      CALL DVPOOL ( LINE )

      CALL TCASE ( 'Check for missing _POLE_DEC, BODEUL' )

      CALL BODEUL ( BODYID, ET, RA, DEC, W, LAMBDA )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL TCASE ( 'Check for missing _POLE_DEC, BODMAT' )

      CALL BODMAT ( BODYID, ET, MBOD )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL TCASE ( 'Check for missing _POLE_DEC, TIPBOD' )

      CALL TIPBOD ( 'J2000', BODYID, ET, MTIP )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL TCASE ( 'Check for missing _POLE_DEC, TISBOD' )

      CALL TISBOD ( 'J2000', BODYID, ET, M6X6 )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

!
!     Try with _NUT_PREC_ANGLES that has too few values.
!
      BODYID = FIRST + 3
      REFID  = ZZBODBRY ( BODYID )
      LINE = 'BODY#_NUT_PREC_ANGLES'
      CALL REPMI  ( LINE, '#', REFID, LINE )
      CALL PDPOOL ( LINE, 2, DVALS )

      CALL TCASE ( 'Check for too small _NUT_PREC_ANGLES, BODEUL' )

      CALL BODEUL ( BODYID, ET, RA, DEC, W, LAMBDA )
      CALL CHCKXC ( .TRUE., 'SPICE(INSUFFICIENTANGLES)', OK )

      CALL TCASE ( 'Check for too small _NUT_PREC_ANGLES, BODMAT' )

      CALL BODMAT ( BODYID, ET, MBOD )
      CALL CHCKXC ( .TRUE., 'SPICE(INSUFFICIENTANGLES)', OK )

      CALL TCASE ( 'Check for too small _NUT_PREC_ANGLES, TIPBOD' )

      CALL TIPBOD ( 'J2000', BODYID, ET, MTIP )
      CALL CHCKXC ( .TRUE., 'SPICE(INSUFFICIENTANGLES)', OK )

      CALL TCASE ( 'Check for too small _NUT_PREC_ANGLES, TISBOD' )

      CALL TISBOD ( 'J2000', BODYID, ET, M6X6 )
      CALL CHCKXC ( .TRUE., 'SPICE(INSUFFICIENTANGLES)', OK )


!
!     Clear POOL before leaving.
!
      CALL CLPOOL ( )

!
!     All done.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
