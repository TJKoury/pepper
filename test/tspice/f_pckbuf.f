C$Procedure      F_PCKBUF ( Text PCK reader buffering tests )

      SUBROUTINE F_PCKBUF ( OK )

C$ Abstract
C
C     This family tests buffering in the PCK natural body rotation
C     routines.
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
C$ Required_Reading
C
C     PCK
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations

      IMPLICIT NONE

      LOGICAL               OK

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     OK         O   Test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK             is a boolean flag indicating test status. OK is
C                    returned .TRUE. only if all tests conducted by
C                    this family have passed.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Not applicable.
C
C$ Files
C
C     Not applicable.
C
C$ Particulars
C
C     This routine tests buffering in the PCK natural body rotation
C     routines introduced in N0066.
C
C     Orientation data are checked by means of calls to the SPICELIB
C     routines
C
C        BODMAT
C        TIPBOD
C        TISBOD
C
C     that were updated to do buffering against
C
C        BODEUL
C
C     that was not.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     B.V. Semenov      (JPL)
C
C$ Version
C
C-    TSPICE Version 2.0.0, 19-NOV-2020 (NJB)
C
C        Updated to look for SPICE(INSUFFICIENTANGLES) short error
C        message from BODEUL. Now performs tests using the optional
C        BODYnnn_MAX_PHASE_DEGREE kernel variable.
C
C-    TSPICE Version 1.0.0, 02-MAR-2016 (BVS)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      HALFPI

      INTEGER               ZZBODBRY

C
C     Local parameters.
C
C     MXPOLY and MAXANG should be the same as TISBOD values.
C
C     MAXBOD should be greater than TISBOD's MAXBOD to exercise reset
C     on fill up.
C
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

C
C     Local Variables
C
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

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_PCKBUF' )

C
C     Clear pool and fill it with bogus PCK data.
C
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

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check PCK rotations with un-disturbed POOL' )

      ET = SPD() * MAXBOD

      DO BODYID = FIRST, FIRST + MAXBOD
C
C        Get rotations using all four PCK routines.
C
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

C
C        Check all matrices against BODEUL matrix.
C
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

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check PCK rotations with disturbed POOL' )

      ET = SPD() * MAXBOD

      DO BODYID = FIRST, FIRST + MAXBOD

C
C        Get rotations using all four PCK routines with "touching"
C        before all of them but BODEUL.
C
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

C
C        Check all matrices against BODEUL matrix.
C
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


C
C --- Some success cases with partial data ------------------------
C
      CALL TCASE ( 'Check PCK rotations with partial data' )

      ET = SPD() * MAXBOD

      DO BODYID = FIRST + MAXBOD - 6, FIRST + MAXBOD

C
C        Remove some optional values from the POOL.
C
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

C
C        Get rotations using all four PCK routines.
C
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

C
C        Check all matrices against BODEUL matrix.
C
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

C
C --- Some failure cases ------------------------------------------
C

C
C     Since we have the POOL conveniently filled with whole bunch of
C     PCK data, let's do some error checks.
C

C
C     Try without _PM keyword.
C
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

C
C     Try without _POLE_RA keyword.
C
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

C
C     Try without _POLE_DEC keyword.
C
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

C
C     Try with _NUT_PREC_ANGLES that has too few values.
C
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


C
C     Clear POOL before leaving.
C
      CALL CLPOOL ( )

C
C     All done.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
