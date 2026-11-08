!$Procedure F_CKW02 ( Family of checks for CKW02 )

      SUBROUTINE F_CKW02 ( OK )

! $ Abstract
!
!     This family of tests checks all CKW02 exceptions and that a good
!     CK segment can indeed get written to a file.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the routine
!
!        CKW02
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    TSPICE Version 2.1.0, 30-MAY-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 2.0.0, 01-JUN-2010 (NJB)
!
!        Updated to account for error handling change in
!        the SPICELIB routine CKW05: now that routine tests
!        for zero-length quaternions rather than non-unit-length
!        quaternions.
!
!-    TSPICE Version 1.2.0, 26-SEP-2005 (BVS)
!
!        This test routine now checks all CKW02 exceptions.
!
!-    TSPICE Version 1.1.0, 15-FEB-2001 (EDW)
!
!        Altered Test case 2 to use an error tolerance of 1.D-14
!        instead of an exact equality. That test failed due to roundoff
!        error (on the order of 1.D-16) when compiled under the
!        Macintosh Absoft FORTRAN compiler.
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     Local Variables
!
      INTEGER               COUNT
      PARAMETER           ( COUNT = 4 )

      INTEGER               HANDLE
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      ENDTIM
      INTEGER               INST
      CHARACTER*(8)         REF
      CHARACTER*(42)        SEGID
      INTEGER               NREC

      DOUBLE PRECISION      START  (      COUNT )
      DOUBLE PRECISION      STOP   (      COUNT )
      DOUBLE PRECISION      QUATS  ( 0:3, COUNT )
      DOUBLE PRECISION      AVVS   (   3, COUNT )
      DOUBLE PRECISION      RATES  (      COUNT )

      DOUBLE PRECISION      EMAT ( 3, 3 )
      DOUBLE PRECISION      AV   ( 3 )
      DOUBLE PRECISION      CMAT ( 3, 3 )
      DOUBLE PRECISION      CLKOUT
      LOGICAL               FOUND

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_CKW02' )

      CALL KILFIL( 'ckw02.bc' )
      CALL CKOPN ( 'ckw02.bc', 'TestCK02', 1000, HANDLE )

      BEGTIM    =  2.D0
      ENDTIM    = 12.D0

      START(1) =  2.0D0
      START(2) =  4.0D0
      START(3) =  8.0D0
      START(4) = 10.0D0


      STOP (1) = START(2)
      STOP (2) = START(3)
      STOP (3) = START(4)
      STOP (4) = ENDTIM


      RATES(1) = 1.0D0
      RATES(2) = 0.75D0
      RATES(3) = 0.5D0
      RATES(4) = 0.25D0


      INST      = -1002
      REF       = 'J2000'
      SEGID     = 'TEST CKTYPE 02'
      NREC      =  COUNT

      QUATS( 0, 1 ) = 1.0D0
      QUATS( 1, 1 ) = 0.0D0
      QUATS( 2, 1 ) = 0.0D0
      QUATS( 3, 1 ) = 0.0D0

      QUATS( 0, 2 ) = DCOS ( 0.1D0 )
      QUATS( 1, 2 ) = DSIN ( 0.1D0 )
      QUATS( 2, 2 ) = 0.0D0
      QUATS( 3, 2 ) = 0.0D0


      QUATS( 0, 3 ) = DCOS ( 0.15D0 )
      QUATS( 1, 3 ) = DSIN ( 0.15D0 )
      QUATS( 2, 3 ) = 0.0D0
      QUATS( 3, 3 ) = 0.0D0

      QUATS( 0, 4 ) = DCOS ( 0.25D0 )
      QUATS( 1, 4 ) = DSIN ( 0.25D0 )
      QUATS( 2, 4 ) = 0.0D0
      QUATS( 3, 4 ) = 0.0D0

      AVVS ( 1, 1 ) = 1.0D0
      AVVS ( 2, 1 ) = 0.0D0
      AVVS ( 3, 1 ) = 0.0D0

      AVVS ( 1, 2 ) = 0.5D0
      AVVS ( 2, 2 ) = 0.0D0
      AVVS ( 3, 2 ) = 0.0D0

      AVVS ( 1, 3 ) = 0.3D0
      AVVS ( 2, 3 ) = 0.0D0
      AVVS ( 3, 3 ) = 0.0D0

      AVVS ( 1, 4 ) = 0.2D0
      AVVS ( 2, 4 ) = 0.0D0
      AVVS ( 3, 4 ) = 0.0D0

!
!     Check all exceptions.
!
      CALL TCASE ( 'Case 1 with invalid number of records.' )

         NREC = 0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

         NREC = 4

      CALL TCASE ( 'Case 2 with invalid number of records.' )

         NREC = -1

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

         NREC = 4

      CALL TCASE ( 'Case with invalid descriptor begin time.' )

         BEGTIM    =  3.0D0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

         BEGTIM    =  2.0D0

      CALL TCASE ( 'Case with invalid descriptor end time.' )

         ENDTIM    = 11.0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

         ENDTIM    = 12.0

      CALL TCASE ( 'Case with invalid reference frame name.' )

         REF       = 'MY_FRAME'

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

         REF       = 'J2000'

      CALL TCASE ( 'Case with non-printing characters in SEGID.' )

         SEGID(5:5) = CHAR(7)

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )

         SEGID(5:5) = ' '

      CALL TCASE ( 'Case with SEGID that is too long.' )

         SEGID     = '12345678901234567890123456789012345678901'

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )

         SEGID     = 'TEST CKTYPE 03'

      CALL TCASE ( 'Case with negative first SCLK time.' )

         START(1)  =  -2.0D0
         BEGTIM    =  -2.0D0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKTIME)', OK )

         START(1)  =  2.0D0
         BEGTIM    =  2.0D0

      CALL TCASE ( 'Case 1 with degenerate first interval.' )

         STOP(1)   = START(1)

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

         STOP (1) = START(2)

      CALL TCASE ( 'Case 2 with degenerate first interval.' )

         STOP (1)  = START(1) - 1.D0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

         STOP (1) = START(2)

      CALL TCASE ( 'Case 1 with degenerate interval.' )

         STOP (2)  = START(2)

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

         STOP (2)  = START(3)

      CALL TCASE ( 'Case 2 with degenerate interval.' )

         STOP (2)  = START(2) - 1.D0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

         STOP (2) = START(3)

      CALL TCASE ( 'Case 1 with times out of order.' )

         START (3) = START(2)

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

         START(3) =  8.0D0

      CALL TCASE ( 'Case 2 with times out of order.' )

         START (3) = START(2) - 1.D0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

         START(3) =  8.0D0

      CALL TCASE ( 'Case 2 with bad stop time.' )

         STOP (2) = START(3) + 1.D0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(BADSTOPTIME)', OK )

         STOP (2) = START(3)

      CALL TCASE ( 'Case with a zero quaternion.' )

         CALL CLEARD ( 4, QUATS( 0, 4 ) )

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(ZEROQUATERNION)',  OK )

         QUATS( 0, 4 ) = DCOS ( 0.25D0 )

!
!     Finally, check one good case.
!
      CALL TCASE ( 'All quaternions unit length.' )

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                      NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CKCLS ( HANDLE )
         CALL CKLPF ( 'ckw02.bc', HANDLE )

         CALL CKGPAV ( INST, 4.0D0, 1.0D0, 'J2000', &
                       CMAT, AV,    CLKOUT, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', 4.0D0, 0.0D0, OK )

         CALL Q2M    ( QUATS(0,2), EMAT )
         CALL CHCKAD ( 'CMAT', CMAT, '~', EMAT,      9, 1.0D-14, OK)
         CALL CHCKAD ( 'AV',   AV,   '~', AVVS(1,2), 3, 1.0D-14, OK)
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

         CALL CKUPF ( HANDLE )
         CALL KILFIL( 'ckw02.bc' )

      CALL T_SUCCESS ( OK )
      RETURN
      END
