!$Procedure      F_CKW03 ( Family of checks for CKW03 )

      SUBROUTINE F_CKW03 ( OK )
      use pepper_legacy

! $ Abstract
!
!     This family of tests checks all CKW03 exceptions and that a good
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
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the routine
!
!        CKW03
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
!     N.J. Bachman     (JPL)
!     E.D. Wright      (JPL)
!     B.V. Semenov     (JPL)
!
! $ Version
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
!        This test routine now checks all CKW03 exceptions.
!
!-    TSPICE Version 1.1.0, 15-FEB-2001 (EDW)
!
!        Altered Test Case 2 to use an error tolerance of 1.D-14
!        instead of an exact equality. That test failed due to roundoff
!        error (on the order of 1.D-16) when compiled under the
!        Macintosh Absoft FORTRAN compiler.
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
      INTEGER               COUNT
      PARAMETER           ( COUNT = 4 )

      INTEGER               HANDLE
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      ENDTIM
      INTEGER               INST
      CHARACTER*(8)         REF
      LOGICAL               AVFLAG
      CHARACTER*(42)        SEGID
      INTEGER               NREC
      DOUBLE PRECISION      SCLKDP (      COUNT )
      DOUBLE PRECISION      QUATS  ( 0:3, COUNT )
      DOUBLE PRECISION      AVVS   (   3, COUNT )
      INTEGER               NINTS
      DOUBLE PRECISION      STARTS (      COUNT )

      DOUBLE PRECISION      EMAT ( 3, 3 )
      DOUBLE PRECISION      AV   ( 3 )
      DOUBLE PRECISION      CMAT ( 3, 3 )
      DOUBLE PRECISION      CLKOUT
      LOGICAL               FOUND


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_CKW03' )

      CALL KILFIL( 'ckw03.bc' )
      CALL CKOPN ( 'ckw03.bc', 'TestCK03', 1000, HANDLE )

      BEGTIM    =  2.0D0
      ENDTIM    = 10.0D0

      SCLKDP(1) =  2.0D0
      SCLKDP(2) =  4.0D0
      SCLKDP(3) =  8.0D0
      SCLKDP(4) = 10.0D0

      INST      = -1003
      REF       = 'J2000'
      SEGID     = 'TEST CKTYPE 03'
      AVFLAG    = .TRUE.
      NINTS     =  1
      STARTS(1) =  BEGTIM

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

      NREC = 4

!
!     Check all exceptions.
!
      CALL TCASE ( 'Case 1 with invalid number of records.' )

         NREC = 0

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

         NREC = 4

      CALL TCASE ( 'Case 2 with invalid number of records.' )

         NREC = -1

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

         NREC = 4

      CALL TCASE ( 'Case 1 with invalid number of intervals.' )

         NINTS     =  0

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMINT)', OK )

         NINTS     =  1

      CALL TCASE ( 'Case 2 with invalid number of intervals.' )

         NINTS     =  -1

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMINT)', OK )

         NINTS     =  1

      CALL TCASE ( 'Case with invalid descriptor begin time.' )

         BEGTIM    =  3.0D0

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

         BEGTIM    =  2.0D0

      CALL TCASE ( 'Case with invalid descriptor end time.' )

         ENDTIM    =  9.0D0

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

         ENDTIM    = 10.0D0

      CALL TCASE ( 'Case with invalid reference frame name.' )

         REF       = 'MY_FRAME'

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

         REF       = 'J2000'

      CALL TCASE ( 'Case with non-printing characters in SEGID.' )

         SEGID(5:5) = CHAR(7)

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )

         SEGID(5:5) = ' '

      CALL TCASE ( 'Case with SEGID that is too long.' )

         SEGID     = '12345678901234567890123456789012345678901'

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )

         SEGID     = 'TEST CKTYPE 03'

      CALL TCASE ( 'Case with negative first SCLK time.' )

         SCLKDP(1) =  -2.0D0
         BEGTIM    =  -2.0D0

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKTIME)', OK )

         SCLKDP(1) =  2.0D0
         BEGTIM    =  2.0D0

      CALL TCASE ( 'Case 1 with SCLK times out of order.' )

         SCLKDP(2) =  8.0D0

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

         SCLKDP(2) =  4.0D0

      CALL TCASE ( 'Case 2 with SCLK times out of order.' )

         SCLKDP(2) =  9.0D0

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

         SCLKDP(2) =  4.0D0

      CALL TCASE ( 'Case with mismatching first times.' )

         STARTS(1) =  SCLKDP(2)

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(TIMESDONTMATCH)', OK )

         STARTS(1) =  BEGTIM

      CALL TCASE ( 'Case 1 with interval starts out of order.' )

         NINTS     =  3
         STARTS(1) = BEGTIM
         STARTS(2) = SCLKDP(2)
         STARTS(3) = SCLKDP(2)

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

         NINTS     =  1
         STARTS(1) =  BEGTIM

      CALL TCASE ( 'Case 2 with interval starts out of order.' )

         NINTS     =  3
         STARTS(1) =  BEGTIM
         STARTS(2) =  SCLKDP(3)
         STARTS(3) =  SCLKDP(2)

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

         NINTS     =  1
         STARTS(1) =  BEGTIM

      CALL TCASE ( 'Case with invalid interval start times.' )

         NINTS     =  2
         STARTS(1) =  BEGTIM
         STARTS(2) =  ( SCLKDP(2) + SCLKDP(3) ) / 2.D0

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTARTTIME)', OK )

         NINTS     =  1
         STARTS(1) =  BEGTIM

      CALL TCASE ( 'Case with a zero quaternion.' )

         CALL CLEARD ( 4, QUATS( 0, 4 ) )

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )

         CALL CHCKXC ( .TRUE., 'SPICE(ZEROQUATERNION)',  OK )

         QUATS( 0, 4 ) = DCOS ( 0.25D0 )

!
!     Finally, check one good case.
!
      CALL TCASE ( 'All quaternions unit length.' )

         CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                      SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                      STARTS                                      )



         CALL CKCLS ( HANDLE )

         CALL CKLPF ( 'ckw03.bc', HANDLE )

         CALL CKGPAV ( INST, 4.0D0, 1.0D0, 'J2000', &
                       CMAT, AV,    CLKOUT, FOUND )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', 4.0D0, 0.0D0, OK )

         CALL Q2M    ( QUATS(0,2), EMAT )
         CALL CHCKAD ( 'CMAT', CMAT, '~', EMAT,      9, 1.0D-14, OK)
         CALL CHCKAD ( 'AV',   AV,   '~', AVVS(1,2), 3, 1.0D-14, OK)
         CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )


         CALL CKUPF ( HANDLE )
         CALL KILFIL( 'ckw03.bc' )

      CALL T_SUCCESS ( OK )
      RETURN
      END

