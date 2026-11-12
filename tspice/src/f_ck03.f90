!$Procedure F_CK03 ( CK data type 03 tests )

      SUBROUTINE F_CK03 ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise routines associated with CK data type 03.
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
!     This routine tests routines that write and read type 03 CK
!     data:
!
!        CKGR03
!        CKNR03
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    TSPICE Version 3.0.0, 17-OCT-2021 (JDR)
!
!        Renamed test family to F_CK03. Included test cases for
!        SPICELIB routines CKNR03 and CKGR03.
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


! $ Index_Entries
!
!     test routines associated with CK data type 03
!
!-&


!
!     SPICELIB functions
!

!
!     Local Parameters
!
      CHARACTER*(*)         CK03E
      PARAMETER           ( CK03E  = 'test03err.bc' )

      CHARACTER*(*)         CK03T1
      PARAMETER           ( CK03T1 = 'ck03t1.bc' )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 60 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               COUNT
      PARAMETER           ( COUNT = 4 )

!
!     Local Variables
!
      CHARACTER*(SIDLEN)    SEGID
      CHARACTER*(FRNMLN)    REF

      DOUBLE PRECISION      AV     ( 3 )
      DOUBLE PRECISION      AVVS   ( 3, COUNT )
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      CLKOUT
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      DESCR  ( 5 )
      DOUBLE PRECISION      DSCTMP ( 5 )
      DOUBLE PRECISION      EMAT   ( 3, 3 )
      DOUBLE PRECISION      ENDTIM
      DOUBLE PRECISION      QUATS  ( 0:3, COUNT )
      DOUBLE PRECISION      RECRD  ( 8 )
      DOUBLE PRECISION      SCLKDP (      COUNT )
      DOUBLE PRECISION      STARTS (      COUNT )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               INST
      INTEGER               NINTS
      INTEGER               NREC

      LOGICAL               AVFLAG
      LOGICAL               FOUND


!
!     Saved values
!
      SAVE

!
!     Open the test family.
!
      CALL TOPEN ( 'F_CK03' )


!
!     *****************************************************************
!
!     CKW03 error cases:
!
!     *****************************************************************
!
!     Test CKW03:  start out with error handling.
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 setup' )

!
!     Create a segment identifier.
!
      SEGID = 'CK type 03 test segment'

!
!     Open a new CK file.
!
      IF ( EXISTS( CK03E ) ) THEN
         CALL DELFIL( CK03E )
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN( CK03E, 'Type 03 CK error file', 4, HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )


      BEGTIM    =  2.0D0
      ENDTIM    = 10.0D0

      SCLKDP(1) =  2.0D0
      SCLKDP(2) =  4.0D0
      SCLKDP(3) =  8.0D0
      SCLKDP(4) = 10.0D0

      INST      = -1003
      REF       = 'J2000'
      AVFLAG    = .TRUE.
      NINTS     =  1
      STARTS(1) =  BEGTIM
      NREC      = 4

      QUATS( 0, 1 ) = 1.0D0
      QUATS( 1, 1 ) = 0.0D0
      QUATS( 2, 1 ) = 0.0D0
      QUATS( 3, 1 ) = 0.0D0

      QUATS( 0, 2 ) = COS ( 0.1D0 )
      QUATS( 1, 2 ) = SIN ( 0.1D0 )
      QUATS( 2, 2 ) = 0.0D0
      QUATS( 3, 2 ) = 0.0D0


      QUATS( 0, 3 ) = COS ( 0.15D0 )
      QUATS( 1, 3 ) = SIN ( 0.15D0 )
      QUATS( 2, 3 ) = 0.0D0
      QUATS( 3, 3 ) = 0.0D0

      QUATS( 0, 4 ) = COS ( 0.25D0 )
      QUATS( 1, 4 ) = SIN ( 0.25D0 )
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
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: case 1 with invalid ' &
              //   'number of records.' )

      NREC      = 0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

      NREC      = 4

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: case 2 with invalid ' &
              //   'number of records.' )

      NREC      = -1

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

      NREC      = 4


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: case 1 with invalid ' &
              //   'number of intervals.' )

      NINTS     =  0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMINT)', OK )

      NINTS     =  1


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: case 2 with invalid ' &
              //   'number of intervals.' )

      NINTS     =  -1

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMINT)', OK )

      NINTS     =  1


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: invalid descriptor begin time.' )

      BEGTIM    =  3.0D0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

      BEGTIM    =  2.0D0


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: invalid descriptor end time.' )

      ENDTIM    =  9.0D0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

      ENDTIM    = 10.0D0


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: invalid reference frame name.' )

      REF       = 'MY_FRAME'

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

      REF       = 'J2000'


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: non-printing characters ' &
              //   'in SEGID.' )

      SEGID(3:3) = CHAR(7)

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )

      SEGID(3:3) = ' '


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: SEGID that is too long.' )

      SEGID     = '12345678901234567890123456789012345678901'

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )

      SEGID     = 'CK type 03 test segment'

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: negative first SCLK time.' )

      SCLKDP(1) =  -2.0D0
      BEGTIM    =  -2.0D0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKTIME)', OK )

      SCLKDP(1) =  2.0D0
      BEGTIM    =  2.0D0


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: case 1 with SCLK times ' &
              //   'out of order.' )

      SCLKDP(2) =  8.0D0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      SCLKDP(2) =  4.0D0


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: case 2 with SCLK times ' &
              //   'out of order.' )

      SCLKDP(2) =  9.0D0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      SCLKDP(2) =  4.0D0


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: mismatching first times.' )

      STARTS(1) =  SCLKDP(2)

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESDONTMATCH)', OK )

      STARTS(1) =  BEGTIM


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: case 1 with interval ' &
              //   'starts out of order.' )

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


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: case 2 with interval ' &
              //   'starts out of order.' )

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


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: invalid interval start times.' )

      NINTS     =  2
      STARTS(1) =  BEGTIM
      STARTS(2) =  ( SCLKDP(2) + SCLKDP(3) ) / 2.D0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTARTTIME)', OK )

      NINTS     =  1
      STARTS(1) =  BEGTIM


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW03 error case: zero quaternion.' )

      CALL CLEARD ( 4, QUATS( 0, 4 ) )

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROQUATERNION)', OK )

      QUATS( 0, 4 ) = COS ( 0.25D0 )


!
!     *****************************************************************
!
!     End of CKW03 error cases:
!
!     *****************************************************************
!
!     Close the CK file at the DAF level; CKCLS won't close
!     a file without segments.
!
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
!     *****************************************************************
!
!      CKW03 Non-error cases:
!
!     *****************************************************************
!
!     Trivial case: write an CK containing all quaternions of
!     unit length.
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Test CKW03: All quaternions unit length.' )

!
!     Open new C-kernel.
!
      IF ( EXISTS( CK03T1 ) ) THEN
         CALL DELFIL( CK03T1 )
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN  ( CK03T1, CK03T1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Write segment.
!
      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG, &
                   SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS, &
                   STARTS                                      )


!
!     Close kernel.
!
      CALL CKCLS  ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the kernel.
!
      CALL FURNSH ( CK03T1 )

      CALL CKGPAV ( INST, 4.0D0, 1.0D0, 'J2000', &
                    CMAT, AV,    CLKOUT, FOUND )

      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', 4.0D0, 0.0D0, OK )

      CALL Q2M    ( QUATS(0,2), EMAT )
      CALL CHCKAD ( 'CMAT', CMAT, '~', EMAT,      9, VTIGHT, OK )
      CALL CHCKAD ( 'AV',   AV,   '~', AVVS(1,2), 3, VTIGHT, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

      CALL UNLOAD ( CK03T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!      CKNR03 Error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKNR03 error case: segment not of data type 3.' )

!
!     Load the a CK file and get the handle.
!
      CALL DAFOPR ( CK03T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Find the first segment.
!
      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the data type to anything other than 3, e.g. 0
!     Data type and angular velocity flag are given by DESCR(4).
!     Save the value in order to restore it.
!
      CALL MOVED ( DESCR, 5, DSCTMP )
      DSCTMP(4) = 0.D0

      CALL CKNR03 ( HANDLE, DSCTMP, NREC )
      CALL CHCKXC ( .TRUE., 'SPICE(CKWRONGDATATYPE)', OK )

!
!     *****************************************************************
!
!      CKGR03 Error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKGR03 error case: segment not of data type 3.' )

!
!     Call CKGR03 using the "corrupted" DESCR from previous test.
!     Note that the CK file we are using has one segment with 4
!     records.
!
      CALL CKGR03 ( HANDLE, DSCTMP, 1, RECRD )
      CALL CHCKXC ( .TRUE., 'SPICE(CKWRONGDATATYPE)', OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKGR03 error case: case 1 with record which ' &
              //   'does not exist.' )

      CALL CKGR03 ( HANDLE, DESCR, 0, RECRD )
      CALL CHCKXC ( .TRUE., 'SPICE(CKNONEXISTREC)', OK )


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKGR03 error case: case 2 with record which ' &
              //   'does not exist.' )

      CALL CKGR03 ( HANDLE, DESCR, 10, RECRD )
      CALL CHCKXC ( .TRUE., 'SPICE(CKNONEXISTREC)', OK )

!
!     Close the CK file.
!
      CALL CKCLS  ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     *****************************************************************
!
!      CKNR03 and CKGR03 non-error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE  ( 'Examine the segment in the file CK03T1.' )

!
!     Load the a CK file and get the handle.
!
      CALL DAFOPR ( CK03T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Find the first (only) segment.
!
      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     How many records does this segment contain?
!
      CALL CKNR03 ( HANDLE, DESCR, NREC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that CKNR03 returns the expected number of records.
!
      CALL CHCKSI ( 'NREC',NREC, '=', COUNT, 0, OK )

      DO I = 1, NREC

!
!        Get the Ith pointing instance in the segment.
!
         CALL CKGR03 ( HANDLE, DESCR, I, RECRD )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check the time, quaternion and av.
!
         CALL CHCKSD ( 'SCLKDP', RECRD(1), '=', SCLKDP(I), 0.0D0, OK )

         CALL CHCKAD ( 'QUATS', RECRD(2),   '=', &
                                QUATS(0,I),  4,  0.0D0, OK )

         CALL CHCKAD ( 'AVVS', RECRD(6),  '=', &
                               AVVS(1,I), 3,   0.0D0, OK )

      END DO

!
!     Close the CK file.
!
      CALL CKCLS  ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     *****************************************************************
!
!     Clean up
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE  ( 'Clean up:  delete CK files.' )


      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF (  EXISTS( CK03E )  ) THEN
         CALL DELFIL ( CK03E )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF (  EXISTS( CK03T1 )  ) THEN
         CALL DELFIL ( CK03T1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END

