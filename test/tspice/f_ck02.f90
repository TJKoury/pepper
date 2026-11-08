!$Procedure F_CK02 ( CK data type 02 tests )

      SUBROUTINE F_CK02 ( OK )

! $ Abstract
!
!     Exercise routines associated with CK data type 02.
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
!     This routine tests routines that write and read type 02 CK
!     data:
!
!        CKGR02
!        CKNR02
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
!-    TSPICE Version 3.0.0, 17-OCT-2021 (JDR) (BVS)
!
!        Renamed test family to F_CK02. Included test cases for
!        SPICELIB routines CKNR02 and CKGR02.
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
!     test routines associated with CK data type 02
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!


!
!     Local Parameters
!
      CHARACTER*(*)         CK02T1
      PARAMETER           ( CK02T1 = 'ckw02.bc' )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      INTEGER               COUNT
      PARAMETER           ( COUNT = 4 )

      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN = 42 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

!
!     Local Variables
!
      CHARACTER*(FRNMLN)    REF
      CHARACTER*(SIDLEN)    SEGID

      DOUBLE PRECISION      AV     ( 3 )
      DOUBLE PRECISION      AVVS   (   3, COUNT )
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      CLKOUT
      DOUBLE PRECISION      CMAT   ( 3, 3 )
      DOUBLE PRECISION      EMAT   ( 3, 3 )
      DOUBLE PRECISION      ENDTIM
      DOUBLE PRECISION      DESCR  (  5 )
      DOUBLE PRECISION      DSCTMP (  5 )
      DOUBLE PRECISION      RECRD  ( 10 )
      DOUBLE PRECISION      START  (      COUNT )
      DOUBLE PRECISION      STOPT  (      COUNT )
      DOUBLE PRECISION      QUATS  ( 0:3, COUNT )
      DOUBLE PRECISION      RATES  (      COUNT )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               INST
      INTEGER               NREC

      LOGICAL               FOUND

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_CK02' )

      CALL KILFIL( CK02T1 )

      CALL CKOPN ( CK02T1, 'TestCK02', 1000, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      BEGTIM    =  2.0
      ENDTIM    = 12.0

      START(1) =  2.0D0
      START(2) =  4.0D0
      START(3) =  8.0D0
      START(4) = 10.0D0

      STOPT(1) = START(2)
      STOPT(2) = START(3)
      STOPT(3) = START(4)
      STOPT(4) = ENDTIM

      RATES(1) = 1.0D0
      RATES(2) = 0.75D0
      RATES(3) = 0.5D0
      RATES(4) = 0.25D0

      INST     = -1002
      REF      = 'J2000'
      SEGID    = 'TEST CKTYPE 02'
      NREC     =  COUNT

      QUATS(0,1) = 1.0D0
      QUATS(1,1) = 0.0D0
      QUATS(2,1) = 0.0D0
      QUATS(3,1) = 0.0D0

      QUATS(0,2) = DCOS ( 0.1D0 )
      QUATS(1,2) = DSIN ( 0.1D0 )
      QUATS(2,2) = 0.0D0
      QUATS(3,2) = 0.0D0

      QUATS(0,3) = DCOS ( 0.15D0 )
      QUATS(1,3) = DSIN ( 0.15D0 )
      QUATS(2,3) = 0.0D0
      QUATS(3,3) = 0.0D0

      QUATS(0,4) = DCOS ( 0.25D0 )
      QUATS(1,4) = DSIN ( 0.25D0 )
      QUATS(2,4) = 0.0D0
      QUATS(3,4) = 0.0D0

      AVVS(1,1) = 1.0D0
      AVVS(2,1) = 0.0D0
      AVVS(3,1) = 0.0D0

      AVVS(1,2) = 0.5D0
      AVVS(2,2) = 0.0D0
      AVVS(3,2) = 0.0D0

      AVVS(1,3) = 0.3D0
      AVVS(2,3) = 0.0D0
      AVVS(3,3) = 0.0D0

      AVVS(1,4) = 0.2D0
      AVVS(2,4) = 0.0D0
      AVVS(3,4) = 0.0D0

!
!     *****************************************************************
!
!     CKW02 error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case 1 with invalid ' // &
                   'number of records.' )

      NREC = 0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

      NREC = 4


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case 2 with invalid ' // &
                   'number of records.' )

      NREC = -1

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

      NREC = 4


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case with invalid descriptor ' // &
                   'begin time.' )

      BEGTIM    =  3.0D0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

      BEGTIM    =  2.0D0


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case with invalid descriptor ' // &
                   'end time.' )

      ENDTIM    = 11.0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

      ENDTIM    = 12.0


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case with invalid reference ' // &
                   'frame name.' )

      REF       = 'MY_FRAME'

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

      REF       = 'J2000'


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case with non-printing ' // &
                   'characters in SEGID.' )

      SEGID(5:5) = CHAR(7)

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )

      SEGID(5:5) = ' '


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case with SEGID that is ' // &
                   'too long.' )

      SEGID     = '12345678901234567890123456789012345678901'

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )

      SEGID     = 'TEST CKTYPE 02'


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case with negative first ' // &
                   'SCLK time.' )

      START(1)  =  -2.0D0
      BEGTIM    =  -2.0D0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKTIME)', OK )

      START(1)  =  2.0D0
      BEGTIM    =  2.0D0


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case 1 with degenerate first ' // &
                   'interval.' )

      STOPT(1)   = START(1)

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

      STOPT(1) = START(2)


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case 2 with degenerate first ' // &
                   'interval.' )

      STOPT(1)  = START(1) - 1.D0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

      STOPT(1) = START(2)


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case 1 with degenerate ' // &
                   'interval.' )

      STOPT(2)  = START(2)

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

      STOPT(2)  = START(3)


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case 2 with degenerate ' // &
                   'interval.' )

      STOPT(2)  = START(2) - 1.D0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

      STOPT(2) = START(3)


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case 1 with times out ' // &
                   'of order.' )

      START(3) = START(2)

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      START(3) =  8.0D0


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case 2 with times out ' // &
                   'of order.' )

      START(3) = START(2) - 1.D0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      START(3) =  8.0D0


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case with bad stop time.' )

      STOPT(2) = START(3) + 1.D0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(BADSTOPTIME)', OK )

      STOPT(2) = START(3)


!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKW02 error case: Case with a zero quaternion.' )

      CALL CLEARD ( 4, QUATS(0,4) )

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                   NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROQUATERNION)',  OK )

      QUATS(0,4) = DCOS ( 0.25D0 )


!
!     *****************************************************************
!
!     CKW02 Non-error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Trivial case: write a CK containing all quaternions of
!     unit length.
!
      CALL TCASE  ( 'Test CKW02: All quaternions unit length.' )

      CALL CKW02  ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID, &
                    NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CKCLS  ( HANDLE )
      CALL CKLPF  ( CK02T1, HANDLE )

      CALL CKGPAV ( INST, 4.0D0, 1.0D0, 'J2000', &
                    CMAT, AV,    CLKOUT, FOUND )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', 4.0D0, 0.0D0, OK )

      CALL Q2M    ( QUATS(0,2), EMAT )
      CALL CHCKAD ( 'CMAT', CMAT, '~', EMAT,      9, VTIGHT, OK)
      CALL CHCKAD ( 'AV',   AV,   '~', AVVS(1,2), 3, VTIGHT, OK)
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

      CALL UNLOAD ( CK02T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     CKNR02 Error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKNR02 error case: segment not of data type 2.' )

!
!     Load the a CK file and get the handle.
!
      CALL DAFOPR ( CK02T1, HANDLE )
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
!     Set the data type to anything other than 2, e.g. 0
!     Data type and angular velocity flag are given by DESCR(4).
!     Save the value in order to restore it.
!
      CALL MOVED ( DESCR, 5, DSCTMP )
      DSCTMP(4) = 0.D0

      CALL CKNR02 ( HANDLE, DSCTMP, NREC )
      CALL CHCKXC ( .TRUE., 'SPICE(CKWRONGDATATYPE)', OK )

!
!     *****************************************************************
!
!     CKGR02 Error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKGR02 error case: segment not of data type 2.' )

!
!     Call CKGR02 using the "corrupted" DESCR from previous test.
!     Note that the CK file we are using has one segment with 4
!     records.
!
      CALL CKGR02 ( HANDLE, DSCTMP, 1, RECRD )
      CALL CHCKXC ( .TRUE., 'SPICE(CKWRONGDATATYPE)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKGR02 error case: case 1 with record which ' &
              //   'does not exist.' )

      CALL CKGR02 ( HANDLE, DESCR, 0, RECRD )
      CALL CHCKXC ( .TRUE., 'SPICE(CKNONEXISTREC)', OK )

!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'CKGR02 error case: case 2 with record which ' &
              //   'does not exist.' )

      CALL CKGR02 ( HANDLE, DESCR, 10, RECRD )
      CALL CHCKXC ( .TRUE., 'SPICE(CKNONEXISTREC)', OK )

!
!     Close the CK file.
!
      CALL CKCLS  ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     *****************************************************************
!
!     CKNR02 and CKGR02 non-error cases:
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
      CALL TCASE  ( 'Examine the segment in the file CK02T1.' )

!
!     Load the a CK file and get the handle.
!
      CALL DAFOPR ( CK02T1, HANDLE )
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
      CALL CKNR02 ( HANDLE, DESCR, NREC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that CKNR02 returns the expected number of records.
!
      CALL CHCKSI ( 'NREC', NREC, '=', COUNT, 0, OK )

      DO I = 1, NREC

!
!        Get the Ith pointing instance in the segment.
!
         CALL CKGR02 ( HANDLE, DESCR, I, RECRD )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check the times, rate, quaternion and av.
!
         CALL CHCKSD ( 'BEGTIM', RECRD(1), '=', START(I), 0.0D0, OK )
         CALL CHCKSD ( 'ENDTIM', RECRD(2), '=', STOPT(I), 0.0D0, OK )
         CALL CHCKSD ( 'RATE',   RECRD(3), '=', RATES(I), 0.0D0, OK )

         CALL CHCKAD ( 'QUATS',  RECRD(4), '=', &
                                         QUATS(0,I),  4,  0.0D0, OK )

         CALL CHCKAD ( 'AVVS',   RECRD(8), '=', &
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
      CALL CKUPF ( HANDLE )
      CALL KILFIL( CK02T1 )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END

