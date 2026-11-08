C$Procedure F_CK02 ( CK data type 02 tests )

      SUBROUTINE F_CK02 ( OK )

C$ Abstract
C
C     Exercise routines associated with CK data type 02.
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
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests routines that write and read type 02 CK
C     data:
C
C        CKGR02
C        CKNR02
C        CKW02
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    TSPICE Version 3.0.0, 17-OCT-2021 (JDR) (BVS)
C
C        Renamed test family to F_CK02. Included test cases for
C        SPICELIB routines CKNR02 and CKGR02.
C
C-    TSPICE Version 2.0.0, 01-JUN-2010 (NJB)
C
C        Updated to account for error handling change in
C        the SPICELIB routine CKW05: now that routine tests
C        for zero-length quaternions rather than non-unit-length
C        quaternions.
C
C-    TSPICE Version 1.2.0, 26-SEP-2005 (BVS)
C
C        This test routine now checks all CKW02 exceptions.
C
C-    TSPICE Version 1.1.0, 15-FEB-2001 (EDW)
C
C        Altered Test case 2 to use an error tolerance of 1.D-14
C        instead of an exact equality. That test failed due to roundoff
C        error (on the order of 1.D-16) when compiled under the
C        Macintosh Absoft FORTRAN compiler.
C
C-&


C$ Index_Entries
C
C     test routines associated with CK data type 02
C
C-&


C
C     Test Utility Functions
C


C
C     SPICELIB Functions
C


C
C     Local Parameters
C
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

C
C     Local Variables
C
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

C
C     Begin every test family with an open call.
C
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

C
C     *****************************************************************
C
C     CKW02 error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case 1 with invalid ' //
     .             'number of records.' )

      NREC = 0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

      NREC = 4


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case 2 with invalid ' //
     .             'number of records.' )

      NREC = -1

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

      NREC = 4


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case with invalid descriptor ' //
     .             'begin time.' )

      BEGTIM    =  3.0D0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

      BEGTIM    =  2.0D0


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case with invalid descriptor ' //
     .             'end time.' )

      ENDTIM    = 11.0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

      ENDTIM    = 12.0


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case with invalid reference ' //
     .             'frame name.' )

      REF       = 'MY_FRAME'

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

      REF       = 'J2000'


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case with non-printing ' //
     .             'characters in SEGID.' )

      SEGID(5:5) = CHAR(7)

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )

      SEGID(5:5) = ' '


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case with SEGID that is ' //
     .             'too long.' )

      SEGID     = '12345678901234567890123456789012345678901'

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )

      SEGID     = 'TEST CKTYPE 02'


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case with negative first ' //
     .             'SCLK time.' )

      START(1)  =  -2.0D0
      BEGTIM    =  -2.0D0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKTIME)', OK )

      START(1)  =  2.0D0
      BEGTIM    =  2.0D0


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case 1 with degenerate first ' //
     .             'interval.' )

      STOPT(1)   = START(1)

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

      STOPT(1) = START(2)


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case 2 with degenerate first ' //
     .             'interval.' )

      STOPT(1)  = START(1) - 1.D0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

      STOPT(1) = START(2)


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case 1 with degenerate ' //
     .             'interval.' )

      STOPT(2)  = START(2)

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

      STOPT(2)  = START(3)


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case 2 with degenerate ' //
     .             'interval.' )

      STOPT(2)  = START(2) - 1.D0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

      STOPT(2) = START(3)


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case 1 with times out ' //
     .             'of order.' )

      START(3) = START(2)

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      START(3) =  8.0D0


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case 2 with times out ' //
     .             'of order.' )

      START(3) = START(2) - 1.D0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      START(3) =  8.0D0


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case with bad stop time.' )

      STOPT(2) = START(3) + 1.D0

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(BADSTOPTIME)', OK )

      STOPT(2) = START(3)


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW02 error case: Case with a zero quaternion.' )

      CALL CLEARD ( 4, QUATS(0,4) )

      CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .             NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROQUATERNION)',  OK )

      QUATS(0,4) = DCOS ( 0.25D0 )


C
C     *****************************************************************
C
C     CKW02 Non-error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Trivial case: write a CK containing all quaternions of
C     unit length.
C
      CALL TCASE  ( 'Test CKW02: All quaternions unit length.' )

      CALL CKW02  ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .              NREC,   START,  STOPT,  QUATS, AVVS, RATES )

      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CKCLS  ( HANDLE )
      CALL CKLPF  ( CK02T1, HANDLE )

      CALL CKGPAV ( INST, 4.0D0, 1.0D0, 'J2000',
     .              CMAT, AV,    CLKOUT, FOUND )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', 4.0D0, 0.0D0, OK )

      CALL Q2M    ( QUATS(0,2), EMAT )
      CALL CHCKAD ( 'CMAT', CMAT, '~', EMAT,      9, VTIGHT, OK)
      CALL CHCKAD ( 'AV',   AV,   '~', AVVS(1,2), 3, VTIGHT, OK)
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

      CALL UNLOAD ( CK02T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     CKNR02 Error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKNR02 error case: segment not of data type 2.' )

C
C     Load the a CK file and get the handle.
C
      CALL DAFOPR ( CK02T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Find the first segment.
C
      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the data type to anything other than 2, e.g. 0
C     Data type and angular velocity flag are given by DESCR(4).
C     Save the value in order to restore it.
C
      CALL MOVED ( DESCR, 5, DSCTMP )
      DSCTMP(4) = 0.D0

      CALL CKNR02 ( HANDLE, DSCTMP, NREC )
      CALL CHCKXC ( .TRUE., 'SPICE(CKWRONGDATATYPE)', OK )

C
C     *****************************************************************
C
C     CKGR02 Error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKGR02 error case: segment not of data type 2.' )

C
C     Call CKGR02 using the "corrupted" DESCR from previous test.
C     Note that the CK file we are using has one segment with 4
C     records.
C
      CALL CKGR02 ( HANDLE, DSCTMP, 1, RECRD )
      CALL CHCKXC ( .TRUE., 'SPICE(CKWRONGDATATYPE)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKGR02 error case: case 1 with record which '
     .        //   'does not exist.' )

      CALL CKGR02 ( HANDLE, DESCR, 0, RECRD )
      CALL CHCKXC ( .TRUE., 'SPICE(CKNONEXISTREC)', OK )

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKGR02 error case: case 2 with record which '
     .        //   'does not exist.' )

      CALL CKGR02 ( HANDLE, DESCR, 10, RECRD )
      CALL CHCKXC ( .TRUE., 'SPICE(CKNONEXISTREC)', OK )

C
C     Close the CK file.
C
      CALL CKCLS  ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     *****************************************************************
C
C     CKNR02 and CKGR02 non-error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE  ( 'Examine the segment in the file CK02T1.' )

C
C     Load the a CK file and get the handle.
C
      CALL DAFOPR ( CK02T1, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Find the first (only) segment.
C
      CALL DAFBFS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFFNA ( FOUND  )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DAFGS  ( DESCR )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     How many records does this segment contain?
C
      CALL CKNR02 ( HANDLE, DESCR, NREC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check that CKNR02 returns the expected number of records.
C
      CALL CHCKSI ( 'NREC', NREC, '=', COUNT, 0, OK )

      DO I = 1, NREC

C
C        Get the Ith pointing instance in the segment.
C
         CALL CKGR02 ( HANDLE, DESCR, I, RECRD )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check the times, rate, quaternion and av.
C
         CALL CHCKSD ( 'BEGTIM', RECRD(1), '=', START(I), 0.0D0, OK )
         CALL CHCKSD ( 'ENDTIM', RECRD(2), '=', STOPT(I), 0.0D0, OK )
         CALL CHCKSD ( 'RATE',   RECRD(3), '=', RATES(I), 0.0D0, OK )

         CALL CHCKAD ( 'QUATS',  RECRD(4), '=',
     .                                   QUATS(0,I),  4,  0.0D0, OK )

         CALL CHCKAD ( 'AVVS',   RECRD(8), '=',
     .                                    AVVS(1,I), 3,   0.0D0, OK )

      END DO

C
C     Close the CK file.
C
      CALL CKCLS  ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     Clean up
C
C     *****************************************************************
C
      CALL CKUPF ( HANDLE )
      CALL KILFIL( CK02T1 )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END

