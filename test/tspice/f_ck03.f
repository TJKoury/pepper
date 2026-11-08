C$Procedure F_CK03 ( CK data type 03 tests )

      SUBROUTINE F_CK03 ( OK )

C$ Abstract
C
C     Exercise routines associated with CK data type 03.
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
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests routines that write and read type 03 CK
C     data:
C
C        CKGR03
C        CKNR03
C        CKW03
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
C-    TSPICE Version 3.0.0, 17-OCT-2021 (JDR)
C
C        Renamed test family to F_CK03. Included test cases for
C        SPICELIB routines CKNR03 and CKGR03.
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
C        This test routine now checks all CKW03 exceptions.
C
C-    TSPICE Version 1.1.0, 15-FEB-2001 (EDW)
C
C        Altered Test Case 2 to use an error tolerance of 1.D-14
C        instead of an exact equality. That test failed due to roundoff
C        error (on the order of 1.D-16) when compiled under the
C        Macintosh Absoft FORTRAN compiler.
C
C-&


C$ Index_Entries
C
C     test routines associated with CK data type 03
C
C-&


C
C     SPICELIB functions
C
      LOGICAL               EXISTS

C
C     Local Parameters
C
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

C
C     Local Variables
C
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


C
C     Saved values
C
      SAVE

C
C     Open the test family.
C
      CALL TOPEN ( 'F_CK03' )


C
C     *****************************************************************
C
C     CKW03 error cases:
C
C     *****************************************************************
C
C     Test CKW03:  start out with error handling.
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 setup' )

C
C     Create a segment identifier.
C
      SEGID = 'CK type 03 test segment'

C
C     Open a new CK file.
C
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

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: case 1 with invalid '
     .        //   'number of records.' )

      NREC      = 0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

      NREC      = 4

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: case 2 with invalid '
     .        //   'number of records.' )

      NREC      = -1

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

      NREC      = 4


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: case 1 with invalid '
     .        //   'number of intervals.' )

      NINTS     =  0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMINT)', OK )

      NINTS     =  1


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: case 2 with invalid '
     .        //   'number of intervals.' )

      NINTS     =  -1

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMINT)', OK )

      NINTS     =  1


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: invalid descriptor begin time.' )

      BEGTIM    =  3.0D0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

      BEGTIM    =  2.0D0


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: invalid descriptor end time.' )

      ENDTIM    =  9.0D0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

      ENDTIM    = 10.0D0


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: invalid reference frame name.' )

      REF       = 'MY_FRAME'

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

      REF       = 'J2000'


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: non-printing characters '
     .        //   'in SEGID.' )

      SEGID(3:3) = CHAR(7)

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )

      SEGID(3:3) = ' '


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: SEGID that is too long.' )

      SEGID     = '12345678901234567890123456789012345678901'

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )

      SEGID     = 'CK type 03 test segment'

C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: negative first SCLK time.' )

      SCLKDP(1) =  -2.0D0
      BEGTIM    =  -2.0D0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKTIME)', OK )

      SCLKDP(1) =  2.0D0
      BEGTIM    =  2.0D0


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: case 1 with SCLK times '
     .        //   'out of order.' )

      SCLKDP(2) =  8.0D0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      SCLKDP(2) =  4.0D0


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: case 2 with SCLK times '
     .        //   'out of order.' )

      SCLKDP(2) =  9.0D0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      SCLKDP(2) =  4.0D0


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: mismatching first times.' )

      STARTS(1) =  SCLKDP(2)

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESDONTMATCH)', OK )

      STARTS(1) =  BEGTIM


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: case 1 with interval '
     .        //   'starts out of order.' )

      NINTS     =  3
      STARTS(1) = BEGTIM
      STARTS(2) = SCLKDP(2)
      STARTS(3) = SCLKDP(2)

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      NINTS     =  1
      STARTS(1) =  BEGTIM


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: case 2 with interval '
     .        //   'starts out of order.' )

      NINTS     =  3
      STARTS(1) =  BEGTIM
      STARTS(2) =  SCLKDP(3)
      STARTS(3) =  SCLKDP(2)

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

      NINTS     =  1
      STARTS(1) =  BEGTIM


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: invalid interval start times.' )

      NINTS     =  2
      STARTS(1) =  BEGTIM
      STARTS(2) =  ( SCLKDP(2) + SCLKDP(3) ) / 2.D0

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTARTTIME)', OK )

      NINTS     =  1
      STARTS(1) =  BEGTIM


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKW03 error case: zero quaternion.' )

      CALL CLEARD ( 4, QUATS( 0, 4 ) )

      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROQUATERNION)', OK )

      QUATS( 0, 4 ) = COS ( 0.25D0 )


C
C     *****************************************************************
C
C     End of CKW03 error cases:
C
C     *****************************************************************
C
C     Close the CK file at the DAF level; CKCLS won't close
C     a file without segments.
C
      CALL DAFCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C     *****************************************************************
C
C      CKW03 Non-error cases:
C
C     *****************************************************************
C
C     Trivial case: write an CK containing all quaternions of
C     unit length.
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Test CKW03: All quaternions unit length.' )

C
C     Open new C-kernel.
C
      IF ( EXISTS( CK03T1 ) ) THEN
         CALL DELFIL( CK03T1 )
         CALL CHCKXC( .FALSE., ' ', OK )
      END IF

      CALL CKOPN  ( CK03T1, CK03T1, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Write segment.
C
      CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  AVFLAG,
     .             SEGID,  NREC,   SCLKDP, QUATS, AVVS, NINTS,
     .             STARTS                                      )


C
C     Close kernel.
C
      CALL CKCLS  ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check the kernel.
C
      CALL FURNSH ( CK03T1 )

      CALL CKGPAV ( INST, 4.0D0, 1.0D0, 'J2000',
     .              CMAT, AV,    CLKOUT, FOUND )

      CALL CHCKXC ( .FALSE., ' ',  OK )

      CALL CHCKSD ( 'CLKOUT', CLKOUT, '=', 4.0D0, 0.0D0, OK )

      CALL Q2M    ( QUATS(0,2), EMAT )
      CALL CHCKAD ( 'CMAT', CMAT, '~', EMAT,      9, VTIGHT, OK )
      CALL CHCKAD ( 'AV',   AV,   '~', AVVS(1,2), 3, VTIGHT, OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

      CALL UNLOAD ( CK03T1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C      CKNR03 Error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKNR03 error case: segment not of data type 3.' )

C
C     Load the a CK file and get the handle.
C
      CALL DAFOPR ( CK03T1, HANDLE )
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
C     Set the data type to anything other than 3, e.g. 0
C     Data type and angular velocity flag are given by DESCR(4).
C     Save the value in order to restore it.
C
      CALL MOVED ( DESCR, 5, DSCTMP )
      DSCTMP(4) = 0.D0

      CALL CKNR03 ( HANDLE, DSCTMP, NREC )
      CALL CHCKXC ( .TRUE., 'SPICE(CKWRONGDATATYPE)', OK )

C
C     *****************************************************************
C
C      CKGR03 Error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKGR03 error case: segment not of data type 3.' )

C
C     Call CKGR03 using the "corrupted" DESCR from previous test.
C     Note that the CK file we are using has one segment with 4
C     records.
C
      CALL CKGR03 ( HANDLE, DSCTMP, 1, RECRD )
      CALL CHCKXC ( .TRUE., 'SPICE(CKWRONGDATATYPE)', OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKGR03 error case: case 1 with record which '
     .        //   'does not exist.' )

      CALL CKGR03 ( HANDLE, DESCR, 0, RECRD )
      CALL CHCKXC ( .TRUE., 'SPICE(CKNONEXISTREC)', OK )


C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'CKGR03 error case: case 2 with record which '
     .        //   'does not exist.' )

      CALL CKGR03 ( HANDLE, DESCR, 10, RECRD )
      CALL CHCKXC ( .TRUE., 'SPICE(CKNONEXISTREC)', OK )

C
C     Close the CK file.
C
      CALL CKCLS  ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     *****************************************************************
C
C      CKNR03 and CKGR03 non-error cases:
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
      CALL TCASE  ( 'Examine the segment in the file CK03T1.' )

C
C     Load the a CK file and get the handle.
C
      CALL DAFOPR ( CK03T1, HANDLE )
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
      CALL CKNR03 ( HANDLE, DESCR, NREC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check that CKNR03 returns the expected number of records.
C
      CALL CHCKSI ( 'NREC',NREC, '=', COUNT, 0, OK )

      DO I = 1, NREC

C
C        Get the Ith pointing instance in the segment.
C
         CALL CKGR03 ( HANDLE, DESCR, I, RECRD )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check the time, quaternion and av.
C
         CALL CHCKSD ( 'SCLKDP', RECRD(1), '=', SCLKDP(I), 0.0D0, OK )

         CALL CHCKAD ( 'QUATS', RECRD(2),   '=',
     .                          QUATS(0,I),  4,  0.0D0, OK )

         CALL CHCKAD ( 'AVVS', RECRD(6),  '=',
     .                         AVVS(1,I), 3,   0.0D0, OK )

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
C     --- Case: -------------------------------------------------------
C
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

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END

