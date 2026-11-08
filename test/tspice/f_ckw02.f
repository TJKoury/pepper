C$Procedure F_CKW02 ( Family of checks for CKW02 )

      SUBROUTINE F_CKW02 ( OK )

C$ Abstract
C
C     This family of tests checks all CKW02 exceptions and that a good
C     CK segment can indeed get written to a file.
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
C     This routine tests the routine
C
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
C-    TSPICE Version 2.1.0, 30-MAY-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Edited the header to comply with NAIF standard.
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
C     None.
C
C-&


C
C     Local Variables
C
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

C
C     Begin every test family with an open call.
C
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

C
C     Check all exceptions.
C
      CALL TCASE ( 'Case 1 with invalid number of records.' )

         NREC = 0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

         NREC = 4

      CALL TCASE ( 'Case 2 with invalid number of records.' )

         NREC = -1

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDNUMREC)', OK )

         NREC = 4

      CALL TCASE ( 'Case with invalid descriptor begin time.' )

         BEGTIM    =  3.0D0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

         BEGTIM    =  2.0D0

      CALL TCASE ( 'Case with invalid descriptor end time.' )

         ENDTIM    = 11.0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDESCRTIME)', OK )

         ENDTIM    = 12.0

      CALL TCASE ( 'Case with invalid reference frame name.' )

         REF       = 'MY_FRAME'

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDREFFRAME)', OK )

         REF       = 'J2000'

      CALL TCASE ( 'Case with non-printing characters in SEGID.' )

         SEGID(5:5) = CHAR(7)

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(NONPRINTABLECHARS)', OK )

         SEGID(5:5) = ' '

      CALL TCASE ( 'Case with SEGID that is too long.' )

         SEGID     = '12345678901234567890123456789012345678901'

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(SEGIDTOOLONG)', OK )

         SEGID     = 'TEST CKTYPE 03'

      CALL TCASE ( 'Case with negative first SCLK time.' )

         START(1)  =  -2.0D0
         BEGTIM    =  -2.0D0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSCLKTIME)', OK )

         START(1)  =  2.0D0
         BEGTIM    =  2.0D0

      CALL TCASE ( 'Case 1 with degenerate first interval.' )

         STOP(1)   = START(1)

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

         STOP (1) = START(2)

      CALL TCASE ( 'Case 2 with degenerate first interval.' )

         STOP (1)  = START(1) - 1.D0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

         STOP (1) = START(2)

      CALL TCASE ( 'Case 1 with degenerate interval.' )

         STOP (2)  = START(2)

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

         STOP (2)  = START(3)

      CALL TCASE ( 'Case 2 with degenerate interval.' )

         STOP (2)  = START(2) - 1.D0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATEINTERVAL)', OK )

         STOP (2) = START(3)

      CALL TCASE ( 'Case 1 with times out of order.' )

         START (3) = START(2)

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

         START(3) =  8.0D0

      CALL TCASE ( 'Case 2 with times out of order.' )

         START (3) = START(2) - 1.D0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(TIMESOUTOFORDER)', OK )

         START(3) =  8.0D0

      CALL TCASE ( 'Case 2 with bad stop time.' )

         STOP (2) = START(3) + 1.D0

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(BADSTOPTIME)', OK )

         STOP (2) = START(3)

      CALL TCASE ( 'Case with a zero quaternion.' )

         CALL CLEARD ( 4, QUATS( 0, 4 ) )

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .TRUE., 'SPICE(ZEROQUATERNION)',  OK )

         QUATS( 0, 4 ) = DCOS ( 0.25D0 )

C
C     Finally, check one good case.
C
      CALL TCASE ( 'All quaternions unit length.' )

         CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,  SEGID,
     .                NREC,   START,  STOP,   QUATS, AVVS, RATES )

         CALL CHCKXC ( .FALSE., ' ',  OK )

         CALL CKCLS ( HANDLE )
         CALL CKLPF ( 'ckw02.bc', HANDLE )

         CALL CKGPAV ( INST, 4.0D0, 1.0D0, 'J2000',
     .                 CMAT, AV,    CLKOUT, FOUND )

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
