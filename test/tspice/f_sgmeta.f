C$Procedure F_SGMETA ( SGMETA Test Family )
 
      SUBROUTINE F_SGMETA ( OK )
 
C$ Abstract
C
C    Test family to exercise the logic and code in the SGMETA routine.
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
 
      INCLUDE              'sgparam.inc'
 
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
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors.  Routines in its call
C     tree may generate errors that are either intentional and trapped
C     or unintentional and need reporting.  The test family utilities
C     manage this.
C
C$ Particulars
C
C     This routine exercises SGMETA's logic and its possible error
C     conditions.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    F_SGMETA Version 1.0.0, 16-JUN-1999 (FST)
C
C
C-&
 
C
C     Local Parameters
C
      INTEGER               OVRSTP
      PARAMETER           ( OVRSTP = 19 )
 
C
C     Local Variables
C
      DOUBLE PRECISION      DC    ( 2 )
      DOUBLE PRECISION      DESCR ( 6 )
      DOUBLE PRECISION      VALUE1
      INTEGER               CASES ( 3 )
      INTEGER               DAFADD
      INTEGER               HANDL1
      INTEGER               HANDL2
      INTEGER               HANDL3
      INTEGER               HANDL4
      INTEGER               HANDL5
      INTEGER               I
      INTEGER               IC    ( 6 )
      INTEGER               VALUE
      LOGICAL               FOUND
C
C     Start the test family with an open call.
C
      CALL TOPEN ( 'F_SGMETA' )
 
      CALL TCASE ( 'Generic segment test DAF file creation' )
 
C
C     Create the first test DAF file.
C
      CASES( 1 ) = 14
      CASES( 2 ) = 15
      CASES( 3 ) = 17
 
      VALUE1     = 1.0D0
 
C
C     The first file will contain the segments we'll use to test
C     the undersized meta data case, and the two nominal cases.
C
      CALL SGCTDF ( 'sample1a.daf', 2, 6, VALUE1, CASES,  3, HANDL1 )
 
C
C     The second file will contain one segment of size 17.  We will
C     use it to verify that the meta data sequence SGMETA is extracting
C     is correct.
C
      CALL SGCTDF ( 'sample2a.daf', 2, 6, VALUE1, 17,     1, HANDL2 )
 
C
C     The third file will be used to check the alignment in the
C     case where the number of meta data items exceeds MXMETA.
C
      CALL SGCTDF ( 'sample3a.daf', 2, 6, VALUE1, OVRSTP, 1, HANDL3 )
 
C
C     This fourth and fifth file will be used to exercise the odd and
C     even NI extraction code respectively.
C
      CASES(1) = 17
      CASES(2) = 17
 
      CALL SGCTDF ( 'sample4a.daf', 2, 5, VALUE1, CASES,  2, HANDL4 )
      CALL SGCTDF ( 'sample5a.daf', 2, 6, VALUE1, CASES,  2, HANDL5 )
 
C
C     No exceptions should be signaled. Check to make certain.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Test Case:
C
C        Since the first segment in the sample1a.daf is undersized by
C        meta data standards MNMETA, investigate SGMETA's capabilities
C        for signaling this error.
C
      CALL TCASE ( 'Check insufficient meta data error' )
 
      CALL DAFBFS ( HANDL1 )
      CALL DAFFNA ( FOUND  )
      CALL DAFGS  ( DESCR  )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SGMETA ( HANDL1, DESCR, NMETA, VALUE )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETADATA)', OK )
 
C
C     Test Case:
C
C        Now force the unbuffered error where the requested meta data
C        is out of scope, first the negative mnemonic.
C
      CALL TCASE ( 'Unbuffered, non-positive mnemonic request' )
 
      CALL DAFFNA ( FOUND )
      CALL DAFGS  ( DESCR )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SGMETA ( HANDL1, DESCR, 0, VALUE )
 
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNMETAITEM)', OK )
 
C
C     Test Case:
C
C        Now force the same unbuffered error where the requested meta
C        data is out of scope, because mnemonic exceeds MXMETA.
C
C     First we need to make certain we end up with the unbuffered
C     case again.  To force this, request data from another segment.
C
      CALL TCASE ( 'Unbuffered, in excess of MXMETA mnemonic request' )
 
      CALL DAFFNA ( FOUND )
      CALL DAFGS  ( DESCR )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL SGMETA ( HANDL1, DESCR, NMETA, VALUE )
 
C
C     Make certain an exception has not been signaled.
C
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Now seek back, re-extract the descriptor.
C
      CALL DAFFPA ( FOUND )
      CALL DAFGS  ( DESCR )
 
      CALL SGMETA ( HANDL1, DESCR, (MXMETA + 1), VALUE )
 
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNMETAITEM)', OK )
 
C
C     Test Case:
C
C        Now we want to repeat the tests for the buffered case.
C        First buffered, non-positive mnemonics.
C
      CALL TCASE ( 'Buffered, non-positive mnemonic request' )
 
      CALL SGMETA ( HANDL1, DESCR, 0, VALUE )
 
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNMETAITEM)', OK )
 
C
C     Test Case:
C
C        And the mnemonics in excess of MXMETA.
C
      CALL TCASE ( 'Buffered, in excess of MXMETA mnemonic request' )
 
      CALL SGMETA ( HANDL1, DESCR, (MXMETA + 1), VALUE )
 
      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNMETAITEM)', OK )
 
C
C     Test Case:
C
C        Now we need to verify that SGMETA is extracting the proper
C        information from the file.
C
      CALL TCASE ( 'Nominal meta data sequence alignment test' )
 
      CALL DAFBFS ( HANDL2 )
      CALL DAFFNA ( FOUND  )
      CALL DAFGS  ( DESCR  )
      CALL DAFUS  ( DESCR, 2, 6, DC, IC )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the first meta data item, CONBAS.  It should return the
C     start address of the segment, since a value of 1 was placed
C     into the array.
C
      CALL SGMETA ( HANDL2, DESCR, CONBAS, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'CONBAS', VALUE, '=', IC(5), 0, OK )
 
C
C     Now check the last meta data item before NMETA.
C
      CALL SGMETA ( HANDL2, DESCR, PKTOFF, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'PKTOFF', VALUE, '=', 1,     0, OK )
 
C
C     Finally check NMETA.
C
      CALL SGMETA ( HANDL2, DESCR, NMETA,  VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NMETA',  VALUE, '=', 17,    0, OK )
 
C
C     Test Case:
C
C        Now we need to test alignment in the case where the number of
C        meta data items exceeds MXMETA.  We want to be certain that
C        SGMETA is capable of reading the meta data it is aware of.
C
      CALL TCASE ( 'Extra meta data sequence alignment' )
 
      CALL DAFBFS ( HANDL3 )
      CALL DAFFNA ( FOUND  )
      CALL DAFGS  ( DESCR  )
      CALL DAFUS  ( DESCR, 2, 6, DC, IC )
 
C
C     Zero out the extra meta data components of which we are unaware.
C     Remember the segment looks like this:
C
C         _________
C        |         |  IC(5)                Known META items, except
C        |         |                       NMETA, which comes at the
C        |_________|  IC(5) + MXMETA - 1   end of the segment.
C            ...
C         _________
C        |_________|  IC(6)                NMETA
C
C
C
      DO I = ( IC(5) + MXMETA - 1 ), ( IC(6) - 1 )
 
         CALL DAFWDA ( HANDL3, I, I, 0.0D0 )
 
      END DO
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check the first meta data item, CONBAS.  It should return the
C     start address of the segment, since a value of 1 was placed
C     into the array.
C
 
      CALL SGMETA ( HANDL3, DESCR, CONBAS, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'CONBAS', VALUE, '=', IC(5),  0, OK )
 
C
C     Now check the last meta data item before NMETA.
C
      CALL SGMETA ( HANDL3, DESCR, PKTOFF, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'PKTOFF', VALUE, '=', 1,      0, OK )
 
C
C     Finally check NMETA.
C
      CALL SGMETA ( HANDL3, DESCR, NMETA,  VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NMETA',  VALUE, '=', MXMETA, 0, OK )
 
C
C     Test Case:
C
C        Check to see whether the segment address decomposition code
C        works properly for the unbuffered, even case.
C
      CALL TCASE ( 'Unbuffered even NI, segment address decomposition' )
 
      CALL DAFBFS ( HANDL5 )
      CALL DAFFNA ( FOUND  )
      CALL DAFGS  ( DESCR  )
 
      CALL SGMETA ( HANDL5, DESCR, NMETA, VALUE )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NMETA', VALUE, '=', MXMETA, 0, OK )
 
C
C     Test Case:
C
C        Now check the buffered segment address decomposition code
C        for the even case.
C
      CALL TCASE ( 'Buffered even NI, segment address decomposition' )
 
      CALL DAFFNA ( FOUND )
      CALL DAFGS  ( DESCR )
 
      CALL SGMETA ( HANDL5, DESCR, NMETA, VALUE )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NMETA', VALUE, '=', MXMETA, 0, OK )
 
C
C     Test Case:
C
C        Check the unbuffered odd segment address decomposition code.
C
      CALL TCASE ( 'Unbuffered odd, segment address decomposition' )
 
      CALL DAFBFS ( HANDL4 )
      CALL DAFFNA ( FOUND  )
      CALL DAFGS  ( DESCR  )
 
      CALL SGMETA ( HANDL4, DESCR, NMETA, VALUE )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NMETA', VALUE, '=', MXMETA, 0, OK )
 
C
C     Test Case:
C
C        Now check the buffered odd segment address decomposition code.
C
      CALL DAFFNA ( FOUND )
      CALL DAFGS  ( DESCR )
 
      CALL SGMETA ( HANDL4, DESCR, NMETA, VALUE )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NMETA', VALUE, '=', MXMETA, 0, OK )
 
 
      CALL DAFUS  ( DESCR, 2, 6, DC, IC )
      DAFADD = IC (4)
C
C     Now check each of the meta data entries for correctness.
C
      CALL TCASE ( 'CONBAS - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, CONBAS, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'CONBAS', VALUE, '=', DAFADD, 0, OK )
 
      CALL TCASE ( 'REFBAS - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, REFBAS, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'REFBAS', VALUE, '=', DAFADD, 0, OK )
 
      CALL TCASE ( 'RDRBAS - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, RDRBAS, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'RDRBAS', VALUE, '=', DAFADD, 0, OK )
 
      CALL TCASE ( 'PDRBAS - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, PDRBAS, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'PDRBAS', VALUE, '=', DAFADD, 0, OK )
 
      CALL TCASE ( 'PKTBAS - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, PKTBAS, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'PKTBAS', VALUE, '=', DAFADD, 0, OK )
 
      CALL TCASE ( 'RSVBAS - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, RSVBAS, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'RSVBAS', VALUE, '=', DAFADD, 0, OK )
 
      CALL TCASE ( 'NCON - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, NCON, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NCON', VALUE, '=', INT(VALUE1), 0, OK )
 
      CALL TCASE ( 'NRDR - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, NRDR, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NRDR', VALUE, '=', INT(VALUE1), 0, OK )
 
      CALL TCASE ( 'RDRTYP - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, RDRTYP, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'RDRTYP', VALUE, '=', INT(VALUE1), 0, OK )
 
      CALL TCASE ( 'NREF - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, NREF, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NREF', VALUE, '=', INT(VALUE1), 0, OK )
 
      CALL TCASE ( 'NPDR - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, NPDR, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NPDR', VALUE, '=', INT(VALUE1), 0, OK )
 
      CALL TCASE ( 'PDRTYP - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, PDRTYP, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'PDRTYP', VALUE, '=', INT(VALUE1), 0, OK )
 
      CALL TCASE ( 'NPKT - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, NPKT, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NPKT', VALUE, '=', INT(VALUE1), 0, OK )
 
      CALL TCASE ( 'NRSV - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, NRSV, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NRSV', VALUE, '=', INT(VALUE1), 0, OK )
 
      CALL TCASE ( 'PKTSZ - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, PKTSZ, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'PKTSZ', VALUE, '=', INT(VALUE1), 0, OK )
 
      CALL TCASE ( 'PKTOFF - value check' )
 
      CALL SGMETA ( HANDL4, DESCR, PKTOFF, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'PKTOFF', VALUE, '=', INT(VALUE1), 0, OK )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
 
C
C     Destroy the sample files.
C
      CALL KILFIL ( 'sample1a.daf' )
      CALL KILFIL ( 'sample2a.daf' )
      CALL KILFIL ( 'sample3a.daf' )
      CALL KILFIL ( 'sample4a.daf' )
      CALL KILFIL ( 'sample5a.daf' )
 
 
      RETURN
 
      END
