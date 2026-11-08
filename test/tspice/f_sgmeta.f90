!$Procedure F_SGMETA ( SGMETA Test Family )

      SUBROUTINE F_SGMETA ( OK )

! $ Abstract
!
!    Test family to exercise the logic and code in the SGMETA routine.
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

      INCLUDE              'sgparam.inc'

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
! $ Files
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors.  Routines in its call
!     tree may generate errors that are either intentional and trapped
!     or unintentional and need reporting.  The test family utilities
!     manage this.
!
! $ Particulars
!
!     This routine exercises SGMETA's logic and its possible error
!     conditions.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     F.S. Turner     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    F_SGMETA Version 1.0.0, 16-JUN-1999 (FST)
!
!
!-&

!
!     Local Parameters
!
      INTEGER               OVRSTP
      PARAMETER           ( OVRSTP = 19 )

!
!     Local Variables
!
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
!
!     Start the test family with an open call.
!
      CALL TOPEN ( 'F_SGMETA' )

      CALL TCASE ( 'Generic segment test DAF file creation' )

!
!     Create the first test DAF file.
!
      CASES( 1 ) = 14
      CASES( 2 ) = 15
      CASES( 3 ) = 17

      VALUE1     = 1.0D0

!
!     The first file will contain the segments we'll use to test
!     the undersized meta data case, and the two nominal cases.
!
      CALL SGCTDF ( 'sample1a.daf', 2, 6, VALUE1, CASES,  3, HANDL1 )

!
!     The second file will contain one segment of size 17.  We will
!     use it to verify that the meta data sequence SGMETA is extracting
!     is correct.
!
      CALL SGCTDF ( 'sample2a.daf', 2, 6, VALUE1, 17,     1, HANDL2 )

!
!     The third file will be used to check the alignment in the
!     case where the number of meta data items exceeds MXMETA.
!
      CALL SGCTDF ( 'sample3a.daf', 2, 6, VALUE1, OVRSTP, 1, HANDL3 )

!
!     This fourth and fifth file will be used to exercise the odd and
!     even NI extraction code respectively.
!
      CASES(1) = 17
      CASES(2) = 17

      CALL SGCTDF ( 'sample4a.daf', 2, 5, VALUE1, CASES,  2, HANDL4 )
      CALL SGCTDF ( 'sample5a.daf', 2, 6, VALUE1, CASES,  2, HANDL5 )

!
!     No exceptions should be signaled. Check to make certain.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Test Case:
!
!        Since the first segment in the sample1a.daf is undersized by
!        meta data standards MNMETA, investigate SGMETA's capabilities
!        for signaling this error.
!
      CALL TCASE ( 'Check insufficient meta data error' )

      CALL DAFBFS ( HANDL1 )
      CALL DAFFNA ( FOUND  )
      CALL DAFGS  ( DESCR  )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SGMETA ( HANDL1, DESCR, NMETA, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETADATA)', OK )

!
!     Test Case:
!
!        Now force the unbuffered error where the requested meta data
!        is out of scope, first the negative mnemonic.
!
      CALL TCASE ( 'Unbuffered, non-positive mnemonic request' )

      CALL DAFFNA ( FOUND )
      CALL DAFGS  ( DESCR )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SGMETA ( HANDL1, DESCR, 0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNMETAITEM)', OK )

!
!     Test Case:
!
!        Now force the same unbuffered error where the requested meta
!        data is out of scope, because mnemonic exceeds MXMETA.
!
!     First we need to make certain we end up with the unbuffered
!     case again.  To force this, request data from another segment.
!
      CALL TCASE ( 'Unbuffered, in excess of MXMETA mnemonic request' )

      CALL DAFFNA ( FOUND )
      CALL DAFGS  ( DESCR )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SGMETA ( HANDL1, DESCR, NMETA, VALUE )

!
!     Make certain an exception has not been signaled.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now seek back, re-extract the descriptor.
!
      CALL DAFFPA ( FOUND )
      CALL DAFGS  ( DESCR )

      CALL SGMETA ( HANDL1, DESCR, (MXMETA + 1), VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNMETAITEM)', OK )

!
!     Test Case:
!
!        Now we want to repeat the tests for the buffered case.
!        First buffered, non-positive mnemonics.
!
      CALL TCASE ( 'Buffered, non-positive mnemonic request' )

      CALL SGMETA ( HANDL1, DESCR, 0, VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNMETAITEM)', OK )

!
!     Test Case:
!
!        And the mnemonics in excess of MXMETA.
!
      CALL TCASE ( 'Buffered, in excess of MXMETA mnemonic request' )

      CALL SGMETA ( HANDL1, DESCR, (MXMETA + 1), VALUE )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNMETAITEM)', OK )

!
!     Test Case:
!
!        Now we need to verify that SGMETA is extracting the proper
!        information from the file.
!
      CALL TCASE ( 'Nominal meta data sequence alignment test' )

      CALL DAFBFS ( HANDL2 )
      CALL DAFFNA ( FOUND  )
      CALL DAFGS  ( DESCR  )
      CALL DAFUS  ( DESCR, 2, 6, DC, IC )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the first meta data item, CONBAS.  It should return the
!     start address of the segment, since a value of 1 was placed
!     into the array.
!
      CALL SGMETA ( HANDL2, DESCR, CONBAS, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'CONBAS', VALUE, '=', IC(5), 0, OK )

!
!     Now check the last meta data item before NMETA.
!
      CALL SGMETA ( HANDL2, DESCR, PKTOFF, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'PKTOFF', VALUE, '=', 1,     0, OK )

!
!     Finally check NMETA.
!
      CALL SGMETA ( HANDL2, DESCR, NMETA,  VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NMETA',  VALUE, '=', 17,    0, OK )

!
!     Test Case:
!
!        Now we need to test alignment in the case where the number of
!        meta data items exceeds MXMETA.  We want to be certain that
!        SGMETA is capable of reading the meta data it is aware of.
!
      CALL TCASE ( 'Extra meta data sequence alignment' )

      CALL DAFBFS ( HANDL3 )
      CALL DAFFNA ( FOUND  )
      CALL DAFGS  ( DESCR  )
      CALL DAFUS  ( DESCR, 2, 6, DC, IC )

!
!     Zero out the extra meta data components of which we are unaware.
!     Remember the segment looks like this:
!
!         _________
!        |         |  IC(5)                Known META items, except
!        |         |                       NMETA, which comes at the
!        |_________|  IC(5) + MXMETA - 1   end of the segment.
!            ...
!         _________
!        |_________|  IC(6)                NMETA
!
!
!
      DO I = ( IC(5) + MXMETA - 1 ), ( IC(6) - 1 )

         CALL DAFWDA ( HANDL3, I, I, 0.0D0 )

      END DO
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check the first meta data item, CONBAS.  It should return the
!     start address of the segment, since a value of 1 was placed
!     into the array.
!

      CALL SGMETA ( HANDL3, DESCR, CONBAS, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'CONBAS', VALUE, '=', IC(5),  0, OK )

!
!     Now check the last meta data item before NMETA.
!
      CALL SGMETA ( HANDL3, DESCR, PKTOFF, VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'PKTOFF', VALUE, '=', 1,      0, OK )

!
!     Finally check NMETA.
!
      CALL SGMETA ( HANDL3, DESCR, NMETA,  VALUE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NMETA',  VALUE, '=', MXMETA, 0, OK )

!
!     Test Case:
!
!        Check to see whether the segment address decomposition code
!        works properly for the unbuffered, even case.
!
      CALL TCASE ( 'Unbuffered even NI, segment address decomposition' )

      CALL DAFBFS ( HANDL5 )
      CALL DAFFNA ( FOUND  )
      CALL DAFGS  ( DESCR  )

      CALL SGMETA ( HANDL5, DESCR, NMETA, VALUE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NMETA', VALUE, '=', MXMETA, 0, OK )

!
!     Test Case:
!
!        Now check the buffered segment address decomposition code
!        for the even case.
!
      CALL TCASE ( 'Buffered even NI, segment address decomposition' )

      CALL DAFFNA ( FOUND )
      CALL DAFGS  ( DESCR )

      CALL SGMETA ( HANDL5, DESCR, NMETA, VALUE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NMETA', VALUE, '=', MXMETA, 0, OK )

!
!     Test Case:
!
!        Check the unbuffered odd segment address decomposition code.
!
      CALL TCASE ( 'Unbuffered odd, segment address decomposition' )

      CALL DAFBFS ( HANDL4 )
      CALL DAFFNA ( FOUND  )
      CALL DAFGS  ( DESCR  )

      CALL SGMETA ( HANDL4, DESCR, NMETA, VALUE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NMETA', VALUE, '=', MXMETA, 0, OK )

!
!     Test Case:
!
!        Now check the buffered odd segment address decomposition code.
!
      CALL DAFFNA ( FOUND )
      CALL DAFGS  ( DESCR )

      CALL SGMETA ( HANDL4, DESCR, NMETA, VALUE )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ( 'NMETA', VALUE, '=', MXMETA, 0, OK )


      CALL DAFUS  ( DESCR, 2, 6, DC, IC )
      DAFADD = IC (4)
!
!     Now check each of the meta data entries for correctness.
!
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

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )

!
!     Destroy the sample files.
!
      CALL KILFIL ( 'sample1a.daf' )
      CALL KILFIL ( 'sample2a.daf' )
      CALL KILFIL ( 'sample3a.daf' )
      CALL KILFIL ( 'sample4a.daf' )
      CALL KILFIL ( 'sample5a.daf' )


      RETURN

      END
