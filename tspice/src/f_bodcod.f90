!$Procedure F_BODCOD ( Body Code/Name Mapping Test Family )

      SUBROUTINE F_BODCOD ( OK )
      use pepper_legacy
      use pepper_inc_zzbodtrn
      use pepper_inc_zzctr

! $ Abstract
!
!     Test family to exercise the logic and code in the body code to
!     name and name to code mapping software.
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
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine exercises the conformance of the body name-to-code
!     and code-to-name mapping software to the changes implemented
!     in N0053.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) After a cursory examination of the body ID codes currently in
!        use in ZZBODTRN, it seems the range (1000,2000) is reasonable
!        to assume as "unused".  This test family makes use of codes
!        in this range for testing purposes.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    TSPICE Version 3.0.0, 16-SEP-2016 (BVS)
!
!        Removed check for even NROOM. Changed "doubled up" test case
!        to not be dependent on even NROOM. Added ZZBODTRN counter
!        tests.
!
!-    TSPICE Version 2.0.0, 22-JUL-2004 (NJB)
!
!        Test cases for BODS2C were added.
!
!-    TSPICE Version 1.0.0, 12-AUG-2002 (FST)
!
!
!-&

!
!     TESTUTIL Functions
!
      CHARACTER*(10)        BEGDAT

!
!     Local Parameters
!
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 200 )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )

      INTEGER               BUFLEN
      PARAMETER           ( BUFLEN = 10 )

      INTEGER               TKBASE
      PARAMETER           ( TKBASE = 10000 )

      INTEGER               BDBASE
      PARAMETER           ( BDBASE = 20000 )

!
!     Local Variables
!
      CHARACTER*(MAXL)      BASNAM ( MAXE )
      CHARACTER*(MAXL)      BASNOR ( MAXE )
      CHARACTER*(LINLEN)    BUFFER ( BUFLEN )
      CHARACTER*(NAMLEN)    HOLD
      CHARACTER*(NAMLEN)    NAME
      CHARACTER*(NAMLEN)    XNAME
      CHARACTER*(NAMLEN)    TKNAME

      INTEGER               BASCOD ( MAXE )
      INTEGER               CODE
      INTEGER               I
      INTEGER               J
      INTEGER               NVALS
      INTEGER               RESTOR

      INTEGER               USRCTR ( CTRSIZ )

      LOGICAL               FOUND
      LOGICAL               UPDATE

!
!     Save everything.
!
      SAVE

!
!     Open the test family.
!
      CALL TOPEN ( 'F_BODCOD' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check cleared state of test module ID codes.' )

!
!     Verify that NROOM is even.  It must be otherwise a few of the
!     test cases will fail.
!
!     This test was commented out because the later test code was
!     changed to work with the even number preceding NROOM.
!
!      CALL CHCKSI ( 'NROOM-EVEN', MOD(NROOM,2), '=', 0, 0, OK )

!
!     Retrieve the built-in code/name arrays.
!
      CALL ZZBODGET ( MAXE, BASNAM, BASNOR, BASCOD, NVALS )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Multiple NAMES to a single code, BODDEF' )

      CALL BODDEF ( 'F_BODCOD_TESTA', 1001 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that this assignment worked as expected.
!
      CALL BODC2N ( 1001, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTA', OK )

!
!     Now assign a new name to the same code using BODDEF.
!
      CALL BODDEF ( 'F_BODCOD_TEST', 1001 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify that the newer name takes precedence.
!
      CALL BODC2N ( 1001, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TEST', OK )

!
!     Depending on the sorting algorithm this could go a number
!     of ways.  Check a name that would occur "after" F_BODCOD_TESTA
!     in the list as well.
!
      CALL BODDEF ( 'F_BODCOD_TESTB', 1001 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify that the newer name takes precedence.
!
      CALL BODC2N ( 1001, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTB', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Multiple NAMES to a single code, TEXT KERNEL' )

      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1002'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_TESTC'''

      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify that the assignment worked.
!
      CALL BODC2N ( 1002, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTC', OK )

!
!     Now append a new name with the same code.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE += 1002'
      BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_ATEST'''

      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify that the newer name takes precedence.
!
      CALL BODC2N ( 1002, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_ATEST', OK )

!
!     Depending on the sorting algorithm this could go a number
!     of ways.  Check a name that would occur "after" F_BODCOD_TESTC
!     in the list as well.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE += 1002'
      BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_TESTD'''

      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify that the newer name takes precedence.
!
      CALL BODC2N ( 1002, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTD', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Multiple NAMES to a single code, BODDEF then TEXT' )

      CALL BODDEF ( 'F_BODCOD_TESTE', 1003 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that this assignment worked as expected.
!
      CALL BODC2N ( 1003, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTE', OK )

!
!     Now add a new name with the text kernel system.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1003'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_TESTF'''

      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify that the text-kernel name takes precedence.
!
      CALL BODC2N ( 1003, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTF', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Multiple NAMES to a single code, TEXT then BODDEF' )

      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1004'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_TESTG'''

      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that this assignment worked as expected.
!
      CALL BODC2N ( 1004, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTG', OK )

!
!     Now add a new name with the text kernel system.
!
      CALL BODDEF ( 'F_BODCOD_TESTH', 1004 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Verify that the text-kernel name takes precedence.
!
      CALL BODC2N ( 1004, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTG', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check whitespace/case-sensitivity on name, BODDEF' )

      CALL BODDEF ( 'F_BODcOd_TestJ  space', 1007 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that the assignment works properly.
!
      CALL BODC2N ( 1007, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODcOd_TestJ  space', OK )

!
!     Add the same name with different case/spacing.
!
      CALL BODDEF ( 'F_BoDCoD_TesTj    space', 1007 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that the assignment worked properly.
!
      CALL BODC2N ( 1007, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BoDCoD_TesTj    space', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check whitespace/case-sensitivity on name, TEXT' )

      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1008'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''f_bodCoD_TesTK    blank'''

      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that the assignment works properly.
!
      CALL BODC2N ( 1008, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'f_bodCoD_TesTK    blank', OK )

!
!     Attempt to override this with a BODDEF assignment.
!
      CALL BODDEF ( 'F_bodCOD_TESTK   space', 1008 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check to see that this assignment worked.
!
      CALL BODN2C ( 'F_BODCOD_TESTK SPACE', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1008, 0, OK )

!
!     Now see that the latest text-kernel entry takes precedence.
!
      CALL BODC2N ( 1008, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'f_bodCoD_TesTK    blank', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test diagnostics on text-kernel load.' )

!
!     Clear the pool.
!
      CALL CLPOOL ( )

!
!     Construct a text-kernel with name-code assignments that
!     will cause FURNSH to signal an error upon loading.
!
      TKNAME = 'testtk.tk'

      BUFFER(  1 ) = BEGDAT()
      BUFFER(  2 ) = 'NAIF_BODY_CODE = ( 1009, 1010 )'
      BUFFER(  3 ) = 'NAIF_BODY_NAME = ( ''SPUD'' )'

      CALL TSTTXT ( TKNAME, BUFFER, 3, .FALSE., .TRUE. )

!
!     Load the kernel and verify that an error has been signaled.
!
      CALL FURNSH ( TKNAME )

      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)', OK )

      CALL CLPOOL ( )
      CALL KILFIL ( TKNAME )

!
!     Try a simple code to name conversion to see if the error
!     is still floating around.
!
      CALL BODC2N ( 399, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
!
!     This test case is commented out, because no changes were made
!     to BODDEF in N0053.
!
      CALL TCASE ( 'BODDEF multiple codes to single name masking.' )

      CALL BODDEF ( 'F_BODCOD_TESTI', 1005 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that this assignment worked as expected.
!
      CALL BODC2N ( 1005, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTI', OK )

!
!     Now attempt to assign F_BODCOD_TESTI a new ID code, this
!     should be allowed due to masking updates.
!
      CALL BODDEF ( 'F_BODCOD_TESTI', 1006 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that the results of the assignment work as expected.
!
      CALL BODC2N ( 1006, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTI', OK )

      NAME = '<UNCHANGED>'
      CALL BODC2N ( 1005, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'TEXT multiple codes to single name masking.' )

      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1011'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_TESTL'''

      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that this assignment worked as expected.
!
      CALL BODC2N ( 1011, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTL', OK )

!
!     Now attempt to assign F_BODCOD_TEST_L a new ID code.  This
!     should be allowed due to masking updates.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE += 1012'
      BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_TESTL'''

      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that the results of the assignment work as expected.
!
      CALL BODC2N ( 1012, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTL', OK )

      NAME = '<UNCHANGED>'
      CALL BODC2N ( 1011, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple BODDEF masked by TEXT case.' )

      CALL BODDEF ( 'F_BODCOD_TESTM', 1013 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that this assignment worked as expected.
!
      CALL BODC2N ( 1013, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTM', OK )

!
!     Now load a definition that masks this name.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1014'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_TESTM'''

      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Perform all of the necessary state checks at this point.
!
      CALL BODN2C ( 'F_BODCOD_TESTM', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1014, 0, OK )

      CALL BODC2N ( 1014, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTM', OK )

!
!     The original BODDEF code should now be effectively masked.
!
      NAME = '<UNCHANGED>'
      CALL BODC2N ( 1013, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Now clear the pool to unload the text kernel assignment.
!
      CALL CLPOOL ( )

      CALL BODN2C ( 'F_BODCOD_TESTM', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1013, 0, OK )

      CALL BODC2N ( 1013, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTM', OK )

      NAME = '<UNCHANGED>'
      CALL BODC2N ( 1014, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'TEXT first, invisible BODDEF' )

!
!     Load a definition via the text-kernel interface.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1015'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_TESTN'''

      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that this assignment worked as expected.
!
      CALL BODC2N ( 1015, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTN', OK )

!
!     Now make a BODDEF assignment that would appear to be
!     invisible as a result of masking.
!
      CALL BODDEF ( 'F_BODCOD_TESTN', 1016 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Perform all of the necessary state checks at this point.
!
      CALL BODN2C ( 'F_BODCOD_TESTN', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1015, 0, OK )

      CALL BODC2N ( 1015, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTN', OK )

!
!     The BODDEF code should now be effectively masked.
!
      NAME = '<UNCHANGED>'
      CALL BODC2N ( 1016, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Now clear the pool to unload the text kernel assignment.
!
      CALL CLPOOL ( )

      CALL BODN2C ( 'F_BODCOD_TESTN', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1016, 0, OK )

      CALL BODC2N ( 1016, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTN', OK )

      NAME = '<UNCHANGED>'
      CALL BODC2N ( 1015, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'BODDEF overriding built-in codes' )

!
!     Obtain the current code for body to override.
!
      CALL BODN2C ( 'EARTH', RESTOR, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Now override the code for EARTH with a new one.
!
      CALL BODDEF ( 'EARTH', 1017 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that the assignment worked.
!
      CALL BODN2C ( 'EARTH', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1017, 0, OK )

      CALL BODC2N ( 1017, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH', OK )

      NAME = '<UNCHANGED>'
      CALL BODC2N ( RESTOR, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Restore the original code for EARTH.
!
      CALL BODDEF ( 'EARTH', RESTOR )

!
!     Check that it is restored.
!
      CALL BODN2C ( 'EARTH', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', RESTOR, 0, OK )

      CALL BODC2N ( RESTOR, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'TEXT overriding built-in codes' )

      CALL BODN2C ( 'MARS', RESTOR, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Load a definition via the text-kernel interface that blasts
!     a built in code.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1018'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''MARS'''

      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that the assignment worked.
!
      CALL BODN2C ( 'MARS', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1018, 0, OK )

      CALL BODC2N ( 1018, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'MARS', OK )

      NAME = '<UNCHANGED>'
      CALL BODC2N ( RESTOR, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Restore the original assignment.
!
      CALL CLPOOL ( )

!
!     Check that it is restored.
!
      CALL BODN2C ( 'MARS', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', RESTOR, 0, OK )

      CALL BODC2N ( RESTOR, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'MARS', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Fill TEXT buffer with unique entries' )

!
!     Load up many more definitions than ZZBODTRN can handle.
!
      DO I = 1, NROOM

         BUFFER(  1 ) = 'NAIF_BODY_CODE += #'
         BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_TEST_#'''

         CALL REPMI ( BUFFER(1), '#', TKBASE+I, BUFFER(1) )
         CALL REPMI ( BUFFER(2), '#', TKBASE+I, BUFFER(2) )

         CALL LMPOOL ( BUFFER, 2 )

      END DO

!
!     See what happens...
!
      DO I = 1, NROOM

         HOLD = 'F_BODCOD_TEST_#'
         NAME = '<UNCHANGED>'
         CALL REPMI ( HOLD, '#', TKBASE+I, HOLD )

         CALL BODC2N ( TKBASE+I, NAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
         CALL CHCKSC ( 'NAME', NAME, '=', HOLD, OK )

         CALL BODN2C ( HOLD, CODE, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
         CALL CHCKSI ( 'CODE', CODE, '=', TKBASE+I, 0, OK )

      END DO

!
!     Clear the pool, resetting kernel pool variable assignments.
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'More unique entries than SPICELIB''s NROOM (PEPPER: no overflow).' )

!
!     Load up many more definitions than ZZBODTRN can handle.
!
      DO I = 1, NROOM + 1

         BUFFER(  1 ) = 'NAIF_BODY_CODE += #'
         BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_TEST_#'''

         CALL REPMI ( BUFFER(1), '#', TKBASE+I, BUFFER(1) )
         CALL REPMI ( BUFFER(2), '#', TKBASE+I, BUFFER(2) )

         CALL LMPOOL ( BUFFER, 2 )

      END DO

!
!     At this point we're set to observe an error signalled by the first
!     routine that causes the processing of the NAME/CODE kernel pool
!     variable pair.
!
!     PEPPER: the kernel name tables grow (SPICELIB's held NROOM names
!     and signaled SPICE(KERVARTOOBIG)): the first and the last of the
!     NROOM+1 names are found.
!
      NAME = '<UNCHANGED>'
      CALL BODC2N ( TKBASE+1, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      XNAME = 'F_BODCOD_TEST_#'
      CALL REPMI  ( XNAME, '#', TKBASE+1, XNAME )
      CALL CHCKSC ( 'NAME', NAME, '=', XNAME, OK )

      CALL BODC2N ( TKBASE+NROOM+1, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND (last)', FOUND, .TRUE., OK )
      XNAME = 'F_BODCOD_TEST_#'
      CALL REPMI  ( XNAME, '#', TKBASE+NROOM+1, XNAME )
      CALL CHCKSC ( 'NAME (last)', NAME, '=', XNAME, OK )

!
!     Clear the pool, resetting kernel pool variable assignments.
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Fill TEXT buffer with doubled up entries.' )

!
!     Fill TEXT buffer with doubled up entries.
!
      DO I = 1, (NROOM/2)*2

         BUFFER(  1 ) = 'NAIF_BODY_CODE += #'
         BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_TEST_#'''

         CALL REPMI ( BUFFER(1), '#', TKBASE+I, BUFFER(1) )
         CALL REPMI ( BUFFER(2), '#', TKBASE+((I+1)/2), BUFFER(2) )

         CALL LMPOOL ( BUFFER, 2 )

      END DO

!
!     Check results.
!
      DO I = 1, (NROOM/2)*2

         HOLD = 'F_BODCOD_TEST_#'
         NAME = '<UNCHANGED>'
         CALL REPMI ( HOLD, '#', TKBASE+((I+1)/2), HOLD )

         CALL BODC2N ( TKBASE+I, NAME, FOUND )

         IF ( MOD(I,2) .EQ. 0 ) THEN
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
            CALL CHCKSC ( 'NAME', NAME, '=', HOLD, OK )
         ELSE
            CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
            CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
         END IF

         CALL BODN2C ( HOLD, CODE, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
         CALL CHCKSI ( 'CODE', CODE, '=', TKBASE+I+MOD(I,2), 0, OK )

      END DO

!
!     Clear the pool, resetting kernel pool variable assignments.
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Fill TEXT buffer with the same masked entries.' )

!
!     Load up one name with NROOM masks.
!
      DO I = 1, NROOM

         BUFFER(  1 ) = 'NAIF_BODY_CODE += #'
         BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_TEST_DUPES'''

         CALL REPMI ( BUFFER(1), '#', TKBASE+I, BUFFER(1) )

         CALL LMPOOL ( BUFFER, 2 )

      END DO

!
!     Check the state of the system.
!
      DO I = 1, NROOM - 1

         NAME = '<UNCHANGED>'

         CALL BODC2N ( TKBASE+I, NAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
         CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

      END DO

      CALL BODC2N ( TKBASE+NROOM, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TEST_DUPES', OK )

      CALL BODN2C ( 'F_BODCOD_TEST_DUPES', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', TKBASE+NROOM, 0, OK )

!
!     Clear the pool to reset the variables.
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
!     This test case must be commented out in general, because
!     BODDEF assignments (as of yet) can not be removed once made.
!
      CALL TCASE ( 'More BODDEF definitions than SPICELIB''s MAXE (PEPPER: no overflow).' )

!
!     We expect this to result in an error being signaled, although
!     when this error is signaled is not exactly known.  This is
!     because it is not clear how many unique BODDEF assignments
!     prior to this test family's execution have occurred.
!
      J = MAXE - NPERM

      DO I = 1, J

         NAME = 'F_BODCOD_TEST_#'
         CALL REPMI ( NAME, '#', BDBASE+I, NAME )

         CALL BODDEF ( NAME, BDBASE+I )

         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      NAME = 'F_BODCOD_TEST_#'
      CALL REPMI ( NAME, '#', BDBASE+J+1, NAME )

      CALL BODDEF ( NAME, BDBASE+J+1 )
!
!     PEPPER: the definitions grow (SPICELIB signaled
!     SPICE(TOOMANYPAIRS) here): the name is defined.
!
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( NAME, CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', BDBASE+J+1, 0, OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check proper handling of spacing by TEXT' )

      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1019'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''TEST ING'''
      BUFFER(  3 ) = 'NAIF_BODY_CODE += 1020'
      BUFFER(  4 ) = 'NAIF_BODY_NAME += ''TEST    ING'''
      BUFFER(  5 ) = 'NAIF_BODY_CODE += 1021'
      BUFFER(  6 ) = 'NAIF_BODY_NAME += ''TESTING'''

      CALL LMPOOL ( BUFFER, 6 )

!
!     Check 'TESTING' -> 1021 mapping.
!
      CALL BODN2C ( 'TESTING', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1021, 0, OK )

!
!     Check 'TEST ING' -> 1020 (masked) mapping.
!
      CALL BODN2C ( 'TEST ING', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1020, 0, OK )

!
!     Check 'TEST    ING' -> 1020 mapping.
!
      CALL BODN2C ( 'TEST    ING', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1020, 0, OK )

!
!     Lookup up the masked code, 1019.
!
      NAME = '<UNCHANGED>'

      CALL BODC2N ( 1019, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Lookup the 1020 -> 'TEST    ING' mapping.
!
      CALL BODC2N ( 1020, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'TEST    ING', OK )

!
!     Lookup the 1021 -> 'TESTING' mapping.
!
      CALL BODC2N ( 1021, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'TESTING', OK )

!
!     Clean up.
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check proper handling of spacing by BODDEF' )

      CALL BODDEF ( 'TEST ING2', 1019 )
      CALL BODDEF ( 'TEST    ING2', 1020 )
      CALL BODDEF ( 'TESTING2', 1021 )

!
!     Check 'TESTING2' -> 1021 mapping.
!
      CALL BODN2C ( 'TESTING2', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1021, 0, OK )

!
!     Check 'TEST ING2' -> 1020 (masked) mapping.
!
      CALL BODN2C ( 'TEST ING2', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1020, 0, OK )

!
!     Check 'TEST    ING2' -> 1020 mapping.
!
      CALL BODN2C ( 'TEST    ING2', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1020, 0, OK )

!
!     Lookup up the masked code, 1019.
!
      NAME = '<UNCHANGED>'

      CALL BODC2N ( 1019, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Lookup the 1020 -> 'TEST    ING2' mapping.
!
      CALL BODC2N ( 1020, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'TEST    ING2', OK )

!
!     Lookup the 1021 -> 'TESTING2' mapping.
!
      CALL BODC2N ( 1021, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'TESTING2', OK )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Replace all built-in assignments with uniques' )

!
!     Set all built-in codes to point to their position in the
!     original sequence.
!
      DO I = 1, NPERM
         CALL BODDEF ( BASNAM(I), I )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

!
!     Verify that the changes worked.
!
      DO I = 1, NPERM

         CALL BODC2N ( I, NAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
         CALL CHCKSC ( 'NAME', NAME, '=', BASNAM(I), OK )

         CALL BODN2C ( BASNAM(I), CODE, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
         CALL CHCKSI ( 'CODE', CODE, '=', I, 0, OK )

      END DO

!
!     Restore built-in codes.
!
      DO I = 1, NPERM
         CALL BODDEF ( BASNAM(I), BASCOD(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Point all built-ins at a single code.' )

!
!     Set all built-in names to point to the code 1.
!
      DO I = 1, NPERM
         CALL BODDEF ( BASNAM(I), 1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

!
!     Verify that the changes worked properly.
!
      DO I = 1, NPERM
         CALL BODN2C ( BASNAM(I), CODE, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
         CALL CHCKSI ( 'CODE', CODE, '=', 1, 0, OK )
      END DO

      CALL BODC2N ( 1, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', BASNAM(NPERM), OK )

!
!     Restore built-in codes.
!
      DO I = 1, NPERM
         CALL BODDEF ( BASNAM(I), BASCOD(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Point all built-ins at a single name.' )

!
!     Set all built-in codes to point at 'NAME'.
!
      DO I = 1, NPERM
         CALL BODDEF ( 'NAME', BASCOD(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

!
!     Verify that the changes worked properly.
!
      DO I = 1, NPERM - 1

         CALL BODC2N ( BASCOD(I), NAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

         FOUND = .FALSE.
         J     = NPERM
         DO WHILE ( ( .NOT. FOUND ) .AND. ( J .GT. I ) )
            IF ( BASCOD(J) .EQ. BASCOD(I) ) THEN
               FOUND = .TRUE.
            ELSE
               J = J - 1
            END IF
         END DO

         CALL CHCKSC ( 'NAME', NAME, '=', BASNAM(J), OK )

      END DO

      CALL BODC2N ( BASCOD(NPERM), NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'NAME', OK )

      CALL BODN2C ( 'NAME', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', BASCOD(NPERM), 0, OK )

!
!     Restore built-in codes.
!
      DO I = 1, NPERM
         CALL BODDEF ( BASNAM(I), BASCOD(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check built-in assignment integrity.' )

!
!     Verify all of the codes and name pairs that exist in the
!     built-in NAME/CODE arrays.
!
      DO I = 1, NPERM

         CODE = 0
         NAME = '<UNCHANGED>'

         CALL BODN2C ( BASNAM(I), CODE, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
         CALL CHCKSI ( 'CODE', CODE, '=', BASCOD(I), 0, OK )

         CALL BODC2N ( BASCOD(I), NAME, FOUND )

         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!        Locate the name that should be found by searching in front
!        of the index I for a match.
!
         FOUND = .FALSE.
         J     = NPERM
         DO WHILE ( ( .NOT. FOUND ) .AND. ( J .GT. I ) )
            IF ( BASCOD(J) .EQ. BASCOD(I) ) THEN
               FOUND = .TRUE.
            ELSE
               J = J - 1
            END IF
         END DO

!
!        At this point J is either I, or J is pointing at the
!        index with the highest precedent assignment for BASCOD(I).
!
         CALL CHCKSC ( 'NAME', NAME, '=', BASNAM(J), OK )

      END DO

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check name to ID translation by BODS2C' )

      CALL BODS2C ( 'JUPITER', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ',               OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,     OK )
      CALL CHCKSI ( 'CODE',  CODE, '=', 599, 0, OK )


      CODE = 777
      CALL BODS2C ( 'JUP', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ',               OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,    OK )
      CALL CHCKSI ( 'CODE',  CODE, '=', 777, 0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check integer to ID translation by BODS2C' )

      CALL BODS2C ( '599', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ',               OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,     OK )
      CALL CHCKSI ( 'CODE',  CODE, '=', 599, 0, OK )

      CALL BODS2C ( '1000000000', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ',                      OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,            OK )
      CALL CHCKSI ( 'CODE',  CODE, '=', 1000000000, 0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check ZZBODTRN counter update' )

!
!     Clean up
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
!     Initialize the local POOL state counter.
!
      CALL ZZCTRUIN( USRCTR )

!
!     Test initial counter update and no update on the second
!     immediate call.
!
      UPDATE = .FALSE.
      CALL ZZBCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'initial counter UPDATE', UPDATE, .TRUE., OK )

      UPDATE = .TRUE.
      CALL ZZBCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no initial counter UPDATE', UPDATE, .FALSE., OK )

!
!     Add a mapping to the POOL. Check counter after BODN2C.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1002'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_1002'''
      CALL LMPOOL ( BUFFER, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODN2C ( 'F_BODCOD_1002', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE',  CODE, '=', 1002, 0, OK )

      UPDATE = .FALSE.
      CALL ZZBCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'Post-BODN2C counter UPDATE', UPDATE, .TRUE., OK )

!
!     Add a mapping to the POOL. Check counter after BODC2N.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1003'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_1003'''
      CALL LMPOOL ( BUFFER, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODC2N ( 1003, NAME, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_1003', OK )

      UPDATE = .FALSE.
      CALL ZZBCTRCK ( USRCTR, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Post-BODC2N counter UPDATE', UPDATE, .TRUE., OK )

!
!     Add a mapping using BODDEF. Check counter.
!
      CALL BODDEF ( 'F_BODCOD_1004', 1004 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .FALSE.
      CALL ZZBCTRCK ( USRCTR, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Post-BODDEF counter UPDATE', UPDATE, .TRUE., OK )

!
!     Add a mapping to the POOL. Check counter after ZZBODKIK.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1005'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_1005'''
      CALL LMPOOL ( BUFFER, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZBODKIK ()
      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .FALSE.
      CALL ZZBCTRCK ( USRCTR, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Post-ZZBODKIK counter UPDATE', UPDATE, .TRUE., OK )

!
!     We already added a BODDEF mapping. Check counter after ZZBODRST.
!
      CALL ZZBODRST ()
      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .FALSE.
      CALL ZZBCTRCK ( USRCTR, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Post-ZZBODRST counter UPDATE', UPDATE, .TRUE., OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
