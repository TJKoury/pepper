C$Procedure F_BODCOD ( Body Code/Name Mapping Test Family )
 
      SUBROUTINE F_BODCOD ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the body code to
C     name and name to code mapping software.
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
 
      INCLUDE              'zzbodtrn.inc'
      INCLUDE              'zzctr.inc'

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
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine exercises the conformance of the body name-to-code
C     and code-to-name mapping software to the changes implemented
C     in N0053.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     1) After a cursory examination of the body ID codes currently in
C        use in ZZBODTRN, it seems the range (1000,2000) is reasonable
C        to assume as "unused".  This test family makes use of codes
C        in this range for testing purposes.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     B.V. Semenov    (JPL)
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    TSPICE Version 3.0.0, 16-SEP-2016 (BVS)
C
C        Removed check for even NROOM. Changed "doubled up" test case
C        to not be dependent on even NROOM. Added ZZBODTRN counter
C        tests.
C
C-    TSPICE Version 2.0.0, 22-JUL-2004 (NJB)
C
C        Test cases for BODS2C were added.     
C
C-    TSPICE Version 1.0.0, 12-AUG-2002 (FST)
C
C
C-&
 
C
C     TESTUTIL Functions
C
      CHARACTER*(10)        BEGDAT
 
C
C     Local Parameters
C
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
 
C
C     Local Variables
C
      CHARACTER*(MAXL)      BASNAM ( MAXE )
      CHARACTER*(MAXL)      BASNOR ( MAXE )
      CHARACTER*(LINLEN)    BUFFER ( BUFLEN )
      CHARACTER*(NAMLEN)    HOLD
      CHARACTER*(NAMLEN)    NAME
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

C
C     Save everything.
C
      SAVE
 
C
C     Open the test family.
C
      CALL TOPEN ( 'F_BODCOD' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check cleared state of test module ID codes.' )
 
C
C     Verify that NROOM is even.  It must be otherwise a few of the
C     test cases will fail.
C
C     This test was commented out because the later test code was 
C     changed to work with the even number preceding NROOM.
C
C      CALL CHCKSI ( 'NROOM-EVEN', MOD(NROOM,2), '=', 0, 0, OK )
 
C
C     Retrieve the built-in code/name arrays.
C
      CALL ZZBODGET ( MAXE, BASNAM, BASNOR, BASCOD, NVALS )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Multiple NAMES to a single code, BODDEF' )
 
      CALL BODDEF ( 'F_BODCOD_TESTA', 1001 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that this assignment worked as expected.
C
      CALL BODC2N ( 1001, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTA', OK )
 
C
C     Now assign a new name to the same code using BODDEF.
C
      CALL BODDEF ( 'F_BODCOD_TEST', 1001 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Verify that the newer name takes precedence.
C
      CALL BODC2N ( 1001, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TEST', OK )
 
C
C     Depending on the sorting algorithm this could go a number
C     of ways.  Check a name that would occur "after" F_BODCOD_TESTA
C     in the list as well.
C
      CALL BODDEF ( 'F_BODCOD_TESTB', 1001 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Verify that the newer name takes precedence.
C
      CALL BODC2N ( 1001, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTB', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Multiple NAMES to a single code, TEXT KERNEL' )
 
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1002'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_TESTC'''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Verify that the assignment worked.
C
      CALL BODC2N ( 1002, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTC', OK )
 
C
C     Now append a new name with the same code.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE += 1002'
      BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_ATEST'''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Verify that the newer name takes precedence.
C
      CALL BODC2N ( 1002, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_ATEST', OK )
 
C
C     Depending on the sorting algorithm this could go a number
C     of ways.  Check a name that would occur "after" F_BODCOD_TESTC
C     in the list as well.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE += 1002'
      BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_TESTD'''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Verify that the newer name takes precedence.
C
      CALL BODC2N ( 1002, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTD', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Multiple NAMES to a single code, BODDEF then TEXT' )
 
      CALL BODDEF ( 'F_BODCOD_TESTE', 1003 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that this assignment worked as expected.
C
      CALL BODC2N ( 1003, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTE', OK )
 
C
C     Now add a new name with the text kernel system.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1003'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_TESTF'''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Verify that the text-kernel name takes precedence.
C
      CALL BODC2N ( 1003, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTF', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Multiple NAMES to a single code, TEXT then BODDEF' )
 
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1004'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_TESTG'''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that this assignment worked as expected.
C
      CALL BODC2N ( 1004, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTG', OK )
 
C
C     Now add a new name with the text kernel system.
C
      CALL BODDEF ( 'F_BODCOD_TESTH', 1004 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Verify that the text-kernel name takes precedence.
C
      CALL BODC2N ( 1004, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTG', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check whitespace/case-sensitivity on name, BODDEF' )
 
      CALL BODDEF ( 'F_BODcOd_TestJ  space', 1007 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that the assignment works properly.
C
      CALL BODC2N ( 1007, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODcOd_TestJ  space', OK )
 
C
C     Add the same name with different case/spacing.
C
      CALL BODDEF ( 'F_BoDCoD_TesTj    space', 1007 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that the assignment worked properly.
C
      CALL BODC2N ( 1007, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BoDCoD_TesTj    space', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check whitespace/case-sensitivity on name, TEXT' )
 
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1008'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''f_bodCoD_TesTK    blank'''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that the assignment works properly.
C
      CALL BODC2N ( 1008, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'f_bodCoD_TesTK    blank', OK )
 
C
C     Attempt to override this with a BODDEF assignment.
C
      CALL BODDEF ( 'F_bodCOD_TESTK   space', 1008 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check to see that this assignment worked.
C
      CALL BODN2C ( 'F_BODCOD_TESTK SPACE', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1008, 0, OK )
 
C
C     Now see that the latest text-kernel entry takes precedence.
C
      CALL BODC2N ( 1008, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'f_bodCoD_TesTK    blank', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Test diagnostics on text-kernel load.' )
 
C
C     Clear the pool.
C
      CALL CLPOOL ( )
 
C
C     Construct a text-kernel with name-code assignments that
C     will cause FURNSH to signal an error upon loading.
C
      TKNAME = 'testtk.tk'
 
      BUFFER(  1 ) = BEGDAT()
      BUFFER(  2 ) = 'NAIF_BODY_CODE = ( 1009, 1010 )'
      BUFFER(  3 ) = 'NAIF_BODY_NAME = ( ''SPUD'' )'
 
      CALL TSTTXT ( TKNAME, BUFFER, 3, .FALSE., .TRUE. )
 
C
C     Load the kernel and verify that an error has been signaled.
C
      CALL FURNSH ( TKNAME )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)', OK )
 
      CALL CLPOOL ( )
      CALL KILFIL ( TKNAME )
 
C
C     Try a simple code to name conversion to see if the error
C     is still floating around.
C
      CALL BODC2N ( 399, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
C
C     This test case is commented out, because no changes were made
C     to BODDEF in N0053.
C
      CALL TCASE ( 'BODDEF multiple codes to single name masking.' )
 
      CALL BODDEF ( 'F_BODCOD_TESTI', 1005 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that this assignment worked as expected.
C
      CALL BODC2N ( 1005, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTI', OK )
 
C
C     Now attempt to assign F_BODCOD_TESTI a new ID code, this
C     should be allowed due to masking updates.
C
      CALL BODDEF ( 'F_BODCOD_TESTI', 1006 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that the results of the assignment work as expected.
C
      CALL BODC2N ( 1006, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTI', OK )
 
      NAME = '<UNCHANGED>'
      CALL BODC2N ( 1005, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'TEXT multiple codes to single name masking.' )
 
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1011'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_TESTL'''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that this assignment worked as expected.
C
      CALL BODC2N ( 1011, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTL', OK )
 
C
C     Now attempt to assign F_BODCOD_TEST_L a new ID code.  This
C     should be allowed due to masking updates.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE += 1012'
      BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_TESTL'''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that the results of the assignment work as expected.
C
      CALL BODC2N ( 1012, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTL', OK )
 
      NAME = '<UNCHANGED>'
      CALL BODC2N ( 1011, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Simple BODDEF masked by TEXT case.' )
 
      CALL BODDEF ( 'F_BODCOD_TESTM', 1013 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that this assignment worked as expected.
C
      CALL BODC2N ( 1013, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTM', OK )
 
C
C     Now load a definition that masks this name.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1014'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_TESTM'''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Perform all of the necessary state checks at this point.
C
      CALL BODN2C ( 'F_BODCOD_TESTM', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1014, 0, OK )
 
      CALL BODC2N ( 1014, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTM', OK )
 
C
C     The original BODDEF code should now be effectively masked.
C
      NAME = '<UNCHANGED>'
      CALL BODC2N ( 1013, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Now clear the pool to unload the text kernel assignment.
C
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
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'TEXT first, invisible BODDEF' )
 
C
C     Load a definition via the text-kernel interface.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1015'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''F_BODCOD_TESTN'''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that this assignment worked as expected.
C
      CALL BODC2N ( 1015, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTN', OK )
 
C
C     Now make a BODDEF assignment that would appear to be
C     invisible as a result of masking.
C
      CALL BODDEF ( 'F_BODCOD_TESTN', 1016 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Perform all of the necessary state checks at this point.
C
      CALL BODN2C ( 'F_BODCOD_TESTN', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1015, 0, OK )
 
      CALL BODC2N ( 1015, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'F_BODCOD_TESTN', OK )
 
C
C     The BODDEF code should now be effectively masked.
C
      NAME = '<UNCHANGED>'
      CALL BODC2N ( 1016, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Now clear the pool to unload the text kernel assignment.
C
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
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'BODDEF overriding built-in codes' )
 
C
C     Obtain the current code for body to override.
C
      CALL BODN2C ( 'EARTH', RESTOR, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Now override the code for EARTH with a new one.
C
      CALL BODDEF ( 'EARTH', 1017 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that the assignment worked.
C
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
 
C
C     Restore the original code for EARTH.
C
      CALL BODDEF ( 'EARTH', RESTOR )
 
C
C     Check that it is restored.
C
      CALL BODN2C ( 'EARTH', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', RESTOR, 0, OK )
 
      CALL BODC2N ( RESTOR, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'TEXT overriding built-in codes' )
 
      CALL BODN2C ( 'MARS', RESTOR, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
 
C
C     Load a definition via the text-kernel interface that blasts
C     a built in code.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1018'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''MARS'''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that the assignment worked.
C
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
 
C
C     Restore the original assignment.
C
      CALL CLPOOL ( )
 
C
C     Check that it is restored.
C
      CALL BODN2C ( 'MARS', CODE, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', RESTOR, 0, OK )
 
      CALL BODC2N ( RESTOR, NAME, FOUND )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'MARS', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Fill TEXT buffer with unique entries' )
 
C
C     Load up many more definitions than ZZBODTRN can handle.
C
      DO I = 1, NROOM
 
         BUFFER(  1 ) = 'NAIF_BODY_CODE += #'
         BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_TEST_#'''
 
         CALL REPMI ( BUFFER(1), '#', TKBASE+I, BUFFER(1) )
         CALL REPMI ( BUFFER(2), '#', TKBASE+I, BUFFER(2) )
 
         CALL LMPOOL ( BUFFER, 2 )
 
      END DO
 
C
C     See what happens...
C
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
 
C
C     Clear the pool, resetting kernel pool variable assignments.
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Overflow TEXT buffer with unique entries.' )
 
C
C     Load up many more definitions than ZZBODTRN can handle.
C
      DO I = 1, NROOM + 1
 
         BUFFER(  1 ) = 'NAIF_BODY_CODE += #'
         BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_TEST_#'''
 
         CALL REPMI ( BUFFER(1), '#', TKBASE+I, BUFFER(1) )
         CALL REPMI ( BUFFER(2), '#', TKBASE+I, BUFFER(2) )
 
         CALL LMPOOL ( BUFFER, 2 )
 
      END DO
 
C
C     At this point we're set to observe an error signalled by the first
C     routine that causes the processing of the NAME/CODE kernel pool
C     variable pair.
C
      NAME = '<UNCHANGED>'
      CALL BODC2N ( TKBASE+1, NAME, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(KERVARTOOBIG)', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Clear the pool, resetting kernel pool variable assignments.
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Fill TEXT buffer with doubled up entries.' )
 
C
C     Fill TEXT buffer with doubled up entries.
C
      DO I = 1, (NROOM/2)*2
 
         BUFFER(  1 ) = 'NAIF_BODY_CODE += #'
         BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_TEST_#'''
 
         CALL REPMI ( BUFFER(1), '#', TKBASE+I, BUFFER(1) )
         CALL REPMI ( BUFFER(2), '#', TKBASE+((I+1)/2), BUFFER(2) )
 
         CALL LMPOOL ( BUFFER, 2 )
 
      END DO
 
C
C     Check results.
C
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
 
C
C     Clear the pool, resetting kernel pool variable assignments.
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Fill TEXT buffer with the same masked entries.' )
 
C
C     Load up one name with NROOM masks.
C
      DO I = 1, NROOM
 
         BUFFER(  1 ) = 'NAIF_BODY_CODE += #'
         BUFFER(  2 ) = 'NAIF_BODY_NAME += ''F_BODCOD_TEST_DUPES'''
 
         CALL REPMI ( BUFFER(1), '#', TKBASE+I, BUFFER(1) )
 
         CALL LMPOOL ( BUFFER, 2 )
 
      END DO
 
C
C     Check the state of the system.
C
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
 
C
C     Clear the pool to reset the variables.
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
C     This test case must be commented out in general, because
C     BODDEF assignments (as of yet) can not be removed once made.
C
      CALL TCASE ( 'Overflow BODDEF buffer.' )
 
C
C     We expect this to result in an error being signaled, although
C     when this error is signaled is not exactly known.  This is
C     because it is not clear how many unique BODDEF assignments
C     prior to this test family's execution have occurred.
C
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
 
      CALL CHCKXC ( .TRUE., 'SPICE(TOOMANYPAIRS)', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check proper handling of spacing by TEXT' )
 
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1019'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''TEST ING'''
      BUFFER(  3 ) = 'NAIF_BODY_CODE += 1020'
      BUFFER(  4 ) = 'NAIF_BODY_NAME += ''TEST    ING'''
      BUFFER(  5 ) = 'NAIF_BODY_CODE += 1021'
      BUFFER(  6 ) = 'NAIF_BODY_NAME += ''TESTING'''
 
      CALL LMPOOL ( BUFFER, 6 )
 
C
C     Check 'TESTING' -> 1021 mapping.
C
      CALL BODN2C ( 'TESTING', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1021, 0, OK )
 
C
C     Check 'TEST ING' -> 1020 (masked) mapping.
C
      CALL BODN2C ( 'TEST ING', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1020, 0, OK )
 
C
C     Check 'TEST    ING' -> 1020 mapping.
C
      CALL BODN2C ( 'TEST    ING', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1020, 0, OK )
 
C
C     Lookup up the masked code, 1019.
C
      NAME = '<UNCHANGED>'
 
      CALL BODC2N ( 1019, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Lookup the 1020 -> 'TEST    ING' mapping.
C
      CALL BODC2N ( 1020, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'TEST    ING', OK )
 
C
C     Lookup the 1021 -> 'TESTING' mapping.
C
      CALL BODC2N ( 1021, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'TESTING', OK )
 
C
C     Clean up.
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check proper handling of spacing by BODDEF' )
 
      CALL BODDEF ( 'TEST ING2', 1019 )
      CALL BODDEF ( 'TEST    ING2', 1020 )
      CALL BODDEF ( 'TESTING2', 1021 )
 
C
C     Check 'TESTING2' -> 1021 mapping.
C
      CALL BODN2C ( 'TESTING2', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1021, 0, OK )
 
C
C     Check 'TEST ING2' -> 1020 (masked) mapping.
C
      CALL BODN2C ( 'TEST ING2', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1020, 0, OK )
 
C
C     Check 'TEST    ING2' -> 1020 mapping.
C
      CALL BODN2C ( 'TEST    ING2', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1020, 0, OK )
 
C
C     Lookup up the masked code, 1019.
C
      NAME = '<UNCHANGED>'
 
      CALL BODC2N ( 1019, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Lookup the 1020 -> 'TEST    ING2' mapping.
C
      CALL BODC2N ( 1020, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'TEST    ING2', OK )
 
C
C     Lookup the 1021 -> 'TESTING2' mapping.
C
      CALL BODC2N ( 1021, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'TESTING2', OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Replace all built-in assignments with uniques' )
 
C
C     Set all built-in codes to point to their position in the
C     original sequence.
C
      DO I = 1, NPERM
         CALL BODDEF ( BASNAM(I), I )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO
 
C
C     Verify that the changes worked.
C
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
 
C
C     Restore built-in codes.
C
      DO I = 1, NPERM
         CALL BODDEF ( BASNAM(I), BASCOD(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Point all built-ins at a single code.' )
 
C
C     Set all built-in names to point to the code 1.
C
      DO I = 1, NPERM
         CALL BODDEF ( BASNAM(I), 1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO
 
C
C     Verify that the changes worked properly.
C
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
 
C
C     Restore built-in codes.
C
      DO I = 1, NPERM
         CALL BODDEF ( BASNAM(I), BASCOD(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Point all built-ins at a single name.' )
 
C
C     Set all built-in codes to point at 'NAME'.
C
      DO I = 1, NPERM
         CALL BODDEF ( 'NAME', BASCOD(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO
 
C
C     Verify that the changes worked properly.
C
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
 
C
C     Restore built-in codes.
C
      DO I = 1, NPERM
         CALL BODDEF ( BASNAM(I), BASCOD(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END DO
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check built-in assignment integrity.' )
 
C
C     Verify all of the codes and name pairs that exist in the
C     built-in NAME/CODE arrays.
C
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
 
C
C        Locate the name that should be found by searching in front
C        of the index I for a match.
C
         FOUND = .FALSE.
         J     = NPERM
         DO WHILE ( ( .NOT. FOUND ) .AND. ( J .GT. I ) )
            IF ( BASCOD(J) .EQ. BASCOD(I) ) THEN
               FOUND = .TRUE.
            ELSE
               J = J - 1
            END IF
         END DO
 
C
C        At this point J is either I, or J is pointing at the
C        index with the highest precedent assignment for BASCOD(I).
C
         CALL CHCKSC ( 'NAME', NAME, '=', BASNAM(J), OK )
 
      END DO
 
C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

C
C --- Case: ------------------------------------------------------
C
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


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check integer to ID translation by BODS2C' )

      CALL BODS2C ( '599', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ',               OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,     OK )
      CALL CHCKSI ( 'CODE',  CODE, '=', 599, 0, OK )

      CALL BODS2C ( '1000000000', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ',                      OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,            OK )
      CALL CHCKSI ( 'CODE',  CODE, '=', 1000000000, 0, OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Check ZZBODTRN counter update' )

C
C     Clean up
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

C
C     Initialize the local POOL state counter. 
C
      CALL ZZCTRUIN( USRCTR )

C
C     Test initial counter update and no update on the second 
C     immediate call.
C
      UPDATE = .FALSE.
      CALL ZZBCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'initial counter UPDATE', UPDATE, .TRUE., OK )
     
      UPDATE = .TRUE.
      CALL ZZBCTRCK ( USRCTR, UPDATE )
      CALL CHCKSL ( 'no initial counter UPDATE', UPDATE, .FALSE., OK )

C
C     Add a mapping to the POOL. Check counter after BODN2C.
C
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

C
C     Add a mapping to the POOL. Check counter after BODC2N.
C
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

C
C     Add a mapping using BODDEF. Check counter.
C
      CALL BODDEF ( 'F_BODCOD_1004', 1004 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .FALSE.
      CALL ZZBCTRCK ( USRCTR, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Post-BODDEF counter UPDATE', UPDATE, .TRUE., OK )

C
C     Add a mapping to the POOL. Check counter after ZZBODKIK.
C
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

C
C     We already added a BODDEF mapping. Check counter after ZZBODRST.
C
      CALL ZZBODRST ()
      CALL CHCKXC ( .FALSE., ' ', OK )

      UPDATE = .FALSE.
      CALL ZZBCTRCK ( USRCTR, UPDATE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'Post-ZZBODRST counter UPDATE', UPDATE, .TRUE., OK )

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
