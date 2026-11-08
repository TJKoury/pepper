C$Procedure F_ZZBDTN ( ZZBODTRN Test Family  )
 
      SUBROUTINE F_ZZBDTN ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in ZZBODTRN and its
C     entry points.
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
C     OK         O   logical indicated test status.
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
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine exercises the entry points of ZZBODTRN.
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
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 25-APR-2014 (BVS)
C
C        Updated ZZBODTRN call to match new calling sequence.
C
C-    TSPICE Version 1.0.0, 21-AUG-2002 (FST)
C
C
C-&
 
C
C     Local Parameters
C
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )
 
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 200 )
 
C
C     Local Variables
C
      CHARACTER*(LINLEN)    BUFFER ( 4 )
      CHARACTER*(NAMLEN)    NAME
 
      INTEGER               CODE
      INTEGER               USRCTR ( CTRSIZ )

      LOGICAL               FOUND
      LOGICAL               UPDATE
 
C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZBDTN' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZBODTRN - SPICE(BOGUSENTRY) exception' )
 
      CALL ZZBODTRN ( NAME, CODE, FOUND, USRCTR, UPDATE )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZBODRST - Coverage Test' )
 
C
C     Install a new mapping.
C
      CALL ZZBODDEF ( 'NAME', 1000 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check that the mapping is installed.
C
      CALL ZZBODN2C ( 'NAME', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1000, 0, OK )
 
C
C     Reset the body name-code system.
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C     Now verify that NAME<->1000 is not currently assigned.
C
      CALL ZZBODN2C ( 'NAME', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
      CALL ZZBODC2N ( 1000, 'NAME', FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZBODTRN - Check watchers are properly set' )
 
C
C     Make certain that the initialization block is executed
C     prior to attempting to check the watcher's set status.
C     It is difficult to verify that each initialization block
C     functions properly in a test family, since once INIT is
C     set the code will not execute again.
C
      CALL ZZBODN2C ( 'EARTH', CODE, FOUND )
 
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 3000'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''ZZBODTRN_TEST_BODY'''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Check to see that the watchers are activated.
C
      CALL CVPOOL ( 'ZZBODTRN', UPDATE )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )
 
C
C     Clean up.
C
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZBODN2C - Coverage Test - UPDATE' )
 
C
C     Cause CVPOOL to set UPDATE to .TRUE.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 3000'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''ZZBODTRN_TEST_BODY_2'
     .//             ''''
 
      CALL LMPOOL ( BUFFER, 2 )
 
      CODE  = 0
      FOUND = .TRUE.
 
C
C     Now invoke the module with an unknown name.
C
      CALL ZZBODN2C ( '<UNDEFINED>', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )
 
C
C     Verify that ZZBODTRN_TEST_BODY_2 is available now. EXTKER
C     success path.
C
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL ZZBODN2C ( 'ZZBODTRN_TEST_BODY_2', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 3000, 0, OK )
 
C
C     Check a built-in assignment, EXTKER failure path.  Make LJUST
C     UCASE, and COMPRSS perform non-trivial operations.
C
      CALL LMPOOL ( BUFFER, 2 )
 
      CALL ZZBODN2C ( '    earth    barycenter', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 3, 0, OK )
 
C
C     Set UPDATE and force EXTKER to be .FALSE., look for unknown name.
C
      CALL CLPOOL ( )
 
      CODE  = 0
      FOUND = .TRUE.
 
      CALL ZZBODN2C ( '<UNDEFINED>', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )
 
C
C     Check to see that ZZBODTRN_TEST_BODY_2 is no longer available.
C
      CALL CLPOOL ( )
 
      CODE  = 0
      FOUND = .TRUE.
 
      CALL ZZBODN2C ( 'ZZBODTRN_TEST_BODY_2', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )
 
C
C     Lastly look up a built-in name's code.
C
      CALL CLPOOL ( )
 
      CALL ZZBODN2C ( 'EARTH', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 399, 0, OK )
 
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZBODN2C - Coverage Test - no UPDATE' )
 
C
C     Force EXTKER to be set.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 3000'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''ZZBODTRN_TEST_BODY_3'
     .//             ''''
 
      CALL LMPOOL ( BUFFER, 2 )
 
C
C     Make sure no watchers set UPDATE.
C
      CODE = 0
 
      CALL ZZBODN2C ( '<UNDEFINED>', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )
 
C
C     Now invoke ZZBODN2C with the name defined in the kernel pool.
C
      CALL ZZBODN2C ( 'ZZBODTRN_TEST_BODY_3', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 3000, 0, OK )
 
C
C     Now invoke ZZBODN2C with a built-in name.
C
      CALL ZZBODN2C ( 'EARTH', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 399, 0, OK )
 
C
C     Lastly invoke it with an unknown name.
C
      CODE = 0
 
      CALL ZZBODN2C ( '<UNDEFINED>', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )
 
C
C     Clear the pool and remove EXTKER's .TRUE. status.
C
      CALL CLPOOL ( )
 
      CODE = 0
 
      CALL ZZBODN2C ( '<UNDEFINED>', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )
 
C
C     Now check the previously defined name in the kernel pool.
C
      CODE = 0
 
      CALL ZZBODN2C ( 'ZZBODTRN_TEST_BODY_3', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )
 
C
C     Check a built-in code.
C
      CALL ZZBODN2C ( 'EARTH', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 399, 0, OK )
 
C
C     Lastly invoke it with an undefined name.
C
      CODE = 0
 
      CALL ZZBODN2C ( '<UNDEFINED>', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )
 
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZBODC2N - Coverage Test - UPDATE' )
 
C
C     Cause CVPOOL to set UPDATE to .TRUE.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 3000'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''ZZBODTRN_TEST_BODY_4'
     .//             ''''
      BUFFER(  3 ) = 'NAIF_BODY_CODE += 3002'
      BUFFER(  4 ) = 'NAIF_BODY_NAME += ''EARTH'''
 
      CALL LMPOOL ( BUFFER, 4 )
 
      NAME = '<UNCHANGED>'
      FOUND = .TRUE.
 
C
C     Now invoke the module with an unknown code.
C
      CALL ZZBODC2N ( 3001, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Verify that 3001 is available now. EXTKER success path.
C
      CALL LMPOOL ( BUFFER, 4 )
 
      CALL ZZBODC2N ( 3000, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'ZZBODTRN_TEST_BODY_4', OK )
 
C
C     Check a built-in assignment, EXTKER failure path no "kernel
C     pool" masking.
C
      CALL LMPOOL ( BUFFER, 4 )
 
      CALL ZZBODC2N ( 3, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH BARYCENTER', OK )
 
C
C     Check a built-in assignment, EXTKER failure path with "kernel
C     pool" masking.
C
      CALL LMPOOL ( BUFFER, 4 )
 
      NAME = '<UNCHANGED>'
 
      CALL ZZBODC2N ( 399, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Set UPDATE and force EXTKER to be .FALSE., look for unknown code.
C
      CALL CLPOOL ( )
 
      NAME  = '<UNCHANGED>'
      FOUND = .TRUE.
 
      CALL ZZBODC2N ( 3001, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Check to see that 3000 is no longer available.
C
      CALL CLPOOL ( )
 
      NAME  = '<UNCHANGED>'
      FOUND = .TRUE.
 
      CALL ZZBODC2N ( 3000, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Lastly look up a built-in code's name.
C
      CALL CLPOOL ( )
 
      CALL ZZBODC2N ( 399, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH', OK )
 
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZBODC2N - Coverage Test - no UPDATE' )
 
C
C     Force EXTKER to be set.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 3000'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''ZZBODTRN_TEST_BODY_5'
     .//             ''''
      BUFFER(  3 ) = 'NAIF_BODY_CODE += 3002'
      BUFFER(  4 ) = 'NAIF_BODY_NAME += ''EARTH'''
 
      CALL LMPOOL ( BUFFER, 4 )
 
C
C     Make sure no watchers set UPDATE.
C
      NAME = '<UNCHANGED>'
 
      CALL ZZBODC2N ( 3001, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Now invoke ZZBODN2C with a code defined in the kernel pool.
C
      CALL ZZBODC2N ( 3000, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'ZZBODTRN_TEST_BODY_5', OK )
 
C
C     Check a built-in code, EXTKER is .TRUE., with no "kernel pool"
C     masking.
C
      CALL ZZBODC2N ( 3, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH BARYCENTER', OK )
 
 
C
C     Check a built-in code, EXTER is .TRUE., with "kernel pool"
C     masking.
C
      NAME = '<UNCHANGED>'
 
      CALL ZZBODC2N ( 399, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Check for an unknown code.
C
      NAME = '<UNCHANGED>'
 
      CALL ZZBODC2N ( 3001, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Now clear the pool to remove EXTKER's .TRUE. status.
C
      CALL CLPOOL ( )
 
      NAME = '<UNCHANGED>'
 
C
C     Look up an unknown code, to clear UPDATE's status.
C
      CALL ZZBODC2N ( 3001, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Now check the previously defined codes in the kernel pool.
C
      NAME = '<UNCHANGED>'
 
      CALL ZZBODC2N ( 3000, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
      NAME = '<UNCHANGED>'
 
      CALL ZZBODC2N ( 3002, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
C
C     Now look up the built-in codes.
C
      CALL ZZBODC2N ( 399, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH', OK )
 
      CALL ZZBODC2N ( 3, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH BARYCENTER', OK )
 
C
C     Lastly invoke it with an unknown code.
C
      NAME = '<UNCHANGED>'
 
      CALL ZZBODC2N ( 3001, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )
 
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZBODDEF - Exception SPICE(BLANKNAMEASSIGNED)' )
 
      CALL ZZBODDEF ( ' ', 1000 )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKNAMEASSIGNED)', OK )
 
C
C     Check that it did nothing to the name-code mapping
C     system.
C
      CALL ZZBODN2C ( ' ', CODE, FOUND )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
      CALL ZZBODC2N ( 1000, NAME, FOUND )
 
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
 
C
C     Clean up.
C
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZBODDEF - Coverage Tests - Name Replace Case' )
 
C
C     Replace the name for EARTH with the same code.  This forces a
C     simple replace, rather than a sort/update.
C
      CALL ZZBODDEF ( 'eArTh', 399 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     See if it worked.
C
      CALL ZZBODC2N ( 399, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'eArTh', OK )
 
      CALL ZZBODN2C ( 'eArTh', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 399, 0, OK )
 
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZBODDEF - Coverage Tests - Name Replace Sort' )
 
C
C     Replace the name for EARTH with a new code.  This forces a
C     compression and append operation.
C
      CALL ZZBODDEF ( 'EaRtH', 1000 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     See if it worked.
C
      CALL ZZBODC2N ( 1000, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EaRtH', OK )
 
      CALL ZZBODN2C ( 'EaRtH', CODE, OK )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1000, 0, OK )
 
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'ZZBODDEF - Coverage Tests - Append New' )
 
C
C     Replace the name for EARTH with a new code.  This forces a
C     compression and append operation.
C
      CALL ZZBODDEF ( 'SPUDSpam', 1000 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     See if it worked.
C
      CALL ZZBODC2N ( 1000, NAME, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'SPUDSpam', OK )
 
      CALL ZZBODN2C ( 'spudspam', CODE, FOUND )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1000, 0, OK )
 
      CALL CLPOOL ( )
      CALL ZZBODRST ( )
 
C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
